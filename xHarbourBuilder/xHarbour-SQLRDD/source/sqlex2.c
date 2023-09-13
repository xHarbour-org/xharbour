/* $CATEGORY$SQLEX/HIDE$FILES$HIDE$
* SQLEX Auxiliar File for INSERT and UPDATE routines
* Copyright (c) 2009 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "compat.h"
#include "hbinit.h"

#include "msg.ch"
#include "rddsys.ch"
#include "hbdbferr.h"

#include "sqlrddsetup.ch"
#include "sqlprototypes.h"

#include <ctype.h>
#include <assert.h>

#if defined(HB_OS_WIN_32) || defined(HB_OS_WIN_64) || defined( HB_OS_WIN ) 
   #include <windows.h>
   #include <odbcinst.h>
#else
   #include <stdlib.h>
   #include <unistd.h>
   #include <errno.h>
   #include <sys/types.h>
   #include <sys/wait.h>
   #define SQL_WCHAR  (-8)
   #define SQL_WLONGVARCHAR  (-10)
   #define SQL_C_WCHAR  SQL_WCHAR
#endif

#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

#include "sqlex.h"

/*------------------------------------------------------------------------*/

static  PHB_DYNS s_pSym_Serial1 = NULL;   /* Pointer to serialization function */

/*------------------------------------------------------------------------*/

char * QualifyName( char * szName, SQLEXAREAP thiswa )
{
   int i, len;

   len = strlen( szName );

   for( i = 0; i < len; i++ )
   {
      if( szName[i] == '\0' )
      {
         break;
      }
      switch( thiswa->nSystemID )
      {
      case SYSTEMID_MSSQL7:
      case SYSTEMID_ORACLE:
      case SYSTEMID_FIREBR:
      case SYSTEMID_IBMDB2:
      case SYSTEMID_ADABAS:
         szName[i] = ( char ) HB_TOUPPER( ( unsigned char ) szName[i] );
         break;
      case SYSTEMID_INGRES:
      case SYSTEMID_POSTGR:
      case SYSTEMID_MYSQL:
      case SYSTEMID_MARIADB:
      case SYSTEMID_OTERRO:
      case SYSTEMID_INFORM:
         szName[i] = ( char ) HB_TOLOWER( ( unsigned char ) szName[i] );
         break;
      }
   }
   return szName;
}

/*------------------------------------------------------------------------*/

static void ResolveSpecialCols( SQLEXAREAP thiswa )
{
   // Resolve all Synthetic Index and FOR clause expressions, storing
   // results in thiswa->aBuffer
   // TO DO: Creating a new Index should reset INSERT Stmt cos it may
   //        create a new field like INDKEY_???

   int i, iIndexes;
   PHB_ITEM pIndex;
   PHB_ITEM pKeyVal;
   PHB_ITEM pIndIt;
   USHORT uiPos;
   int iOldArea;
   
   if( !thiswa->pIndexMgmnt )
   {
      hb_objSendMsg( thiswa->oWorkArea, "AINDEXMGMNT", 0 );
      thiswa->pIndexMgmnt  = hb_itemNew( NULL ) ;      
      hb_itemForwardValue( thiswa->pIndexMgmnt, hb_stackReturnItem()  );            
   }
   iOldArea = hb_rddGetCurrentWorkAreaNumber();
   if (iOldArea != thiswa->area.uiArea )    
   {
      hb_rddSelectWorkAreaNumber( thiswa->area.uiArea );
   }   
   iIndexes    = hb_arrayLen( thiswa->pIndexMgmnt );

   
   for( i=1; i <= iIndexes; i++ )
   {
      pIndex = hb_arrayGetItemPtr( thiswa->pIndexMgmnt, i );
          pIndIt = hb_itemArrayGet( pIndex, INDEXMAN_COLUMNS );
         //pIndIt = hb_arrayGetItemPtr( pIndex, INDEXMAN_COLUMNS );
         

      if( !SR_itemEmpty( pIndIt ) )
      {
         EVALINFO info;
         hb_evalNew( &info, hb_itemArrayGet( pIndex, INDEXMAN_KEY_CODEBLOCK ) );
         pKeyVal = hb_evalLaunch( &info );
         hb_evalRelease( &info );

         // Get field position in ::aLocalBuffer
         //uiPos = (USHORT) hb_itemGetNI( hb_arrayGetItemPtr( pIndex, INDEXMAN_SYNTH_COLPOS ) );
         uiPos = (USHORT) hb_itemGetNI( hb_itemArrayGet( pIndex, INDEXMAN_SYNTH_COLPOS ) );
         thiswa->specialMask[ uiPos ] = '1';

         hb_arraySetForward( thiswa->aBuffer, uiPos, pKeyVal );
         hb_itemRelease( pKeyVal );
      }

      //pIndIt = hb_arrayGetItemPtr( pIndex, INDEXMAN_FOR_CODEBLOCK );
      pIndIt = hb_itemArrayGet( pIndex, INDEXMAN_FOR_CODEBLOCK );

      if( !SR_itemEmpty( pIndIt ) )
      {
         EVALINFO info;
         hb_evalNew( &info, hb_itemArrayGet( pIndex, INDEXMAN_FOR_CODEBLOCK ) );
         pKeyVal = hb_evalLaunch( &info );
         hb_evalRelease( &info );

         // Get field position in ::aLocalBuffer
         //uiPos = (USHORT) hb_itemGetNI( hb_arrayGetItemPtr( pIndex, INDEXMAN_FOR_COLPOS ) );
         uiPos = (USHORT) hb_itemGetNI( hb_itemArrayGet( pIndex, INDEXMAN_FOR_COLPOS ) );
         thiswa->specialMask[ uiPos ] = '1';
         hb_arraySetForward( thiswa->aBuffer, uiPos, pKeyVal );
         hb_itemRelease( pKeyVal );
      }
   }
if (iOldArea != thiswa->area.uiArea )    
      hb_rddSelectWorkAreaNumber(iOldArea );   
}

/*------------------------------------------------------------------------*/

static void SerializeMemo( PHB_ITEM pFieldData )
{
   if( !s_pSym_Serial1 )
   {
      s_pSym_Serial1 = hb_dynsymFindName( "SR_SERIALIZE1" );
   }
   hb_vmPushDynSym( s_pSym_Serial1 );
   hb_vmPushNil();
   hb_vmPush( pFieldData );
   hb_vmDo( 1 );
   hb_itemForwardValue( pFieldData, hb_stackReturnItem() );
}

/*------------------------------------------------------------------------*/

void SetInsertRecordStructure( SQLEXAREAP thiswa )
{
   thiswa->InsertRecord = (COLUMNBINDP) hb_xgrab( hb_arrayLen( thiswa->aFields ) * sizeof( COLUMNBIND ) );
   memset( thiswa->InsertRecord, 0, hb_arrayLen( thiswa->aFields ) * sizeof( COLUMNBIND ) );
}

/*------------------------------------------------------------------------*/

void CreateInsertStmt( SQLEXAREAP thiswa )
{
   int iCols, i;
   PHB_ITEM pFieldStruct, pFieldLen, pFieldDec;
   LONG lFieldPosWA, lType;
   char * colName, * sFields, * sParams, * temp;
   char ident[200] = {0};
   char tablename[100] = {0};
   char declare[200] = {0};
   char cType;
   BOOL bNullable, bMultiLang, bIsMemo;
   COLUMNBINDP InsertRecord;
   USHORT uiPos;

   iCols    = hb_arrayLen( thiswa->aFields );

   if( ! thiswa->InsertRecord )
   {
      SetInsertRecordStructure( thiswa );
   }

   InsertRecord = thiswa->InsertRecord;
   sFields      = (char *) hb_xgrab( FIELD_LIST_SIZE * sizeof( char ) );
   sParams      = (char *) hb_xgrab( (FIELD_LIST_SIZE_PARAM) * sizeof( char ) );
   uiPos        = 0;
   sFields[0]   = '\0';

   for( i=1; i <= iCols; i++ )
   {
      pFieldStruct = hb_arrayGetItemPtr( thiswa->aFields, i );
      bNullable    = hb_arrayGetL( pFieldStruct, FIELD_NULLABLE );
      pFieldLen    = hb_arrayGetItemPtr( pFieldStruct, FIELD_LEN );
      pFieldDec    = hb_arrayGetItemPtr( pFieldStruct, FIELD_DEC );
      lFieldPosWA  = hb_arrayGetNL( pFieldStruct, FIELD_WAOFFSET );
      lType        = hb_arrayGetNL( pFieldStruct, FIELD_DOMAIN );
      cType        = ( * hb_arrayGetCPtr( pFieldStruct, FIELD_TYPE ));
      colName      = hb_arrayGetC( pFieldStruct, FIELD_NAME );
      bMultiLang   = hb_arrayGetL( pFieldStruct, FIELD_MULTILANG );
      if ( bMultiLang ) 
         cType = 'M';
      bIsMemo      = cType == 'M' || bMultiLang ;

      if( i != (int)(thiswa->ulhRecno) )      // RECNO is never included in INSERT column list
      {
         temp = hb_strdup( (const char *) sFields );
         sprintf( sFields, "%s,%c%s%c", temp, OPEN_QUALIFIER( thiswa ), QualifyName( colName, thiswa ), CLOSE_QUALIFIER( thiswa ) );
         sParams[uiPos]   = ',';
         sParams[++uiPos] = '?';
         sParams[++uiPos] = '\0';
         hb_xfree( temp );
      }

      hb_xfree( colName );

      InsertRecord->iSQLType        = (int)lType;
      InsertRecord->isNullable      = bNullable;
      InsertRecord->isBoundNULL     = FALSE;
      InsertRecord->lFieldPosDB     = i;
      InsertRecord->lFieldPosWA     = lFieldPosWA;
		InsertRecord->ColumnSize      = (SQLUINTEGER) hb_itemGetNI( pFieldLen );
		InsertRecord->DecimalDigits   = (SQLSMALLINT) hb_itemGetNI( pFieldDec );
      InsertRecord->isArgumentNull  = FALSE;
      InsertRecord->isMemo          = bIsMemo;
      InsertRecord->isMultiLang     = bMultiLang;

#ifdef SQLRDD_TOPCONN
      switch ( lType )
      {
         case SQL_FAKE_NUM:
         {
            lType = SQL_FLOAT;
            break;
         }
         case SQL_FAKE_DATE:
         {
            lType = SQL_CHAR;
            break;
         }
      }
#endif

      switch( cType )
      {
         case 'M':
         {
            InsertRecord->iCType            = SQL_C_BINARY;
            InsertRecord->asChar.value      = (SQLCHAR *) hb_xgrab( INITIAL_MEMO_ALLOC );
            InsertRecord->asChar.size_alloc = INITIAL_MEMO_ALLOC;
            InsertRecord->asChar.size       = 0;
            InsertRecord->asChar.value[0]   = '\0';
            InsertRecord->ColumnSize        = 0;
            break;
         }
         case 'C':
         {
            InsertRecord->asChar.value      = (SQLCHAR *) hb_xgrab( InsertRecord->ColumnSize + 1 );
            InsertRecord->asChar.size_alloc = InsertRecord->ColumnSize + 1;
            InsertRecord->iCType            = SQL_C_CHAR;
            InsertRecord->asChar.size       = 0;
            InsertRecord->asChar.value[0]   = '\0';
            break;
         }
         case 'N':
         {
            InsertRecord->iCType          = SQL_C_DOUBLE;
            break;
         }
        
         case 'D':
         {
           // Corrigido 27/12/2013 09:53 - lpereira
           // Estava atribuindo o valor de SYSTEMID_ORACLE para thiswa->nSystemID.
           //if ( thiswa->nSystemID = SYSTEMID_ORACLE )
	        if ( thiswa->nSystemID == SYSTEMID_ORACLE )
	           InsertRecord->iCType          = SQL_C_TYPE_TIMESTAMP;        // May be DATE or TIMESTAMP
	        else
            InsertRecord->iCType          = lType;        // May be DATE or TIMESTAMP
            break;
         }
         case 'T':
         {
	         //DebugBreak();
            InsertRecord->iCType          = SQL_C_TYPE_TIMESTAMP;        // May be DATE or TIMESTAMP
            break;
         }         
         case 'L':
         {
            InsertRecord->iCType          = SQL_C_BIT;
            break;
         }
      }
      //if (InsertRecord->isMultiLang) // culik, se e multiplang, binda como binario
      // InsertRecord->iCType            = SQL_C_BINARY;
      InsertRecord++;
   }

   sParams[0] = ' ';
   sFields[0] = ' ';

   switch ( thiswa->nSystemID )
   {
   case SYSTEMID_MSSQL7:
   case SYSTEMID_SYBASE:
   {
//      sprintf( ident, " SELECT @@IDENTITY ;" );
      sprintf( ident, "SELECT %s FROM @InsertedData;",thiswa->sRecnoName);
      sprintf( declare,"Declare @InsertedData table ( %s numeric(15,0) );",thiswa->sRecnoName);
//      sprintf( ident, ";SELECT SCOPE_IDENTITY() AS NewID ;" );
      break;
   }
   case SYSTEMID_FIREBR:
   {
      sprintf( ident, " RETURNING %s", thiswa->sRecnoName );
      break;
   }
   case SYSTEMID_POSTGR:
   {
      sprintf( tablename, "%s", thiswa->szDataFileName );
      if( strlen( tablename ) > ( MAX_TABLE_NAME_LENGHT - 3 ) )
      {
         tablename[MAX_TABLE_NAME_LENGHT-4] = '\0';
      }
      sprintf( ident, "; SELECT currval('%s_SQ');", tablename );
      break;
   }
   case SYSTEMID_ORACLE:
   case SYSTEMID_CACHE:
   case SYSTEMID_INFORM:
   case SYSTEMID_MYSQL:
   {
      ident[0] = '\0';
      break;
   }
   case SYSTEMID_IBMDB2:
   {
      sprintf( ident, "; VALUES IDENTITY_VAL_LOCAL();" );
      break;
   }
   default:
      ident[0] = '\0';
   }
   if ( thiswa->sSql ) 
      hb_xfree(thiswa->sSql ) ;
   thiswa->sSql               = ( char * ) hb_xgrab( MAX_SQL_QUERY_LEN * sizeof( char ) );
   memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   if (thiswa->nSystemID ==  SYSTEMID_MSSQL7 )
   {
      sprintf( thiswa->sSql, "%s INSERT INTO %s (%s ) OUTPUT Inserted.%s INTO @InsertedData(%s) VALUES (%s );%s", declare, thiswa->sTable, sFields, thiswa->sRecnoName, thiswa->sRecnoName,sParams ,ident);

   // sprintf( thiswa->sSql, "%s INSERT INTO %s (%s ) VALUES (%s );%s", declare, thiswa->sTable, sFields, sParams ,ident);

   }   
   else
   {
   sprintf( thiswa->sSql, "INSERT INTO %s (%s ) VALUES (%s )%s", thiswa->sTable, sFields, sParams, ident );
   }   

   hb_xfree( sFields );
   hb_xfree( sParams );
}

/*------------------------------------------------------------------------*/

HB_ERRCODE PrepareInsertStmt( SQLEXAREAP thiswa )
{
   SQLRETURN res;

   res = SQLAllocHandle( SQL_HANDLE_STMT, (HDBC) thiswa->hDbc, &(thiswa->hStmtInsert) );

   if ( CHECK_SQL_N_OK( res ) )
   {
      odbcErrorDiagRTE( thiswa->hStmtInsert, "PrepareInsertStmt/SQLAllocStmt", thiswa->sSql, res, __LINE__, __FILE__ );
      return HB_FAILURE;
   }

   res = SQLPrepare( thiswa->hStmtInsert, (SQLCHAR *) (thiswa->sSql), SQL_NTS );

   if ( CHECK_SQL_N_OK( res ) )
   {
      odbcErrorDiagRTE( thiswa->hStmtInsert, "PrepareInsertStmt", thiswa->sSql, res, __LINE__, __FILE__ );
      return HB_FAILURE;
   }
         
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE BindInsertColumns( SQLEXAREAP thiswa )
{
   int iCol, iCols, iBind;
   COLUMNBINDP InsertRecord;
   SQLRETURN res = SQL_ERROR;

   iCols        = hb_arrayLen( thiswa->aFields );
   InsertRecord = thiswa->InsertRecord;
   iBind        = 0;

   for (iCol = 1; iCol <= iCols; iCol++)
   {
      if( iCol != (int)(thiswa->ulhRecno) )                // RECNO is never included in INSERT column list
      {
         iBind++;
         switch (InsertRecord->iCType)
         {
            case SQL_C_CHAR:
            {
               InsertRecord->lIndPtr = SQL_NTS;

               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtInsert, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) InsertRecord->iCType,
                                       (SQLSMALLINT  ) InsertRecord->iSQLType,
                                       (SQLULEN      ) InsertRecord->ColumnSize,
                                       (SQLSMALLINT  ) InsertRecord->DecimalDigits,
                                       (SQLPOINTER   ) InsertRecord->asChar.value, 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) &(InsertRecord->lIndPtr) );
               break;
            }
            case SQL_C_BINARY:
            {
               SQLINTEGER nInd;
               InsertRecord->lIndPtr = SQL_NTS;
               nInd = strlen((const char *)(InsertRecord->asChar.value));
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtInsert, 
			                           (SQLUSMALLINT ) iBind,
                                       (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) SQL_C_CHAR,
                                       (SQLSMALLINT  ) SQL_LONGVARCHAR,
                                       (SQLULEN      ) InsertRecord->asChar.size_alloc,
                                       (SQLSMALLINT  ) 0,
                                       (SQLPOINTER   ) InsertRecord->asChar.value,
                                       (SQLLEN       ) nInd,
                                       (SQLLEN*      ) &(InsertRecord->lIndPtr));
               break;
            }
            case SQL_C_DOUBLE:
            {
               InsertRecord->lIndPtr = 0;
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtInsert, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) InsertRecord->iCType,
                                       (SQLSMALLINT  ) InsertRecord->iSQLType,
                                       (SQLULEN      ) InsertRecord->ColumnSize,
                                       (SQLSMALLINT  ) InsertRecord->DecimalDigits,
                                       (SQLPOINTER   ) &(InsertRecord->asNumeric), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) &(InsertRecord->lIndPtr) );
               break;
            }
            case SQL_C_TYPE_TIMESTAMP:
            {
               //DebugBreak();
               InsertRecord->lIndPtr = 0;
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtInsert, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) SQL_C_TYPE_TIMESTAMP,
                                       (SQLSMALLINT  ) SQL_TYPE_TIMESTAMP,
                                       (SQLULEN      ) SQL_TIMESTAMP_LEN,
                                       (SQLSMALLINT  ) thiswa->nSystemID == SYSTEMID_MSSQL7 ||thiswa->nSystemID == SYSTEMID_AZURE ? 3 : 0 ,
                                       (SQLPOINTER   ) &(InsertRecord->asTimestamp), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) &(InsertRecord->lIndPtr) );
               break;
            }
            case SQL_C_TYPE_DATE:
            {
               InsertRecord->lIndPtr = 0;
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtInsert, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) SQL_C_TYPE_DATE,
                                       (SQLSMALLINT  ) SQL_TYPE_DATE,
                                       (SQLULEN      ) SQL_DATE_LEN,
                                       (SQLSMALLINT  ) 0,
                                       (SQLPOINTER   ) &(InsertRecord->asDate), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) &(InsertRecord->lIndPtr) );
               break;
            }
            case SQL_C_BIT:
            {
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtInsert, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) InsertRecord->iCType,
                                       (SQLSMALLINT  ) InsertRecord->iSQLType,
                                       (SQLULEN      ) InsertRecord->ColumnSize,
                                       (SQLSMALLINT  ) InsertRecord->DecimalDigits,
                                       (SQLPOINTER   ) &(InsertRecord->asLogical), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) NULL );
               break;
            }
         }

         InsertRecord->iParNum = iBind;

         if ( CHECK_SQL_N_OK( res ) )
         {
            odbcErrorDiagRTE( thiswa->hStmtInsert, "BindInsertColumns", thiswa->sSql, res, __LINE__, __FILE__ );
            return HB_FAILURE;
         }
      }
      InsertRecord++;
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE FeedRecordCols( SQLEXAREAP thiswa, BOOL bUpdate )
{
   int iCols, i;
   PHB_ITEM pFieldData, pTemp;
   COLUMNBINDP InsertRecord;

   iCols    = hb_arrayLen( thiswa->aFields );

   if( bUpdate )
   {
      InsertRecord = thiswa->CurrRecord;
   }
   else
   {
      InsertRecord = thiswa->InsertRecord;
   }

   if ( !bUpdate )
   {

       hb_arraySetNL( thiswa->aInfo, AINFO_RECNO, GetCurrentRecordNum( thiswa )-1 );
   }   
   
   ResolveSpecialCols( thiswa );    // Fix INDKEY and FOR CLAUSE columns
                  
   for( i=1; i <= iCols; i++ )
   {
      if( (!bUpdate) || (bUpdate && (thiswa->editMask[ i-1 ] || thiswa->specialMask[ i-1 ]) ) )
      {
         if( i == (int)(thiswa->ulhDeleted) )
         {
            SetBindEmptylValue( InsertRecord );     // Writes a ' ' to deleted flag
         }
         else if( i != (int)(thiswa->ulhRecno) )                // RECNO is never included in INSERT column list
         {
            // Get item value from Workarea
            pFieldData   = hb_arrayGetItemPtr( thiswa->aBuffer, i );

            if( SR_itemEmpty( pFieldData ) && (!InsertRecord->isNullable) )
            {
               if( SetBindEmptylValue( InsertRecord ) == HB_FAILURE )
                  return HB_FAILURE;
            }
            else
            {
               if( InsertRecord->isMultiLang && HB_IS_STRING( pFieldData ) )
               {
                  // Transform multilang field in HASH
                  PHB_ITEM pLangItem = hb_itemNew( NULL );
                  pTemp = hb_hashNew( NULL );
#ifdef __XHARBOUR__
                  hb_hashAdd( pTemp, ULONG_MAX, sr_getBaseLang( pLangItem ), pFieldData );
#else
                  hb_hashAdd( pTemp, sr_getBaseLang( pLangItem ), pFieldData );
#endif
                  hb_itemRelease( pLangItem );
                  hb_itemForwardValue( pFieldData, pTemp );
                  hb_itemRelease( pTemp );
               }
               if( InsertRecord->isMemo && (! HB_IS_STRING( pFieldData ) ) )
               {
                  // Serialize memo
                  SerializeMemo( pFieldData );
               }

               if( SetBindValue( pFieldData, InsertRecord, bUpdate ? thiswa->hStmtUpdate : thiswa->hStmtInsert ) == HB_FAILURE )
                  return HB_FAILURE;
            }
         }
      }
      InsertRecord++;
   }

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE ExecuteInsertStmt( SQLEXAREAP thiswa )
{
   SQLRETURN res;

   
   res = SQLExecute( thiswa->hStmtInsert );

      
   if ( CHECK_SQL_N_OK( res ) )
   {
	  
      odbcErrorDiagRTE( thiswa->hStmtInsert, "ExecuteInsertStmt/SQLExecute", thiswa->sSql, res, __LINE__, __FILE__ );
      SQLCloseCursor( thiswa->hStmtInsert );
      return (HB_FAILURE);
   }

   // Retrieve RECNO

   switch ( thiswa->nSystemID )
   {
   case SYSTEMID_MSSQL7:
   case SYSTEMID_SYBASE:
   case SYSTEMID_IBMDB2:
   case SYSTEMID_POSTGR:
   case SYSTEMID_FIREBR:
   {
      if( thiswa->nSystemID != SYSTEMID_FIREBR )
      {
         //#if defined( _MSC_VER ) 
            res = SQLMoreResults( thiswa->hStmtInsert );
            if ( res != SQL_SUCCESS )
            {
         res = SQLMoreResults( thiswa->hStmtInsert );
         if ( CHECK_SQL_N_OK( res ) )
         {
            odbcErrorDiagRTE( thiswa->hStmtInsert, "SQLMoreResults", thiswa->sSql, res, __LINE__, __FILE__ );
                  
                  SQLCloseCursor(  thiswa->hStmtInsert );

            return (HB_FAILURE);
         }
      }
         //#endif
      }
      res = SQLFetch( thiswa->hStmtInsert );
      if ( CHECK_SQL_N_OK( res ) )
      {
         odbcErrorDiagRTE( thiswa->hStmtInsert, "ExecuteInsertStmt/Fetch", thiswa->sSql, res, __LINE__, __FILE__ );
         return (HB_FAILURE);
      }
      res = SQLGetData( thiswa->hStmtInsert, 1, SQL_C_ULONG, &(thiswa->recordList[0]), sizeof( SQL_C_ULONG ), NULL );
      if ( CHECK_SQL_N_OK( res ) )
      {
         odbcErrorDiagRTE( thiswa->hStmtInsert, "ExecuteInsertStmt/GetData", thiswa->sSql, res, __LINE__, __FILE__ );
         return (HB_FAILURE);
      }
      break;
   }
   case SYSTEMID_ORACLE:
   case SYSTEMID_MYSQL:
   {
      SQLRETURN res2;
      char ident[200]={0};
      char tablename[100]={0};

      if( thiswa->hStmtNextval == NULL )
      {
         switch ( thiswa->nSystemID )
         {
            case SYSTEMID_ORACLE:
            {
               sprintf( tablename, "%s", thiswa->szDataFileName );
               if( strlen( tablename ) > ( MAX_TABLE_NAME_LENGHT - 3 ) )
               {
                  tablename[MAX_TABLE_NAME_LENGHT-4] = '\0';
               }
               sprintf( ident, "SELECT %s%s_SQ.CURRVAL FROM DUAL", thiswa->sOwner, tablename );
               break;
            }
            case SYSTEMID_MYSQL:
            {
               sprintf( ident, "SELECT LAST_INSERT_ID()" );
               break;
            }
         }

         res2 = SQLAllocHandle( SQL_HANDLE_STMT, (HDBC) thiswa->hDbc, &(thiswa->hStmtNextval) );
         if ( CHECK_SQL_N_OK( res2 ) )
         {
            odbcErrorDiagRTE( thiswa->hStmtNextval, "SQLAllocStmt", ident, res2, __LINE__, __FILE__ );
            return HB_FAILURE;
         }

         res2 = SQLPrepare( thiswa->hStmtNextval, (SQLCHAR *) (ident), SQL_NTS );
         if ( CHECK_SQL_N_OK( res2 ) )
         {
            odbcErrorDiagRTE( thiswa->hStmtNextval, "SQLPrepare", ident, res2, __LINE__, __FILE__ );
            return (HB_FAILURE);
         }
      }
      else
      {
         ident[0] = '\0';
      }

      res2 = SQLExecute( thiswa->hStmtNextval );
      if ( CHECK_SQL_N_OK( res2 ) )
      {
         odbcErrorDiagRTE( thiswa->hStmtNextval, "SQLExecute", ident, res2, __LINE__, __FILE__ );
         return (HB_FAILURE);
      }
      res2 = SQLFetch( thiswa->hStmtNextval );
      if ( CHECK_SQL_N_OK( res2 ) )
      {
         odbcErrorDiagRTE( thiswa->hStmtNextval, "ExecuteInsertStmt/Fetch", ident, res2, __LINE__, __FILE__ );
         return (HB_FAILURE);
      }
      res2 = SQLGetData( thiswa->hStmtNextval, 1, SQL_C_ULONG, &(thiswa->recordList[0]), sizeof( SQL_C_ULONG ), NULL );
      if ( CHECK_SQL_N_OK( res2 ) )
      {
         odbcErrorDiagRTE( thiswa->hStmtNextval, "ExecuteInsertStmt/GetData", ident, res2, __LINE__, __FILE__ );
         return (HB_FAILURE);
      }
      SQLFreeStmt( thiswa->hStmtNextval, SQL_CLOSE );
      break;
   }
   case SYSTEMID_CACHE:
   case SYSTEMID_INFORM:
   default:
   ;
   }

   thiswa->deletedList[0] = ' ';
   thiswa->recordListPos  = 0;
   thiswa->recordListSize = 1;
   hb_arraySetNL( thiswa->aInfo, AINFO_RCOUNT, thiswa->recordList[0] );
   thiswa->lLastRec      = thiswa->recordList[0] + 1;

   SQLCloseCursor(  thiswa->hStmtInsert );
   
   return (HB_SUCCESS);
}

/*------------------------------------------------------------------------*/

HB_ERRCODE CreateUpdateStmt( SQLEXAREAP thiswa )
{
   SQLRETURN res;
   int iCols, i, iBind;
   COLUMNBINDP CurrRecord;
   PHB_ITEM pColumns;
   char * temp;

   if( ! thiswa->CurrRecord )
   {
      SetCurrRecordStructure( thiswa );
   }
   if( thiswa->hStmtUpdate )
   {
      SQLFreeStmt( thiswa->hStmtUpdate, SQL_DROP );
   }

   res = SQLAllocHandle( SQL_HANDLE_STMT, (HDBC) thiswa->hDbc, &(thiswa->hStmtUpdate) );
   if ( CHECK_SQL_N_OK( res ) )
   {
      odbcErrorDiagRTE( thiswa->hStmtUpdate, "CreateUpdateStmt", thiswa->sSql, res, __LINE__, __FILE__ );
   }

   iCols        = (int) hb_arrayLen( thiswa->aFields );
   CurrRecord   = thiswa->CurrRecord;
   iBind        = 0;
   thiswa->bIndexTouchedInUpdate = FALSE;
   if ( thiswa->sSql ) 
   memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   sprintf( thiswa->sSql, "UPDATE %s SET", thiswa->sTable );

   for( i = 0; i < iCols; i++ )
   {
      if( thiswa->editMask[ i ] || thiswa->specialMask[ i ] )
      {
         if ( !thiswa->specialMask[ i ] )
         {
            thiswa->updatedMask[ i ] = '1';
         }
         else if( thiswa->hOrdCurrent != 0 )
         {
            thiswa->bIndexTouchedInUpdate = TRUE;     // If there is any special column, we cannot be sure
                                                      // current order is not affected by UPDATE, so it takes
                                                      // worst scenario
         }
         if (strcmp(CurrRecord->colName,thiswa->sRecnoName )== 0)
            break;
         // Bind the query column
         iBind++;
         switch (CurrRecord->iCType)
         {
            case SQL_C_CHAR:
            {
               CurrRecord->lIndPtr = SQL_NTS;
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtUpdate, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) CurrRecord->iCType,
                                       (SQLSMALLINT  ) CurrRecord->iSQLType,
                                       (SQLULEN      ) CurrRecord->ColumnSize,
                                       (SQLSMALLINT  ) CurrRecord->DecimalDigits,
                                       (SQLPOINTER   ) CurrRecord->asChar.value, 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) &(CurrRecord->lIndPtr) );
               break;
            }
            case SQL_C_BINARY:
            {
               SQLINTEGER nInd;
               CurrRecord->lIndPtr = SQL_NTS;
               nInd = strlen((const char *)(CurrRecord->asChar.value));
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtUpdate, 
			                           (SQLUSMALLINT ) iBind,
                                       (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) SQL_C_CHAR,
                                       (SQLSMALLINT  ) SQL_LONGVARCHAR,
                                       (SQLULEN      ) CurrRecord->asChar.size_alloc,
                                       (SQLSMALLINT  ) 0,
                                       (SQLPOINTER   ) CurrRecord->asChar.value,
                                       (SQLLEN       ) nInd,
                                       (SQLLEN*      ) &(CurrRecord->lIndPtr));
               break;
            }
            case SQL_C_DOUBLE:
            {
               CurrRecord->lIndPtr = 0;
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtUpdate, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) CurrRecord->iCType,
                                       (SQLSMALLINT  ) CurrRecord->iSQLType,
                                       (SQLULEN      ) CurrRecord->ColumnSize,
                                       (SQLSMALLINT  ) CurrRecord->DecimalDigits,
                                       (SQLPOINTER   ) &(CurrRecord->asNumeric), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) &(CurrRecord->lIndPtr) );
               break;
            }
            case SQL_C_TYPE_TIMESTAMP:
            {
	           //DebugBreak();
               CurrRecord->lIndPtr = 0;
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtUpdate, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) SQL_C_TYPE_TIMESTAMP,
                                       (SQLSMALLINT  ) SQL_TYPE_TIMESTAMP,
                                       (SQLULEN      ) SQL_TIMESTAMP_LEN,
                                       (SQLSMALLINT  ) thiswa->nSystemID == SYSTEMID_MSSQL7 ||thiswa->nSystemID == SYSTEMID_AZURE ? 3 : 0 ,
                                       (SQLPOINTER   ) &(CurrRecord->asTimestamp), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) &(CurrRecord->lIndPtr) );
               break;
            }
            case SQL_C_TYPE_DATE:
            {
               CurrRecord->lIndPtr = 0;
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtUpdate, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) SQL_C_TYPE_DATE,
                                       (SQLSMALLINT  ) SQL_TYPE_DATE,
                                       (SQLULEN      ) SQL_DATE_LEN,
                                       (SQLSMALLINT  ) 0,
                                       (SQLPOINTER   ) &(CurrRecord->asDate), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) &(CurrRecord->lIndPtr) );
               break;
            }
            case SQL_C_BIT:
            {
               res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtUpdate, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) CurrRecord->iCType,
                                       (SQLSMALLINT  ) CurrRecord->iSQLType,
                                       (SQLULEN      ) CurrRecord->ColumnSize,
                                       (SQLSMALLINT  ) CurrRecord->DecimalDigits,
                                       (SQLPOINTER   ) &(CurrRecord->asLogical), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) NULL );
               break;
            }
         }

         CurrRecord->iParNum = iBind;

         // Create SQL
         temp = hb_strdup( (const char *) thiswa->sSql );
         sprintf( thiswa->sSql, "%s%c %c%s%c = ?", temp, iBind > 1 ? ',' : ' ',
                                                   OPEN_QUALIFIER( thiswa ), CurrRecord->colName,
                                                   CLOSE_QUALIFIER( thiswa ) );
         hb_xfree( temp );

         if ( CHECK_SQL_N_OK( res ) )
         {
            odbcErrorDiagRTE( thiswa->hStmtUpdate, "BindUpdateColumns", thiswa->sSql, res, __LINE__, __FILE__ );
            return HB_FAILURE;
         }
      }
      CurrRecord++;
   }
   temp = hb_strdup( (const char *) thiswa->sSql );
   sprintf( thiswa->sSql, "%s\n WHERE %c%s%c = ?", temp, OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ) );
   hb_xfree( temp );
   res = SQLBindParameter( (SQLHSTMT     ) thiswa->hStmtUpdate, 
                           (SQLUSMALLINT ) ++iBind, 
						   (SQLSMALLINT  ) SQL_PARAM_INPUT, 
						   (SQLSMALLINT  ) SQL_C_ULONG, 
						   (SQLSMALLINT  ) SQL_INTEGER, 
						   (SQLULEN      ) 15, 
						   (SQLSMALLINT  ) 0, 
						   (SQLPOINTER   ) &(thiswa->lUpdatedRecord), 
						   (SQLLEN       ) 0, 
						   (SQLLEN*      ) NULL );
   if ( CHECK_SQL_N_OK( res ) )
   {
      odbcErrorDiagRTE( thiswa->hStmtUpdate, "BindUpdateColumns", thiswa->sSql, res, __LINE__, __FILE__ );
      return HB_FAILURE;
   }

   res = SQLPrepare( thiswa->hStmtUpdate, (SQLCHAR *) (thiswa->sSql), SQL_NTS );
   if ( CHECK_SQL_N_OK( res ) )
   {
      odbcErrorDiagRTE( thiswa->hStmtUpdate, "CreateUpdateStmt", thiswa->sSql, res, __LINE__, __FILE__ );
      return HB_FAILURE;
   }

   if( (!thiswa->bIndexTouchedInUpdate) && thiswa->hOrdCurrent )
   {
      // Check if any updated column is included in current index column list
      pColumns = hb_arrayGetItemPtr( hb_arrayGetItemPtr( thiswa->aOrders, ( ULONG ) thiswa->hOrdCurrent ), INDEX_FIELDS );
      thiswa->indexColumns = hb_arrayLen( pColumns );

      for( i = 1; i <= thiswa->indexColumns; i++ )
      {
         if( thiswa->editMask[ hb_arrayGetNL( hb_arrayGetItemPtr( pColumns, i ), 2 ) -1 ] )
            thiswa->bIndexTouchedInUpdate = TRUE;
      }
   }

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE ExecuteUpdateStmt( SQLEXAREAP thiswa )
{
   PHB_ITEM pKey, aRecord;
   HB_SIZE lPos;
   SQLRETURN res;

   // Feed current record to bindings

   thiswa->lUpdatedRecord = GetCurrentRecordNum( thiswa );

   if( FeedRecordCols( thiswa, TRUE ) == HB_FAILURE )  // Stmt created and prepared, only need to push data
      return (HB_FAILURE);

   // Execute statement

   res = SQLExecute( thiswa->hStmtUpdate );

   if ( res == SQL_ERROR )
   {
      odbcErrorDiagRTE( thiswa->hStmtUpdate, "ExecuteUpdateStmt", thiswa->sSql, res, __LINE__, __FILE__ );
      SQLCloseCursor( thiswa->hStmtUpdate );
      thiswa->hStmtUpdate = NULL; 
      return (HB_FAILURE);
   }

   // If any Index column was touched, SKIP buffer is not valid anymore

   if( thiswa->bIndexTouchedInUpdate )
   {
      thiswa->recordList[0]  = thiswa->recordList[thiswa->recordListPos];
      thiswa->recordListPos  = 0;
      thiswa->recordListSize = 1;
   }

   // Update Buffer Pool if needed

   pKey    = hb_itemNew( NULL );
   hb_itemPutNL( pKey, thiswa->recordList[thiswa->recordListPos] );

   if ( hb_hashScan( thiswa->hBufferPool, pKey, &lPos  ) )
   {
      aRecord = hb_hashGetValueAt( thiswa->hBufferPool, lPos );
      hb_arrayCopy( thiswa->aBuffer, aRecord, NULL, NULL, NULL );
   }
   hb_itemRelease( pKey );
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

