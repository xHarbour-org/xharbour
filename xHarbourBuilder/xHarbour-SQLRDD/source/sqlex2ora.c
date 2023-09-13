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

// #if defined(HB_OS_WIN_32) || defined(HB_OS_WIN_64) || defined( HB_OS_WIN ) 
//    #include <windows.h>
//    #include <odbcinst.h>
// #else
//    #include <stdlib.h>
//    #include <unistd.h>
//    #include <errno.h>
//    #include <sys/types.h>
//    #include <sys/wait.h>
//    #define SQL_WCHAR  (-8)
//    #define SQL_WLONGVARCHAR  (-10)
//    #define SQL_C_WCHAR  SQL_WCHAR
// #endif
// 
// #include <sql.h>
// #include <sqlext.h>
// #include <sqltypes.h>
#include "ocilib.h"
#include "sqlexora.h"

/*------------------------------------------------------------------------*/

static  PHB_DYNS s_pSym_Serial1 = NULL;   /* Pointer to serialization function */

#define LOGFILE "oci2.log"
/*------------------------------------------------------------------------*/


/*------------------------------------------------------------------------*/

char * QualifyName2( char * szName, SQLEXORAAREAP thiswa )
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
      case SYSTEMID_OTERRO:
      case SYSTEMID_INFORM:
         szName[i] = ( char ) HB_TOLOWER( ( unsigned char ) szName[i] );
         break;
      }
   }
   return szName;
}

/*------------------------------------------------------------------------*/

static void ResolveSpecialCols( SQLEXORAAREAP thiswa )
{
   // Resolve all Synthetic Index and FOR clause expressions, storing
   // results in thiswa->sqlarea.aBuffer
   // TO DO: Creating a new Index should reset INSERT Stmt cos it may
   //        create a new field like INDKEY_???

   int i, iIndexes;
   PHB_ITEM pIndex;
   PHB_ITEM pKeyVal;
   PHB_ITEM pIndIt;
   USHORT uiPos;
   
   if( !thiswa->pIndexMgmnt )
   {
      hb_objSendMsg( thiswa->sqlarea.oWorkArea, "AINDEXMGMNT", 0 );
      thiswa->pIndexMgmnt  = hb_itemNew( NULL ) ;      
      hb_itemForwardValue( thiswa->pIndexMgmnt, hb_stackReturnItem()  );            
   }

   iIndexes    = hb_arrayLen( thiswa->pIndexMgmnt );

   for( i=1; i <= iIndexes; i++ )
   {
      pIndex = hb_arrayGetItemPtr( thiswa->pIndexMgmnt, i );
          pIndIt = hb_itemArrayGet( pIndex, INDEXMAN_COLUMNS );
         //pIndIt = hb_arrayGetItemPtr( pIndex, INDEXMAN_COLUMNS );
         

      if( !SR_itemEmpty2( pIndIt ) )
      {
         EVALINFO info;
         hb_evalNew( &info, hb_itemArrayGet( pIndex, INDEXMAN_KEY_CODEBLOCK ) );
         pKeyVal = hb_evalLaunch( &info );
         hb_evalRelease( &info );

         // Get field position in ::aLocalBuffer
         //uiPos = (USHORT) hb_itemGetNI( hb_arrayGetItemPtr( pIndex, INDEXMAN_SYNTH_COLPOS ) );
         uiPos = (USHORT) hb_itemGetNI( hb_itemArrayGet( pIndex, INDEXMAN_SYNTH_COLPOS ) );
         thiswa->specialMask[ uiPos ] = '1';
         hb_arraySetForward( thiswa->sqlarea.aBuffer, uiPos, pKeyVal );
         hb_itemRelease( pKeyVal );
      }

      //pIndIt = hb_arrayGetItemPtr( pIndex, INDEXMAN_FOR_CODEBLOCK );
      pIndIt = hb_itemArrayGet( pIndex, INDEXMAN_FOR_CODEBLOCK );

      if( !SR_itemEmpty2( pIndIt ) )
      {
         EVALINFO info;
         hb_evalNew( &info, hb_itemArrayGet( pIndex, INDEXMAN_FOR_CODEBLOCK ) );
         pKeyVal = hb_evalLaunch( &info );
         hb_evalRelease( &info );

         // Get field position in ::aLocalBuffer
         //uiPos = (USHORT) hb_itemGetNI( hb_arrayGetItemPtr( pIndex, INDEXMAN_FOR_COLPOS ) );
         uiPos = (USHORT) hb_itemGetNI( hb_itemArrayGet( pIndex, INDEXMAN_FOR_COLPOS ) );
         thiswa->specialMask[ uiPos ] = '1';
         hb_arraySetForward( thiswa->sqlarea.aBuffer, uiPos, pKeyVal );
         hb_itemRelease( pKeyVal );
      }
   }
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

void SetInsertRecordStructureOra( SQLEXORAAREAP thiswa )
{
   thiswa->InsertRecord = (COLUMNBINDORAP) hb_xgrabz( hb_arrayLen( thiswa->aFields ) * sizeof( COLUMNBINDORA ) );
   //memset( thiswa->InsertRecord, 0, hb_arrayLen( thiswa->aFields ) * sizeof( COLUMNBINDORA ) );
}

/*------------------------------------------------------------------------*/

void CreateInsertStmtOra( SQLEXORAAREAP thiswa )
{
   int iCols, i;
   PHB_ITEM pFieldStruct, pFieldLen, pFieldDec;
   LONG lFieldPosWA, lType;
   char * colName, * sFields, * sParams, * temp, * temp1;
   char ident[200] = {0};

   char declare[200] = {0};
   char cType;
   BOOL bNullable, bMultiLang, bIsMemo;
   COLUMNBINDORAP InsertRecord;
//    USHORT uiPos;

   iCols    = hb_arrayLen( thiswa->aFields );
   

   if( ! thiswa->InsertRecord )
   {
      SetInsertRecordStructureOra( thiswa );
   }

   InsertRecord = thiswa->InsertRecord;
   sFields      = (char *) hb_xgrabz( FIELD_LIST_SIZE * sizeof( char ) );
   sParams      = (char *) hb_xgrabz( (FIELD_LIST_SIZE * 2) * sizeof( char ) );

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
      bIsMemo      = cType == 'M';

  
      if( i != (int)(thiswa->sqlarea.ulhRecno) )      // RECNO is never included in INSERT column list
      {
         temp = hb_strdup( (const char *) sFields );
         temp1 = hb_strdup( (const char *) sParams );
         sprintf( sFields, "%s,%c%s%c", temp, OPEN_QUALIFIER( thiswa ), QualifyName2( colName, thiswa ), CLOSE_QUALIFIER( thiswa ) );
         sprintf( sParams,"%s, :%s ",temp1,colName);
//          sParams[uiPos]   = ',';
//          sParams[++uiPos] = ':';
//          sParams[++uiPos] = '\0';
         hb_xfree(  temp );
         hb_xfree(  temp1 );
//          sprintf(InsertRecord->szBindName,":%i",i);
            sprintf(InsertRecord->szBindName,":%s",colName);
      }

      hb_xfree(  colName );
       
      InsertRecord->iSQLType        = (int)lType;
      InsertRecord->isNullable      = bNullable;
      InsertRecord->isBoundNULL     = FALSE;
      InsertRecord->lFieldPosDB     = i;
      InsertRecord->lFieldPosWA     = lFieldPosWA;
	  InsertRecord->ColumnSize      = (unsigned int) hb_itemGetNI( pFieldLen );
	  InsertRecord->DecimalDigits   = (unsigned short) hb_itemGetNI( pFieldDec );
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
            InsertRecord->asChar.value      = (char *) hb_xgrab( INITIAL_MEMO_ALLOC );
            InsertRecord->asChar.size_alloc = INITIAL_MEMO_ALLOC;
            InsertRecord->asChar.size       = 0;
            InsertRecord->asChar.value[0]   = '\0';
            InsertRecord->ColumnSize        = 0;            
            break;
         }
         case 'C':
         {
            InsertRecord->asChar.value      = (char *) hb_xgrab( InsertRecord->ColumnSize + 1 );
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

            InsertRecord->iCType          = SQL_C_TYPE_DATE;        // May be DATE or TIMESTAMP
            break;
         }
         case 'T':
         {
	         
            InsertRecord->iCType          = SQL_C_TYPE_TIMESTAMP;        // May be DATE or TIMESTAMP
            break;
         }         
         case 'L':
         {
            InsertRecord->iCType          = SQL_C_BIT;
            break;
         }
      }
      InsertRecord++;
   }

   sParams[0] = ' ';
   sFields[0] = ' ';

   switch ( thiswa->nSystemID )
   {

   case SYSTEMID_ORACLE:
   {
      ident[0] = '\0';
      break;
   }
   default:
      ident[0] = '\0';
   }
   if ( thiswa->sSql ) 
   memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   if (thiswa->nSystemID ==  SYSTEMID_MSSQL7 )
   {
		 sprintf( thiswa->sSql, "%s INSERT INTO %s (%s ) OUTPUT Inserted.%s INTO @InsertedData VALUES (%s );%s", declare, thiswa->sTable, sFields, thiswa->sRecnoName, sParams ,ident);
   }   
   else
   {
   sprintf( thiswa->sSql, "INSERT INTO %s (%s ) VALUES (%s )%s", thiswa->sTable, sFields, sParams, ident );
   }   

      
   hb_xfree(  sFields );
   hb_xfree(  sParams );
}

/*------------------------------------------------------------------------*/

HB_ERRCODE PrepareInsertStmtOra( SQLEXORAAREAP thiswa )
{


//    res = SQLAllocHandle( SQL_HANDLE_STMT, (HDBC) thiswa->hDbc, &(thiswa->hStmtInsert) );
   thiswa->hStmtInsert =  OCI_StatementCreate(GetConnection(thiswa->hDbc));

   if ( thiswa->hStmtInsert == NULL )
   {
      OraErrorDiagRTE( thiswa->hStmtInsert, "PrepareInsertStmtOra/SQLAllocStmt", thiswa->sSql, 0, __LINE__, __FILE__ );
      return HB_FAILURE;
   }
   OCI_AllowRebinding(thiswa->hStmtInsert,1);
//    res = SQLPrepare( thiswa->hStmtInsert, (char *) (thiswa->sSql), SQL_NTS );

   if ( !OCI_Prepare( thiswa->hStmtInsert, (thiswa->sSql) ) ) //    if ( CHECK_SQL_N_OK( res ) )
   {
      OraErrorDiagRTE( thiswa->hStmtInsert, "PrepareInsertStmtOra", thiswa->sSql, 0, __LINE__, __FILE__ );
      return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE BindInsertColumnsOra( SQLEXORAAREAP thiswa )
{
   int iCol, iCols, iBind;
   COLUMNBINDORAP InsertRecord;
   int res = SQL_ERROR;

   iCols        = hb_arrayLen( thiswa->aFields );
   InsertRecord = thiswa->InsertRecord;
   iBind        = 0;

   for (iCol = 1; iCol <= iCols; iCol++)
   {
      if( iCol != (int)(thiswa->sqlarea.ulhRecno) )                // RECNO is never included in INSERT column list
      {
         iBind++;
         switch (InsertRecord->iCType)
         {
            case SQL_C_CHAR:
            {
//                InsertRecord->lIndPtr = SQL_NTS;

//                res = SQLBindParameter( thiswa->hStmtInsert, iBind, SQL_PARAM_INPUT,
//                                        InsertRecord->iCType,
//                                        InsertRecord->iSQLType,
//                                        InsertRecord->ColumnSize,
//                                        InsertRecord->DecimalDigits,
//                                        InsertRecord->asChar.value, 0, &(InsertRecord->lIndPtr) );
                res =OCI_BindString(thiswa->hStmtInsert,InsertRecord->szBindName,InsertRecord->asChar.value, InsertRecord->ColumnSize  ) ;

               break;
            }
            case SQL_C_BINARY:
            {
//                SQLINTEGER nInd;
//                InsertRecord->lIndPtr = SQL_NTS;
//                nInd = strlen((const char *)(InsertRecord->asChar.value));
//                res = SQLBindParameter( thiswa->hStmtInsert, iBind,
//                                        SQL_PARAM_INPUT,
//                                        SQL_C_CHAR,
//                                        SQL_LONGVARCHAR,
//                                        InsertRecord->asChar.size_alloc,
//                                        0,
//                                        InsertRecord->asChar.value,
//                                        nInd,
//                                        &(InsertRecord->lIndPtr));
               InsertRecord->lob1= OCI_LobCreate(GetConnection(thiswa->hDbc), OCI_CLOB);              
               OCI_BindLob(thiswa->hStmtInsert, InsertRecord->szBindName,InsertRecord->lob1); 

               break;
            }
            case SQL_C_NUMERIC:
            {
	            res = OCI_BindUnsignedBigInt(thiswa->hStmtInsert, InsertRecord->szBindName, &InsertRecord->asNumeric) ;
	            break;
            } 
            case SQL_C_DOUBLE:
            {
               InsertRecord->lIndPtr = 0;
//                res = SQLBindParameter( thiswa->hStmtInsert, iBind, SQL_PARAM_INPUT,
//                                        InsertRecord->iCType,
//                                        InsertRecord->iSQLType,
//                                        InsertRecord->ColumnSize,
//                                        InsertRecord->DecimalDigits,
//                                        &(InsertRecord->asNumeric), 0, &(InsertRecord->lIndPtr) );
            res = OCI_BindDouble(thiswa->hStmtInsert, InsertRecord->szBindName, &InsertRecord->asDouble) ;
               break;
            }
            case SQL_C_TYPE_TIMESTAMP: 
            {
               
               InsertRecord->lIndPtr = 0;
//                res = SQLBindParameter( thiswa->hStmtInsert, iBind, SQL_PARAM_INPUT,
//                                        SQL_C_TYPE_TIMESTAMP,
//                                        SQL_TYPE_TIMESTAMP,
//                                        SQL_TIMESTAMP_LEN,
//                                        0,
//                                        &(InsertRecord->asTimestamp), 0, &(InsertRecord->lIndPtr) );
               InsertRecord->asDate2 = OCI_DateCreate(GetConnection(thiswa->hDbc));
//                OCI_DateSetDateTime(InsertRecord->asDate2,InsertRecord->asTimestamp.year, InsertRecord->asTimestamp.month, InsertRecord->asTimestamp.day,InsertRecord->asTimestamp.hour,InsertRecord->asTimestamp.minute,InsertRecord->asTimestamp.second) ;
               res=OCI_BindDate(thiswa->hStmtInsert, InsertRecord->szBindName, InsertRecord->asDate2);
               break;
            }
            case SQL_C_TYPE_DATE:
            {
               InsertRecord->lIndPtr = 0;
//                res = SQLBindParameter( thiswa->hStmtInsert, iBind, SQL_PARAM_INPUT,
//                                        SQL_C_TYPE_DATE,
//                                        SQL_TYPE_DATE,
//                                        SQL_DATE_LEN,
//                                        0,
//                                        &(InsertRecord->asDate), 0, &(InsertRecord->lIndPtr) );
               InsertRecord->asDate1 = OCI_DateCreate(GetConnection(thiswa->hDbc));

               res=OCI_BindDate(thiswa->hStmtInsert, InsertRecord->szBindName, InsertRecord->asDate1);
               break;
            }
            case SQL_C_BIT:
            {
//                res = SQLBindParameter( thiswa->hStmtInsert, iBind, SQL_PARAM_INPUT,
//                                        InsertRecord->iCType,
//                                        InsertRecord->iSQLType,
//                                        InsertRecord->ColumnSize,
//                                        InsertRecord->DecimalDigits,
//                                        &(InsertRecord->asLogical), 0, NULL );
               res =  OCI_BindUnsignedBigInt( thiswa->hStmtInsert,InsertRecord->szBindName,&InsertRecord->asLogical)  ;  
               break;
            }
         }
         if (InsertRecord->lIndPtr == (unsigned int )SQL_NULL_DATA)
            OCI_BindSetNull(  OCI_GetBind(thiswa->hStmtInsert, iCol ) );
         InsertRecord->iParNum = iBind;

         if ( !res  )
         {
            OraErrorDiagRTE( thiswa->hStmtInsert, "BindInsertColumnsOra", thiswa->sSql, 0, __LINE__, __FILE__ );
            return HB_FAILURE;
         }
      }
      InsertRecord++;
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE FeedRecordColsOra( SQLEXORAAREAP thiswa, BOOL bUpdate )
{
   int iCols, i;
   PHB_ITEM pFieldData, pTemp;
   COLUMNBINDORAP InsertRecord;

   iCols    = hb_arrayLen( thiswa->aFields );

   if( bUpdate )
   {
      InsertRecord = thiswa->CurrRecord;
   }
   else
   {
      InsertRecord = thiswa->InsertRecord;
   }

   ResolveSpecialCols( thiswa );    // Fix INDKEY and FOR CLAUSE columns

   for( i=1; i <= iCols; i++ )
   {
      if( (!bUpdate) || (bUpdate && (thiswa->editMask[ i-1 ] || thiswa->specialMask[ i-1 ]) ) )
      {
         if( i == (int)(thiswa->sqlarea.ulhDeleted) )
         {
            SetBindEmptylValue2( InsertRecord );     // Writes a ' ' to deleted flag
         }
         else if( i != (int)(thiswa->sqlarea.ulhRecno) )                // RECNO is never included in INSERT column list
         {
            // Get item value from Workarea
            pFieldData   = hb_arrayGetItemPtr( thiswa->sqlarea.aBuffer, i );

            if( SR_itemEmpty2( pFieldData ) && (!InsertRecord->isNullable) )
            {
               if( SetBindEmptylValue2( InsertRecord ) == HB_FAILURE )
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

               if( SetBindValue2( pFieldData, InsertRecord, bUpdate ? thiswa->hStmtUpdate : thiswa->hStmtInsert ) == HB_FAILURE )
                  return HB_FAILURE;
            }
         }
      }
      InsertRecord++;
   }

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE ExecuteInsertStmtOra( SQLEXORAAREAP thiswa )
{
   int res;
   int iCols, i;
   OCI_Resultset  *rs     ;
   COLUMNBINDORAP InsertRecord;


   InsertRecord = thiswa->InsertRecord;   
   iCols    = hb_arrayLen( thiswa->aFields );
   for( i=1; i <= iCols; i++ )
   {
      if ( InsertRecord->iCType == SQL_C_BINARY )
      {

// 	      TraceLog("ccc.log" , "escrevendo lob  InsertRecord->asChar.value %s InsertRecord->asChar.size %lu \n " ,InsertRecord->asChar.value,InsertRecord->asChar.size);
	      res = OCI_LobSeek(InsertRecord->lob1, 0, OCI_SEEK_SET); 
	      res =OCI_LobWrite(InsertRecord->lob1, (void*)InsertRecord->asChar.value,  InsertRecord->asChar.size  );
      }
      if (InsertRecord->lIndPtr     == SQL_NULL_DATA )
      {
            OCI_BindSetNull(  OCI_GetBind(thiswa->hStmtInsert, i ) );	      
      }
	  InsertRecord++;
   }
   
//    res = SQLExecute( thiswa->hStmtInsert );
   res = OCI_Execute(thiswa->hStmtInsert );                                                         

   if (!res  )
   {
      OraErrorDiagRTE( thiswa->hStmtInsert, "ExecuteInsertStmtOra/SQLExecute", thiswa->sSql, res, __LINE__, __FILE__ );
//       OCI_StatementFree( thiswa->hStmtInsert );
      return (HB_FAILURE);
   }
   
   // manda os blobs
   
   
   

   // Retrieve RECNO

   switch ( thiswa->nSystemID )
   {
   case SYSTEMID_ORACLE:
   {
      int res2;
      char ident[200]={0};
      char tablename[100]={0};

      if( thiswa->hStmtNextval == NULL )
      {
         switch ( thiswa->nSystemID )
         {
            case SYSTEMID_ORACLE:
            {
               sprintf( tablename, "%s", thiswa->sqlarea.szDataFileName );
               if( strlen( tablename ) > ( MAX_TABLE_NAME_LENGHT - 3 ) )
               {
                  tablename[MAX_TABLE_NAME_LENGHT-4] = '\0';
               }
               sprintf( ident, "SELECT %s%s_SQ.CURRVAL FROM DUAL", thiswa->sOwner, tablename );
               break;
            }

         }

//          res = SQLAllocHandle( SQL_HANDLE_STMT, (HDBC) thiswa->hDbc, &(thiswa->hStmtNextval) );
         
         thiswa->hStmtNextval= OCI_StatementCreate(GetConnection(thiswa->hDbc));
         if ( thiswa->hStmtNextval == NULL )
         {
            OraErrorDiagRTE( thiswa->hStmtNextval, "SQLAllocStmt", ident, 0, __LINE__, __FILE__ );
            return HB_FAILURE;
         }

         OCI_AllowRebinding(thiswa->hStmtNextval,1);
         res2 = OCI_Prepare(  thiswa->hStmtNextval,  (ident));
         if ( !res2 )
         {
            OraErrorDiagRTE( thiswa->hStmtNextval, "SQLPrepare", ident, res2, __LINE__, __FILE__ );
            return (HB_FAILURE);
         }
      }
      else
      {
         ident[0] = '\0';
      }

//       res = SQLExecute( thiswa->hStmtNextval );
      res2 = OCI_Execute(thiswa->hStmtNextval );                                                         
      if ( !res2)
      {
         OraErrorDiagRTE( thiswa->hStmtNextval, "SQLExecute", ident, res2, __LINE__, __FILE__ );
         return (HB_FAILURE);
      }
//       res = SQLFetch( thiswa->hStmtNextval );
//       if ( CHECK_SQL_N_OK( res ) )

      rs = OCI_GetResultset( thiswa->hStmtNextval);
      if ( rs ==  NULL )
      {
         OraErrorDiagRTE( thiswa->hStmtNextval, "ExecuteInsertStmtOra/Fetch", ident, res2, __LINE__, __FILE__ );
//          thiswa->hStmtNextval=NULL;
         return (HB_FAILURE);
      }

      res2 = OCI_FetchNext(rs);

      thiswa->recordList[0]= OCI_GetUnsignedBigInt( rs,1 ) ;

      if (thiswa->recordList[0] == 0)
      {
         OraErrorDiagRTE( thiswa->hStmtNextval, "ExecuteInsertStmtOra/GetData", ident, res2, __LINE__, __FILE__ );

         return (HB_FAILURE);
      }

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
   hb_arraySetNLL( thiswa->sqlarea.aInfo, AINFO_RCOUNT, thiswa->recordList[0] );
   thiswa->lLastRec      = thiswa->recordList[0] + 1;

   
   return (HB_SUCCESS);
}

/*------------------------------------------------------------------------*/

HB_ERRCODE CreateUpdateStmtOra( SQLEXORAAREAP thiswa )
{
   int res;
   int iCols, i, iBind;
   COLUMNBINDORAP CurrRecord;
   PHB_ITEM pColumns;
   char * temp;
   char szBindName[10]={0};

   if( ! thiswa->CurrRecord )
   {
      SetCurrRecordStructureOra( thiswa );
   }
   if( thiswa->hStmtUpdate )
   {
	    
       OCI_StatementFree( thiswa->hStmtUpdate);
   }

//    res = SQLAllocHandle( SQL_HANDLE_STMT, (HDBC) thiswa->hDbc, &(thiswa->hStmtUpdate) );
   thiswa->hStmtUpdate =  OCI_StatementCreate(GetConnection(thiswa->hDbc));
   if ( thiswa->hStmtUpdate == NULL )
   {
      OraErrorDiagRTE( thiswa->hStmtUpdate, "CreateUpdateStmtOra", thiswa->sSql, 0, __LINE__, __FILE__ );
   }
   
   iCols        = (int) hb_arrayLen( thiswa->aFields );
   CurrRecord   = thiswa->CurrRecord;
   iBind        = 0;
   thiswa->bIndexTouchedInUpdate = FALSE;
   if ( thiswa->sSql ) 
   memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   sprintf( thiswa->sSql, "UPDATE %s SET", thiswa->sTable );

   
   //Cria o Sql
      for( i = 0; i < iCols; i++ )
   {
      if( thiswa->editMask[ i ] || thiswa->specialMask[ i ] )
      {
         if ( !thiswa->specialMask[ i ] )
         {
            thiswa->updatedMask[ i ] = '1';
         }
         else if( thiswa->sqlarea.hOrdCurrent != 0 )
         {
            thiswa->bIndexTouchedInUpdate = TRUE;     // If there is any special column, we cannot be sure
                                                      // current order is not affected by UPDATE, so it takes
                                                      // worst scenario
         }
         // Bind the query column
iBind++;
         // Create SQL
         temp = hb_strdup( (const char *) thiswa->sSql );
         sprintf( thiswa->sSql, "%s%c %c%s%c = :%s", temp, iBind > 1 ? ',' : ' ',
                                                   OPEN_QUALIFIER( thiswa ), CurrRecord->colName,
                                                   CLOSE_QUALIFIER( thiswa ),CurrRecord->colName );
         hb_xfree(  temp );

      }
      CurrRecord++;
   }
   temp = hb_strdup( (const char *) thiswa->sSql );
     sprintf(szBindName,":%s",thiswa->sRecnoName );
   sprintf( thiswa->sSql, "%s\n WHERE %c%s%c = %s", temp, OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),szBindName); 
   hb_xfree(  temp );
//TraceLog("aaa.log" , "query update %s\n",(thiswa->sSql));
   res = OCI_Prepare( thiswa->hStmtUpdate, (char *) (thiswa->sSql));
   if ( !res )
   {
      OraErrorDiagRTE( thiswa->hStmtUpdate, "CreateUpdateStmtOra", thiswa->sSql, res, __LINE__, __FILE__ );
      return HB_FAILURE;
   }
   
   //Binda os Valores
   //
   
   CurrRecord   = thiswa->CurrRecord;   
   iBind=0;
   for( i = 0; i < iCols; i++ )
   {
      if( thiswa->editMask[ i ] || thiswa->specialMask[ i ] )
      {
         if ( !thiswa->specialMask[ i ] )
         {
            thiswa->updatedMask[ i ] = '1';
         }
         else if( thiswa->sqlarea.hOrdCurrent != 0 )
         {
            thiswa->bIndexTouchedInUpdate = TRUE;     // If there is any special column, we cannot be sure
                                                      // current order is not affected by UPDATE, so it takes
                                                      // worst scenario
         }
         // Bind the query column
         iBind++;
         switch (CurrRecord->iCType)
         {
            case SQL_C_CHAR:
            {
	           sprintf(szBindName,":%i",iBind ); 
//                CurrRecord->lIndPtr = SQL_NTS;
//                res = SQLBindParameter( thiswa->hStmtUpdate, iBind, SQL_PARAM_INPUT,
//                                        CurrRecord->iCType,
//                                        CurrRecord->iSQLType,
//                                        CurrRecord->ColumnSize,
//                                        CurrRecord->DecimalDigits,
//                                        CurrRecord->asChar.value, 0, &(CurrRecord->lIndPtr) );
              res =OCI_BindString(thiswa->hStmtUpdate, CurrRecord->szBindName, CurrRecord->asChar.value, CurrRecord->ColumnSize) ;
               break;
            }
            case SQL_C_BINARY:
            {
//                SQLINTEGER nInd;
//                sprintf(szBindName,":%i",iBind ); 
//                CurrRecord->lIndPtr = SQL_NTS;
//                nInd = strlen((const char *)(CurrRecord->asChar.value));
//                res = SQLBindParameter( thiswa->hStmtUpdate, iBind,
//                                        SQL_PARAM_INPUT,
//                                        SQL_C_CHAR,
//                                        SQL_LONGVARCHAR,
//                                        CurrRecord->asChar.size_alloc,
//                                        0,
//                                        CurrRecord->asChar.value,
//                                        nInd,
//                                        &(CurrRecord->lIndPtr));
               break;
            }
            case SQL_C_NUMERIC:
            {
	            sprintf(szBindName,":%i",iBind ); 
	            res = OCI_BindUnsignedBigInt(thiswa->hStmtUpdate, CurrRecord->szBindName, &CurrRecord->asNumeric) ;
	            break;
            }                
            case SQL_C_DOUBLE:
            {
	            sprintf(szBindName,":%i",iBind ); 
               CurrRecord->lIndPtr = 0;
//                res = SQLBindParameter( thiswa->hStmtUpdate, iBind, SQL_PARAM_INPUT,
//                                        CurrRecord->iCType,
//                                        CurrRecord->iSQLType,
//                                        CurrRecord->ColumnSize,
//                                        CurrRecord->DecimalDigits,
//                                        &(CurrRecord->asNumeric), 0, &(CurrRecord->lIndPtr) );
               res = OCI_BindDouble(thiswa->hStmtUpdate, CurrRecord->szBindName, &CurrRecord->asDouble) ;
               break;
            }
            case SQL_C_TYPE_TIMESTAMP:
            {
	           
	           sprintf(szBindName,":%i",iBind ); 
//                CurrRecord->lIndPtr = 0;
//                res = SQLBindParameter( thiswa->hStmtUpdate, iBind, SQL_PARAM_INPUT,
//                                        SQL_C_TYPE_TIMESTAMP,
//                                        SQL_TYPE_TIMESTAMP,
//                                        SQL_TIMESTAMP_LEN,
//                                        0,
//                                        &(CurrRecord->asTimestamp), 0, &(CurrRecord->lIndPtr) );
               CurrRecord->asDate2 = OCI_DateCreate(GetConnection(thiswa->hDbc));
               OCI_DateSetDateTime(CurrRecord->asDate2,CurrRecord->asTimestamp.year, CurrRecord->asTimestamp.month, CurrRecord->asTimestamp.day,CurrRecord->asTimestamp.hour,CurrRecord->asTimestamp.minute,CurrRecord->asTimestamp.second) ;
               res=OCI_BindDate(thiswa->hStmtUpdate, CurrRecord->szBindName, CurrRecord->asDate2);
               break;
            }
            case SQL_C_TYPE_DATE:
            {
	           sprintf(szBindName,":%i",iBind ); 
               CurrRecord->lIndPtr = 0;
//                res = SQLBindParameter( thiswa->hStmtUpdate, iBind, SQL_PARAM_INPUT,
//                                        SQL_C_TYPE_DATE,
//                                        SQL_TYPE_DATE,
//                                        SQL_DATE_LEN,
//                                        0,
//                                        &(CurrRecord->asDate), 0, &(CurrRecord->lIndPtr) );
               CurrRecord->asDate1 = OCI_DateCreate(GetConnection(thiswa->hDbc));
               OCI_DateSetDate(CurrRecord->asDate1,CurrRecord->asDate.year, CurrRecord->asDate.month, CurrRecord->asDate.day) ;
               res=OCI_BindDate(thiswa->hStmtUpdate, CurrRecord->szBindName, CurrRecord->asDate1);
               break;
            }
            case SQL_C_BIT:
            {
	            sprintf(szBindName,":%i",iBind ); 
//                res = SQLBindParameter( thiswa->hStmtUpdate, iBind, SQL_PARAM_INPUT,
//                                        CurrRecord->iCType,
//                                        CurrRecord->iSQLType,
//                                        CurrRecord->ColumnSize,
//                                        CurrRecord->DecimalDigits,
//                                        &(CurrRecord->asLogical), 0, NULL );
               res =  OCI_BindUnsignedBigInt( thiswa->hStmtUpdate,CurrRecord->szBindName,&CurrRecord->asLogical)  ;  
               break;
            }
         }

         CurrRecord->iParNum = iBind;



         if (!res )
         {
            OraErrorDiagRTE( thiswa->hStmtUpdate, "BindUpdateColumns", thiswa->sSql, res, __LINE__, __FILE__ );
            return HB_FAILURE;
         }
      }
      CurrRecord++;
   }
   
//    temp = hb_strdup( (const char *) thiswa->sSql );
//     sprintf(szBindName,":%s",thiswa->sRecnoName );
//    sprintf( thiswa->sSql, "%s\n WHERE %c%s%c = %s", temp, OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),szBindName);
//    hb_xfree(  temp );
//    res = SQLBindParameter( thiswa->hStmtUpdate, ++iBind, SQL_PARAM_INPUT, SQL_C_ULONG, SQL_INTEGER, 15, 0, &(thiswa->lUpdatedRecord), 0, NULL );
   res = OCI_BindUnsignedBigInt( thiswa->hStmtUpdate, thiswa->sRecnoName, &thiswa->lUpdatedRecord) ; 
   if ( !res  )
   {
      OraErrorDiagRTE( thiswa->hStmtUpdate, "BindUpdateColumns", thiswa->sSql, res, __LINE__, __FILE__ );
      return HB_FAILURE;
   }

//    res = SQLPrepare( thiswa->hStmtUpdate, (char *) (thiswa->sSql), SQL_NTS );
//    res = OCI_Prepare( thiswa->hStmtUpdate, (char *) (thiswa->sSql));
//    if ( !res )
//    {
//       OraErrorDiagRTE( thiswa->hStmtUpdate, "CreateUpdateStmtOra", thiswa->sSql, res, __LINE__, __FILE__ );
//       return HB_FAILURE;
//    }

   if( (!thiswa->bIndexTouchedInUpdate) && thiswa->sqlarea.hOrdCurrent )
   {
      // Check if any updated column is included in current index column list
      pColumns = hb_arrayGetItemPtr( hb_arrayGetItemPtr( thiswa->sqlarea.aOrders, ( ULONG ) thiswa->sqlarea.hOrdCurrent ), INDEX_FIELDS );
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

HB_ERRCODE ExecuteUpdateStmtOra( SQLEXORAAREAP thiswa )
{
   PHB_ITEM pKey, aRecord;
   HB_SIZE lPos;
   int res;

   // Feed current record to bindings

   thiswa->lUpdatedRecord = GetCurrentRecordNumOra( thiswa );

   if( FeedRecordColsOra( thiswa, TRUE ) == HB_FAILURE )  // Stmt created and prepared, only need to push data
      return (HB_FAILURE);

   // Execute statement

//    res = SQLExecute( thiswa->hStmtUpdate );
   res  = OCI_Execute(thiswa->hStmtUpdate );

   if ( !res )
   {
      OraErrorDiagRTE( thiswa->hStmtUpdate, "ExecuteUpdateStmtOra", thiswa->sSql, res, __LINE__, __FILE__ );
//       OCI_StatementFree( thiswa->hStmtInsert );
      
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
   hb_itemPutNLL( pKey, thiswa->recordList[thiswa->recordListPos] );

   if ( hb_hashScan( thiswa->hBufferPool, pKey, &lPos  ) )
   {
      aRecord = hb_hashGetValueAt( thiswa->hBufferPool, lPos );
      hb_arrayCopy( thiswa->sqlarea.aBuffer, aRecord, NULL, NULL, NULL );
   }
   hb_itemRelease( pKey );
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

