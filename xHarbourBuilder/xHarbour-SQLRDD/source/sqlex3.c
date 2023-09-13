/* $CATEGORY$SQLEX/HIDE$FILES$HIDE$
* SQLEX Auxiliar File for SEEK routines
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

static void createSeekQuery( SQLEXAREAP thiswa, BOOL bUseOptimizerHints )
{
   if( getColumnList( thiswa ) )
   {
      thiswa->bConditionChanged1 = TRUE;     // SEKIP statements are no longer valid - column list has changed!
   }
   if ( thiswa->sSql ) 
   memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   if( bUseOptimizerHints )
   {
      sprintf( thiswa->sSql, "SELECT /*+ INDEX_ASC( A %s ) */ %s %s \nFROM %s A %s AND ROWNUM <= 1",
                             thiswa->sOrderBy,     // thiswa->sOrderBy has the index name, not the index column list
                             thiswa->sLimit1,
                             thiswa->sFields,
                             thiswa->sTable,
                             thiswa->sWhere );
   }
   else
   {
      sprintf( thiswa->sSql, "SELECT %s %s \nFROM %s A %s %s %s",
                             thiswa->sLimit1,
                             thiswa->sFields,
                             thiswa->sTable,
                             thiswa->sWhere,
                             thiswa->sOrderBy,     // thiswa->sOrderBy has the index column list
                             thiswa->sLimit2 );
   }

}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getSeekWhereExpression( SQLEXAREAP thiswa, int iListType, int queryLevel, BOOL * bUseOptimizerHints )
{
   BOOL bWhere = FALSE;
   int iCol;
   INDEXBINDP SeekBind;
   COLUMNBINDP BindStructure;
   BOOL bDirectionFWD;
   char * temp;

   thiswa->sWhere[0] = '\0';

   SeekBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
   //Nao necessario esta aqui, fazia com que fosse para o ultimo item da chave
   //SeekBind += ( queryLevel -1 );         // place offset

   thiswa->recordListDirection = ( iListType == LIST_SKIP_FWD ? LIST_FORWARD : LIST_BACKWARD );
   bDirectionFWD               = iListType == LIST_SKIP_FWD;

   if( thiswa->bReverseIndex )
   {
      bDirectionFWD = !bDirectionFWD;
   }

   for (iCol = 1; iCol <= queryLevel; iCol++)
   {
      BindStructure   = GetBindStruct( thiswa, SeekBind );

      if( BindStructure->isArgumentNull )
      {
         * bUseOptimizerHints = FALSE;    // We cannot use this high speed solution
                                          // because Oracle does not store NULLs in indexes

         if( BindStructure->iCType == SQL_C_DOUBLE )
         {
            temp = hb_strdup( (const char *) thiswa->sWhere );
            sprintf( thiswa->sWhere, "%s %s ( A.%c%s%c %s %s OR A.%c%s%c IS NULL )", bWhere ? temp : "\nWHERE", bWhere ? "AND" : "",
                                                       OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ),
                                                       iCol == queryLevel ? ( bDirectionFWD ? ">=" : "<=" ) : "IS",
                                                       iCol == queryLevel ? "0" : "NULL",
                                                       OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ) );
             hb_xfree( temp );
         }
         else
         {
            if (iCol == queryLevel && iListType == LIST_SKIP_FWD )
            {
               // This condition should create a WHERE clause like "COLUMN >= NULL".
               // Since this is not numeric, EVERYTHING is greater
               // or equal to NULL, so we do not add any restriction to WHERE clause.
            }
            else
            {
               temp = hb_strdup( (const char *) thiswa->sWhere );
               sprintf( thiswa->sWhere, "%s %s A.%c%s%c IS NULL", bWhere ? temp : "\nWHERE", bWhere ? "AND" : "",
                                                          OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ) );
               hb_xfree( temp );
            }
         }
      }
      else
      {
         temp = hb_strdup( (const char *) thiswa->sWhere );
         sprintf( thiswa->sWhere, "%s %s A.%c%s%c %s ?", bWhere ? temp : "\nWHERE", bWhere ? "AND" : "",
                                                    OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ),
                                                    iCol == queryLevel ? ( bDirectionFWD ? ">=" : "<=" ) : "=" );
         hb_xfree( temp );
      }
      bWhere = TRUE;
      // Culik Movido a posicao do seekbind para essa posicao, onde estava assumuia que o inicio era o ultimo item da chave
      SeekBind ++;         // place offset      
   }
   bWhere  = strlen(  thiswa->sWhere ) >0;
   SolveFilters( thiswa, bWhere );

   return ( HB_SUCCESS );
}
/*
static HB_ERRCODE getSeekWhereExpression( SQLEXAREAP thiswa, int iListType, int queryLevel, BOOL * bUseOptimizerHints )
{
   SqlExLog( "getSeekWhereExpression()", 3 );

   BOOL bWhere = FALSE;
   int iCol;
   INDEXBINDP SeekBind;
   COLUMNBINDP BindStructure;
   BOOL bDirectionFWD;
   char * temp;

   thiswa->sWhere[0] = '\0';

   SeekBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
   //Nao necessario esta aqui, fazia com que fosse para o ultimo item da chave
   //SeekBind += ( queryLevel -1 );         // place offset

   thiswa->recordListDirection = ( iListType == LIST_SKIP_FWD ? LIST_FORWARD : LIST_BACKWARD );
   bDirectionFWD               = iListType == LIST_SKIP_FWD;

   if( thiswa->bReverseIndex )
   {
      bDirectionFWD = !bDirectionFWD;
   }

   for (iCol = 1; iCol <= queryLevel; iCol++)
   {
      BindStructure   = GetBindStruct( thiswa, SeekBind );

      if( BindStructure->isArgumentNull )
      {
         * bUseOptimizerHints = FALSE;    // We cannot use this high speed solution
                                          // because Oracle does not store NULLs in indexes

         if( BindStructure->iCType == SQL_C_DOUBLE )
         {
            temp = hb_strdup( (const char *) thiswa->sWhere );
            sprintf( thiswa->sWhere, "%s %s ( A.%c%s%c %s %s OR A.%c%s%c IS NULL )", bWhere ? temp : "\nWHERE", bWhere ? "AND" : "",
                                                       OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ),
                                                       iCol == queryLevel ? ( bDirectionFWD ? ">=" : "<=" ) : "IS",
                                                       iCol == queryLevel ? "0" : "NULL",
                                                       OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ) );
             hb_xfree( temp );
         }
         else
         {
            if (iCol == queryLevel && iListType == LIST_SKIP_FWD )
            {
               // This condition should create a WHERE clause like "COLUMN >= NULL".
               // Since this is not numeric, EVERYTHING is greater
               // or equal to NULL, so we do not add any restriction to WHERE clause.
            }
            else
            {
               temp = hb_strdup( (const char *) thiswa->sWhere );
               sprintf( thiswa->sWhere, "%s %s A.%c%s%c IS NULL", bWhere ? temp : "\nWHERE", bWhere ? "AND" : "",
                                                          OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ) );
               hb_xfree( temp );
            }
         }
      }
      else
      {
         temp = hb_strdup( (const char *) thiswa->sWhere );
         sprintf( thiswa->sWhere, "%s %s A.%c%s%c %s ?", bWhere ? temp : "\nWHERE", bWhere ? "AND" : "",
                                                    OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ),
                                                    iCol == queryLevel ? ( bDirectionFWD ? ">=" : "<=" ) : "=" );
         hb_xfree( temp );
      }
      bWhere = TRUE;
      // Culik Movido a posicao do seekbind para essa posicao, onde estava assumuia que o inicio era o ultimo item da chave
      SeekBind ++;         // place offset      
   }

   SolveFilters( thiswa, bWhere );

   return ( HB_SUCCESS );
}
*/
/*------------------------------------------------------------------------*/

HB_ERRCODE prepareSeekQuery( SQLEXAREAP thiswa, INDEXBINDP SeekBind )
{
   SQLRETURN res;
   HSTMT hPrep;

   res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &hPrep );

   if ( CHECK_SQL_N_OK( res ) )
   {
      return (HB_FAILURE);
   }

   if ( CHECK_SQL_N_OK( SQLPrepare( hPrep, (SQLCHAR *) (thiswa->sSql), SQL_NTS ) ) )
   {
      return (HB_FAILURE);
   }

   if ( thiswa->recordListDirection == LIST_FORWARD )
   {
      SeekBind->SeekFwdStmt = hPrep;
      memset( &SeekBind->SeekFwdSql,0,PREPARED_SQL_LEN ) ;
      hb_xmemcpy( SeekBind->SeekFwdSql, thiswa->sSql, PREPARED_SQL_LEN -1 );
      SeekBind->SeekFwdSql[PREPARED_SQL_LEN-1] = '\0';
   }
   else
   {
      SeekBind->SeekBwdStmt = hPrep;
      memset( &SeekBind->SeekBwdSql,0,PREPARED_SQL_LEN ) ;
      hb_xmemcpy( SeekBind->SeekBwdSql, thiswa->sSql, PREPARED_SQL_LEN -1 );
      SeekBind->SeekBwdSql[PREPARED_SQL_LEN-1] = '\0';
   }
   return (HB_SUCCESS);
}

/*------------------------------------------------------------------------*/

BOOL CreateSeekStmt( SQLEXAREAP thiswa, int queryLevel )
{
   PHB_ITEM pColumns, pIndexRef;
   INDEXBINDP SeekBind;
   BOOL bUseOptimizerHints;

   bUseOptimizerHints         = thiswa->nSystemID == SYSTEMID_ORACLE;
   thiswa->bConditionChanged1 = TRUE;     // SKIP statements are no longer valid

   // Alloc memory for binding structures, if first time

   if ( ! thiswa->IndexBindings[ thiswa->hOrdCurrent ] )
   {
      SetIndexBindStructure( thiswa );
   }

   SeekBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
   //SeekBind += ( queryLevel -1 );         // place offset

   // Check if stmt must be created or recreated

   if( thiswa->bConditionChanged2 || thiswa->bRebuildSeekQuery ||
     ( thiswa->recordListDirection == LIST_FORWARD  && (! SeekBind->SeekFwdStmt ) ) ||
     ( thiswa->recordListDirection == LIST_BACKWARD && (! SeekBind->SeekBwdStmt ) ) )
   {

      pIndexRef = hb_arrayGetItemPtr( thiswa->aOrders, ( ULONG ) thiswa->hOrdCurrent );
      pColumns = hb_arrayGetItemPtr( pIndexRef, INDEX_FIELDS );
      thiswa->indexColumns = hb_arrayLen( pColumns );

      // Free the statements we are about to recreate

      if ( SeekBind->SeekFwdStmt )
      {
         SQLFreeStmt( SeekBind->SeekFwdStmt, SQL_DROP );
         SeekBind->SeekFwdStmt = NULL;
      }

      if ( SeekBind->SeekBwdStmt )
      {
         SQLFreeStmt( SeekBind->SeekBwdStmt, SQL_DROP );
         SeekBind->SeekBwdStmt = NULL;
      }

      getSeekWhereExpression( thiswa, thiswa->recordListDirection == LIST_FORWARD ? LIST_SKIP_FWD : LIST_SKIP_BWD, queryLevel, &bUseOptimizerHints );
      getOrderByExpression( thiswa, bUseOptimizerHints );
      setResultSetLimit( thiswa, 1 );
      createSeekQuery( thiswa, bUseOptimizerHints );

      prepareSeekQuery( thiswa, SeekBind );
      thiswa->bOrderChanged = FALSE ; // we set to use the new key after enter here, so we disable for next seek
      return (TRUE);
   }
   else
   {
      return (FALSE);
   }
}

/*------------------------------------------------------------------------*/

HB_ERRCODE FeedSeekKeyToBindings( SQLEXAREAP thiswa, PHB_ITEM pKey, int * queryLevel )
{
   INDEXBINDP SeekBind;
   COLUMNBINDP BindStructure;
   int i, lenKey, size, iCol;
   const char * szKey;

   SeekBind   = thiswa->IndexBindings[ thiswa->hOrdCurrent ];

   if(  thiswa->hOrdCurrent != SeekBind->hIndexOrder )//  || thiswa->bOrderChanged )
   {
      // SeekBindings is not constructed based on same index order as
      // previous SEEK, so we must reconstruct thiswa->IndexBindings[ thiswa->hOrdCurrent ]
      // based on current index

      thiswa->bConditionChanged2 = TRUE;                 // Force SEEK query to be rebuilt
      SeekBind->hIndexOrder      = thiswa->hOrdCurrent;  // Store latest prepared index order query

      for (iCol = 1; iCol <= thiswa->indexColumns; iCol++)
      {
         BindStructure   = GetBindStruct( thiswa, SeekBind );

         if( ! thiswa->uiFieldList[(BindStructure->lFieldPosDB)-1] )
         {
            thiswa->uiFieldList[(BindStructure->lFieldPosDB)-1] = TRUE;         // Force index columns to be present in query
                                                               // cos sqlKeyCompare will need it
            thiswa->iColumnListStatus = FIELD_LIST_CHANGED;
         }

         SeekBind->iLevel              = iCol;
         SeekBind->iIndexColumns       = thiswa->indexColumns;
         BindStructure->isArgumentNull = FALSE;

         // Free previous statements

         if ( SeekBind->SeekFwdStmt )
         {
            SQLFreeStmt( SeekBind->SeekFwdStmt, SQL_DROP );
            SeekBind->SeekFwdStmt = NULL;
            thiswa->bRebuildSeekQuery = TRUE;
         }
         if ( SeekBind->SeekBwdStmt )
         {
            SQLFreeStmt( SeekBind->SeekBwdStmt, SQL_DROP );
            SeekBind->SeekBwdStmt = NULL;
            thiswa->bRebuildSeekQuery = TRUE;
         }

         SeekBind++;
      }
      SeekBind  = thiswa->IndexBindings[ thiswa->hOrdCurrent ];      // Reset position to 1st index column
   }

   // Push pKey value splitted into SeekBindings structures

   if( HB_IS_STRING( pKey ) )
   {
      // parse Key string and split it in index fields

      lenKey  = hb_itemGetCLen( pKey );
      szKey   = hb_itemGetCPtr( pKey );
      * queryLevel = thiswa->indexColumns;

      for( i=1; i <= thiswa->indexColumns; i++ )
      {
         BindStructure   = GetBindStruct( thiswa, SeekBind );
         size = 0;

         switch ( BindStructure->iCType )
         {
            case SQL_C_CHAR:
            {
               int nTrim, i2;
               size  = lenKey > (int)(BindStructure->ColumnSize) ? ((int) (BindStructure->ColumnSize)) : lenKey;
               nTrim = size;

               // RTrim() the string value

               for (i2 = (size -1); i2 >= 0; i2-- )
               {
                  if( szKey[i2] == '\0' || szKey[i2] != ' ' )
                  {
                     nTrim = i2+1;
                     break;
                  }
               }
               if( i2 < 0 )
               {
                  nTrim = 0;
               }

               if( nTrim == 0 )
               {
                  // We have a NULL argument to SEEK for

                  if( !(BindStructure->isNullable) )
                  {
                     // Column cannot be NULL by definition, so
                     // SQLRDD uses 1 blank space in it
                     // This is an SQLRDD rule.

                     BindStructure->asChar.value[ 0 ] = ' ';
                     BindStructure->asChar.value[ 1 ] = '\0';
                  }
                  else
                  {
                     BindStructure->asChar.value[ 0 ] = '\0';
                     if( BindStructure->isArgumentNull == FALSE )   // Check if NULL status has changed
                     {
                        thiswa->bRebuildSeekQuery = TRUE;
                     }
                     BindStructure->isArgumentNull     = TRUE;
                  }
               }
               else
               {
                  hb_xmemcpy( BindStructure->asChar.value, szKey, nTrim );
                  BindStructure->asChar.value[ nTrim ] = '\0';
                  if( BindStructure->isArgumentNull )   // Check if NULL status has changed
                  {
                     thiswa->bRebuildSeekQuery = TRUE;
                     BindStructure->isArgumentNull  = FALSE;
                  }
               }
               break;
            }
            case SQL_C_DOUBLE:
            {
               size = BindStructure->ColumnSize;
               BindStructure->asNumeric = (SQLDOUBLE) hb_strVal( szKey, BindStructure->ColumnSize );
               break;
            }
            case SQL_C_TYPE_TIMESTAMP:
            {	            
               int iPos;
               HB_MAXINT lVal;
               double dVal;

               char datemask[9] = "10000101";
               char * mask = datemask;
//DebugBreak();
               size  = lenKey > (int)(BindStructure->ColumnSize) ? ((int) (BindStructure->ColumnSize)) : lenKey;

               // Must fix partial date seek
               for( iPos=0; iPos < size; iPos++ )
               {
                  datemask[iPos] = szKey[iPos];
               }

               hb_compStrToNum( datemask, 4, &lVal, &dVal, NULL, NULL );
               BindStructure->asTimestamp.year  = (SQLSMALLINT) lVal;
               mask += 4;
               hb_compStrToNum( mask, 2, &lVal, &dVal, NULL, NULL );
               BindStructure->asTimestamp.month = (SQLUSMALLINT) lVal;
               mask += 2;
               hb_compStrToNum( mask, 2, &lVal, &dVal, NULL, NULL );
               BindStructure->asTimestamp.day   = (SQLUSMALLINT) lVal;
               BindStructure->asTimestamp.hour     = 0;
               BindStructure->asTimestamp.minute   = 0;
               BindStructure->asTimestamp.second   = 0;
               BindStructure->asTimestamp.fraction = 0;
               break;
            }
            case SQL_C_TYPE_DATE:
            {
               int iPos;
               HB_MAXINT lVal;
               double dVal;

               char datemask[9] = "10000101";
               char * mask = datemask;

               size  = lenKey > (int)(BindStructure->ColumnSize) ? ((int) (BindStructure->ColumnSize)) : lenKey;

               // Must fix partial date seek
               for( iPos=0; iPos < size; iPos++ )
               {
                  datemask[iPos] = szKey[iPos];
               }

               hb_compStrToNum( datemask, 4, &lVal, &dVal, NULL, NULL );
               BindStructure->asDate.year  = (SQLSMALLINT) lVal;
               mask += 4;
               hb_compStrToNum( mask, 2, &lVal, &dVal, NULL, NULL );
               BindStructure->asDate.month = (SQLUSMALLINT) lVal;
               mask += 2;
               hb_compStrToNum( mask, 2, &lVal, &dVal, NULL, NULL );
               BindStructure->asDate.day   = (SQLUSMALLINT) lVal;

               break;
            }
         }

         lenKey -= size;
         szKey  += size;

         if( lenKey <= 0 )
         {
            * queryLevel = i;
            break;
         }

         SeekBind++;      // Advance to next index column in bind structure
      }
   }
   else
   {
      * queryLevel = 1;
      BindStructure   = GetBindStruct( thiswa, SeekBind );

      if( HB_IS_NUMERIC( pKey ) )
      {
         if( BindStructure->iCType != SQL_C_DOUBLE )      // Check column data type
         {
            // To Do: Raise RT error
            return (HB_FAILURE);
         }
         BindStructure->asNumeric = (SQLDOUBLE) hb_itemGetND( pKey );
      }
      else if( HB_IS_DATE( pKey ) || HB_IS_DATETIME( pKey ) )
      {
         int iYear, iMonth, iDay;
         int iHour, iMinute;

         hb_dateDecode( hb_itemGetDL( pKey ), &iYear, &iMonth, &iDay );

         if( BindStructure->iCType == SQL_C_TYPE_DATE )
         {
            BindStructure->asDate.year  = (SQLSMALLINT) iYear;
            BindStructure->asDate.month = (SQLUSMALLINT) iMonth;
            BindStructure->asDate.day   = (SQLUSMALLINT) iDay;
         }
         else if( BindStructure->iCType == SQL_C_TYPE_TIMESTAMP )
         {
#ifdef __XHARBOUR__
            double seconds;
            hb_timeDecode( pKey->item.asDate.time, &iHour, &iMinute, &seconds ); 
#else
            long lJulian, lMilliSec;
            int seconds, millisec;
            hb_itemGetTDT( pKey, &lJulian, &lMilliSec );
            hb_timeDecode( lMilliSec, &iHour, &iMinute, &seconds, &millisec ); 
#endif

            BindStructure->asTimestamp.year  = (SQLSMALLINT) iYear;
            BindStructure->asTimestamp.month = (SQLUSMALLINT) iMonth;
            BindStructure->asTimestamp.day   = (SQLUSMALLINT) iDay;
            BindStructure->asTimestamp.hour     = (SQLUSMALLINT)iHour;    ;
            BindStructure->asTimestamp.minute   = (SQLUSMALLINT)iMinute;  ;
            BindStructure->asTimestamp.second   = (SQLUSMALLINT)seconds;
            BindStructure->asTimestamp.fraction = 0;
         }
         else
         {
            // To Do: Raise RT error
            return (HB_FAILURE);
         }
      }
      else if( HB_IS_LOGICAL( pKey ) )
      {
         if( BindStructure->iCType != SQL_C_BIT )      // Check column data type
         {
            // To Do: Raise RT error
            return (HB_FAILURE);
         }
         BindStructure->asLogical = (SQLCHAR) hb_itemGetL( pKey );
      }
   }
   return (HB_SUCCESS);
}


/*------------------------------------------------------------------------*/

void BindSeekStmt( SQLEXAREAP thiswa, int queryLevel )
{
   HSTMT hStmt;
   INDEXBINDP SeekBind, SeekBindParam;
   COLUMNBINDP BindStructure;
   int iBind, iLoop;
   SQLRETURN res = SQL_ERROR;
   char * sSql;

   SeekBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
   //Culik 
   //removed, this line bellow make the data be the last field name
//   SeekBind += ( queryLevel -1 );         // place offset

   hStmt          = thiswa->recordListDirection == LIST_FORWARD ? SeekBind->SeekFwdStmt : SeekBind->SeekBwdStmt;
   sSql           = thiswa->recordListDirection == LIST_FORWARD ? SeekBind->SeekFwdSql : SeekBind->SeekBwdSql;
   SeekBindParam  = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
   iBind          = 1;

   for (iLoop = 1; iLoop <= queryLevel; iLoop++ )
   {
      BindStructure   = GetBindStruct( thiswa, SeekBindParam );
      if( !BindStructure->isArgumentNull )
      {

       // Corrigido 27/12/2013 09:53 - lpereira
       // Estava atribuindo o valor de SYSTEMID_ORACLE para thiswa->nSystemID.
       //if ( thiswa->nSystemID = SYSTEMID_ORACLE )
       if ( thiswa->nSystemID == SYSTEMID_ORACLE )
          if ( BindStructure->iCType == SQL_C_TYPE_DATE ) 
	           BindStructure->iCType = SQL_C_TYPE_TIMESTAMP;        // May be DATE or TIMESTAMP
	           
         switch (BindStructure->iCType)
         {
            case SQL_C_CHAR:
            {
               res = SQLBindParameter( (SQLHSTMT     ) hStmt, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) BindStructure->iCType,
                                       (SQLSMALLINT  ) BindStructure->iSQLType,
                                       (SQLULEN      ) BindStructure->ColumnSize,
                                       (SQLSMALLINT  ) BindStructure->DecimalDigits,
                                       (SQLPOINTER   ) BindStructure->asChar.value, 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) NULL );

							   
               break;
            }
            case SQL_C_DOUBLE:
            {
               res = SQLBindParameter( (SQLHSTMT     ) hStmt, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) BindStructure->iCType,
                                       (SQLSMALLINT  ) BindStructure->iSQLType,
                                       (SQLULEN      ) BindStructure->ColumnSize,
                                       (SQLSMALLINT  ) BindStructure->DecimalDigits,
                                       (SQLPOINTER   ) &(BindStructure->asNumeric), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) NULL );
               break;
            }
            case SQL_C_TYPE_TIMESTAMP:
            {
	            //DebugBreak();
               //res = SQLBindParameter( hStmt, iBind, SQL_PARAM_INPUT,
               //                        SQL_C_TYPE_DATE,
               //                        SQL_TYPE_DATE,
               //                        SQL_TIMESTAMP_LEN,
               //                        0,
               //                        &(BindStructure->asTimestamp), 0, 0 );
               res = SQLBindParameter( (SQLHSTMT     ) hStmt, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) SQL_C_TYPE_TIMESTAMP,
                                       (SQLSMALLINT  ) SQL_TYPE_TIMESTAMP,
                                       (SQLULEN      ) SQL_TIMESTAMP_LEN,
                                       (SQLSMALLINT  ) thiswa->nSystemID == SYSTEMID_MSSQL7 ||thiswa->nSystemID == SYSTEMID_AZURE ? 3 : 0,
                                       (SQLPOINTER   ) &(BindStructure->asTimestamp), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) 0 );
               break;
            }
            case SQL_C_TYPE_DATE:
            {
               res = SQLBindParameter( (SQLHSTMT     ) hStmt, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) SQL_C_TYPE_DATE,
                                       (SQLSMALLINT  ) SQL_TYPE_DATE,
                                       (SQLULEN      ) SQL_DATE_LEN,
                                       (SQLSMALLINT  ) 0,
                                       (SQLPOINTER   ) &(BindStructure->asDate), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) NULL );
               break;
            }
            case SQL_C_BIT:
            {
               res = SQLBindParameter( (SQLHSTMT     ) hStmt, 
			                           (SQLUSMALLINT ) iBind, 
									   (SQLSMALLINT  ) SQL_PARAM_INPUT,
                                       (SQLSMALLINT  ) BindStructure->iCType,
                                       (SQLSMALLINT  ) BindStructure->iSQLType,
                                       (SQLULEN      ) BindStructure->ColumnSize,
                                       (SQLSMALLINT  ) BindStructure->DecimalDigits,
                                       (SQLPOINTER   ) &(BindStructure->asLogical), 
									   (SQLLEN       ) 0, 
									   (SQLLEN*      ) NULL );
               break;
            }
         }
         if ( CHECK_SQL_N_OK( res ) )
         {
            odbcErrorDiagRTE( hStmt, "BindSeekStmt", sSql, res, __LINE__, __FILE__ );
         }
         iBind++;
         BindStructure->iParNum = iBind;
      }
      SeekBindParam++;
      //Culik Correct place so we can get the data from next field
      SeekBind ++ ; //= ( queryLevel -1 );         // place offset      
   }
}

/*------------------------------------------------------------------------*/

HB_ERRCODE getPreparedSeek( SQLEXAREAP thiswa, int queryLevel, USHORT * iIndex, HSTMT * hStmt ) // Returns TRUE if any result found
{
   SQLRETURN res;
   INDEXBINDP SeekBind;

   SeekBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
   //this line bellow make the last field current
   //SeekBind += ( queryLevel -1 );         // place offset
   HB_SYMBOL_UNUSED( queryLevel );

   * hStmt = thiswa->recordListDirection == LIST_FORWARD ? SeekBind->SeekFwdStmt : SeekBind->SeekBwdStmt;

   res = SQLExecute( * hStmt );

   if ( CHECK_SQL_N_OK( res ) )
   {
      odbcErrorDiagRTE( * hStmt, "getPreparedSeek", "", res, __LINE__, __FILE__ );
//       SQLCloseCursor( * hStmt );
      SQLFreeStmt( * hStmt, SQL_CLOSE );
      return (HB_FAILURE);
   }

   res = SQLFetch( * hStmt );
   if ( res != SQL_SUCCESS )
   {
      return HB_FAILURE;
   }

   res = SQLGetData( * hStmt, 1, SQL_C_ULONG, &(thiswa->recordList[0]), sizeof( SQL_C_ULONG ), NULL );

   if( res == SQL_ERROR )
   {
      SQLFreeStmt( * hStmt, SQL_CLOSE );
      return (HB_FAILURE);
   }

   if( thiswa->ulhDeleted > 0 )
   {
      SQLCHAR szValue[2];
      res = SQLGetData( * hStmt, 2, SQL_C_CHAR, szValue, 2, NULL );
      if( res == SQL_ERROR )
      {
         SQLFreeStmt( * hStmt, SQL_CLOSE );
         return (HB_FAILURE);
      }
      else
      {
         if( szValue[0] == 0 )
         {
            thiswa->deletedList[0] = ' ';    // MySQL driver climps spaces from right side
         }
         else
         {
            thiswa->deletedList[0] = szValue[0];
         }
      }
      * iIndex = 2;
   }
   else
   {
      * iIndex = 1;
      thiswa->deletedList[0] = ' ';
   }

   thiswa->recordListSize      = 1L;
   thiswa->recordListPos       = 0;

   return HB_SUCCESS;
}

