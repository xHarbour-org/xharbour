/* $CATEGORY$SQLEX/HIDE$FILES$HIDE$
* SQLEX Main File
* Copyright (c) 2008 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved

Quick to do list, 2009 feb 23:

    4.4 - Review all filter functionalities in WHERE clauses and fully
          implement thiswa->bConditionChanged support
    4.5 - Good idea to bind both input and output for recor list

5 - Finish implementation of iColumnListStatus strategy described in
    getColumnList() function

*/
//#define DEBUG_XGRAB
#include "compat.h"
#include "hbinit.h"

#include "msg.ch"
#include "rddsys.ch"
#include "hbdbferr.h"

#include "sqlrddsetup.ch"
#include "sqlprototypes.h"

#include <ctype.h>
#include <assert.h>

#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
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

#include <time.h>
static RDDFUNCS sqlExSuper;

/*static PHB_DYNS s_pSym_SOLVERESTRICTORS;*/
/*
void startSQLEXSymbols()
{
   HB_THREAD_STUB

   if( s_pSym_SOLVERESTRICTORS == NULL )
   {

      hb_dynsymLock();

      s_pSym_SOLVERESTRICTORS = hb_dynsymFindName( "SOLVERESTRICTORS" );

      if ( s_pSym_SOLVERESTRICTORS == NULL ) printf( "Could not find Symbol %s\n", "SOLVERESTRICTORS" );

      hb_dynsymUnlock();
   }
}
*/
HB_FUNC_EXTERN( SR_END );
HB_FUNC_EXTERN( SR_INIT );
HB_FUNC_EXTERN( __SR_STARTSQL );
HB_FUNC_EXTERN( SQLRDD );

static HB_ISIZ pageReadSize   = PAGE_READ_SIZE;
static HB_SIZE bufferPoolSize = BUFFER_POOL_SIZE;

static BOOL CreateSkipStmt( SQLEXAREAP thiswa );
static int bOldReverseIndex  = 0;
static int sqlKeyCompareEx( SQLEXAREAP thiswa, PHB_ITEM pKey, BOOL fExact );

static PHB_DYNS s_pSym_SR_DESERIALIZE = NULL;

static BOOL _SqlExIsLogFirst = TRUE;
static BOOL _SqlExIsLogFile = FALSE;

BOOL SqlExIsLog();

void SqlExLog( const char * str, int ver );

HB_EXTERN_BEGIN
   extern  PHB_ITEM loadTagDefault( SQLEXAREAP thiswa, LPDBORDERINFO pInfo, LONG * lorder );
HB_EXTERN_END
/*------------------------------------------------------------------------*/
/*
static char * sqlSolveRestrictors( SQLEXAREAP thiswa )
{
   if( s_pSym_SOLVERESTRICTORS )
   {
      hb_objSendMessage( thiswa->oWorkArea, s_pSym_SOLVERESTRICTORS, 0 );
      return ( hb_itemGetCPtr( hb_stackReturnItem() ) );
   } else
      return "";
}
*/

/*------------------------------------------------------------------------*/

ULONG GetCurrentRecordNum( SQLEXAREAP thiswa )
{
   if( thiswa->bIsInsert || thiswa->area.fEof )
   {
      return( thiswa->lLastRec );
   }
   else
   {
      return( (ULONG) thiswa->recordList[thiswa->recordListPos] );
   }
}

/*------------------------------------------------------------------------*/

BOOL IsItemNull( PHB_ITEM pFieldData, SQLEXAREAP thiswa )
{
   if( SR_itemEmpty( pFieldData ) && (!(    HB_IS_ARRAY( pFieldData )
                                         || HB_IS_OBJECT( pFieldData )
                                         || HB_IS_HASH( pFieldData ) ))
                                  && (      ( (thiswa->nSystemID == SYSTEMID_POSTGR) && HB_IS_DATE( pFieldData ) )
                                         || ( (thiswa->nSystemID != SYSTEMID_POSTGR) && ( !HB_IS_LOGICAL( pFieldData ) ) ) ) )
   {
      return TRUE;
   }
   return FALSE;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE ConcludeSkipraw( SQLEXAREAP thiswa )
{
   /* Force relational movement in child WorkAreas */

   if( thiswa->area.lpdbRelations )
   {
      return SELF_SYNCCHILDREN( ( AREAP ) thiswa );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static void sqlGetCleanBuffer( SQLEXAREAP thiswa )
{
   HB_SIZE nPos, nLen;
   PHB_ITEM pCol;

   pCol = hb_itemNew( NULL );
   for( nPos = 1, nLen = hb_arrayLen( thiswa->aEmptyBuff ); nPos <= nLen; nPos++ )
   {
      hb_arrayGet( thiswa->aEmptyBuff, nPos, pCol );
      hb_arraySet( thiswa->aOldBuffer, nPos, pCol );
      hb_arraySetForward( thiswa->aBuffer, nPos, pCol );
   }

   hb_itemRelease( pCol );

   // fix lastrec()+1
   pCol = hb_arrayGetItemPtr( thiswa->aInfo, AINFO_RCOUNT );
   thiswa->lLastRec = hb_itemGetNL( pCol ) + 1;
   thiswa->area.fEof = TRUE;
}

/*------------------------------------------------------------------------*/

void setResultSetLimit( SQLEXAREAP thiswa, int iRows )
{
   char * fmt1, * fmt2;

   if( iRows > 1 )
   {
      iRows++;     // Add one more to multiple line queries
   }

   switch ( thiswa->nSystemID )
   {
   case SYSTEMID_MSSQL7:
   case SYSTEMID_CACHE:
   case SYSTEMID_SYBASE:
      fmt1 = "TOP %i";
      fmt2 = "";
      break;
   case SYSTEMID_FIREBR:
   case SYSTEMID_INFORM:
      fmt1 = "FIRST %i";
      fmt2 = "";
      break;
   case SYSTEMID_ORACLE:
      fmt1         = "";
      fmt2         = "";
      break;
   case SYSTEMID_POSTGR:
   case SYSTEMID_MYSQL:
   case SYSTEMID_MARIADB:
      fmt1 = "";
      fmt2 = "LIMIT %i";
      break;
   case SYSTEMID_IBMDB2:
      fmt1 = "";
      fmt2 = "fetch first %i rows only";
      break;
   default:
      fmt1         = "";
      fmt2         = "";
   }
   sprintf( thiswa->sLimit1, (const char *)fmt1, iRows );
   sprintf( thiswa->sLimit2, (const char *)fmt2, iRows );
}

/*------------------------------------------------------------------------*/

static LONG getMessageNL( PHB_ITEM obj, char * message )
{
   hb_objSendMsg( obj, message, 0 );
   return ( hb_itemGetNL( hb_stackReturnItem() ) );

}

static void * getMessagePtr( PHB_ITEM obj, char * message )
{

   hb_objSendMsg( obj, message, 0 );
   return ( hb_itemGetPtr( hb_stackReturnItem() ) );
}

/*------------------------------------------------------------------------*/

static LONG getMessageNI( PHB_ITEM obj, char * message )
{
   hb_objSendMsg( obj, message, 0 );
   return ( hb_itemGetNI( hb_stackReturnItem() ) );
}

/*------------------------------------------------------------------------*/

static char * getMessageC( PHB_ITEM obj, char * message )
{
   hb_objSendMsg( obj, message, 0 );
   return ( hb_itemGetC( hb_stackReturnItem() ) );
}

/*------------------------------------------------------------------------*/

static BOOL getMessageL( PHB_ITEM obj, char * message )
{
   hb_objSendMsg( obj, message, 0 );
   return ( hb_itemGetL( hb_stackReturnItem() ) );
}

/*------------------------------------------------------------------------*/

static PHB_ITEM getMessageItem( PHB_ITEM obj, char * message )
{
   hb_objSendMsg( obj, message, 0 );
   return( hb_itemNew( hb_stackReturnItem() ) );
}

/*------------------------------------------------------------------------*/

static void createRecodListQuery( SQLEXAREAP thiswa )
{
   if ( thiswa->sSql )
      memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   if( thiswa->ulhDeleted == 0 )
   {
      if (thiswa->bIsSelect)
      {
         sprintf( thiswa->sSql, "SELECT %s A.%c%s%c FROM (%s) A %s %s %s", thiswa->sLimit1,
                             OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),
                             thiswa->szDataFileName,
                             thiswa->sWhere,
                             thiswa->sOrderBy,
                             thiswa->sLimit2 );
      }
      else
      {
         sprintf( thiswa->sSql, "SELECT %s A.%c%s%c FROM %s A %s %s %s", thiswa->sLimit1,
                             OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),
                             thiswa->sTable,
                             thiswa->sWhere,
                             thiswa->sOrderBy,
                             thiswa->sLimit2 );

      }
   }
   else
   {
      if (thiswa->bIsSelect)
      {
         sprintf( thiswa->sSql, "SELECT %s A.%c%s%c, A.%c%s%c FROM (%s) A %s %s %s",
                             thiswa->sLimit1,
                             OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),
                             OPEN_QUALIFIER( thiswa ), thiswa->sDeletedName, CLOSE_QUALIFIER( thiswa ),
                             thiswa->sTable,
                             thiswa->sWhere,
                             thiswa->sOrderBy,
                             thiswa->sLimit2 );

      }
      else
      {
         sprintf( thiswa->sSql, "SELECT %s A.%c%s%c, A.%c%s%c FROM %s A %s %s %s",
                             thiswa->sLimit1,
                             OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),
                             OPEN_QUALIFIER( thiswa ), thiswa->sDeletedName, CLOSE_QUALIFIER( thiswa ),
                             thiswa->sTable,
                             thiswa->sWhere,
                             thiswa->sOrderBy,
                             thiswa->sLimit2 );
      }
   }
}

/*------------------------------------------------------------------------*/

static void createCountQuery( SQLEXAREAP thiswa )
{
   if ( thiswa->sSql )
      memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   sprintf( thiswa->sSql, "SELECT COUNT( A.%c%s%c ) \nFROM %s A %s",
                          OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),
                          thiswa->sTable,
                          thiswa->sWhere );
}

/*------------------------------------------------------------------------*/

void getOrderByExpression( SQLEXAREAP thiswa, BOOL bUseOptimizerHints )
{
   PHB_ITEM pIndexRef;

   if( bUseOptimizerHints )
   {
      // The the index phisical name
      if ( thiswa->hOrdCurrent > 0 )
      {
         pIndexRef = hb_arrayGetItemPtr( thiswa->aOrders, ( ULONG ) thiswa->hOrdCurrent );
         sprintf( thiswa->sOrderBy , "%s", hb_arrayGetCPtr( pIndexRef, INDEX_PHISICAL_NAME ) );
      }
      else
      {
         thiswa->sOrderBy[0] = '\0';
      }
   }
   else
   {
      BOOL bDirectionFWD = thiswa->recordListDirection == LIST_FORWARD;

      if( thiswa->bReverseIndex )
      {
         bDirectionFWD = !bDirectionFWD;
      }
      // Get the index column list
      if ( thiswa->hOrdCurrent > 0 )
      {
         pIndexRef = hb_arrayGetItemPtr( thiswa->aOrders, ( ULONG ) thiswa->hOrdCurrent );
         sprintf( thiswa->sOrderBy , "\n%s", hb_arrayGetCPtr( pIndexRef, ( bDirectionFWD ? ORDER_ASCEND : ORDER_DESEND ) ) );
      }
      else
      {
         //sprintf( thiswa->sOrderBy , "\nORDER BY %c%s%c %s", OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ), ( bDirectionFWD ? "ASC" : "DESC" ) );
         sprintf( thiswa->sOrderBy , "\nORDER BY A.%c%s%c %s", OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ), ( bDirectionFWD ? "ASC" : "DESC" ) );
      }
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getMissingColumn( SQLEXAREAP thiswa, PHB_ITEM pFieldData, LONG lFieldPosDB )
{
   PHB_ITEM pFieldStruct;
   char * colName;
   char sSql[DEFAULT_INDEX_COLUMN_MAX_LEN];
   HB_ERRCODE res;
//    LONG lLen, lLenOut, lInitBuff ;
//    char *  bBuffer;
//    char *bOut=NULL;
//    int     iReallocs  = 0;
//    int iError = 0;
//    char        buffer[ 2 ];
   LONG lType;

   pFieldStruct = hb_arrayGetItemPtr( thiswa->aFields, lFieldPosDB );

   if( thiswa->colStmt[lFieldPosDB - 1] == NULL )
   {
      res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->colStmt[lFieldPosDB - 1]) );

      if ( CHECK_SQL_N_OK( res ) )
      {
         return (HB_FAILURE);
      }

      colName      = QualifyName( hb_arrayGetC( pFieldStruct, FIELD_NAME ), thiswa );

      if (thiswa->bIsSelect)
	  {
	     sprintf( sSql, "SELECT %c%s%c FROM (%s) WHERE %c%s%c = ?", OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ),
                                                               thiswa->szDataFileName,
                                                               OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ) );

      }
      else
      {
      sprintf( sSql, "SELECT %c%s%c FROM %s WHERE %c%s%c = ?", OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ),
                                                               thiswa->sTable,
                                                               OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ) );
      }
      hb_xfree( colName);

      res = SQLPrepare( thiswa->colStmt[lFieldPosDB - 1], (SQLCHAR *) sSql, SQL_NTS );

      if ( CHECK_SQL_N_OK( res ) )
      {
         return (HB_FAILURE);
      }

      res = SQLBindParameter( thiswa->colStmt[lFieldPosDB - 1], 1, SQL_PARAM_INPUT, SQL_C_ULONG, SQL_INTEGER, 15, 0, &(thiswa->lCurrentRecord), 0, NULL );

      if ( CHECK_SQL_N_OK( res ) )
      {
         return (HB_FAILURE);
      }
   }

   thiswa->lCurrentRecord = GetCurrentRecordNum( thiswa );     // Feed bound parameter

   res = SQLExecute( thiswa->colStmt[lFieldPosDB - 1] );

   if ( CHECK_SQL_N_OK( res ) )
   {
      odbcErrorDiagRTE( thiswa->colStmt[lFieldPosDB - 1], "getMissingColumn/SQLExecute", sSql, res, __LINE__, __FILE__ );
      SQLFreeStmt( thiswa->colStmt[lFieldPosDB - 1], SQL_CLOSE );
      return (HB_FAILURE);
   }

   // Now fetch and store result in pFieldData

   res = SQLFetch( thiswa->colStmt[lFieldPosDB - 1] );
   if ( res != SQL_SUCCESS )
   {
	   if (res == SQL_ERROR)
	   {
      odbcErrorDiagRTE( thiswa->colStmt[lFieldPosDB - 1], "getMissingColumn/SQLFetch", sSql, res, __LINE__, __FILE__ );
      SQLFreeStmt( thiswa->colStmt[lFieldPosDB - 1], SQL_CLOSE );
      return (HB_FAILURE);
   }
   }

            lType = ( LONG ) hb_arrayGetNL(pFieldStruct, FIELD_DOMAIN );
   odbcGetData( ( HSTMT ) ( HSTMT )thiswa->colStmt[lFieldPosDB - 1],hb_arrayGetItemPtr( thiswa->aFields, lFieldPosDB ),pFieldData,  0,  thiswa->nSystemID, FALSE, 1 );
//   odbcFieldGet(hb_arrayGetItemPtr( thiswa->aFields, lFieldPosDB ), pFieldData, (char * ) bBuffer, lLenOut, 0, thiswa->nSystemID, FALSE );             

   SQLFreeStmt( thiswa->colStmt[lFieldPosDB - 1], SQL_CLOSE );

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE SetBindValue( PHB_ITEM pFieldData, COLUMNBINDP BindStructure, HSTMT hStmt )
{
   BOOL bEmpty          = SR_itemEmpty( pFieldData );
   SQLRETURN res;

   switch ( BindStructure->iCType )
   {
      case SQL_C_CHAR:
      {
         int nTrim, i;
         int size = (int) hb_itemGetCLen( pFieldData );
         const char * pszText = hb_itemGetCPtr( pFieldData );

         nTrim = size;

         // RTrim() the string value

         for (i = (size - 1); i >= 0; i-- )
         {
            if( pszText[i] == '\0' || pszText[i] != ' ' )
            {
               nTrim = i+1;
               break;
            }
         }

         if( i < 0 )
         {
            nTrim = 0;
         }
         if( nTrim == 0 )
         {
            BindStructure->asChar.value[ 0 ] = '\0';
            if( !BindStructure->isBoundNULL )
            {
               BindStructure->lIndPtr     = SQL_NULL_DATA;
               BindStructure->isBoundNULL = TRUE;
            }
         }
         else
         {
            hb_xmemcpy( BindStructure->asChar.value, pszText, nTrim );
            BindStructure->asChar.value[ nTrim ] = '\0';

            if( BindStructure->isBoundNULL )
            {
               BindStructure->isBoundNULL = FALSE;
               BindStructure->lIndPtr     = SQL_NTS;
            }
         }
         BindStructure->asChar.size = nTrim;
         break;
      }
      case SQL_C_BINARY:
      {
         int nTrim, i;
         int size = (int) hb_itemGetCLen( pFieldData );
         const char * pszText = hb_itemGetCPtr( pFieldData );

         nTrim = size;

         // RTrim() the string value

         for (i = (size -1); i >= 0; i-- )
         {
            if( pszText[i] == '\0' || pszText[i] != ' ' )
            {
               nTrim = i+1;
               break;
            }
         }

         if( i < 0 )
         {
            nTrim = 0;
         }

         if( nTrim >= BindStructure->asChar.size_alloc )
         {
            BindStructure->asChar.value      = (SQLCHAR *) hb_xrealloc(  BindStructure->asChar.value, nTrim + 1 );
            BindStructure->asChar.size_alloc = nTrim + 1;
         }

         if( nTrim >= BindStructure->asChar.size_alloc || nTrim > BindStructure->asChar.size )
         {
            hb_xmemcpy( BindStructure->asChar.value, pszText, nTrim );
            BindStructure->asChar.value[ nTrim ] = '\0';
            BindStructure->lIndPtr               = SQL_NTS;

            // Pointer may be changed during realloc, so parameter should be re-bound
            res = SQLBindParameter( hStmt,
                                    BindStructure->iParNum,
                                    SQL_PARAM_INPUT,
                                    SQL_C_CHAR,
                                    SQL_LONGVARCHAR,
                                    BindStructure->asChar.size_alloc,
                                    0,
                                    BindStructure->asChar.value,
                                    0,
                                    &(BindStructure->lIndPtr) );

            if ( CHECK_SQL_N_OK( res ) )
            {
               odbcErrorDiagRTE( hStmt, "SetBindValue", "", res, __LINE__, __FILE__ );
               return HB_FAILURE;
            }
            break;
         }

         if( nTrim == 0 )
         {
            BindStructure->asChar.value[ 0 ] = '\0';
         }
         else
         {
            hb_xmemcpy( BindStructure->asChar.value, pszText, nTrim );
            BindStructure->asChar.value[ nTrim ] = '\0';
         }
         BindStructure->asChar.size = nTrim;
         break;
      }
      case SQL_C_DOUBLE:
      {
         if( (!bEmpty) && BindStructure->isBoundNULL && hStmt )     // Param was NULL, should be re-bound
         {
            BindStructure->isBoundNULL     = FALSE;
            BindStructure->lIndPtr         = 0;
         }
         else if( bEmpty && (!(BindStructure->isBoundNULL)) && hStmt )
         {
            BindStructure->lIndPtr      = SQL_NULL_DATA;
            BindStructure->isBoundNULL  = TRUE;
            break;
         }
         else if( bEmpty && BindStructure->isBoundNULL )
         {
            break;
         }

         BindStructure->asNumeric = (SQLDOUBLE) hb_itemGetND( pFieldData );
         break;
      }
      case SQL_C_TYPE_DATE:
      {
         int iYear, iMonth, iDay;

         if( (!bEmpty) && BindStructure->isBoundNULL && hStmt )     // Param was NULL, should be re-bound
         {
            BindStructure->isBoundNULL     = FALSE;
            BindStructure->lIndPtr         = 0;
         }
         else if( bEmpty && (!(BindStructure->isBoundNULL)) && hStmt )
         {
            BindStructure->lIndPtr      = SQL_NULL_DATA;
            BindStructure->isBoundNULL  = TRUE;
            break;
         }
         else if( bEmpty && BindStructure->isBoundNULL )
         {
            break;
         }

         hb_dateDecode( hb_itemGetDL( pFieldData ), &iYear, &iMonth, &iDay );
         BindStructure->asDate.year  = (SQLSMALLINT) iYear;
         BindStructure->asDate.month = (SQLUINTEGER) iMonth;
         BindStructure->asDate.day   = (SQLUINTEGER) iDay;
         break;
      }
      case SQL_C_TYPE_TIMESTAMP:
      {
         int iYear, iMonth, iDay;
         int iHour, iMinute;
         BOOL bEmpty = SR_itemEmpty( pFieldData );
//DebugBreak();
         if( (!bEmpty) && BindStructure->isBoundNULL && hStmt )     // Param was NULL, should be re-bound
         {
            BindStructure->isBoundNULL  = FALSE;
            BindStructure->lIndPtr      = 0;
         }
         else if( bEmpty && (!(BindStructure->isBoundNULL)) && hStmt )
         {
            BindStructure->lIndPtr      = SQL_NULL_DATA;
            BindStructure->isBoundNULL  = TRUE;
            break;
         }
         else if( bEmpty && BindStructure->isBoundNULL )
         {
            break;
         }

         {
#ifdef __XHARBOUR__
            // hb_dateDecode( pFieldData->item.asDate.value, &iYear, &iMonth, &iDay );
            //hb_timeDecode( pFieldData->item.asDate.time, piHour, piMin, pdSec );         
            //DebugBreak();
            double seconds;
            hb_datetimeDecode(pFieldData->item.asDate.value,  pFieldData->item.asDate.time,
                              &iYear, &iMonth, &iDay,
                              &iHour, &iMinute, &seconds );
#else
            long lJulian, lMilliSec;
            int seconds, millisec;
            hb_itemGetTDT( pFieldData, &lJulian, &lMilliSec );
            hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
            hb_timeDecode( lMilliSec, &iHour, &iMinute, &seconds, &millisec ); 
#endif
            BindStructure->asTimestamp.year     = (SQLSMALLINT) iYear;
            BindStructure->asTimestamp.month    = (SQLUINTEGER) iMonth;
            BindStructure->asTimestamp.day      = (SQLUINTEGER) iDay;
            BindStructure->asTimestamp.hour     = (SQLUINTEGER) iHour;
            BindStructure->asTimestamp.minute   = (SQLUINTEGER) iMinute;
            BindStructure->asTimestamp.second   = (SQLUSMALLINT)seconds;
            BindStructure->asTimestamp.fraction = 0;
         }
         break;
      }
      case SQL_C_BIT:
      {
         BindStructure->asLogical = (SQLCHAR) hb_itemGetL( pFieldData );
         break;
      }
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE SetBindEmptylValue( COLUMNBINDP BindStructure )
{
   switch ( BindStructure->iCType )
   {
      case SQL_C_CHAR:
      {
         BindStructure->asChar.value[0] = ' ';
         BindStructure->asChar.value[1] = '\0';
         BindStructure->asChar.size     = 1;
         break;
      }
      case SQL_C_BINARY:
      {
         BindStructure->asChar.value[0] = '\0';
         break;
      }
      case SQL_C_DOUBLE:
      {
         BindStructure->asNumeric = 0;
         break;
      }
      case SQL_C_TYPE_TIMESTAMP:
      {
	      //DebugBreak();
         if( !BindStructure->isBoundNULL )
         {
            BindStructure->lIndPtr     = SQL_NULL_DATA;
            BindStructure->isBoundNULL = TRUE;
         }
         break;
      }
      case SQL_C_TYPE_DATE:
      {
         if( !BindStructure->isBoundNULL )
         {
            BindStructure->lIndPtr     = SQL_NULL_DATA;
            BindStructure->isBoundNULL = TRUE;
         }
         break;
      }
      case SQL_C_BIT:
      {
         BindStructure->asLogical = FALSE;
         break;
      }
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

void ReleaseInsertRecordStructure( SQLEXAREAP thiswa, int iCols )
{
   COLUMNBINDP InsertRecord;
   if( thiswa->InsertRecord )
   {
      int n;
      if( iCols == 0 )
      {
         iCols = (int) hb_arrayLen( thiswa->aFields );
      }
      InsertRecord  = thiswa->InsertRecord;

      for (n=0; n < iCols; n++)
      {
         if( InsertRecord->asChar.value )
         {
            hb_xfree( InsertRecord->asChar.value );
         }
         if( InsertRecord->colName )
         {
            hb_xfree( InsertRecord->colName );
         }
         InsertRecord++;
      }
      hb_xfree( thiswa->InsertRecord );
   }
}

/*------------------------------------------------------------------------*/

void ReleaseCurrRecordStructure( SQLEXAREAP thiswa, int iCols )
{
   COLUMNBINDP CurrRecord;

   if( thiswa->CurrRecord )
   {
      int n;
      if( iCols == 0 )
      {
         iCols = (int) hb_arrayLen( thiswa->aFields );
      }
      CurrRecord  = thiswa->CurrRecord;

      for (n=0; n < iCols; n++)
      {
         if( CurrRecord->asChar.value )
         {
            hb_xfree( CurrRecord->asChar.value );
         }
         if( CurrRecord->colName )
         {
            hb_xfree( CurrRecord->colName );
         }
         CurrRecord++;
      }
      hb_xfree( thiswa->CurrRecord );
   }
}

/*------------------------------------------------------------------------*/

void ReleaseColStatements( SQLEXAREAP thiswa, int iCols )
{
   int i;
   if ( thiswa->colStmt )
   {
      if( iCols == 0 )
      {
         iCols = (int) hb_arrayLen( thiswa->aFields );
      }

      for (i=0; i < iCols; i++)
      {
         if ( thiswa->colStmt[i] )
         {
            SQLFreeStmt( thiswa->colStmt[i], SQL_DROP );
         }
      }
      hb_xfree( thiswa->colStmt );
   }
}

/*------------------------------------------------------------------------*/

void SetColStatements( SQLEXAREAP thiswa )
{
   thiswa->colStmt = (HSTMT *) hb_xgrab(hb_arrayLen( thiswa->aFields ) * sizeof( HSTMT ) );
   memset( thiswa->colStmt, 0,  hb_arrayLen( thiswa->aFields ) * sizeof( HSTMT ) );
}

/*------------------------------------------------------------------------*/

void ReleaseIndexBindStructure( SQLEXAREAP thiswa )
{
   int i, n, iCols;
   INDEXBINDP IndexBind;
   for (i=0; i < MAX_INDEXES; i++)
   {
      IndexBind = thiswa->IndexBindings[i];
      if (IndexBind)
      {
         iCols = IndexBind->iIndexColumns;
         for (n=0; n < iCols; n++)
         {
            if ( IndexBind->SkipFwdStmt )
            {
               SQLFreeStmt( IndexBind->SkipFwdStmt, SQL_DROP );
            }
            if ( IndexBind->SkipBwdStmt )
            {
               SQLFreeStmt( IndexBind->SkipBwdStmt, SQL_DROP );
            }
            if ( IndexBind->SeekFwdStmt )
            {
               SQLFreeStmt( IndexBind->SeekFwdStmt, SQL_DROP );
            }
            if ( IndexBind->SeekBwdStmt )
            {
               SQLFreeStmt( IndexBind->SeekBwdStmt, SQL_DROP );
            }
            IndexBind++;
         }
         hb_xfree( thiswa->IndexBindings[i] );
         thiswa->IndexBindings[i] = NULL;
      }
   }
}

/*------------------------------------------------------------------------*/

COLUMNBINDP GetBindStruct( SQLEXAREAP thiswa, INDEXBINDP IndexBind )
{
   COLUMNBINDP pBind = thiswa->CurrRecord;
   pBind += (IndexBind->lFieldPosDB -1);     // Place offset
   return pBind;
}

/*------------------------------------------------------------------------*/

static void BindAllIndexStmts( SQLEXAREAP thiswa )
{
   HSTMT hStmt;
   INDEXBINDP IndexBind, IndexBindParam;
   COLUMNBINDP BindStructure;
   int iCol, iBind, iLoop;
   SQLRETURN res = SQL_ERROR;
   char * sSql;

   if( thiswa->hOrdCurrent == 0 )
   {
      // Natural order

      IndexBind = thiswa->IndexBindings[0];
      hStmt     = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
      sSql      = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdSql : IndexBind->SkipBwdSql;

      BindStructure = GetBindStruct( thiswa, IndexBind );

      res = SQLBindParameter( hStmt, 1, SQL_PARAM_INPUT,
                                          BindStructure->iCType,
                                          BindStructure->iSQLType,
                                          BindStructure->ColumnSize,
                                          BindStructure->DecimalDigits,
                                          &(BindStructure->asNumeric), 0, NULL );

      if ( CHECK_SQL_N_OK( res ) )
      {
         odbcErrorDiagRTE( hStmt, "BindAllIndexStmts", sSql, res, __LINE__, __FILE__ );
      }
   }
   else
   {
      IndexBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];

      for (iCol = 1; iCol <= thiswa->indexColumns; iCol++)
      {
         hStmt          = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
         sSql           = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdSql : IndexBind->SkipBwdSql;
         IndexBindParam = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
         iBind          = 1;

         for (iLoop = 1; iLoop <= IndexBind->iLevel; iLoop++ )
         {
            BindStructure = GetBindStruct( thiswa, IndexBindParam );
            if( !BindStructure->isArgumentNull )
            {
               switch (BindStructure->iCType)
               {
                  case SQL_C_CHAR:
                  {
                     res = SQLBindParameter( hStmt, iBind, SQL_PARAM_INPUT,
                                             BindStructure->iCType,
                                             BindStructure->iSQLType,
                                             BindStructure->ColumnSize,
                                             BindStructure->DecimalDigits,
                                             BindStructure->asChar.value, 0, NULL );
                     break;
                  }
                  case SQL_C_DOUBLE:
                  {
                     res = SQLBindParameter( hStmt, iBind, SQL_PARAM_INPUT,
                                             BindStructure->iCType,
                                             BindStructure->iSQLType,
                                             BindStructure->ColumnSize,
                                             BindStructure->DecimalDigits,
                                             &(BindStructure->asNumeric), 0, NULL );
                     break;
                  }
                  case SQL_C_TYPE_TIMESTAMP:
                  {
                     //DebugBreak();
                     //res = SQLBindParameter( hStmt, iBind, SQL_PARAM_INPUT,
                     //                        SQL_C_TYPE_TIMESTAMP,
                     //                        SQL_TYPE_TIMESTAMP,
                     //                        SQL_TIMESTAMP_LEN,
                     //                        0,
                     //                        &(BindStructure->asTimestamp), 0, 0 );

                     res = SQLBindParameter( hStmt, iBind, SQL_PARAM_INPUT,
                                             SQL_C_TYPE_TIMESTAMP,
                                             SQL_TYPE_TIMESTAMP,
                                             SQL_TIMESTAMP_LEN,
                                             thiswa->nSystemID == SYSTEMID_MSSQL7 ||thiswa->nSystemID == SYSTEMID_AZURE ? 3 : 0 ,
                                             &(BindStructure->asTimestamp), 0, 0 );
                     break;
                  }
                  case SQL_C_TYPE_DATE:
                  {
                     res = SQLBindParameter( hStmt, iBind, SQL_PARAM_INPUT,
                                             SQL_C_TYPE_DATE,
                                             SQL_TYPE_DATE,
                                             SQL_DATE_LEN,
                                             0,
                                             &(BindStructure->asDate), 0, 0 );
                     break;
                  }
                  case SQL_C_BIT:
                  {
                     res = SQLBindParameter( hStmt, iBind, SQL_PARAM_INPUT,
                                             BindStructure->iCType,
                                             BindStructure->iSQLType,
                                             BindStructure->ColumnSize,
                                             BindStructure->DecimalDigits,
                                             &(BindStructure->asLogical), 0, NULL );
                     break;
                  }
               }
               if ( CHECK_SQL_N_OK( res ) )
               {
                  odbcErrorDiagRTE( hStmt, "BindAllIndexStmts()", sSql, res, __LINE__, __FILE__ );
               }
               iBind++;
            }
            IndexBindParam++;
         }
         IndexBind++;
      }
   }
}

/*------------------------------------------------------------------------*/

static void FeedCurrentRecordToBindings( SQLEXAREAP thiswa )
{
   PHB_ITEM pFieldData;
   int iCol;
   INDEXBINDP IndexBind;
   COLUMNBINDP BindStructure;
   BOOL newFieldData;

   if( thiswa->hOrdCurrent == 0 )
   {
      // Natural order, pretty simple
      BindStructure            = GetBindStruct( thiswa, thiswa->IndexBindings[0] );
      BindStructure->asNumeric = (SQLDOUBLE) GetCurrentRecordNum( thiswa );
   }
   else
   {
      IndexBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];

      for (iCol = 1; iCol <= thiswa->indexColumns; iCol++)
      {
         BindStructure = GetBindStruct( thiswa, IndexBind );

         if( BindStructure->lFieldPosWA > 0 )
         {
            /*  Get item value from Workarea */
            pFieldData   = hb_arrayGetItemPtr( thiswa->aBuffer, BindStructure->lFieldPosWA );
            newFieldData = FALSE;
         }
         else
         {
            pFieldData   = hb_itemNew( NULL );
            newFieldData = TRUE;
         }

         if ( IndexBind->lFieldPosDB == ( LONG ) (thiswa->ulhRecno) )
         {
            hb_itemPutNL( pFieldData, thiswa->recordList[thiswa->recordListPos] );
         }

         if ( HB_IS_NIL( pFieldData ) )
         {
            getMissingColumn( thiswa, pFieldData, IndexBind->lFieldPosDB );
         }

         // Check if column is NULL

         if( SR_itemEmpty( pFieldData ) && ( ( (thiswa->nSystemID == SYSTEMID_POSTGR) && HB_IS_DATE( pFieldData ) )
                                        || ( (thiswa->nSystemID != SYSTEMID_POSTGR) && ( !HB_IS_LOGICAL( pFieldData ) ) ) ) )
         {
            if( BindStructure->isNullable && BindStructure->isArgumentNull )
            {
               // It is STILL NULL, so no problem
               HSTMT hStmt = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
               SetBindValue( pFieldData, BindStructure, hStmt );
            }
            else if( !BindStructure->isNullable )
            {
               // Just get an empty value to be bound, because column is NOT nullable
               SetBindEmptylValue( BindStructure );
            }
            else if( BindStructure->isNullable && ( !BindStructure->isArgumentNull ) )
            {
               // Now we have a problem. Current record column is NULL, database accept NULLS
               // but query in NOT prepared for NULL values. So we must RE-PREPARE all queries

               thiswa->bConditionChanged1 = TRUE;
               CreateSkipStmt( thiswa );
               BindAllIndexStmts( thiswa );
               FeedCurrentRecordToBindings( thiswa );     // Recursive call
            }
         }
         else
         {
            HSTMT hStmt = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
            SetBindValue( pFieldData, BindStructure, hStmt );
         }

         if (newFieldData)
         {
            hb_itemRelease( pFieldData );
         }
         IndexBind++;
      }
   }
}

/*------------------------------------------------------------------------*/

void SolveFilters( SQLEXAREAP thiswa, BOOL bWhere )
{
   /*
   *  Resolve SET FILTER TO
   */

   char * temp;

   /////////////////////////////////////////////////////////////////////////////

   char * szfor= getMessageC( thiswa->oWorkArea, "CFOR" );
   if ( szfor )
   {
      if ( szfor[0] )
      {
         if( bWhere )
         {
            temp = hb_strdup( (const char *) thiswa->sWhere );
            sprintf( thiswa->sWhere, "%s AND ( %s )", temp, szfor );
            hb_xfree( temp );
         }
         else
         {
            sprintf( thiswa->sWhere, "\nWHERE ( %s )", szfor );
            bWhere = TRUE;
         }
         hb_xfree( szfor );
      }
   }

   /////////////////////////////////////////////////////////////////////////////
      
   if( thiswa->sqlfilter )
   {
      char * sFilter = getMessageC( thiswa->oWorkArea, "CFILTER" );
	  if ( sFilter )
	  {
	     if ( sFilter[0] )
	     {
      
            if( bWhere )
            {
               temp = hb_strdup( (const char *) thiswa->sWhere );
               sprintf( thiswa->sWhere, "%s AND ( %s )", temp, sFilter );
               hb_xfree( temp );
            }
            else
            {
               sprintf( thiswa->sWhere, "\nWHERE ( %s )", sFilter );
               bWhere = TRUE;
            }
            hb_xfree( sFilter  );
         }
      }
      
   }
   else
   {
	  char * sFilter = getMessageC( thiswa->oWorkArea, "CFILTER" );
	  if ( sFilter )
	  {
	     if ( sFilter[0] )
	     {
            if( bWhere )
            {
               temp = hb_strdup( (const char *) thiswa->sWhere );
               sprintf( thiswa->sWhere, "%s AND ( %s )", temp, sFilter );
               hb_xfree( temp );
            }
            else
            {
               sprintf( thiswa->sWhere, "\nWHERE ( %s )", sFilter );
               bWhere = TRUE;
            }
            hb_xfree( sFilter  );
         }
      }
   }

   /*
   *  Resolve SET SCOPE TO
   */

   if ( thiswa->hOrdCurrent > 0 )
   {
      PHB_ITEM pIndexRef = hb_arrayGetItemPtr( thiswa->aOrders, ( ULONG ) thiswa->hOrdCurrent );
      const char * szFilter = hb_arrayGetCPtr( pIndexRef, SCOPE_SQLEXPR );
      if( szFilter && szFilter[ 0 ] )
      {
         if( bWhere )
         {
            temp = hb_strdup( (const char *) thiswa->sWhere );
            sprintf( thiswa->sWhere, "%s AND ( %s )", temp, szFilter );
            hb_xfree( temp );
         }
         else
         {
            sprintf( thiswa->sWhere, "\nWHERE ( %s )", szFilter );
            bWhere = TRUE;
         }
      }
   }

   /*
   *  Resolve other restrictors - MISSING!!!
   */
   {
      char * sFilter = getMessageC( thiswa->oWorkArea, "CSCOPE" );
      if( sFilter )
      {
         if( sFilter[0] )
         {
            if( bWhere )
            {
               temp = hb_strdup( (const char *) thiswa->sWhere );
               sprintf( thiswa->sWhere, "%s AND ( %s )", temp, sFilter );
               hb_xfree( temp );
            } else {
               sprintf( thiswa->sWhere, "\nWHERE ( %s )", sFilter );
               bWhere = TRUE;
            }
         }
         hb_xfree( sFilter  );
      }
   }

   {
      char * sFilter = getMessageC( thiswa->oWorkArea, "CFLTUSR" );
      if( sFilter )
      {
         if( sFilter[0] )
         {
            if( bWhere )
            {
               temp = hb_strdup( (const char *) thiswa->sWhere );
               sprintf( thiswa->sWhere, "%s AND ( %s )", temp, sFilter );
               hb_xfree( temp );
            }
            else
            {
               sprintf( thiswa->sWhere, "\nWHERE ( %s )", sFilter );
               /* bWhere = TRUE; */
            }
         }
         hb_xfree( sFilter  );
      }
   }
}

/*------------------------------------------------------------------------*/

void SetIndexBindStructure( SQLEXAREAP thiswa )
{
   PHB_ITEM pColumns, pIndexRef;
   INDEXBINDP IndexBind;
   int i;

   if ( thiswa->hOrdCurrent > 0 )
   {
      pIndexRef = hb_arrayGetItemPtr( thiswa->aOrders, ( ULONG ) thiswa->hOrdCurrent );
      pColumns = hb_arrayGetItemPtr( pIndexRef, INDEX_FIELDS );
      thiswa->indexColumns = hb_arrayLen( pColumns );

      // Alloc memory for binding structures
      thiswa->IndexBindings[ thiswa->hOrdCurrent ] = (INDEXBINDP) hb_xgrab(thiswa->indexColumns * sizeof( INDEXBIND ) );
      memset( thiswa->IndexBindings[ thiswa->hOrdCurrent ], 0, thiswa->indexColumns * sizeof( INDEXBIND ) );

      // Now we should bind all index columns to be used by SKIP

      IndexBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];

      for ( i=1; i <= thiswa->indexColumns; i++ )
      {
         IndexBind->lFieldPosDB     = hb_arrayGetNL( hb_arrayGetItemPtr( pColumns, i ), 2 );
         IndexBind->hIndexOrder     = thiswa->hOrdCurrent;
         IndexBind->iLevel          = i;
         IndexBind->iIndexColumns   = thiswa->indexColumns;
         IndexBind++;
      }
   }
   else
   {
      thiswa->indexColumns = 1;     // Natural order, RECNO
      // Alloc memory for binding structures
      thiswa->IndexBindings[ thiswa->hOrdCurrent ] = (INDEXBINDP) hb_xgrab(thiswa->indexColumns * sizeof( INDEXBIND ) );
      memset( thiswa->IndexBindings[ thiswa->hOrdCurrent ], 0, thiswa->indexColumns * sizeof( INDEXBIND ) );
      IndexBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
      IndexBind->lFieldPosDB     = thiswa->ulhRecno;
      IndexBind->hIndexOrder     = 0;
      IndexBind->iLevel          = 1;
      IndexBind->iIndexColumns   = 1;
   }
}

/*------------------------------------------------------------------------*/

void SetCurrRecordStructure( SQLEXAREAP thiswa )
{
   PHB_ITEM pFieldStruct, pFieldLen, pFieldDec;
   int i, iCols;
   LONG lType;
   char cType;
   COLUMNBINDP BindStructure;
//    BOOL bNullable, bMultiLang, bIsMemo;
   BOOL  bMultiLang;

   iCols = (int) hb_arrayLen( thiswa->aFields );

   thiswa->CurrRecord = (COLUMNBINDP) hb_xgrab(iCols * sizeof( COLUMNBIND ) );
   memset( thiswa->CurrRecord, 0, iCols * sizeof( COLUMNBIND ) );

   BindStructure = thiswa->CurrRecord;

   for( i = 1; i <= iCols; i++ )
   {
      pFieldStruct = hb_arrayGetItemPtr( thiswa->aFields, i );
      pFieldLen    = hb_arrayGetItemPtr( pFieldStruct, FIELD_LEN );
      pFieldDec    = hb_arrayGetItemPtr( pFieldStruct, FIELD_DEC );
      lType        = hb_arrayGetNL( pFieldStruct, FIELD_DOMAIN );
      cType        = ( * hb_arrayGetCPtr( pFieldStruct, FIELD_TYPE ));
      bMultiLang   = hb_arrayGetL( pFieldStruct, FIELD_MULTILANG );
      if ( bMultiLang ) 
         cType = 'M';

      BindStructure->iSQLType        = (int)lType;
      BindStructure->isNullable      = hb_arrayGetL( pFieldStruct, FIELD_NULLABLE );
      BindStructure->isBoundNULL     = FALSE;
      BindStructure->isArgumentNull  = FALSE;
      BindStructure->lFieldPosDB     = i;
      BindStructure->lFieldPosWA     = hb_arrayGetNL( pFieldStruct, FIELD_WAOFFSET );
		BindStructure->ColumnSize      = (SQLUINTEGER) hb_itemGetNI( pFieldLen );
		BindStructure->DecimalDigits   = (SQLSMALLINT) hb_itemGetNI( pFieldDec );
      BindStructure->colName         = QualifyName( hb_arrayGetC( pFieldStruct, FIELD_NAME ), thiswa );
      BindStructure->isMemo          = cType == 'M';

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
         case 'C':
         {
            BindStructure->asChar.value      = (SQLCHAR *) hb_xgrabz(BindStructure->ColumnSize + 1 );
//             memset(BindStructure->asChar.value ,0, BindStructure->ColumnSize + 1 ); // Culik Zero all memory
            BindStructure->asChar.size_alloc = BindStructure->ColumnSize + 1;
            BindStructure->iCType            = SQL_C_CHAR;
            BindStructure->asChar.size       = 0;
            break;
         }
         case 'M':
         {
            BindStructure->iCType            = SQL_C_BINARY;
            BindStructure->asChar.value      = (SQLCHAR *) hb_xgrabz(INITIAL_MEMO_ALLOC );
//             memset(BindStructure->asChar.value ,0, INITIAL_MEMO_ALLOC ); // Culik Zero all memory
            BindStructure->asChar.size_alloc = INITIAL_MEMO_ALLOC;
            BindStructure->asChar.size       = 0;
            BindStructure->asChar.value[0]   = '\0';
            BindStructure->ColumnSize        = 0;
            break;
         }
         case 'N':
         {
            BindStructure->iCType          = SQL_C_DOUBLE;
            break;
         }
         case 'T':
         {
            BindStructure->iCType          = SQL_C_TYPE_TIMESTAMP;    
            break;
         }   
           
         case 'D':
         {
//             BindStructure->iCType          = lType;    // DATE or TIMESTAMP

           // Corrigido 27/12/2013 09:53 - lpereira
           // Estava atribuindo o valor de SYSTEMID_ORACLE para thiswa->nSystemID.
           //if ( thiswa->nSystemID = SYSTEMID_ORACLE )
	        if ( thiswa->nSystemID == SYSTEMID_ORACLE )
	           BindStructure->iCType          = SQL_C_TYPE_TIMESTAMP;        // May be DATE or TIMESTAMP
	        else
               BindStructure->iCType          = lType;        // May be DATE or TIMESTAMP

            break;
         }
         case 'L':
         {
            BindStructure->iCType          = SQL_C_BIT;
            break;
         }
      }
      if (BindStructure->isMultiLang) // culik, se e multiplang, binda como binario
         BindStructure->iCType            = SQL_C_BINARY;
      BindStructure++;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getWhereExpression( SQLEXAREAP thiswa, int iListType )
{
   // This function creates WHERE expression to some workarea movment methods,
   // including dbGoTop()/dbGobottom() and dbSkip()

   BOOL bWhere = FALSE;
   int iCol;
   PHB_ITEM pFieldData, pTemp;
   BOOL bArgumentIsNull;
   BOOL bDirectionFWD;
   COLUMNBINDP BindStructure;
   char * temp;
   // Culik Let Clear all memorym this is more eficient and safe the adding an \0 to position 0
   memset( thiswa->sWhere, 0 ,MAX_SQL_QUERY_LEN / 10 * sizeof( char ) );
   //thiswa->sWhere[0] = '\0';
   thiswa->bConditionChanged1 = FALSE;

   // Resolve record or index navigation

   if( iListType == LIST_SKIP_FWD || iListType == LIST_SKIP_BWD )
   {
      INDEXBINDP IndexBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];

      thiswa->recordListDirection = ( iListType == LIST_SKIP_FWD ? LIST_FORWARD : LIST_BACKWARD );
      bDirectionFWD               = iListType == LIST_SKIP_FWD;

      if( thiswa->bReverseIndex )
      {
         bDirectionFWD = !bDirectionFWD;
      }

      if( thiswa->hOrdCurrent == 0 )      // Natural order
      {
         sprintf( thiswa->sWhere, "\nWHERE A.%c%s%c %s ?", OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),
                                                           bDirectionFWD ? ">=" : "<=" );
         BindStructure              = GetBindStruct( thiswa, IndexBind );
         BindStructure->asNumeric   = (SQLDOUBLE) GetCurrentRecordNum( thiswa );
         bWhere = TRUE;
      }
      else
      {
         for (iCol = 1; iCol <= thiswa->indexLevel; iCol++)
         {
            BindStructure = GetBindStruct( thiswa, IndexBind );

            pTemp        = NULL;
            pFieldData   = NULL;

            if( BindStructure->lFieldPosWA > 0 )
            {
               /*  Get item value from Workarea */
               pFieldData   = hb_arrayGetItemPtr( thiswa->aBuffer, BindStructure->lFieldPosWA );
            }

            if ( BindStructure->lFieldPosDB == ( LONG ) (thiswa->ulhRecno) )
            {
               pTemp = hb_itemNew( NULL );
               hb_itemPutNL( pTemp, thiswa->recordList[thiswa->recordListPos] );
               pFieldData = pTemp;
            }
            else if ( BindStructure->lFieldPosWA == 0 || (pFieldData && HB_IS_NIL( pFieldData )) )
            {
               if( !pFieldData )
               {
                  pTemp = hb_itemNew( NULL );
                  pFieldData = pTemp;
               }
               getMissingColumn( thiswa, pFieldData, BindStructure->lFieldPosDB );
               /*  Get the synthetic index item value from database */
            }

            bArgumentIsNull = BindStructure->isNullable && IsItemNull( pFieldData, thiswa );

            if( iCol == thiswa->indexLevel )
            {
               BindStructure->isArgumentNull  = bArgumentIsNull;

               if( !bArgumentIsNull )
               {
                  // Bind column value only if argument is NOT null
                  HSTMT hStmt = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
                  SetBindValue( pFieldData, BindStructure, hStmt );
               }
            }

            if( bArgumentIsNull )               // This is the same to be directly used or prepared
            {
               if( BindStructure->iCType == SQL_C_DOUBLE )     // If NUMERIC
               {
                  temp = hb_strdup( (const char *) thiswa->sWhere );
                  sprintf( thiswa->sWhere, "%s %s ( A.%c%s%c %s %s OR A.%c%s%c IS NULL )", bWhere ? temp : "\nWHERE", bWhere ? "AND" : "",
                                                             OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ),
                                                             iCol == thiswa->indexLevel ? ( bDirectionFWD ? ">=" : "<=" ) : "IS",
                                                             iCol == thiswa->indexLevel ? "0" : "NULL",
                                                             OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ) );
                  bWhere = TRUE;
                  hb_xfree( temp );
               }
               else
               {
                  if (iCol == thiswa->indexLevel && bDirectionFWD )
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
                     bWhere = TRUE;
                     hb_xfree( temp );
                  }
               }
            }
            else
            {
               temp = hb_strdup( (const char *) thiswa->sWhere );
               sprintf( thiswa->sWhere, "%s %s A.%c%s%c %s ?", bWhere ? temp : "\nWHERE", bWhere ? "AND" : "",
                                                          OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ),
                                                          iCol == thiswa->indexLevel ? ( bDirectionFWD ? ">=" : "<=" ) : "=" );
               bWhere = TRUE;
               hb_xfree( temp );
            }
            if ( pTemp )
            {
               hb_itemRelease( pTemp );
            }
            IndexBind++;
         }
      }
   }
   else
   {
      thiswa->indexLevel          = -1;      // Reset index navigation
   }

   if( iListType == LIST_FROM_TOP )
   {
      thiswa->recordListDirection = LIST_FORWARD;
   }
   else if( iListType == LIST_FROM_BOTTOM )
   {
      thiswa->recordListDirection = LIST_BACKWARD;
   }

   SolveFilters( thiswa, bWhere );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

HB_ERRCODE getWorkareaParams( SQLEXAREAP thiswa )
{

   ULONG lCnnType;
   if( !thiswa->oSql )
   {
      thiswa->oSql            = getMessageItem( thiswa->oWorkArea, "OSQL" );
      thiswa->aFields         = getMessageItem( thiswa->oWorkArea, "AFIELDS" );
      thiswa->hDbc            = (HDBC) getMessagePtr( thiswa->oSql, "HDBC" );
      thiswa->nSystemID       = getMessageNL( thiswa->oSql, "NSYSTEMID" );
      thiswa->sTable          = getMessageC( thiswa->oWorkArea, "CQUALIFIEDTABLENAME" );
      thiswa->sOwner          = getMessageC( thiswa->oWorkArea, "COWNER" );
      thiswa->sRecnoName      = getMessageC( thiswa->oWorkArea, "CRECNONAME" );
      thiswa->sDeletedName    = getMessageC( thiswa->oWorkArea, "CDELETEDNAME" );
      thiswa->iTCCompat       = getMessageNI( thiswa->oWorkArea, "NTCCOMPAT" );
      thiswa->bHistoric       = getMessageL( thiswa->oWorkArea, "LHISTORIC" );

      thiswa->sRecnoName   = QualifyName( thiswa->sRecnoName, thiswa );
      thiswa->sDeletedName = QualifyName( thiswa->sDeletedName, thiswa );

      SetColStatements( thiswa );
   }

   if( !thiswa->bConnVerified )
   {
      lCnnType = getMessageNL( thiswa->oSql, "NCONNECTIONTYPE" );

      if( !(lCnnType == CONNECT_ODBC || lCnnType == CONNECT_ODBC_QUERY_ONLY) )
      {
         commonError( (AREAP) thiswa, EG_OPEN, ESQLRDD_OPEN, "SQLEX supports only ODBC connections." );
         return HB_FAILURE;
      }
      thiswa->bConnVerified = TRUE;
   }
      thiswa->bIsSelect        = getMessageL( thiswa->oWorkArea, "LTABLEISSELECT" ); 
    //  if  (!thiswa->hStmtInsert)
     //SQLAllocHandle( SQL_HANDLE_STMT, (HDBC) thiswa->hDbc, &(thiswa->hStmtInsert) );
      
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getPreparedRecordList( SQLEXAREAP thiswa, int iMax ) // Returns TRUE if any result found
{
   SQLRETURN res;
   int i, recordListChanged;
   INDEXBINDP IndexBind;
   HSTMT hStmt;
   ULONG lRecord;
   char * sSql;

   IndexBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
   // not nedded
   //IndexBind += (thiswa->indexLevel - 1);    // Place Offset

   hStmt = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
   sSql  = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdSql : IndexBind->SkipBwdSql;

   res = SQLExecute( hStmt );

   if ( CHECK_SQL_N_OK( res ) )
   {
      odbcErrorDiagRTE( hStmt, "getPreparedRecordList", sSql, res, __LINE__, __FILE__ );
      SQLCloseCursor( hStmt );

      return (HB_FAILURE);
   }

   // Should SKIP over results to ignore current line
   // Note: If we're in a deeper simplification level in current index
   //       navigation, we can have MANY processed lines in result set,
   //       so we should FETCH until find the current line to re-start
   //       appending records to RecordList

   // Current line is always in result set if we are SKIPPING

   do
   {
      res = SQLFetch( hStmt );
      if ( res != SQL_SUCCESS )
      {
         // Ops, where are previously retrieved record ?
         // Run query again and try to find it in result
         // set since it can be deleted by other user - MISSING!!!
         SQLFreeStmt( hStmt, SQL_CLOSE );
         return HB_RETRY;
      }

      if ( CHECK_SQL_N_OK( res ) )
      {
         return (HB_FAILURE);       // Any other error means a fault in SQL statement
      }
      res = SQLGetData( hStmt, 1, SQL_C_ULONG, &lRecord, sizeof( SQL_C_ULONG ), NULL );
      if ( CHECK_SQL_N_OK( res ) )
      {
         return (HB_FAILURE);       // Any other error means a fault in SQL statement
      }
   }
   while ( thiswa->lCurrentRecord != lRecord );

   recordListChanged = 0;

   for( i=0; i<iMax; i++ )
   {
      res = SQLFetch( hStmt );
      if ( res != SQL_SUCCESS )
      {
         if( i > 0 && thiswa->indexLevel == 1 && res == SQL_NO_DATA_FOUND )
         {
            if( thiswa->recordListDirection == LIST_FORWARD )
            {
               thiswa->lEofAt = thiswa->recordList[i-1];
            }
            else
            {
               thiswa->lBofAt = thiswa->recordList[i-1];
            }
         }
         else if( i == 0 && thiswa->indexLevel == 1 && res == SQL_NO_DATA_FOUND && thiswa->recordListSize > 0 )
         {
            if( thiswa->recordListDirection == LIST_FORWARD )
            {
               thiswa->lEofAt = thiswa->recordList[thiswa->recordListPos];
            }
            else
            {
               thiswa->lBofAt = thiswa->recordList[thiswa->recordListPos];
            }
         }
         break;
      }
      res = SQLGetData( hStmt, 1, SQL_C_ULONG, &(thiswa->recordList[i]), sizeof( SQL_C_ULONG ), NULL );
      if( res == SQL_ERROR )
      {
         SQLFreeStmt( hStmt, SQL_CLOSE );
         return (HB_FAILURE);
      }

      recordListChanged++;

      if( thiswa->ulhDeleted > 0 )
      {
         SQLCHAR szValue[2];
         res = SQLGetData( hStmt, 2, SQL_C_CHAR, szValue, 2, NULL );
         if( res == SQL_ERROR )
         {
            SQLFreeStmt( hStmt, SQL_CLOSE );
            return (HB_FAILURE);
         }
         else
         {
            if( szValue[0] == 0 )
            {
               thiswa->deletedList[i] = ' ';    // MySQL driver climps spaces from right side
            }
            else
            {
               thiswa->deletedList[i] = szValue[0];
            }
         }
      }
      else
      {
         thiswa->deletedList[i] = ' ';
      }
   }

   SQLFreeStmt( hStmt, SQL_CLOSE );

   if( recordListChanged )
   {
      thiswa->recordListSize = (ULONG) i;
      thiswa->recordListPos  = 0;
      return RESULTSET_OK;
   }

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getRecordList( SQLEXAREAP thiswa, int iMax ) // Returns TRUE if any result found
{
   SQLRETURN res;
   int i, recordListChanged;

   res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmt) );

   if ( CHECK_SQL_N_OK( res ) )
   {
      return (HB_FAILURE);
   }

   res = SQLExecDirect( thiswa->hStmt, (SQLCHAR *) thiswa->sSql, SQL_NTS );

   if ( res == SQL_ERROR )
   {
      return (HB_FAILURE);    // It means a fault in SQL statement
   }

   recordListChanged = 0;
   for( i=0; i<iMax; i++ )
   {
      res = SQLFetch( thiswa->hStmt );
      if ( res != SQL_SUCCESS )
      {
         if( i > 0 && res == SQL_NO_DATA_FOUND )
         {
            if( thiswa->recordListDirection == LIST_FORWARD )
            {
               thiswa->lEofAt = thiswa->recordList[i-1];
            }
            else
            {
               thiswa->lBofAt = thiswa->recordList[i-1];
            }
         }
         SQLFreeStmt( thiswa->hStmt, SQL_CLOSE );
         break;
      }
      res = SQLGetData( thiswa->hStmt, 1, SQL_C_ULONG, &(thiswa->recordList[i]), sizeof( SQL_C_ULONG ), NULL );
      if( res == SQL_ERROR )
      {
         SQLFreeStmt( thiswa->hStmt, SQL_CLOSE );
         return (HB_FAILURE);
      }

      recordListChanged++;

      if( thiswa->ulhDeleted > 0 )
      {
         SQLCHAR szValue[2];
         res = SQLGetData( thiswa->hStmt, 2, SQL_C_CHAR, szValue, 2, NULL );
         if( res == SQL_ERROR )
         {
            SQLFreeStmt( thiswa->hStmt, SQL_CLOSE );
            return (HB_FAILURE);
         }
         else
         {
            if( szValue[0] == 0 )
            {
               thiswa->deletedList[i] = ' ';    // MySQL driver climps spaces from right side
            }
            else
            {
               thiswa->deletedList[i] = szValue[0];
            }
         }
      }
      else
      {
         thiswa->deletedList[i] = ' ';
      }
   }

   SQLFreeStmt( thiswa->hStmt, SQL_CLOSE );

   if( recordListChanged )
   {
      thiswa->recordListSize = (ULONG) i;
      thiswa->recordListPos  = 0;
      return RESULTSET_OK;
   }

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getFirstColumnAsLong( SQLEXAREAP thiswa, long * szValue ) // Returns OK if result set could be get
{
   SQLRETURN res;

   res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmt) );

   if( CHECK_SQL_N_OK( res ) )
      return (HB_FAILURE);

   res = SQLExecDirect( thiswa->hStmt, ( SQLCHAR * ) thiswa->sSql, SQL_NTS );

   if( res == SQL_ERROR )
      return (HB_FAILURE);    // It means a fault in SQL statement

   res = SQLFetch( thiswa->hStmt );
   if( res == SQL_ERROR )
   {
      SQLFreeStmt( thiswa->hStmt, SQL_CLOSE );
      return (HB_FAILURE);    // It means a fault in SQL statement
   }

   res = SQLGetData( thiswa->hStmt, 1, SQL_C_ULONG, szValue, sizeof( SQL_C_ULONG ), NULL );
   if( res == SQL_ERROR )
   {
      SQLFreeStmt( thiswa->hStmt, SQL_CLOSE );
      return (HB_FAILURE);
   }

   SQLFreeStmt( thiswa->hStmt, SQL_CLOSE );

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

BOOL getColumnList( SQLEXAREAP thiswa )
{
   USHORT n, uiFlds;
   LPFIELD pField;
   char * colName;
   char * fName, * temp;
   int len;

   /* How iColumnListStatus works:

   Initial status is FIELD_LIST_LEARNING. It means it will as for ALL columns
   in query because we do not know what will the application need. It will remain
   like this until:

   1. Field value is read, so it turns to FIELD_LIST_NEW_VALUE_READ. It means
      application has read WA field information, so we now have a clue of what's
      really needed to be included in query column list

   2. New field list was created by this function (getColumnList) so now
      iColumnListStatus turns to FIELD_LIST_STABLE. It means same column list
      will be used for further queries until application reads a new field
      (that is not included in this list) and changes the iColumnListStatus
      status like in 1.1

   3. If situation in item 1 happens in more than 10% of 'pageReadSize' records,
      after iColumnListStatus assumes the FIELD_LIST_STABLE status, iColumnListStatus
      must be FIELD_LIST_CHANGED, so current Buffer Pool cache should be descarted
      and new query generated with new column list
      THIS IS STILL NOT IMPLEMENTED

   */

   colName = (char *) hb_xgrab(HB_SYMBOL_NAME_LEN + 1 );

   if ( thiswa->iColumnListStatus == FIELD_LIST_LEARNING )
   {
      if (!thiswa->sFields)
      {
         thiswa->sFields = (char *) hb_xgrab(FIELD_LIST_SIZE * sizeof( char ) );
         uiFlds = 0;
         for ( n=1; n <= thiswa->area.uiFieldCount; n++ )
         {
            pField  = thiswa->area.lpFields + n -1;
            fName   = (char *) hb_dynsymName( ( PHB_DYNS ) pField->sym );
            len     = strlen( fName );
            memset(colName,0,HB_SYMBOL_NAME_LEN);
            hb_xmemcpy( colName, fName, len );
            colName = QualifyName( colName, thiswa );
            colName[len] = '\0';

            if ( uiFlds == 0 )
            {
               // Should ALWAYS ask for RECNO in first column to
               // be used in the BufferPool
               if( thiswa->ulhDeleted == 0 )
               {
                  sprintf( thiswa->sFields, "A.%c%s%c, A.%c%s%c", OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ), OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ) );
               }
               else  // If deleted records control exists in current WA, we should add it first, also
               {
                  sprintf( thiswa->sFields, "A.%c%s%c, A.%c%s%c, A.%c%s%c", OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ), OPEN_QUALIFIER( thiswa ), thiswa->sDeletedName, CLOSE_QUALIFIER( thiswa ), OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ) );
               }
            }
            else
            {
               temp = hb_strdup( (const char *) thiswa->sFields );
               sprintf( thiswa->sFields, "%s, A.%c%s%c", temp, OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ) );
               hb_xfree( temp );
            }
            uiFlds++;
         }
         hb_xfree( colName );
         return TRUE;
      }
   }
   else if ( thiswa->iColumnListStatus == FIELD_LIST_CHANGED || thiswa->iColumnListStatus == FIELD_LIST_NEW_VALUE_READ )
   {
      uiFlds = 0;
      if (!thiswa->sFields)
      {
         thiswa->sFields = (char *) hb_xgrab(FIELD_LIST_SIZE * sizeof( char ) );
      }
      for ( n=1; n <= thiswa->area.uiFieldCount; n++ )
      {
         if ( thiswa->uiFieldList[n-1] )
         {
            pField  = thiswa->area.lpFields + n -1;
            fName   = (char *) hb_dynsymName( ( PHB_DYNS ) pField->sym );
            len     = strlen( fName );
            memset(colName,0,HB_SYMBOL_NAME_LEN);
            hb_xmemcpy( colName, fName, len );
            colName = QualifyName( colName, thiswa );
            colName[len] = '\0';

            if ( uiFlds == 0 )
            {
               // Should ALWAYS ask for RECNO in first column to
               // be used in the BufferPool

               if( thiswa->ulhDeleted == 0 )
               {
                  sprintf( thiswa->sFields, "A.%c%s%c, A.%c%s%c", OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ), OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ) );
               }
               else
               {
                  sprintf( thiswa->sFields, "A.%c%s%c, A.%c%s%c, A.%c%s%c", OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ), OPEN_QUALIFIER( thiswa ), thiswa->sDeletedName, CLOSE_QUALIFIER( thiswa ), OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ) );
               }
            }
            else
            {
               temp = hb_strdup( (const char *) thiswa->sFields );
               sprintf( thiswa->sFields, "%s, A.%c%s%c", temp, OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ) );
               hb_xfree( temp );
            }
            uiFlds++;
         }
      }
      hb_xfree( colName );
      thiswa->iColumnListStatus = FIELD_LIST_STABLE;
      return TRUE;
   }
   hb_xfree( colName );
   return FALSE;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE updateRecordBuffer( SQLEXAREAP thiswa, BOOL bUpdateDeleted )
{
   SQLRETURN res;
//    LONG lLenOut, lLen, lInitBuff, lCurrRecord;
   LONG lCurrRecord;
   HB_SIZE lPos;
   BOOL bTranslate;
//   PTR bBuffer, bOut;
//    char * bBuffer;
//    char * bOut=NULL;
   USHORT i,  iIndex, iEnd, iRow;
   PHB_ITEM aRecord, pKey;
   PHB_ITEM temp;
//   HB_ITEM temp;

   // To do: Must check if buffer pool have to be clared due to change in
   // column list


   // First, try to look for record in current buffer pool

   pKey    = hb_itemNew( NULL );
   hb_itemPutNL( pKey, thiswa->recordList[thiswa->recordListPos] );

   if (!bUpdateDeleted)       // Cache NEVER holds deleted() information
   {
      if ( hb_hashScan( thiswa->hBufferPool, pKey, &lPos  ) )
      {
         aRecord = hb_hashGetValueAt( thiswa->hBufferPool, lPos );
         hb_arrayCopy( aRecord, thiswa->aBuffer, NULL, NULL, NULL );
         hb_itemRelease( pKey );
         return HB_SUCCESS;
      }
   }

   // Check for maximum buffer pool size

   if( ( hb_hashLen(thiswa->hBufferPool)) > bufferPoolSize )
   {
      hb_hashNew( thiswa->hBufferPool );
      hb_hashPreallocate( thiswa->hBufferPool, ( ULONG ) ( bufferPoolSize * 1.2 ) );
   }

   // Not found, so let's try the database...

   if( getColumnList( thiswa )  || thiswa->hStmtBuffer == NULL)     // Check if field list has changed and if so
                                     // creates a new one in thiswa structure
   {

      thiswa->bConditionChanged2 = TRUE;     // SEEK statements are no longer valid - column list has changed!
      memset(thiswa->sSqlBuffer , 0 , MAX_SQL_QUERY_LEN / 5  * sizeof( char ) );
      if( thiswa->bIsSelect )
      {
         sprintf( thiswa->sSqlBuffer, "SELECT %s FROM (%s) A WHERE A.%c%s%c IN ( ?", thiswa->sFields, thiswa->szDataFileName, OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ) );
      }
      else
      {
         sprintf( thiswa->sSqlBuffer, "SELECT %s \nFROM %s A \nWHERE A.%c%s%c IN ( ?", thiswa->sFields, thiswa->sTable, OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ) );
      }

      iEnd = ( USHORT ) strlen( thiswa->sSqlBuffer );
      for ( i = 20; i < (MAX_SQL_QUERY_LEN/5); i++ )
      {
         if( thiswa->sSqlBuffer[i] == '?' )
         {
            iEnd = i;
            break;
         }
      }

      // Adjust SQL to 'pageReadSize' params

      for ( i = 1; i < pageReadSize; i++ )
      {
         thiswa->sSqlBuffer[++iEnd] = ',';
         thiswa->sSqlBuffer[++iEnd] = '?';
      }

      thiswa->sSqlBuffer[++iEnd] = ')';
      thiswa->sSqlBuffer[++iEnd] = '\0';

      if ( thiswa->hStmtBuffer )
      {
         res = SQLFreeStmt( thiswa->hStmtBuffer, SQL_CLOSE );
         if ( CHECK_SQL_N_OK( res ) )
         {
	         
            return (HB_FAILURE);
         }
         
//         thiswa->hStmtBuffer = NULL;
      }

      res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmtBuffer) );
      if ( CHECK_SQL_N_OK( res ) )
      {
	  
         return (HB_FAILURE);
      }

      res = SQLPrepare( thiswa->hStmtBuffer, (SQLCHAR *) (thiswa->sSqlBuffer), SQL_NTS );
      if ( CHECK_SQL_N_OK( res ) )
      {
         return (HB_FAILURE);
      }

      for ( i = 0; i < pageReadSize; i++ )
      {
         res = SQLBindParameter( thiswa->hStmtBuffer, i+1, SQL_PARAM_INPUT, SQL_C_ULONG, SQL_INTEGER, 15, 0, &(thiswa->lRecordToRetrieve[i]), 0, NULL );
      
         if ( CHECK_SQL_N_OK( res ) )
         {
	        //thiswa->hStmtBuffer =NULL;
            
            return (HB_FAILURE);
         }
      }
   }

   bTranslate = FALSE;

   // Sets the bindvar contents
   for ( i = 0; i < pageReadSize; i++ )
   {
      thiswa->lRecordToRetrieve[i] = ( thiswa->recordListPos + i < thiswa->recordListSize ? thiswa->recordList[thiswa->recordListPos + i] : 0 );
   }

   res = SQLExecute( thiswa->hStmtBuffer );

   if ( CHECK_SQL_N_OK( res ) )
   {
      odbcErrorDiagRTE( thiswa->hStmtBuffer, "updateRecordBuffer", thiswa->sSqlBuffer, res, __LINE__, __FILE__ );
      SQLCloseCursor( thiswa->hStmtBuffer );
      // culik null the handle
      //thiswa->hStmtBuffer=NULL;

      return (HB_FAILURE);
   }

  // bBuffer = hb_xgrab(COLUMN_BLOCK_SIZE + 1 );

   for( iRow = 1; iRow <= pageReadSize; iRow++ )
   {
      res = SQLFetch( thiswa->hStmtBuffer );
      if ( res != SQL_SUCCESS )
      {
//	     if (res == SQL_ERROR)
//	     {  
         break;
  //       }
      }
   

      // Get the RECNO from 1st column in result set

      res = SQLGetData( thiswa->hStmtBuffer, 1, SQL_C_ULONG, &(lCurrRecord), sizeof( SQL_C_ULONG ), NULL );
      if( res == SQL_ERROR )
      {
         SQLFreeStmt( thiswa->hStmtBuffer, SQL_CLOSE );
         //thiswa->hStmtBuffer =NULL;
         
         return (HB_FAILURE);
      }

      hb_itemPutNL( pKey, lCurrRecord );     // To be used as HASH key in Pool Buffer

      iIndex     = 1;      // Recno is the 1st so we have 1 position offset

      if( thiswa->ulhDeleted > 0 )
      {
         if( (( LONG ) (thiswa->recordList[thiswa->recordListPos])) == lCurrRecord )
         {
            SQLCHAR szValue[2];
            res = SQLGetData( thiswa->hStmtBuffer, 2, SQL_C_CHAR, szValue, 2, NULL );
            if( res == SQL_ERROR )
            {
               SQLFreeStmt( thiswa->hStmtBuffer, SQL_CLOSE );

               //thiswa->hStmtBuffer =NULL;
               
               return (HB_FAILURE);
            }
            else
            {
               if( szValue[0] == 0 )
               {
                  thiswa->deletedList[thiswa->recordListPos] = ' ';    // MySQL driver climps spaces from right side
               }
               else
               {
                  thiswa->deletedList[thiswa->recordListPos] = szValue[0];
               }
            }
         }
         iIndex = 2;
      }

      // Create a line array to hold the record
     aRecord = hb_itemNew( NULL );
     hb_arrayNew( aRecord, hb_arrayLen( thiswa->aBuffer ) );

     


      for( i=1; i <= thiswa->area.uiFieldCount; i++ )
      {
	     
// 	     bBuffer = (char*)hb_xgrab(COLUMN_BLOCK_SIZE + 1 ); 
// 	     lLen    = COLUMN_BLOCK_SIZE;
// 	     memset( bBuffer, 0, COLUMN_BLOCK_SIZE ) ; 
//          bOut       = NULL;
//          lInitBuff  = lLen;
//          lLenOut    = 0;
//          iReallocs  = 0;
//          //temp.type = HB_IT_NIL;        // I know this is not a good practice, but we save tons of allocs.
                                       // please keep as is. ML.
         temp = hb_itemNew( NULL);

         if( (thiswa->uiFieldList[i-1] == 0) && thiswa->iColumnListStatus != FIELD_LIST_LEARNING )
         {
            hb_arraySetForward( aRecord, i, temp );     // Field is temporaly NIL since it's have never
                                                         // been needed in current WA. Will be filled on demand
         }
         else
         {
// 	        LONG lType = ( LONG ) hb_arrayGetNL( hb_arrayGetItemPtr( thiswa->aFields, thiswa->uiBufferIndex[i-1] ), FIELD_DOMAIN );
            ++iIndex;
	        odbcGetData( ( HSTMT )thiswa->hStmtBuffer,hb_arrayGetItemPtr( thiswa->aFields, thiswa->uiBufferIndex[i-1] ),temp,  0,  thiswa->nSystemID, bTranslate,iIndex  );
                           hb_arraySetForward( aRecord, i, temp );
                          
           	           }
         hb_itemRelease( temp );
         
      }

      // Add new array to Buffer Pool
#ifdef __XHARBOUR__
      hb_hashAdd( thiswa->hBufferPool, ULONG_MAX, pKey, aRecord );
#else
      hb_hashAdd( thiswa->hBufferPool, pKey, aRecord );
#endif

      // Feeds current record when it is found
      if ( ((LONG) (thiswa->recordList[thiswa->recordListPos])) == lCurrRecord )
      {
         hb_arrayCopy( aRecord, thiswa->aBuffer, NULL, NULL, NULL );
      }
      hb_itemRelease( aRecord );
   }

   hb_itemRelease( pKey );
   
   SQLFreeStmt( thiswa->hStmtBuffer, SQL_CLOSE );


   if( res == SQL_NO_DATA_FOUND && iRow == 1 )
   {
      return (HB_FAILURE);    // Could not get at least one line from database
   }
   else
   {
      return ( HB_SUCCESS );
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE trySkippingOnCache( SQLEXAREAP thiswa, LONG lToSkip )
{
   LONG lSupposedPos;

   if ( thiswa->skipDirection != 0 )
   {
      if ( thiswa->recordListDirection == LIST_FORWARD )
      {
         lSupposedPos = thiswa->recordListPos + lToSkip;

         if( lSupposedPos >= 0 && lSupposedPos < thiswa->recordListSize )
         {
            thiswa->recordListPos = lSupposedPos;
            if ( updateRecordBuffer( thiswa, FALSE ) == HB_FAILURE )
            {
               commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
               return (HB_FAILURE);
            }
            return HB_SUCCESS;
         }
         else
         {
            // now, for sure thiswa->recordListPos is on the FIRST or LAST position in array
            // First, try to optimize EOF and BOF position cache

            if( lSupposedPos < 0 && thiswa->recordList[0] == thiswa->lBofAt )
            {
               thiswa->area.fBof   = TRUE;
               return HB_SUCCESS;
            }

            if( lSupposedPos >= 0 && thiswa->recordList[thiswa->recordListPos] == thiswa->lEofAt )
            {
               sqlGetCleanBuffer( thiswa );
               return HB_SUCCESS;
            }
         }
      }
      else
      {
         lSupposedPos = thiswa->recordListPos - lToSkip;

         if( lSupposedPos >= 0 && lSupposedPos < thiswa->recordListSize )
         {
            thiswa->recordListPos = lSupposedPos;
            if ( updateRecordBuffer( thiswa, FALSE ) == HB_FAILURE )
            {
               commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
               return (HB_FAILURE);
            }
            return HB_SUCCESS;
         }
         else
         {
            // now, for sure thiswa->recordListPos is on the FIRST or LAST position in array
            // First, try to optimize EOF and BOF position cache
            if( lSupposedPos < 0 && thiswa->recordList[0] == thiswa->lEofAt )
            {
               sqlGetCleanBuffer( thiswa );
               return HB_SUCCESS;
            }

            if( lSupposedPos >= 0 && thiswa->recordList[thiswa->recordListPos] == thiswa->lBofAt )
            {
               thiswa->area.fBof   = TRUE;
               return HB_SUCCESS;
            }
         }
      }
   }
   return HB_FAILURE;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE prepareRecordListQuery( SQLEXAREAP thiswa )
{
   INDEXBINDP IndexBind;
   HSTMT hPrep;
   SQLRETURN res;

   IndexBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];
   // culik not needed, we we are in the offset
   IndexBind += (thiswa->indexLevel - 1);    // Place Offset

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
      IndexBind->SkipFwdStmt = hPrep;
      memset( &IndexBind->SkipFwdSql,0,PREPARED_SQL_LEN ) ;
      hb_xmemcpy( IndexBind->SkipFwdSql, thiswa->sSql, PREPARED_SQL_LEN -1 );
      IndexBind->SkipFwdSql[PREPARED_SQL_LEN-1] = '\0';
   }
   else
   {
      IndexBind->SkipBwdStmt = hPrep;
      memset( &IndexBind->SkipBwdSql,0,PREPARED_SQL_LEN ) ;
      hb_xmemcpy( IndexBind->SkipBwdSql, thiswa->sSql, PREPARED_SQL_LEN -1 );
      IndexBind->SkipBwdSql[PREPARED_SQL_LEN-1] = '\0';
   }
   return (HB_SUCCESS);
}

/*------------------------------------------------------------------------*/

static BOOL CreateSkipStmt( SQLEXAREAP thiswa )
{
   PHB_ITEM pColumns, pIndexRef;
   INDEXBINDP IndexBind;
   int i;

   // Note about this IF: I assume that if query is prepared for level 1 (without changing IndexBind offset),
   // all queries are prepaered, since it loops to all levels when doing it, as code below. That's why it
   // checks for thiswa->IndexBindings[ thiswa->hOrdCurrent ]))->index???Stmt only.

   if( thiswa->bOrderChanged || thiswa->bConditionChanged1 || ( !(thiswa->IndexBindings[ thiswa->hOrdCurrent ]) ) || ( thiswa->IndexBindings[ thiswa->hOrdCurrent ] &&
      ( ( thiswa->recordListDirection == LIST_FORWARD  && (! ((INDEXBINDP) (thiswa->IndexBindings[ thiswa->hOrdCurrent ]))->SkipFwdStmt ) ) ||
        ( thiswa->recordListDirection == LIST_BACKWARD && (! ((INDEXBINDP) (thiswa->IndexBindings[ thiswa->hOrdCurrent ]))->SkipBwdStmt ) ) ) ) )    // Filter or controlling order has changed, or stmt is not prepared
   {
      thiswa->lBofAt     = 0;
      thiswa->lEofAt     = 0;
      thiswa->bOrderChanged = FALSE;

      if ( thiswa->hOrdCurrent > 0 )
      {
         pIndexRef = hb_arrayGetItemPtr( thiswa->aOrders, ( ULONG ) thiswa->hOrdCurrent );
         pColumns = hb_arrayGetItemPtr( pIndexRef, INDEX_FIELDS );
         thiswa->indexColumns = hb_arrayLen( pColumns );
      }
      else
      {
         thiswa->indexColumns = 1;     // Natural order, RECNO
      }

      // Alloc memory for binding structures, if first time

      if ( ! thiswa->IndexBindings[ thiswa->hOrdCurrent ] )
      {
         SetIndexBindStructure( thiswa );
      }

      // Now we should bind all index columns to be used by SKIP

      IndexBind = thiswa->IndexBindings[ thiswa->hOrdCurrent ];

      // Free the statements we are about to recreate

      for ( i=1; i <= thiswa->indexColumns; i++ )
      {
         if ( IndexBind->SkipFwdStmt )
         {
            SQLFreeStmt( IndexBind->SkipFwdStmt, SQL_DROP );
            IndexBind->SkipFwdStmt = NULL;
         }

         if ( IndexBind->SkipBwdStmt )
         {
            SQLFreeStmt( IndexBind->SkipBwdStmt, SQL_DROP );
            IndexBind->SkipBwdStmt = NULL;
         }
         IndexBind++;
      }

      getOrderByExpression( thiswa, FALSE );
      setResultSetLimit( thiswa, RECORD_LIST_SIZE );

      thiswa->indexLevel          = thiswa->indexColumns;

      // Create and prepare queries to scroll to each index column level

      for ( i=1; i <= thiswa->indexColumns; i++ )
      {
         getWhereExpression( thiswa, thiswa->recordListDirection == LIST_FORWARD ? LIST_SKIP_FWD : LIST_SKIP_BWD );
         createRecodListQuery( thiswa );
         prepareRecordListQuery( thiswa );
         thiswa->indexLevel--;
      }
      return (TRUE);
   }
   else
   {
      return (FALSE);
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExBof( SQLEXAREAP thiswa, BOOL * bof )
{
   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   *bof = thiswa->area.fBof;
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExEof( SQLEXAREAP thiswa, BOOL * eof )
{
   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }

   if ( thiswa->bIsInsert && thiswa->bufferHot )
   {
      *eof = FALSE;
   }
   else
   {
      *eof = thiswa->area.fEof;
   }
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExFound( SQLEXAREAP thiswa, BOOL * found )
{
   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   *found = thiswa->area.fFound;
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExGoBottom( SQLEXAREAP thiswa )
{
   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;
   thiswa->area.fFound = FALSE;

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( thiswa->lEofAt )
   {
      SELF_GOTO( (AREAP) thiswa, (LONG) thiswa->lEofAt );
      if ( thiswa->bReverseIndex  !=  bOldReverseIndex)
      {
         thiswa->recordListDirection = LIST_BACKWARD;
         getOrderByExpression( thiswa, FALSE );
         getWhereExpression( thiswa, LIST_FROM_BOTTOM );
         setResultSetLimit( thiswa, RECORD_LIST_SIZE / 10 );
         createRecodListQuery( thiswa );

         if ( getRecordList( thiswa, RECORD_LIST_SIZE / 10 ) == HB_FAILURE )
         {
            odbcErrorDiagRTE( thiswa->hStmt, "dbGoBottom", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
            commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
            return (HB_FAILURE);
         }
      }

   }
   else
   {
      thiswa->recordListDirection = LIST_BACKWARD;

      getOrderByExpression( thiswa, FALSE );
      getWhereExpression( thiswa, LIST_FROM_BOTTOM );
      setResultSetLimit( thiswa, RECORD_LIST_SIZE / 10 );
      createRecodListQuery( thiswa );

      if ( getRecordList( thiswa, RECORD_LIST_SIZE / 10 ) == HB_FAILURE )
      {
         odbcErrorDiagRTE( thiswa->hStmt, "dbGoBottom", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
         commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
         return (HB_FAILURE);
      }
   }

   thiswa->area.fTop    = FALSE;
   thiswa->area.fBottom = TRUE;
   thiswa->skipDirection  = -1;

   if ( thiswa->recordListSize == 0 )
   {
      thiswa->area.fEof = TRUE;
      thiswa->area.fBof = TRUE;
      sqlGetCleanBuffer( thiswa );
   }
   else
   {
      thiswa->area.fEof   = FALSE;
      thiswa->area.fBof   = FALSE;
      thiswa->lEofAt = thiswa->recordList[thiswa->recordListPos];
      if ( updateRecordBuffer( thiswa, FALSE ) == HB_FAILURE )
      {
         commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
         return (HB_FAILURE);
      }
   }

   SELF_SKIPFILTER( ( AREAP ) thiswa, -1 );

   if( thiswa->area.lpdbRelations )
   {
      return SELF_SYNCCHILDREN( ( AREAP ) thiswa );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExGoTo( SQLEXAREAP thiswa, LONG recno )
{
   int i;

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   /* Reset parent rel struct */
   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;
   thiswa->area.fFound = FALSE;

   if( recno == 0 )
   {
      // Move to phantom
      sqlGetCleanBuffer( thiswa );
      thiswa->area.fBof = TRUE;
      return HB_SUCCESS;
   }

   // 1 - Try to look for the record in current skip sequence
   for (i = 0; i < thiswa->recordListSize; i++ )
   {
      if( thiswa->recordList[i] == (ULONG)recno )
      {
         thiswa->recordListPos = i;
         if ( updateRecordBuffer( thiswa, FALSE ) == HB_SUCCESS )
         {
            thiswa->area.fEof = FALSE;
            thiswa->area.fBof = FALSE;
            return HB_SUCCESS;
         }
      }
   }

   // 2 - Get it from database

   thiswa->recordList[0]  = (ULONG) recno;
   thiswa->recordListSize = 1;
   thiswa->recordListDirection = LIST_FORWARD;
   thiswa->recordListPos = 0;

   if ( updateRecordBuffer( thiswa, TRUE ) == HB_SUCCESS )
   {
      thiswa->area.fEof = FALSE;
      thiswa->area.fBof = FALSE;
      return HB_SUCCESS;
   }

   // 3 - Move to phantom
   sqlGetCleanBuffer( thiswa );
   thiswa->area.fBof = TRUE;
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExGoToId( SQLEXAREAP thiswa, PHB_ITEM pItem )
{
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( HB_IS_NUMERIC( pItem ) )
   {
      return SELF_GOTO( (AREAP) thiswa, (LONG) hb_itemGetNL( pItem ) );
   }
   else
   {
      commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
      return (HB_FAILURE);
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExGoTop( SQLEXAREAP thiswa )
{
   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;
   thiswa->area.fFound = FALSE;

   if( getWorkareaParams( thiswa ) == HB_FAILURE )     // If workarea was opened by dbCreate()
   {
      return HB_FAILURE;
   }

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( thiswa->lBofAt )
   {
      SELF_GOTO( (AREAP) thiswa, (LONG) thiswa->lBofAt );
      if ( thiswa->bReverseIndex  !=  bOldReverseIndex)
      {
	     thiswa->recordListDirection = LIST_FORWARD;
         getOrderByExpression( thiswa, FALSE );
         getWhereExpression( thiswa, LIST_FROM_TOP );
         setResultSetLimit( thiswa, RECORD_LIST_SIZE / 10 );
         createRecodListQuery( thiswa );

         if ( getRecordList( thiswa, RECORD_LIST_SIZE / 10 ) == HB_FAILURE )
         {
            odbcErrorDiagRTE( thiswa->hStmt, "dbGoTop", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
            commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
            return (HB_FAILURE);
         }
      }

   }
   else
   {
      thiswa->recordListDirection = LIST_FORWARD;
      getOrderByExpression( thiswa, FALSE );
      getWhereExpression( thiswa, LIST_FROM_TOP );
      setResultSetLimit( thiswa, RECORD_LIST_SIZE / 10 );
      createRecodListQuery( thiswa );

      if ( getRecordList( thiswa, RECORD_LIST_SIZE / 10 ) == HB_FAILURE )
      {
         odbcErrorDiagRTE( thiswa->hStmt, "dbGoTop", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
         commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
         return (HB_FAILURE);
      }
   }

   thiswa->area.fTop    = TRUE;
   thiswa->area.fBottom = FALSE;
   thiswa->skipDirection  = 1;

   if ( thiswa->recordListSize == 0 )
   {
      thiswa->area.fEof = TRUE;
      thiswa->area.fBof = TRUE;
      sqlGetCleanBuffer( thiswa );
   }
   else
   {
      thiswa->area.fEof   = FALSE;
      thiswa->area.fBof   = FALSE;
      thiswa->lBofAt = thiswa->recordList[thiswa->recordListPos];
      if ( updateRecordBuffer( thiswa, FALSE ) == HB_FAILURE )
      {
         commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
         return (HB_FAILURE);
      }
   }

   SELF_SKIPFILTER( ( AREAP ) thiswa, 1 );

   if( thiswa->area.lpdbRelations )
   {
      return SELF_SYNCCHILDREN( ( AREAP ) thiswa );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExSeek( SQLEXAREAP thiswa, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast )
{
   int queryLevel;
   USHORT iIndex;
   HB_SIZE i;
   HB_ERRCODE retvalue = HB_SUCCESS;
   PHB_ITEM pNewKey = NULL;
   HSTMT hStmt = NULL;

   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;
   thiswa->area.fTop   = thiswa->area.fBottom = FALSE;
   thiswa->area.fEof   = FALSE;
   thiswa->area.fFound = FALSE;

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if ( thiswa->hOrdCurrent == 0 )
   {
      commonError( (AREAP) thiswa, EG_NOORDER, EDBF_NOTINDEXED, thiswa->sTable );
      return HB_FAILURE;
   }

#ifndef HB_CDP_SUPPORT_OFF
   if( HB_IS_STRING( pKey ) )
   {
      PHB_CODEPAGE cdpSrc = thiswa->cdPageCnv ? thiswa->cdPageCnv : hb_vmCDP();
      if( thiswa->area.cdPage && thiswa->area.cdPage != cdpSrc )
      {
         HB_SIZE nLen = hb_itemGetCLen( pKey );
         char * pszVal = hb_cdpnDup( hb_itemGetCPtr( pKey ), &nLen,
                                     cdpSrc, thiswa->area.cdPage );
         pKey = pNewKey = hb_itemPutCLPtr( NULL, pszVal, nLen );
      }
   }
#endif

   if( ! thiswa->CurrRecord )
   {
      SetCurrRecordStructure( thiswa );
   }

   // Start search code here

   thiswa->recordListDirection = ( bFindLast ? LIST_BACKWARD : LIST_FORWARD );

   // Set binding structures and push pKey to it
   if ( ! thiswa->IndexBindings[ thiswa->hOrdCurrent ] )
   {
      SetIndexBindStructure( thiswa );
   }

   if( FeedSeekKeyToBindings( thiswa, pKey, &queryLevel ) != HB_SUCCESS )
   {
      if( pNewKey )
         hb_itemRelease( pNewKey );
      return HB_FAILURE;
   }

   thiswa->bRebuildSeekQuery = TRUE;
   if( CreateSeekStmt( thiswa, queryLevel ) )      // Create and prepare new SEEK statement, if needed
   {
      BindSeekStmt( thiswa, queryLevel );          // Bind parameters to IndexBind structure
   }

   thiswa->bConditionChanged2 = FALSE;

   if ( getPreparedSeek( thiswa, queryLevel, &iIndex, &hStmt ) == HB_SUCCESS )     // Fetch line from database, read RECNO and DELETED
   {
      // Create a line array to hold the record
//       LONG lLenOut, lLen, lInitBuff;
      BOOL bTranslate;
//       PTR bBuffer, bOut;
//       USHORT iReallocs;
      PHB_ITEM temp;
      //HB_ITEM temp;
      int iComp;
      PHB_ITEM aRecord = hb_itemNew( NULL );

      hb_arrayNew( aRecord, hb_arrayLen( thiswa->aBuffer ) );
   
      //bBuffer = hb_xgrab(COLUMN_BLOCK_SIZE + 1 );
      bTranslate = FALSE;
      
      for( i=1; i <= thiswa->area.uiFieldCount; i++ )
      { 
  //       bBuffer = hb_xgrab(COLUMN_BLOCK_SIZE + 1 );
//   //       bBuffer = hb_xgrab(COLUMN_BLOCK_SIZE + 1 );
//          lLen    = COLUMN_BLOCK_SIZE;
//          memset( bBuffer, 0, lLen ) ;
//          bOut       = NULL;
//          lInitBuff  = lLen;
//          lLenOut    = 0;
//          iReallocs  = 0;
         
         temp=hb_itemNew(NULL) ;
         //temp.type = HB_IT_NIL;        // I know this is not a good practice, but we save tons of allocs.
                                       // please keep as is. ML.

         if( (thiswa->uiFieldList[i-1] == 0) && thiswa->iColumnListStatus != FIELD_LIST_LEARNING )
         {
            hb_arraySetForward( aRecord, i, temp );     // Field is temporaly NIL since it's have never
                                                         // been needed in current WA. Will be filled on demand
         }
         else
         {
//      	    LONG lType = ( LONG ) hb_arrayGetNL( hb_arrayGetItemPtr( thiswa->aFields, thiswa->uiBufferIndex[i-1] ), FIELD_DOMAIN );
            ++iIndex;
	        odbcGetData( ( HSTMT )hStmt,hb_arrayGetItemPtr( thiswa->aFields, thiswa->uiBufferIndex[i-1] ),temp,  0,  thiswa->nSystemID, bTranslate,iIndex  );
                    hb_arraySetForward( aRecord, i, temp );                 
               
         }
         hb_itemRelease( temp );
         
      }
      

      hb_arrayCopy( aRecord, thiswa->aBuffer, NULL, NULL, NULL );
      hb_itemRelease( aRecord );

      SQLFreeStmt( hStmt, SQL_CLOSE );
    
      // End search code

      iComp = sqlKeyCompareEx( thiswa, pKey, FALSE );

      if ( iComp != 0 )
      {
         thiswa->area.fFound = TRUE;
         thiswa->area.fBof   = FALSE;
         thiswa->area.fEof   = FALSE;
      }
      else
      {
         thiswa->area.fFound = FALSE;
         if( !bSoftSeek )
         {
            sqlGetCleanBuffer( thiswa );
         }
      }

      if (( hb_setGetDeleted() || thiswa->area.dbfi.itmCobExpr != NULL ) && !thiswa->area.fEof )
      {
         retvalue = SELF_SKIPFILTER( ( AREAP ) thiswa, ( bFindLast ? -1 : 1 ) );

         if ( thiswa->area.fEof )
         {
            thiswa->area.fFound = FALSE;
         }
         else
         {
            if ( sqlKeyCompareEx(  thiswa, pKey, FALSE ) != 0 )
            {
               thiswa->area.fFound = TRUE;
            }
            else
            {
               thiswa->area.fFound = FALSE;

               if( !bSoftSeek )
               {
                  sqlGetCleanBuffer( thiswa );
               }
            }
         }
      }
   }
   else
   {
      sqlGetCleanBuffer( thiswa );
      thiswa->area.fFound = FALSE;
   }
   thiswa->bRebuildSeekQuery = TRUE;
   if( pNewKey )
      hb_itemRelease( pNewKey );

   if( thiswa->area.lpdbRelations && retvalue == HB_SUCCESS )
   {
      return SELF_SYNCCHILDREN( ( AREAP ) thiswa );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExSkip( SQLEXAREAP thiswa, LONG lToSkip )
{
   LONG lSkip;

   if( thiswa->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) thiswa ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   else if ( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   /* Flush record and exit */
   if( lToSkip == 0 )
      return SELF_GOCOLD( (AREAP) thiswa );

   // We need save lCurrentRecord previous to lost fEof flag. To to correct SKIPRAW
   thiswa->lCurrentRecord = GetCurrentRecordNum( thiswa );

   thiswa->area.fTop = thiswa->area.fBottom = FALSE;
   thiswa->wasdel = 0;
   thiswa->area.fBof = thiswa->area.fEof = FALSE;

   if( lToSkip > 0 )
      lSkip = 1;
   else
   {
      lSkip = -1;
      lToSkip *= -1;
   }
   while( --lToSkip >= 0 )
   {
      if( SELF_SKIPRAW( (AREAP) thiswa, lSkip ) != HB_SUCCESS )
         return HB_FAILURE;
      if( SELF_SKIPFILTER( (AREAP) thiswa, lSkip ) != HB_SUCCESS )
         return HB_FAILURE;
      if( thiswa->area.fBof || thiswa->area.fEof )
         break;
   }

   /* Update Bof and Eof flags */
   if( lSkip < 0 )
      thiswa->area.fEof = FALSE;
   else /* ( lSkip > 0 ) */
      thiswa->area.fBof = FALSE;

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExSkipFilter( SQLEXAREAP thiswa, LONG lUpDown )
{
   // This was copied from workarea.c since SUPER_ method
   // does not fir in this RDD needs.
   BOOL fBottom, fDeleted;
   HB_ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("hb_waSkipFilter(%p, %ld)", thiswa, lUpDown));

   if( !hb_setGetDeleted() && thiswa->area.dbfi.itmCobExpr == NULL )
      return HB_SUCCESS;

   /* Since lToSkip is passed to SkipRaw, it should never request more than  a single skip.
             The implied purpose of hb_waSkipFilter is to get off of a "bad" record
             after a skip was performed, NOT to skip lToSkip filtered records.
         */
   lUpDown = ( lUpDown < 0  ? -1 : 1 );

   /* remember if we are here after SLEF_GOTOP() */
   fBottom = thiswa->area.fBottom;

   while( !thiswa->area.fBof && !thiswa->area.fEof )
   {
      /* SET DELETED */
      if( hb_setGetDeleted() )
      {
         if( SELF_DELETED( (AREAP) thiswa, &fDeleted ) != HB_SUCCESS )
            return HB_FAILURE;
         if( fDeleted )
         {
            if( SELF_SKIPRAW( (AREAP) thiswa, lUpDown ) != HB_SUCCESS )
               return HB_FAILURE;
            continue;
         }
      }

      /* SET FILTER TO */
      if( thiswa->area.dbfi.itmCobExpr )
      {
         if( SELF_EVALBLOCK( (AREAP) thiswa, thiswa->area.dbfi.itmCobExpr ) != HB_SUCCESS )
            return HB_FAILURE;

         if( HB_IS_LOGICAL( thiswa->area.valResult ) &&
             !hb_itemGetL( thiswa->area.valResult ) )
         {
            if( SELF_SKIPRAW( (AREAP) thiswa, lUpDown ) != HB_SUCCESS )
               return HB_FAILURE;
            continue;
         }
      }

      break;
   }

   /*
    * The only one situation when we should repos is backward skipping
    * if we are at BOTTOM position (it's SKIPFILTER called from GOBOTTOM)
    * then GOEOF() if not then GOTOP()
    */

   if( thiswa->area.fBof && lUpDown < 0 )
   {
      if( fBottom )
      {
         /* GOTO EOF (phantom) record -
            this is the only one place where GOTO is used by xHarbour
            directly and RDD which does not operate on numbers should
            serve this method only as SELF_GOEOF() synonym. If it's a
            problem then we can remove this if and always use SELF_GOTOP()
            but it also means second table scan if all records filtered
            are out of filter so I do not want to do that. I will prefer
            explicit add SELF_GOEOF() method
          */
         uiError = SELF_GOTO( (AREAP) thiswa, 0 );
      }
      else
      {
         uiError = SELF_GOTOP( (AREAP) thiswa );
         thiswa->area.fBof = TRUE;
      }
   }
   else
   {
      uiError = HB_SUCCESS;
   }

   return uiError;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExSkipRaw( SQLEXAREAP thiswa, LONG lToSkip )
{
   HB_ERRCODE res;

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }
   /* if we are over phantom record we go bottom. */
   if( lToSkip < 0 && thiswa->lCurrentRecord == thiswa->lLastRec )
      return ( SELF_GOBOTTOM( ( AREAP ) thiswa ) );

   if( ! thiswa->CurrRecord )
   {
      SetCurrRecordStructure( thiswa );
   }

   if ( lToSkip != 0 )
   {
      // Try to find needed record in record list cache
      thiswa->skipDirection = lToSkip > 0 ? 1 : -1;

      if( trySkippingOnCache( thiswa, lToSkip ) == HB_SUCCESS )
         return ( ConcludeSkipraw( thiswa ) );

      // Cache was unsuccessful, so get a new list from database

      if ( thiswa->hOrdCurrent > 0 )
      {
         thiswa->indexColumns = hb_arrayLen( hb_arrayGetItemPtr( hb_arrayGetItemPtr( thiswa->aOrders, ( ULONG ) thiswa->hOrdCurrent ), INDEX_FIELDS ) );
      }
      else
      {
         thiswa->indexColumns = 1;     // Natural order, RECNO
      }

      thiswa->recordListDirection = ( lToSkip > 0 ? LIST_FORWARD : LIST_BACKWARD );

      // Set binding structures and SQL stmts for
      // SKIP and SEEK over current index order

      if (!CreateSkipStmt( thiswa ))
      {
         // If queries were not re-createds and re-prepared, we should
         // feed bind structures with current record information (CreateSkipStmt
         // does it in GetWhereExpression() if queries were re-prepared

         FeedCurrentRecordToBindings( thiswa );
      }
      else
      {
         BindAllIndexStmts( thiswa );
      }

      thiswa->indexLevel          = thiswa->indexColumns;

      do
      {
         res = getPreparedRecordList( thiswa, RECORD_LIST_SIZE );
         thiswa->indexLevel--;

         if(  res == RESULTSET_OK )
         {
            break;
         }
         else if( res == HB_FAILURE )
         {
            commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
            return (HB_FAILURE);
         }
         else if( res == HB_RETRY )
         {
            if( lToSkip > 0 )
            {
               sqlGetCleanBuffer( thiswa );
               break;
            }
            else
            {
               SELF_GOTOP( ( AREAP ) thiswa );
               break;
            }
         }
      }
      while ( thiswa->indexLevel > 0 );

      // Now new database cache should had been read

      if(  res == RESULTSET_OK )
      {
         if( updateRecordBuffer( thiswa, FALSE ) == HB_FAILURE )
         {
            commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_READ, thiswa->sTable );
            return (HB_FAILURE);
         }
         return ( ConcludeSkipraw( thiswa ) );
      }
      else
      {
         if( lToSkip < 0 )
         {
            thiswa->area.fBof   = TRUE;
            if( thiswa->recordListSize )
               thiswa->lBofAt = thiswa->recordList[thiswa->recordListPos];
         }
         else
         {
            sqlGetCleanBuffer( thiswa );
         }
      }
   }
   return ( ConcludeSkipraw( thiswa ) );
}

/*------------------------------------------------------------------------*/

#define sqlExAddField						NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExAppend( SQLEXAREAP thiswa )
{
   /* Reset parent rel struct */
   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;

   hb_arraySize(thiswa->aLocked, 0);

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return( HB_FAILURE );
   }

   thiswa->bufferHot = TRUE;
   thiswa->bIsInsert = TRUE;

   sqlGetCleanBuffer( thiswa );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExCreateFields				NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExDeleteRec( SQLEXAREAP thiswa )
{
   BOOL isDeleted;
   SQLRETURN res;

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }

   SELF_DELETED( ( AREAP ) thiswa, &isDeleted );

   if( (!isDeleted) && (!thiswa->area.fEof) )
   {
	  if (  thiswa->sSql  )
	  memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
      if( thiswa->ulhDeleted > 0 && sr_UseDeleteds() )
      {
         sprintf( thiswa->sSql, "UPDATE %s SET %s = '%c'%s WHERE %s = %i",
                                thiswa->sTable, thiswa->sDeletedName, thiswa->iTCCompat >= 2 ? '*' : 'T',
                                thiswa->iTCCompat >= 4 ? ", R_E_C_D_E_L_ = R_E_C_N_O_" : " ",
                                thiswa->sRecnoName, (int) GetCurrentRecordNum( thiswa ) );

      }
      else
      {
         sprintf( thiswa->sSql, "DELETE FROM %s WHERE %s = %i",
                                thiswa->sTable, thiswa->sRecnoName,
                                (int) GetCurrentRecordNum( thiswa ) );
      }

      res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmt) );
      if ( CHECK_SQL_N_OK( res ) )
      {
         return (HB_FAILURE);
      }

      res = SQLExecDirect( thiswa->hStmt, (SQLCHAR *) thiswa->sSql, SQL_NTS );
      if ( res == SQL_ERROR )
      {
         return (HB_FAILURE);    // It means a fault in SQL statement
      }
      SQLFreeStmt( thiswa->hStmt, SQL_DROP );
   }

   thiswa->deletedList[thiswa->recordListPos] = (thiswa->iTCCompat ? '*' : 'T');

   if( thiswa->lEofAt == thiswa->recordList[thiswa->recordListPos] )
      thiswa->lEofAt = 0;

   if( thiswa->lBofAt == thiswa->recordList[thiswa->recordListPos] )
      thiswa->lBofAt = 0;

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExDeleted( SQLEXAREAP thiswa, BOOL * isDeleted )
{
   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }

   if( thiswa->ulhDeleted == 0 || thiswa->bIsInsert || thiswa->area.fEof )
   {
      * isDeleted = FALSE;
   }
   else
   {
      * isDeleted = thiswa->deletedList[thiswa->recordListPos] != ' ';
   }

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExFieldCount					NULL
#define sqlExFieldDisplay				NULL
#define sqlExFieldInfo					NULL
#define sqlExFieldName					NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExFlush( SQLEXAREAP thiswa )
{
   return ( SELF_GOCOLD( ( AREAP ) thiswa ) );
}

/*------------------------------------------------------------------------*/

#define sqlExGetRec						NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExGetValue( SQLEXAREAP thiswa, USHORT fieldNum, PHB_ITEM value  )
{
   PHB_ITEM itemTemp, itemTemp3;
   HB_SIZE ulPos;

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   itemTemp = hb_itemArrayGet( thiswa->aBuffer, thiswa->uiBufferIndex[fieldNum - 1] );

   if( HB_IS_NIL( itemTemp ) )
   {
      getMissingColumn( thiswa, hb_arrayGetItemPtr( thiswa->aBuffer, thiswa->uiBufferIndex[fieldNum - 1] ), (LONG) (thiswa->uiBufferIndex[fieldNum - 1]) );
      hb_itemRelease( itemTemp );
      itemTemp = hb_itemArrayGet( thiswa->aBuffer, thiswa->uiBufferIndex[fieldNum - 1] );
   }
      if (HB_IS_STRING( itemTemp ) )   
   {
      char * bBuffer = hb_itemGetCPtr( itemTemp ) ;
      LONG lLenBuff = hb_itemGetCLen(itemTemp ) ;
      PHB_ITEM pTemp;
      if( lLenBuff > 10 && strncmp( bBuffer, SQL_SERIALIZED_SIGNATURE, 10 ) == 0 && (!sr_lSerializedAsString()) )
      {
               if( s_pSym_SR_DESERIALIZE == NULL )
               {
                  hb_dynsymLock();
                  s_pSym_SR_DESERIALIZE = hb_dynsymFindName( "SR_DESERIALIZE" );
                  hb_dynsymUnlock();
                  if ( s_pSym_SR_DESERIALIZE  == NULL ) printf( "Could not find Symbol SR_DESERIALIZE\n" );
               }
               hb_vmPushDynSym( s_pSym_SR_DESERIALIZE );
               hb_vmPushNil();
               hb_vmPushString( bBuffer, lLenBuff );
               hb_vmDo( 1 );

               pTemp = hb_itemNew( NULL );
               hb_itemForwardValue( pTemp, hb_stackReturnItem() );

               if( HB_IS_HASH( pTemp ) && sr_isMultilang() )
               {
                  PHB_ITEM pLangItem = hb_itemNew( NULL );
                  HB_SIZE ulPos;
                  if( hb_hashScan( pTemp, sr_getBaseLang( pLangItem ), &ulPos ) ||
                      hb_hashScan( pTemp, sr_getSecondLang( pLangItem ), &ulPos ) ||
                      hb_hashScan( pTemp, sr_getRootLang( pLangItem ), &ulPos ) )
                  {
                     hb_itemCopy( itemTemp, hb_hashGetValueAt( pTemp, ulPos ) );
                  }
                  hb_itemRelease( pLangItem );
               }
               else
               {
                  hb_itemForwardValue( itemTemp, pTemp );
               }
               hb_itemRelease( pTemp );
            }    
}
   if( !thiswa->uiFieldList[fieldNum - 1] )
   {
      thiswa->uiFieldList[fieldNum - 1] = 1;
      thiswa->iColumnListStatus          = FIELD_LIST_NEW_VALUE_READ;
   }

   if (HB_IS_ARRAY( itemTemp ))
   {
#ifdef __XHARBOUR__
      itemTemp3 = hb_arrayClone( itemTemp, NULL );
      hb_itemForwardValue( value, itemTemp3 );
      hb_itemRelease( itemTemp3 );
#else
      hb_arrayCloneTo( value, itemTemp );
#endif
   }
   else if(HB_IS_HASH( itemTemp ) && sr_isMultilang() )
   {
      LPFIELD pField = thiswa->area.lpFields + fieldNum - 1;

      if( pField->uiType == HB_FT_MEMO )
      {
         PHB_ITEM pLangItem = hb_itemNew( NULL );

         if( hb_hashScan( itemTemp, sr_getBaseLang( pLangItem ), &ulPos ) ||
             hb_hashScan( itemTemp, sr_getSecondLang( pLangItem ), &ulPos ) ||
             hb_hashScan( itemTemp, sr_getRootLang( pLangItem ), &ulPos ) )
         {
            hb_itemCopy( value, hb_hashGetValueAt( itemTemp, ulPos ) );
         }
         else
         {
            hb_itemPutC( pLangItem, NULL );
            hb_itemForwardValue( value, pLangItem );
         }
         hb_itemRelease( pLangItem );
      }
      else
      {
         PHB_ITEM pLangItem = hb_itemNew( NULL );
         HB_SIZE nLen = pField->uiLen, nSrcLen;
         char * empty = ( char * ) hb_xgrab(nLen + 1 );

         if( hb_hashScan( itemTemp, sr_getBaseLang( pLangItem ), &ulPos ) ||
             hb_hashScan( itemTemp, sr_getSecondLang( pLangItem ), &ulPos ) ||
             hb_hashScan( itemTemp, sr_getRootLang( pLangItem ), &ulPos ) )
         {
            itemTemp3 = hb_hashGetValueAt( itemTemp, ulPos );
            nSrcLen = hb_itemGetCLen( itemTemp3 );
            hb_xmemcpy( empty, hb_itemGetCPtr( itemTemp3 ), HB_MIN( nLen, nSrcLen ) );
            if( nLen > nSrcLen )
            {
               memset( empty + nSrcLen, ' ', nLen - nSrcLen );
            }
#ifndef HB_CDP_SUPPORT_OFF
            if( pField->uiType == HB_FT_STRING  )
            {
               PHB_CODEPAGE cdpDest = thiswa->cdPageCnv ? thiswa->cdPageCnv : hb_vmCDP();
               if( thiswa->area.cdPage && thiswa->area.cdPage != cdpDest )
               {
                  char * pszVal = hb_cdpnDup( empty, &nLen, thiswa->area.cdPage, cdpDest );
                  hb_xfree( empty );
                  empty = pszVal;
               }
            }
#endif
         }
         else
         {
            memset( empty, ' ', nLen );
         }
         empty[ nLen ] = '\0';
         hb_itemPutCLPtr( value, empty, nLen );
         hb_itemRelease( pLangItem );
      }
   }
   else
   {
      hb_itemForwardValue( value, itemTemp );
   }
   hb_itemRelease( itemTemp );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExGetVarLen					NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExGoCold( SQLEXAREAP thiswa )
{
   if ( thiswa->bufferHot )   // && (!(thiswa->ulhDeleted > 0 ? TRUE : thiswa->deletedList[thiswa->recordListPos] == ' ') )
   {
      if ( thiswa->bHistoric )
      {
         hb_arraySetL( thiswa->aInfo, AINFO_ISINSERT, thiswa->bIsInsert );
         hb_arraySetL( thiswa->aInfo, AINFO_HOT,      thiswa->bufferHot );
         if( !thiswa->bIsInsert )
         {
            hb_arraySetNL( thiswa->aInfo, AINFO_RECNO, GetCurrentRecordNum( thiswa ) );
         }

         return SUPER_GOCOLD( ( AREAP ) thiswa );     // Historic workareas are handled by xBase code
                                                      // in sqlrdd2.c as in SQLRDD
      }

      if( thiswa->bIsInsert )
      {
         if( !thiswa->hStmtInsert )    // Check if we have the INSERT statement prepared
         {
            CreateInsertStmt( thiswa );   // Create also column binding structures

            if( PrepareInsertStmt( thiswa ) == HB_FAILURE )
               return (HB_FAILURE);

            if( BindInsertColumns( thiswa ) == HB_FAILURE )
               return (HB_FAILURE);
           
               
         }
         else
         {
            thiswa->sSql[0] = '\0';                    // To prevent erroneous error message
         }

         if( FeedRecordCols( thiswa, FALSE ) == HB_FAILURE )  // Stmt created and prepared, only need to push data
            return (HB_FAILURE);

            if( ExecuteInsertStmt( thiswa ) == HB_FAILURE )
               return (HB_FAILURE);
      }
      else if( !thiswa->area.fEof )
      {
         if( (!thiswa->hStmtUpdate) || memcmp( (const void *) (thiswa->editMask),
                                               (const void *) (thiswa->updatedMask ), MAX_FIELDS ) != 0 )
         {
            if( CreateUpdateStmt( thiswa ) == HB_FAILURE )
               return (HB_FAILURE);
         }
         else
         {
            thiswa->sSql[0] = '\0';    // Avoid wrong error message in Execute
         }

         if( ExecuteUpdateStmt( thiswa ) == HB_FAILURE )
            return (HB_FAILURE);
      }
      thiswa->bufferHot = FALSE;
      thiswa->bIsInsert = FALSE;
   }

   memset( thiswa->editMask,    0, MAX_FIELDS );      // Clear edited mask

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExGoHot						NULL
#define sqlExPutRec						NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExPutValue( SQLEXAREAP thiswa, USHORT fieldNum, PHB_ITEM value )
{
   PHB_ITEM pDest;
   LPFIELD pField;
   char * cfield;
   double dNum;
   USHORT len, dec, fieldindex;

   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }

   fieldindex = (USHORT)thiswa->uiBufferIndex[fieldNum - 1];
   thiswa->editMask[ fieldindex-1 ] = '1';
   pDest  = hb_itemArrayGet( thiswa->aBuffer, fieldindex );

   if( !thiswa->uiFieldList[fieldNum - 1] )
   {
      thiswa->uiFieldList[fieldNum - 1] = 1;
      thiswa->iColumnListStatus          = FIELD_LIST_NEW_VALUE_READ;
   }

   if( HB_IS_NIL( pDest ) )
   {
      getMissingColumn( thiswa, pDest, fieldindex );
   }

   if( !thiswa->uiFieldList[fieldNum - 1] )     // Columns to be included in SELECT statement further
   {
      hb_arraySetNL( thiswa->aSelectList, thiswa->uiBufferIndex[fieldNum - 1], 1 );
      thiswa->uiFieldList[fieldNum - 1] = 1;
   }

   pField = thiswa->area.lpFields + fieldNum - 1;

   /* test compatible datatypes */

   if( (HB_IS_NUMBER( pDest ) && HB_IS_NUMBER( value )) || (HB_IS_STRING( pDest ) && HB_IS_STRING( value )) ||
       (HB_IS_LOGICAL( pDest ) && HB_IS_LOGICAL( value )) || (HB_IS_DATE( pDest ) && HB_IS_DATE( value )) ||
       (HB_IS_DATETIME( pDest ) && HB_IS_DATETIME( value )) )
   {

      if( pField->uiType == HB_FT_STRING )
      {
         HB_SIZE nSize = hb_itemGetCLen( value ), nLen = pField->uiLen;

         cfield = (char *) hb_xgrab(nLen + 1 );
#ifndef HB_CDP_SUPPORT_OFF
         hb_cdpnDup2( hb_itemGetCPtr( value ), nSize,
                      cfield, &nLen,
                      thiswa->cdPageCnv ? thiswa->cdPageCnv : hb_vmCDP(), thiswa->area.cdPage );
         nSize = nLen;
         nLen = pField->uiLen;
#else
         memcpy( cfield, hb_itemGetCPtr( value ), HB_MIN( nLen, nSize ) );
#endif
         if( nLen > nSize )
            memset( cfield + nSize, ' ', nLen - nSize );
         cfield[ nLen ] =  '\0';
         hb_itemPutCLPtr( value, cfield, nLen );
      }
      else if( pField->uiType == HB_FT_LONG )
      {
         len = pField->uiLen;
         dec = pField->uiDec;
         if( dec > 0 )
         {
            len -= (dec + 1);
         }
         dNum = hb_itemGetND( value );
         hb_itemPutNLen( value, dNum, len, dec );
      }

      hb_arraySet( thiswa->aBuffer, fieldindex, value );
   }
   else if(HB_IS_STRING( value ) && HB_IS_HASH( pDest ) && sr_isMultilang() )
   {
      PHB_ITEM pLangItem = hb_itemNew( NULL );
#ifdef __XHARBOUR__
      hb_hashAdd( pDest, ULONG_MAX, sr_getBaseLang( pLangItem ), value );
#else
      hb_hashAdd( pDest, sr_getBaseLang( pLangItem ), value );
#endif
      hb_itemRelease( pLangItem );
   }
   else if( pField->uiType == HB_FT_MEMO )    // Memo fields can hold ANY datatype
   {
      hb_arraySet( thiswa->aBuffer, fieldindex, value );
   }
   
   
   else
   {
#ifdef SQLRDD_NWG_SPECIFIC
      thiswa->bufferHot = TRUE;
      return ( HB_SUCCESS );
#else
      char type_err[128];
      sprintf( type_err, "data type origin: %i - data type target %i", hb_itemType( value ), hb_itemType( pDest ) );
      commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, type_err );
      return ( HB_FAILURE );
#endif
   }

               
   thiswa->bufferHot = TRUE;
   hb_itemRelease( pDest );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExRecall( SQLEXAREAP thiswa )
{
   BOOL isDeleted;
   SQLRETURN res;

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   SELF_DELETED( ( AREAP ) thiswa, &isDeleted );

   if( isDeleted && thiswa->ulhDeleted > 0 && sr_UseDeleteds() )
   {
	  memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
      sprintf( thiswa->sSql, "UPDATE %s SET %s = '%c'%s WHERE %s = %i",
                             thiswa->sTable, thiswa->sDeletedName, ' ',
                             thiswa->iTCCompat >= 4 ? ", R_E_C_D_E_L_ = R_E_C_N_O_" : " ",
                             thiswa->sRecnoName, (int) GetCurrentRecordNum( thiswa ) );

      res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmt) );
      if ( CHECK_SQL_N_OK( res ) )
      {
         return (HB_FAILURE);
      }

      res = SQLExecDirect( thiswa->hStmt, (SQLCHAR *) thiswa->sSql, SQL_NTS );
      if ( res == SQL_ERROR )
      {
         return (HB_FAILURE);    // It means a fault in SQL statement
      }
      SQLFreeStmt( thiswa->hStmt, SQL_DROP );
   }

   thiswa->deletedList[thiswa->recordListPos] = ' ';
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExRecCount( SQLEXAREAP thiswa, ULONG * recCount )
{
   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }

   if ( thiswa->bIsInsert && thiswa->bufferHot )
   {
      *recCount = (ULONG) (hb_arrayGetNL( thiswa->aInfo, AINFO_RCOUNT ) + 1);
   }
   else
   {
      *recCount = (ULONG) (hb_arrayGetNL( thiswa->aInfo, AINFO_RCOUNT ));
   }

   thiswa->lLastRec = (*recCount) + 1;
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExRecInfo						NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExRecNo( SQLEXAREAP thiswa, ULONG * recno )
{
#ifdef SQLRDD_NWG_SPECIFIC
   if( thiswa->bIsInsert )
   {
      commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_NOT_COMMITED_YET, NULL );
      return ( HB_FAILURE );
   }
#endif
   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   *recno = GetCurrentRecordNum( thiswa );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExRecId( SQLEXAREAP thiswa, PHB_ITEM recno )
{
   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if ( thiswa->initialized )
   {
      if( thiswa->bIsInsert || thiswa->area.fEof )
      {
      hb_itemPutNL( recno, thiswa->lLastRec );
      }
      else
      {
      hb_itemPutNL( recno, (ULONG) thiswa->recordList[thiswa->recordListPos] );
      }
   }
   else
   {
      hb_itemPutNL( recno, 0 );
   }

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExSetFieldExtent				NULL
#define sqlExAlias							NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExClose( SQLEXAREAP thiswa )
{
	HB_ERRCODE code;
   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }
   code = (SUPER_CLOSE( (AREAP) thiswa ));
   /* Reset parent rel struct */
   thiswa->lpdbPendingRel = NULL;

   if ((thiswa->oSql) && HB_IS_OBJECT( thiswa->oSql ))
      hb_itemRelease( thiswa->oSql );
   if ( thiswa->sTable )
      hb_xfree( thiswa->sTable );
   if ( thiswa->sOwner )
      hb_xfree( thiswa->sOwner );
   if ( thiswa->sFields )
      hb_xfree( thiswa->sFields  );
   if ( thiswa->sRecnoName )
      hb_xfree( thiswa->sRecnoName  );
   if ( thiswa->sDeletedName )
      hb_xfree( thiswa->sDeletedName  );
   if ( thiswa->recordList )
      hb_xfree( thiswa->recordList );
   if ( thiswa->deletedList )
      hb_xfree( thiswa->deletedList );
   if ( thiswa->sSql )
      hb_xfree( thiswa->sSql );
   if ( thiswa->sSqlBuffer )
      hb_xfree( thiswa->sSqlBuffer );
   if ( thiswa->sWhere )
      hb_xfree( thiswa->sWhere );
   if ( thiswa->sOrderBy )
      hb_xfree( thiswa->sOrderBy );
   if ( thiswa->hBufferPool )
      hb_itemRelease( thiswa->hBufferPool );
   if ( thiswa->lRecordToRetrieve )
      hb_xfree( thiswa->lRecordToRetrieve );
   if ( thiswa->hStmtBuffer )
      SQLFreeStmt( thiswa->hStmtBuffer, SQL_DROP );
   if ( thiswa->hStmtInsert )
      SQLFreeStmt( thiswa->hStmtInsert, SQL_DROP );
   if ( thiswa->hStmtNextval )
      SQLFreeStmt( thiswa->hStmtNextval, SQL_DROP );
   if ( thiswa->hStmtUpdate )
      SQLFreeStmt( thiswa->hStmtUpdate, SQL_DROP );

   ReleaseColStatements( thiswa, 0 );
   ReleaseInsertRecordStructure( thiswa, 0 );
   ReleaseCurrRecordStructure( thiswa, 0 );

   if ( thiswa->aFields )
      hb_itemRelease( thiswa->aFields );

   ReleaseIndexBindStructure( thiswa );
   // We now use as an true structure, so let freeit
   if ( thiswa->IndexBindings )
      hb_xfree( thiswa->IndexBindings) ;


   return code; //(SUPER_CLOSE( (AREAP) thiswa ));
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExCreate( SQLEXAREAP thiswa, LPDBOPENINFO OpenInfo )
{
   HB_ERRCODE err;

   err = SUPER_CREATE( (AREAP) thiswa, OpenInfo );

   return err;

   // Note: getWorkareaParams() is executed by GoTop call
   // from super class
}

/*------------------------------------------------------------------------*/

#define sqlExInfo							NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExNewArea( SQLEXAREAP thiswa )
{
   HB_ERRCODE errCode;
   //int i;

   errCode = SUPER_NEW( ( AREAP ) thiswa );

   thiswa->oSql               = NULL;
   thiswa->hBufferPool        = hb_hashNew( NULL );
   thiswa->aFields            = NULL;
   thiswa->sTable             = NULL;
   thiswa->sOwner             = NULL;
   thiswa->sRecnoName         = NULL;
   thiswa->sDeletedName       = NULL;
   thiswa->iColumnListStatus   = FIELD_LIST_LEARNING;
   thiswa->hStmt              = NULL;
   thiswa->hStmtBuffer        = NULL;
   thiswa->hStmtInsert        = NULL;
   thiswa->hStmtNextval       = NULL;
   thiswa->hStmtUpdate        = NULL;
   thiswa->recordListPos      = 0;
   thiswa->indexColumns       = 0;
   thiswa->skipDirection      = 0;
   thiswa->lBofAt             = 0;
   thiswa->lEofAt             = 0;
   thiswa->indexLevel         = -1;
   thiswa->sFields            = NULL;
   thiswa->iTCCompat          = 0;
   thiswa->bIsInsert          = FALSE;
   thiswa->bufferHot          = FALSE;
   thiswa->bConditionChanged1 = FALSE;
   thiswa->bConditionChanged2 = FALSE;
   thiswa->bOrderChanged      = FALSE;
   thiswa->bConnVerified      = FALSE;
   thiswa->recordList         = ( ULONG * ) hb_xgrab(RECORD_LIST_SIZE * sizeof( ULONG ) );
   thiswa->lRecordToRetrieve  = ( ULONG * ) hb_xgrab(pageReadSize * sizeof( ULONG ) );
   thiswa->deletedList        = ( char * ) hb_xgrab(RECORD_LIST_SIZE * sizeof( char ) );
   thiswa->sSql               = ( char * ) hb_xgrab(MAX_SQL_QUERY_LEN * sizeof( char ) );
   memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   thiswa->sSqlBuffer         = ( char * ) hb_xgrab(MAX_SQL_QUERY_LEN / 5  * sizeof( char ) );
   memset(thiswa->sSqlBuffer , 0 , MAX_SQL_QUERY_LEN / 5  * sizeof( char ) );
   thiswa->sOrderBy           = ( char * ) hb_xgrab(MAX_SQL_QUERY_LEN / 20 * sizeof( char ) );
   memset( thiswa->sOrderBy, 0, MAX_SQL_QUERY_LEN / 20 * sizeof( char ));
   thiswa->sWhere             = ( char * ) hb_xgrab(MAX_SQL_QUERY_LEN / 10 * sizeof( char ) );
   memset( thiswa->sWhere, 0, MAX_SQL_QUERY_LEN / 10 * sizeof( char ) ) ;
   thiswa->InsertRecord       = NULL;
   thiswa->CurrRecord         = NULL;

   hb_hashPreallocate( thiswa->hBufferPool, ( ULONG ) ( bufferPoolSize * 1.2 ) );

   memset( thiswa->updatedMask, 0, MAX_FIELDS );
   memset( thiswa->editMask,    0, MAX_FIELDS );
   memset( thiswa->specialMask, 0, MAX_FIELDS );
   thiswa->IndexBindings = (INDEXBINDP *) hb_xgrab(sizeof( INDEXBINDP) * MAX_INDEXES ) ;
   memset( thiswa->IndexBindings, 0, sizeof( INDEXBINDP) * MAX_INDEXES ) ;
 

   return errCode;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOpen( SQLEXAREAP thiswa, LPDBOPENINFO OpenInfo )
{
   HB_ERRCODE errCode;

   errCode = SUPER_OPEN( ( AREAP ) thiswa, OpenInfo );

   if( errCode != HB_SUCCESS )
   {
      return errCode;
   }

   if( getWorkareaParams( thiswa ) == HB_FAILURE )     // If workarea was opened by dbCreate()
   {
      return HB_FAILURE;
   }

   // Releases allocated cache for SQLRDD, since
   // SQLEX does not use it

   thiswa->bOrderChanged = TRUE;

   return errCode;
}

/*------------------------------------------------------------------------*/

#define sqlExRelease						NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExStructSize( SQLEXAREAP thiswa, USHORT * StructSize )
{
   HB_SYMBOL_UNUSED( thiswa );     /* Avoid compiler warning */
   *StructSize = sizeof( SQLEXAREA );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExSysName						NULL
#define sqlExEval							NULL
#define sqlExPack							NULL
#define sqlExPackRec						NULL
#define sqlExSort							NULL
#define sqlExTrans						NULL
#define sqlExTransRec					NULL
#define sqlExZap							NULL
#define sqlExChildEnd					NULL
#define sqlExChildStart					NULL
#define sqlExChildSync					NULL
#define sqlExSyncChildren				NULL
#define sqlExClearRel					NULL
#define sqlExForceRel					NULL
#define sqlExRelArea						NULL
#define sqlExRelEval						NULL
#define sqlExRelText						NULL
#define sqlExSetRel						NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOrderListAdd( SQLEXAREAP thiswa, LPDBORDERINFO pOrderInfo )
{
   HB_ERRCODE err;
   LONG hOldOrder = thiswa->hOrdCurrent;

   err = SUPER_ORDLSTADD( (AREAP) thiswa, pOrderInfo );

   if( hOldOrder != thiswa->hOrdCurrent )
   {
      thiswa->lBofAt        = 0;
      thiswa->lEofAt        = 0;
      thiswa->indexLevel    = -1;
      bOldReverseIndex = thiswa->bReverseIndex;
      thiswa->bReverseIndex = hb_arrayGetL( thiswa->aInfo, AINFO_REVERSE_INDEX );
   }
   return (err);
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOrderListClear( SQLEXAREAP thiswa )
{
   thiswa->lBofAt     = 0;
   thiswa->lEofAt     = 0;
   thiswa->indexLevel = -1;

   SUPER_ORDLSTCLEAR( ( AREAP ) thiswa );

   ReleaseIndexBindStructure( thiswa );
   thiswa->hOrdCurrent  = 0;

   return (HB_SUCCESS);
}

/*------------------------------------------------------------------------*/

#define sqlExOrderListDelete			NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOrderListFocus( SQLEXAREAP thiswa, LPDBORDERINFO pOrderInfo )
{
   HB_ERRCODE err;
   LONG hOldOrder = thiswa->hOrdCurrent;

   err = SUPER_ORDLSTFOCUS( (AREAP) thiswa, pOrderInfo );

   if( hOldOrder != thiswa->hOrdCurrent )
   {
      thiswa->bOrderChanged = TRUE;
      thiswa->lBofAt     = 0;
      thiswa->lEofAt     = 0;
   }

   if ( thiswa->hOrdCurrent > 0 )
   {
      thiswa->indexColumns  = hb_arrayLen( hb_arrayGetItemPtr( hb_arrayGetItemPtr( thiswa->aOrders, ( ULONG ) thiswa->hOrdCurrent ), INDEX_FIELDS ) );
      bOldReverseIndex = thiswa->bReverseIndex;
      thiswa->bReverseIndex = hb_arrayGetL( thiswa->aInfo, AINFO_REVERSE_INDEX );
      if (thiswa->IndexBindings[ thiswa->hOrdCurrent ]) {
         hb_xfree( thiswa->IndexBindings[ thiswa->hOrdCurrent ] );
         thiswa->IndexBindings[ thiswa->hOrdCurrent ] = NULL;
       }
   }
   else
   {
      thiswa->indexColumns  = 1;     // Natural order, RECNO
   }

   return (err);
}

/*------------------------------------------------------------------------*/

#define sqlExOrderListRebuild			NULL
#define sqlExOrderCondition			NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOrderCreate( SQLEXAREAP thiswa, LPDBORDERCREATEINFO pOrderCreateInfo )
{
   HB_ERRCODE err;
   int iLen           = (int) hb_arrayLen( thiswa->aFields );
   thiswa->lBofAt     = 0;
   thiswa->lEofAt     = 0;
   thiswa->indexLevel = -1;

   err = SUPER_ORDCREATE( (AREAP) thiswa, pOrderCreateInfo );

   /* Now a big GPF trap: If created index added a new database field
      (FOR clause or Synthetic Index) all allocated structures for binding
      columns are now invalid and will GPF when unalloc
   */

   if( iLen != (int) hb_arrayLen( thiswa->aFields ) )
   {
      // Release structures
      ReleaseColStatements( thiswa, iLen );
      ReleaseInsertRecordStructure( thiswa, iLen );
      ReleaseCurrRecordStructure( thiswa, iLen );
      // Realloc structures
      SetCurrRecordStructure( thiswa );
      SetColStatements( thiswa );
      SetInsertRecordStructure( thiswa );
   }

   if( err == HB_SUCCESS )
   {
	  bOldReverseIndex = thiswa->bReverseIndex;
      thiswa->bReverseIndex = hb_arrayGetL( thiswa->aInfo, AINFO_REVERSE_INDEX );
   }

   return (err);
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOrderDestroy( SQLEXAREAP thiswa, LPDBORDERINFO pOrderInfo )
{
   thiswa->lBofAt     = 0;
   thiswa->lEofAt     = 0;
   thiswa->indexLevel = -1;

   ReleaseIndexBindStructure( thiswa );

   return (SUPER_ORDDESTROY( (AREAP) thiswa, pOrderInfo ));
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOrderInfo( SQLEXAREAP thiswa, USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LONG lIndexes;
   HB_ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("sqlExOrderInfo(%p, %hu, %p)", thiswa, uiIndex, pInfo));

   lIndexes = hb_itemSize( thiswa->aOrders );

   if( lIndexes )
   {
      switch( uiIndex )
      {
         case DBOI_KEYCOUNT:
         {
            long lValue;
            getWhereExpression( thiswa, LIST_FROM_TOP );
            createCountQuery( thiswa );

            if ( getFirstColumnAsLong( thiswa, &lValue ) == HB_FAILURE )
            {
               odbcErrorDiagRTE( thiswa->hStmt, "OrdKeyCount", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
               uiError = HB_FAILURE;
            }
            else
            {
               pInfo->itmResult = hb_itemPutNI( pInfo->itmResult, ( int ) lValue );
               uiError = HB_SUCCESS;
            }
            break;
         }
         case DBOI_POSITION:
         case DBOI_KEYNORAW:
         {
            pInfo->itmResult = hb_itemPutNL( pInfo->itmResult, ( long ) (thiswa->recordListPos + 1) );
            uiError = HB_SUCCESS;
            break;
         }
         case DBOI_SCOPETOP:
         case DBOI_SCOPEBOTTOM:
         case DBOI_SCOPETOPCLEAR:
         case DBOI_SCOPEBOTTOMCLEAR:
         case DBOI_SCOPESET:
         {
            uiError = SUPER_ORDINFO( ( AREAP ) thiswa, uiIndex, pInfo );
            thiswa->lBofAt = 0;
            thiswa->lEofAt = 0;
            thiswa->bConditionChanged1 = TRUE;
            thiswa->bConditionChanged2 = TRUE;
            break;
         }
         default:
         {
            uiError = SUPER_ORDINFO( ( AREAP ) thiswa, uiIndex, pInfo );
            bOldReverseIndex = thiswa->bReverseIndex;
            thiswa->bReverseIndex = hb_arrayGetL( thiswa->aInfo, AINFO_REVERSE_INDEX );      // OrderInfo() may change this flag
         }
      }
   }
   else
   {
		uiError = SUPER_ORDINFO( ( AREAP ) thiswa, uiIndex, pInfo );
   }

   return uiError;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExClearFilter( SQLEXAREAP thiswa )
{

   thiswa->lBofAt = 0;
   thiswa->lEofAt = 0;
   thiswa->bConditionChanged1 = TRUE;
   thiswa->bConditionChanged2 = TRUE;

   return (SUPER_CLEARFILTER( ( AREAP ) thiswa ));
}

/*------------------------------------------------------------------------*/

#define sqlExClearLocate				NULL
#define sqlExClearScope					NULL
#define sqlExCountScope					NULL
#define sqlExFilterText					NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExScopeInfo( SQLEXAREAP thiswa, USHORT nScope, PHB_ITEM pItem )
{
   thiswa->lBofAt = 0;
   thiswa->lEofAt = 0;
   thiswa->bConditionChanged1 = TRUE;
   thiswa->bConditionChanged2 = TRUE;
   return ( SUPER_SCOPEINFO( ( AREAP ) thiswa, nScope, pItem ) );
}

/*------------------------------------------------------------------------*/

//#define sqlExSetFilter					NULL     // Must be written to update thiswa->bConditionChanged
//culik 2010/07/07 implemented sqlExSetFilter
static HB_ERRCODE sqlExSetFilter( SQLEXAREAP thiswa, LPDBFILTERINFO pFilterInfo )
{
   HB_ERRCODE ret;
   ret = SUPER_SETFILTER( ( AREAP ) thiswa, pFilterInfo );
   if ( ret == HB_SUCCESS )
   {
      thiswa->bConditionChanged1 = TRUE;
      thiswa->bConditionChanged2 = TRUE;
   }
   return ret;

}
#define sqlExSetLocate					NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExSetScope( SQLEXAREAP thiswa, LPDBORDSCOPEINFO sInfo )
{
   thiswa->lBofAt = 0;
   thiswa->lEofAt = 0;
   thiswa->bConditionChanged1 = TRUE;
   thiswa->bConditionChanged2 = TRUE;
   return ( SUPER_SETSCOPE( ( AREAP ) thiswa, sInfo ) );
}

/*------------------------------------------------------------------------*/

#define sqlExSkipScope					NULL
#define sqlExLocate						NULL
#define sqlExCompile						NULL
#define sqlExError						NULL
#define sqlExEvalBlock					NULL
#define sqlExRawLock						NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExLock( SQLEXAREAP thiswa, LPDBLOCKINFO pLockInfo )
{
   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }
   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }
   hb_arraySetL( thiswa->aInfo, AINFO_BOF, thiswa->area.fBof );
   hb_arraySetL( thiswa->aInfo, AINFO_EOF, thiswa->area.fEof );
   hb_arraySetNL( thiswa->aInfo, AINFO_RECNO, GetCurrentRecordNum( thiswa ) );
   return( SUPER_LOCK( ( AREAP ) thiswa, pLockInfo ) );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExUnLock( SQLEXAREAP thiswa, PHB_ITEM pRecNo )
{
   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }
   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }
   hb_arraySetL( thiswa->aInfo, AINFO_BOF, thiswa->area.fBof );
   hb_arraySetL( thiswa->aInfo, AINFO_EOF, thiswa->area.fEof );
   hb_arraySetNL( thiswa->aInfo, AINFO_RECNO, GetCurrentRecordNum( thiswa ) );
   return( SUPER_UNLOCK( ( AREAP ) thiswa, pRecNo ) );
}

/*------------------------------------------------------------------------*/

#define sqlExCloseMemFile				NULL
#define sqlExCreateMemFile				NULL
#define sqlExGetValueFile				NULL
#define sqlExOpenMemFile				NULL
#define sqlExPutValueFile				NULL
#define sqlExReadDBHeader				NULL
#define sqlExWriteDBHeader				NULL
#define sqlExInit							NULL
#define sqlExExit							NULL
#define sqlExDrop							NULL
#define sqlExExists						NULL
#define sqlExInfo							NULL
#define sqlExWhoCares					NULL

/*------------------------------------------------------------------------*/

static const RDDFUNCS sqlTable =
{

   /* Movement and positioning methods */

   ( DBENTRYP_BP )  sqlExBof,
   ( DBENTRYP_BP )  sqlExEof,
   ( DBENTRYP_BP )  sqlExFound,
   ( DBENTRYP_V )   sqlExGoBottom,
   ( DBENTRYP_UL )  sqlExGoTo,
   ( DBENTRYP_I )   sqlExGoToId,
   ( DBENTRYP_V )   sqlExGoTop,
   ( DBENTRYP_BIB ) sqlExSeek,
   ( DBENTRYP_L )   sqlExSkip,
   ( DBENTRYP_L )   sqlExSkipFilter,
   ( DBENTRYP_L )   sqlExSkipRaw,


   /* Data management */

   ( DBENTRYP_VF )  sqlExAddField,
   ( DBENTRYP_B )   sqlExAppend,
   ( DBENTRYP_I )   sqlExCreateFields,
   ( DBENTRYP_V )   sqlExDeleteRec,
   ( DBENTRYP_BP )  sqlExDeleted,
   ( DBENTRYP_SP )  sqlExFieldCount,
   ( DBENTRYP_VF )  sqlExFieldDisplay,
   ( DBENTRYP_SSI ) sqlExFieldInfo,
   ( DBENTRYP_SCP ) sqlExFieldName,
   ( DBENTRYP_V )   sqlExFlush,
   ( DBENTRYP_PP )  sqlExGetRec,
   ( DBENTRYP_SI )  sqlExGetValue,
   ( DBENTRYP_SVL ) sqlExGetVarLen,
   ( DBENTRYP_V )   sqlExGoCold,
   ( DBENTRYP_V )   sqlExGoHot,
   ( DBENTRYP_P )   sqlExPutRec,
   ( DBENTRYP_SI )  sqlExPutValue,
   ( DBENTRYP_V )   sqlExRecall,
   ( DBENTRYP_ULP ) sqlExRecCount,
   ( DBENTRYP_ISI ) sqlExRecInfo,
   ( DBENTRYP_ULP ) sqlExRecNo,
   ( DBENTRYP_I )   sqlExRecId,
   ( DBENTRYP_S )   sqlExSetFieldExtent,


   /* WorkArea/Database management */

   ( DBENTRYP_CP )  sqlExAlias,
   ( DBENTRYP_V )   sqlExClose,
   ( DBENTRYP_VO )  sqlExCreate,
   ( DBENTRYP_SI )  sqlExInfo,
   ( DBENTRYP_V )   sqlExNewArea,
   ( DBENTRYP_VO )  sqlExOpen,
   ( DBENTRYP_V )   sqlExRelease,
   ( DBENTRYP_SP )  sqlExStructSize,
   ( DBENTRYP_CP )  sqlExSysName,
   ( DBENTRYP_VEI ) sqlExEval,
   ( DBENTRYP_V )   sqlExPack,
   ( DBENTRYP_LSP ) sqlExPackRec,
   ( DBENTRYP_VS )  sqlExSort,
   ( DBENTRYP_VT )  sqlExTrans,
   ( DBENTRYP_VT )  sqlExTransRec,
   ( DBENTRYP_V )   sqlExZap,


   /* Relational Methods */

   ( DBENTRYP_VR )  sqlExChildEnd,
   ( DBENTRYP_VR )  sqlExChildStart,
   ( DBENTRYP_VR )  sqlExChildSync,
   ( DBENTRYP_V )   sqlExSyncChildren,
   ( DBENTRYP_V )   sqlExClearRel,
   ( DBENTRYP_V )   sqlExForceRel,
   ( DBENTRYP_SSP ) sqlExRelArea,
   ( DBENTRYP_VR )  sqlExRelEval,
   ( DBENTRYP_SI )  sqlExRelText,
   ( DBENTRYP_VR )  sqlExSetRel,


   /* Order Management */

   ( DBENTRYP_VOI ) sqlExOrderListAdd,
   ( DBENTRYP_V )   sqlExOrderListClear,
   ( DBENTRYP_VOI ) sqlExOrderListDelete,
   ( DBENTRYP_VOI ) sqlExOrderListFocus,
   ( DBENTRYP_V )   sqlExOrderListRebuild,
   ( DBENTRYP_VOO ) sqlExOrderCondition,
   ( DBENTRYP_VOC ) sqlExOrderCreate,
   ( DBENTRYP_VOI )  sqlExOrderDestroy,
   ( DBENTRYP_SVOI ) sqlExOrderInfo,


   /* Filters and Scope Settings */

   ( DBENTRYP_V )    sqlExClearFilter,
   ( DBENTRYP_V )    sqlExClearLocate,
   ( DBENTRYP_V )    sqlExClearScope,
   ( DBENTRYP_VPLP ) sqlExCountScope,
   ( DBENTRYP_I )    sqlExFilterText,
   ( DBENTRYP_SI )   sqlExScopeInfo,
   ( DBENTRYP_VFI )  sqlExSetFilter,
   ( DBENTRYP_VLO )  sqlExSetLocate,
   ( DBENTRYP_VOS )  sqlExSetScope,
   ( DBENTRYP_VPL )  sqlExSkipScope,
   ( DBENTRYP_B )    sqlExLocate,


   /* Miscellaneous */

   ( DBENTRYP_CC )  sqlExCompile,
   ( DBENTRYP_I )   sqlExError,
   ( DBENTRYP_I )   sqlExEvalBlock,


   /* Network operations */

   ( DBENTRYP_VSP ) sqlExRawLock,
   ( DBENTRYP_VL )  sqlExLock,
   ( DBENTRYP_I )   sqlExUnLock,


   /* Memofile functions */

   ( DBENTRYP_V )    sqlExCloseMemFile,
   ( DBENTRYP_VO )   sqlExCreateMemFile,
   ( DBENTRYP_SCCS ) sqlExGetValueFile,
   ( DBENTRYP_VO )   sqlExOpenMemFile,
   ( DBENTRYP_SCCS ) sqlExPutValueFile,


   /* Database file header handling */

   ( DBENTRYP_V )     sqlExReadDBHeader,
   ( DBENTRYP_V )     sqlExWriteDBHeader,


   /* non WorkArea functions       */
   ( DBENTRYP_R )     sqlExInit,
   ( DBENTRYP_R )     sqlExExit,
   ( DBENTRYP_RVVL )  sqlExDrop,
   ( DBENTRYP_RVVL )  sqlExExists,
   ( DBENTRYP_RVVVL ) NULL,   /* sqlExRename */
   ( DBENTRYP_RSLV )  sqlExInfo,

   /* Special and reserved methods */

   ( DBENTRYP_SVP )   sqlExWhoCares
};

HB_FUNC( SQLEX ) {;}

HB_FUNC( SQLEX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;
/*
   startSQLEXSymbols();
*/
   uiCount = ( USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );

   HB_TRACE(HB_TR_DEBUG, ("SQLEX_GETFUNCTABLE(%p, %p)", uiCount, pTable));

   if( pTable )
   {
      HB_ERRCODE errCode;

      if ( uiCount )
         * uiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInherit( pTable, &sqlTable, &sqlExSuper, ( const char * ) "SQLRDD"  );
      hb_retni( errCode );
   }
   else
      hb_retni( HB_FAILURE );
}


#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
#  undef HB_PRG_PCODE_VER
#  define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

static void hb_sqlExRddInit( void * cargo )
{

   USHORT usResult;
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "SQLRDD", RDT_FULL ) <= 1 )
   {
      usResult = ( USHORT ) hb_rddRegister( "SQLEX", RDT_FULL );
      if( usResult <= 1 )
      {
         if( usResult == 0 )
         {
            PHB_DYNS pDynSym;
            HB_FUNC_EXEC( SR_INIT );

            pDynSym = hb_dynsymFind( "__SR_STARTSQL" );

            if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
            {
               hb_vmPushDynSym( pDynSym );
               hb_vmPushNil();
               hb_vmDo(0);
            }
         }
         return;
      }
   }

   hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );

   /* not executed, only to force DBF RDD linking */
   HB_FUNC_EXEC( SQLRDD );
}

HB_INIT_SYMBOLS_BEGIN( sqlEx1__InitSymbols )
{ "SQLEX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SQLEX )}, NULL },
{ "SQLEX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SQLEX_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( sqlEx1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_sqlEx_rdd_init_ )
   hb_vmAtInit( hb_sqlExRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_sqlEx_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup sqlEx1__InitSymbols
   #pragma startup _hb_sqlEx_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( sqlEx1__InitSymbols) \
                              HB_DATASEG_FUNC( _hb_sqlEx_rdd_init_)
   #include "hbiniseg.h"
   
#endif

HB_FUNC( SR_SETPAGEREADSIZE )
{
   if( ISNUM( 1 ) )
   {
      pageReadSize = hb_itemGetNL( hb_param( 1, HB_IT_NUMERIC ) );
   }
}

HB_FUNC( SR_SETBUFFERPOOLSIZE )
{
   if( ISNUM( 1 ) )
   {
      bufferPoolSize = hb_itemGetNL( hb_param( 1, HB_IT_NUMERIC ) );
   }
}

static int sqlKeyCompareEx( SQLEXAREAP thiswa, PHB_ITEM pKey, BOOL fExact )
{
   LONG lorder  = 0;
   PHB_ITEM pTag, pKeyVal, itemTemp;
   int iLimit, iResult = 0;
   BYTE len1, len2;
   const char * val1, * val2;
   char * valbuf = NULL;

   pTag = loadTagDefault( thiswa, NULL, &lorder );
   if( pTag )
   {
      if( thiswa->firstinteract )
      {
         SELF_GOTOP( (AREAP) thiswa );
         thiswa->firstinteract = 0;
      }

      itemTemp = hb_itemArrayGet( pTag, INDEX_KEY_CODEBLOCK );
      //if (pTag)
      //{
         //EVALINFO info;
         //hb_evalNew( &info, hb_itemArrayGet( pTag, INDEXMAN_KEY_CODEBLOCK ) );
         //pKey1 = hb_evalLaunch( &info );
         //hb_evalRelease( &info );
      //}

      if ( HB_IS_NUMBER( itemTemp ) )
      {
         pKeyVal = hb_itemArrayGet( thiswa->aBuffer, hb_arrayGetNL( pTag, INDEX_KEY_CODEBLOCK )-2 );
         len1 = hb_strRTrimLen( hb_itemGetCPtr( pKeyVal ), hb_itemGetCLen( pKeyVal ), FALSE ) - 15;
         val1 = hb_itemGetCPtr( pKeyVal );
      }
      else
      {
         EVALINFO info;
         hb_evalNew( &info, hb_itemArrayGet( pTag, INDEX_KEY_CODEBLOCK ) );
         pKeyVal = hb_evalLaunch( &info );
         hb_evalRelease( &info );
         len1 = hb_itemGetCLen( pKeyVal );
         val1 = hb_itemGetCPtr( pKeyVal );
      }
      hb_itemRelease( itemTemp );
      hb_itemRelease( pTag );
   }
   else
      return 0;

   if( HB_IS_DATE( pKey ) )
   {
      len2 = 8;
      valbuf = ( char * ) hb_xgrab(9 );
      val2 = hb_itemGetDS( pKey, valbuf );
   }
   else if( HB_IS_NUMBER( pKey ) )
   {
      PHB_ITEM pLen = hb_itemPutNL( NULL, (const LONG) len1 );
      val2 = valbuf = hb_itemStr( pKey, pLen, NULL );
      len2 = strlen( val2 );
      hb_itemRelease( pLen );
   }
   else if( HB_IS_LOGICAL( pKey ) )
   {
      len2 = 1;
      val2 = hb_itemGetL( pKey ) ? "T" : "F";
   }
   else
   {
      len2 = hb_itemGetCLen( pKey );
      val2 = hb_itemGetCPtr( pKey );
   }

   iLimit = (len1 > len2) ? len2 : len1;

   if ( HB_IS_STRING( pKeyVal ) )
   {
      if ( iLimit > 0 )
         iResult = memcmp( val1, val2, iLimit );

      if ( iResult == 0 )
      {
         if ( len1 >= len2 )
            iResult = 1;
         else if ( len1 < len2 && fExact )
            iResult = -1;
      }
      else
         iResult = 0;
   }
   else
   {
      if ( iLimit == 0 || (iResult = memcmp( val1, val2, iLimit )) == 0 )
      {
         if ( len1 >= len2 )
            iResult = 1;
         else if ( len1 < len2 )
            iResult = -1;
      }
   }

   if( valbuf )
      hb_xfree( valbuf );

   hb_itemRelease( pKeyVal );

   return iResult;
}

void SqlExLog( const char * str, int ver )
{
   /* 
   if ( SqlExIsLog() )
   {
      char date[9];
      //time_t timer = time(0);
      //tm *ltm = localtime(&timer);
      FILE * log;
      log = fopen( "sqlex.log", "a" );
      //fprintf(log, "%s %n:%n:%n : %s : %i\n", _strdate(date), ltm->tm_hour, ltm->tm_min, ltm->tm_sec, str, ver );
      fprintf(log, "%s : %s : %i\n", _strdate(date), str, ver );
      fclose(log);
   }
   */
}

BOOL SqlExIsLog()
{
/*
  if (_SqlExIsLogFirst)
  {
     _SqlExIsLogFirst = FALSE;

     if (FILE *file = fopen("sqlex.debug", "r"))
     {
        fclose(file);
        _SqlExIsLogFile = true;
     }
  }
*/  
  return _SqlExIsLogFile;
}
