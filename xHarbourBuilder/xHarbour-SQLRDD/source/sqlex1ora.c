/* $CATEGORY$sqlExOra/HIDE$FILES$HIDE$
* sqlExOra Main File
* Copyright (c) 2008 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved

Quick to do list, 2009 feb 23:

    4.4 - Review all filter functionalities in WHERE clauses and fully
          implement thiswa->bConditionChanged support
    4.5 - Good idea to bind both input and output for recor list

5 - Finish implementation of iColumnListStatus strategy described in
    getColumnListOra() function

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


#include "ocilib.h"
#include "sqlexora.h"

static RDDFUNCS sqlExOraSuper;
static PHB_DYNS s_pSym_SR_DESERIALIZE = NULL;
static PHB_DYNS s_pSym_TODATA = NULL;
static PHB_DYNS s_pSym_SR_FROMJSON = NULL;
// #define hb_xgrabz( n )        memset( hb_xgrab( ( n ) ), 0, ( n ) )
// #define hb_xmemdup( p, n )    memcpy( hb_xgrab( ( n ) ), ( p ), ( n
/*static PHB_DYNS s_pSym_SOLVERESTRICTORS;*/
/*
void startsqlExOraSymbols()
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

static BOOL CreateSkipStmtOra( SQLEXORAAREAP thiswa );
static int bOldReverseIndex  = 0;
static int sqlKeyCompareEx( SQLEXORAAREAP thiswa, PHB_ITEM pKey, BOOL fExact );
extern void SQLO_FieldGet( PHB_ITEM pField, PHB_ITEM pItem, int iField, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate,OCI_Resultset * rs );
extern HB_ERRCODE FeedSeekStmtOra( SQLEXORAAREAP thiswa, int queryLevel  );
HB_EXTERN_BEGIN
   extern  PHB_ITEM loadTagDefault( SQLEXORAAREAP thiswa, LPDBORDERINFO pInfo, LONG * lorder );
HB_EXTERN_END
/*------------------------------------------------------------------------*/
/*
static char * sqlSolveRestrictors( SQLEXORAAREAP thiswa )
{
   if( s_pSym_SOLVERESTRICTORS )
   {
      hb_objSendMessage( thiswa->sqlarea.oWorkArea, s_pSym_SOLVERESTRICTORS, 0 );
      return ( hb_itemGetCPtr( hb_stackReturnItem() ) );
   } else
      return "";
}
*/
#define LOGFILE "oci2.log"
/*------------------------------------------------------------------------*/


ULONGLONG GetCurrentRecordNumOra( SQLEXORAAREAP thiswa )
{
   if( thiswa->bIsInsert || thiswa->sqlarea.area.fEof )
   {
      return( thiswa->lLastRec );
   }
   else
   {
      return( (ULONGLONG) thiswa->recordList[thiswa->recordListPos] );
   }
}

/*------------------------------------------------------------------------*/

BOOL IsItemNull2( PHB_ITEM pFieldData, SQLEXORAAREAP thiswa )
{
   if( SR_itemEmpty2( pFieldData ) && (!(    HB_IS_ARRAY( pFieldData )
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

static HB_ERRCODE ConcludeSkipraw( SQLEXORAAREAP thiswa )
{
   /* Force relational movement in child WorkAreas */

   if( thiswa->sqlarea.area.lpdbRelations )
   {
      return SELF_SYNCCHILDREN(&thiswa->sqlarea.area );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static void sqlGetCleanBufferOra( SQLEXORAAREAP thiswa )
{
   HB_SIZE nPos, nLen;
   PHB_ITEM pCol;

   pCol = hb_itemNew( NULL );
   for( nPos = 1, nLen = hb_arrayLen( thiswa->sqlarea.aEmptyBuff ); nPos <= nLen; nPos++ )
   {
      hb_arrayGet( thiswa->sqlarea.aEmptyBuff, nPos, pCol );
      hb_arraySet( thiswa->sqlarea.aOldBuffer, nPos, pCol );
      hb_arraySetForward( thiswa->sqlarea.aBuffer, nPos, pCol );
   }

   hb_itemRelease( pCol );

   // fix lastrec()+1
   pCol = hb_arrayGetItemPtr( thiswa->sqlarea.aInfo, AINFO_RCOUNT );
   thiswa->lLastRec = hb_itemGetNL( pCol ) + 1;
   thiswa->sqlarea.area.fEof = TRUE;
}

/*------------------------------------------------------------------------*/

void setResultSetLimitOra( SQLEXORAAREAP thiswa, int iRows )
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

static void createRecodListQueryOra( SQLEXORAAREAP thiswa )
{
   if ( thiswa->sSql )
      memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   if( thiswa->sqlarea.ulhDeleted == 0 )
   {
      if (thiswa->bIsSelect)
      {
         sprintf( thiswa->sSql, "SELECT %s A.%c%s%c FROM (%s) A %s %s %s", thiswa->sLimit1,
                             OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),
                             thiswa->sqlarea.szDataFileName,
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

static void createCountQuery( SQLEXORAAREAP thiswa )
{
   if ( thiswa->sSql )
      memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   sprintf( thiswa->sSql, "SELECT COUNT( A.%c%s%c ) \nFROM %s A %s",
                          OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),
                          thiswa->sTable,
                          thiswa->sWhere );
}

/*------------------------------------------------------------------------*/

void getOrderByExpressionOra( SQLEXORAAREAP thiswa, BOOL bUseOptimizerHints )
{
   PHB_ITEM pIndexRef;

   if( bUseOptimizerHints )
   {
      // The the index phisical name
      if ( thiswa->sqlarea.hOrdCurrent > 0 )
      {
         pIndexRef = hb_arrayGetItemPtr( thiswa->sqlarea.aOrders, ( ULONG ) thiswa->sqlarea.hOrdCurrent );
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
      if ( thiswa->sqlarea.hOrdCurrent > 0 )
      {
         pIndexRef = hb_arrayGetItemPtr( thiswa->sqlarea.aOrders, ( ULONG ) thiswa->sqlarea.hOrdCurrent );
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

static HB_ERRCODE getMissingColumn( SQLEXORAAREAP thiswa, PHB_ITEM pFieldData, HB_LONG lFieldPosDB )
{
   PHB_ITEM pFieldStruct;
   char * colName;
   char sSql[DEFAULT_INDEX_COLUMN_MAX_LEN];
   BOOL  res;



   OCI_Resultset  *rs     ;

   pFieldStruct = hb_arrayGetItemPtr( thiswa->aFields, (HB_SIZE)lFieldPosDB );

   if( thiswa->colStmt[lFieldPosDB - 1].pStmt == NULL )
   {
//       res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->colStmt[lFieldPosDB - 1]) );
      thiswa->colStmt[lFieldPosDB - 1].pStmt  = OCI_StatementCreate(GetConnection(thiswa->hDbc));

      if ( thiswa->colStmt[lFieldPosDB - 1].pStmt   == NULL )
      {
         return (HB_FAILURE);
      }
      OCI_AllowRebinding( thiswa->colStmt[lFieldPosDB - 1].pStmt,1 ) ;

      colName      = QualifyName2( hb_arrayGetC( pFieldStruct, FIELD_NAME ), thiswa );

      if (thiswa->bIsSelect)
     {
        sprintf( sSql, "SELECT %c%s%c FROM (%s) WHERE %c%s%c = :sz001", OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ),
                                                               thiswa->sqlarea.szDataFileName,
                                                               OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ) );

      }
      else
      {
      sprintf( sSql, "SELECT %c%s%c FROM %s WHERE %c%s%c = :sz001", OPEN_QUALIFIER( thiswa ), colName, CLOSE_QUALIFIER( thiswa ),
                                                               thiswa->sTable,
                                                               OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ) );
      }
      hb_xfree( colName );

      res = OCI_Prepare( thiswa->colStmt[lFieldPosDB - 1].pStmt , (char *) sSql );

      if ( !res )
      {
         return (HB_FAILURE);
      }

//       res = SQLBindParameter( thiswa->colStmt[lFieldPosDB - 1], 1, SQL_PARAM_INPUT, SQL_C_ULONG, SQL_INTEGER, 15, 0, &(thiswa->lCurrentRecord), 0, NULL );
//          sprintf(szBind,":sr_rec%i",1);
         res  = OCI_BindUnsignedBigInt(thiswa->colStmt[lFieldPosDB - 1].pStmt , ":sz001", &thiswa->lCurrentRecord);

      if ( !res )
      {
         return (HB_FAILURE);
      }
   }

   thiswa->lCurrentRecord = GetCurrentRecordNumOra( thiswa );     // Feed bound parameter

//    res = sqlExOraecute( thiswa->colStmt[lFieldPosDB - 1] );
   res = OCI_Execute( thiswa->colStmt[lFieldPosDB - 1].pStmt  );
   //TraceLog("aaa.log","comando %s\n",OCI_GetSql( thiswa->colStmt[lFieldPosDB - 1].pStmt));

   if ( !res )
   {
      OraErrorDiagRTE( thiswa->colStmt[lFieldPosDB - 1].pStmt , "getMissingColumn/sqlExOraecute", sSql, res, __LINE__, __FILE__ );
//       SQLFreeStmt( thiswa->colStmt[lFieldPosDB - 1], SQL_CLOSE );
//          OCI_StatementFree( thiswa->colStmt[lFieldPosDB - 1].pStmt );
//          thiswa->colStmt[lFieldPosDB - 1].pStmt = NULL;
      return (HB_FAILURE);
   }

   // Now fetch and store result in pFieldData

//    res = SQLFetch( thiswa->colStmt[lFieldPosDB - 1] );
//    if ( res != SQL_SUCCESS )

      rs = OCI_GetResultset( thiswa->colStmt[lFieldPosDB - 1].pStmt   );
      if ( rs ==  NULL )
   {
      OraErrorDiagRTE( thiswa->colStmt[lFieldPosDB - 1].pStmt  , "getMissingColumn/SQLFetch", sSql, res, __LINE__, __FILE__ );
//       OCI_StatementFree( thiswa->colStmt->pStmt );
      return (HB_FAILURE);
   }

   OCI_FetchNext(rs);
//    bBuffer     = hb_xgrabDebug( __LINE__,__FILE__, COLUMN_BLOCK_SIZE + 1 );
//    memset( bBuffer, 0, COLUMN_BLOCK_SIZE ) ;
//    lLen        = COLUMN_BLOCK_SIZE;
//    lLenOut     = 0;

//    res = SQLGetData( thiswa->colStmt[lFieldPosDB - 1], 1, SQL_CHAR, (char*) bBuffer, lLen, &lLenOut );
//    if( res == SQL_SUCCESS )
//    {
//       odbcFieldGet( hb_arrayGetItemPtr( thiswa->aFields, lFieldPosDB   ), pFieldData, (char * ) bBuffer, lLenOut, 0, thiswa->nSystemID, FALSE );
      SQLO_FieldGet( hb_arrayGetItemPtr( thiswa->aFields, (HB_SIZE)lFieldPosDB  ), pFieldData, 1 , 0, thiswa->nSystemID, 0 , rs);
//    }
//    else
//    {
//       OraErrorDiagRTE( thiswa->colStmt[lFieldPosDB - 1], "getMissingColumn/SQLGetData", sSql, res, __LINE__, __FILE__ );
//       OCI_StatementFree( thiswa->colStmt[lFieldPosDB - 1]);
//       return (HB_FAILURE);
//    }

//    OCI_StatementFree( thiswa->colStmt[lFieldPosDB - 1].pStmt  );
//    thiswa->colStmt[lFieldPosDB - 1].pStmt = NULL;
//    hb_xfree( bBuffer );
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE SetBindValue2( PHB_ITEM pFieldData, COLUMNBINDORAP BindStructure, OCI_Statement * hStmt )
{
   BOOL bEmpty          = SR_itemEmpty2( pFieldData );



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
            BindStructure->asChar.value      = (char *) hb_xrealloc( BindStructure->asChar.value, nTrim + 1 );
            BindStructure->asChar.size_alloc = nTrim + 1;
         }

         if( nTrim >= BindStructure->asChar.size_alloc || nTrim > BindStructure->asChar.size )
         {
            hb_xmemcpy( BindStructure->asChar.value, pszText, nTrim );
            BindStructure->asChar.value[ nTrim ] = '\0';
            BindStructure->lIndPtr               = SQL_NTS;

         }
//          TraceLog("ccc.log" , "escrevendo lob pszText %s BindStructure->asChar.value %s nTrim %lu BindStructure->asChar.size %lu \n ", pszText,BindStructure->asChar.value,nTrim,BindStructure->asChar.size);

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
//          OCI_LobTruncate(BindStructure->lob1,0);
//           OCI_LobWrite(BindStructure->lob1, (void*)BindStructure->asChar.value,  BindStructure->asChar.size  );
         break;
      }
      case SQL_C_NUMERIC:
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

         BindStructure->asNumeric = (HB_ULONG) hb_itemGetNLL( pFieldData );
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

         BindStructure->asDouble = (double) hb_itemGetND( pFieldData );
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
         BindStructure->asDate.year  = (unsigned int) iYear;
         BindStructure->asDate.month = (unsigned int) iMonth;
         BindStructure->asDate.day   = (unsigned int) iDay;
         OCI_DateSetDate(BindStructure->asDate1,BindStructure->asDate.year, BindStructure->asDate.month, BindStructure->asDate.day) ;
         break;
      }
      case SQL_C_TYPE_TIMESTAMP:
      {
         int iYear, iMonth, iDay;
         int iHour, iMinute;
         BOOL bEmpty2 = SR_itemEmpty2( pFieldData );

         if( (!bEmpty2) && BindStructure->isBoundNULL && hStmt )     // Param was NULL, should be re-bound
         {
            BindStructure->isBoundNULL  = FALSE;
            BindStructure->lIndPtr      = 0;
         }
         else if( bEmpty2 && (!(BindStructure->isBoundNULL)) && hStmt )
         {
            BindStructure->lIndPtr      = SQL_NULL_DATA;
            BindStructure->isBoundNULL  = TRUE;
            break;
         }
         else if( bEmpty2 && BindStructure->isBoundNULL )
         {
            break;
         }

         {
#ifdef __XHARBOUR__
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
            BindStructure->asTimestamp.year     = (unsigned int) iYear;
            BindStructure->asTimestamp.month    = (unsigned int) iMonth;
            BindStructure->asTimestamp.day      = (unsigned int) iDay;
            BindStructure->asTimestamp.hour     = (unsigned int) iHour;
            BindStructure->asTimestamp.minute   = (unsigned int) iMinute;
            BindStructure->asTimestamp.second   = (unsigned int)seconds;
            BindStructure->asTimestamp.fraction = 0;
            OCI_DateSetDateTime(BindStructure->asDate2,BindStructure->asTimestamp.year, BindStructure->asTimestamp.month, BindStructure->asTimestamp.day,BindStructure->asTimestamp.hour,BindStructure->asTimestamp.minute,BindStructure->asTimestamp.second) ;
         }
         break;
      }
      case SQL_C_BIT:
      {
         BindStructure->asLogical = (HB_ULONG) hb_itemGetL( pFieldData );
         break;
      }
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE SetBindEmptylValue2( COLUMNBINDORAP BindStructure )
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

void ReleaseInsertRecordStructureOra( SQLEXORAAREAP thiswa, int iCols )
{
   COLUMNBINDORAP InsertRecord;
   if( thiswa->InsertRecord )
   {
      int n;
      if( iCols == 0 )
      {
         iCols = (int) hb_arrayLen( thiswa->aFields );
      }
      InsertRecord  = thiswa->InsertRecord;
      //TraceLog("aaa.log","liberando %lu colunas \n",iCols);
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
         if (InsertRecord->asDate1)
         {
            OCI_DateFree(InsertRecord->asDate1);
         }
         if (InsertRecord->asDate2)
         {
            OCI_DateFree(InsertRecord->asDate2);
         }
         if (InsertRecord->lob1)
         {
            OCI_LobFree(InsertRecord->lob1);
         }
         InsertRecord++;
      }
      hb_xfree( thiswa->InsertRecord );
      thiswa->InsertRecord = NULL;
   }
}

/*------------------------------------------------------------------------*/

void ReleaseCurrRecordStructureOra( SQLEXORAAREAP thiswa, int iCols )
{
   COLUMNBINDORAP CurrRecord;

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
         if (CurrRecord->asDate1)
            OCI_DateFree(CurrRecord->asDate1);
         if (CurrRecord->asDate2)
            OCI_DateFree(CurrRecord->asDate2);

         CurrRecord++;
      }
      hb_xfree( thiswa->CurrRecord );
   }
}

/*------------------------------------------------------------------------*/

void ReleaseColStatementsOra( SQLEXORAAREAP thiswa, int iCols )
{
   int i;
   if ( thiswa->colStmt )
   {
      if( iCols == 0 )
      {
        if ( thiswa->aFields  )
            iCols = (int) hb_arrayLen( thiswa->aFields );
      }

      for (i=0; i < iCols; i++)
       {
//         OCI_Statement *hStmt = (OCI_Statement*)thiswa->colStmt;
         if ( thiswa->colStmt[i].pStmt )
         {
            OCI_StatementFree(thiswa->colStmt[i].pStmt);
//             thiswa->colStmt = NULL;
         }
       }
       hb_xfree( thiswa->colStmt );
       thiswa->colStmt = NULL;

   }
}

/*------------------------------------------------------------------------*/

void SetColStatementsOra( SQLEXORAAREAP thiswa )
{
//    thiswa->colStmt = (PCOLUMNSTATEMENT  ) hb_xgrabz( hb_arrayLen( thiswa->aFields ) * sizeof( COLUMNSTATEMENT   ) );
//    memset( thiswa->colStmt, 0,  hb_arrayLen( thiswa->aFields ) * sizeof( COLUMNSTATEMENT   ) );

    thiswa->colStmt = (STATEMENT_DATA*  ) hb_xgrabz( hb_arrayLen( thiswa->aFields ) * sizeof( STATEMENT_DATA   ) );
    memset( thiswa->colStmt, 0,  hb_arrayLen( thiswa->aFields ) * sizeof( STATEMENT_DATA   ) );
}

/*------------------------------------------------------------------------*/

void ReleaseIndexBindStructureOra( SQLEXORAAREAP thiswa )
{
   int i, n, iCols;
   INDEXBINDORAP IndexBind;
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
               OCI_StatementFree( IndexBind->SkipFwdStmt);
            }
            if ( IndexBind->SkipBwdStmt )
            {
               OCI_StatementFree( IndexBind->SkipBwdStmt);
            }
            if ( IndexBind->SeekFwdStmt )
            {
               OCI_StatementFree( IndexBind->SeekFwdStmt);
            }
            if ( IndexBind->SeekBwdStmt )
            {
               OCI_StatementFree( IndexBind->SeekBwdStmt);
            }
            IndexBind++;
         }
         hb_xfree( thiswa->IndexBindings[i] );
         thiswa->IndexBindings[i] = NULL;
      }
   }
}

/*------------------------------------------------------------------------*/

COLUMNBINDORAP GetBindStructOra( SQLEXORAAREAP thiswa, INDEXBINDORAP IndexBind )
{
   COLUMNBINDORAP pBind = thiswa->CurrRecord;
   pBind += (IndexBind->lFieldPosDB -1);     // Place offset
   return pBind;
}

/*------------------------------------------------------------------------*/

static void BindAllIndexStmts( SQLEXORAAREAP thiswa )
{
   OCI_Statement *hStmt;
   INDEXBINDORAP IndexBind, IndexBindParam;
   COLUMNBINDORAP BindStructure;
   int iCol, iBind, iLoop;
   unsigned int  res = (unsigned int)SQL_ERROR;
   char * sSql;

   if( thiswa->sqlarea.hOrdCurrent == 0 )
   {
      // Natural order

      IndexBind = thiswa->IndexBindings[0];
      hStmt     = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
      sSql      = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdSql : IndexBind->SkipBwdSql;

      BindStructure = GetBindStructOra( thiswa, IndexBind );

//       res = SQLBindParameter( hStmt, 1, SQL_PARAM_INPUT,
//                                           BindStructure->iCType,
//                                           BindStructure->iSQLType,
//                                           BindStructure->ColumnSize,
//                                           BindStructure->DecimalDigits,
//                                           &(BindStructure->asNumeric), 0, NULL );
      res  = OCI_BindUnsignedBigInt(hStmt, BindStructure->szBindName, &BindStructure->asNumeric);

      if ( CHECK_SQL_N_OK( res ) )
      {
         OraErrorDiagRTE( hStmt, "BindAllIndexStmts", sSql, res, __LINE__, __FILE__ );
      }
   }
   else
   {
      IndexBind = thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ];

      for (iCol = 1; iCol <= thiswa->indexColumns; iCol++)
      {
         hStmt          = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
         sSql           = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdSql : IndexBind->SkipBwdSql;
         IndexBindParam = thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ];
         iBind          = 1;

         for (iLoop = 1; iLoop <= IndexBind->iLevel; iLoop++ )
         {
            BindStructure = GetBindStructOra( thiswa, IndexBindParam );
            if( !BindStructure->isArgumentNull )
            {
               switch (BindStructure->iCType)
               {
                  case SQL_C_CHAR:
                  {
//                      res = SQLBindParameter( hStmt, iBind, SQL_PARAM_INPUT,
//                                              BindStructure->iCType,
//                                              BindStructure->iSQLType,
//                                              BindStructure->ColumnSize,
//                                              BindStructure->DecimalDigits,
//                                              BindStructure->asChar.value, 0, NULL );
                     res =OCI_BindString(hStmt, BindStructure->szBindName, BindStructure->asChar.value, BindStructure->ColumnSize) ;
                     break;
                  }
                  case SQL_C_NUMERIC:
                  {
                    res = OCI_BindUnsignedBigInt(hStmt, BindStructure->szBindName, &BindStructure->asNumeric) ;
                    break;
                  }
                  case SQL_C_DOUBLE:
                  {
//                      res = SQLBindParameter( hStmt, iBind, SQL_PARAM_INPUT,
//                                              BindStructure->iCType,
//                                              BindStructure->iSQLType,
//                                              BindStructure->ColumnSize,
//                                              BindStructure->DecimalDigits,
//                                              &(BindStructure->asNumeric), 0, NULL );
                     res = OCI_BindDouble(hStmt, BindStructure->szBindName, &BindStructure->asDouble) ;
                     break;
                  }
                  case SQL_C_TYPE_TIMESTAMP:
                  {
                  BindStructure->asDate2 = OCI_DateCreate(GetConnection(thiswa->hDbc));
                  OCI_DateSetDateTime(BindStructure->asDate2,BindStructure->asTimestamp.year, BindStructure->asTimestamp.month, BindStructure->asTimestamp.day,BindStructure->asTimestamp.hour,BindStructure->asTimestamp.minute,BindStructure->asTimestamp.second) ;
                  res=OCI_BindDate(hStmt, BindStructure->szBindName, BindStructure->asDate2);

                     break;
                  }
                  case SQL_C_TYPE_DATE:
                  {
                                    BindStructure->asDate1 = OCI_DateCreate(GetConnection(thiswa->hDbc));
                  OCI_DateSetDate(BindStructure->asDate1,BindStructure->asDate.year, BindStructure->asDate.month, BindStructure->asDate.day) ;
                  res=OCI_BindDate(hStmt, BindStructure->szBindName, BindStructure->asDate1);
                     break;
                  }
                  case SQL_C_BIT:
                  {
                     res =  OCI_BindUnsignedBigInt( hStmt,BindStructure->szBindName,&BindStructure->asLogical)  ;
                     break;
                  }
               }
               if ( CHECK_SQL_N_OK( res ) )
               {
                  OraErrorDiagRTE( hStmt, "BindAllIndexStmts()", sSql, res, __LINE__, __FILE__ );
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

static void FeedCurrentRecordToBindings( SQLEXORAAREAP thiswa )
{
   PHB_ITEM pFieldData;
   int iCol;
   INDEXBINDORAP IndexBind;
   COLUMNBINDORAP BindStructure;
   BOOL newFieldData;

   if( thiswa->sqlarea.hOrdCurrent == 0 )
   {
      // Natural order, pretty simple
      BindStructure            = GetBindStructOra( thiswa, thiswa->IndexBindings[0] );
      BindStructure->asNumeric = GetCurrentRecordNumOra( thiswa );
   }
   else
   {
      IndexBind = thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ];

      for (iCol = 1; iCol <= thiswa->indexColumns; iCol++)
      {
         BindStructure = GetBindStructOra( thiswa, IndexBind );

         if( BindStructure->lFieldPosWA > 0 )
         {
            /*  Get item value from Workarea */
            pFieldData   = hb_arrayGetItemPtr( thiswa->sqlarea.aBuffer, (HB_SIZE)BindStructure->lFieldPosWA );
            newFieldData = FALSE;
         }
         else
         {
            pFieldData   = hb_itemNew( NULL );
            newFieldData = TRUE;
         }

         if ( IndexBind->lFieldPosDB == ( LONG ) (thiswa->sqlarea.ulhRecno) )
         {
            hb_itemPutNLL( pFieldData, thiswa->recordList[thiswa->recordListPos] );
         }

         if ( HB_IS_NIL( pFieldData ) )
         {
            getMissingColumn( thiswa, pFieldData, IndexBind->lFieldPosDB );
         }

         // Check if column is NULL

         if( SR_itemEmpty2( pFieldData ) && ( ( (thiswa->nSystemID == SYSTEMID_POSTGR) && HB_IS_DATE( pFieldData ) )
                                        || ( (thiswa->nSystemID != SYSTEMID_POSTGR) && ( !HB_IS_LOGICAL( pFieldData ) ) ) ) )
         {
            if( BindStructure->isNullable && BindStructure->isArgumentNull )
            {
               // It is STILL NULL, so no problem
               OCI_Statement *hStmt = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
               SetBindValue2( pFieldData, BindStructure, hStmt );
            }
            else if( !BindStructure->isNullable )
            {
               // Just get an empty value to be bound, because column is NOT nullable
               SetBindEmptylValue2( BindStructure );
            }
            else if( BindStructure->isNullable && ( !BindStructure->isArgumentNull ) )
            {
               // Now we have a problem. Current record column is NULL, database accept NULLS
               // but query in NOT prepared for NULL values. So we must RE-PREPARE all queries

               thiswa->bConditionChanged1 = TRUE;
               CreateSkipStmtOra( thiswa );
               BindAllIndexStmts( thiswa );
               FeedCurrentRecordToBindings( thiswa );     // Recursive call
            }
         }
         else
         {
            OCI_Statement *hStmt = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
            SetBindValue2( pFieldData, BindStructure, hStmt );
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

void SolveFiltersOra( SQLEXORAAREAP thiswa, BOOL bWhere )
{
   /*
   *  Resolve SET FILTER TO
   */

   char * temp;

   if( thiswa->sqlarea.sqlfilter )
   {
      char * sFilter = getMessageC( thiswa->sqlarea.oWorkArea, "CFILTER" );
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
     char * sFilter = getMessageC( thiswa->sqlarea.oWorkArea, "CFILTER" );
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

   if ( thiswa->sqlarea.hOrdCurrent > 0 )
   {
      PHB_ITEM pIndexRef = hb_arrayGetItemPtr( thiswa->sqlarea.aOrders, ( ULONG ) thiswa->sqlarea.hOrdCurrent );
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
      char * sFilter = getMessageC( thiswa->sqlarea.oWorkArea, "CSCOPE" );
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
      char * sFilter = getMessageC( thiswa->sqlarea.oWorkArea, "CFLTUSR" );
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

void SetIndexBindStructureOra( SQLEXORAAREAP thiswa )
{
   PHB_ITEM pColumns, pIndexRef;
   INDEXBINDORAP IndexBind;
   int i;

   if ( thiswa->sqlarea.hOrdCurrent > 0 )
   {
      pIndexRef = hb_arrayGetItemPtr( thiswa->sqlarea.aOrders, ( ULONG ) thiswa->sqlarea.hOrdCurrent );
      pColumns = hb_arrayGetItemPtr( pIndexRef, INDEX_FIELDS );
      thiswa->indexColumns = hb_arrayLen( pColumns );

      // Alloc memory for binding structures
      thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ] = (INDEXBINDORAP) hb_xgrabz( thiswa->indexColumns * sizeof( INDEXBINDORA ) );
//       memset( thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ], 0, thiswa->indexColumns * sizeof( INDEXBIND ) );

      // Now we should bind all index columns to be used by SKIP

      IndexBind = thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ];

      for ( i=1; i <= thiswa->indexColumns; i++ )
      {
         IndexBind->lFieldPosDB     = hb_arrayGetNL( hb_arrayGetItemPtr( pColumns, i ), 2 );
         IndexBind->hIndexOrder     = thiswa->sqlarea.hOrdCurrent;
         IndexBind->iLevel          = i;
         IndexBind->iIndexColumns   = thiswa->indexColumns;
         IndexBind++;
      }
   }
   else
   {
      thiswa->indexColumns = 1;     // Natural order, RECNO
      // Alloc memory for binding structures
      thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ] = (INDEXBINDORAP) hb_xgrabz( thiswa->indexColumns * sizeof( INDEXBINDORA ) );
//       memset( thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ], 0, thiswa->indexColumns * sizeof( INDEXBIND ) );
      IndexBind = thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ];
      IndexBind->lFieldPosDB     = thiswa->sqlarea.ulhRecno;
      IndexBind->hIndexOrder     = 0;
      IndexBind->iLevel          = 1;
      IndexBind->iIndexColumns   = 1;
   }
}

/*------------------------------------------------------------------------*/

void SetCurrRecordStructureOra( SQLEXORAAREAP thiswa )
{
   PHB_ITEM pFieldStruct, pFieldLen, pFieldDec;
   int i, iCols;
   LONG lType;
   char cType;
   COLUMNBINDORAP BindStructure;

   iCols = (int) hb_arrayLen( thiswa->aFields );

   thiswa->CurrRecord = (COLUMNBINDORAP) hb_xgrabz( iCols * sizeof( COLUMNBINDORA ) );
//    memset( thiswa->CurrRecord, 0, iCols * sizeof( COLUMNBIND ) );

   BindStructure = thiswa->CurrRecord;

   for( i = 1; i <= iCols; i++ )
   {
      pFieldStruct = hb_arrayGetItemPtr( thiswa->aFields, i );
      pFieldLen    = hb_arrayGetItemPtr( pFieldStruct, FIELD_LEN );
      pFieldDec    = hb_arrayGetItemPtr( pFieldStruct, FIELD_DEC );
      lType        = hb_arrayGetNL( pFieldStruct, FIELD_DOMAIN );
      cType        = ( * hb_arrayGetCPtr( pFieldStruct, FIELD_TYPE ));

      BindStructure->iSQLType        = (int)lType;
      BindStructure->isNullable      = hb_arrayGetL( pFieldStruct, FIELD_NULLABLE );
      BindStructure->isBoundNULL     = FALSE;
      BindStructure->isArgumentNull  = FALSE;
      BindStructure->lFieldPosDB     = i;
      BindStructure->lFieldPosWA     = hb_arrayGetNL( pFieldStruct, FIELD_WAOFFSET );
     BindStructure->ColumnSize      = (unsigned int) hb_itemGetNI( pFieldLen );
     BindStructure->DecimalDigits   = (unsigned short) hb_itemGetNI( pFieldDec );
      BindStructure->colName         = QualifyName2( hb_arrayGetC( pFieldStruct, FIELD_NAME ), thiswa );
      sprintf(BindStructure->szBindName,":%s",hb_arrayGetCPtr( pFieldStruct, FIELD_NAME ));

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
            BindStructure->asChar.value      = (char *) hb_xgrabz( BindStructure->ColumnSize + 1 );
//             memset(BindStructure->asChar.value ,0, BindStructure->ColumnSize + 1 ); // Culik Zero all memory
            BindStructure->asChar.size_alloc = BindStructure->ColumnSize + 1;
            BindStructure->iCType            = SQL_C_CHAR;
            BindStructure->asChar.size       = 0;
            break;
         }
         case 'M':
         {
            BindStructure->iCType            = SQL_C_BINARY;
            BindStructure->asChar.value      = (char *) hb_xgrabz( INITIAL_MEMO_ALLOC );
//             memset(BindStructure->asChar.value ,0, INITIAL_MEMO_ALLOC ); // Culik Zero all memory
            BindStructure->asChar.size_alloc = INITIAL_MEMO_ALLOC;
            BindStructure->asChar.size       = 0;
            BindStructure->asChar.value[0]   = '\0';
            BindStructure->ColumnSize        = 0;
            break;
         }
         case 'N':
         {
           if (BindStructure->DecimalDigits  > 0 )
            BindStructure->iCType          = SQL_C_DOUBLE;
            else
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
               BindStructure->iCType          = SQL_C_TYPE_DATE;        // May be DATE or TIMESTAMP

            break;
         }
         case 'L':
         {
            BindStructure->iCType          = SQL_C_BIT;
            break;
         }
      }
      BindStructure++;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getWhereExpressionOra( SQLEXORAAREAP thiswa, int iListType )
{
   // This function creates WHERE expression to some workarea movment methods,
   // including dbGoTop()/dbGobottom() and dbSkip()

   BOOL bWhere = FALSE;
   int iCol;
   PHB_ITEM pFieldData, pTemp;
   BOOL bArgumentIsNull;
   BOOL bDirectionFWD;
   COLUMNBINDORAP BindStructure;
   char * temp;
   // Culik Let Clear all memorym this is more eficient and safe the adding an \0 to position 0
   memset( thiswa->sWhere, 0 ,MAX_SQL_QUERY_LEN / 10 * sizeof( char ) );
   //thiswa->sWhere[0] = '\0';
   thiswa->bConditionChanged1 = FALSE;

   // Resolve record or index navigation

   if( iListType == LIST_SKIP_FWD || iListType == LIST_SKIP_BWD )
   {
      INDEXBINDORAP IndexBind = thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ];

      thiswa->recordListDirection = ( iListType == LIST_SKIP_FWD ? LIST_FORWARD : LIST_BACKWARD );
      bDirectionFWD               = iListType == LIST_SKIP_FWD;

      if( thiswa->bReverseIndex )
      {
         bDirectionFWD = !bDirectionFWD;
      }

      if( thiswa->sqlarea.hOrdCurrent == 0 )      // Natural order
      {

         BindStructure              = GetBindStructOra( thiswa, IndexBind );
         BindStructure->asNumeric   = GetCurrentRecordNumOra( thiswa );
         sprintf( thiswa->sWhere, "\nWHERE A.%c%s%c %s %s", OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ),
                                                           bDirectionFWD ? ">=" : "<=" ,BindStructure->szBindName);
         bWhere = TRUE;
      }
      else
      {
         for (iCol = 1; iCol <= thiswa->indexLevel; iCol++)
         {
            BindStructure = GetBindStructOra( thiswa, IndexBind );

            pTemp        = NULL;
            pFieldData   = NULL;

            if( BindStructure->lFieldPosWA > 0 )
            {
               /*  Get item value from Workarea */
               pFieldData   = hb_arrayGetItemPtr( thiswa->sqlarea.aBuffer, (HB_SIZE)BindStructure->lFieldPosWA );
            }

            if ( BindStructure->lFieldPosDB == ( LONG ) (thiswa->sqlarea.ulhRecno) )
            {
               pTemp = hb_itemNew( NULL );
               hb_itemPutNLL( pTemp, thiswa->recordList[thiswa->recordListPos] );
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

            bArgumentIsNull = BindStructure->isNullable && IsItemNull2( pFieldData, thiswa );

            if( iCol == thiswa->indexLevel )
            {
               BindStructure->isArgumentNull  = bArgumentIsNull;

               if( !bArgumentIsNull )
               {
                  // Bind column value only if argument is NOT null
                  OCI_Statement *hStmt = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
                  SetBindValue2( pFieldData, BindStructure, hStmt );
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
               sprintf( thiswa->sWhere, "%s %s A.%c%s%c %s %s", bWhere ? temp : "\nWHERE", bWhere ? "AND" : "",
                                                          OPEN_QUALIFIER( thiswa ), BindStructure->colName, CLOSE_QUALIFIER( thiswa ),
                                                          iCol == thiswa->indexLevel ? ( bDirectionFWD ? ">=" : "<=" ) : "=" ,BindStructure->szBindName);
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

   SolveFiltersOra( thiswa, bWhere );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

HB_ERRCODE getWorkareaParamsOra( SQLEXORAAREAP thiswa )
{
   ULONG lCnnType;
   if( !thiswa->oSql )
   {
     thiswa->sqlarea.lpdbPendingRel  = NULL;
      thiswa->oSql            = getMessageItem( thiswa->sqlarea.oWorkArea, "OSQL" );
      thiswa->aFields         = getMessageItem( thiswa->sqlarea.oWorkArea, "AFIELDS" );
      thiswa->hDbc            = (OCI_ORASESSION*) getMessagePtr( thiswa->oSql, "HDBC" );
      thiswa->nSystemID       = getMessageNL( thiswa->oSql, "NSYSTEMID" );
      thiswa->sTable          = getMessageC( thiswa->sqlarea.oWorkArea, "CQUALIFIEDTABLENAME" );
      thiswa->sOwner          = getMessageC( thiswa->sqlarea.oWorkArea, "COWNER" );
      thiswa->sRecnoName      = getMessageC( thiswa->sqlarea.oWorkArea, "CRECNONAME" );
      thiswa->sDeletedName    = getMessageC( thiswa->sqlarea.oWorkArea, "CDELETEDNAME" );
      thiswa->iTCCompat       = getMessageNI( thiswa->sqlarea.oWorkArea, "NTCCOMPAT" );
      thiswa->bHistoric       = getMessageL( thiswa->sqlarea.oWorkArea, "LHISTORIC" );
      thiswa->bOracle12       = getMessageL( thiswa->oSql, "LORACLE12" );

      thiswa->sRecnoName   = QualifyName2( thiswa->sRecnoName, thiswa );
      thiswa->sDeletedName = QualifyName2( thiswa->sDeletedName, thiswa );


      SetColStatementsOra( thiswa );
   }

   if( !thiswa->bConnVerified )
   {
      lCnnType = getMessageNL( thiswa->oSql, "NCONNECTIONTYPE" );

      if( !(lCnnType == CONNECT_ORACLE || lCnnType == CONNECT_ORACLE_QUERY_ONLY) )
      {
         commonError( &thiswa->sqlarea.area, EG_OPEN, ESQLRDD_OPEN, "sqlExOra supports only ODBC connections." );
         return HB_FAILURE;
         }
      thiswa->bConnVerified = TRUE;
   }
      thiswa->bIsSelect        = getMessageL( thiswa->sqlarea.oWorkArea, "LTABLEISSELECT" );
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getPreparedRecordList( SQLEXORAAREAP thiswa, int iMax ) // Returns TRUE if any result found
{
   unsigned int  res;
   int i, recordListChanged;
   INDEXBINDORAP IndexBind;
   OCI_Statement *hStmt;
   HB_ULONG lRecord;
   char * sSql;
   OCI_Resultset  *rs     ;
   //TraceLog("aaa.log", "estou em  getPreparedRecordList %p \n",thiswa->hStmtSkip);
   IndexBind = thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ];
   // not nedded
   //IndexBind += (thiswa->indexLevel - 1);    // Place Offset

   hStmt = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdStmt : IndexBind->SkipBwdStmt;
   sSql  = thiswa->recordListDirection == LIST_FORWARD ? IndexBind->SkipFwdSql : IndexBind->SkipBwdSql;

   res = OCI_Execute( hStmt );


   //TraceLog("aaa.log","comando %s\n",OCI_GetSql( hStmt));

   if ( !res )
   {
      OraErrorDiagRTE( hStmt, "getPreparedRecordList", sSql, res, __LINE__, __FILE__ );
      OCI_StatementFree( hStmt );

      return (HB_FAILURE);
   }

   // Should SKIP over results to ignore current line
   // Note: If we're in a deeper simplification level in current index
   //       navigation, we can have MANY processed lines in result set,
   //       so we should FETCH until find the current line to re-start
   //       appending records to RecordList

   // Current line is always in result set if we are SKIPPING
  rs = OCI_GetResultset( hStmt);

   do
   {
//       res = SQLFetch( hStmt );

      if ( rs == NULL)
      {
         // Ops, where are previously retrieved record ?
         // Run query again and try to find it in result
         // set since it can be deleted by other user - MISSING!!!
//          OCI_StatementFree(thiswa->hStmtSkip);
//          thiswa->hStmtSkip = NULL;
         return HB_RETRY;
      }

      if (!OCI_FetchNext(rs))
      {
//          OCI_StatementFree(thiswa->hStmtSkip);
//          thiswa->hStmtSkip = NULL;
         return HB_RETRY;
      }


//       res = SQLGetData( hStmt, 1, SQL_C_ULONG, &lRecord, sizeof( SQL_C_ULONG ), NULL );
       lRecord= OCI_GetUnsignedBigInt( rs,1 ) ;
//        ////TraceLog("aaa.log", "getPreparedRecordList 1 registro %i pego %lu\n",1,lRecord);

//       if ( CHECK_SQL_N_OK( res ) )
//       {
//          return (HB_FAILURE);       // Any other error means a fault in SQL statement
//       }
//        ////TraceLog("aaa.log","thiswa->lCurrentRecord %lu lRecord %lu\n",thiswa->lCurrentRecord , lRecord);
   }
   while ( thiswa->lCurrentRecord != lRecord );

   recordListChanged = 0;

   for( i=0; i<iMax; i++ )
   {
//       res = SQLFetch( hStmt );
//       rs = OCI_GetResultset( thiswa->colStmt  );
      res =   OCI_FetchNext(rs);
      if ( !res )
      {

         if( i > 0 && thiswa->indexLevel == 1 )//  && rs.fetch_status == OCI_NO_DATA ) //&& res == SQL_NO_DATA_FOUND )
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
         else if( i == 0 && thiswa->indexLevel == 1)// && rs->fetch_status == OCI_NO_DATA  && thiswa->recordListSize > 0 )//&& res == SQL_NO_DATA_FOUND && thiswa->recordListSize > 0 )
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

      thiswa->recordList[i]= OCI_GetUnsignedBigInt( rs,1 ) ;

      recordListChanged++;

      if( thiswa->sqlarea.ulhDeleted > 0 )
      {
         char szValue[2];
         unsigned int uiLen;

         if ( OCI_GetString(rs,2 ) == NULL )
         {

//          OCI_StatementFree(thiswa->hStmtSkip);
//          thiswa->hStmtSkip = NULL;
            return (HB_FAILURE);
         }
         else
         {
           uiLen = OCI_GetDataLength(rs,2);

            hb_xmemcpy( szValue,(char*)OCI_GetString(rs,2 ),uiLen ) ;
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

//    OCI_StatementFree(thiswa->hStmtSkip);
//          thiswa->hStmtSkip = NULL;
   if( recordListChanged )
   {
      thiswa->recordListSize = (ULONG) i;
      thiswa->recordListPos  = 0;
      return RESULTSET_OK;
   }

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getRecordList( SQLEXORAAREAP thiswa, int iMax ) // Returns TRUE if any result found
{
   unsigned int  res;
   int i, recordListChanged;
   OCI_Resultset  *rs     ;

//    res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmt) );
   thiswa->hStmt=OCI_StatementCreate( GetConnection(thiswa->hDbc) );

   if ( thiswa->hStmt == NULL )
   {
      return (HB_FAILURE);
   }

   res= OCI_ExecuteStmt( thiswa->hStmt, thiswa->sSql);

   if ( !res )
   {
      return (HB_FAILURE);    // It means a fault in SQL statement
   }

   recordListChanged = 0;
   rs = OCI_GetResultset( thiswa->hStmt);
   for( i=0; i<iMax; i++ )
   {
//       res = SQLFetch( thiswa->hStmt );
      res = OCI_FetchNext(rs);
      if ( !res)
      {
         if( i > 0 && !res )
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
//          OCI_StatementFree( thiswa->hStmt );
         break;
      }
//       res = SQLGetData( thiswa->hStmt, 1, SQL_C_ULONG, &(thiswa->recordList[i]), sizeof( SQL_C_ULONG ), NULL );
      thiswa->recordList[i]= OCI_GetUnsignedBigInt( rs,1 ) ;
//       ////TraceLog("aaa.log", "getRecordList registro %i pego %lu\n",i,thiswa->recordList[i]);

      recordListChanged++;

      if( thiswa->sqlarea.ulhDeleted > 0 )
      {
         char szValue[2];
         unsigned int uiLen;
         if ( OCI_GetString(rs,2 ) == NULL )
         {
             OCI_StatementFree( thiswa->hStmt);
            return (HB_FAILURE);
         }
         else
         {
            uiLen = OCI_GetDataLength(rs,2);

            hb_xmemcpy( szValue,(char*)OCI_GetString(rs,2 ),uiLen ) ;
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

    OCI_StatementFree( thiswa->hStmt);

   if( recordListChanged )
   {
      thiswa->recordListSize = (ULONG) i;
      thiswa->recordListPos  = 0;
      return RESULTSET_OK;
   }

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE getFirstColumnAsLong( SQLEXORAAREAP thiswa, HB_ULONG * szValue ) // Returns OK if result set could be get
{
   int res;
   OCI_Resultset  *rs     ;

//    res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmt) );
   thiswa->hStmt= OCI_StatementCreate(GetConnection(thiswa->hDbc));

   if( thiswa->hStmt == NULL)
      return (HB_FAILURE);

   res = OCI_ExecuteStmt( thiswa->hStmt, ( char * ) thiswa->sSql );

   if( !res )
      return (HB_FAILURE);    // It means a fault in SQL statement

//    res = SQLFetch( thiswa->hStmt );
   rs = OCI_GetResultset( thiswa->hStmtNextval);
   if( rs == NULL)
   {
       OCI_StatementFree( thiswa->hStmt);
      return (HB_FAILURE);    // It means a fault in SQL statement
   }

//    res = SQLGetData( thiswa->hStmt, 1, SQL_C_ULONG, szValue, sizeof( SQL_C_ULONG ), NULL );
   *szValue= OCI_GetUnsignedBigInt( rs,1 ) ;
//    ////TraceLog("aaa.log", "getFirstColumnAsLong registro %i pego %lu\n",1,*szValue);

//    if( res == SQL_ERROR )
//    {
//        OCI_StatementFree( thiswa->hStmt);
//       return (HB_FAILURE);
//    }

    OCI_StatementFree( thiswa->hStmt);

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

BOOL getColumnListOra( SQLEXORAAREAP thiswa )
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

   2. New field list was created by this function (getColumnListOra) so now
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

   colName = (char *) hb_xgrabz( HB_SYMBOL_NAME_LEN + 1 );

   if ( thiswa->sqlarea.iFieldListStatus == FIELD_LIST_LEARNING )
   {
      if (!thiswa->sFields)
      {
         thiswa->sFields = (char *) hb_xgrabz( FIELD_LIST_SIZE * sizeof( char ) );
         uiFlds = 0;
         for ( n=1; n <= thiswa->sqlarea.area.uiFieldCount; n++ )
         {
            pField  = thiswa->sqlarea.area.lpFields + n -1;
            fName   = (char *) hb_dynsymName( ( PHB_DYNS ) pField->sym );
            len     = strlen( fName );
            memset(colName,0,HB_SYMBOL_NAME_LEN);
            hb_xmemcpy( colName, fName, len );
            colName = QualifyName2( colName, thiswa );
            colName[len] = '\0';

            if ( uiFlds == 0 )
            {
               // Should ALWAYS ask for RECNO in first column to
               // be used in the BufferPool
               if( thiswa->sqlarea.ulhDeleted == 0 )
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
   else if ( thiswa->sqlarea.iFieldListStatus == FIELD_LIST_CHANGED || thiswa->sqlarea.iFieldListStatus == FIELD_LIST_NEW_VALUE_READ )
   {
      uiFlds = 0;
      if (!thiswa->sFields)
      {
         thiswa->sFields = (char *) hb_xgrabz( FIELD_LIST_SIZE * sizeof( char ) );
      }
      for ( n=1; n <= thiswa->sqlarea.area.uiFieldCount; n++ )
      {
         if ( thiswa->sqlarea.uiFieldList[n-1] )
         {
            pField  = thiswa->sqlarea.area.lpFields + n -1;
            fName   = (char *) hb_dynsymName( ( PHB_DYNS ) pField->sym );
            len     = strlen( fName );
            memset(colName,0,HB_SYMBOL_NAME_LEN);
            hb_xmemcpy( colName, fName, len );
            colName = QualifyName2( colName, thiswa );
            colName[len] = '\0';

            if ( uiFlds == 0 )
            {
               // Should ALWAYS ask for RECNO in first column to
               // be used in the BufferPool

               if( thiswa->sqlarea.ulhDeleted == 0 )
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
      thiswa->sqlarea.iFieldListStatus = FIELD_LIST_STABLE;
      return TRUE;
   }
   hb_xfree( colName );
   return FALSE;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE updateRecordBuffer( SQLEXORAAREAP thiswa, BOOL bUpdateDeleted )
{
   unsigned int  res;

   HB_ULONG lCurrRecord;
   HB_SIZE lPos;
   BOOL bTranslate;
   HB_ISIZ i                   ,iRow;
   HB_SIZE   iIndex, iEnd;
   PHB_ITEM aRecord, pKey;
   PHB_ITEM temp;
   OCI_Resultset  *rs     ;
   char * szEnd;

//   HB_ITEM temp;

   // To do: Must check if buffer pool have to be clared due to change in
   // column list


   // First, try to look for record in current buffer pool

   pKey    = hb_itemNew( NULL );
   hb_itemPutNLL( pKey, thiswa->recordList[thiswa->recordListPos] );

   if (!bUpdateDeleted)       // Cache NEVER holds deleted() information
   {
      if ( hb_hashScan( thiswa->hBufferPool, pKey, &lPos  ) )
      {
         aRecord = hb_hashGetValueAt( thiswa->hBufferPool, lPos );
         hb_arrayCopy( aRecord, thiswa->sqlarea.aBuffer, NULL, NULL, NULL );
         hb_itemRelease( pKey );
         return HB_SUCCESS;
      }
   }

   // Check for maximum buffer pool size

   if( ( hb_hashLen(thiswa->hBufferPool)) > bufferPoolSize )
   {
      hb_hashNew( thiswa->hBufferPool );
      hb_hashPreallocate( thiswa->hBufferPool,  ( bufferPoolSize * 2 ) );
   }

   // Not found, so let's try the database...

   if( getColumnListOra( thiswa )  || thiswa->hStmtBuffer == NULL)     // Check if field list has changed and if so
                                     // creates a new one in thiswa structure
   {

      thiswa->bConditionChanged2 = TRUE;     // SEEK statements are no longer valid - column list has changed!
      memset(thiswa->sSqlBuffer , 0 , MAX_SQL_QUERY_LEN / 5  * sizeof( char ) );

      if( thiswa->bIsSelect )
      {
         sprintf( thiswa->sSqlBuffer, "SELECT %s FROM (%s) A WHERE A.%c%s%c IN ( :1", thiswa->sFields, thiswa->sqlarea.szDataFileName, OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ));
      }
      else
      {
         sprintf( thiswa->sSqlBuffer, "SELECT %s \nFROM %s A \nWHERE A.%c%s%c IN ( :1", thiswa->sFields, thiswa->sTable, OPEN_QUALIFIER( thiswa ), thiswa->sRecnoName, CLOSE_QUALIFIER( thiswa ) );
      }
////TraceLog("aaa.log","thiswa->sSqlBuffer %s\n",thiswa->sSqlBuffer);
//       iEnd = ( USHORT ) strlen( thiswa->sSqlBuffer );
//       for ( i = 20; i < (MAX_SQL_QUERY_LEN/5); i++ )
//       {
//          if( thiswa->sSqlBuffer[i] == '?' )
//          {
//            thiswa->sSqlBuffer[i]  = ":";
//            thiswa->sSqlBuffer[++i]  = "s";
//            thiswa->sSqlBuffer[++i]  = "z";
//            thiswa->sSqlBuffer[++i]  = "0";
//            thiswa->sSqlBuffer[++i]  = "0";
//            thiswa->sSqlBuffer[++i]  = "1";
//             iEnd = i;
//             break;
//          }
//       }
//       iEnd = ( USHORT ) strlen( thiswa->sSqlBuffer );
//       for ( i = 20; i < (MAX_SQL_QUERY_LEN/5); i++ )
//       {
//          if( thiswa->sSqlBuffer[i] == '?' )
//          {
//            thiswa->sSqlBuffer[i]  = ' ';
//             iEnd = i;
//             break;
//          }
//       }
  ////TraceLog("aaa.log","thiswa->sSqlBuffer %s\n",thiswa->sSqlBuffer);

      // Adjust SQL to 'pageReadSize' params

      for ( i = 1; i < pageReadSize; i++ )
      {
        char *tempBuff = hb_strdup( (const char *) thiswa->sSqlBuffer );
        sprintf(thiswa->sSqlBuffer,"%s,:%i",tempBuff,i+1);
        iEnd = (HB_SIZE)strlen(thiswa->sSqlBuffer);
        //TraceLog("aaa.log","montando    thiswa->sSqlBuffer %s\n",thiswa->sSqlBuffer);
        hb_xfree(tempBuff);
      }
//TraceLog("aaa.log","thiswa->sSqlBuffer %s\n",thiswa->sSqlBuffer);
      szEnd=hb_strdup( (const char *) thiswa->sSqlBuffer );
      sprintf(thiswa->sSqlBuffer,"%s)",szEnd);
//TraceLog("aaa.log","thiswa->sSqlBuffer %s %s\n",thiswa->sSqlBuffer,szEnd);
      iEnd = (HB_SIZE)strlen(thiswa->sSqlBuffer);
      hb_xfree(szEnd);

      thiswa->sSqlBuffer[++iEnd] = '\0';
  //TraceLog("aaa.log","thiswa->sSqlBuffer %s\n",thiswa->sSqlBuffer);
      if ( thiswa->hStmtBuffer )
      {

         res= OCI_StatementFree( thiswa->hStmtBuffer);
         if ( ! res  )
         {

            return (HB_FAILURE);
         }

//         thiswa->hStmtBuffer = NULL;
      }

//       res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmtBuffer) );
      thiswa->hStmtBuffer= OCI_StatementCreate(GetConnection(thiswa->hDbc));
      if (thiswa->hStmtBuffer ==NULL )
      {

         return (HB_FAILURE);
      }

//       res = SQLPrepare( thiswa->hStmtBuffer, (SQLCHAR *) (thiswa->sSqlBuffer), SQL_NTS );
      if (!OCI_Prepare(thiswa->hStmtBuffer, (char *) (thiswa->sSqlBuffer) ) )
      {
         return (HB_FAILURE);
      }

      for ( i = 0; i < pageReadSize; i++ )
      {
//          res = SQLBindParameter( thiswa->hStmtBuffer, i+1, SQL_PARAM_INPUT, SQL_C_ULONG, SQL_INTEGER, 15, 0, &(thiswa->lRecordToRetrieve[i]), 0, NULL );
         char szBind[10]={0};
         sprintf(szBind,":%i",i+1);
//          ////TraceLog("aaa.log" , "bindando registro %lu\n",thiswa->lRecordToRetrieve[i]);
         res  = OCI_BindUnsignedBigInt(thiswa->hStmtBuffer, szBind, &thiswa->lRecordToRetrieve[i]);
         if ( !res )
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
//       ////TraceLog("aaa.log","thiswa->lRecordToRetrieve[i] %lu thiswa->recordList[thiswa->recordListPos + i] %lu , thiswa->recordListPos + i %lu thiswa->recordListSize %lu \n",thiswa->lRecordToRetrieve[i],thiswa->recordList[thiswa->recordListPos + i],thiswa->recordListPos + i,thiswa->recordListSize);
   }

//    res = sqlExOraecute( thiswa->hStmtBuffer );
   res = OCI_Execute(thiswa->hStmtBuffer );
   //TraceLog("aaa.log","comando %s\n",OCI_GetSql( thiswa->hStmtBuffer));

   if (!res )
   {
      OraErrorDiagRTE( thiswa->hStmtBuffer, "updateRecordBuffer", thiswa->sSqlBuffer, res, __LINE__, __FILE__ );
      OCI_StatementFree( thiswa->hStmtBuffer );
      // culik null the handle
      //thiswa->hStmtBuffer=NULL;

      return (HB_FAILURE);
   }

  // bBuffer = hb_xgrabDebug( __LINE__,__FILE__, COLUMN_BLOCK_SIZE + 1 );
     rs = OCI_GetResultset( thiswa->hStmtBuffer );
     if ( rs == NULL )
     {
//          break;
         return (HB_FAILURE);
     }

   for( iRow = 1; iRow <= pageReadSize; iRow++ )
   {
//       res = SQLFetch( thiswa->hStmtBuffer );

//       bBuffer =(char*) hb_xgrabDebug( __LINE__,__FILE__, COLUMN_BLOCK_SIZE + 1 );
//       memset( bBuffer, 0, COLUMN_BLOCK_SIZE ) ;


      if (!OCI_FetchNext(rs))
      {
         break;
      }


      // Get the RECNO from 1st column in result set



      lCurrRecord= OCI_GetUnsignedBigInt( rs,1 ) ;
//       ////TraceLog("aaa.log", "updateRecordBuffer registro %i pego %lu\n",1,lCurrRecord);


      hb_itemPutNInt( pKey, lCurrRecord );     // To be used as HASH key in Pool Buffer

      iIndex     = 1;      // Recno is the 1st so we have 1 position offset

      if( thiswa->sqlarea.ulhDeleted > 0 )
      {
         if( ( (thiswa->recordList[thiswa->recordListPos])) == lCurrRecord )
         {

            char szValue[2];
            unsigned int uiLen;
            if ( OCI_GetString(rs,2 ) == NULL )
            {
              OCI_StatementFree( thiswa->hStmtBuffer );
            }
            else
            {
              uiLen = OCI_GetDataLength(rs,2);
               hb_xmemcpy( szValue,(char*)OCI_GetString(rs,2 ),uiLen ) ;
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
      hb_arrayNew( aRecord, hb_arrayLen( thiswa->sqlarea.aBuffer ) );

      for( i=1; i <= thiswa->sqlarea.area.uiFieldCount; i++ )
      {

         temp = hb_itemNew( NULL);

         if( (thiswa->sqlarea.uiFieldList[i-1] == 0) && thiswa->sqlarea.iFieldListStatus != FIELD_LIST_LEARNING )
         {
            hb_arraySetForward( aRecord, i, temp );     // Field is temporaly NIL since it's have never
                                                        // been needed in current WA. Will be filled on demand
         }
         else
         {

            SQLO_FieldGet( hb_arrayGetItemPtr( thiswa->aFields, thiswa->sqlarea.uiBufferIndex[i-1]  ), temp, ++iIndex           , 0, thiswa->nSystemID, bTranslate , rs);
            hb_arraySetForward( aRecord, i, temp );

         }
      }

      // Add new array to Buffer Pool
#ifdef __XHARBOUR__
//                if( s_pSym_TODATA  == NULL )
//                {
//                   hb_dynsymLock();
//                   s_pSym_TODATA = hb_dynsymFindName( "TODATA" );
//                   hb_dynsymUnlock();
//                   if ( s_pSym_TODATA  == NULL ) printf( "Could not find Symbol SR_DESERIALIZE\n" );
//                }
//                 hb_vmPushDynSym( s_pSym_TODATA );
//                 hb_vmPushNil();
//                 hb_vmPush( aRecord );
//                 hb_vmDo( 1 );
//                 hb_vmPushDynSym( s_pSym_TODATA );
//                 hb_vmPushNil();
//                 hb_vmPush( pKey);
//                 hb_vmDo( 1 );
//                 hb_vmPushDynSym( s_pSym_TODATA );
//                 hb_vmPushNil();
//                 hb_vmPush( thiswa->hBufferPool);
//                 hb_vmDo( 1 );

      hb_hashAdd( thiswa->hBufferPool, ULONG_MAX, pKey, aRecord );
#else
      hb_hashAdd( thiswa->hBufferPool, pKey, aRecord );
#endif

      // Feeds current record when it is found
      if ( ( (thiswa->recordList[thiswa->recordListPos])) == lCurrRecord )
      {
         hb_arrayCopy( aRecord, thiswa->sqlarea.aBuffer, NULL, NULL, NULL );
      }
      hb_itemRelease( aRecord );
//       hb_xfree( ( char* ) bBuffer );
   }

   //hb_xfree( (char*) bBuffer );
   hb_itemRelease( (PHB_ITEM)pKey );

   OCI_StatementFree( thiswa->hStmtBuffer);
   thiswa->hStmtBuffer =NULL;

//    if( res == SQL_NO_DATA_FOUND && iRow == 1 )
//    if ( rs->fetch_status == OCI_NO_DATA && iRow == 1 )
   if ( iRow == 1 )
   {
      return HB_FAILURE;    // Could not get at least one line from database
   }
   else
   {
      return  HB_SUCCESS ;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE trySkippingOnCache( SQLEXORAAREAP thiswa, LONG lToSkip )
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
               commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
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
               thiswa->sqlarea.area.fBof   = TRUE;
               return HB_SUCCESS;
            }

            if( lSupposedPos >= 0 && thiswa->recordList[thiswa->recordListPos] == thiswa->lEofAt )
            {
               sqlGetCleanBufferOra( thiswa );
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
               commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
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
               sqlGetCleanBufferOra( thiswa );
               return HB_SUCCESS;
            }

            if( lSupposedPos >= 0 && thiswa->recordList[thiswa->recordListPos] == thiswa->lBofAt )
            {
               thiswa->sqlarea.area.fBof   = TRUE;
               return HB_SUCCESS;
            }
         }
      }
   }
   return HB_FAILURE;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE prepareRecordListQueryOra( SQLEXORAAREAP thiswa )
{
   INDEXBINDORAP IndexBind;

   unsigned int  res;

   IndexBind = thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ];
   // culik not needed, we we are in the offset
   IndexBind += (thiswa->indexLevel - 1);    // Place Offset

//    res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &hPrep );
//    hPrep=OCI_StatementCreate( GetConnection(thiswa->hDbc) );
   if ( thiswa->recordListDirection == LIST_FORWARD )
   {
      IndexBind->SkipFwdStmt = OCI_StatementCreate( GetConnection(thiswa->hDbc) );
      OCI_AllowRebinding(IndexBind->SkipFwdStmt,1);
   }
   else
   {
      IndexBind->SkipBwdStmt = OCI_StatementCreate( GetConnection(thiswa->hDbc) );
      OCI_AllowRebinding(IndexBind->SkipBwdStmt,1);
   }






//    if ( CHECK_SQL_N_OK( res ) )
//    {
//       return (HB_FAILURE);
   //}

// if ( CHECK_SQL_N_OK( SQLPrepare( hPrep, (char *) (thiswa->sSql), SQL_NTS ) ) )

//    res = OCI_Prepare( hPrep, (char *) thiswa->sSql );
    if ( thiswa->recordListDirection == LIST_FORWARD )
      res = OCI_Prepare(IndexBind->SkipFwdStmt , (char *) thiswa->sSql );
    else
       res = OCI_Prepare(IndexBind->SkipBwdStmt , (char *) thiswa->sSql );

   if (!res)
   {
      return (HB_FAILURE);
   }
////TraceLog("aaa.log","skips %s IndexBind->SkipFwdStmt %p IndexBind->SkipBwdStmt   %p\n",thiswa->sSql,IndexBind->SkipFwdStmt , IndexBind->SkipBwdStmt );
   if ( thiswa->recordListDirection == LIST_FORWARD )
   {
//       IndexBind->SkipFwdStmt = hPrep;

      memset( &IndexBind->SkipFwdSql,0,PREPARED_SQL_LEN ) ;
      hb_xmemcpy( IndexBind->SkipFwdSql, thiswa->sSql, PREPARED_SQL_LEN -1 );
      IndexBind->SkipFwdSql[PREPARED_SQL_LEN-1] = '\0';
   }
   else
   {
//       IndexBind->SkipBwdStmt = hPrep;
      memset( &IndexBind->SkipBwdSql,0,PREPARED_SQL_LEN ) ;
      hb_xmemcpy( IndexBind->SkipBwdSql, thiswa->sSql, PREPARED_SQL_LEN -1 );
      IndexBind->SkipBwdSql[PREPARED_SQL_LEN-1] = '\0';
   }
   return (HB_SUCCESS);
}

/*------------------------------------------------------------------------*/

static BOOL CreateSkipStmtOra( SQLEXORAAREAP thiswa )
{
   PHB_ITEM pColumns, pIndexRef;
   INDEXBINDORAP IndexBind;
   int i;

   // Note about this IF: I assume that if query is prepared for level 1 (without changing IndexBind offset),
   // all queries are prepaered, since it loops to all levels when doing it, as code below. That's why it
   // checks for thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ]))->index???Stmt only.

   if( thiswa->bOrderChanged || thiswa->bConditionChanged1 || ( !(thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ]) ) || ( thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ] &&
      ( ( thiswa->recordListDirection == LIST_FORWARD  && (! ((INDEXBINDORAP) (thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ]))->SkipFwdStmt ) ) ||
        ( thiswa->recordListDirection == LIST_BACKWARD && (! ((INDEXBINDORAP) (thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ]))->SkipBwdStmt ) ) ) ) )    // Filter or controlling order has changed, or stmt is not prepared
   {
      thiswa->lBofAt     = 0;
      thiswa->lEofAt     = 0;
      thiswa->bOrderChanged = FALSE;

      if ( thiswa->sqlarea.hOrdCurrent > 0 )
      {
         pIndexRef = hb_arrayGetItemPtr( thiswa->sqlarea.aOrders, ( ULONG ) thiswa->sqlarea.hOrdCurrent );
         pColumns = hb_arrayGetItemPtr( pIndexRef, INDEX_FIELDS );
         thiswa->indexColumns = hb_arrayLen( pColumns );
      }
      else
      {
         thiswa->indexColumns = 1;     // Natural order, RECNO
      }

      // Alloc memory for binding structures, if first time

      if ( ! thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ] )
      {
         SetIndexBindStructureOra( thiswa );
      }

      // Now we should bind all index columns to be used by SKIP

       IndexBind = thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ];

      // Free the statements we are about to recreate

//       if ( thiswa->hStmtSkip )
//       {
//          OCI_StatementFree( thiswa->hStmtSkip ) ;
//          thiswa->hStmtSkip = NULL;
//       }


      for ( i=1; i <= thiswa->indexColumns; i++ )
      {
         if ( IndexBind->SkipFwdStmt )
         {
            OCI_StatementFree( IndexBind->SkipFwdStmt);
            IndexBind->SkipFwdStmt = NULL;
      }

         if ( IndexBind->SkipBwdStmt )
         {
            OCI_StatementFree( IndexBind->SkipBwdStmt);
            IndexBind->SkipBwdStmt = NULL;
         }
         IndexBind++;
      }

      getOrderByExpressionOra( thiswa, FALSE );
      setResultSetLimitOra( thiswa, RECORD_LIST_SIZE );

      thiswa->indexLevel          = thiswa->indexColumns;

      // Create and prepare queries to scroll to each index column level

      for ( i=1; i <= thiswa->indexColumns; i++ )
      {
         getWhereExpressionOra( thiswa, thiswa->recordListDirection == LIST_FORWARD ? LIST_SKIP_FWD : LIST_SKIP_BWD );
         createRecodListQueryOra( thiswa );
         prepareRecordListQueryOra( thiswa );
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

static HB_ERRCODE sqlExOraBof( SQLEXORAAREAP thiswa, BOOL * bof )
{
   if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area);
   }
   *bof = thiswa->sqlarea.area.fBof;
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraEof( SQLEXORAAREAP thiswa, BOOL * eof )
{

   if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }

   if ( thiswa->bIsInsert && thiswa->bufferHot )
   {
      *eof = FALSE;
   }
   else
   {
      *eof = thiswa->sqlarea.area.fEof;
   }
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraFound( SQLEXORAAREAP thiswa, BOOL * found )
{
   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }
   *found = thiswa->sqlarea.area.fFound;
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraGoBottom( SQLEXORAAREAP thiswa )
{
   thiswa->sqlarea.lpdbPendingRel = NULL;
   thiswa->sqlarea.firstinteract = 0;
   thiswa->sqlarea.wasdel = 0;
   thiswa->sqlarea.area.fFound = FALSE;

   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( thiswa->lEofAt )
   {
      SELF_GOTO( &thiswa->sqlarea.area, (LONG) thiswa->lEofAt );
      if ( thiswa->bReverseIndex  !=  bOldReverseIndex)
      {
         thiswa->recordListDirection = LIST_BACKWARD;
         getOrderByExpressionOra( thiswa, FALSE );
         getWhereExpressionOra( thiswa, LIST_FROM_BOTTOM );
         setResultSetLimitOra( thiswa, RECORD_LIST_SIZE / 10 );
         createRecodListQueryOra( thiswa );
//          ////TraceLog("aaa.log", "chamando getRecord list de  sqlExOraGoBottom\n");
         if ( getRecordList( thiswa, RECORD_LIST_SIZE / 10 ) == HB_FAILURE )
         {
            OraErrorDiagRTE( thiswa->hStmt, "dbGoBottom", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
            commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
            return (HB_FAILURE);
         }
//          ////TraceLog("aaa.log", "chamei getRecord list de  sqlExOraGoBottom\n");
      }

   }
   else
   {
      thiswa->recordListDirection = LIST_BACKWARD;

      getOrderByExpressionOra( thiswa, FALSE );
      getWhereExpressionOra( thiswa, LIST_FROM_BOTTOM );
      setResultSetLimitOra( thiswa, RECORD_LIST_SIZE / 10 );
      createRecodListQueryOra( thiswa );
//      ////TraceLog("aaa.log", "chamando getRecord list de  sqlExOraGoBottom\n");
      if ( getRecordList( thiswa, RECORD_LIST_SIZE / 10 ) == HB_FAILURE )
      {
         OraErrorDiagRTE( thiswa->hStmt, "dbGoBottom", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
         commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
         return (HB_FAILURE);
      }
//       ////TraceLog("aaa.log", "chamei getRecord list de  sqlExOraGoBottom\n");
   }

   thiswa->sqlarea.area.fTop    = FALSE;
   thiswa->sqlarea.area.fBottom = TRUE;
   thiswa->skipDirection  = -1;

   if ( thiswa->recordListSize == 0 )
   {
      thiswa->sqlarea.area.fEof = TRUE;
      thiswa->sqlarea.area.fBof = TRUE;
      sqlGetCleanBufferOra( thiswa );
   }
   else
   {
      thiswa->sqlarea.area.fEof   = FALSE;
      thiswa->sqlarea.area.fBof   = FALSE;
      thiswa->lEofAt = thiswa->recordList[thiswa->recordListPos];
      if ( updateRecordBuffer( thiswa, FALSE ) == HB_FAILURE )
      {
         commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
         return (HB_FAILURE);
      }
   }

   SELF_SKIPFILTER(&thiswa->sqlarea.area, -1 );

   if( thiswa->sqlarea.area.lpdbRelations )
   {
      return SELF_SYNCCHILDREN(&thiswa->sqlarea.area );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraGoTo( SQLEXORAAREAP thiswa, LONG recno )
{
   int i;

   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   /* Reset parent rel struct */
   thiswa->sqlarea.lpdbPendingRel = NULL;
   thiswa->sqlarea.firstinteract = 0;
   thiswa->sqlarea.wasdel = 0;
   thiswa->sqlarea.area.fFound = FALSE;

   if( recno == 0 )
   {
      // Move to phantom
      sqlGetCleanBufferOra( thiswa );
      thiswa->sqlarea.area.fBof = TRUE;
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
            thiswa->sqlarea.area.fEof = FALSE;
            thiswa->sqlarea.area.fBof = FALSE;
            return HB_SUCCESS;
         }
      }
   }

   // 2 - Get it from database

   thiswa->recordList[0]  = (HB_ULONG) recno;
   thiswa->recordListSize = 1;
   thiswa->recordListDirection = LIST_FORWARD;
   thiswa->recordListPos = 0;

   if ( updateRecordBuffer( thiswa, TRUE ) == HB_SUCCESS )
   {
      thiswa->sqlarea.area.fEof = FALSE;
      thiswa->sqlarea.area.fBof = FALSE;
      return HB_SUCCESS;
   }

   // 3 - Move to phantom
   sqlGetCleanBufferOra( thiswa );
   thiswa->sqlarea.area.fBof = TRUE;
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraGoToId( SQLEXORAAREAP thiswa, PHB_ITEM pItem )
{
   thiswa->sqlarea.firstinteract = 0;
   thiswa->sqlarea.wasdel = 0;

   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( HB_IS_NUMERIC( pItem ) )
   {
      return SELF_GOTO( &thiswa->sqlarea.area, (LONG) hb_itemGetNL( pItem ) );
   }
   else
   {
      commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
      return (HB_FAILURE);
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraGoTop( SQLEXORAAREAP thiswa )
{
   thiswa->sqlarea.lpdbPendingRel = NULL;
   thiswa->sqlarea.firstinteract = 0;
   thiswa->sqlarea.wasdel = 0;
   thiswa->sqlarea.area.fFound = FALSE;

   if( getWorkareaParamsOra( thiswa ) == HB_FAILURE )     // If workarea was opened by dbCreate()
   {
      return HB_FAILURE;
   }

   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( thiswa->lBofAt )
   {
      SELF_GOTO( &thiswa->sqlarea.area, (LONG) thiswa->lBofAt );
      if ( thiswa->bReverseIndex  !=  bOldReverseIndex)
      {
        thiswa->recordListDirection = LIST_FORWARD;
         getOrderByExpressionOra( thiswa, FALSE );
         getWhereExpressionOra( thiswa, LIST_FROM_TOP );
         setResultSetLimitOra( thiswa, RECORD_LIST_SIZE / 10 );
         createRecodListQueryOra( thiswa );
//          ////TraceLog("aaa.log", "chamando getRecord list de  sqlExOraGoTop\n");
         if ( getRecordList( thiswa, RECORD_LIST_SIZE / 10 ) == HB_FAILURE )
         {
            OraErrorDiagRTE( thiswa->hStmt, "dbGoTop", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
            commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
            return (HB_FAILURE);
         }
//          ////TraceLog("aaa.log", "chamei getRecord list de  sqlExOraGoTop\n");
      }

   }
   else
   {
      thiswa->recordListDirection = LIST_FORWARD;
      getOrderByExpressionOra( thiswa, FALSE );
      getWhereExpressionOra( thiswa, LIST_FROM_TOP );
      setResultSetLimitOra( thiswa, RECORD_LIST_SIZE / 10 );
      createRecodListQueryOra( thiswa );
//     ////TraceLog("aaa.log", "chamando getRecord list de  sqlExOraGoTop\n");
      if ( getRecordList( thiswa, RECORD_LIST_SIZE / 10 ) == HB_FAILURE )
      {
         OraErrorDiagRTE( thiswa->hStmt, "dbGoTop", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
         commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
         return (HB_FAILURE);
      }
//       ////TraceLog("aaa.log", "chamei getRecord list de  sqlExOraGoTop\n");
   }

   thiswa->sqlarea.area.fTop    = TRUE;
   thiswa->sqlarea.area.fBottom = FALSE;
   thiswa->skipDirection  = 1;

   if ( thiswa->recordListSize == 0 )
   {
      thiswa->sqlarea.area.fEof = TRUE;
      thiswa->sqlarea.area.fBof = TRUE;
      sqlGetCleanBufferOra( thiswa );
   }
   else
   {
      thiswa->sqlarea.area.fEof   = FALSE;
      thiswa->sqlarea.area.fBof   = FALSE;
      thiswa->lBofAt = thiswa->recordList[thiswa->recordListPos];
      if ( updateRecordBuffer( thiswa, FALSE ) == HB_FAILURE )
      {
         commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
         return (HB_FAILURE);
      }
   }

   SELF_SKIPFILTER(&thiswa->sqlarea.area, 1 );

   if( thiswa->sqlarea.area.lpdbRelations )
   {
      return SELF_SYNCCHILDREN(&thiswa->sqlarea.area );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraSeek( SQLEXORAAREAP thiswa, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast )
{
   int queryLevel;
   USHORT iIndex, i;
   HB_ERRCODE  retvalue = HB_SUCCESS;
   PHB_ITEM pNewKey = NULL;
   OCI_Statement *hStmt;
   OCI_Resultset * rs;

   thiswa->sqlarea.lpdbPendingRel = NULL;
   thiswa->sqlarea.firstinteract = 0;
   thiswa->sqlarea.wasdel = 0;
   thiswa->sqlarea.area.fTop   = thiswa->sqlarea.area.fBottom = FALSE;
   thiswa->sqlarea.area.fEof   = FALSE;
   thiswa->sqlarea.area.fFound = FALSE;

   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if ( thiswa->sqlarea.hOrdCurrent == 0 )
   {
      commonError( &thiswa->sqlarea.area, EG_NOORDER, EDBF_NOTINDEXED, thiswa->sTable );
      return HB_FAILURE;
   }

#ifndef HB_CDP_SUPPORT_OFF
   if( HB_IS_STRING( pKey ) )
   {
      PHB_CODEPAGE cdpSrc = thiswa->sqlarea.cdPageCnv ? thiswa->sqlarea.cdPageCnv : hb_vmCDP();
      if( thiswa->sqlarea.area.cdPage && thiswa->sqlarea.area.cdPage != cdpSrc )
      {
         HB_SIZE nLen = hb_itemGetCLen( pKey );
         char * pszVal = hb_cdpnDup( hb_itemGetCPtr( pKey ), &nLen,
                                     cdpSrc, thiswa->sqlarea.area.cdPage );
         pKey = pNewKey = hb_itemPutCLPtr( NULL, pszVal, nLen );
      }
   }
#endif

   if( ! thiswa->CurrRecord )
   {
      SetCurrRecordStructureOra( thiswa );
   }

   // Start search code here

   thiswa->recordListDirection = ( bFindLast ? LIST_BACKWARD : LIST_FORWARD );

   // Set binding structures and push pKey to it
   if ( ! thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ] )
   {
      SetIndexBindStructureOra( thiswa );
   }

   if( FeedSeekKeyToBindingsOra( thiswa, pKey, &queryLevel ) != HB_SUCCESS )
   {
      if( pNewKey )
         hb_itemRelease( pNewKey );
      return HB_FAILURE;
   }

   if( CreateSeekStmtora( thiswa, queryLevel ) )      // Create and prepare new SEEK statement, if needed
   {
      BindSeekStmtora( thiswa, queryLevel );          // Bind parameters to IndexBind structure

   }
   else
   {
      FeedSeekStmtOra( thiswa ,queryLevel);          // Bind parameters to IndexBind structure
   }
   thiswa->bConditionChanged2 = FALSE;

   if ( getPreparedSeekora( thiswa, queryLevel, &iIndex, &hStmt,&rs ) == HB_SUCCESS )     // Fetch line from database, read RECNO and DELETED
   {
      // Create a line array to hold the record
      PHB_ITEM temp;
      BOOL bTranslate;
      int iComp;
      //HB_ITEM temp;

      PHB_ITEM aRecord = hb_itemNew( NULL );


      hb_arrayNew( aRecord, hb_arrayLen( thiswa->sqlarea.aBuffer ) );


      bTranslate = FALSE;

      for( i=1; i <= thiswa->sqlarea.area.uiFieldCount; i++ )
      {
//         PHB_ITEM pF = hb_arrayGetItemPtr( thiswa->aFields, thiswa->sqlarea.uiBufferIndex[i-1]  );
         temp=hb_itemNew(NULL) ;
         //temp.type = HB_IT_NIL;        // I know this is not a good practice, but we save tons of allocs.
                                       // please keep as is. ML.

//         if( (thiswa->sqlarea.uiFieldList[i-1] == 0) && thiswa->sqlarea.iFieldListStatus != FIELD_LIST_LEARNING )
//         {
//            hb_arraySetForward( aRecord, i, temp );     // Field is temporaly NIL since it's have never
//                                                         // been needed in current WA. Will be filled on demand
//         }
//         else
//         {
//             PHB_ITEM pF = hb_arrayGetItemPtr( thiswa->aFields, thiswa->sqlarea.uiBufferIndex[i-1]  );

//                if( s_pSym_TODATA  == NULL )
//                {
//                   hb_dynsymLock();
//                   s_pSym_TODATA = hb_dynsymFindName( "TODATA" );
//                   hb_dynsymUnlock();
//                   if ( s_pSym_TODATA  == NULL ) printf( "Could not find Symbol SR_DESERIALIZE\n" );
//                }
//                hb_vmPushDynSym( s_pSym_TODATA );
//                hb_vmPushNil();
//                hb_vmPush( thiswa->aFields );
//                hb_vmDo( 1 );

//              TraceLog("ccc.log", "Valor stringzado %s pos i %i index %lu campo %s \n",OCI_GetString(rs,iIndex+1),i,iIndex+1,hb_arrayGetC(pF,1));
             SQLO_FieldGet( hb_arrayGetItemPtr( thiswa->aFields, thiswa->sqlarea.uiBufferIndex[i-1]  ), temp, ++iIndex           , 0, thiswa->nSystemID, bTranslate , rs);
             hb_arraySetForward( aRecord, i, temp );

  //       }
//                if( s_pSym_TODATA  == NULL )
//                {
//                   hb_dynsymLock();
//                   s_pSym_TODATA = hb_dynsymFindName( "TODATA" );
//                   hb_dynsymUnlock();
//                   if ( s_pSym_TODATA  == NULL ) printf( "Could not find Symbol SR_DESERIALIZE\n" );
//                }
//                 hb_vmPushDynSym( s_pSym_TODATA );
//                 hb_vmPushNil();
//                 hb_vmPush( aRecord );
//                 hb_vmDo( 1 );


      }
//       hb_xfree( ( char*) bBuffer );

      hb_arrayCopy( aRecord, thiswa->sqlarea.aBuffer, NULL, NULL, NULL );
      hb_itemRelease( aRecord );
      //hb_xfree( (char*) bBuffer );
//       OCI_StatementFree( hStmt);
//       hStmt=NULL;

      // End search code

      iComp = sqlKeyCompareEx( thiswa, pKey, FALSE );

      if ( iComp != 0 )
      {
         thiswa->sqlarea.area.fFound = TRUE;
         thiswa->sqlarea.area.fBof   = FALSE;
         thiswa->sqlarea.area.fEof   = FALSE;
      }
      else
      {
         thiswa->sqlarea.area.fFound = FALSE;
         if( !bSoftSeek )
         {
            sqlGetCleanBufferOra( thiswa );
         }
      }

      if (( hb_setGetDeleted() || thiswa->sqlarea.area.dbfi.itmCobExpr != NULL ) && !thiswa->sqlarea.area.fEof )
      {
         retvalue = SELF_SKIPFILTER(&thiswa->sqlarea.area, ( bFindLast ? -1 : 1 ) );

         if ( thiswa->sqlarea.area.fEof )
         {
            thiswa->sqlarea.area.fFound = FALSE;
         }
         else
         {
            if ( sqlKeyCompareEx(  thiswa, pKey, FALSE ) != 0 )
            {
               thiswa->sqlarea.area.fFound = TRUE;
            }
            else
            {
               thiswa->sqlarea.area.fFound = FALSE;

               if( !bSoftSeek )
               {
                  sqlGetCleanBufferOra( thiswa );
               }
            }
         }
      }
   }
   else
   {
      sqlGetCleanBufferOra( thiswa );
      thiswa->sqlarea.area.fFound = FALSE;
   }

   if( pNewKey )
      hb_itemRelease( pNewKey );

   if( thiswa->sqlarea.area.lpdbRelations && retvalue == HB_SUCCESS )
   {
      return SELF_SYNCCHILDREN(&thiswa->sqlarea.area );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraSkip( SQLEXORAAREAP thiswa, LONG lToSkip )
{
   LONG lSkip;

   if( thiswa->sqlarea.lpdbPendingRel )
   {
      if( SELF_FORCEREL(&thiswa->sqlarea.area ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   else if ( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   /* Flush record and exit */
   if( lToSkip == 0 )
      return SELF_GOCOLD( &thiswa->sqlarea.area );

   // We need save lCurrentRecord previous to lost fEof flag. To to correct SKIPRAW
   thiswa->lCurrentRecord = GetCurrentRecordNumOra( thiswa );

   thiswa->sqlarea.area.fTop = thiswa->sqlarea.area.fBottom = FALSE;
   thiswa->sqlarea.wasdel = 0;
   thiswa->sqlarea.area.fBof = thiswa->sqlarea.area.fEof = FALSE;

   if( lToSkip > 0 )
      lSkip = 1;
   else
   {
      lSkip = -1;
      lToSkip *= -1;
   }
   while( --lToSkip >= 0 )
   {
      if( SELF_SKIPRAW( &thiswa->sqlarea.area, lSkip ) != HB_SUCCESS )
         return HB_FAILURE;
      if( SELF_SKIPFILTER( &thiswa->sqlarea.area, lSkip ) != HB_SUCCESS )
         return HB_FAILURE;
      if( thiswa->sqlarea.area.fBof || thiswa->sqlarea.area.fEof )
         break;
   }

   /* Update Bof and Eof flags */
   if( lSkip < 0 )
      thiswa->sqlarea.area.fEof = FALSE;
   else /* ( lSkip > 0 ) */
      thiswa->sqlarea.area.fBof = FALSE;

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraSkipFilter( SQLEXORAAREAP thiswa, LONG lUpDown )
{
   // This was copied from workarea.c since SUPER_ method
   // does not fir in this RDD needs.
   BOOL fBottom, fDeleted;
   HB_ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("hb_waSkipFilter(%p, %ld)", thiswa, lUpDown));

   if( !hb_setGetDeleted() && thiswa->sqlarea.area.dbfi.itmCobExpr == NULL )
      return HB_SUCCESS;

   /* Since lToSkip is passed to SkipRaw, it should never request more than  a single skip.
             The implied purpose of hb_waSkipFilter is to get off of a "bad" record
             after a skip was performed, NOT to skip lToSkip filtered records.
         */
   lUpDown = ( lUpDown < 0  ? -1 : 1 );

   /* remember if we are here after SLEF_GOTOP() */
   fBottom = thiswa->sqlarea.area.fBottom;

   while( !thiswa->sqlarea.area.fBof && !thiswa->sqlarea.area.fEof )
   {
      /* SET DELETED */
      if( hb_setGetDeleted() )
      {
         if( SELF_DELETED( &thiswa->sqlarea.area, &fDeleted ) != HB_SUCCESS )
            return HB_FAILURE;
         if( fDeleted )
         {
            if( SELF_SKIPRAW( &thiswa->sqlarea.area, lUpDown ) != HB_SUCCESS )
               return HB_FAILURE;
            continue;
         }
      }

      /* SET FILTER TO */
      if( thiswa->sqlarea.area.dbfi.itmCobExpr )
      {
         if( SELF_EVALBLOCK( &thiswa->sqlarea.area, thiswa->sqlarea.area.dbfi.itmCobExpr ) != HB_SUCCESS )
            return HB_FAILURE;

         if( HB_IS_LOGICAL( thiswa->sqlarea.area.valResult ) &&
             !hb_itemGetL( thiswa->sqlarea.area.valResult ) )
         {
            if( SELF_SKIPRAW( &thiswa->sqlarea.area, lUpDown ) != HB_SUCCESS )
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

   if( thiswa->sqlarea.area.fBof && lUpDown < 0 )
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
         uiError = SELF_GOTO( &thiswa->sqlarea.area, 0 );
      }
      else
      {
         uiError = SELF_GOTOP( &thiswa->sqlarea.area );
         thiswa->sqlarea.area.fBof = TRUE;
      }
   }
   else
   {
      uiError = HB_SUCCESS;
   }

   return uiError;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraSkipRaw( SQLEXORAAREAP thiswa, LONG lToSkip )
{
   HB_ERRCODE res;

   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }
   /* if we are over phantom record we go bottom. */
   if( lToSkip < 0 && thiswa->lCurrentRecord == thiswa->lLastRec )
      return ( SELF_GOBOTTOM(&thiswa->sqlarea.area ) );

   if( ! thiswa->CurrRecord )
   {
      SetCurrRecordStructureOra( thiswa );
   }

   if ( lToSkip != 0 )
   {
      // Try to find needed record in record list cache
      thiswa->skipDirection = lToSkip > 0 ? 1 : -1;

      if( trySkippingOnCache( thiswa, lToSkip ) == HB_SUCCESS )
         return ( ConcludeSkipraw( thiswa ) );

      // Cache was unsuccessful, so get a new list from database

      if ( thiswa->sqlarea.hOrdCurrent > 0 )
      {
         thiswa->indexColumns = hb_arrayLen( hb_arrayGetItemPtr( hb_arrayGetItemPtr( thiswa->sqlarea.aOrders, ( ULONG ) thiswa->sqlarea.hOrdCurrent ), INDEX_FIELDS ) );
      }
      else
      {
         thiswa->indexColumns = 1;     // Natural order, RECNO
      }

      thiswa->recordListDirection = ( lToSkip > 0 ? LIST_FORWARD : LIST_BACKWARD );

      // Set binding structures and SQL stmts for
      // SKIP and SEEK over current index order

      if (!CreateSkipStmtOra( thiswa ))
      {
         // If queries were not re-createds and re-prepared, we should
         // feed bind structures with current record information (CreateSkipStmtOra
         // does it in getWhereExpressionOra() if queries were re-prepared

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
            commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
            return (HB_FAILURE);
         }
         else if( res == HB_RETRY )
         {
            if( lToSkip > 0 )
            {
               sqlGetCleanBufferOra( thiswa );
               break;
            }
            else
            {
               SELF_GOTOP(&thiswa->sqlarea.area );
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
            commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_READ, thiswa->sTable );
            return (HB_FAILURE);
         }
         return ( ConcludeSkipraw( thiswa ) );
      }
      else
      {
         if( lToSkip < 0 )
         {
            thiswa->sqlarea.area.fBof   = TRUE;
            if( thiswa->recordListSize )
               thiswa->lBofAt = thiswa->recordList[thiswa->recordListPos];
         }
         else
         {
            sqlGetCleanBufferOra( thiswa );
         }
      }
   }
   return ( ConcludeSkipraw( thiswa ) );
}

/*------------------------------------------------------------------------*/

#define sqlExOraAddField                  NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraAppend( SQLEXORAAREAP thiswa )
{
   /* Reset parent rel struct */
   thiswa->sqlarea.lpdbPendingRel = NULL;
   thiswa->sqlarea.firstinteract = 0;
   thiswa->sqlarea.wasdel = 0;

   hb_arraySize(thiswa->sqlarea.aLocked, 0);

   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return( HB_FAILURE );
   }

   thiswa->bufferHot = TRUE;
   thiswa->bIsInsert = TRUE;

   sqlGetCleanBufferOra( thiswa );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExOraCreateFields           NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraDeleteRec( SQLEXORAAREAP thiswa )
{
   BOOL isDeleted;
   unsigned int  res;

   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }

   SELF_DELETED(&thiswa->sqlarea.area, &isDeleted );

   if( (!isDeleted) && (!thiswa->sqlarea.area.fEof) )
   {
     if (  thiswa->sSql  )
     memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
      if( thiswa->sqlarea.ulhDeleted > 0 && sr_UseDeleteds() )
      {
         sprintf( thiswa->sSql, "UPDATE %s SET %s = '%c'%s WHERE %s = %i",
                                thiswa->sTable, thiswa->sDeletedName, thiswa->iTCCompat >= 2 ? '*' : 'T',
                                thiswa->iTCCompat >= 4 ? ", R_E_C_D_E_L_ = R_E_C_N_O_" : " ",
                                thiswa->sRecnoName, (int) GetCurrentRecordNumOra( thiswa ) );

      }
      else
      {
         sprintf( thiswa->sSql, "DELETE FROM %s WHERE %s = %i",
                                thiswa->sTable, thiswa->sRecnoName,
                                (int) GetCurrentRecordNumOra( thiswa ) );
      }

//       res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmt) );
      thiswa->hStmt=OCI_StatementCreate( GetConnection(thiswa->hDbc) );
      if ( thiswa->hStmt == NULL)
      {
         return (HB_FAILURE);
      }

      res= OCI_ExecuteStmt( thiswa->hStmt, thiswa->sSql);
      if ( !res )
      {
         return (HB_FAILURE);    // It means a fault in SQL statement
      }
       OCI_StatementFree( thiswa->hStmt);
   }

   thiswa->deletedList[thiswa->recordListPos] = (thiswa->iTCCompat ? '*' : 'T');

   if( thiswa->lEofAt == thiswa->recordList[thiswa->recordListPos] )
      thiswa->lEofAt = 0;

   if( thiswa->lBofAt == thiswa->recordList[thiswa->recordListPos] )
      thiswa->lBofAt = 0;

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraDeleted( SQLEXORAAREAP thiswa, BOOL * isDeleted )
{
   if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }

   if( thiswa->sqlarea.ulhDeleted == 0 || thiswa->bIsInsert || thiswa->sqlarea.area.fEof )
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

#define sqlExOraFieldCount             NULL
#define sqlExOraFieldDisplay           NULL
#define sqlExOraFieldInfo              NULL
#define sqlExOraFieldName              NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraFlush( SQLEXORAAREAP thiswa )
{
   return ( SELF_GOCOLD(&thiswa->sqlarea.area ) );
}

/*------------------------------------------------------------------------*/

#define sqlExOraGetRec                 NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraGetValue( SQLEXORAAREAP thiswa, USHORT fieldNum, PHB_ITEM value  )
{
   PHB_ITEM itemTemp, itemTemp3;
   HB_SIZE ulPos;

   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }
   else if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   itemTemp = hb_itemArrayGet( thiswa->sqlarea.aBuffer, thiswa->sqlarea.uiBufferIndex[fieldNum - 1] );

   if( HB_IS_NIL( itemTemp ) )
   {
      getMissingColumn( thiswa, hb_arrayGetItemPtr( thiswa->sqlarea.aBuffer, thiswa->sqlarea.uiBufferIndex[fieldNum - 1] ), (LONG) (thiswa->sqlarea.uiBufferIndex[fieldNum - 1]) );
      hb_itemRelease( itemTemp );
      itemTemp = hb_itemArrayGet( thiswa->sqlarea.aBuffer, thiswa->sqlarea.uiBufferIndex[fieldNum - 1] );
   }

   if( !thiswa->sqlarea.uiFieldList[fieldNum - 1] )
   {
      thiswa->sqlarea.uiFieldList[fieldNum - 1] = 1;
      thiswa->sqlarea.iFieldListStatus          = FIELD_LIST_NEW_VALUE_READ;
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
      LPFIELD pField = thiswa->sqlarea.area.lpFields + fieldNum - 1;

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
         char * empty = ( char * ) hb_xgrab(  nLen + 1 );

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
               PHB_CODEPAGE cdpDest = thiswa->sqlarea.cdPageCnv ? thiswa->sqlarea.cdPageCnv : hb_vmCDP();
               if( thiswa->sqlarea.area.cdPage && thiswa->sqlarea.area.cdPage != cdpDest )
               {
                  char * pszVal = hb_cdpnDup( empty, &nLen, thiswa->sqlarea.area.cdPage, cdpDest );
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
      /*
      if( HB_IS_NIL( itemTemp ) )
      {
         TraceLog( NULL, "Empty buffer found at position %i, fieldpos %i\n", (int)thiswa->sqlarea.uiBufferIndex[fieldNum - 1], (int) fieldNum );
      }
      */
      hb_itemForwardValue( value, itemTemp );
   }
   hb_itemRelease( itemTemp );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExOraGetVarLen              NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraGoCold( SQLEXORAAREAP thiswa )
{
   if ( thiswa->bufferHot )   // && (!(thiswa->sqlarea.ulhDeleted > 0 ? TRUE : thiswa->deletedList[thiswa->recordListPos] == ' ') )
   {
      if ( thiswa->bHistoric )
      {
         hb_arraySetL( thiswa->sqlarea.aInfo, AINFO_ISINSERT, thiswa->bIsInsert );
         hb_arraySetL( thiswa->sqlarea.aInfo, AINFO_HOT,      thiswa->bufferHot );
         if( !thiswa->bIsInsert )
         {
            hb_arraySetNLL( thiswa->sqlarea.aInfo, AINFO_RECNO, GetCurrentRecordNumOra( thiswa ) );
         }

         return SUPER_GOCOLD( &thiswa->sqlarea.area);     // Historic workareas are handled by xBase code
                                                      // in sqlrdd2.c as in SQLRDD
      }

      if( thiswa->bIsInsert )
      {

         if( !thiswa->hStmtInsert )    // Check if we have the INSERT statement prepared
         {
            CreateInsertStmtOra( thiswa );   // Create also column binding structures

            if( PrepareInsertStmtOra( thiswa ) == HB_FAILURE )
               return (HB_FAILURE);

            if( BindInsertColumnsOra( thiswa ) == HB_FAILURE )
               return (HB_FAILURE);
         }
         else
         {
            thiswa->sSql[0] = '\0';                    // To prevent erroneous error message
         }

         if( FeedRecordColsOra( thiswa, FALSE ) == HB_FAILURE )  // Stmt created and prepared, only need to push data
            return (HB_FAILURE);

            if( ExecuteInsertStmtOra( thiswa ) == HB_FAILURE )
               return (HB_FAILURE);
//             ReleaseInsertRecordStructureOra( thiswa, 0 );

      }
      else if( !thiswa->sqlarea.area.fEof )
      {
         if( (!thiswa->hStmtUpdate) || memcmp( (const void *) (thiswa->editMask),
                                               (const void *) (thiswa->updatedMask ), MAX_FIELDS ) != 0 )
         {
            if( CreateUpdateStmtOra( thiswa ) == HB_FAILURE )
               return (HB_FAILURE);
         }
         else
         {
            thiswa->sSql[0] = '\0';    // Avoid wrong error message in Execute
         }

         if( ExecuteUpdateStmtOra( thiswa ) == HB_FAILURE )
            return (HB_FAILURE);
      }
      thiswa->bufferHot = FALSE;
      thiswa->bIsInsert = FALSE;
   }

   memset( thiswa->editMask,    0, MAX_FIELDS );      // Clear edited mask

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExOraGoHot                  NULL
#define sqlExOraPutRec                 NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraPutValue( SQLEXORAAREAP thiswa, USHORT fieldNum, PHB_ITEM value )
{
   PHB_ITEM pDest;
   LPFIELD pField;
   char * cfield;
   double dNum;
   USHORT len, dec, fieldindex;

   // TraceLog( NULL, "sqlPutValue, writing column %i\n", fieldNum );

   if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }

   fieldindex = (USHORT)thiswa->sqlarea.uiBufferIndex[fieldNum - 1];
   thiswa->editMask[ fieldindex-1 ] = '1';
   pDest  = hb_itemArrayGet( thiswa->sqlarea.aBuffer, fieldindex );

   if( !thiswa->sqlarea.uiFieldList[fieldNum - 1] )
   {
      thiswa->sqlarea.uiFieldList[fieldNum - 1] = 1;
      thiswa->sqlarea.iFieldListStatus          = FIELD_LIST_NEW_VALUE_READ;
   }

   if( HB_IS_NIL( pDest ) )
   {
      getMissingColumn( thiswa, pDest, fieldindex );
   }

   if( !thiswa->sqlarea.uiFieldList[fieldNum - 1] )     // Columns to be included in SELECT statement further
   {
      hb_arraySetNL( thiswa->sqlarea.aSelectList, thiswa->sqlarea.uiBufferIndex[fieldNum - 1], 1 );
      thiswa->sqlarea.uiFieldList[fieldNum - 1] = 1;
   }

   pField = thiswa->sqlarea.area.lpFields + fieldNum - 1;

   /* test compatible datatypes */

   if( (HB_IS_NUMBER( pDest ) && HB_IS_NUMBER( value )) || (HB_IS_STRING( pDest ) && HB_IS_STRING( value )) ||
       (HB_IS_LOGICAL( pDest ) && HB_IS_LOGICAL( value )) || (HB_IS_DATE( pDest ) && HB_IS_DATE( value )) ||
       (HB_IS_DATETIME( pDest ) && HB_IS_DATETIME( value )) )
   {

      if( pField->uiType == HB_FT_STRING )
      {
         HB_SIZE nSize = hb_itemGetCLen( value ), nLen = pField->uiLen;

         cfield = (char *) hb_xgrabz( nLen + 1 );
#ifndef HB_CDP_SUPPORT_OFF
         hb_cdpnDup2( hb_itemGetCPtr( value ), nSize,
                      cfield, &nLen,
                      thiswa->sqlarea.cdPageCnv ? thiswa->sqlarea.cdPageCnv : hb_vmCDP(), thiswa->sqlarea.area.cdPage );
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

      hb_arraySet( thiswa->sqlarea.aBuffer, fieldindex, value );
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
      hb_arraySet( thiswa->sqlarea.aBuffer, fieldindex, value );
   }
   else
   {
#ifdef SQLRDD_NWG_SPECIFIC
      thiswa->bufferHot = TRUE;
      return ( HB_SUCCESS );
#else
      char type_err[128];
      sprintf( type_err, "data type origin: %i - data type target %i", hb_itemType( value ), hb_itemType( pDest ) );
      commonError( &thiswa->sqlarea.area, EG_DATATYPE, ESQLRDD_DATATYPE, type_err );
      return ( HB_FAILURE );
#endif
   }

   thiswa->bufferHot = TRUE;
   hb_itemRelease( pDest );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraRecall( SQLEXORAAREAP thiswa )
{
   BOOL isDeleted;
   unsigned int  res;

   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }
   else if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   SELF_DELETED(&thiswa->sqlarea.area, &isDeleted );

   if( isDeleted && thiswa->sqlarea.ulhDeleted > 0 && sr_UseDeleteds() )
   {
     memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
      sprintf( thiswa->sSql, "UPDATE %s SET %s = '%c'%s WHERE %s = %i",
                             thiswa->sTable, thiswa->sDeletedName, ' ',
                             thiswa->iTCCompat >= 4 ? ", R_E_C_D_E_L_ = R_E_C_N_O_" : " ",
                             thiswa->sRecnoName, (int) GetCurrentRecordNumOra( thiswa ) );

//       res = SQLAllocStmt( ( HDBC ) thiswa->hDbc, &(thiswa->hStmt) );
      thiswa->hStmt=OCI_StatementCreate( GetConnection(thiswa->hDbc) );
      if (thiswa->hStmt == NULL )
      {
         return (HB_FAILURE);
      }

      res= OCI_ExecuteStmt( thiswa->hStmt, thiswa->sSql);
      if ( !res )
      {
         return (HB_FAILURE);    // It means a fault in SQL statement
      }
       OCI_StatementFree( thiswa->hStmt);
   }

   thiswa->deletedList[thiswa->recordListPos] = ' ';
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraRecCount( SQLEXORAAREAP thiswa, ULONG * recCount )
{
   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }

   if ( thiswa->bIsInsert && thiswa->bufferHot )
   {
      *recCount = (ULONG) (hb_arrayGetNL( thiswa->sqlarea.aInfo, AINFO_RCOUNT ) + 1);
   }
   else
   {
      *recCount = (ULONG) (hb_arrayGetNL( thiswa->sqlarea.aInfo, AINFO_RCOUNT ));
   }

   thiswa->lLastRec = (*recCount) + 1;
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExOraRecInfo                NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraRecNo( SQLEXORAAREAP thiswa, ULONG * recno )
{
#ifdef SQLRDD_NWG_SPECIFIC
   if( thiswa->bIsInsert )
   {
      commonError( &thiswa->sqlarea.area, EG_ARG, ESQLRDD_NOT_COMMITED_YET, NULL );
      return ( HB_FAILURE );
   }
#endif
   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }
   else if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   *recno = (ULONG)GetCurrentRecordNumOra( thiswa );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraRecId( SQLEXORAAREAP thiswa, PHB_ITEM recno )
{
   if( thiswa->sqlarea.lpdbPendingRel )
   {
      SELF_FORCEREL(&thiswa->sqlarea.area );
   }
   else if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }

   if ( thiswa->sqlarea.initialized )
   {
      if( thiswa->bIsInsert || thiswa->sqlarea.area.fEof )
      {
      hb_itemPutNLL( recno, thiswa->lLastRec );
      }
      else
      {
      hb_itemPutNLL( recno,  thiswa->recordList[thiswa->recordListPos] );
      }
   }
   else
   {
      hb_itemPutNLL( recno, 0 );
   }

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExOraSetFieldExtent            NULL
#define sqlExOraAlias                     NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraClose( SQLEXORAAREAP thiswa )
{
   HB_ERRCODE code;
   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }
   code = (SUPER_CLOSE( &thiswa->sqlarea.area ));
   /* Reset parent rel struct */
   thiswa->sqlarea.lpdbPendingRel = NULL;

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
      OCI_StatementFree( thiswa->hStmtBuffer );
   if ( thiswa->hStmtInsert )
      OCI_StatementFree( thiswa->hStmtInsert );
   if ( thiswa->hStmtNextval )
      OCI_StatementFree( thiswa->hStmtNextval );
   if ( thiswa->hStmtUpdate )
      OCI_StatementFree( thiswa->hStmtUpdate );
   if ( thiswa->hStmtSkip       )
      OCI_StatementFree( thiswa->hStmtSkip       );


   ReleaseColStatementsOra( thiswa, 0 );
   ReleaseInsertRecordStructureOra( thiswa, 0 );
   ReleaseCurrRecordStructureOra( thiswa, 0 );

   if ( thiswa->aFields )
      hb_itemRelease( thiswa->aFields );

   ReleaseIndexBindStructureOra( thiswa );
   // We now use as an true structure, so let freeit
   if ( thiswa->IndexBindings )
      hb_xfree( thiswa->IndexBindings ) ;


   return code; //(SUPER_CLOSE( &thiswa->sqlarea.area ));
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraCreate( SQLEXORAAREAP thiswa, LPDBOPENINFO OpenInfo )
{
   HB_ERRCODE err;

   err = SUPER_CREATE( &thiswa->sqlarea.area, OpenInfo );

   return err;

   // Note: getWorkareaParamsOra() is executed by GoTop call
   // from super class
}

/*------------------------------------------------------------------------*/

#define sqlExOraInfo                   NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraNewArea( SQLEXORAAREAP thiswa )
{
   HB_ERRCODE errCode;
   //int i;

   errCode = SUPER_NEW(&thiswa->sqlarea.area );

   thiswa->oSql               = NULL;
   thiswa->hBufferPool        = hb_hashNew( NULL );
   thiswa->aFields            = NULL;
   thiswa->sTable             = NULL;
   thiswa->sOwner             = NULL;
   thiswa->sRecnoName         = NULL;
   thiswa->sDeletedName       = NULL;
   thiswa->sqlarea.iFieldListStatus   = FIELD_LIST_LEARNING;
   thiswa->hStmt              = NULL;
   thiswa->hStmtBuffer        = NULL;
   thiswa->hStmtInsert        = NULL;
   thiswa->hStmtNextval       = NULL;
   thiswa->hStmtUpdate        = NULL;
   thiswa->hStmtSkip          = NULL;
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

//    thiswa->recordList         = ( HB_ULONG * ) hb_xgrabDebug( __LINE__,__FILE__, RECORD_LIST_SIZE * sizeof( HB_ULONG ) );
//    thiswa->lRecordToRetrieve  = ( HB_ULONG * ) hb_xgrabDebug( __LINE__,__FILE__, pageReadSize * sizeof( HB_ULONG ) );
//    thiswa->deletedList        = ( char * ) hb_xgrabDebug( __LINE__,__FILE__, RECORD_LIST_SIZE * sizeof( char ) );
//    thiswa->sSql               = ( char * ) hb_xgrabDebug( __LINE__,__FILE__, MAX_SQL_QUERY_LEN * sizeof( char ) );
   thiswa->recordList         = ( ULONGLONG * ) hb_xgrabz( RECORD_LIST_SIZE * sizeof( ULONGLONG ) );
   thiswa->lRecordToRetrieve  = ( ULONGLONG * ) hb_xgrabz( pageReadSize * sizeof( ULONGLONG ) );
   thiswa->deletedList        = ( char * ) hb_xgrabz( RECORD_LIST_SIZE * sizeof( char ) );
   thiswa->sSql               = ( char * ) hb_xgrabz( MAX_SQL_QUERY_LEN * sizeof( char ) );
//    memset( thiswa->sSql, 0,  MAX_SQL_QUERY_LEN * sizeof( char ) );
   thiswa->sSqlBuffer         = ( char * ) hb_xgrabz( MAX_SQL_QUERY_LEN / 5  * sizeof( char ) );
//    memset(thiswa->sSqlBuffer , 0 , MAX_SQL_QUERY_LEN / 5  * sizeof( char ) );
   thiswa->sOrderBy           = ( char * ) hb_xgrabz( MAX_SQL_QUERY_LEN / 20 * sizeof( char ) );
//    memset( thiswa->sOrderBy, 0, MAX_SQL_QUERY_LEN / 20 * sizeof( char ));
   thiswa->sWhere             = ( char * ) hb_xgrabz( MAX_SQL_QUERY_LEN / 10 * sizeof( char ) );
//    memset( thiswa->sWhere, 0, MAX_SQL_QUERY_LEN / 10 * sizeof( char ) ) ;
   thiswa->InsertRecord       = NULL;
   thiswa->CurrRecord         = NULL;

   if ( thiswa->hBufferPool )
      hb_hashPreallocate( thiswa->hBufferPool,  ( bufferPoolSize * 2 ) );

   memset( thiswa->updatedMask, 0, MAX_FIELDS );
   memset( thiswa->editMask,    0, MAX_FIELDS );
   memset( thiswa->specialMask, 0, MAX_FIELDS );
   thiswa->IndexBindings = (INDEXBINDORAP *) hb_xgrabz( sizeof( INDEXBINDORAP) * MAX_INDEXES ) ;
//    memset( thiswa->IndexBindings, 0, sizeof( INDEXBINDP) * MAX_INDEXES ) ;
   //for( i = 0; i < MAX_INDEXES; i++ )
   //{
      //thiswa->IndexBindings[i] = NULL;
   //}

   return errCode;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraOpen( SQLEXORAAREAP thiswa, LPDBOPENINFO OpenInfo )
{
   HB_ERRCODE errCode;


   errCode = SUPER_OPEN(&thiswa->sqlarea.area, OpenInfo );

   if( errCode != HB_SUCCESS )
   {
      return errCode;
   }

   if( getWorkareaParamsOra( thiswa ) == HB_FAILURE )     // If workarea was opened by dbCreate()
   {
      return HB_FAILURE;
   }

   // Releases allocated cache for SQLRDD, since
   // sqlExOra does not use it

   thiswa->bOrderChanged = TRUE;

   return errCode;
}

/*------------------------------------------------------------------------*/

#define sqlExOraRelease                NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraStructSize( SQLEXORAAREAP thiswa, USHORT * StructSize )
{
   HB_SYMBOL_UNUSED( thiswa );     /* Avoid compiler warning */
   *StructSize = sizeof( SQLEXORAAREA );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlExOraSysName                NULL
#define sqlExOraEval                   NULL
#define sqlExOraPack                   NULL
#define sqlExOraPackRec                NULL
#define sqlExOraSort                   NULL
#define sqlExOraTrans                  NULL
#define sqlExOraTransRec               NULL
#define sqlExOraZap                    NULL
#define sqlExOraChildEnd               NULL
#define sqlExOraChildStart             NULL
#define sqlExOraChildSync              NULL
#define sqlExOraSyncChildren           NULL
#define sqlExOraClearRel               NULL
#define sqlExOraForceRel               NULL
#define sqlExOraRelArea                NULL
#define sqlExOraRelEval                NULL
#define sqlExOraRelText                NULL
#define sqlExOraSetRel                 NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraOrderListAdd( SQLEXORAAREAP thiswa, LPDBORDERINFO pOrderInfo )
{
   HB_ERRCODE err;
   LONG hOldOrder = thiswa->sqlarea.hOrdCurrent;

   err = SUPER_ORDLSTADD( &thiswa->sqlarea.area, pOrderInfo );

   if( hOldOrder != thiswa->sqlarea.hOrdCurrent )
   {
      thiswa->lBofAt        = 0;
      thiswa->lEofAt        = 0;
      thiswa->indexLevel    = -1;
      bOldReverseIndex = thiswa->bReverseIndex;
      thiswa->bReverseIndex = hb_arrayGetL( thiswa->sqlarea.aInfo, AINFO_REVERSE_INDEX );
   }
   return (err);
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraOrderListClear( SQLEXORAAREAP thiswa )
{
   thiswa->lBofAt     = 0;
   thiswa->lEofAt     = 0;
   thiswa->indexLevel = -1;

   SUPER_ORDLSTCLEAR(&thiswa->sqlarea.area );

   ReleaseIndexBindStructureOra( thiswa );
   thiswa->sqlarea.hOrdCurrent  = 0;

   return (HB_SUCCESS);
}

/*------------------------------------------------------------------------*/

#define sqlExOraOrderListDelete        NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraOrderListFocus( SQLEXORAAREAP thiswa, LPDBORDERINFO pOrderInfo )
{
   HB_ERRCODE err;
   LONG hOldOrder = thiswa->sqlarea.hOrdCurrent;

   err = SUPER_ORDLSTFOCUS( &thiswa->sqlarea.area, pOrderInfo );

   if( hOldOrder != thiswa->sqlarea.hOrdCurrent )
   {
      thiswa->bOrderChanged = TRUE;
      thiswa->lBofAt     = 0;
      thiswa->lEofAt     = 0;
   }

   if ( thiswa->sqlarea.hOrdCurrent > 0 )
   {
      thiswa->indexColumns  = hb_arrayLen( hb_arrayGetItemPtr( hb_arrayGetItemPtr( thiswa->sqlarea.aOrders, ( ULONG ) thiswa->sqlarea.hOrdCurrent ), INDEX_FIELDS ) );
      bOldReverseIndex = thiswa->bReverseIndex;
      thiswa->bReverseIndex = hb_arrayGetL( thiswa->sqlarea.aInfo, AINFO_REVERSE_INDEX );
      if (thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ]) {
         hb_xfree( thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ] );
         thiswa->IndexBindings[ thiswa->sqlarea.hOrdCurrent ] = NULL;
       }
   }
   else
   {
      thiswa->indexColumns  = 1;     // Natural order, RECNO
   }

   return (err);
}

/*------------------------------------------------------------------------*/

#define sqlExOraOrderListRebuild       NULL
#define sqlExOraOrderCondition         NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraOrderCreate( SQLEXORAAREAP thiswa, LPDBORDERCREATEINFO pOrderCreateInfo )
{
   HB_ERRCODE err;
   int iLen           = (int) hb_arrayLen( thiswa->aFields );
   thiswa->lBofAt     = 0;
   thiswa->lEofAt     = 0;
   thiswa->indexLevel = -1;

   err = SUPER_ORDCREATE( &thiswa->sqlarea.area, pOrderCreateInfo );

   /* Now a big GPF trap: If created index added a new database field
      (FOR clause or Synthetic Index) all allocated structures for binding
      columns are now invalid and will GPF when unalloc
   */

   if( iLen != (int) hb_arrayLen( thiswa->aFields ) )
   {
      // Release structures
      ReleaseColStatementsOra( thiswa, iLen );
      ReleaseInsertRecordStructureOra( thiswa, iLen );
      ReleaseCurrRecordStructureOra( thiswa, iLen );
      // Realloc structures
      SetCurrRecordStructureOra( thiswa );
      SetColStatementsOra( thiswa );
      SetInsertRecordStructureOra( thiswa );
   }

   if( err == HB_SUCCESS )
   {
     bOldReverseIndex = thiswa->bReverseIndex;
      thiswa->bReverseIndex = hb_arrayGetL( thiswa->sqlarea.aInfo, AINFO_REVERSE_INDEX );
   }

   return (err);
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraOrderDestroy( SQLEXORAAREAP thiswa, LPDBORDERINFO pOrderInfo )
{
   thiswa->lBofAt     = 0;
   thiswa->lEofAt     = 0;
   thiswa->indexLevel = -1;

   ReleaseIndexBindStructureOra( thiswa );

   return (SUPER_ORDDESTROY(&thiswa->sqlarea.area, pOrderInfo ));
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraOrderInfo( SQLEXORAAREAP thiswa, USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LONG lIndexes;
   HB_ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("sqlExOraOrderInfo(%p, %hu, %p)", thiswa, uiIndex, pInfo));

   lIndexes = hb_itemSize( thiswa->sqlarea.aOrders );

   if( lIndexes )
   {
      switch( uiIndex )
      {
         case DBOI_KEYCOUNT:
         {
            HB_ULONG lValue;
            getWhereExpressionOra( thiswa, LIST_FROM_TOP );
            createCountQuery( thiswa );

            if ( getFirstColumnAsLong( thiswa, &lValue ) == HB_FAILURE )
            {
               OraErrorDiagRTE( thiswa->hStmt, "OrdKeyCount", thiswa->sSql, SQL_ERROR, __LINE__, __FILE__ );
               uiError = HB_FAILURE;
            }
            else
            {
               pInfo->itmResult = hb_itemPutNInt( pInfo->itmResult,  lValue );
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
            uiError = SUPER_ORDINFO(&thiswa->sqlarea.area, uiIndex, pInfo );
            thiswa->lBofAt = 0;
            thiswa->lEofAt = 0;
            thiswa->bConditionChanged1 = TRUE;
            thiswa->bConditionChanged2 = TRUE;
            break;
         }
         default:
         {
            uiError = SUPER_ORDINFO(&thiswa->sqlarea.area, uiIndex, pInfo );
            bOldReverseIndex = thiswa->bReverseIndex;
            thiswa->bReverseIndex = hb_arrayGetL( thiswa->sqlarea.aInfo, AINFO_REVERSE_INDEX );      // OrderInfo() may change this flag
         }
      }
   }
   else
   {
      uiError = SUPER_ORDINFO(&thiswa->sqlarea.area, uiIndex, pInfo );
   }

   return uiError;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraClearFilter( SQLEXORAAREAP thiswa )
{

   thiswa->lBofAt = 0;
   thiswa->lEofAt = 0;
   thiswa->bConditionChanged1 = TRUE;
   thiswa->bConditionChanged2 = TRUE;

   return (SUPER_CLEARFILTER(&thiswa->sqlarea.area ));
}

/*------------------------------------------------------------------------*/

#define sqlExOraClearLocate            NULL
#define sqlExOraClearScope             NULL
#define sqlExOraCountScope             NULL
#define sqlExOraFilterText             NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraScopeInfo( SQLEXORAAREAP thiswa, USHORT nScope, PHB_ITEM pItem )
{
   thiswa->lBofAt = 0;
   thiswa->lEofAt = 0;
   thiswa->bConditionChanged1 = TRUE;
   thiswa->bConditionChanged2 = TRUE;
   return ( SUPER_SCOPEINFO(&thiswa->sqlarea.area, nScope, pItem ) );
}

/*------------------------------------------------------------------------*/

//#define sqlExOraSetFilter               NULL     // Must be written to update thiswa->bConditionChanged
//culik 2010/07/07 implemented sqlExOraSetFilter
static HB_ERRCODE sqlExOraSetFilter( SQLEXORAAREAP thiswa, LPDBFILTERINFO pFilterInfo )
{
   HB_ERRCODE ret;
   ret = SUPER_SETFILTER(&thiswa->sqlarea.area, pFilterInfo );
   if ( ret == HB_SUCCESS )
   {
      thiswa->bConditionChanged1 = TRUE;
      thiswa->bConditionChanged2 = TRUE;
   }
   return ret;

}
#define sqlExOraSetLocate              NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraSetScope( SQLEXORAAREAP thiswa, LPDBORDSCOPEINFO sInfo )
{
   thiswa->lBofAt = 0;
   thiswa->lEofAt = 0;
   thiswa->bConditionChanged1 = TRUE;
   thiswa->bConditionChanged2 = TRUE;
   return ( SUPER_SETSCOPE(&thiswa->sqlarea.area, sInfo ) );
}

/*------------------------------------------------------------------------*/

#define sqlExOraSkipScope              NULL
#define sqlExOraLocate                 NULL
#define sqlExOraCompile                NULL
#define sqlExOraError                  NULL
#define sqlExOraEvalBlock              NULL
#define sqlExOraRawLock                NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraLock( SQLEXORAAREAP thiswa, LPDBLOCKINFO pLockInfo )
{
   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }
   if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }
   hb_arraySetL( thiswa->sqlarea.aInfo, AINFO_BOF, thiswa->sqlarea.area.fBof );
   hb_arraySetL( thiswa->sqlarea.aInfo, AINFO_EOF, thiswa->sqlarea.area.fEof );
   hb_arraySetNLL( thiswa->sqlarea.aInfo, AINFO_RECNO, GetCurrentRecordNumOra( thiswa ) );
   return( SUPER_LOCK(&thiswa->sqlarea.area, pLockInfo ) );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExOraUnLock( SQLEXORAAREAP thiswa, PHB_ITEM pRecNo )
{
   if( SELF_GOCOLD(&thiswa->sqlarea.area ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }
   if( thiswa->sqlarea.firstinteract )
   {
      SELF_GOTOP( &thiswa->sqlarea.area );
      thiswa->sqlarea.firstinteract = 0;
   }
   hb_arraySetL( thiswa->sqlarea.aInfo, AINFO_BOF, thiswa->sqlarea.area.fBof );
   hb_arraySetL( thiswa->sqlarea.aInfo, AINFO_EOF, thiswa->sqlarea.area.fEof );
   hb_arraySetNLL( thiswa->sqlarea.aInfo, AINFO_RECNO, GetCurrentRecordNumOra( thiswa ) );
   return( SUPER_UNLOCK(&thiswa->sqlarea.area, pRecNo ) );
}

/*------------------------------------------------------------------------*/

#define sqlExOraCloseMemFile           NULL
#define sqlExOraCreateMemFile          NULL
#define sqlExOraGetValueFile           NULL
#define sqlExOraOpenMemFile            NULL
#define sqlExOraPutValueFile           NULL
#define sqlExOraReadDBHeader           NULL
#define sqlExOraWriteDBHeader          NULL
#define sqlExOraInit                   NULL
#define sqlExOraExit                   NULL
#define sqlExOraDrop                   NULL
#define sqlExOraExists                 NULL
#define sqlExOraInfo                   NULL
#define sqlExOraWhoCares               NULL

/*------------------------------------------------------------------------*/

static const RDDFUNCS sqlTable =
{

   /* Movement and positioning methods */

   ( DBENTRYP_BP )  sqlExOraBof,
   ( DBENTRYP_BP )  sqlExOraEof,
   ( DBENTRYP_BP )  sqlExOraFound,
   ( DBENTRYP_V )   sqlExOraGoBottom,
   ( DBENTRYP_UL )  sqlExOraGoTo,
   ( DBENTRYP_I )   sqlExOraGoToId,
   ( DBENTRYP_V )   sqlExOraGoTop,
   ( DBENTRYP_BIB ) sqlExOraSeek,
   ( DBENTRYP_L )   sqlExOraSkip,
   ( DBENTRYP_L )   sqlExOraSkipFilter,
   ( DBENTRYP_L )   sqlExOraSkipRaw,


   /* Data management */

   ( DBENTRYP_VF )  sqlExOraAddField,
   ( DBENTRYP_B )   sqlExOraAppend,
   ( DBENTRYP_I )   sqlExOraCreateFields,
   ( DBENTRYP_V )   sqlExOraDeleteRec,
   ( DBENTRYP_BP )  sqlExOraDeleted,
   ( DBENTRYP_SP )  sqlExOraFieldCount,
   ( DBENTRYP_VF )  sqlExOraFieldDisplay,
   ( DBENTRYP_SSI ) sqlExOraFieldInfo,
   ( DBENTRYP_SCP ) sqlExOraFieldName,
   ( DBENTRYP_V )   sqlExOraFlush,
   ( DBENTRYP_PP )  sqlExOraGetRec,
   ( DBENTRYP_SI )  sqlExOraGetValue,
   ( DBENTRYP_SVL ) sqlExOraGetVarLen,
   ( DBENTRYP_V )   sqlExOraGoCold,
   ( DBENTRYP_V )   sqlExOraGoHot,
   ( DBENTRYP_P )   sqlExOraPutRec,
   ( DBENTRYP_SI )  sqlExOraPutValue,
   ( DBENTRYP_V )   sqlExOraRecall,
   ( DBENTRYP_ULP ) sqlExOraRecCount,
   ( DBENTRYP_ISI ) sqlExOraRecInfo,
   ( DBENTRYP_ULP ) sqlExOraRecNo,
   ( DBENTRYP_I )   sqlExOraRecId,
   ( DBENTRYP_S )   sqlExOraSetFieldExtent,


   /* WorkArea/Database management */

   ( DBENTRYP_CP )  sqlExOraAlias,
   ( DBENTRYP_V )   sqlExOraClose,
   ( DBENTRYP_VO )  sqlExOraCreate,
   ( DBENTRYP_SI )  sqlExOraInfo,
   ( DBENTRYP_V )   sqlExOraNewArea,
   ( DBENTRYP_VO )  sqlExOraOpen,
   ( DBENTRYP_V )   sqlExOraRelease,
   ( DBENTRYP_SP )  sqlExOraStructSize,
   ( DBENTRYP_CP )  sqlExOraSysName,
   ( DBENTRYP_VEI ) sqlExOraEval,
   ( DBENTRYP_V )   sqlExOraPack,
   ( DBENTRYP_LSP ) sqlExOraPackRec,
   ( DBENTRYP_VS )  sqlExOraSort,
   ( DBENTRYP_VT )  sqlExOraTrans,
   ( DBENTRYP_VT )  sqlExOraTransRec,
   ( DBENTRYP_V )   sqlExOraZap,


   /* Relational Methods */

   ( DBENTRYP_VR )  sqlExOraChildEnd,
   ( DBENTRYP_VR )  sqlExOraChildStart,
   ( DBENTRYP_VR )  sqlExOraChildSync,
   ( DBENTRYP_V )   sqlExOraSyncChildren,
   ( DBENTRYP_V )   sqlExOraClearRel,
   ( DBENTRYP_V )   sqlExOraForceRel,
   ( DBENTRYP_SSP ) sqlExOraRelArea,
   ( DBENTRYP_VR )  sqlExOraRelEval,
   ( DBENTRYP_SI )  sqlExOraRelText,
   ( DBENTRYP_VR )  sqlExOraSetRel,


   /* Order Management */

   ( DBENTRYP_VOI ) sqlExOraOrderListAdd,
   ( DBENTRYP_V )   sqlExOraOrderListClear,
   ( DBENTRYP_VOI ) sqlExOraOrderListDelete,
   ( DBENTRYP_VOI ) sqlExOraOrderListFocus,
   ( DBENTRYP_V )   sqlExOraOrderListRebuild,
   ( DBENTRYP_VOO ) sqlExOraOrderCondition,
   ( DBENTRYP_VOC ) sqlExOraOrderCreate,
   ( DBENTRYP_VOI )  sqlExOraOrderDestroy,
   ( DBENTRYP_SVOI ) sqlExOraOrderInfo,


   /* Filters and Scope Settings */

   ( DBENTRYP_V )    sqlExOraClearFilter,
   ( DBENTRYP_V )    sqlExOraClearLocate,
   ( DBENTRYP_V )    sqlExOraClearScope,
   ( DBENTRYP_VPLP ) sqlExOraCountScope,
   ( DBENTRYP_I )    sqlExOraFilterText,
   ( DBENTRYP_SI )   sqlExOraScopeInfo,
   ( DBENTRYP_VFI )  sqlExOraSetFilter,
   ( DBENTRYP_VLO )  sqlExOraSetLocate,
   ( DBENTRYP_VOS )  sqlExOraSetScope,
   ( DBENTRYP_VPL )  sqlExOraSkipScope,
   ( DBENTRYP_B )    sqlExOraLocate,


   /* Miscellaneous */

   ( DBENTRYP_CC )  sqlExOraCompile,
   ( DBENTRYP_I )   sqlExOraError,
   ( DBENTRYP_I )   sqlExOraEvalBlock,


   /* Network operations */

   ( DBENTRYP_VSP ) sqlExOraRawLock,
   ( DBENTRYP_VL )  sqlExOraLock,
   ( DBENTRYP_I )   sqlExOraUnLock,


   /* Memofile functions */

   ( DBENTRYP_V )    sqlExOraCloseMemFile,
   ( DBENTRYP_VO )   sqlExOraCreateMemFile,
   ( DBENTRYP_SCCS ) sqlExOraGetValueFile,
   ( DBENTRYP_VO )   sqlExOraOpenMemFile,
   ( DBENTRYP_SCCS ) sqlExOraPutValueFile,


   /* Database file header handling */

   ( DBENTRYP_V )     sqlExOraReadDBHeader,
   ( DBENTRYP_V )     sqlExOraWriteDBHeader,


   /* non WorkArea functions       */
   ( DBENTRYP_R )     sqlExOraInit,
   ( DBENTRYP_R )     sqlExOraExit,
   ( DBENTRYP_RVVL )  sqlExOraDrop,
   ( DBENTRYP_RVVL )  sqlExOraExists,
   ( DBENTRYP_RVVVL ) NULL,   /* sqlExOraRename */
   ( DBENTRYP_RSLV )  sqlExOraInfo,

   /* Special and reserved methods */

   ( DBENTRYP_SVP )   sqlExOraWhoCares
};

HB_FUNC( SQLEXORA ) {;}

HB_FUNC( SQLEXORA_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;
/*
   startsqlExOraSymbols();
*/
   uiCount = ( USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );

   HB_TRACE(HB_TR_DEBUG, ("sqlExOra_GETFUNCTABLE(%p, %p)", uiCount, pTable));

   if( pTable )
   {
      HB_ERRCODE errCode;

      if ( uiCount )
         * uiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInherit( pTable, &sqlTable, &sqlExOraSuper, ( const char * ) "SQLRDD"  );
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

static void hb_sqlExOraRddInitora( void * cargo )
{

   USHORT usResult;
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "SQLRDD", RDT_FULL ) <= 1 )
   {
      usResult = ( USHORT ) hb_rddRegister( "SQLEXORA", RDT_FULL );
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

HB_INIT_SYMBOLS_BEGIN( sqlExOra1Ora__InitSymbols )
{ "SQLEXORA",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SQLEXORA )}, NULL },
{ "SQLEXORA_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SQLEXORA_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( sqlExOra1Ora__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_sqlExOraora_rdd_init_ )
   hb_vmAtInit( hb_sqlExOraRddInitora, NULL );
HB_CALL_ON_STARTUP_END( _hb_sqlExOraora_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup sqlExOra1Ora__InitSymbols
   #pragma startup _hb_sqlExOraora_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( sqlExOra1Ora__InitSymbols) \
                              HB_DATASEG_FUNC( _hb_sqlExOraora_rdd_init_)
   #include "hbiniseg.h"

#endif

HB_FUNC( SR_SETPAGEREADSIZE2 )
{
   if( ISNUM( 1 ) )
   {
      pageReadSize = hb_itemGetNS( hb_param( 1, HB_IT_NUMERIC ) );
   }
}

HB_FUNC( SR_SETBUFFERPOOLSIZE2 )
{
   if( ISNUM( 1 ) )
   {
      bufferPoolSize = hb_itemGetNS( hb_param( 1, HB_IT_NUMERIC ) );
   }
}

static int sqlKeyCompareEx( SQLEXORAAREAP thiswa, PHB_ITEM pKey, BOOL fExact )
{
   LONG lorder  = 0;
   PHB_ITEM pTag, pKeyVal, itemTemp;
   int iLimit, iResult = 0;
   HB_SIZE len1, len2;
   const char * val1, * val2;
   char * valbuf = NULL;

   // TraceLog( NULL, "sqlKeyCompare\n" );

   pTag = loadTagDefault( thiswa, NULL, &lorder );
   if( pTag )
   {
      if( thiswa->sqlarea.firstinteract )
      {
         SELF_GOTOP( &thiswa->sqlarea.area );
         thiswa->sqlarea.firstinteract = 0;
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
         pKeyVal = hb_itemArrayGet( thiswa->sqlarea.aBuffer, hb_arrayGetNL( pTag, INDEX_KEY_CODEBLOCK )-2 );
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
      valbuf = ( char * ) hb_xgrab( 9 );
      val2 = hb_itemGetDS( pKey, valbuf );
   }
   else if( HB_IS_NUMBER( pKey ) )
   {
      PHB_ITEM pLen = hb_itemPutNL( NULL, (const LONG) len1 );
      val2 = valbuf = hb_itemStr( pKey, pLen, NULL );
      len2 = (HB_SIZE)strlen( val2 );
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

                           /*

void SQLO_FieldGet( PHB_ITEM pField, PHB_ITEM pItem, int iField, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate,OCI_Resultset * rs )
{
   LONG lType;
   LONG lLen, lDec;
   PHB_ITEM pTemp;
   unsigned int uiLen;

   HB_SYMBOL_UNUSED( bQueryOnly );
   HB_SYMBOL_UNUSED( ulSystemID );

   lType = ( LONG ) hb_arrayGetNL( pField, 6 );
   lLen  = ( LONG ) hb_arrayGetNL( pField, 3 );
   lDec  = ( LONG ) hb_arrayGetNL( pField, 4 );

   //if( lLenBuff <= 0 )     // database content is NULL
   if (OCI_IsNull( rs, iField ) )
   {
      switch( lType )
      {
         case SQL_CHAR:
         {
            char * szResult = ( char * ) hb_xgrabDebug( __LINE__,__FILE__, lLen + 1 );
            hb_xmemset( szResult, ' ', lLen );
            hb_itemPutCLPtr( pItem, szResult, (HB_SIZE) lLen );
            break;
         }
         case SQL_NUMERIC:
         case SQL_FAKE_NUM:
         {
            char szResult[2] = { ' ', '\0' };
            sr_escapeNumber( szResult, (HB_SIZE) lLen, (HB_SIZE) lDec, pItem );
//             hb_itemPutNL(pItem,0);
            break;
         }
         case SQL_DATE:
         {
            char dt[9] = {' ',' ',' ',' ',' ',' ',' ',' ','\0'};
            hb_itemPutDS( pItem, dt );
            break;
         }
         case SQL_LONGVARCHAR:
         {
            hb_itemPutCL( pItem, "", 0 );
            break;
         }
         case SQL_BIT:
         {
            hb_itemPutL( pItem, FALSE );
            break;
         }

#ifdef SQLRDD_TOPCONN
         case SQL_FAKE_DATE:
         {
            hb_itemPutDS( pItem, bBuffer );
            break;
         }
#endif
         case SQL_DATETIME:
         {
#ifdef __XHARBOUR__
            hb_itemPutDT( pItem, 0, 0, 0, 0, 0, 0, 0 );
#else
            hb_itemPutTDT( pItem, 0, 0 );
#endif
            break;
         }

         default:
            TraceLog( "oci.log", "Invalid data type detected: %i\n", lType );
      }
   }
   else
   {
      switch( lType )
      {
         case SQL_CHAR:
         {
            HB_SIZE lPos;
            char * szResult = ( char * ) hb_xgrabDebug( __LINE__,__FILE__, lLen + 1 );
            memset(szResult,' ', lLen) ;
            uiLen = OCI_GetDataLength(rs,iField);

            hb_xmemcpy( szResult,(char*)OCI_GetString(rs,iField ),uiLen ) ;

            //hb_itemPutCLPtr( pItem, szResult, (ULONG) uiLen );
            hb_itemPutCLPtr( pItem, szResult, (HB_SIZE) lLen );
            break;
         }
         case SQL_NUMERIC:
         {
            char * bBuffer = (char*)OCI_GetString( rs, iField );
            sr_escapeNumber( bBuffer, (HB_SIZE) lLen, (HB_SIZE) lDec, pItem );
            break;
         }
         case SQL_DATE:
         {
            char dt[9];
            char * bBuffer = (char*)OCI_GetString( rs, iField );

            dt[0] = bBuffer[0];
            dt[1] = bBuffer[1];
            dt[2] = bBuffer[2];
            dt[3] = bBuffer[3];
            dt[4] = bBuffer[4];
            dt[5] = bBuffer[5];
            dt[6] = bBuffer[6];
            dt[7] = bBuffer[7];
            dt[8] = '\0';

            if ( strcmp( dt, "19000101")  == 0 )
            {
               dt[0] = ' ';
               dt[1] = ' ';
               dt[2] = ' ';
               dt[3] = ' ';
               dt[4] = ' ';
               dt[5] = ' ';
               dt[6] = ' ';
               dt[7] = ' ';
               dt[8] = '\0';
            }
            hb_itemPutDS( pItem, dt );
            break;
         }
         case SQL_LONGVARCHAR:
         {
            char *bBuffer = (char*)OCI_GetString(rs,iField ) ;
            ULONG lLenBuff = strlen( bBuffer ) ;
            if( lLenBuff > 0 && (strncmp( bBuffer, "[", 1 ) == 0 || strncmp( bBuffer, "[]", 2 ) )   && (sr_lSerializeArrayAsJson()) )
            {
               if (s_pSym_SR_FROMJSON == NULL )
               {
                  hb_dynsymLock();
                  s_pSym_SR_FROMJSON = hb_dynsymFindName( "HB_JSONDECODE" );
                  hb_dynsymUnlock();
                  if ( s_pSym_SR_FROMJSON  == NULL ) printf( "Could not find Symbol HB_JSONDECODE\n" );
               }
               hb_vmPushDynSym( s_pSym_SR_FROMJSON );
               hb_vmPushNil();
               hb_vmPushString( bBuffer, lLenBuff );
               pTemp = hb_itemNew( NULL );
               hb_vmPush(pTemp);
               hb_vmDo( 2 );

               hb_itemForwardValue( pItem, pTemp );
               hb_itemRelease( pTemp );

            }

            else if( lLenBuff > 10 && strncmp( bBuffer, SQL_SERIALIZED_SIGNATURE, 10 ) == 0  && (!sr_lSerializedAsString()) )
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

               if( HB_IS_HASH( pTemp ) && sr_isMultilang() && bTranslate )
               {
                  PHB_ITEM pLangItem = hb_itemNew( NULL );
                  ULONG ulPos;
                  if( hb_hashScan( pTemp, sr_getBaseLang( pLangItem ), &ulPos ) ||
                       hb_hashScan( pTemp, sr_getSecondLang( pLangItem ), &ulPos ) ||
                       hb_hashScan( pTemp, sr_getRootLang( pLangItem ), &ulPos ) )
                  {
                     hb_itemCopy( pItem, hb_hashGetValueAt( pTemp, ulPos ) );
                  }
                  hb_itemRelease( pLangItem );
               }
               else
               {
                  hb_itemForwardValue( pItem, pTemp );
               }
               hb_itemRelease( pTemp );
            }

            else
            {
               hb_itemPutCL( pItem, bBuffer, (ULONG) lLenBuff );
            }
            break;
         }
         case SQL_BIT:
         {
            //hb_itemPutL( pItem, bBuffer[0] == '1' ? TRUE : FALSE );
            hb_itemPutL( pItem, OCI_GetBigInt(rs,iField) ==1 ? TRUE : FALSE );
            break;
         }

#ifdef SQLRDD_TOPCONN
         case SQL_FAKE_DATE:
         {
            hb_itemPutDS( pItem, bBuffer );
            break;
         }
#endif
         case SQL_DATETIME:
         {
         char *bBuffer = (char*)OCI_GetString( rs, iField );
#ifdef __XHARBOUR__
            //hb_retdts(bBuffer);
            char dt[18];

            dt[0] = bBuffer[0];
            dt[1] = bBuffer[1];
            dt[2] = bBuffer[2];
            dt[3] = bBuffer[3];
            dt[4] = bBuffer[5];
            dt[5] = bBuffer[6];
            dt[6] = bBuffer[8];
            dt[7] = bBuffer[9];
            dt[8] = bBuffer[11];
            dt[9] = bBuffer[12];
            dt[10] = bBuffer[14];
            dt[11] = bBuffer[15];
            dt[12] = bBuffer[17];
            dt[13] = bBuffer[18];
            dt[14] = '\0';

            hb_itemPutDTS( pItem, dt );
#else
            long lJulian, lMilliSec;
            hb_timeStampStrGetDT( bBuffer, &lJulian, &lMilliSec );
            hb_itemPutTDT( pItem, lJulian, lMilliSec );
#endif
            break;
         }

         default:
            TraceLog( "oci.log", "Invalid data type detected: %i\n", lType );
      }
   }
}
*/

void OraErrorDiagRTE( OCI_Statement* hStmt, char * routine, char * szSql, int res, int line, char * module )
{
   PHB_ITEM pArg;
   PHB_ITEM pError = hb_errNew();


   char      *    ErrMsg = ( char*) hb_xgrabz(1024*2);
   OCI_Error *err = OCI_GetLastError();

   HB_SYMBOL_UNUSED( hStmt );
   if( sr_isShutdownProcess() )
   {
      return;
   }





   sprintf( ErrMsg, "SQL execution error at %s, return code: %i, state: %i, description: %s.", routine, res, OCI_ErrorGetOCICode(err), OCI_ErrorGetString(err) );

   if( szSql )
   {
      pArg = hb_itemNew( NULL );
      hb_itemPutC( pArg, szSql );
      if( hb_itemGetCLen( pArg ) )
      {
         hb_errPutArgs( pError, 1, pArg );
      }
      hb_itemRelease( pArg );
   }

   hb_errPutDescription( pError, ErrMsg );
   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutTries( pError, EF_NONE );
   hb_errPutSubSystem( pError, "SQLRDD" );
#ifdef __XHARBOUR__
   hb_errPutModuleName( pError, module );
   if( routine )
   {
      hb_errPutProcName( pError, routine );
      hb_errPutProcLine( pError, line );
   }
#else
   HB_SYMBOL_UNUSED( line );
   HB_SYMBOL_UNUSED( module );
#endif
   hb_errLaunch( pError );
   hb_itemRelease( pError );
   hb_xfree(ErrMsg);
   return;
}