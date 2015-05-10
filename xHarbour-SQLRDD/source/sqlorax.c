/* $CATEGORY$SQLRDD/HIDE$FILES$HIDE$
* SQLRDD Oracle native connection
* Copyright (c) 2005 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
* SQLRDD Oracle Native Bind Utility Functions
* Copyright (c) 2004-2005 - Luiz Rafael Culik Guimaraes <luiz@xharbour.com.br>
*/

#include "compat.h"

#include "sqlrddsetup.ch"
#include "sqlprototypes.h"
#include "sqlodbc.ch"
#include "sqlora.h"

#if !defined(__GNUC__) && defined(WIN32)
#define inline __inline
#define __STDC__
#endif

#define MAX_CONNECTIONS    50
#define MAX_CURSORS        65535
#define MAX_COLUMNS        1024

static PHB_DYNS s_pSym_SR_DESERIALIZE = NULL;
static PHB_DYNS s_pSym_SR_FROMJSON = NULL;

//-----------------------------------------------------------------------------//

typedef struct _ORA_BIND_COLS
{
   char * col_name;
   short sVal;
   double  dValue;
   int iType;
   ULONG  ulValue;
   char sDate[ 7 ];
   int iValue;
   char sValue[31];   
//    OCIRowId * RowId;
} ORA_BIND_COLS ;


typedef struct _OCI_SESSION
{
   int dbh;                      // Connection handler
   int stmt;                     // Current statement handler
   int status;                   // Execution return value
   int numcols;                  // Result set columns
   char server_version[128];
   //bellow for bind vars
   sqlo_stmt_handle_t stmtParam;
   ORA_BIND_COLS *  pLink;
   unsigned int   ubBindNum;
   sqlo_stmt_handle_t stmtParamRes;
   unsigned int uRows;
} OCI_SESSION;

typedef OCI_SESSION * POCI_SESSION;

static USHORT OCI_initilized = 0;

#ifdef HAVE_USLEEP
#  define SQLO_USLEEP usleep(20000)
#else
#  define SQLO_USLEEP
#endif

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_CONNECT )
{
   POCI_SESSION session = (POCI_SESSION) hb_xgrab( sizeof( OCI_SESSION ) );

   if (!OCI_initilized)
   {
#if defined(ENABLE_PTHREADS)  && defined(HAVE_PTHREAD_H)
      session->status = sqlo_init(1, MAX_CONNECTIONS, MAX_CURSORS );
#else
      session->status = sqlo_init(0, MAX_CONNECTIONS, MAX_CURSORS );
#endif
   }
   else
   {
      session->status = SQLO_SUCCESS;
   }

   OCI_initilized ++;

   if (SQLO_SUCCESS != session->status)
   {

      hb_retni( SQL_ERROR );
   }

   session->status = sqlo_connect( &(session->dbh), hb_parcx(1) );

   if (SQLO_SUCCESS != session->status)
   {
      hb_retni( SQL_ERROR );
   }
   else
   {
      session->stmtParamRes= SQLO_STH_INIT;
      sqlo_server_version(session->dbh, session->server_version, sizeof(session->server_version));
      hb_storptr( ( void * ) session, 2 );
      hb_retni( SQL_SUCCESS );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_DBMSNAME )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      hb_retc( session->server_version );
   }
   else
   {
      hb_retc( "Not connected to Oracle" );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_DISCONNECT )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      sqlo_finish( session->dbh );

      OCI_initilized--;
      if( !OCI_initilized )
      {
         sqlo_freeall();
      }
      hb_xfree( session );
      hb_retni( SQL_SUCCESS );
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_GETERRORDESCR )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      hb_retc( (char *) sqlo_geterror( session->dbh ) );
   }
   else
   {
      hb_retc( "Not connected to Oracle" );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_GETERRORCODE )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      hb_retni( sqlo_geterrcode( session->dbh ) );
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_EXECDIRECT )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   const char * stm = hb_parcx(2);

   if( session )
   {
      while (SQLO_STILL_EXECUTING == (session->status = sqlo_exec(session->dbh, stm,&session->uRows)))
      {
         SQLO_USLEEP;
      }
      switch (session->status)
      {
      case SQLO_SUCCESS_WITH_INFO:
      case SQLO_SUCCESS:
      case SQLO_NO_DATA:
      	hb_retni( SQL_SUCCESS );
      	break;
      default:
      	hb_retni( SQL_ERROR );
      }
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_EXECUTE )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   BOOL lStmt = ISLOG( 3 ) ? hb_parl( 3 ) : 0;
   if( session )
   {


      if (lStmt )
      {

         while (SQLO_STILL_EXECUTING == (session->status = sqlo_executeselect( session->stmt , 1 )))
     	  {
            SQLO_USLEEP;
         }
      }
      else
      while (SQLO_STILL_EXECUTING == (session->status = sqlo_open2( &(session->stmt), session->dbh, hb_parcx(2), 0, NULL )))
     	{
         SQLO_USLEEP;
      }

      if(SQLO_SUCCESS != session->status && SQLO_SUCCESS_WITH_INFO != session->status)
      {
         session->numcols = 0;
         hb_retni( SQL_ERROR );
      }
      else
      {
         session->numcols = sqlo_ncols( session->stmt, 0 );
         hb_retni( SQL_SUCCESS );
      }
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_NUMCOLS )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      hb_retni( session->numcols );
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

int sqlo_sqldtype( USHORT type )
{
   int isqltype;

   switch( type )
   {
      case SQLOT_CHR:
      case SQLOT_STR:
      case SQLOT_VCS:
      case SQLOT_NON:
      case SQLOT_VBI:
      case SQLOT_BIN:
      case SQLOT_LBI:
      case SQLOT_SLS:
      case SQLOT_LVC:
      case SQLOT_LVB:
      case SQLOT_AFC:
      case SQLOT_AVC:
      case SQLOT_CUR:
      case SQLOT_RDD:
      case SQLOT_LAB:
      case SQLOT_OSL:
      case SQLOT_NTY:
      case SQLOT_REF:
      case SQLOT_TIME:
      case SQLOT_TIME_TZ:
      case SQLOT_VST:
         isqltype = SQL_CHAR;
         break;
      case SQLOT_CLOB:
      case SQLOT_BLOB:
      case SQLOT_BFILEE:
      case SQLOT_CFILEE:
      case SQLOT_RSET:
      case SQLOT_NCO:
         isqltype = SQL_LONGVARCHAR;
         break;
      case SQLOT_NUM:
      case SQLOT_UIN:
      case SQLOT_INT:
      case SQLOT_FLT:
      case SQLOT_VNU:
      case SQLOT_PDN:
      case SQLOT_LNG:
      case SQLOT_RID:
      case SQLOT_INTERVAL_YM:
      case SQLOT_INTERVAL_DS:
         isqltype = SQL_NUMERIC;
         break;
      case SQLOT_DAT:
      case SQLOT_ODT:
      case SQLOT_DATE:
         isqltype = SQL_DATE;
         break;
      case SQLOT_TIMESTAMP:
      case SQLOT_TIMESTAMP_TZ:
      case SQLOT_TIMESTAMP_LTZ:
         isqltype= SQL_DATETIME;
         break;
      default:
         isqltype = 0;
   }
   return isqltype;
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_DESCRIBECOL ) // ( hStmt, nCol, @cName, @nDataType, @nColSize, @nDec, @nNull )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   USHORT dType, ncol;
   int prec, scale, nullok, namelen, dbsize, type;
   char * name;
    sqlo_stmt_handle_t stmtParamRes;

   if( session )
   {
      ncol = (USHORT)hb_parni(2)-1;
      stmtParamRes = session->stmtParamRes != -1 ? session->stmtParamRes : session->stmt;
      sqlo_describecol( stmtParamRes, ncol, &dType, &name, &namelen, &prec, &scale, &dbsize, &nullok );
      type = sqlo_sqldtype( dType );
      hb_storni( type, 4 );

      if (dType == SQLOT_RDD)
      {
	      dbsize = 18 ;
      }
      if( type == SQL_CHAR )
      {
         hb_storni( 0, 6 );
         hb_storni( dbsize, 5 );
      }      
      else if ( type == SQL_NUMERIC )
      {
         if( prec == 0 )
         {
            hb_storni( 19, 5 );
            hb_storni( 6, 6 );
         }
         else
         {
            hb_storni( prec, 5 );
            hb_storni( scale, 6 );
         }
      }
      else if ( type == SQL_DATETIME )       
      {
	     hb_storni( 0, 6 );
         hb_storni( 8, 5 );
      }
      else
      {
         hb_storni( prec, 5 );
         hb_storni( scale, 6 );
      }

      hb_storl( nullok, 7 );
      hb_storc( name, 3 );
      hb_retni( SQL_SUCCESS );
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_FETCH )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   sqlo_stmt_handle_t stmtParamRes;

   if( session )
   {
      stmtParamRes = session->stmtParamRes != -1 ? session->stmtParamRes : session->stmt;
      while (SQLO_STILL_EXECUTING == (session->status = sqlo_fetch( stmtParamRes, 1 )))
      {
         SQLO_USLEEP;
      }

      if( session->status == 0 || session->status == 1 )
      {
         hb_retni( SQL_SUCCESS );
      }
      else if( session->status < 0 )
      {
         hb_retni( SQL_NO_DATA_FOUND );
      }
      else
      {
         hb_retni( SQL_NO_DATA_FOUND );
      }
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_COMMIT )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session  )
   {
      session->status = sqlo_commit( session->dbh );
      if( SQLO_SUCCESS == session->status )
      {
         hb_retni( SQL_SUCCESS );
      }
      else
      {
         hb_retni( SQL_ERROR );
      }
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_ROLLBACK )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      session->status = sqlo_rollback( session->dbh );
      if( SQLO_SUCCESS == session->status )
      {
         hb_retni( SQL_SUCCESS );
      }
      else
      {
         hb_retni( SQL_ERROR );
      }
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_CLOSESTMT )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {

      if ( session->stmtParamRes  != -1)    
      {
         session->status = sqlo_close( session->stmtParamRes );
         session->stmtParamRes= SQLO_STH_INIT;
         session->status = sqlo_close( session->stmtParam );
      }	 
      session->status = sqlo_close( session->stmt );
      hb_retni( session->status );
   }
   hb_retni( SQL_SUCCESS );
}


//-----------------------------------------------------------------------------//

void SQLO_FieldGet( PHB_ITEM pField, PHB_ITEM pItem, char * bBuffer, HB_SIZE lLenBuff, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate )
{
   LONG lType;
   HB_SIZE lLen;
   HB_SIZE lDec; 
   PHB_ITEM pTemp;

   HB_SYMBOL_UNUSED( bQueryOnly );
   HB_SYMBOL_UNUSED( ulSystemID );

   lType = ( LONG ) hb_arrayGetNL( pField, 6 );
   lLen  = hb_arrayGetNL( pField, 3 );
   lDec  = hb_arrayGetNL( pField, 4 );

   if( lLenBuff <= 0 )     // database content is NULL
   {
      switch( lType )
      {
         case SQL_CHAR:
         {
            char * szResult = ( char * ) hb_xgrab( lLen + 1 );
            hb_xmemset( szResult, ' ', lLen );
            szResult[ lLen ] =  '\0';            
            hb_itemPutCLPtr( pItem, szResult,  lLen );
            break;
         }
         case SQL_NUMERIC:
         case SQL_FAKE_NUM:
         {
            char szResult[2] = { ' ', '\0' };
            sr_escapeNumber( szResult, lLen,  lDec, pItem );
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
            hb_itemPutCL( pItem, bBuffer, 0 );
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
// #ifdef __XHARBOUR__
//             hb_itemPutDT( pItem, 0, 0, 0, 0, 0, 0, 0 );
// #else
            hb_itemPutTDT( pItem, 0, 0 );
// #endif
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
            char * szResult = ( char * ) hb_xgrab( lLen + 1 );
            hb_xmemcpy( szResult, bBuffer,  (lLen < lLenBuff ? lLen : lLenBuff ) );

            for( lPos =  lLenBuff; lPos < lLen; lPos++ )
            {
               szResult[ lPos ] = ' ';
            }
            szResult[ lLen ] =  '\0';
            hb_itemPutCLPtr( pItem, szResult,  lLen );
            break;
         }
         case SQL_NUMERIC:
         {
            sr_escapeNumber( bBuffer,  lLen,  lDec, pItem );
            break;
         }
         case SQL_DATE:
         {
            char dt[9];
            dt[0] = bBuffer[0];
            dt[1] = bBuffer[1];
            dt[2] = bBuffer[2];
            dt[3] = bBuffer[3];
            dt[4] = bBuffer[4];
            dt[5] = bBuffer[5];
            dt[6] = bBuffer[6];
            dt[7] = bBuffer[7];
            dt[8] = '\0';
            hb_itemPutDS( pItem, dt );
            break;
         }
         case SQL_LONGVARCHAR:
         {
            if( lLenBuff > 0 && (strncmp( bBuffer, "[", 1 ) == 0 || strncmp( bBuffer, "[]", 2 ) )&& (sr_lSerializeArrayAsJson()) )
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
               /* TOFIX: */
               hb_itemForwardValue( pItem, pTemp );
               hb_itemRelease( pTemp );

            }

            else if( lLenBuff > 10 && strncmp( bBuffer, SQL_SERIALIZED_SIGNATURE, 10 ) == 0 && (!sr_lSerializedAsString()) )
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
                  HB_SIZE ulPos;
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
               hb_itemPutCL( pItem, bBuffer,  lLenBuff );
            }
            break;
         }
         case SQL_BIT:
         {
            hb_itemPutL( pItem, bBuffer[0] == '1' ? TRUE : FALSE );
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
            //hb_retdts(bBuffer);
//             char dt[18];
//             
//             dt[0] = bBuffer[0];
//             dt[1] = bBuffer[1];
//             dt[2] = bBuffer[2];
//             dt[3] = bBuffer[3];
//             dt[4] = bBuffer[4];
//             dt[5] = bBuffer[5];
//             dt[6] = bBuffer[6];
//             dt[7] = bBuffer[7];
//             dt[8] = bBuffer[9];
//             dt[9] = bBuffer[10];
//             dt[10] = bBuffer[12];
//             dt[11] = bBuffer[13];
//             dt[12] = bBuffer[15];
//             dt[13] = bBuffer[16];
//             dt[14] = '\0';
// 
//             hb_itemPutDTS( pItem, dt );
            long lJulian, lMilliSec;
            hb_dateTimeStampStrGet( bBuffer, &lJulian, &lMilliSec );
            hb_itemPutTDT( pItem, lJulian, lMilliSec );            
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

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_LINE )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   const char ** line;
   const unsigned int * lens;
   PHB_ITEM ret, temp;
   USHORT i;
   sqlo_stmt_handle_t stmtParamRes;

   ret  = hb_itemNew( NULL );

   if( session )
   {
      stmtParamRes = session->stmtParamRes != -1 ? session->stmtParamRes : session->stmt;
      line = sqlo_values(stmtParamRes, NULL, 0);
      lens = sqlo_value_lens(stmtParamRes, NULL);
      hb_arrayNew( ret, session->numcols );

      for( i=0; i < session->numcols; i++ )
      {
         temp = hb_itemNew( NULL );
         hb_arraySetForward( ret, i+1 , hb_itemPutCL( temp, (char * ) line[i], lens[i] ) );
         hb_itemRelease( temp );
      }
   }
   hb_itemReturnForward(ret);
   hb_itemRelease( ret );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO_LINEPROCESSED )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   const char ** line;
   const unsigned int * lens;
   LONG lIndex;
   PHB_ITEM temp;
   HB_SIZE i, cols;
   PHB_ITEM pFields = hb_param( 3, HB_IT_ARRAY );
   BOOL  bQueryOnly = hb_parl( 4 );
   ULONG ulSystemID = hb_parnl( 5 );
   BOOL  bTranslate = hb_parl( 6 );
   PHB_ITEM pRet    = hb_param( 7, HB_IT_ARRAY );
   sqlo_stmt_handle_t stmtParamRes;

   if( session )
   {
      stmtParamRes = session->stmtParamRes != -1 ? session->stmtParamRes : session->stmt;
      line = sqlo_values(stmtParamRes, NULL, 0);
      lens = sqlo_value_lens(stmtParamRes, NULL);

      cols = hb_arrayLen( pFields );

      for( i=0; i < cols; i++ )
      {
         lIndex  = hb_arrayGetNL( hb_arrayGetItemPtr( pFields, i+1 ), FIELD_ENUM );
         temp = hb_itemNew( NULL );

         if( lIndex != 0 )
         {
            SQLO_FieldGet( hb_arrayGetItemPtr( pFields, i+1 ), temp, (char * ) line[lIndex-1], lens[lIndex-1], bQueryOnly, ulSystemID, bTranslate );
         }
         hb_arraySetForward( pRet, i+1 , temp );
         hb_itemRelease( temp );
      }
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( ORACLEWRITEMEMO )
{
   POCI_SESSION session  = ( POCI_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   const char * sTable = hb_parc( 2 );
   ULONG ulRecno = hb_parnl( 3 ) ;
   const char * sRecnoName = hb_parcx( 4 );
   sqlo_lob_desc_t loblp;
   sqlo_stmt_handle_t sth;
   int status;

   PHB_ITEM pArray = hb_param( 5, HB_IT_ARRAY );

   HB_SIZE uiLen, uiSize;

   uiLen = hb_arrayLen( pArray );

   if (( !session ) || uiLen == 0 )
   {
      hb_retni( 0 );
      return;
   }
   else
   {
      for( uiSize = 0; uiSize < uiLen; uiSize++ )
      {
         PHB_ITEM pFieldDesc = hb_arrayGetItemPtr( pArray, uiSize + 1 );
         char szSql[256] = {0};
         const char * sMemo  = hb_arrayGetCPtr( pFieldDesc, 2 );
         const char * sField = hb_arrayGetCPtr( pFieldDesc, 1 );
         sprintf( szSql, "UPDATE %s SET %s = EMPTY_CLOB() WHERE %s = %lu RETURNING %s INTO :b1", sTable, sField, sRecnoName, ulRecno, sField );

         sth = sqlo_prepare( session->dbh, szSql );
         sqlo_alloc_lob_desc( session->dbh, &loblp );
         sqlo_bind_by_pos(sth, 1, SQLOT_CLOB, &loblp, 0, NULL, 0);
         status = sqlo_execute(sth, 1);

         if (SQLO_SUCCESS != status)
         {
            sqlo_free_lob_desc(session->dbh, &loblp);
	         sqlo_close(sth);
	         hb_retni( -1 );
            return;
         }

         status = sqlo_lob_write_buffer(session->dbh, loblp, strlen(sMemo), sMemo, strlen(sMemo), SQLO_ONE_PIECE );

         if (status < 0)
         {
            sqlo_free_lob_desc(session->dbh, &loblp);
	         sqlo_close(sth);
	         hb_retni( -2 );
            return;
         }

         sqlo_free_lob_desc( session->dbh, &loblp );
         sqlo_close(sth);
      }
      hb_retni(0) ;
   }
}

/* Oracle Bind utility functions for usage with stored procedures with out parameters */
void OracleFreeLink( int num_recs, POCI_SESSION p )
{
   int  i;

   if ( p->pLink )
   {

      for ( i =0 ; i < num_recs; i++ )
      {
         if ( p->pLink[ i ].col_name )
         {
            hb_xfree( p->pLink[ i ].col_name );
         }

      }

      hb_xfree( p->pLink );
      p->pLink = NULL;
      p->ubBindNum = 0;
   }

}


/*
Bind an Parameter to Oracle stord procedure
usage
ORACLEINBINDPARAM(hDbc,nParamnum,nParamType,iFieldSize,iFieldDec,xData)
   OracleinBindParam( oSql:hdbc, 1, 2, 12, 0, 8 )
type 6->refcursor
type 2 ->double
type 4 -> integer
type 8 -> date
type 9 -> datetime
type 3 -> bool
type any->char
*/
HB_FUNC( ORACLEINBINDPARAM )
{

   POCI_SESSION Stmt= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int iParamNum  = hb_parni(2);
   int iParamType = hb_parni(3);
   int iFieldSize = hb_parni(4);
   int iPos = iParamNum-1;
   int ret = SQL_ERROR;
   BOOL lStmt = ISLOG( 7 ) ? hb_parl( 7 ) : 0;
   BOOL isNull = ISLOG( 8 ) ? hb_parl( 8 ) : 0;

  


   if ( Stmt)
   {

   
      Stmt->pLink[iPos].sVal = isNull ? -1 : 0;
      Stmt->pLink[iPos].iType = iParamType;
      switch (Stmt->pLink[iPos].iType)
      {
      case 6 :
      {
         ret =  sqlo_bind_ref_cursor(Stmt->stmtParam, ":c1", &Stmt->stmtParamRes);
      }
         break;	 
      case  4 :
      {
         if (ISNUM( 6 ) )
            Stmt->pLink[ iPos ].dValue = hb_parnd( 6 );

      ret = sqlo_bind_by_pos( lStmt ? Stmt->stmt : Stmt->stmtParam,
                          iParamNum,
                          SQLOT_FLT,
                          &Stmt->pLink[ iPos ].dValue,
                          sizeof(double),
                          &Stmt->pLink[iPos].sVal,
                          0);
      }
                          break;
     case 2 :
     {
         if (ISNUM( 6 ) )
            Stmt->pLink[ iPos ].ulValue = hb_parnl( 6 );

         ret = sqlo_bind_by_pos( lStmt ? Stmt->stmt :Stmt->stmtParam,
                          iParamNum,
                          SQLOT_INT,
                          &Stmt->pLink[ iPos ].ulValue,
                          sizeof(ULONG),
                          &Stmt->pLink[iPos].sVal,
                          0);
     }
      break;
     case 3 :
     {
         if (ISNUM( 6 ) )
            Stmt->pLink[ iPos ].iValue = hb_parl( 6 );

         ret = sqlo_bind_by_pos( lStmt ? Stmt->stmt :Stmt->stmtParam,
                          iParamNum,
                          SQLOT_INT,
                          &Stmt->pLink[ iPos ].iValue,
                          sizeof(ULONG),
                          &Stmt->pLink[iPos].sVal,
                          0);
     }
      break;
      
     case 8 :
     {
#ifdef __XHARBOUR__	     
	     
          if ( ISDATE( 6 ) )
#else
          if ( HB_ISDATE( 6 ) )
#endif          
      {
	     int iYear, iMonth, iDay; 
	     PHB_ITEM pFieldData = hb_param(6,HB_IT_DATE);
         hb_dateDecode( hb_itemGetDL( pFieldData ), &iYear, &iMonth, &iDay );
//         hb_dateStrPut( Stmt->pLink[ iPos ].sDate, iYear, iMonth, iDay );
         Stmt->pLink[ iPos ].sDate[0]= (char)(iYear / 100) + 100; // century
         Stmt->pLink[ iPos ].sDate[1]= (char)(iYear % 100) + 100; // year
         Stmt->pLink[ iPos ].sDate[2]= (char)iMonth;
         Stmt->pLink[ iPos ].sDate[3]= (char)iDay;
         Stmt->pLink[ iPos ].sDate[4]= 1;
         Stmt->pLink[ iPos ].sDate[5]= 1;
         Stmt->pLink[ iPos ].sDate[6]= 1;         
      }    

         ret = sqlo_bind_by_pos( lStmt ? Stmt->stmt :Stmt->stmtParam,
                               iParamNum,
                          SQLOT_DAT,
                          &Stmt->pLink[ iPos ].sDate,
                          sizeof(Stmt->pLink[ iPos ].sDate),
                          &Stmt->pLink[iPos].sVal,
                          0);
     }
      break;

      case 9 :
     {
#ifdef __XHARBOUR__	     
          if ( ISDATETIME( 6 ) )
#else
          if ( HB_ISDATETIME( 6 ) )
#endif          
      {
	     int iYear, iMonth, iDay; 
         int  iHour,  iMin;
         #ifdef __XHARBOUR__
         double  dSec;
         #else
         int mSec;
         int iSeconds;
         #endif
	     PHB_ITEM pFieldData = hb_param(6,HB_IT_DATETIME);
	     #ifdef __XHARBOUR__
         hb_dateDecode( hb_itemGetDL( pFieldData ), &iYear, &iMonth, &iDay );
         hb_timeDecode( hb_itemGetT(  pFieldData) , &iHour, &iMin, &dSec );         
         #else
         long  plJulian;
         long  plMilliSec ;
         hb_itemGetTDT(pFieldData,&plJulian, &plMilliSec );
         hb_dateDecode( plJulian, &iYear, &iMonth, &iDay );
         hb_timeDecode( plMilliSec , &iHour, &iMin, &iSeconds, &mSec );         
         
         #endif
//         hb_dateStrPut( Stmt->pLink[ iPos ].sDate, iYear, iMonth, iDay );
         Stmt->pLink[ iPos ].sDate[0]= (char)(iYear / 100) + 100; // century
         Stmt->pLink[ iPos ].sDate[1]= (char)(iYear % 100) + 100; // year
         Stmt->pLink[ iPos ].sDate[2]= (char)iMonth;
         Stmt->pLink[ iPos ].sDate[3]= (char)iDay;
         Stmt->pLink[ iPos ].sDate[4]= (char)iHour+1;
         Stmt->pLink[ iPos ].sDate[5]= (char)iMin+1;
	     #ifdef __XHARBOUR__         
         Stmt->pLink[ iPos ].sDate[6]= (char)dSec+1;         
         #else
         Stmt->pLink[ iPos ].sDate[6]= (char)iSeconds+1;         
         #endif
      }    

         ret = sqlo_bind_by_pos( lStmt ? Stmt->stmt :Stmt->stmtParam,
                               iParamNum,
                          SQLOT_DAT,
                          &Stmt->pLink[ iPos ].sDate,
                          sizeof(Stmt->pLink[ iPos ].sDate),
                          &Stmt->pLink[iPos].sVal,
                          0);
     }
      break;
     case 1:
     {
	     
        if ( ISCHAR( 6 ) )
        {
           sprintf(Stmt->pLink[ iPos ].sValue,hb_parcx(6),hb_parclen( 6 ));          
        }
         ret = sqlo_bind_by_pos( lStmt ? Stmt->stmt : Stmt->stmtParam,
                           iParamNum,
                           SQLOT_AFC ,
                           &Stmt->pLink[ iPos ].sValue,
                           iFieldSize ,
                           &Stmt->pLink[iPos].sVal,
                          0);
	     
     } 
     break;
           

      default :
      {
      if (iFieldSize  == 0 )
      {
         iFieldSize = 1;
      }
         Stmt->pLink[iPos].col_name = (char *) hb_xgrab( sizeof(char) * (iFieldSize + 1));
         memset(Stmt->pLink[iPos].col_name,'\0',(iFieldSize+1) * sizeof(char));

      
      if ( ISCHAR( 6 ) )
      {
         hb_xmemcpy(Stmt->pLink[ iPos ].col_name, hb_parc( 6 ),hb_parclen( 6 ) ) ;
      }
      
      
      ret = sqlo_bind_by_pos( lStmt ? Stmt->stmt :Stmt->stmtParam,
                          iParamNum,
                          SQLOT_STR,
                          Stmt->pLink[ iPos ].col_name,
                          iFieldSize+1,
                          &Stmt->pLink[iPos].sVal,
                          0);
     }                     
     break;
     }


   }

   hb_retni( ret );
}
/*
 Get the content of an binded parameter returned from stored procedure
usage : ORACLEGETBINDDATA(hDbc,nParameterNumber)
*/
HB_FUNC( ORACLEGETBINDDATA)
{

   POCI_SESSION p = (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int iPos;

   PHB_ITEM p1 = hb_param( 2, HB_IT_ANY );

   if ( HB_IS_NUMBER( p1 ) &&   p )
   {

      iPos = hb_itemGetNI( p1 );
      if( p->pLink[ iPos - 1 ].iType == 4 )
      {
         hb_retnd(p->pLink[ iPos - 1 ].dValue);
      }
      else if( p->pLink[ iPos - 1 ].iType == 2)
      {
         hb_retnint(p->pLink[ iPos - 1 ].ulValue);
      }
      else if ( p->pLink[ iPos - 1 ].iType == 8 ||   p->pLink[ iPos - 1 ].iType == 9)
      {
          int century = p->pLink[ iPos - 1 ].sDate[0];
          if(century >= 100)
          {
             int year    = (unsigned char)(p->pLink[ iPos - 1 ].sDate[1]);
             int month;
             int day  ;
             int hour ;
             int min  ;
             int sec  ;
             year = ((century-100)*100) + (year-100);
             month = p->pLink[ iPos - 1 ].sDate[2];
             day   = p->pLink[ iPos - 1 ].sDate[3];
             hour  = p->pLink[ iPos - 1 ].sDate[4] - 1;
             min   = p->pLink[ iPos - 1 ].sDate[5] - 1;
             sec   = p->pLink[ iPos - 1 ].sDate[6] - 1;
             if ( p->pLink[ iPos - 1 ].iType == 8) 
               hb_retd( year, month,day ); /* returns a date */
            else
            #ifdef __XHARBOUR__            
               hb_retdt( year, month,day, hour, min, sec,0 ); /* returns a datetime */
            #else
               hb_rettd ( hb_timeStampPack(year, month,day, hour, min, sec,0));
            #endif   
             
         }
                                 
      }
      else  if( p->pLink[ iPos - 1 ].iType == 3)
      {
	      hb_retl(p->pLink[ iPos - 1 ].iValue);
      }
      else
      {
         hb_retc( p->pLink[ iPos - 1 ].col_name );
      }
      return ;

   }
   hb_retc("");
}

/*
Free all Binded Parameters memory allocated
usage
ORACLEFREEBIND(nOrahandle)
*/

HB_FUNC(ORACLEFREEBIND)
{
   POCI_SESSION Stmt= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   if ( Stmt->pLink )
   {
      OracleFreeLink( Stmt->ubBindNum, Stmt );
   }
}

/*
Prepare an stored procedure for execution
usage ORACLEPREPARE(nOracleHandle,cSql) -> nPreparedHandle)
*/
HB_FUNC(ORACLEPREPARE)
{
   POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   const char * szSql = hb_parc( 2 ) ;
   BOOL lStmt = ISLOG( 3 ) ? hb_parl( 3 ) : 0;

   if ( session )
   {
	  if ( lStmt ) 
	  {
	     session->stmt  = sqlo_prepare( session->dbh, szSql );
      }
	  else
	  {
      session->stmtParam = sqlo_prepare( session->dbh, szSql );
      }   
      hb_retni( 1 );
      return;
   }

   hb_retni( -1 );

}

/*
Execute the  Stored Procedure
usage
ORACLEEXECDIR(nOraHandle[,nPreparedHandle]) ->nStatus
*/
HB_FUNC(ORACLEEXECDIR)
{
   POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int ret = SQL_ERROR ;
   if ( session )
   {
      ret = sqlo_execute( session->stmtParam , 1 )     ;
      session->status = sqlo_close( session->stmtParam );
   }
   hb_retni( ret );

}

HB_FUNC( ORACLE_PROCCURSOR )
{ 
  POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
  sqlo_stmt_handle_t sth = SQLO_STH_INIT;
  sqlo_stmt_handle_t st2h = SQLO_STH_INIT;                     /* handle of the ref cursor */

  int ret = SQL_ERROR ;
   
  const char * stmt = hb_parc( 2 );
  const char * parc = hb_parc( 3 );
  
  if ( session )
   { 
   /* parse the statement */
   ret = sqlo_prepare(session->dbh, stmt);
     
   if (ret >= SQLO_SUCCESS )
      {       
       if ( 0 <= ( sth = ret )) 
       {
        /* bind all variables */
         if (SQLO_SUCCESS != sqlo_bind_ref_cursor(sth, parc, &st2h) )
           {            
           hb_retni( SQL_ERROR );
           return ;
           }
        }   
//       session->stmt         = stmt;
      session->stmtParam    = sth;
      session->stmtParamRes = st2h;
      
      /* execute the PL/SQL block */
      if (SQLO_SUCCESS != sqlo_execute(sth, 1)) {
         hb_retni( SQL_ERROR );          
         return;
      }
       /* execute the refcursor */
       if (SQLO_SUCCESS != sqlo_execute(st2h, 1)) {         
         hb_retni( SQL_ERROR );
         return ;
       }
          
      session->numcols = sqlo_ncols( st2h, 0 );        
      
     } else {
         hb_retni( SQL_ERROR );
      return ; }
  } 
   
  hb_retni( ret );
  //sqlo_close( session->stmt );   
  //sqlo_close( session->stmtParamRes ); 
  //
}       
HB_FUNC( ORACLE_SAVE_HANDLE_ST )     
{
  POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
  if (session){
  hb_retptr( (void * )session->stmtParam );
  }
}

HB_FUNC( ORACLE_CLOSE_FCURSOR )
{ 
   POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
      
   if (SQLO_SUCCESS != sqlo_close(session->stmtParamRes)) {
       session->stmtParamRes= SQLO_STH_INIT;
       hb_retni( SQL_ERROR );
       return; }
 
 /* if (SQLO_SUCCESS != sqlo_close(session->stmtParam)) {
     hb_retni( SQL_ERROR );
     return; }
 */
   
   hb_retni( SQLO_SUCCESS );
   
}

HB_FUNC( ORACLE_BIND_BY_NAME ) 
{
   POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );    
   int iPos = hb_parni( 2 );

   hb_retni( sqlo_bind_by_name( session->stmtParam,
                          ":c1",
                          SQLOT_FLT,
                          &session->pLink[ iPos ].dValue,
                          sizeof(session->pLink[ iPos ].dValue),
                          0,
                          0) );
}
 
HB_FUNC(ORACLEEXECDIRCURSOR)
{
   POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int ret = SQL_ERROR ;
   if ( session )
   {
      ret = sqlo_execute( session->stmtParam , 1 );
      if (ret == SQLO_SUCCESS )
      {
            ret = sqlo_execute( session->stmtParamRes , 1 );    
            session->numcols = sqlo_ncols( session->stmtParamRes, 0 );
            //session->status = sqlo_close( session->stmtParam );
            //ret = sqlo_execute( session->stmtParamRes , 1 );    
            //session->numcols = sqlo_ncols( session->stmtParam, 0 );

      }
      hb_retni( ret );
   }
}   
 

/*
Prepare the necessary data for Binded Parameters
usage
ORACLEBINDALLOC(noraHandle,nNumberofParameters)
*/
HB_FUNC( ORACLEBINDALLOC )
{
   POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int iBind ;

   if ( session )
   {
      iBind = hb_parni( 2 ) ;
//       session->pLink = ( ORA_BIND_COLS * ) hb_xgrab( sizeof( ORA_BIND_COLS ) * iBind );
//       memset(session->pLink,0,sizeof( ORA_BIND_COLS ) * iBind );      
      session->pLink = (ORA_BIND_COLS * ) hb_xgrabz( sizeof(ORA_BIND_COLS ) * iBind  ); 
      session->ubBindNum = iBind;
   }
   hb_retni( 1 );
}


HB_FUNC( ORACLE_BINDCURSOR )
{ 
  POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
  sqlo_stmt_handle_t sth = SQLO_STH_INIT;
  sqlo_stmt_handle_t st2h = SQLO_STH_INIT;                     /* handle of the ref cursor */

  int ret = SQL_ERROR ;
   
  const char * stmt = hb_parc( 2 );
  const char * parc = hb_parc( 3 );
  
  if ( session )
   { 
   /* parse the statement */
   ret = sqlo_prepare(session->dbh,stmt );
   if (ret == SQLO_SUCCESS )
      {
       if ( 0 <= (sth = ret )) 
       {
        /* bind all variables */
         if (SQLO_SUCCESS != sqlo_bind_ref_cursor(sth, parc, &st2h) )
           {
           hb_retni( SQL_ERROR );
           return ;
           }
        }   

//       session->stmt         = stmt;
      session->stmtParam    = sth;
      session->stmtParamRes = st2h;

     }
    }
    hb_retni( ret );
}   
 
HB_FUNC( ORACLE_EXECCURSOR )      
{
  POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
  int ret = 1 ; 
   /* execute the PL/SQL block */
   if (SQLO_SUCCESS != sqlo_execute(session->stmtParam, 1)) {

      hb_retni( SQL_ERROR );
      return;
   }
   /* execute the refcursor */
   if (SQLO_SUCCESS != sqlo_execute(session->stmtParamRes, 1)) {

     hb_retni( SQL_ERROR );
     return ;
   }

   session->numcols = sqlo_ncols( session->stmtParamRes, 0 );               
   

   hb_retni( ret );
}
HB_FUNC( CLOSECURSOR )
{  
  POCI_SESSION session= (POCI_SESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) ); 

   if( session ) 
   { 
   	  //sqlo_close( session->stmt );
   	  //sqlo_close( session->stmtParam );   
// culik fecha primeiro o pai, apos fecha o cursor
      session->status = sqlo_close( session->stmtParam );
      session->status = sqlo_close( session->stmtParamRes ); 
      hb_retni( session->status ); 
   } 
   hb_retni( SQL_SUCCESS );
}  


HB_FUNC( GETAFFECTROWS) 
{
   OCI_SESSION * session  = ( OCI_SESSION* ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   if ( session ) 
      hb_retnl( session->uRows ) ;
   else
      hb_retnl(0);                      
}



HB_FUNC( GETORAHANDLE)
{
   OCI_SESSION* p  = ( OCI_SESSION* ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   if ( p )
      hb_retni(p->stmt);
}

HB_FUNC( SETORAHANDLE)
{
   OCI_SESSION* p  = ( OCI_SESSION* ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if ( p )
   {
      p->stmt = hb_parni(2);
   }
}
