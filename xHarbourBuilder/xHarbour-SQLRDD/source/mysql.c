/* $CATEGORY$SQLRDD/HIDE$FILES$HIDE$
* SQLRDD Mysql native connection
* Copyright (c) 2006 - Marcelo Lombardo <lombardo@uol.com.br>
* All Rights Reserved
*/

/* this is workaround for problems with xHarbour core header files which
 * define _WINSOCKAPI_ what effectively breaks compilation of code using
 * sockets. It means that we have to include windows.h before xHarbour
 * header files.
 */
#if defined( WINNT ) || defined( _Windows ) || defined( __NT__ ) || defined( _WIN32 ) || \
    defined( _WINDOWS_ ) || defined( __WINDOWS_386__ ) || defined( __WIN32__ )
   #include <windows.h>
#endif

#include "compat.h"

#include "sqlrddsetup.ch"
#include "sqlprototypes.h"

#include "mysql.ch"
#include "mysql.h"
#include "mysqld_error.h"
#include "errmsg.h"
#include "sqlodbc.ch"

#include <assert.h>

#define MYSQL_OK     0

#define CLIENT_ALL_FLAGS        (CLIENT_COMPRESS | CLIENT_MULTI_RESULTS | CLIENT_MULTI_STATEMENTS)
#define CLIENT_ALL_FLAGS2        (CLIENT_MULTI_RESULTS | CLIENT_MULTI_STATEMENTS)
static PHB_DYNS s_pSym_SR_DESERIALIZE = NULL;
static PHB_DYNS s_pSym_SR_FROMJSON = NULL;
static int iConnectionCount = 0;
#define LOGFILE               "mysql.log"
typedef struct _MYSQL_SESSION
{
   int status;                   // Execution return value
   int numcols;                  // Result set columns
   int ifetch;                   // Fetch position in result set
   MYSQL * dbh;                  // Connection handler
   MYSQL_RES * stmt;             // Current statement handler
   ULONGLONG ulAffected_rows;    // Number of affected rows
} MYSQL_SESSION;

typedef MYSQL_SESSION * PMYSQL_SESSION;

HB_FUNC( MYSCONNECT )
{
//    PMYSQL_SESSION session = (PMYSQL_SESSION) hb_xgrab( sizeof( MYSQL_SESSION ) );
   PMYSQL_SESSION session  = (PMYSQL_SESSION) hb_xgrabz( sizeof( MYSQL_SESSION ) );
   const char *szHost=hb_parc(1);
   const char *szUser=hb_parc(2);
   const char *szPass=hb_parc(3);
   const char *szDb = hb_parc(4);
   UINT uiPort    = ISNUM(5) ? hb_parnl(5) : MYSQL_PORT ;
   UINT uiTimeout = ISNUM(7) ? hb_parnl(7) : 3600 ;
   BOOL lCompress = ISLOG(8) ?  hb_parl(8) : 0 ;
   mysql_library_init(0,NULL,NULL);
//    memset( session, 0, sizeof( MYSQL_SESSION ) );

   session->dbh    = mysql_init( ( MYSQL * ) 0 );
   session->ifetch = -2;

   if (  session->dbh != NULL )
   {
	  iConnectionCount ++ ; 
      mysql_options( session->dbh, MYSQL_OPT_CONNECT_TIMEOUT, (const char *) &uiTimeout );
      if (lCompress)
         mysql_real_connect( session->dbh, szHost, szUser, szPass, szDb, uiPort, NULL, CLIENT_ALL_FLAGS );
      else
         mysql_real_connect( session->dbh, szHost, szUser, szPass, szDb, uiPort, NULL, CLIENT_ALL_FLAGS2 );
      hb_retptr( (void *) session );
   }
   else
   {
      
      mysql_close( NULL );
      if ( iConnectionCount == 0 ) 
         mysql_library_end();

      hb_retptr( NULL );
   }
}

HB_FUNC( MYSFINISH )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session != NULL );
   assert( session->dbh != NULL );
   mysql_close( session->dbh );

   hb_xfree( session );
   if (iConnectionCount >  0)
      iConnectionCount -- ;    
   if ( iConnectionCount == 0 ) 
      mysql_library_end();
   hb_ret();
}

HB_FUNC( MYSGETCONNID )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   ULONG ulThreadID;

   assert( session != NULL );
   assert( session->dbh != NULL );
   ulThreadID = mysql_thread_id( session->dbh );
   hb_retnl( ulThreadID );
}

HB_FUNC( MYSKILLCONNID )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   ULONG ulThreadID = (ULONG) hb_itemGetNL( hb_param( 2, HB_IT_LONG ) );

   assert( session != NULL );
   assert( session->dbh != NULL );
   hb_retni( mysql_kill( session->dbh, ulThreadID ) );
}

HB_FUNC( MYSEXEC )
{
   /* TraceLog( NULL, "mysqlExec : %s\n", hb_parc(2) ); */
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   const char * szQuery = hb_parc(2);

   assert( session != NULL );
   assert( session->dbh != NULL );
   session->ulAffected_rows = 0;
   //mysql_query( session->dbh, szQuery );
   mysql_real_query( session->dbh, szQuery, hb_parclen( 2 ) );
   session->stmt = mysql_store_result( session->dbh );
   session->ulAffected_rows = mysql_affected_rows(session->dbh) ;
   if ( session->stmt )
   {
	   session->numcols = mysql_num_fields( session->stmt );
   }
   else
   {
	   session->numcols = 0;
   }
   hb_retptr( (void *) session->stmt );
   session->ifetch = -1;
}

HB_FUNC( MYSFETCH )     /* MYSFetch( ConnHandle,ResultSet ) => nStatus */
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int rows;

   assert( session != NULL );
   assert( session->dbh != NULL );
   assert( session->stmt != NULL );

   session->status =  mysql_errno( session->dbh ) ;

   if ( session->status != MYSQL_OK )
   {
      hb_retni( SQL_INVALID_HANDLE );
   }
   else
   {
      if ( session->ifetch >= -1 )
      {
         session->ifetch++;
         rows = (int) ( mysql_num_rows( session->stmt ) -1 );

         if ( session->ifetch > rows )
         {
            hb_retni( SQL_NO_DATA_FOUND );
         }
         else
         {
            hb_retni( SQL_SUCCESS );
         }
      }
      else
      {
         hb_retni( SQL_INVALID_HANDLE );
      }
   }
}

//-----------------------------------------------------------------------------//

void MSQLFieldGet( PHB_ITEM pField, PHB_ITEM pItem, char * bBuffer, HB_SIZE lLenBuff, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate )
{
   LONG lType;
   HB_SIZE lLen, lDec;
   PHB_ITEM pTemp;

   HB_SYMBOL_UNUSED( bQueryOnly );
   HB_SYMBOL_UNUSED( ulSystemID );

   lType = ( LONG ) hb_arrayGetNL( pField, FIELD_DOMAIN );
   lLen  = hb_arrayGetNL( pField, FIELD_LEN );
   lDec  = hb_arrayGetNL( pField, FIELD_DEC );

   if( lLenBuff <= 0 )     // database content is NULL
   {
      switch( lType )
      {
         case SQL_CHAR:
         {
            char * szResult = ( char * ) hb_xgrab( lLen + 1 );
            hb_xmemset( szResult, ' ', lLen );
            szResult[ lLen ] =  '\0';
            hb_itemPutCLPtr( pItem, szResult, lLen );
            break;
         }
         case SQL_NUMERIC:
         case SQL_FAKE_NUM:
         {
            char szResult[2] = { ' ', '\0' };
            sr_escapeNumber( szResult, lLen, lDec, pItem );
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
         case SQL_TIME:
         {
	         hb_itemPutTDT( pItem, 0, 0 );
	         break;
         }         

         default:
            TraceLog( LOGFILE, "Invalid data type detected: %i\n", lType );
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
            memset( szResult, ' ',   lLen  );
            hb_xmemcpy( szResult, bBuffer,  (lLen < lLenBuff ? lLen : lLenBuff ) );

            for( lPos =  lLenBuff; lPos < lLen; lPos++ )
            {
               szResult[ lPos ] = ' ';
            }
            szResult[ lLen ] =  '\0';
            hb_itemPutCLPtr( pItem, szResult, lLen );
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
            dt[4] = bBuffer[5];
            dt[5] = bBuffer[6];
            dt[6] = bBuffer[8];
            dt[7] = bBuffer[9];
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
         case SQL_TIME:
         {
	        long  lMilliSec;
            lMilliSec = hb_timeEncStr( bBuffer );         
            hb_itemPutTDT( pItem, 0, lMilliSec );    
            break;
         }

         default:
            TraceLog( LOGFILE, "Invalid data type detected: %i\n", lType );
      }
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( MYSLINEPROCESSED )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int col, cols;
   PHB_ITEM temp;
   MYSQL_ROW thisrow;
   ULONG * lens;
   LONG lIndex;

   PHB_ITEM pFields = hb_param( 3, HB_IT_ARRAY );
   BOOL  bQueryOnly = hb_parl( 4 );
   ULONG ulSystemID = hb_parnl( 5 );
   BOOL  bTranslate = hb_parl( 6 );
   PHB_ITEM pRet    = hb_param( 7, HB_IT_ARRAY );

   assert( session != NULL );
   assert( session->dbh != NULL );
   assert( session->stmt != NULL );

   session->status =  mysql_errno( session->dbh ) ;

   if ( session->status != MYSQL_OK )
   {
      hb_retni( SQL_INVALID_HANDLE );
   }
   else
   {
      if ( session->ifetch >= -1 )
      {
         cols = hb_arrayLen( pFields );

         mysql_data_seek( session->stmt, session->ifetch );
         thisrow = mysql_fetch_row( session->stmt );
         lens    = mysql_fetch_lengths( session->stmt );

         for( col = 0; col < cols; col++ )
         {
            temp    = hb_itemNew( NULL );
            lIndex  = hb_arrayGetNL( hb_arrayGetItemPtr( pFields, col+1 ), FIELD_ENUM );

            if( lIndex != 0 )
            {
               if( thisrow[lIndex-1] )
               {
                  MSQLFieldGet( hb_arrayGetItemPtr( pFields, col+1 ), temp, (char * ) thisrow[lIndex-1], lens[lIndex-1], bQueryOnly, ulSystemID, bTranslate );
               }
               else
               {
                  MSQLFieldGet( hb_arrayGetItemPtr( pFields, col+1 ), temp, "", 0, bQueryOnly, ulSystemID, bTranslate );
               }
            }
            hb_arraySetForward( pRet, col+1, temp );
            hb_itemRelease( temp );
         }
         hb_retni( SQL_SUCCESS );
      }
      else
      {
         hb_retni( SQL_INVALID_HANDLE );
      }
   }
}

HB_FUNC( MYSSTATUS )
{
   int ret;
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   assert( session != NULL );
   assert( session->dbh != NULL );

   ret = mysql_errno( session->dbh );

   if (ret ==MYSQL_OK)
   {
      ret = SQL_SUCCESS;
   }
   else
   {
      ret = SQL_ERROR;
   }
   hb_retni( ret );
}

HB_FUNC( MYSRESULTSTATUS )
{
   UINT ret;
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   assert( session != NULL );
   assert( session->dbh != NULL );

   ret = (UINT) mysql_errno( session->dbh );

   switch (ret)
   {
   case MYSQL_OK:
      ret = SQL_SUCCESS;
      break;
   case CR_COMMANDS_OUT_OF_SYNC:
   case CR_UNKNOWN_ERROR :
   case CR_SERVER_GONE_ERROR:
   case CR_SERVER_LOST:
   case ER_NO_DB_ERROR:
      ret = (UINT)SQL_ERROR;
      break;
   }
   hb_retnl( (LONG) ret );
}

HB_FUNC( MYSRESSTATUS )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   assert( session != NULL );
   assert( session->dbh != NULL );
   hb_retc( (char *) mysql_error( session->dbh ) );
}

HB_FUNC( MYSCLEAR )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   assert( session != NULL );
   assert( session->dbh != NULL );

   if( session->stmt )
   {
      mysql_free_result( session->stmt );
      session->stmt   = NULL;
      session->ifetch = -2;
   }
}

HB_FUNC( MYSCOLS )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   assert( session != NULL );
   assert( session->dbh != NULL );
   hb_retni( session->numcols );
}

HB_FUNC( MYSVERS )         /* MYSVERS( hConnection ) => nVersion */
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session != NULL );
   assert( session->dbh != NULL );
   hb_retnl( (long) mysql_get_server_version( session->dbh ) );
}

HB_FUNC( MYSERRMSG )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session != NULL );
   assert( session->dbh != NULL );
   hb_retc( (char *) mysql_error( session->dbh ));
}

HB_FUNC( MYSCOMMIT )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   assert( session != NULL );
   assert( session->dbh != NULL );

   if( mysql_commit( session->dbh ) )
   {
      hb_retni( SQL_SUCCESS );
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

HB_FUNC( MYSROLLBACK )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   assert( session != NULL );
   assert( session->dbh != NULL );

   if( mysql_rollback( session->dbh ) )
   {
      hb_retni( SQL_SUCCESS );
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

HB_FUNC( MYSQUERYATTR )
{
   int row, rows, type;
   PHB_ITEM ret, temp, atemp;
   PMYSQL_SESSION session;
   MYSQL_FIELD * field;

   if (hb_pcount() != 1)
   {
      hb_retnl( -2 );
   }

   session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh != NULL );
   assert( session->stmt != NULL );

   rows  = session->numcols;
   ret   = hb_itemNew( NULL );
   temp  = hb_itemNew( NULL );
   atemp = hb_itemNew( NULL );

   hb_arrayNew( ret, rows );

   for ( row = 0; row < rows; row++ )
   {

      /* Column name */
      field = mysql_fetch_field_direct( session->stmt, row );
      hb_arrayNew( atemp, FIELD_INFO_SIZE );
      hb_arraySetForward( atemp, FIELD_NAME, hb_itemPutC( temp, hb_strupr( field->name ) ));

      /* Data type, len, dec */
      type   =  field->type;
      switch (type)
      {
      case MYSQL_STRING_TYPE:
      case MYSQL_VAR_STRING_TYPE:
      //case MYSQL_DATETIME_TYPE:
      
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "C" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, (int)field->length) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_CHAR ) );
         break;
      case MYSQL_TINY_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "L" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 1 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_BIT ) );
         break;
      case MYSQL_TINY_BLOB_TYPE:
      case MYSQL_MEDIUM_BLOB_TYPE:
      case MYSQL_LONG_BLOB_TYPE:
      case MYSQL_BLOB_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "M" ));
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 10 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_LONGVARCHAR ) );
         break;
      case MYSQL_DATE_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "D" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 8 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_DATE ) );
         break;
      case MYSQL_DATETIME_TYPE:   
      case MYSQL_TIMESTAMP_TYPE:      
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "T" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 8 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_DATETIME ) );
         break;
      case MYSQL_TIME_TYPE:   
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "T" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 4 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_TIME ) );
         break;
      
      case MYSQL_SHORT_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "N" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 6 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      case MYSQL_LONGLONG_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "N" ));
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 20 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      case MYSQL_LONG_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "N" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, HB_MIN(11,(int)field->length) ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      case MYSQL_INT24_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "N" ));
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, HB_MIN(8, (int)field->length) ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      case MYSQL_FLOAT_TYPE:
      case MYSQL_DECIMAL_TYPE:
      case MYSQL_DOUBLE_TYPE:
      case MYSQL_NEWDECIMAL_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "N" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, (int)field->length ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, field->decimals ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      default:
         TraceLog( LOGFILE, "Invalid data type in query : %i\n", type );
      }

      /* Nullable */
      hb_arraySetForward( atemp, FIELD_NULLABLE, hb_itemPutL( temp, IS_NOT_NULL(field->flags) ? 0 : 1 ) );
      /* add to main array */
      hb_arraySetForward( ret, row+1, atemp );
   }
   hb_itemRelease( atemp );
   hb_itemRelease( temp );
   hb_itemReturnForward( ret );
   hb_itemRelease( ret );
}

HB_FUNC( MYSTABLEATTR )
{
   char attcmm[256]={0};
   int row, rows, type;
   PHB_ITEM ret, atemp, temp;
   PMYSQL_SESSION session;

   MYSQL_FIELD * field;

   if (hb_pcount() != 2)
   {
      hb_retnl( -2 );
   }

   session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh != NULL );

   sprintf( attcmm, "select * from %s where 0 = 1", hb_parc(2) );

   //mysql_query( session->dbh, attcmm );
   mysql_real_query( session->dbh, attcmm, strlen(attcmm) );
   session->stmt = mysql_store_result( session->dbh );

   if ( !session->stmt )
   {
      TraceLog( LOGFILE, "Query error : %i - %s\n", mysql_errno( session->dbh ), mysql_error( session->dbh ) );
   }

   ret   = hb_itemNew( NULL );
   temp  = hb_itemNew( NULL );
   atemp = hb_itemNew( NULL );

   rows = mysql_num_fields( session->stmt );
   hb_arrayNew( ret, rows );

   for ( row = 0; row < rows; row++ )
   {
     field = mysql_fetch_field_direct( session->stmt, row );
      /* Column name */
      hb_arrayNew( atemp, 6 );
      hb_itemPutC( temp, hb_strupr( field->name ) );
      hb_arraySetForward( atemp, 1, temp );

      /* Data type, len, dec */
      type   =  field->type;

      switch (type)
      {
      case MYSQL_STRING_TYPE:
      case MYSQL_VAR_STRING_TYPE:
      //case MYSQL_DATETIME_TYPE:      
         hb_itemPutC( temp, "C" );
         hb_arraySetForward( atemp, FIELD_TYPE, temp );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, (int)field->length) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_CHAR ) );
         break;
      case MYSQL_TINY_TYPE:
         hb_itemPutC( temp, "L" );
         hb_arraySetForward( atemp, FIELD_TYPE, temp );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 1 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_BIT ) );
         break;
      case MYSQL_TINY_BLOB_TYPE:
      case MYSQL_MEDIUM_BLOB_TYPE:
      case MYSQL_LONG_BLOB_TYPE:
      case MYSQL_BLOB_TYPE:
         hb_itemPutC( temp, "M" );
         hb_arraySetForward( atemp, FIELD_TYPE, temp );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 10 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_LONGVARCHAR ) );
         break;
      case MYSQL_DATE_TYPE:
         hb_itemPutC( temp, "D" );
         hb_arraySetForward( atemp, FIELD_TYPE, temp );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 8 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_DATE ) );
         break;
      case MYSQL_DATETIME_TYPE:  
      case MYSQL_TIMESTAMP_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "T" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 8 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_DATETIME ) );
         break;
      case MYSQL_TIME_TYPE:   
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "T" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 4 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_TIME ) );
         break;
         
      case MYSQL_SHORT_TYPE:
         hb_itemPutC( temp, "N" );
         hb_arraySetForward( atemp, FIELD_TYPE, temp );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 6 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      case MYSQL_LONGLONG_TYPE:
         hb_itemPutC( temp, "N" );
         hb_arraySetForward( atemp, FIELD_TYPE, temp );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 20 ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      case MYSQL_LONG_TYPE:
         hb_itemPutC( temp, "N" );
         hb_arraySetForward( atemp, FIELD_TYPE, temp );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, HB_MIN(11,(int)field->length)  ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      case MYSQL_INT24_TYPE:
         hb_itemPutC( temp, "N" );
         hb_arraySetForward( atemp, FIELD_TYPE, temp );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, HB_MIN(8,(int)field->length) ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      case MYSQL_FLOAT_TYPE:
      case MYSQL_DECIMAL_TYPE:
      case MYSQL_DOUBLE_TYPE:
         hb_arraySetForward( atemp, FIELD_TYPE, hb_itemPutC( temp, "N" ) );
         hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, (int)field->length ) );
         hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, field->decimals ) );
         hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
         break;
      }

      /* Nullable */
      hb_arraySetForward( atemp, FIELD_NULLABLE, hb_itemPutL( temp, ( IS_NOT_NULL(field->flags) ? 0 : 1 ) ));
      /* add to main array */
      hb_arraySetForward( ret, row+1, atemp );
   }
   hb_itemRelease( atemp );
   hb_itemRelease( temp );
   hb_itemReturnForward( ret );
   hb_itemRelease( ret );
   mysql_free_result( session->stmt );
   session->stmt = NULL;
}



HB_FUNC( MYSAFFECTEDROWS )
{
   PMYSQL_SESSION session  = ( PMYSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   if( session )
   {
      hb_retnll( session->ulAffected_rows) ;
      return;
   }   
   hb_retni( 0) ;
}
//-----------------------------------------------------------------------------//
