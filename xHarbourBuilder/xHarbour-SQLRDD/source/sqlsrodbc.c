/* $CATEGORY$SQLRDD/ODBC$FILES$sql.lib$
 * $Id$
 */

#include "compat.h"

#include "sqlrddsetup.ch"
#include "sqlprototypes.h"

#if !defined(HB_OS_DOS) && !defined(HB_OS_OS2)

#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
   #include <windows.h>
   #include <odbcinst.h>
#else
   #define SQL_WCHAR  (-8)
   #define SQL_WLONGVARCHAR  (-10)
   #define SQL_C_WCHAR  SQL_WCHAR
#endif

#include <math.h>
#include <ctype.h>
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>
#include <assert.h>
#if !defined( HB_OS_WIN )
#  if !defined( SQLLEN ) && !defined( SQLTCHAR )
      typedef unsigned char   SQLTCHAR;
#  endif
#endif

#define SQL_NVARCHAR                        -9
#define SQL_DB2_CLOB                        -99
#define SQL_FAKE_LOB                        -100
#define SQL_FAKE_DATE                       -101
#define SQL_FAKE_NUM                        -102

#define AINFO_BOF                      1
#define AINFO_EOF                      2
#define AINFO_BOF_AT                  13
#define AINFO_EOF_AT                  14
#define ORD_DIR_FWD                    1
#define ORD_DIR_BWD                    2

#define AINFO_NPOSCACHE               20
#define SYSTEMID_ORACLE                1

#ifndef HB_OS_WIN_32
   #ifndef SQL_GUID
      #define SQL_GUID                            -11
   #endif
#endif
#if !defined( HB_OS_WIN ) && !defined( SQL_GUID)
#define SQL_GUID                            -11
#endif

#define LOGFILE               "odbc.log"
static PHB_DYNS s_pSym_SR_DESERIALIZE = NULL;
static PHB_DYNS s_pSym_SR_FROMJSON = NULL;
void odbcErrorDiagRTE( SQLHSTMT hStmt, char * routine, char * szSql, SQLRETURN res, int line, char * module );
void odbcGetData( SQLHSTMT hStmt, PHB_ITEM pField,PHB_ITEM pItem,  BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate,USHORT ui  );
//-----------------------------------------------------------------------------//

#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )

HB_FUNC( SR_INSTALLERROR )
{
	WORD  iErr;
	DWORD	pfErrorCode;
	char	lpszErrorMsg[301]={0};
	char  lpszRetErrorMsg[350]={0};
	WORD	cbErrorMsgMax = 300;
	WORD	pcbErrorMsg;
	int		rc;

   if (hb_pcount() != 1)
   {
      iErr = 1;
   }
   else
   {
      iErr = (WORD) hb_parni(1);
   }

   lpszErrorMsg[0] = '\0';
   rc = SQLInstallerError( iErr, &pfErrorCode, lpszErrorMsg, cbErrorMsgMax, &pcbErrorMsg );
   if( rc == SQL_SUCCESS || rc == SQL_SUCCESS_WITH_INFO )
   {
      sprintf( lpszRetErrorMsg, "ErrCode %i - %s", (int) pfErrorCode, lpszErrorMsg );
      hb_retc( lpszRetErrorMsg );
   }
   else
   {
      hb_retc( "" );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_INSTALLDSN )
{
   int      x, y;

   if (hb_pcount() != 2)
   {
      hb_retl( 0 );
   }
   else
   {
      const char   *szDriver = hb_parc(1);
      char   *szAttributes = hb_strdup( hb_parc(2) );

	   for( x = strlen( szAttributes ), y = 0 ; y < x ; y++ )
      {
         if( szAttributes[ y ] == ';' )
            szAttributes[ y ] = '\0';
      }

      /* remove the DSN if it already existed */
      SQLConfigDataSource( NULL, ODBC_REMOVE_SYS_DSN, szDriver, szAttributes );

      /* create a new DSN */
      hb_retl(  SQLConfigDataSource( NULL, ODBC_ADD_SYS_DSN, szDriver, szAttributes ) );

      hb_xfree( (void *) szAttributes );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_UNINSTALLDSN )
{
   int      x, y;

   if (hb_pcount() != 2)
   {
      hb_retl( 0 );
   }
   else
   {
      const char   *szDriver = hb_parc(1);
      char   *szAttributes = hb_strdup( hb_parc(2) );

	   for( x = strlen( szAttributes ), y = 0 ; y < x ; y++ )
      {
         if( szAttributes[ y ] == ';' )
            szAttributes[ y ] = '\0';
      }

      /* remove the DSN if it already existed */
      SQLConfigDataSource( NULL, ODBC_REMOVE_SYS_DSN, szDriver, szAttributes );

      hb_xfree( (void *) szAttributes );
   }
}

#endif

//-----------------------------------------------------------------------------//

HB_FUNC( SR_ALLOCEN )
{
   SQLHENV hEnv;
   #if ODBCVER >= 0x0300
      RETCODE ret= SQLAllocHandle( SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv ) ;   
   #else
      RETCODE ret = SQLAllocEnv( &hEnv );
   #endif   

//    SQLSetEnvAttr(hEnv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3, 0);
   SQLSetEnvAttr( hEnv, SQL_ATTR_ODBC_VERSION, ( SQLPOINTER ) SQL_OV_ODBC3, SQL_IS_UINTEGER );

   hb_storptr( ( void* ) hEnv, 1 );
   hb_retni( ret );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_ALLOCCO )
{
   SQLHDBC hDbc;
   RETCODE ret = SQLAllocConnect( ( SQLHENV ) hb_parptr( 1 ), &hDbc );

   hb_storptr( ( void* ) hDbc, 2 );
   hb_retni( ret );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_DRIVERC )
{
   BYTE  bBuffer1[ 1024 ] = {0};
   SQLSMALLINT  wLen;
   #if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
      RETCODE ret =  SQLDriverConnect( ( SQLHDBC ) hb_parptr( 1 ),
                             GetDesktopWindow(),
                             (SQLCHAR *) hb_parcx( 2 ), 
                             (SQLSMALLINT)strlen(hb_parcx(2)),
                             (SQLCHAR *  )bBuffer1, 
                             (SQLSMALLINT)1024, 
                             (SQLSMALLINT * )&wLen, 
                             (SQLUSMALLINT)SQL_DRIVER_NOPROMPT ); // SQL_DRIVER_COMPLETE ) ;
   #elif defined(HB_OS_UNIX)
      RETCODE ret =  SQLDriverConnect( ( SQLHDBC ) hb_parptr( 1 ),
                             0,
                             (SQLCHAR *) hb_parcx( 2 ), 
                             (SQLSMALLINT )strlen(hb_parcx(2)),
                             (SQLCHAR * )bBuffer1, 
                             (SQLSMALLINT )1024, 
                             (SQLSMALLINT *)&wLen, 
                             (SQLUSMALLINT)SQL_DRIVER_COMPLETE ) ;

   #endif
   hb_storc( (char *) bBuffer1 , 3 );
   hb_retni( ret );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_DISCONN )
{
   hb_retni( SQLDisconnect( ( SQLHDBC ) hb_parptr( 1 ) ) );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_FREECON )
{
   hb_retni( SQLFreeConnect( ( SQLHDBC ) hb_parptr( 1 ) ) );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_FREEENV )
{
   hb_retni( SQLFreeEnv( ( SQLHENV  ) hb_parptr( 1 ) ) );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_ALLOCST )
{
   SQLHSTMT hStmt;
   SQLHDBC  hdbc  = ( SQLHDBC ) hb_parptr( 1 );
   
 //  hb_retni( SQLAllocStmt( ( HDBC ) hb_parptr( 1 ), &hStmt ) );
#if ODBCVER >= 0x0300 
   hb_retni( SQLAllocHandle( SQL_HANDLE_STMT, hdbc, &hStmt ) );
#else
   hb_retni( SQLAllocStmt( hdbc, &hStmt ) ) )
#endif
   
   hb_storptr( ( void* ) hStmt, 2 );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_FREESTM )
{
	SQLHSTMT  p =  ( SQLHSTMT ) hb_parptr( 1 ) ;
	if (p ) 
	{
#if ODBCVER >= 0x0300		
	   hb_retni( SQLFreeHandle( SQL_HANDLE_STMT, p )); 
#else
       hb_retni( SQLFreeStmt( p, SQL_DROP ) ); 
#endif	   
	   return ;
    }
    hb_retni( -1) ;   
	   	
   //hb_retni( SQLFreeStmt( ( HSTMT ) hb_parptr( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_EXECDIR )
{
   hb_retni( SQLExecDirect( ( SQLHSTMT ) hb_parptr( 1 ), (SQLCHAR *) hb_parcx( 2 ),  ( SQLINTEGER ) hb_parclen( 2 ) ) );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_FETCH )
{
    SQLHSTMT      hstmt = ( SQLHSTMT ) hb_parptr( 1 );
    RETCODE wResult;
    wResult= SQLFetch(  hstmt  );
    hb_retni( wResult );
}

//-----------------------------------------------------------------------------//

void odbcFieldGet( PHB_ITEM pField, PHB_ITEM pItem, char * bBuffer, HB_ISIZ lLenBuff, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate )
{	
   LONG lType;
   HB_SIZE lLen, lDec;
   char * cType;
   PHB_ITEM pTemp;

   cType  = ( char * ) hb_arrayGetCPtr(pField, FIELD_TYPE );
   lType = ( LONG ) hb_arrayGetNL( pField, FIELD_DOMAIN );
   lLen  = hb_arrayGetNL( pField, FIELD_LEN );
   lDec  = hb_arrayGetNL( pField, FIELD_DEC );

   if( (HB_ISIZ)lLenBuff <= 0 )     // database content is NULL
   {
      switch( lType )
      {
         case SQL_CHAR:
         case SQL_VARCHAR:
         case SQL_NVARCHAR:
         case SQL_WCHAR:
         case SQL_GUID:
         {
            char * szResult = ( char * ) hb_xgrab( lLen + 1 );
            hb_xmemset( szResult, ' ', lLen );
            hb_itemPutCLPtr( pItem, szResult,  lLen );
            break;
         }
         case SQL_NUMERIC:
         case SQL_DECIMAL:
         case SQL_DOUBLE:
         case SQL_INTEGER:
         case SQL_FLOAT:
         case SQL_REAL:
         case SQL_BIGINT:
         case SQL_FAKE_NUM:
         {
            if( cType[0] == 'L' && ulSystemID == SYSTEMID_ORACLE )
            {
               hb_itemPutL( pItem, FALSE );
            }
            else
            {
               char szResult[2] = { ' ', '\0' };
               sr_escapeNumber( szResult,  lLen,  lDec, pItem );
            }
            break;
         }
         case SQL_DATE:
         case SQL_TYPE_DATE:
         {
            char dt[9] = {' ',' ',' ',' ',' ',' ',' ',' ','\0'};
            hb_itemPutDS( pItem, dt );
            break;
         }
         case SQL_TIMESTAMP:
         case SQL_TYPE_TIMESTAMP:
         {
            if( ulSystemID == SYSTEMID_IBMDB2 )
            {
//                char * szResult = ( char * ) hb_xgrab( 26 + 1 );
//                hb_xmemset( szResult, ' ', lLen );
//                hb_itemPutCLPtr( pItem, szResult,  lLen );
               hb_itemPutTDT( pItem, 0, 0 );
            }
            else
            {
               if( ( ulSystemID == SYSTEMID_POSTGR ) || ( ulSystemID == SYSTEMID_ORACLE )|| ( ulSystemID == SYSTEMID_FIREBR )||( ulSystemID == SYSTEMID_MYSQL ) ||( ulSystemID ==  SYSTEMID_MARIADB ) || (  ulSystemID ==SYSTEMID_MSSQL7  && sr_lsql2008newTypes() ) )  
               {
// #ifdef __XHARBOUR__
//                   hb_itemPutDT( pItem, 0, 0, 0, 0, 0, 0, 0 );
// #else
                  hb_itemPutTDT( pItem, 0, 0 );
// #endif
               }
               else
               {
                  char dt[9] = {' ',' ',' ',' ',' ',' ',' ',' ','\0'};
                  hb_itemPutDS( pItem, dt );
               }
            }
            break;
         }
         case SQL_LONGVARCHAR:
         case SQL_WLONGVARCHAR:
         case SQL_DB2_CLOB:
         case SQL_FAKE_LOB:
         case SQL_LONGVARBINARY:
         {
            hb_itemPutCL( pItem, bBuffer, 0 );
            break;
         }
         case SQL_BIT:
         {
            hb_itemPutL( pItem, FALSE );
            break;
         }
         case SQL_SMALLINT:
         case SQL_TINYINT:
         {
            if( bQueryOnly )
            {
               hb_itemPutNI( pItem, 0 );
            }
            else
            {
               hb_itemPutL( pItem, FALSE );
            }
            break;
         }

#ifdef SQLRDD_TOPCONN
         case SQL_FAKE_DATE:
         {
            hb_itemPutDS( pItem, bBuffer );
            break;
         }
#endif
         default:
            TraceLog( LOGFILE, "Invalid data type detected: %i\n", lType );
      }
   }
   else
   {
      switch( lType )
      {
         case SQL_CHAR:
         case SQL_VARCHAR:
         case SQL_NVARCHAR:
         case SQL_WCHAR:
         case SQL_GUID:
         {
            HB_SIZE lPos;
            char * szResult = ( char * ) hb_xgrab( lLen + 1 );
            hb_xmemcpy( szResult, bBuffer,  ((HB_ISIZ)lLen < lLenBuff ? lLen : lLenBuff ) );

            for( lPos =  lLenBuff; lPos < lLen; lPos++ )
            {
               szResult[ lPos ] = ' ';
            }
            szResult[ lLen ] =  '\0';
            hb_itemPutCLPtr( pItem, szResult,  lLen );
            break;
         }
         case SQL_NUMERIC:
         case SQL_DECIMAL:
         case SQL_DOUBLE:
         case SQL_INTEGER:
         case SQL_FLOAT:
         case SQL_REAL:
         case SQL_BIGINT:
         case SQL_FAKE_NUM:
         {
            if( cType[0] == 'L' && ulSystemID == SYSTEMID_ORACLE )
            {
               hb_itemPutL( pItem, bBuffer[0] == '1' ? TRUE : FALSE );
            }
            else
            {
               sr_escapeNumber( bBuffer,  lLen,  lDec, pItem );
            }
            break;
         }
         case SQL_DATE:
         case SQL_TIMESTAMP:
         case SQL_TYPE_TIMESTAMP:
         case SQL_TYPE_DATE:
         {
	        
            char dt[9];
            
            if (( ulSystemID == SYSTEMID_OTERRO ) ) 
            {
               dt[0] = bBuffer[6];
               dt[1] = bBuffer[7];
               dt[2] = bBuffer[8];
               dt[3] = bBuffer[9];
               dt[4] = bBuffer[0];
               dt[5] = bBuffer[1];
               dt[6] = bBuffer[3];
               dt[7] = bBuffer[4];
            }
            else if ( ulSystemID == SYSTEMID_IBMDB2 && (lType == SQL_TIMESTAMP || lType == SQL_TYPE_TIMESTAMP) )
            {
//                hb_itemPutCL( pItem, bBuffer,  lLenBuff );
               long lJulian, lMilliSec;
               hb_dateTimeStampStrGet( bBuffer, &lJulian, &lMilliSec );
               hb_itemPutTDT( pItem, lJulian, lMilliSec );

               break;
            }
            else if ( ( (ulSystemID == SYSTEMID_POSTGR ) || ( ulSystemID == SYSTEMID_ORACLE ) || ( ulSystemID == SYSTEMID_FIREBR )||( ulSystemID == SYSTEMID_MYSQL ) ||( ulSystemID ==  SYSTEMID_MARIADB )  || (  ulSystemID ==SYSTEMID_MSSQL7  && sr_lsql2008newTypes() ) ) && (lType == SQL_TIMESTAMP|| lType == SQL_TYPE_TIMESTAMP) )
            {
#ifdef __XHARBOUR__
//                char dt1[18];
//                dt1[0] = bBuffer[0];
//                dt1[1] = bBuffer[1];
//                dt1[2] = bBuffer[2];
//                dt1[3] = bBuffer[3];
//                dt1[4] = bBuffer[5];
//                dt1[5] = bBuffer[6];
//                dt1[6] = bBuffer[8];
//                dt1[7] = bBuffer[9];
//                dt1[8] = bBuffer[11];
//                dt1[9] = bBuffer[12];
//                dt1[10] = bBuffer[14];
//                dt1[11] = bBuffer[15];
//                dt1[12] = bBuffer[17];
//                dt1[13] = bBuffer[18];
//                dt1[14] = '\0';
//                hb_itemPutDTS( pItem, dt1 );
               long lJulian, lMilliSec;
               hb_dateTimeStampStrGet( bBuffer, &lJulian, &lMilliSec );
               hb_itemPutTDT( pItem, lJulian, lMilliSec );

#else
            long lJulian, lMilliSec;
            hb_dateTimeStampStrGet( bBuffer, &lJulian, &lMilliSec );
            hb_itemPutTDT( pItem, lJulian, lMilliSec );               
#endif
               break;
            }
            else
            {
               dt[0] = bBuffer[0];
               dt[1] = bBuffer[1];
               dt[2] = bBuffer[2];
               dt[3] = bBuffer[3];
               dt[4] = bBuffer[5];
               dt[5] = bBuffer[6];
               dt[6] = bBuffer[8];
               dt[7] = bBuffer[9];
            }
            dt[8] = '\0';
            hb_itemPutDS( pItem, dt );
            break;
         }
         case SQL_LONGVARCHAR:
         case SQL_WLONGVARCHAR:
         case SQL_DB2_CLOB:
         case SQL_FAKE_LOB:
         case SQL_LONGVARBINARY:
         case SQL_VARBINARY:
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
               hb_itemPutC( pItem, bBuffer);
            }
            break;
         }
         case SQL_BIT:
         {
            hb_itemPutL( pItem, bBuffer[0] == '1' ? TRUE : FALSE );
            break;
         }
         case SQL_SMALLINT:
         case SQL_TINYINT:
         {
            if( bQueryOnly )
            {
               hb_itemPutNI( pItem, (int)hb_strVal( bBuffer, lLenBuff ) );
            }
            else
            {
               hb_itemPutL( pItem, hb_strVal( bBuffer, lLenBuff ) > 0  ? TRUE : FALSE );
            }
            break;
         }

#ifdef SQLRDD_TOPCONN
         case SQL_FAKE_DATE:
         {
            hb_itemPutDS( pItem, bBuffer );
            break;
         }
#endif
         default:
            TraceLog( LOGFILE, "Invalid data type detected: %i\n", lType );
      }
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_ODBCLINEPROCESSED )
{
//    LONG lLen,  lInitBuff, lIndex;
    LONG lLen;
    USHORT lIndex;
//    SQLLEN lLenOut;
//    PTR  bBuffer, bOut;
//    RETCODE wResult;
//    int iReallocs;
   PHB_ITEM temp;
   int i, cols;
   PHB_ITEM pFields = hb_param( 3, HB_IT_ARRAY );
   BOOL  bQueryOnly = hb_parl( 4 );
   ULONG ulSystemID = hb_parnl( 5 );
   BOOL  bTranslate = hb_parl( 6 );
   PHB_ITEM pRet    = hb_param( 7, HB_IT_ARRAY );

   if( !pFields )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, "SR_ODBCLINEPROCESSED", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }

   cols = hb_arrayLen( pFields );

   if( cols <= 0 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, "SR_ODBCLINEPROCESSED", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }

   lLen       = ( LONG )( hb_pcount() > 1 ? hb_parnl( 2 ) : 4096 );

   if( lLen <= 0 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, "SR_ODBCLINEPROCESSED", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }

//    bBuffer    = hb_xgrab( (ULONG) lLen+1 );

   for( i=1; i <= cols; i++ )
   {
   
      temp       = hb_itemNew( NULL );
      lIndex     = (USHORT)hb_arrayGetNI( hb_arrayGetItemPtr( pFields, i ), FIELD_ENUM );

      if( lIndex == 0 )
      {
         hb_arraySetForward( pRet, i, temp );
      }
      else
      {
	      
         odbcGetData( ( SQLHSTMT ) hb_parptr( 1 ), (PHB_ITEM)hb_arrayGetItemPtr( pFields, i ),(PHB_ITEM)temp,  (BOOL)bQueryOnly, (ULONG)ulSystemID, (BOOL)bTranslate,(USHORT)lIndex  );	              
         hb_arraySetForward( pRet, i, temp );                     
      }
      hb_itemRelease( temp );
   }
//    hb_xfree( ( PTR ) bBuffer );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_ODBCGETLINES ) // ( ::hStmt, nLenBuff, aFields, aCache, nSystemID, lTranslate, nCurrentFetch, aInfo, nDirection, hnRecno, lFetchAll, aFetch, uRecord, nPos )
{
//    LONG lLen, lLenOut, lInitBuff, lIndex;
   LONG   lInitBuff, lIndex;
   HB_SIZE lLen;
   SQLLEN lLenOut;
   PTR  bBuffer, bOut;
   RETCODE wResult, wReturn = SQL_ERROR;
   int iReallocs;
   PHB_ITEM pLine, temp;
   int i, cols, line;

   PHB_ITEM pFields   = hb_param( 3, HB_IT_ARRAY );
   PHB_ITEM pCache    = hb_param( 4, HB_IT_ARRAY );
   ULONG ulSystemID   = hb_parnl( 5 );
   BOOL  bTranslate   =  hb_parl( 6 );
   int iCurrFetch     = hb_parni( 7 );
   PHB_ITEM pInfo     = hb_param( 8, HB_IT_ARRAY );
   ULONG ulDirect     = hb_parnl( 9 );
   ULONG ulnRecno     = hb_parnl( 10 );
   BOOL bFetchAll     =  hb_parl( 11 );
   PHB_ITEM pFetch    = hb_param( 12, HB_IT_ARRAY );
   PHB_ITEM pRec      = hb_param( 13, HB_IT_ANY );
   LONG lPos          = hb_parnl( 14 );

   ULONG ulPosCache   = hb_arrayGetNL( pInfo, AINFO_NPOSCACHE );

   if( !pFields )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, "SR_ODBCLINEPROCESSED", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }

   cols = hb_arrayLen( pFields );

   if( cols <= 0 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, "SR_ODBCLINEPROCESSED", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }

   lLen       = ( hb_pcount() > 1 ? hb_parnl( 2 ) : 4096 );

   if( lLen <= 0 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1111, NULL, "SR_ODBCLINEPROCESSED", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }

   bBuffer = hb_xgrab(  lLen+1 );

   for( line = 1; line <= iCurrFetch; line++ )
   {
      memset( bBuffer, 0, lLen  ) ;
      wReturn = SQLFetch( ( SQLHSTMT ) hb_parptr( 1 ) );

      if( wReturn != SQL_SUCCESS )
      {
         if( wReturn == SQL_ERROR )
         {
            break;
         }
         if( ulDirect == (ULONG) ORD_DIR_FWD )
         {
            hb_arraySet( pInfo, AINFO_EOF_AT, pRec );
            hb_arraySetNL( pInfo, AINFO_NCACHEEND, lPos );
         }
         else
         {
            hb_arraySet( pInfo, AINFO_BOF_AT, pRec );
            hb_arraySetNL( pInfo, AINFO_NCACHEBEGIN, lPos );
         }
         break;
      }

      if( ulDirect == (ULONG) ORD_DIR_FWD )
      {
         lPos++;
         if( lPos > (CAHCE_PAGE_SIZE * 3) )
         {
            lPos -= (CAHCE_PAGE_SIZE * 3);
         }
      }
      else
      {
         lPos--;
         if( lPos < 1 )
         {
            lPos += (CAHCE_PAGE_SIZE * 3);
         }
      }
      pLine = (PHB_ITEM) hb_arrayGetItemPtr( pCache, lPos );

      if( HB_IS_NIL( pLine ) )
      {
         hb_arrayNew( pLine, cols );
      }

      for( i=1; i <= cols; i++ )
      {
         bOut       = NULL;
         lInitBuff  = lLen;
         lLenOut    = 0;
         iReallocs  = 0;
         temp       = hb_itemNew( NULL );
         lIndex     = hb_arrayGetNL( hb_arrayGetItemPtr( pFields, i ), FIELD_ENUM );
         lIndex     = lIndex ? lIndex : i;

         if( lIndex == 0 )
         {
            hb_arraySetForward( pLine, i, temp );
         }
         else
         {
            do
            {
               wResult    = SQLGetData( ( SQLHSTMT ) hb_parptr( 1 ), (SQLUSMALLINT)lIndex, (SQLSMALLINT)SQL_CHAR, ( PTR ) bBuffer, (SQLLEN)lLen, (SQLLEN*)&lLenOut );
               if( wResult == SQL_SUCCESS && iReallocs == 0 )
               {
                  odbcFieldGet( hb_arrayGetItemPtr( pFields, i ), temp, (char * ) bBuffer, lLenOut, 0, ulSystemID, bTranslate );
                  hb_arraySetForward( pLine, i, temp );
                  break;
               }
               else if ( wResult == SQL_SUCCESS_WITH_INFO && iReallocs == 0 )
               {
                  /* Perheps a data truncation */
                  if( lLenOut >= lInitBuff )
                  {
                     /* data right truncated! */
                     bOut    = ( char * ) hb_xgrab(  lLenOut + 1 );
                     lLen = lLenOut + 3;
                     strcpy( (char *) bOut, (char *) bBuffer );
                     bBuffer = ( char * ) hb_xrealloc( bBuffer,  lLen );
                     iReallocs++;
                  }
                  else
                  {
                     odbcFieldGet( hb_arrayGetItemPtr( pFields, i ), temp, (char * ) bBuffer, lLenOut, 0, ulSystemID, bTranslate );
                     hb_arraySetForward( pLine, i, temp );
                     break;
                  }
               }
               else if( (wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO ) && iReallocs > 0 )
               {
                  strcat( (char*) bOut, (char *) bBuffer );
                  odbcFieldGet( hb_arrayGetItemPtr( pFields, i ), temp, (char * ) bOut, lLenOut + lInitBuff - 1, 0, ulSystemID, bTranslate );
                  hb_arraySetForward( pLine, i, temp );
                  hb_xfree( ( PTR ) bOut );
                  break;
               }
               else
               {
                  break;
               }
            }
            while(wResult != SQL_NO_DATA);
         }
         hb_itemRelease( temp );
      }

      hb_itemPutNL( pRec, hb_arrayGetNL( pLine, ulnRecno ) );

      if( bFetchAll )
      {
         hb_arrayAdd( pFetch, pRec );
      }
   }
   hb_arraySetNL( pInfo, AINFO_NPOSCACHE, ulPosCache );
   hb_xfree( ( PTR ) bBuffer );
   hb_retnl( wReturn );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_NUMRES )
{
    SQLSMALLINT nCols;
    SQLHSTMT      hstmt = ( SQLHSTMT ) hb_parptr( 1 );
    
    RETCODE wResult = SQLNumResultCols( hstmt , &nCols );
    hb_stornl( ( LONG ) nCols, 2 );    
    hb_retni( wResult );


// Execute the SQL statement and return any errors or warnings.

    
}

//-----------------------------------------------------------------------------//

void odbcErrorDiagRTE( SQLHSTMT hStmt, char * routine, char * szSql, SQLRETURN res, int line, char * module )
{
   PHB_ITEM pArg;
   PHB_ITEM pError = hb_errNew();

   SQLCHAR       SqlState[6]={0}, Msg[2048]={0};
   char          ErrMsg[4096]={0};
   SQLINTEGER    NativeError = 0;
   SQLSMALLINT   i, MsgLen = 0;

   if( sr_isShutdownProcess() )
   {
      return;
   }

   SqlState[0] = '\0';
   Msg[0] = '\0';

   i = 1;
   while ((SQLGetDiagRec(SQL_HANDLE_STMT, hStmt, i, SqlState, &NativeError,
            Msg, sizeof(Msg), &MsgLen)) != SQL_NO_DATA)
   {
      if( i > 5 )
      {
         break;         // Or we can be lead to an infinite loop (ODBC sucks!)
      }
      i++;
   }

   sprintf( ErrMsg, "SQL execution error at %s, return code: %i, state: %s, description: %s.", routine, res, SqlState, Msg );

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

   return;
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_DESCRIB )
{
    SQLSMALLINT      lLen      = ( SQLSMALLINT ) hb_parni( 4 );   
    SQLSMALLINT wBufLen   = ( SQLSMALLINT)hb_parni( 5 );
    SQLSMALLINT wDataType = ( SQLSMALLINT)hb_parni( 6 );
    SQLULEN     wColSize =  ( SQLULEN )hb_parnint( 7 );
    SQLSMALLINT wDecimals = ( SQLSMALLINT)hb_parni( 8 );
    SQLSMALLINT wNullable = ( SQLSMALLINT) hb_parni( 9 );
//     SQLTCHAR   bBuffer[128]={0};

    SQLTCHAR *  bBuffer;
    RETCODE     wResult  ;
    ULONG ulSystemID = hb_parnl( 10 ) ;

    if( lLen <= 0 )
         lLen = 64;

    bBuffer      = ( SQLTCHAR * ) hb_xgrab( lLen * sizeof( SQLTCHAR ) );
    bBuffer[ 0 ] = '\0';


    wResult   = SQLDescribeCol( ( HSTMT ) hb_parptr( 1 ), 
                                            ( SQLUSMALLINT )  hb_parni( 2 ),
                                            ( SQLTCHAR * )    bBuffer, 
                                            ( SQLSMALLINT )   lLen, 
                                            ( SQLSMALLINT * ) &wBufLen,
                                            ( SQLSMALLINT * ) &wDataType, 
                                            ( SQLULEN * )     &wColSize, 
                                            ( SQLSMALLINT * ) &wDecimals,
                                            ( SQLSMALLINT * ) &wNullable );
    if( wDataType == -8  && ulSystemID == SYSTEMID_MYSQL)     // MySQL ODBC Bug
    {
      odbcErrorDiagRTE( ( SQLHSTMT ) hb_parptr( 1 ), "SQLCONNECT", "MySQL Driver version 5 is not compatible with SQLRDD", 0, __LINE__, __FILE__ );
    }

    if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
    {
       hb_storclen( ( LPSTR ) bBuffer,
                    ( HB_SIZE ) wBufLen, 3 );
       hb_storni( ( int ) wBufLen, 5 );
       hb_storni( ( int ) wDataType, 6 );
       hb_stornint(  wColSize, 7 );
       hb_storni( ( int ) wDecimals, 8 );
       hb_storni( ( int ) wNullable, 9 );

    }

    hb_xfree(  bBuffer );
    hb_retni( wResult );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_COLATTRIBUTE )
{
    SQLSMALLINT lLen      = (SQLSMALLINT)hb_parni( 5 );
    char *      bBuffer   = (char *) hb_xgrab( lLen );
    SQLSMALLINT wBufLen   = (SQLSMALLINT)hb_parni( 6 );
    SQLLEN      wNumPtr   = (SQLLEN)hb_parnint( 7 );
    RETCODE     wResult   = SQLColAttribute( ( SQLHSTMT ) hb_parptr( 1 ), ( SQLUSMALLINT )hb_parni( 2 ), ( SQLUSMALLINT )hb_parni( 3 ),
                                             ( SQLPOINTER ) bBuffer, (SQLSMALLINT)hb_parni( 5 ), ( SQLSMALLINT * )&wBufLen,
                                             (SQLLEN* )&wNumPtr );

    if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
    {
//       hb_storclen( ( LPSTR ) bBuffer,
//                    ( int ) wBufLen, 4 );
//       hb_stornl( ( LONG ) wBufLen, 6 );
       hb_stornint(  wNumPtr, 7 );
    }

    hb_xfree( ( PTR ) bBuffer );
    hb_retni( wResult );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_ERROR )
{
   SQLTCHAR    bBuffer1[ 256 ]={0}, szErrorMsg[ 256 ]={0};
   SQLINTEGER lError;
   SQLSMALLINT wLen;

   hb_retni( SQLError( ( SQLHENV  ) hb_parptr( 1 ), (  SQLHDBC  ) hb_parptr( 2 ),
                       ( SQLHSTMT ) hb_parptr( 3 ), ( SQLTCHAR *)bBuffer1, ( SQLINTEGER * )&lError,
                       ( SQLTCHAR *) szErrorMsg, ( SQLSMALLINT ) HB_SIZEOFARRAY( szErrorMsg ), ( SQLSMALLINT * )&wLen ) );

   hb_storc( (char *) bBuffer1, 4 );
   hb_stornl( lError, 5 );
   hb_storclen( (char *) szErrorMsg,wLen, 6 );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_GETINFO )
{
   char bBuffer[ 512 ]={0};
   SQLSMALLINT wLen;
   RETCODE wResult = SQLGetInfo( (  SQLHDBC  ) hb_parptr( 1 ),  ( SQLUSMALLINT )  hb_parnl( 2 ), ( SQLPOINTER )bBuffer, ( SQLSMALLINT ) 512, ( SQLSMALLINT *) &wLen );

   hb_storclen( (char *) bBuffer, wLen, 3 );
   hb_retni( wResult );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_SETCONNECTATTR )
{
//    hb_retnl( ( LONG ) SQLSetConnectAttr( (  SQLHDBC  ) hb_parptr( 1 ), ( UWORD ) hb_parnl( 2 ),
//            ( ULONG ) ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parcx( 3 ) : ( SQLPOINTER ) hb_parnl( 3 ), hb_parni( 4 ) ) );
#if ODBCVER >= 0x0300
   hb_retni(  SQLSetConnectAttr( (  SQLHDBC  ) hb_parptr( 1 ), ( SQLINTEGER ) hb_parnl( 2 ),
           ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ), 
           ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) ) ;
#else
   hb_retni( SQLSetConnectOption( (  SQLHDBC  ) hb_parptr( 1 ),
                                     ( SQLUSMALLINT ) hb_parni( 2 ),
                                     ISCHAR( 3 ) ? ( SQLULEN ) hb_parc( 3 ) : ( SQLULEN ) hb_parnl( 3 ) ) );
#endif           

}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_SETCONNECTOPTION )
{
#if ODBCVER >= 0x0300
      hb_retni( SQLSetConnectAttr( (  SQLHDBC  ) hb_parptr( 1 ),
                                   ( SQLINTEGER ) hb_parnl( 2 ),
                                   ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                                   ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) );
	
#else	
   hb_retni(  SQLSetConnectOption( (  SQLHDBC  ) hb_parptr( 1 ), ( SQLINTEGER ) hb_parnl( 2 ),
           ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ) ) );
#endif           
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_SETSTMTOPTION )
{
#if ODBCVER >= 0x0300
      hb_retni( SQLSetStmtAttr( ( SQLHSTMT ) hb_parptr( 1 ),
                                ( SQLINTEGER ) hb_parnl( 2 ),
                                ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                                ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
   hb_retni( SQLSetStmtOption( ( SQLHSTMT ) hb_parptr( 1 ),
                                  ( SQLINTEGER ) hb_parnl( 2 ),
                                  ISCHAR( 3 ) ? ( SQLULEN ) hb_parc( 3 ) : ( SQLULEN ) hb_parnl( 3 ) ) );
#endif

}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_GETCONNECTOPTION )
{
	#if ODBCVER >= 0x0300
	      SQLPOINTER buffer[ 512 ];
	      SQLINTEGER lLen = 0;
	      buffer[ 0 ] = '\0';
	      hb_retni( SQLGetConnectAttr( (  SQLHDBC  ) hb_parptr( 1 ),
	                                   ( SQLINTEGER ) hb_parnl( 2 ),
	                                   ( SQLPOINTER ) buffer,
	                                   ( SQLINTEGER ) sizeof( buffer ),
	                                   ( SQLINTEGER * ) &lLen ) );
	      hb_storclen( ( char * ) buffer, lLen, 3 );
	#else
	
   BYTE bBuffer[ 512 ]={0};
      RETCODE wResult = SQLGetConnectOption( (  SQLHDBC  ) hb_parptr( 1 ), (SQLSMALLINT)hb_parni( 2 ), ( SQLPOINTER )bBuffer );
   if( wResult == SQL_SUCCESS )
      hb_storclen( (char *) bBuffer, 512, 3 );

   hb_retni( wResult );
   #endif   
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_COMMIT ) // hEnv, hDbc
{
   hb_retni( SQLTransact( ( SQLHENV  ) hb_parptr( 1 ), (  SQLHDBC  ) hb_parptr( 2 ), SQL_COMMIT ) );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_ROLLBACK )  // hEnv, hDbc
{
   hb_retni( SQLTransact( ( SQLHENV  ) hb_parptr( 1 ), (  SQLHDBC  ) hb_parptr( 2 ), SQL_ROLLBACK ) );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_MORERESULTS ) // hEnv, hDbc
{
   hb_retni( SQLMoreResults( ( SQLHSTMT ) hb_parptr( 1 ) ) );
}

//-----------------------------------------------------------------------------//

void odbcErrorDiag( SQLHSTMT hStmt, char * routine, char * szSql, int line )
{
   SQLTCHAR       SqlState[6]={0}, Msg[2048]={0};
   SQLINTEGER    NativeError = 0;
   SQLSMALLINT   i, MsgLen = 0;

   SqlState[0] = '\0';
   Msg[0] = '\0';

   i = 1;
   while ((SQLGetDiagRec(SQL_HANDLE_STMT, hStmt,( SQLSMALLINT ) i, ( SQLTCHAR * )SqlState, ( SQLINTEGER * )&NativeError,
            ( SQLTCHAR * )Msg, ( SQLSMALLINT )sizeof(Msg),( SQLSMALLINT * ) &MsgLen)) != SQL_NO_DATA)
   {
      i++;
   }

   TraceLog( LOGFILE, "Error at %s, local %i: State: %s - Message: %s\r\nOriginal SQL code:\n%s\n", routine, line, SqlState, Msg, szSql );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_TABLES )
{
   RETCODE ret;
   ret = SQLTables( ( SQLHSTMT ) hb_parptr( 1 ), NULL, SQL_NTS, NULL, SQL_NTS, NULL, SQL_NTS, (SQLCHAR *) "TABLE", 5 );
   hb_retni( ret );
}

//-----------------------------------------------------------------------------//

HB_FUNC( SR_ODBCWRITEMEMO )
{
    SQLHDBC  hDbc;
   SQLHSTMT hStmt;
   ULONG uiLen, uiSize;
   const char * sTable = hb_parc( 2 );
   ULONG ulRecno = hb_parnl( 3 ) ;
   const char * sRecnoName = hb_parcx( 4 );
   SQLRETURN retcode3, retcode2, retcode = 0;
   SQLLEN cbSize = 0;

   PHB_ITEM pArray = hb_param( 5, HB_IT_ARRAY );
   hDbc = (  SQLHDBC  ) hb_parptr( 1 );

   uiLen =  hb_arrayLen( pArray );

   if ( hDbc && uiLen > 0 )
   {
#if ODBCVER >= 0x0300 
    SQLAllocHandle( SQL_HANDLE_STMT, hDbc, &hStmt ) ;
#else	  
    SQLAllocStmt( hDbc, &hStmt );
#endif      
      for( uiSize = 0; uiSize < uiLen; uiSize++ )
      {
         PHB_ITEM pFieldDesc = hb_arrayGetItemPtr( pArray, uiSize + 1 );
         char szSql[512] = {0};
         const char * sMemo  = hb_arrayGetCPtr( pFieldDesc, 2 );
         const char * sField = hb_arrayGetCPtr( pFieldDesc, 1 );
         sprintf( szSql, "UPDATE %s SET %s = ? WHERE %s = %lu", sTable, sField, sRecnoName, ulRecno );

         cbSize = strlen(sMemo);

         retcode = SQLBindParameter(hStmt, 1, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_LONGVARCHAR, cbSize, 0, (void*) sMemo, cbSize, &cbSize);

         if (!(retcode == SQL_SUCCESS || retcode == SQL_SUCCESS_WITH_INFO))
         {
            odbcErrorDiag( hStmt, "SQLBindParameter", szSql, __LINE__ );
            break;
         }

         retcode = SQLExecDirect(hStmt, (SQLCHAR *)szSql, SQL_NTS);
         if (!(retcode == SQL_SUCCESS || retcode == SQL_SUCCESS_WITH_INFO))
         {
            odbcErrorDiag( hStmt, "SQLExecDirect", szSql, __LINE__ );
            break;
         }
         else
         {
            retcode3 = SQLFreeStmt( ( SQLHSTMT ) hStmt, SQL_UNBIND );
            if (!(retcode3 == SQL_SUCCESS || retcode3 == SQL_SUCCESS_WITH_INFO))
            {
               odbcErrorDiag( hStmt, "SQLFreeStmt, SQL_CLOSE", szSql, __LINE__ );
               break;
            }
         }
      }
      retcode2 = SQLFreeHandle (SQL_HANDLE_STMT, hStmt);

      if (!(retcode2 == SQL_SUCCESS || retcode2 == SQL_SUCCESS_WITH_INFO))
      {
         odbcErrorDiag( hStmt, "SQLFreeStmt, SQL_CLOSE", "-", __LINE__ );
      }
   }
   hb_retni( retcode );
}

//-----------------------------------------------------------------------------//

void odbcGetData( SQLHSTMT hStmt, PHB_ITEM pField,PHB_ITEM pItem,  BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate,USHORT ui  )
{
   LONG lType;
   HB_SIZE  lDec,lLen	;
   char * cType;
//    PHB_ITEM pTemp;  
   SQLLEN iLen;
   SQLLEN lLenOut;
   SQLRETURN res;   
   
   cType  = ( char * ) hb_arrayGetCPtr(pField, FIELD_TYPE );
   lType = ( LONG ) hb_arrayGetNL( pField, FIELD_DOMAIN );
   lLen  = hb_arrayGetNL( pField, FIELD_LEN );
   lDec  = hb_arrayGetNL( pField, FIELD_DEC );          
   iLen     = SQL_NULL_DATA;
          switch (lType )
          {
	     case SQL_CHAR:
             case SQL_VARCHAR:
             case SQL_NVARCHAR:
             case SQL_WCHAR:
             case SQL_LONGVARCHAR:
             case SQL_WLONGVARCHAR:
             case SQL_DB2_CLOB:
             case SQL_FAKE_LOB:
             case SQL_LONGVARBINARY:            
             case SQL_VARBINARY:
             {

	           char buffer[2];
               lLenOut        = 0;
               res = 0;
               res = SQLGetData( ( HSTMT ) hStmt, ui, SQL_CHAR  ,  buffer, 0, &lLenOut  );                              
               if( SQL_SUCCEEDED( res   ) )
               {               
	   		      if ( lLenOut == SQL_NO_TOTAL ) {
			        lLenOut = lLen;
			      }
                  if( (int)lLenOut == SQL_NULL_DATA  || lLenOut == 0)
	              {

		             odbcFieldGet(pField, pItem, NULL, -1, bQueryOnly, ulSystemID, bTranslate );
	              }
                  else if( lLenOut > 0 )
                  {
	              
	                 char * val = ( char * ) hb_xgrab( lLenOut+1   );
                     res = SQLGetData(  ( HSTMT ) hStmt, ui, SQL_CHAR, val, lLenOut+1 , &lLenOut ) ;                     
                     if( SQL_SUCCEEDED( res ) )
                     {
                        odbcFieldGet( pField, pItem, (char * ) val, lLenOut, bQueryOnly, ulSystemID, bTranslate );
//                         hb_arraySetForward( pRet, i, temp );                     
        	         }
        	         if ( val )  
    	                hb_xfree( val );                     
                     }
               }    
               break;
            }
			case SQL_GUID:
  {

	           SQLGUID buffer;
               lLenOut        = 0;
               res = 0;
               res = SQLGetData( ( HSTMT ) hStmt, ui, SQL_GUID  ,  &buffer, sizeof(buffer), &lLenOut  );                              
			   
               if( SQL_SUCCEEDED( res   ) )
               {               
	   		      
                  if( (int)lLenOut == SQL_NULL_DATA  || lLenOut == 0)
	              {

		             odbcFieldGet(pField, pItem, NULL, -1, bQueryOnly, ulSystemID, bTranslate );
	              }
                  else if( lLenOut > 0 )
                  {
	              
	                 char * val = ( char * ) hb_xgrab( lLen +1  );
					 
                     sprintf(val,
	                 "%08x-%04x-%04x-%02x%02x-%02x%02x%02x%02x%02x%02x",
	                 buffer.Data1, buffer.Data2, buffer.Data3,
	                 buffer.Data4[0], buffer.Data4[1], buffer.Data4[2], buffer.Data4[3],
	                 buffer.Data4[4], buffer.Data4[5], buffer.Data4[6], buffer.Data4[7]);			
                     
                     odbcFieldGet( pField, pItem, (char * ) val, lLen, bQueryOnly, ulSystemID, bTranslate );
//                         hb_arraySetForward( pRet, i, temp );                     
        	         
        	         if ( val )  
    	                hb_xfree( val );                     
                     }
               }    
               break;
            }			
						           
            case SQL_INTEGER:
            case SQL_BIGINT:
            case SQL_FAKE_NUM:
            {
	            if ( lLen <  10 ) 
	            {
                  long int val = 0;
                  if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_LONG, &val, sizeof( val ), &iLen ) ) )
                  {
                       hb_itemPutNLLen( pItem, val, lLen );
                  }		            
                  if (  (int)iLen ==  SQL_NULL_DATA )
                  {
                     if( cType[0] == 'L' && ulSystemID == SYSTEMID_ORACLE )
                     {
                        hb_itemPutL( pItem, FALSE );
                     }
	                 else
	                 { 
	                  hb_itemPutNLLen( pItem, 0, lLen );
                     }
                  }
		        }
		        else
		        {		           
			      #ifdef __XHARBOUR__
			         LONGLONG val = 0;
			      #else  
                     HB_I64 val = 0;
                  #endif
                  if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_SBIGINT, &val, sizeof( val ), &iLen ) ) )
                  {
                     hb_itemPutNIntLen( pItem, val, lLen );
                  }
                  if (  (int)iLen ==  SQL_NULL_DATA )
                  {
                     if( cType[0] == 'L' && ulSystemID == SYSTEMID_ORACLE )
                     {
                        hb_itemPutL( pItem, FALSE );
                     }
	                 else
	                 { 	                  
	                    hb_itemPutNIntLen( pItem, 0, lLen );
                     }
                  }
                }
	            break;
	            }


            case SQL_NUMERIC:                       
	        case SQL_FLOAT:    
            case SQL_REAL:
            case SQL_DECIMAL:
            case SQL_DOUBLE:
            {
               double val = 0.0;
               if (lDec >0 )
                     lLen -= (lDec + 1);
               if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_DOUBLE, &val, sizeof( val ), &iLen ) ) )
               {
                    hb_itemPutNDLen( pItem, val, lLen, lDec );
               }
               if (  (int)iLen ==  SQL_NULL_DATA )
               {
	               hb_itemPutNDLen( pItem, 0.0, lLen, lDec );
               }

	            
	           break   ;
	          }
            case SQL_BIT:
            {
               unsigned char val = 0;
               if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_BIT, &val, sizeof( val ), &iLen ) ) )
               {
                  pItem = hb_itemPutL( pItem, val != 0 );
               }
               if (  (int)iLen ==  SQL_NULL_DATA )
               {
	               hb_itemPutL( pItem, 0 );
               }
               
               break;
            }
         case SQL_DATE:
         case SQL_TYPE_DATE:
         {
               DATE_STRUCT val = { 0, 0, 0 };
               if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_DATE, &val, sizeof( val ), &iLen ) ) )
               {
                  hb_itemPutD( pItem, val.year, val.month, val.day );
               }
               if (  (int)iLen ==  SQL_NULL_DATA )
               {
	               hb_itemPutD( pItem, 0, 0, 0 );
               }

            break;
         }
         case SQL_TIMESTAMP:
         case SQL_TYPE_TIMESTAMP:
         {
               TIMESTAMP_STRUCT val = { 0, 0, 0, 0, 0, 0, 0 };
               if( SQL_SUCCEEDED( res = SQLGetData( hStmt, ui, SQL_C_TIMESTAMP, &val, sizeof( val ), &iLen ) ) )
               {
	               #ifdef __XHARBOUR__
                    hb_itemPutTDT( pItem, hb_dateEncode( val.year, val.month, val.day ),
                                         hb_timeEncode( val.hour, val.minute, val.second ) );
	               
	               #else
                    hb_itemPutTDT( pItem, hb_dateEncode( val.year, val.month, val.day ),
                                         hb_timeEncode( val.hour, val.minute, val.second, val.fraction / 1000000 ) );
                   #endif                       
               }
               if (  (int)iLen ==  SQL_NULL_DATA )
               {
	              hb_itemPutTDT( pItem, 0, 0 );
               }
	         
            break;
         }

	          
   
    }   
   	
}



#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
HB_FUNC( SR_BINDBYVALUE )
{
  hb_retni( MessageBox( 0, hb_parcx( 1 ), hb_parcx( 2 ), hb_parni( 3 ) ) );
}
#endif
#endif

//-----------------------------------------------------------------------------//
