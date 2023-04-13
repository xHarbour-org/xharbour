/*
 * $Id$
 */

/*
 * Harbour Project source code
 * This file contains source for first ODBC routines.
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Felipe G. Coury <fcoury@creation.com.br>
 *    HB_SQLNUMRES()
 *    HB_SQLDESCRIB()
 *    HB_SQLEXTENDE()
 *
 * Copyright 1996 Marcelo Lombardo <lombardo@uol.com.br>
 *    SQLGETINFO()
 *    SQLSETCONNECTOPTION()
 *    SQLSETSTMTOPTION()
 *    SQLGETCONNECTOPTION()
 *    SQLGETSTMTOPTION()
 *    SQLCOMMIT()
 *    SQLROLLBACK()
 *    SQLCOLATTRIBUTE()
 *    SQLBINDOUTPARAM()
 *    SQLMORERESULTS()
 *
 * See doc/license.txt for licensing terms.
 */

#include "hbapi.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>

#include <sql.h>
#include <sqlext.h>

#ifndef SQLLEN
   #ifdef HB_OS_WIN_64
typedef INT64 SQLLEN;
   #endif
#endif

#if defined( __DMC__ )
   #define SQL_NO_DATA     SQL_NO_DATA_FOUND
   #define SQLColAttribute SQLColAttributes
SQLRETURN SQL_API SQLFetchScroll( SQLHSTMT StatementHandle,
                                  SQLSMALLINT FetchOrientation, SQLINTEGER FetchOffset );
#endif

HB_FUNC( SQLALLOCEN ) /* HB_SQLALLOCENV( @hEnv ) --> nRetCode */
{
   HENV     hEnv;
   SQLRETURN  ret = SQLAllocEnv( &hEnv );

   //hb_stornl( ( LONG ) hEnv, 1 );
   hb_storns( ( HB_ISIZ ) hEnv, 1 );
   hb_retni( ret );
}

#ifdef SQL_SUCCESS

HB_FUNC( SQLALLOCCO ) /* HB_SQLALLOCCONNECT( hEnv, @ hDbc ) --> nRetCode */
{
   HDBC     hDbc;
   SQLRETURN  ret = SQLAllocConnect( ( HENV ) hb_parns( 1 ), &hDbc );

   //hb_stornl( ( LONG ) hDbc, 2 );
   hb_storns( ( HB_ISIZ ) hDbc, 2 );
   hb_retni( ret );
}

HB_FUNC( SQLDRIVERC ) /* HB_SQLDRIVERCONNECT( hDbc, @ cConnectString ) --> nRetCode */
{
   BYTE     bBuffer1[ 1024 ];
   SWORD    wLen;
   SQLRETURN  ret;

   bBuffer1[ 0 ]  = '\0';
   ret            = SQLDriverConnect( ( HDBC ) hb_parns( 1 ),
                                      0,
                                      ( SQLCHAR * ) hb_parcx( 2 ), ( SQLSMALLINT ) hb_parclen( 2 ),
                                      ( SQLCHAR * ) bBuffer1, 1024, &wLen, SQL_DRIVER_COMPLETE );
   hb_storc( ( char * ) bBuffer1, 3 );
   hb_retni( ret );
}

HB_FUNC( SQLCONNECT ) /* HB_SQLCONNECT( hDbc, cDSN, cUseName, cPassword ) --> nRetCode */
{
   SQLRETURN ret = SQLConnect( ( HDBC ) hb_parns( 1 ),
                             ( SQLCHAR * ) hb_parcx( 2 ),
                             ( SQLSMALLINT ) hb_parclen( 2 ),
                             ( SQLCHAR * ) hb_parcx( 3 ),
                             ( SQLSMALLINT ) hb_parclen( 3 ),
                             ( SQLCHAR * ) hb_parcx( 4 ),
                             ( SQLSMALLINT ) hb_parclen( 4 ) );

   hb_retni( ret );
}

HB_FUNC( SQLDISCONN )  /* HB_SQLDISCONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLDisconnect( ( HDBC ) hb_parns( 1 ) ) );
}

HB_FUNC( SQLFREECON )  /* HB_SQLFREECONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLFreeConnect( ( HDBC ) hb_parns( 1 ) ) );
}

HB_FUNC( SQLFREEENV )  /* HB_SQLFREEENV( hEnv ) --> nRetCode */
{
   hb_retni( SQLFreeEnv( ( HENV ) hb_parns( 1 ) ) );
}

HB_FUNC( SQLALLOCST )  /* HB_SQLALLOCSTMT( hDbc, @ hStmt ) --> nRetCode */
{
   HSTMT hStmt;

   hb_retni( SQLAllocStmt( ( HDBC ) hb_parns( 1 ), &hStmt ) );
   hb_storns( ( HB_ISIZ ) hStmt, 2 );
}

HB_FUNC( SQLFREESTM ) /* HB_SQLFREESTMT( hStmt, nType ) --> nRetCode */
{
   hb_retni( SQLFreeStmt( ( HSTMT ) hb_parns( 1 ), ( SQLUSMALLINT ) hb_parni( 2 ) ) );
}

HB_FUNC( SQLEXECDIR )  /* HB_SQLEXECDIRECT( hStmt, cStatement ) --> nRetCode */
{
   hb_retni( SQLExecDirect( ( HSTMT ) hb_parns( 1 ), ( SQLCHAR * ) hb_parcx( 2 ), ( SQLINTEGER ) hb_parclen( 2 ) ) );
}

HB_FUNC( SQLFETCH )   /* HB_SQLFETCH( hStmt ) --> nRetCode */
{
   hb_retni( SQLFetch( ( HSTMT ) hb_parns( 1 ) ) );
}

HB_FUNC( SQLGETDATA ) /* HB_SQLGETDATA( hStmt, nField, nType, nLen, @cBuffer ) --> nRetCode */
{
   SQLLEN   lLen, lInitBuff;
   PTR      bBuffer, bOut;
   SQLRETURN  wResult;
   SQLSMALLINT wType;
   int      iReallocs = 0;

   lLen        = ( SQLLEN ) ( hb_parnl( 4 ) ? hb_parnl( 4 ) : 64 );
   bBuffer     = hb_xgrab( ( ULONG ) lLen + 1 );
   bOut        = NULL;
   lInitBuff   = lLen;
   wType       = ( hb_parni( 3 ) ? ( WORD ) hb_parni( 3 ) : SQL_BINARY );

   wResult     = ! SQL_NO_DATA;
   while( wResult != SQL_NO_DATA )
   {
#if defined( __POCC__ ) && defined( HB_OS_WIN_64 )
      wResult  = SQLGetData( ( HSTMT ) hb_parns( 1 ), hb_parni( 2 ), wType, ( PTR ) bBuffer, lLen, ( long long int * ) &lLen );
#else
      wResult  = SQLGetData( ( HSTMT ) hb_parns( 1 ), ( SQLUSMALLINT ) hb_parni( 2 ), wType, ( PTR ) bBuffer, lLen, (SQLLEN *) &lLen );
#endif
      if( wResult == SQL_SUCCESS && iReallocs == 0 )
      {
         hb_storclen( ( LPSTR ) bBuffer, ( ULONG ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
         break;
      }
      else if( wResult == SQL_SUCCESS_WITH_INFO && iReallocs == 0 )
      {
         /* Perheps a data truncation */
         if( lLen >= lInitBuff )
         {
            /* data right truncated! */
            bOut     = ( char * ) hb_xgrab( ( ULONG ) lLen + 1 );
            lLen     = lLen - lInitBuff + 2;
            hb_xstrcpy( ( char * ) bOut, ( char * ) bBuffer, 0 );
            bBuffer  = ( char * ) hb_xrealloc( bBuffer, ( ULONG ) lLen );
         }
         else
         {
            hb_storclen( ( LPSTR ) bBuffer, ( ULONG ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
            break;
         }
         iReallocs++;
      }
      else if( ( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO ) && iReallocs > 0 )
      {
         hb_xstrcat( ( char * ) bOut, ( char * ) bBuffer, 0 );
         hb_storclen( ( LPSTR ) bOut, ( ULONG ) ( lLen + lInitBuff - 1 ), 5 );
         wResult = SQL_SUCCESS;
         break;
      }
      else
      {
         break;
      }
   }
   hb_xfree( ( PTR ) bBuffer );
   if( bOut )
   {
      hb_xfree( ( PTR ) bOut );
   }
   hb_retni( wResult );
}

/* HB_NUMRESULTCOLS( hStmt, @nColCount ) */
HB_FUNC( SQLNUMRES )
{
   SQLSMALLINT nCols;
   WORD        wResult = SQLNumResultCols( ( HSTMT ) hb_parns( 1 ), &nCols );

/* if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO ) */
   hb_stornl( ( LONG ) nCols, 2 );

   hb_retni( wResult );
}

/* HB_SQLDESCRIBECOL( hStmt, nCol, @cName, nLen, @nBufferLen, @nDataType, @nColSize, @nDec, @nNull ) --> nRetCode */
HB_FUNC( SQLDESCRIB )
{
   SDWORD      lLen        = ( SDWORD ) hb_parnl( 4 );
   PTR         bBuffer     = hb_xgrab( lLen );
   SQLSMALLINT wBufLen     = ( SQLUSMALLINT ) hb_parni( 5 );
   SQLSMALLINT wDataType   = ( SQLUSMALLINT ) hb_parni( 6 );
   SQLUINTEGER wColSize    = hb_parni( 7 );
   SQLSMALLINT wDecimals   = ( SQLUSMALLINT ) hb_parni( 8 );
   SQLSMALLINT wNullable   = ( SQLUSMALLINT ) hb_parni( 9 );

#if defined( __POCC__ ) && defined( HB_OS_WIN_64 )
   WORD        wResult     = SQLDescribeCol( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ),
                                             ( SQLCHAR * ) bBuffer, hb_parni( 4 ), &wBufLen,
                                             &wDataType, ( unsigned long long int * ) &wColSize, &wDecimals,
                                             &wNullable );
#else
   WORD wResult = SQLDescribeCol( ( HSTMT ) hb_parns( 1 ), ( SQLUSMALLINT ) hb_parni( 2 ),
                                  ( SQLCHAR * ) bBuffer, ( SQLUSMALLINT ) hb_parni( 4 ), &wBufLen,
                                  &wDataType, (SQLULEN *) &wColSize, &wDecimals,
                                  &wNullable );
#endif

   if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
   {
      hb_storclen( ( LPSTR ) bBuffer,
                   ( WORD ) wBufLen, 3 );
      hb_stornl( ( LONG ) wBufLen, 5 );
      hb_stornl( ( LONG ) wDataType, 6 );
      hb_stornl( ( LONG ) wColSize, 7 );
      hb_stornl( ( LONG ) wDecimals, 8 );
      hb_stornl( ( LONG ) wNullable, 9 );
   }

   hb_xfree( ( PTR ) bBuffer );
   hb_retni( wResult );
}

/* SQLCOLATTRIBUTE( hStmt, nCol, nField, @cName, nLen, @nBufferLen, @nAttribute ) --> nRetCode */
HB_FUNC( SQLCOLATTRIBUTE )
{
   SDWORD      lLen     = ( SDWORD ) hb_parnl( 5 );
   PTR         bBuffer  = hb_xgrab( lLen );
   SQLSMALLINT wBufLen  = ( SQLUSMALLINT ) hb_parni( 6 );
   SQLSMALLINT wNumPtr  = ( SQLUSMALLINT ) hb_parni( 7 );
   WORD        wResult  = SQLColAttribute( ( HSTMT ) hb_parns( 1 ), ( SQLUSMALLINT ) hb_parni( 2 ), ( SQLUSMALLINT ) hb_parni( 3 ),
                                           ( unsigned char * ) bBuffer, ( SQLUSMALLINT ) hb_parni( 5 ), &wBufLen,
#if defined( __DMC__ )
                                           ( SQLINTEGER FAR * ) &wNumPtr );

#else
                                           ( SQLPOINTER ) &wNumPtr );
#endif

   if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
   {
      hb_storclen( ( LPSTR ) bBuffer,
                   ( WORD ) wBufLen, 4 );
      hb_stornl( ( LONG ) wBufLen, 6 );
      hb_stornl( ( LONG ) wNumPtr, 7 );
   }

   hb_xfree( ( PTR ) bBuffer );
   hb_retni( wResult );
}

/* HB_SQLEXTENDEDFETCH( hStmt, nOrientation, nOffset, @nRows, @nRowStatus ) */
HB_FUNC( SQLEXTENDE )
{
   SQLUINTEGER    uiRowCountPtr  = hb_parni( 4 );
   SQLUSMALLINT   siRowStatus    = ( SQLUSMALLINT ) hb_parni( 5 );
   WORD           wResult        = SQLExtendedFetch( ( HSTMT ) hb_parns( 1 ),
                                                     ( USHORT ) hb_parnl( 2 ),
                                                     ( USHORT ) hb_parnl( 3 ),
#if defined(__POCC__) && defined( HB_OS_WIN_64 )
                                                     &uiRowCountPtr,
#else
                                                     (SQLULEN *) &uiRowCountPtr,
#endif
                                                     &siRowStatus );

   if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
   {
      hb_stornl( ( LONG ) uiRowCountPtr, 4 );
      hb_stornl( ( LONG ) siRowStatus, 5 );
   }

   hb_retni( wResult );
}

HB_FUNC( SQLFETCHSC )
{
   hb_retni( SQLFetchScroll( ( HSTMT ) hb_parns( 1 ),
                             ( SHORT ) hb_parnl( 2 ), hb_parnl( 3 ) ) );
}

HB_FUNC( SQLERROR ) //  hEnv, hDbc, hStmt, @ cErrorClass, @ nType, @ cErrorMsg
{
   BYTE        bBuffer1[ 256 ], szErrorMsg[ 256 ];
   SQLINTEGER  lError;
   SWORD       wLen;

   hb_retni( SQLError( ( HENV ) hb_parns( 1 ), ( HDBC ) hb_parns( 2 ),
                       ( HSTMT ) hb_parns( 3 ), ( SQLCHAR * ) bBuffer1, &lError,
                       ( SQLCHAR * ) szErrorMsg, 256, &wLen ) );

   hb_storc( ( char * ) bBuffer1, 4 );
   hb_stornl( ( LONG ) lError, 5 );
   hb_storc( ( char * ) szErrorMsg, 6 );
}

HB_FUNC( SQLROWCOUN )
{
   SQLLEN   iRowCountPtr   = hb_parni( 2 );
   SQLRETURN wResult        = SQLRowCount( ( HSTMT ) hb_parns( 1 ),
                                          (SQLLEN * )&iRowCountPtr );

   if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
   {
      hb_stornint( iRowCountPtr, 2 );
   }

   hb_retni( wResult );
}

HB_FUNC( SQLGETINFO ) // hDbc, nType, @cResult
{
   BYTE        bBuffer[ 512 ];
   SQLSMALLINT wLen;
   WORD        wResult = SQLGetInfo( ( HDBC ) hb_parns( 1 ), ( UWORD ) hb_parnl( 2 ), bBuffer, 512, &wLen );

   hb_storclen( ( char * ) bBuffer, wLen, 3 );
   hb_retni( wResult );
}

HB_FUNC( SQLSETCONNECTOPTION ) // hDbc, nOption, uOption
{
#if ODBCVER >= 0x0300
   hb_retns(  SQLSetConnectAttr( ( HDBC ) hb_parns( 1 ), ( SQLINTEGER ) ( UWORD ) hb_parns( 2 ),
           ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ), 
           ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) ) ;
#else           
#if ( defined( __MINGW32__ ) || defined( _MSC_VER ) ) && defined( HB_OS_WIN_64 )
   hb_retns( ( HB_ISIZ ) SQLSetConnectOption( ( HDBC ) hb_parns( 1 ), ( UWORD ) hb_parns( 2 ),
                                           ( UDWORD ) ISCHAR( 3 ) ? ( HB_LONG ) hb_parcx( 3 ) : hb_parns( 3 ) ) );
#else
   hb_retns( ( HB_ISIZ ) SQLSetConnectOption( ( HDBC ) hb_parns( 1 ), ( UWORD ) hb_parns( 2 ),
                                           ( UDWORD ) ISCHAR( 3 ) ? ( LONG ) hb_parcx( 3 ) : hb_parns( 3 ) ) );
#endif
#endif
}

HB_FUNC( SQLSETSTMTOPTION ) // hStmt, nOption, uOption )  --> nRetCode
{
#if ODBCVER >= 0x0300
      hb_retns( SQLSetStmtAttr( ( SQLHSTMT ) hb_parns( 1 ),
                                ( SQLINTEGER ) hb_parns( 2 ),
                                ISCHAR( 3 ) ? ( SQLPOINTER ) hb_parc( 3 ) : ( SQLPOINTER ) ( HB_PTRUINT ) hb_parnint( 3 ),
                                ISCHAR( 3 ) ? ( SQLINTEGER ) hb_parclen( 3 ) : ( SQLINTEGER ) SQL_IS_INTEGER ) );
#else
	
#if ( defined( __MINGW32__ ) || defined( _MSC_VER ) ) && defined( HB_OS_WIN_64 )
   hb_retns( ( HB_ISIZ ) SQLSetStmtOption( ( SQLHSTMT ) hb_parns( 1 ), ( UWORD ) hb_parns( 2 ),
                                        ( UDWORD ) ISCHAR( 3 ) ? ( HB_LONG ) hb_parcx( 3 ) : hb_parns( 3 ) ) );
#else
   hb_retns( ( HB_ISIZ ) SQLSetStmtOption( ( SQLHSTMT ) hb_parns( 1 ), ( UWORD ) hb_parns( 2 ),
                                        ( UDWORD ) ISCHAR( 3 ) ? ( LONG ) hb_parcx( 3 ) : hb_parns( 3 ) ) );
#endif
#endif
}

HB_FUNC( SQLGETCONNECTOPTION ) // hDbc, nOption, @cOption
{
   #if ODBCVER >= 0x0300
	      SQLPOINTER buffer[ 512 ];
	      SQLINTEGER lLen = 0;
	      buffer[ 0 ] = '\0';
	      hb_retni( SQLGetConnectAttr( ( HDBC ) hb_parns( 1 ),
	                                   ( SQLINTEGER ) hb_parni( 2 ),
	                                   ( SQLPOINTER ) buffer,
	                                   ( SQLINTEGER ) sizeof( buffer ),
	                                   ( SQLINTEGER * ) &lLen ) );
	      hb_storclen( ( char * ) buffer, lLen, 3 );
   #else
	
      BYTE  bBuffer[ 512 ];
      SQLRETURN  wResult = SQLGetConnectOption( ( HDBC ) hb_parns( 1 ), ( SQLUSMALLINT ) hb_parni( 2 ), bBuffer );
      
      if( wResult == SQL_SUCCESS )
         hb_storclen( ( char * ) bBuffer, 512, 3 );
      
      hb_retni( wResult );
   #endif   
}

HB_FUNC( SQLGETSTMTOPTION ) // hStmt, nOption, @cOption
{
   #if ODBCVER >= 0x0300
	      SQLPOINTER buffer[ 512 ];
	      SQLINTEGER lLen = 0;
	      buffer[ 0 ] = '\0';
	      hb_retni( SQLGetStmtAttr( ( SQLHSTMT ) hb_parns( 1 ),
	                                   ( SQLUSMALLINT ) hb_parni( 2 ),
	                                   ( SQLPOINTER ) buffer,
	                                   ( SQLINTEGER ) sizeof( buffer ),
	                                   ( SQLINTEGER * ) &lLen ) );
	      hb_storclen( ( char * ) buffer, lLen, 3 );
   #else
	
   BYTE  bBuffer[ 512 ];
   SQLRETURN  wResult = SQLGetStmtOption( ( SQLHSTMT ) hb_parns( 1 ), ( SQLUSMALLINT ) hb_parni( 2 ), bBuffer );

   if( wResult == SQL_SUCCESS )
   {
      hb_storclen( ( char * ) bBuffer, 512, 3 );
   }

   hb_retni( wResult );
   #endif   
}

HB_FUNC( SQLCOMMIT ) // hEnv, hDbc
{
   hb_retni( SQLTransact( ( HENV ) hb_parns( 1 ), ( HDBC ) hb_parns( 2 ), SQL_COMMIT ) );
}

HB_FUNC( SQLROLLBACK )  // hEnv, hDbc
{
   hb_retni( SQLTransact( ( HENV ) hb_parns( 1 ), ( HDBC ) hb_parns( 2 ), SQL_ROLLBACK ) );
}
HB_FUNC( SETNUMLEN )  /* SETNUMLEN( nValue, nSize, nDecimals ) ==> nValue (nSize, nDec) */
{
   hb_retnlen( hb_parnd( 1 ), hb_parnl( 2 ), hb_parnl( 3 ) );
}

HB_FUNC( SQLPREPARE )  /* HB_SQLPREPARE( hStmt, cStatement ) --> nRetCode */
{
   hb_retni( SQLPrepare( ( HSTMT ) hb_parns( 1 ), ( SQLCHAR * ) hb_parcx( 2 ), SQL_NTS ) );
}

HB_FUNC( SQLEXECUTE )  /* HB_SQLEXECUTE( hStmt ) --> nRetCode */
{
   hb_retni( SQLExecute( ( HSTMT ) hb_parns( 1 ) ) );
}

HB_FUNC( SQLEXECUTESCALAR )
{
   HSTMT    hStmt;
   SQLLEN   lLen;
   BYTE     bBuffer[ 256 ];
   SQLRETURN    wResult;

   wResult = SQLAllocStmt( ( HDBC ) hb_parns( 2 ), &hStmt );

   if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
   {
      wResult = SQLExecDirect( ( HSTMT ) hStmt, ( SQLCHAR * ) hb_parcx( 1 ), SQL_NTS );
      if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
      {
         wResult = SQLFetch( ( HSTMT ) hStmt );
         if( wResult != SQL_NO_DATA )
         {
#if defined( __POCC__ ) && defined( HB_OS_WIN_64 )
            wResult  = SQLGetData( ( HSTMT ) hStmt, 1, SQL_C_CHAR, bBuffer, sizeof( bBuffer ), ( long long int * ) &lLen );
#else
            wResult  = SQLGetData( ( HSTMT ) hStmt, 1, SQL_C_CHAR, bBuffer, sizeof( bBuffer ), (SQLLEN *) &lLen );
#endif
            hb_storc( ( char * ) bBuffer, 3 );
         }
      }
   }

   hb_retni( wResult );

   SQLFreeStmt( ( HSTMT ) hStmt, 0 );

}

HB_FUNC( SQLSTOD )
{
   if( hb_parclen( 1 ) >= 10 )
   {
      const char *   szSqlDate = hb_parc( 1 );  /* YYYY-MM-DD */
      char           szHrbDate[ 9 ];            /* YYYYMMDD */

      szHrbDate[ 0 ] = szSqlDate[ 0 ];
      szHrbDate[ 1 ] = szSqlDate[ 1 ];
      szHrbDate[ 2 ] = szSqlDate[ 2 ];
      szHrbDate[ 3 ] = szSqlDate[ 3 ];
      szHrbDate[ 4 ] = szSqlDate[ 5 ];
      szHrbDate[ 5 ] = szSqlDate[ 6 ];
      szHrbDate[ 6 ] = szSqlDate[ 8 ];
      szHrbDate[ 7 ] = szSqlDate[ 9 ];
      szHrbDate[ 8 ] = '\0';
      hb_retds( szHrbDate );
   }
   else
   {
      hb_retds( NULL );
   }
}

HB_FUNC( SQLMORERESULTS ) // hEnv, hDbc
{
   hb_retni( SQLMoreResults( ( SQLHSTMT ) hb_parns( 1 ) ) );
}

#if 0
HB_FUNC( SQLBINDOUTPARAM ) /* SqlBindOutParam( nStatementHandle, nParameterNumber, nParameterType, ColumnSize, DecimalDigits, @ParamValue, @ParamLength    ) --> nRetCode */
{
   SQLLEN   lLen = hb_parnl( 7 );
   SQLRETURN  ret;

   ret = SQLBindParameter( ( HSTMT ) hb_parnl( 1 ), ( USHORT ) hb_parni( 2 ),
                           SQL_PARAM_OUTPUT, SQL_CHAR, ( USHORT ) hb_parni( 3 ),
                           ( USHORT ) hb_parni( 4 ), ( USHORT ) hb_parni( 5 ),
                           hb_parcx( 6 ), hb_parclen( 6 ),
                           &lLen );
   hb_stornl( ( LONG ) lLen, 7 );
   hb_retni( ret );
}
#endif

#endif     /* HSTMT */
