/*
 * $Id: odbc.c,v 1.4 2003/04/27 15:58:52 paultucker Exp $
 */

/*
 * Harbour Project source code
 *
 * This file contains source for first odbc routines.
 *
 * Copyright 1999  Antonio Linares <alinares@fivetech.com>
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
 * See doc/license.txt for licensing terms.
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.xharbour.org
 *
 * Copyright 1996 Marcelo Lombardo <lombardo@uol.com.br>
 *    SQLGETINFO()
 *    SQLSETCONNECTOPTION()
 *    SQLSETSTMTOPTION()
 *    SQLGETCONNECTOPTION()
 *    SQLGETSTMTOPTION()
 *    SQLCOMMIT()
 *    SQLROLLBACK()
 *
 * See doc/license.txt for licensing terms.
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbdefs.h"

#if !defined( __DJGPP__) && !defined(HB_OS_OS2)

#if defined(HB_OS_WIN_32)
   #include <windows.h>
#endif

#include <limits.h>
#include <malloc.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

HB_FUNC( SQLALLOCEN ) /* HB_SQLALLOCENV( @hEnv ) --> nRetCode */
{
   HENV hEnv;
   RETCODE ret = SQLAllocEnv( &hEnv );

   hb_stornl( ( LONG ) hEnv, 1 );
   hb_retni( ret );
}

HB_FUNC( SQLALLOCCO ) /* HB_SQLALLOCCONNECT( hEnv, @ hDbc ) --> nRetCode */
{
   HDBC hDbc;
   RETCODE ret = SQLAllocConnect( ( HENV ) hb_parnl( 1 ), &hDbc );

   hb_stornl( ( LONG ) hDbc, 2 );
   hb_retni( ret );
}

HB_FUNC( SQLDRIVERC ) /* HB_SQLDRIVERCONNECT( hDbc, @ cConnectString ) --> nRetCode */
{
   BYTE  bBuffer1[ 1024 ];
   SWORD  wLen;
   #if defined(HB_OS_WIN_32)
      RETCODE ret =  SQLDriverConnect( ( HDBC ) hb_parnl( 1 ),
                             GetDesktopWindow(),
                             (unsigned char *) hb_parc( 2 ), strlen(hb_parc(2)),
                             bBuffer1, 1024, &wLen, SQL_DRIVER_COMPLETE ) ;
   #elif defined(HB_OS_UNIX)
      RETCODE ret =  SQLDriverConnect( ( HDBC ) hb_parnl( 1 ),
                             0,
                             hb_parc( 2 ), strlen(hb_parc(2)),
                             bBuffer1, 1024, &wLen, SQL_DRIVER_COMPLETE ) ;

   #endif
   hb_storc( (char *) bBuffer1 , 3 );
   hb_retni( ret );
}

HB_FUNC( SQLCONNECT ) /* HB_SQLCONNECT( hDbc, cDSN, cUseName, cPassword ) --> nRetCode */
{
      RETCODE ret =  SQLConnect( ( HDBC ) hb_parnl( 1 ),
                                 (unsigned char*) hb_parc( 2 ),
                                 strlen( hb_parc(2) ),
                                 (unsigned char*) hb_parc( 3 ),
                                 strlen( hb_parc(3) ),
                                 (unsigned char*) hb_parc( 4 ),
                                 strlen( hb_parc(4) ) );
   hb_retni( ret );
}

HB_FUNC( SQLDISCONN )  /* HB_SQLDISCONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLDisconnect( ( HDBC ) hb_parnl( 1 ) ) );
}

HB_FUNC( SQLFREECON )  /* HB_SQLFREECONNECT( hDbc ) --> nRetCode */
{
   hb_retni( SQLFreeConnect( ( HDBC ) hb_parnl( 1 ) ) );
}

HB_FUNC( SQLFREEENV )  /* HB_SQLFREEENV( hEnv ) --> nRetCode */
{
   hb_retni( SQLFreeEnv( ( HENV ) hb_parnl( 1 ) ) );
}

HB_FUNC( SQLALLOCST )  /* HB_SQLALLOCSTMT( hDbc, @ hStmt ) --> nRetCode */
{
   HSTMT hStmt;

   hb_retni( SQLAllocStmt( ( HDBC ) hb_parnl( 1 ), &hStmt ) );
   hb_stornl( ( LONG ) hStmt, 2 );
}

HB_FUNC( SQLFREESTM ) /* HB_SQLFREESTMT( hStmt, nType ) --> nRetCode */
{
   hb_retni( SQLFreeStmt( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( SQLEXECDIR )  /* HB_SQLEXECDIRECT( hStmt, cStatement ) --> nRetCode */
{
   hb_retni( SQLExecDirect( ( HSTMT ) hb_parnl( 1 ), (unsigned char*) hb_parc( 2 ), SQL_NTS ) );
}

HB_FUNC( SQLFETCH )   /* HB_SQLFETCH( hStmt ) --> nRetCode */
{
   hb_retni( SQLFetch( ( HSTMT ) hb_parnl( 1 ) ) );
}

HB_FUNC( SQLGETDATA ) /* HB_SQLGETDATA( hStmt, nField, nType, nLen, @cBuffer ) --> nRetCode */
{
   SDWORD lLen, lInitBuff;
   PTR  bBuffer, bOut, pPos;
   WORD wType, wResult;
   int iReallocs = 0;
   int i;

   lLen       = ( SDWORD )( hb_parnl( 4 ) ? hb_parnl( 4 ) : 64 );
   bBuffer    = hb_xgrab( lLen );
   lInitBuff  = (int) lLen;
   wType      = ( hb_parni( 3 ) ? hb_parni( 3 ) : SQL_BINARY );

   do
   {
      wResult    = SQLGetData( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ), wType, ( PTR ) bBuffer, lLen, &lLen );
      if( wResult == SQL_SUCCESS && iReallocs == 0 )
      {
         hb_storclen( ( LPSTR ) bBuffer, ( WORD ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
         hb_xfree( ( PTR ) bBuffer );
         break;
      }
      else if ( wResult == SQL_SUCCESS_WITH_INFO && iReallocs == 0 )
      {
         /* Perheps a data truncation */
         if( lLen > lInitBuff )
         {
            /* data right truncated! */
            bOut    = ( char * ) hb_xgrab( lLen + 1 );
            lLen = lLen - lInitBuff+2;
            strcpy( bOut, bBuffer );
            bBuffer = ( char * ) hb_xrealloc( bBuffer, lLen );
         }
         else
         {
            hb_storclen( ( LPSTR ) bBuffer, ( WORD ) ( lLen < 0 ? 0 : ( lLen < hb_parnl( 4 ) ? lLen : hb_parnl( 4 ) ) ), 5 );
            hb_xfree( ( PTR ) bBuffer );
            break;
         }
         iReallocs++;
      }
      else if( (wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO ) && iReallocs > 0 )
      {
         strcat( bOut, bBuffer );
         hb_storclen( ( LPSTR ) bOut, ( WORD ) ( lLen + lInitBuff - 1 ), 5 );
         hb_xfree( ( PTR ) bBuffer );
         hb_xfree( ( PTR ) bOut );
         wResult = SQL_SUCCESS;
         break;
      }
      else
      {
         break;
      }
   }
   while(wResult != SQL_NO_DATA);
   hb_retni( wResult );
}

/* HB_NUMRESULTCOLS( hStmt, @nColCount ) */
HB_FUNC( SQLNUMRES )
{
    SQLSMALLINT nCols;
    WORD wResult = SQLNumResultCols( ( HSTMT ) hb_parnl( 1 ), &nCols );

 /*   if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO ) */
       hb_stornl( ( LONG ) nCols, 2 );

    hb_retni( wResult );
}

/* HB_SQLDESCRIBECOL( hStmt, nCol, @cName, nLen, @nBufferLen, @nDataType, @nColSize, @nDec, @nNull ) --> nRetCode */
HB_FUNC( SQLDESCRIB )
{
    SDWORD      lLen      = ( SDWORD ) hb_parnl( 4 );
    PTR         bBuffer   = hb_xgrab( lLen );
    SQLSMALLINT wBufLen   = hb_parni( 5 );
    SQLSMALLINT wDataType = hb_parni( 6 );
    SQLUINTEGER wColSize  = hb_parni( 7 );
    SQLSMALLINT wDecimals = hb_parni( 8 );
    SQLSMALLINT wNullable = hb_parni( 9 );
    WORD        wResult   = SQLDescribeCol( ( HSTMT ) hb_parnl( 1 ), hb_parni( 2 ),
                                            (unsigned char*) bBuffer, hb_parni( 4 ), &wBufLen,
                                            &wDataType, &wColSize, &wDecimals,
                                            &wNullable );

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

/* HB_SQLEXTENDEDFETCH( hStmt, nOrientation, nOffset, @nRows, @nRowStatus ) */
HB_FUNC( SQLEXTENDE )
{
    SQLUINTEGER  uiRowCountPtr = hb_parni( 4 );
    SQLUSMALLINT siRowStatus   = hb_parni( 5 );
    WORD         wResult       = SQLExtendedFetch( ( HSTMT ) hb_parnl( 1 ),
                                                   ( USHORT )hb_parnl( 2 ),
                                                   ( USHORT )hb_parnl( 3 ),
                                                   &uiRowCountPtr,
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
    hb_retni( SQLFetchScroll( ( HSTMT ) hb_parnl( 1 ),
                              ( SHORT ) hb_parnl( 2 ), hb_parnl( 3 ) ) );
}

HB_FUNC( SQLERROR ) //  hEnv, hDbc, hStmt, @ cErrorClass, @ nType, @ cErrorMsg
{
   BYTE bBuffer1[ 256 ], szErrorMsg[ 256 ];
   UDWORD lError;
   SWORD wLen;

   hb_retni( SQLError( ( HENV ) hb_parnl( 1 ), ( HDBC ) hb_parnl( 2 ),
                       ( HSTMT ) hb_parnl( 3 ), bBuffer1, (long *) &lError,
                       szErrorMsg, 256, &wLen ) );

   hb_storc( (char *) bBuffer1, 4 );
   hb_stornl( lError, 5 );
   hb_storc( (char *) szErrorMsg, 6 );
}

HB_FUNC( SQLROWCOUN )
{
    SQLUINTEGER  uiRowCountPtr = hb_parni( 2 );
    WORD         wResult       = SQLRowCount( ( HSTMT ) hb_parnl( 1 ),
                                              (long *) &uiRowCountPtr );

    if( wResult == SQL_SUCCESS || wResult == SQL_SUCCESS_WITH_INFO )
    {
       hb_stornl( ( LONG ) uiRowCountPtr, 2 );
    }

    hb_retni( wResult );

}

HB_FUNC( SQLGETINFO ) // hDbc, nType, @cResult
{
   BYTE bBuffer[ 512 ];
   SQLSMALLINT wLen;
   WORD wResult = SQLGetInfo( ( HDBC ) hb_parnl( 1 ), ( UWORD ) hb_parnl( 2 ), bBuffer, 512, &wLen );

   hb_storclen( bBuffer, wLen, 3 );
   hb_retni( wResult );
}

HB_FUNC( SQLSETCONNECTOPTION ) // hDbc, nOption, uOption
{
   hb_retnl( ( LONG ) SQLSetConnectOption( ( HDBC ) hb_parnl( 1 ), ( UWORD ) hb_parnl( 2 ),
           ( UDWORD ) ISCHAR( 3 ) ? ( LONG ) hb_parc( 3 ) : hb_parnl( 3 ) ) );
}

HB_FUNC( SQLSETSTMTOPTION ) // hStmt, nOption, uOption )  --> nRetCode
{
   hb_retnl( ( LONG ) SQLSetStmtOption( ( SQLHSTMT ) hb_parnl( 1 ), ( UWORD ) hb_parnl( 2 ),
           ( UDWORD ) ISCHAR( 3 ) ? ( LONG ) hb_parc( 3 ) : hb_parnl( 3 ) ) );
}

HB_FUNC( SQLGETCONNECTOPTION ) // hDbc, nOption, @cOption
{
   BYTE bBuffer[ 512 ];
   WORD wResult = SQLGetConnectOption( ( HDBC ) hb_parnl( 1 ), hb_parni( 2 ), bBuffer );
   if( wResult == SQL_SUCCESS )
      hb_storclen( bBuffer, 512, 3 );

   hb_retni( wResult );
}

HB_FUNC( SQLGETSTMTOPTION ) // hStmt, nOption, @cOption
{
   BYTE bBuffer[ 512 ];
   WORD wResult = SQLGetStmtOption( ( SQLHSTMT ) hb_parnl( 1 ), hb_parni( 2 ), bBuffer );

   if( wResult == SQL_SUCCESS )
    hb_storclen( bBuffer, 512,3 );

   hb_retni( wResult );
}

HB_FUNC( SQLCOMMIT ) // hEnv, hDbc
{
   hb_retni( SQLTransact( ( HENV ) hb_parnl( 1 ), ( HDBC ) hb_parnl( 2 ), SQL_COMMIT ) );
}

HB_FUNC( SQLROLLBACK )  // hEnv, hDbc
{
   hb_retni( SQLTransact( ( HENV ) hb_parnl( 1 ), ( HDBC ) hb_parnl( 2 ), SQL_ROLLBACK ) );
}

#endif
