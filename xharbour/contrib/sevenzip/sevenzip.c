/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SevenZip xHarbour Interface
 *
 * Copyright 2011 Andi Jahja <andi.jahja@yahoo.co.id>
 * www - http://www.xharbour.org
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

#include "hbapi.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "windows.h"

static HINSTANCE hDll = NULL;

#define SEVENZIPDLL "7-zip32.dll"

typedef	HGLOBAL	HARC;

typedef	WORD  ( WINAPI *SEVENZIPGETVERSION ) ( void );
typedef WORD  ( WINAPI *SEVENZIPGETSUBVERSION ) ( void );
typedef int   ( WINAPI *SEVENZIPGETARCHIVETYPE ) ( LPCSTR _szFileName );
typedef BOOL  ( WINAPI *SEVENZIPSETUNICODEMODE ) ( BOOL _bUnicode );
typedef int   ( WINAPI *SEVENZIPSETDEFAULTPASSWORD ) ( HARC _harc, LPCSTR _szPassword );
typedef DWORD ( WINAPI *SEVENZIPGETDEFAULTPASSWORD ) ( HARC _harc, LPSTR _szPassword, DWORD _dwSize );
typedef HARC  ( WINAPI *SEVENZIPOPENARCHIVE ) ( const HWND _hwnd, LPCSTR _szFileName, const DWORD _dwMode );
typedef int   ( WINAPI *SEVENZIPCLOSEARCHIVE ) ( HARC _harc );
typedef BOOL  ( WINAPI *SEVENZIPSETPRIORITY ) ( const int _nPriority );
typedef int   ( WINAPI *SEVENZIP) ( const HWND _hwnd, LPCSTR _szCmdLine, LPSTR _szOutput, const DWORD _dwSize );
typedef BOOL  ( WINAPI *SEVENZIPGETCURSORMODE ) ( void );
typedef BOOL  ( WINAPI *SEVENZIPSETCURSORMODE ) ( const BOOL _CursorMode );
typedef DWORD ( WINAPI *SEVENZIPGETARCFILESIZE ) (HARC _harc);
typedef DWORD ( WINAPI *SEVENZIPGETARCORIGINALSIZE ) (HARC _harc);
typedef DWORD ( WINAPI *SEVENZIPGETARCCOMPRESSEDSIZE ) (HARC _harc);
typedef WORD  ( WINAPI *SEVENZIPGETARCRATIO ) (HARC _harc);
typedef DWORD ( WINAPI *SEVENZIPGETORIGINALSIZE ) (HARC _harc);
typedef DWORD ( WINAPI *SEVENZIPGETCOMPRESSEDSIZE ) (HARC _harc);
typedef WORD  ( WINAPI *SEVENZIPGETRATIO ) (HARC _harc);

static UINT s_7ZipWindowMessage = 0;

HB_EXTERN_BEGIN
UINT hb_7zipmessage( void );
HB_EXTERN_END

//------------------------------------------------------------------------------
static FARPROC sevenzip_GetProcAddress( const char* szFuncName )
{
   FARPROC pFunc = GetProcAddress( hDll, szFuncName );

   if ( ! pFunc )
   {
      char __szError[256];
      hb_snprintf( __szError, sizeof( __szError ), "Cannot find function: %s in %s", szFuncName, SEVENZIPDLL );
      hb_errInternal( 5999, __szError, NULL, NULL );
      exit( 0 );
   }

   return pFunc;
}

//------------------------------------------------------------------------------
HB_FUNC( INIT7ZIPDLL )
{
   if( hDll == NULL )
      hDll = LoadLibrary( (LPCSTR) SEVENZIPDLL );

   if( !hDll )
   {
      char __szError[256];
      hb_snprintf( __szError, sizeof( __szError ), "Cannot load %s", SEVENZIPDLL );
      hb_errInternal( 5999, __szError, NULL, NULL );
   }

   s_7ZipWindowMessage = RegisterWindowMessage( "wm_arcextract" );
}

//------------------------------------------------------------------------------
UINT hb_7zipmessage( void )
{
   return s_7ZipWindowMessage;
}

//------------------------------------------------------------------------------
HB_FUNC( HB_7ZIPMESSAGE )
{
   if ( hDll )
   {
      hb_retnl( hb_7zipmessage() );
      return;
   }

   hb_retnl( 0 );
}

//------------------------------------------------------------------------------
HB_FUNC( EXIT7ZIPDLL )
{
   if( hDll )
   {
      FreeLibrary( hDll );
      hDll = NULL;
   }
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPGETVERSION )
{
#if defined( __cplusplus )
   static SEVENZIPGETVERSION
#else
   static SEVENZIPGETVERSION pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETVERSION) sevenzip_GetProcAddress( "SevenZipGetVersion" );

   hb_retnl( pFunc() );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPGETSUBVERSION )
{
#if defined( __cplusplus )
   static SEVENZIPGETSUBVERSION
#else
   static SEVENZIPGETSUBVERSION pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETSUBVERSION) sevenzip_GetProcAddress( "SevenZipGetSubVersion" );

   hb_retnl( pFunc() );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPGETARCHIVETYPE )
{
#if defined( __cplusplus )
   static SEVENZIPGETARCHIVETYPE
#else
   static SEVENZIPGETARCHIVETYPE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETARCHIVETYPE) sevenzip_GetProcAddress( "SevenZipGetArchiveType" );

   if ( pFunc )
      hb_retni( pFunc( hb_parcx( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPSETUNICODEMODE )
{
#if defined( __cplusplus )
   static SEVENZIPSETUNICODEMODE
#else
   static SEVENZIPSETUNICODEMODE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPSETUNICODEMODE) sevenzip_GetProcAddress( "SevenZipSetUnicodeMode" );

   hb_retl( pFunc( hb_parl( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPSETDEFAULTPASSWORD )
{
#if defined( __cplusplus )
   static SEVENZIPSETDEFAULTPASSWORD
#else
   static SEVENZIPSETDEFAULTPASSWORD pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPSETDEFAULTPASSWORD) sevenzip_GetProcAddress( "SevenZipSetDefaultPassword" );

   hb_retni( pFunc( (void*) hb_parns( 1 ), (LPCSTR) hb_parc( 2 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPGETDEFAULTPASSWORD )
{
   #define PASSWORD_LENGTH 256
   char *szPassword = (char*) hb_xgrab( PASSWORD_LENGTH );

#if defined( __cplusplus )
   static SEVENZIPGETDEFAULTPASSWORD
#else
   static SEVENZIPGETDEFAULTPASSWORD pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETDEFAULTPASSWORD) sevenzip_GetProcAddress( "SevenZipGetDefaultPassword" );

   pFunc( (void*) hb_parns( 1 ), szPassword, PASSWORD_LENGTH );
   hb_retcAdopt( szPassword );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPOPENARCHIVE )
{
#if defined( __cplusplus )
   static SEVENZIPOPENARCHIVE
#else
   static SEVENZIPOPENARCHIVE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPOPENARCHIVE) sevenzip_GetProcAddress( "SevenZipOpenArchive" );

#if defined( __MINGW32__ ) && defined( HB_OS_WIN_64 )
   hb_retnl( (long) ( HB_LONG ) pFunc( (HWND) hb_parns( 1 ), (LPCSTR) hb_parc( 2 ), ( const DWORD) hb_parns( 3 ) ) );
#else
   hb_retnl( (LONG) pFunc( (HWND) hb_parns( 1 ), (LPCSTR) hb_parc( 2 ), ( const DWORD) hb_parns( 3 ) ) );
#endif
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPCLOSEARCHIVE )
{
#if defined( __cplusplus )
   static SEVENZIPCLOSEARCHIVE
#else
   static SEVENZIPCLOSEARCHIVE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPCLOSEARCHIVE) sevenzip_GetProcAddress( "SevenZipCloseArchive" );

   hb_retni( pFunc( (HARC) hb_parns( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPSETPRIORITY )
{
#if defined( __cplusplus )
   static SEVENZIPSETPRIORITY
#else
   static SEVENZIPSETPRIORITY pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPSETPRIORITY) sevenzip_GetProcAddress( "SevenZipSetPriority" );

   hb_retl( pFunc( hb_parni( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPGETCURSORMODE )
{
#if defined( __cplusplus )
   static SEVENZIPGETCURSORMODE
#else
   static SEVENZIPGETCURSORMODE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETCURSORMODE) sevenzip_GetProcAddress( "SevenZipGetCursorMode" );

   hb_retl( pFunc() );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPSETCURSORMODE )
{
#if defined( __cplusplus )
   static SEVENZIPSETCURSORMODE
#else
   static SEVENZIPSETCURSORMODE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPSETCURSORMODE) sevenzip_GetProcAddress( "SevenZipSetCursorMode" );

   hb_retl( pFunc( hb_parl( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIP )
{
   ULONG uOut = hb_parnl( 4 );
   LPSTR szOut = (LPSTR) hb_xgrab( ( uOut ? uOut : 1024 ) + 1 );

#if defined( __cplusplus )
   static SEVENZIP
#else
   static SEVENZIP pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIP) sevenzip_GetProcAddress( "SevenZip" );

   hb_retni( pFunc( (const HWND) hb_parns( 1 ), (LPCSTR) hb_parc( 2 ), szOut, (const DWORD) uOut ) );

   if ( ISBYREF ( 3 ) )
      hb_storc( szOut, 3 );

   hb_xfree( szOut );
}

//------------------------------------------------------------------------------
HB_FUNC( HB_SEVENZIPGETARCFILESIZE )
{
#if defined( __cplusplus )
   static SEVENZIPGETARCFILESIZE
#else
   static SEVENZIPGETARCFILESIZE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETARCFILESIZE) sevenzip_GetProcAddress( "SevenZipGetArcFileSize" );

   hb_retnl( (LONG) pFunc( (HARC) hb_parns( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC ( HB_SEVENZIPGETARCORIGINALSIZE )
{
#if defined( __cplusplus )
   static SEVENZIPGETARCORIGINALSIZE
#else
   static SEVENZIPGETARCORIGINALSIZE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETARCORIGINALSIZE) sevenzip_GetProcAddress( "SevenZipGetArcOriginalSize" );

   hb_retnl( (LONG) pFunc( (HARC) hb_parns( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC ( HB_SEVENZIPGETARCCOMPRESSEDSIZE )
{
#if defined( __cplusplus )
   static SEVENZIPGETARCCOMPRESSEDSIZE
#else
   static SEVENZIPGETARCCOMPRESSEDSIZE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETARCCOMPRESSEDSIZE) sevenzip_GetProcAddress( "SevenZipGetArcCompressedSize" );

   hb_retnl( (LONG) pFunc( (HARC) hb_parns( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC ( HB_SEVENZIPGETARCRATIO )
{
#if defined( __cplusplus )
   static SEVENZIPGETARCRATIO
#else
   static SEVENZIPGETARCRATIO pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETARCRATIO) sevenzip_GetProcAddress( "SevenZipGetArcRatio" );

   hb_retnl( (LONG) pFunc( (HARC) hb_parns( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC ( HB_SEVENZIPGETORIGINALSIZE )
{
#if defined( __cplusplus )
   static SEVENZIPGETORIGINALSIZE
#else
   static SEVENZIPGETORIGINALSIZE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETORIGINALSIZE) sevenzip_GetProcAddress( "SevenZipGetOriginalSize" );

   hb_retnl( (LONG) pFunc( (HARC) hb_parns( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC ( HB_SEVENZIPGETCOMPRESSEDSIZE )
{
#if defined( __cplusplus )
   static SEVENZIPGETCOMPRESSEDSIZE
#else
   static SEVENZIPGETCOMPRESSEDSIZE pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETCOMPRESSEDSIZE) sevenzip_GetProcAddress( "SevenZipGetCompressedSize" );

   hb_retnl( (LONG) pFunc( (HARC) hb_parns( 1 ) ) );
}

//------------------------------------------------------------------------------
HB_FUNC ( HB_SEVENZIPGETRATIO )
{
#if defined( __cplusplus )
   static SEVENZIPGETRATIO
#else
   static SEVENZIPGETRATIO pFunc = NULL;
   if ( ! pFunc )
#endif
      pFunc = (SEVENZIPGETRATIO) sevenzip_GetProcAddress( "SevenZipGetRatio" );

   hb_retnl( (LONG) pFunc( (HARC) hb_parns( 1 ) ) );
}
