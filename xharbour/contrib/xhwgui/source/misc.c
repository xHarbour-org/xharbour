/*
 *$Id: misc.c,v 1.2 2004/02/07 15:24:22 lculik Exp $
 * HWGUI - Harbour Win32 GUI library source code:
 * Miscellaneous functions
 *
 * Copyright 2003 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define OEMRESOURCE
#include <windows.h>
#include <commctrl.h>
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "item.api"
#include "guilib.h"

void writelog( char* s )
{
   FHANDLE handle;

   if( hb_fsFile( (unsigned char *) "ac.log" ) )
      handle = hb_fsOpen( (unsigned char *) "ac.log", FO_WRITE );
   else
      handle = hb_fsCreate( (unsigned char *) "ac.log", 0 );

   hb_fsSeek( handle,0, SEEK_END );
   hb_fsWrite( handle, (unsigned char *) s, strlen(s) );
   hb_fsWrite( handle, (unsigned char *) "\n\r", 2 );

   hb_fsClose( handle );
}

HB_FUNC ( HWG_SETDLGRESULT )
{
   SetWindowLong( (HWND) hb_parnl(1), DWL_MSGRESULT, hb_parni(2) );
}

HB_FUNC ( SETCAPTURE )
{
   hb_retnl( (LONG) SetCapture( (HWND) hb_parnl(1) ) );
}

HB_FUNC ( RELEASECAPTURE )
{
   hb_retl( ReleaseCapture() );
}

HB_FUNC ( COPYSTRINGTOCLIPBOARD )
{
   HGLOBAL hglbCopy;
   char * lptstrCopy;
   char * cStr = hb_parc( 1 );
   int nLen = strlen( cStr );


   if ( !OpenClipboard( GetActiveWindow() ) )
      return;

   EmptyClipboard(); 

   hglbCopy = GlobalAlloc( GMEM_DDESHARE, (nLen+1) * sizeof(TCHAR) );
   if (hglbCopy == NULL) 
   { 
       CloseClipboard(); 
       return;
   } 

   // Lock the handle and copy the text to the buffer. 
 
   lptstrCopy = (char*) GlobalLock( hglbCopy );
   memcpy( lptstrCopy, cStr, nLen * sizeof(TCHAR)); 
   lptstrCopy[nLen] = (TCHAR) 0;    // null character 
   GlobalUnlock(hglbCopy); 
 
   // Place the handle on the clipboard. 
   SetClipboardData( CF_TEXT, hglbCopy );

   CloseClipboard(); 
 
}

HB_FUNC ( GETSTOCKOBJECT )
{
   hb_retnl( (LONG) GetStockObject( hb_parni(1) ) );
}

HB_FUNC ( LOWORD )
{
   hb_retni( (int) ( hb_parnl( 1 ) & 0xFFFF ) );
}

HB_FUNC ( HIWORD )
{
   hb_retni( (int) ( ( hb_parnl( 1 ) >> 16 ) & 0xFFFF ) );
}

HB_FUNC( HWG_BITOR )
{
   hb_retnl( hb_parnl(1) | hb_parnl(2) );
}

HB_FUNC( HWG_BITAND )
{
   hb_retnl( hb_parnl(1) & hb_parnl(2) );
}

HB_FUNC( HWG_BITANDINVERSE )
{
   hb_retnl( hb_parnl(1) & (~hb_parnl(2)) );
}

HB_FUNC ( SETBIT )
{
   if( hb_pcount() < 3 || hb_parni( 3 ) )
      hb_retnl( hb_parnl(1) | ( 1 << (hb_parni(2)-1) ) );
   else
      hb_retnl( hb_parnl(1) & ~( 1 << (hb_parni(2)-1) ) );
}

HB_FUNC ( CHECKBIT )
{
   hb_retl( hb_parnl(1) & ( 1 << (hb_parni(2)-1) ) );
}

HB_FUNC( HWG_SIN )
{
   hb_retnd( sin( hb_parnd(1) ) );
}

HB_FUNC( HWG_COS )
{
   hb_retnd( cos( hb_parnd(1) ) );
}

HB_FUNC( CLIENTTOSCREEN )
{
   POINT pt;
   PHB_ITEM aPoint = _itemArrayNew( 2 );
   PHB_ITEM temp;

   pt.x = hb_parnl(2);
   pt.y = hb_parnl(3);
   ClientToScreen( (HWND) hb_parnl(1), &pt );

   temp = _itemPutNL( NULL, pt.x );
   _itemArrayPut( aPoint, 1, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, pt.y );
   _itemArrayPut( aPoint, 2, temp );
   _itemRelease( temp );

   _itemReturn( aPoint );
   _itemRelease( aPoint );
}

HB_FUNC( SCREENTOCLIENT )
{
   POINT pt;
   PHB_ITEM aPoint = _itemArrayNew( 2 );
   PHB_ITEM temp;

   pt.x = hb_parnl(2);
   pt.y = hb_parnl(3);
   ScreenToClient( (HWND) hb_parnl(1), &pt );

   temp = _itemPutNL( NULL, pt.x );
   _itemArrayPut( aPoint, 1, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, pt.y );
   _itemArrayPut( aPoint, 2, temp );
   _itemRelease( temp );

   _itemReturn( aPoint );
   _itemRelease( aPoint );
}

HB_FUNC ( HWG_GETCURSORPOS )
{
   POINT pt;
   PHB_ITEM aPoint = _itemArrayNew( 2 );
   PHB_ITEM temp;

   GetCursorPos( &pt );

   temp = _itemPutNL( NULL, pt.x );
   _itemArrayPut( aPoint, 1, temp );
   _itemRelease( temp );

   temp = _itemPutNL( NULL, pt.y );
   _itemArrayPut( aPoint, 2, temp );
   _itemRelease( temp );

   _itemReturn( aPoint );
   _itemRelease( aPoint );

}

HB_FUNC ( GETCURRENTDIR )
{
   BYTE pbyBuffer[ _POSIX_PATH_MAX + 1 ];
   GetCurrentDirectory( _POSIX_PATH_MAX, ( char * ) pbyBuffer );
   hb_retc( pbyBuffer );
}

HB_FUNC ( WINEXEC )
{
   hb_retni( WinExec( (LPCSTR) hb_parc(1), (UINT) hb_parni(2) ) );
}

HB_FUNC ( GETKEYBOARDSTATE )
{
   BYTE lpbKeyState[256];
   GetKeyboardState( lpbKeyState );
   lpbKeyState[255] = '\0';
   hb_retclen( lpbKeyState,255 );
}

/* Functions Contributed  By Luiz Rafael Culik Guimaraes( culikr@uol.com.br) */

HB_FUNC( GETWINDOWSDIR )
{
   char szBuffer[ MAX_PATH + 1 ] = {0} ;
   GetWindowsDirectory( szBuffer,MAX_PATH);
   hb_retc(szBuffer);
}

HB_FUNC( GETSYSTEMDIR )
{
   char szBuffer[ MAX_PATH + 1 ] = {0} ;
   GetSystemDirectory( szBuffer,MAX_PATH);
   hb_retc(szBuffer);
}

HB_FUNC( GETTEMPDIR )
{
   char szBuffer[ MAX_PATH + 1 ] = {0} ;
   GetTempPath(MAX_PATH, szBuffer);
   hb_retc(szBuffer);
}

/* Contributed by Rodrigo Moreno rodrigo_moreno@yahoo.com base upon code minigui */

HB_FUNC( SHELLABOUT )
{
   ShellAbout( 0, hb_parc( 1 ), hb_parc( 2 ), (HICON) hb_parnl(3) );
}

HB_FUNC (GETDESKTOPWIDTH) 
{
   hb_retni ( GetSystemMetrics(SM_CXSCREEN) ) ;
}

HB_FUNC (GETDESKTOPHEIGHT)
{
   hb_retni ( GetSystemMetrics(SM_CYSCREEN) ) ;
}
