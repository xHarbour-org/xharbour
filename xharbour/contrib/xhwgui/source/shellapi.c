/*
 * HWGUI - Harbour Win32 GUI library source code:
 * Shell API wrappers
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
// #define OEMRESOURCE
#include <windows.h>
#include <shlobj.h>
#include "hbapi.h"
#include "hbapiitm.h"

#define  ID_NOTIFYICON   1
#define  WM_NOTIFYICON   WM_USER+1000

#ifndef BIF_USENEWUI
#ifndef BIF_NEWDIALOGSTYLE
#define BIF_NEWDIALOGSTYLE     0x0040   // Use the new dialog layout with the ability to resize
#endif
#define BIF_USENEWUI           (BIF_NEWDIALOGSTYLE | BIF_EDITBOX)
#endif

/*
 *  SelectFolder( cTitle )
 */

HB_FUNC( SELECTFOLDER )
{ 
   BROWSEINFO bi; 
   char *lpBuffer = (char*) hb_xgrab( MAX_PATH+1 );
   LPITEMIDLIST pidlBrowse;    // PIDL selected by user 

    bi.hwndOwner = GetActiveWindow();
    bi.pidlRoot = NULL; 
    bi.pszDisplayName = lpBuffer; 
    bi.lpszTitle = ( ISCHAR(1) )? hb_parc(1):"";
    bi.ulFlags = BIF_USENEWUI;
    bi.lpfn = NULL; 
    bi.lParam = 0; 
 
    // Browse for a folder and return its PIDL. 
    pidlBrowse = SHBrowseForFolder( &bi );
    SHGetPathFromIDList( pidlBrowse,lpBuffer );
    hb_retc( lpBuffer );
    hb_xfree( lpBuffer );
}

/*
 *  ShellNotifyIcon( lAdd, hWnd, hIcon, cTooltip )
 */

HB_FUNC ( SHELLNOTIFYICON )
{
   NOTIFYICONDATA tnid;

   memset( (void*) &tnid, 0, sizeof( NOTIFYICONDATA ) );

   tnid.cbSize = sizeof( NOTIFYICONDATA );
   tnid.hWnd   = (HWND) hb_parnl(2);
   tnid.uID    = ID_NOTIFYICON;
   tnid.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP;
   tnid.uCallbackMessage = WM_NOTIFYICON;
   tnid.hIcon  = (HICON) hb_parnl(3);
   if( ISCHAR(4) )
      lstrcpy( tnid.szTip,TEXT(hb_parc(4)) );

   if( (BOOL) hb_parl(1) )
      Shell_NotifyIcon( NIM_ADD,&tnid );
   else
      Shell_NotifyIcon( NIM_DELETE,&tnid );
}

/*
 * ShellExecute( cFile, cOperation, cParams, cDir, nFlag )
 */
HB_FUNC( SHELLEXECUTE )
{
   hb_retnl( (LONG) ShellExecute( GetActiveWindow(),
              ISNIL(2) ? NULL : (LPCSTR) hb_parc(2),
              (LPCSTR) hb_parc(1),
              ISNIL(3) ? NULL : (LPCSTR) hb_parc(3),
              ISNIL(4) ? "C:\\" : (LPCSTR) hb_parc(4),
              ISNIL(5) ? 1 : hb_parni(5) ) ) ;
}
