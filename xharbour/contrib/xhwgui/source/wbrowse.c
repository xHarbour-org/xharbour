/*
 * HWGUI - Harbour Win32 GUI library source code:
 * C level browse functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define OEMRESOURCE
#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

LRESULT CALLBACK BrwProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

/* SetScrollInfo( hWnd, nType, nRedraw, nPos, nPage )
*/
HB_FUNC ( SETSCROLLINFO )
{
   SCROLLINFO si;
   UINT fMask = (hb_pcount()<4)? SIF_DISABLENOSCROLL:0;

   if( hb_pcount() > 3 && !ISNIL( 4 ) )
   {
      si.nPos = hb_parni( 4 );
      fMask |= SIF_POS;
   }
   if( hb_pcount() > 4 && !ISNIL( 5 ) )
   {
      si.nPage = hb_parni( 5 );
      fMask |= SIF_PAGE;
   }

   if( hb_pcount() > 5 && !ISNIL( 6 ) )
   {
      si.nMin = 1;
      si.nMax = hb_parni( 6 );
      fMask |= SIF_RANGE;
   }

   si.cbSize = sizeof( SCROLLINFO );
   si.fMask = fMask;

   SetScrollInfo(
    (HWND) hb_parnl( 1 ), // handle of window with scroll bar
    hb_parni( 2 ),	  // scroll bar flags
    &si, hb_parni( 3 )    // redraw flag
   );
}

HB_FUNC ( GETSCROLLRANGE )
{

   int MinPos, MaxPos;

   GetScrollRange(
    (HWND) hb_parnl( 1 ),	// handle of window with scroll bar
    hb_parni( 2 ),	// scroll bar flags
    &MinPos,	// address of variable that receives minimum position
    &MaxPos 	// address of variable that receives maximum position
   );
   hb_storni( MinPos, 3 );
   hb_storni( MaxPos, 4 );
}

HB_FUNC ( GETSCROLLPOS )
{

   hb_retni( GetScrollPos(
               (HWND) hb_parnl( 1 ),	// handle of window with scroll bar
               hb_parni( 2 )	// scroll bar flags
             ) );
}

HB_FUNC ( SETSCROLLPOS )
{

   SetScrollPos(
      (HWND) hb_parnl( 1 ),	// handle of window with scroll bar
      hb_parni( 2 ),	// scroll bar flags
      hb_parni( 3 ),
      TRUE
   );
}

/*
HB_FUNC ( SETWINDOWPOS )
{ 
   HWND hwndParent = (HWND) hb_parnl( 1 ), hwndHeader = (HWND) hb_parnl( 2 );
   RECT rcParent; 
   HD_LAYOUT hdl; 
   WINDOWPOS wp; 

   GetClientRect(hwndParent, &rcParent); 

   hdl.prc = &rcParent; 
   hdl.pwpos = &wp; 
   if (!SendMessage(hwndHeader, HDM_LAYOUT, 0, (LPARAM) &hdl)) 
   {
       hb_retnl( 0 );
       return;
   }

   // Set the size, position, and visibility of the header control. 
   SetWindowPos(hwndHeader, wp.hwndInsertAfter, wp.x, wp.y, 
       wp.cx, wp.cy, wp.flags | SWP_SHOWWINDOW);

}
*/

LRESULT CALLBACK BrwProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{

   PHB_DYNS pSymTest;
   long int res;

   if( ( pSymTest = hb_dynsymFind( "BRWPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();                 /* places NIL at self */
      hb_vmPushLong( (LONG ) hWnd );    /* pushes parameters on to the hvm stack */
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );  /* where iArgCount is the number of pushed parameters */
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      if( res == -1 )
         return( DefWindowProc( hWnd, message, wParam, lParam ));
      else
         return res;
    }
    else
       return( DefWindowProc( hWnd, message, wParam, lParam ));
}

BOOL RegisterBrowse(void)
{

   static TCHAR szAppName[] = TEXT ( "BROWSE" );
   WNDCLASS     wndclass ;

   wndclass.style = CS_OWNDC | CS_VREDRAW | CS_HREDRAW | CS_DBLCLKS;
   wndclass.lpfnWndProc   = BrwProc ;
   wndclass.cbClsExtra    = 0 ;
   wndclass.cbWndExtra    = 0 ;
   wndclass.hInstance     = GetModuleHandle( NULL );
   wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
   wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
   wndclass.hbrBackground = (HBRUSH)( COLOR_WINDOW+1 );
   wndclass.lpszMenuName  = NULL;
   wndclass.lpszClassName = szAppName ;

   return RegisterClass (&wndclass);
}
