/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
* C window low level Windows API interface and functions
*
*/
#define _WIN32_WINNT 0x0400
#define WINVER 0x0400
#define _WIN32_IE 0x0501

#include <shlobj.h>

#include <winuser.h>
#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "item.api"

#include "woopgui.h"

// Global Variables:

static HINSTANCE hInst;      // current instance
static char *szAppName = "PROVA";      // Name of the application
//static char *cClassName;     // Name of the application
LPINITCOMMONCONTROLSEX lpInitCtrls;  // Dichiarazione per l'inizializzazione dei controlli

// Foward declarations of functions included in this code module:
LRESULT CALLBACK WG_WndProc(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK WG_DlgProc(HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY WG_EdtProc( HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam );

static WNDPROC wpOriginalEditProc;

WINUSERAPI HWND WINAPI GetAncestor(
    IN HWND hwnd,
    IN UINT gaFlags
);

extern PHB_ITEM Rect2Array( RECT *rc  );
extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM Point2Array( POINT *pt  );
extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );
extern BOOL Array2Size(PHB_ITEM aSize, SIZE *siz );
extern PHB_ITEM Size2Array( SIZE *siz  );
extern void Point2ArrayEx( POINT *pt  , PHB_ITEM aPoint);
extern void Rect2ArrayEx( RECT *pt  , PHB_ITEM aRect);
extern void Size2ArrayEx( SIZE *siz  ,  PHB_ITEM aSize);


/*

//
// RegisterClassEx( cClassName, cIcon, nBkColor, cIconSm ) --> nInstance
//
HB_FUNC ( REGISTERCLASSEX )
{
    WNDCLASSEX wcex;
    HANDLE hInstance = GetModuleHandle( NULL );
    LPCTSTR cClassName;
    char *cIcon, *cIconSm, *cWndProc;
    long nBkColor;
    WNDPROC nlpWinProc;

    hInst = hInstance;

    cClassName         = ( hb_pcount()>0 && !ISNIL(1) ) ? hb_parc(1) : "WoopGUIWinClass" ;
    cIcon              = ( hb_pcount()>1 && !ISNIL(2) ) ? hb_parc(2) : "" ;
    nBkColor           = ( hb_pcount()>2 && !ISNIL(3) ) ? hb_parnl(3) : COLOR_BTNFACE; //COLOR_WINDOW ;
    cIconSm            = ( hb_pcount()>3 && !ISNIL(4) ) ? hb_parc(4) : "" ;
    cWndProc           = ( hb_pcount()>4 && !ISNIL(5) ) ? hb_parc(5) : "W_WndProc" ;

    nlpWinProc         = ( cWndProc == "W_DlgProc" ) ? W_DlgProc : W_WndProc ;

    if (szAppName == NULL)
    {
       lstrcpy (szAppName, cClassName);
    }

    wcex.cbSize        = sizeof(WNDCLASSEX);
    wcex.style         = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
    wcex.lpfnWndProc   = nlpWinProc;
    wcex.cbClsExtra    = 0;
    wcex.cbWndExtra    = 0;
    wcex.hInstance     = hInstance;
    wcex.hIcon         = ( cIcon == "" ? LoadIcon(NULL, IDI_APPLICATION) : LoadIcon (hInstance, cIcon ) );
    wcex.hCursor       = LoadCursor(NULL, IDC_ARROW);
    wcex.hbrBackground = (HBRUSH)( nBkColor + 1 );
    wcex.lpszMenuName  = NULL;
    wcex.lpszClassName = cClassName;
    wcex.hIconSm       = ( cIconSm == "" ? LoadIcon(NULL, IDI_APPLICATION) : LoadIcon (hInstance, cIconSm ) );

    if ( ! RegisterClassEx(&wcex) )
    {
       MessageBox(0, "Window Registration Failed!", "Error!",
       MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return;
    }

    hb_retnl( (LONG) hInstance );
}
*/

//
// RegisterClassEx( cClassName, cIcon, nBkColor, cIconSm ) --> nInstance
//
HB_FUNC ( REGISTERCLASSEX )
{
    WNDCLASSEX wcex;

    UINT      nStyle        = (UINT)      hb_parnl(1);    // Specifies the class style(s). This member can be any combination of the class styles.
    WNDPROC   cWndProc      = WG_WndProc; //(WNDPROC)   hb_parnl(2);    // Pointer to the window procedure
    //HINSTANCE hInstance     = (HINSTANCE) hb_parnl(3);    // Handle to the instance that contains the window procedure for the class
    HICON     hIcon         = (HICON)     hb_parnl(4);    // Handle to the class icon. This member must be a handle to an icon resource. If this member is NULL, the system provides a default icon
    HCURSOR   hCursor       = ( !ISNIL(5) ? (HCURSOR) hb_parnl(5) : LoadCursor(NULL, IDC_ARROW) );    // Handle to the class cursor. This member must be a handle to a cursor resource. If this member is NULL, an application must explicitly set the cursor shape whenever the mouse moves into the application's window.
    int       hbrBackground =             hb_parni(6);    // Handle to the class background brush
    LPCTSTR   cMenuName     = (LPCTSTR)   hb_parc(7);     // Pointer to a null-terminated character string that specifies the resource name of the class menu, as the name appears in the resource file. If you use an integer to identify the menu, use the MAKEINTRESOURCE macro. If this member is NULL, windows belonging to this class have no default menu
    LPCTSTR   cClassName    = (LPCTSTR)   hb_parc(8);     // Pointer to a null-terminated string
    HICON     hIconSm       = (HICON)     hb_parnl(9);    // Handle to a small icon that is associated with the window class. If this member is NULL, the system searches the icon resource specified by the hIcon member for an icon of the appropriate size to use as the small icon

    HINSTANCE hInstance = GetModuleHandle( NULL );

    wcex.cbSize        = sizeof(WNDCLASSEX);
    wcex.style         = nStyle;
    wcex.lpfnWndProc   = cWndProc;
    wcex.cbClsExtra    = 0;
    wcex.cbWndExtra    = 0;
    wcex.hInstance     = hInstance;
    wcex.hIcon         = hIcon;
    wcex.hCursor       = hCursor;
    wcex.hbrBackground = (HBRUSH) (hbrBackground);
    wcex.lpszMenuName  = cMenuName;
    wcex.lpszClassName = cClassName;
    wcex.hIconSm       = hIconSm;

    hInst = hInstance;

    if ( ! RegisterClassEx(&wcex) )
    {
       WG_error("Window Registration Failed!", "RegisterClassEx");
       //MessageBox(0, "Window Registration Failed!", "Error!",
       //MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return;
    }

    hb_retnl( (LONG) hInstance );
}


//
// UnRegisterClassEx( cClassName, nHinstance ) --> lOk
//
HB_FUNC ( UNREGISTERCLASS )
{

    LPCTSTR cClassName = (LPCTSTR) hb_parc(1);
    HANDLE hInstance   = (HANDLE)  hb_parnl(2);
    BOOL   lOk ;

    if ( ! ( lOk = UnregisterClass(cClassName,(HINSTANCE)hInstance ) ) )
    {
       MessageBox(0, "Window Registration Failed!", "Error!",
       MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return;
    }

    hb_retl( (BOOL) lOk );
}


// The BringWindowToTop function brings the specified window to the top of the Z order.
// If the window is a top-level window, it is activated. If the window is a child window,
// the top-level parent window associated with the child window is activated.
//
HB_FUNC ( BRINGWINDOWTOTOP )
{
    HWND hWnd = (HWND) hb_parnl (1);         // handle to window

    hb_retl( (BOOL) BringWindowToTop( hWnd ) );
}


//void DefineInstance( HINSTANCE hInstance )
//{
//   //hInst = hInstance;
//}

// Creates an overlapped, pop-up, or child window with an extended window style.
//

/*
HB_FUNC ( CREATEWINDOWEX )
{
    //CLIENTCREATESTRUCT ccs;
                           // Bisogna definire i defaults
    HWND hwnd;
    DWORD nExStyle       = hb_parnd(1);
    LPCTSTR lcClassName  = (!ISNIL(2) ? (LPCTSTR) hb_parc(2) : szAppName );
    LPCTSTR cName        = (LPCTSTR) hb_parc(3);
    DWORD nStyle         = (DWORD) hb_parnd(4);
    int   nX             = hb_parni(6);
    int   nY             = hb_parni(5);
    int   nWidth         = hb_parni(7);
    int   nHeight        = hb_parni(8);
    HWND  nParent        = (HWND)  hb_parnl(9);
    HMENU nMenu          = (HMENU) hb_parnl(10);
    HINSTANCE hInstance  = hInst;
    LPVOID pStruct       = NULL;   // da sistemare

    HB_TRACE(HB_TR_DEBUG, ("CreateWindowsEx()"));

    //warning("Prima della creazione della finestra", "CreateWindowEx" );

    hwnd = CreateWindowEx( nExStyle,
                           lcClassName,
                           cName,
                           nStyle,
                           nX,
                           nY,
                           nWidth,
                           nHeight,
                           nParent,
                           nMenu,
                           hInstance,
                           pStruct);

    //warning("Dopo la creazione della finestra", "CreateWindowEx" );

    if(hwnd == NULL)
    {
      WG_error( (LPCTSTR) "Window Creation", (LPCTSTR) cName );
      //MessageBox(0, "Window Creation Failed!", "Error!",
      //MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return;
    }

    hb_retnl ((LONG)hwnd);
}
*/

// Funzioni di Window

/*
HB_FUNC ( ADJUSTWINDOWRECTEX )
{
    LPRECT lpRect =

    hb_retl( (BOOL) AdjustWindowRectEx(
                                        LPRECT lpRect,    // client-rectangle structure
                                        DWORD dwStyle,    // window styles
                                        BOOL bMenu,       // menu-present option
                                        DWORD dwExStyle   // extended window style
                                      )
           );

}
*/
/*
HB_FUNC ( WG_CENTERWINDOW )
{
    RECT rect;
    int w, h, x, y;
    HWND hWnd = (HWND) hb_parnl (1);

    GetWindowRect(hWnd, &rect);

    w = rect.right  - rect.left;
    h = rect.bottom - rect.top;
    x = GetSystemMetrics(SM_CXSCREEN);
    y = GetSystemMetrics(SM_CYSCREEN);
    SetWindowPos(hWnd, HWND_TOP, (x - w) / 2, (y - h) / 2, 0, 0, SWP_NOSIZE);
}
*/

// centre the window with respect to its parent in either (or both) directions
HB_FUNC ( WG_CENTERWINDOW )
//
//  FUNCTION: CenterWindow(HWND, HWND, INT)
//
//  PURPOSE:  Center one window over another.
//
//  PARAMETERS:
//    hwndChild - The handle of the window to be centered.
//    hwndParent- The handle of the window to center on.
//    iDirection- WIN_CENTER_HORIZONTAL | WIN_CENTER_VERTICAL
//
//  RETURN VALUE:
//
//    TRUE  - Success
//    FALSE - Failure
//
//  COMMENTS:
//
//    Dialog boxes take on the screen position that they were designed
//    at, which is not always appropriate. Centering the dialog over a
//    particular window usually results in a better position.
//
{

    //char *szBuf[200];

    // parameters
    HWND hWndChild  = (HWND) hb_parnl (1);
    // if not defined i use desktop
    HWND hWndParent =  ( !ISNIL(2) ? (HWND) hb_parnl (2) : (HWND) GetDesktopWindow() );
    int  iDirection =  ( !ISNIL(3) ? hb_parni(3) : WIN_CENTER_VERTICAL | WIN_CENTER_HORIZONTAL );


    RECT    rcChild, rcParent;
    int     cxChild, cyChild, cxParent, cyParent;
    int     cxScreen, cyScreen, xNew, yNew;
    HDC     hdc;

    // Get the Height and Width of the child window
    GetWindowRect(hWndChild, &rcChild);
    cxChild = rcChild.right - rcChild.left;
    cyChild = rcChild.bottom - rcChild.top;

    // Get the Height and Width of the parent window
    GetWindowRect(hWndParent, &rcParent);
    cxParent = rcParent.right - rcParent.left;
    cyParent = rcParent.bottom - rcParent.top;

    // Get the display limits
    hdc = GetDC(hWndChild);
    cxScreen = GetDeviceCaps(hdc, HORZRES);
    cyScreen = GetDeviceCaps(hdc, VERTRES);
    ReleaseDC(hWndChild, hdc);

    xNew = -1;
    yNew = -1;

    // Calculate new X position, then adjust for screen
    if ( iDirection & WIN_CENTER_HORIZONTAL )
         xNew = rcParent.left + ((cxParent - cxChild) / 2);
    if (xNew < 0)
    {
        xNew = 0;
    }
    else if ((xNew + cxChild) > cxScreen)
    {
        xNew = cxScreen - cxChild;
    }

    // Calculate new Y position, then adjust for screen
    if ( iDirection & WIN_CENTER_VERTICAL )
       yNew = rcParent.top  + ((cyParent - cyChild) / 2);
    if (yNew < 0)
    {
        yNew = 0;
    }
    else if ((yNew + cyChild) > cyScreen)
    {
        yNew = cyScreen - cyChild;
    }

    //sprintf( szBuf, "x=%u, y=%u, nx=%u, ny=%u, Dir=%i, wS=%u, hS=%u",
    //                rcChild.left, rcChild.top, xNew, yNew, iDirection, cxScreen, cyScreen );
    //MessageBox( 0, szBuf, "CenterWindow",
    //            MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);

    // Set it, and return
    hb_retl( SetWindowPos(hWndChild,
                        NULL,
                        xNew, yNew,
                        0, 0,
                        SWP_NOSIZE | SWP_NOZORDER) );
}


HB_FUNC ( GETWINDOWWIDTH )
{
    RECT rect;
    int w;
    HWND hWnd = (HWND) hb_parnl (1);

    GetWindowRect(hWnd, &rect);

    w = rect.right  - rect.left;
    hb_retni( w );
}

HB_FUNC ( GETWINDOWHEIGHT )
{
    RECT rect;
    int h;
    HWND hWnd = (HWND) hb_parnl (1);

    GetWindowRect(hWnd, &rect);

    h = rect.bottom - rect.top;
    hb_retni( h );
}

// Minimizes (but does not destroy) the specified window.
//
HB_FUNC ( CLOSEWINDOW )
{
    HWND hWnd = (HWND) hb_parnl (1);
    CloseWindow(hWnd);
}

// The DestroyWindow function destroys the specified window.
//
HB_FUNC ( DESTROYWINDOW )
{
    HWND hWnd = (HWND) hb_parnl (1);
    DestroyWindow(hWnd);
}

HB_FUNC ( ENABLEWINDOW )
{
    HWND hwnd          = (HWND) hb_parnl(1);    // handle to window
    BOOL bEnable       = hb_parl(2);            // enable or disable input

    hb_retl( (BOOL) EnableWindow( hwnd, bEnable ) );
}

// The FindWindow function retrieves a handle to the top-level window whose class name and
// window name match the specified strings. This function does not search child windows.
// This function does not perform a case-sensitive search.
//
HB_FUNC ( FINDWINDOW )
{
    LPCTSTR lpszClass   = (LPCTSTR) hb_parc(1);  // class name
    LPCTSTR lpszWindow  = (LPCTSTR) hb_parc(2);  // window name

    hb_retnl( (long) FindWindow( lpszClass, lpszWindow ) );
}

// The FindWindowEx function retrieves a handle to a window whose class name and window name
// match the specified strings. The function searches child windows, beginning with the one
// following the specified child window. This function does not perform a case-sensitive search.
//
HB_FUNC ( FINDWINDOWEX )
{
    HWND hwndParent     = (HWND) hb_parnl(1);    // handle to parent window
    HWND hwndChildAfter = (HWND) hb_parnl(2);    // handle to child window
    LPCTSTR lpszClass   = (LPCTSTR) hb_parc(3);  // class name
    LPCTSTR lpszWindow  = (LPCTSTR) hb_parc(4);  // window name

    hb_retnl( (long) FindWindowEx( hwndParent, hwndChildAfter, lpszClass, lpszWindow ) );
}


// The GetAncestor function retrieves the handle to the ancestor of the specified window
//
//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetAncestor( IN HWND hwnd, IN UINT gaFlags );
HB_FUNC ( GETANCESTOR )
{
   HWND hwnd     = (HWND) hb_parnl( 1 );    // handle to window
   UINT gaFlags  = (UINT) hb_parni( 2 );    // ancestor

   hb_retnl( (long) GetAncestor( hwnd, gaFlags ) ) ;
}


// The GetFocus function retrieves the handle to the window that has the keyboard focus,
// if the window is attached to the calling thread's message queue.
//
HB_FUNC ( GETFOCUS )
{
    hb_retnl( (long) GetFocus() );      // Return the handle of current window or control focused
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetWindow( IN HWND, IN UINT );


HB_FUNC ( GETWINDOW )
{
   hb_retnl( (LONG) GetWindow( (HWND)hb_parnl(1), (UINT) hb_parni( 2 ) ) ) ;
}

 //-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ClientToScreen( IN HWND hWnd, IN OUT LPPOINT lpPoint);
// SYNTAX CLIENTTOSCREEN( nWhd , @aArray ) -> lSuccess
/*Example
aArray:={20,20,60,80}
if CLIENTTOSCREEN(nWnd , @aArray)
endif
*/

HB_FUNC ( CLIENTTOSCREEN )
{
   POINT Point ;
   PHB_ITEM pArray;
   pArray=  hb_param( 2 , HB_IT_ARRAY );
   if (Array2Point( pArray ,&Point  ) )
   {
      if (ClientToScreen( (HWND) hb_parnl( 1 ), &Point ))
      {
          Point2ArrayEx( &Point   , pArray );
          hb_retl( TRUE ) ;
      }
      else
         hb_retl( FALSE ) ;
   }
      else
         hb_retl( FALSE ) ;

}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ScreenToClient( IN HWND hWnd, IN OUT LPPOINT lpPoint);
// SYNTAX SCREENTOCLIENT( nWhd , @aArray ) -> lSuccess
/*Example
aArray:={20,20}
if SCREENTOCLIENT(nWnd , @aArray)
endif
*/

HB_FUNC ( SCREENTOCLIENT )
{
   POINT Point ;
   PHB_ITEM pArray = hb_param( 2 , HB_IT_ARRAY );

   if (Array2Point(pArray, &Point ) )
   {
      if( ScreenToClient( (HWND) hb_parnl( 1 ), &Point ) >0)
      {
          Point2ArrayEx( &Point   , pArray );
          hb_retl( TRUE ) ;
      }
      else
         hb_retl( FALSE ) ;
   }
      else
         hb_retl( FALSE ) ;


}


//-----------------------------------------------------------------------------
//WINUSERAPI BOOL WINAPI GetClientRect(    IN HWND hWnd,    OUT LPRECT lpRect);
//Syntax GETCLIENTRECT( hWnd ) -> aRect

HB_FUNC ( GETCLIENTRECT )
{
   RECT rc;

   PHB_ITEM aMetr ;
   GetClientRect( (HWND) hb_parnl( 1 ), &rc );

   aMetr = Rect2Array( &rc  );

   _itemReturn( aMetr );
   _itemRelease( aMetr );
}

//-----------------------------------------------------------------------------

//WINUSERAPI BOOL WINAPI GetWindowRect(    IN HWND hWnd,    OUT LPRECT lpRect);
//Syntax GETWINDOWRECT( hWnd ) -> aRect

HB_FUNC ( GETWINDOWRECT )
{
   RECT rc;
   PHB_ITEM aMetr ;

   GetWindowRect( (HWND) hb_parnl( 1 ),   &rc );
   aMetr = Rect2Array( &rc  );

   _itemReturn( aMetr );
   _itemRelease( aMetr );
}



// WINUSERAPI HWND WINAPI GetTopWindow( IN HWND hWnd);

HB_FUNC ( GETTOPWINDOW )
{
   hb_retnl( (LONG) GetTopWindow( (HWND) hb_parnl( 1 ) ) ) ;
}


// The GetParent function retrieves a handle to the specified window's parent or owner.
//
HB_FUNC ( GETPARENT )
{
    HWND hwndChild     = (HWND) hb_parnl(1);    // handle of child window

    hb_retnl( (long) GetParent( hwndChild ) );
}

// The GetWindowText function copies the text of the specified window's title bar (if it has one)
// into a buffer. If the specified window is a control, the text of the control is copied.
// However, GetWindowText cannot retrieve the text of a control in another application.
//
HB_FUNC ( GETWINDOWTEXT )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle to window or control
    LPCTSTR lpString    = (LPCTSTR) hb_parc(2);  // text buffer
    int nMaxCount       =  hb_parni(3);          // maximum number of characters to copy

    hb_retni( (int) GetWindowText( hwnd, (char *)&lpString, nMaxCount ) );
    hb_storc( (char *)lpString, 2 );
}

// The IsChild function tests whether a window is a child window or descendant window of a specified
// parent window.
//
HB_FUNC ( ISCHILD )
{
    HWND hwndParent     = (HWND) hb_parnl(1);    // handle to parent window
    HWND hwnd           = (HWND) hb_parnl(2);    // handle of window to test

    hb_retl( (BOOL) IsChild( hwndParent, hwnd ) );
}

// The IsIconic function determines whether the specified window is minimized (iconic).
//
HB_FUNC ( ISICONIC )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle of window to test

    hb_retl( (BOOL) IsIconic( hwnd ) );
}

// The IsWindow function determines whether the specified window handle identifies an
// existing window.
//
HB_FUNC ( ISWINDOW )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle of window to test

    hb_retl( (BOOL) IsWindow( hwnd ) );
}

// The IsWindow function determines whether the specified window handle identifies an
// existing window.
//
HB_FUNC ( ISWINDOWVISIBLE )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle of window to test

    hb_retl( (BOOL) IsWindowVisible( hwnd ) );
}

// The IsZoomed function determines whether a window is maximized.
//
HB_FUNC ( ISZOOMED )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle of window to test

    hb_retl( (BOOL) IsZoomed( hwnd ) );
}

// The MoveWindow function changes the position and dimensions of the specified window.
// For a top-level window, the position and dimensions are relative to the upper-left corner of
// the screen. For a child window, they are relative to the upper-left corner of the parent
// window's client area.
//
HB_FUNC ( MOVEWINDOW )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle to window
    int  X              = hb_parni(2);           // horizontal position
    int  Y              = hb_parni(3);           // vertical position
    int  nWidth         = hb_parni(4);           // width
    int  nHeight        = hb_parni(5);           // height
    BOOL bRepaint       = hb_parl(6);            // repaint option

    hb_retl( (BOOL) MoveWindow( hwnd, X, Y, nWidth, nHeight, bRepaint ) );
}

// The OpenIcon function restores a minimized (iconic) window to its previous size and position;
// it then activates the window.
//
HB_FUNC ( OPENICON )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle to window

    hb_retl( (BOOL) OpenIcon( hwnd ) );
}

// The SetFocus function sets the keyboard focus to the specified window. The window must be
// attached to the calling thread's message queue.
//
HB_FUNC ( SETFOCUS )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle to window or control

    hb_retnl( (long) SetFocus( hwnd ) );      // Return the handle of previous window or control focused
}

// The SetParent function changes the parent window of the specified child window.
//
HB_FUNC ( SETPARENT )
{
    HWND hwndChild     = (HWND) hb_parnl(1);    // handle of child window
    HWND hwndNewParent = (HWND) hb_parnl(2);    // handle of new parent window

    HWND hwndOldParent = SetParent( hwndChild, hwndNewParent );    // handle of old parent window

    hb_retnl( (long) hwndOldParent );
}

// The SetWindowText function changes the text of the specified window's title bar (if it has one).
// If the specified window is a control, the text of the control is changed.
// However, SetWindowText cannot change the text of a control in another application.
//
HB_FUNC ( SETWINDOWTEXT )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle to window or control
    LPCTSTR lpString    = (LPCTSTR) hb_parc(2);  // title or text

    hb_retl( (BOOL) SetWindowText( hwnd, lpString ) );
}

// The SetWindowLongPtr function changes an attribute of the specified window.
// The function also sets a value at the specified offset in the extra window memory.
//
HB_FUNC ( SETWINDOWLONGPTR )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle to window or control
    int  nIndex         = hb_parni(2);           // offset of value to set
    LONG_PTR dwNewLong  = (LONG_PTR) hb_parnl(3); // new value

    hb_retnl( (LONG_PTR) SetWindowLongPtr( hwnd, nIndex, dwNewLong ) );
}

// The SetWindowPos function changes the size, position, and Z order of a child, pop-up, or
// top-level window. Child, pop-up, and top-level windows are ordered according to their
// appearance on the screen. The topmost window receives the highest rank and is the first
// window in the Z order.
//
HB_FUNC ( SETWINDOWPOS )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle to window or control
    HWND hWndInsertAfter= (HWND) hb_parnl(2);    // placement-order handle
    int X               =        hb_parni(3);    // horizontal position
    int Y               =        hb_parni(4);    // vertical position
    int cx              =        hb_parni(5);    // width
    int cy              =        hb_parni(6);    // height
    UINT uFlags         = (UINT) hb_parni(7);    // window-positioning options

    hb_retl( (BOOL) SetWindowPos( hwnd, hWndInsertAfter, X, Y, cx, cy, uFlags ) );
}

// The ShowWindow function sets the specified window's show state.
//
HB_FUNC ( SHOWWINDOW )
{
    HWND hWnd         = (HWND) hb_parnl (1);    // handle to window
    int  nCmdShow     = hb_parni (2);           // show state

    hb_retl( (BOOL) ShowWindow(hWnd, nCmdShow) );
}

// The UpdateWindow function updates the client area of the specified window by sending
// a WM_PAINT message to the window if the window's update region is not empty.
// The function sends a WM_PAINT message directly to the window procedure of the specified
// window, bypassing the application queue. If the update region is empty, no message is sent.
//
HB_FUNC ( UPDATEWINDOW )
{
    HWND hWnd         = (HWND) hb_parnl (1);    // handle to window

    hb_retl( (BOOL) UpdateWindow(hWnd) );
}

//
// Interfaccia per la gestione degli eventi con le api di windows
// Questa procedura richiama la funzione DEFWNDPROC (se definita) passando l'evento
// da controllare.
// Se non viene fatto nulla passa alla gestione di default
//

LRESULT CALLBACK WG_WndProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    PHB_DYNS pSymTest;
    long int res;

    if( ( pSymTest = hb_dynsymFind( "WG_DEFWNDEVENTS" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );

      return res;
   }

  else // It must be called in prg source, but if it not happens we must call here
      return DefWindowProc( hWnd, message, wParam, lParam );

}

HB_FUNC ( WG_INITEDITPROC )
{
   wpOriginalEditProc = (WNDPROC) SetWindowLong( (HWND) hb_parnl(1),
                                                  GWL_WNDPROC, (LONG) WG_EdtProc );

   hb_retnl( (LONG) wpOriginalEditProc );
}

LRESULT APIENTRY WG_EdtProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   long int res = -1;

   if( msg == WM_KEYUP || msg == WM_KEYDOWN || msg == WM_CHAR )
   {
      PHB_DYNS pSymTest;

      if( ( pSymTest = hb_dynsymFind( "WG_DEFEDTEVENTS" ) ) != NULL )
      {
         hb_vmPushSymbol( pSymTest->pSymbol );
         hb_vmPushNil();
         hb_vmPushLong( (LONG ) hWnd );
         hb_vmPushLong( (LONG ) msg );
         hb_vmPushLong( (LONG ) wParam );
         hb_vmPushLong( (LONG ) lParam );
         hb_vmDo( 4 );
         res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      }
   }
   if( res == -1 )
       res = CallWindowProc( wpOriginalEditProc, hWnd, msg, wParam, lParam );
   return res;
}


HB_FUNC ( DEFDLGPROC )
{
  hb_retnl( DefDlgProc( (HWND) hb_parnl(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}


HB_FUNC ( DEFWINDOWPROC )
{
  hb_retnl( DefWindowProc( (HWND) hb_parnl(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

HB_FUNC ( DEFMDICHILDPROC )
{
  hb_retnl( DefMDIChildProc( (HWND) hb_parnl(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

//-----------------------------------------------------------------------------

HB_FUNC ( DEFFRAMEPROC )
{
  hb_retnl( DefFrameProc( (HWND) hb_parnl(1), (HWND) hb_parnl(2), hb_parnl(3), hb_parnl(4), hb_parnl(5)));
}


//-----------------------------------------------------------------------------

HB_FUNC ( CALLWINDOWPROC )
{
  hb_retnl( CallWindowProc( (WNDPROC) hb_parnl(1), (HWND) hb_parnl(2), (LONG) hb_parnl(3), (WPARAM) hb_parnl(4), (LPARAM) hb_parnl(5)));
}



LRESULT CALLBACK W_DlgProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    static PHB_DYNS Dyns = 0 ;
    long int res;

    if ( ! Dyns )
    {
        Dyns = hb_dynsymFind( "W_DLGEVENTS" );
    }

    hb_vmPushSymbol( Dyns->pSymbol );
    hb_vmPushNil();
    hb_vmPushLong( ( LONG ) hWnd );
    hb_vmPushLong( message );
    hb_vmPushLong( wParam );
    hb_vmPushLong( lParam );
    hb_vmDo( 4 );

    res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
    if( res == -1 )
    {
        return DefDlgProc( ( HWND ) hWnd, message, wParam, lParam );
    }
    else
    {
        return res;
    }
}


//HB_FUNC ( POSTQUITMESSAGE )
//{
//    PostQuitMessage( hb_parnl( 1 ) );
//}
//
// HB_FUNC ( SENDMESSAGE )
// {
//     hb_retnl( (LONG) SendMessage(
//                        (HWND) hb_parnl( 1 ),    // handle of destination window
//                        (UINT) hb_parni( 2 ),    // message to send
//                        (WPARAM) hb_parnl( 3 ),  // first message parameter
//                        (LPARAM) hb_parnl( 4 )   // second message parameter
//                      ) );
// }

//HB_FUNC ( SENDMESSAGE )
//{
//
//  char *cText ;
//
//  if (ISBYREF(4)) {
//     cText = hb_strdup( hb_parc(4) ) ;
//  }
//
//   hb_retnl( (LONG) SendMessage( (HWND) hb_parnl( 1 ), (UINT) hb_parni( 2 ),
//                                (ISNIL(3) ? 0 : (WPARAM) hb_parnl( 3 ))   ,
//                                (ISNIL(4) ? 0 : ( ISBYREF(4)? (LPARAM) (LPSTR) cText : ( ISCHAR(4) ? (LPARAM)(LPSTR) hb_parc(4) : (LPARAM) hb_parnl( 4 ))))
//                               )
//            );
//
//  if (ISBYREF( 4 ))
//   {
//      hb_storclen( cText, hb_parcsiz(4), 4 ) ;
//      hb_xfree( cText );
//   }
//
//}
//
//
// ok
// WINUSERAPI HWND WINAPI GetForegroundWindow( VOID);


HB_FUNC ( GETFOREGROUNDWINDOW )
{
   hb_retnl( (LONG) GetForegroundWindow(  ) ) ;
}

// WINUSERAPI BOOL WINAPI SetForegroundWindow( IN HWND hWnd);

HB_FUNC ( SETFOREGROUNDWINDOW )
{
   hb_retl( SetForegroundWindow( (HWND) hb_parnl( 1 ) ) ) ;
}



//HB_FUNC ( POSTMESSAGE )
//{
//    hb_retnl( (LONG) PostMessage(
//                       (HWND) hb_parnl( 1 ),    // handle of destination window
//                       (UINT) hb_parni( 2 ),    // message to send
//                       (WPARAM) hb_parnl( 3 ),  // first message parameter
//                       (LPARAM) hb_parnl( 4 )   // second message parameter
//                     ) );
//}
//
// Restituisce
HB_FUNC ( GETWINDOWLONG )
{
    hb_retnl( (LONG) GetWindowLong(
                            (HWND) hb_parnl( 1 ),   // handle of destination window
                            hb_parni(2)             // offset of value to retrieve
                                       )
            );
}

// The GetWindowLongPtr function retrieves information about the specified window.
//The function also retrieves the value at a specified offset into the extra window memory.
//
HB_FUNC ( GETWINDOWLONGPTR )
{
    HWND hwnd           = (HWND) hb_parnl(1);    // handle to window or control
    int  nIndex         = hb_parni(2);           // offset of value to retrieve

    hb_retnl( (LONG_PTR) GetWindowLongPtr( hwnd, nIndex ) );
}



// Inizializza i controlli dell'applicazione
HB_FUNC ( INITCOMMONCONTROLSEX )
{
  hb_retl( (BOOL) InitCommonControlsEx( (LPINITCOMMONCONTROLSEX) lpInitCtrls ) );

}


BOOL WG_DebugTrace( char* cMsg )
{
   static PHB_DYNS Dyns = 0 ;

   if ( ! Dyns )
   {
       Dyns = hb_dynsymFind( "WG_DEBUGTRACE" );
   }

   hb_vmPushSymbol( Dyns->pSymbol );
   hb_vmPushNil();
   hb_vmPushString( cMsg, strlen(cMsg) );
   hb_vmDo( 1 );

   return FALSE;

}

BOOL WG_WriteEvents( MSG msg )
{
   static PHB_DYNS Dyns = 0 ;

   if ( ! Dyns )
   {
       Dyns = hb_dynsymFind( "WG_WRITEEVENTS" );
   }

   hb_vmPushSymbol( Dyns->pSymbol );
   hb_vmPushNil();
   hb_vmPushLong( ( LONG ) msg.hwnd );
   hb_vmPushLong( msg.message );
   hb_vmPushLong( msg.wParam );
   hb_vmPushLong( msg.lParam );
   hb_vmDo( 4 );

   return FALSE;

}

HB_FUNC ( GETACTIVEWINDOW )
{
   HWND hwnd;

   hwnd = GetActiveWindow();

   hb_retnl ( (LONG) hwnd );
}

HB_FUNC ( SETACTIVEWINDOW )
{
   HWND hwnd;

   hwnd = (HWND) hb_parnl (1);

   SetActiveWindow (hwnd);
}

HB_FUNC ( REDRAWWINDOW )
{
   RedrawWindow(
    (HWND) hb_parnl( 1 ),  // handle of window
    NULL,   // address of structure with update rectangle
    NULL,   // handle of update region
    hb_parni( 2 )    // array of redraw flags
   );
}

