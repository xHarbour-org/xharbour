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

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

#include "woopgui.h"

//void DefineInstance( HINSTANCE hInstance );

// The GetModuleHandle function retrieves a module handle for the specified module if the file
// has been mapped into the address space of the calling process
//
// GetModuleHandle( cModuleName ) --> nHandle
//
/*
HB_FUNC ( GETMODULEHANDLE )
{
   HINSTANCE hInstance;
   LPCTSTR lpModuleName;  // Pointer to a null-terminated string that contains the name of the
                          // module (either a .dll or .exe file). If the file name extension is
                          // omitted, the default library extension .dll is appended. The file name
                          // string can include a trailing point character (.) to indicate that the
                          // module name has no extension. The string does not have to specify a
                          // path. When specifying a path, be sure to use backslashes (\), not
                          // forward slashes (/). The name is compared (case independently) to the
                          // names of modules currently mapped into the address space of the
                          // calling process.
                          // If this parameter is NULL, GetModuleHandle returns a handle to the
                          // file used to create the calling process.
  lpModuleName = (LPCSTR) hb_parcx(1);
  hInstance = GetModuleHandle( lpModuleName );
  //DefineInstance( hInstance );
  hb_retnl( (LONG) hInstance );
}
*/

HB_FUNC ( GETSYSTEMDIR )
{
   char szBuffer[ MAX_PATH + 1 ]  = { 0 } ;

   GetSystemDirectory(  szBuffer , MAX_PATH );

   hb_retc( szBuffer );

}


// The LoadIcon function loads the specified icon resource from the executable (.exe) file
// associated with an application instance
//
HB_FUNC ( LOADCURSOR )
{
  HINSTANCE hInstance    = (HINSTANCE) hb_parnl(1);  // handle to application instance
  LPCTSTR   lpCursorName = (LPCTSTR)   hb_parcx(2);   // name string or resource identifier

  hb_retnl( (long) LoadCursor( hInstance, lpCursorName ) );
}

// The LoadIcon function loads the specified icon resource from the executable (.exe) file
// associated with an application instance
//
HB_FUNC ( LOADICON )
{
  HINSTANCE hInstance  = (HINSTANCE) hb_parnl(1);  // handle to application instance
  LPCTSTR   lpIconName = (LPCTSTR)   hb_parcx(2);   // name string or resource identifier

  hb_retnl( (long) LoadIcon( hInstance, lpIconName ) );
}

// The LoadImage function loads an icon, cursor, animated cursor, or bitmap.
//
HB_FUNC ( LOADIMAGE )
{
  HINSTANCE hinst   = (HINSTANCE) hb_parnl(1);     // Handle to an instance of the module that contains the image to be loaded. To load an OEM image, set this parameter to zero.
  LPCTSTR lpszName  = (LPCTSTR)   hb_parcx(2);      // image to load
  UINT uType        = (UINT)      hb_parnl(3);     // image type
  int cxDesired     =             hb_parni(4);     // desired width
  int cyDesired     =             hb_parni(5);     // desired height
  UINT fuLoad       = (UINT)      hb_parnl(6);     // load options

  hb_retnl( (long) LoadImage( hinst, lpszName, uType, cxDesired, cyDesired, fuLoad ) );

}



/*
//
// RegisterClassEx( cClassName, cIcon, nBkColor, cIconSm ) --> nInstance
//
HB_FUNC ( REGISTERCLASSEX )
{
    WNDCLASSEX wcex;

    UINT      nStyle        = (UINT)      hb_parnl(1);    // Specifies the class style(s). This member can be any combination of the class styles.
    WNDPROC   cWndProc      = (WNDPROC)   hb_parnl(2);    // Pointer to the window procedure
    HINSTANCE hInstance     = (HINSTANCE) hb_parnl(3);    // Handle to the instance that contains the window procedure for the class
    HICON     hIcon         = (HICON)     hb_parnl(4);    // Handle to the class icon. This member must be a handle to an icon resource. If this member is NULL, the system provides a default icon
    HCURSOR   hCursor       = (HCURSOR)   hb_parnl(5);    // Handle to the class cursor. This member must be a handle to a cursor resource. If this member is NULL, an application must explicitly set the cursor shape whenever the mouse moves into the application's window.
    HBRUSH    hbrBackground = (HBRUSH)    hb_parnl(6);    // Handle to the class background brush
    LPCTSTR   cMenuName     = (LPCTSTR)   hb_parcx(7);     // Pointer to a null-terminated character string that specifies the resource name of the class menu, as the name appears in the resource file. If you use an integer to identify the menu, use the MAKEINTRESOURCE macro. If this member is NULL, windows belonging to this class have no default menu
    LPCTSTR   cClassName    = (LPCTSTR)   hb_parcx(8);     // Pointer to a null-terminated string
    HICON     hIconSm       = (HICON)     hb_parnl(9);    // Handle to a small icon that is associated with the window class. If this member is NULL, the system searches the icon resource specified by the hIcon member for an icon of the appropriate size to use as the small icon

    wcex.cbSize        = sizeof(WNDCLASSEX);
    wcex.style         = nStyle;
    wcex.lpfnWndProc   = cWndProc;
    wcex.cbClsExtra    = 0;
    wcex.cbWndExtra    = 0;
    wcex.hInstance     = hInstance;
    wcex.hIcon         = hIcon;
    wcex.hCursor       = hCursor;
    wcex.hbrBackground = hbrBackground;
    wcex.lpszMenuName  = cMenuName;
    wcex.lpszClassName = cClassName;
    wcex.hIconSm       = hIconSm;

    if ( ! RegisterClassEx(&wcex) )
    {
       MessageBox(0, "Window Registration Failed!", "Error!",
       MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
    return;
    }

    hb_retnl( (LONG) hInstance );
}
*/
/*
HB_FUNC ( GETCLASSINFOEX )
{
    HINSTANCE hInstance     = (HINSTANCE) hb_parnl(1);    // Handle to the instance that contains the window procedure for the class
    LPCTSTR   cClassName    = (LPCTSTR)   hb_parcx(2);     // Pointer to a null-terminated string
    LPWNDCLASSEX lpwcx;                                   // class data
    bool lOk;

    if ( lOk = GetClassInfoEx( hInstance, cClassName, &lpwcx ) )
    {


    }

}
*/
