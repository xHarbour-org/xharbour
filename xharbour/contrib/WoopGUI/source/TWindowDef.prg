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
* Base Class - All windows and controls derive from this class
* NOT USE THIS CLASS DIRECTLY !!!
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"

CLASS TWindowDef FROM TObject

    CLASSDATA nClassStyle    AS NUMERIC      // Specifies the class style(s)
    //CLASSDATA pWndProc       AS NUMERIC      // Pointer to the window procedure
    CLASSDATA bWindowProc    AS CODEBLOCK    // Codeblock of window procedure - class defaults
    CLASSDATA hInstance      AS NUMERIC      // Handle to the instance that contains the window procedure for the class
    CLASSDATA hIcon          AS NUMERIC      // Handle to the class icon. This member must be a handle to an icon resource. If this member is NULL, the system provides a default icon
    CLASSDATA hCursor        AS NUMERIC      // Handle to the class cursor. This member must be a handle to a cursor resource. If this member is NULL, an application must explicitly set the cursor shape whenever the mouse moves into the application's window
    CLASSDATA hbrBackground  AS NUMERIC      // Handle to the class background brush. This member can be a handle to the physical brush to be used for painting the background, or it can be a color value
    CLASSDATA cMenuName      AS STRING       // character string that specifies the resource name of the class menu. If this member is NULL, windows belonging to this class have no default menu
    CLASSDATA cClassName     AS STRING       // Specifies the window class name. The class name can be any name registered, or any of the predefined control-class names.
    CLASSDATA hIconSm        AS NUMERIC      // Handle to a small icon that is associated with the window class. If this member is NULL, the system searches the icon resource specified by the hIcon member for an icon of the appropriate size to use as the small icon

    // Public methods
    METHOD New() CONSTRUCTOR
    //METHOD WindowProc()


    METHOD SetCursorFromFile()
    METHOD SetIconFromFile()
    METHOD SetIconSmFromFile()

ENDCLASS

METHOD New( nStyle, bWindowProc, hInstance, hIcon, hCursor, hbrBackground, cMenuName, cClassName, hIconSm ) CLASS TWindowDef
    LOCAL xVal

    // window defaults
    ASSIGN ::nClassStyle   WITH nStyle         DEFAULT CS_HREDRAW + CS_VREDRAW + CS_OWNDC + CS_DBLCLKS
    // pWndProc - pointer to window procedure - modified in bWindowProc which is called from C
    ASSIGN ::bWindowProc   WITH bWindowProc    //DEFAULT {|hWnd, message, lParam, wParam| WG_WndEvents(hWnd, message, lParam, wParam) }
    ASSIGN ::hInstance     WITH hInstance      DEFAULT GetModuleHandle()
    ASSIGN ::hIcon         WITH hIcon          DEFAULT LoadIcon(NULL, IDI_APPLICATION)
    ASSIGN ::hCursor       WITH hCursor        //DEFAULT LoadCursor(NULL, IDC_ARROW) // FSG - to be checked
    ASSIGN ::hbrBackground WITH hbrBackground  DEFAULT COLOR_BTNFACE + 1
    ASSIGN ::cMenuName     WITH cMenuName
    ASSIGN ::cClassName    WITH cClassName     DEFAULT "WoopGUIClass"
    ASSIGN ::hIconSm       WITH hIconSm        DEFAULT LoadIcon(NULL, IDI_APPLICATION)

    //ObjDisplayData( Self )

RETURN Self

METHOD SetCursorFromFile( cFileName ) CLASS TWindowDef
   ::hCursor := WG_GetCursorFromFile( cFileName )
RETURN ::hCursor

METHOD SetIconFromFile( cFileName ) CLASS TWindowDef
   ::hIcon := WG_GetIconFromFile( cFileName )
RETURN ::hIcon

METHOD SetIconSmFromFile( cFileName ) CLASS TWindowDef
   ::hIconSm := WG_GetIconFromFile( cFileName )
RETURN ::hIconSm

//METHOD WindowProc( hWnd, nMessage, wParam, lParam ) CLASS TWindowDef
//  LOCAL nRet := -1
//  IF ValType( ::bWindowProc ) == "B" // ::HasEventHandler()
//     nRet := Eval( ::bWindowProc, hWnd, nMessage, wParam, lParam )
//  ENDIF
//RETURN nRet

