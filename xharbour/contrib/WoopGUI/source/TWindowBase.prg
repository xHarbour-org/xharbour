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

static snWinID



CLASS TWindowBase FROM TObject

    CLASSDATA aoWindows    AS ARRAY INIT {} HIDDEN // Array of windows (this array contains all windows, windows are controls too)
    CLASSDATA aoWinClasses AS ARRAY INIT {} HIDDEN // Array of window classes
    CLASSDATA oCurrentWindow AS OBJECT      HIDDEN // Current Window

    DATA nHandle           AS NUMERIC //PROTECTED // Handle of this window
    DATA cargo                                   // cargo as in Clipper
    DATA nID               AS NUMERIC             //
    DATA oParent           AS OBJECT  HIDDEN      // Parent windows object
    DATA lEnabled          AS LOGICAL INIT TRUE HIDDEN      // FSG - to be correctly implemented
    DATA lStartFocusEvents AS LOGICAL INIT TRUE // This to managing Focus Events

    DATA lRegistered       AS LOGICAL INIT FALSE HIDDEN

    DATA oWindowClass      AS OBJECT HIDDEN

    //CLASSDATA lRegistered AS LOGICAL INIT FALSE

    //DATA bWindowProc      AS CODEBLOCK     // Code block to evaluate events

    // Data of RegisterClassEx template
    DATA nClassStyle        AS NUMERIC      // Specifies the class style(s)
    //DATA pWndProc           AS NUMERIC      // Pointer to the window procedure
    DATA bWindowProc        AS CODEBLOCK    // Codeblock of window procedure - class defaults
    DATA hInstance          AS NUMERIC      // Handle to the instance that contains the window procedure for the class
    DATA hIcon              AS NUMERIC      // Handle to the class icon. This member must be a handle to an icon resource. If this member is NULL, the system provides a default icon
    DATA hCursor            AS NUMERIC      // Handle to the class cursor. This member must be a handle to a cursor resource. If this member is NULL, an application must explicitly set the cursor shape whenever the mouse moves into the application's window
    DATA hbrBackground      AS NUMERIC      // Handle to the class background brush. This member can be a handle to the physical brush to be used for painting the background, or it can be a color value
    //DATA cMenuName          AS STRING       // character string that specifies the resource name of the class menu. If this member is NULL, windows belonging to this class have no default menu
    DATA cClassName         AS STRING       // Specifies the window class name. The class name can be any name registered, or any of the predefined control-class names.
    DATA hIconSm            AS NUMERIC      // Handle to a small icon that is associated with the window class. If this member is NULL, the system searches the icon resource specified by the hIcon member for an icon of the appropriate size to use as the small icon


    // Data of WindowEx template
    DATA nExStyle           AS NUMERIC       // Extended style
    //DATA cClassName         AS STRING        // Registered class name
    DATA cName              AS STRING        // Window name
    DATA nStyle             AS NUMERIC       // Window style
    DATA nRow               AS NUMERIC       // horizontal position of window
    DATA nCol               AS NUMERIC       // vertical position of window
    DATA nWidth             AS NUMERIC       // window width
    DATA nHeight            AS NUMERIC       // window height
    DATA nParent            AS NUMERIC       // Handle to parent or owner window
    DATA nMenu              AS NUMERIC       // Handle to menu or to child window
    DATA nApplication       AS NUMERIC       // Handle to application
    DATA pStruct                             // window-creation data (not used)

    METHOD New() CONSTRUCTOR
    METHOD Init()
    METHOD Create()

    METHOD AddWinClass( oWin AS OBJECT )       INLINE aAdd( ::aoWinClasses, oWin )

    METHOD AddWindow( oWin AS OBJECT )         INLINE aAdd( ::aoWindows, oWin )
    METHOD DelWindow( oWin AS OBJECT )         INLINE LOCAL nPos,;
                                                      oWin := IIF( oWin == NIL, Self, oWin ),;
                                                      nPos := ::FindWindowPos( oWin ),;
                                                      WG_DebugTrace( "TWindowBase:DelWindow()", "nPos", nPos, "::nHandle", ::nHandle ),;
                                                      WG_aShrink( ::aoWindows, nPos ),;
                                                      Self := NIL
    METHOD GetWindows()                        INLINE ::aoWindows
    METHOD FindWindowPos()
    METHOD FindWindowByHandle()
    METHOD FindWindowByID()
    METHOD FindWindowByName()
    METHOD FindWindowbyPos()

    METHOD Activate()                          INLINE ShowWindow( ::nHandle, SW_SHOW )
    METHOD CanAcceptFocus()                    INLINE ::IsShown() .AND. ::IsEnabled()
    METHOD CaptureMouse()                      VIRTUAL // FSG - to be implemented
    METHOD CreateId()                          INLINE snWinID++   //
    METHOD Center()                            INLINE CenterWindow( ::nHandle )
    METHOD CenterOnParent()                    VIRTUAL // FSG - to be implemented
    METHOD CenterOnScreen()                    INLINE ::Center()
    METHOD Centre()                            INLINE ::Center()
    METHOD CentreOnParent()                    INLINE ::CenterOnParent()
    METHOD CentreOnScreen()                    INLINE ::CenterOnScreen()
    METHOD Close()                             INLINE CloseWindow( ::nHandle )
    METHOD Destroy()                           INLINE DestroyWindow( ::nHandle ), Self := NIL
    METHOD Disable()                           INLINE EnableWindow( ::nHandle, FALSE )
    METHOD Enable()                            INLINE EnableWindow( ::nHandle, TRUE )
    METHOD FindFocus()                         INLINE ::GetFocus()
    METHOD GetCapture()                        VIRTUAL
    METHOD GetCurrentWindow()
    METHOD GetEventHandler()                   INLINE ::bWindowProc
    METHOD GetExtraStyle()                     INLINE ::nExStyle
    METHOD GetFocus()                          INLINE ::FindWindowByHandle( GetFocus() ) // GetFocus() --> nHandle
    METHOD GetGrandParent()                    INLINE LOCAL nH, ;
                                                      nH := GetAncestor( ::nHandle, GA_ROOT ),;
                                                      ::FindWindowbyHandle( nH )
    METHOD GetHandle()                         INLINE ::nHandle
    METHOD GetHeight()                         INLINE GetWindowHeight( ::nHandle )
    METHOD GetId()                             INLINE ::nId
    METHOD GetParent()                         INLINE  IIF( ValType( ::oParent ) == "O", ::oParent, NIL )
    METHOD GetParentHandle()                   INLINE  IIF( ValType( ::oParent ) == "O", ::oParent:nHandle, NIL )
    METHOD GetStyle()                          INLINE GetWindowLongPtr( ::nHandle, GWL_STYLE ) //::nStyle
    METHOD GetTitle()                          INLINE ::GetValue()
    METHOD GetValue()                          INLINE ""
    METHOD GetWidth()                          INLINE GetWindowWidth( ::nHandle )
    METHOD GetWindowStyle()                    INLINE ::nStyle
    METHOD GetWindowStyleFlag()                INLINE ::nStyle
    METHOD HasCapture()                        INLINE ( Self == ::GetCapture() )
    METHOD HasEventHandler()                   INLINE ( ValType( ::bWindowProc ) == "B" )
    METHOD HasParent()                         INLINE ( ::oParent <> NIL )
    METHOD Hide()                              INLINE ShowWindow( ::nHandle, SW_HIDE )
    METHOD IsChild( nParent )                  INLINE IsChild( nParent, ::nHandle )
    METHOD IsEnabled()                         INLINE ::lEnabled
    METHOD IsIconic()                          INLINE IsIconic( ::nHandle )
    METHOD IsShown()                           INLINE IsWindowVisible( ::nHandle )
    METHOD IsTopLevel()                        INLINE ( ::nHandle == GetTopWindow() )
    METHOD IsWindow()                          INLINE IsWindow( ::nHandle )
    METHOD IsRegistered()                      INLINE ::lRegistered
    METHOD Maximize()                          INLINE ShowWindow( ::nHandle, SW_MAXIMIZE )
    METHOD Minimize()                          INLINE ShowWindow( ::nHandle, SW_MINIMIZE )
    METHOD MessageBox( cMsg, cTitle, uStyle )  INLINE MessageBox( ::nHandle, cMsg, cTitle, uStyle )
    METHOD Move( nX, nY, nWidth, nHeight, lRepaint ) INLINE MoveWindow( ::nHandle, nY, nX, nWidth, nHeight, lRepaint )
    METHOD PopEventHandler()                   VIRTUAL // FSG - to be implemented
    METHOD PushEventHandler()                  VIRTUAL // FSG - to be implemented
    METHOD Redraw( nFlags )                    INLINE IIF( nFlags == NIL, nFlags := RDW_ERASE + RDW_INVALIDATE, NIL ),;
                                                      RedrawWindow( ::nHandle, nFlags )
    METHOD ReleaseMouse()                      VIRTUAL // FSG - to be implemented
    METHOD Register()
    METHOD Reparent()
    METHOD Restore()                           INLINE ShowWindow( ::nHandle, SW_RESTORE )
    METHOD SendMessage( nMsg, wParam, lParam ) INLINE SendMessage( ::nHandle, nMsg, wParam, lParam )

    METHOD SetBackgroundFromFile()
    METHOD SetCurrentWindow()
    METHOD SetCursorFromFile()
    METHOD SetEventHandler()
    METHOD SetExtraStyle( nExStyle )           INLINE ::nExStyle := nExStyle, ;
                                                      SetWindowLongPtr( ::nHandle, GWL_EXSTYLE, nExStyle )

    METHOD SetFocus( lStartEvents )            INLINE LOCAL lCurrentEvents,;
                                                      lCurrentEvents := ::lStartFocusEvents,;
                                                      IIF( lStartEvents == NIL, lStartEvents := TRUE, NIL ),;
                                                      ::lStartFocusEvents := lStartEvents,;
                                                      SetFocus( ::nHandle ),;
                                                      ::lStartFocusEvents := lCurrentEvents

    METHOD SetIconFromFile()
    METHOD SetIconSmFromFile()
    METHOD SetId( nId )                        INLINE ::nID := nId
    METHOD SetName()                           INLINE ::SetValue()
    METHOD SetSize( nWidth, nHeight )          INLINE ::Move( ::nRow, ::nCol, nWidth, nHeight, TRUE /*lRepaint*/ )
    METHOD SetStyle( nStyle )                  INLINE ::nStyle := nStyle ,;
                                                      SetWindowLongPtr( ::nHandle, GWL_STYLE, nStyle )
    METHOD SetTitle()                          INLINE ::SetValue()
    METHOD SetValue( cName AS STRING )         INLINE ::cName := cName,;
                                                      SetWindowText( ::nHandle, cName )
    METHOD SetWindowPos()
    METHOD SetWindowStyle()                    INLINE ::SetStyle()
    METHOD SetWindowStyleFlag()                INLINE ::SetStyle()
    METHOD Show()                              INLINE ShowWindow( ::nHandle, SW_SHOW )
    METHOD SetParentByHandle()
    METHOD ToTop()                             INLINE BringWindowToTop( ::nHandle )
    METHOD UnRegister()
    METHOD UnSetCurrentWindow()                INLINE ::SetCurrentWindow( NIL )
    METHOD Update()                            INLINE UpdateWindow( ::nHandle )
    METHOD WindowProc()

ENDCLASS

METHOD New( nExStyle, cClassName, cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, nMenu, nApplication, pStruct ) CLASS TWindowBase

    LOCAL oClass := TWindowDef()    // Get Windows class defaults

    //WG_ParamDisplay( Self, hb_aparams(), "TWindowBase" )

    DEFAULT snWinID TO 10000
    //ParamDisplay( Self, hb_aparams() )

    // class defaults
    ASSIGN ::nClassStyle   WITH oClass:nClassStyle  DEFAULT CS_HREDRAW + CS_VREDRAW + CS_OWNDC + CS_DBLCLKS
    // pWndProc - pointer to window procedure - modified in bWindowProc which is called from C
    ASSIGN ::bWindowProc   WITH oClass:bWindowProc    //DEFAULT {|hWnd, message, lParam, wParam| WG_WndEvents(hWnd, message, lParam, wParam) }
    ASSIGN ::hInstance     WITH oClass:hInstance      DEFAULT GetModuleHandle()
    ASSIGN ::hIcon         WITH oClass:hIcon          DEFAULT LoadIcon(NULL, IDI_APPLICATION)
    ASSIGN ::hCursor       WITH oClass:hCursor        //DEFAULT LoadCursor(NULL, IDC_ARROW) // FSG - to be checked

    ASSIGN ::hbrBackground WITH oClass:hbrBackground  DEFAULT COLOR_BTNFACE + 1

    //ASSIGN ::cMenuName     WITH oClass:cMenuName
    //ASSIGN ::cClassName    WITH oClass:cClassName     DEFAULT "WoopGUIClass"
    ASSIGN ::hIconSm       WITH oClass:hIconSm        DEFAULT LoadIcon(NULL, IDI_APPLICATION)

    // window defaults
    ASSIGN ::nExStyle     WITH nExStyle          DEFAULT WS_EX_LEFT //WS_EX_OVERLAPPEDWINDOW
    //ASSIGN ::cClassName   WITH cClassName        //DEFAULT "WoopGUIBaseClass"
    ASSIGN ::cName        WITH cName             DEFAULT "WindowBase_1"
    ASSIGN ::nStyle       WITH nStyle            DEFAULT WS_OVERLAPPEDWINDOW
    ASSIGN ::nRow         WITH nRow              DEFAULT CW_USEDEFAULT
    ASSIGN ::nCol         WITH nCol              DEFAULT CW_USEDEFAULT
    ASSIGN ::nWidth       WITH nWidth            DEFAULT CW_USEDEFAULT
    ASSIGN ::nHeight      WITH nHeight           DEFAULT CW_USEDEFAULT

    IF ValType( oParent ) == "O"
       ::oParent := oParent
       ::nParent := ::oParent:nHandle
    ELSE
       ::nParent := NIL
    ENDIF

    ASSIGN ::nMenu        WITH nMenu
    ASSIGN ::nApplication WITH NULL // WG_ApplObj()
    ASSIGN ::pStruct      WITH pStruct

    //IF ::nMenu == NIL .AND. Left( Upper( ::cClassName ), 4 ) <> "Woop"
    ::nID := snWinID++
    //ENDIF

    //WG_ObjDisplayData( Self )

    //::DisplayData()

    WG_DebugTrace( "TWindowBase:New()", "Self", Self, "::cClassName", ::cClassName )

RETURN Self

METHOD Init( lRegister, lCreate ) CLASS TWindowBase

   WG_DebugTrace( "TWindowBase:Init()" )

//   DEFAULT lRegister TO !GetClassInfo( _GetInstance(), ::cClassName )
   DEFAULT lCreate   TO TRUE
//
//   ::Super:Init()
//   IF lRegister
//      ::RegisterClass()
//   ENDIF
//   IF lCreate
//      ::Create()
//   ENDIF

RETURN Self

METHOD Create() CLASS TWindowBase

   WG_DebugTrace( "TWindowBase:Create() - Before creation", "Self", Self, "::cClassName", ::cClassName, "nHandle", ::nHandle )
    //WG_ObjDisplayData( Self, "TWindowBase" )

   IF GetClassInfo( _GetInstance(), ::cClassName ) == NIL .AND. GetClassInfo( , ::cClassName ) == NIL // !::IsRegistered()
      WG_DebugTrace( "TWindowBase:Create() - Register window" )
      ::Register()
   ENDIF

   //::DisplayData( "TWindowBase" )

   WG_DebugTrace( "TWindowBase:Create()", "::nExStyle", ::nExStyle, "::cClassName", ::cClassName, "::cName", ::cName, ;
                                          "::nStyle", ::nStyle, ;
                                          "::nRow", ::nRow, "::nCol", ::nCol, "::nWidth", ::nWidth, "::nHeight", ::nHeight, ;
                                          "::nParent", ::nParent, "::nMenu", ::nMenu, "::nApplication", ::nApplication,;
                                          "::pStruct", ::pStruct )

   ::nHandle := CreateWindowEx( ::nExStyle, ::cClassName, ::cName, ::nStyle, ;
                                ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                                ::nParent, ::nMenu, ::nApplication, ::pStruct )

   //::DisplayData( "TWindowBase" )

   IF ValType( ::nHandle ) == "N" .AND. ::nHandle > 0
      ::AddWindow( Self )
      WG_DebugTrace( "TWindowBase:Create() - After creation", "Self", Self, "nHandle", ::nHandle )
   ELSE
      //PostQuitMessage(0)
      WG_DebugTrace( "TWindowBase:Create() - Error during creation. Quitting." )
      MessageBox( , "Error. Invalid Window Handle. Quitting" )
      WG_ApplObj():Quit()
      __Quit()
   ENDIF

RETURN Self

METHOD Register() CLASS TWindowBase
  LOCAL nHandle

  WG_DebugTrace( "TWindowBase:Register()", "::cClassName", ::cClassName )

  nHandle := RegisterClassEx( ::nClassStyle, 0/*bWindowProc*/, ::hInstance, ::hIcon, ::hCursor, ::hbrBackground,;
                              ::nMenu, ::cClassName, ::hIconSm )
  IF nHandle <> 0
     ::AddWinClass( Self )
  ENDIF

RETURN nHandle


METHOD FindWindowPos( oWin ) CLASS TWindowBase
   LOCAL nPos
   DEFAULT oWin TO Self
   nPos := aScan( ::aoWindows, {|o| o == oWin } )
RETURN nPos

METHOD FindWindowByID( nID ) CLASS TWindowBase
   LOCAL nPos := aScan( ::aoWindows, {|o| o:nID == nID } )
RETURN IIF( nPos > 0, ::aoWindows[ nPos ], NIL )

METHOD FindWindowByHandle( nHandle AS NUMERIC ) CLASS TWindowBase
   LOCAL nPos := aScan( ::aoWindows, {|o| o:nHandle == nHandle } )
RETURN IIF( nPos > 0, ::aoWindows[ nPos ], NIL )

METHOD FindWindowbyPos( nPos AS NUMERIC ) CLASS TWindowBase
  LOCAL oWin
  IF nPos > 0 .AND. nPos <= Len( ::aoWindows ) THEN oWin := ::aoWindows[ nPos ]
RETURN oWin

METHOD FindWindowByName( cName ) CLASS TWindowBase
   LOCAL nPos := aScan( ::aoWindows, {|o| o:cName == cName } )
RETURN IIF( nPos > 0, ::aoWindows[ nPos ], NIL )



METHOD ReParent( oP AS OBJECT ) CLASS TWindowBase
  LOCAL oOldParent
  IF oP <> NIL
     oOldParent := ::oParent
     ::oParent := oP
     SetParent( ::nHandle, ::oParent:nHandle )
  ENDIF
RETURN oOldParent

METHOD SetEventHandler( bWindowProc AS CODEBLOCK ) CLASS TWindowBase
  LOCAL bOldWndProc := ::bWindowProc
  ::bWindowProc := bWindowProc
RETURN bOldWndProc

METHOD SetParentByHandle( nParent AS NUMERIC ) CLASS TWindowBase
   ::oParent := ::FindWindowByHandle( nParent )
RETURN Self

METHOD SetWindowPos( nhWndInsertAfter, nX, nY, nWidth, nHeight, nFlags ) CLASS TWindowBase
   LOCAL lOk := SetWindowPos( ::nHandle, nhWndInsertAfter, nX, nY, nWidth, nHeight, nFlags )
   UPDATE ::nRow    TO nX      NOT NIL
   UPDATE ::nCol    TO nY      NOT NIL
   UPDATE ::nWidth  TO nWidth  NOT NIL
   UPDATE ::nHeight TO nHeight NOT NIL
RETURN lOk

METHOD WindowProc( nMessage, wParam, lParam ) CLASS TWindowBase
   LOCAL nRet := -1
   IF ValType( ::bWindowProc ) == "B"
      nRet := Eval( ::bWindowProc, ::nHandle, nMessage, wParam, lParam )
   ENDIF
RETURN nRet


METHOD SetBackgroundFromFile( cFileName ) CLASS TWindowBase
   LOCAL hBmp   := WG_GetBitmapFromFile( cFileName )
   ::hbrBackground := CreatePatternBrush( hBmp )
   WG_DebugTrace( "TWindowBase:SetBackgroundFromFile()", "Self", Self, "hBmp", hBmp, "::hbrBackground", ::hbrBackground )
   DeleteObject(hBmp)
RETURN ::hbrBackground

METHOD SetCursorFromFile( cFileName ) CLASS TWindowBase
   ::hCursor := WG_GetCursorFromFile( cFileName )
RETURN ::hCursor

METHOD SetIconFromFile( cFileName ) CLASS TWindowBase
   ::hIcon := WG_GetIconFromFile( cFileName )
RETURN ::hIcon

METHOD SetIconSmFromFile( cFileName ) CLASS TWindowBase
   ::hIconSm := WG_GetIconFromFile( cFileName )
RETURN ::hIconSm

METHOD UnRegister() CLASS TWindowBase
RETURN UnRegisterClass( ::cClassName, ::hInstance )


METHOD GetCurrentWindow() CLASS TWindowBase
  IF ValType( ::oCurrentWindow ) <> "O" THEN MessageBox(0,"Default Windows Object not defined!","Error")
RETURN ::oCurrentWindow

METHOD SetCurrentWindow( oWnd ) CLASS TWindowBase
  LOCAL nOldWnd := ::oCurrentWindow
  ::oCurrentWindow := oWnd
RETURN nOldWnd

FUNCTION WG_GetBitmapFromFile( cFileName )
   LOCAL nHandle := WG_LoadImageFromFile( cFileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE )
RETURN nHandle

FUNCTION WG_GetCursorFromFile( cFileName )
   LOCAL nHandle := WG_LoadImageFromFile( cFileName, IMAGE_CURSOR, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE )
RETURN nHandle

FUNCTION WG_GetIconFromFile( cFileName )
   LOCAL nHandle := WG_LoadImageFromFile( cFileName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE + LR_DEFAULTSIZE )
RETURN nHandle

// WG_LoadImage - Call API LoadImage with some checks
FUNCTION WG_LoadImageFromFile( cFileName, nType, nWidth, nHeight, nLoad )
   LOCAL nHandle
   LOCAL lCheck := TRUE

   IF cFileName == NIL .OR. ValType( cFileName ) <> "C"
      MessageBox( , "WG_LoadImageFromFile() - FileName String not passed" )
      lCheck := FALSE
   ELSEIF !File( cFileName )
      MessageBox( , "WG_LoadImageFromFile() - FileName not exist" )
      lCheck := FALSE
   ENDIF
   IF lCheck
      nHandle := LoadImage( NIL, cFileName, nType, nWidth, nHeight, nLoad )
   ENDIF
RETURN nHandle

EXIT PROCEDURE __WG_TWindowBase_Exit()
   LOCAL aoWinClasses := TWindowBase():aoWinClasses
   WG_DebugTrace( "TWindowBase_Exit_Proc - Unregister Classes", "TWindowBase():aoWinClasses", TWindowBase():aoWinClasses )
   aEval( aoWinClasses, {|o| o:UnRegister() } )
RETURN