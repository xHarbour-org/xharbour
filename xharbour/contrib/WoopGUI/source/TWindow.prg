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

#include "winuser.ch"

//static nWinID

CLASS TWindow FROM TWindowBase

    // Base definitions for CreateWindowEx
    // DATA nExStyle      AS NUMERIC       // Extended style
    // DATA cClassName    AS STRING        // Registered class name
    // DATA cName         AS STRING        // Window name
    // DATA nStyle        AS NUMERIC       // Window style
    // DATA nTop          AS NUMERIC       // horizontal position of window
    // DATA nLeft          AS NUMERIC       // vertical position of window
    // DATA nWidth        AS NUMERIC       // window width
    // DATA nHeight       AS NUMERIC       // window height
    // DATA nParent       AS NUMERIC       // Handle to parent or owner window
    // DATA nMenu         AS NUMERIC       // Handle to menu or to child window
    // DATA nApplication  AS NUMERIC       // Handle to application
    // DATA pStruct                        // window-creation data (not used)
    //
    // DATA nHandle AS NUMERIC PROTECTED   // Handle of this window

    ACCESS nChild      INLINE ::nMenu
    ASSIGN nChild( n ) INLINE (::nMenu := n)

    ACCESS cTitle      INLINE ::cName
    ASSIGN cTitle( n ) INLINE ::cName := n

    DATA lCreated      AS LOGICAL INIT FALSE

    DATA aoChilds      AS ARRAY INIT {} HIDDEN   // Childs windows - controls

    // Extension
    //DATA bWindowProc   AS CODEBLOCK     // Code block to evaluate events
    DATA bAction       AS CODEBLOCK     // Code block for action to execute
    DATA bActivate     AS CODEBLOCK     // Code block for action when active window
    DATA bOnLeftClick  AS CODEBLOCK     // Code block for action when left click window

    //DATA aoChilds      AS ARRAY  INIT {} HIDDEN // Array that contains child windows
    DATA oToolTip      AS OBJECT HIDDEN // Add tooltip control object
    DATA oFont         AS OBJECT HIDDEN // Font object
    // DATA oParent       AS OBJECT HIDDEN // Parent windows object
    DATA oMenu         AS OBJECT HIDDEN // Menu object
    DATA oContextMenu  AS OBJECT HIDDEN // Context Menu Object

    DATA lPixel        AS LOGICAL INIT FALSE

    DATA oDC            AS OBJECT
    DATA oBrush         AS OBJECT
    DATA oBgColor       AS OBJECT
    DATA oFgColor       AS OBJECT

    DATA oStackColors   AS OBJECT
    DATA hDC
    DATA cPaint
    //DATA oBrushFocus    AS OBJECT
    //DATA oBgColorFocus  AS OBJECT
    //DATA oFgColorFocus  AS OBJECT

    // Public methods
    METHOD Activate()
    METHOD New() CONSTRUCTOR
    METHOD AddChild( oChild )               INLINE aAdd( ::aoChilds, oChild )
    //METHOD Center()
    //METHOD CenterOnParent()                 VIRTUAL // FSG - to be implemented
    //METHOD CenterOnScreen()                 VIRTUAL // FSG - to be implemented
    //MESSAGE Centre METHOD  Center()
    //METHOD CentreOnParent VIRTUAL //MESSAGE CentreOnParent METHOD CenterOnParent()
    //METHOD CentreOnScreen VIRTUAL //MESSAGE CentreOnScreen METHOD CenterOnScreen()
    METHOD ChangeColors()
    METHOD Clear()                          VIRTUAL // FSG - to be implemented
    METHOD ClientToScreen()                 VIRTUAL // FSG - to be implemented
    //METHOD Close()                          INLINE CloseWindow( ::nHandle )
    METHOD ConvertDialogToPixelsX( nDlg )   INLINE WG_Dialog2PixelX( nDlg )
    METHOD ConvertPixelsToDialogX( nPxl )   INLINE WG_Pixel2DialogX( nPxl )
    METHOD ConvertDialogToPixelsY( nDlg )   INLINE WG_Dialog2PixelY( nDlg )
    METHOD ConvertPixelsToDialogY( nPxl )   INLINE WG_Pixel2DialogY( nPxl )
    METHOD Create()
    METHOD Destroy()
    METHOD DestroyChildren()                INLINE WG_DebugTrace( "TWindow:DestroyChildren()", "Self", Self, "::aoChilds", ::aoChilds ),;
                                                   aEval(::aoChilds, {|o| o:Destroy()} )
    METHOD DragAcceptFiles()                VIRTUAL // FSG - to be implemented
    //METHOD Disable()                        INLINE EnableWindow( ::nHandle, FALSE )
    //METHOD Enable()                         INLINE EnableWindow( ::nHandle, TRUE )
    METHOD ExecAction()                     INLINE  Eval( ::bAction, Self )

    METHOD FindChildByHandle()
    METHOD FindChildByID()
    METHOD FindChildByName()
    METHOD Fit()                            VIRTUAL // FSG - to be implemented
    METHOD GetBackgroundColor()             INLINE ::oBgColor
    METHOD GetBestSize()                    VIRTUAL // FSG - to be implemented
    METHOD GetCaret()                       VIRTUAL // FSG - to be implemented
    METHOD GetCharHeight()                  INLINE WG_GetCharHeight()
    METHOD GetCharWidth()                   INLINE WG_GetCharWidth()
    METHOD GetChildren()                    INLINE ::aoChilds
    METHOD GetClientSize()                  VIRTUAL // FSG - to be implemented
    METHOD GetConstraints()                 VIRTUAL // FSG - to be implemented
    METHOD GetDropTarget()                  VIRTUAL // FSG - to be implemented
    //METHOD GetEventHandler()                INLINE ::bWindowProc
    //METHOD GetExtraStyle()                  VIRTUAL // FSG - to be implemented
    METHOD GetFont()                        INLINE ::oFont
    METHOD GetForegroundColor()             INLINE ::oFgColor
    //METHOD GetGrandParent()                 VIRTUAL // FSG - to be implemented
    //METHOD GetHandle()                      INLINE ::nHandle
    //METHOD GetId()                          VIRTUAL // FSG - to be implemented
    METHOD GetMessageName( nMsg )           INLINE WG_DecodeMessageName( nMsg )
    //MESSAGE GetLabel()      METHOD  GetValue()
    //METHOD GetParent()                      INLINE  IIF( ValType( ::oParent ) == "O", ::oParent, NIL )
    //METHOD GetParentHandle()                INLINE  IIF( ValType( ::oParent ) == "O", ::oParent:nHandle, NIL )
    METHOD GetPosition()                    VIRTUAL // FSG - to be implemented
    METHOD GetRect()                        VIRTUAL // FSG - to be implemented
    METHOD GetScrollThumb()                 VIRTUAL // FSG - to be implemented
    METHOD GetScrollPos()                   VIRTUAL // FSG - to be implemented
    METHOD GetScrollRange()                 VIRTUAL // FSG - to be implemented
    METHOD GetSize()                        VIRTUAL // FSG - to be implemented
    METHOD GetTextExtent()                  VIRTUAL // FSG - to be implemented
    MESSAGE GetTitle()                       METHOD  GetValue()
    METHOD GetToolTip()                     INLINE ::oToolTip
    METHOD GetUpdateRegion()                VIRTUAL // FSG - to be implemented
    METHOD GetValidator()                   VIRTUAL // FSG - to be implemented
    METHOD GetValue()
    //METHOD GetWindowStyleFlag()             VIRTUAL // FSG - to be implemented
    METHOD HasAction()                      INLINE ( ValType( ::bAction ) == "B" )
    METHOD HasChildren()                    INLINE ( !Empty( ::aoChilds ) )
    METHOD HasEventHandler()                INLINE ( ValType( ::bWindowProc ) == "B" )
    //METHOD Hide()                           INLINE ShowWindow( ::nHandle, SW_HIDE )
    METHOD InitDialog()                     VIRTUAL // FSG - to be implemented
    //MESSAGE IsChild  METHOD WG_IsChild()
    METHOD IsExposed()                      VIRTUAL // FSG - to be implemented
    //MESSAGE IsIconic METHOD WG_IsIconic()
    //METHOD IsShown()                        VIRTUAL // FSG - to be implemented
    //METHOD IsTopLevel()                     VIRTUAL // FSG - to be implemented
    //MESSAGE IsWindow METHOD WG_IsWindow()
    METHOD Layout()                         VIRTUAL // FSG - to be implemented
    METHOD LoadFromResource()               VIRTUAL // FSG - to be implemented
    METHOD Lower()                          VIRTUAL // FSG - to be implemented
    METHOD MakeModal()                      VIRTUAL // FSG - to be implemented
    //METHOD Maximize()                       INLINE ShowWindow( ::nHandle, SW_MAXIMIZE )
    //METHOD Minimize()                       INLINE ShowWindow( ::nHandle, SW_MINIMIZE )
    METHOD Move()

    // Events
    METHOD OnActivate()                     VIRTUAL // FSG - to be implemented
    METHOD OnChar()                         VIRTUAL // FSG - to be implemented
    METHOD OnCharHook()                     VIRTUAL // FSG - to be implemented
    METHOD OnCommand()                      VIRTUAL // FSG - to be implemented
    METHOD OnContextMenu()
    METHOD OnCtlColor()                     VIRTUAL
    METHOD OnClose()                        VIRTUAL // FSG - to be implemented
    METHOD OnCloseWindow()                  VIRTUAL // FSG - to be implemented
    METHOD OnCreate( lParam )               VIRTUAL //INLINE ::Create( lParam )
    METHOD OnDestroy()                      VIRTUAL
    METHOD OnDropFile()                     VIRTUAL // FSG - to be implemented
    METHOD OnEraseBackground()              VIRTUAL // FSG - to be implemented
    METHOD OnIdle()                         VIRTUAL // FSG - to be implemented
    METHOD OnInitDialog()                   VIRTUAL // FSG - to be implemented
    METHOD OnKeyDown()                      VIRTUAL // FSG - to be implemented
    METHOD OnKeyUp()                        VIRTUAL // FSG - to be implemented
    METHOD OnKillFocus()                    VIRTUAL // FSG - to be implemented
    METHOD OnMenuCommand()                  VIRTUAL // FSG - to be implemented
    METHOD OnMenuSelect()                   VIRTUAL // FSG - to be implemented
    //METHOD OnMenuHighlight()                VIRTUAL // FSG - to be implemented
    //METHOD OnMouseEvent()                   VIRTUAL // FSG - to be implemented
    METHOD OnMouseMove()                    VIRTUAL // FSG - to be implemented
    METHOD OnMove()                         VIRTUAL // FSG - to be implemented
    METHOD OnNCDestroy()                    //VIRTUAL // FSG - to be implemented
    METHOD OnNCMouseMove()                  VIRTUAL
    METHOD OnNotify()                       VIRTUAL // FSG - to be implemented

//    METHOD OnPaint()                        VIRTUAL // FSG - to be implemented
   METHOD OnGetMinMaxInfo()                 VIRTUAL
   METHOD OnPaint()                 ;
          INLINE ;
            ::hDC := BeginPaint(::nHandle, ::cPaint),     ;
            ::Paint(), EndPaint(::nHandle, ::cPaint), 0

    METHOD OnHScroll()                      VIRTUAL // FSG - to be implemented
    METHOD OnSetFocus()                     VIRTUAL // FSG - to be implemented
    METHOD OnSize()                         VIRTUAL // FSG - to be implemented
    METHOD OnSysColourChanged()             VIRTUAL // FSG - to be implemented
    METHOD OnSysCommand()                   VIRTUAL // FSG - to be implemented
    METHOD OnTimer()                        VIRTUAL // FSG - to be implemented
    METHOD OnVScroll()                      VIRTUAL // FSG - to be implemented

    //METHOD PopEventHandler()                VIRTUAL // FSG - to be implemented

    METHOD Paint()                          VIRTUAL
    METHOD PopUpMenu()                      VIRTUAL // FSG - to be implemented
    METHOD PopColors()
    METHOD PushColors()
    //METHOD PushEventHandler()               VIRTUAL // FSG - to be implemented
    METHOD Raise()                          VIRTUAL // FSG - to be implemented
    METHOD Refresh()                        INLINE ::Redraw()
                                                   //LOCAL nHWnd, nHWnd := GetFocus(), SetFocus( ::nHandle ), UpdateWindow( ::nHandle ), SetFocus( nHWnd )
    METHOD RemoveChild()                    VIRTUAL // FSG - to be implemented
    //METHOD Reparent()
    //METHOD Restore()                        INLINE ShowWindow( ::nHandle, SW_RESTORE )
    METHOD ScreenToClient()                 VIRTUAL // FSG - to be implemented
    METHOD ScrollWindow()                   VIRTUAL // FSG - to be implemented
    METHOD SetAcceleratorTable()            VIRTUAL // FSG - to be implemented
    METHOD SetAutoLayout()                  VIRTUAL // FSG - to be implemented
    METHOD SetBackgroundColor()
    METHOD SetCaret()                       VIRTUAL // FSG - to be implemented
    METHOD SetClientSize()                  VIRTUAL // FSG - to be implemented
    METHOD SetCursor()                      VIRTUAL // FSG - to be implemented
    METHOD SetConstraints()                 VIRTUAL // FSG - to be implemented
    METHOD SetContextMenu( oMenu )          INLINE ::oContextMenu := oMenu
    METHOD SetDropTarget()                  VIRTUAL // FSG - to be implemented
    //METHOD SetEventHandler()
    //METHOD SetExtraStyle()
    //MESSAGE SetFocus METHOD WG_SetFocus()
    METHOD SetFont()
    METHOD SetFontByAttribute()
    METHOD SetForegroundColor()
    //METHOD SetId()                          VIRTUAL // FSG - to be implemented
    //MESSAGE SetName() METHOD  SetValue()
    METHOD SetPalette()                     VIRTUAL // FSG - to be implemented
    METHOD SetScrollbar()                   VIRTUAL // FSG - to be implemented
    METHOD SetScrollPos()                   VIRTUAL // FSG - to be implemented
    //METHOD SetSize()                        VIRTUAL // FSG - to be implemented
    METHOD SetSizeHints()                   VIRTUAL // FSG - to be implemented
    METHOD SetSizer()                       VIRTUAL // FSG - to be implemented
    //MESSAGE SetTitle() METHOD SetValue()
    //METHOD SetValue()
    METHOD SetValidator()                   VIRTUAL // FSG - to be implemented
    METHOD SetToolTip()
    //METHOD SetWindowStyle()                 VIRTUAL // FSG - to be implemented
    //METHOD SetWindowStyleFlag()             VIRTUAL // FSG - to be implemented
    //METHOD Show()
    METHOD SetMenu()
    //METHOD SetParentByHandle()
    //METHOD SetStyle()
    //METHOD SetWindowPos()
    //METHOD ToTop()                          INLINE BringWindowToTop( ::nHandle )
    METHOD TransferDataFromWindow()         VIRTUAL // FSG - to be implemented
    METHOD TransferDataToWindow()           VIRTUAL // FSG - to be implemented
    METHOD Validate()                       VIRTUAL // FSG - to be implemented
    METHOD WarpPointer()                    VIRTUAL // FSG - to be implemented
    METHOD WindowProc()


    // nChild == nMenu
    //METHOD  nChild() SETGET

    //ON ERROR ErrorHandler( oError )

ENDCLASS

METHOD New( nExStyle, cClassName, cName, nStyle, nTop, nLeft, nWidth, nHeight, oParent, nChild, nApplication, pStruct ) CLASS TWindow

    WG_DebugTrace( "TWindow:New()" )
    //DEFAULT nWinID TO 1000

    // window defaults
    ASSIGN ::nExStyle     WITH nExStyle          DEFAULT WS_EX_LEFT //WS_EX_OVERLAPPEDWINDOW
    ASSIGN ::cClassName   WITH cClassName        DEFAULT WG_ApplObj():oFrmClass:cClassName //"WoopGUIWinClass"
    ASSIGN ::cName        WITH cName             DEFAULT "Window_1"
    ASSIGN ::nStyle       WITH nStyle            DEFAULT WS_OVERLAPPEDWINDOW
    ASSIGN ::nTop         WITH nTop              DEFAULT CW_USEDEFAULT
    ASSIGN ::nLeft        WITH nLeft             DEFAULT CW_USEDEFAULT
    ASSIGN ::nWidth       WITH nWidth            DEFAULT CW_USEDEFAULT
    ASSIGN ::nHeight      WITH nHeight           DEFAULT CW_USEDEFAULT
    IF ValType( oParent ) == "O"
       ::oParent := oParent
       ::nParent := ::oParent:nHandle
    ELSE
       ::nParent := NIL
    ENDIF
    ASSIGN ::nChild       WITH nChild
    ASSIGN ::nApplication WITH NULL // WG_ApplObj()
    ASSIGN ::pStruct      WITH pStruct

    ::oStackColors := TStack():New()

    //ASSIGN ::oBgColor     WITH TSystemSetting():GetColour(COLOR_BTNFACE)
    //ASSIGN ::oFgColor     WITH TSystemSetting():GetColour(COLOR_WINDOWTEXT)

    //IF ::nChild == NIL .AND. Left( Upper( ::cClassName ), 4 ) <> "Woop"
    //   ::nChild := nWinID++
    //ENDIF

    //IF ::nParent <> NIL    THEN ::SetParentByHandle( ::nParent )

   // ObjDisplayData( Self )

    // Low level call
    //::nHandle := CreateWindowEx( ::nExStyle, ::cClassName, ::cName, ::nStyle, ;
    //                             ::nTop, ::nLeft, ::nWidth, ::nHeight, ;
    //                             ::nParent, ::nChild, ::nApplication, ::pStruct )
    //
    //IF ValType( ::nHandle ) == "N" .AND. ::nHandle > 0
    //   WG_ApplObj():AddWindow( Self )
    //ELSE
    //   //PostQuitMessage(0)
    //   WG_ApplObj():Quit()
    //   __Quit()
    //ENDIF

    ::Super:New( nExStyle, cClassName, cName, nStyle, nTop, nLeft, nWidth, nHeight, oParent, nChild, nApplication, pStruct )

RETURN Self

// METHOD Center() CLASS TWindow
// RETURN CenterWindow( ::nHandle )


METHOD Activate() CLASS TWindow
   WG_DebugTrace( "TWindow:Activate()", "Self", Self )
   IF !::lCreated
      ::Create()
   ENDIF

RETURN ::Super:Activate()

METHOD Create( lParam ) CLASS TWindow
   WG_DebugTrace( "TWindow:Create()", "Self", Self )
   ::Super:Create( lParam )
   ::lCreated := TRUE

RETURN Self

METHOD ChangeColors( ncoFore, ncoBack ) CLASS TWindow
   WG_DebugTrace( "TWindow:ChangeColors()" )
   IF ncoFore != NIL THEN ::SetForeGroundColor( ncoFore )
   IF ncoBack != NIL THEN ::SetBackGroundColor( ncoBack )
   ::Redraw(RDW_ERASE + RDW_INVALIDATE)
RETURN Self

METHOD OnContextMenu( wParam, lParam ) CLASS TWindow
   LOCAL nRet := -1
   LOCAL x := GET_X_LPARAM( lParam )
   LOCAL y := GET_Y_LPARAM( lParam )

   WG_DebugTrace( "TWindow:OnContextMenu()" )

   IF ::oContextMenu <> NIL
      nRet := WG_OnContextMenu( ::nHandle, x, y, ::oContextMenu:nHandle )
   ENDIF
RETURN nRet

METHOD PushColors( ncoFore, ncoBack ) CLASS TWindow
   WG_DebugTrace( "TWindow:PushColors()" )
   ::oStackColors:Push( { ::GetForeGroundColor(), ::GetBackGroundColor() } )
   ::ChangeColors( ncoFore, ncoBack )
RETURN Self

METHOD PopColors() CLASS TWindow
   LOCAL aColors
   WG_DebugTrace( "TWindow:PopColors()" )
   IF !( ::oStackColors:Empty() )
      aColors := ::oStackColors:Pop()
      ::ChangeColors( aColors[1], aColors[2] )
   ENDIF
RETURN Self

METHOD Destroy() CLASS TWindow
   WG_DebugTrace( "TWindow:Destroy()", "Self", Self )
   //IF __objHasMethod( Self, "DestroyControls" ) THEN ::DestroyControls()

   //IF ValType( ::oToolTip ) == "O"     THEN ::oToolTip:Destroy()
   //IF ValType( ::oMenu ) == "O"        THEN ::oMenu:Destroy()
   //IF ValType( ::oContextMenu ) == "O" THEN ::oContextMenu:Destroy()
   //
   //IF ::HasChildren()
   //   // Shrink array, when this window will destroy windows destroy childs too
   //   WG_DebugTrace( "TWindow:Destroy() - Has Children, shrink array" )
   //   ::DestroyChildren()
   //ENDIF
   //IF ::HasParent()
   //   // Parent will destroy handle, here only shrink array
   //   WG_DebugTrace( "TWindow:Destroy() - Has Parent, shrink array" )
   //   ::DelWindow()
   //ELSE
   IF ::HasParent()
      WG_DebugTrace( "TWindow:Destroy() - Has parent. Windows automatically destroy" )
   ELSE
      WG_DebugTrace( "TWindow:Destroy() - No Parent, so call DestroyWindow()" )
      DestroyWindow( ::nHandle )
   ENDIF
      //::DelWindow()
      //Self := NIL
   //ENDIF

   //IF ValType( ::oFont ) == "O" THEN ::oFont:Destroy()  // Now in exit font procedure

RETURN NIL

METHOD FindChildByID( nID ) CLASS TWindow
   LOCAL nPos := aScan( ::aoChilds, {|o| o:nID == nID } )
RETURN IIF( nPos > 0, ::aoChilds[ nPos ], NIL )

METHOD FindChildByHandle( nHandle ) CLASS TWindow
   LOCAL nPos := aScan( ::aoChilds, {|o| o:nHandle == nHandle } )
RETURN IIF( nPos > 0, ::aoChilds[ nPos ], NIL )

METHOD FindChildByName( cName ) CLASS TWindow
   LOCAL nPos := aScan( ::aoChilds, {|o| o:cName == cName } )
RETURN IIF( nPos > 0, ::aoChilds[ nPos ], NIL )

METHOD GetValue( nMaxCount AS NUMERIC) CLASS TWindow
   LOCAL cValue := ""
   LOCAL nLen
//   MessageBox(, "GetValue" )
   nLen   := SetWindowText( ::nHandle, @cValue, nMaxCount )
RETURN cValue

// METHOD WG_IsChild( nParent )
// RETURN IsChild( nParent, ::nHandle )

// METHOD WG_IsIconic()
// RETURN IsIconic( ::nHandle )

// METHOD WG_IsWindow()
// RETURN IsWindow( ::nHandle )

METHOD Move( nX, nY, nWidth, nHeight, lRepaint ) CLASS TWindow
   LOCAL nRet
   DEFAULT nX      TO ::nTop
   DEFAULT nY      TO ::nLeft
   DEFAULT nWidth  TO ::nWidth
   DEFAULT nHeight TO ::nHeight
   IF ::lPixel
      nRet := ::Super:Move( nX, nY, nWidth, nHeight, lRepaint )
   ELSE
      nRet := ::Super:Move( ::ConvertDialogToPixelsX( nX ), ;
                            ::ConvertDialogToPixelsY( nY ), ;
                            ::ConvertDialogToPixelsX( nWidth ), ;
                            ::ConvertDialogToPixelsY( nHeight ), ;
                            lRepaint )
   ENDIF
RETURN nRet

// METHOD nChild( nMenu ) CLASS TWindow
//    LOCAL nOldMenu := ::nMenu
//    IF nMenu <> NIL
//       ::nMenu := nMenu
//    ENDIF
// RETURN nOldMenu

// METHOD SetExtraStyle( nExStyle ) CLASS TWindow
//    ::nExStyle := nExStyle
// RETURN SetWindowLongPtr( ::nHandle, GWL_EXSTYLE, nExStyle )

// METHOD WG_SetFocus() CLASS TWindow
//    LOCAL nPrevHandleFocused
//    nPrevHandleFocused := SetFocus( ::nHandle )
// RETURN nPrevHandleFocused


METHOD OnNCDestroy() CLASS TWindow
   WG_DebugTrace( "TWindow:OnNCDestroy() - shrink array", "Self", Self )
   IF ::HasChildren()
      // Windows automatically destroyes all child windows, so here we must release only
      ::aoChilds := {}
      IF ValType( ::oMenu ) == "O"        THEN ::oMenu := NIL
      IF ValType( ::oContextMenu ) == "O" THEN ::oContextMenu := NIL
      IF ValType( ::oToolTip ) == "O"     THEN ::oToolTip := NIL
   ENDIF
   ::DelWindow()
   Self := NIL
RETURN -1

METHOD SetFont( oFont AS OBJECT, lRedraw AS LOGICAL ) CLASS TWindow
  DEFAULT lRedraw TO TRUE
  ::oFont := oFont
RETURN SendMessage( ::nHandle, WM_SETFONT, ::oFont:nHandle, IIF( lRedraw, 1, 0 ) )

METHOD SetFontByAttribute( cFontName AS STRING, nFontSize AS NUMERIC, ;
                          lBold, lItalic, lUnderline, lStrikeOut, lRedraw ) CLASS TWindow
  DEFAULT lRedraw TO TRUE
  ::oFont := TFont():New(cFontName, nFontSize, lItalic, lUnderline, lStrikeOut, lBold)
RETURN SendMessage( ::nHandle, WM_SETFONT, ::oFont:nHandle, IIF( lRedraw, 1, 0 ) )

// METHOD ReParent( oP AS OBJECT ) CLASS TWindow
//   LOCAL oOldParent
//   IF oP <> NIL
//      oOldParent := ::oParent
//      ::oParent := oP
//      SetParent( ::nHandle, ::oParent:nHandle )
//   ENDIF
// RETURN oOldParent

// METHOD SetEventHandler( bWindowProc AS CODEBLOCK ) CLASS TWindow
//   LOCAL bOldWndProc := ::bWindowProc
//   ::bWindowProc := bWindowProc
// RETURN bOldWndProc

METHOD SetBackGroundColor( ncoBgColor ) CLASS TWindow
   LOCAL oOldColor := ::oBgColor

    // Check if the color is a clipper color
    // The rule is that only ncFgColor can be defined as Clipper color
    IF ncoBgColor != NIL
       IF ValType( ncoBgColor ) == "O"
          ::oBgColor := ncoBgColor
       ELSE
          ::oBgColor := TColor():New( ncoBgColor )
       ENDIF
       IF ::oBrush != NIL
          ::oBrush:DelResource()
       ENDIF
       ::oBrush := TBrush():New( ::oBgColor:GetColor() )
    ENDIF

RETURN oOldColor

METHOD SetForeGroundColor( ncoFgColor ) CLASS TWindow
   LOCAL oOldColor := ::oFgColor
   LOCAL cFgColor, cBgColor
   LOCAL aClipColor, cClipForeColor, cClipBackColor

    // Check if the color is a clipper color
    // The rule is that only ncFgColor can be defined as Clipper color
    IF ValType( ncoFgColor ) == "O"
       ::oFgColor := ncoFgColor

    ELSEIF ValType( ncoFgColor ) == "C" .OR. ;
           ValType( ncoFgColor ) == "N"

       IF ( aClipColor := WG_SplitClipperColor( ncoFgColor ) ) != NIL // Is a clipper color

          cClipForeColor := aClipColor[1]
          cClipBackColor := aClipColor[2]

          //MessageBox( , "Color = " + cStr( ncFgColor ) + CRLF +;
          //              "Fore = " + cStr( ncClipForeColor ) + CRLF +;
          //              "Back = " + cStr( ncClipBackColor ) )

          IF !( cClipForeColor == "" )  // ForeGround Color can be not defined
             ::oFgColor := TColor():New( WG_GetClipperColor( cClipForeColor ) )
          ENDIF

          IF !( cClipBackColor == "" )  // BackGround Color can be not defined
             ::SetBackGroundColor( WG_GetClipperColor( cClipBackColor ) )
          ENDIF

       ELSE
          ::oFgColor := TColor():New( ncoFgColor )
       ENDIF

    ENDIF

RETURN oOldColor

METHOD SetMenu( oMenu ) CLASS TWindow
   LOCAL lOk := TRUE
   ::oMenu := oMenu
   IF ::oMenu <> NIL
      ::oMenu:SetParent( Self )  // Define parent window of menu
      lOk := SetMenu( ::nHandle, oMenu:nHandle )
   ENDIF
RETURN lOk

// METHOD SetParentByHandle( nParent AS NUMERIC ) CLASS TWindow
//    ::oParent := WG_ApplObj():FindWindowByHandle( nParent )
// RETURN Self

// METHOD SetStyle( nStyle ) CLASS TWindow
//    ::nStyle := nStyle
// RETURN SetWindowLongPtr( ::nHandle, GWL_STYLE, nStyle )

METHOD SetToolTip( cToolTip ) CLASS TWindow
   LOCAL cOldToolTip
   IF ::oToolTip <> NIL
      cOldToolTip := ::oToolTip:GetValue()
      ::oToolTip:SetValue( cToolTip )
   ELSE
      ::oToolTip := TToolTip():New( Self, cToolTip )
   ENDIF
RETURN cOldToolTip

// METHOD SetValue( cTitle AS STRING) CLASS TWindow
//   ::cName := cTitle
// RETURN SetWindowText( ::nHandle, cTitle )

// METHOD SetWindowPos( nhWndInsertAfter, nX, nY, nWidth, nHeight, nFlags ) CLASS TWindow
//    LOCAL lOk := SetWindowPos( ::nHandle, nhWndInsertAfter, nX, nY, nWidth, nHeight, nFlags )
//    UPDATE ::nTop    TO nX      NOT NIL
//    UPDATE ::nLeft    TO nY      NOT NIL
//    UPDATE ::nWidth  TO nWidth  NOT NIL
//    UPDATE ::nHeight TO nHeight NOT NIL
// RETURN lOk

// METHOD Show() CLASS TWindow
//    ShowWindow( ::nHandle, SW_SHOW )
//    BringWindowToTop(::nHandle)
//    UpdateWindow( ::nHandle )
// RETURN Self

// METHOD WindowProc( nMessage, wParam, lParam ) CLASS TWindow
//    LOCAL nRet := -1
//    IF ValType( ::bWindowProc ) == "B"
//       nRet := Eval( ::bWindowProc, ::nHandle, nMessage, wParam, lParam )
//    ENDIF
// RETURN nRet


METHOD WindowProc( nMsg, wParam, lParam ) CLASS TWindow
   LOCAL nRet := -1  // = TRUE
   LOCAL wmId, wmEvent, wmHandle
   LOCAL oWin

   WG_DebugTrace( "TWindow:WindowProc()", "Self", Self, "nHandle", ::nHandle )

   // Check if there is a user event handler
   IF ::HasEventHandler() // ValType( ::bWindowProc ) == "B"
      // Evaluate User event handler
      nRet := Eval( ::bWindowProc, ::nHandle, nMsg, wParam, lParam )
      //MessageBox(, "TDIALOG Window Proc Event handler return " + cStr( nRet ) )
   ENDIF
   IF nRet == -1
      // Class event handler

      /*
       *  Any method can return nil to get the default message processing.
       */
      DO CASE
         CASE nMsg == WM_GETMINMAXINFO
              ::OnGetMinMaxInfo(lParam)
         CASE nMsg == WM_ACTIVATE
              WG_DebugTrace( "TWindow:WindowProc() - WM_ACTIVATE - ::OnActivate", "Self", Self )
              nRet := ::OnActivate(wParam, lParam)
         CASE nMsg == WM_ACTIVATEAPP   // FSG - to be implemented
         //CASE nMsg == WM_APPCOMMAND    // FSG - to be implemented
         CASE nMsg == WM_CANCELMODE    // FSG - to be implemented
         CASE nMsg == WM_CHAR
              nRet := ::OnChar(wParam, lParam)
         CASE nMsg == WM_COMMAND
              WG_DebugTrace( "TWindow:WindowProc() - WM_COMMAND - ::OnCommand", "Self", Self )
              nRet := ::OnCommand(wParam, lParam)
         CASE nMsg == WM_CONTEXTMENU
              nRet := ::OnContextMenu(wParam, lParam)
         CASE nMsg == WM_CLOSE
              WG_DebugTrace( "TWindow:WindowProc() - WM_CLOSE - ::OnClose", "Self", Self )
              nRet := ::OnClose(wParam, lParam)
         CASE nMsg == WM_CREATE
              WG_DebugTrace( "TWindow:WindowProc() - WM_CREATE - ::OnCreate", "Self", Self )
              nRet := ::OnCreate(lParam)
         CASE nMsg == WM_CTLCOLORDLG
              //SetBrushOrgEx(wParam, 11,11,NIL)
              //nRet := GetStockObject(NULL_BRUSH)
              //nRet := ::OnCtlColor(wParam, lParam)
         CASE nMsg == WM_CTLCOLORBTN
              nRet := ::OnCtlColor(wParam, lParam)
         CASE nMsg == WM_CTLCOLORSTATIC
              nRet := ::OnCtlColor(wParam, lParam)
         CASE nMsg == WM_CTLCOLOREDIT
              nRet := ::OnCtlColor(wParam, lParam)
         //CASE nMsg == WM_CTLCOLOR
         //     nRet := ::OnCtlColor(wParam, LoWord(lParam), HiWord(lParam))
         CASE nMsg == WM_DRAWITEM
              nRet := ::OnDrawItem(wParam, lParam )
         CASE nMsg == WM_DESTROY
              WG_DebugTrace( "TWindow:WindowProc() - WM_DESTROY - ::OnDestroy", "Self", Self )
              nRet := ::OnDestroy()
         CASE nMsg == WM_DROPFILES
              nRet := ::OnDropFile(wParam)
         CASE nMsg == WM_HSCROLL
              nRet := ::OnHScroll(wParam, LoWord(lParam), HiWord(lParam))
         CASE nMsg == WM_INITDIALOG
              WG_DebugTrace( "TWindow:WindowProc() - WM_INITDIALOG - ::OnInitDialog", "Self", Self )
              nRet := ::OnInitDialog(wParam, lParam)
         CASE nMsg == WM_KEYDOWN
              nRet := ::OnKeyDown(wParam, lParam)
         CASE nMsg == WM_KILLFOCUS
              WG_DebugTrace( "TWindow:WindowProc() - WM_KILLFOCUS - ::OnKillFocus", "Self", Self )
              IF ::lStartFocusEvents THEN nRet := ::OnKillFocus()
         //CASE nMsg == WM_LBUTTONDBLCLK
         //     nRet := ::OnLDblClick(LoWord(lParam), HiWord(lParam), wParam)
         //CASE nMsg == WM_LBUTTONDOWN
         //     nRet := ::OnLBtnDown(LoWord(lParam), HiWord(lParam), wParam)
         //CASE nMsg == WM_LBUTTONUP
         //     nRet := ::OnLBtnUp(LoWord(lParam), HiWord(lParam), wParam)
         //CASE nMsg == WM_MENUCOMMAND  // THIS COMMAND EXIST IN WINVER > 5
         //     nRet := ::OnMenuCommand(wParam, lParam)
         //CASE nMsg == WM_MENUSELECT
         //     nRet := ::OnMenuSelect(wParam, lParam)
         //CASE nMsg == WM_MDIACTIVATE
         //     nRet := ::OnMDIActivate(wParam == 1, LoWord(lParam), HiWord(lParam))
         CASE nMsg == WM_MOUSEMOVE
              nRet := ::OnMouseMove(LoWord(lParam), HiWord(lParam), wParam)
         CASE nMsg == WM_NCDESTROY
              WG_DebugTrace( "TWindow:WindowProc() - WM_NCDESTROY - ::OnNCDestroy", "Self", Self )
              nRet := ::OnNCDestroy()
         //     if (nRet := ::OnNCDestroy()) == nil
         //         if (nRet := ::DefaultMessage(nMsg, wParam, lParam)) == nil
         //             // must not be nil, as ::DelWindow() will remove ::hWnd
         //             nRet := 0
         //         endif
         //     endif
         //     ::DelWindow()   // (saSelf, sahWnd)
         //     if !empty(::hWnd)
         //         MessageBox(0, "::hWnd != nil during WM_NCDESTROY", ::ClassName)
         //         ::hWnd = nil
         //     endif
         //     ::Axit()
         CASE nMsg == WM_NCMOUSEMOVE
              nRet := ::OnNCMouseMove( wParam, lParam )
         CASE nMsg == WM_MOVE   // x, y
              WG_DebugTrace( "TWindow:WindowProc() - WM_MOVE - ::OnMove", "Self", Self )
              nRet := ::OnMove(LoWord(lParam), HiWord(lParam))
         CASE nMsg == WM_NOTIFY
              WG_DebugTrace( "TWindow:WindowProc() - WM_NOTIFY - ::OnNotify", "Self", Self )
              nRet := ::OnNotify( wParam, lParam )
         CASE nMsg == WM_PAINT
              WG_DebugTrace( "TWindow:WindowProc() - WM_PAINT - ::OnPaint", "Self", Self )
              nRet := ::OnPaint()
         //CASE nMsg == WM_RBUTTONDOWN
         //     nRet := ::OnRBtnDown(LoWord(lParam), HiWord(lParam), wParam)
         //CASE nMsg == WM_RBUTTONUP
         //     nRet := ::OnRBtnUp(LoWord(lParam), HiWord(lParam), wParam)
         CASE nMsg == WM_SETFOCUS
              WG_DebugTrace( "TWindow:WindowProc() - WM_SETFOCUS - ::OnSetFocus", "Self", Self )
              IF ::lStartFocusEvents THEN nRet := ::OnSetFocus()
         CASE nMsg == WM_SIZE
              nRet := ::OnSize(wParam, LoWord(lParam), HiWord(lParam))
         CASE nMsg == WM_SYSCOMMAND
              nRet := ::OnSysCommand(wParam, LoWord(lParam), HiWord(lParam))
         CASE nMsg == WM_TIMER
              nRet := ::OnTimer( wParam )
         //CASE nMsg == WM_QUERYENDSESSION
         //     nRet := ::OnQueryEndSession()
         CASE nMsg == WM_VSCROLL
              nRet := ::OnVScroll(wParam, LoWord(lParam), HiWord(lParam))
      ENDCASE

      IF ValType( nRet ) <> "N" .OR. nRet == nil
         //return DefWindowProc(hWnd, nMsg, wParam, lParam)
         //nRet := ::DefaultMessage(nMsg, wParam, lParam)
         nRet := -1
      ENDIF

   ENDIF
   IF nRet == -1
      // Standard Event Handler
      nRet := ::Super:WindowProc( nMsg, wParam, lParam )
   ENDIF

RETURN nRet


FUNCTION WG_WriteEvents( hWnd, nMsg, wParam, lParam )
   WG_ApplObj():EventsWrite( "EVE", hWnd, nMsg, wParam, lParam )
RETURN 0




// ----------------------------------------------------
//
// W_DefWndEvents( hWnd, nMsg, wParam, lParam ) --> Bool
//
// interface between Windows API event handling and Class handling
// ----------------------------------------------------
FUNCTION WG_DefWndEvents( hWnd, nMsg, wParam, lParam )
  LOCAL oWin, oInCstr
  LOCAL nRet := -1
  //LOCAL hPrevWndProc :=

  // Send messages to function that write events.log for debug - See TApplication
  //WG_DebugTrace( "WG_DefWndEvents" )
  WG_ApplObj():EventsWrite( "WND", hWnd, nMsg, wParam, lParam )

  DO CASE
     CASE nMsg == WM_INITDIALOG
          oWin         := aTail( TWindow():GetWindows() )
          oWin:nHandle := hWnd
          //oWin:DisplayData()
  ENDCASE

  // Search window
  oWin      := TWindow():FindWindowByHandle( hwnd )
  IF oWin <> NIL
     //WG_DebugTrace( "WG_DefWndEvents - Call oWin:WindowProc" )
     // Call his window procedure
     nRet := oWin:WindowProc( nMsg, wParam, lParam )
  ELSE
     IF ( oInCstr := TWindowBase():GetWindowInCreation() ) <> NIL .AND. ;
          oInCstr:nHandle == 0 .AND. ;
          nMsg == WM_CREATE
        WG_DebugTrace( "WG_DefWndEvents - oWin == NIL. Call oInCstr:WindowProc", "oInCstr", oInCstr, "nHandle", oInCstr:nHandle )
        oInCstr:nHandle := hWnd
        nRet := oInCstr:WindowProc( nMsg, wParam, lParam )
     ENDIF
  ENDIF
  IF nRet == NIL .OR. nRet == -1
     // Windows not yet create
     //WG_DebugTrace( "WG_DefWndEvents - Call DefWindowProc()", "nRet", nRet )
     nRet := DefWindowProc(hWnd, nMsg, wParam, lParam)  // C CALL
  ENDIF

  // I return to c call function ( Call DefWindowProc() )
RETURN nRet



// #include "error.ch"
//
// METHOD ErrorHandler() CLASS TWindow
//    LOCAL oErr
//    // Qui la gestione errori
//    MessageBox( , "Errore: parametri"+str(PCOUNT())  )
//    oErr := ErrorNew()
//       oErr:severity    := ES_ERROR
//       oErr:genCode     := EG_OPEN
//       oErr:subSystem   := "BASE"
//       oErr:SubCode     := 2009
//       oErr:Description := "Errore generico"
//       Eval( ErrorBlock(), oErr )
//
// RETURN TRUE

/*
// ----------------------------------------------------
//
// W_WndEvents( hWnd, nMsg, wParam, lParam ) --> Bool
//
// ----------------------------------------------------
FUNCTION W_WndEvents( hWnd, nMsg, wParam, lParam )
  Local i,procname,c,z,x,ContrlCount,FormCount

    //PAINTSTRUCT ps;
    //HDC hdc;
    LOCAL nRet := -1
    LOCAL wmId, wmEvent, wmHandle
    LOCAL nMouseState
    LOCAL xPos, yPos
    //LOCAL oWin   // Oggetto Window o Controllo
    LOCAL oItem, oParent
    LOCAL aoWindows := WG_ApplObj():aoWindows // Array of windows and controls
    LOCAL oWin := WG_ApplObj():FindWindowByHandle( hWnd )
    LOCAL oWinF

    //WG_ApplObj():EventsWrite( hWnd, nMsg, wParam, lParam )

    IF oWin <> NIL
       nRet := oWin:WindowProc( nMsg, wParam, lParam )
       IF ValType( nRet ) <> "N" THEN nRet := -1 // To handle a NIL or Self return
    ENDIF

    IF nRet == -1
       DO CASE

         // CASE nMsg == WM_MENUCOMMAND (WINVER >= 5.00)
         // CASE nMsg == WM_COMMAND
         //    wmId    = LOWORD(wParam) // menu identifier
         //    wmEvent = HIWORD(wParam) // 0 = menu, 1 = accelerator
         //    wmHandle = lParam        // control handle
         //
         //
         //    //MessageBox( , "Passato" )
         //
         //    if wmEvent == 0    // Command from menu or window
         //
         //       if IsWindow( wmHandle ) // Is from a window
         //
         //          i := aScan( aoWindows, {|W| W:nHandle == wmHandle } )
         //
         //          //MessageBox( , "Finestra I = " + STR(I) )
         //
         //          IF i > 0
         //             oWinF := aoWindows[i]
         //             IF oWinF:HasAction()
         //                //MessageBox( , "Passato" )
         //                oWinF:ExecAction( oWinF )
         //             ENDIF
         //          ENDIF
         //
         //      // else // IsMenu( wmHandle )
         //      //
         //      //    i := aScan( aoWindows, {|W| W:nHandle == hWnd } )
         //      //
         //      //    // MessageBox( , "Trovata finestra Menu I = " + STR(I) )
         //      //
         //      //    IF i > 0
         //      //       oWinF  := aoWindows[i]
         //      //       oItem := oWinF:FindMenuItem( wmId )
         //      //
         //      //       IF oItem <> NIL
         //      //          //MessageBox( , "Trovato item" )
         //      //          IF oItem:HasAction()
         //      //             //MessageBox( , "Passato" )
         //      //             oItem:ExecAction( oWinF )
         //      //          ENDIF
         //      //       ENDIF
         //      //    ENDIF
         //       nRet := 0
         //       endif
         //
         //    endif



         CASE nMsg == WM_ACTIVATE
            wmId    = LOWORD(wParam) // Remember, these are...
            wmEvent = HIWORD(wParam) // ...different for Win32!

            DO CASE
               CASE wmId == WA_ACTIVE
               CASE wmId == WA_CLICKACTIVE
               CASE wmId == WA_INACTIVE
            ENDCASE

            i := aScan( aoWindows, hWnd )

            IF i > 0
               oWinF := aoWindows[i]
               IF ValType( oWinF:bActivate ) == "B"
                  Eval( oWinF:bActivate )
               ENDIF
            ENDIF


         *CASE nMsg == WM_ACTIVATEAPP
         *CASE nMsg == WM_CANCELMODE
         *CASE nMsg == WM_CHILDACTIVATE
        // CASE nMsg == WM_CLOSE
        //      // Create the message box. If the user clicks
        //      // the Yes button, destroy the main window.
        //
        //      // FSG - Relate to application var if you want confirm
        //      if (MessageBox(hwnd, "Are you sure ?", "Quit Application", ;
        //                           MB_YESNO) == IDYES)
        //          DestroyWindow(hwnd)
        //          nRet := 0
        //      else
        //          nRet := 0
        //      endif

         *CASE nMsg == WM_COMPACTING
         *CASE nMsg == WM_CREATE
         CASE nMsg == WM_DESTROY
              PostQuitMessage( 0 )
              nRet := 0

         *CASE nMsg == WM_ENABLE
         *CASE nMsg == WM_ENTERSIZEMOVE
         *CASE nMsg == WM_EXITSIZEMOVE
         *CASE nMsg == WM_GETFONT
         *CASE nMsg == WM_GETICON
         *CASE nMsg == WM_GETTEXT
         *CASE nMsg == WM_GETTEXTLENGTH
         *CASE nMsg == WM_INPUTLANGCHANGE
         *CASE nMsg == WM_INPUTLANGCHANGEREQUEST
         *CASE nMsg == WM_MOVE
         *CASE nMsg == WM_MOVING
         *CASE nMsg == WM_NCACTIVATE
         *CASE nMsg == WM_NCCALCSIZE
         *CASE nMsg == WM_NCCREATE
         *CASE nMsg == WM_NCDESTROY
         *CASE nMsg == WM_NULL
         *CASE nMsg == WM_PAINT
         *CASE nMsg == WM_PARENTNOTIFY
         *CASE nMsg == WM_QUERYDRAGICON
         *CASE nMsg == WM_QUERYOPEN
         CASE nMsg == WM_QUIT
         *CASE nMsg == WM_SETFONT
         *CASE nMsg == WM_SETICON

         CASE nMsg == WM_SETFOCUS


         *CASE nMsg == WM_SETTEXT
         *CASE nMsg == WM_SHOWWINDOW
       //  CASE nMsg == WM_SIZE
       //
       //     // Status bar repaint check
       //     oItem := WG_ApplObj():FindWindowbyHandle( hwnd )
       //     IF oItem <> NIL
       //        // Has object a status bar ?
       //        IF __objHasData( oItem, "OSTATUSBAR" )
       //           // If so, is initialized
       //           IF ValType( oItem:oStatusBar ) == "O"
       //              // Ok. Repaint.
       //              oItem:oStatusBar:Repaint()
       //           ENDIF
       //        ENDIF
       //     ENDIF

         *CASE nMsg == WM_SIZING
         *CASE nMsg == WM_STYLECHANGED
         *CASE nMsg == WM_STYLECHANGING
         *CASE nMsg == WM_THEMECHANGED
         *CASE nMsg == WM_USERCHANGED
         *CASE nMsg == WM_WINDOWPOSCHANGED
         *CASE nMsg == WM_WINDOWPOSCHANGING

         CASE  nMsg == WM_LBUTTONDOWN
            nMouseState = wParam
            xPos = GET_X_LPARAM(lParam)
            yPos = GET_Y_LPARAM(lParam)

            // Find window
            i := aScan( aoWindows, hWnd )
            IF i > 0
               oWinF := aoWindows[i]
               IF ValType( oWinF:bOnLeftClick ) == "B"
                  Eval( oWinF:bOnLeftClick )
               ENDIF
            ENDIF

           CASE  nMsg == WM_CONTEXTMENU

// FSG - from Roberto Lopez MiniGUI - change for WoopGUI
//
//         I := Ascan ( _aFormhandles , hWnd )
//         if i > 0
//            if _aFormContextMenuHandle [i] != 0
//                TrackPopupMenu ( _aFormContextMenuHandle [i]  , LOWORD(lparam) , HIWORD(lparam) , hWnd )
//            Endif
//         EndIf

          CASE  nMsg == WM_TIMER

// FSG - from Roberto Lopez MiniGUI - change for WoopGUI
//
//        i := Ascan ( _aControlIds , wParam )
//
//        if i > 0
//            procname = _aControlProcedures [i]
//            if ValType( procname ) == "C"
//                   do &procname
//            else
//                   Eval( procname )
//            Endif
//        EndIf

       ENDCASE
    ENDIF

return nRet
*/
