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

static nWinID

CLASS TWindowBase

    PROTECTED:
    DATA nHandle       AS NUMERIC       // the window handle
    DATA noldWndProc   AS NUMERIC       // the old window proc (we subclass all windows)
    DATA oMenu         AS OBJECT        // Object Menu, if any
    
    PUBLIC:
    
    // Base definitions for CreateWindowEx
    DATA nExStyle      AS NUMERIC       // Extended style
    DATA cClassName    AS STRING        // Registered class name
    DATA cName         AS STRING        // Window name
    DATA nStyle        AS NUMERIC       // Window style
    DATA nRow          AS NUMERIC       // horizontal position of window
    DATA nCol          AS NUMERIC       // vertical position of window
    DATA nWidth        AS NUMERIC       // window width
    DATA nHeight       AS NUMERIC       // window height
    DATA nParent       AS NUMERIC       // Handle to parent or owner window
    DATA nMenu         AS NUMERIC       // Handle to menu or to child window
    DATA nApplication  AS NUMERIC       // Handle to application
    DATA pStruct                        // window-creation data (not used)

    DATA nHandle AS NUMERIC PROTECTED   // Handle of this window


    // Extension
    DATA bWindowProc   AS CODEBLOCK     // Code block to evaluate events
    DATA bAction       AS CODEBLOCK     // Code block for action to execute
    DATA bActivate     AS CODEBLOCK     // Code block for action when active window
    DATA bOnLeftClick  AS CODEBLOCK     // Code block for action when left click window

    DATA aoChilds      AS ARRAY  INIT {} HIDDEN // Array that contains child windows
    DATA oToolTip      AS OBJECT HIDDEN // Add tooltip control object
    DATA oFont         AS OBJECT HIDDEN // Font object
    DATA oParent       AS OBJECT HIDDEN // Parent windows object
    DATA oMenu         AS OBJECT HIDDEN // Menu object
    DATA oWindowDef    AS OBJECT HIDDEN // Windows class definition object

    // Public methods
    METHOD  Activate()                       INLINE ShowWindow( ::nHandle, SW_SHOW )
    METHOD  New() CONSTRUCTOR
    METHOD  AddChild()                       BLOCK {|Self, oChild| aAdd( ::aoChilds, oChild ) }
    METHOD  CaptureMouse()                   VIRTUAL // FSG - to be implemented
    METHOD  Center()
    METHOD  CenterOnParent()                 VIRTUAL // FSG - to be implemented
    METHOD  CenterOnScreen()                 VIRTUAL // FSG - to be implemented
    MESSAGE Centre METHOD  Center()
    METHOD  CentreOnParent VIRTUAL //MESSAGE CentreOnParent METHOD CenterOnParent()
    METHOD  CentreOnScreen VIRTUAL //MESSAGE CentreOnScreen METHOD CenterOnScreen()
    METHOD  Clear()                          VIRTUAL // FSG - to be implemented
    METHOD  ClientToScreen()                 VIRTUAL // FSG - to be implemented
    METHOD  Close()                          INLINE CloseWindow( ::nHandle )
    METHOD  ConvertDialogToPixels()          VIRTUAL // FSG - to be implemented
    METHOD  ConvertPixelsToDialog()          VIRTUAL // FSG - to be implemented
    METHOD  Destroy()
    METHOD  DestroyChildren()                INLINE aEval(::aoChilds, {|o| o:Destroy()} ) //VIRTUAL // FSG - to be implemented
    METHOD  DragAcceptFiles()                VIRTUAL // FSG - to be implemented
    METHOD  Disable()                        INLINE EnableWindow( ::nHandle, FALSE )
    METHOD  Enable()                         INLINE EnableWindow( ::nHandle, TRUE )
    METHOD  ExecAction()                     BLOCK  {|Self, oWin| Eval( ::bAction, oWin ) }
    METHOD  FindFocus()                      VIRTUAL // FSG - to be implemented
    METHOD  FindWindow()                     VIRTUAL // FSG - to be implemented
    METHOD  FindWindowByID()                 VIRTUAL // FSG - to be implemented
    METHOD  FindWindowByName()               VIRTUAL // FSG - to be implemented
    METHOD  Fit()                            VIRTUAL // FSG - to be implemented
    METHOD  GetBackgroundColour()            VIRTUAL // FSG - to be implemented
    METHOD  GetBestSize()                    VIRTUAL // FSG - to be implemented
    METHOD  GetCaret()                       VIRTUAL // FSG - to be implemented
    METHOD  GetCharHeight()                  VIRTUAL // FSG - to be implemented
    METHOD  GetCharWidth()                   VIRTUAL // FSG - to be implemented
    METHOD  GetChildren()                    VIRTUAL // FSG - to be implemented
    METHOD  GetClientSize()                  VIRTUAL // FSG - to be implemented
    METHOD  GetConstraints()                 VIRTUAL // FSG - to be implemented
    METHOD  GetDropTarget()                  VIRTUAL // FSG - to be implemented
    METHOD  GetEventHandler()                INLINE ::bWindowProc
    METHOD  GetExtraStyle()                  VIRTUAL // FSG - to be implemented
    METHOD  GetFont()                        INLINE ::oFont
    METHOD  GetForegroundColour()            VIRTUAL // FSG - to be implemented
    METHOD  GetGrandParent()                 VIRTUAL // FSG - to be implemented
    METHOD  GetHandle()                      INLINE ::nHandle
    METHOD  GetId()                          VIRTUAL // FSG - to be implemented
    METHOD  GetMessageName()
    //MESSAGE GetLabel()      METHOD  GetValue()
    METHOD  GetParent()                      INLINE  IIF( ValType( ::oParent ) == "O", ::oParent, NIL )
    METHOD  GetParentHandle()                INLINE  IIF( ValType( ::oParent ) == "O", ::oParent:nHandle, NIL )
    METHOD  GetPosition()                    VIRTUAL // FSG - to be implemented
    METHOD  GetRect()                        VIRTUAL // FSG - to be implemented
    METHOD  GetScrollThumb()                 VIRTUAL // FSG - to be implemented
    METHOD  GetScrollPos()                   VIRTUAL // FSG - to be implemented
    METHOD  GetScrollRange()                 VIRTUAL // FSG - to be implemented
    METHOD  GetSize()                        VIRTUAL // FSG - to be implemented
    METHOD  GetTextExtent()                  VIRTUAL // FSG - to be implemented
    MESSAGE GetTitle()      METHOD  GetValue()
    METHOD  GetToolTip()
    METHOD  GetUpdateRegion()                VIRTUAL // FSG - to be implemented
    METHOD  GetValidator()                   VIRTUAL // FSG - to be implemented
    METHOD  GetValue()
    METHOD  GetWindowStyleFlag()             VIRTUAL // FSG - to be implemented
    METHOD  HasAction()                      INLINE ( ValType( ::bAction ) == "B" )
    METHOD  HasEventHandler()                INLINE ( ValType( ::bWindowProc ) == "B" )
    METHOD  Hide()                           INLINE ShowWindow( ::nHandle, SW_HIDE )
    METHOD  InitDialog()                     VIRTUAL // FSG - to be implemented
    MESSAGE IsChild  METHOD W_IsChild()
    METHOD  IsEnabled()                      VIRTUAL // FSG - to be implemented
    METHOD  IsExposed()                      VIRTUAL // FSG - to be implemented
    MESSAGE IsIconic METHOD W_IsIconic()
    METHOD  IsShown()                        VIRTUAL // FSG - to be implemented
    METHOD  IsTopLevel()                     VIRTUAL // FSG - to be implemented
    MESSAGE IsWindow METHOD W_IsWindow()
    METHOD  Layout()                         VIRTUAL // FSG - to be implemented
    METHOD  LoadFromResource()               VIRTUAL // FSG - to be implemented
    METHOD  Lower()                          VIRTUAL // FSG - to be implemented
    METHOD  MakeModal()                      VIRTUAL // FSG - to be implemented
    METHOD  Maximize()                       INLINE ShowWindow( ::nHandle, SW_MAXIMIZE )
    METHOD  Minimize()                       INLINE ShowWindow( ::nHandle, SW_MINIMIZE )
    METHOD  Move()
    METHOD  OnActivate()                     VIRTUAL // FSG - to be implemented
    METHOD  OnChar()                         VIRTUAL // FSG - to be implemented
    METHOD  OnCharHook()                     VIRTUAL // FSG - to be implemented
    METHOD  OnCommand()                      VIRTUAL // FSG - to be implemented
    METHOD  OnClose()                        VIRTUAL // FSG - to be implemented
    METHOD  OnCloseWindow()                  VIRTUAL // FSG - to be implemented
    METHOD  OnDropFile()                     VIRTUAL // FSG - to be implemented
    METHOD  OnEraseBackground()              VIRTUAL // FSG - to be implemented
    METHOD  OnKeyDown()                      VIRTUAL // FSG - to be implemented
    METHOD  OnKeyUp()                        VIRTUAL // FSG - to be implemented
    METHOD  OnKillFocus()                    VIRTUAL // FSG - to be implemented
    METHOD  OnIdle()                         VIRTUAL // FSG - to be implemented
    METHOD  OnMenuCommand()                  VIRTUAL // FSG - to be implemented
    METHOD  OnMenuHighlight()                VIRTUAL // FSG - to be implemented
    METHOD  OnMouseEvent()                   VIRTUAL // FSG - to be implemented
    METHOD  OnMove()                         VIRTUAL // FSG - to be implemented
    METHOD  OnPaint()                        VIRTUAL // FSG - to be implemented
    METHOD  OnScroll()                       VIRTUAL // FSG - to be implemented
    METHOD  OnSetFocus()                     VIRTUAL // FSG - to be implemented
    METHOD  OnSize()                         VIRTUAL // FSG - to be implemented
    METHOD  OnSysColourChanged()             VIRTUAL // FSG - to be implemented
    METHOD  PopEventHandler()                VIRTUAL // FSG - to be implemented
    METHOD  PopUpMenu()                      VIRTUAL // FSG - to be implemented
    METHOD  PushEventHandler()               VIRTUAL // FSG - to be implemented
    METHOD  Raise()                          VIRTUAL // FSG - to be implemented
    METHOD  Refresh()                        VIRTUAL // FSG - to be implemented
    METHOD  ReleaseMouse()                   VIRTUAL // FSG - to be implemented
    METHOD  RemoveChild()                    VIRTUAL // FSG - to be implemented
    METHOD  Reparent()                       
    METHOD  Restore()                        INLINE ShowWindow( ::nHandle, SW_RESTORE )
    METHOD  ScreenToClient()                 VIRTUAL // FSG - to be implemented
    METHOD  ScrollWindow()                   VIRTUAL // FSG - to be implemented
    METHOD  SetAcceleratorTable()            VIRTUAL // FSG - to be implemented
    METHOD  SetAutoLayout()                  VIRTUAL // FSG - to be implemented
    METHOD  SetBackgroundColour()            VIRTUAL // FSG - to be implemented
    METHOD  SetCaret()                       VIRTUAL // FSG - to be implemented
    METHOD  SetClientSize()                  VIRTUAL // FSG - to be implemented
    METHOD  SetCursor()                      VIRTUAL // FSG - to be implemented
    METHOD  SetConstraints()                 VIRTUAL // FSG - to be implemented
    METHOD  SetDropTarget()                  VIRTUAL // FSG - to be implemented
    METHOD  SetEventHandler()                
    METHOD  SetExtraStyle()
    MESSAGE SetFocus METHOD W_SetFocus()
    METHOD  SetFont()
    METHOD  SetFontByAttribute()
    METHOD  SetForegroundColour()            VIRTUAL // FSG - to be implemented
    METHOD  SetId()                          VIRTUAL // FSG - to be implemented
    MESSAGE SetName() METHOD  SetValue()
    METHOD  SetPalette()                     VIRTUAL // FSG - to be implemented
    METHOD  SetScrollbar()                   VIRTUAL // FSG - to be implemented
    METHOD  SetScrollPos()                   VIRTUAL // FSG - to be implemented
    METHOD  SetSize()                        VIRTUAL // FSG - to be implemented
    METHOD  SetSizeHints()                   VIRTUAL // FSG - to be implemented
    METHOD  SetSizer()                       VIRTUAL // FSG - to be implemented
    MESSAGE SetTitle() METHOD SetValue()
    METHOD  SetValue()
    METHOD  SetValidator()                   VIRTUAL // FSG - to be implemented
    METHOD  SetToolTip()
    METHOD  SetWindowStyle()                 VIRTUAL // FSG - to be implemented
    METHOD  SetWindowFlag()                  VIRTUAL // FSG - to be implemented
    METHOD  Show()
    METHOD  SetMenu()
    METHOD  SetParentByHandle()
    METHOD  SetStyle()
    METHOD  SetWindowPos()
    METHOD  ToTop()                          INLINE BringWindowToTop( ::nHandle )
    METHOD  TransferDataFromWindow()         VIRTUAL // FSG - to be implemented
    METHOD  TransferDataToWindow()           VIRTUAL // FSG - to be implemented
    METHOD  Validate()                       VIRTUAL // FSG - to be implemented
    METHOD  WarpPointer()                    VIRTUAL // FSG - to be implemented
    METHOD  WindowProc()


    // nChild == nMenu
    METHOD  nChild() SETGET

    ON ERROR ErrorHandler( oError )

ENDCLASS

METHOD New( nExStyle, cClassName, cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, nChild, nApplication, pStruct )

    DEFAULT nWinID TO 1000
    //ParamDisplay( Self, hb_aparams() )

    // window defaults
    ::oWindowDef := ApplObj():oFrmClass
    ASSIGN ::nExStyle     WITH nExStyle          DEFAULT WS_EX_LEFT //WS_EX_OVERLAPPEDWINDOW
    ASSIGN ::cClassName   WITH cClassName        DEFAULT ::oWindowDefs:cClassName //"WoopGUIWinClass"
    ASSIGN ::cName        WITH cName             DEFAULT "Window_1"
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
    ASSIGN ::nChild       WITH nChild  
    ASSIGN ::nApplication WITH NULL // ApplObj()
    ASSIGN ::pStruct      WITH pStruct 
    
    IF ::nChild == NIL .AND. Left( Upper( ::cClassName ), 4 ) <> "Woop"
       ::nChild := nWinID++
    ENDIF

    //IF ::nParent <> NIL    THEN ::SetParentByHandle( ::nParent )
    //IF ::oWindowDef <> NIL THEN ::SetEventHandler( ::oWindowDef:bWindowProc )

   // ObjDisplayData( Self )

    // Low level call
    ::nHandle := CreateWindowEx( ::nExStyle, ::cClassName, ::cName, ::nStyle, ;
                                 ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                                 ::nParent, ::nChild, ::nApplication, ::pStruct )

    IF ValType( ::nHandle ) == "N" .AND. ::nHandle > 0
       ApplObj():AddWindow( Self )
    ELSE
       //PostQuitMessage(0)
       ApplObj():Quit()
       __Quit()
    ENDIF

RETURN Self

METHOD Center()
RETURN CenterWindow( ::nHandle )

METHOD Destroy()
   //IF __objHasMethod( Self, "DestroyControls" ) THEN ::DestroyControls()
   ::DestroyChildren()
   IF ValType( ::oMenu ) == "O" THEN ::oMenu:Destroy()
   IF ValType( ::oFont ) == "O" THEN ::oFont:Destroy()
   IF ValType( ::oToolTip ) == "O" THEN ::oToolTip:Destroy()
RETURN DestroyWindow( ::nHandle )

METHOD GetMessageName( nMsg )
RETURN W_DecodeMessageName( nMsg )

METHOD GetToolTip()
RETURN ::oToolTip

METHOD GetValue( nMaxCount AS NUMERIC)
  LOCAL cValue := ""
  LOCAL nLen
  MessageBox(, "GetValue" )
  nLen   := SetWindowText( ::nHandle, @cValue, nMaxCount )
RETURN cValue

METHOD W_IsChild( nParent )
RETURN IsChild( nParent, ::nHandle )

METHOD W_IsIconic()
RETURN IsIconic( ::nHandle )

METHOD W_IsWindow()
RETURN IsWindow( ::nHandle )

METHOD Move( nX, nY, nWidth, nHeight, lRepaint )
RETURN MoveWindow( ::nHandle, nX, nY, nWidth, nHeight, lRepaint )

METHOD nChild( nMenu )
   LOCAL nOldMenu := ::nMenu
   IF nMenu <> NIL
      ::nMenu := nMenu
   ENDIF
RETURN nOldMenu

METHOD SetExtraStyle( nExStyle )
   ::nExStyle := nExStyle
RETURN SetWindowLongPtr( ::nHandle, GWL_EXSTYLE, nExStyle )

METHOD W_SetFocus()
   LOCAL nPrevHandleFocused
   nPrevHandleFocused := SetFocus( ::nHandle )
RETURN nPrevHandleFocused

METHOD SetFont( oFont AS OBJECT, lRedraw AS LOGICAL )
  DEFAULT lRedraw TO TRUE
  ::oFont := oFont
RETURN SendMessage( ::nHandle, WM_SETFONT, ::oFont:nHandle, IIF( lRedraw, 1, 0 ) )

METHOD SetFontByAttribute( cFontName AS STRING, nFontSize AS NUMERIC, ;
                          lBold, lItalic, lUnderline, lStrikeOut, lRedraw )
  DEFAULT lRedraw TO TRUE
  ::oFont := TFont():New(cFontName, nFontSize, lItalic, lUnderline, lStrikeOut, lBold)
RETURN SendMessage( ::nHandle, WM_SETFONT, ::oFont:nHandle, IIF( lRedraw, 1, 0 ) )

METHOD ReParent( oP AS OBJECT )
  LOCAL oOldParent
  IF oP <> NIL
     oOldParent := ::oParent
     ::oParent := oP
     SetParent( ::nHandle, ::oParent:nHandle )
  ENDIF
RETURN oOldParent

METHOD SetEventHandler( bWindowProc AS CODEBLOCK )
  LOCAL bOldWndProc := ::bWindowProc
  ::bWindowProc := bWindowProc
RETURN bOldWndProc

METHOD SetMenu( oMenu )
   ::oMenu := oMenu
   ::oMenu:SetParent( Self )  // Define parent window of menu
RETURN SetMenu( ::nHandle, oMenu:nHandle )

METHOD SetParentByHandle( nParent AS NUMERIC )
   ::oParent := ApplObj():FindWindowByHandle( nParent )
RETURN Self

METHOD SetStyle( nStyle )
   ::nStyle := nStyle
RETURN SetWindowLongPtr( ::nHandle, GWL_STYLE, nStyle )

METHOD SetToolTip( cToolTip )
   LOCAL cOldToolTip
   IF ::oToolTip <> NIL
      cOldToolTip := ::oToolTip:GetLabel()
      ::oToolTip:SetLabel( cToolTip )
   ELSE
      ::oToolTip := TToolTip():New( Self, cToolTip )
   ENDIF
RETURN cOldToolTip

METHOD SetValue( cTitle AS STRING)
  ::cName := cTitle
RETURN SetWindowText( ::nHandle, cTitle )

METHOD SetWindowPos( nhWndInsertAfter, nX, nY, nWidth, nHeight, nFlags )
   LOCAL lOk := SetWindowPos( ::nHandle, nhWndInsertAfter, nX, nY, nWidth, nHeight, nFlags )
   UPDATE ::nRow    TO nX      NOT NIL
   UPDATE ::nCol    TO nY      NOT NIL
   UPDATE ::nWidth  TO nWidth  NOT NIL
   UPDATE ::nHeight TO nHeight NOT NIL
RETURN lOk

METHOD Show()
   ShowWindow( ::nHandle, SW_SHOW )
   BringWindowToTop(::nHandle)
   UpdateWindow( ::nHandle )
RETURN Self

METHOD WindowProc( nMessage, wParam, lParam )
   LOCAL nRet := -1
   IF ValType( ::bWindowProc ) == "B"
      nRet := Eval( ::bWindowProc, ::nHandle, nMessage, wParam, lParam )
   ENDIF 
RETURN nRet

#include "error.ch"

METHOD ErrorHandler()
   LOCAL oErr
   // Qui la gestione errori
   MessageBox( , "Errore: parametri"+str(PCOUNT())  )
   oErr := ErrorNew()
      oErr:severity    := ES_ERROR
      oErr:genCode     := EG_OPEN
      oErr:subSystem   := "BASE"
      oErr:SubCode     := 2009
      oErr:Description := "Errore generico"
      Eval( ErrorBlock(), oErr )

RETURN TRUE

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
    LOCAL aoWindows := ApplObj():aoWindows // Array of windows and controls
    LOCAL oWin := ApplObj():FindWindowByHandle( hWnd )
    LOCAL oWinF

    //ApplObj():EventsWrite( hWnd, nMsg, wParam, lParam )
    
    IF oWin <> NIL
       nRet := oWin:WindowProc( nMsg, wParam, lParam )
       IF ValType( nRet ) <> "N" THEN nRet := -1 // To handle a NIL or Self return
    ENDIF
    
    IF nRet == -1
       do case
       
         // case nMsg == WM_MENUCOMMAND (WINVER >= 5.00)
         // case nMsg == WM_COMMAND
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
       
            
       
         case nMsg == WM_ACTIVATE
            wmId    = LOWORD(wParam) // Remember, these are...
            wmEvent = HIWORD(wParam) // ...different for Win32!
       
            do case
               case wmId == WA_ACTIVE
               case wmId == WA_CLICKACTIVE
               case wmId == WA_INACTIVE
            endcase
       
            i := aScan( aoWindows, hWnd )
       
            IF i > 0
               oWinF := aoWindows[i]
               IF ValType( oWinF:bActivate ) == "B"
                  Eval( oWinF:bActivate )
               ENDIF
            ENDIF
       
       
         *case nMsg == WM_ACTIVATEAPP
         *case nMsg == WM_CANCELMODE
         *case nMsg == WM_CHILDACTIVATE
        // case nMsg == WM_CLOSE
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
       
         *case nMsg == WM_COMPACTING
         *case nMsg == WM_CREATE
         case nMsg == WM_DESTROY
              PostQuitMessage( 0 )
              nRet := 0
       
         *case nMsg == WM_ENABLE
         *case nMsg == WM_ENTERSIZEMOVE
         *case nMsg == WM_EXITSIZEMOVE
         *case nMsg == WM_GETFONT
         *case nMsg == WM_GETICON
         *case nMsg == WM_GETMINMAXINFO
         *case nMsg == WM_GETTEXT
         *case nMsg == WM_GETTEXTLENGTH
         *case nMsg == WM_INPUTLANGCHANGE
         *case nMsg == WM_INPUTLANGCHANGEREQUEST
         *case nMsg == WM_MOVE
         *case nMsg == WM_MOVING
         *case nMsg == WM_NCACTIVATE
         *case nMsg == WM_NCCALCSIZE
         *case nMsg == WM_NCCREATE
         *case nMsg == WM_NCDESTROY
         *case nMsg == WM_NULL
         *case nMsg == WM_PAINT
         *case nMsg == WM_PARENTNOTIFY
         *case nMsg == WM_QUERYDRAGICON
         *case nMsg == WM_QUERYOPEN
         case nMsg == WM_QUIT
         *case nMsg == WM_SETFONT
         *case nMsg == WM_SETICON
       
         case nMsg == WM_SETFOCUS
       
       
         *case nMsg == WM_SETTEXT
         *case nMsg == WM_SHOWWINDOW
       //  case nMsg == WM_SIZE
       //
       //     // Status bar repaint check
       //     oItem := ApplObj():FindWindowbyHandle( hwnd )
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
       
         *case nMsg == WM_SIZING
         *case nMsg == WM_STYLECHANGED
         *case nMsg == WM_STYLECHANGING
         *case nMsg == WM_THEMECHANGED
         *case nMsg == WM_USERCHANGED
         *case nMsg == WM_WINDOWPOSCHANGED
         *case nMsg == WM_WINDOWPOSCHANGING
       
         case  nMsg == WM_LBUTTONDOWN
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
       
           case  nMsg == WM_CONTEXTMENU

// FSG - from Roberto Lopez MiniGUI - change for WoopGUI
//
//         I := Ascan ( _aFormhandles , hWnd )
//         if i > 0
//            if _aFormContextMenuHandle [i] != 0
//                TrackPopupMenu ( _aFormContextMenuHandle [i]  , LOWORD(lparam) , HIWORD(lparam) , hWnd )
//            Endif
//         EndIf

          case  nMsg == WM_TIMER

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

       endcase
    ENDIF

return nRet
*/

/* DA SISTEMARE
 FSG - On Event write to status bar
                 oItem := ApplObj():FindWindowbyHandle( hwnd )
                 IF oItem <> NIL
                    IF __objHasData( oItem, "CSTATUSBAR" )
                       MessageBox( , "Trovata finestra" )
                       IF ValType( oItem:cStatusBar ) == "C"
                          oParent := ApplObj():FindWindowbyHandle( oItem:nParent )
                          IF oParent <> NIL .AND. ValType( oParent:oStatusBar ) == "O"
                             oParent:SetStatusBar( oItem:cStatusBar )
                               //MessageBox( , "Passato" )
                          ENDIF
                       ENDIF
                    ENDIF
                 ENDIF

*/
