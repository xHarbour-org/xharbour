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
* From this class descends application window
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "winuser.ch"
#include "commctrl.ch"

CLASS TDialog FROM TPanel

    DATA nMenu       AS NUMERIC INIT 0

    DATA lStatusBar  AS LOGICAL INIT FALSE

    DATA oStatusBar      AS OBJECT HIDDEN

    // Public Methods
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR

    METHOD CreateStatusBar()
    METHOD GetStatusBar()
    METHOD PopStatusBar()
    METHOD PushStatusBar()
    METHOD SetStatusBar()
    METHOD WindowProc()

    // Events
    METHOD OnCreate()
    METHOD OnInitDialog()
    METHOD OnSysCommand()
    METHOD OnNotify()

ENDCLASS

METHOD New( cName, nTop, nLeft, nWidth, nHeight, oParent, lStatusBar, lPixel, lModal ) CLASS TDialog

    ASSIGN ::nExStyle     WITH WS_EX_DLGMODALFRAME //+ WS_EX_CONTROLPARENT
    ASSIGN ::cName        WITH cName                DEFAULT "Dialog_1"
    ASSIGN ::nStyle       WITH IIF( ::lModal, ;
                                    WS_POPUP + WS_SYSMENU + WS_CAPTION + DS_MODALFRAME + DS_3DLOOK,;
                                    WS_TABSTOP + WS_POPUP + WS_DLGFRAME + WS_BORDER + DS_3DLOOK + WS_SYSMENU + WS_MINIMIZEBOX )
    ASSIGN ::nMenu        WITH 0
    ASSIGN ::lStatusBar   WITH lStatusBar           DEFAULT FALSE

    ::Super:New( ::nExStyle, ::cName, ::nStyle, nTop, nLeft, ;
                 nWidth, nHeight, oParent, lPixel, lModal )

RETURN Self

METHOD NewExtended( cTitle, nTop, nLeft, nWidth, nHeight,;
                    oMenu, oBrush, oIcon, oParent AS OBJECT, lStatusBar, lPixel, lModal, ;
                    lvScroll, lhScroll, nClrFore, nClrBack, oCursor,;
                    cBorder, lNoSysMenu, lNoCaption,;
                    lNoIconize, lNoMaximize, oFont, cFontName, nFontSize ) CLASS TDialog


   ::New( cTitle, nTop, nLeft, nWidth, nHeight, oParent, lStatusBar, lPixel, lModal )

   IF ValType( oMenu ) == "O" THEN ::SetMenu( oMenu )
   // IF ValType( oBrush ) == "O" THEN ::SetBrush( oBrush )
   // IF ValType( oIcon ) == "O" THEN ::SetIcon( oIcon )

RETURN Self


METHOD CreateStatusBar( nParts, nStyle ) CLASS TDialog
  DEFAULT nStyle TO WS_CHILD + WS_VISIBLE + SBARS_TOOLTIPS
  ::oStatusBar := tStatusBar():New( Self, nParts, nStyle )
  ::oStatusBar:Create()
RETURN IIF( ::oStatusBar <> NIL, TRUE, FALSE )

METHOD GetStatusBar() CLASS TDialog
RETURN ::oStatusBar

METHOD PopStatusBar() CLASS TDialog
   IF ValType( ::oStatusBar ) == "O"
      ::oStatusBar:PopValue()
   ENDIF
RETURN Self

METHOD PushStatusBar( cString, nPart, uFlags ) CLASS TDialog
   IF ValType( ::oStatusBar ) == "O"
      ::oStatusBar:PushValue( cString, nPart, uFlags )
   ENDIF
RETURN Self

METHOD SetStatusBar( cString, nPart, uFlags ) CLASS TDialog
   IF ValType( ::oStatusBar ) == "O"
      ::oStatusBar:SetValue( cString, nPart, uFlags )
   ENDIF
RETURN Self

//------------------------------------------------------------------------------
// Events

METHOD OnCreate()
  LOCAL nRet := ::Super:OnCreate()
  ::SetMenu( ::oMenu )
RETURN nRet

METHOD OnInitDialog( wParam, lParam ) CLASS TDialog
   LOCAL nRet        := -1
   LOCAL lSetDefault := FALSE

   WG_DebugTrace( "TDialog:OnInitDialog()", "wParam", wParam, "lParam", lParam )
   //MessageBox(, "TDIALOG Window Proc INIT DIALOG" )
   IF ::lInitialize
      aEval( ::GetChildren(), ;
             {|oItem| ;
              WG_DebugTrace( "TDialog:OnInitDialog() - Initialize Children", "oItem:ClassName", oItem:ClassName, "oItem:nID", oItem:nID ),;
              oItem:nHandle := GetDlgItem( ::nHandle, oItem:nID ), ; // Set nHandle
              oItem:Init(), ; // Init controls
              IIF( oItem:lDefault .AND. !lSetDefault, ( oItem:SetFocus(), lSetDefault := TRUE ), NIL ) ;
             } )
      IF lSetDefault
         nRet := -1
      ELSE
         nRet := 0  // No default set, make defwndproc do this
      ENDIF
      // MessageBox(,"Impostati Handle ai controlli" )

      // Active read getlist


      IF ::lStatusBar
         // Create SIMPLE status bar
         // FSG - Define application variable to make a standard statusbar
         ::CreateStatusBar()
      ENDIF

      ::lInitialize := FALSE
   ENDIF

RETURN nRet

METHOD OnSysCommand( wParam, nXPos, nYPos ) CLASS TDialog
   LOCAL nRet := -1

   If wParam == SC_CLOSE
      //DestroyWindow(::nHandle)
      ::EndDialog()
   EndIf

RETURN nRet

METHOD OnNotify( wParam, lParam ) CLASS TDialog
   LOCAL nRet := -1
   // Search the control which notify the events
   LOCAL nID    := LoWord( wParam )
   LOCAL oChild := ::FindChildByID( nID )

   IF oChild <> NIL
      nRet := oChild:OnNotify( wParam, lParam )
   ENDIF

RETURN nRet

//------------------------------------------------------------------------------

METHOD WindowProc( nMsg, wParam, lParam ) CLASS TDialog
   LOCAL nRet := -1  // = TRUE
   LOCAL wmId, wmEvent, wmHandle
   LOCAL oWin

   // Check if there is a user event handler
   IF ::HasEventHandler() // ValType( ::bWindowProc ) == "B"
      // Evaluate User event handler
      nRet := Eval( ::bWindowProc, ::nHandle, nMsg, wParam, lParam )
      //MessageBox(, "TDIALOG Window Proc Event handler return " + cStr( nRet ) )
   ENDIF
   IF nRet == -1

      // A dialog can have a status bar, so we must check its event handler
      IF nRet == -1 .AND. ValType( ::oStatusBar ) == "O" THEN nRet := ::oStatusBar:WindowProc( nMsg, wParam, lParam )

      DO CASE
         CASE nMsg == WM_GETDLGCODE
              WG_DebugTrace( "TDialog:WindowProc() - WM_GETDLGCODE" )
              nRet := DLGC_HASSETSEL + DLGC_WANTALLKEYS + DLGC_WANTARROWS + DLGC_WANTCHARS
      ENDCASE
   ENDIF
   IF nRet == -1
      // Standard Event Handler
      nRet := ::Super:WindowProc( nMsg, wParam, lParam )
   ENDIF

RETURN nRet


