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
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"

// Windows definitions
CLASS WG_TButton FROM WG_TControl
    // Base

    // METODI
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR
    METHOD GetValue()          INLINE ::xValue := Sendmessage( ::nHandle, BM_GETCHECK, 0, 0 )
    METHOD GetLabel()          INLINE ::Super:GetValue()
    METHOD SetValue( nState )  INLINE Sendmessage( ::nHandle, BM_SETCHECK, nState, 0 ), ::Super:SetValue( nState )
    METHOD SetLabel( cText )   INLINE ::Super:SetValue( cText )
    //METHOD  WindowProc()

    // Events
    METHOD OnCommand()

ENDCLASS

METHOD New( cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, ;
            lDefault, nID ) CLASS WG_TButton

    WG_DebugTrace( "TButton:New()" )

    DEFAULT lPixel TO TRUE

    ASSIGN ::cClassName WITH "BUTTON"
    ASSIGN ::cName      WITH cName    DEFAULT "Button_1"
    ASSIGN ::nStyle     WITH nStyle   DEFAULT WS_VISIBLE + WS_CHILD + WS_TABSTOP + BS_PUSHBUTTON + BS_NOTIFY
    ASSIGN ::nRow       WITH nRow     DEFAULT 0
    ASSIGN ::nCol       WITH nCol     DEFAULT 0
    ASSIGN ::nWidth     WITH nWidth   DEFAULT IIF( lPixel, 80, WG_Pixel2DialogX( 80 ) )
    ASSIGN ::nHeight    WITH nHeight  DEFAULT IIF( lPixel, 24, WG_Pixel2DialogY( 24 ) )


    // Creo l'istanza tramite la classe window
    ::Super:New( ::cClassName, ::cName, ::nStyle, ;
                                 ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                                 oParent, bAction, cToolTip, cStatusBar, lPixel, lDefault, nID )

RETURN Self

METHOD NewExtended( cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip,;
                    cStatusBar, lPixel, lDefault, nID, bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS WG_TButton

    WG_DebugTrace( "TButton:NewExtended()" )

    ::New( cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, lDefault, nID )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self

METHOD OnCommand( wParam, lParam ) CLASS WG_TButton
   LOCAL nRet   := -1
   LOCAL nEvent := HiWord( wParam )
   LOCAL nID    := LoWord( wParam )


   //MessageBox(,"Passato da BN_ command")
   DO CASE
      CASE nEvent == BN_CLICKED
           WG_ApplObj():EventsWrite( "BTN", ::nHandle, "BN_CLICKED", wParam, lParam )
           IF ::HasAction()
              ::ExecAction()
              nRet := 0  // Something exec, stop handling
           ENDIF

      CASE nEvent == BN_PAINT
           WG_ApplObj():EventsWrite( "BTN", ::nHandle, "BN_PAINT", wParam, lParam )

      CASE nEvent == BN_HILITE
           WG_ApplObj():EventsWrite( "BTN", ::nHandle, "BN_HILITE", wParam, lParam )

      CASE nEvent == BN_UNHILITE
           WG_ApplObj():EventsWrite( "BTN", ::nHandle, "BN_UNHILITE", wParam, lParam )

      CASE nEvent == BN_DISABLE
           WG_ApplObj():EventsWrite( "BTN", ::nHandle, "BN_DISABLE", wParam, lParam )

      CASE nEvent == BN_DOUBLECLICKED
           WG_ApplObj():EventsWrite( "BTN", ::nHandle, "BN_DOUBLECLICKED", wParam, lParam )

      CASE nEvent == BN_SETFOCUS
           WG_ApplObj():EventsWrite( "BTN", ::nHandle, "BN_SETFOCUS", wParam, lParam )
           //MessageBox(,"Passato da BN_SETFOCUS")
           nRet = ::OnSetFocus()

      CASE nEvent == BN_KILLFOCUS
           WG_ApplObj():EventsWrite( "BTN", ::nHandle, "BN_KILLFOCUS", wParam, lParam )
           //MessageBox(,"Passato da BN_KILLFOCUS")
           nRet = ::OnKillFocus()

   ENDCASE

RETURN nRet

/*
METHOD WindowProc( nMsg, wParam, lParam ) CLASS WG_TButton
   LOCAL nRet := -1
   LOCAL wmId, wmEvent, wmHandle
   LOCAL oWin

   // Check if there is a user event handler
   IF ::HasEventHandler() // ValType( ::bWindowProc ) == "B"
      // Evaluate User event handler
      nRet := Eval( ::bWindowProc, ::nHandle, nMsg, wParam, lParam )
   ENDIF
   IF nRet == -1
      // Class event handler
      //DO CASE
      //   CASE nMsg == WM_COMMAND
      //        wmId     = LOWORD(wParam) // button's control identifier
      //        wmEvent  = HIWORD(wParam) // the notification message = BN_CLICKED
      //        wmHandle = lParam         // control handle
      //
      //        IF wmEvent == BN_CLICKED
      //           IF IsWindow( wmHandle ) // Is from a window
      //              // Find window
      //              oWin := WG_ApplObj():FindWindowByHandle( wmHandle )
      //              IF oWin <> NIL
      //                 // Exec control procedure
      //                 IF ValType( oWin:bVarBlock ) == "B"
      //                    Eval( oWin:bVarBlock, oWin:GetValue() )
      //                    //nRet := 0
      //                 ENDIF
      //              ENDIF
      //           ENDIF
      //       ENDIF
      //ENDCASE
   ENDIF
   IF nRet == -1
      // Standard Event Handler
      nRet := ::Super:WindowProc( nMsg, wParam, lParam )
   ENDIF
RETURN nRet
*/
