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
* From this class descends controls
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "winuser.ch"
#include "wingdi.ch"

CLASS TControl FROM TWindow

    DATA nHelpID     AS NUMERIC    INIT 0

    DATA cStatusBar  AS STRING        // Add status bar string
    DATA bVarBlock   AS CODEBLOCK     // User variable code block - set/get
    DATA xValue                       // Value of control
    DATA cToolTip    AS STRING     INIT ""
    DATA lDefault    AS LOGICAL    INIT FALSE

    // ACCESS nID       INLINE ::nChild
    // ASSIGN nID(n)    INLINE ::nChild := n

    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR
    METHOD Extend()
    METHOD Init()

    //METHOD Reader()            INLINE

    METHOD GetLabel()          INLINE ::Super:GetValue()
    METHOD GetValue()          INLINE ::xValue
    METHOD SetLabel( cText )   INLINE ::Super:SetValue( cText )
    METHOD SetValue()
    METHOD GetValueAsString()  INLINE cStr( ::GetValue() )
    METHOD UpdateVar( xVal )   INLINE LOCAL xRet, IIF( ValType( ::bVarBlock ) == "B", ;
                                                  xRet := Eval( ::bVarBlock, IIF( xVal <> NIL, xVal, ::GetValue() ) ) ,;
                                                  NIL ), xRet
    //METHOD WindowProc()
    METHOD OnContextMenu()
    METHOD OnCtlColor()
    METHOD OnKillFocus()
    METHOD OnSetFocus()
    //METHOD OnDrawItem()

ENDCLASS

METHOD New( cClassName, cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, ;
            cStatusBar, lPixel, lDefault, nID ) CLASS TControl

    //WG_ParamDisplay( Self, hb_aparams(), "TControl_New" )
    WG_DebugTrace( "TControl:New()" )

    ASSIGN ::oParent  WITH oParent  DEFAULT TWindow():GetCurrentWindow()
    ASSIGN ::cToolTip WITH cToolTip DEFAULT ""
    ASSIGN ::lDefault WITH lDefault DEFAULT FALSE
    ASSIGN ::lPixel   WITH lPixel   DEFAULT FALSE

    // Make instance through TControl class
    ::Super:New( 0, cClassName, cName, nStyle, ;
                 nRow, nCol, nWidth, nHeight, ;
                 ::oParent )

    UPDATE ::nID TO nID NOT NIL

    ASSIGN ::cStatusBar WITH cStatusBar

    // Add control to parent window
    ::oParent:AddChild( Self )
    // Add control to windows because is itself a window
    ::AddWindow( Self )

    ::bAction := bAction

    IF !::oParent:IsDerivedFrom( "TDIALOG" )
       ::Init()
    ENDIF

RETURN Self

METHOD Init() CLASS TControl
   WG_DebugTrace( "TControl:Init()" )
   //::Super:Init()
   ::SetToolTip( ::cToolTip )
   // Set font
   IF ::oFont != Nil
      ::SetFont( ::oFont )
   ELSEIF ::oParent:oFont != Nil
      ::SetFont( ::oParent:oFont )
   ELSE
      ::SetFont( TFont():New( "MS Sans Serif", 8 ) )
   ENDIF
   // Set default control color
   //IF ::oFgColor == NIL THEN ::SetForeGroundColor( TSystemSetting():GetColor(COLOR_WINDOWTEXT) )
   //IF ::oBgColor == NIL THEN ::SetBackGroundColor( TSystemSetting():GetColor(COLOR_BTNFACE) )

RETURN Self

METHOD NewExtended( cClassName, cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel,;
                    lDefault, nID, bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TControl

    WG_DebugTrace( "TControl:NewExtended()" )

    ::New( cClassName, cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, lDefault, nID )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self

METHOD Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TControl

    WG_DebugTrace( "TControl:Extend()" )

    IF ValType( bVarBlock ) == "B"
       ::bVarBlock := bVarBlock
    ENDIF

    IF ValType( oFont ) == "O"
       ::SetFont( oFont )
    ELSE
       IF ValType( cFontName ) == "C" .OR. ;
          ValType( nFontSize ) == "N"
          ::SetFontByAttribute( cFontName, nFontSize )
       ENDIF
    ENDIF

    // Set Colors
    IF ncFgColor != NIL
       ::SetForegroundColor( ncFGColor )
    ENDIF
    IF ncBgColor != NIL
       ::SetBackgroundColor( ncBGColor )
    ENDIF

RETURN Self

METHOD SetValue( xValue ) CLASS TControl
   WG_DebugTrace( "TControl:SetValue()" )
   ::xValue := xValue
   ::UpdateVar( xValue )
RETURN Self

METHOD OnContextMenu( x, y ) CLASS TControl
   LOCAL nRet   := -1
   WG_DebugTrace( "TControl:OnContextMenu()" )

   IF ::oContextMenu <> NIL
      nRet := WG_OnContextMenu( ::nHandle, x, y, ::oContextMenu:nHandle )
   ENDIF

RETURN nRet

METHOD OnCtlColor( hDC ) CLASS TControl
   LOCAL nRet   := -1
   WG_DebugTrace( "TControl:OnCtlColor()", "Self", Self, "::oFgColor", ::oFgColor, "::oBgColor", ::oBgColor  )

   IF ::oFgColor != Nil
      SetTextColor( hDC, ::oFgColor:GetColor() )
   ENDIF

   IF ::oBgColor != Nil
      //SetBkMode(hDC,1)
      //nRet := GetStockObject(NULL_BRUSH)
      SetBkColor( hDC, ::oBgColor:GetColor() )
      nRet := ::oBrush:nHandle
   ELSE
       WG_DebugTrace( "TControl:OnCtlColor()", "Self", Self, "::cClassName", ::cClassName )
      DO CASE
         CASE ::cClassName == "STATIC"
              SetBkMode(hDC,1)
              ::SetBackGroundColor( TSystemSetting():GetColor(COLOR_BTNFACE) )
              SetBkColor( hDC, ::oBgColor:GetColor() )
              nRet := ::oBrush:nHandle
         CASE ::cClassName == "BUTTON"
              //SetBkMode(hDC,0)
              //::SetBackGroundColor( TSystemSetting():GetColor(COLOR_BTNFACE) )
              //SetBkColor( hDC, ::oBgColor:GetColor() )
              //nRet := ::oBrush:nHandle
              SetBkColor( hDC, GetSysColor( COLOR_BTNFACE ) )
              nRet := GetSysColorBrush( COLOR_BTNFACE )
         OTHERWISE
              SetBkMode(hDC,1)
              nRet := GetStockObject(NULL_BRUSH)
      ENDCASE
   ENDIF

RETURN nRet

METHOD OnSetFocus() CLASS TControl
  WG_DebugTrace( "TControl:OnSetFocus()" )
  IF ::cStatusBar <> NIL THEN ::oParent:SetStatusBar( ::cStatusBar, 1 )
RETURN Self

METHOD OnKillFocus() CLASS TControl
  WG_DebugTrace( "TControl:OnKillFocus()" )
  IF ::cStatusBar <> NIL THEN ::oParent:SetStatusBar( "", 1 )
RETURN Self

//METHOD OnDrawItem( lpDrawItem ) CLASS TControl
//   LOCAL nRet   := -1
//   MessageBox( , "Passato" )
//   WG_DrawItem( lpDrawItem, ::nHandle, ::oBgColor:GetColor(), ::oFgColor:GetColor() )
//RETURN nRet


//METHOD WindowProc( nMsg, wParam, lParam ) CLASS TControl
//   LOCAL nRet := -1
//   LOCAL wmId, wmEvent, wmHandle
//   LOCAL oWin
//
//   // Check if there is a user event handler
//   IF ::HasEventHandler() // ValType( ::bWindowProc ) == "B"
//      // Evaluate User event handler
//      nRet := Eval( ::bWindowProc, ::nHandle, nMsg, wParam, lParam )
//   ENDIF
//   IF nRet == -1
//    // Class event handler
//     // DO CASE
//     //    CASE nMsg == WM_COMMAND
//     //         wmId    = LOWORD(wParam) // menu identifier
//     //         wmEvent = HIWORD(wParam) // 0 = menu, 1 = accelerator
//     //         wmHandle = lParam        // control handle
//     //
//     //         IF wmEvent == 0    // Command from menu or window
//     //            IF IsWindow( wmHandle ) // Is from a window
//     //               //WG_ApplObj():EventsWrite( wmHandle, nMsg, wParam, lParam )
//     //               // Find window
//     //               oWin := WG_ApplObj():FindWindowByHandle( wmHandle )
//     //               // if found it and it has an action we exec
//     //               IF oWin <> NIL .AND. oWin:HasAction()
//     //                  oWin:ExecAction( oWin )
//     //                  nRet := 0  // Something exec, stop handling
//     //               ENDIF
//     //            ENDIF
//     //        ENDIF
//     // ENDCASE
//   ENDIF
//   IF nRet == -1
//      // Standard Event Handler
//      nRet := ::Super:WindowProc( nMsg, wParam, lParam )
//   ENDIF
//RETURN nRet

