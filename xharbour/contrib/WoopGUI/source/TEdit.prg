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
CLASS TEdit FROM TControl
    // Base

    DATA   cInitValue    AS STRING
    DATA   nLimitText    AS NUMERIC
    DATA   lReadOnly     AS LOGICAL INIT FALSE

    // METODI
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR

    METHOD Init()

    METHOD AppendText( cString )           INLINE ::SetInsertionPointEnd(), ::Replace( cString )
    METHOD CanAcceptFocus()                INLINE ::IsEditable() .AND. ::Parent:CanAcceptFocus()
    METHOD CanCopy()                       VIRTUAL // FSG - to be implemented
    METHOD CanCut()                        VIRTUAL // FSG - to be implemented
    METHOD CanPaste()                      VIRTUAL // FSG - to be implemented
    METHOD CanRedo()                       INLINE ::SendMessage( EM_CANUNDO, 0, 0 ) != 0
    METHOD CanUndo()                       INLINE ::SendMessage( EM_CANUNDO, 0, 0 ) != 0
    METHOD Clear()                         INLINE ::SetText( "" )
    METHOD Copy()                          INLINE ::SendMessage( WM_COPY, 0, 0 )
    METHOD Create()                        VIRTUAL // FSG - to be implemented
    METHOD Cut()                           INLINE ::SendMessage( WM_CUT, 0, 0 )
    METHOD DiscardEdits()                  INLINE ::SetModified( FALSE )
    METHOD FormatLines( lOn )              INLINE ::SendMessage( EM_FMTLINES, IIF( lOn == NIL, 0, IIF( lOn, 1, 0 ) ), 0 )
    METHOD GetInsertionPoint()             INLINE LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
    METHOD GetLastPosition()               INLINE ::Len()
    METHOD GetLeftMargin()                 INLINE LoWord( ::SendMessage( EM_GETMARGINS, 0, 0 ) )
    METHOD GetLineLength()                 VIRTUAL // FSG - to be implemented
    METHOD GetLineText()                   VIRTUAL //INLINE SendMessage( ::nHandle, EM_GETFIRSTVISIBLELINE, 0, 0 ) + 1
    METHOD GetFirstVisibleLine()           INLINE ::SendMessage( EM_GETFIRSTVISIBLELINE, 0, 0 ) + 1
    METHOD GetNumberOfLines()              INLINE ::SendMessage( EM_GETLINECOUNT, 0, 0 )
    METHOD GetPasswordChar()               INLINE ::SendMessage( EM_GETPASSWORDCHAR, 0, 0 )
    METHOD GetRightMargin()                INLINE HiWord( ::SendMessage( EM_GETMARGINS, 0, 0 ) )
    METHOD GetSelection()                  INLINE LOCAL nPos, ;
	                                              nPos := ::SendMessage( EM_GETSEL, 0, 0 ), ;
	                                              { LoWord( nPos ) + 1, HiWord( nPos ) + 1 }
    METHOD GetText()                       INLINE GetDlgItemText( ::oParent:nHandle, ::nID, ::Len() ) //GetEditText( ::oParent:nHandle, ::nID )
    METHOD GetValue()                      INLINE ::GetText()
    METHOD IsEditable()                    INLINE !::IsReadOnly()
    METHOD IsModified()                    INLINE ::SendMessage( EM_GETMODIFY, 0, 0 ) != 0
    METHOD IsReadOnly()                    INLINE AND( ::GetStyle(), ES_READONLY ) != 0 //AND( GetWindowLongPtr( ::nHandle, GWL_STYLE ), ES_READONLY ) != 0
    METHOD IsPassword()                    INLINE AND( ::GetStyle(), ES_PASSWORD ) != 0 // AND( GetWindowLongPtr( ::nHandle, GWL_STYLE ), ES_PASSWORD ) != 0
    METHOD Len()			                INLINE ::SendMessage( WM_GETTEXTLENGTH, 0, 0 ) + 1
    METHOD LoadFile()                      VIRTUAL // FSG - to be implemented
    METHOD OnChar()                        VIRTUAL // FSG - to be implemented
    METHOD OnDropFiles()                   VIRTUAL // FSG - to be implemented
    METHOD Paste()                         INLINE ::SendMessage( WM_PASTE, 0, 0 )
    METHOD PositionToXY()                  VIRTUAL // FSG - to be implemented
    METHOD Redo()                          INLINE IIF( ::CanRedo(),;
                                                       ::SendMessage( EM_UNDO, 0, 0 ),; // Same as Undo, since Undo undoes the undo, i.e. a redo.
                                                       NIL )
    METHOD Remove( lUndo )                 INLINE ::SendMessage( EM_REPLACESEL, IIF( lUndo == NIL, 1, IIF( lUndo, 1, 0 ) ), GetStringPtr( "" ) )
    METHOD Replace( cString, lUndo )       INLINE ::SendMessage( EM_REPLACESEL, IIF( lUndo == NIL, 1, IIF( lUndo, 1, 0 ) ), GetStringPtr( cString ) )
    METHOD SaveFile()                      VIRTUAL // FSG - to be implemented
    METHOD SelectAll()                     INLINE ::SetSelection( 1, 0 ) //SendMessage( EM_SETSEL, 0, -1 ),;
    METHOD SetEditable( lReadOnly )        INLINE ::SendMessage( EM_SETREADONLY, lReadOnly, 0 )
    METHOD SetInsertionPoint( nPos )       INLINE ::SetSelection( nPos, nPos )
    METHOD SetInsertionPointEnd()          INLINE ::SetInsertionPoint( ::Len() )
    METHOD SetLeftMargin( nPixelWidth )    INLINE ::SendMessage( EM_SETMARGINS, EC_LEFTMARGIN, MAKELPARAM( nPixelWidth, 0 ) )
    METHOD SetLimitText( nLimit )          INLINE ::SendMessage( EM_LIMITTEXT, nLimit, 0 )
    METHOD SetModified( lModified )        INLINE ::SendMessage( EM_SETMODIFY, IIF( lModified == NIL, 1, IIF( lModified, 1, 0 ) ), 0 )
    METHOD SetReadOnly( lReadOnly )        INLINE ::SendMessage( EM_SETREADONLY, IIF( lReadOnly == NIL, 1, IIF( lReadOnly, 1, 0 ) ), 0 )
    METHOD SetPassword( lPassword )        INLINE IIF( lPassword == NIL .OR. lPassword, ::SetPasswordChar( "*" ), ::SetPasswordChar( "" ) )
                                                  //::SetPasswordChar( "*" ), SendMessage( ::nHandle, EM_SETPASSWORDCHAR, IIF( lPassword, "*", 0 )
                                                  //IIF( lPassword, SetWindowLongPtr( ::nHandle, GWL_STYLE, ES_PASSWORD ),;
                                                  //                SendMessage( ::nHandle, EM_SETPASSWORDCHAR, 0, 0 ) )
    METHOD SetPasswordChar( cChar )        INLINE ::SendMessage( EM_SETPASSWORDCHAR, IIF( cChar == "", 0, Asc( cChar )), 0 )  // Send a ::Redraw() after set password
    METHOD SetRightMargin( nPixelWidth )   INLINE ::SendMessage( EM_SETMARGINS, EC_RIGHTMARGIN, MAKELPARAM( 0, nPixelWidth ) )
    METHOD SetSelection( nStart, nEnd )    INLINE IIF( ValType( nStart ) == "A", ( nEnd := nStart[2], nStart := nStart[1] ), NIL ),;
                                                  ::SendMessage( EM_SETSEL, nStart - 1, IIF( nEnd == NIL, -1, nEnd - 1 ) ),;
                                                  ::SendMessage( EM_SCROLLCARET, 0, 0)

    //METHOD SetText( cString )              INLINE ::SendMessage( WM_SETTEXT, 0, GetStringPtr( cString ) ), ::UpdateVar()
    METHOD SetText( cString )              INLINE SetDlgItemText( ::oParent:nHandle, ::nID, cString )
    METHOD SetValue( cString )             INLINE ::SetText( cString )
    METHOD ShowPosition()                  VIRTUAL // FSG - to be implemented
    METHOD Undo()                          INLINE IIF( ::CanUndo(),;
                                                       ::SendMessage( EM_UNDO, 0, 0 ),;
                                                       NIL )
    METHOD UnSelect()                      INLINE ::SetSelection( 0, 0 ) //::SendMessage( EM_SETSEL, -1, 0 )
    //METHOD WordWrap( lOn )                 INLINE ::FormatLines( lOn ), ::SelectAll(), ::Cut(), ::Paste()
    METHOD WriteText()                     VIRTUAL // FSG - to be implemented
    METHOD XYToPosition()                  VIRTUAL // FSG - to be implemented

    // Events
    METHOD OnCommand()
    //METHOD OnKillFocus()
    //METHOD OnSetFocus()

ENDCLASS

METHOD New( cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, cValue, ;
            nLimitText, lReadOnly, lPassword ) CLASS TEdit

    WG_DebugTrace( "TEdit:New()" )

    DEFAULT lPixel    TO FALSE
    DEFAULT lReadOnly TO FALSE
    DEFAULT lPassword TO FALSE

    ASSIGN ::nExStyle    WITH WS_EX_CLIENTEDGE
    ASSIGN ::cClassName  WITH "EDIT"
    ASSIGN ::cName       WITH cName      DEFAULT "" //"Edit_1"
    ASSIGN ::nStyle      WITH nStyle     DEFAULT WS_CHILD + WS_VISIBLE + WS_TABSTOP + WS_BORDER	+ ES_LEFT + ES_AUTOHSCROLL
                                                 //ES_WANTRETURN | WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_MULTILINE | WS_VSCROLL | WS_HSCROLL | ES_AUTOHSCROLL
    ASSIGN ::nRow        WITH nRow       DEFAULT 0
    ASSIGN ::nCol        WITH nCol       DEFAULT 0
    ASSIGN ::nWidth      WITH nWidth     DEFAULT IIF( lPixel, 100, WG_Pixel2DialogX( 100 ) )
    ASSIGN ::nHeight     WITH nHeight    DEFAULT IIF( lPixel,  20, WG_Pixel2DialogY(  20 ) )

    ASSIGN ::cInitValue  WITH cValue     DEFAULT ""
    ASSIGN ::nLimitText  WITH nLimitText DEFAULT -1

    IF lReadOnly THEN ::nStyle += ES_READONLY

    // Creo l'istanza tramite la classe window
    ::Super:New( ::cClassName, ::cName, ::nStyle, ;
                                 ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                                 oParent, bAction, cToolTip, cStatusBar, lPixel,, nID )

RETURN Self

METHOD NewExtended( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip,;
                    cStatusBar, lPixel, nID, cValue, nLimit, lReadOnly, lPassword, ;
                    bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TEdit

    WG_DebugTrace( "TEdit:NewExtended()" )

    ::New( cName,, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, cValue, nLimit, lReadOnly, lPassword )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self

METHOD Init() CLASS TEdit

    WG_DebugTrace( "TEdit:Init()" )

    // Set default colors
    IF ::oFgColor == NIL THEN ::SetForeGroundColor( TSystemSetting():GetColor(COLOR_WINDOWTEXT) )
    IF ::oBgColor == NIL THEN ::SetBackGroundColor( TSystemSetting():GetColor(COLOR_BTNHIGHLIGHT) )
    ::Super:Init()
    IF ::nLimitText <> NIL THEN ::SetLimitText( ::nLimitText )
    IF ValType( ::bVarBlock ) == "B"
       Eval( ::bVarBlock, ::GetValue() )
    ENDIF
    //SendMessage ( ::nHandle, EM_LIMITTEXT, 10, 0 )
    //IF ::cInitValue <> NIL THEN ::SetValue( ::cInitValue )
    //::DisplayData()
RETURN Self


METHOD OnCommand( wParam, lParam ) CLASS TEdit
   LOCAL nRet   := -1
   LOCAL nEvent := HiWord( wParam )
   LOCAL nID    := LoWord( wParam )

   //MessageBox(,"Passato da BN_ command")
   DO CASE

      CASE nEvent == EN_SETFOCUS
           //MessageBox(,"Passato da EN_SETFOCUS")
           WG_ApplObj():EventsWrite( "EDT", ::nHandle, "EN_SETFOCUS", wParam, lParam )
           nRet = ::Super:OnSetFocus()

      CASE nEvent == EN_KILLFOCUS
           //MessageBox(,"Passato da BN_KILLFOCUS")
           WG_ApplObj():EventsWrite( "EDT", ::nHandle, "EN_KILLFOCUS", wParam, lParam )
           nRet = ::Super:OnKillFocus()

      CASE nEvent == EN_CHANGE
           WG_ApplObj():EventsWrite( "EDT", ::nHandle, "EN_CHANGE", wParam, lParam )
           ::UpdateVar()

      CASE nEvent == EN_UPDATE
           WG_ApplObj():EventsWrite( "EDT", ::nHandle, "EN_UPDATE", wParam, lParam )

      CASE nEvent == EN_MAXTEXT
           WG_ApplObj():EventsWrite( "EDT", ::nHandle, "EN_MAXTEXT", wParam, lParam )

      CASE nEvent == EN_HSCROLL
           WG_ApplObj():EventsWrite( "EDT", ::nHandle, "EN_HSCROLL", wParam, lParam )

      CASE nEvent == EN_VSCROLL
           WG_ApplObj():EventsWrite( "EDT", ::nHandle, "EN_VSCROLL", wParam, lParam )

      //OTHERWISE
      //     IF ::HasAction()
      //        ::ExecAction( Self )
      //        nRet := 0  // Something exec, stop handling
      //     ENDIF
   ENDCASE

RETURN nRet

//METHOD OnSetFocus() CLASS TEdit
//  //::PushColors( "W+/B" )
//  ::Super:OnSetFocus()
//RETURN Self
//
//METHOD OnKillFocus() CLASS TEdit
//  //::PopColors()
//  ::Super:OnKillFocus()
//RETURN Self


// ----------------------------------------------------
//
// W_DefEdtEvents( hWnd, nMsg, wParam, lParam ) --> Bool
//
// interface between Windows API event handling and Class handling
// ----------------------------------------------------
FUNCTION WG_DefEdtEvents( hWnd, nMsg, wParam, lParam )
  LOCAL oWin, oWinClass
  LOCAL nRet := -1

  // Send messages to function that write events.log for debug - See TApplication
  WG_ApplObj():EventsWrite( "EDTS", hWnd, nMsg, wParam, lParam )

  // Search window
  oWin      := TWindow():FindWindowByHandle( hwnd )
  IF oWin <> NIL
     // Call his window procedure
     nRet := oWin:WindowProc( nMsg, wParam, lParam )
  ENDIF

  IF nRet == NIL
     nRet := -1
  ENDIF

  // I return to c call function ( Call DefWindowProc() )
RETURN nRet    