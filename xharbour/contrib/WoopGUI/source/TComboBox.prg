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
#include "winuser.ch"

// Windows definitions
CLASS TComboBox FROM TControl
    // Base

    DATA    nInitValue    AS NUMERIC

    // METODI
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR

    METHOD AddRows()
    METHOD AddString( cString )            INLINE SendMessage( ::nHandle, CB_ADDSTRING, 0, GetStringPtr( cString ) )
    METHOD DeleteString( nPos )            INLINE SendMessage( ::nHandle, CB_DELETESTRING, nPos, 0 )
    METHOD FindString( cString, nStart )   INLINE SendMessage( ::nHandle, CB_FINDSTRING, nStart, GetStringPtr( cString ) )
    METHOD GetComboBoxInfo()               VIRTUAL // FSG - to be implemented
    METHOD GetCount()                      INLINE SendMessage( ::nHandle, CB_GETCOUNT, 0, 0 )
    METHOD GetCurSel()                     INLINE SendMessage( ::nHandle, CB_GETCURSEL, 0, 0 ) + 1
    METHOD GetDroppedControlRect()         VIRTUAL // FSG - to be implemented
    METHOD GetDroppedState()               INLINE SendMessage( ::nHandle, CB_GETDROPPEDSTATE, 0, 0 )
    METHOD GetDroppedWidth()               INLINE SendMessage( ::nHandle, CB_GETDROPPEDWIDTH, 0, 0 )
    METHOD GetEditSel( nStart, nEnd )      INLINE SendMessage( ::nHandle, CB_GETEDITSEL, nStart, nEnd )
    METHOD GetExtendedUI()                 INLINE SendMessage( ::nHandle, CB_GETEXTENDEDUI, 0, 0 )
    METHOD GetHorizontalExtent()           INLINE SendMessage( ::nHandle, CB_GETHORIZONTALEXTENT, 0, 0 )
    METHOD GetItemData( nPos )             INLINE SendMessage( ::nHandle, CB_GETITEMDATA, nPos, 0 )
    METHOD GetItemHeight( nPos )           INLINE SendMessage( ::nHandle, CB_GETITEMHEIGHT, nPos, 0 )
    METHOD GetLbText( nPos )               VIRTUAL // FSG - to be implemented
    METHOD GetTopIndex()                   INLINE SendMessage( ::nHandle, CB_GETTOPINDEX, 0, 0 ) + 1
    METHOD GetValue()                      INLINE ::GetCurSel()
    METHOD Init()
    METHOD InsertString( nPos, cString )   INLINE SendMessage( ::nHandle, CB_INSERTSTRING, nPos, GetStringPtr( cString ) )
    METHOD LimitText( nMaxChars )          INLINE SendMessage( ::nHandle, CB_LIMITTEXT, nMaxChars, 0 )
    METHOD ResetContent()                  INLINE SendMessage( ::nHandle, CB_GETEDITSEL, 0, 0 )
    METHOD SelectString( nStart, cString ) INLINE SendMessage( ::nHandle, CB_SELECTSTRING, nStart, GetStringPtr( cString ) )
    METHOD SetCurSel( nItem )              INLINE SendMessage( ::nHandle, CB_SETCURSEL, nItem-1, 0 )
    METHOD SetDroppedWidth( nWidth )       INLINE SendMessage( ::nHandle, CB_SETDROPPEDWIDTH, nWidth, 0 )
    METHOD SetEditSel( nStart, nEnd )      INLINE SendMessage( ::nHandle, CB_SETEDITSEL, 0, MAKELPARAM( nStart-1, nEnd-1 ) )
    METHOD SetExtendedUI( lExt )           INLINE SendMessage( ::nHandle, CB_SETEXTENDEDUI, lExt, 0 )
    METHOD SetHorizontalExtent( nWidth )   INLINE SendMessage( ::nHandle, CB_SETHORIZONTALEXTENT, nWidth, 0 )
    METHOD SetItemData( nPos, xValue )     INLINE SendMessage( ::nHandle, CB_SETITEMDATA, nPos, xValue )
    METHOD SetItemHeight( nPos, nHeight )  INLINE SendMessage( ::nHandle, CB_SETITEMHEIGHT, nPos, nHeight )
    METHOD SetTopIndex( nItem )            INLINE SendMessage( ::nHandle, CB_SETTOPINDEX, nItem - 1, 0 )
    METHOD SetValue( nItem )               INLINE ::SetCurSel( nItem )
    METHOD ShowDropDown( lShow )           INLINE SendMessage( ::nHandle, CB_SHOWDROPDOWN, lShow, 0 )

    METHOD OnCommand()

ENDCLASS

METHOD New( cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, aRows, bAction, cToolTip, cStatusBar, lPixel, nID, nValue ) CLASS TComboBox

    DEFAULT lPixel TO TRUE

    ASSIGN ::cClassName  WITH "COMBOBOX"
    ASSIGN ::cName       WITH cName      DEFAULT "ComboBox_1"
    ASSIGN ::nStyle      WITH nStyle     DEFAULT WS_VISIBLE + WS_CHILD + WS_TABSTOP + WS_VSCROLL + CBS_AUTOHSCROLL
    ASSIGN ::nRow        WITH nRow       DEFAULT 0
    ASSIGN ::nCol        WITH nCol       DEFAULT 0
    ASSIGN ::nWidth      WITH nWidth     DEFAULT IIF( lPixel, 100, WG_Pixel2DialogX( 100 ) )
    ASSIGN ::nHeight     WITH nHeight    DEFAULT IIF( lPixel, 100, WG_Pixel2DialogY( 100 ) )

    ASSIGN ::nInitValue  WITH nValue     DEFAULT 0

    // Creo l'istanza tramite la classe window
    ::Super:New( ::cClassName, ::cName, ::nStyle, ;
                                 ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                                 oParent, bAction, cToolTip, cStatusBar, lPixel,, nID )

    // Save value for init
    ::xValue := aRows

RETURN Self

METHOD NewExtended( cName, nRow, nCol, nWidth, nHeight, oParent, aRows, bAction, cToolTip,;
                    cStatusBar, lPixel, nID, nValue, bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TComboBox

    ::New( cName, nRow, nCol, nWidth, nHeight, oParent, aRows, bAction, cToolTip, cStatusBar, lPixel, nID, nValue )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self

METHOD Init() CLASS TComboBox
    ::AddRows( ::xValue )
    // Set default colors
    IF ::oFgColor == NIL THEN ::SetForeGroundColor( TSystemSetting():GetColor(COLOR_WINDOWTEXT) )
    IF ::oBgColor == NIL THEN ::SetBackGroundColor( TSystemSetting():GetColor(COLOR_BTNHIGHLIGHT) )
    ::Super:Init()
    IF ::nInitValue <> NIL THEN ::SetValue( ::nInitValue )
RETURN Self

METHOD AddRows( aRows ) CLASS TComboBox
    aEval( aRows, {|e| ::AddString( e ) } )
RETURN Self

METHOD OnCommand( wParam, lParam ) CLASS TComboBox
   LOCAL nRet   := -1
   LOCAL nEvent := HiWord( wParam )
   LOCAL nID    := LoWord( wParam )

/*
 * Combo Box Notification Codes
 */

   DO CASE
      CASE nEvent == CBN_ERRSPACE
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_ERRSPACE", wParam, lParam )

      CASE nEvent == CBN_SELCHANGE
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_SELCHANGE", wParam, lParam )

      CASE nEvent == CBN_DBLCLK
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_DBLCLK", wParam, lParam )

      CASE nEvent == CBN_SETFOCUS
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_SETFOCUS", wParam, lParam )
           nRet = ::OnSetFocus()

      CASE nEvent == CBN_KILLFOCUS
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_KILLFOCUS", wParam, lParam )
           nRet = ::OnKillFocus()

      CASE nEvent == CBN_EDITCHANGE
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_EDITCHANGE", wParam, lParam )

      CASE nEvent == CBN_EDITUPDATE
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_EDITUPDATE", wParam, lParam )

      CASE nEvent == CBN_DROPDOWN
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_DROPDOWN", wParam, lParam )

      CASE nEvent == CBN_CLOSEUP
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_CLOSEUP", wParam, lParam )

      CASE nEvent == CBN_SELENDOK
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_SELENDOK", wParam, lParam )

      CASE nEvent == CBN_SELENDCANCEL
           WG_ApplObj():EventsWrite( "CBN", ::nHandle, "CBN_SELENDCANCEL", wParam, lParam )

   ENDCASE

RETURN nRet
