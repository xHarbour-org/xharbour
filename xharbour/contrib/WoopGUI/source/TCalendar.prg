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
CLASS WG_TCalendar FROM WG_TControl
    // Base

    DATA    nInitValue    AS NUMERIC
    DATA    oDateTime     AS OBJECT  HIDDEN

    // METODI
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR

    METHOD Init()

    METHOD GetDate()              INLINE ::oDateTime:GetDate()
    METHOD GetTime()              INLINE ::oDateTime:GetTime()
    METHOD GetValue()             INLINE ::GetDate()
    METHOD SetDate( x )           INLINE ::oDateTime:SetDate( x )
    METHOD SetValue( x )          INLINE ::SetDate( x )

    // Events
    METHOD OnNotify()
    //METHOD  OnKillFocus()
    //METHOD  OnSetFocus()

ENDCLASS

METHOD New( cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, dValue ) CLASS WG_TCalendar


    //WG_ParamDisplay( Self, hb_aparams(), "TCalendar_New" )
    DEFAULT lPixel TO TRUE

    ASSIGN ::cClassName  WITH "SysDateTimePick32"
    ASSIGN ::cName       WITH cName      DEFAULT "Calendar_1"
    ASSIGN ::nStyle      WITH nStyle     DEFAULT WS_VISIBLE + WS_CHILD + WS_TABSTOP
    ASSIGN ::nRow        WITH nRow       DEFAULT 0
    ASSIGN ::nCol        WITH nCol       DEFAULT 0
    ASSIGN ::nWidth      WITH nWidth     DEFAULT IIF( lPixel, 100, WG_Pixel2DialogX( 100 ) )
    ASSIGN ::nHeight     WITH nHeight    DEFAULT IIF( lPixel, 20, WG_Pixel2DialogY( 20 ) )
    ASSIGN ::xValue      WITH dValue     DEFAULT Date()
    //::oDateTime := WG_TDateTime():New( dValue )

    //ASSIGN ::nInitValue  WITH nValue     DEFAULT 0

    // Creo l'istanza tramite la classe window
    ::Super:New( ::cClassName, ::cName, ::nStyle, ;
                                 ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                                 oParent, bAction, cToolTip, cStatusBar, lPixel,, nID )
    //MessageBox(,"Passato da calendar" )

RETURN Self

METHOD NewExtended( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip,;
                    cStatusBar, lPixel, nID, dValue, bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS WG_TCalendar

    //WG_ParamDisplay( Self, hb_aparams(), "TCalendar_NewExtended" )

    ::New( cName, ,nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, dValue )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self

METHOD Init() CLASS WG_TCalendar
    // Set default colors
    IF ::oFgColor == NIL THEN ::SetForeGroundColor( WG_TSystemSetting():GetColor(COLOR_WINDOWTEXT) )
    IF ::oBgColor == NIL THEN ::SetBackGroundColor( WG_TSystemSetting():GetColor(COLOR_BTNHIGHLIGHT) )
    ::Super:Init()
    ::oDateTime := WG_TDateTime():New( ::xValue )
    //::oDateTime:DisplayData()
    ::UpdateVar( ::GetDate() )
    //::DisplayData()
    //IF ::nInitValue <> NIL THEN ::SetValue( ::nInitValue )
RETURN Self

METHOD OnNotify( wParam, lParam ) CLASS WG_TCalendar
   LOCAL nRet   := -1
   LOCAL nEvent  := WG_GetNMHDRCode( lParam )

   //MessageBox(,"Passato da Calendar command - codice == " + cStr( nEvent ))
   DO CASE
      //CASE nEvent == BN_CLICKED
      //CASE nEvent == BN_DBLCLK // or BN_DOUBLECLICKED

      //CASE nEvent == EN_UPDATE
      //CASE nEvent == EN_CHANGE
      //     ::UpdateVar()

      CASE nEvent == NM_KILLFOCUS
           //MessageBox(,"Passato da BN_KILLFOCUS")
           nRet = ::OnKillFocus()
           //::DisplayArray( DATETIME_GETSYSTEMTIME( ::nHandle ) )
           ::oDateTime:SetDateTime( DATETIME_GETSYSTEMTIME( ::nHandle ) )
           ::UpdateVar( ::GetDate() )
           //::DisplayArray( DATETIME_GETRANGE( ::nHandle ) )
      CASE nEvent == NM_SETFOCUS
           //MessageBox(,"Passato da EN_SETFOCUS")
           DATETIME_SETSYSTEMTIME( ::nHandle, ::oDateTime:GetDateTime() )
           ::UpdateVar( ::GetDate() )
           nRet = ::OnSetFocus()
      OTHERWISE
           IF ::HasAction()
              ::ExecAction( Self )
              nRet := 0  // Something exec, stop handling
           ENDIF
   ENDCASE

RETURN nRet

