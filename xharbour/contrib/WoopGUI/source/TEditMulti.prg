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
CLASS TEditMultiline FROM TEdit
    // Base
    DATA   lHScroll      AS LOGICAL INIT FALSE
    DATA   lVScroll      AS LOGICAL INIT FALSE

    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR
    //METHOD Init()


ENDCLASS

METHOD New( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, cValue, ;
            nLimitText, lReadOnly, lPassword, lHScroll, lVScroll ) CLASS TEditMultiline

    WG_DebugTrace( "TEditMultiline:New()")

    DEFAULT lPixel TO TRUE
    ASSIGN ::lHScroll    WITH lHScroll   DEFAULT FALSE
    ASSIGN ::lVScroll    WITH lVScroll   DEFAULT FALSE
    ASSIGN ::nStyle      WITH WS_BORDER + ES_WANTRETURN + WS_CHILD + WS_VISIBLE + WS_TABSTOP +;
                              ES_MULTILINE + ;
                              IIF( ::lHScroll, WS_HSCROLL + ES_AUTOHSCROLL, 0 ) +;
                              IIF( ::lVScroll, WS_VSCROLL + ES_AUTOVSCROLL, 0 ) +;
                              ES_LEFT
    ASSIGN ::nRow        WITH nRow       DEFAULT 0
    ASSIGN ::nCol        WITH nCol       DEFAULT 0
    ASSIGN ::nWidth      WITH nWidth     DEFAULT IIF( lPixel, IIF( ::lVScroll, 120, 100 ), WG_Pixel2DialogX( IIF( ::lVScroll, 120, 100 ) ) )
    ASSIGN ::nHeight     WITH nHeight    DEFAULT IIF( lPixel, 100, WG_Pixel2DialogY( 100 ) )

    // Creo l'istanza tramite la classe window
    ::Super:New( cName, ::nStyle, ;
                                 ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                                 oParent, bAction, cToolTip, cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword )

RETURN Self

METHOD NewExtended( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip,;
                    cStatusBar, lPixel, nID, cValue, nLimit, lReadOnly, lPassword, lHScroll, lVScroll, ;
                    bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TEditMultiline

    WG_DebugTrace( "TEditMultiline:NewExtended()")

    ::New( cName,nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, cValue, nLimit, lReadOnly, lPassword, lHScroll, lVScroll )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self


