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
CLASS WG_TPushButton FROM WG_TButton

    // METODI
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR

ENDCLASS

METHOD New( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, lDefault, nID ) CLASS WG_TPushButton

    WG_DebugTrace( "TPushButton:New()" )

    DEFAULT lPixel TO TRUE

    ASSIGN ::cName     WITH cName    DEFAULT "PushBtn_1"
    ASSIGN ::nStyle    WITH WS_VISIBLE + WS_CHILD + WS_TABSTOP + BS_PUSHBUTTON + BS_NOTIFY
    ASSIGN ::nRow      WITH nRow     DEFAULT 0
    ASSIGN ::nCol      WITH nCol     DEFAULT 0
    ASSIGN ::nWidth    WITH nWidth   DEFAULT IIF( lPixel, 80, WG_Pixel2DialogX( 80 ) )
    ASSIGN ::nHeight   WITH nHeight  DEFAULT IIF( lPixel, 24, WG_Pixel2DialogY( 24 ) )

    // Creo l'istanza tramite la classe window
    ::Super:New( ::cName, ::nStyle, ;
                          ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                          oParent, bAction, cToolTip, cStatusBar, lPixel, lDefault, nID )

RETURN Self

METHOD NewExtended( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel,;
                    lDefault, nID, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS WG_TPushButton

    WG_DebugTrace( "TPushButton:NewExtended()" )

    //WG_ParamDisplay( Self, hb_aparams(), "TPushButton_Extend" )

    ::New( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, lDefault, nID )
    ::Extend( , oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self
