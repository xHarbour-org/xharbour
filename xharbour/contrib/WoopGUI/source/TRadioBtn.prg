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
CLASS TRadioButton FROM TButton

    DATA   lSelected     AS LOGICAL INIT FALSE
    // METODI
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR
    METHOD Init()

ENDCLASS

METHOD New( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, lFirst, lSelected ) CLASS TRadioButton

    DEFAULT lFirst    TO FALSE
    DEFAULT lPixel    TO TRUE
    //ParamDisplay( Self, hb_aparams() )

    ASSIGN ::cName     WITH cName     DEFAULT "RadioBtn_1"
    ASSIGN ::nStyle    WITH WS_VISIBLE + WS_CHILD + WS_TABSTOP + BS_AUTORADIOBUTTON //+ WS_GROUP
    IF lFirst THEN ::nStyle += WS_GROUP
    ASSIGN ::nRow      WITH nRow      DEFAULT 0
    ASSIGN ::nCol      WITH nCol      DEFAULT 0
    ASSIGN ::nWidth    WITH nWidth    DEFAULT IIF( lPixel, 100, WG_Pixel2DialogX( 100 ) )
    ASSIGN ::nHeight   WITH nHeight   DEFAULT IIF( lPixel,  28, WG_Pixel2DialogY(  28 ) )
    ASSIGN ::lSelected WITH lSelected DEFAULT FALSE

    // Creo l'istanza tramite la classe window
    ::Super:New( ::cName, ::nStyle, ;
                          ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                          oParent, bAction, cToolTip, cStatusBar, lPixel )

RETURN Self

METHOD Init() CLASS TRadioButton
    IF ::lSelected THEN ::SetValue( BST_CHECKED )
    ::Super:Init()
RETURN Self

METHOD NewExtended( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, lFirst, lSelected,;
                    bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TRadioButton

    ::New( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, lFirst, lSelected )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self
