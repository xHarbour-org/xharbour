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
CLASS TCheckBox FROM TButton
    // Base
    DATA   lSelected     AS LOGICAL INIT FALSE

    // METODI
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR

    METHOD Init()

    METHOD GetValue()
    METHOD SetValue()

ENDCLASS

METHOD New( cName, nTop, nLeft, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, lSelected ) CLASS TCheckBox

    DEFAULT lPixel TO TRUE

    ASSIGN ::cName     WITH cName    DEFAULT "CheckBox_1"
    ASSIGN ::nStyle    WITH WS_VISIBLE + WS_CHILD + WS_TABSTOP + BS_AUTOCHECKBOX + WS_GROUP
    ASSIGN ::nTop      WITH nTop     DEFAULT 0
    ASSIGN ::nLeft     WITH nLeft    DEFAULT 0
    ASSIGN ::nWidth    WITH nWidth   DEFAULT IIF( lPixel, 100, WG_Pixel2DialogX( 100 ) )
    ASSIGN ::nHeight   WITH nHeight  DEFAULT IIF( lPixel,  28, WG_Pixel2DialogY(  28 ) )
    ASSIGN ::lSelected WITH lSelected DEFAULT FALSE

    // Creo l'istanza tramite la classe window
    ::Super:New( ::cName, ::nStyle, ;
                          ::nTop, ::nLeft, ::nWidth, ::nHeight, ;
                          oParent, bAction, cToolTip, cStatusBar, lPixel, nID )

RETURN Self

METHOD NewExtended( cName, nTop, nLeft, nWidth, nHeight, oParent, bAction, cToolTip,;
                    cStatusBar, lPixel, nID, lSelected, bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TCheckBox

    ::New( cName, nTop, nLeft, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, lSelected )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self

METHOD Init() CLASS TCheckBox
    IF ::lSelected THEN ::SetValue( ::lSelected )
    ::Super:Init()
RETURN Self

METHOD SetValue( lSelected ) CLASS TCheckBox
   ASSIGN ::lSelected WITH lSelected DEFAULT TRUE
   ::Super:SetValue( IIF( lSelected, 1, 0 ) )
   ::UpdateVar( IIF( ::xValue == 0, FALSE, TRUE ) )
RETURN Self

METHOD GetValue() CLASS TCheckBox
RETURN IIF( ::Super:GetValue() == 0, FALSE, TRUE )
