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

CLASS TDropListComboBox FROM TComboBox

    METHOD New() CONSTRUCTOR

ENDCLASS

METHOD New( cName, nTop, nLeft, nWidth, nHeight, oParent, aRows, bAction, cToolTip, cStatusBar, lPixel, nID, nValue ) CLASS TDropListComboBox

    DEFAULT lPixel TO TRUE

    ASSIGN ::cName     WITH cName    DEFAULT "DropLCB_1"
    ASSIGN ::nStyle    WITH WS_VISIBLE + WS_CHILD + WS_TABSTOP + WS_VSCROLL + CBS_DROPDOWNLIST
    ASSIGN ::nTop      WITH nTop     DEFAULT 0
    ASSIGN ::nLeft     WITH nLeft    DEFAULT 0
    ASSIGN ::nWidth    WITH nWidth   DEFAULT IIF( lPixel, 100, WG_Pixel2DialogX( 100 ) )
    ASSIGN ::nHeight   WITH nHeight  DEFAULT IIF( lPixel, 100, WG_Pixel2DialogY( 100 ) )

    ::Super:New( ::cName, ::nStyle, ;
                          ::nTop, ::nLeft, ::nWidth, ::nHeight, ;
                          oParent, aRows, bAction, cToolTip, cStatusBar, lPixel, nID, nValue )

RETURN Self

