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

CLASS TStaticText FROM TStatic

    METHOD New() CONSTRUCTOR

ENDCLASS

METHOD New( cName, nTop, nLeft, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, cAlign ) CLASS TStaticText
    LOCAL nAlign

    DEFAULT cAlign TO "LEFT"

    DO CASE
       CASE cAlign == "LEFT"
            nAlign := SS_LEFT
       CASE cAlign == "RIGHT"
            nAlign := SS_RIGHT
       CASE cAlign == "CENTER" .OR.;
            cAlign == "CENTERED"
            nAlign := SS_CENTER
    ENDCASE

    DEFAULT nAlign TO SS_LEFT

    ASSIGN ::cName     WITH cName    DEFAULT "Label_1"
    ASSIGN ::nStyle    WITH WS_VISIBLE + WS_CHILD + nAlign
    // ASSIGN ::nTop      WITH nTop     DEFAULT 0
    // ASSIGN ::nLeft     WITH nLeft    DEFAULT 0
    // ASSIGN ::nWidth    WITH nWidth   DEFAULT 100
    // ASSIGN ::nHeight   WITH nHeight  DEFAULT 28

    // Creo l'istanza tramite la classe window
    ::Super:New( ::cName, ::nStyle, ;
                          nTop, nLeft, nWidth, nHeight, ;
                          oParent, bAction, cToolTip, cStatusBar, lPixel,, nID )

RETURN Self

