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

CLASS TToolTip FROM TWindow

    //DATA    cValue  AS STRING INIT ""

    METHOD New() CONSTRUCTOR
    METHOD GetValue()       INLINE ::cName
    METHOD SetValue()

ENDCLASS

METHOD New( oParent, cText, nStyle ) CLASS TToolTip

    // Impostazioni di default
    DEFAULT nStyle     TO TTS_NOPREFIX + TTS_ALWAYSTIP

    ASSIGN ::cClassName WITH "tooltips_class32"
    ASSIGN ::cName      WITH NIL
    ASSIGN ::nStyle     WITH WS_POPUP + nStyle

    // Creo l'istanza tramite la classe window
    ::Super:New( , ::cClassName, ::cName, ::nStyle, ;
                                 , , , , ;
                                 oParent )
    ::Create()
    //::DisplayData()

    ::SetWindowPos( ::nHandle, HWND_TOPMOST, 0, 0, 0, 0, ;
                    SWP_NOMOVE + SWP_NOSIZE + SWP_NOACTIVATE);

    ::SetValue( cText )

RETURN Self

METHOD SetValue( cString AS STRING ) CLASS TToolTip
  ::cName := cString
  //::DisplayData()
RETURN SetToolTip( ::oParent:nHandle, ::nHandle, cString )
