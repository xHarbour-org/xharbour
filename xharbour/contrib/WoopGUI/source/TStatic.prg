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

CLASS TStatic FROM TControl

    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR

    METHOD GetValue()    INLINE Sendmessage( ::nHandle, BM_GETCHECK, 0, 0 )

    METHOD SetStyle()

ENDCLASS

METHOD New( cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID ) CLASS TStatic

    DEFAULT lPixel TO TRUE

    ASSIGN ::cClassName WITH "STATIC"
    ASSIGN ::cName      WITH cName    DEFAULT "Static_1"
    ASSIGN ::nStyle     WITH nStyle   DEFAULT WS_VISIBLE + WS_CHILD + SS_LEFT
    ASSIGN ::nRow       WITH nRow     DEFAULT 0
    ASSIGN ::nCol       WITH nCol     DEFAULT 0
    ASSIGN ::nWidth     WITH nWidth   DEFAULT IIF( lPixel,  50, WG_Pixel2DialogX( 50 ) )
    ASSIGN ::nHeight    WITH nHeight  DEFAULT IIF( lPixel,  20, WG_Pixel2DialogY( 20 ) )

    ::Super:New( ::cClassName, ::cName, ::nStyle, ;
                                 ::nRow, ::nCol, ::nWidth, ::nHeight, ;
                                 oParent, bAction, cToolTip, cStatusBar, lPixel,, nID )

RETURN Self

METHOD NewExtended( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip,;
                    cStatusBar, lPixel, nID, cAlign, bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TStatic
    //WG_ParamDisplay( Self, hb_aparams(), "TStatic_NewExtended" )
    ::New( cName, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, nID, cAlign )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self


METHOD SetStyle( nStyle ) CLASS TStatic
   ::nStyle := WS_VISIBLE + WS_CHILD + nStyle
RETURN ::Super:SetStyle( ::nStyle )


   