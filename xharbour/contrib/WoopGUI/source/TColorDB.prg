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

CLASS TColorDatabase FROM TObject

    CLASSDATA  aColorTable AS ARRAY  HIDDEN

    METHOD Create()

    METHOD FindColor( cName )

ENDCLASS

METHOD FindColor( cName ) CLASS TColorDatabase
   LOCAL nPos, aColor
   ::Create()
   cName := Upper( cName )
   nPos := aScan( ::aColorTable, {|o| o[1] == cName } )
   IF nPos > 0
      aColor := ::aColorTable[ nPos ]
   ENDIF
RETURN aColor


METHOD Create() CLASS TColorDatabase
   IF  ::aColorTable == NIL
       ::aColorTable := ;
       { ;
           {"AQUAMARINE",112, 219, 147}, ;
           {"BLACK",0, 0, 0}, ;
           {"BLUE", 0, 0, 255}, ;
           {"BLUE VIOLET", 159, 95, 159}, ;
           {"BROWN", 165, 42, 42}, ;
           {"CADET BLUE", 95, 159, 159}, ;
           {"CORAL", 255, 127, 0}, ;
           {"CORNFLOWER BLUE", 66, 66, 111}, ;
           {"CYAN", 0, 255, 255}, ;
           {"DARK GREY", 47, 47, 47}, ;   // ?
           {"DARK GREEN", 47, 79, 47}, ;
           {"DARK OLIVE GREEN", 79, 79, 47}, ;
           {"DARK ORCHID", 153, 50, 204}, ;
           {"DARK SLATE BLUE", 107, 35, 142}, ;
           {"DARK SLATE GREY", 47, 79, 79}, ;
           {"DARK TURQUOISE", 112, 147, 219}, ;
           {"DIM GREY", 84, 84, 84}, ;
           {"FIREBRICK", 142, 35, 35}, ;
           {"FOREST GREEN", 35, 142, 35}, ;
           {"GOLD", 204, 127, 50}, ;
           {"GOLDENROD", 219, 219, 112}, ;
           {"GRAY", 128, 128, 128}, ;
           {"GREY", 128, 128, 128}, ;
           {"GREEN", 0, 255, 0}, ;
           {"GREEN YELLOW", 147, 219, 112}, ;
           {"INDIAN RED", 79, 47, 47}, ;
           {"KHAKI", 159, 159, 95}, ;
           {"LIGHT BLUE", 191, 216, 216}, ;
           {"LIGHT GREY", 192, 192, 192}, ;
           {"LIGHT STEEL BLUE", 143, 143, 188}, ;
           {"LIME GREEN", 50, 204, 50}, ;
           {"LIGHT MAGENTA", 255, 0, 255}, ;
           {"MAGENTA", 255, 0, 255}, ;
           {"MAROON", 142, 35, 107}, ;
           {"MEDIUM AQUAMARINE", 50, 204, 153}, ;
           {"MEDIUM BLUE", 50, 50, 204}, ;
           {"MEDIUM CYAN", 0, 230, 230}, ;
           {"MEDIUM GRAY", 100, 100, 100}, ;
           {"MEDIUM GREY", 100, 100, 100}, ;
           {"MEDIUM FOREST GREEN", 107, 142, 35}, ;
           {"MEDIUM GOLDENROD", 234, 234, 173}, ;
           {"MEDIUM GREEN", 0, 200, 0}, ;
           {"MEDIUM MAGENTA", 219, 0, 219}, ;
           {"MEDIUM ORCHID", 147, 112, 219}, ;
           {"MEDIUM RED", 0, 230, 0}, ;
           {"MEDIUM SEA GREEN", 66, 111, 66}, ;
           {"MEDIUM SLATE BLUE", 127, 0, 255}, ;
           {"MEDIUM SPRING GREEN", 127, 255, 0}, ;
           {"MEDIUM TURQUOISE", 112, 219, 219}, ;
           {"MEDIUM VIOLET RED", 219, 112, 147}, ;
           {"MEDIUM WHITE", 230, 230, 230}, ;
           {"MIDNIGHT BLUE", 47, 47, 79}, ;
           {"NAVY", 35, 35, 142}, ;
           {"ORANGE", 204, 50, 50}, ;
           {"ORANGE RED", 255, 0, 127}, ;
           {"ORCHID", 219, 112, 219}, ;
           {"PALE GREEN", 143, 188, 143}, ;
           {"PINK", 188, 143, 234}, ;
           {"PLUM", 234, 173, 234}, ;
           {"PURPLE", 176, 0, 255}, ;
           {"RED", 255, 0, 0}, ;
           {"SALMON", 111, 66, 66}, ;
           {"SEA GREEN", 35, 142, 107}, ;
           {"SIENNA", 142, 107, 35}, ;
           {"SKY BLUE", 50, 153, 204}, ;
           {"SLATE BLUE", 0, 127, 255}, ;
           {"SPRING GREEN", 0, 255, 127}, ;
           {"STEEL BLUE", 35, 107, 142}, ;
           {"TAN", 219, 147, 112}, ;
           {"THISTLE", 216, 191, 216}, ;
           {"TURQUOISE", 173, 234, 234}, ;
           {"VIOLET", 79, 47, 79}, ;
           {"VIOLET RED", 204, 50, 153}, ;
           {"WHEAT", 216, 216, 191}, ;
           {"WHITE", 255, 255, 255}, ;
           {"YELLOW", 255, 255, 0}, ;
           {"YELLOW GREEN", 153, 204, 50}, ;
           {"MEDIUM GOLDENROD", 234, 234, 173}, ;
           {"MEDIUM FOREST GREEN", 107, 142, 35}, ;
           {"LIGHT MAGENTA", 255, 0, 255}, ;
           {"MEDIUM GREY", 100, 100, 100} ;
       }
   ENDIF
RETURN Self
