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

CLASS WG_TColor FROM WG_TObject

    DATA   nRGBColor

    METHOD New() CONSTRUCTOR

    METHOD GetValue()                 INLINE ::GetColor()
    METHOD SetValue()                 INLINE ::SetColor()

    METHOD GetBlue()                  INLINE GetBValue( ::nRGBColor )
    METHOD GetGreen()                 INLINE GetGValue( ::nRGBColor )
    METHOD GetRed()                   INLINE GetRValue( ::nRGBColor )
    METHOD GetColor()                 INLINE ::nRGBColor
    METHOD SetColor( nRGB )           INLINE ::nRGBColor := nRGB
    METHOD SetRGBColor( nR, nG, nB )  INLINE ::nRGBColor := RGB( nR, nG, nB )
    METHOD RGB()                      INLINE ::SetRGBColor()
    METHOD FindColor( cColor )        INLINE LOCAL aColor, ;
                                             aColor := WG_TColorDatabase():FindColor( cColor ),;
                                             IIF( aColor <> NIL, ;
                                                  ::SetRGBColor( aColor[2], aColor[3], aColor[4] ),;
                                                  NIL ;
                                                )

ENDCLASS

METHOD New( ncColor ) CLASS WG_TColor
   LOCAL cClipperColor
   IF     ValType( ncColor ) == "N"  // == nRGB
          ::SetColor( ncColor )
   ELSEIF ValType( ncColor ) == "C"  // == cColor
          ::FindColor( ncColor )
   ENDIF
RETURN Self

FUNCTION WG_IsClipperColor( ncColor )
RETURN ( WG_SplitClipperColor( ncColor ) <> NIL )

FUNCTION WG_SplitClipperColor( ncColor )
   LOCAL lIsClipperColor := TRUE
   LOCAL aClipperColor
   LOCAL nPos, nColor, cFore := "", cBack := ""

   IF     ValType( ncColor ) == "C"

      // Check if has a / character inside
      IF ( nPos := At( "/", ncColor ) ) > 0
         // Ok, i consider fore and back color defined
         cFore := AllTrim( Left( ncColor, nPos - 1 ) )
         cBack := AllTrim( SubStr( ncColor, nPos + 1 ) )
      ELSE
         // Ok, i consider only fore color defined
         cFore := AllTrim( ncColor )
         cBack := ""
      ENDIF

      //MessageBox( , "Color = " + cStr( ncColor ) + CRLF +;
      //               "Fore = " + cStr( cFore ) + CRLF +;
      //               "Back = " + cStr( cBack ) )

      // Now i check fore color
      IF WG_GetClipperColor( cFore ) == ""
         cFore := ""
      ENDIF

      // Now i check back color
      IF WG_GetClipperColor( cBack ) == ""
         cBack := ""
      ENDIF

   ELSEIF ValType( ncColor ) == "N"
      // Clipper color can be defined only as a string otherwise it conflict with numerical
      // windows color
      cFore := ""
      cBack := ""
   ENDIF

   IF !( cFore == "" .AND. cBack == "" )
      aClipperColor := { cFore, cBack }
   ENDIF

RETURN aClipperColor

FUNCTION WG_ClipperColorDB()
   STATIC aClipperColorTable

   IF  aClipperColorTable == NIL
 // Clipper Letter, Clipper Color Name, WoopGUI Color Name,  Clipper Number
       aClipperColorTable := ;
       { ;
           {"N  " , "BLACK"           , "BLACK"           ,  0}, ;
           {"B  " , "BLUE"            , "MEDIUM BLUE"     ,  1}, ;
           {"G  " , "GREEN"           , "MEDIUM GREEN"    ,  2}, ;
           {"BG " , "CYAN"            , "MEDIUM CYAN"     ,  3}, ;
           {"R  " , "RED"             , "MEDIUM RED"      ,  4}, ;
           {"RB " , "MAGENTA"         , "MEDIUM MAGENTA"  ,  5}, ;
           {"GR " , "BROWN"           , "BROWN"           ,  6}, ;
           {"W  " , "WHITE"           , "MEDIUM WHITE"    ,  7}, ;
           {"N+ " , "GRAY"            , "GRAY"            ,  8}, ;
           {"B+ " , "BRIGHT BLUE"     , "BLUE"            ,  9}, ;
           {"G+ " , "BRIGHT GREEN"    , "GREEN"           , 10}, ;
           {"BG+" , "BRIGHT CYAN"     , "CYAN"            , 11}, ;
           {"R+ " , "BRIGHT RED"      , "RED"             , 12}, ;
           {"RB+" , "BRIGHT MAGENTA"  , "MAGENTA"         , 13}, ;
           {"GR+" , "YELLOW"          , "YELLOW"          , 14}, ;
           {"W+ " , "BRIGHT WHITE"    , "WHITE"           , 15}  ;
       }
   ENDIF

RETURN aClipperColorTable

FUNCTION WG_GetClipperColor( cColor AS STRING )
   LOCAL aClipperColorTable := WG_ClipperColorDB()
   LOCAL  nPos, cWoopGUIColor := ""

   cColor := PadR( Upper( cColor ), 3, " " )
   nPos := aScan( aClipperColorTable, {|e| e[1] == cColor } )
   IF nPos > 0
      cWoopGUIColor := aClipperColorTable[nPos, 3]
   ENDIF

RETURN cWoopGUIColor
