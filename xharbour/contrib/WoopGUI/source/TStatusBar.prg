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

CLASS WG_TStatusBar FROM WG_TWindowBase

    DATA oStack   AS OBJECT
    DATA aParts   AS ARRAY
    DATA oColor   AS OBJECT

    METHOD New() CONSTRUCTOR
    METHOD Create()
    METHOD GetFieldsCount()   INLINE ::SendMessage( SB_GETPARTS, 0, 0 )
    METHOD GetValue()
    //METHOD CreateAsArray()
    METHOD PopValue()
    METHOD PushValue()
    METHOD Repaint()
    METHOD SetBackgroundColor()
    METHOD SetValue()
    METHOD SetToolTip()
    METHOD SetWidths()
    METHOD WindowProc()

ENDCLASS

METHOD New( oParent, aParts, nStyle ) CLASS WG_TStatusBar

    ASSIGN ::cClassName   WITH STATUSCLASSNAME
    ASSIGN ::cName        WITH ""
    ASSIGN ::nStyle       WITH nStyle DEFAULT WS_CHILD + /*WS_BORDER +*/ WS_VISIBLE + SBARS_SIZEGRIP + SBARS_TOOLTIPS     // includes a sizing grip
    ASSIGN ::aParts       WITH aParts DEFAULT {-1, 50}

    // Creo l'istanza tramite la classe window
    ::Super:New( , ::cClassName, ::cName, ::nStyle, ;
                                 0, 0, 0, 0, ;
                                 oParent )

    ::Create( ::aParts ) // Numero di riquadri

RETURN Self

// METHOD Create( nParts ) CLASS WG_TStatusBar
//     DEFAULT nParts     TO 1
//     ::nParts       := nParts
//     ::Super:Create()
// RETURN WG_CreateStatusBar( ::oParent:nHandle, ::nHandle, nParts )


METHOD Create( ) CLASS WG_TStatusBar //aParts AS ARRAY )
    LOCAL cSizes := ""
    //ASSIGN ::aParts WITH aParts DEFAULT {-1}

    ::Super:Create()  // Make the window

    ::oStack := WG_TStack():New()
    //::SetWidths( { 200, 20, 50, 100, -1 } )
    ::SetWidths( { 200, -1, 60, 100 } )
    //::SetTooltip( "Message" )

RETURN Self

METHOD GetValue( nPart ) CLASS WG_TStatusBar
    LOCAL cString := ""
    DEFAULT nPart   TO 1
    //DEFAULT uFlags TO SBT_NOBORDERS
    ::SendMessage( SB_GETTEXT, nPart-1, @cString )
RETURN cString

METHOD PopValue() CLASS WG_TStatusBar
    LOCAL aBar := ::oStack:Pop() // { nPart, uFlags, cString }
    //DEFAULT uFlags TO SBT_NOBORDERS
RETURN SetStatusBar( ::nHandle, aBar[1], aBar[2], aBar[3] )

METHOD PushValue( cString, nPart, uFlags ) CLASS WG_TStatusBar
    DEFAULT nPart  TO 1
    ::oStack:Push( { nPart, uFlags, cString } )
    //DEFAULT uFlags TO SBT_NOBORDERS
RETURN SetStatusBar( ::nHandle, nPart, uFlags, cString )

METHOD Repaint(lParam) CLASS WG_TStatusBar
   LOCAL nWidth  := LoWord( lParam )
   LOCAL nHeight := HiWord( lParam )
   ::Move( 0,0,0,0, TRUE )
   ::SetWidths(,nWidth)
RETURN Self

METHOD SetBackgroundColor( ncBackColor, nPart ) CLASS WG_TStatusBar
   LOCAL nColor
   DEFAULT ncBackColor TO CLR_DEFAULT
   DEFAULT nPart       TO 1
   ::oColor := WG_TColor():New( ncBackColor )
   nColor := IIF( ncBackColor == CLR_DEFAULT, CLR_DEFAULT, ::oColor:GetColor() )
RETURN ::SendMessage( SB_SETBKCOLOR, 0, nColor )



METHOD SetValue( cString, nPart, uFlags ) CLASS WG_TStatusBar
    DEFAULT cString TO ""
    DEFAULT nPart   TO 1
    //DEFAULT uFlags TO SBT_NOBORDERS
    SetStatusBar( ::nHandle, nPart, uFlags, cString )
    ::SetToolTip( cString, nPart )
RETURN Self

METHOD SetToolTip( cString, nPart ) CLASS WG_TStatusBar
    DEFAULT nPart   TO 1
    DEFAULT cString TO ::GetValue( nPart )
RETURN ::SendMessage( SB_SETTIPTEXT, nPart-1, GetStringPtr( cString ) )

METHOD SetWidths( aParts AS ARRAY, nWidth AS NUMERIC ) CLASS WG_TStatusBar
    LOCAL cSizes := ""
    LOCAL nX := 0  // This is the x coordinate value that be incremented
                   // of the relative size of part
    LOCAL n, nLen, nPart
    LOCAL nParentWidth := IIF( nWidth == NIL, ::oParent:GetWidth(), nWidth )  // Get Parent Width
    LOCAL nTotalPartsWidth := 0

    IF aParts == NIL
       aParts := ::aParts
    ELSE
       ::aParts := aParts // Save array in object
    ENDIF

    // Before i get total length of absolute parts
    AEVAL( aParts, {|x| nTotalPartsWidth += IIF( x <> - 1, x, 0 ) } )

    //MessageBox( , "ParentWidth = " + cStr( nParentWidth ) + CRLF +;
    //              "TotalWidth = " + cStr( nTotalPartsWidth ) )

    nLen := LEN( aParts )

    FOR n := 1 TO nLen
        nPart := aParts[ n ]
        IF nPart == -1  // This part is relative only one admitted
           IF n == nLen // This is last part, leave as windows default
              nX := nPart
           ELSE
              nX += nParentWidth - nTotalPartsWidth
           ENDIF
        ELSE
           nX += nPart
        ENDIF
        cSizes += L2BIN( nX )
        // IF nPart == -1 // This can only last part
        //    EXIT
        // ENDIF
    NEXT
    //AEVAL(::aParts, {|x| IIF( x == -1, nX := x, nX += x), cSizes += L2BIN( nX ) } )
RETURN ::SendMessage( SB_SETPARTS, LEN( aParts ), cSizes )

METHOD WindowProc( nMsg, wParam, lParam ) CLASS WG_TStatusBar
   LOCAL nRet := -1
   IF ValType( ::bWindowProc  ) == "B"
      // User event handler
      nRet := Eval( ::bWindowProc, ::nHandle, nMsg, wParam, lParam )
   ENDIF
   IF nRet == -1
      // Class event handler
      DO CASE
         CASE nMsg == WM_SIZE
              ::Repaint(lParam)
              //nRet := 0
      ENDCASE
   ENDIF
   IF nRet == -1
      // Standard Event Handler
      nRet := ::Super:WindowProc( nMsg, wParam, lParam )
   ENDIF
RETURN nRet
