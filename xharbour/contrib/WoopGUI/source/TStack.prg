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

CLASS WG_TStack FROM WG_TObject

   DATA   aData

   METHOD New()       INLINE ( ::aData := {}, Self )
   METHOD Push( x )   INLINE AAdd( ::aData, x )
   METHOD Pop()       BLOCK  { |x, y| y:= ATail(x:aData), ;
                                      Asize( x:aData, Max( Len( x:aData ) - 1, 0 ) ), y }
   METHOD Len()       INLINE Len( ::aData )
   METHOD Empty()     INLINE Empty( ::aData )
   METHOD Tail()      INLINE ATail( ::aData )
   METHOD Top()       INLINE ATail( ::aData )
   METHOD Bottom()    INLINE IIF( !::Empty(), ::aData[ 1 ], NIL )
   MESSAGE Del()               METHOD tDel
   MESSAGE Ins()               METHOD tIns
   METHOD Scan( xbSearch )
   METHOD Pos( x )       INLINE ::aData[ x ]
   METHOD RotateUp()
   METHOD RotateDown()
   METHOD Sort( bEval, nStart, nCount ) INLINE ( ASort( ::aData, nStart,;
                                                        nCount, bEval ), Self )
ENDCLASS

//----------------------------------------------------------------------------//

METHOD tDel( nPos ) CLASS WG_TStack
   LOCAL xValue

   DEFAULT nPos TO 1

   IF nPos > 0
      xValue := ::aData[ nPos ]
      ADel( ::aData, nPos )
      ASize( ::aData, Max( Len( ::aData ) - 1, 0 ) )
   ENDIF

   RETURN xValue

//----------------------------------------------------------------------------//

METHOD tIns( xValue, nPos ) CLASS WG_TStack

   DEFAULT nPos TO 1

   IF nPos > 0
      ASize( ::aData, Len( ::aData ) + 1 )
      AIns( ::aData, nPos )
      ::aData[ nPos ] := xValue
   ENDIF

   RETURN xValue

//----------------------------------------------------------------------------//

METHOD Scan( xbSearch ) CLASS WG_TStack
   LOCAL nPos
   LOCAL xValue

   IF ( nPos := AScan( ::aData, xbSearch ) ) > 0
   ENDIF

   RETURN nPos

//----------------------------------------------------------------------------//

METHOD RotateUp() CLASS WG_TStack
   LOCAL xValue

   xValue := ::Del()
   ::Push( xValue )

   RETURN Self

//----------------------------------------------------------------------------//

METHOD RotateDown() CLASS WG_TStack
   LOCAL xValue

   xValue := ::Pop()
   ::Ins( xValue )

   RETURN Self

//----------------------------------------------------------------------------//

