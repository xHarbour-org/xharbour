/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TBColumn Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

//-------------------------------------------------------------------//
#pragma w2
#include "hbclass.ch"
#include "common.ch"
#include "tbrowse.ch"

//-------------------------------------------------------------------//

CLASS TBColumn

   ACCESS Block      INLINE ::bBlock
   ASSIGN Block( b ) INLINE ::bBlock := ::SetValue( b, @::bBlock, "B" )

   ACCESS Cargo      INLINE ::xCargo
   ASSIGN Cargo( x ) INLINE ::xCargo := iif( !HB_ISNIL( x ), x, ::xCargo )

   ACCESS ColorBlock      INLINE ::bColorBlock
   ASSIGN ColorBlock( b ) INLINE ::SetValue( b, @::bColorBlock, "B" )

   ACCESS ColSep      INLINE ::cColSep
   ASSIGN ColSep( c ) INLINE ::SetValue( c, @::cColSep, "C" )

   ACCESS DefColor      INLINE ::aDefColor
   ASSIGN DefColor( a ) INLINE ::aDefColor := ::SetDefColor( a )

   ACCESS Footing      INLINE ::cFooting
   ASSIGN Footing( c ) INLINE ::SetValue( c, @::cFooting, "C" )

   ACCESS FootSep      INLINE ::cFootSep
   ASSIGN FootSep( c ) INLINE ::SetValue( c, @::cFootSep, "C" )

   ACCESS Heading      INLINE ::cHeading
   ASSIGN Heading( c ) INLINE ::SetValue( c, @::cHeading, "C" )

   ACCESS HeadSep      INLINE ::cHeadSep
   ASSIGN HeadSep( c ) INLINE ::SetValue( c, @::cHeadSep, "C" )

   ACCESS PICTURE      INLINE ::cPicture
   ASSIGN Picture( c ) INLINE ::SetValue( c, @::cPicture, "C" )

   ACCESS Width      INLINE ::nWidth
   ASSIGN Width( n ) INLINE ::nWidth := ::SetValue( n, @::nWidth, "N" )


   METHOD  New( cHeading, bBlock )

#ifdef HB_COMPAT_C53
   ACCESS PreBlock      INLINE ::bPreBlock
   ASSIGN PreBlock( b ) INLINE ::SetValue( b, @::bPreBlock, "B" )

   ACCESS PostBlock      INLINE ::bPostBlock
   ASSIGN PostBlock( b ) INLINE ::SetValue( b, @::bPostBlock, "B" )
#endif


   METHOD  Block( b )         INLINE ::SetValue( b, @::bBlock, "B" )
   METHOD  Cargo( x )         INLINE ::xCargo := iif( !HB_ISNIL( x ), x, ::xCargo )
   METHOD  ColorBlock( b )    INLINE ::SetValue( b, @::bColorBlock, "B" )
   METHOD  ColSep( c )        INLINE ::SetValue( c, @::cColSep, "C" )
   METHOD  DefColor( a )      INLINE ::aDefColor := ::SetDefColor( a )
   METHOD  Footing( c )       INLINE ::SetValue( c, @::cFooting, "C" )
   METHOD  FootSep( c )       INLINE ::SetValue( c, @::cFootSep, "C" )
   METHOD  Heading( c )       INLINE ::SetValue( c, @::cHeading, "C" )
   METHOD  HeadSep( c )       INLINE ::SetValue( c, @::cHeadSep, "C" )
   METHOD  Picture( c )       INLINE ::SetValue( c, @::cPicture, "C" )
   METHOD  Width( n )         INLINE ::SetValue( n, @::nWidth, "N" )


#ifdef HB_COMPAT_C53
   METHOD PreBlock( b )      INLINE ::SetValue( b, @::bPreBlock, "B" )
   METHOD PostBlock( b )     INLINE ::SetValue( b, @::bPostBlock, "B" )
   METHOD SetStyle( nMode, lSetting )
   METHOD AddStyle( nMode, lSetting )
#endif

   PROTECTED:     /* P R O T E C T E D */

   DATA   bBlock                         // Code block to retrieve data for the column
   DATA   xCargo                         // User-definable variable
   DATA   bColorBlock   INIT {|| NIL }   // Code block that determines color of data items
   DATA   cColSep                        // Column separator character
#ifdef HB_COMPAT_C53
   DATA   aDefColor     INIT { 1, 2, 1, 1 }   // Array of numeric indexes into the color table
#else
   DATA   aDefColor     INIT { 1, 2 }
#endif
   DATA   cFooting                       // Column footing character
   DATA   cFootSep                       // Footing separator character
   DATA   cHeading                       // Column heading character
   DATA   cHeadSep                       // Heading separator character
   DATA   cPicture                       // Column picture string
   DATA   nWidth                         // Column diplay width

#ifdef HB_COMPAT_C53
   DATA   bPreBlock
   DATA   bPostBlock
#endif

   HIDDEN:     /* H I D D E N */

   METHOD   SetDefColor( a )
   METHOD   SetValue( uValue, uData, cType )

#ifdef HB_COMPAT_C53
   DATA     aSetStyle
#endif

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( cHeading, bBlock ) CLASS TBColumn

   DEFAULT cHeading TO ""

#ifdef HB_COMPAT_C53
   ::DefColor := { 1, 2, 1, 1 }
#else
   ::DefColor := { 1, 2 }
#endif
   ::ColSep   := nil

   ::nWidth   := nil
   ::Heading  := if( HB_ISSTRING( cHeading ), cHeading, ' ' )

   /* NOTE: needs to be initialized to an empty string or TBrowse()::WriteMLineText() does not work
            if there are columns which have a footing and others which don't
   */
   ::Footing  := ""
//   ::block    := bBlock
   IF HB_ISBLOCK( bBlock )
      ::block := bBlock
   ELSE
      ::block := {|| NIL }
   ENDIF

#ifdef HB_COMPAT_C53
   ::aSetStyle := Array( 4 )

   ::aSetStyle[ TBC_READWRITE ] := .F.
   ::aSetStyle[ TBC_MOVE ]      := .F.
   ::aSetStyle[ TBC_SIZE ]      := .F.
   ::aSetStyle[ TBC_CUSTOM ]    := .F.
#endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetValue( uValue, uData, cType ) CLASS TBColumn

   uData := iif( ValType( uValue ) == cType, uValue, uData )

   RETURN ( uData )

//-------------------------------------------------------------------//

METHOD SetDefColor( aDef ) CLASS TBColumn

   LOCAL a

   a := ::aDefColor

   IF HB_ISARRAY( aDef )  .AND. !Empty( aDef )
      a := aDef
      IF Len( a ) == 1
         AAdd( a, aDef[1] )
#ifdef HB_COMPAT_C53
         AAdd( a, aDef[1] )
         AAdd( a, aDef[1] )
      ELSEIF Len( a ) == 2
         AAdd( a, aDef[1] )
         AAdd( a, aDef[2] )
      ELSEIF Len( a ) == 3
         AAdd( a, aDef[1] )
#endif
      ENDIF

#ifdef HB_COMPAT_C53
      IF Len( a ) > 4
         ASize( a, 4 )
      ENDIF
#else
      IF Len( a ) > 2
         ASize( a, 2 )
      ENDIF
#endif
   ENDIF

   RETURN a

//-------------------------------------------------------------------//

#ifdef HB_COMPAT_C53

METHOD SetStyle( nMode, lSetting ) CLASS TBColumn

   LOCAL lRet

   IF nMode > Len( ::aSetStyle )
      RETURN .F.
   ENDIF

   lRet := ::aSetStyle[ nMode ]

   IF ISLOGICAL( lSetting )
      ::aSetStyle[ nMode ] := lSetting
   ENDIF

   RETURN lRet

//-------------------------------------------------------------------//

METHOD AddStyle( nMode, lSetting ) CLASS TBColumn

   LOCAL lRet
   LOCAL nLen := Len( ::aSetStyle )

   IF nMode != nLen + 1
      RETURN .F.
   ENDIF

   IF ISLOGICAL( lSetting )
      AAdd( ::aSetStyle, lSetting )
   ENDIF

   lRet := ( Len( ::aSetStyle ) == nLen + 1 )

   RETURN lRet

#endif

//-------------------------------------------------------------------//

FUNCTION TBColumnNew( cHeading, bBlock )

   RETURN TBColumn():New( cHeading, bBlock )

//-------------------------------------------------------------------//
