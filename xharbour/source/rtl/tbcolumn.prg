/*
 * $Id: tbcolumn.prg,v 1.12 0000/00/00 00:00:00 modalsist Exp $
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

#include "hbclass.ch"
#include "common.ch"
#include "tbrowse.ch"

//-------------------------------------------------------------------//

CLASS TBColumn


   ACCESS Block          INLINE ::bBlock
   ASSIGN Block(b)       INLINE ::bBlock := iif(Valtype(b)="B",b,::bBlock)

   ACCESS Cargo          INLINE ::uCargo
   ASSIGN Cargo(u)       INLINE ::uCargo := u

   ACCESS ColorBlock     INLINE ::bColorBlock
   ASSIGN ColorBlock(b)  INLINE ::bColorBlock := iif(Valtype(b)="B",b,::bColorBlock)

   ACCESS ColSep         INLINE ::cColSep
   ASSIGN ColSep(c)      INLINE ::cColSep := iif(Valtype(c)="C",c,::cColSep)

   ACCESS DefColor       INLINE ::aDefColor
   ASSIGN DefColor(a)    INLINE ::aDefColor := iif(Valtype(a)="A",::SetDefColor(a),::aDefColor)

   ACCESS Footing        INLINE ::cFooting
   ASSIGN Footing(c)     INLINE ::cFooting := iif(Valtype(c)="C",c,::cFooting)

   ACCESS FootSep        INLINE ::cFootSep
   ASSIGN FootSep(c)     INLINE ::cFootSep := iif(Valtype(c)="C",c,::cFootSep)

   ACCESS Heading        INLINE ::cHeading
   ASSIGN Heading(c)     INLINE ::cHeading := iif(Valtype(c)="C",c,::cHeading)

   ACCESS HeadSep        INLINE ::cHeadSep
   ASSIGN HeadSep(c)     INLINE ::cHeadSep := iif(Valtype(c)="C",c,::cHeadSep)

   ACCESS Picture        INLINE ::cPicture
   ASSIGN Picture(c)     INLINE ::cPicture := iif(Valtype(c)="C",c,::cPicture)

   ACCESS Width          INLINE ::nWidth
   ASSIGN Width(n)       INLINE ::nWidth := iif(Valtype(n)="N",n,::nWidth)


   METHOD New(cHeading, bBlock)


#ifdef HB_COMPAT_C53

   DATA  bPreBlock             //
   DATA  bPostBlock            //

   ACCESS PreBlock        INLINE ::bPreBlock
   ASSIGN PreBlock(b)     INLINE ::bPreBlock := iif(Valtype(b)="B",b,::bPreBlock)

   ACCESS PostBlock        INLINE ::bPostBlock
   ASSIGN PostBlock(b)     INLINE ::bPostBlock := iif(Valtype(b)="B",b,::bPostBlock)

   METHOD SetStyle( nMode, lSetting )

#endif

   HIDDEN:     /* H I D D E N */

   DATA  bBlock            // Code block to retrieve data for the column
   DATA  uCargo            // User-definable variable
   DATA  bColorBlock       // Code block that determines color of data items
   DATA  cColSep           // Column separator character
   DATA  aDefColor         // Array of numeric indexes into the color table
   DATA  cFooting          // Column footing string
   DATA  cFootSep          // Footing separator character
   DATA  cHeading          // Column heading string
   DATA  cHeadSep          // Heading separator character
   DATA  cPicture          // Column picture string
   DATA  nWidth            // Widht number of column


#ifdef HB_COMPAT_C53
   DATA  aSetStyle
#endif

   METHOD SetDefcolor(aDefColor)   // Set default color to column (2005/09/10) - E.F.)

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( cHeading, bBlock ) CLASS TBColumn

   local xRes, cType, nTokenPos := 0, nL

   DEFAULT cHeading TO ""

   ::aDefColor := { 1, 2, 1, 1 }
   ::cColSep   := nil

   ::nWidth    := nil
   ::cHeading  := if( valtype( cHeading ) == 'C', cHeading, ' ' )

   /* NOTE: needs to be initialized to an empty string or TBrowse()::WriteMLineText() does not work
            if there are columns which have a footing and others which don't
   */
   ::cFooting := ""
   ::bBlock    := bBlock

   #ifdef HB_COMPAT_C53
   ::aSetStyle := ARRAY( 3 )

   ::aSetStyle[ TBC_READWRITE ] := .f.
   ::aSetStyle[ TBC_MOVE ]      := .f.
   ::aSetStyle[ TBC_SIZE ]      := .f.
   #endif

return Self

//-------------------------------------------------------------------//


#ifdef HB_COMPAT_C53
METHOD SetStyle( nMode, lSetting ) CLASS TBColumn
  LOCAL lRet := .F.

  IF nMode > LEN( ::aSetStyle )
     RETURN .F.
  ENDIF

  lRet := ::aSetStyle[ nMode ]

  IF ISLOGICAL( lSetting )
     ::aSetStyle[ nMode ] := lSetting
  ENDIF

RETURN lRet
#endif

//-------------------------------------------------------------------//

METHOD SetDefcolor( aDefColor ) CLASS TBColumn

  aSize( aDefColor, 4 )
  DEFAULT aDefColor[ 1 ] TO 1
  DEFAULT aDefColor[ 2 ] TO 2
  DEFAULT aDefColor[ 3 ] TO 1
  DEFAULT aDefColor[ 4 ] TO 1

RETURN (aDefColor)

//-------------------------------------------------------------------//

function TBColumnNew(cHeading, bBlock)

return TBColumn():New(cHeading, bBlock)

//-------------------------------------------------------------------//

