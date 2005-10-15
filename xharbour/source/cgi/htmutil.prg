/*
 * $Id: htmutil.prg,v 1.2 2005/10/13 16:29:45 lculik Exp $
 */

/*
 * Harbour Project source code:
 * Misc Suport Functions for HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "html.ch"

/****
*
*     backButton()
*
*/

PROCEDURE BackButton( cImage, oHtm )

   DEFAULT cImage TO "back.gif"
   DEFAULT oHtm TO HtmlPageObject()

   IMAGE( cImage ) ;
          URL "" ;
          ONCLICK "history.back()" ;
          OF oHtm

   RETURN

/****
*
*     BackFormButton()
*
*/

PROCEDURE BackFormButton( cImage, oForm )

   LOCAL oBut

   DEFAULT oForm TO HtmlFormObject()

   IF cImage == NIL
      DEFINE BUTTON oBut ;
         NAME "BackButton" ;
         VALUE "go Back" ;
         ONCLICK "history.back()" ;
         IN oForm
   ELSE
      DEFINE IMAGE oBut ;
         NAME "BackButton" ;
         SOURCE( cImage ) ;
         ONCLICK "history.back()" ;
         IN oForm
   ENDIF

   RETURN

/****
*
*     PutCounter()
*
*/

FUNCTION PutCounter( oHtm, nNumber, cDir, nDigits, nWidth, bgColor, nBorder )

   LOCAL i
   LOCAL cStr    := ""
   LOCAL cLetter := ""

   DEFAULT oHtm TO HtmlPageObject()
   DEFAULT nNumber TO 0
   DEFAULT cDir TO "/images/counters/"
   DEFAULT nWidth TO 50
   DEFAULT nDigits TO Len( Alltrim( Str( nNumber ) ) )
   DEFAULT nBorder TO 1
   DEFAULT BGCOLOR TO "black"

   IF Valtype( nNumber ) == "N"
      cStr := Strzero( nNumber, nDigits )
   ENDIF

   oHtm:Write( "<center>" )
   DEFINE TABLE ;
      BORDER( nBorder ) ;
      WIDTH( nWidth ) ;
      COLORBG( bgColor ) ;
      OF oHtm

   oHtm:newTableRow()
   oHtm:newTableCell( "center" )

   FOR i := 1 TO Len( cStr )
      cLetter := Substr( cStr, i, 1 )
      IMAGE cDir + cLetter + ".gif" ;
         BORDER 0 ;
         OF oHtm
   NEXT

   oHtm:endTableCell()
   oHtm:endTableRow()
   oHtm:endTable()

   oHtm:Write( "</center>" )

RETURN Nil


