/*
 * $Id: htmbrows.prg,v 1.1 2001/12/25 16:51:58 lculik Exp $
 */

/*
 * Harbour Project source code:
 * HTMBROWS -Browse module
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
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *    HtmlBrowseSql()

 * See doc/license.txt for licensing terms.
 *
 */

#include "default.ch"
#include "html.ch"

/****
*     htmlBrowse()
*
*
*     Create an HTML table containing the raw contents of a .DBF table.
*     Every record is associated with an action pushbutton, presented
*     in the leftmost table cell.
*
*     <oHtm>      Active HTML() object
*     <cAction>   Action to perform if the button is clicked
*     <lUseLinks> Generate record number links instead of push buttons
*     <cTarget>   Target frame for <cAction>
*     <cAlias>    Database alias. Defaults to ALIAS()
*
*     Numbers are right formatted. Everything else is centered.
*
*/

PROC htmlBrowse( oHtm, cAction, lUseLinks, cTarget, cAlias )

   LOCAL i
   LOCAL n      := 0
   LOCAL aFlds  := Dbstruct()
   LOCAL cAlign

   DEFAULT cAction := "confirm('RECORD: '+this.name+'\nPlace your action here !!!')"
   DEFAULT lUseLinks := .F.

   /*
// browse caption...
oHtm:defineTable( 1, 1, 98 )
oHtm:newTableRow("black")
oHtm:newTableCell(,,,3,"white")
oHtm:Write( htmlSpace( 5 ) +"Browsing Table: <B>"+ALIAS()+"</B>" )
oHtm:endTableCell()
oHtm:endTableRow("black")
oHtm:endTable()
*/

   oHtm:defineTable( Fcount(), 1, 98 )

   oHtm:TableHead( " ? " )
   FOR i := 1 TO Fcount()
      oHtm:TableHead( aFlds[ i, 1 ] )
   NEXT

   WHILE !( Eof() )

      // each row has a different color...
      IF n == 0
         oHtm:newTableRow( "lightyellow" )
         n := 1
      ELSE
         oHtm:newTableRow( "#9196A0" )
         n := 0
      ENDIF

      // put an action pushbutton...
      oHtm:newTableCell( "center" )
      IF lUseLinks
         LINK( cAction ) ;
               TEXT( NTrim( Recno() ) ) ;
               OF oHtm
      ELSE
         PUSH BUTTON ;
            NAME "'B" + NTRIM( Recno() ) + "'" ;
            CAPTION "' ? '" ;
            ONCLICK( cAction ) ;
            OF oHtm
      ENDIF
      oHtm:EndTableCell()

      // --> put the formatted fields data...
      FOR i := 1 TO Len( aFlds )
         cAlign := IF( aFlds[ i, 2 ] == "N", "RIGHT", "CENTER" )
         oHtm:newTableCell( cAlign,,,, "black" )
         oHtm:Write( greek2Html( any2Str( Fieldget( i ) ) ) )
         oHtm:EndTableCell()
      NEXT
      oHtm:endTableRow()
      SKIP
   ENDDO

   oHtm:endTable()

   RETURN
#ifdef MYSQL
PROC htmlBrowseSql( oHtm, cAction, lUseLinks, cTarget, oServer, oQuery )

   LOCAL i
   LOCAL p
   LOCAL n       := 0
   LOCAL oCurRow

   LOCAL cAlign

   DEFAULT cAction := "confirm('RECORD: '+this.name+'\nPlace your action here !!!')"
   DEFAULT lUseLinks := .F.

   /*
// browse caption...
oHtm:defineTable( 1, 1, 98 )
oHtm:newTableRow("black")
oHtm:newTableCell(,,,3,"white")
oHtm:Write( htmlSpace( 5 ) +"Browsing Table: <B>"+ALIAS()+"</B>" )
oHtm:endTableCell()
oHtm:endTableRow("black")
oHtm:endTable()
*/
   oquery := oServer:query( 'Select * from rafael' )

   oHtm:defineTable( oQuery:FCount(), 1, 98 )
   oCurRow := oQuery:getRow( 1 )
   oHtm:TableHead( " ? " )
   FOR i := 1 TO oQuery:FCount()
      oHtm:TableHead( oCurRow:FieldName( i ) )
   NEXT

   FOR p := 1 TO oQuery:LastRec()
      oCurRow := oQuery:getRow( P )
      // each row has a different color...
      IF n == 0
         oHtm:newTableRow( "lightyellow" )
         n := 1
      ELSE
         oHtm:newTableRow( "#9196A0" )
         n := 0
      ENDIF

      // put an action pushbutton...
      oHtm:newTableCell( "center" )
      IF lUseLinks
         LINK( cAction ) ;
               TEXT( NTrim( oQuery:RECNO() ) ) ;
               OF oHtm
      ELSE
         PUSH BUTTON ;
            NAME "'B" + NTRIM( oQuery:RECNO() ) + "'" ;
            CAPTION "' ? '" ;
            ONCLICK( cAction ) ;
            OF oHtm
      ENDIF
      oHtm:EndTableCell()

      // --> put the formatted fields data...

      FOR i := 1 TO oquery:fcount()

         cAlign := IF( oCurRow:FieldType( i ) == "N", "RIGHT", "CENTER" )
         oHtm:newTableCell( cAlign,,,, "black" )
         oHtm:Write( greek2Html( any2Str( oCurRow:FieldGet( i ) ) ) )
         oHtm:EndTableCell()
      END
      oHtm:endTableRow()
      IF !oquery:eof()
         oquery:skip()
      ENDIF

   NEXT
   oHtm:endTable()

   RETURN
#endif

   //*** EOF ***//
