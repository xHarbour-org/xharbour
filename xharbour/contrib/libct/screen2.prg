/*
 * $Id: screen2.prg,v 1.3 2004/06/04 09:40:29 likewolf Exp $
 */

/*
 * Harbour Project source code:
 *   CT3 video functions (screen-like functions):
 *
 * SCREENMIX()
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>:
 * www - http://www.harbour-project.org
 *
 * SAYSCREEN()
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
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

#include "color.ch"
#include "common.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      SCREENMIX()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      SCREENMIX (<cCharString>, <cAttributeString>, [<nRow>], [<nCol>]) -> <cEmptyString>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen2.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */

FUNCTION SCREENMIX( c, a, row, col )

   DEFAULT row TO Row()
   DEFAULT col TO Col()

   RestScreen( row, col, row, col + Len( a ) - 1, CHARMIX( c, a ) )

   RETURN ""


/*  $DOC$
 *  $FUNCNAME$
 *      SAYSCREEN()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      SAYSCREEN( <cString>, [<nRow>], [<nCol>] ) -> <cEmptyString>
 *  $ARGUMENTS$
 *      <cString> - the string to output. Although undocumented, can be NIL.
 *      <nRow> - row number, defaults to cursor row.
 *      <nCol> - column number, defaults to cursor column.
 *  $RETURNS$
 *      Returns an empty string.
 *  $DESCRIPTION$
 *      Outputs a string at specified coordinates without changing character
 *      attributes.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen2.prg, library is libct.
 *  $SEEALSO$
 *      SCREENMIX()
 *  $END$
 */

FUNCTION SAYSCREEN( cStr, nRow, nCol )
   LOCAL cBuf
   LOCAL nRight
   LOCAL i
   
   DEFAULT cStr TO ""
   DEFAULT nRow TO Row()
   DEFAULT nCol TO Col()
   
   IF Len( cStr ) > 0
     nRight := Min( nCol + Len( cStr ) - 1, MaxCol() )
     cBuf := SaveScreen( nRow, nCol, nRow, nRight )
     FOR i := 1 TO nRight - nCol + 1
       cBuf[ i * 2 - 1 ] := cStr[ i ]
     NEXT
     RestScreen( nRow, nCol, nRow, nRight, cBuf )
   ENDIF
RETURN ""
