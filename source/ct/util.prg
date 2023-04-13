/*
 *  $Id$
 */

/*
 * xHarbour Project source code:
 * CT lib util functions.
 *
 * Default()
 * IsDir()
 * Occurs()
 *
 * Copyright 2004 Eduardo Fernandes <modalsist@yahoo.com.br>
 * http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modIFy
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
 * along with this software; see the file COPYING.  IF not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, IF you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  IF you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modIFied files, you must delete
 * this exception notice from them.
 *
 * IF you write modIFications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modIFications.
 * IF you do not wish that, delete this exception notice.
 *
 */

//----------------------------------------

FUNCTION DEFAULT( cVarByRef , uDefValue )

//----------------------------------------
/* This function is similar to SET DEFAULT <var> TO <value> command.
* <cVarByRef> Variable passed by reference
* <uDefValue> Default value to assign to the cVarByRef. Can be any data type.
* ex: Default( @AnyVar, "hello")
*     Default( @AnyVar, 10)
*/
   IF cVarByRef == NIL
      cVarByRef := uDefValue
   ENDIF

   RETURN ( Nil )

//---------------------

FUNCTION IsDir( cDir )

//---------------------
/*
* Short function name.
* IsDirectory() is xHarbour rtl function.
* Source is in \source\rtl\file.c and
*              \source\rtl\filehb.c
*/

   RETURN IsDirectory( cDir )

//------------------------

FUNCTION Occurs( c1, c2 )

//------------------------
/*
Return the ammout of times that c1 occurs into c2
*/
   LOCAL nRet, nPos

   IF !HB_ISSTRING( c1 ) .OR. !HB_ISSTRING( c2 )
      RETURN 0
   ENDIF

   IF Len( c1 ) == 0 .OR. Len( c2 ) == 0
      RETURN 0
   ENDIF

   nRet := 0

   WHILE !Empty( c2 )
      nPos := At( c1, c2 )
      IF nPos > 0
         nRet++
         c2 := SubStr( c2, nPos + 1 )
      ELSE
         c2 := ""
      ENDIF
   ENDDO

   RETURN nRet
