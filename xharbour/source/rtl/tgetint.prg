/*
 * $Id: tgetint.prg,v 1.3 2002/10/19 16:30:46 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Get Class
 *
 * Copyright 1999 Ignacio Ortiz de Z£niga <ignacio@fivetech.com>
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 RonPinkas <Ron@Profit-Master.com>
 *    __GET()
 *    __GETA()
 *
 */

#include "hbclass.ch"
#include "hbsetup.ch"

//---------------------------------------------------------------------------//

FUNCTION GetNew( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )
   RETURN Get():New( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )

//---------------------------------------------------------------------------//

FUNCTION __GET( bSetGet, cVarName, cPicture, bValid, bWhen )

   LOCAL oGet

   IF bSetGet == NIL
      IF Left( cVarName, 3 ) == "M->"
         cVarName := SubStr( cVarName, 4 )
         bSetGet := {|_1| IIF( _1 == NIL,  __MVGET( cVarName ), __MVPUT( cVarName, _1 ) ) }
      ELSEIF FieldPos( cVarName ) > 0
         // "{|_1| IIF( _1 == NIL, FIELD->&cVarName, FIELD->&cVarName := _1 )"
         bSetGet := &( "{|_1| IIF( _1 == NIL, FIELD->" + cVarName + ", FIELD->" + cVarName + " := _1 ) }" )
      ELSEIF __MVEXIST( cVarName )
         // "{|_1| IIF( _1 == NIL, M->&cVarName, M->&cVarName := _1 )"
         bSetGet := {|_1| iif( _1 == NIL,  __MVGET( cVarName ), __MVPUT( cVarName, _1 ) ) }
      ELSE
         bSetGet := &( "{|_1| IIF( _1 == NIL, " + cVarName + ", " + cVarName + " := _1 ) }" )
      ENDIF
   ENDIF

   oGet := Get():New( , ,bSetGet, cVarName, cPicture )

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

   RETURN oGet

FUNCTION __GETA( bGetArray, cVarName, cPicture, bValid, bWhen, aIndex )

   LOCAL oGet
   LOCAL nDim := Len( aIndex )
   LOCAL aGetVar
   LOCAL nCounter

   IF bGetArray == NIL
      IF Left( cVarName, 3 ) == "M->"
         cVarName := SubStr( cVarName, 4 )
         bGetArray := {|| __MVGET( cVarName ) }
      ELSEIF FieldPos( cVarName ) > 0
         // "{|| FIELD->&cVarName )"
         bGetArray := &( "{|| FIELD->" + cVarName + "}" )
      ELSEIF __MVEXIST( cVarName )
         // "{|| M->&cVarName )"
         bGetArray := {|| __MVGET( cVarName ) }
      ELSE
         // "{|| &cVarName )"
         bGetArray := &( "{|| " + cVarName + "}" )
      ENDIF
   ENDIF

   oGet := Get():New(,, bGetArray, cVarName, cPicture )
   oGet:SubScript := aIndex

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

   RETURN oGet
