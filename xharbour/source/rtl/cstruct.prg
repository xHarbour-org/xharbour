/*
 * $Id: cstruct.prg,v 1.0 2002/05/23 20:12:05 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 * C Structure Support.
 *
 * Copyright 2000 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.xharbour.org
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

#include "hboo.ch"

static aActiveStructure

Function HB_CStructure( aStructure )

   LOCAL nLen := Len( aStructure ) - 1
   LOCAL hClass
   LOCAL oStructure
   LOCAL Counter

   hClass := __clsNew( aStructure[1], 1 + nLen )

   __clsAddMsg( hClass,  "Value", @Value(), HB_OO_MSG_METHOD )
   __clsAddMsg( hClass,  "FromBuffer", @DeValue(), HB_OO_MSG_METHOD )

   aDel( aStructure, 1 )

   FOR Counter := 1 TO nLen
      __clsAddMsg( hClass, aStructure[Counter][1], Counter, HB_OO_MSG_DATA )
      __clsAddMsg( hClass, "_" + aStructure[Counter][1], Counter, HB_OO_MSG_DATA )
   NEXT

   __clsAddMsg( hClass,  "aCTypes", Counter, HB_OO_MSG_DATA )
   __clsAddMsg( hClass, "_aCTypes", Counter, HB_OO_MSG_DATA )

   oStructure := __clsInst( hClass )

   oStructure:aCTypes := {}
   FOR Counter := 1 TO nLen
      aAdd( oStructure:aCTypes, aStructure[Counter][2] )
   NEXT

Return oStructure

STATIC Function Value()

   LOCAL aValues := {}
   LOCAL nLen := Len( QSelf() ) - 1

   aScan( QSelf(), {|xVal| aAdd( aValues, xVal ) }, 1, nLen )

Return ArrayToStructure( aValues, QSelf():aCTypes )

STATIC Function DeValue( sBuffer )

   LOCAL aValues := StructureToArray( sBuffer, QSelf():aCTypes )
   LOCAL Counter, nLen := Len( QSelf() ) - 1

   FOR Counter := 1 TO nLen
      QSelf()[Counter] := aValues[Counter]
   NEXT

Return aValues

Function __ActiveStructure( aStructure )

   IF PCount() == 1
      aActiveStructure := aStructure
   ENDIF

Return aActiveStructure
