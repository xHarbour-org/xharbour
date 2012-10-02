/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Scalar Classes
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

#include "hbclass.ch"

//----------------------------------------------------------------------------//

CLASS ScalarObject

   METHOD  COPY
   MESSAGE DeepCopy     METHOD COPY

   METHOD  IsScalar     INLINE ( Self ), .T.

   METHOD  asString
   METHOD  asExpStr

ENDCLASS

//----------------------------------------------------------------------------//

METHOD COPY       CLASS ScalarObject

   RETURN Self

//----------------------------------------------------------------------------//

METHOD asString   CLASS ScalarObject

   SWITCH ValType( Self )

   CASE 'A' ; RETURN '{ ... }'
   CASE 'H' ; RETURN '{ => }'
   CASE 'B' ; RETURN '{ || ... }'
   CASE 'C' ; RETURN Self
   CASE 'D' ; RETURN DToC( Self )
   CASE 'L' ; RETURN IF( Self, '.T.', '.F.' )
   CASE 'N' ; RETURN LTrim( Str( Self ) )
   CASE 'U' ; RETURN 'Nil'

   END

   RETURN 'Errror!!'

//----------------------------------------------------------------------------//

METHOD asExpStr   CLASS ScalarObject

   LOCAL cTemp

   SWITCH ValType( Self )

   CASE 'A' ; RETURN '{ ... }'
   CASE 'H' ; RETURN '{ => }'
   CASE 'C' ; RETURN '"' + Self + '"'
   CASE 'D'
      cTemp := DToS( Self )
      RETURN '{^' + SubStr( cTemp, 1 , 4 ) + '/' + ;
         SubStr( cTemp, 5, 2 ) + '/' + ;
         SubStr( cTemp, 7, 2 ) + '}'
   END

   RETURN ::asString

//----------------------------------------------------------------------------//

CLASS Array FROM ScalarObject FUNCTION _Array

   MESSAGE Add             METHOD APPEND
   METHOD  AddAll
   METHOD  APPEND
   METHOD  asString        INLINE ( Self ), ValtoPrg( HB_QSelf() )
   METHOD  At( n )         INLINE Self[ n ]
   METHOD  AtIndex( n )    INLINE Self[ n ]
   METHOD  AtPut( n, x )   INLINE Self[ n ] := x
   METHOD  Collect
   METHOD  COPY            INLINE ACopy( Self, Array( Len( Self ) ) )
   METHOD  DeleteAt
   METHOD  DO
   METHOD  IndexOf
   METHOD  Init( nLen )    INLINE ::Size := iif( nLen == NIL, 0, nLen ), Self
   METHOD  InsertAt
   METHOD  Remove
   METHOD  Scan( bScan )   INLINE AScan( Self, bScan )
   METHOD  _Size( nLen )   INLINE ASize( Self, nLen ), nLen

ENDCLASS

//----------------------------------------------------------------------------//

METHOD AddAll( otherCollection ) CLASS Array

   otherCollection:Do( {|x| ::Add( x ) } )

   RETURN Self

//----------------------------------------------------------------------------//

METHOD APPEND( x )   CLASS Array

   AAdd( Self, x )

   RETURN Self

//----------------------------------------------------------------------------//

METHOD Collect( bCollect ) CLASS Array

   LOCAL xElement, aResult[0]

   FOR EACH xElement IN Self
      IF Eval( bCollect, UnRef( xElement ) )
         AAdd( aResult, UnRef( xElement ) )
      END
   NEXT

   RETURN aResult

//----------------------------------------------------------------------------//

METHOD deleteAt( nPos ) CLASS Array

   IF nPos > 0 .AND. nPos <= Len( Self )
      ADel( Self, nPos, .T. )
   ENDIF

   RETURN Self

//----------------------------------------------------------------------------//

METHOD DO( bEval ) CLASS Array

   LOCAL xElement

   FOR EACH xElement IN Self
      bEval:Eval( UnRef( xElement ), HB_EnumIndex() )
   NEXT

   RETURN Self

//----------------------------------------------------------------------------//

METHOD IndexOf( xValue ) CLASS Array

   LOCAL xElement, cType := ValType( xValue )

   FOR EACH xElement IN Self
      IF ValType( xElement ) == cType .AND. xElement == xValue
         RETURN HB_EnumIndex()
      END
   NEXT

   RETURN 0

//----------------------------------------------------------------------------//

METHOD InsertAt( nPos, xValue ) CLASS Array

   IF nPos > Len( self )
      ASize( Self, nPos )
      Self[ nPos ] := xValue
   ELSEIF nPos > 0
      AIns( Self, nPos, xValue, .T. )
   ENDIF

   RETURN Self

//----------------------------------------------------------------------------//

METHOD Remove( xValue ) CLASS Array

   ::DeleteAt( ::IndexOf( xValue ) )

   RETURN Self

//----------------------------------------------------------------------------//

CLASS HASH FROM SCALAROBJECT FUNCTION _HASH

   METHOD Add( xKey, xValue )      INLINE Self[ xKey ] := xValue, Self
   METHOD AddAll( oCollection )
   METHOD AtIndex( nPos )          INLINE HGetValueAt( Self, nPos )
   METHOD AtPut( nPos, xValue )    INLINE HSetValueAt( Self, nPos, xValue )
   METHOD APPEND( xKey, xValue )   INLINE Self[ xKey ] := xValue, Self
   METHOD AsString()               INLINE ( Self ), ValToPrg( HB_QSelf() )
   METHOD Collect( bCollect )
   METHOD COPY()                   INLINE hCopy( Self, Hash() )
   METHOD DeleteAt( nPos )         INLINE hDelat( Self, nPos )
   METHOD DO( bBlock )
   METHOD IndexOf( xValue )        INLINE hScan( Self, xValue )
   METHOD Init( nLen )             INLINE ::Size := iif( nLen == NIL, 0, nLen ), Self
   METHOD Remove( xValue )         INLINE hDel( Self, xValue )
   METHOD Scan( bScan )            INLINE hScan( Self, bScan )
   METHOD _Size( nLen )

ENDCLASS

//----------------------------------------------------------------------------//

METHOD AddAll( oCollection ) CLASS HASH

   oCollection:Do( { |xKey, xValue| Self[ xKey ] := xValue } )

   RETURN Self

//----------------------------------------------------------------------------//

METHOD Collect( bCollect ) CLASS HASH

   LOCAL xElement, aResult[0]

   FOR EACH xElement IN Self:Values
      IF Eval( bCollect, UnRef( xElement ) )
         AAdd( aResult, UnRef( xElement ) )
      END
   NEXT

   RETURN aResult

//----------------------------------------------------------------------------//

METHOD DO( bDo ) CLASS HASH

   LOCAL xKey

   FOR EACH xKey IN Self:Keys
      Eval( bDo, xKey, Self[ xKey ] )
   NEXT

   RETURN Self

//----------------------------------------------------------------------------//

METHOD _Size( nLen ) CLASS HASH

   LOCAL nOldLen := Len( Self ), Counter

   IF nLen == nOldLen
      RETURN nLen
   ELSEIF nLen > nOldLen
      hAllocate( Self, nLen )

      FOR Counter := nOldLen + 1 TO nLen
         Self[ "_SIZED_" + LTrim( Str( Counter ) ) ] := NIL
      NEXT
   ELSE
      FOR Counter := nOldLen TO nLen + 1
         hDelAt( Self, nLen + 1 )
      NEXT
   ENDIF

   RETURN nLen

//----------------------------------------------------------------------------//

CLASS BLOCK FROM SCALAROBJECT FUNCTION _BLOCK

   METHOD AsString() INLINE ( Self ), "{||...}"
//METHOD Exec  // Implemented at hbvm.c

ENDCLASS

//----------------------------------------------------------------------------//

CLASS CHARACTER FROM SCALAROBJECT FUNCTION _CHARACTER

   METHOD AsString INLINE ( Self ), HB_QSelf()

ENDCLASS

//----------------------------------------------------------------------------//

CLASS DATE FROM SCALAROBJECT FUNCTION _DATE

   METHOD AsString INLINE ( Self ), DToC( HB_QSelf() )

ENDCLASS

//----------------------------------------------------------------------------//

CLASS LOGICAL FROM SCALAROBJECT FUNCTION _LOGICAL

   METHOD AsString INLINE ( Self ), iif( HB_QSelf(), ".T.", ".F." )

ENDCLASS

//----------------------------------------------------------------------------//

CLASS NIL FROM SCALAROBJECT FUNCTION _NIL

   METHOD AsString INLINE ( Self ), "NIL"

ENDCLASS

//----------------------------------------------------------------------------//

CLASS NUMERIC FROM SCALAROBJECT FUNCTION _NUMERIC

   METHOD AsString INLINE ( Self ), LTrim( Str( ( HB_QSelf() ) ) )

ENDCLASS

//----------------------------------------------------------------------------//

CLASS POINTER FROM SCALAROBJECT FUNCTION _POINTER

   METHOD AsString INLINE ( Self ), "0x" + NumToHex( HB_QSelf() )
   METHOD FuncName

ENDCLASS

//----------------------------------------------------------------------------//

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbstack.h"
#include "hbvm.h"

//be careful maybe a pointer it's not (PHB_SYMB)

HB_FUNC_STATIC( POINTER_FUNCNAME )
{
   hb_retc( ((PHB_SYMB)hb_stackSelfItem()->item.asPointer.value)->szName );
}

#pragma ENDDUMP

//----------------------------------------------------------------------------//

FUNCTION UnRef( xValue )

   RETURN xValue

//----------------------------------------------------------------------------//

