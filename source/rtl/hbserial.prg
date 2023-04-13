/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Remote Procedure Call code
 * xHarbour part
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
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
 
#include "error.ch"
#include "common.ch"

FUNCTION hb_Serialize( xValue, lRecursive, aObj, aHash, aArray, aBlock )

   LOCAL cSerial
   LOCAL xElement, aPropertiesAndValues, aPropertyAndValue
   LOCAL nPos
   LOCAL cType := ValType( xValue )
   
   DEFAULT lRecursive TO .F.
   DEFAULT aObj TO {}
   DEFAULT aHash TO {}
   DEFAULT aArray TO {}
   DEFAULT aBlock TO {}

   IF lRecursive
   
      SWITCH cType
      CASE "A"
         IF ( nPos := AScan( aArray, {|a| a == xValue } ) ) > 0
            cType := "R"
            xValue := "RA" + HB_CreateLen8( nPos )
         ENDIF
         EXIT
            
      CASE "H"
         IF ( nPos := AScan( aHash, {|h| h == xValue } ) ) > 0
            cType := "R"
            xValue := "RH" + HB_CreateLen8( nPos )
         ENDIF
         EXIT
            
      CASE "O"
         IF ( nPos := AScan( aObj, {|o| o == xValue } ) ) > 0
            cType := "R"
            xValue := "RO" + HB_CreateLen8( nPos )
         ENDIF
         EXIT

      CASE "B"
         IF ( nPos := AScan( aBlock, {|b| b == xValue } ) ) > 0
            cType := "R"
            xValue := "RB" + HB_CreateLen8( nPos )
         ENDIF
         EXIT
      END
      
   ENDIF

   SWITCH cType
   CASE "A"
      
      AAdd( aArray, xValue )
      
      cSerial := "A" + HB_CreateLen8( Len( xValue ) )

      FOR EACH xElement IN xValue
         cSerial += hb_Serialize( xElement, .T. , aObj, aHash, aArray, aBlock )
      NEXT

      EXIT

   CASE "B"

      AAdd( aBlock, xValue )

      xElement := HB_SaveBlock( xValue )
      cSerial  := "B" + hb_Serialize( xElement )

      EXIT


   CASE "H"
      
      AAdd( aHash, xValue )
         
      cSerial := "H" + HB_CreateLen8( Len( xValue ) )

      FOR nPos := 1 TO Len( xValue )
         cSerial += hb_Serialize( HGetKeyAt( xValue, nPos ) )
         cSerial += hb_Serialize( HGetValueAt( xValue, nPos ), .T. , aObj, aHash, aArray, aBlock )
      NEXT

      EXIT

   CASE "O"
      
      AAdd( aObj, xValue )
         
      IF __objDerivedFrom( xValue, "HBPersistent" )
         cSerial := HB_SerializeSimple( xValue:ClassName )
         cSerial += xValue:SaveToText()
         cSerial := "Q" + HB_CreateLen8( Len( cSerial ) ) + cSerial

      ELSE
         aPropertiesAndValues := __ClsGetPropertiesAndValues( xValue )
         cSerial := "O" + HB_CreateLen8( Len( aPropertiesAndValues ) )
         cSerial += HB_SerializeSimple( xValue:ClassName )

         FOR EACH aPropertyAndValue in aPropertiesAndValues
            // saves name and content
            cSerial += hb_Serialize( aPropertyAndValue[1], .T. )
            cSerial += hb_Serialize( aPropertyAndValue[2], .T. , aObj, aHash, aArray, aBlock )
         NEXT
      end IF
      EXIT
         
   CASE "R"
      cSerial := xValue
      EXIT

      DEFAULT
      cSerial := HB_SerializeSimple( xValue )
   END

   RETURN cSerial

FUNCTION hb_Deserialize( cSerial, nMaxLen, lRecursive, aObj, aHash, aArray, aBlock )

   LOCAL oObject
   LOCAL oElem, cClassName, oVal, lScope
   LOCAL nLen, nClassID
   LOCAL nLenBytes, nClassNameLen, oErr
   
   DEFAULT lRecursive TO .F.
   DEFAULT aObj TO {}
   DEFAULT aHash TO {}
   DEFAULT aArray TO {}
   DEFAULT aBlock TO {}

   IF Len( cSerial ) < 2 // note
      RETURN NIL
   ENDIF

   SWITCH cSerial[1]
   CASE "A"
      nLen := HB_GetLen8( SubStr( cSerial, 2 ) )
      oObject := Array( nLen )
      cSerial := SubStr( cSerial, 10 )
      AAdd( aArray, oObject )
      nLen := HB_DeserializeArray( oObject, cSerial, nMaxLen, aObj, aHash, aArray, aBlock )
      cSerial := SubStr( cSerial, nLen + 1 )
      EXIT

   CASE "B"
      cSerial := SubStr( cSerial, 2 )
      oElem   := hb_Deserialize( cSerial, nMaxLen )
      oObject := HB_RestoreBlock( oElem )
      AAdd( aBlock, oObject )
      EXIT


   CASE "H"
      oObject := Hash()
      nLen := HB_GetLen8( SubStr( cSerial, 2 ) )
      cSerial := SubStr( cSerial, 10 )

      IF nLen > 0
         HAllocate( oObject, nLen )
      ENDIF

      AAdd( aHash, oObject )
      DO WHILE nLen > 0
         oElem := hb_Deserialize( cSerial, nMaxLen )
         cSerial :=  SubStr( cSerial, HB_SerialNext( cSerial ) + 1 )
         oVal := hb_Deserialize( cSerial, nMaxLen, .T. , aObj, aHash, aArray, aBlock )
         HSet( oObject, oElem, oVal )
         cSerial := SubStr( cSerial, HB_SerialNext( cSerial ) + 1 )
         nLen --
      ENDDO
      EXIT

   CASE "O"
      nLen := HB_GetLen8( SubStr( cSerial, 2 ) )
      cSerial := SubStr( cSerial, 10 )
      cClassName := HB_DeserializeSimple( cSerial, 128 )
      IF cClassName == NIL
         RETURN NIL
      ENDIF
      cSerial :=  SubStr( cSerial, HB_SerialNext( cSerial ) + 1 )
      // Let's create a new instance of this class, if possible
      nClassID := __ClsGetHandleFromName( cClassName )
      IF nClassID == 0
         oErr := ErrorNew()
         oErr:Args          := { cSerial, nMaxLen }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .F.
         oErr:Description   := "Cannot find class implementation: '" + cClassName + "'"
         oErr:Operation     := "HB_Deserialize()"
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := 3
         oErr:SubSystem     := "Serialization"
         RETURN Eval( ErrorBlock(), oErr )
      ENDIF
      oObject := __clsInst( nClassId )
      lScope  := __SetClassScope( .F. )

      AAdd( aObj, oObject )
      DO WHILE nLen > 0
         // retreives name
         oElem := HB_DeserializeSimple( cSerial, 128 )
         cSerial := SubStr( cSerial, HB_SerialNext( cSerial ) + 1 )
         // then the value
         oVal := hb_Deserialize( cSerial, nMaxLen, .T. , aObj, aHash, aArray, aBlock )

         __objSendMsgCase( oObject, "_" + oElem, oVal )
         cSerial := SubStr( cSerial, HB_SerialNext( cSerial ) + 1 )

         nLen--
      ENDDO
      __SetClassScope( lScope )
      EXIT

   CASE "Q"                               // Object inherited from HBPersistent
      nLenBytes := HB_GetLen8( SubStr( cSerial, 2 ) )
      cSerial := SubStr( cSerial, 10 )

      cClassName := HB_DeserializeSimple( cSerial, 128 )
      IF cClassName == NIL
         RETURN NIL
      ENDIF
      nClassNameLen := HB_SerialNext( cSerial )
      cSerial :=  SubStr( cSerial, nClassNameLen + 1 )
      nLenBytes := nLenBytes - nClassNameLen
      // Let's create a new instance of this class, if possible
      nClassID := __ClsGetHandleFromName( cClassName )
      IF nClassID == 0
         oErr := ErrorNew()
         oErr:Args          := { cSerial, nMaxLen }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .F.
         oErr:Description   := "Cannot find class implementation: '" + cClassName + "'"
         oErr:Operation     := "HB_Deserialize()"
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := 1
         oErr:SubSystem     := "Serialization"
         RETURN Eval( ErrorBlock(), oErr )
      ENDIF

      oObject := __clsInst( nClassId )
      IF ! __objDerivedFrom( oObject, "HBPersistent" )
         oErr := ErrorNew()
         oErr:Args          := { cSerial, nMaxLen }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .F.
         oErr:Description   := "Class is not derived from HBPersistent: '" + cClassName + "'"
         oErr:Operation     := "HB_Deserialize()"
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := 2
         oErr:SubSystem     := "Serialization"
         RETURN Eval( ErrorBlock(), oErr )
      ENDIF

      oObject:LoadFromText( SubStr( cSerial,1,nLenBytes ) )
      cSerial :=  SubStr( cSerial, nLenBytes + 1 )
      AAdd( aObj, oObject )
      EXIT
      
   CASE "R"
      SWITCH cSerial[2]
      CASE "A"
         oObject := aArray[ HB_GetLen8( Substr( cSerial, 3, 8 ) ) ]
         EXIT
         
      CASE "H"
         oObject := aHash[ HB_GetLen8( Substr( cSerial, 3, 8 ) ) ]
         EXIT
         
      CASE "O"
         oObject := aObj[ HB_GetLen8( Substr( cSerial, 3, 8 ) ) ]
         EXIT

      CASE "B"
         oObject := aBlock[ HB_GetLen8( Substr( cSerial, 3, 8 ) ) ]
         EXIT
         
      END
      EXIT

      DEFAULT
      oObject := HB_DeserializeSimple( cSerial, nMaxLen )
   END

   RETURN oObject

FUNCTION HB_DeserialNext( cSerial, nMaxLen )

   LOCAL oObject, cStr
   LOCAL nPos

   nPos := HB_GetLen8( cSerial )
   IF nPos < 0 .OR. nPos >= Len( cSerial )
      RETURN NIL
   ENDIF

   cStr := SubStr( cSerial, nPos )
   oObject := hb_Deserialize( cStr, nMaxLen )
   IF oObject != NIL
      nPos += HB_SerialNext( cStr )
      HB_CreateLen8( @cSerial, nPos )
   ENDIF

   RETURN oObject
