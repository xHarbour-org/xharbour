/*
 * $Id: hbserial.prg,v 1.4 2003/04/13 12:35:17 jonnymind Exp $
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


FUNCTION HB_Serialize( oObject )
   LOCAL cSerial
   LOCAL oElem, aProperties

   SWITCH ValType( oObject )
      CASE "A"
         cSerial :="A" + HB_CreateLen8( Len( oObject ) )
         FOR EACH oElem IN oObject
            cSerial += HB_Serialize( oElem )
         NEXT
      EXIT

      CASE "O"
         aProperties := __ClassSelPersistent( __ClassH( oObject ) )
         cSerial := "O" + HB_CreateLen8( Len( aProperties ) )
         cSerial += HB_SerializeSimple( oObject:ClassName )

         FOR EACH oElem in aProperties
            cSerial += HB_Serialize( __objSendMsg( oObject, oElem ) )
         NEXT

      EXIT

      DEFAULT
         cSerial := HB_SerializeSimple( oObject )
   END
RETURN cSerial

FUNCTION HB_Deserialize( cSerial, nMaxLen )
   LOCAL oObject
   LOCAL oElem, cClassName, aProperties, oVal
   LOCAL nLen, nClassID

   IF Len( cSerial ) < 8
      RETURN NIL
   ENDIF

   SWITCH cSerial[1]
      CASE "A"
         oObject := {}
         cSerial := Substr( cSerial, 2 )
         nLen := HB_GetLen8( cSerial )
         cSerial := Substr( cSerial, 9 )
         DO WHILE nLen > 0
            oElem := HB_Deserialize( cSerial, nMaxLen )
            Aadd( oObject, oElem )
            cSerial := Substr( cSerial, HB_SerialNext( cSerial )+1 )
            nLen --
         ENDDO
      EXIT

      CASE "O"
         cSerial := Substr( cSerial, 2 )
         nLen := HB_GetLen8( cSerial )
         cSerial := Substr( cSerial, 9 )
         cClassName := HB_DeserializeSimple( cSerial, 128 )
         IF cClassName == NIL
            RETURN NIL
         ENDIF
         cSerial :=  Substr( cSerial, HB_SerialNext( cSerial )+1 )
         // Let's create a new instance of this class, if possible
         nClassID := __ClsGetHandleFromName( cClassName )
         IF nClassID == 0
            // TODO: Rise an error
            RETURN NIL
         ENDIF
         oObject := __ClsInst( nClassId )
         aProperties := __ClassSelPersistent( nClassId )

         IF Len( aProperties ) != nLen
            // todo: RISE AN ERROR
            RETURN NIL
         ENDIF

         FOR EACH oElem in aProperties
            oVal := HB_Deserialize( cSerial, nMaxLen )
            __objSendMsg( oObject, "_" + oElem, oVal )
            cSerial := Substr( cSerial, HB_SerialNext( cSerial )+1 )
         NEXT
         ? "cSerial :", cSerial
      EXIT

      DEFAULT
         oObject := HB_DeserializeSimple( cSerial, nMaxLen )
   END

RETURN oObject


FUNCTION HB_DeserialNext( cSerial, nMaxLen )
   LOCAL oObject, cStr
   LOCAL nPos

   nPos := HB_GetLen8( cSerial )
   IF nPos < 0 .or. nPos >= Len( cSerial )
      RETURN NIL
   ENDIF

   cStr := Substr(cSerial, nPos)
   oObject := HB_Deserialize( cStr, nMaxLen )
   IF oObject != NIL
      nPos += HB_SerialNext( cStr )
      HB_CreateLen8( cSerial, nPos )
   ENDIF

RETURN oObject
