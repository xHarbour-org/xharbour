/*
 * $Id: txml.prg,v 1.2 2003/06/16 20:13:34 jonnymind Exp $
 */

/*
 * xHarbour Project source code:
 * HBXML - XML DOM oriented routines - Classes encapsulating the document
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *    See also MXML library related copyright in hbxml.c
 *
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

#include "hbxml.ch"
#include "hbclass.ch"

CLASS TXMLNode
   DATA nType
   DATA cName
   DATA aAttributes
   DATA cData

   DATA oNext
   DATA oPrev
   DATA oParent
   DATA oChild

   METHOD New( nType, cName, aAttributes, cData )  CONSTRUCTOR
   METHOD Clone()                      INLINE HBXml_node_clone( Self )
   METHOD CloneTree()                  INLINE HBXml_node_clone_tree( Self )

   METHOD Unlink()                     INLINE HBXml_node_unlink( Self )
   METHOD NextInTree()

   METHOD InsertBefore( oNode )        INLINE HBXml_node_insert_before( oNode, Self )
   METHOD InsertAfter( oNode )         INLINE HBXml_node_insert_after( oNode, Self )
   METHOD InsertBelow( oNode )         INLINE HBXml_node_insert_below( oNode, Self )
   METHOD AddBelow( oNode )            INLINE HBXml_node_add_below( oNode, Self )

   METHOD GetAttribute( cAttrib )
   METHOD SetAttribute( cAttrib, cValue )

   METHOD Depth()
   METHOD Path()

   METHOD ToString( nStyle )        INLINE HBXml_node_to_string( Self, nStyle )
   METHOD Write( fHandle, nStyle )  INLINE HBXml_node_write( Self, fHandle, nStyle )

   //Useful for debugging purposes
   METHOD ToArray()                 INLINE;
             { ::nType, ::cName, ::aAttributes, ::cData }
ENDCLASS

METHOD New( nType, cName, aAttributes, cData ) class TXmlNode
   IF nType == NIL
      ::nType := HBXML_TYPE_TAG
   ELSE
      ::nType := nType
   ENDIF

   IF aAttributes == NIL
      ::aAttributes := {}
   ELSE
      ::aAttributes := aAttributes
   ENDIF

   ::cName := cName
   ::cData := cData
RETURN Self


METHOD NextInTree() CLASS TXmlNode
   LOCAL oNext := NIL, oTemp

   IF ::oChild != NIL
      oNext := ::oChild
   ELSEIF ::oNext != NIL
      oNext := ::oNext
   ELSE
      oTemp := ::oParent
      DO WHILE oTemp != NIL
         IF oTemp:oNext != NIL
            oNext := oTemp:oNext
            EXIT
         ENDIF
         oTemp := oTemp:oParent
      ENDDO
   ENDIF

RETURN oNext


METHOD GetAttribute( cAttrib ) CLASS TXmlNode
   LOCAL aElem

   FOR EACH aElem IN ::aAttributes
      IF aElem[1] == cAttrib
         RETURN aElem[2]
      ENDIF
   NEXT

RETURN NIL


METHOD SetAttribute( cAttrib, cValue ) CLASS TXmlNode
   LOCAL aElem

   cAttrib := CStr( cAttrib )
   cValue := CStr( cValue )

   FOR EACH aElem IN ::aAttributes
      IF aElem[1] == cAttrib
         aElem[2] := cValue
         RETURN NIL
      ENDIF
   NEXT

   AAdd( ::aAttributes, { cAttrib, cValue } )

RETURN NIL


METHOD Depth() CLASS TXmlNode
   IF ::oParent != NIL
      RETURN ::oParent:Depth() + 1
   ENDIF
RETURN 0


METHOD Path() CLASS TXmlNode
   IF ::nType == HBXML_TYPE_DOCUMENT
      RETURN ""
   ENDIF

   IF ::cName != NIL
      IF ::oParent != NIL
         IF ::oParent:Path() != NIL
            RETURN ::oParent:Path() + "/" + ::cName
         ENDIF
      ELSE
         RETURN "/" + ::cName
      ENDIF
   ENDIF
RETURN NIL

/********************************************
   Iterator class
*********************************************/
CLASS TXmlIterator
   METHOD New( oNodeTop, nType ) CONSTRUCTOR
   METHOD Next()
   METHOD Rewind()                  INLINE   ::oNode := ::oTop
   METHOD Find( cName, cAttribute, cValue, cData )

   METHOD GetNode()                 INLINE   ::oNode

HIDDEN:
   DATA nType
   DATA cName
   DATA cAttribute
   DATA cValue
   DATA cData

   DATA oNode
   DATA oTop
ENDCLASS

METHOD New( oNodeTop, nType ) CLASS TXmlIterator
   ::nType := IIF( nType == NIL, HBXML_ITERATOR_ALL, nType )
   ::oTop  := oNodeTop
   ::oNode := oNodeTop
RETURN Self

METHOD Find( cName, cAttribute, cValue, cData ) CLASS TXmlIterator
   ::cName := cName
   ::cAttribute := cAttribute
   ::cValue := cValue
   ::cData := cData
RETURN ::Next()

METHOD Next() CLASS TXmlIterator
   LOCAL oFound := ::oNode:NextInTree()
   LOCAL bFound := .F.

   SWITCH ::nType
      CASE HBXML_ITERATOR_ALL
         IF oFound != NIL
            bFound := .T.
         ENDIF
      EXIT

      CASE HBXML_ITERATOR_SCAN
         DO WHILE oFound != NIL .and. .not. bFound
            IF ::cName != NIL
               IF oFound:cName == NIL .or. ::cName != oFound:cName
                  oFound := oFound:NextInTree()
                  LOOP
               ENDIF
            ENDIF

            IF ::cAttribute != NIL
               IF AScan( oFound:aAttributes, {|elem| ::cAttribute == elem[1] }) == 0
                  oFound := oFound:NextInTree()
                  LOOP
               ENDIF
            ENDIF

            IF ::cValue != NIL
               IF AScan( oFound:aAttributes, {|elem| ::cValue == elem[2]}) == 0
                  oFound := oFound:NextInTree()
                  LOOP
               ENDIF
            ENDIF

            IF ::cData != NIL
               IF oFound:cData == NIL .or. ::cData != oFound:cData
                  oFound := oFound:NextInTree()
                  LOOP
               ENDIF
            ENDIF

            // if it arrives here, this means a match!
            bFound := .T.
         ENDDO

      EXIT

      CASE HBXML_ITERATOR_REGEX
         DO WHILE oFound != NIL .and. .not. bFound
            IF ::cName != NIL
               IF oFound:cName == NIL .or. .not. oFound:cName LIKE ::cName
                  oFound := oFound:NextInTree()
                  LOOP
               ENDIF
            ENDIF

            IF ::cAttribute != NIL
               IF AScan( oFound:aAttributes, {|elem| elem[1] LIKE ::cAttribute } ) == 0
                  oFound := oFound:NextInTree()
                  LOOP
               ENDIF
            ENDIF

            IF ::cValue != NIL
               IF AScan( oFound:aAttributes, {|elem| elem[2] LIKE ::cValue } ) == 0
                  oFound := oFound:NextInTree()
                  LOOP
               ENDIF
            ENDIF

            IF ::cData != NIL
               IF oFound:cData == NIL .or. .not. oFound:cData HAS ::cData
                  oFound := oFound:NextInTree()
                  LOOP
               ENDIF
            ENDIF

            // if it arrives here, this means a match!
            bFound := .T.
         ENDDO

         CASE HBXML_ITERATOR_PATH
            //TODO: Implement this
         EXIT

      EXIT
   END

   // move pointer only if found
   IF bFound
      ::oNode := oFound
   ENDIF

RETURN oFound


/********************************************
   Document Class
*********************************************/

CLASS TXmlDocument
   DATA oRoot
   DATA nStatus
   DATA nError
   DATA nLine
   DATA nNodeCount

   METHOD New( xElem )                CONSTRUCTOR
   METHOD Read( xData )               INLINE HBXML_DATAREAD( Self, xData )
   METHOD ToString( nStyle )          INLINE ::oRoot:ToString( nStyle )
   METHOD Write( fHandle, nStyle )    INLINE ::oRoot:Write( fHandle, nStyle )

   METHOD FindFirst( cName, cAttrib, cValue, cData )
   METHOD FindFirstRegex( cName, cAttrib, cValue, cData )
   METHOD FindNext()                   INLINE ::oIterator:Next()

HIDDEN:
   DATA oIterator

ENDCLASS

METHOD New( xElem ) CLASS TXmlDocument
   ::nStatus := HBXML_STATUS_OK
   ::nError := HBXML_ERROR_NONE
   ::nLine := 1
   ::nNodeCount := 0

   IF xElem == NIL
      ::oRoot := TXmlNode():New( HBXML_TYPE_DOCUMENT )
   ELSE
      SWITCH ValType( xElem )
         CASE 'O'
            ::oRoot = xElem
         EXIT

         CASE 'N'
         CASE 'C'
            ::oRoot := TXmlNode():New( HBXML_TYPE_DOCUMENT )
            ::Read( xElem )
      END
   ENDIF

RETURN Self

METHOD FindFirst( cName, cAttrib, cValue, cData ) CLASS TXmlDocument
   ::oIterator := TXmlIterator():New( ::oRoot, HBXML_ITERATOR_SCAN )
RETURN ::oIterator:Find( cName, cAttrib, cValue, cData )

METHOD FindFirstRegex( cName, cAttrib, cValue, cData ) CLASS TXmlDocument
   ::oIterator := TXmlIterator():New( ::oRoot, HBXML_ITERATOR_REGEX )
RETURN ::oIterator:Find( cName, cAttrib, cValue, cData )
