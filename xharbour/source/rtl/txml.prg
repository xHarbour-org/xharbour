/*
 * $Id: hbxml.c,v 1.1 2003/06/15 20:27:01 jonnymind Exp $
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

   METHOD New( nType, cName, aAttributes, cData )
   METHOD Clone()                      INLINE HBXml_node_clone( ::Self )
   METHOD CloneTree()                  INLINE HBXml_node_clone_tree( ::Self )

   METHOD Unlink()                     INLINE HBXml_node_unlink( ::Self )
   METHOD NextInTree()

   METHOD InsertBefore( oNode )        INLINE HBXml_node_insert_before( oNode, ::Self )
   METHOD InsertAfter( oNode )         INLINE HBXml_node_insert_after( oNode, ::Self )
   METHOD InsertBelow( oNode )         INLINE HBXml_node_insert_below( oNode, ::Self )
   METHOD AddBelow( oNode )            INLINE HBXml_node_add_below( oNode, ::Self )
   
   METHOD GetAttribute( cAttrib )
   METHOD SetAttribute( cAttrib, cValue )
   
   METHOD Depth()
   METHOD Path()

   METHOD ToString( nStyle )        INLINE HBXml_node_to_string( ::Self, nStyle )
   METHOD Write( fHandle, nStyle )  INLINE HBXml_node_write( ::Self, fHandle, nStyle )

ENDCLASS


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
            BREAK
         ENDIF
         oTemp := oTemp:oParent
      ENDDO
   ENDIF
   
RETURN oNext


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

   IF ValType( cAttrib ) != cValue
      cValue := CStr( cValue )
   ENDIF

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
   IF ::cName != NIL
      IF ::oParent != NIL
         RETURN ::oParent:Path() + "/" + ::cName
      ELSE
         RETURN "/" + ::cName
      ENDIF
   ENDIF
RETURN NIL 

/********************************************
   Document Class
*********************************************/

CLASS TXmlDocument
   DATA oRoot
   DATA nStatus
   DATA nError
   DATA nLine
   DATA nNodeCount

   METHOD New( oNode )
   METHOD ToString( nStyle )          INLINE ::oRoot:ToString( nStyle )
   METHOD Write( fHandle, nStyle )    INLINE ::oRoot:Write( fHandle, nStyle )
ENDCLASS

METHOD New( oNode ) CLASS TXmlDocument
   IF oNode == NIL
      ::oRoot := TXmlNode():New( HBXML_TYPE_DOCUMENT )
   ELSE
      ::oRoot = oNode
   ENDIF
   
   ::nStatus := HBXML_STATUS_OK
   ::nError := HBXML_ERROR_NONE
   ::nLine := 1
   ::nNodeCount := 0

RETURN Self

