/*
 * $Id: txml.prg,v 1.19 2009/11/05 00:15:26 modalsist Exp $
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
#include "fileio.ch"

/********************************************
   XML Node  class
*********************************************/
CLASS TXMLNode
   DATA nType
   DATA cName
   DATA aAttributes
   DATA nBeginLine
   DATA cData

   DATA oNext
   DATA oPrev
   DATA oParent
   DATA oChild

   METHOD New( nType, cName, aAttributes, cData ) CONSTRUCTOR

   METHOD Clone()                      INLINE HBXml_node_clone( Self )
   METHOD CloneTree()                  INLINE HBXml_node_clone_tree( Self )

   METHOD Unlink()                     INLINE HBXml_node_unlink( Self )
   METHOD NextInTree()

   METHOD InsertBefore( oNode )        INLINE HBXml_node_insert_before( Self, oNode )
   METHOD InsertAfter( oNode )         INLINE HBXml_node_insert_after( Self, oNode )
   METHOD InsertBelow( oNode )         INLINE HBXml_node_insert_below( Self, oNode )
   METHOD AddBelow( oNode )            INLINE HBXml_node_add_below( Self, oNode )

   METHOD GetAttribute( xAttrib )
   METHOD SetAttribute( cAttrib, xValue )

   METHOD Depth()
   METHOD Path()

   METHOD ToString( nStyle )            INLINE HBXml_node_to_string( Self, nStyle )
   METHOD Write( fHandle, nStyle )      INLINE HBXml_node_write( Self, fHandle, nStyle )

   //Useful for debugging purposes
   METHOD ToArray()                     INLINE { ::nType, ::cName, ::aAttributes, ::cData }

   METHOD AttribCount()                INLINE Len(::aAttributes)
   METHOD GetValues()                   INLINE HGetValues(::aAttributes)
   METHOD GetKeys()                     INLINE HGetKeys(::aAttributes)
   METHOD GetPair(nPos)                 INLINE HGetPairAt(::aAttributes,nPos)

ENDCLASS

*-----------------------------------------------------------------------------*
METHOD New( nType, cName, aAttributes, cData ) class TXmlNode
*-----------------------------------------------------------------------------*

   IF nType == NIL
      ::nType := HBXML_TYPE_TAG
   ELSEIF HB_isNumeric( nType)
      ::nType := nType
   ENDIF

   IF HB_isHash( aAttributes )
      ::aAttributes := aAttributes
   ELSE
      ::aAttributes := {=>}
   ENDIF

   IF HB_isString( cName )
      ::cName := cName
   ENDIF

   IF HB_isString( cData )
     ::cData := cData
   ENDIF

RETURN Self

*-----------------------------------------------------------------------------*
METHOD GetAttribute( xAttrib ) CLASS TXmlNode
*-----------------------------------------------------------------------------*
Local xRet

if ! empty( xAttrib )

    if HB_IsString( xAttrib )  // attribute name (key name)
       xRet := iif( HHasKey( ::aAttributes, xAttrib), HGet(::aAttributes, xAttrib ), NIL )
    elseif HB_isNumeric( xAttrib ) // attribute position (key ordinal position)
       xRet := iif( Len( ::aAttributes ) >= xAttrib, HGetValueAt( ::aAttributes, xAttrib ), NIL )
    endif

endif

RETURN xRet

*-----------------------------------------------------------------------------*
METHOD SetAttribute( xAttrib, xValue )  CLASS TXmlNode
*-----------------------------------------------------------------------------*

if ! empty( xAttrib )

   if HB_isString( xAttrib )  // attribute name (key name)

      ::aAttributes[ xAttrib ] := xValue

   elseif HB_isNumeric( xAttrib ) // attribute position (key ordinal position)

      if Len( ::aAttributes ) >= xAttrib
         HSetValueAt( ::aAttributes, xAttrib, xValue )
      endif

   endif

endif

RETURN xValue

*-----------------------------------------------------------------------------*
METHOD NextInTree() CLASS TXmlNode
*-----------------------------------------------------------------------------*
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

*-----------------------------------------------------------------------------*
METHOD Depth() CLASS TXmlNode
*-----------------------------------------------------------------------------*
   IF ::oParent != NIL
      RETURN ::oParent:Depth() + 1
   ENDIF
RETURN 0

*-----------------------------------------------------------------------------*
METHOD Path() CLASS TXmlNode
*-----------------------------------------------------------------------------*
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
   METHOD New( oNodeTop )           CONSTRUCTOR

   METHOD Next()
   METHOD Rewind()                  INLINE   ::oNode := ::oTop
   METHOD GetNode()                 INLINE   ::oNode
   METHOD SetContext()
   METHOD Clone()

   METHOD Find( cName, cAttribute, cValue, cData )
   METHOD FindNext()

   DATA   lRegex INIT .F.           // to find regular expression.

HIDDEN:
   /* Values of last search criteria */
   DATA cName
   DATA cAttribute
   DATA cValue
   DATA cData

   DATA nTopLevel

   DATA oNode
   DATA oTop

   METHOD MatchCriteria( oNode )

ENDCLASS

*-----------------------------------------------------------------------------*
METHOD New( oNodeTop ) CLASS TXmlIterator
*-----------------------------------------------------------------------------*
   ::oTop  := oNodeTop
   ::oNode := oNodeTop
   ::nTopLevel := oNodeTop:Depth()
RETURN Self


*-----------------------------------------------------------------------------*
METHOD Clone() CLASS TXmlIterator
*-----------------------------------------------------------------------------*
   LOCAL oRet

   oRet := TXmlIterator():New( ::oNodeTop )
   oRet:cName := ::cName
   oRet:cAttribute := ::cAttribute
   oRet:cValue := ::cValue
   oRet:cData := ::cData
RETURN oRet

*-----------------------------------------------------------------------------*
METHOD SetContext() CLASS TXmlIterator
*-----------------------------------------------------------------------------*
   ::oTop := ::oNode
RETURN Self

*-----------------------------------------------------------------------------*
METHOD Next() CLASS TXmlIterator
*-----------------------------------------------------------------------------*
   LOCAL oNext := ::oNode:NextInTree()

   if oNext != NIL .and. oNext:Depth() <= ::nTopLevel
      RETURN NIL
   endif

   ::oNode := oNext

RETURN oNext

*-----------------------------------------------------------------------------*
METHOD Find( cName, cAttribute, cValue, cData ) CLASS TXmlIterator
*-----------------------------------------------------------------------------*

   ::cName      := cName
   ::cAttribute := cAttribute
   ::cValue     := cValue
   ::cData      := cData

   ::Rewind()

   IF ::oNode:nType == HBXML_TYPE_DOCUMENT
      IF ::oNode:oChild == NIL
         RETURN NIL
      ENDIF
      ::oNode := ::oNode:oChild
   ENDIF

   if ! ::MatchCriteria( ::oNode )
      ::FindNext()
   endif

RETURN ::oNode

*-----------------------------------------------------------------------------*
METHOD FindNext() CLASS TXmlIterator
*-----------------------------------------------------------------------------*

   IF ::cName = NIL .and. ::cAttribute = NIL .and. ::cValue = NIL .and. ::cData = NIL
      RETURN NIL
   ENDIF

   ::oNode := ::oNode:NextInTree()

   WHILE ::oNode != NIL

      IF ::oNode:Depth() <= ::nTopLevel
         RETURN NIL
      ENDIF

      IF ::MatchCriteria( ::oNode )
         exit
      ENDIF

      ::oNode := ::oNode:NextInTree()

   ENDDO

RETURN ::oNode

*-----------------------------------------------------------------------------*
METHOD MatchCriteria( oNode ) CLASS TXmlIterator
*-----------------------------------------------------------------------------*
Local lFound := .f.


   if ::cName != NIL .and. oNode:cName != NIL
      if ::lRegex
         lFound := ( oNode:cName LIKE ::cName )
      else
         lFound := ( oNode:cName == ::cName  )
      endif
   endif

   if ::cAttribute != NIL .and. ! empty( oNode:aAttributes )
      if ::lRegex
         lFound := ( HScan( oNode:aAttributes, {|cKey| cKey LIKE ::cAttribute } ) > 0 )
      else
         lFound := ( ::cAttribute IN oNode:aAttributes )
      endif
   endif

   if ::cValue != NIL .and. ! empty( oNode:aAttributes )
      if ::lRegex
         lFound := ( HScan( oNode:aAttributes, {| /*xKey*/ ,cValue| cValue LIKE ::cValue } ) > 0 )
      else
         lFound := ( HScan( oNode:aAttributes, ::cValue ) != 0 )
      endif
   endif

   if ::cData != NIL .and. oNode:cData != NIL
      if ::lRegex
         lFound :=  ( oNode:cData HAS ::cData )
      else
         lFound := ( oNode:cData == ::cData )
      endif
   endif


RETURN lFound


/********************************************
   IteratorScan / IteratorRegex classes
   note: Wrapper classes for old ones
*********************************************/

CLASS TxmlIteratorScan FROM TxmlIterator
  METHOD New( oNodeTop ) CONSTRUCTOR
  METHOD Next() INLINE Super:FindNext()
END CLASS

METHOD New( oNodeTop ) CLASS TxmlIteratorScan
  super:New( oNodeTop )
Return Self


CLASS TxmlIteratorRegex FROM TxmlIterator
  METHOD New( oNodeTop ) CONSTRUCTOR
  METHOD Next() INLINE Super:FindNext()
END CLASS

METHOD New( oNodeTop ) CLASS TxmlIteratorRegex
  super:New( oNodeTop )
  ::lRegex:= .T.
Return Self




/********************************************
   XML Document Class
*********************************************/
CLASS TXmlDocument

   DATA oRoot
   DATA nStatus
   DATA nError
   DATA nLine
   DATA oErrorNode
   DATA nNodeCount
   DATA cSignature
   
   METHOD New( uXml, nStyle )         CONSTRUCTOR
   METHOD Read( xData, nStyle )       
   METHOD ToString( nStyle )          INLINE iif( ::nStatus = HBXML_STATUS_OK, ::oRoot:ToString( nStyle ) ,"")
   METHOD Write( cFileName, nStyle )

   METHOD FindFirst( cName, cAttrib, cValue, cData )
   METHOD FindFirstRegex( cName, cAttrib, cValue, cData )
   METHOD FindNext()                  INLINE iif( ::nStatus = HBXML_STATUS_OK, ::oIterator:FindNext(), NIL )
   METHOD Next()                      INLINE iif( ::nStatus = HBXML_STATUS_OK, ::oIterator:Next(), NIL )

   METHOD GetContext()


   ACCESS CurNode                     INLINE iif( ::nStatus = HBXML_STATUS_OK, ::oIterator:GetNode(), NIL )
   ACCESS ErrorMsg                    INLINE HB_XMLERRORDESC( ::nError )

HIDDEN:

   DATA oIterator
   DATA cHeader
   
ENDCLASS

*-----------------------------------------------------------------------------*
METHOD New( uXml, nStyle ) CLASS TXmlDocument
*-----------------------------------------------------------------------------*
Local nh, lNew

   ::nStatus := HBXML_STATUS_OK
   ::nError := HBXML_ERROR_NONE
   ::nLine := 1
   ::nNodeCount := 0
   ::cSignature := ""
     
   lNew := .f.

   IF uXml == NIL
      ::oRoot := TXmlNode():New( HBXML_TYPE_DOCUMENT )
      ::cHeader := '<?xml version="1.0"?>'
      lNew := .t.
   ELSE
      SWITCH ValType( uXml )
         CASE 'O'    /* node object */
            ::oRoot = uXml
            EXIT
         CASE 'N'    /* file handle */
            ::oRoot := TXmlNode():New( HBXML_TYPE_DOCUMENT )
            ::Read( uXml, nStyle )
            EXIT
         CASE 'C'    /* xml file name or xml header */
            if ! file( uXml )
               ::oRoot := TXmlNode():New( HBXML_TYPE_DOCUMENT )
               ::Read( uXml, nStyle )
            else
               nh := FOpen( uXml )
               if nh == -1
                  ::nStatus := HBXML_STATUS_ERROR
                  ::nError := HBXML_ERROR_IO
               else
                  ::oRoot := TXmlNode():New( HBXML_TYPE_DOCUMENT )
                  ::Read( nh, nStyle )
                  FClose( nh )
               endif
            endif
            EXIT
         DEFAULT
            ::nStatus := HBXML_STATUS_ERROR
            ::nError := HBXML_ERROR_WRONGENTITY
      END
   ENDIF

   if ::nStatus = HBXML_STATUS_OK
      if empty( ::cHeader ) .and. lNew .and. ::oRoot:oChild != NIL .and. ::oRoot:oChild:cName == "xml"
         ::cHeader := "<?xml "+::oRoot:oChild:cData+"?>"
      endif
      ::oIterator := TXmlIterator():New( ::oRoot )
   else
      ::nStatus := HBXML_STATUS_ERROR
      ::nError := HBXML_ERROR_INVNODE
   endif

RETURN Self

*-----------------------------------------------------------------------------*
METHOD FindFirst( cName, cAttrib, cValue, cData ) CLASS TXmlDocument
*-----------------------------------------------------------------------------*
Local oNode

   if ::nStatus = HBXML_STATUS_OK
      ::oIterator:lRegex := .f.
      oNode := ::oIterator:Find( cName, cAttrib, cValue, cData )
   endif
   
RETURN oNode


*-----------------------------------------------------------------------------*
METHOD FindFirstRegex( cName, cAttrib, cValue, cData ) CLASS TXmlDocument
*-----------------------------------------------------------------------------*
Local oNode

   if ::nStatus = HBXML_STATUS_OK
      ::oIterator:lRegex := .t.
      oNode := ::oIterator:Find( cName, cAttrib, cValue, cData )
   endif
   
RETURN oNode

*-----------------------------------------------------------------------------*
METHOD GetContext() CLASS TXmlDocument
*-----------------------------------------------------------------------------*
   LOCAL oDoc

   if ::nStatus = HBXML_STATUS_OK
      oDoc := TXmlDocument():New()
      oDoc:oRoot := ::oIterator:GetNode()
   endif
   
RETURN oDoc

*-----------------------------------------------------------------------------*
METHOD Write( xFile, nStyle ) CLASS TXmlDocument
*-----------------------------------------------------------------------------*
Local fHandle, cHeader, lOK := .f., cFileName

  if empty( xFile ) .or. ::nStatus != HBXML_STATUS_OK
     return .f.
  endif

  if nStyle = NIL
     nStyle := HBXML_STYLE_INDENT
  endif

  cHeader := ::cHeader

  if nStyle != HBXML_STYLE_NONEWLINE  .and. ! empty( cHeader )
     cHeader += hb_osnewline()
  endif


  if hb_isString( xFile )

     cFilename := alltrim( xFile )

     if ! "." in cFileName .and. ! ".xml" in lower( cFileName )
        cFileName += ".xml"
     endif

     fHandle := FCreate( cFileName )

     lOK := ( FError() == 0 )

  elseif hb_isNumeric( xFile )
     fHandle := xFile
     lOK := .t.
  else
     return .f.
  endif

  if lOK
     if ! empty( ::cSignature )
        FWrite( fHandle, ::cSignature, len( ::cSignature ) )
        lOK := ( FError() == 0 )
     endif
     if lOK .and. ! empty( cHeader )
        FWrite( fHandle, cHeader, len(cHeader) )
        lOK := ( FError() == 0 )
     endif
     if lOK
        ::oRoot:Write( fHandle, nStyle )
        lOK := ( FError() == 0 )
     endif
     if hb_isString( xFile )
        FClose(fHandle)
     endif
  endif

  if ! lOK
     ::nStatus := HBXML_STATUS_ERROR
     ::nError  := HBXML_ERROR_IO
  endif

Return lOK

*-----------------------------------------------------------------------------*
METHOD Read( xData, nStyle ) CLASS TXmlDocument
*-----------------------------------------------------------------------------*
* Read a xml file through file handle or xml content <xData>.
*-----------------------------------------------------------------------------*
Local cBOM 

 if ValType( xData ) == "N"  // file handle
    FSeek( xData, 0, FS_SET )
    cBOM := FReadStr( xData, 3 )
    FSeek( xData, 0, FS_SET )
 elseif valtype( xData ) == "C"  // Xml content.
    cBOM := Left(xData,3)
 else
    ::nStatus := HBXML_STATUS_MALFORMED
    ::nError := HBXML_ERROR_INVNODE
    Return Self
 endif
 
 /* The xml document can have the utf-8 signature named BOM (Byte Order Mark),
    composed by a sequence of 3 characters, like chr(239), chr(187) and chr(191) 
    or EF, BB and BF.
    This signature is always at the beginning of the file, before <?xml> tag and can
    cause unexpected results. So we need treat it.
    More detais at http://www.w3.org/International/questions/qa-utf8-bom */
 if Asc( cBOM[1] ) = 239 .and. Asc( cBOM[2] ) = 187 .and. Asc( cBOM[3] ) = 191
    ::cSignature := cBOM
 endif 

 ::nStatus := HBXML_DATAREAD( Self, xData, nStyle ) 

Return Self    
