************************************************************
* xmliter.prg
* $Id: xmltest.prg,v 1.6 2004/03/23 18:09:29 jonnymind Exp $
*
* Test for XML ITERATOR routines of Xharbour rtl (MXML/HBXML)
*
* USAGE:  xmliter <cFileName> <cNode> <cAttrib> <cValue> <cData>
*   cFileName: the XML to parse (defaults to xmltest.xml)
*   cNode: if you want to test regex match on node name
*   cNode: if you want to test if a node has an attribute
*   cValue: if you want to test if a node has an attribute with a given value
*   cData: if you want to test regex match on node content
*   (You can pass NIL/unused elements by setting them to "" on the command line)
*
* (C) Giancarlo Niccolai
*

#include "fileio.ch"
#include "hbxml.ch"

PROCEDURE Main( cFileName, cNode, cAttrib, cValue, cData )
   LOCAL hFile, cXml
   LOCAL xmlDoc, xmlIter , xmlNode

   SET EXACT OFF

   CLEAR SCREEN
   @1,15 SAY "X H A R B O U R - XML ITERATOR test "

   IF cFileName == NIL
      cFileName := "xmltest.xml"
   ENDIF

   // this can happen if I call xmltest filename "" cdata
   IF ValType( cNode ) == "C" .and. Len( cNode ) == 0
      cNode := NIL
   ENDIF

   // this can happen if I call xmltest filename "" cdata
   IF ValType( cAttrib ) == "C" .and. Len( cAttrib ) == 0
      cAttrib := NIL
   ENDIF

   // this can happen if I call xmltest filename "" cdata
   IF ValType( cValue ) == "C" .and. Len( cValue ) == 0
      cValue := NIL
   ENDIF

   hFile := FOpen( cFileName )

   IF hFile == -1
      @3, 10 SAY "Can't open file " + cFileName
      @4,10 SAY "Terminating, press any key to continue"
      Inkey( 0 )
      RETURN
   ENDIF

   @3,10 SAY "File " + cFileName + " Opened. Processing XML."

   xmlDoc := TXmlDocument():New( hFile )

   IF xmlDoc:nStatus != HBXML_STATUS_OK
      @4,10 SAY "Error While Processing File: "
      @5,10 SAY "On Line: " + AllTrim( Str( xmlDoc:nLine ) )
      @6,10 SAY "Error: " + HB_XmlErrorDesc( xmlDoc:nError )
      @7,10 SAY "Tag Error on tag: " + xmlDoc:oErrorNode:cName
      @8,10 SAY "Tag Begun on line: " + AllTrim( Str( xmlDoc:oErrorNode:nBeginLine ) )
      @10,10 SAY "Program Terminating, press any key"
      Inkey( 0 )
      RETURN
   ENDIF

   ? "-----------------------"
   ? "Navigating all nodes with a base iterator"
   ? ""

   xmlIter := TXmlIterator():New( xmlDoc:oRoot )
   xmlNode := xmlIter:Find()
   DO WHILE xmlNode != NIL
      cXml := xmlNode:Path()
      IF cXml == NIL
         cXml :=  "(Node without path)"
      ENDIF

      ? Alltrim( Str( xmlNode:nType ) ), ", ", xmlNode:cName, ", ", ;
            ValToPrg( xmlNode:aAttributes ), ", ", xmlNode:cData, ": ", cXml

      xmlNode := xmlIter:Next()
   ENDDO

   IF cNode != NIL .or. cAttrib != NIL .or. cValue != NIL .or. cData != NIL
      Inkey( 0 )
      ? ""
      ? "-----------------"
      ? "Iterator - Navigating all nodes", cNode, ",", cAttrib, "=", cValue,;
            " with data having ", cData
      ? ""

      xmlIter := TXmlIteratorRegex():New( xmlDoc:oRoot )
      IF cNode != NIL
         cNode := HB_RegexComp( cNode )
      ENDIF
      IF cAttrib != NIL
         cAttrib := HB_RegexComp( cAttrib )
      ENDIF
      IF cValue != NIL
         cValue := HB_RegexComp( cValue )
      ENDIF
      IF cData != NIL
         cData := HB_RegexComp( cData )
      ENDIF
      xmlNode := xmlIter:Find( cNode, cAttrib, cValue, cData )
      WHILE xmlNode != NIL
         ? "Found node ", xmlNode:Path() , ValToPrg( xmlNode:ToArray() )
         xmlNode := xmlIter:Next()
      ENDDO
   ENDIF

   ? ""
   ? "Terminated. Press any key to continue"
   Inkey( 0 )
   ?
   ?
RETURN
