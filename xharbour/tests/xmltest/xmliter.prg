************************************************************
* xmliter.prg
* $Id$
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

   LOCAL cXml
   LOCAL oDoc, oNode, oIter, lFind

   SET EXACT OFF

   CLS
   ? "X H A R B O U R - XML ITERATOR test "

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

   ? "Processing file "+cFileName+"..."

   oDoc := TXmlDocument():New( cFileName )

   IF oDoc:nStatus != HBXML_STATUS_OK
      @4,10 SAY "Error While Processing File: "+cFileName
      @5,10 SAY "On Line: " + AllTrim( Str( oDoc:nLine ) )
      @6,10 SAY "Error: " + oDoc:ErrorMsg 
      @10,10 SAY "Program Terminating, press any key"
      Inkey( 0 )
      RETURN
   ENDIF

   lFind := (cNode != NIL .or. cAttrib != NIL .or. cValue != NIL .or. cData != NIL )

   ? "-----------------------"
   ? "Navigating all nodes with a base iterator"
   ? ""

   oNode := oDoc:CurNode

if ! lFind

   DO WHILE oNode != NIL
      cXml := oNode:Path()
      IF cXml == NIL
         cXml :=  "(Node without path)"
      ENDIF

      ? Alltrim( Str( oNode:nType ) ), ", ", oNode:cName, ", ", ;
            ValToPrg( oNode:aAttributes ), ", ", oNode:cData, ": ", cXml

      oNode := oDoc:Next()
   ENDDO

else
      ? 
      ? "Iterator - Navigating all nodes", cNode, ",", cAttrib, "=", cValue,;
            " with data having ", cData
      ? ""

      oIter := TXmlIterator():New( oDoc:oRoot )
      oIter:lRegex := .t.

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

      oNode := oIter:Find( cNode, cAttrib, cValue, cData )

      WHILE oNode != NIL
         ? "Found node ", oNode:Path() , ValToPrg( oNode:ToArray() )
         oNode := oIter:FindNext()
      ENDDO

endif

   ? 
   ? "Terminated. Press any key to continue"
   Inkey( 0 )
   ?
RETURN
