************************************************************
* xmltest.prg
* $Id: xmltest.prg,v 1.3 2003/06/16 20:13:34 jonnymind Exp $
*
* Test for XML routines of Xharbour rtl (MXML/HBXML)
*
* USAGE:  xmlnode <cFileName> <cNode> <cAttrib> <cValue> <cData>
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
   LOCAL xmlDoc, xmlNode

   SET EXACT OFF

   CLEAR SCREEN
   @1,15 SAY "X H A R B O U R - XML Test "

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
      @8,10 SAY "Program Terminating, press any key"
      Inkey( 0 )
      RETURN
   ENDIF

   ? "XML Dump beginning here"
   ? "-----------------------"
   ? ""

   cXml := xmlDoc:ToString( HBXML_STYLE_INDENT + HBXML_STYLE_THREESPACES )
   ? cXml
   ? "--- Press any key for next test"
   Inkey(0)

   ? "-----------------------"
   ? "Navigating all nodes"
   ? ""

   xmlNode := xmlDoc:oRoot:oChild
   DO WHILE xmlNode != NIL
      cXml := xmlNode:Path()
      IF cXml == NIL
         cXml :=  "(Node without path)"
      ENDIF

      ? Alltrim( Str( xmlNode:nType ) ), ", ", xmlNode:cName, ", ", ;
            ValToPrg( xmlNode:aAttributes ), ", ", xmlNode:cData, ": ", cXml

      xmlNode := xmlNode:NextInTree()
   ENDDO

   IF cNode != NIL .or. cAttrib != NIL .or. cValue != NIL .or. cData != NIL
      Inkey( 0 )
      ? ""
      ? "-----------------"
      ? "Searching for node named", cNode, ",", cAttrib, "=", cValue,;
            " with data having", cData
      ? ""

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
      xmlNode := xmlDoc:FindFirstRegex( cNode, cAttrib, cValue, cData )
      WHILE xmlNode != NIL
         ? "Found node ", xmlNode:Path() , ValToPrg( xmlNode:ToArray() )
         xmlNode := xmlDoc:FindNext()
      ENDDO

   ENDIF

   ? ""
   ? "Terminated. Press any key to continue"
   Inkey( 0 )
RETURN
