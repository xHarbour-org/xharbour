************************************************************
* xmltest.prg
* $Id: xmltest.prg,v 1.2 2003/06/16 15:07:19 jonnymind Exp $
*
* Test for XML routines of Xharbour rtl (MXML/HBXML)
*
* (C) Giancarlo Niccolai
*

#include "fileio.ch"
#include "hbxml.ch"

PROCEDURE Main( cFileName )
   LOCAL hFile, cData
   LOCAL xmlDoc, xmlNode

   CLEAR SCREEN
   @1,15 SAY "X H A R B O U R - XML Test "

   IF cFileName == NIL
      cFileName := "xmltest.xml"
   ENDIF

   hFile := FOpen( cFileName )

   IF hFile == -1
      @3, 10 SAY "Can't open file " + cFileName
      @4,10 SAY "Terminating, press any key to continue"
      Inkey( 0 )
      RETURN
   ENDIF

   @3,10 SAY "File " + cFileName + " Opened. Processing XML."

   xmlDoc := TXmlDocument():New()
   xmlDoc:Read( hFile )
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

   cData := xmlDoc:ToString( HBXML_STYLE_INDENT + HBXML_STYLE_THREESPACES )
   ? cData
   ? "--- Press any key for next test"
   Inkey(0)

   ? "-----------------------"
   ? "Navigating all nodes"
   ? ""

   xmlNode := xmlDoc:oRoot:oChild
   DO WHILE xmlNode != NIL
      cData := xmlNode:Path()
      IF cData == NIL
         cData :=  "(Node without path)"
      ENDIF

      ? Alltrim( Str( xmlNode:nType ) ), ", ", xmlNode:cName, ", ", ;
            ValToPrg( xmlNode:aAttributes ), ", ", xmlNode:cData, ": ", cData

      xmlNode := xmlNode:NextInTree()
   ENDDO

   ? ""
   ? "Terminated. Press any key to continue"
   Inkey( 0 )
RETURN
