************************************************************
* xmltest.prg
* $Id: random.prg,v 1.1 2003/01/29 23:39:10 jonnymind Exp $
*
* Test for XML routines of Xharbour rtl (MXML/HBXML)
*
* (C) Giancarlo Niccolai
*

#include "fileio.ch"
#include "hbxml.ch"

PROCEDURE Main( cFileName )
   LOCAL hFile, cData
   LOCAL xmlDoc, xmlIter, aNode

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

   xmlDoc := HB_XmlCreate( hFile )
   IF HB_XmlStatus( xmlDoc ) != HBXML_STATUS_OK
      @4,10 SAY "Error While Processing File: "
      @5,10 SAY "On Line: " + AllTrim( Str( HB_XmlLine( xmlDoc ) ) )
      @6,10 SAY "Error: " + HB_XmlErrorDesc( HB_XmlError( xmlDoc ) )
      @8,10 SAY "Program Terminating, press any key"
      Inkey( 0 )
      HB_XmlDestroy( xmlDoc )
      RETURN
   ENDIF

   ? "XML Dump beginning here"
   ? "-----------------------"
   ? ""

   cData := HB_XmlToString( xmlDoc, HBXML_STYLE_INDENT + HBXML_STYLE_THREESPACES )
   ? cData
   ? "--- Press any key for next test"
   Inkey(0)

   ? "-----------------------"
   ? "Navigating all nodes"
   ? ""

   xmlIter := HB_XmlGetIterator( xmlDoc )

   DO WHILE HB_XmlNextInTree( xmlIter )
      cData := HB_XmlGetPath( xmlIter )
      IF cData == NIL
         cData :=  "(Node without path)"
      ENDIF
      aNode := HB_XmlGetNode( xmlIter )
      ? ValToPrg( aNode ) + " " + cData
   ENDDO

   ? ""
   ? "Terminated. Press any key to continue"
   Inkey( 0 )
   HB_XmlDestroy( xmlDoc )
RETURN
