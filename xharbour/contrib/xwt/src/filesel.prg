/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Luiz Rafael

   $Id: filesel.prg,v 1.1 2003/04/14 22:35:07 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTFileSel FROM XWTWidget
   METHOD New( cText )
   METHOD GetFile()
ENDCLASS

METHOD New( cText ) CLASS XWTFileSel
   ::Super:New()
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_FILESEL )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF  
RETURN Self

METHOD GetFile() Class XWTFileSel
   LOCAL cFile := Space( 100 )
   IF  XWT_GetProperty(::oRawWidget,XWT_PROP_FILENAME,@cFile)
	return cFile   
ENDIF

RETURN ""    	