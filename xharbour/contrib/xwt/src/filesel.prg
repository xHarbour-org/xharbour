/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Luiz Rafael

   $Id: button.prg,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTFileSel FROM XWTWidget
   METHOD New( cText )
ENDCLASS

METHOD New( cText ) CLASS XWTFileSel
   ::Super:New()
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_FILESEL )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF
RETURN Self
