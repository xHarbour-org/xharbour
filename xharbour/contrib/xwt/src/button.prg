/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: button.prg,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTButton FROM XWTWidget
   METHOD New( cText )
ENDCLASS

METHOD New( cText ) CLASS XWTButton
   ::Super:New()
   ::nWidgetType := XWT_TYPE_BUTTON
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_BUTTON )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF
RETURN Self
