/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: button.prg,v 1.4 2003/03/28 14:44:40 gian Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTButton FROM XWTWidget
   METHOD New( cText )
ENDCLASS

METHOD New( cText ) CLASS XWTButton
   ::Super:New()
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_BUTTON )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF
RETURN Self
