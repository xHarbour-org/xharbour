/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: button.prg,v 1.2 2003/05/11 15:14:43 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTButton FROM XWTWidget
   METHOD New( cText, x, y, oParent )
ENDCLASS

METHOD New( cText, x, y, oParent ) CLASS XWTButton
   ::Super:New()
   ::nWidgetType := XWT_TYPE_BUTTON
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_BUTTON )

   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

   IF Valtype( x ) == "N" .and. Valtype( y ) == "N"
      ::Move(x,y)
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF
   
RETURN Self
