/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: label.prg,v 1.2 2003/03/28 14:44:40 gian Exp $

   Label class. Just a concrete widget implementation
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTLabel FROM XWTWidget
   METHOD New( cText )
ENDCLASS   

METHOD New( cText, x, y ) CLASS XWTLabel
   ::Super:New( cText )
   // sooner or later I will want to add event management for labels,
   // so, I put here the Self parameter needed for event callbacks
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_LABEL )
   
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

   IF ValType( x ) == "N" .and. ValType( y ) == "N"
      ::Move( x, y )
   ENDIF
RETURN Self
