/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: label.prg,v 1.7 2003/08/31 19:02:19 xthefull Exp $

   Label class. Just a concrete widget implementation
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTLabel FROM XWTWidget
   METHOD New( xProps, oParent )
ENDCLASS

METHOD New( xProps, oParent ) CLASS XWTLabel

   ::nWidgetType := XWT_TYPE_LABEL
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_LABEL )
   ::Super:New( xProps )

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF
   
RETURN Self
