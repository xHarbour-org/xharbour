/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: textbox.prg,v 1.3 2003/07/21 18:14:40 xthefull Exp $

   Text box Control. 
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTTextBox FROM XWTWidget   
   METHOD New( xProps, oParent )       CONSTRUCTOR      

ENDCLASS

METHOD New( xProps, oParent ) CLASS XWTTextBox
   ::nWidgetType := XWT_TYPE_TEXTBOX
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_TEXTBOX )
   ::Super:New( xProps )

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF
   
RETURN Self

