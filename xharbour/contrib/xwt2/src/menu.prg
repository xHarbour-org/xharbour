/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id$

   Menu class
*/
#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTMenu FROM XWTContainer   
   METHOD New( xProps, oParent )       CONSTRUCTOR
ENDCLASS

METHOD New( xProps , oParent ) CLASS XWTMenu
   // Self is not needed here
   ::nWidgetType := XWT_TYPE_MENU
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_MENU )
   ::Super:New( xProps )

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self
