/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: menu.prg,v 1.2 2003/05/11 15:14:43 jonnymind Exp $

   Menu class
*/
#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTMenu FROM XWTContainer
   
   METHOD New( cStr, oParent )
ENDCLASS

METHOD New( cStr , oParent ) CLASS XWTMenu
   ::Super:New()
   // Self is not needed here
   ::nWidgetType := XWT_TYPE_MENU
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_MENU )

   IF .not. Empty( cStr )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cStr )
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self
