/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: menu.prg,v 1.2 2003/03/28 14:44:40 gian Exp $

   Menu class
*/
#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTMenu FROM XWTContainer
   
   METHOD New( cStr )
ENDCLASS

METHOD New( cStr ) CLASS XWTMenu
   ::Super:New()
   // Self is not needed here

   ::oRawWidget := XWT_Create( Self, XWT_TYPE_MENU )

   IF .not. Empty( cStr )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cStr )
   ENDIF

RETURN Self
