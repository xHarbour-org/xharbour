/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: framewindow.prg,v 1.1 2004/05/11 14:57:50 jonnymind Exp $

   Frame window class. A window with a menu, a central area and a statusbar
*/

#include "hbclass.ch"
#include "xwt.ch"


CLASS XWTFrameWindow FROM XWTConnector
   DATA aMenus
   
   METHOD New( xProps, aMenus )            CONSTRUCTOR
   METHOD SetMenuBar( aMenus )
   
ENDCLASS


METHOD New( cText, aMenus ) CLASS XWTFrameWindow
   ::nWidgetType := XWT_TYPE_FRAME
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_FRAME )

   ::Super:New( cText )
      
   IF .not. Empty( aMenus )
      ::SetMenuBar( aMenus )
   ENDIF
   
RETURN Self


METHOD SetMenuBar( aMenus ) CLASS XWTFrameWindow
   LOCAL oMenu
   IF .not. Empty( ::aMenus )
      FOR EACH oMenu IN ::aMenus
         oMenu:oOwner := NIL
      NEXT
   ENDIF

   ::aMenus := aMenus
   XWT_SetProperty( ::oRawWidget, "menubar", ::aMenus )
   IF .not. Empty( ::aMenus )
      FOR EACH oMenu IN ::aMenus
         oMenu:oOwner := Self
      NEXT
   ENDIF

RETURN .T.

