/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: framewindow.prg,v 1.3 2003/08/31 20:02:35 xthefull Exp $

   Frame window class. A window with a menu, a central area and a statusbar
*/

#include "hbclass.ch"
#include "xwt.ch"


CLASS XWTFrameWindow FROM XWTContainer
   DATA aMenus
   
   METHOD New( cText )
   METHOD SetMenuBar( aMenus )
   
   METHOD Destroy()
ENDCLASS


METHOD New( cText, aMenus ) CLASS XWTFrameWindow
   ::Super:New()
   ::nWidgetType := XWT_TYPE_FRAME
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_FRAME )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

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
      XWT_SetProperty( ::oRawWidget, XWT_PROP_RSTMENUBAR, aMenus )
   ENDIF

   ::aMenus := aMenus
   IF .not. Empty( ::aMenus )
      FOR EACH oMenu IN ::aMenus
         oMenu:oOwner := Self
      NEXT
      XWT_SetProperty( ::oRawWidget, XWT_PROP_SETMENUBAR, aMenus )
   ENDIF

RETURN .T.


METHOD Destroy() CLASS XWTFrameWindow
   LOCAL oMenu

   IF ::aMenus != NIL   
      FOR EACH oMenu IN ::aMenus
         oMenu:oOwner := NIL
         oMenu:Destroy()
      NEXT
   ENDIF

RETURN ::Super:Destroy()
