/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: menuitem.prg,v 1.3 2003/05/11 15:14:43 jonnymind Exp $

   Menuitem class.
*/
#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTMenuItem FROM XWTWidget
   DATA aCallback
   DATA nId
   CLASSDATA nAutoId INIT 1

   METHOD New( cStr, nId, oCalled, oMethod )
   METHOD SetIcon( cFileName )

ENDCLASS

METHOD New( cStr, nId, oCalled, oMethod, cIcon, oMenu ) CLASS XWTMenuItem
   ::Super:New()

   IF nId != NIL
      ::nId := nId
   ELSE
      ::nId := ::nAutoId++
   ENDIF

   ::nWidgetType := XWT_TYPE_MENUITEM
   IF .not. Empty( oCalled )
      ::AddEventListener( XWT_E_CLICKED, oCalled, oMethod )
   ENDIF
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_MENUITEM )

   IF .not. Empty( cStr )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cStr )
   ENDIF

   IF cIcon != NIL
      ::SetIcon( cIcon )
   ENDIF

   IF oMenu != NIL
      oMenu:Add( Self )
   ENDIF

RETURN Self

METHOD SetIcon( cFileName ) CLASS XWTMenuItem
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_IMAGE, cFilename )

