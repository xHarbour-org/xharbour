/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: menuitem.prg,v 1.2 2003/03/28 14:44:40 gian Exp $

   Menuitem class.
*/
#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTMenuItem FROM XWTWidget
   DATA aCallback
   DATA nId
   
   METHOD New( cStr, nId, oCalled, oMethod )
   
ENDCLASS

METHOD New( cStr, nId, oCalled, oMethod ) CLASS XWTMenuItem
   ::Super:New()
   ::nId := nId

   IF .not. Empty( oCalled )
      ::AddEventListener( XWT_E_CLICKED, oCalled, oMethod )
   ENDIF
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_MENUITEM )

   IF .not. Empty( cStr )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cStr )
   ENDIF

RETURN Self
