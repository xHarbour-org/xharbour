/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: pane.prg,v 1.3 2003/04/07 18:20:31 jonnymind Exp $

   Pane class. A basic void container.
   (Container is an abstract class, and have not a DRV method)
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTPane FROM XWTContainer
   METHOD New()
ENDCLASS

METHOD New() CLASS XWTPane
   ::Super:New()
   ::nWidgetType := XWT_TYPE_PANE
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_PANE )
RETURN Self

