/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: pane.prg,v 1.2 2003/04/07 15:41:06 jonnymind Exp $

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
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_PANE )
RETURN Self

