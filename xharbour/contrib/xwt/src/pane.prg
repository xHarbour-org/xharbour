/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: pane.prg,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   Pane class. A basic void container.
   (Container is an abstract class, and have not a DRV method)
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTPane FROM XWTContainer
   METHOD New()

   METHOD SetBox( bMode, cText )
   METHOD HasBox( bMode, cText )
ENDCLASS

METHOD New() CLASS XWTPane
   ::Super:New()
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_PANE )
RETURN Self


METHOD SetBox( bHasBox, cBoxTitle ) CLASS XWTPane

   IF XWT_SetProperty( ::oRawWidget, XWT_PROP_BOX, bHasBox )
      IF .not. Empty( cBoxTitle )
         RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cBoxTitle )
      ENDIF
      RETURN .T.
   ENDIF
RETURN .F.


METHOD HasBox() CLASS XWTPane
   LOCAL bBox

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_BOX, @bBox )
      RETURN bBox
   ENDIF
RETURN .F.

