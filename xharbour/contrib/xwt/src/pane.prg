/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: pane.prg,v 1.4 2003/05/11 15:14:43 jonnymind Exp $

   Pane class. A basic void container.
   (Container is an abstract class, and have not a DRV method)
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTPane FROM XWTContainer
   METHOD New( lHasBox, cTitle, oParent )
ENDCLASS

METHOD New( lHasBox, cTitle, oParent ) CLASS XWTPane
   ::Super:New()
   ::nWidgetType := XWT_TYPE_PANE
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_PANE )

   IF !Empty( cTitle )
      ::SetBox( .T., cTitle )
   ELSEIF lHasBox
      ::SetBox( .T. )
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self

