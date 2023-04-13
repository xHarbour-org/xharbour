/*
   XWT - xHarbour Windowing Toolkit

   (C) 2004 Lorenzo Fiorini

   $Id$

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTEditBox FROM XWTWidget
   METHOD New( nX, nY, oParent )
ENDCLASS

METHOD New( nX, nY, oParent ) CLASS XWTEditBox

   ::Super:New()
   ::nWidgetType := XWT_TYPE_EDITBOX
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_EDITBOX )
   
   IF ValType( nX ) == "N" .and. ValType( nY ) == "N"
      ::Move( nX, nY )
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self
