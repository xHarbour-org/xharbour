/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: viewport.prg,v 1.2 2003/05/11 15:14:43 jonnymind Exp $

   View port class. Should contain a single widget, most likely
   a container, but also html panes etc.
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTViewport FROM XWTConnector
   METHOD New( xProps, oParent )   CONSTRUCTOR
ENDCLASS

/**
   Properties:
   - row-position (int)
   - col-position (int)
   
   events:
   - scrolled (int,int)
*/
   
METHOD New( xProps, oParent )  CLASS XWTViewPort
   ::nWidgetType := XWT_TYPE_VIEWPORT
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_VIEWPORT )
   ::Super:New( xProps, oParent )
   
   IF oParent != NIL
      oParent:Add( Self )
   ENDIF
RETURN Self

