/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: menuitem.prg,v 1.6 2003/08/31 19:02:19 xthefull Exp $

   Menuitem class.
*/
#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTMenuItem FROM XWTWidget
   METHOD New( xProps, oParent )         CONSTRUCTOR
ENDCLASS

METHOD New( xProps, oParent) CLASS XWTMenuItem
   ::nWidgetType := XWT_TYPE_MENUITEM
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_MENUITEM )
   ::Super:New( xProps )

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self

