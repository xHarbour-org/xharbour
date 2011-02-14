/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id$

   Pane class. Each widget is arranged below the
   previous one.
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTLayout FROM XWTContainer

   METHOD New( xProps, oParent ) CONSTRUCTOR

ENDCLASS

/* Properties:
   mode: horizontal / vertical (h,v, 0,1)
   padding: (integer)
   expand: (logical)
   fill: (logical)
   homogeneous: (logical)
   --- and the other container props.
*/
    
METHOD New( xProps, oParent ) CLASS XWTLayout

   ::nWidgetType := XWT_TYPE_LAYOUT
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_LAYOUT )
   ::Super:New( xProps )

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self

