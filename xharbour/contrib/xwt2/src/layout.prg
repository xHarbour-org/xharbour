/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: layout.prg,v 1.7 2003/07/21 18:14:40 xthefull Exp $

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

