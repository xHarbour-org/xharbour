/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: connector.prg,v 1.1 2004/05/11 14:57:50 jonnymind Exp $

   Textarea class; a (not so basic) text editor interface
*/

#include "hbclass.ch"
#include "xwt.ch"


CLASS XWTTextArea FROM XWTWidget
   METHOD New( xProps, oParent )         CONSTRUCTOR
   METHOD AddTag( cTag, nStart, nEnd )   INLINE XWT_SetProperty( ::oRawWidget, "add-tag", { cTag, nStart, nEnd } )
   METHOD SetTags( aTags )               INLINE XWT_SetProperty( ::oRawWidget, "taglist", aTags )
   METHOD GetTags()                      INLINE XWT_GetProperty( ::oRawWidget, "taglist"  )

   METHOD GetSlice( nStart, nEnd )
   METHOD SetSlice( cText, nStart, nEnd )

HIDDEN:
   DATA nSliceStart
   DATA nSliceEnd

ENDCLASS

/*
   PROPERTIES:
      row      (numeric 1-n)
      column   (numeric 1-n)
      position (numeric 1-n)
      modified (logical)
      editable (logical)
      justification (left, center, right)
      wordwrap (.t.)
      rowcount (read only)

   TAGS:
      "bold"
      "italic"
      "underline"
      "font-face: xxx"
      "font-size: xxx"
      "color: xxx"

   EVENTS:
      changed (nId)

*/


METHOD New( xProps, oParent ) CLASS XWTTextArea
   ::nWidgetType := XWT_TYPE_TEXTAREA
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_TEXTAREA )
   Super:New( xProps )

   IF oParent != NIL
      oParent:Add( self )
   ENDIF

RETURN Self


METHOD GetSlice( nStart, nEnd ) CLASS XWTTextArea
   ::nSliceStart := nStart
   ::nSliceEnd := nEnd
RETURN XWT_GetProperty( ::oRawWidget, "text-slice" )


METHOD SetSlice( cText, nStart, nEnd ) CLASS XWTTextArea
   ::nSliceStart := nStart
   ::nSliceEnd := nEnd
RETURN XWT_SetProperty( ::oRawWidget, "text-slice", cText )
