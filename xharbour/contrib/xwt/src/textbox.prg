/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: textbox.prg,v 1.2 2003/03/28 14:44:40 gian Exp $

   Text box Control. 
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTTextBox FROM XWTWidget
   DATA nSelStart
   DATA nSelEnd
   DATA bEditable
   DATA bVisible
   DATA nCursorPos
   
   METHOD New( cText )
/*
   METHOD SetEditable( bEdit )
   METHOD SetVisibility( bVisibility )
   METHOD SelectRegion( nStart, nEnd )
   METHOD SetCursorPos( nPos )
*/

ENDCLASS   

METHOD New( cText, x, y ) CLASS XWTTextBox
   ::Super:New( cText )
   // sooner or later I will want to add event management for labels,
   // so, I put here the Self parameter needed for event callbacks
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_TEXTBOX )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

   IF ValType( x ) == "N" .and. ValType( y ) == "N"
      ::Move( x, y )
   ENDIF
   
   ::bVisible := .T.
   ::bEditable := .T.
   ::nCursorPos := 1
   ::nSelStart := 0
   ::nSelEnd := 0
   
RETURN Self

/*
METHOD SetEditable( bEdit ) CLASS XWTTextBox
   LOCAL bRes := XWT_WidgetSet( ::oRawWidget, XWT_SET_EDITABLE, bEdit )
   
   IF bRes
      ::bEditable := bEdit
      XWT_FastRiseEvent( XWT_E_CHANGED, Self )
   ENDIF
   
RETURN bRes

METHOD SetVisibility( bVis ) CLASS XWTTextBox
   LOCAL bRes := XWT_WidgetSet( ::oRawWidget, XWT_SET_VISIBLE, bVis );
   
   IF bRes
      ::bVisible := bVis
      XWT_FastRiseEvent( XWT_E_CHANGED, Self )
   ENDIF
   
RETURN bRes

METHOD SelectRegion( nStart, nEnd ) CLASS XWTTextBox
   LOCAL bRes:=  XWT_WidgetSet( ::oRawWidget, XWT_SET_SELREGION, nStart, nEnd );
   
   IF bRes
      ::nSelStart := 0
      ::nSelEnd := 0
      XWT_FastRiseEvent( XWT_E_CHANGED, Self )
   ENDIF
   
RETURN bRes
*/
