/*
   XWT - xHarbour Windowing Toolkit

   (C) 2004 Luiz Rafael Culik

   $Id: calendar.prg,v 1.1 2004/01/25 02:44:16 lculik Exp $

   Text box Control. 
*/

#include "hbclass.ch"
#include "xwt.ch"
#include "common.ch"

CLASS XWTCalendar FROM XWTWidget
   
   METHOD New( cText, x, y, oParent )
/*
   METHOD SetEditable( bEdit )
   METHOD SetVisibility( bVisibility )
   METHOD SelectRegion( nStart, nEnd )
   METHOD SetCursorPos( nPos )
*/
METHOD GetDate()
METHOD GetDateModal()
METHOD domodal()

ENDCLASS

METHOD New( cText, x, y, oParent ,lModal,cDate) CLASS XWTCalendar
default lModal to .f.
   ::Super:New( )
   ::nWidgetType := XWT_TYPE_CALENDAR
   // sooner or later I will want to add event management for labels,
   // so, I put here the Self parameter needed for event callbacks
   tracelog( cText, x, y, oParent ,lModal)
   if lModal
     ::oRawWidget := XWT_Create( Self, XWT_TYPE_CALENDARM )
   else
     ::oRawWidget := XWT_Create( Self, XWT_TYPE_CALENDAR )
   endif
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

   IF .not. Empty( cDate )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_SETDATE, cDate )
   ENDIF

   IF ValType( x ) == "N" .and. ValType( y ) == "N"
      ::Move( x, y )
   ENDIF
   IF oParent != NIL
      oParent:Add( Self )
   ENDIF
tracelog(' do new do calendar')   
RETURN Self

METHOD GetDate() Class XWTCalendar
   LOCAL cFile := "  /  /    "

   IF  XWT_GetProperty( ::oRawWidget, XWT_PROP_GETDATE, @cFile)

      RETURN cFile
   ENDIF

RETURN CTOD("")

METHOD GetDateModal() Class XWTCalendar
   LOCAL cFile := "  /  /    "

   IF  XWT_GetProperty( ::oRawWidget, XWT_PROP_GETDATEMODAL, @cFile)

      RETURN cFile
   ENDIF

RETURN CTOD("")

/*
METHOD SetEditable( bEdit ) CLASS XWTCalendar
   LOCAL bRes := XWT_WidgetSet( ::oRawWidget, XWT_SET_EDITABLE, bEdit )
   
   IF bRes
      ::bEditable := bEdit
      XWT_FastRiseEvent( XWT_E_CHANGED, Self )
   ENDIF
   
RETURN bRes

METHOD SetVisibility( bVis ) CLASS XWTCalendar
   LOCAL bRes := XWT_WidgetSet( ::oRawWidget, XWT_SET_VISIBLE, bVis );
   
   IF bRes
      ::bVisible := bVis
      XWT_FastRiseEvent( XWT_E_CHANGED, Self )
   ENDIF
   
RETURN bRes

METHOD SelectRegion( nStart, nEnd ) CLASS XWTCalendar
   LOCAL bRes:=  XWT_WidgetSet( ::oRawWidget, XWT_SET_SELREGION, nStart, nEnd );
   
   IF bRes
      ::nSelStart := 0
      ::nSelEnd := 0
      XWT_FastRiseEvent( XWT_E_CHANGED, Self )
   ENDIF
   
RETURN bRes
*/

METHOD DoModal() Class  XWTCalendar
   LOCAL cFile

   ::Show()
   XWT_Modal( ::oRawWidget )
   cFile := ::GetdateMODAL()
   ::Destroy()

RETURN cFile
