/*
   XWT - xHarbour Windowing Toolkit

   (C) 2004 Luiz Rafael Culik

   $Id: combobox.prg,v 1.2 2004/01/26 20:30:11 lculik Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTListBox FROM XWTWidget
   Data aItems
   METHOD New( cText, bStatus )
   METHOD GetItem()
ENDCLASS

METHOD New( cText, nX, nY, oParent ,aItems ) CLASS XWTListBox

Local bRes,bVis := .T.
   ::Super:New()
   ::nWidgetType := XWT_TYPE_LISTBOX
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_LISTBOX )
   
   IF ValType( aItems )  == "A"
   ::aItems :=  aItems
      XWT_SetProperty(::oRawWidget,XWT_PROP_SETLISTITEMS,aItems)
   ENDIF  
   
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

   
   IF ValType( nX ) == "N" .and. ValType( nY ) == "N"
      ::Move( nX, nY )
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self

METHOD GetiTem() CLASS XWTListBox
   LOCAL cItem

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_TEXT, @cItem )
   tracelog(cItem)
      RETURN cItem
   ENDIF

RETURN ""
