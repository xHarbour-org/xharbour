/*
   XWT - xHarbour Windowing Toolkit

   (C) 2004 Lorenzo Fiorini

   $Id$

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTNoteBook FROM XWTContainer
   METHOD New( nX, nY, oParent )
   METHOD AddPage( oPage )
ENDCLASS

METHOD New( nX, nY, oParent ) CLASS XWTNoteBook

   ::Super:New()
   ::nWidgetType := XWT_TYPE_NOTEBOOK
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_NOTEBOOK )
   
   IF ValType( nX ) == "N" .and. ValType( nY ) == "N"
      ::Move( nX, nY )
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self

METHOD AddPage( oPage ) CLASS XWTNoteBook
   ::Super:Add( oPage )
RETURN Self
