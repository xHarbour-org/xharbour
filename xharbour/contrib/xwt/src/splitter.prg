/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: splitter.prg,v 1.3 2003/05/11 15:14:43 jonnymind Exp $

   Splitter - a container with two resizable area;
   It is not really implemented as a container (in fact is a
   subclass of widget), as you must set layouts, pane or
   containers into its two areas.
*/
#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTSplitter FROM XWTWidget

   METHOD New( nMode, oFirstWidget, oSecondWidget, nSizeX, nSizeY, oParent )

   METHOD SetFirst( oWidget )
   METHOD SetSecond( oWidget )

   METHOD GetFirst()          INLINE ::oFirst
   METHOD GetSecond()         INLINE ::oSecond

   METHOD RemoveFirst()
   METHOD RemoveSecond()

   METHOD SetShrinkFirst( lMode )
   METHOD SetShrinkSecond( lMode )
   METHOD GetShrinkFirst()
   METHOD GetShrinkSecond()

   METHOD Destroy()

HIDDEN:
   DATA oFirst
   DATA oSecond

ENDCLASS

METHOD New( nMode, oFirstWidget, oSecondWidget, nSizeX, nSizeY, oParent ) CLASS XWTSplitter
   ::Super:New()
   ::nWidgetType := XWT_TYPE_SPLITTER
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_SPLITTER )
   XWT_SetProperty( ::oRawWidget, XWT_PROP_LAYMODE, nMode )

   IF .not. Empty( oFirstWidget )
      ::SetFirst( oFirstWidget )
   ENDIF

   IF .not. Empty( oSecondWidget )
      ::SetSecond( oSecondWidget )
   ENDIF

   IF ValType( nSizeX ) == "N" .and. ValType( nSizeY ) == "N"
      ::Size( nSizeX, nSizeY )
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self

METHOD SetFirst( oWidget ) CLASS XWTSplitter

   IF XWT_SetProperty( ::oRawWidget, XWT_PROP_FIRSTWID, oWidget:oRawWidget )
      ::oFirst := oWidget
      RETURN .T.
   ENDIF

RETURN .F.

METHOD SetSecond( oWidget ) CLASS XWTSplitter
   ? "Second is calling ", XWT_PROP_SECWID, "  "
   IF XWT_SetProperty( ::oRawWidget, XWT_PROP_SECWID, oWidget:oRawWidget )
      ::oSecond := oWidget
      RETURN .T.
   ENDIF

RETURN .F.

METHOD RemoveFirst() CLASS XWTSplitter

   IF XWT_SetProperty( ::oRawWidget, XWT_PROP_FIRSTWID, NIL )
      ::oFirst := NIL
      RETURN .T.
   ENDIF

RETURN .F.

METHOD RemoveSecond() CLASS XWTSplitter

   IF XWT_SetProperty( ::oRawWidget, XWT_PROP_SECWID, NIL )
      ::oSecond := NIL
      RETURN .T.
   ENDIF

RETURN .F.


METHOD SetShrinkFirst( bMode ) CLASS XWTSplitter
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_FIRSTSHRINK, bMode )

METHOD SetShrinkSecond( bMode ) CLASS XWTSplitter
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_SECSHRINK, bMode )


METHOD GetShrinkFirst() CLASS XWTSplitter
   LOCAL bMode

   IF XWT_SetProperty( ::oRawWidget, XWT_PROP_FIRSTSHRINK, @bMode )
      RETURN bMode
   ENDIF

RETURN .F.


METHOD GetShrinkSecond() CLASS XWTSplitter
   LOCAL bMode

   IF XWT_SetProperty( ::oRawWidget, XWT_PROP_SECSHRINK, @bMode )
      RETURN bMode
   ENDIF

RETURN .F.


METHOD Destroy() CLASS XWTSplitter
   IF ::oFirst != NIL
      ::oFirst:Destroy()
   ENDIF

   IF ::oSecond != NIL
      ::oSecond:Destroy()
   ENDIF

RETURN ::Super:Destroy()

