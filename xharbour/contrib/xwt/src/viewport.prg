/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: layout.prg,v 1.3 2003/04/07 18:20:30 jonnymind Exp $

   View port class. Should contain a single widget, most likely
   a container, but also html panes etc.
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTViewport FROM XWTWidget
   DATA  oContent

   METHOD New( nWidth, nHeight )

   METHOD SetScrollPos( nX, nY )
   METHOD GetScrollPos( nX, nY )

   METHOD SetContent( oWidget )
   METHOD RemoveContent( oWidget )

   METHOD Show()
   METHOD Hide()
   METHOD Destroy()

   METHOD ScrolledEventListener( oEvent )
ENDCLASS


METHOD New( nWidth, nHeight ) CLASS XWTViewPort
   ::Super:New()
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_VIEWPORT )

   IF .not. Empty( nWidth ) .and. .not. Empty( nHeight )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_SIZE, nWidth, nHeight )
   ENDIF

   ::AddEventListener( XWT_E_SCROLL, Self, "ScrolledEventListener" )
RETURN Self


METHOD SetScrollPos( nX, nY ) CLASS XWTViewPort
   LOCAL bRes := .F.


   IF Empty( nX )
      nX := -1
   ENDIF

   IF Empty( nY )
      nY := -1
   ENDIF

   IF .not. XWT_FastRiseEvent( XWT_E_SCROLL, Self, nX, nY )
      bRes := XWT_SetProperty( ::oRawWidget, XWT_PROP_SCROLL, nX, nY )
   ENDIF

RETURN bRes


METHOD GetScrollPos( nX, nY ) CLASS XWTViewPort
RETURN XWT_GetProperty( ::oRawWidget, XWT_PROP_SCROLL, nX, nY )


METHOD SetContent( oContent ) CLASS XWTViewPort
   LOCAL bRet

   // Remove the old content
   IF .not. Empty( ::oContent )
      IF XWT_FastRiseEvent( XWT_E_REMOVECHILD, Self, ::oContent )
         RETURN .F.
      ENDIF
      ::oContent:oOwner := NIL
      ::oContent := NIL
   ENDIF

   // Remove
   bRet := .not. XWT_FastRiseEvent( XWT_E_ADDCHILD, Self, oContent )

   IF bRet
      ::oContent := oContent
      oContent:oOwner := Self
      XWT_add( ::oRawWidget, ::oContent:oRawWidget )
   ENDIF

RETURN bRet


METHOD RemoveContent() CLASS XWTViewPort
   LOCAL bRet := .T.

   // Remove the old content
   IF .not. Empty( ::oContent )
      IF XWT_FastRiseEvent( XWT_E_REMOVECHILD, Self, ::oContent )
         RETURN .F.
      ENDIF
      ::oContent:oOwner := NIL
      ::oContent := NIL
   ELSE
      bRet := .F.
   ENDIF

RETURN bRet


METHOD Destroy() CLASS XWTViewPort
   IF .not. Empty( ::oContent )
      ::oContent:Destroy()
   ENDIF
RETURN ::Super:Destroy()


METHOD ScrolledEventListener( oEvent ) CLASS XWTViewPort
   // Signal the child that it has been scrolled
   XWT_FastRiseEvent( XWT_E_SCROLLED, ::oContent, oEvent:aParams[1], oEvent:aParams[2] )
RETURN .F.


METHOD Show() CLASS XWTViewPort
   IF .not. Empty( ::oContent )
      ::oContent:Show()
   ENDIF
RETURN ::Super:Show()

METHOD Hide() CLASS XWTViewPort
   IF .not. Empty( ::oContent )
      ::oContent:Hide()
   ENDIF
RETURN ::Super:Hide()
