/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: container.prg,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTContainer FROM XWTWidget
   DATA aChildren

   METHOD New()
   METHOD Add( oWidget )
   METHOD Remove( oWidget )
   
   METHOD Show()
   METHOD Hide()
   
   METHOD Destroy()

ENDCLASS


METHOD New() CLASS XWTContainer
   ::Super:New()
   ::aChildren := {}
RETURN Self


METHOD Add( oChild ) CLASS XWTContainer
   LOCAL bRet

   bRet := ::RiseEvent( XwtEvent():New( XWT_E_ADDCHILD, Self, { oChild } ) )

   IF .not. bRet
      AAdd( ::aChildren, oChild )
      oChild:oOwner := Self
      XWT_add( ::oRawWidget, oChild:oRawWidget )
      // ensure a relative refresh after addition.
      oChild:Move( oChild:x, oChild:y )
   ENDIF
   
RETURN bRet


METHOD Remove( oChild ) CLASS XWTContainer
   LOCAL nPos, bRet := .T.

   nPos := AScan( ::aChildren, {|oElem| oElem == oChild} )
   IF nPos > 0
      ::aChildren:oOwner := NIL
      ADel( ::aChildren, nPos )
      ASize( ::aChildren, Len( ::aChildren ) -1 )
      
      bRet := ::RiseEvent( XwtEvent():New( XWT_E_REMOVECHILD, Self, { oChild } ) )
      IF .not. bRet
         XWT_remove( Self:oRawWidget, oChild:oRawWidget )
      ENDIF
   ENDIF

RETURN bRet


METHOD Show() CLASS XWTContainer
   LOCAL oChild

   FOR EACH oChild IN ::aChildren
      oChild:Show()
   NEXT

RETURN ::Super:Show()


METHOD Hide() CLASS XWTContainer
   LOCAL oChild

   FOR EACH oChild IN ::aChildren
      oChild:Hide()
   NEXT

RETURN ::Super:Hide()


METHOD Destroy() CLASS XWTContainer
   LOCAL oChild
   
   FOR EACH oChild IN ::aChildren
      oChild:Destroy()
   NEXT
   
RETURN ::Super:Destroy()
