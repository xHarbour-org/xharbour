/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: container.prg,v 1.4 2003/04/07 22:06:39 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTContainer FROM XWTWidget
   DATA aChildren

   // The radio box is at disposal of the drivers to set
   // the radio button group. Can be any kind of object
   // depending on the underlying system.

   // When a radio button is first added to a container,
   // it is stored also in this variable. The underlying
   // system must then be able to retreive the data to
   // set the same radio button group as the first one.

   DATA oRadioBox

   METHOD New()
   METHOD Add( oWidget )
   METHOD Remove( oWidget )

   METHOD Show()
   METHOD Hide()

   METHOD Destroy()

   METHOD SetBox( bHasBox, cBoxTitle )
   METHOD HasBox()

   METHOD SetBorder( iBorder )
   METHOD GetBorder()

ENDCLASS


METHOD New() CLASS XWTContainer
   ::Super:New()
   ::aChildren := {}
RETURN Self


METHOD Add( oChild ) CLASS XWTContainer
   LOCAL bRet

   bRet := .not. XWT_FastRiseEvent( XWT_E_ADDCHILD, Self, oChild )

   IF bRet
      XWT_add( ::oRawWidget, oChild:oRawWidget )
      XWT_FastRiseEvent( XWT_E_ADDEDTO, oChild, Self )
      oChild:oOwner := Self
      AAdd( ::aChildren, oChild )
      // ensure a relative refresh after addition.
      oChild:Move( oChild:x, oChild:y )
   ENDIF

RETURN bRet


METHOD Remove( oChild ) CLASS XWTContainer
   LOCAL nPos, bRet := .T.

   nPos := AScan( ::aChildren, {|oElem| oElem == oChild} )
   IF nPos > 0
      bRet := .not. XWT_FastRiseEvent( XWT_E_REMOVECHILD, Self, oChild  )
      IF bRet
         ::aChildren[nPos]:oOwner := NIL
         ADel( ::aChildren, nPos )
         ASize( ::aChildren, Len( ::aChildren ) -1 )
         XWT_remove( Self:oRawWidget, oChild:oRawWidget )
         XWT_FastRiseEvent( XWT_E_REMOVEDFROM, oChild, Self )
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

METHOD SetBox( bHasBox, cBoxTitle ) CLASS XWTContainer

   IF XWT_SetProperty( ::oRawWidget, XWT_PROP_BOX, bHasBox )
      IF .not. Empty( cBoxTitle )
         RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cBoxTitle )
      ENDIF
      RETURN .T.
   ENDIF
RETURN .F.


METHOD HasBox() CLASS XWTContainer
   LOCAL bBox

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_BOX, @bBox )
      RETURN bBox
   ENDIF
RETURN .F.


METHOD SetBorder( iBorder ) CLASS XWTContainer
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_BORDER, iBorder )


METHOD GetBorder() CLASS XWTContainer
   LOCAL iBorder

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_BORDER, @iBorder )
      RETURN iBorder
   ENDIF
RETURN -1
