/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: container.prg,v 1.5 2003/04/12 23:47:15 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"

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

   METHOD New( cText )
   METHOD Add( oWidget )
   METHOD Remove( oWidget )

   /* METHOD Show()
   METHOD Hide()
   */
ENDCLASS

/**
   Standard properties:
   box .t./.f.
   text => box title
   border
   bordercolor
   borderbackground
*/

METHOD New( cText ) CLASS XWTContainer
   ::Super:New( cText )
   ::aChildren := {}
RETURN Self


METHOD Add( oChild ) CLASS XWTContainer
   LOCAL bRet

   bRet := .not. XWT_FastRiseEvent( "addchild", Self, oChild )

   IF bRet
      XWT_add( ::oRawWidget, oChild:oRawWidget )
      XWT_FastRiseEvent( "addedto", oChild, Self )
      oChild:oOwner := Self
      AAdd( ::aChildren, oChild )
      // ensure a relative refresh after addition.
   ENDIF

RETURN bRet


METHOD Remove( oChild ) CLASS XWTContainer
   LOCAL nPos, bRet := .T.

   nPos := AScan( ::aChildren, {|oElem| oElem == oChild} )
   IF nPos > 0
      bRet := .not. XWT_FastRiseEvent( "removechild", Self, oChild  )
      IF bRet
         ::aChildren[nPos]:oOwner := NIL
         ADel( ::aChildren, nPos )
         ASize( ::aChildren, Len( ::aChildren ) -1 )
         XWT_remove( Self:oRawWidget, oChild:oRawWidget )
         XWT_FastRiseEvent( "removedfrom", oChild, Self )
      ENDIF
   ENDIF

RETURN bRet

/*
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

*/
