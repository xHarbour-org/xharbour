
// WHOO.LIB

#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"

CLASS TPopup

   DATA Parent AS OBJECT   READONLY
   DATA handle
   DATA oMenu  AS OBJECT
   DATA Text   AS CHARACTER
   
   METHOD New() CONSTRUCTOR
   METHOD Add()
   METHOD AddItem()

ENDCLASS

*-----------------------------------------------------------------------------*
   
METHOD New( oMenu, cText ) CLASS TPopup

   ::handle := CreatePopupMenu()
   ::oMenu  := oMenu
   ::Text   := cText

   return( self )

*-----------------------------------------------------------------------------*

METHOD Add() CLASS TPopup

   AppendMenu( ::oMenu:handle, MF_ENABLED + MF_POPUP , ::handle, ::Text )

   return( self )

*-----------------------------------------------------------------------------*

METHOD AddItem( cText, nId, bAction ) CLASS TPopup

   local oItem

   oItem := TMenuItem():New( cText, nId, bAction, self )

   return( oItem )

