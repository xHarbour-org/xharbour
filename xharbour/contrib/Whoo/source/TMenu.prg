#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"


CLASS TMenu

   DATA Parent AS OBJECT   READONLY
   DATA handle
   DATA Popup  AS OBJECT
   DATA aItems AS ARRAY INIT {}
   
   METHOD New() CONSTRUCTOR
   METHOD AddPopUp()
   METHOD Set()
   METHOD GetItem()
   
ENDCLASS

   
METHOD New( oParent ) CLASS TMenu

   ::handle := CreateMenu()
   ::Parent := oParent
   
return( self )


METHOD AddPopUp( cText ) CLASS TMenu

   if ::Popup != NIL
      ::Popup:Add()
   endif
   ::PopUp := TPopup():New( self, cText)
   
return(self)


METHOD Set() CLASS TMenu

   if ::Popup != NIL
      ::Popup:Add()
   endif
   SetMenu( ::Parent:handle, ::handle )

return( self )

METHOD GetItem( nId ) CLASS TMenu
   
   local n:= aScan( ::aItems,{|o|o:id == nId} )
   
   if n>0
      return( ::aItems[n] )
   endif
   
return( nil )

//------------------------------------------------------------------------------------------

CLASS TPopup

   DATA Parent AS OBJECT   READONLY
   DATA handle
   DATA oMenu  AS OBJECT
   DATA Text   AS CHARACTER
   
   METHOD New() CONSTRUCTOR
   METHOD Add()
   METHOD AddItem()
   
ENDCLASS
   
METHOD New( oMenu, cText ) CLASS TPopup

   ::handle := CreatePopupMenu()
   ::oMenu  := oMenu
   ::Text   := cText

return( self )

METHOD Add() CLASS TPopup
   AppendMenu( ::oMenu:handle, MF_ENABLED + MF_POPUP , ::handle, ::Text )
return( self )

METHOD AddItem( cText, nId, bAction ) CLASS TMenu
   local oItem
   oItem := TMenuItem():New( cText, nId, bAction, self )
return( oItem )

//------------------------------------------------------------------------------------------

CLASS TMenuItem

   DATA Text    AS CHARACTER
   DATA oMenu   AS OBJECT
   DATA Id
   DATA Action  AS CODEBLOCK
   METHOD New() CONSTRUCTOR
   
ENDCLASS

METHOD New( cText, nId, bAction, oMenu ) CLASS TMenuItem

   ::Text   := cText
   ::Id     := nId
   ::Action := bAction
   ::oMenu  := oMenu
   
   aAdd( ::oMenu:oMenu:aItems, self )

   AppendMenu( oMenu:handle, MF_ENABLED + MF_STRING, nId, cText)

return(self)

