
// WHOO.LIB

#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"

*-----------------------------------------------------------------------------*

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

*-----------------------------------------------------------------------------*
   
METHOD New( oParent ) CLASS TMenu

   ::handle := CreateMenu()
   ::Parent := oParent

   return( self )

*-----------------------------------------------------------------------------*

METHOD AddPopUp( cText ) CLASS TMenu

   if ::Popup != NIL
      ::Popup:Add()
   endif
   ::PopUp := TPopup():New( self, cText)

   return(self)

*-----------------------------------------------------------------------------*

METHOD Set() CLASS TMenu

   if ::Popup != NIL
      ::Popup:Add()
   endif
   SetMenu( ::Parent:handle, ::handle )

   return( self )

*-----------------------------------------------------------------------------*

METHOD GetItem( nId ) CLASS TMenu

   local n:= aScan( ::aItems,{|o|o:id == nId} )

   if n>0
      return( ::aItems[n] )
   endif

   return( nil )

