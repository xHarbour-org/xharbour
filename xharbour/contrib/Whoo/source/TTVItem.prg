#include "hbclass.ch"
#include "windows.ch"
#include "what32.ch"

#define TV_FIRST          4352   // 0x1100
#define TVM_EXPAND      TV_FIRST + 2

#define TVE_EXPAND           2

//----------------------------------------------------------------------------//

CLASS TTVItem
   DATA   handle
   DATA   Items
   DATA   Tree
   DATA   Cargo
   METHOD New() CONSTRUCTOR
   METHOD Add()
   METHOD Expand() INLINE SendMessage( ::Tree:handle, TVM_EXPAND, TVE_EXPAND, ::handle )
ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( hItem, oTree, Cargo ) CLASS TTVItem
   ::Items  := {}
   ::handle := hItem
   ::Tree   := oTree
   ::Cargo  := Cargo
return Self

//----------------------------------------------------------------------------//

METHOD Add( cPrompt, nImage, Cargo ) CLASS TTVItem
   local oItem := TTVItem():New( TVInsertItem( ::Tree:handle, cPrompt, ::handle, nImage ), ::Tree, Cargo )
   AAdd( ::Items, oItem )
return oItem

//----------------------------------------------------------------------------//
