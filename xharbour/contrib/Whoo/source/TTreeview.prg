#include "hbclass.ch"
#include "windows.ch"
#include "what32.ch"
#include "wingdi.ch"


#define FD_BORDER            8
#define FD_HEIGHT           22

#define SC_KEYMENU       61696   // 0xF100

#define TVS_HASBUTTONS       1
#define TVS_HASLINES         2
#define TVS_LINESATROOT      4


//----------------------------------------------------------------------------//

CLASS TTreeView FROM TControl

   DATA   Items
   DATA   Option
   DATA   Action
   DATA   ImageList
   DATA   bChanged

   METHOD New() CONSTRUCTOR

   METHOD Add()
   METHOD VScroll() VIRTUAL   // standard behavior requested
   METHOD HScroll() VIRTUAL
   METHOD Expand()  INLINE AEval( ::Items, { | oItem | oItem:Expand() } )
   METHOD GetSelected()
   METHOD GetSelText() INLINE TVGetSelText( ::handle )
   METHOD SelChanged() INLINE If( ::bChanged != nil, Eval( ::bChanged, Self ), nil )
   METHOD SetImageList()
ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oParent, nId, nTop, nLeft, nWidth, nHeight ) CLASS TTreeView

   ::Style  := OR(WS_CHILD+WS_VISIBLE+WS_TABSTOP+TVS_HASBUTTONS+TVS_HASLINES+TVS_LINESATROOT, ::Style)
   ::Id     := nId
   ::Parent := oParent
   ::Top    := nTop
   ::Left   := nLeft
   ::Height := nHeight
   ::Width  := nWidth
   ::name   := "SysTreeView32"
   ::Items  := {}
   ::lRegister := .F.
   ::lControl  := .T.
   ::Msgs      := IFNIL( ::Msgs, {WM_DESTROY}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Caption   := ""
   ::ExStyle   := OR(WS_EX_CLIENTEDGE,::ExStyle)
return Self

//----------------------------------------------------------------------------//

METHOD Add( cPrompt, nImage ) CLASS TTreeView
   local oItem
   oItem := TTVItem():New( TVInsertItem( ::handle, cPrompt,, nImage ), Self )
   AAdd( ::Items, oItem )
return oItem

//----------------------------------------------------------------------------//

static function ScanItem( aItems, hItem )
   local n, oItem
   for n = 1 to Len( aItems )
      if Len( aItems[ n ]:Items ) > 0
         if ( oItem := ScanItem( aItems[ n ]:Items, hItem ) ) != nil
            return oItem
         endif
      endif
      if aItems[ n ]:handle == hItem
         return aItems[ n ]
      endif
   next
return nil

//----------------------------------------------------------------------------//

METHOD GetSelected() CLASS TTreeView
return ScanItem( ::Items, TVGetSelected( ::handle ) )

//----------------------------------------------------------------------------//

METHOD SetImageList( oImageList ) CLASS TTreeView
   ::oImageList = oImageList
   TVSetImageList( ::handle, oImageList:handle, 0 )
return nil

//----------------------------------------------------------------------------//