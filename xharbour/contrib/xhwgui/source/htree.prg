/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HTree class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

#define TVM_DELETEITEM       4353   // (TV_FIRST + 1) 0x1101
#define TVM_EXPAND           4354   // (TV_FIRST + 2)
#define TVM_SETIMAGELIST     4361   // (TV_FIRST + 9)
#define TVM_GETNEXTITEM      4362   // (TV_FIRST + 10)
#define TVM_SELECTITEM       4363   // (TV_FIRST + 11)
#define TVM_EDITLABEL        4366   // (TV_FIRST + 14)

#define TVE_COLLAPSE            0x0001
#define TVE_EXPAND              0x0002
#define TVE_TOGGLE              0x0003

#define TVSIL_NORMAL            0

#define TVS_HASBUTTONS          1   // 0x0001
#define TVS_HASLINES            2   // 0x0002
#define TVS_LINESATROOT         4   // 0x0004
#define TVS_EDITLABELS          8   // 0x0008
#define TVS_DISABLEDRAGDROP    16   // 0x0010
#define TVS_SHOWSELALWAYS      32   // 0x0020
#define TVS_RTLREADING         64   // 0x0040
#define TVS_NOTOOLTIPS        128   // 0x0080
#define TVS_CHECKBOXES        256   // 0x0100
#define TVS_TRACKSELECT       512   // 0x0200
#define TVS_SINGLEEXPAND     1024   // 0x0400
#define TVS_INFOTIP          2048   // 0x0800
#define TVS_FULLROWSELECT    4096   // 0x1000
#define TVS_NOSCROLL         8192   // 0x2000
#define TVS_NONEVENHEIGHT   16384   // 0x4000
#define TVS_NOHSCROLL       32768   // 0x8000  // TVS_NOSCROLL overrides this

#define TVGN_ROOT               0   // 0x0000
#define TVGN_NEXT               1   // 0x0001
#define TVGN_PREVIOUS           2   // 0x0002
#define TVGN_PARENT             3   // 0x0003
#define TVGN_CHILD              4   // 0x0004
#define TVGN_FIRSTVISIBLE       5   // 0x0005
#define TVGN_NEXTVISIBLE        6   // 0x0006
#define TVGN_PREVIOUSVISIBLE    7   // 0x0007
#define TVGN_DROPHILITE         8   // 0x0008
#define TVGN_CARET              9   // 0x0009
#define TVGN_LASTVISIBLE       10   // 0x000A

#define TVN_SELCHANGED       (-402)
#define TVN_ITEMEXPANDING    (-405)
#define TVN_BEGINLABELEDIT   (-410)
#define TVN_ENDLABELEDIT     (-411)

#define TVI_ROOT             (-65536)

CLASS HTreeNode INHERIT HObject

   DATA handle
   DATA oTree, oParent
   DATA aItems INIT {}
   DATA bAction
   DATA cargo

   METHOD New( oTree, oParent, oPrev, oNext, cTitle, bAction,aImages )
   METHOD AddNode( cTitle, oPrev, oNext, bAction, aImages )
   METHOD Delete()
   METHOD FindChild( h )
   METHOD GetText()  INLINE TreeGetNodeText( ::oTree:handle,::handle )

ENDCLASS

METHOD New( oTree, oParent, oPrev, oNext, cTitle, bAction,aImages ) CLASS HTreeNode
LOCAL aItems, i, h, im1, im2, cImage, op, nPos

   ::oTree := oTree
   ::oParent := oParent
   ::bAction := bAction

   IF aImages == Nil
      IF oTree:Image1 != Nil
         im1 := oTree:Image1
         IF oTree:Image2 != Nil
            im2 := oTree:Image2
         ENDIF
      ENDIF
   ELSE
      FOR i := 1 TO Len( aImages )
         cImage := Upper( aImages[i] )
         IF ( h := Ascan( oTree:aImages,cImage ) ) == 0
            Aadd( oTree:aImages, cImage )
            aImages[i] := Iif( oTree:type,LoadBitmap( aImages[i] ),OpenBitmap( aImages[i] ) )
            Imagelist_Add( oTree:himl,aImages[i] )
            h := Len( oTree:aImages )
         ENDIF
         h --
         IF i == 1
            im1 := h
         ELSE
            im2 := h
         ENDIF
      NEXT
   ENDIF
   IF im2 == Nil
      im2 := im1
   ENDIF

   nPos := Iif( oPrev==Nil,2,0 )
   IF oPrev == Nil .AND. oNext != Nil
      op := Iif( oNext:oParent==Nil,oNext:oTree,oNext:oParent )
      FOR i := 1 TO Len( op:aItems )
         IF op:aItems[i]:handle == oNext:handle
            EXIT
         ENDIF
      NEXT
      IF i > 1
         oPrev := op:aItems[i-1]
         nPos := 0
      ELSE
         nPos := 1
      ENDIF
   ENDIF
   ::handle := TreeAddNode( Self, oTree:handle,               ;
                      Iif( oParent==Nil,Nil,oParent:handle ), ;
                      Iif( oPrev==Nil,Nil,oPrev:handle), nPos, cTitle, im1, im2 )

   aItems := Iif( oParent==Nil, oTree:aItems, oParent:aItems )
   IF nPos == 2
      Aadd( aItems, Self )
   ELSEIF nPos == 1
      Aadd( aItems, Nil )
      Ains( aItems,1 )
      aItems[ 1 ] := Self
   ELSE
      Aadd( aItems, Nil )
      h := oPrev:handle
      IF ( i := Ascan( aItems,{|o|o:handle==h} ) ) == 0
         aItems[ Len( aItems ) ] := Self
      ELSE
         Ains( aItems,i+1 )
         aItems[ i+1 ] := Self
      ENDIF
   ENDIF

Return Self

METHOD AddNode( cTitle, oPrev, oNext, bAction, aImages ) CLASS HTreeNode
Local oParent := Self
Local oNode := HTreeNode():New( ::oTree,oParent,oPrev,oNext,cTitle,bAction,aImages ), i, h

Return oNode

METHOD Delete( lInternal ) CLASS HTreeNode
Local h := ::handle, j, alen, aItems

   IF !Empty( ::aItems )
      alen := Len( ::aItems )
      FOR j := 1 TO alen
         ::aItems[j]:Delete( .T. )
         ::aItems[j] := Nil
      NEXT
   ENDIF
   SendMessage( ::oTree:handle,TVM_DELETEITEM,0,::handle )
   IF lInternal == Nil
      aItems := Iif( ::oParent==Nil,::oTree:aItems,::oParent:aItems )
      j := Ascan( aItems,{|o|o:handle==h} )
      Adel( aItems,j )
      Asize( aItems,Len( aItems ) - 1 )
   ENDIF

Return Nil

METHOD FindChild( h ) CLASS HTreeNode
Local aItems := ::aItems, i, alen := Len( aItems ), oNode
   FOR i := 1 TO alen
      IF aItems[i]:handle == h
         Return aItems[i]
      ELSEIF !Empty( aItems[i]:aItems )
         IF ( oNode := aItems[i]:FindChild( h ) ) != Nil
            Return oNode
         ENDIF
      ENDIF
   NEXT
Return Nil


CLASS HTree INHERIT HControl

   CLASS VAR winclass   INIT "SysTreeView32"

   DATA aItems INIT {}
   DATA oSelected
   DATA hIml, aImages, Image1, Image2
   DATA bItemChange, bExpand, bRClick
   DATA lEmpty INIT .T.

   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,oFont, ;
                  bInit,bSize,color,bcolor,aImages,lResour,lEditLabels )
   METHOD Init()
   METHOD Activate()
   METHOD AddNode( cTitle, oPrev, oNext, bAction, aImages )
   METHOD FindChild( h )
   METHOD GetSelected()   INLINE TreeGetSelected( ::handle )
   METHOD EditLabel( oNode ) BLOCK {|Self,o|SendMessage(::handle,TVM_EDITLABEL,0,o:handle)}
   METHOD Expand( oNode ) BLOCK {|Self,o|SendMessage(::handle,TVM_EXPAND,TVE_EXPAND,o:handle)}
   METHOD Select( oNode ) BLOCK {|Self,o|SendMessage(::handle,TVM_SELECTITEM,TVGN_CARET,o:handle)}
   METHOD Clean()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,oFont, ;
                  bInit,bSize,color,bcolor,aImages,lResour,lEditLabels ) CLASS HTree
LOCAL i, aBmpSize

   // ::classname:= "HTREE"
   ::title   := ""
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), WS_CHILD+WS_VISIBLE+ ;
         TVS_HASLINES+TVS_LINESATROOT+TVS_HASBUTTONS+TVS_SHOWSELALWAYS+ ;
         Iif(lEditLabels==Nil.OR.!lEditLabels,0,TVS_EDITLABELS) )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::tcolor  := color
   ::bcolor  := bcolor
   ::type    := lResour

   IF aImages != Nil
      ::aImages := {}
      FOR i := 1 TO Len( aImages )
         Aadd( ::aImages, Upper(aImages[i]) )
         aImages[i] := Iif( lResour,LoadBitmap( aImages[i] ),OpenBitmap( aImages[i] ) )
      NEXT
      aBmpSize := GetBitmapSize( aImages[1] )
      ::himl := CreateImageList( aImages,aBmpSize[1],aBmpSize[2],12 )
      ::Image1 := 0
      IF Len( aImages ) > 1
         ::Image2 := 1
      ENDIF
   ENDIF

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Init CLASS HTree

   Super:Init()
   IF ::himl != Nil
      SendMessage( ::handle, TVM_SETIMAGELIST, TVSIL_NORMAL, ::himl )
   ENDIF

Return Nil

METHOD Activate CLASS HTree

   IF ::oParent:handle != 0
      ::handle := CreateTree( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::tcolor, ::bcolor )
      ::Init()
   ENDIF

Return Nil

METHOD AddNode( cTitle, oPrev, oNext, bAction, aImages ) CLASS HTree
Local oNode := HTreeNode():New( Self,Nil,oPrev,oNext,cTitle,bAction,aImages )
   ::lEmpty := .F.
Return oNode

METHOD FindChild( h ) CLASS HTree
Local aItems := ::aItems, i, alen := Len( aItems ), oNode
   FOR i := 1 TO alen
      IF aItems[i]:handle == h
         Return aItems[i]
      ELSEIF !Empty( aItems[i]:aItems )
         IF ( oNode := aItems[i]:FindChild( h ) ) != Nil
            Return oNode
         ENDIF
      ENDIF
   NEXT
Return Nil

METHOD Clean() CLASS HTree

   ::lEmpty := .T.
   SendMessage( ::handle,TVM_DELETEITEM,0,TVI_ROOT )
   ::aItems := {}

Return Nil

#define TREE_GETNOTIFY_HANDLE       1
#define TREE_GETNOTIFY_PARAM        2
#define TREE_GETNOTIFY_EDIT         3
#define TREE_GETNOTIFY_EDITPARAM    4
#define TREE_GETNOTIFY_ACTION       5

#define TREE_SETITEM_TEXT           1

Function TreeNotify( oTree,lParam )
Local nCode := GetNotifyCode( lParam ), oItem, cText, nAct

   IF nCode == TVN_SELCHANGED
      oItem := Tree_GetNotify( lParam,TREE_GETNOTIFY_PARAM )
      oItem:oTree:oSelected := oItem
      IF !oItem:oTree:lEmpty .AND. oItem:bAction != Nil
         Eval( oItem:bAction,oItem )
      ENDIF
   ELSEIF nCode == TVN_BEGINLABELEDIT
      // Return 1
   ELSEIF nCode == TVN_ENDLABELEDIT
      IF !Empty( cText := Tree_GetNotify( lParam,TREE_GETNOTIFY_EDIT ) )
         oItem := Tree_GetNotify( lParam,TREE_GETNOTIFY_EDITPARAM )
         IF cText != oItem:GetText() .AND. ;
             ( oItem:oTree:bItemChange == Nil .OR. Eval( oItem:oTree:bItemChange,oItem,cText ) )
            TreeSetItem( oItem:oTree:handle,oItem:handle,TREE_SETITEM_TEXT,cText )
         ENDIF
      ENDIF
   ELSEIF nCode == TVN_ITEMEXPANDING
      oItem := Tree_GetNotify( lParam,TREE_GETNOTIFY_PARAM )
      IF oTree:bExpand != Nil
         return Iif( Eval( oItem:oTree:bExpand,oItem, ;
           CheckBit( Tree_GetNotify( lParam,TREE_GETNOTIFY_ACTION ),TVE_EXPAND ) ), ;
           0, 1 )
      ENDIF
   ELSEIF nCode == -5
      oItem  := tree_Hittest( oTree:handle,,,@nAct )
      IF oTree:bRClick != Nil
         Eval( oTree:bRClick, oTree, oItem, nAct )
      ENDIF
   ENDIF
Return 0
