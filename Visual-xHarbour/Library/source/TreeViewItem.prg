/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// TreeViewItem.prg                                                                                     *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"

//----------------------------------------------------------------------------//

CLASS TreeViewItem INHERIT Object
   DATA hItem          EXPORTED
   DATA Items          EXPORTED INIT {}
   DATA Parent         EXPORTED
   DATA Expanded       EXPORTED INIT .F.
   DATA tvi            PROTECTED
   DATA ColItems
   DATA Level          EXPORTED
   DATA Action         EXPORTED
   DATA HotImageIndex  EXPORTED
   DATA PointerItem    EXPORTED
   DATA InsertAfter    EXPORTED INIT TVI_LAST

   PROPERTY Bold    INDEX TVIS_BOLD READ xBold    WRITE SetItemState PROTECTED
   PROPERTY Caption                 READ xCaption WRITE SetItemText
   PROPERTY ImageIndex              READ xImageIndex WRITE SetImageIndex DEFAULT 0
   
   ACCESS Expanded INLINE ::Parent:SendMessage( TVM_GETITEMSTATE, ::hItem, TVIF_STATE ) & TVIS_EXPANDED == TVIS_EXPANDED
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD AddItem()
   METHOD Delete()
   METHOD Toggle()              INLINE IIF( ::Parent != NIL, ::Parent:SendMessage( TVM_EXPAND, TVE_TOGGLE, ::hItem ),)
   METHOD Expand()              INLINE IIF( ::Parent != NIL, ::Parent:SendMessage( TVM_EXPAND, TVE_EXPAND, ::hItem ),)
   METHOD ExpandAll()
   METHOD SortChildren( lRec )  INLINE IIF( ::Parent != NIL, ::Parent:SendMessage( TVM_SORTCHILDREN, IFNIL( lRec, .F., lRec ), ::hItem ),)

   METHOD Select(nFlag)         INLINE IIF( ::Parent != NIL, ::Parent:SelectItem( ::hItem, nFlag ),)
   METHOD EnsureVisible()       INLINE IIF( ::Parent != NIL, ::Parent:EnsureVisible( ::hItem ),)
   METHOD EditLabel()           INLINE IIF( ::Parent != NIL, ::Parent:Editlabel( ::hItem ),)

   METHOD SetFont( hFont )      INLINE SendMessage( ::hItem, WM_SETFONT, hFont, 0 )

   METHOD GetItemState( nMask ) INLINE IIF( ::Parent != NIL, ::Parent:GetItemState( ::hItem, nMask ),)
   METHOD GetItemRect(lItem)    INLINE IIF( ::Parent != NIL, ::Parent:GetItemRect( ::hItem, lItem ),)
   METHOD SetItemState()
   METHOD GetNextItem()         INLINE IIF( ::Parent != NIL, ::Parent:GetNextItem( ::hItem ),)
   METHOD GetChild()            INLINE IIF( ::Parent != NIL, ::Parent:GetChild( ::hItem ),)
   METHOD SetStyle()
   METHOD SearchString()
   METHOD CreateDragImage()     INLINE ::Parent:DragImage := ImageList( Self,,,, .F.), ::Parent:DragImage:Handle := ::Parent:SendMessage( TVM_CREATEDRAGIMAGE, 0, ::hItem )
   METHOD SetItemText()
   METHOD GetExpandedCount()
   METHOD SetImageIndex()
   METHOD SetOwner()
ENDCLASS

//----------------------------------------------------------------------------//

METHOD Init( oParent ) CLASS TreeViewItem
   DEFAULT ::__xCtrlName TO "TreeViewItem"
   ::Items  := {}
   ::Parent := oParent
RETURN Self

METHOD Create() CLASS TreeViewItem
   LOCAL nPos, tvis, cName, oOwner := ::Owner
   DEFAULT oOwner TO ::Parent
   DEFAULT nPos   TO 0
   tvis := (struct TV_INSERTSTRUCT)

   tvis:hParent             := IIF( ::Owner != NIL, ::Owner:hItem, )
   tvis:hInsertAfter        := ::InsertAfter

   tvis:item:mask           := TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE
   tvis:item:pszText        := ::Caption
   tvis:item:cchTextMax     := MAX_PATH + 1
   tvis:item:iImage         := ::ImageIndex-1
   tvis:item:iSelectedImage := IIF( ::HotImageIndex == NIL, ::ImageIndex-1, ::HotImageIndex-1 )

   ::hItem := SendMessage( ::Parent:hWnd, TVM_INSERTITEM, 0, tvis )
   
   nPos := 0
   IF ::InsertAfter != TVI_LAST
      IF ::InsertAfter == TVI_FIRST
         nPos := 1
       ELSE
         IF ( nPos := ASCAN( oOwner:Items, {|o|o:hItem == ::InsertAfter} ) ) > 0
            IF nPos == LEN( ::Items )
               nPos := 0
             ELSE
               nPos ++
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF nPos == 0
      AAdd( oOwner:Items, Self )
    ELSE
      AINS( oOwner:Items, nPos, Self, .T. )
   ENDIF

   ::Owner      := oOwner
   ::Level      := oOwner:Level + 1

   AEVAL( ::Items, {|o| IIF( valtype(o)=="O", o:Create(),)} )

RETURN Self

METHOD ExpandAll() CLASS TreeViewItem
   LOCAL oItem
   SendMessage( ::Parent:hWnd, TVM_EXPAND, TVE_EXPAND, ::hItem )
   FOR EACH oItem IN ::Items
       oItem:ExpandAll()
   NEXT
RETURN Self

METHOD GetExpandedCount() CLASS TreeViewItem
   LOCAL oItem, n := 0
   FOR EACH oItem IN ::Items
      IF oItem:Expanded
         n+=LEN(oItem:Items)
      ENDIF
   NEXT
RETURN n

//----------------------------------------------------------------------------//

METHOD SetStyle( nStyle, xValue )
   ::Parent:SendMessage( nStyle, xValue, 0 )
RETURN Self

//----------------------------------------------------------------------------//

METHOD SearchString(  cStr, cRoot, cParent ) CLASS TreeViewItem
   LOCAL Item, oItem
   FOR EACH Item IN ::Items
       IF UPPER(Item:Caption) == UPPER(cStr)
          IF cRoot == NIL .OR. ( cParent != NIL .AND. cRoot == cParent )
             RETURN Item
          ENDIF
       ENDIF
       IF ( oItem := Item:SearchString( cStr, cRoot, Item:Caption ) ) != NIL
          RETURN oItem
       ENDIF
   NEXT

RETURN NIL

//----------------------------------------------------------------------------//

METHOD SetItemState( nState ) CLASS TreeViewItem
   ::tvi := (struct TVITEM)
   ::tvi:mask  := TVIF_HANDLE + TVIF_STATE
   ::tvi:hItem := ::hItem
   ::tvi:state := nState
   ::tvi:stateMask := TVIS_BOLD + TVIS_CUT + TVIS_DROPHILITED + TVIS_EXPANDEDONCE + TVIS_EXPANDPARTIAL + TVIS_SELECTED + TVIS_OVERLAYMASK + TVIS_STATEIMAGEMASK + TVIS_USERMASK

   SendMessage( ::Parent:hWnd, TVM_SETITEM, 0, ::tvi )

RETURN Self

//----------------------------------------------------------------------------//

METHOD AddItem( cPrompt, nImage, aColItems ) CLASS TreeViewItem
   LOCAL oItem

   DEFAULT nImage TO 0
   oItem := TreeViewItem( ::Parent )
   oItem:Caption     := cPrompt
   oItem:xImageIndex := nImage
   oItem:Owner       := Self
   oItem:Create()

   DEFAULT aColItems TO {}

   oItem:ColItems    := aColItems //ACLONE( aColItems )

return oItem

//----------------------------------------------------------------------------//

METHOD SetOwner( oOwner ) CLASS TreeViewItem
   LOCAL n
   IF ::Owner != NIL .AND. ( n := ASCAN( ::Owner:Items, {|o|o:hItem == ::hItem } ) ) > 0
      ADEL( ::Owner:Items, n, .T. )
   ENDIF
   TVDeleteItem( ::Parent:handle, ::hItem )
   ::Owner := oOwner
   ::Create()
   ::Expand()
   ::EnsureVisible()
   ::Select()
RETURN Self

//----------------------------------------------------------------------------//

METHOD Delete() CLASS TreeViewItem
   LOCAL n
   FOR n := 1 TO LEN( ::Items )
       ::Items[n]:Delete()
   NEXT
   
   IF ::Owner != NIL .AND. ( n := ASCAN( ::Owner:Items, {|o|o:hItem == ::hItem } ) ) > 0
      ADEL( ::Owner:Items, n, .T. )
   ENDIF

   TRY
      TVDeleteItem( ::Parent:handle, ::hItem )
   CATCH
   END
   ::Cargo    := NIL
   ::ColItems := NIL
   ::Parent   := NIL
   ::Owner    := NIL
   ::Items := {}
return( len( ::Items) )

//----------------------------------------------------------------------------//

METHOD SetItemText( cText ) CLASS TreeViewItem
   LOCAL tvi
   IF ::Parent:hWnd != NIL
      tvi := (struct TVITEM)
      tvi:mask    := TVIF_TEXT
      tvi:hItem   := ::hItem
      tvi:pszText := cText
      SendMessage( ::Parent:hWnd, TVM_SETITEM, 0, tvi )
   ENDIF
RETURN Self

METHOD SetImageIndex(n) CLASS TreeViewItem
   LOCAL tvi
   IF ::Parent:hWnd != NIL .AND. ::hItem != NIL
      tvi := (struct TVITEM)
      tvi:mask    := TVIF_IMAGE | TVIF_SELECTEDIMAGE
      tvi:hItem   := ::hItem
      tvi:iImage  := n-1
      tvi:iSelectedImage := IIF( ::HotImageIndex == NIL, n-1, ::HotImageIndex-1 )
      SendMessage( ::Parent:hWnd, TVM_SETITEM, 0, tvi )
   ENDIF
RETURN Self
