/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// TreeView.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*
#include "debug.ch"
#include "vxh.ch"
#define TVS_EX_DOUBLEBUFFER 0x0004
#define TVM_SETEXTENDEDSTYLE (TV_FIRST + 44)

#define UM_CHECKSTATECHANGE (WM_USER + 123)

#xcommand ODEFAULT <v> TO <x> [, <vN> TO <xN>]         ;
       => IF <v> == nil .OR. VALTYPE( <v> ) == "O"; <v> := <x> ; END            ;
          [; IF <vN> == nil ; <vN> := <xN> ; END]

//----------------------------------------------------------------------------//

CLASS TreeView FROM TitleControl
   PROPERTY DragItems                                                DEFAULT .F.
   PROPERTY HasLines        SET ::SetStyle( TVS_HASLINES, v )        DEFAULT .T.
   PROPERTY HasButtons      SET ::SetStyle( TVS_HASBUTTONS, v )      DEFAULT .T.
   PROPERTY LinesAtRoot     SET ::SetStyle( TVS_LINESATROOT, v )     DEFAULT .T.
   PROPERTY ShowSelAlways   SET ::SetStyle( TVS_SHOWSELALWAYS, v )   DEFAULT .F.
   PROPERTY CheckBoxes      SET ::SetStyle( TVS_CHECKBOXES, v )      DEFAULT .F.
   PROPERTY FullRowSelect   SET ::SetStyle( TVS_FULLROWSELECT, v )   DEFAULT .F.
   PROPERTY NoHScroll       SET ::SetStyle( TVS_NOHSCROLL, v )       DEFAULT .F.
   PROPERTY NoToolTips      SET ::SetStyle( TVS_NOTOOLTIPS, v )      DEFAULT .F.
   PROPERTY SingleExpand    SET ::SetStyle( TVS_SINGLEEXPAND, v )    DEFAULT .F.
   PROPERTY TrackSelect     SET ::SetStyle( TVS_TRACKSELECT, v )     DEFAULT .F.
   PROPERTY DisableDragDrop SET ::SetStyle( TVS_DISABLEDRAGDROP, v ) DEFAULT .F.
   PROPERTY BackColor       ROOT "Colors" SET ( _SendMessage( ::hWnd, TVM_SETBKCOLOR, 0, v ), IIF( ::IsWindowVisible(), ::InvalidateRect(),) ) DEFAULT GetSysColor( COLOR_WINDOW )
   PROPERTY ImageList       GET __ChkComponent( Self, @::xImageList );
                            SET (::xImageList := __ChkComponent( Self, v ), ::SetImageList(::xImageList))

   DATA Items            EXPORTED INIT {}
   DATA Header
   DATA OnSelDelete      EXPORTED
   DATA OnBeginLabelEdit EXPORTED
   DATA OnBeginRDrag     EXPORTED
   DATA OnLabelEdit      EXPORTED
   DATA OnEndLabelEdit   EXPORTED
   DATA Columns          EXPORTED INIT {}
   DATA Level            EXPORTED INIT -1
   DATA SelectedItem     EXPORTED
   DATA PreviousItem     EXPORTED
   DATA ClickedItem      EXPORTED

   DATA __lResetting     PROTECTED INIT .F.
   DATA __oDrag          PROTECTED

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD AddItem()
   METHOD GetSelected()
   METHOD GetItem()
   METHOD ExpandAll()
   METHOD GetSelText()                INLINE TVGetSelText( ::hWnd )
   METHOD SetBkColor( nColor )        INLINE _SendMessage( ::hWnd, TVM_SETBKCOLOR, 0, nColor )
   METHOD SetTextColor( nColor )      INLINE _SendMessage( ::hWnd, TVM_SETTEXTCOLOR, 0, nColor )
   METHOD SelectItem( hItem, nFlag )  INLINE _SendMessage( ::hWnd, TVM_SELECTITEM, IIF( nFlag == NIL, TVGN_CARET, nFlag ), hItem )
   METHOD SetScrollTime(n)            INLINE _SendMessage( ::hWnd, TVM_SETSCROLLTIME, n, 0 )
   METHOD EnsureVisible( hItem )      INLINE _SendMessage( ::hWnd, TVM_ENSUREVISIBLE, 0, hItem )
   METHOD Toggle( hItem )             INLINE _SendMessage( ::hWnd, TVM_EXPAND, TVE_TOGGLE, hItem )
   METHOD Expand( hItem )             INLINE _SendMessage( ::hWnd, TVM_EXPAND, TVE_EXPAND, hItem )

   METHOD EditLabel( hItem )          INLINE _SendMessage( ::hWnd, TVM_EDITLABEL, 0, hItem )
   METHOD SetIndent( n )              INLINE _SendMessage( ::hWnd, TVM_SETINDENT, n, 0 )
   METHOD GetCount()                  INLINE _SendMessage( ::hWnd, TVM_GETCOUNT, 0, 0 )
   METHOD GetVisibleCount()           INLINE _SendMessage( ::hWnd, TVM_GETVISIBLECOUNT, 0, 0 )
   METHOD GetRoot()                   INLINE _SendMessage( ::hWnd, TVM_GETNEXTITEM, TVGN_ROOT, 0 )
   METHOD GetFirstVisibleItem()       INLINE _SendMessage( ::hWnd, TVM_GETNEXTITEM, TVGN_FIRSTVISIBLE, 0 )
   METHOD GetLastVisibleItem()        INLINE _SendMessage( ::hWnd, TVM_GETNEXTITEM, TVGN_LASTVISIBLE, 0 )

   METHOD GetNextVisibleItem( hItem ) INLINE _SendMessage( ::hWnd, TVM_GETNEXTITEM, TVGN_NEXTVISIBLE, hItem )
   METHOD GetNextItem( hItem )        INLINE _SendMessage( ::hWnd, TVM_GETNEXTITEM, TVGN_NEXT, hItem )
   METHOD GetChild( hItem )           INLINE _SendMessage( ::hWnd, TVM_GETNEXTITEM, TVGN_CHILD, IIF( hItem==NIL, ::hWnd, hItem ) )
   METHOD SetItemHeight( nHeight )    INLINE _SendMessage( ::hWnd, TVM_SETITEMHEIGHT, nHeight, 0 )
   METHOD GetItemHeight()             INLINE _SendMessage( ::hWnd, TVM_GETITEMHEIGHT, 0, 0 )
   METHOD GetItemState()
   METHOD SetImageList()
   METHOD OnParentNotify()
   METHOD ResetContent()
   METHOD GetItemRect()
   METHOD HitTest()
   METHOD OnClick()              VIRTUAL
   METHOD OnRightClick()         VIRTUAL
   METHOD TvnKeyDown()           VIRTUAL
   METHOD SearchString()

   METHOD OnBeginDrag()          VIRTUAL
   METHOD OnEndDrag()            VIRTUAL
   METHOD OnSelChanged()         VIRTUAL
   METHOD OnSelChanging()        VIRTUAL

   METHOD __SetScrollBars() INLINE NIL
   METHOD GetExpandedCount()
   METHOD OnUserMsg()
   METHOD OnMouseMove()
   METHOD OnLButtonUp()

   METHOD MoveItem()
   METHOD ResetFrame() INLINE ::SetWindowPos(,0,0,0,0,(SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER))
   METHOD __Enable( lEnable ) INLINE EnableWindow( ::hWnd, lEnable )
ENDCLASS

//----------------------------------------------------------------------------//

METHOD Init( oParent ) CLASS TreeView
   ::ClsName := "SysTreeView32"
   ::Level   := -1
   ::Style   := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | TVS_HASLINES | TVS_HASBUTTONS | TVS_LINESATROOT | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::Border  := WS_EX_CLIENTEDGE
   DEFAULT ::__xCtrlName TO "TreeView"

   ::Super:Init( oParent )
   ::xWidth  := 160
   ::xHeight := 160

   ::Events  := ;
            { ;
            {"Mouse",       {;
                            { "OnLButtonDblClk"  , "", "" },;
                            { "OnLButtonDown"    , "", "" },;
                            { "OnLButtonUp"      , "", "" },;
                            { "OnMButtonDown"    , "", "" },;
                            { "OnMButtonUp"      , "", "" },;
                            { "OnMouseHover"     , "", "" },;
                            { "OnMouseleave"     , "", "" },;
                            { "OnMouseMove"      , "", "" },;
                            { "OnRButtonDown"    , "", "" },;
                            { "OnRButtonUp"      , "", "" } } },;
            {"Interface",   {;
                            { "OnActivate"       , "", "" },;
                            { "OnCreate"         , "", "" },;
                            { "OnDestroy"        , "", "" },;
                            { "OnEnable"         , "", "" },;
                            { "OnHideWindow"     , "", "" },;
                            { "OnKillFocus"      , "", "" },;
                            { "OnSetFocus"       , "", "" },;
                            { "OnShowWindow"     , "", "" },;
                            { "OnUserMsg"        , "", "" } } },;
            {"Keyboard",    {;
                            { "OnChar"           , "", "" },;
                            { "OnChildChar"      , "", "" },;
                            { "OnChildGetDlgCode", "", "" },;
                            { "OnChildKeyDown"   , "", "" },;
                            { "OnGetDlgCode"     , "", "" },;
                            { "OnHelp"           , "", "" },;
                            { "OnKeyDown"        , "", "" },;
                            { "OnKeyUp"          , "", "" },;
                            { "OnHotKey"         , "", "" },;
                            { "OnSysChar"        , "", "" },;
                            { "OnSysKeyDown"     , "", "" },;
                            { "OnSysKeyUp"       , "", "" } } },;
          {"Notifications", {;
                            { "Click"            , "", "" },;
                            { "ItemCheck"        , "", "" },;
                            { "RightClick"       , "", "" },;
                            { "CustomDraw"       , "", "" },;
                            { "AfterExpand"      , "", "" },;
                            { "AfterCollapse"    , "", "" },;
                            { "BeforeCollapse"   , "", "" },;
                            { "BeforeExpand"     , "", "" },;
                            { "KeyDown"          , "", "" },;
                            { "AfterSelect"      , "", "" },;
                            { "BeforeSelect"     , "", "" },;
                            { "OnBeginDrag"      , "", "" },;
                            { "OnEndDrag"        , "", "" },;
                            { "BeginRDrag"       , "", "" },;
                            { "DeleteItem"       , "", "" },;
                            { "AfterLabelEdit"   , "", "" },;
                            { "BeforeLabelEdit"  , "", "" },;
                            { "OnDropFiles"      , "", "" } } } }

RETURN Self

METHOD Create() CLASS TreeView
   IF ! ::DragItems
      ::Style := (::Style | TVS_DISABLEDRAGDROP)
   ENDIF
   ::Super:Create()
   IF ::xImageList != NIL
      ::SetImageList( ::xImageList )
   ENDIF
   IF ::xForeColor != NIL
      ::SetForeColor( ::xForeColor )
   ENDIF
   IF ::xBackColor != NIL
      ::SetBackColor( ::xBackColor )
   ENDIF

   //::SendMessage( TVM_SETEXTENDEDSTYLE, TVS_EX_DOUBLEBUFFER | TVS_CHECKBOXES, TVS_EX_DOUBLEBUFFER | TVS_CHECKBOXES )

RETURN Self

//----------------------------------------------------------------------------//

METHOD AddItem( cPrompt, nImage, aColItems ) CLASS TreeView
   LOCAL oItem

   DEFAULT nImage TO 0

   oItem := TreeViewItem( Self )
   oItem:Caption     := cPrompt
   oItem:xImageIndex := nImage
   oItem:Create()

   DEFAULT aColItems TO {}

   oItem:ColItems := aColItems //ACLONE( aColItems )
return oItem

//----------------------------------------------------------------------------//

METHOD GetExpandedCount() CLASS TreeView
   LOCAL oItem, n := LEN( ::Items )
   FOR EACH oItem IN ::Items
      IF oItem:Expanded
         n+=LEN(oItem:Items)
      ENDIF
   NEXT
RETURN n


METHOD ExpandAll() CLASS TreeView
   LOCAL oItem
   FOR EACH oItem IN ::Items
       oItem:ExpandAll()
   NEXT
RETURN Self

METHOD GetItemState( hItem, nMask ) CLASS TreeView
RETURN ::SendMessage( TVM_GETITEMSTATE, hItem, nMask )

METHOD GetItemRect( hItem, lItem )

   LOCAL rc := (struct RECT)
   DEFAULT lItem TO .F.
   rc:left := hItem

   SendMessage( ::hWnd, TVM_GETITEMRECT, lItem, @rc )

RETURN rc

//----------------------------------------------------------------------------//

FUNCTION FindTreeItem( aItems, hItem )

   LOCAL oReturn, n

   //TraceLog( hItem )

   FOR n := 1 TO LEN( aItems )
      IF aItems[n] != NIL
         IF VALTYPE( aItems[n]:Items )=="A" .AND. Len( aItems[n]:Items ) > 0
            IF ( oReturn := FindTreeItem( aItems[n]:Items, hItem ) ) != NIL
               RETURN oReturn
            ENDIF
         ENDIF

         IF aItems[n]:hItem == hItem
            RETURN aItems[n]
         ENDIF
      ENDIF
   NEXT

RETURN NIL

//----------------------------------------------------------------------------//

METHOD SearchString( cStr, cRoot ) CLASS TreeView
   LOCAL Item, oItem

   FOR EACH Item IN ::Items
       IF UPPER(Item:Caption) == UPPER(cStr) .AND. EMPTY( cRoot )
          RETURN Item
       ENDIF
       IF ( oItem := Item:SearchString( cStr, cRoot, Item:Caption ) ) != NIL
          RETURN oItem
       ENDIF
   NEXT

RETURN NIL
//----------------------------------------------------------------------------//

METHOD ResetContent() CLASS TreeView
   //LOCAL n
   ::__lResetting := .T.
   SendMessage( ::hWnd, TVM_DELETEITEM, 0, TVI_ROOT  )
   //FOR n := 1 TO LEN( ::Items )
   //   ::Items[n]:Cargo := NIL
   //   ::Items[n]:ColItems := NIL
   //   ::Items[n]:Delete()
   //NEXT
   ::Items := {}
   ::__lResetting := .F.
RETURN Self

//----------------------------------------------------------------------------//

METHOD GetSelected() CLASS TreeView
return FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )

//----------------------------------------------------------------------------//

METHOD GetItem(n) CLASS TreeView
return( ::Items[n] )

//----------------------------------------------------------------------------//

METHOD SetImageList( oList ) CLASS TreeView
   IF ::hWnd != NIL
      TVSetImageList( ::hWnd, IIF( oList != NIL, oList:handle, NIL ) , 0 )
   ENDIF
RETURN Self

//----------------------------------------------------------------------------//
METHOD OnUserMsg( hWnd, nMsg, nwParam, nlParam ) CLASS TreeView
   (hWnd,nwParam)
   IF nMsg == UM_CHECKSTATECHANGE
      ExecuteEvent( "ItemCheck", Self, FindTreeItem( ::Items, nlParam ) )
      RETURN 0
   ENDIF
RETURN Self

//----------------------------------------------------------------------------//
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS TreeView
   LOCAL tvht, rc //, tvdi
   LOCAL tvkd, nState, lRet, nPos, hItem//, nChecked
   DEFAULT hdr TO ::Parent:hdr

   IF ::__lResetting
      RETURN 0
   ENDIF

   DO CASE
      CASE hdr:code == NM_CUSTOMDRAW
           ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )
           lRet := ExecuteEvent( "CustomDraw", Self, nlParam )

      CASE hdr:code == NM_RCLICK
           ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )
           ::OnRightClick( nwParam, nlParam )
           lRet := ExecuteEvent( "RightClick", Self )

      CASE hdr:code == NM_CLICK
           ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )
           tvht := (struct TVHITTESTINFO)
           nPos := GetMessagePos()

           tvht:pt:x := GET_X_LPARAM( nPos )
           tvht:pt:y := GET_Y_LPARAM( nPos )

           MapWindowPoints( HWND_DESKTOP, ::hWnd, @tvht:pt, 1 )

           SendMessage( ::hWnd, TVM_HITTEST, 0, @tvht )
           IF (tvht:flags & TVHT_ONITEMSTATEICON) == TVHT_ONITEMSTATEICON
              PostMessage( ::hWnd, UM_CHECKSTATECHANGE, 0, tvht:hItem )
           ELSE
              tvht:flags := (TVHT_ONITEM | TVHT_ONITEMICON)
              GetCursorPos( @tvht:pt )
              ScreenToClient( ::hWnd, @tvht:pt )

              ::ClickedItem := NIL
              hItem := SendMessage( ::hWnd, TVM_HITTEST, 0, tvht )
              IF hItem != 0
                 rc := ::GetItemRect( hItem, .T. )
                 IF ::ImageList != NIL .AND. ::ImageList:Handle != NIL
                    rc:Left -= ::ImageList:IconWidth
                 ENDIF
                 IF !PtInRect( rc, tvht:pt )
                    hItem := 0
                 ENDIF
              ENDIF
              IF hItem != NIL
                 ::ClickedItem := FindTreeItem( ::Items, hItem )
              ENDIF
              ::OnClick()
              lRet := ExecuteEvent( "Click", Self )
           ENDIF

      CASE hdr:code == TVN_BEGINDRAG
           IF ::DragItems
              ::__oDrag := FindTreeItem( ::Items, __GetTreeViewNewItem( nlParam ) )
              ::__oDrag:Select()
              ::OnBeginDrag( ::__oDrag )
              lRet := ExecuteEvent( "OnBeginDrag", Self, ::__oDrag )
           ENDIF

      CASE hdr:code == TVN_BEGINRDRAG
           IF ( ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) ) ) != NIL
              __Evaluate( ::OnBeginRDrag, ::SelectedItem )
           ENDIF
           lRet := ExecuteEvent( "BeginRDrag", Self )

      CASE hdr:code == TVN_DELETEITEM
           IF ( ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) ) ) != NIL
              __Evaluate( ::OnSelDelete, ::SelectedItem )
           ENDIF
           lRet := ExecuteEvent( "DeleteItem", Self )

      CASE hdr:code == TVN_ENDLABELEDIT
           IF ( ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) ) ) != NIL
              __Evaluate( ::OnEndLabelEdit, ::SelectedItem )
           ENDIF
           lRet := ExecuteEvent( "AfterLabelEdit", Self )

      CASE hdr:code == TVN_BEGINLABELEDIT
           IF ( ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) ) ) != NIL
              __Evaluate( ::OnLabelEdit, ::SelectedItem )
           ENDIF
           lRet := ExecuteEvent( "BeforeLabelEdit", Self )

      CASE hdr:code == TVN_GETDISPINFO
      CASE hdr:code == TVN_GETINFOTIP

      CASE hdr:code == TVN_ITEMEXPANDED
           IF ( ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) ) ) != NIL
              nState := ::SelectedItem:GetItemState( TVIF_STATE )
           ENDIF
           IF nState != NIL
              lRet := ExecuteEvent( IIF( (nState & TVIS_EXPANDED) == TVIS_EXPANDED, "AfterExpand", "AfterCollapse" ), Self )
           ENDIF

      CASE hdr:code == TVN_ITEMEXPANDING
           IF ( ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) ) ) != NIL
              nState := ::SelectedItem:GetItemState( TVIF_STATE )
           ENDIF
           IF nState != NIL
              lRet := ExecuteEvent( IIF( (nState & TVIS_EXPANDED) == TVIS_EXPANDED, "BeforeCollapse", "BeforeExpand" ), Self )
           ENDIF

      CASE hdr:code == TVN_KEYDOWN
           IF ( ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) ) ) != NIL
              tvkd := (struct NMTVKEYDOWN*) nlParam
              ::TVNKeyDown( tvkd:wVKey, ::SelectedItem )
              ::wParam := tvkd:wVKey

              IF tvkd:wVKey == VK_SPACE .AND. ::CheckBoxes
                 PostMessage( ::hWnd, UM_CHECKSTATECHANGE, 0, ::SelectedItem:hItem )
              ELSE
                 lRet := ExecuteEvent( "KeyDown", Self )
              ENDIF
           ENDIF

      CASE hdr:code == TVN_SELCHANGED
           IF ( ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) ) ) != NIL
              hItem := __GetTreeViewOldItem( nlParam )
              ::PreviousItem := FindTreeItem( ::Items, hItem )
              __Evaluate( ::Action, ::SelectedItem )
              __Evaluate( ::SelectedItem:Action, ::SelectedItem )
              ::OnSelChanged( ::SelectedItem )
           ENDIF
           lRet := ExecuteEvent( "AfterSelect", Self )
           ODEFAULT lRet TO .F.

      CASE hdr:code == TVN_SELCHANGING
           IF ( ::SelectedItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) ) ) != NIL
              ::OnSelChanging( ::SelectedItem )
              lRet := ExecuteEvent( "BeforeSelect", Self )
              ODEFAULT lRet TO .F.
           ENDIF

      CASE hdr:code == TVN_SETDISPINFO
      CASE hdr:code == TVN_SINGLEEXPAND

   ENDCASE
   IF VALTYPE( lRet ) == "O"
      lRet := NIL
   ENDIF
RETURN lRet

//----------------------------------------------------------------------------------------------------------
METHOD HitTest( x, y ) CLASS TreeView
   LOCAL hItem, oItem, tvht
   tvht := (struct TVHITTESTINFO)
   tvht:pt:x := x
   tvht:pt:y := y
   hItem := SendMessage( ::hWnd, TVM_HITTEST, 0, tvht )
   oItem := FindTreeItem( ::Items, hItem )
RETURN oItem

//----------------------------------------------------------------------------------------------------------
METHOD OnMouseMove( nwParam, nlParam ) CLASS TreeView
   LOCAL oItem, pt

   ::Super:OnMouseMove( nwParam, nlParam )

   IF nwParam == MK_LBUTTON .AND. ::DragItems .AND. ::__oDrag != NIL
      pt := (struct POINT)
      pt:x := LOWORD( nlParam )
      pt:y := HIWORD( nlParam )
      IF ( oItem := ::HitTest( pt:x, pt:y ) ) != NIL .AND. ::__oDrag:hItem != oItem:hItem
         SendMessage( ::hWnd, TVM_SETINSERTMARK, .T., oItem:hItem )
      ENDIF

   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------
METHOD OnLButtonUp( w, x, y ) CLASS TreeView
   LOCAL oItem
   (w)
   IF ::DragItems .AND. ::__oDrag != NIL
      SendMessage( ::hWnd, TVM_SETINSERTMARK, .F., NIL )
      IF ( oItem := ::HitTest( x, y ) ) != NIL .AND. ::__oDrag:hItem != oItem:hItem
         ::OnEndDrag( oItem )
         ExecuteEvent( "OnEndDrag", Self, oItem )
         ::__oDrag := NIL
      ENDIF
      ::InvalidateRect()
      oItem:Select()
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------
METHOD MoveItem( oDrag, nPos, oOwner ) CLASS TreeView
   LOCAL oItem
   IF oDrag != NIL .AND. oOwner != NIL
      oDrag:Delete()

      oItem             := TreeViewItem( Self )
      oItem:Text        := oDrag:Text
      oItem:ImageIndex  := oDrag:ImageIndex
      oItem:Cargo       := IIF( VALTYPE(oDrag:Cargo) == "A", ACLONE( oDrag:Cargo ), oDrag:Cargo )

      oItem:InsertAfter := IIF( nPos == 1, TVI_FIRST, IIF( LEN( oOwner:Items ) < nPos-1, TVI_LAST, oOwner:Items[nPos-1]:hItem ) )
      oItem:Owner       := oOwner

      oItem:Create()
   ENDIF
RETURN oItem
