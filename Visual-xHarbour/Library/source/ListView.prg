/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// ListView.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"
#include "colors.ch"

#define LVGF_HEADER             0x00000001
#define LVGF_ALIGN              0x00000008
#define LVGF_GROUPID            0x00000010

#define LVGA_HEADER_LEFT        0x00000001
#define LVGA_HEADER_CENTER      0x00000002
#define LVGA_HEADER_RIGHT       0x00000004

#define LVM_INSERTGROUP         (LVM_FIRST + 145)
#define LVM_SETGROUPINFO        (LVM_FIRST + 147)
#define LVM_REMOVEGROUP         (LVM_FIRST + 150)
#define LVM_MOVEITEMTOGROUP     (LVM_FIRST + 154)
#define LVM_ENABLEGROUPVIEW     (LVM_FIRST + 157)

#define LVGS_NORMAL             0x00000000
#define LVGS_COLLAPSED          0x00000001
#define LVS_EX_GRADIENT         0x20000000
#define LVIF_GROUPID            0x0100

#define LV_VIEW_ICON            0x0000
#define LV_VIEW_DETAILS         0x0001
#define LV_VIEW_SMALLICON       0x0002
#define LV_VIEW_LIST            0x0003
#define LV_VIEW_TILE            0x0004

#define LVM_SETTILEWIDTH        (LVM_FIRST + 141)
#define LVM_SETVIEW             (LVM_FIRST + 142)
#define LVM_SETTILEINFO         (LVM_FIRST + 164)
#define LVM_SETTILEVIEWINFO     (LVM_FIRST + 162)

#define LVTVIF_AUTOSIZE         0x00000000
#define LVTVIF_FIXEDWIDTH       0x00000001
#define LVTVIF_FIXEDHEIGHT      0x00000002
#define LVTVIF_FIXEDSIZE        0x00000003

#define LVTVIM_TILESIZE         0x00000001
#define LVTVIM_COLUMNS          0x00000002
#define LVTVIM_LABELMARGIN      0x00000004


CLASS ListView INHERIT Control
   DATA LvExStyle             EXPORTED INIT 0
   DATA Items
   DATA CurPos                EXPORTED INIT 0
   DATA Columns INIT {}
   DATA Groups  INIT {=>}
   DATA __pSortProc PROTECTED
   
   PROPERTY ImageList      GET __ChkComponent( Self, ::xImageList )      SET SetImageList
   PROPERTY ImageListSmall GET __ChkComponent( Self, ::xImageListSmall ) SET SetImageListSmall

   PROPERTY DataSource  GET __ChkComponent( Self, ::xDataSource ) SET SetDataSource

   PROPERTY Gradient       INDEX LVS_EX_GRADIENT      READ xGradient       WRITE SetLVExStyle DEFAULT .F. PROTECTED
   PROPERTY FlatScrollBar  INDEX LVS_EX_FLATSB        READ xFlatScrollBar  WRITE SetLVExStyle DEFAULT .F. PROTECTED
   PROPERTY GridLines      INDEX LVS_EX_GRIDLINES     READ xGridLines      WRITE SetLVExStyle DEFAULT .F. PROTECTED
   PROPERTY FullRowSelect  INDEX LVS_EX_FULLROWSELECT READ xFullRowSelect  WRITE SetLVExStyle DEFAULT .F. PROTECTED
   PROPERTY AlignLeft      INDEX LVS_ALIGNLEFT        READ xAlignLeft      WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY AlignMask      INDEX LVS_ALIGNMASK        READ xAlignMask      WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY AlignTop       INDEX LVS_ALIGNTOP         READ xAlignTop       WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY NoColumnHeader INDEX LVS_NOCOLUMNHEADER   READ xNoColumnHeader WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY NoLabelWrap    INDEX LVS_NOLABELWRAP      READ xNoLabelWrap    WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY NoScroll       INDEX LVS_NOSCROLL         READ xNoScroll       WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY NoSortHeader   INDEX LVS_NOSORTHEADER     READ xNoSortHeader   WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY OwnerDrawFixed INDEX LVS_OWNERDRAWFIXED   READ xOwnerDrawFixed WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY OwnerData      INDEX LVS_OWNERDATA        READ xOwnerData      WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY EditLabels     INDEX LVS_EDITLABELS       READ xEditLabels     WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY AutoArrange    INDEX LVS_AUTOARRANGE      READ xAutoArrange    WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY ShowSelAlways  INDEX LVS_SHOWSELALWAYS    READ xShowSelAlways  WRITE SetStyle     DEFAULT .F. PROTECTED
   PROPERTY SingleSel      INDEX LVS_SINGLESEL        READ xSingleSel      WRITE SetStyle     DEFAULT .F. PROTECTED
   
   PROPERTY BackColor                                 READ xBackColor      WRITE SetBackColor             PROTECTED
   PROPERTY ForeColor                                 READ xForeColor      WRITE SetForeColor             PROTECTED
   PROPERTY ViewStyle                                 READ xViewStyle      WRITE __SetViewStyle DEFAULT LVS_ICON  PROTECTED INVERT

   DATA EnumViewStyle EXPORTED  INIT { { "Icon", "Report", "SmallIcon", "List", "Tile" }, {LV_VIEW_ICON,LV_VIEW_DETAILS,LV_VIEW_SMALLICON,LV_VIEW_LIST,LV_VIEW_TILE} }

   DATA nCurRec             PROTECTED INIT -1

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD InsertItem()
   METHOD InsertItems()
   METHOD SetLVExStyle()
   METHOD AddColumn()
   METHOD DeleteColumn(nCol)            INLINE ListViewDeleteColumn( ::hWnd, nCol )
   METHOD GetCurPos()                   INLINE ListViewGetNextItem( ::hWnd, -1, LVNI_FOCUSED )+1
   METHOD SetItemText()
   METHOD SetImageList()
   METHOD SetImageListSmall()
   METHOD SetCurSel()
   METHOD ResetContent()                INLINE ::SendMessage( LVM_DELETEALLITEMS, 0, 0 )
   METHOD GetHotItem()                  INLINE ::SendMessage( LVM_GETHOTITEM, 0, 0 )
   METHOD EnsureVisible( nItem, lPart ) INLINE ::SendMessage( LVM_ENSUREVISIBLE, nItem, lPart )
   METHOD OnParentNotify()
   //METHOD OnLVNSetDispInfo() VIRTUAL
   //METHOD OnLVNGetDispInfo() VIRTUAL
   METHOD SetDataSource()
   METHOD GetVirtualValue()
   METHOD OnSize() INLINE ::InvalidateRect(), NIL
   METHOD AutoAddColumns() INLINE Self
   METHOD SortItems()
   METHOD SetBackColor()
   METHOD SetForeColor()
   METHOD FindItem()
   METHOD GetSearchString()
   METHOD __SetScrollBars()                INLINE Self
   METHOD __SetViewStyle()
ENDCLASS

//-------------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ListView
   DEFAULT ::__xCtrlName TO "ListView"
   ::Style        := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ClsName      := "SysListView32"
   ::Super:Init( oParent )
   ::Width        := 245
   ::Height       := 153
//   ::ExStyle      := WS_EX_STATICEDGE
   ::ClientEdge   := .T.
   IF ::__ClassInst != NIL
      ::__ClassInst:xClientEdge   := .T.
   ENDIF
   ::ClipSiblings := .T.
   ::TabStop      := .T.
   ::SmallCaption := .T.

   __DeleteEvents( ::Events,{ "OnLButtonDown",;
                              "OnLButtonUp",;
                              "OnMButtonDown",;
                              "OnMButtonUp",;
                              "OnMouseActivate",;
                              "OnClear",;
                              "OnRButtonDown" } )

RETURN self

//-------------------------------------------------------------------------------------------------------

METHOD Create( lNew ) CLASS ListView
   ::Super:Create()
   DEFAULT lNew TO .F.

   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "Add Group", {|o| o:=ListViewGroup( Self ),;
                                                   ::Application:Project:Modified := .T.,;
                                                   o:Caption := o:Name,;
                                                   o:Create() } }}
   ENDIF

   IF !EMPTY( ::Caption )
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
   ENDIF
   IF ::ImageList != NIL
      ::SetImageList( ::ImageList )
   ENDIF
   IF ::ImageListSmall != NIL
      ::SetImageListSmall( ::ImageListSmall )
   ENDIF
   IF ::xDataSource != NIL
      ::SetDataSource( ::xDataSource )
   ENDIF

   IF ::LvExStyle != 0
      ::SendMessage( LVM_SETEXTENDEDLISTVIEWSTYLE, 0, ::LvExStyle )
   ENDIF
   IF ::xForeColor != NIL
      ::SetForeColor( ::xForeColor )
   ENDIF
   IF ::xBackColor != NIL
      ::SetBackColor( ::xBackColor )
   ENDIF
   
   ::__SetViewStyle()
   DEFAULT ::xBackColor TO ::SendMessage( LVM_GETBKCOLOR, 0, 0 )
   DEFAULT ::xForeColor TO ::SendMessage( LVM_GETTEXTCOLOR, 0, 0 )

RETURN Self

METHOD SetForeColor(n) CLASS ListView
   IF ::hWnd != NIL
      ::SendMessage( LVM_SETTEXTCOLOR, 0, n )
      ::InvalidateRect()
   ENDIF
RETURN Self


METHOD SetBackColor(n) CLASS ListView
   IF ::hWnd != NIL
      ::SendMessage( LVM_SETBKCOLOR, 0, n )
      ::SendMessage( LVM_SETTEXTBKCOLOR, 0, n )
      ::InvalidateRect()
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD SetCurSel( n ) CLASS ListView
   LOCAL lvi     := (struct LVITEM)
   lvi:mask      := 0x000F
   lvi:stateMask := LVIS_SELECTED | LVIS_FOCUSED
   lvi:iItem     := n-1
   lvi:state     := LVIS_SELECTED | LVIS_FOCUSED
RETURN ::SendMessage( LVM_SETITEMSTATE, n-1, lvi )

//-------------------------------------------------------------------------------------------------------

METHOD GetSearchString() CLASS ListView
   LOCAL n, cStr
   n := SendMessage( ::hWnd, LVM_GETISEARCHSTRING, 0, NIL )
   IF n > 0
      cStr := SPACE(n)
      SendMessage( ::hWnd, LVM_GETISEARCHSTRING, 0, @cStr )
   ENDIF
   DEFAULT cStr TO ""
RETURN cStr

//-------------------------------------------------------------------------------------------------------

METHOD SetDataSource( oSource ) CLASS ListView
   LOCAL aField, n, nAlign
   oSource := __ChkComponent( Self, oSource )
   
   ::xDataSource := oSource

   IF VALTYPE( oSource )=="O" .AND. oSource:IsOpen
      ::OwnerData := .T.
      // Clear previous Columns
      FOR n := 1 TO LEN( ::Columns )
          ::Columns[n]:Destroy()
          n--
      NEXT
      // Insert New Columns
      FOR EACH aField IN oSource:Structure
          DO CASE
             CASE aField[2]=="C"
                  nAlign := LVCFMT_LEFT
             CASE aField[2]=="D"
                  nAlign := LVCFMT_CENTER
             CASE aField[2]=="L"
                  nAlign := LVCFMT_CENTER
             CASE aField[2]=="N"
                  nAlign := LVCFMT_RIGHT
          ENDCASE
          ::AddColumn( __Proper( aField[1] ), MAX( aField[3], LEN(aField[1])+2 )*7, nAlign )
      NEXT
      ListViewDeleteAllItems( ::hWnd )
      ListViewSetItemCount( ::hWnd, oSource:RecCount(), LVSICF_NOINVALIDATEALL | LVSICF_NOSCROLL)
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD __SetViewStyle( n ) CLASS ListView
   LOCAL lvti
   DEFAULT n TO ::ViewStyle
   IF IsWindow( ::hWnd  )
      SendMessage( ::hWnd, LVM_SETVIEW, n, 0 )

      lvti := (struct LVTILEVIEWINFO)
      lvti:cbSize  := lvti:SizeOf()
      lvti:dwMask  := LVTVIM_TILESIZE | LVTVIM_COLUMNS
      lvti:dwFlags := LVTVIF_AUTOSIZE
      lvti:cLines  := 1
      SendMessage( ::hWnd, LVM_SETTILEVIEWINFO, 0, lvti )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD SetItemText( nRow, nCol, cText ) CLASS ListView
   LOCAL lvi := (struct LVWITEM)
   DEFAULT nRow TO ::SendMessage( LVM_GETITEMCOUNT, 0, 0 )
   DEFAULT nCol TO 0
   lvi:mask       := LVIF_TEXT
   lvi:iSubItem   := nCol
   lvi:pszText    := cText
   SendMessage( ::hWnd, LVM_SETITEMTEXT, nRow, lvi )
RETURN Self
//-------------------------------------------------------------------------------------------------------

METHOD SetLVExStyle( nStyle, lAdd ) CLASS ListView
   DEFAULT lAdd TO .T.
   IF ::IsWindow()
      ::LvExStyle := ::SendMessage( LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0 )
   ENDIF
   IF lAdd
      ::LvExStyle := ::LvExStyle | nStyle
    ELSE
      ::LvExStyle := ::LvExStyle & NOT( nStyle )
   ENDIF
   IF ::IsWindow()
      ::SendMessage( LVM_SETEXTENDEDLISTVIEWSTYLE, 0, ::LvExStyle )
   ENDIF
RETURN Self

METHOD FindItem( cItem ) CLASS ListView
   LOCAL lvfi       := (struct LVFINDINFO)
   lvfi:flags       := LVFI_PARTIAL | LVFI_STRING
   lvfi:lParam      := NIL
   lvfi:psz         := cItem
   lvfi:vkDirection := VK_DOWN
RETURN ::SendMessage( LVM_FINDITEM, -1, lvfi )


//-------------------------------------------------------------------------------------------------------

METHOD SetImageList( oList ) CLASS ListView
   oList := __ChkComponent( Self, oList )
   IF ::hWnd != NIL
      ::SendMessage( LVM_SETIMAGELIST, LVSIL_NORMAL, IIF( oList != NIL, oList:Handle, NIL ) )
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------

METHOD SetImageListSmall( oList ) CLASS ListView
   oList := __ChkComponent( Self, oList )
   IF ::hWnd != NIL
      ::SendMessage( LVM_SETIMAGELIST, LVSIL_SMALL, IIF( oList != NIL, oList:Handle, NIL ) )
   ENDIF
RETURN Self

//--------------------------------------------------------------------------------------------------------

METHOD SortItems( nColumn, lAscending ) CLASS ListView
   IF ::hWnd != NIL
      __ListViewSortColumn( ::hWnd, nColumn, lAscending )
   ENDIF
RETURN Self
    
//--------------------------------------------------------------------------------------------------------

METHOD AddColumn( cCaption, nWidth, nAlign )
   LOCAL oColumn := ListViewColumn( Self, cCaption, nWidth, nAlign, .T. )
RETURN Self

//--------------------------------------------------------------------------------------------------------

METHOD GetVirtualValue( nItem, nSubItem ) CLASS ListView
   LOCAL cText := ""
   IF nSubItem >= 0
      IF nSubItem == 0 .AND. nItem <> ::nCurRec
         IF ::nCurRec != -1
            IF nItem == 0
               ::DataSource:GoTop()
              ELSE
               ::DataSource:Skip( nItem - ::nCurRec )
            ENDIF
         ENDIF
         ::nCurRec := nItem
      ENDIF
      TRY
         cText := ::DataSource:Structure[ nSubItem+1 ][1]
         cText := XStr( ::DataSource:Fields:&cText )
      CATCH
      END
   ENDIF
RETURN cText

//--------------------------------------------------------------------------------------------------------

METHOD OnParentNotify( nwParam, nlParam ) CLASS ListView
   LOCAL nmia, pnkd, lCopy := .F., lpnmh := (struct NMHDR*) nlParam, pnmv
   (nwParam)
   SWITCH lpnmh:code
      CASE NM_RCLICK 
           nmia := (struct NMITEMACTIVATE*) nlParam
           ::CurPos := nmia:iItem + 1
           ExecuteEvent( "OnRButtonUp", Self )
           EXIT

      CASE NM_CLICK 
           nmia := (struct NMITEMACTIVATE*) nlParam
           ::CurPos := nmia:iItem + 1
           ExecuteEvent( "OnClick", Self )
           EXIT
           
      CASE LVN_KEYDOWN
           pnkd = (struct NMLVKEYDOWN*) nlParam
           ::wParam := pnkd:wVKey
           ExecuteEvent( "OnKeyDown", Self )
           EXIT

      CASE LVN_ITEMCHANGED
           pnmv := (struct NMLISTVIEW*) nlParam
           ::CurPos := pnmv:iItem + 1
           ExecuteEvent( "OnItemChanged", Self )
           EXIT
      CASE LVN_ITEMACTIVATE

      CASE LVN_GETDISPINFO
           IF ::DataSource != NIL .AND. ::DataSource:IsOpen
              ListViewNotify( Self, "GetVirtualValue", nlParam )
           ENDIF
   END
RETURN 0

//--------------------------------------------------------------------------------------------------------

METHOD InsertItem( cText, nImage, nRow, nGroup ) CLASS ListView
   LOCAL lvi := (struct LVWITEM)

   DEFAULT nImage TO 0
   DEFAULT nRow   TO ::SendMessage( LVM_GETITEMCOUNT, 0, 0 ) + 1
   lvi:mask       := LVIF_TEXT | LVIF_IMAGE
   IF nGroup != NIL
      lvi:mask := lvi:mask | LVIF_GROUPID
      lvi:iGroupId   := nGroup
   ENDIF
   lvi:iItem      := nRow
   lvi:iSubItem   := 0
   lvi:iImage     := nImage
   lvi:pszText    := cText
   lvi:cchTextMax := 256

   SendMessage( ::hWnd, LVM_INSERTITEM, 0, lvi )
return(self)

METHOD InsertItems() CLASS ListView
   ListViewDeleteAllItems(::hWnd)
   ListViewSetItemCount(::hWnd, LEN( ::Items ), LVSICF_NOINVALIDATEALL+LVSICF_NOSCROLL)
return(self)

//----------------------------------------------------------------------------------------------

CLASS SysListView32 INHERIT ListView
ENDCLASS

//----------------------------------------------------------------------------------------------

CLASS ListViewColumn INHERIT Object
   DATA Parent     EXPORTED
   DATA Caption    EXPORTED
   DATA Width      EXPORTED
   DATA Align      EXPORTED
   DATA ImageIndex EXPORTED INIT 0
   DATA Name       EXPORTED
   DATA Index      EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Destroy()
ENDCLASS

//----------------------------------------------------------------------------------------------

METHOD Init( oParent, cCaption, nWidth, nAlign, lCreate ) CLASS ListViewColumn

   DEFAULT lCreate TO .F.

   ::Parent    := oParent
   ::Caption   := cCaption
   ::Width     := nWidth
   ::Align     := nAlign
   ::__CreateProperty( "ListViewColumn" )
   IF lCreate
      ::Create()
   ENDIF

RETURN Self

//----------------------------------------------------------------------------------------------

METHOD Create() CLASS ListViewColumn
   LOCAL lvc := (struct LV_COLUMN)

   lvc:mask       := LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM
   lvc:iOrder     := 0
   lvc:iSubItem   := LEN( ::Parent:Columns )
   lvc:cchTextMax := 0
   lvc:iImage     := ::ImageIndex-1
   lvc:pszText    := ::Caption
   lvc:fmt        := ::Align
   lvc:cx         := ::Width
   ::Index        := LEN(::Parent:Columns)
   
   AADD( ::Parent:Columns, Self )
   
   ListViewInsertColumn( ::Parent:hWnd, ::Index, lvc:Value )
RETURN Self

METHOD Destroy() CLASS ListViewColumn
   LOCAL n := 1
   ::Parent:SendMessage( LVM_DELETECOLUMN, ::Index, 0 )
   ADEL( ::Parent:Columns, ::Index, .T. )
   AEVAL( ::Parent:Columns, {|o| o:Index := n++ } )
RETURN Self

//------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------

CLASS ListViewGroup INHERIT Control
   DATA __lCreated  PROTECTED INIT .F.
   PROPERTY Caption     READ xCaption   WRITE SetCaption PROTECTED
   PROPERTY Alignment   READ xAlignment WRITE SetAlignment DEFAULT __GetSystem():ListViewGroupAlign:Left PROTECTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Destroy()
   METHOD SetCaption()
   METHOD SetAlignment()
   METHOD AddItem( cText, nImage, nRow ) INLINE ::Parent:InsertItem( cText, nImage, nRow, ::Id )
   METHOD GetRectangle()
ENDCLASS

//------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS ListViewGroup
   ::__xCtrlName := "ListViewGroup"
   ::Id := LEN( oParent:Children )
   Super:Init( oParent )
RETURN Self

//------------------------------------------------------------------------------------------------------
METHOD Create() CLASS ListViewGroup
   LOCAL nRet, plvg := (struct LVGROUP)

   ::Parent:SendMessage( LVM_ENABLEGROUPVIEW, 1, 0 )

   plvg:cbSize    := plvg:sizeof()
   plvg:mask      := LVGF_HEADER | LVGF_GROUPID | LVGF_ALIGN
   plvg:pszHeader := ANSITOWIDE( ::Caption )
   plvg:cchHeader := LEN( ::Caption )
   plvg:uAlign    := ::Alignment
   plvg:iGroupId  := ::Id
   plvg:stateMask := LVGS_NORMAL
   plvg:state     := LVGS_NORMAL
   ::Parent:SendMessage( LVM_INSERTGROUP, -1, plvg )
   ::Parent:SendMessage( LVM_MOVEITEMTOGROUP, 0, 0 )
   AADD( ::Parent:Children, Self )
   IF HGetPos( ::EventHandler, "OnCreate" ) != 0
      nRet := ::Form:&( ::EventHandler[ "OnCreate" ] )( Self )
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------
METHOD Destroy() CLASS ListViewGroup
   LOCAL n
   ADEL( ::Parent:Children, ::Id+1, .T. )
   ::Parent:SendMessage( LVM_REMOVEGROUP, ::Id, NIL )
   IF ( n := hScan( ::Form:Property, Self ) ) > 0
      HDelAt( ::Form:Property, n, .T. )
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------
METHOD SetCaption() CLASS ListViewGroup
RETURN Self

//------------------------------------------------------------------------------------------------------
METHOD SetAlignment() CLASS ListViewGroup
RETURN Self

//------------------------------------------------------------------------------------------------------
METHOD GetRectangle() CLASS ListViewGroup
RETURN {0,0,0,0}

//------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------

#pragma BEGINDUMP

#include <windows.h>
#include <shlobj.h>
#include <commctrl.h>
#include "item.api"
#include "hbapi.h"
#include "commctrl.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "classes.h"

HB_FUNC ( LISTVIEWNOTIFY )
{
   PHB_ITEM pSelf = hb_param( 1, HB_IT_OBJECT );
   LPARAM lParam = (LPARAM) hb_parnl(3);
   LPNMHDR lpnmh = (LPNMHDR) lParam;
   LPCSTR cRet;

   switch( lpnmh->code )
   {
      case LVN_GETDISPINFO:
      {
         LV_DISPINFO *lpdi = (LV_DISPINFO *) lParam;

         if( lpdi->item.mask & LVIF_TEXT )
         {
            PHB_DYNS pDynSym;
            PHB_ITEM pString = hb_param( 2, HB_IT_ANY );

            pDynSym = hb_dynsymFindName( pString->item.asString.value );

            if ( pDynSym )
            {
               hb_vmPushSymbol( pDynSym->pSymbol );
               hb_vmPush( pSelf );
               hb_vmPushLong( (LONG) lpdi->item.iItem );
               hb_vmPushLong( (LONG) lpdi->item.iSubItem );
               hb_vmSend( (USHORT) ( 2 ) );

               cRet= hb_itemGetC( hb_stackReturnItem() );
               lstrcpy( lpdi->item.pszText, cRet );
            }
         }
         if( lpdi->item.mask & LVIF_IMAGE )
         {
            lpdi->item.iImage = lpdi->item.iItem;
         }
      }
      hb_retni(0);
   }
   hb_retni(0);
}

#pragma ENDDUMP
