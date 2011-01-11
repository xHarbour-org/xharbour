/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// ListBox.prg                                                                                          *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

//--------------------------------------------------------------------------------------------------------------

CLASS ListBox FROM Control

   DATA ImageList  EXPORTED
   DATA ImageIndex PROTECTED
   DATA __nWidth   PROTECTED INIT 0

   DATA OnChange  EXPORTED
   ACCESS CurSel  INLINE ::GetCurSel()

   PROPERTY VertScroll        INDEX WS_VSCROLL            READ xVertScroll        WRITE SetStyle          DEFAULT .F. PROTECTED
   PROPERTY HorzScroll        INDEX WS_HSCROLL            READ xHorzScroll        WRITE SetStyle          DEFAULT .F. PROTECTED
   PROPERTY IntegralHeight    INDEX LBS_NOINTEGRALHEIGHT  READ xIntegralHeight    WRITE SetIntegralHeight DEFAULT .F. PROTECTED
   PROPERTY ExtendedSel       INDEX LBS_EXTENDEDSEL       READ xExtendedSel       WRITE SetStyle          DEFAULT .F. PROTECTED
   PROPERTY MultiColumn       INDEX LBS_MULTICOLUMN       READ xMultiColumn       WRITE SetStyle          DEFAULT .F. PROTECTED
   PROPERTY NoRedraw          INDEX LBS_NOREDRAW          READ xNoRedraw          WRITE SetStyle          DEFAULT .F. PROTECTED
   PROPERTY Notify            INDEX LBS_NOTIFY            READ xNotify            WRITE SetStyle          DEFAULT .T. PROTECTED
   PROPERTY Sort              INDEX LBS_SORT              READ xSort              WRITE SetStyle          DEFAULT .F. PROTECTED
   PROPERTY UseTabStops       INDEX LBS_USETABSTOPS       READ xUseTabStops       WRITE SetStyle          DEFAULT .F. PROTECTED
   PROPERTY WantKeyboardInput INDEX LBS_WANTKEYBOARDINPUT READ xWantKeyboardInput WRITE SetStyle          DEFAULT .F. PROTECTED
   PROPERTY DisableNoScroll   INDEX LBS_DISABLENOSCROLL   READ xDisableNoScroll   WRITE SetStyle          DEFAULT .F. PROTECTED
   PROPERTY HasStrings        INDEX LBS_HASSTRINGS        READ xHasStrings        WRITE SetStyle          DEFAULT .T. PROTECTED
   PROPERTY Border            INDEX WS_BORDER             READ xBorder            WRITE SetStyle          DEFAULT .F. PROTECTED

   PROPERTY ClientEdge        INDEX WS_EX_CLIENTEDGE      READ xClientEdge        WRITE SetExStyle        DEFAULT .T. PROTECTED

   PROPERTY OwnerDraw                                     READ xOwnerDraw         WRITE SetDrawStyle      DEFAULT 1   PROTECTED

   DATA OwnerDraw_Styles EXPORTED  INIT { "No", "Fixed", "Variable" }

   METHOD Init()  CONSTRUCTOR
   METHOD Create()

   METHOD GetString()
   METHOD GetItemRect()
   METHOD GetSelItems()
   METHOD SetDrawStyle()
   METHOD AddItem( cText, lSel )           INLINE ::AddString( cText, lSel )
   METHOD SetCurSel(nLine)                 INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETCURSEL, nLine-1, 0), NIL )
   METHOD SetSel(nLine,lSel)               INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETSEL, IIF(lSel,1,0), MAKELPARAM(nLine, 0)), NIL )
   METHOD FindString(nStart,cStr)          INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_FINDSTRING, IFNIL(nStart,-1,nStart), cStr), NIL )
   METHOD FindExact(nStart,cStr)           INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_FINDSTRINGEXACT, IFNIL(nStart,-1,nStart), cStr), NIL )
   METHOD GetCount()                       INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETCOUNT, 0, 0), NIL )
   METHOD GetCurSel()                      INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETCURSEL, 0, 0)+1, NIL )
   METHOD Dir(nAttr, cFileSpec)            INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_DIR, nAttr, cFileSpec), NIL )
   METHOD GetSelCount()                    INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETSELCOUNT, 0, 0), NIL )

   METHOD SelItemRangeEx(nFirst, nLast)    INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SELITEMRANGEEX, nFirst, nLast ), NIL )
   METHOD ResetContent()                   INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_RESETCONTENT, 0, 0 ) , NIL )
   METHOD GetSel(nLine)                    INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETSEL, nLine, 0 ), NIL )
   METHOD GetText( nLine, cBuffer )        INLINE IIF( ::hWnd != NIL, ( SendMessage( ::hWnd, LB_GETTEXT,nLine, @cBuffer ), cBuffer), NIL )
   METHOD GetTextLen( nLine )              INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETTEXTLEN, nLine, 0 ), NIL )
   METHOD GetItemText()
   METHOD SelectString( nLine, cText )     INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SELECTSTRING, nLine, cText ), NIL )
   METHOD GetTopIndex( nLine )             INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETTOPINDEX, nLine, 0 ) , NIL )
   //METHOD GetSelItems( nMax, cBuffer )  INLINE IIF( ::hWnd != NIL, SendMessage( ::hWnd, LB_GETSELITEMS, nMax, @cBuffer ), NIL )
   METHOD SetTabStops( nTabs, abTabs )     INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETTABSTOPS, nTabs, abTabs ) , NIL )
   METHOD GetHorizontalExtent()            INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETHORIZONTALEXTENT, 0, 0 ) , NIL )
   METHOD SetHorizontalExtent(nWidth)      INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETHORIZONTALEXTENT, nWidth, 0 ) , NIL )
   METHOD SetColumnWidth( nWidth )         INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETCOLUMNWIDTH, nWidth, 0 ) , NIL )
   METHOD AddFile( cFile )                 INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_ADDFILE, 0, cFile ), NIL )
   METHOD SetTopIndex( nLine )             INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETTOPINDEX, nLine, 0 ), NIL )
   //METHOD GetItemRect( nLine, bRect )   INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETITEMRECT, nLine, bRect ) , NIL )
   METHOD GetItemData( nLine )             INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETITEMDATA, nLine, 0 ) , NIL )
   METHOD SetItemData( nLine, cData )      INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETITEMDATA, nLine, cData ) , NIL )
   METHOD SelItemRange( nFrom, nTo, lSel ) INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SELITEMRANGE, IIF( lSel, 1, 0 ), MAKELONG( nFrom, nTo ) ) , NIL )
   METHOD SetAnchorIndex( nLine )          INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETANCHORINDEX, nLine, 0 ), NIL )
   METHOD GetAnchorIndex()                 INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETANCHORINDEX, 0, 0 ) , NIL )
   METHOD SetCaretIndex( nLine, lScroll)   INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETCARETINDEX, nLine, IF (lScroll, 1, 0 ) ) , NIL )
   METHOD GetCaretIndex()                  INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETCARETINDEX, 0, 0 )+1 , NIL )
   METHOD SetItemHeight( nLine, nHeight )  INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETITEMHEIGHT, nLine, nHeight ) , NIL )
   METHOD GetItemHeight( nLine )           INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETITEMHEIGHT, nLine, 0 ) , NIL )
   METHOD SetLocale( nID )                 INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETLOCALE, nID, 0 ) , NIL )
   METHOD GetLocale()                      INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETLOCALE, 0, 0 ), NIL )
   METHOD SetCount(nCount)                 INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SETCOUNT, nCount, 0 ) , NIL )
   METHOD InitStorage( nItems, nBytes )    INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_INITSTORAGE, nItems, nBytes ) , NIL )
   METHOD ItemFromPoint( x,y )             INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_ITEMFROMPOINT, 0, MAKELONG( x, y ) ) , NIL )
   METHOD __SetScrollBars()                INLINE Self

   METHOD AddString()
   METHOD InsertString(nLine,cText)
   MESSAGE DeleteItem METHOD DeleteString
   METHOD DeleteString(nLine)

   METHOD OnGetDlgCode( msg )              INLINE IIF( msg != NIL .AND. msg:message == WM_KEYDOWN .AND. ( msg:wParam == VK_TAB .OR. msg:wParam == VK_ESCAPE ), NIL, DLGC_WANTMESSAGE )

   METHOD OnParentCommand()

   METHOD OnSelChange()    VIRTUAL
   METHOD OnDblClk()       VIRTUAL
   METHOD OnErrSpace()     VIRTUAL
   METHOD OnLBNKillFocus() VIRTUAL
   METHOD OnLBNSetFocus()  VIRTUAL

   METHOD OnHScroll() INLINE NIL
   METHOD OnVScroll() INLINE NIL
   METHOD OnCtlColorListBox()
   METHOD SetIntegralHeight( n, lSet ) INLINE ::SetStyle( n, !lSet )

ENDCLASS

METHOD AddString( cText, lSel ) CLASS ListBox
   LOCAL n
   IF ::hWnd != NIL
      ::SendMessage( LB_ADDSTRING, 0, cText )
      IF ::HorzScroll
         n := ::Drawing:GetTextExtentPoint32( cText )[1]
         IF n > ::__nWidth
            ::SetHorizontalExtent( n + 3 )
            ::__nWidth := n
         ENDIF
      ENDIF
      DEFAULT lSel TO .F.
      IF lSel
         ::SetCurSel( ::GetCount() )
      ENDIF
   ENDIF
RETURN NIL

METHOD InsertString(nLine,cText) CLASS ListBox
   LOCAL n
   IF ::hWnd != NIL
      ::SendMessage( LB_INSERTSTRING, nLine, cText )
      IF ::HorzScroll
         n := ::Drawing:GetTextExtentPoint32( cText )[1]
         IF n > ::__nWidth
            ::SetHorizontalExtent( n + 3 )
            ::__nWidth := n
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

METHOD DeleteString(nLine) CLASS ListBox
   LOCAL n, x
   IF ::hWnd != NIL
      ::SendMessage( LB_DELETESTRING, nLine-1, 0)
      ::__nWidth := 0
      FOR x := 1 TO ::GetCount()
         n := ::Drawing:GetTextExtentPoint32( ::GetItemText(x-1) )[1]
         IF n > ::__nWidth
            ::__nWidth := n
         ENDIF
      NEXT
      ::SetHorizontalExtent( ::__nWidth + 3 )
   ENDIF
RETURN NIL

//--------------------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ListBox
   ::ClsName      := "ListBox"
   DEFAULT ::Style   TO WS_CHILD | WS_VISIBLE | WS_TABSTOP | LBS_NOTIFY | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ExStyle := WS_EX_CLIENTEDGE

   DEFAULT ::__xCtrlName TO "ListBox"
   ::Super:Init( oParent )
   ::Width        := 80
   ::Height       := 80
   ::SmallCaption := .T.
   ::DeferRedraw  := .F.
   ::BackSysColor := GetSysColor( COLOR_WINDOW )
   IF !EMPTY( ::Events )
      AADD( ::Events[2][2], { "OnSelChange" , "", "" } )
      AADD( ::Events[2][2], { "OnDblClk" , "", "" } )
      AADD( ::Events[2][2], { "OnErrSpace" , "", "" } )
      AADD( ::Events[2][2], { "OnLBNKillFocus" , "", "" } )
      AADD( ::Events[2][2], { "OnLBNSetFocus" , "", "" } )
   ENDIF
RETURN Self

METHOD Create() CLASS ListBox
   LOCAL n
   ::Super:Create()
   IF !EMPTY( ::Caption )
      IF ::Flat
         ::ClientEdge := .F.
      ENDIF
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER)
   ENDIF
//   IF ::HorzScroll
//      ::SetHorizontalExtent( n )
//   ENDIF
RETURN Self

METHOD SetDrawStyle(n) CLASS ListBox
   SWITCH n
      CASE 1
         ::SetStyle( LBS_OWNERDRAWFIXED, .F. )
         ::SetStyle( LBS_OWNERDRAWVARIABLE, .F. )
         EXIT
      CASE 2
         ::SetStyle( LBS_OWNERDRAWVARIABLE, .F. )
         ::SetStyle( LBS_OWNERDRAWFIXED, .T. )
         EXIT
      CASE 3
         ::SetStyle( LBS_OWNERDRAWFIXED, .F. )
         ::SetStyle( LBS_OWNERDRAWVARIABLE, .T. )
         EXIT
   END
RETURN Self

//----------------------------------------------------------------------------------------------------------------

METHOD GetString(nLine) CLASS ListBox
   LOCAL nLen
   LOCAL cBuf
   DEFAULT nLine TO ::CurSel
   cBuf := space(SendMessage(::hWnd, LB_GETTEXTLEN, nLine-1, 0) + 1)
   nLen := SendMessage(::hWnd, LB_GETTEXT, nLine-1, @cBuf)
RETURN( IIF(nLen == LB_ERR, nil, left(cBuf, nLen) ) )

//----------------------------------------------------------------------------------------------------------------

METHOD GetItemRect( nLine) CLASS ListBox
   LOCAL rc := (struct RECT)
   LOCAL cRect := space(16)
   SendMessage( ::hWnd, LB_GETITEMRECT, nLine, @cRect)
   rc:Buffer( cRect )
RETURN(rc:Value)

//----------------------------------------------------------------------------------------------------------------

METHOD GetSelItems() CLASS ListBox
RETURN ListBoxGetSelItems( ::hWnd )

//----------------------------------------------------------------------------------------------------------------

METHOD OnParentCommand( nId, nCode, nlParam ) CLASS ListBox
   LOCAL nRet
   DO CASE
      CASE nCode == LBN_SELCHANGE
           nRet := ExecuteEvent( "OnSelChange", Self )
           DEFAULT nRet TO ::OnSelChange( nId, nCode, nlParam )

      CASE nCode == LBN_DBLCLK
           nRet := ExecuteEvent( "OnDblClk", Self )
           DEFAULT nRet TO ::OnDblClk( nId, nCode, nlParam )

      CASE nCode == LBN_ERRSPACE
           nRet := ExecuteEvent( "OnErrSpace", Self )
           DEFAULT nRet TO ::OnErrSpace( nId, nCode, nlParam )

      CASE nCode == LBN_KILLFOCUS
           nRet := ExecuteEvent( "OnLBNKillFocus", Self )
           DEFAULT nRet TO ::OnLBNKillFocus( nId, nCode, nlParam )

      CASE nCode == LBN_SETFOCUS
           nRet := ExecuteEvent( "OnLBNSetFocus", Self )
           DEFAULT nRet TO ::OnLBNSetFocus( nId, nCode, nlParam )

   ENDCASE
RETURN nRet

//----------------------------------------------------------------------------------------------------------------

METHOD OnCtlColorListBox( nwParam, nlParam ) CLASS ListBox
   LOCAL hBkGnd := ::BkBrush
//   DEFAULT hBkGnd TO ::Parent:BkBrush

   IF ::ForeColor != NIL .AND. ::ForeColor != ::ForeSysColor
      SetTextColor( nwParam, ::ForeColor )
   ENDIF
   IF hBkGnd != NIL
      SetBkMode( nwParam, TRANSPARENT )
      RETURN hBkGnd
    ELSEIF ::ForeColor != NIL .AND. ::ForeColor != ::ForeSysColor
      SetBkMode( nwParam, TRANSPARENT )
      IF ::BackColor == ::BackSysColor
         RETURN GetSysColorBrush( COLOR_BTNFACE )
      ENDIF
   ENDIF
RETURN NIL

METHOD GetItemText( nItem ) CLASS ListBox
  LOCAL cText := ::GetText( nItem, SPACE( ::GetTextLen( nItem )+1 ) )
RETURN cText
