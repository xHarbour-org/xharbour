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

   ACCESS CurSel       INLINE    ::GetCurSel()

   DATA ImageList      EXPORTED
   DATA OnChange       EXPORTED

   DATA ImageIndex     PROTECTED
   DATA __nWidth       PROTECTED INIT 0
   DATA __nItemTip     PROTECTED INIT 0
   DATA __tipWnd       PROTECTED
   DATA __OriginalSel  PROTECTED INIT LB_ERR
   DATA __isEnter      PROTECTED INIT .F.

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
   METHOD FindString(nStart,cStr)          INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_FINDSTRING, IFNIL(nStart,-1,nStart), cStr)+1, NIL )
   METHOD FindExact(nStart,cStr)           INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_FINDSTRINGEXACT, IFNIL(nStart,-1,nStart), cStr), NIL )
   METHOD GetCount()                       INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETCOUNT, 0, 0), NIL )
   METHOD GetCurSel()                      INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETCURSEL, 0, 0)+1, NIL )
   METHOD Dir(nAttr, cFileSpec)            INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_DIR, nAttr, cFileSpec), NIL )
   METHOD GetSelCount()                    INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETSELCOUNT, 0, 0), NIL )

   METHOD SelItemRangeEx(nFirst, nLast)    INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_SELITEMRANGEEX, nFirst, nLast ), NIL )
   METHOD ResetContent()                   INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_RESETCONTENT, 0, 0 ) , NIL )
   METHOD GetSel(nLine)                    INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETSEL, nLine, 0 ), NIL )
   METHOD GetText( nLine, cBuffer )        INLINE IIF( ::hWnd != NIL, ( SendMessage( ::hWnd, LB_GETTEXT,nLine-1, @cBuffer ), cBuffer), NIL )
   METHOD GetTextLen( nLine )              INLINE IIF( ::hWnd != NIL, ::SendMessage( LB_GETTEXTLEN, nLine-1, 0 ), NIL )
   METHOD GetItemText( nItem )             INLINE ::GetText( nItem, SPACE( ::GetTextLen( nItem )+1 ) )
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
   METHOD OnMouseMove()
   METHOD __SetItemToolTips()
   METHOD __ListCallBack()
ENDCLASS

METHOD __SetItemToolTips( lTips ) CLASS ListBox
   LOCAL wcex
   IF lTips

      IF IsWindow( ::hWnd )
         ::cbi := ::GetComboBoxInfo()
         IF IsWindow( ::cbi:hwndList )
            ::__pListCallBack := WinCallBackPointer( HB_ObjMsgPtr( Self, "__ListCallBack" ), Self )
            ::__nListProc := SetWindowLong( ::cbi:hwndList, GWL_WNDPROC, ::__pListCallBack )
         ENDIF
         wcex := (struct WNDCLASSEX)
         wcex:cbSize         := wcex:SizeOf()
         wcex:style          := CS_OWNDC | CS_DBLCLKS | CS_SAVEBITS | CS_DROPSHADOW
         wcex:hInstance      := ::AppInstance
         wcex:hbrBackground  := COLOR_BTNFACE+1
         wcex:lpszClassName  := "CBTT"
         wcex:hCursor        := LoadCursor(, IDC_ARROW )
         wcex:lpfnWndProc    := DefWindowProcAddress()
         RegisterClassEx( wcex )

         ::__tipWnd := CreateWindowEx( WS_EX_TOOLWINDOW, "CBTT", "", WS_POPUP, 0, 0, 0, 0, 0, 0, ::AppInstance )

         IF IsWindow( ::__tipWnd )
            ::__pTipCallBack := WinCallBackPointer( HB_ObjMsgPtr( Self, "__TipCallBack" ), Self )
            ::__nTipProc := SetWindowLong( ::__tipWnd, GWL_WNDPROC, ::__pTipCallBack )
         ENDIF
      ENDIF

    ELSE

      IF IsWindow( ::__tipWnd ) .AND. ::__nTipProc != NIL
         SetWindowLong( ::__tipWnd, GWL_WNDPROC, ::__nTipProc )
         ::__nTipProc := NIL
         FreeCallBackPointer( ::__pTipCallBack )
         ::__pTipCallBack := NIL

         DestroyWindow( ::__tipWnd )

         IF IsWindow( ::cbi:hwndList ) .AND. ::__nListProc != NIL
            SetWindowLong( ::cbi:hwndList, GWL_WNDPROC, ::__nListProc )
            ::__nListProc := NIL
            FreeCallBackPointer( ::__pListCallBack )
            ::__pListCallBack := NIL
         ENDIF
      ENDIF

   ENDIF
RETURN Self

METHOD __ListCallBack( hWnd, nMsg, nwParam, nlParam ) CLASS ComboBox
   LOCAL aPt, aRect
   SWITCH nMsg
      CASE WM_MOUSEMOVE
           aPt := { LOWORD( nlParam ), HIWORD( nlParam ) }
           aRect := _GetClientRect( hWnd )
           IF _PtInRect( aRect, aPt )
              ::__ListboxMouseMove( hWnd, nwParam, aPt )
            ELSE
              SendMessage( hWnd, WM_MOUSELEAVE, nwParam, nlParam )
           ENDIF
           
           IF ! ::__isEnter
              ::__TrackMouseEvent( hWnd, TME_HOVER|TME_LEAVE )
           ENDIF
           EXIT

      CASE WM_MOUSELEAVE
           ::__OriginalSel := LB_ERR
           ::__isEnter := .F.
           ShowWindow( ::__tipWnd, SW_HIDE )
           EXIT
   END
RETURN CallWindowProc( ::__nListProc, hWnd, nMsg, nwParam, nlParam )

METHOD __TipCallBack( hWnd, nMsg, nwParam, nlParam ) CLASS ListBox
   SWITCH nMsg
      CASE WM_PAINT
           RETURN ::__HandleOnPaint( hWnd )
           
      CASE WM_TIMER
           ::__HandleOnTimer( nwParam )
           EXIT
   END
RETURN CallWindowProc( ::__nTipProc, hWnd, nMsg, nwParam, nlParam )

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
      ::SendMessage( LB_INSERTSTRING, nLine-1, cText )
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
         n := ::Drawing:GetTextExtentPoint32( ::GetItemText(x) )[1]
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
   ::Super:Create()
   IF !EMPTY( ::Caption )
      IF ::Flat
         ::ClientEdge := .F.
      ENDIF
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER)
   ENDIF
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
   SendMessage( ::hWnd, LB_GETITEMRECT, nLine+1, @rc)
RETURN rc:Array

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
METHOD OnMouseMove( nwParam, x, y ) CLASS ListBox
   LOCAL nItem, aText, cText, aSize, oTip
   (nwParam)
   nItem := ::ItemFromPoint( x, y )

   IF ::__OriginalSel == nItem .OR. nItem == LB_ERR .OR. nItem < 0
      RETURN NIL 
   ENDIF
   ::__OriginalSel := nItem
   
   aText := ::GetItemRect( nItem+1 )
   cText := ::GetItemText( nItem+1 )
   aSize := ::Drawing:GetTextExtentPoint32( cText )
   IF aSize[1] > ::ClientWidth
      VIEW cText, nItem
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------------------------------------
METHOD OnCtlColorListBox( nwParam ) CLASS ListBox
   LOCAL hBkGnd := ::BkBrush
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
