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
#include "uxtheme.ch"

#define CB_SETMINVISIBLE 0x1701
#define CS_DROPSHADOW 131072

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
   DATA __pTipCallBack PROTECTED
   DATA __nTipProc     PROTECTED

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
   PROPERTY ItemToolTips                                  READ xItemToolTips      WRITE __SetItemToolTips DEFAULT .F. PROTECTED 

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
   METHOD SetHorizontalExtent(nWidth)
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
   METHOD OnDestroy()                      INLINE ::__SetItemToolTips(.F.), NIL

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
   METHOD __TipCallBack()
   METHOD __ListBoxMouseMove()
   METHOD __HandleOnPaint()
   METHOD __HandleOnTimer()
   METHOD __TrackMouseEvent()
ENDCLASS

//----------------------------------------------------------------------------------------------------------------
METHOD __SetItemToolTips( lTips ) CLASS ListBox
   LOCAL wcex
   IF lTips

      IF IsWindow( ::hWnd )
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
      ENDIF

   ENDIF
RETURN Self

METHOD __TipCallBack( hWnd, nMsg, nwParam, nlParam ) CLASS ListBox
   static lMouseHover := .F.
   SWITCH nMsg
      CASE WM_MOUSEMOVE
           IF ! lMouseHover
              lMouseHover := .T.
              ::__TrackMouseEvent( hWnd, TME_LEAVE )
           ENDIF
           RETURN 0

      CASE WM_PAINT
           RETURN ::__HandleOnPaint( hWnd )
           
      CASE WM_TIMER
           ::__HandleOnTimer( nwParam )
           EXIT

      CASE WM_LBUTTONDOWN
           lMouseHover := .F.
           ShowWindow( hWnd, SW_HIDE )
           PostMessage( ::hWnd, nMsg, nwParam, nlParam )
           RETURN 0

      CASE WM_MOUSELEAVE
           lMouseHover := .F.
           ::__OriginalSel := LB_ERR
           ShowWindow( hWnd, SW_HIDE )
           RETURN 0
   END
RETURN CallWindowProc( ::__nTipProc, hWnd, nMsg, nwParam, nlParam )

METHOD __TrackMouseEvent( hWnd, nFlags ) CLASS ListBox
   LOCAL tme := (struct TRACKMOUSEEVENT)
   tme:cbSize      := tme:SizeOf()
   tme:dwFlags     := nFlags
   tme:hwndTrack   := hWnd
   tme:dwHoverTime := HOVER_DEFAULT
   TrackMouseEvent( tme )
RETURN Self

METHOD __ListboxMouseMove( nwParam, aPt ) CLASS ListBox
   LOCAL hDC, hOldFont, cBuf, pt := (struct POINT)
   LOCAL rcBounds := (struct RECT)
   LOCAL rcDraw := (struct RECT)
   LOCAL cRect := space(16)
   LOCAL nCurSel := SendMessage( ::hWnd, LB_ITEMFROMPOINT, 0, MAKELONG( aPt[1], aPt[2] ) )
   (nwParam)

   IF ::__OriginalSel == nCurSel
      RETURN NIL 
   ENDIF
   IF nCurSel == 65540 .AND. IsWindowVisible( ::__tipWnd )
      ShowWindow( ::__tipWnd, SW_HIDE )
      ::__OriginalSel := nCurSel
      RETURN NIL
   ENDIF
   IF nCurSel == LB_ERR .OR. nCurSel < 0 .OR. nCurSel >= SendMessage( ::hWnd, LB_GETCOUNT, 0, 0 )
      RETURN NIL
   ENDIF

   ::__OriginalSel := nCurSel

   hDC := GetDC( ::__tipWnd )
   hOldFont := SelectObject( hDC, ::Font:Handle )
   
   cBuf := space( SendMessage( ::hWnd, LB_GETTEXTLEN, nCurSel, 0 ) + 1 )
   SendMessage( ::hWnd, LB_GETTEXT, nCurSel, @cBuf)

   SendMessage( ::hWnd, LB_GETITEMRECT, nCurSel, @rcBounds)
   rcDraw:left   := rcBounds:left  
   rcDraw:top    := rcBounds:top   
   rcDraw:right  := rcBounds:right 
   rcDraw:bottom := rcBounds:bottom

   DrawText( hDC, cBuf, @rcDraw, DT_CALCRECT|DT_SINGLELINE|DT_CENTER|DT_VCENTER|DT_NOPREFIX )

   SelectObject( hDC, hOldFont )
   ReleaseDC( ::__tipWnd, hDC )

   IF rcDraw:right <= rcBounds:right
      ShowWindow( ::__tipWnd, SW_HIDE )
      RETURN NIL
   ENDIF

   InflateRect( @rcDraw, 2, 2 )
   
   pt:x := rcDraw:left
   pt:y := rcDraw:top
   ClientToScreen( ::hWnd, @pt )
   rcDraw:left := pt:x
   rcDraw:top  := pt:y

   pt:x := rcDraw:right
   pt:y := rcDraw:bottom
   ClientToScreen( ::hWnd, @pt )
   rcDraw:right  := pt:x
   rcDraw:bottom := pt:y
   
   SetWindowText( ::__tipWnd, cBuf )

   ShowWindow( ::__tipWnd, SW_HIDE )

   SetWindowPos( ::__tipWnd, HWND_TOPMOST, rcDraw:left+1, rcDraw:top, rcDraw:Right-rcDraw:left+4, rcDraw:Bottom-rcDraw:top, SWP_NOACTIVATE | SWP_SHOWWINDOW )
   SetTimer( ::__tipWnd, 1, 9000, NIL )
RETURN NIL

//----------------------------------------------------------------------------------------------------------------
METHOD __HandleOnPaint( hWnd ) CLASS ListBox
   LOCAL hDC, cPaint, aRect, cText, hOldFont, hTheme
   hDC := _BeginPaint( hWnd, @cPaint )
   aRect := _GetClientRect( hWnd )

   hTheme := OpenThemeData(,"TOOLTIP")
   DrawThemeBackground( hTheme, hDC, TTP_STANDARD, 0, { 0, 0, aRect[3], aRect[4] } )
   CloseThemeData( hTheme )

//   SelectObject( hDC, GetSysColorbrush( COLOR_INFOBK ) )
//   Rectangle( hDC, 0, 0, aRect[3], aRect[4] )

   cText := _GetWindowText( hWnd )
   SetBkMode( hDC, TRANSPARENT )
   hOldFont := SelectObject( hDC, ::Font:Handle )

   _DrawText( hDC, cText, aRect, DT_SINGLELINE|DT_CENTER|DT_VCENTER|DT_NOPREFIX )

   SelectObject( hDC, hOldFont )
   _EndPaint( hWnd, cPaint)
RETURN 0

METHOD __HandleOnTimer( nwParam ) CLASS ListBox
   KillTimer( ::__tipWnd, nwParam )
   ShowWindow( ::__tipWnd, SW_HIDE)
RETURN Self

//----------------------------------------------------------------------------------------------------------------

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
   IF ::hWnd != NIL
      ::SendMessage( LB_DELETESTRING, nLine-1, 0)
   ENDIF
RETURN NIL

METHOD SetHorizontalExtent( nWidth ) CLASS ListBox
   LOCAL x, nCnt, n
   IF ::hWnd != NIL
      IF nWidth == NIL
         nWidth := 0
         nCnt := ::GetCount()
         FOR x := 1 TO nCnt
             n := ::Drawing:GetTextExtentPoint32( ::GetItemText(x) )[1]
             IF n > nWidth
                nWidth := n
             ENDIF
         NEXT
      ENDIF
      ::SendMessage( LB_SETHORIZONTALEXTENT, nWidth, 0 )
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
   IF ::ItemToolTips
      ::__SetItemToolTips( .T. )
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
   LOCAL aRect := _GetClientRect( ::hWnd )
   IF _PtInRect( aRect, {x,y} )
      ::__ListboxMouseMove( nwParam, {x,y} )
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
