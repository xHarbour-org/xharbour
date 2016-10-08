/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// TabControl.prg                                                                                       *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#Include "vxh.ch"
#Include "colors.ch"

#define TABP_TABITEM             1
#define TABP_TABITEMLEFTEDGE     2
#define TABP_TABITEMRIGHTEDGE    3
#define TABP_TABITEMBOTHEDGE     4
#define TABP_TOPTABITEM          5
#define TABP_TOPTABITEMLEFTEDGE  6
#define TABP_TOPTABITEMRIGHTEDGE 7
#define TABP_TOPTABITEMBOTHEDGE  8
#define TABP_PANE                9
#define TABP_BODY               10


#define TIS_NORMAL               1
#define TIS_HOT                  2
#define TIS_SELECTED             3
#define TIS_DISABLED             4
#define TIS_FOCUSED              5

#define ETDT_DISABLE        0x00000001
#define ETDT_ENABLE         0x00000002
#define ETDT_USETABTEXTURE  0x00000004
#define ETDT_ENABLETAB      (ETDT_ENABLE | ETDT_USETABTEXTURE)

typedef struct tagLOGBRUSH {;
   UINT      lbStyle;
   COLORREF  lbColor;
   ULONG_PTR lbHatch;
} LOGBRUSH;

//-----------------------------------------------------------------------------------------------

CLASS TabControl INHERIT Control
   PROPERTY MultiLine           SET ::SetPos( TCS_MULTILINE, v )                        DEFAULT .F.
   PROPERTY ImageList           SET ::SetImageList(v)
   PROPERTY FocusNever          SET ::SetStyle( TCS_FOCUSNEVER, v )                     DEFAULT .F.
   PROPERTY FixedWidth          SET ::SetStyle( TCS_FIXEDWIDTH, v )                     DEFAULT .F.
   PROPERTY ColorInactiveHeader SET ( ::xColorInactiveHeader := v, ::InvalidateRect() ) DEFAULT .F.
   PROPERTY MinTabWidth         SET ::SetMinTabWidth(v)                                 DEFAULT 0
   PROPERTY ImageList           GET __ChkComponent( Self, @::xImageList )
   PROPERTY Theming  ROOT "Appearance" SET ::Refresh(v) DEFAULT IsThemeActive()

   PROPERTY BoldSelection                                 DEFAULT .F.
   PROPERTY ShowTabs                                      DEFAULT .T.
   PROPERTY TabPosition         SET ::__SetTabPosition(v) DEFAULT 2

   PROPERTY InactiveTabHotColor ROOT "Colors" HELP "Sets the background color when hovering on an inactive tab"

   DATA CurSel           EXPORTED  INIT 1
   DATA VertFont         EXPORTED
   DATA BoldFont         EXPORTED
   DATA VertFocusFont    EXPORTED
   DATA FocusFont        EXPORTED
   DATA Frame            EXPORTED  INIT .T.

   DATA OnSelChanged     EXPORTED
   DATA TabBrush         EXPORTED

   DATA CloseBtn         PROTECTED
   DATA PinBtn           PROTECTED
   DATA __nPrevHot       PROTECTED INIT 0
   DATA __nHot           PROTECTED INIT 0
   DATA __nPrevHot       PROTECTED INIT 0

   DATA __pUpDnPtr       PROTECTED
   DATA __lMouseDown     PROTECTED INIT .F.
   DATA __ColorTable     PROTECTED

   DATA __UpDn           PROTECTED

   DATA EnumTabPosition  EXPORTED INIT { { "Left", "Top", "Right", "Bottom" }, {1,2,3,4} }

   ACCESS CurPage        INLINE ::Children[ ::CurSel ]

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD InsertTab()
   METHOD DeleteTab()

   METHOD DeleteAllTabs()

   METHOD AlignTop                   INLINE ::SetStyle( TCS_VERTICAL, .F. ),;
                                            ::SetStyle( TCS_BOTTOM, .F. ),;
                                            ::SetStyle( TCS_MULTILINE, ::Multiline ),;
                                            Self

   METHOD AlignBottom                INLINE ::SetStyle( TCS_VERTICAL, .F. ),;
                                            ::SetStyle( TCS_BOTTOM ),;
                                            ::SetStyle( TCS_MULTILINE, ::Multiline ),;
                                            Self


   METHOD AlignLeft                  INLINE ::SetStyle( TCS_BOTTOM, .F. ),;
                                            ::SetStyle( TCS_VERTICAL ),;
                                            ::SetStyle( TCS_MULTILINE, ::Multiline ),;
                                            Self

   METHOD AlignRight                 INLINE ::SetStyle( TCS_BOTTOM ),;
                                            ::SetStyle( TCS_VERTICAL ),;
                                            ::SetStyle( TCS_MULTILINE, ::Multiline ),;
                                            Self

   METHOD AdjustRect(lDisplay,aRect) INLINE TabCtrl_AdjustRect( ::hWnd, lDisplay, @aRect), aRect
   METHOD SetMinTabWidth( dx )       INLINE TabCtrl_SetMinTabWidth( ::hWnd, IIF( dx == 0, -1, dx ) )
   METHOD GetCurFocus()              INLINE TabCtrl_GetCurFocus(::hWnd )+1
   METHOD GetCurSel()                INLINE ::CurSel := TabCtrl_GetCurSel(::hWnd)+1, ::CurSel
   METHOD GetExtendedStyle()         INLINE TabCtrl_GetExtendedStyle(::hWnd)
   METHOD GetItem(nItem,ptrItem)     INLINE TabCtrl_GetItem(::hWnd,nItem-1,@ptrItem)
   METHOD GetItemText()
   METHOD GetItemObject()
   METHOD SetItemText()

   METHOD GetItemCount()             INLINE TabCtrl_GetItemCount(::hWnd)
   METHOD GetItemRect(nItem)         INLINE IIF( nItem == NIL, nItem := ::CurSel,), TabCtrl_GetItemRect(::hWnd,nItem-1)
   METHOD GetRowCount()              INLINE TabCtrl_GetRowCount(::hWnd)
   METHOD GetToolTips()              INLINE TabCtrl_GetToolTips(::hWnd)
   METHOD GetUnicodeFormat()         INLINE TabCtrl_GetUnicodeFormat(::hWnd)
   METHOD HighlightItem(n,nHigh)     INLINE TabCtrl_HighlightItem(::hWnd,n-1,nHigh)
   METHOD HitTest()
   METHOD RemoveImage(nImageIndex)   INLINE TabCtrl_RemoveImage(::hWnd, nImageIndex-1)
   METHOD SetCurFocus(nItem)         INLINE TabCtrl_SetCurFocus(::hWnd, nItem-1)
   METHOD SetCurSel(nItem)           INLINE TabCtrl_SetCurSel(::hWnd, nItem-1), ::CurSel := nItem
   METHOD SetExtendedStyle(nExStyle) INLINE TabCtrl_SetExtendedStyle(::hWnd,nExStyle)
   METHOD SetImageList( oImageList ) INLINE TabCtrl_SetImageList(::hWnd, IIF( oImageList != NIL, __ChkComponent( Self, oImageList ):handle, NIL ) )
   METHOD SetItem(nItem, cText)      INLINE TabCtrl_SetItem(::hWnd, nItem-1, cText )
   METHOD SetItemValues()
   METHOD GetTabPosByName()

   METHOD SetItemExtra(nBytes)       INLINE TabCtrl_SetItemExtra(::hWnd, nBytes)
   METHOD SetItemSize(x,y)           INLINE TabCtrl_SetItemSize(::hWnd, x, y )
   METHOD SetPadding( cx, cy )       INLINE TabCtrl_SetPadding( ::hWnd, cx, cy )
   METHOD SetToolTips( hToolTips )   INLINE TabCtrl_SetToolTips( ::hWnd, hToolTips )
   METHOD SetUnicodeFormat(lUnicode) INLINE TabCtrl_SetUnicodeFormat( ::hWnd, lUnicode )
   METHOD DeselectAll(lExcludeFocus) INLINE TabCtrl_DeselectAll(::hWnd,lExcludeFocus)

   METHOD OnTabChanging()            VIRTUAL
   METHOD OnTabChanged()             VIRTUAL
   METHOD OnTabInsert()              VIRTUAL

   METHOD OnParentNotify()
   METHOD OnEraseBkGnd()             INLINE IIF( ! ::Theming .OR. ! IsThemeActive(), NIL, 1 )

   METHOD SetPos(n,l)                INLINE ::SetStyle(n,l),;
                                            IIF( ::IsWindow(),;
                                               ( SetWindowPos( ::hWnd,, 0, 0, 0, 0, (SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER) ),;
                                            ::__OnParentSize( ::ClientWidth, ::ClientHeight ) ),)

   METHOD Refresh()                  INLINE ::Super:Refresh(), ::OnNcCalcSize()
   METHOD OnDestroy()                INLINE ::Super:OnDestroy(),;
                                            IIF( ::FocusFont     != NIL, ::FocusFont:Delete(),     NIL ),;
                                            IIF( ::VertFont      != NIL, ::VertFont:Delete(),      NIL ),;
                                            IIF( ::BoldFont      != NIL, ::BoldFont:Delete(),      NIL ),;
                                            IIF( ::VertFocusFont != NIL, ::VertFocusFont:Delete(), NIL ),;
                                            NIL
   METHOD OnPaint()
   METHOD OnMove(x,y)                INLINE IIF( ::IsWindow() .AND. ::Cursel > 0 .AND. ::GetItemCount() >= ::Cursel .AND. VALTYPE(::GetItemObject())=="O", ::GetItemObject():__OnParentSize(x,y), ), NIL
   METHOD OnSize()

   METHOD OnHScroll(x,y,n)           INLINE ::Super:OnHScroll(x,y,n)
   METHOD Hide()
   METHOD Show()

   METHOD OnMouseMove()
   METHOD OnMouseLeave(n,l)    INLINE ::Super:OnMouseLeave(n,l), ::__nPrevHot := 0, ::__nHot := 0, ::InvalidateRect(, .F. ), NIL

   METHOD OnUserMsg(n)

   METHOD OnLButtonDown()

   METHOD __SetTabPosition()
   METHOD SelectPage()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS TabControl

   ::ClsName      := WC_TABCONTROL
   DEFAULT ::__xCtrlName TO "TabControl"
   ::Style        := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TCS_OWNERDRAWFIXED)
   ::ExStyle      := WS_EX_CONTROLPARENT
   ::Super:Init( oParent )
   ::Width        := 200
   ::Height       := 200
   ::IsContainer  := .T.
   ::Events := ;
            { ;
            {"Command",     {;
                            { "OnTabChanging"      , "", "" },;
                            { "OnTabChanged"       , "", "" },;
                            { "OnTabInsert"        , "", "" },;
                            { "OnParentCommand"    , "", "" },;
                            { "OnSysCommand"       , "", "" },;
                            { "OnToolTipNotify"    , "", "" } } },;
            {"Drawing",     {;
                            { "OnEraseBkGnd"       , "", "" },;
                            { "OnPaint"            , "", "" } } },;
            {"Keyboard",    {;
                            { "OnChar"             , "", "" },;
                            { "OnGetDlgCode"       , "", "" },;
                            { "OnKeyDown"          , "", "" },;
                            { "OnKeyUp"            , "", "" },;
                            { "OnSysChar"          , "", "" },;
                            { "OnSysKeyDown"       , "", "" },;
                            { "OnSysKeyUp"         , "", "" } } },;
            {"Layout",      { ;
                            { "OnEnterSizeMove"    , "", "" },;
                            { "OnExitSizeMove"     , "", "" },;
                            { "OnGetMinMaxInfo"    , "", "" },;
                            { "OnMove"             , "", "" },;
                            { "OnMoving"           , "", "" },;
                            { "OnParentMove"       , "", "" },;
                            { "OnSize"             , "", "" },;
                            { "OnSizing"           , "", "" },;
                            { "OnWindowPosChanged" , "", "" },;
                            { "OnWindowPosChanging", "", "" } } },;
            {"Mouse",       {;
                            { "OnLButtonDblClk"    , "", "" },;
                            { "OnLButtonDown"      , "", "" },;
                            { "OnLButtonUp"        , "", "" },;
                            { "OnMButtonDown"      , "", "" },;
                            { "OnMButtonUp"        , "", "" },;
                            { "OnMouseActivate"    , "", "" },;
                            { "OnMouseHover"       , "", "" },;
                            { "OnMouseleave"       , "", "" },;
                            { "OnMouseMove"        , "", "" },;
                            { "OnRButtonDown"      , "", "" },;
                            { "OnRButtonUp"        , "", "" } } },;
            {"Non Client",  { ;
                            { "OnNCActivate"       , "", "" },;
                            { "OnNCCalcSize"       , "", "" },;
                            { "OnNCCreate"         , "", "" },;
                            { "OnNCDestroy"        , "", "" },;
                            { "OnNCHitTest"        , "", "" },;
                            { "OnNCLButtonDown"    , "", "" },;
                            { "OnNCLButtonUp"      , "", "" },;
                            { "OnNCMouseHover"     , "", "" },;
                            { "OnNCMouseleave"     , "", "" },;
                            { "OnNCMouseMove"      , "", "" },;
                            { "OnNCPaint"          , "", "" } } },;
            {"Parent",      {;
                            { "OnParentDrawItem"   , "", "" },;
                            { "OnParentNotify"     , "", "" },;
                            { "OnParentSysCommand" , "", "" } } },;
            {"Scroll",      {;
                            { "OnHorzScroll"       , "", "" },;
                            { "OnVertScroll"       , "", "" } } },;
            {"Timer",       {;
                            { "OnTimer"            , "", "" } } },;
            {"Window",      {;
                            { "OnActivate"         , "", "" },;
                            { "OnCreate"           , "", "" },;
                            { "OnDestroy"          , "", "" },;
                            { "OnEnable"           , "", "" },;
                            { "OnHideWindow"       , "", "" },;
                            { "OnInitDialog"       , "", "" },;
                            { "OnKillFocus"        , "", "" },;
                            { "OnMessage"          , "", "" },;
                            { "OnSetCursor"        , "", "" },;
                            { "OnSetFocus"         , "", "" },;
                            { "OnSetFont"          , "", "" },;
                            { "OnSetText"          , "", "" },;
                            { "OnShowWindow"       , "", "" },;
                            { "OnUserMsg"          , "", "" } } } }
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS TabControl
   IF ( !::Theming .OR. !IsThemeActive() ) .AND. ::ShowTabs
      ::Style := (::Style & NOT( TCS_OWNERDRAWFIXED ))
   ENDIF
   IF ::VertFont == NIL
      ::VertFont := Font( NIL )
      ::VertFont:Escapement := 900
      ::VertFont:Create()
   ENDIF
   ::Super:Create()

   IF ! ::ShowTabs
      ::SendMessage( TCM_SETITEMSIZE, 0, MAKELPARAM(0,0) )
   ENDIF
   ::SetMinTabWidth( ::MinTabWidth )
   IF ::DesignMode
      ::__IdeContextMenuItems := { { "Add TabPage", {|o| o:=::Application:Project:AddControl( "TabPage", Self ) } }}
   ENDIF

   IF ::ImageList != NIL
      ::SetImageList( ::ImageList )
   ENDIF

   IF ::BoldSelection
      IF ::FocusFont == NIL
         ::FocusFont := Font( NIL )
         ::FocusFont:Bold := .T.
         ::FocusFont:Create()
         ::VertFocusFont := Font( NIL )
         ::VertFocusFont:Bold := .T.
         ::VertFocusFont:Escapement := 900
         ::VertFocusFont:Create()
      ENDIF
   ENDIF

   ::BoldFont := Font( NIL )
   ::BoldFont:Bold := .T.
   ::BoldFont:Create()

   IF ::DesignMode
      ::SetWindowPos(, ::Left, ::Top, ::Width, ::Height, (SWP_FRAMECHANGED | SWP_NOZORDER ) )
   ENDIF

RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnSize(w,l) CLASS TabControl
   LOCAL oPage
   Super:OnSize(w,l)
   IF ::Cursel > 0 .AND. ::GetItemCount() >= ::Cursel
      oPage := ::GetItemObject()
      IF oPage != NIL
         oPage:__OnParentSize(LOWORD(w),HIWORD(w))
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnUserMsg( hWnd, nMsg ) CLASS TabControl
   (hWnd)
   IF nMsg == WM_USER + 1111
      ::xHeight++
      ::MoveWindow()
      ::xHeight--
      ::MoveWindow()
   ENDIF
RETURN NIL

METHOD SelectPage( n ) CLASS TabControl
   LOCAL oPage := ::GetItemObject(n)
   IF oPage != NIL
      oPage:Select()
      RETURN oPage
    ELSE
      ::SetCurSel( n )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD GetItemText( n ) CLASS TabControl
   LOCAL tci      := (struct TCITEM)
   tci:mask       := TCIF_TEXT
   tci:pszText    := SPACE(255)
   tci:cchTextMax := 255
   DEFAULT n TO ::CurSel
   SendMessage( ::hWnd, TCM_GETITEM, n-1, @tci )
RETURN ALLTRIM( STRTRAN( tci:pszText, CHR(0) ) )

//-----------------------------------------------------------------------------------------------
METHOD GetItemObject( n ) CLASS TabControl
   LOCAL tci := (struct TCITEM)
   tci:mask  := TCIF_PARAM
   DEFAULT n TO ::CurSel
   SendMessage( ::hWnd, TCM_GETITEM, n-1, @tci )
RETURN ObjFromHandle( tci:lParam )

//-----------------------------------------------------------------------------------------------
METHOD SetItemText( n, cText, lBold ) CLASS TabControl
   LOCAL tci := (struct TCITEM)
   TRY
      DEFAULT n TO ::CurSel

      tci:mask := TCIF_TEXT
      tci:pszText := SPACE(255)
      tci:cchTextMax := 255
      SendMessage( ::hWnd, TCM_GETITEM, n-1, @tci )
      IF ! ( tci:pszText == cText )
         IF !lBold
            lBold := NIL
         ENDIF
         tci:mask := TCIF_TEXT
         tci:pszText := cText
         tci:cchTextMax := LEN( cText )
         SendMessage( ::hWnd, TCM_SETITEM, n-1, @tci )
         //SetWindowPos( ::hWnd,, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
         ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
      ENDIF
    catch
   END
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Hide() CLASS TabControl
   LOCAL n, oTab
   FOR n := 1 TO ::GetItemCount()
       oTab := ::GetItemObject(n)
       IF oTab != NIL
          oTab:HideTab()
       ENDIF
   NEXT
   Super:Hide()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Show(n) CLASS TabControl
   LOCAL oTab
   Super:Show(n)
   IF ::GetItemCount() >= ::Cursel
      IF ( oTab := ::GetItemObject() ) != NIL
         oTab:ShowTab()
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD HitTest( x, y, nFlags ) CLASS TabControl
   LOCAL tch := (struct TCHITTESTINFO)
   DEFAULT nFlags TO (TCHT_NOWHERE | TCHT_ONITEM | TCHT_ONITEMICON | TCHT_ONITEMLABEL)
   tch:pt:x  := x
   tch:pt:y  := y
   tch:flags := nFlags
RETURN SendMessage( ::hWnd, TCM_HITTEST, 0, tch )+1

//-----------------------------------------------------------------------------------------------
METHOD OnMouseMove( nwParam, nlParam ) CLASS TabControl
   LOCAL aItem

   ::Super:OnMouseMove( nwParam, nlParam )

   IF ( ::__nHot := ::HitTest( LOWORD( nlParam ), HIWORD( nlParam ) ) ) > 0
      IF ::__lMouseDown .AND. ::CurSel <> ::__nHot
         ::GetItemObject():Position := ::__nHot
         ::CurSel := ::__nHot
         RETURN 0
      ENDIF
      IF ::__nPrevHot <> ::__nHot
         IF ::__nPrevHot > 0
            aItem := ::GetItemRect( ::__nPrevHot )
            ::InvalidateRect( aItem, .F. )
         ENDIF
         aItem := ::GetItemRect( ::__nHot )
         ::InvalidateRect( aItem, .F. )
         ::__nPrevHot := ::__nHot
      ENDIF
    ELSEIF ::__nPrevHot > 0
      aItem := ::GetItemRect( ::__nPrevHot )
      ::InvalidateRect( aItem, .F. )
      ::__nPrevHot := 0
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD __SetTabPosition(n) CLASS TabControl
   LOCAL cPos := ::EnumTabPosition[1][n]
   IF n <> ::xTabPosition
      DO CASE
         CASE n == 1
              ::AlignLeft()
         CASE n == 2
              ::AlignTop()
         CASE n == 3
              ::AlignRight()
         CASE n == 4
              ::AlignBottom()
      ENDCASE
      ::PostMessage( WM_USER + 1111 )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnPaint() CLASS TabControl
   LOCAL aRect, n, nSel, hBrush, nItems, nState, nPart
   LOCAL aItem, oPage, hWnd, rc, tci, hDC, nColor, hPen, pBrush, hHotBrush
   LOCAL hMemDC, hMemBitmap, hOldBitmap, hOldFont, sz := (struct SIZE), nBkMode

   IF ( !::Theming .OR. !IsThemeActive() ) .AND. ::ShowTabs
      RETURN NIL
   ENDIF
   IF !::ShowTabs .AND. ::__UpDn == NIL .AND. ! ::DesignMode
      hWnd := GetWindow( ::hWnd, (GW_CHILD | GW_HWNDFIRST ))
      WHILE hWnd <> 0
         IF GetClassName( hWnd ) == "msctls_updown32"
            ::__UpDn := hWnd
            DestroyWindow( hWnd )
            EXIT
         ENDIF
         hWnd := GetWindow( hWnd, GW_HWNDNEXT )
      ENDDO
   ENDIF
   hDC := ::BeginPaint()

   rc  := (struct RECT)
   tci := (struct TCITEM)

   aRect := _GetClientRect( ::hWnd )

   rc:Left   := aRect[1]
   rc:Top    := aRect[2]
   rc:Right  := aRect[3]
   rc:Bottom := aRect[4]

   nSel   := TabCtrl_GetCurSel( ::hWnd )+1

   SendMessage( ::hWnd, TCM_ADJUSTRECT, 0, @rc )

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   aItem := ::GetItemRect( 1 )
   IF (::Style & TCS_BOTTOM) == 0
      aRect[2] := aItem[4]
    ELSE
      aRect[2] += 2
      aRect[4] -= ( aRect[4]-aItem[2]-2 )
   ENDIF
   hBrush := ::BkBrush
   IF hBrush == NIL .AND. ::Parent:BkBrush != NIL
      hBrush := ::Parent:BkBrush
      SetBrushOrgEx( hMemDC, ::Parent:ClientWidth-::Left, ::Parent:ClientHeight-::Top )
   ENDIF
   DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )
   IF ::Border == 0
      _FillRect( hMemDC, { ::ClientWidth-2, 0, ::ClientWidth, ::ClientHeight }, GetSysColorBrush( COLOR_BTNFACE ) )
      _FillRect( hMemDC, { 0, 0, ::ClientWidth-2, ::ClientHeight }, hBrush )
    ELSE
      _FillRect( hMemDC, { 0, 0, ::ClientWidth, ::ClientHeight }, hBrush )
   ENDIF

   IF ::Theming .AND. IsThemeActive()
      DrawThemeBackground( ::System:hTabTheme, hMemDC, TABP_PANE, , aRect  )
    ELSE
      _DrawFrameControl( hMemDC, aRect, DFC_BUTTON, DFCS_BUTTONPUSH )
   ENDIF

   oPage := ::GetItemObject()
   IF oPage != NIL .AND. oPage:__TabBrush != NIL .AND. oPage:TopMargin > 0
      _FillRect( hMemDC, { aRect[1]+IIF(nSel>1,1,0), aRect[2]+1, aRect[3]-3, aRect[2]+oPage:TopMargin+1 }, oPage:__TabBrush )
   ENDIF

   nItems := ::GetItemCount()
   IF ( ::ShowTabs .OR. ::DesignMode ) .AND. nItems > 0

      IF ::ColorInactiveHeader
         hHotBrush := CreateSolidBrush( LightenColor( ::BackColor, 50 ) )
      ENDIF

      FOR n := nItems TO 1 STEP -1

          IF n != nSel
             oPage := ::GetItemObject(n)

             tci:mask := TCIF_TEXT
             tci:pszText := SPACE(255)
             tci:cchTextMax := 255

             SendMessage( ::hWnd, TCM_GETITEM, n-1, @tci )

             aItem  := ::GetItemRect( n )

             nColor := SetTextColor( hMemDC, IIF( oPage != NIL .AND. ! ::ColorInactiveHeader, oPage:ForeColor, ::ForeColor ) )

             nState := TIS_NORMAL
             aItem[2]+=1
             IF ::__nHot == n
                nState := TIS_HOT
             ENDIF

             IF ::ShowTabs
                IF ! ::ColorInactiveHeader
                   IF nState == TIS_HOT .AND. oPage:__TabBrush != NIL
                      _FillRect( hMemDC, aItem, oPage:__TabBrush )
                   ELSE
                      DrawThemeBackground( ::System:hTabTheme, hMemDC, TABP_TABITEM, nState, aItem )
                   ENDIF
                ELSEIF nState == TIS_HOT
                   _FillRect( hMemDC, aItem, hHotBrush )
                ENDIF
                IF ::ImageList != NIL .AND. oPage != NIL .AND. oPage:ImageIndex > 0
                   ::ImageList:DrawImage( hMemDC, oPage:ImageIndex, aItem[1]+6, aItem[2]+3, ILD_TRANSPARENT )
                   aItem[1]+=::ImageList:IconWidth + 6
                ENDIF
             ENDIF
             IF !EMPTY( tci:pszText )
                SetBkMode( hMemDC, TRANSPARENT )
                hOldFont := SelectObject( hMemDC, ::Font:Handle )

                //aSize    := _GetTextExtentPoint32( hMemDC, tci:pszText  )
                _DrawText( hMemDC, tci:pszText, aItem, (DT_CENTER|DT_VCENTER|DT_SINGLELINE ))
                SelectObject( hMemDC, hOldFont )
             ENDIF
             SetTextColor( hMemDC, nColor )
          ENDIF
      NEXT
      IF hHotBrush != NIL
         DeleteObject( hHotBrush )
      ENDIF
      oPage := ::GetItemObject()
      aItem := ::GetItemRect( nSel )

      aItem[1]-= 2
      aItem[3]+= 2
      aItem[2]-= 1
      aItem[4]+= 2

      nPart  := TABP_TABITEM
      nState := TIS_SELECTED

      IF nSel == 1
         nPart  := TABP_TOPTABITEMLEFTEDGE
       ELSEIF aItem[3] == aRect[3]
         nPart  := TABP_TOPTABITEMRIGHTEDGE
      ENDIF

      IF ::ShowTabs .AND. oPage != NIL
         IF oPage:__TabBrush != NIL
            _FillRect( hMemDC, aItem, oPage:__TabBrush )
          ELSE
            DrawThemeBackground( ::System:hTabTheme, hMemDC, nPart, nState, aItem, aItem )
         ENDIF
      ENDIF
      aItem[4]-=2
      n := nSel
      oPage := ::GetItemObject(n)

      IF ::ShowTabs .AND. ::ImageList != NIL .AND. oPage != NIL .AND. oPage:ImageIndex > 0
         ::ImageList:DrawImage( hMemDC, oPage:ImageIndex, aItem[1]+6, aItem[2]+3, ILD_TRANSPARENT )
         aItem[1]+=::ImageList:IconWidth + 3
      ENDIF

      tci:mask := TCIF_TEXT
      tci:pszText := SPACE(255)
      tci:cchTextMax := 255
      SendMessage( ::hWnd, TCM_GETITEM, n-1, @tci )

      IF ! EMPTY( tci:pszText )
         nColor := GetTextColor( hMemDC )
         SetTextColor( hMemDC, IIF( oPage == NIL, ::ForeColor, IIF( oPage:TabTextColor != NIL, oPage:TabTextColor, IIF( oPage:ForeColor != NIL, oPage:ForeColor, ::ForeColor ) ) ) )
         nBkMode := SetBkMode( hMemDC, TRANSPARENT )
         hOldFont := SelectObject( hMemDC, ::Font:Handle )
         _DrawText( hMemDC, tci:pszText, aItem, (DT_CENTER|DT_VCENTER|DT_SINGLELINE) )
         SetBkMode( hMemDC, nBkMode )
         SelectObject( hMemDC, hOldFont )
         SetTextColor( hMemDC, nColor )
      ENDIF
      IF !::FocusNever .AND. ::HasFocus
         aRect := ::GetItemRect( nSel )
         IF nSel > 0 .AND. oPage:__TabBrush != NIL

            pBrush := (struct LOGBRUSH)
            pBrush:lbStyle := BS_SOLID
            pBrush:lbColor := oPage:TabTextColor //DIB_RGB_COLORS
            pBrush:lbHatch := NIL

            hPen := SelectObject( hMemDC, ExtCreatePen( (PS_GEOMETRIC | PS_USERSTYLE), 1, pBrush, {1,2} ) )
            SelectObject( hMemDC, GetStockObject( NULL_BRUSH ) )

            Rectangle( hMemDC, aRect[1], aRect[2]+2, aRect[3], aRect[4] )
            DeleteObject( SelectObject( hMemDC, hPen ) )
         ELSE
            _DrawFocusRect( hMemDC, { aRect[1],aRect[2]+2,aRect[3],aRect[4]} )
         ENDIF
      ENDIF

   ENDIF
   BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )
   ::EndPaint()
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD SetItemValues( nItem, cText, nImgIndex, hWnd ) CLASS TabControl
   LOCAL tci := (struct TCITEM)
   tci:mask    := (TCIF_IMAGE | TCIF_TEXT | TCIF_PARAM)
   tci:pszText := cText
   tci:iImage  := nImgIndex-1
   tci:lParam  := hWnd
   DEFAULT cText TO ""
   tci:cchTextMax := LEN( cText )
   SendMessage( ::hWnd, TCM_SETITEM,  nItem, tci )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnLButtonDown() CLASS TabControl
   LOCAL oPage
   ::CallWindowProc()
   IF !::__lMouseDown
      oPage := ::GetItemObject()
      IF oPage != NIL .AND. ::GetItemObject():AllowDrag
         ::__lMouseDown := .T.
         ::Cursor := ::System:Cursor:SizeWE
      ENDIF
   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS TabControl
   LOCAL nSel, lAllow, nOldSel, oCurPage, oSelPage, aRect, h
   (nwParam, nlParam)
   DO CASE
      CASE hdr:code == NM_CLICK
           ::__lMouseDown := .F.
           ::Cursor := NIL

      CASE hdr:code == TCN_SELCHANGING
           IF !::ShowTabs
              RETURN .T.
           ENDIF
           nSel := TabCtrl_GetCurSel( ::hWnd ) + 1
           lAllow := ::OnTabChanging( ::Cursel, nSel )

           IF VALTYPE( lAllow := ExecuteEvent( "OnTabChanging", Self ) ) != "L"
              lAllow := NIL
           ENDIF

           DEFAULT lAllow TO .T.
           IF !lAllow
              RETURN(1)
           ENDIF

      CASE hdr:code == TCN_SELCHANGE
           nSel := TabCtrl_GetCurSel( ::hWnd ) + 1
           oSelPage := ::GetItemObject( nSel )
           IF ::Cursel != nSel
              IF oSelPage != NIL .AND. ! oSelPage:IsWindowEnabled()
                 ::SetCurSel( ::Cursel )
                 RETURN 0
              ENDIF
              oCurPage := ::GetItemObject()
              IF oCurPage != NIL
                 oCurPage:LastFocus := ::System:GetFocus()
                 IF oCurPage:LastFocus != NIL
                    oCurPage:LastFocus:TabValidate := .F.
                 ENDIF
                 oCurPage:HideTab()
              ENDIF
              nOldSel := ::CurSel
              ::CurSel := nSel

              oCurPage := ::GetItemObject()

              IF oCurPage != NIL
                 oCurPage:__OnParentSize( ::ClientWidth, ::ClientHeight )
                 oCurPage:ShowTab()
                 IF oCurPage:Action != NIL
                    Eval( oCurPage:Action, oCurPage )
                 ENDIF
                 IF GetFocus() != ::hWnd
                    IF oCurPage:LastFocus != NIL .AND. oCurPage:LastFocus:IsChildOf( oCurPage:hWnd )
                       oCurPage:LastFocus:SetFocus(.T.)
                       IF oCurPage:LastFocus:ClsName == "DataGrid"
                          oCurPage:LastFocus:InvalidateRect(, .F.)
                       ENDIF
                    ELSEIF ! Empty( oCurPage:Children )
                       IF ( h := GetNextDlgTabItem( oCurPage:hWnd ) ) <> 0
                          SetFocus(h)
                       ELSE
                          oCurPage:Children[1]:SetFocus( .T. )
                       ENDIF
                    ENDIF

                 ENDIF
              ENDIF
              aRect := ::GetItemRect( nSel )
              aRect[1] -= 2
              aRect[2] -= 2
              aRect[3] += 2
              aRect[4] += 2

              IF oCurPage != NIL .AND. oCurPage:TopMargin > 0
                 aRect[1] := 0
                 aRect[3] := ::ClientWidth
                 aRect[4] += oCurPage:TopMargin
              ENDIF

              ::Redraw( aRect )

              oSelPage := ::GetItemObject( nOldSel )
              IF oSelPage != NIL
                 IF oSelPage:LastFocus != NIL
                    oSelPage:LastFocus:TabValidate := .T.
                 ENDIF
              ENDIF
              ::OnTabChanged( nOldSel, nSel )
              IF VALTYPE( ::OnSelChanged ) == "B"
                 EVAL( ::OnSelChanged, Self, nOldSel, nSel )
              ENDIF
              ExecuteEvent( "OnTabChanged", Self )

              ExecuteEvent( "OnSelected", oCurPage )

              IF ::__xCtrlName == "TabStrip"
                 ::Redraw()
                 ::UpdateWindow()
              ENDIF
              RETURN 0
           ENDIF
   ENDCASE
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD InsertTab(cText, nPos, nImg) CLASS TabControl
   LOCAL tci, lAllow
   DEFAULT nPos TO ::GetItemCount()+1
   lAllow := ::OnTabInsert( nPos)
   IF VALTYPE( lAllow := ExecuteEvent( "OnTabInsert", Self ) ) != "L"
      lAllow := NIL
   ENDIF
   DEFAULT lAllow TO .T.
   IF lAllow
      tci := (struct TCITEM)
      tci:mask    := TCIF_IMAGE + TCIF_TEXT
      tci:pszText := cText
      IF nImg != NIL
         tci:iImage  := nImg-1
      ENDIF
      RETURN SendMessage( ::hWnd, TCM_INSERTITEM, nPos, tci ) > -1
   ENDIF
RETURN .F.

//-----------------------------------------------------------------------------------------------
METHOD DeleteTab(nPos) CLASS TabControl
   LOCAL oPage
   oPage := ::GetItemObject( nPos )
   IF oPage != NIL
      oPage:Delete()
    ELSE
      TabCtrl_DeleteItem(::hWnd, nPos-1)
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD DeleteAllTabs() CLASS TabControl
   LOCAL n, nTabs := ::GetItemCount()
   FOR n := nTabs TO 1 STEP -1
       ::DeleteTab(n)
   NEXT
   ::CurSel := 0
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD GetTabPosByName( cName ) CLASS TabControl
   LOCAL x, n := ::GetItemCount(), tci, cText, nRet := 0
   tci := (struct TCITEM)
   FOR x := 1 TO n
       tci:mask       := TCIF_TEXT
       tci:pszText    := SPACE( 255 )
       tci:cchTextMax := 255
       SendMessage( ::hWnd, TCM_GETITEM, x-1, @tci )
       cText := ALLTRIM( STRTRAN( tci:pszText, CHR(0) ) )
       IF UPPER( cText ) == UPPER( cName )
          nRet := x
          EXIT
       ENDIF
   NEXT
RETURN nRet


//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS TabHolder INHERIT Panel
   DATA TabCtrl EXPORTED
   METHOD Redock() INLINE ::TabCtrl:Redock()
   METHOD OnSysCommand(n) INLINE IIF( ::TabCtrl:OnSysCommand(n) == 0, (::Hide(),0),)
ENDCLASS

//-----------------------------------------------------------------------------------------------
//----------------------------------------- P A G E ---------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS TabPage INHERIT Dialog
   PROPERTY ImageIndex     SET ::__SetImageIndex(v)     DEFAULT 0
   PROPERTY Position       SET ::__SetPosition(v)
   PROPERTY Visible        SET ::__SetVisible(v)        DEFAULT .T.
   DATA VertScrollSize //SET ::__SetVertScrollSize(v) DEFAULT 0
   DATA HorzScrollSize //SET ::__SetHorzScrollSize(v) DEFAULT 0
   PROPERTY Text           SET ::__SetCaption(v)
   PROPERTY Theming        ROOT "Appearance" SET ::EnableThemeDialogTexture( IIF( v, ETDT_ENABLETAB, ETDT_DISABLE ) ) DEFAULT IsThemeActive()

   PROPERTY TabBackColor   ROOT "Colors"     SET ::__SetTabColor(v)
   PROPERTY TabTextColor   ROOT "Colors"

   PROPERTY TopMargin      DEFAULT 0
   PROPERTY LeftMargin     DEFAULT 0
   PROPERTY RightMargin    DEFAULT 0
   PROPERTY BottomMargin   DEFAULT 0
   PROPERTY AllowDrag      DEFAULT .F.

   DATA Index              EXPORTED
   DATA ColorMode          EXPORTED INIT 0
   DATA ItemSize           EXPORTED
   DATA LastFocus          EXPORTED
   DATA __xCtrlName        EXPORTED
   DATA __TabBrush         EXPORTED

   DATA MinWidth           EXPORTED  INIT 0
   DATA MinHeight          EXPORTED  INIT 0
   DATA MaxWidth           EXPORTED  INIT 0
   DATA MaxHeight          EXPORTED  INIT 0

   DATA Left               EXPORTED
   DATA Top                EXPORTED
   DATA Width              EXPORTED
   DATA Height             EXPORTED

   DATA Modal              EXPORTED INIT .T.
   DATA AutoClose          EXPORTED INIT .T.
   DATA ShowMode           EXPORTED INIT 1
   DATA Center             EXPORTED INIT .F.
   DATA Icon               EXPORTED

   ACCESS ControlParent    INLINE (::ExStyle & WS_EX_CONTROLPARENT) != 0
   ACCESS MdiContainer     INLINE ::xMdiContainer

   ACCESS SysMenu          INLINE (::Style & WS_SYSMENU) != 0
   ACCESS ThickFrame       INLINE (::Style & WS_THICKFRAME) != 0
   ACCESS MinimizeBox      INLINE (::Style & WS_MINIMIZEBOX) != 0
   ACCESS MaximizeBox      INLINE (::Style & WS_MAXIMIZEBOX) != 0

   ACCESS TabStop          INLINE (::Style & WS_TABSTOP) != 0
   ACCESS ClipSiblings     INLINE (::Style & WS_CLIPSIBLINGS) != 0
   ACCESS CaptionBar       INLINE (::Style & WS_CAPTION) != 0
   ACCESS ClipChildren     INLINE (::Style & WS_CLIPCHILDREN) != 0
   ACCESS Transparent      INLINE (::ExStyle & WS_EX_TRANSPARENT) != 0
   ACCESS FrameStyle       INLINE ::xFrameStyle
   ACCESS DlgModalFrame    INLINE ::xDlgModalFrame
   ACCESS Icon             INLINE ::xIcon
   ACCESS Opacity          INLINE ::xOpacity
   ACCESS TabOrder         INLINE ::xTabOrder
   ACCESS TopMost          INLINE ::xTopMost

   ACCESS Form             INLINE IIF( ::Parent != NIL, ::Parent:Form, Self )

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnGetDlgCode()   INLINE DLGC_WANTMESSAGE
   METHOD Destroy()        INLINE ::Delete()
   METHOD HideTab()        INLINE ShowWindow( ::hWnd, SW_HIDE ), ::UpdateWindow()//Super:Hide()
   METHOD ShowTab()        INLINE ShowWindow( ::hWnd, SW_SHOW ), ::UpdateWindow()//Super:Show()
   METHOD OnHotKey()       INLINE ::Select(), NIL
   METHOD OnCtlColorDlg()
   METHOD Delete()
   METHOD Select()
   METHOD __SetImageIndex()
   METHOD __SetCaption()
   METHOD __Hide()
   METHOD __Show()
   METHOD __SetPosition()
   METHOD __GetPosition()
   METHOD __OnParentSize()
   METHOD __CreateProperty()
   METHOD __SetVisible()
   METHOD __CreateBkBrush()
   METHOD __SetTabColor()
   METHOD __ResetHandle()  INLINE ::hWnd := NIL, ::__nProc := NIL, ::Template := NIL
   METHOD OnMouseLeave(n,l) INLINE ::Super:OnMouseLeave(n,l), ::InvalidateRect(, .F. ), NIL
   METHOD OnEraseBkgnd()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent, acResource ) CLASS TabPage
   DEFAULT ::__xCtrlName TO "TabPage"

   ::Super:Init( oParent, acResource )
   ::__hObjects    := NIL
   ::Style         := (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::ExStyle       := WS_EX_CONTROLPARENT
   ::ClsName       := "TabPage"
   ::ClipChildren  := .T.
   ::ClipSiblings  := .T.
   ::modal         := .F.
   ::AutoClose     := .F.
   ::IsContainer   := .T.
   ::Dock:Margin   := 0

   ::__lResizeable := {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
   ::__lMoveable   := .F.
   ::__lCopyCut    := .F.
   ::__IsControl   := .T.
   ::__IsStandard  := .F.
   ::__lAllowCopy  := .F.

   IF ::DesignMode
      ::Events := ;
             { ;
               {"Command",     {;
                               { "OnSelected"         , "", "" },;
                               { "OnParentCommand"    , "", "" },;
                               { "OnSysCommand"       , "", "" },;
                               { "OnToolTipNotify"    , "", "" } } },;
               {"Color",       {;
                               { "OnCtlColorBtn"      , "", "" },;
                               { "OnCtlColorDlg"      , "", "" },;
                               { "OnCtlColorEdit"     , "", "" },;
                               { "OnCtlColorListBox"  , "", "" },;
                               { "OnCtlColorScrollBar", "", "" },;
                               { "OnCtlColorStatic"   , "", "" },;
                               { "OnSysColorChange"   , "", "" } } },;
               {"Drag & Drop", {;
                               { "OnDropFiles"        , "", "" } } },;
               {"Drawing",     {;
                               { "OnEraseBkGnd"       , "", "" },;
                               { "OnPaint"            , "", "" } } },;
               {"Layout",      { ;
                               { "OnEnterSizeMove"    , "", "" },;
                               { "OnExitSizeMove"     , "", "" },;
                               { "OnGetMinMaxInfo"    , "", "" },;
                               { "OnMove"             , "", "" },;
                               { "OnMoving"           , "", "" },;
                               { "OnParentMove"       , "", "" },;
                               { "OnSize"             , "", "" },;
                               { "OnSizing"           , "", "" },;
                               { "OnWindowPosChanged" , "", "" },;
                               { "OnWindowPosChanging", "", "" } } },;
               {"Mouse",       {;
                               { "OnLButtonDblClk"    , "", "" },;
                               { "OnLButtonDown"      , "", "" },;
                               { "OnLButtonUp"        , "", "" },;
                               { "OnMButtonDown"      , "", "" },;
                               { "OnMButtonUp"        , "", "" },;
                               { "OnMouseActivate"    , "", "" },;
                               { "OnMouseHover"       , "", "" },;
                               { "OnMouseleave"       , "", "" },;
                               { "OnMouseMove"        , "", "" },;
                               { "OnRButtonDown"      , "", "" },;
                               { "OnRButtonUp"        , "", "" } } },;
               {"Parent",      {;
                               { "OnParentDrawItem"   , "", "" },;
                               { "OnParentNotify"     , "", "" },;
                               { "OnParentSysCommand" , "", "" } } },;
               {"Scroll",      {;
                               { "OnHorzScroll"       , "", "" },;
                               { "OnVertScroll"       , "", "" } } },;
               {"Timer",       {;
                               { "OnTimer"            , "", "" } } },;
               {"User",        {;
                               { "OnUserMsg"          , "", "" } } },;
               {"Window",      {;
                               { "OnActivate"         , "", "" },;
                               { "OnCreate"           , "", "" },;
                               { "OnDestroy"          , "", "" },;
                               { "OnEnable"           , "", "" },;
                               { "OnHideWindow"       , "", "" },;
                               { "OnSetCursor"        , "", "" },;
                               { "OnSetFont"          , "", "" },;
                               { "OnShowWindow"       , "", "" } } } }
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetTabColor( nColor ) CLASS TabPage
   IF ::__TabBrush != NIL
      DeleteObject( ::__TabBrush )
   ENDIF
   IF nColor != NIL
      ::__TabBrush := CreateSolidBrush( nColor )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnEraseBkgnd( hDC ) CLASS TabPage
   IF ::BkBrush != NIL
      ::GetClientRect()
      _FillRect( hDC, { 0, 0, ::ClientWidth, ::ClientHeight }, ::BkBrush )
      IF ::DesignMode .AND. ::Application:ShowGrid == 1
         DrawGrid( hDC, ::Form:CtrlMask:xGrid, ::Form:CtrlMask:yGrid, ::ClientWidth, ::ClientHeight, RGB(0,0,0) )
      ENDIF
      RETURN 1
   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS TabPage
   LOCAL tci, rc, n, cText

   IF EMPTY( ::Caption )
      ::Caption := NIL
   ENDIF

   IF ::Parent:BoldSelection .AND. ::Caption != NIL
      ::Parent:Font:Bold := .T.
   ENDIF

   tci := (struct TCITEM)
   tci:mask    := (TCIF_IMAGE | TCIF_TEXT | TCIF_PARAM)

   cText := ::Caption

   IF ! ::DesignMode
      IF VALTYPE(cText)=="C" .AND. LEFT(cText,2)=="{|"
         cText := &cText
      ENDIF
      IF VALTYPE(cText)=="B"
         cText := EVAL(cText)
      ENDIF
   ENDIF

   tci:pszText := cText
   tci:iImage  := IIF( ::ImageIndex > 0, ::ImageIndex-1, -1 )
   IF ::Parent:hWnd == NIL
      ::Parent:Create()
   ENDIF
   ::Border := 0
   ::Super:Create()
   tci:lParam := ::hWnd
   ::EnableThemeDialogTexture( IIF( ::Parent:Theming, ETDT_ENABLETAB, ETDT_DISABLE ) )
/*
   IF ::VertScrollSize > 0
      ::OriginalRect[4] := ::VertScrollSize
    ELSE
      ::VertScrollSize := ::ClientHeight
      IF ::DesignMode
         __SetInitialValues( Self, "VertScrollSize", ::ClientHeight )
      ENDIF
   ENDIF
   IF ::HorzScrollSize > 0
      ::OriginalRect[3] := ::HorzScrollSize
    ELSE
      ::HorzScrollSize := ::ClientWidth
      IF ::DesignMode
         __SetInitialValues( Self, "HorzScrollSize", ::ClientWidth )
      ENDIF
   ENDIF
*/
   IF SendMessage( ::Parent:hWnd, TCM_INSERTITEM, SendMessage( ::Parent:hWnd, TCM_GETITEMCOUNT ), tci ) > -1

      ::xPosition := SendMessage( ::Parent:hWnd, TCM_GETITEMCOUNT )
      AADD( ::Parent:Children, Self )

      rc := (struct RECT)
      rc:Left  := 0
      rc:Top   := 0
      rc:Right := ::Parent:Width
      rc:Bottom:= ::Parent:Height

      SendMessage( ::Parent:hWnd, TCM_ADJUSTRECT, 0, @rc )

      ::xLeft   := 0               + ::LeftMargin
      ::xTop    := rc:Top          + ::TopMargin
      ::xWidth  := ::Parent:Width  - ::RightMargin - ::LeftMargin
      ::xheight := ::Parent:height - ::BottomMargin - ::TopMargin

      ::MoveWindow()

      IF ::DesignMode .AND. ::__CustomOwner
         __SetInitialValues( Self, "Caption", cText )
         __SetInitialValues( Self, "ImageIndex", cText )

         IF ::__SysBackColor != ::xBackColor .AND. ::xBackColor != NIL
            ::__SysBackColor := ::xBackColor
         ENDIF
         IF ::__SysForeColor != ::xForeColor .AND. ::xForeColor != NIL
            ::__SysForeColor := ::xForeColor
         ENDIF

         IF ::__CustomOwner
            ::__OriginalName := ::Name
         ENDIF
      ENDIF

//      ::RegisterDocking()

      IF ! ::DesignMode
         //SetParent( ::hWnd, ::Parent:Parent:hWnd )
         IF !EMPTY( cText ) .AND. ( n := AT( "&", cText ) ) > 0
            RegisterHotKey( ::hWnd, ::Id, MOD_ALT, ASC( cText[n+1] ) )
         ENDIF
      ENDIF

      IF ::Parent:GetItemCount() != ::Parent:Cursel .OR. ( ! ::DesignMode .AND. !::Parent:Visible )
         ::HideTab()
       ELSE
         ::ShowTab()
         ::Parent:SetCurSel( ::Parent:Cursel )
      ENDIF

      ::__OnParentSize( ::ClientWidth, ::ClientHeight )

      IF ::DesignMode
         ::Application:ObjectTree:Set( Self )
         ::Application:Props[ "ComboSelect" ]:Reset()
      ENDIF
      RedrawWindow( ::Parent:hWnd,,, (RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT ))
      ::Parent:InvalidateRect(,.f.)
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __CreateProperty( cBaseName ) CLASS TabPage
   ::Object:__CreateProperty( cBaseName )
RETURN SELF

//-----------------------------------------------------------------------------------------------
METHOD __CreateBkBrush( hDC ) CLASS TabPage
   LOCAL hMemBitmap, hOldBitmap, hMemDC, hBrush

   IF ::xBackColor == NIL .OR. ( ::BackgroundImage != NIL .AND. ::BackgroundImage:hDIB != NIL )
      DEFAULT hDC TO ::Drawing:hDC

      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)

      IF ::xBackColor != NIL
         hBrush := CreateSolidBrush( ::xBackColor )
       ELSEIF ! ::Parent:Theming
         hBrush := GetSysColorBrush( COLOR_BTNFACE )
      ENDIF

      IF hBrush == NIL .AND. ::Application:OsVersion:dwMajorVersion >= 5
         DrawThemeBackground( ::System:hTabTheme, hMemDC, TABP_BODY, 0, { 0, 0, ::xWidth, ::xHeight } )
       ELSEIF hBrush != NIL .AND. hBrush <> 0
         _FillRect( hMemDC, { 0, 0, ::xWidth, ::xHeight }, hBrush )
      ENDIF

      IF ::BackgroundImage != NIL .AND. ::BackgroundImage:hDIB != NIL
         ::BackgroundImage:Draw( hMemDC, hMemBitmap )
      ENDIF

      IF ::BkBrush != NIL
         DeleteObject( ::BkBrush )
      endif
      ::BkBrush   := CreatePatternBrush( hMemBitmap )

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
      IF ::xBackColor != NIL
         DeleteObject( hBrush )
      ENDIF
   ENDIF
RETURN 1

//-----------------------------------------------------------------------------------------------
METHOD __SetVisible( lVisible, lForce ) CLASS TabPage
   DEFAULT lForce TO .F.
   IF ( lVisible != ::Visible .OR. lForce ) .AND. ! ::DesignMode .AND. ::hWnd != NIL
      IF ! lVisible
         ::__Hide()
       ELSE
         ::__Show()
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __GetPosition() CLASS TabPage
   LOCAL n, nRet := 0
   FOR n := 1 TO ::Parent:GetItemCount()
       IF ::Parent:GetItemObject( n ):hWnd == ::hWnd
          nRet := n
          EXIT
       ENDIF
   NEXT
RETURN nRet

//-----------------------------------------------------------------------------------------------
METHOD __SetPosition(n) CLASS TabPage
   LOCAL tci

   ::Parent:SendMessage( TCM_DELETEITEM, ::__GetPosition() - 1 )

   tci := (struct TCITEM)
   tci:mask    := (TCIF_IMAGE | TCIF_TEXT | TCIF_PARAM)
   tci:pszText := ::Caption
   tci:iImage  := IIF( ::ImageIndex > 0, ::ImageIndex-1, -1 )
   tci:lParam  := ::hWnd
   SendMessage( ::Parent:hWnd, TCM_INSERTITEM, n-1, tci )

   ::Parent:Cursel := TabCtrl_GetCurSel( ::Parent:hWnd ) + 1

   ADEL( ::Parent:Children, ::xPosition, .T. )
   IF n > LEN( ::Parent:Children )
      AADD( ::Parent:Children, Self )
    ELSE
      AINS( ::Parent:Children, n, Self, .T. )
   ENDIF
   FOR n := 1 TO LEN( ::Parent:Children )
       ::Parent:Children[n]:xPosition := n
       IF ::Parent:Children[n]:DesignMode
          __SetInitialValues( ::Parent:Children[n], "Position", n )
       ENDIF
   NEXT

RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Delete() CLASS TabPage
   LOCAL oParent := ::Parent
   oParent:SendMessage( TCM_DELETEITEM, ::__GetPosition() - 1 )
   Super:Destroy()

   oParent:SetCurSel( MIN( oParent:CurSel, oParent:GetItemCount() ) )
   IF oParent:CurSel > 0 .AND. oParent:GetItemCount() >= oParent:CurSel
      oParent:GetItemObject():ShowTab()
   ENDIF
   IF ::__TabBrush != NIL
      DeleteObject( ::__TabBrush )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetCaption( c ) CLASS TabPage
   LOCAL n
   ::xCaption := c
   IF ::IsWindow()
      ::Parent:SetItemValues( ::__GetPosition()-1, c, ::xImageIndex, ::hWnd )
   ENDIF
   IF !EMPTY( c ) .AND. ( n := AT( "&", c ) ) > 0
      RegisterHotKey( ::hWnd, ::Id, MOD_ALT, ASC( ::Caption[n+1] ) )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetImageIndex( nImgIndex ) CLASS TabPage
   LOCAL tci
   IF ::hWnd != NIL
      tci := (struct TCITEM)
      tci:mask    := (TCIF_IMAGE | TCIF_TEXT)
      tci:pszText := ::Caption
      tci:iImage  := nImgIndex-1
      SendMessage( ::Parent:hWnd, TCM_SETITEM,  ::__GetPosition()-1, tci )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnCtlColorDlg( nwParam, nlParam ) CLASS TabPage
   IF ( ::ColorMode == 0 .OR. ::ColorMode == 1 ) .AND. ::BkBrush != NIL
      RETURN( ::BkBrush )
   ENDIF
RETURN ::Super:OnCtlColorDlg( nwParam, nlParam )

//-----------------------------------------------------------------------------------------------
METHOD __OnParentSize( x, y, hDef ) CLASS TabPage
   LOCAL rc, aRect, nBorder := 0
   (x, y)
   rc := (struct RECT)
   aRect := _GetClientRect( ::Parent:hWnd )
   rc:Left   := 0
   rc:Top    := 0
   rc:Right  := aRect[3]
   rc:Bottom := aRect[4]

   SendMessage( ::Parent:hWnd, TCM_ADJUSTRECT, 0, @rc )

   IF ::Application:ThemeActive .AND. ::Parent:Theming
      rc:Left   -= 3
      rc:Top    -= 1
      rc:Right  += 1
      rc:Bottom += 2
    ELSE
      rc:Left   -= 2
      rc:Top    -= 1
      rc:Right  += 2
      rc:Bottom += 2
      IF (::Parent:Style & TCS_VERTICAL) == TCS_VERTICAL
         rc:Top  -= 1
         IF (::Parent:Style & TCS_BOTTOM) == 0 // Left
            rc:Left  += 2
          ELSE
            rc:Right -= 2
         ENDIF
       ELSEIF (::Parent:Style & TCS_BOTTOM) == TCS_BOTTOM
         rc:Top    -= 1
         rc:Bottom -= 2
      ENDIF
   ENDIF

   ::Left    := rc:Left + ::LeftMargin
   ::Top     := rc:Top  + ::TopMargin
   ::Width   := rc:Right - rc:left - ::RightMargin - ::LeftMargin
   ::Height  := rc:Bottom - rc:top - ::BottomMargin - ::TopMargin

   IF ::Parent:__xCtrlName == "TabStrip"
      DO CASE
         CASE ::Parent:TabPosition == 1 // Left
              ::Left += 2
              ::Top -= 2
              ::Height += 4

         CASE ::Parent:TabPosition == 2 // Top
              ::Width += 2
              ::Height += 2

         CASE ::Parent:TabPosition == 3 // Right
              ::Top -= 2
              ::Height += 4

         CASE ::Parent:TabPosition == 4 // Bottom
              ::Top -= 2
              ::Width += 2
              ::Height += 2
      ENDCASE
      IF ::Application:OsVersion:dwMajorVersion > 5
         ::Height --
      ENDIF
   ENDIF

   IF ::Parent:Border <> 0
      nBorder := 1
   ENDIF
   ::MoveWindow( ::Left-nBorder, ::Top-nBorder, ::Width+(nBorder*4), ::Height+(nBorder*3) )
   ::__CreateBkBrush()

RETURN hDef

//-----------------------------------------------------------------------------------------------
METHOD Select() CLASS TabPage
   ::Parent:GetItemObject():HideTab()
   IF ::Parent:Visible
      ::ShowTab()
   ENDIF
   ::Parent:SetCurSel( ::__GetPosition() )
   ::__OnParentSize( ::ClientWidth, ::ClientHeight )
   ::Parent:Redraw()
   ExecuteEvent( "OnSelected", Self )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __Hide() CLASS TabPage
   LOCAL nPos := ::__GetPosition()

   ::Parent:SendMessage( TCM_DELETEITEM, nPos - 1, 0 )

   IF ::Parent:CurSel == nPos
      Super:Hide()
      IF ::Parent:GetItemCount() > 0
         ::Parent:SetCurSel( MIN( ::Parent:CurSel, ::Parent:GetItemCount() ) )
         ::Parent:GetItemObject():ShowTab()
      ENDIF
    ELSEIF ::Parent:CurSel > nPos
      ::Parent:CurSel--
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __Show() CLASS TabPage
   LOCAL n := 0, oCur, tci := (struct TCITEM)

   oCur := ::Parent:GetItemObject()

   ::xVisible  := .T.
   ::Parent:SendMessage( TCM_DELETEALLITEMS, 0, 0 )

   FOR n := 1 TO LEN( ::Parent:Children )
       IF ::Parent:Children[ n ]:ClsName == "TabPage"
          IF ::Parent:Children[ n ]:xVisible
             tci:mask    := (TCIF_IMAGE | TCIF_TEXT | TCIF_PARAM)
             tci:pszText := ::Parent:Children[ n ]:xText
             tci:lParam  := ::Parent:Children[ n ]:hWnd
             tci:iImage  := IIF( ::Parent:Children[ n ]:ImageIndex > 0, ::Parent:Children[ n ]:ImageIndex-1, -1 )
             SendMessage( ::Parent:hWnd, TCM_INSERTITEM, SendMessage( ::Parent:hWnd, TCM_GETITEMCOUNT ), tci )
          ENDIF
       ENDIF
   NEXT
   ::Application:DoEvents()

   //oCur:Select()
RETURN Self

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS TabStrip INHERIT TabControl
   DATA AllowMaximize, ClipChildren, ClipSiblings, NoActivate PROTECTED INIT .F.
   DATA Caption, AnimationStyle
   DATA BoldSelection    EXPORTED INIT .F.
   DATA ItemHeight       EXPORTED INIT 19
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD OnPaint()
   METHOD DrawTabStripItem()
   METHOD __GetGradienBrush()
   METHOD OnEraseBkGnd() INLINE 1
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS TabStrip
   ::__xCtrlName := "TabStrip"
   ::Super:Init( oParent )
RETURN Self

METHOD Create() CLASS TabStrip
   ::Super:Create()
   IF ::ShowTabs
      ::SendMessage( TCM_SETITEMSIZE, 0, MAKELPARAM(0,::ItemHeight) )
   ENDIF
RETURN Self
//-----------------------------------------------------------------------------------------------
METHOD OnPaint() CLASS TabStrip
   LOCAL hOldPen, aRect, nSel, aItem, nItems, n
   LOCAL hDC, hMemDC, hMemBitmap, hOldBitmap, hBrush//, hMemDC1, hOldBitmap1

   aRect := _GetClientRect( ::hWnd )
   nSel  := TabCtrl_GetCurSel( ::hWnd )+1
   IF nSel == 0
      ::SetCurSel(1)
   ENDIF

   hDC := ::BeginPaint()

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   //---------------------------------------------------------------------------
   hBrush := ::GetBkBrush()

   IF hBrush != NIL
      _FillRect( hMemDC, { 0, 0, ::ClientWidth, ::ClientHeight }, hBrush )
   ENDIF
   //---------------------------------------------------------------------------


   aItem := ::GetItemRect( 1 )

   DO CASE
      CASE ::xTabPosition == 1
           aRect[1] := aItem[3]

      CASE ::xTabPosition == 2
           aRect[2] := aItem[4]

      CASE ::xTabPosition == 3
           aRect[3] -= ( aRect[3]-aItem[1] )

      CASE ::xTabPosition == 4
           aRect[4] -= ( aRect[4]-aItem[2] )
   ENDCASE

   hOldPen := SelectObject( hMemDC, CreatePen( PS_SOLID, 0, RGB(128,128,128) ) )
   Rectangle( hMemDC, aRect[1], aRect[2], aRect[3], aRect[4] )
   DeleteObject( SelectObject( hMemDC, hOldPen ) )

   IF ::ShowTabs
      IF ( nItems := ::GetItemCount() ) > 0
         FOR n := nItems TO 1 STEP -1
             IF n != nSel
                ::DrawTabStripItem( hMemDC, .F., n )
             ENDIF
         NEXT
         ::DrawTabStripItem( hMemDC, .T., nSel )
      ENDIF
   ENDIF

   BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )

   SelectObject( hMemDC, hOldBitmap )
   DeleteDC( hMemDC )
   DeleteObject( hMemBitmap )
   ::EndPaint()
RETURN 0


//-----------------------------------------------------------------------------------------------
METHOD DrawTabStripItem( hDC, lCurSel, nSel ) CLASS TabStrip
   LOCAL d, hRgn, aRect, n := 0, aItem := ::GetItemRect( nSel )
   LOCAL x, y, hOldFont, hBrush, hOldPen, tci, lGrad := .F., lVert := .F.
   LOCAL oPage
   DEFAULT lCurSel TO .T.
   IF lCurSel
      n := 1
   ENDIF
   DO CASE
      CASE ::xTabPosition == 1
           lVert := .T.
           d := Round( (aItem[3]-aItem[1])/2, 0 ) + 0
           aRect := { { aItem[3]+n, aItem[2]      },;
                      { aItem[1]+2, aItem[2]      },;
                      { aItem[1],   aItem[2]+ 2   },;
                      { aItem[1],   aItem[4]-10   },;
                      { aItem[1]+3, aItem[4]- 6   },;
                      { aItem[3]+n, aItem[4]+ d+n } }
      CASE ::xTabPosition == 2
           d := Round( (aItem[4]-aItem[2])/2, 0 ) + 0
           aRect := { { aItem[1],      aItem[4]+n },;
                      { aItem[1],      aItem[2]+2 },;
                      { aItem[1]+ 2,   aItem[2]   },;
                      { aItem[3]-10,   aItem[2]   },;
                      { aItem[3]- 6,   aItem[2]+3 },;
                      { aItem[3]+ d+n, aItem[4]+n } }
      CASE ::xTabPosition == 3
           d := Round( (aItem[3]-aItem[1])/2, 0 ) + 0
           lVert := .T.
           aRect := { { aItem[1]-n, aItem[2]      },;
                      { aItem[3]-2, aItem[2]      },;
                      { aItem[3],   aItem[2]+ 2   },;
                      { aItem[3],   aItem[4]-10   },;
                      { aItem[3]-3, aItem[4]- 6   },;
                      { aItem[1]-n, aItem[4]+ d+n } }
      CASE ::xTabPosition == 4
           d := Round( (aItem[4]-aItem[2])/2, 0 ) + 0
           aRect := { { aItem[1],      aItem[2]-n },;
                      { aItem[1],      aItem[4]-2 },;
                      { aItem[1]+ 2,   aItem[4]   },;
                      { aItem[3]-10,   aItem[4]   },;
                      { aItem[3]- 6,   aItem[4]-3 },;
                      { aItem[3]+ d+n, aItem[2]-n } }
   ENDCASE
   DEFAULT nSel TO ::CurSel
   oPage := ::GetItemObject( nSel )
   hRgn := _CreatePolygonRgn( aRect, ALTERNATE )
   IF hRgn != NIL

      IF nSel > 0 .AND. oPage != NIL .AND. ::GetItemCount() >= nSel .AND. oPage:BkBrush != NIL
         hBrush := oPage:BkBrush
      ENDIF

      IF !lCurSel .AND. ( !::ColorInactiveHeader .OR. hBrush == NIL )
         hBrush := ::__GetGradienBrush( hDC, hRgn, aItem, n, d )
         lGrad := .T.
      ENDIF

      DEFAULT hBrush TO GetStockObject( WHITE_BRUSH )

      FillRgn( hDC, hRgn, hBrush )

      IF lGrad
         DeleteObject( hBrush )
      ENDIF

      DeleteObject( hRgn )
   ENDIF

   hOldPen := SelectObject( hDC, CreatePen( PS_SOLID, 0, RGB(128,128,128) ) )
   _PolyLine( hDC, aRect )
   DeleteObject( SelectObject( hDC, hOldPen ) )

   IF nSel > 0 .AND. ::GetItemCount() >= nSel .AND. ::ImageList != NIL .AND. oPage:ImageIndex > 0
      IF !lVert
         ::ImageList:DrawImage( hDC, oPage:ImageIndex, aItem[1]+6, aItem[2]+3, ILD_TRANSPARENT )
         aItem[1]+=::ImageList:IconWidth + 3
       ELSE
         ::ImageList:DrawImage( hDC, oPage:ImageIndex, aItem[1]+3, aItem[2]+4, ILD_TRANSPARENT )
         aItem[2]+=::ImageList:IconWidth + 1
      ENDIF
   ENDIF

   tci := (struct TCITEM)
   tci:mask := TCIF_TEXT
   tci:pszText := SPACE(255)
   tci:cchTextMax := 255

   SendMessage( ::hWnd, TCM_GETITEM, nSel-1, @tci )

   IF !EMPTY( tci:pszText )

      IF oPage != NIL
         SetTextColor( hDC, oPage:ForeColor )
      ENDIF
      IF lGrad
         SetTextColor( hDC, RGB(0,0,0) )
      ENDIF

      SetBkMode( hDC, TRANSPARENT )

      IF !lVert
         hOldFont := SelectObject( hDC, ::Font:Handle )
         _DrawText( hDC, tci:pszText, aItem, (DT_CENTER|DT_VCENTER|DT_SINGLELINE) )
       ELSE
         hOldFont := SelectObject( hDC, ::VertFont:Handle )

         x := ( (aItem[1]+aItem[3])/2 ) - ( _GetTextExtentPoint32( hDC,"X")[2]/2 )
         y := (aItem[2]+aItem[4])/2

         SetTextAlign( hDC, TA_CENTER )

         _ExtTextOut( hDC, x, y, ETO_CLIPPED, aItem, STRTRAN( tci:pszText, "&" ) )

      ENDIF


      SelectObject( hDC, hOldFont )
   ENDIF

RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __GetGradienBrush( hDC, hRgn, aItem, n, d ) CLASS TabStrip
   LOCAL nColor1, nColor2, x
   LOCAL hBrush, hOldBitmap, hMemBitmap, hMemDC
   LOCAL __aMesh, aPt := { {=>}, {=>} }
   (hRgn)
   nColor1 := ::ColorScheme:ToolStripGradientBegin
   nColor2 := ::ColorScheme:ToolStripGradientEnd

   hMemDC  := CreateCompatibleDC( hDC )

   __aMesh      := { {=>} }
   __aMesh[1]:UpperLeft  := 0
   __aMesh[1]:LowerRight := 1
   aPt[1]:Red   := GetRValue( nColor1 ) * 256
   aPt[1]:Green := GetGValue( nColor1 ) * 256
   aPt[1]:Blue  := GetBValue( nColor1 ) * 256
   aPt[1]:Alpha := 0
   aPt[1]:x     := aItem[1]
   aPt[1]:y     := aItem[2]

   aPt[2]:Red   := GetRValue( nColor2 ) * 256
   aPt[2]:Green := GetGValue( nColor2 ) * 256
   aPt[2]:Blue  := GetBValue( nColor2 ) * 256
   aPt[2]:Alpha := 0


   IF ::xTabPosition == 2 .OR. ::xTabPosition == 4
      aPt[2]:x     := aItem[3]+d+n
      aPt[2]:y     := aItem[4]+n
      x := 1
    ELSE
      aPt[2]:x     := aItem[3]+n
      aPt[2]:y     := aItem[4]+d+n
      x := 0
   ENDIF

   hMemBitmap := CreateCompatibleBitmap( hDC, aPt[2]:x, aPt[2]:y )

   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   __GradientFill( hMemDC, aPt, 2, __aMesh, 1, x )

   hBrush := CreatePatternBrush( hMemBitmap )

   SelectObject( hMemDC, hOldBitmap )
   DeleteDC( hMemDC )
   DeleteObject( hMemBitmap )
RETURN hBrush

