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

//-----------------------------------------------------------------------------------------------

CLASS TabControl INHERIT Control

   PROPERTY MultiLine           INDEX TCS_MULTILINE      READ xMultiline           WRITE SetPos         DEFAULT .F. PROTECTED
   PROPERTY ImageList                                    READ xImageList           WRITE SetImageList               
   PROPERTY FocusNever          INDEX TCS_FOCUSNEVER     READ xFocusNever          WRITE SetStyle       DEFAULT .F. PROTECTED
   PROPERTY FixedWidth          INDEX TCS_FIXEDWIDTH     READ xFixedWidth          WRITE SetStyle       DEFAULT .F. PROTECTED
   PROPERTY ColorInactiveHeader                          READ xColorInactiveHeader WRITE InvalidateRect DEFAULT .F. PROTECTED

   ACCESS ImageList        INLINE __ChkComponent( Self, ::xImageList )     PERSISTENT

   DATA BoldSelection    PUBLISHED INIT .F.
   DATA ShowTabs         PUBLISHED INIT .T.
   DATA Flat             PUBLISHED INIT .F.
   
   DATA CurSel           EXPORTED  INIT 1
   DATA VertFont         EXPORTED
   DATA BoldFont         EXPORTED
   DATA VertFocusFont    EXPORTED
   DATA FocusFont        EXPORTED
   DATA Frame            EXPORTED    INIT .T.

   DATA OnSelChanged     EXPORTED
   DATA TabBrush         EXPORTED
   DATA __Tabs           EXPORTED

   DATA CloseBtn         PROTECTED
   DATA PinBtn           PROTECTED
   DATA __nHot           PROTECTED INIT 0
   DATA __nPrevHot       PROTECTED INIT 0
   
   DATA __pUpDnPtr       PROTECTED
   DATA __lMouseDown     PROTECTED INIT .F.
   DATA __ColorTable     PROTECTED 

   DATA __aDel           PROTECTED INIT {}
   DATA __UpDn           PROTECTED

   PROPERTY TabPosition  READ xTabPosition WRITE __SetTabPosition DEFAULT 2 INVERT
   DATA EnumTabPosition  EXPORTED INIT { { "Left", "Top", "Right", "Bottom" }, {1,2,3,4} }

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
   METHOD SetMinTabWidth( dx )       INLINE TabCtrl_SetMinTabWidth( ::hWnd, dx )
   METHOD GetCurFocus()              INLINE TabCtrl_GetCurFocus(::hWnd )+1
   METHOD GetCurSel()                INLINE ::CurSel := TabCtrl_GetCurSel(::hWnd)+1, ::CurSel
   METHOD GetExtendedStyle()         INLINE TabCtrl_GetExtendedStyle(::hWnd)
   METHOD GetItem(nItem,ptrItem)     INLINE TabCtrl_GetItem(::hWnd,nItem-1,@ptrItem)
   METHOD GetItemText()
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
   METHOD GetObj()

   METHOD OnTabChanging()            VIRTUAL
   METHOD OnTabChanged()             VIRTUAL
   METHOD OnTabInsert()              VIRTUAL

   METHOD OnParentNotify()
   METHOD OnEraseBkGnd()

   METHOD SetPos(n,l)                INLINE ::SetStyle(n,l),; 
                                            IIF( ::IsWindow(),;
                                               ( SetWindowPos( ::hWnd,, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER ),;
                                            ::__OnParentSize( ::ClientWidth, ::ClientHeight ) ),)

   METHOD Refresh()                  INLINE ::Super:Refresh(), ::OnNcCalcSize()
   METHOD OnNCDestroy()              INLINE IIF( ::FocusFont     != NIL, ::FocusFont:Delete(),     NIL ),;
                                            IIF( ::VertFont      != NIL, ::VertFont:Delete(),      NIL ),;
                                            IIF( ::BoldFont      != NIL, ::BoldFont:Delete(),      NIL ),;
                                            IIF( ::VertFocusFont != NIL, ::VertFocusFont:Delete(), NIL ),;
                                            NIL
   METHOD OnWindowPaint()
   METHOD OnMove(x,y)                INLINE IIF( ::Cursel > 0 .AND. LEN( ::Children ) >= ::Cursel .AND. VALTYPE(::Children[::Cursel])=="O", ::Children[::Cursel]:__OnParentSize(x,y), ), NIL

   METHOD OnHScroll(x,y,n)           INLINE ::Super:OnHScroll(x,y,n)
   METHOD UnDock()
   METHOD ReDock()
   METHOD Hide()
   METHOD Show()
   
   METHOD OnDestroy()                INLINE ::CloseThemeData(), NIL
   METHOD OnMouseMove()
   METHOD OnMouseLeave()             INLINE ::OnMouseMove(0,0,0), NIL
   METHOD OnUserMsg(n)
   
   METHOD RestoreTabPos()
   
   METHOD OnLButtonDown()
   
   METHOD __OnParentSize()
   METHOD __SetTheming()
   METHOD __SetTabPosition()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS TabControl

   ::ClsName      := WC_TABCONTROL
   DEFAULT ::__xCtrlName TO "TabControl"
   ::Style        := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TCS_OWNERDRAWFIXED
   ::Super:Init( oParent )
   ::Width        := 200
   ::Height       := 200
   ::IsContainer  := .T.
   ::ThemeName    := "TAB"
   ::OpenThemeData()
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
                            { "OnParentSize"       , "", "" },;
                            { "OnSize"             , "", "" },;
                            { "OnSizing"           , "", "" },;
                            { "OnWindowPaint"      , "", "" },;
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
   ::Enabled := ::ShowTabs

   IF ( !::Theming .OR. !IsThemeActive() ) .AND. ::ShowTabs
      ::Style := ::Style & NOT( TCS_OWNERDRAWFIXED )
   ENDIF
   IF ::VertFont == NIL
      ::VertFont := Font()
      ::VertFont:Escapement := 900
      ::VertFont:Create()
   ENDIF
   ::Super:Create()

   IF !::ShowTabs
      ::SendMessage( TCM_SETITEMSIZE, 0, MAKELPARAM(0,0) )
   ENDIF
   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "Add TabPage", {|o| o:=::Application:Project:AddControl( "TabPage", Self ) } }}
   ENDIF

   IF ::ImageList != NIL
      ::SetImageList( ::ImageList )
   ENDIF

   IF ::BoldSelection
      IF ::FocusFont == NIL
         ::FocusFont := Font()
         ::FocusFont:Bold := .T.
         ::FocusFont:Create()
         ::VertFocusFont := Font()
         ::VertFocusFont:Bold := .T.
         ::VertFocusFont:Escapement := 900
         ::VertFocusFont:Create()
      ENDIF
   ENDIF

   ::BoldFont := Font()
   ::BoldFont:Bold := .T.
   ::BoldFont:Create()

   IF ::Flat
      IF ::AllowUndock
         ::PinBtn   := TabPinButton( Self )
      ENDIF
      IF ::AllowClose
         ::CloseBtn := TabCloseButton( Self )
      ENDIF
   ENDIF

   IF ::__ClassInst != NIL
      ::SetWindowPos(, ::Left, ::Top, ::Width, ::Height, SWP_FRAMECHANGED | SWP_NOZORDER )
   ENDIF

RETURN Self

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


//-----------------------------------------------------------------------------------------------
METHOD RestoreTabPos() CLASS TabControl
   LOCAL tci, n, aChildren := ARRAY( LEN( ::Children ) )
   
   FOR n := 1 TO LEN( ::Children )
      aChildren[ ::Children[n]:Position ] := ::Children[n]
   NEXT

   tci := (struct TCITEM)
   tci:mask := TCIF_IMAGE | TCIF_TEXT
   
   ::Children := aChildren

   FOR n := 1 TO LEN( ::Children )
      ::Children[n]:Index := n

      SendMessage( ::hWnd, TCM_DELETEITEM, n-1 )
      tci:pszText := ::Children[n]:Caption
      tci:iImage  := IIF( ::Children[n]:ImageIndex > 0, ::Children[n]:ImageIndex-1, -1 )
      SendMessage( ::hWnd, TCM_INSERTITEM, n-1, tci )

   NEXT   
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetTheming( lSet ) CLASS TabControl
   AEVAL( ::Children, {|o| o:Theming := lSet, o:__OnParentSize( ::ClientWidth, ::ClientHeight ) } )
RETURN Self

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
         ::Children[n] := lBold
         tci:mask := TCIF_TEXT
         tci:pszText := cText
         tci:cchTextMax := LEN( cText )
         SendMessage( ::hWnd, TCM_SETITEM, n-1, @tci )
         SetWindowPos( ::hWnd,, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
         ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
      ENDIF
    catch
   END
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Hide() CLASS TabControl
   LOCAL Tab
   FOR EACH Tab IN ::Children
       Tab:HideTab()
   NEXT
   Super:Hide()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Show(n) CLASS TabControl
   Super:Show(n)
   IF LEN( ::Children ) >= ::Cursel
      TRY
         ::Children[::Cursel]:ShowTab()
       CATCH
      END
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnEraseBkGnd( hDC ) CLASS TabControl
   LOCAL hBrush
   IF !::Flat
      IF ( ::Theming .AND. IsThemeActive() ) .OR. !::ShowTabs
         hBrush := ::bkBrush
         DEFAULT hBrush TO ::Parent:bkBrush
         IF hBrush != NIL
            _FillRect( hDC, _GetClientRect( ::hWnd ), hBrush )
            RETURN .T.
         ENDIF
      ENDIF
    ELSEIF ::Flat
      RETURN .T.
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD HitTest( x, y, nFlags ) CLASS TabControl
   LOCAL tch := (struct TCHITTESTINFO)
   DEFAULT nFlags TO TCHT_NOWHERE | TCHT_ONITEM | TCHT_ONITEMICON | TCHT_ONITEMLABEL
   tch:pt:x  := x
   tch:pt:y  := y
   tch:flags := nFlags
RETURN SendMessage( ::hWnd, TCM_HITTEST, 0, tch )+1

//-----------------------------------------------------------------------------------------------
METHOD __OnParentSize( x, y, hDef ) CLASS TabControl
   ::Super:__OnParentSize( x, y, hDef, .T., .F. )
RETURN hDef

//-----------------------------------------------------------------------------------------------
METHOD OnMouseMove(nwParam,x,y) CLASS TabControl
   LOCAL aItem
   static nHot := 0
   (nwParam)
   
   IF ( ::__nHot := ::HitTest( x, y ) ) > 0
      IF ::__lMouseDown .AND. ::CurSel <> ::__nHot
         ::Children[ ::CurSel ]:Position := ::__nHot
         ::CurSel := ::__nHot
         RETURN 0
      ENDIF
      IF nHot <> ::__nHot
         IF nHot > 0
            aItem := ::GetItemRect( nHot )
            ::InvalidateRect( aItem, .F. )
         ENDIF
         aItem := ::GetItemRect( ::__nHot )
         ::InvalidateRect( aItem, .F. )
         nHot := ::__nHot
      ENDIF
    ELSEIF nHot > 0
      aItem := ::GetItemRect( nHot )
      ::InvalidateRect( aItem, .F. )
      nHot := 0
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD __SetTabPosition(n) CLASS TabControl
   LOCAL cPos := ::EnumTabPosition[1][n]
   IF n <> ::xTabPosition
      ::Align&cPos()
      ::PostMessage( WM_USER + 1111 )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnWindowPaint() CLASS TabControl
   LOCAL hDC, cPaint, aRect, n, nSel, hBrush, lTop, nItems
   LOCAL lVert, x, y, nTextHeight, nTop, nState, nPart
   LOCAL nLeft, nRight, nBottom, aCover, aItem
   LOCAL aBackRect, aFramRect, hWnd
   LOCAL rc, tci//, oChild, hMemDC1, hOldBitmap1
   LOCAL hMemDC, hMemBitmap, hOldBitmap, hOldFont, aText, sz := (struct SIZE)
   IF !::Flat
      IF ( !::Theming .OR. !IsThemeActive() ) .AND. ::ShowTabs
         RETURN NIL
      ENDIF
   ENDIF

   IF !::ShowTabs .AND. ::__UpDn == NIL
      hWnd := GetWindow( ::hWnd, GW_CHILD | GW_HWNDFIRST )
      WHILE hWnd <> 0
         IF GetClassName( hWnd ) == "msctls_updown32" 
            ::__UpDn := hWnd
            DestroyWindow( hWnd )
            EXIT
         ENDIF
         hWnd := GetWindow( hWnd, GW_HWNDNEXT )
      ENDDO
   ENDIF
   

   rc  := (struct RECT)
   tci := (struct TCITEM)

   aRect := _GetClientRect( ::hWnd )

   rc:Left   := aRect[1]
   rc:Top    := aRect[2]
   rc:Right  := aRect[3]
   rc:Bottom := aRect[4]

   nSel   := TabCtrl_GetCurSel( ::hWnd )+1

   SendMessage( ::hWnd, TCM_ADJUSTRECT, 0, @rc )
   hDC   := _BeginPaint( ::hWnd, @cPaint )


   IF !::Flat
      aItem := ::GetItemRect( 1 )
      IF ::Style & TCS_BOTTOM == 0
         aRect[2] := aItem[4]
       ELSE
         aRect[2] += 2
         aRect[4] -= ( aRect[4]-aItem[2]-2 )
      ENDIF
      
      IF IsThemeActive()
         DrawThemeBackground( ::hTheme, hDC, TABP_PANE, , aRect  )
       ELSE
         _DrawFrameControl( hDC, aRect, DFC_BUTTON, DFCS_BUTTONPUSH )
      ENDIF
      
      IF LEN( ::Children ) == 0 .OR. !::ShowTabs
         _EndPaint( ::hWnd, cPaint)
         RETURN 0
      ENDIF
      IF ( nItems := ::GetItemCount() ) > 0
         FOR n := nItems TO 1 STEP -1

             IF n != nSel

                tci:mask := TCIF_TEXT //+ TCIF_IMAGE + TCIF_STATE + TCIF_PARAM
                tci:pszText := SPACE(255)
                tci:cchTextMax := 255

                SendMessage( ::hWnd, TCM_GETITEM, n-1, @tci )

                aItem  := ::GetItemRect( n )

                IF LEN( ::Children ) >= n .AND. VALTYPE( ::Children[n] ) == "O"
                   SetTextColor( hDC, ::Children[n]:ForeColor )
                ENDIF
                nState := TIS_NORMAL
                aItem[2]+=1
                IF ::__nHot == n
                   nState := TIS_HOT
                ENDIF
                DrawThemeBackground( ::hTheme, hDC, TABP_TABITEM, nState, aItem )
                IF ::ImageList != NIL .AND. LEN( ::Children ) >= n .AND. ::Children[n]:ImageIndex > 0
                   ::ImageList:DrawImage( hDC, ::Children[n]:ImageIndex, aItem[1]+6, aItem[2]+3, ILD_TRANSPARENT )
                   aItem[1]+=::ImageList:IconWidth + 6
                ENDIF
                IF !EMPTY( tci:pszText )
                   SetBkMode( hDC, TRANSPARENT )
                   hOldFont := SelectObject( hDC, ::Font:Handle )

                   //aSize    := _GetTextExtentPoint32( hDC, tci:pszText  )
                   _DrawText( hDC, tci:pszText, aItem, DT_CENTER|DT_VCENTER|DT_SINGLELINE  )
                   SelectObject( hDC, hOldFont )
                ENDIF
             ENDIF
         NEXT

         aItem := ::GetItemRect( nSel )

         aItem[1]-= 2
         aItem[3]+= 2
         aItem[2]-=1
         aItem[4]+=2
         nPart  := TABP_TABITEM
         nState := TIS_SELECTED
         IF nSel == 1
            nPart  := TABP_TOPTABITEMLEFTEDGE
          ELSEIF aItem[3] == aRect[3]
            nPart  := TABP_TOPTABITEMRIGHTEDGE
         ENDIF

         DrawThemeBackground( ::hTheme, hDC, nPart, nState, aItem, aItem )
         aItem[4]-=2
         n := nSel

         IF ::ImageList != NIL .AND. LEN( ::Children ) >= n .AND. ::Children[n]:ImageIndex > 0
            ::ImageList:DrawImage( hDC, ::Children[n]:ImageIndex, aItem[1]+6, aItem[2]+3, ILD_TRANSPARENT )
            aItem[1]+=::ImageList:IconWidth + 3
         ENDIF

         tci:mask := TCIF_TEXT //+ TCIF_IMAGE + TCIF_STATE + TCIF_PARAM
         tci:pszText := SPACE(255)
         tci:cchTextMax := 255
         SendMessage( ::hWnd, TCM_GETITEM, n-1, @tci )

         IF LEN( ::Children ) >= n .AND. !EMPTY( tci:pszText )
            IF VALTYPE( ::Children[n] ) == "O"
               SetTextColor( hDC, ::Children[n]:ForeColor )
            ENDIF
            SetBkMode( hDC, TRANSPARENT )
            hOldFont := SelectObject( hDC, ::Font:Handle )

            //aSize    := _GetTextExtentPoint32( hDC, tci:pszText )
            //aItem[2] := aItem[2]+ ((aItem[4]-aSize[2])/2)
            _DrawText( hDC, tci:pszText, aItem, DT_CENTER|DT_VCENTER|DT_SINGLELINE  )
            SelectObject( hDC, hOldFont )
         ENDIF
         IF !::FocusNever .AND. ::HasFocus
            aRect := ::GetItemRect( nSel )
            _DrawFocusRect( hDC, { aRect[1],aRect[2]+2,aRect[3],aRect[4]} )
         ENDIF

      ENDIF
/*
      FOR EACH oChild IN ::Children[ ::CurSel ]:Children
          IF oChild:__hBrush != NIL
             DeleteObject( oChild:__hBrush )
          ENDIF
          DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )
          hMemDC1      := CreateCompatibleDC( hDC )
          hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )
          BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin+oChild:CaptionHeight, SRCCOPY )
          oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )
          SelectObject( hMemDC1,  hOldBitmap1 )
          DeleteDC( hMemDC1 )
      NEXT
*/
      _EndPaint( ::hWnd, cPaint)
      RETURN 0
   ENDIF 

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)


   lTop  := ::Style & TCS_BOTTOM == 0
   lVert := ::Style & TCS_VERTICAL == TCS_VERTICAL
   aText := _GetTextExtentPoint32( hMemDC,"X")
   IF aText == NIL .AND. GetTextExtentPoint32( hMemDC, "X", @sz )
      aText := sz:Array
   ENDIF
   IF aText == NIL
      aText := {0,0}
   ENDIF

   IF aText != NIL
      nTextHeight := aText[2]
    ELSE
      nTextHeight := 10
   ENDIF
   IF ::AllowUndock
      ::PinBtn:MoveWindow( ::Width-20-IIF(::AllowClose,18,0), (rc:Top-16)/2, 17, 16, .T. )
   ENDIF
   IF ::AllowClose
      ::CloseBtn:MoveWindow( ::Width-20, (rc:Top-16)/2, 17, 16, .T. )
   ENDIF

   nLeft   := 0
   nTop    := 0
   nRight  := 0
   nBottom := 0

   IF lTop
      IF !lVert
         aCover    := { 0, 0, 0, 2 }
         nBottom   :=  IIF( ::Frame, 2, 1 )
         aBackRect := { aRect[1], aRect[2], aRect[3], rc:Top }
         aFramRect := { aRect[1], rc:Top-1, aRect[3], aRect[4] }
       ELSE
         aCover    := { 0, 0, 1, 0 }
         nRight    :=  3
         nBottom   :=  1
         aBackRect := { aRect[1], aRect[2], rc:Left, aRect[4] }
         aFramRect := { rc:Left-1, aRect[2], aRect[3], aRect[4] }
      ENDIF
    ELSE
      IF !lVert
         aCover    := { 0, IIF( ::Frame,-2,-3 ), 0, 0 }
         nTop      := IIF( ::Frame, -1, -1)
         aBackRect := { aRect[1], rc:Bottom, aRect[3], aRect[4] }
         aFramRect := { aRect[1], aRect[2], aRect[3], rc:Bottom+1 }
      ELSE
         aCover    := { -1, 0, 0, 0 }
         nLeft     := -3
         nBottom   :=  1
         aBackRect := { rc:Right, aRect[2], aRect[3], aRect[4] }
         aFramRect := { aRect[1], aRect[2], aBackRect[1]+1, aRect[4] }
      ENDIF
   ENDIF

   IF ::ShowTabs
      _FillRect( hMemDC, aFramRect, IIF( ::BkBrush != NIL, ::BkBrush, GetSysColorBrush( COLOR_BTNFACE ) ) )
      _FillRect( hMemDC, aBackRect, IIF( ::BkBrush != NIL, ::BkBrush, GetSysColorbrush( COLOR_3DHIGHLIGHT ) ) )

      __Draw3dRect( hMemDC, aBackRect, GetSysColor(COLOR_3DDKSHADOW), GetSysColor( COLOR_3DHIGHLIGHT ) )

      IF ::Frame .AND. LEN( ::Children ) > 0 .AND. aFramRect[4]>aFramRect[2]
         _FillRect( hMemDC, aFramRect, IIF( ::BkBrush != NIL, ::BkBrush, GetSysColorBrush( COLOR_BTNFACE ) ) )
         __Draw3dRect( hMemDC, aFramRect, GetSysColor( COLOR_3DHIGHLIGHT ), GetSysColor(COLOR_3DDKSHADOW) )
      ENDIF
    ELSE
      hBrush := ::bkBrush
      DEFAULT hBrush TO ::Parent:bkBrush
      _FillRect( hMemDC, aBackRect, IIF( hBrush != NIL, hBrush, GetSysColorbrush( COLOR_BTNFACE ) ) )
     __Draw3dRect( hMemDC, aFramRect, GetSysColor( COLOR_3DHIGHLIGHT ), GetSysColor(COLOR_3DDKSHADOW) )
   ENDIF

   nItems := ::GetItemCount()

   IF !::ShowTabs
      nItems := 0
   ENDIF
   FOR n := 1 TO nItems
       IF VALTYPE( ::Children[n] ) == "L" .AND. ::Children[n]
          SelectObject( hMemDC, ::BoldFont:Handle )
        ELSE
          SelectObject( hMemDC, IIF( lVert, ::VertFont:Handle, IIF( VALTYPE( ::Children[n] ) == "O", ::Children[n]:Font:Handle, ::Font:Handle ) ) )
       ENDIF
       aRect := ::GetItemRect( n )

       aRect[1] += nLeft
       aRect[2] += nTop
       aRect[3] += nRight+1
       aRect[4] += nBottom

       IF LEN( ::Children ) >= n .AND. VALTYPE( ::Children[n] ) == "O"
          hBrush := ::Children[n]:BkBrush
       ENDIF

       DEFAULT hBrush TO ::TabBrush
       DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )
       SetTextColor( hMemDC, GetSysColor( COLOR_BTNTEXT ) )

       IF n == nSel .OR. ( ::ColorInactiveHeader .AND. !(hBrush == GetSysColorBrush( COLOR_BTNFACE )) )
          IF n == nSel
             __Draw3dRect( hMemDC, aRect, GetSysColor( COLOR_3DHIGHLIGHT ), GetSysColor(COLOR_3DDKSHADOW) )
          ENDIF
          aRect[1] += aCover[1]
          aRect[2] += aCover[2]
          aRect[3] += aCover[3]
          aRect[4] += aCover[4]

          IF !(n == nSel) .AND. ::ColorInactiveHeader
             IF lVert
                IF lTop
                   aRect[3]-=3
                 ELSE
                   aRect[1]+=2
                ENDIF
              ELSE
                IF lTop
                   aRect[4]-=2
                 ELSE
                   aRect[2]+=1
                ENDIF
             ENDIF
          ENDIF
          IF ::Frame
             _FillRect( hMemDC, aFramRect, hBrush )
             __Draw3dRect( hMemDC, aFramRect, GetSysColor( COLOR_3DHIGHLIGHT ), GetSysColor(COLOR_3DDKSHADOW) )
          ENDIF
          _FillRect( hMemDC, { aRect[1]+1,aRect[2]+1,aRect[3]-1,aRect[4]-1}, hBrush )

          IF n == nSel .AND. ::HasFocus
             IF lVert .AND. lTop
                aRect[3]--
             ENDIF
             _DrawFocusRect( hMemDC, { aRect[1]+2,aRect[2]+2,aRect[3]-2,aRect[4]-2} )
          ENDIF

       ENDIF
       SetBkMode( hMemDC, TRANSPARENT )
       aRect := ::GetItemRect( n )

       tci:mask := TCIF_TEXT + TCIF_IMAGE + TCIF_STATE + TCIF_PARAM
       tci:pszText := SPACE(255)
       tci:cchTextMax := 255

       SendMessage( ::hWnd, TCM_GETITEM, n-1, @tci )

       IF !EMPTY( aRect ) .AND. ( !EMPTY( tci:pszText ) .OR. tci:iImage >= 0 )
          IF !lVert
             IF tci:iImage >= 0
                IF ::ImageList != NIL
                   y := aRect[2] + ( ( (aRect[4]-aRect[2]) - ::ImageList:IconHeight ) / 2 )
                   ::ImageList:DrawImage( hMemDC, tci:iImage+1, aRect[1]+6, y+3, ILD_TRANSPARENT )
                   aRect[1]+=::ImageList:IconWidth + 2
                ENDIF
             ENDIF
             IF LEN( ::Children ) >= n .AND. VALTYPE( ::Children[n] ) == "O"
                SetTextColor( hMemDC, ::Children[n]:ForeColor )
             ENDIF
             IF n==nSel
                IF ::BoldSelection
                   IF ::FocusFont == NIL
                      ::FocusFont := Font()
                      ::FocusFont:Bold := .T.
                      ::FocusFont:Create()
                      IF ::VertFocusFont == NIL
                         ::VertFocusFont := Font()
                         ::VertFocusFont:Bold := .T.
                         ::VertFocusFont:Escapement := 900
                         ::VertFocusFont:Create()
                      ENDIF
                   ENDIF
                   SelectObject( hMemDC, ::FocusFont:Handle )
                ENDIF
             ENDIF
             IF !EMPTY( tci:pszText )
                _DrawText( hMemDC, tci:pszText, aRect, DT_CENTER + DT_VCENTER + DT_SINGLELINE  )
             ENDIF
             IF n==nSel .AND. ::BoldSelection
                SelectObject( hMemDC, IIF( VALTYPE( ::Children[n] ) == "O", ::Children[n]:Font:Handle, ::Font:Handle )  )
             ENDIF
             IF n != nSel .AND. !::ColorInactiveHeader
                SetTextColor( hMemDC, GetSysColor( COLOR_BTNSHADOW ) )
                MoveTo( hMemDC, aRect[3], aRect[2]+3 )
                LineTo( hMemDC, aRect[3], aRect[4]-3 )
                SetTextColor( hMemDC, GetSysColor( COLOR_BTNTEXT ) )
             ENDIF
           ELSE
             x := ( (aRect[1]+aRect[3])/2 ) - ( nTextHeight/2 )
             y := (aRect[2]+aRect[4])/2

             SetTextAlign( hMemDC, TA_CENTER )
             IF n==nSel
                IF LEN( ::Children ) >= n .AND. VALTYPE( ::Children[n] ) == "O"
                   SetTextColor( hMemDC, ::Children[n]:ForeColor )
                ENDIF
                IF ::BoldSelection
                   SelectObject( hMemDC, ::FocusFont:Handle )
                ENDIF
             ENDIF
             _ExtTextOut( hMemDC, x, y, ETO_CLIPPED, aRect, STRTRAN( tci:pszText, "&" ) )

             IF n != nSel .AND. !::ColorInactiveHeader
                SetTextColor( hMemDC, GetSysColor( COLOR_BTNSHADOW ) )
                MoveTo( hMemDC, aRect[1]+3, aRect[4] )
                LineTo( hMemDC, aRect[3]-3, aRect[4] )
                SetTextColor( hMemDC, GetSysColor( COLOR_BTNTEXT ) )
             ENDIF

          ENDIF
       ENDIF
   NEXT
/*
   IF hMemBitmap != NIL
      FOR EACH oChild IN ::Children
          IF oChild:__hBrush != NIL
             DeleteObject( oChild:__hBrush )
          ENDIF
          DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )
          hMemDC1      := CreateCompatibleDC( hDC )
          hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )
          BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin+oChild:CaptionHeight, SRCCOPY )
          oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )
          SelectObject( hMemDC1,  hOldBitmap1 )
          DeleteDC( hMemDC1 )
      NEXT
   ENDIF
*/
   BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )

   _EndPaint( ::hWnd, cPaint)
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD UnDock() CLASS TabControl
   LOCAL Tab, oParent := ::Parent

   IF __Evaluate( ::OnWMUnDock, Self ) == NIL
      ::Super:Undock()
      IF ::CloseBtn != NIL
         ::CloseBtn:Hide()
      ENDIF
      IF ::PinBtn != NIL
         ::PinBtn:Hide()
      ENDIF

      FOR EACH Tab IN ::Children
          SetParent( Tab:hWnd, ::hWnd )
      NEXT
      ::Children[::CurSel]:ShowTab()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD ReDock() CLASS TabControl
   LOCAL Tab, oParent := ::Parent

   IF __Evaluate( ::OnWMReDock, Self ) == NIL
      ::Super:Redock()
      IF ::CloseBtn != NIL
         ::CloseBtn:Show()
      ENDIF
      IF ::PinBtn != NIL
         ::PinBtn:Show()
      ENDIF

      FOR EACH Tab IN ::Children
          SetParent( Tab:hWnd, ::Parent:hWnd )
      NEXT
      ::Children[::CurSel]:ShowTab()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetItemValues( nItem, cText, nImgIndex ) CLASS TabControl
   LOCAL tci := (struct TCITEM)
   tci:mask    := TCIF_IMAGE | TCIF_TEXT
   tci:pszText := cText
   tci:iImage  := nImgIndex-1
   DEFAULT cText TO ""
   tci:cchTextMax := LEN( cText )
   SendMessage( ::hWnd, TCM_SETITEM,  nItem, tci )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnLButtonDown() CLASS TabControl
   ::CallWindowProc()
   IF !::__lMouseDown
      IF VALTYPE( ::Children[ ::CurSel ] ) == "O" .AND. ::Children[ ::CurSel ]:AllowDrag
         ::__lMouseDown := .T.
         ::Cursor := ::System:Cursor:SizeWE
      ENDIF
   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS TabControl
   LOCAL nSel, lAllow, nOldSel, pPtr
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
           IF ::Cursel != nSel
              IF !Empty(::Children) .AND. VALTYPE( ::Children[nSel] ) == "O"
                 IF !::Children[nSel]:IsWindowEnabled()
                    ::SetCurSel( ::Cursel )
                    RETURN 0
                 ENDIF
                 IF VALTYPE( ::Children[::CurSel] ) == "O"

                    pPtr := GetProp( GetFocus(), "PROP_CLASSOBJECT" )
                    IF pPtr != NIL .AND. pPtr != 0
                       ::Children[::CurSel]:LastFocus := ArrayFromPointer( pPtr )
                       ::Children[::CurSel]:LastFocus:TabValidate := .F.
                     ELSE
                       ::Children[::CurSel]:LastFocus := NIL
                    ENDIF
                    ::Children[::CurSel]:HideTab()
                 ENDIF
              ENDIF

              nOldSel := ::CurSel
              ::CurSel := nSel
              IF !Empty( ::Children) .AND. VALTYPE( ::Children[::CurSel] ) == "O"
                 ::Children[::CurSel]:__OnParentSize( ::ClientWidth, ::ClientHeight )
                 ::Children[::CurSel]:ShowTab()

                 IF ::Children[::CurSel]:Action != NIL
                    Eval( ::Children[::CurSel]:Action, ::Children[::CurSel] )
                 ENDIF

                 IF GetFocus() != ::hWnd

                    IF ::Children[::CurSel]:LastFocus != NIL
                       ::Children[::CurSel]:LastFocus:SetFocus(.T.)
                       IF ::Children[::CurSel]:LastFocus:ClsName == "DataGrid"
                          ::Children[::CurSel]:LastFocus:InvalidateRect(, .F.)
                       ENDIF
                     ELSEIF !EMPTY( ::Children[::CurSel]:Children )
                       ::Children[::CurSel]:Children[1]:SetFocus( .T. )
                    ENDIF

                 ENDIF
                 IF ::Children[nOldSel]:LastFocus != NIL
                    ::Children[nOldSel]:LastFocus:TabValidate := .T.
                 ENDIF
              ENDIF
              ::OnTabChanged( nOldSel, nSel )
              IF VALTYPE( ::OnSelChanged ) == "B"
                 EVAL( ::OnSelChanged, Self, nOldSel, nSel )
              ENDIF
              ExecuteEvent( "OnTabChanged", Self )
              IF ::__xCtrlName == "TabStrip"
                 ::Redraw()
              ENDIF
              RETURN 0
           ENDIF
   ENDCASE
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD InsertTab(cText, nPos, nImg, lBold) CLASS TabControl
   LOCAL tci, lAllow := ::OnTabInsert( nPos)
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
      DEFAULT nPos TO Len( ::Children )
      IF SendMessage( ::hWnd, TCM_INSERTITEM,  nPos, tci ) > -1
         AADD( ::Children, lBold )
         IF lBold != NIL
            SetWindowPos( ::hWnd,, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
            ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
         ENDIF
         RETURN(.T.)
      ENDIF
   ENDIF
RETURN .F.

//-----------------------------------------------------------------------------------------------
METHOD DeleteTab(nPos) CLASS TabControl
   ADEL( ::Children, nPos, .T. )
RETURN TabCtrl_DeleteItem(::hWnd, nPos-1)

//-----------------------------------------------------------------------------------------------
METHOD DeleteAllTabs() CLASS TabControl
   LOCAL lRet := .F.
   lRet := TabCtrl_DeleteAllItems(::hWnd)
   IF ! Empty( ::Children)
      AEVAL( ::Children, {|o| IF( valtype(o)=="O" .and. o:isWindow(), o:DestroyWindow(), ) } )
      ::Children :={}
   ENDIF
   ::CurSel   :=0
RETURN lRet

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
METHOD GetObj( cName ) CLASS TabControl
   LOCAL n
   IF ! Empty(::Children)
      IF( n := ASCAN( ::Children, {|o|o:ClsName == cName } ) ) > 0
         RETURN ::Children[n]
      ENDIF
   ENDIF
RETURN NIL

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
   PROPERTY ImageIndex READ xImageIndex WRITE __SetImageIndex DEFAULT 0 PROTECTED
   PROPERTY Position   READ xPosition   WRITE __SetPosition   INVERT
   PROPERTY Visible    READ xVisible    WRITE __SetVisible    DEFAULT .T. INVERT 
   DATA TopMargin          PUBLISHED INIT 0
   DATA LeftMargin         PUBLISHED INIT 0
   DATA RightMargin        PUBLISHED INIT 0
   DATA BottomMargin       PUBLISHED INIT 0
   DATA AllowDrag          PUBLISHED INIT .F.

   DATA Index              EXPORTED
   DATA ColorMode          EXPORTED INIT 0
   DATA ItemSize           EXPORTED
   DATA LastFocus          EXPORTED
   DATA __xCtrlName        EXPORTED
   DATA Action             EXPORTED

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
   DATA DisableParent      EXPORTED INIT .F.
   DATA ShowMode           EXPORTED INIT 1
   DATA Center             EXPORTED INIT .F.
   DATA Icon               EXPORTED

   DATA xCaption           PROTECTED
   ACCESS Caption          INLINE ::xCaption PERSISTENT
   ASSIGN Caption( c )     INLINE ::SetCaption(c)

   ACCESS ControlParent    INLINE ::ExStyle & WS_EX_CONTROLPARENT != 0
   ACCESS MdiContainer     INLINE ::xMdiContainer

   ACCESS SysMenu          INLINE ::Style & WS_SYSMENU != 0
   ACCESS ThickFrame       INLINE ::Style & WS_THICKFRAME != 0
   ACCESS MinimizeBox      INLINE ::Style & WS_MINIMIZEBOX != 0
   ACCESS MaximizeBox      INLINE ::Style & WS_MAXIMIZEBOX != 0
   ACCESS Border           INLINE ::Style & WS_BORDER != 0
   ACCESS TabStop          INLINE ::Style & WS_TABSTOP != 0
   ACCESS ClipSiblings     INLINE ::Style & WS_CLIPSIBLINGS != 0
   ACCESS CaptionBar       INLINE ::Style & WS_CAPTION != 0
   ACCESS StaticEdge       INLINE ::ExStyle & WS_EX_STATICEDGE != 0
   ACCESS ClientEdge       INLINE ::ExStyle & WS_EX_CLIENTEDGE != 0
   ACCESS ClipChildren     INLINE ::Style & WS_CLIPCHILDREN != 0
   ACCESS Transparent      INLINE ::ExStyle & WS_EX_TRANSPARENT != 0
   ACCESS FrameStyle       INLINE ::xFrameStyle
   ACCESS DlgModalFrame    INLINE ::xDlgModalFrame
   ACCESS Icon             INLINE ::xIcon
   ACCESS Opacity          INLINE ::xOpacity
   ACCESS TabOrder         INLINE ::xTabOrder
   ACCESS TopMost          INLINE ::xTopMost

   ACCESS Form             INLINE ::Parent:Form
   
   DATA __aRect            PROTECTED
   
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnGetDlgCode()   INLINE DLGC_WANTMESSAGE
   METHOD Destroy()        INLINE ::Delete()
   METHOD HideTab()        INLINE Super:Hide()
   METHOD ShowTab()        INLINE Super:Show()
   METHOD OnHotKey()       INLINE ::Select(), NIL
   METHOD OnCtlColorDlg()
   METHOD Delete()
   METHOD Select()
   METHOD __SetImageIndex()
   METHOD SetCaption()
   METHOD __Hide()
   METHOD __Show()
   METHOD __SetPosition()
   METHOD __OnParentSize()
   METHOD __SetVisible()
   //METHOD OnPaint()
   METHOD __ResetHandle()  INLINE ::hWnd := NIL, ::__nProc := NIL, ::Template := NIL
ENDCLASS
/*
//-----------------------------------------------------------------------------------------------
METHOD OnPaint( hDC, hMemDC ) CLASS TabPage
   LOCAL oChild, hOldBitmap1, hMemDC1
   (hMemDC)
   IF hDC != NIL
      FOR EACH oChild IN ::Children
          TRY
             IF oChild:__hBrush != NIL
                DeleteObject( oChild:__hBrush )
             ENDIF
             hMemDC1 := CreateCompatibleDC( hDC )
             DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )
             hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )
             BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin+oChild:CaptionHeight, SRCCOPY )
             oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )
             SelectObject( hMemDC1,  hOldBitmap1 )
             DeleteDC( hMemDC1 )
          CATCH
          END
      NEXT
   ENDIF
RETURN 0
*/
//-----------------------------------------------------------------------------------------------
METHOD Init( oParent, acResource ) CLASS TabPage
   DEFAULT ::__xCtrlName TO "TabPage"
   
   ::Super:Init( oParent, acResource )
   ::Property      := NIL
   ::Style         := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ClsName       := "TabPage"
   ::ThemeName     := "TAB"
   ::ControlParent := .T.
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

   IF ::__ClassInst != NIL
      ::Events := ;
             { ;
               {"Command",     {;
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
                               { "OnParentSize"       , "", "" },;
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
METHOD Create() CLASS TabPage
   LOCAL tci, rc, n

   IF EMPTY( ::Caption )
      ::Caption := NIL
   ENDIF

   IF ::Parent:BoldSelection .AND. ::Caption != NIL
      ::Parent:Font:Bold := .T.
   ENDIF

   tci := (struct TCITEM)
   tci:mask    := TCIF_IMAGE | TCIF_TEXT
   tci:pszText := ::Caption
   tci:iImage  := IIF( ::ImageIndex > 0, ::ImageIndex-1, -1 )
   IF ::Parent:hWnd == NIL
      ::Parent:Create()
   ENDIF
   IF SendMessage( ::Parent:hWnd, TCM_INSERTITEM, Len( ::Parent:Children ), tci ) > -1

      rc := (struct RECT)
      rc:Left  := 0
      rc:Top   := 0
      rc:Right := ::Parent:Width
      rc:Bottom:= ::Parent:Height

      SendMessage( ::Parent:hWnd, TCM_ADJUSTRECT, 0, @rc )

      ::Left   := 0               + ::LeftMargin
      ::Top    := rc:Top          + ::TopMargin
      ::Width  := ::Parent:Width  - ::RightMargin - ::LeftMargin
      ::height := ::Parent:height - ::BottomMargin - ::TopMargin

      IF ::__ClassInst != NIL .AND. ::__CustomOwner

         __ResetClassInst( Self )
         ::__ClassInst:Caption    := ::Caption
         ::__ClassInst:ImageIndex := ::ImageIndex

         IF ::BackSysColor != ::xBackColor .AND. ::xBackColor != NIL
            ::BackSysColor := ::xBackColor
         ENDIF
         IF ::ForeSysColor != ::xForeColor .AND. ::xForeColor != NIL
            ::ForeSysColor := ::xForeColor
         ENDIF

         IF ::__CustomOwner
            ::__OriginalName := ::Name
         ENDIF
      ENDIF

      ::Super:Create()

      IF ::__ClassInst == NIL
         SetParent( ::hWnd, ::Parent:Parent:hWnd )
         IF !EMPTY( ::Caption ) .AND. ( n := AT( "&", ::Caption ) ) > 0
            RegisterHotKey( ::hWnd, ::Id, MOD_ALT, ASC( ::Caption[n+1] ) )
         ENDIF
      ENDIF

      ::Index := LEN( ::Parent:Children )

      ::xPosition := ::Index
      IF ::__ClassInst != NIL
         ::__ClassInst:xPosition := ::Index
      ENDIF
      
      IF ::Index != ::Parent:Cursel .OR. ( ::__ClassInst == NIL .AND. !::Parent:Visible )
         ::HideTab()
       ELSE
         ::ShowTab()
         ::Parent:SetCurSel( ::Parent:Cursel )
      ENDIF
      
      ::__OnParentSize()
      
      IF ::__ClassInst != NIL
         ::Application:ObjectTree:Set( Self, .F. )
         ::Application:Props[ "ComboSelect" ]:Reset()
      ENDIF
      RedrawWindow( ::Parent:hWnd,,, RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
      ::Parent:InvalidateRect(,.f.)
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetVisible( lVisible, lForce ) CLASS TabPage
   DEFAULT lForce TO .F.
   IF ( lVisible != ::Visible .OR. lForce ) .AND. ::__ClassInst == NIL .AND. ::hWnd != NIL
      IF !lVisible
         ::__Hide()
       ELSE
         ::__Show()
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetPosition(n) CLASS TabPage
   LOCAL tci, nIndex, o
   n := MIN( n, LEN( ::Parent:Children ) )
   
   IF ::xPosition != n .AND. ::Index != NIL
      ADEL( ::Parent:Children, ::Index, .T. )
      ::Parent:SendMessage( TCM_DELETEITEM, ::Index - 1 )

      AINS( ::Parent:Children, n, Self, .T. )
      tci := (struct TCITEM)
      tci:mask    := TCIF_IMAGE | TCIF_TEXT
      tci:pszText := ::Caption
      tci:iImage  := IIF( ::ImageIndex > 0, ::ImageIndex-1, -1 )
      SendMessage( ::Parent:hWnd, TCM_INSERTITEM, n-1, tci )

      IF ::__ClassInst != NIL
         ::__ClassInst:xPosition := n
      ENDIF
      ::xPosition := n
      ::Index := n

      FOR nIndex := 1 TO LEN( ::Parent:Children )
          o := ::Parent:Children[nIndex]
          o:Index := nIndex
          o:xPosition := nIndex
          IF ::__ClassInst != NIL
             ::__ClassInst:xPosition := nIndex
          ENDIF
      NEXT
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Delete() CLASS TabPage
   LOCAL o, nIndex
   Super:Destroy()
   ::Parent:SendMessage( TCM_DELETEITEM, ::Index - 1 )
   FOR nIndex := 1 TO LEN( ::Parent:Children )
       o := ::Parent:Children[nIndex]
       o:Index := nIndex
       o:xPosition := nIndex
       IF ::__ClassInst != NIL
          ::__ClassInst:xPosition := nIndex
       ENDIF
   NEXT
   ::Parent:SetCurSel( MIN( ::Parent:CurSel, LEN( ::Parent:Children ) ) )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetCaption( c ) CLASS TabPage
   LOCAL n
   ::xCaption := c
   IF ::IsWindow()
      ::Parent:SetItemValues( ::Index-1, c, ::xImageIndex )
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
      tci:mask    := TCIF_IMAGE | TCIF_TEXT
      tci:pszText := ::Caption
      tci:iImage  := nImgIndex-1
      SendMessage( ::Parent:hWnd, TCM_SETITEM,  ::Index-1, tci )
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
   LOCAL rc
   ::OnParentSize( x, y, hDef )
   IF ::Index == NIL
      RETURN NIL
   ENDIF

   rc := (struct RECT)

   IF GetClassName( GetParent( ::hWnd ) ) == WC_TABCONTROL
      rc:Left   := 0
      rc:Top    := 0
      rc:Right  := ::Parent:ClientWidth
      rc:Bottom := ::Parent:ClientHeight
    ELSE
      rc:Left   := ::Parent:Left
      rc:Top    := ::Parent:Top
      rc:Right  := ::Parent:Left+::Parent:ClientWidth
      rc:Bottom := ::Parent:Top+::Parent:ClientHeight
   ENDIF

   SendMessage( ::Parent:hWnd, TCM_ADJUSTRECT, 0, @rc )

   IF ::Application:ThemeActive .AND. !::Parent:Flat .AND. ::Theming
      rc:Top    -= 1
      rc:Left   -= 3
      rc:Right  += 1
      rc:Bottom += 1
      IF ::Application:OsVersion:dwMajorVersion > 5
         rc:Bottom += 1
      ENDIF      
    ELSEIF ::Parent:Flat
      IF ::Parent:Style & TCS_VERTICAL == 0
         rc:Left   -= 1
      ENDIF
      rc:Right  += 2
      IF ::Parent:Style & TCS_BOTTOM == 0
         rc:Bottom += 3
         rc:Left   -= 2
         rc:Right  += 1
         IF ::Parent:Style & TCS_VERTICAL == TCS_VERTICAL
            rc:Top    -= 3
            rc:Left   += 2
         ENDIF
       ELSE
         rc:Left   -= 1
         rc:Right  += 1
         rc:Top    -= 3
         IF ::Parent:Style & TCS_VERTICAL != 0
            rc:Left   -= 2
            rc:Right  -= 3
            rc:Bottom += 3
         ENDIF
      ENDIF
    ELSEIF ::Application:ThemeActive .AND. ::Theming
      rc:Left   -= 1
      rc:Right  += 1
      rc:Bottom -= 1
    ELSE
      rc:Left   -= 3
      rc:Top    -= 1
      rc:Right  += 2
      rc:Bottom += 2
      IF ::Parent:Style & TCS_VERTICAL == TCS_VERTICAL
         rc:Top  -= 2
         IF ::Parent:Style & TCS_BOTTOM == 0
            rc:Left  += 2
          ELSE
            rc:Right -= 2
         ENDIF
       ELSEIF ::Parent:Style & TCS_BOTTOM == TCS_BOTTOM
         rc:Top    -= 2
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

   IF hDef == NIL
      ::SetWindowPos( , ::Left, ::Top, ::Width, ::Height, SWP_NOACTIVATE + SWP_NOOWNERZORDER + SWP_NOZORDER + IIF( ::OsVer:dwMajorVersion < 5, SWP_DEFERERASE, 0 ) )
    ELSE
      ::DeferWindowPos( hDef, , ::Left, ::Top, ::Width, ::Height, SWP_NOACTIVATE + SWP_NOOWNERZORDER + SWP_NOZORDER + IIF( ::OsVer:dwMajorVersion < 5, SWP_DEFERERASE, 0 ) )
   ENDIF

RETURN hDef

//-----------------------------------------------------------------------------------------------
METHOD Select() CLASS TabPage
   ::Parent:Children[ ::Parent:CurSel ]:HideTab()
   IF ::Parent:Visible
      ::ShowTab()
   ENDIF
   ::Parent:SetCurSel( ::Index )
   ::Parent:Redraw()
   ::Application:DoEvents()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __Hide() CLASS TabPage
   LOCAL n, nCur := ::Index
   DEFAULT ::Parent:__Tabs TO ARRAY( LEN( ::Parent:Children ) )
   IF ::Parent:__Tabs[ ::Index ] == NIL
      
      IF ( n := ASCAN( ::Parent:Children, Self,,, .T. ) ) > 0
         ADEL( ::Parent:Children, n, .T. )
         ::Parent:SendMessage( TCM_DELETEITEM, n - 1, 0 )
         ::Parent:__Tabs[ ::Index ] := n

         IF ::Parent:CurSel == n
            Super:Hide()
            IF LEN( ::Parent:Children ) > 0
               ::Parent:SetCurSel( MIN( ::Parent:CurSel, LEN( ::Parent:Children ) ) )
               ::Parent:Children[ ::Parent:CurSel ]:ShowTab()
            ENDIF
          ELSEIF ::Parent:CurSel > n
            ::Parent:CurSel--
         ENDIF
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __Show() CLASS TabPage
   LOCAL tci, nPos, n := 0
   IF ::Parent:__Tabs[ ::Index ] != NIL

      nPos := ::Index
      FOR n := 1 TO ::Index - 1
          IF ::Parent:__Tabs[n] != NIL
             nPos--
          ENDIF
      NEXT

      AINS( ::Parent:Children, nPos, Self, .T. )
      
      tci := (struct TCITEM)
      tci:mask    := TCIF_IMAGE | TCIF_TEXT
      tci:pszText := ::Caption
      tci:iImage  := IIF( ::ImageIndex > 0, ::ImageIndex-1, -1 )

      SendMessage( ::Parent:hWnd, TCM_INSERTITEM, nPos-1, tci )
      
      IF ::Parent:CurSel >= nPos
         ::Parent:CurSel := MIN( LEN( ::Parent:Children ), ::Parent:CurSel+1 )
      ENDIF
      ::Parent:Children[ ::Parent:CurSel ]:ShowTab()
      ::Parent:__Tabs[ ::Index ] := NIL

   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS TabCloseButton INHERIT Button
   METHOD Init() CONSTRUCTOR
   METHOD SetImageIndex()       INLINE Self
   METHOD DrawClose()
   METHOD OnEraseBkGnd()        INLINE 1
   METHOD OnMouseMove(n,x,y)    INLINE ::Super:OnMouseHover(n,x,y),;
                                       ::ClosePushed:= n == MK_LBUTTON,;
                                       ::CloseHover := .T.,;
                                       ::InvalidateRect(, .F. ),;
                                       NIL

   METHOD OnMouseLeave(n,x,y)   INLINE ::Super:OnMouseLeave(n,x,y), ::CloseHover := .F., ::InvalidateRect(, .F. ), NIL
   METHOD OnParentDrawItem()
   METHOD OnLButtonDown(n,x,y)  INLINE ::Super:OnLButtonDown(n,x,y),;
                                       ::ClosePushed:= .T.,;
                                       ::CloseHover := .T.,;
                                       ::InvalidateRect(, .F. ),;
                                       NIL
   METHOD OnClick()             INLINE ::Parent:Close()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS TabCloseButton
   ::__xCtrlName := "TabCloseButton"
   ::Super:Init( oParent )
   ::Left   := ::Parent:Width - 25
   ::Top    := 5
   ::Style  := WS_CHILD | WS_VISIBLE | BS_OWNERDRAW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Width  := 20
   ::Height := 18
   ::SetChildren := .F.
   ::Tooltip:Text := "Close"
   ::Create()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD DrawClose( hDC, aRect ) CLASS TabCloseButton
   LOCAL hOld
   IF ::CloseHover
      hOld := SelectObject( hDC, ::System:CurrentScheme:Pen:ButtonSelectedBorder )
      SelectObject( hDC, IIF( !::ClosePushed, ::System:CurrentScheme:Brush:ButtonSelectedGradientEnd, ::System:CurrentScheme:Brush:ButtonPressedGradientBegin ) )
      Rectangle( hDC, aRect[1], aRect[2], aRect[3], aRect[4] )
      SelectObject( hDC, hOld )
    ELSE
      _FillRect( hDC, aRect, ::Parent:BkBrush )
   ENDIF
   aRect[1]+=4
   aRect[2]+=4
   aRect[3]-=4
   aRect[4]-=4

   MoveTo( hDC, aRect[1],   aRect[2]   )
   LineTo( hDC, aRect[3]-1, aRect[4]   )
   MoveTo( hDC, aRect[1]+1, aRect[2]   )
   LineTo( hDC, aRect[3],   aRect[4]   )

   MoveTo( hDC, aRect[1],   aRect[4]-1 )
   LineTo( hDC, aRect[3]-1, aRect[2]-1 )
   MoveTo( hDC, aRect[1]+1, aRect[4]-1 )
   LineTo( hDC, aRect[3],   aRect[2]-1 )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnParentDrawItem() CLASS TabCloseButton
   IF ::Parent:DrawItemStruct:itemState & ODS_FOCUS != 0
      ::ClosePushed := .T.
   ENDIF
   IF ::Parent:DrawItemStruct:itemState & ODS_SELECTED == 0
      ::ClosePushed := .F.
   ENDIF
   ::DrawClose( ::Parent:DrawItemStruct:hDC, { 0, 0, ::Width, ::Height } )
RETURN NIL

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS TabPinButton INHERIT Button
   METHOD Init() CONSTRUCTOR
   METHOD SetImageIndex()       INLINE Self
   METHOD DrawPin()
   METHOD OnEraseBkGnd()        INLINE 1
   METHOD OnMouseMove(n,x,y)    INLINE ::Super:OnMouseHover(n,x,y),;
                                       ::ClosePushed:= n == MK_LBUTTON,;
                                       ::CloseHover := .T.,;
                                       ::InvalidateRect(, .F. ),;
                                       NIL

   METHOD OnMouseLeave(n,x,y)   INLINE ::Super:OnMouseLeave(n,x,y), ::CloseHover := .F., ::InvalidateRect(, .F. ), NIL
   METHOD OnParentDrawItem()
   METHOD OnLButtonDown(n,x,y)  INLINE ::Super:OnLButtonDown(n,x,y),;
                                       ::ClosePushed:= .T.,;
                                       ::CloseHover := .T.,;
                                       ::InvalidateRect(, .F. ),;
                                       NIL
   METHOD OnClick()             INLINE ::Parent:UnDock()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS TabPinButton
   ::__xCtrlName := "TabPinButton"
   ::Super:Init( oParent )
   ::Left   := ::Parent:Width - 25
   ::Top    := 5
   ::Style  := WS_CHILD | WS_VISIBLE | BS_OWNERDRAW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Width  := 20
   ::Height := 18
   ::SetChildren := .F.
   ::Tooltip:Text := "Undock"
   ::Create()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD DrawPin( hDC, aRect ) CLASS TabPinButton
   LOCAL hOld
   LOCAL nLeft, nRight, nBottom
   IF ::CloseHover
      hOld := SelectObject( hDC, ::System:CurrentScheme:Pen:ButtonSelectedBorder )
      SelectObject( hDC, IIF( !::ClosePushed, ::System:CurrentScheme:Brush:ButtonSelectedGradientEnd, ::System:CurrentScheme:Brush:ButtonPressedGradientBegin ) )
      Rectangle( hDC, aRect[1], aRect[2], aRect[3], aRect[4] )
      SelectObject( hDC, hOld )
    ELSE
      _FillRect( hDC, aRect, ::Parent:BkBrush )
   ENDIF
   aRect[1]+=4
   aRect[2]+=4
   aRect[3]-=4
   aRect[4]-=4

   nLeft   := aRect[1] + ( (aRect[3]-aRect[1])/2 ) - 2
   nRight  := aRect[1] + ( (aRect[3]-aRect[1])/2 ) + 3
   nBottom := aRect[2] + ( (aRect[4]-aRect[2])/2 ) + 2

   Rectangle( hDC, nLeft, aRect[2], nRight, nBottom )
   MoveTo( hDC, nRight-2, aRect[2]+1 )
   LineTo( hDC, nRight-2, nBottom )

   MoveTo( hDC, nLeft-1, nBottom-1 )
   LineTo( hDC, nRight+1, nBottom-1 )

   MoveTo( hDC, nLeft+2, nBottom )
   LineTo( hDC, nLeft+2, aRect[4]+1 )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnParentDrawItem() CLASS TabPinButton
   IF ::Parent:DrawItemStruct:itemState & ODS_FOCUS != 0
      ::ClosePushed := .T.
   ENDIF
   IF ::Parent:DrawItemStruct:itemState & ODS_SELECTED == 0
      ::ClosePushed := .F.
   ENDIF
   ::DrawPin( ::Parent:DrawItemStruct:hDC, { 0, 0, ::Width, ::Height } )
RETURN NIL

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS TabStrip INHERIT TabControl
   PROPERTY ColorScheme  READ xColorScheme WRITE __SetColorScheme DEFAULT 1
   DATA EnumColorScheme  EXPORTED INIT { { "System Default", "Classic", "NormalColor", "HomeStead", "Metallic", "MediaCenter", "Aero" }, {1,2,3,4,5,6,7} }
      
   DATA Border, AllowMaximize, HighlightCaption, SmallCaption, Theming, ClientEdge, StaticEdge, ClipChildren, ClipSiblings, NoActivate PROTECTED INIT .F.
   DATA Caption, AnimationStyle
   DATA BoldSelection, Flat INIT .F.
   DATA ItemHeight       EXPORTED INIT 19
   METHOD Init()  CONSTRUCTOR
   METHOD Create() 
   METHOD OnWindowPaint()
   METHOD DrawTabStripItem()
   METHOD __SetColorScheme()
   METHOD __GetGradienBrush()
   METHOD OnEraseBkGnd()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS TabStrip
   ::__xCtrlName := "TabStrip"
   Super:Init( oParent )
RETURN Self

METHOD Create() CLASS TabStrip
   Super:Create()
   IF ::ShowTabs
      ::SendMessage( TCM_SETITEMSIZE, 0, MAKELPARAM(0,::ItemHeight) )
   ENDIF
RETURN Self
//-----------------------------------------------------------------------------------------------
METHOD OnEraseBkGnd( hDC ) CLASS TabStrip
   LOCAL hBrush := ::BkBrush
   DEFAULT hBrush TO ::__hBrush
   DEFAULT hBrush TO ::Parent:BkBrush
   DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )
   IF hBrush != NIL
      _FillRect( hDC, { 0, 0, ::ClientWidth, ::ClientHeight }, hBrush )
   ENDIF
RETURN 1

//-----------------------------------------------------------------------------------------------
METHOD OnWindowPaint() CLASS TabStrip
   LOCAL hOldPen, aRect, nSel, aItem, hDC, cPaint, nItems, n, nColor
   LOCAL hMemDC, hMemBitmap, hOldBitmap, hBrush//, hMemDC1, hOldBitmap1

   aRect := _GetClientRect( ::hWnd )
   nSel  := TabCtrl_GetCurSel( ::hWnd )+1
   IF nSel == 0
      ::SetCurSel(1)
   ENDIF
   hDC   := _BeginPaint( ::hWnd, @cPaint )

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   //---------------------------------------------------------------------------
   hBrush := ::BkBrush
   //DEFAULT hBrush TO ::__hBrush
   DEFAULT hBrush TO ::Parent:BkBrush
   IF hBrush == NIL
      nColor := GetPixel( ::Parent:Drawing:hDC, 2,2 )
      ::BkBrush := CreateSolidBrush( nColor )
      hBrush := ::BkBrush
   ENDIF
   DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )

   
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
   
   IF LEN( ::Children ) > 0 .AND. ::ShowTabs
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

   _EndPaint( ::hWnd, cPaint)
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD __SetColorScheme(n) CLASS TabStrip
   ::__ColorTable := ProfessionalColorTable():Load( ::EnumColorScheme[1][n] )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD DrawTabStripItem( hDC, lCurSel, nSel ) CLASS TabStrip
   LOCAL d, hRgn, aRect, n := 0, aItem := ::GetItemRect( nSel )
   LOCAL x, y, hOldFont, hBrush, hOldPen, tci, lGrad := .F., lVert := .F.
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
   
   hRgn := _CreatePolygonRgn( aRect, ALTERNATE )
   IF hRgn != NIL
      
      IF nSel > 0 .AND. LEN( ::Children ) >= nSel .AND. VALTYPE( ::Children[ nSel ] ) == "O" .AND. ::Children[ nSel ]:BkBrush != NIL
         hBrush := ::Children[ nSel ]:BkBrush
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

   IF nSel > 0 .AND. LEN( ::Children ) >= nSel .AND. ::ImageList != NIL .AND. ::Children[nSel]:ImageIndex > 0
      IF !lVert
         ::ImageList:DrawImage( hDC, ::Children[nSel]:ImageIndex, aItem[1]+6, aItem[2]+3, ILD_TRANSPARENT )
         aItem[1]+=::ImageList:IconWidth + 3
       ELSE
         ::ImageList:DrawImage( hDC, ::Children[nSel]:ImageIndex, aItem[1]+3, aItem[2]+4, ILD_TRANSPARENT )
         aItem[2]+=::ImageList:IconWidth + 1
      ENDIF
   ENDIF

   tci := (struct TCITEM)
   tci:mask := TCIF_TEXT
   tci:pszText := SPACE(255)
   tci:cchTextMax := 255

   SendMessage( ::hWnd, TCM_GETITEM, nSel-1, @tci )

   IF !EMPTY( tci:pszText )

      IF VALTYPE( ::Children[nSel] ) == "O"
         SetTextColor( hDC, ::Children[nSel]:ForeColor )
      ENDIF
      IF lGrad
         SetTextColor( hDC, RGB(0,0,0) )
      ENDIF

      SetBkMode( hDC, TRANSPARENT )
      
//      hOldFont := SelectObject( hDC, IIF( lVert, ::VertFont:Handle, ::Font:Handle ) )
      
      IF !lVert
         hOldFont := SelectObject( hDC, ::Font:Handle )
         _DrawText( hDC, tci:pszText, aItem, DT_CENTER|DT_VCENTER|DT_SINGLELINE  )
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
   IF ::ColorScheme == 1
      nColor1 := ::System:CurrentScheme:ToolStripGradientBegin
      nColor2 := ::System:CurrentScheme:ToolStripGradientEnd
    ELSE
      nColor1 := ::__ColorTable:ToolStripGradientBegin
      nColor2 := ::__ColorTable:ToolStripGradientEnd
   ENDIF
   hMemDC     := CreateCompatibleDC( hDC )
   
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

