/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// ToolBar.prg                                                                                          *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*
static aMonitors
static s_oItem
static s_nMenu

#define CCM_SETWINDOWTHEME      (CCM_FIRST + 0xb)
#define CCM_DPISCALE            (CCM_FIRST + 0xc) // wParam == Awareness

#define TB_SETWINDOWTHEME       CCM_SETWINDOWTHEME

#include "debug.ch"
#include "vxh.ch"

#define MBM_POPUPMENU  (WM_USER + 1801)
#define SM_CXSHADOW 4


#define ATL_IDM_FIRST_MDICHILD 50000
#define IDM_MDI_BASE      (ATL_IDM_FIRST_MDICHILD - 5)
#define IDM_MDI_ICON      (IDM_MDI_BASE + 0)
#define IDM_MDI_GAP       (IDM_MDI_BASE + 1)
#define IDM_MDI_MINIMIZE  (IDM_MDI_BASE + 2)
#define IDM_MDI_RESTORE   (IDM_MDI_BASE + 3)
#define IDM_MDI_CLOSE     (IDM_MDI_BASE + 4)
#define IDM_MDI_FIRST     IDM_MDI_ICON
#define IDM_MDI_LAST      IDM_MDI_CLOSE

#define WP_MDISYSBUTTON          14
#define WP_MDIMINBUTTON          16
#define WP_MDICLOSEBUTTON        20
#define WP_MDIRESTOREBUTTON      22
#define WP_MDIHELPBUTTON         24
#define MDIBS_NORMAL              1
#define MDIBS_HOT                 2
#define MDIBS_PUSHED              3
#define MDIBS_DISABLED            4

#define DG_ADDCONTROL             1

#xtranslate NTRIM( < n > ) = > ALLTRIM( STR( < n > ) )

EXIT PROCEDURE __CleanTB()
   s_oItem := NIL
RETURN

//-----------------------------------------------------------------------------------------------

CLASS ToolBar INHERIT Control

   ACCESS RowCount          INLINE SendMessage( ::handle, TB_GETROWS, 0, 0 )
   ASSIGN RowCount( nRows ) INLINE SendMessage( ::handle, TB_SETROWS, nRows )

   PROPERTY List               INDEX TBSTYLE_LIST                  READ xList               WRITE SetStyle     DEFAULT .F.
   PROPERTY ToolTips           INDEX TBSTYLE_TOOLTIPS              READ xToolTips           WRITE SetStyle     DEFAULT .F.
   PROPERTY Flat               INDEX TBSTYLE_FLAT                  READ xFlat               WRITE SetStyle     DEFAULT .T.
   PROPERTY Transparent        INDEX TBSTYLE_TRANSPARENT           READ xTransparent        WRITE SetStyle     DEFAULT .T.
   PROPERTY ImageList                                              READ xImageList          WRITE SetImageList
   PROPERTY HotImageList                                           READ xHotImageList       WRITE SetHotImageList

   PROPERTY HorzPadding        INDEX 1                             READ xHorzPadding        WRITE SetPadding
   PROPERTY VertPadding        INDEX 2                             READ xVertPadding        WRITE SetPadding

   PROPERTY DrawArrows         INDEX TBSTYLE_EX_DRAWDDARROWS       READ xDrawArrows         WRITE SetExtStyle  DEFAULT .T.
   PROPERTY MixedButtons       INDEX TBSTYLE_EX_MIXEDBUTTONS       READ xMixedButtons       WRITE SetExtStyle  DEFAULT .F.
   PROPERTY HideClippedButtons INDEX TBSTYLE_EX_HIDECLIPPEDBUTTONS READ xHideClippedButtons WRITE SetExtStyle  DEFAULT .F.
   PROPERTY Border             INDEX WS_BORDER                     READ xBorder             WRITE SetStyle     DEFAULT .F.

   PROPERTY BitmapWidth                                            READ xBitmapWidth        WRITE _SetBitmapSize DEFAULT -1
   PROPERTY BitmapHeight                                           READ xBitmapHeight       WRITE _SetBitmapSize DEFAULT -1

   ACCESS ImageList        INLINE __ChkComponent( Self, ::xImageList )     PERSISTENT

   EXPORTED:
      DATA Caption
      DATA ClipChildren  INIT .T.
      DATA ClipSiblings  INIT .T.
      DATA StaticEdge    INIT .F.
      DATA ClientEdge    INIT .F.
      DATA TabStop       INIT .F.

   DATA Banner           EXPORTED
   DATA aItems           EXPORTED
   DATA NoThemeCheck     EXPORTED    INIT .F.
   DATA PrevFoc          EXPORTED
   DATA aReset           EXPORTED
   DATA Chevron          EXPORTED

   DATA hWndHook         EXPORTED
   DATA nPressed         EXPORTED INIT -1
   DATA pRect            EXPORTED
   DATA lKeyboard        EXPORTED  INIT .F.
   DATA PushPos          EXPORTED AS ARRAY
   DATA SelPopup         EXPORTED INIT .F.
   DATA lToolMenu        EXPORTED  INIT .T.
   DATA ButtonCheckSolid EXPORTED  INIT .F.
   DATA ButtonCheckColor EXPORTED INIT GetSysColor( COLOR_BTNFACE )
   DATA MenuWindow       EXPORTED
   DATA ChevronRect      EXPORTED
   DATA AllowUnDock      EXPORTED INIT FALSE
   DATA AllowClose       EXPORTED INIT FALSE
   DATA MenuWnd          EXPORTED
   DATA __hDesList       EXPORTED
   DATA __lIcon          PROTECTED INIT .F.
   DATA __aBtns          PROTECTED INIT {}
   DATA aBitmaps         PROTECTED
   DATA ParProc          PROTECTED
   DATA ExtStyle         PROTECTED INIT TBSTYLE_EX_DRAWDDARROWS
   DATA lDestroyed       PROTECTED INIT .F.
   DATA oMenu            PROTECTED
   DATA PrevMenuProc     PROTECTED
   DATA hWndMenu         PROTECTED
   DATA hPrevWndMenu     PROTECTED
   DATA hBmpBkGnd        PROTECTED
   DATA aDeleted         PROTECTED AS ARRAY INIT {}
   DATA nItem            PROTECTED
   DATA nPushed          PROTECTED
   DATA ShowMDIButtons   PROTECTED INIT .T.

   METHOD SetBanner()
   METHOD IsButtonChecked( nId ) INLINE ::GetButtonInfo( nId ):fsState & TBSTATE_CHECKED != 0
   METHOD AddBitmap()
   METHOD Init( oParent )        CONSTRUCTOR
   METHOD Create()
   METHOD Disableall()
   METHOD Enableall()
   METHOD __AddNewItemMenuItem()
   METHOD __ChkImageList()
   
   METHOD GetImageList()                     INLINE ::SendMessage( TB_GETIMAGELIST, 0, 0 )
   METHOD SetDrawTextFlags( nFlag1, nFlag2 ) INLINE ::SendMessage( TB_SETDRAWTEXTFLAGS, nFlag1, nFlag2 )
   METHOD SetBitmapSize( nWidth, nHeight )   INLINE ::SendMessage( TB_SETBITMAPSIZE, 0, MAKELONG( nWidth, nHeight ) )
   METHOD SetButtonSize( nWidth, nHeight )   INLINE ::SendMessage( TB_SETBUTTONSIZE, 0, MAKELONG( nWidth, nHeight ) )
   METHOD SetButtonWidth( nMin, nMax )       INLINE ::SendMessage( TB_SETBUTTONWIDTH, 0, MAKELONG( nMin, nMax ) )
   METHOD GetButtonSize()                    INLINE ::SendMessage( TB_GETBUTTONSIZE, 0, 0 )
   METHOD GetButtonBitmap( nButton )         INLINE ::SendMessage( TB_GETBITMAP, nButton, 0 )

   METHOD DisableBtn( nBtn )      INLINE ::SendMessage( TB_ENABLEBUTTON, nBtn, 0)
   METHOD EnableBtn( nBtn, lEnable ) INLINE ::SendMessage( TB_ENABLEBUTTON, nBtn, IIF( lEnable==NIL, .T., lEnable ) )
   METHOD GetButtonCount()        INLINE SendMessage( ::hWnd, TB_BUTTONCOUNT, 0, 0 )
   METHOD SetImageList(oImg)      INLINE ::__ChkImageList(1,@oImg), SendMessage( ::hWnd, TB_SETIMAGELIST, 0, IIF( oImg != NIL, oImg:handle, NIL ) ), ::xImageList := oImg
   METHOD SetHotImageList(oImg)   INLINE ::__ChkImageList(2,@oImg), SendMessage( ::hWnd, TB_SETHOTIMAGELIST, 0, IIF( oImg != NIL, oImg:handle, NIL ) ), ::xHotImageList := oImg
   METHOD CheckButton(nBtn,lFlag) INLINE lFlag := IFNIL( lFlag, .T., lFlag ),;
                                         ::SendMessage( TB_CHECKBUTTON, nBtn, If( lFlag, 1, 0 ) )

   METHOD IsButtonClicked(nBtn)   INLINE IF( ::SendMessage( TB_ISBUTTONCHECKED, nBtn, 0) == 0, .F., .T. )
   METHOD SetListStyle()          INLINE ::Style := ::Style | TBSTYLE_LIST
   METHOD Disable()               INLINE ::DisableAll()
   METHOD Enable()                INLINE ::EnableAll()

   METHOD SetPadding()
   METHOD GetGapWidth()
//   METHOD Hook()
   METHOD SetButtonState()
   METHOD GetButtonInfo()
   METHOD GetButton()
   METHOD DrawArrow()
   METHOD CreateDropDown()
   METHOD SetColorScheme()
   METHOD GetHeight() INLINE HiWord(::GetButtonSize())*(LEN(::aItems))
   METHOD GetButtonsWidth()
   METHOD OpenPopup()

   METHOD OnUserMsg()
   METHOD OnInitMenuPopup()
   METHOD OnMeasureItem()
   METHOD OnDrawItem()
   METHOD OnLButtonDblClk(nwParam,x,y,hWnd) INLINE ::OnLButtonDown(nwParam,x,y,hWnd)
   METHOD OnKillFocus()
   METHOD OnToolTipNotify()
   METHOD OnParentNotify()
   METHOD OnParentCommand()
   METHOD OnDestroy()
   METHOD OnSize()
   METHOD MoveMenu()
   METHOD UpdateMenu()
   METHOD AddControl()
   METHOD __OnParentSize()
   METHOD SetExtStyle()
   METHOD __AddButton()
   METHOD _SetBitmapSize() INLINE IIF( ::hWnd != NIL .AND. ::BitmapWidth >= 0 .AND. ::BitmapHeight >= 0 , ::SendMessage( TB_SETBITMAPSIZE, 0, MAKELONG( ::BitmapWidth, ::BitmapHeight ) ), NIL )
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ToolBar

   ::ClsName  := "ToolBarWindow32"
   DEFAULT ::__xCtrlName TO "ToolBar"
   ::Super:Init( oParent )
   ::Style           :=  WS_CHILD | WS_VISIBLE | TBSTYLE_TRANSPARENT | CCS_NODIVIDER | CCS_NORESIZE | TBSTYLE_FLAT | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ThemeName := "toolbar"

   //::CCS_Adjustable  := .T.
   //::CCS_NoResize    := .T.
   //::CCS_NoDevider   := .T.

   ::Transparent     := .T.
   ::ToolTips        := .T.
//   ::Flat            := .T.

   ::pRect           := (struct POINT)
   ::pRect:x         := 0
   ::pRect:y         := 0
   ::Left            := 0
   ::Top             := 0
   ::Width           := 150
   ::Height          := 37
   ::ExStyle         := WS_EX_TOOLWINDOW
   ::Id              := 0
   ::aItems          := {}
   ::aReset          := {}
   ::Id              := ::Form:GetNextControlId()
   DEFAULT ::Application:__hMenuHook TO SetWindowsHookEx( WH_MSGFILTER, @__SysVxhMenuHook(), NIL, GetCurrentThreadId() )
RETURN( Self )


//-----------------------------------------------------------------------------------------------
//FUNCTION EnumMonitorsProc( hMonitor, hdc, lpRMonitor, dwData )
//   LOCAL cLabel, n, cAddress, mix := (struct MONITORINFOEX)
//   mix:cbSize := mix:SizeOf()
//   IF GetMonitorInfo( hMonitor, @mix )
//      AADD( aMonitors, mix )
//   ENDIF
//RETURN .T.

METHOD __ChkImageList(n,oImg) CLASS ToolBar
   IF n == 1
      DEFAULT oImg TO ::xImageList
    ELSE
      DEFAULT oImg TO ::xHotImageList
   ENDIF
   IF VALTYPE( oImg ) == "C"
      IF ASCAN( ::Form:Property:Keys, {|c| UPPER(c) == UPPER(oImg) } ) > 0
         oImg := ::Form:Property[ oImg ]
       ELSE
         IF ::__ClassInst != NIL
            oImg := ::Application:Project:Forms[1]:Property[ oImg ]
          ELSE
            oImg := ::Application:MainForm:Property[ oImg ]
         ENDIF
      ENDIF
   ENDIF
RETURN Self

METHOD __AddButton() CLASS ToolBar
   ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., Self, "ToolButton",,,1, {}, } }, ::Application:Project:aUndo )
RETURN( Self )

METHOD Create() CLASS ToolBar
   LOCAL nPadding, tbb := (struct TBBUTTON)
   ::Super:Create()

   //aMonitors := {}
   //EnumDisplayMonitors( NIL, NIL, WinCallBackPointer( @EnumMonitorsProc() ) )

   SendMessage( ::hWnd, TB_BUTTONSTRUCTSIZE, tbb:SizeOf(), 0 )
   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "&Add Button", {|| ::__AddButton()} } }
   ENDIF
   IF !::__MenuBar
      IF ::ImageList != NIL
         SendMessage( ::hWnd, TB_SETIMAGELIST, 0, ::ImageList:handle )
      ENDIF
      IF ::HotImageList != NIL
         SendMessage( ::hWnd, TB_SETHOTIMAGELIST, 0, ::HotImageList:handle )
      ENDIF
   ENDIF

   ::xTransparent   := ::Style & TBSTYLE_TRANSPARENT   != 0
   IF ::ExtStyle != 0
      ::SendMessage( TB_SETEXTENDEDSTYLE, 0, ::ExtStyle )
   ENDIF

   nPadding := SendMessage( ::hWnd, TB_GETPADDING, 0, 0 )

   IF ::xHorzPadding == NIL
      ::xHorzPadding := LOWORD( nPadding )
      IF ::__ClassInst != NIL
         ::__ClassInst:xHorzPadding := LOWORD( nPadding )
      ENDIF
   ENDIF

   IF ::xVertPadding == NIL
      ::xVertPadding := HIWORD( nPadding )
      IF ::__ClassInst != NIL
         ::__ClassInst:xVertPadding := HIWORD( nPadding )
      ENDIF
   ENDIF

   ::SetPadding( 1, ::xHorzPadding )
   ::SetPadding( 2, ::xVertPadding )
   ::_SetBitmapSize()
   ::Border := ::xBorder .AND. ( !::Theming .OR. !::Application:IsThemedXP )
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD AddControl( oControl ) CLASS ToolBar
   IF oControl:__xCtrlName != "ComboBox"
      oControl:Dock:Bottom := Self
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetExtStyle( nStyle, lSet ) CLASS ToolBar
   DEFAULT lSet TO .T.
   IF lSet
      ::ExtStyle := ::ExtStyle | nStyle
    ELSE
      ::ExtStyle := ::ExtStyle & NOT( nStyle )
   ENDIF
   IF ::hWnd != NIL
      ::SendMessage( TB_SETEXTENDEDSTYLE, 0, ::ExtStyle )
   ENDIF
RETURN Self

METHOD OnKillFocus() CLASS ToolBar
   IF ::__ClassInst == NIL
      ::nPressed := -1
      SendMessage( ::hWndHook, WM_CANCELMODE, 0, 0)
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD SetBanner( hInst, cName, cPath, nMask ) CLASS ToolBar

   ::Banner := Bitmap( hInst, cName, cPath, nMask, .T. )
   ::Height := MAX( ::ClientHeight, ::Banner:height )

RETURN SELF


//-----------------------------------------------------------------------------------------------

METHOD OnDestroy() CLASS ToolBar
   ::Super:OnDestroy()
   IF ::Banner != NIL
      ::Banner:Delete()
   ENDIF
   IF ::__hDesList != NIL
      ImageListDestroy( ::__hDesList )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD SetColorScheme( nHighlight, nShadow ) CLASS ToolBar
   LOCAL cs := (struct COLORSCHEME)
   cs:dwSize          := cs:SizeOf()
   cs:clrBtnHighlight := nHighlight
   cs:clrBtnShadow    := nShadow
   SendMessage( ::hWnd, TB_SETCOLORSCHEME, 0, cs )
RETURN SELF


METHOD SetPadding( x, n ) CLASS ToolBar
   LOCAL lSet := .T.
   IF x == 1
      ::xHorzPadding := n
    ELSE
      ::xVertPadding := n
   ENDIF
   IF ::hWnd != NIL
      ::SendMessage( TB_SETPADDING, 0, MAKELPARAM( ::xHorzPadding, ::xVertPadding ) )
   ENDIF
RETURN SELF


//-----------------------------------------------------------------------------------------------

METHOD OnMeasureItem( nwParam, nlParam ) CLASS ToolBar
   LOCAL n, oItem, oButton, oSub
   (nwParam)
   IF ::MeasureItemStruct:CtlType == ODT_MENU

      IF ::Chevron != NIL
         IF ( n := ASCAN( ::Chevron:aItems, {|o|o:Id==::MeasureItemStruct:itemID } ) ) > 0
            RETURN ::Chevron:aItems[n]:MeasureItem( ::MeasureItemStruct, nlParam )
         ELSE
            FOR EACH oSub IN ::Chevron:Menu:aItems
               IF ( oItem := oSub:GetMenuById( ::MeasureItemStruct:itemID ) )!= NIL
                  RETURN oItem:MeasureItem( ::MeasureItemStruct, nlParam )
               ENDIF
            NEXT
         ENDIF
      ENDIF

      FOR EACH oButton IN ::aItems
         IF oButton:Style & BTNS_SEP == 0 .AND. oButton:Menu != NIL
            IF ( n := ASCAN( oButton:Menu:aItems, {|o|o:Id==::MeasureItemStruct:itemID } ) ) > 0
               RETURN oButton:Menu:aItems[n]:MeasureItem( ::MeasureItemStruct, nlParam )
            ENDIF
            // it is NOT the 1st level, let's see the rest
            FOR EACH oSub IN oButton:Menu:aItems
               IF ( oItem := oSub:GetMenuById( ::MeasureItemStruct:itemID ) )!= NIL
                  RETURN oItem:MeasureItem( ::MeasureItemStruct, nlParam )
               ENDIF
            NEXT
         ENDIF
      NEXT
   ENDIF

RETURN NIL


//-----------------------------------------------------------------------------------------------

METHOD OnDrawItem() CLASS ToolBar
   LOCAL hDC, rc, n:=0, hBrush, xIcon:=0, yIcon:=0
   LOCAL nTop, oButton, oItem, oMenu, nOff := 0, Chk
   LOCAL lCheck := .F., nPos, lArrow := .F., lWhole := .F., oSub
   LOCAL lDisabled, lSelected, nIcon, lDrop := .F.
   LOCAL Rect, pt

   IF ::DrawItemStruct:CtlType == ODT_MENU

      IF ::DrawItemStruct:itemState > 200
         ::DrawItemStruct:itemState -= 256
      ENDIF

      FOR EACH oButton IN ::aItems
         IF oButton:Style & BTNS_SEP == 0 .AND. oButton:Menu != NIL
            IF ( n := ASCAN( oButton:Menu:aItems, {|o|o:Id==::DrawItemStruct:itemID } ) ) > 0
               oButton:Menu:aItems[n]:DrawItem( ::DrawItemStruct, .F. )
               RETURN 1
            ENDIF
            // it is NOT the 1st level, let's see the rest
            FOR EACH oSub IN oButton:Menu:aItems
               IF ( oItem := oSub:GetMenuById(::DrawItemStruct:itemID ) )!= NIL
                  oItem:DrawItem( ::DrawItemStruct, .F. )
                  RETURN 1
               ENDIF
            NEXT
         ENDIF
      NEXT

      IF ::Chevron != NIL
         n := 0
         IF ( n := ASCAN( ::Chevron:aItems, {|o|o:Id==::DrawItemStruct:itemID } ) ) > 0
            oMenu:= ::Chevron
            oItem:= ::Chevron:aItems[n]
            nPos := n
         ELSE
            FOR EACH oSub IN ::Chevron:Menu:aItems
               IF ( oItem := oSub:GetMenuById( ::DrawItemStruct:itemID ) )!= NIL
                  oItem:DrawItem( ::DrawItemStruct, .F. )
                  RETURN 1
               ENDIF
            NEXT
         ENDIF
      ENDIF

      IF n > 0

         Chk := (struct RECT)
         pt  := (struct POINT)

         hDC   := ::DrawItemStruct:hDC
         rc := (struct RECT)
         rc:Buffer( ::DrawItemStruct:rcItem:value )

         IF oMenu:ImageList != NIL
            xIcon := oMenu:ImageList:IconWidth
            yIcon := oMenu:ImageList:IconHeight
         ENDIF
         nIcon := xIcon
         xIcon := MAX( xIcon, GetSystemMetrics( SM_CXMENUCHECK )+2 )//+6
         yIcon := MAX( yIcon, GetSystemMetrics( SM_CYMENUCHECK ) )


         rc:right  := xIcon + rc:right
         rc:bottom := MAX( rc:bottom, 2+yIcon+1 )
         rc:bottom := MAX( rc:bottom, GetSystemMetrics( SM_CYMENU ) )

         lDisabled  := ::DrawItemStruct:itemState & ODS_DISABLED != 0
         lSelected  := ::DrawItemStruct:itemState & ODS_SELECTED != 0

         IF lSelected .AND. !lDisabled
            // for chevron bar use only
            IF ::DrawItemStruct:itemAction==ODA_DRAWENTIRE .AND. ::OsVer:dwMajorVersion <= 4
               hBrush:= GetSysColorBrush( COLOR_MENU )
               FillRect( hDC, rc, hBrush )
            ENDIF
            rc:right := rc:right-xIcon
            n := 0

            DO CASE
               CASE oItem:Style & BTNS_WHOLEDROPDOWN != 0
                    Chk:left   := rc:right-GetSystemMetrics( SM_CXMENUCHECK )
                    Chk:top    := rc:top
                    Chk:right  := rc:right
                    Chk:bottom := rc:bottom

                    lArrow := .T.
                    IF ::DrawItemStruct:itemAction==ODA_DRAWENTIRE
                       OffsetRect( @Chk, 1, 1 )
                       lWhole := .T.
                    ENDIF

               CASE oItem:Style & BTNS_DROPDOWN != 0
                    n := rc:right
                    rc:right -= GetSystemMetrics( SM_CXMENUCHECK )
                    IF ::DrawItemStruct:itemAction==ODA_DRAWENTIRE
                       _ScreenToClient( WindowFromDC( hDC ), ::PushPos )
                       IF ::PushPos[1] >= rc:right
                          lCheck := .T.
                       ENDIF
                    ENDIF
                    lArrow := .T.

            ENDCASE

            IF ( ::Application:IsThemedXP .AND. ::Theming )

               //hBrush := GetSysColorBrush( COLOR_HIGHLIGHT )
               //SetTextColor( hDC, GetSysColor( COLOR_HIGHLIGHTTEXT ) )
               //FillRect( hDC, rc, hBrush )

               IF n>0
                  Chk:left   := rc:right
                  Chk:top    := rc:top
                  Chk:right  := n
                  Chk:bottom := rc:bottom
                  rc:right := n
               ENDIF
               SetTextColor( hDC, GetSysColor( COLOR_HIGHLIGHTTEXT ) )
               _FillRect( hDC, { rc:Left, rc:top, rc:right, rc:bottom }, GetSysColorBrush( COLOR_HIGHLIGHT ) )
               _FillRect( hDC, { rc:Left+1, rc:top+1, rc:right-1, rc:bottom-1}, GetSysColorBrush( IIF( ::Application:IsThemedXP, 29, COLOR_HIGHLIGHT ) ) )
               IF n>0
                  _FillRect( hDC, { Chk:Left, Chk:top, Chk:right, Chk:bottom }, GetSysColorBrush( COLOR_HIGHLIGHT ) )
                  _FillRect( hDC, { Chk:Left+1, Chk:top+1, Chk:right-1, Chk:bottom-1 }, GetSysColorBrush( IIF( ::Application:IsThemedXP, 29, COLOR_HIGHLIGHT ) ) )
               ENDIF

               IF n>0
                  IF ::DrawItemStruct:itemAction==ODA_DRAWENTIRE
                     OffsetRect( @Chk, 1, 1 )
                  ENDIF
               ENDIF
             ELSE
               DrawEdge( hDC, rc, IIF( ::DrawItemStruct:itemAction==ODA_DRAWENTIRE .AND. !lCheck, BDR_SUNKENOUTER, BDR_RAISEDINNER), BF_RECT )
               IF n>0
                  Chk:left   := rc:right
                  Chk:top    := rc:top
                  Chk:right  := n
                  Chk:bottom := rc:bottom
                  rc:right := n
                  DrawEdge( hDC, Chk, IIF( !::DrawItemStruct:itemAction==ODA_DRAWENTIRE, BDR_RAISEDINNER,BDR_SUNKENOUTER), BF_RECT )
                  IF ::DrawItemStruct:itemAction==ODA_DRAWENTIRE
                     OffsetRect( @Chk, 1, 1 )
                  ENDIF
               ENDIF
            ENDIF

            IF lArrow
               ::DrawArrow( hDC, Chk )
            ENDIF
            IF ::DrawItemStruct:itemAction==ODA_DRAWENTIRE.AND.!lCheck
               OffsetRect( @rc, 1, 1 )
               nOff := 1
            ENDIF

         ELSE
            IF ::Parent:StatusBar != NIL
               ::Parent:StatusBar:SendMessage( SB_SIMPLE, .F., 0 )
            ENDIF

            hBrush:= oItem:BkBrush
            FillRect( hDC, rc, hBrush )

            IF oMenu:ImageBkColor != NIL
               Rect := (struct RECT)
               Rect:left   := rc:right
               Rect:top    := rc:top
               Rect:right  := xIcon+3
               Rect:bottom := rc:bottom
               FillRect( hDC, Rect, oMenu:ImageBkColor )
            ENDIF

            IF oItem:Style != NIL .AND. ( oItem:Style & BTNS_DROPDOWN != 0 .OR. oItem:Style & BTNS_WHOLEDROPDOWN != 0 )

               Rect := (struct RECT)
               Rect:left   := rc:right - ( ( GetSystemMetrics( SM_CXMENUCHECK ) )+xIcon )
               Rect:top    := rc:top
               Rect:right  := rc:right - xIcon
               Rect:bottom := rc:bottom
               ::DrawArrow( hDC, Rect )
            ENDIF
         ENDIF
         nTop := rc:top + ( ( (rc:bottom-rc:top) - yIcon) / 2 )
         SetBkMode( hDC, 1 )

         IF oMenu:ImageList != NIL .AND. oItem:ImageIndex != NIL .AND. oItem:ImageIndex > 0 .AND. ::DrawItemStruct:itemState & ODS_CHECKED == 0
            IF lDisabled
               oMenu:ImageList:DrawDisabled( hDC, oItem:ImageIndex, rc:left+2, nTop )
            ELSE
               IF oItem:Caption == NIL
                  rc:left := (::DrawItemStruct:rcItem:Right/2)-(xIcon/2)
               ENDIF
               IF ::DrawItemStruct:itemState == ODS_SELECTED .AND. oMenu:HotImageList != NIL
                  oMenu:HotImageList:DrawImage( hDC, oItem:ImageIndex, rc:left+2, nTop )
               ELSE
                  oMenu:ImageList:DrawImage( hDC, oItem:ImageIndex, rc:left+2, nTop, ILD_TRANSPARENT )
               ENDIF
            ENDIF
         ENDIF
         IF oItem:Caption != NIL
            rc:left := 2+xIcon+4+nOff
            oItem:DrawItemText( hDC, ::Parent:DrawItemStruct, rc:Array, lDisabled )
         ENDIF

         //----------------------------------------------------------------------------------------------
         IF ::DrawItemStruct:itemAction==ODA_DRAWENTIRE .AND. ( lCheck.OR.lWhole ) .AND. ( oItem:Style & BTNS_DROPDOWN != 0 .OR. oItem:Style & BTNS_WHOLEDROPDOWN != 0 )
            IF ( nPos := ASCAN( ::aItems, {|o|o:Id==::DrawItemStruct:itemID} ) ) > 0
               pt:x := ::DrawItemStruct:rcItem:Left
               pt:y := ::DrawItemStruct:rcItem:Bottom
               ClientToScreen( WindowFromDC( hDC ), @pt )
               ::CreateDropDown( nPos, pt:x, pt:y, TPM_LEFTALIGN | TPM_TOPALIGN, ::hWnd )
               SendMessage( ::hWnd, WM_CANCELMODE, 0, 0 )
            ENDIF
         ENDIF
         //----------------------------------------------------------------------------------------------
         RETURN 1
      ENDIF
   ENDIF

RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD DrawArrow(hDC,aRect) CLASS ToolBar

   LOCAL nLeft,nTop,nRight,nBottom,hOld,nM,nL,nR,nT

   nLeft  :=aRect[1]
   nTop   :=aRect[2]
   nRight :=aRect[3]-1
   nBottom:=aRect[4]-1
   SelectObject(hDC,GetStockObject(BLACK_PEN))
   hOld:=SelectObject(hDC,GetStockObject(BLACK_BRUSH))
   nM:=((nRight+nLeft)/2)
   nL:=(nM-2)
   nR:=(nM+2)
   nT:=nTop+((nBottom-nTop)/2)
   _Polygon(hDC,{ {nL,nT},{nR,nT},{nM,nT+2} })
   SelectObject(hDC,hOld)

RETURN SELF


//-----------------------------------------------------------------------------------------------

METHOD AddBitmap( hInst, nhIdBmp ) CLASS ToolBar

   LOCAL tbab := (struct TBADDBITMAP)

   tbab:hInst := hInst
   tbab:nID[1]:= nhIdBmp

   SendMessage( ::hWnd, TB_ADDBITMAP, 0, tbab )

RETURN SELF


//-----------------------------------------------------------------------------------------------

METHOD SetButtonState( nBtn, nState ) CLASS ToolBar

   LOCAL tbbi := (struct TBBUTTONINFO)

   tbbi:cbSize  := tbbi:sizeof()
   tbbi:dwMask  := TBIF_STATE
   tbbi:fsState := nState

   SendMessage( ::hWnd, TB_SETBUTTONINFO, nBtn, tbbi )

RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD GetButtonsWidth( x ) CLASS ToolBar

   LOCAL n, nWidth := 0
   LOCAL rc := (struct RECT)
   LOCAL tbb

   DEFAULT x TO SendMessage( ::hWnd, TB_BUTTONCOUNT, 0, 0 )

   FOR n := 0 TO x-1
       SendMessage( ::hWnd, TB_GETITEMRECT, n, @rc )

       tbb := ::GetButton( n )

       IF tbb:idCommand <= IDM_MDI_ICON //::aItems[n+1]:id <= IDM_MDI_ICON
          nWidth += rc:Right - rc:Left
       ENDIF
   NEXT

RETURN nWidth


//-----------------------------------------------------------------------------------------------

METHOD GetButton( nBtn ) CLASS ToolBar

   LOCAL tbb := (struct TBBUTTON)

   SendMessage( ::hWnd, TB_GETBUTTON, nBtn, @tbb )

RETURN tbb


//-----------------------------------------------------------------------------------------------

METHOD GetButtonInfo( nBtn, nMask ) CLASS ToolBar
   LOCAL tbbi := (struct TBBUTTONINFO)

   DEFAULT nMask TO TBIF_STATE

   tbbi:cbSize  := tbbi:sizeof()
   tbbi:dwMask  := nMask

   SendMessage( ::hWnd, TB_GETBUTTONINFO, nBtn, @tbbi )

RETURN tbbi

//-----------------------------------------------------------------------------------------------

METHOD OnInitMenuPopup( nwParam, nPos ) CLASS ToolBar
   IF ::Chevron!=NIL .AND. ::Chevron:hMenu == nwParam .AND. !::Chevron:CoolBar
      RETURN 0
   ENDIF

   IF ::__MenuBar
      ::SelPopup := .F.
      IF nPos == 0 .AND. ::lKeyboard .AND. ::MenuWnd == NIL
         PostMessage( ::hWndHook, WM_KEYDOWN, VK_DOWN, 0)
         ::lKeyboard := .F.
      ENDIF
      ::MenuWnd := nwParam
      IF nPos == 0
         ::MenuWindow := 1
      ENDIF

   ENDIF

RETURN NIL


//-----------------------------------------------------------------------------------------------

METHOD OnUserMsg( hWnd, nMsg ) CLASS ToolBar

   DO CASE
      CASE nMsg == MBM_POPUPMENU

           IF ::nPressed < 0
              ::nPressed := SendMessage( hWnd, TB_BUTTONCOUNT ) - 1
              IF ::aItems[::nPressed+1]:Id == IDM_MDI_CLOSE
                 ::nPressed -= 4
              ENDIF
           ENDIF
           IF ::nPressed >= SendMessage( hWnd, TB_BUTTONCOUNT )
              ::nPressed := 0
           ENDIF
           IF ::aItems[::nPressed+1]:Enabled
              ::OpenPopup( hWnd )
           ENDIF
           RETURN 0
   ENDCASE

RETURN NIL


//-----------------------------------------------------------------------------------------------

METHOD OnParentCommand( nId, nCode, nlParam ) CLASS ToolBar

   LOCAL n, oChild, oButton, oItem, nRet

   ::UpdateWindow()

   IF nlParam == ::hWnd .OR.nlParam == 0
      IF (n:=ASCAN(::aItems, {|btn| btn:oStruct:idCommand==nId } ) ) > 0
         IF ::Parent:ClsName == "OptionBar" .AND. ::Parent:CheckGroup
            IF ::nPushed != NIL
               SendMessage( ::hWnd, TB_PRESSBUTTON, ::nPushed, MAKELONG(0,0) )
            ENDIF
            ::nPushed := ::aItems[n]:Id
            SendMessage( ::hWnd, TB_PRESSBUTTON, ::nPushed, MAKELONG(1,0) )
         ENDIF
         nRet := ExecuteEvent( "OnClick", ::aItems[n] )
         __Evaluate( ::aItems[n]:Action, ::aItems[n] )
         ::aItems[n]:OnClick()
         RETURN nRet
      ENDIF

      FOR EACH oButton IN ::aItems
          IF oButton:Menu != NIL .AND. ( oItem := oButton:Menu:GetItem( nId ) )!= NIL
             RETURN __Evaluate( oItem:Action, oItem )
          ENDIF
      NEXT
   ENDIF

   FOR EACH oChild IN ::Children
       IF oChild:ClsName != "ToolButton"
          nRet := oChild:OnParentCommand( nId, nCode, nlParam )
          IF nRet != NIL
             EXIT
          ENDIF
        ELSE
       ENDIF
   NEXT
RETURN nRet

//-----------------------------------------------------------------------------------------------

METHOD OpenPopup( hWnd ) CLASS ToolBar

   LOCAL n, oBtn, nAdd, nHeight, hMonitor, mix
   LOCAL rc := (struct RECT)
   LOCAL pt := (struct POINT)
   LOCAL oItem
   DEFAULT hWnd TO ::hWnd

   IF (n:=ASCAN( ::aItems,{|o| o:xPosition==::nPressed })) > 0
      oBtn  := ::aItems[n]

      IF oBtn:Menu != NIL .AND. !EMPTY( oBtn:Menu:aItems )
/*
         SendMessage( hWnd, TB_GETRECT, oBtn:ID, @rc )
         pt:x := rc:Left
         pt:y := rc:Bottom
         ClientToScreen( hWnd, @pt )

         rcWorkArea := (struct RECT)
         cBuff := rcWorkArea:Value
         SystemParametersInfo( SPI_GETWORKAREA, 0, @cBuff, 0 )
         rcWorkArea:Buffer( cBuff )
         IF rcWorkArea:right - pt:x < 150
            pt:x := rc:right
            pt:y := rc:bottom
            ClientToScreen( hWnd, @pt )
         ENDIF
*/
         SendMessage( hWnd, TB_GETITEMRECT, oBtn:xPosition, @rc )
         pt:x := rc:Left
         pt:y := rc:Bottom
         ClientToScreen( hWnd, @pt )

         //-----------------------------------------------------
         oBtn:Menu:Style := TPM_LEFTALIGN+TPM_TOPALIGN

         IF ( hMonitor := MonitorFromPoint( pt, MONITOR_DEFAULTTONEAREST ) ) != 0
            mix := (struct MONITORINFOEX)
            mix:cbSize := mix:SizeOf()
            IF GetMonitorInfo( hMonitor, @mix )
               pt:x := MAX( pt:x, mix:rcMonitor:Left )
            ENDIF
         ENDIF
         oBtn:Menu:Left  := pt:x
         oBtn:Menu:Top   := pt:y

         IF ( nAdd := ASCAN( oBtn:Menu:aItems, {|o| o:Caption == "[ Add New Item ]" } ) ) > 0
            FOR EACH oItem IN oBtn:Menu:aItems
                // SUBMENUS [ Add New Item ]
                IF oItem:Caption != "[ Add New Item ]"
                   oItem:CheckForDefaultItem()
                ENDIF
            NEXT
         ENDIF


         oBtn:Menu:Create()

         FOR EACH oItem IN oBtn:Menu:aItems
             DeleteMenu( oItem:Menu:hMenu, oItem:Id, MF_BYCOMMAND )
         NEXT

         nHeight := 0
         n := GetSystemMetrics( SM_CYMENUSIZE )
         FOR EACH oItem IN oBtn:Menu:aItems
            IF oItem:Visible .OR. ::__ClassInst != NIL
               oItem:Create()
               nHeight += n
            ENDIF
         NEXT
         //------------------------------------------------------

         SendMessage( hWnd, TB_PRESSBUTTON, oBtn:Id, MAKELONG(1,0) )
         UpdateWindow( hWnd )
         ::hWndHook := hWnd

         ::Application:__CurCoolMenu := Self
         ::oMenu := oBtn:Menu

         IF pt:y + nHeight + GetSystemMetrics( SM_CYMIN ) > GetSystemMetrics( SM_CYSCREEN )
            pt:x := rc:Left
            pt:y := rc:Top
            ClientToScreen( hWnd, @pt )
            oBtn:Menu:Top   := pt:y
            oBtn:Menu:Style := TPM_LEFTALIGN+TPM_BOTTOMALIGN
         ENDIF

         oBtn:Menu:Context( ::hWnd )

         ::hWndHook  := NIL


         SendMessage( hWnd, TB_PRESSBUTTON, oBtn:Id, MAKELONG(0,0) )
         DestroyMenu( ::oMenu:hMenu )

         oBtn:Menu:Destroy()

         ::oMenu     := NIL
         ::MenuWnd   := NIL

         IF ::lKeyboard .AND. ::__ClassInst != NIL .AND. oBtn:Caption != "[ Add New Item ]"
            ::Application:Project:CurrentForm:SelectControl( oBtn, .F. )
            ::Application:ObjectTree:Set( oBtn )
            SendMessage( ::hWnd, TB_SETHOTITEM, ::nPressed, 0 )
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD CreateDropDown( nBtn, nLeft, nBottom, nStyle, hWnd, nTop ) CLASS ToolBar

   LOCAL nHeight, oItem, oBtn := ::aItems[nBtn]

   IF oBtn:Menu != NIL
      DEFAULT nTop TO nBottom
      oBtn:Menu:Style := nStyle
      oBtn:Menu:Left  := nLeft
      oBtn:Menu:Top   := nBottom
      oBtn:Menu:Create()
      nHeight := 0
      FOR EACH oItem IN oBtn:Menu:aItems
         oItem:Create()
         nHeight += GetSystemMetrics( SM_CYMENUSIZE )
      NEXT

      IF nBottom + nHeight + GetSystemMetrics( SM_CYMIN ) > GetSystemMetrics( SM_CYSCREEN )
         oBtn:Menu:Top   := nTop
         oBtn:Menu:Style := TPM_LEFTALIGN+TPM_BOTTOMALIGN
      ENDIF

      oBtn:Menu:Context( hWnd )
      oBtn:Menu:Destroy()
   ENDIF

RETURN(1)


//-----------------------------------------------------------------------------------------------------

METHOD MoveMenu( hWnd, lChevron ) CLASS ToolBar

   LOCAL nScreen, Rect, rc, nLeft, pt := (struct POINT)

   nScreen := GetSystemMetrics( SM_CXSCREEN )
   GetWindowRect( hWnd, @Rect )

   IF !lChevron
      rc := (struct RECT)

      SendMessage( ::hWnd, TB_GETITEMRECT, ::nPressed, @rc )

      pt:x := rc:right
      pt:y := rc:top

      ClientToScreen( ::hWnd, @pt )

      IF Rect:left != ( pt:x - (rc:right-rc:left) )
         nLeft := pt:x - ( Rect:right-Rect:left )
         IF nLeft + ( Rect:right-Rect:left ) < nScreen
            MoveWindow( hWnd, nLeft, Rect:top, ( Rect:right-Rect:left ), (Rect:bottom-Rect:top), .T. )
         ENDIF
      ENDIF
    ELSE
      pt:x := ::ChevronRect:left
      pt:y := ::ChevronRect:top

      ClientToScreen( ::Parent:hWnd, @pt )

      IF Rect:left-2 != pt:x
         nLeft := Rect:left + (::ChevronRect:right-::ChevronRect:left) - 2//              aPt[1] - ( aRect[3]-aRect[1] ) + 3
         IF nLeft + ( Rect:right-Rect:left ) < nScreen
            MoveWindow( hWnd, nLeft, Rect:top, ( Rect:right-Rect:left ), (Rect:bottom-Rect:top), .T. )
         ENDIF
      ENDIF
   ENDIF

RETURN 0

//-----------------------------------------------------------------------------------------------

METHOD __AddNewItemMenuItem( oParent ) CLASS ToolBar
   LOCAL oItem, oSubItem

   oItem := CMenuItem( oParent )
   oItem:Position := LEN( oItem:Menu:aItems )-2
   oItem:Caption  := oItem:Name

   oSubItem := CMenuItem()
   oSubItem:GenerateMember := .F.
   oSubItem:Init( oItem, .T. )

   oSubItem:Caption   := "[ Add New Item ]"
   oSubItem:Font:Bold := .T.
   oSubItem:Default   := .T.
   oSubItem:Create()

   oItem:Create()

   ::Application:Project:CurrentForm:SelectControl( oItem, .F. )
   ::Application:Project:Modified := .T.
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD OnToolTipNotify( nwParam, nlParam, hdr ) CLASS ToolBar
   LOCAL n, cText
   (nwParam)
   IF (n:=ASCAN(::aItems,{|btn| btn:oStruct:idCommand==hdr:idFrom})) > 0 .AND. hdr:code == TTN_NEEDTEXT
      cText := ::aItems[n]:ToolTip
      IF VALTYPE( cText ) == "B" 
         cText := EVAL( cText, Self )
      ENDIF

      VXH_SetToolBarToolTip( nlParam, cText )
      RETURN 1
   ENDIF

RETURN NIL


//-----------------------------------------------------------------------------------------------

METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS ToolBar
   LOCAL oButton, oBtn, nDif, n, nState, x, nRet
   LOCAL nHot, oMdi, nTop, aRect, hIcon, hTheme, nButton, hBrush
   LOCAL tbcd, nmt, nmtb, rc, pt, cr, lColor := .F.
   (nwParam)
   DO CASE
      CASE hdr:code==NM_CUSTOMDRAW .AND. !::Parent:ClsName == "StatusBarPanel"
           nRet := CDRF_DODEFAULT
           tbcd := (struct NMTBCUSTOMDRAW)
           tbcd:Pointer( nlParam )

           IF .T. //( ::ForeColor != NIL .AND. !(::ForeColor == ::ForeSysColor ) ) .OR. ::ButtonCheckColor != NIL .OR. ::Banner != NIL .OR. tbcd:nmcd:dwItemSpec >= IDM_MDI_GAP

              rc := (struct RECT)
              pt := (struct POINT)
              DO CASE
                 CASE tbcd:nmcd:dwDrawStage == CDDS_PREPAINT
                      nRet := CDRF_NOTIFYITEMDRAW

                 CASE tbcd:nmcd:dwDrawStage == CDDS_ITEMPREPAINT
                      //IF ::ButtonCheckColor != NIL
                         tbcd:clrMark := RGB(0,0,0)//::ButtonCheckColor
                         lColor := .T.
                      //ENDIF
                      IF !(::ForeColor == ::ForeSysColor) .AND. ::ForeColor != NIL
                         tbcd:clrText    := ::ForeColor
                         tbcd:clrTextHighlight := ::ForeColor
                         lColor := .T.
                      ENDIF
                      IF !(::BackColor == ::BackSysColor) .AND. ::BackColor != NIL
                         tbcd:clrBtnFace := ::BackColor
                         lColor := .T.
                      ENDIF
                      IF lColor
                         tbcd:CopyTo( nlParam )
                      ENDIF

                      IF ::__MenuBar
                         nState := NIL
                         DO CASE
                            CASE tbcd:nmcd:dwItemSpec == IDM_MDI_ICON
                                 oMdi := ::Parent:MDIClient:GetActive()
                                 hIcon := oMdi:GetIcon( ICON_SMALL )

                                 IF hIcon == 0
                                    hIcon := GetClassLong( oMdi:hWnd, GCL_HICONSM )
                                    IF hIcon == 0
                                       hIcon := LoadIcon( 0, IDI_WINLOGO )
                                    ENDIF
                                 ENDIF

                                 nTop  := ( ( tbcd:nmcd:rc:Bottom - GetSystemMetrics(SM_CYSMICON) ) / 2 )
                                 DrawIconEx( tbcd:nmcd:hdc, 0, nTop, hIcon, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), 0,, DI_NORMAL )
                                 DestroyIcon( hIcon )
                                 SetWindowLong( ::Parent:hWnd, DWL_MSGRESULT, CDRF_SKIPDEFAULT )
                                 RETURN CDRF_SKIPDEFAULT

                            CASE tbcd:nmcd:dwItemSpec == IDM_MDI_GAP
                                 IF ::OsVer:dwMajorVersion < 5 .OR. !::Application:ThemeActive .OR. !::Theming
                                    FillRect( tbcd:nmcd:hdc, tbcd:nmcd:rc, GetSysColorBrush( COLOR_BTNFACE ) )
                                 ENDIF
                                 SetWindowLong( ::Parent:hWnd, DWL_MSGRESULT, CDRF_SKIPDEFAULT )
                                 RETURN CDRF_SKIPDEFAULT

                            CASE tbcd:nmcd:dwItemSpec == IDM_MDI_MINIMIZE
                                 nState := DFCS_CAPTIONMIN
                                 nButton:= WP_MDIMINBUTTON

                            CASE tbcd:nmcd:dwItemSpec == IDM_MDI_RESTORE
                                 nState := DFCS_CAPTIONRESTORE
                                 nButton:= WP_MDIRESTOREBUTTON

                            CASE tbcd:nmcd:dwItemSpec == IDM_MDI_CLOSE
                                 nState := DFCS_CAPTIONCLOSE
                                 nButton:= WP_MDICLOSEBUTTON
                         ENDCASE

                         IF nState != NIL
                            IF ::ShowMDIButtons
                               tbcd:nmcd:rc:Top += (tbcd:nmcd:rc:bottom - tbcd:nmcd:rc:top - 14) / 2 + 1
                               tbcd:nmcd:rc:bottom := tbcd:nmcd:rc:top + 14
                               tbcd:nmcd:rc:right  := tbcd:nmcd:rc:left + 16
                               IF nState == DFCS_CAPTIONCLOSE
                                  tbcd:nmcd:rc:Left+=2
                                  tbcd:nmcd:rc:Right+=2
                               ENDIF
                               rc:left   := tbcd:nmcd:rc:Left//-1
                               rc:top    := tbcd:nmcd:rc:top
                               rc:right  := tbcd:nmcd:rc:right//+1
                               rc:bottom := tbcd:nmcd:rc:bottom
                               IF !::Application:IsThemedXP .OR. !::Theming
                                  FillRect( tbcd:nmcd:hdc, rc, GetSysColorBrush( COLOR_BTNFACE ) )
                                  IF tbcd:nmcd:uItemState & CDIS_DISABLED == 0
                                     DrawFrameControl( tbcd:nmcd:hdc, tbcd:nmcd:rc, DFC_CAPTION, nState + IIF( tbcd:nmcd:uItemState & CDIS_SELECTED != 0, DFCS_PUSHED, 0 ) )
                                   ELSE
                                     DrawFrameControl( tbcd:nmcd:hdc, tbcd:nmcd:rc, DFC_CAPTION, nState + DFCS_INACTIVE )
                                  ENDIF
                                ELSE
                                  nState := IIF( ::Parent:Active, MDIBS_NORMAL, MDIBS_DISABLED )
                                  IF tbcd:nmcd:uItemState & CDIS_SELECTED != 0
                                     nState := MDIBS_PUSHED
                                  ENDIF
                                  IF tbcd:nmcd:uItemState & CDIS_DISABLED != 0
                                     nState := MDIBS_DISABLED
                                  ENDIF
                                  IF tbcd:nmcd:uItemState & CDIS_HOT != 0
                                     nState := MDIBS_HOT
                                  ENDIF

                                  hTheme := OpenThemeData(,"window")
                                  aRect := { tbcd:nmcd:rc:Left, Int(tbcd:nmcd:rc:Top-2), tbcd:nmcd:rc:Right, Int(tbcd:nmcd:rc:Bottom) }
                                  DrawThemeBackground( hTheme, tbcd:nmcd:hdc, nButton, nState, aRect, aRect )
                                  CloseThemeData( hTheme )
                               ENDIF
                               RETURN CDRF_SKIPDEFAULT

                            ENDIF
                            nRet   := CDRF_SKIPDEFAULT
                         ENDIF
//---------------------------------------------------------------------------------------------------------------------------------------------------------
                         IF ( tbcd:nmcd:uItemState == CDIS_HOT .OR. tbcd:nmcd:uItemState == CDIS_SELECTED .OR. tbcd:nmcd:uItemState == 65 )

                            IF ::Application:IsThemedXP
                               nHot := SendMessage( ::hWnd, TB_GETHOTITEM, 0, 0 )+1
                               IF nHot == 0
                                  nHot := ::nPressed+1
                               ENDIF
                               IF nHot > 0
                                  hBrush := GetSysColorBrush( COLOR_HIGHLIGHT )

                                  SelectObject( tbcd:nmcd:hdc, hBrush )
                                  SelectObject( tbcd:nmcd:hdc, GetStockObject( NULL_PEN ) )
                                  Rectangle( tbcd:nmcd:hdc, tbcd:nmcd:rc:Left, tbcd:nmcd:rc:Top, tbcd:nmcd:rc:Right, tbcd:nmcd:rc:Bottom+1 )
                                  SetBkMode( tbcd:nmcd:hdc, TRANSPARENT )
                                  SelectObject( tbcd:nmcd:hdc, ::GetFont() )
                                  SetTextColor( tbcd:nmcd:hdc, GetSysColor( COLOR_HIGHLIGHTTEXT ) )
                                  DrawText( tbcd:nmcd:hdc, ::aItems[nHot]:Caption, tbcd:nmcd:rc, DT_CENTER | DT_VCENTER | DT_SINGLELINE )
                                  nRet := CDRF_SKIPDEFAULT
                               ENDIF
                            ENDIF
                         ENDIF
//---------------------------------------------------------------------------------------------------------------------------------------------------------

                      ENDIF
              ENDCASE

           ENDIF
           SetWindowLong( ::Parent:hWnd, DWL_MSGRESULT, nRet )
           RETURN nRet

      CASE hdr:code==TBN_DROPDOWN
           Nmt := (struct NMTOOLBAR)
           Nmt:Pointer( nlParam )
           IF (n:=ASCAN( ::aItems,{|o| o:Id==nmt:iItem })) > 0
              oBtn  := ::aItems[n]
              IF oBtn:Menu != NIL
                 rc := (struct RECT)
                 cr := (struct RECT)
                 GetWindowRect( hdr:hwndFrom, @rc )
                 GetClientRect( hdr:hwndFrom, @cr )

                 nDif  := cr:bottom - Nmt:rcButton:Bottom

                 ::CreateDropDown( n, Nmt:rcButton:Left+rc:left, rc:bottom-nDif, TPM_LEFTALIGN+TPM_TOPALIGN,, rc:top )

                 RETURN 0
              ENDIF
           ENDIF

      CASE hdr:code==TBN_QUERYINSERT
           RETURN .T.

      CASE hdr:code==TBN_QUERYDELETE
           RETURN .T.

      CASE hdr:code==TBN_ENDADJUST
           IF ::Owner != NIL
              ::Owner:Chevron := .T.
           ENDIF
           RETURN 1

      CASE hdr:code==TBN_GETBUTTONINFO
           IF Nmt == NIL
              Nmt := (struct NMTOOLBAR)
           ENDIF
           nmt:Pointer( nlParam )
           IF nmt:iItem >= 0 .AND. nmt:iItem <= LEN( ::aItems )
              IF nmt:iItem == 0
                 nmt:iItem := 1
              ENDIF
              nmt:tbbutton := ::aItems[nmt:iItem]:oStruct
              nmt:pszText  := ::aItems[nmt:iItem]:Caption
              nmt:CopyTo( nlParam )

              ::nItem := nmt:iItem
              RETURN .T.
           ENDIF
           RETURN .F.

      CASE hdr:code==TBN_TOOLBARCHANGE
           IF ::nItem != NIL
              SendMessage( hdr:hwndFrom, TB_SETBUTTONINFO, ::aItems[::nItem]:Id, ::aItems[::nItem]:oBtnInfo )
              ::nItem := NIL
           ENDIF
           RETURN 0

      CASE hdr:code==TBN_RESET
           IF hdr:hwndFrom == ::hWnd

              x := LEN( ::aItems )
              FOR n := 1 TO x
                  SendMessage( ::hWnd, TB_DELETEBUTTON, 0, 0 )
              NEXT
              ::aItems := ACLONE( ::aReset )
              ::aDeleted := {}
              x := LEN( ::aItems )
              n := -1
              FOR EACH oButton IN ::aItems
                  oButton:xPosition := n++
                  SendMessage( ::hWnd, TB_ADDBUTTONS, 1, oButton:oStruct )
                  SendMessage( ::hWnd, TB_SETBUTTONINFO, oButton:Id, oButton:oBtnInfo )
              NEXT

           ENDIF

      CASE hdr:code==TBN_HOTITEMCHANGE
           IF Nmtb == NIL
              Nmtb := (struct NMTBHOTITEM)
           ENDIF
           Nmtb:Pointer( nlParam )

           IF ::Form:StatusBar != NIL .AND. !::__MenuBar
              IF Nmtb:idNew==0 .AND. Nmtb:idOld > 0
                 ::Form:StatusBar:SendMessage( SB_SIMPLE, .F., 0 )

               ELSEIF ( n := ASCAN( ::aItems, {|o|o:Id == Nmtb:idNew} ) ) > 0
                 IF ::aItems[n]:Message != NIL
                    ::Form:StatusBar:SendMessage( SB_SIMPLE, .T., 0 )
                    ::Form:StatusBar:SendMessage( SB_SETTEXT, SB_SIMPLEID + SBT_NOBORDERS, " "+::aItems[n]:Message )
                  ELSE
                    ::Form:StatusBar:SendMessage( SB_SIMPLE, .F., 0 )
                 ENDIF
               ELSE
                 ::Form:StatusBar:SendMessage( SB_SIMPLE, .F., 0 )
              ENDIF
           ENDIF

           IF ::__MenuBar .AND. ::nPressed < 0

              IF Nmtb:dwFlags & HICF_ACCELERATOR != 0
                 SendMessage( ::hWnd, TB_SETHOTITEM, -1, 0 )
                 ::nPressed := Nmtb:idNew
                 ::lKeyboard := .T.
                 ::OpenPopup( ::hWnd )
                 RETURN 1
              ENDIF
           ENDIF

   ENDCASE

RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD disableall() CLASS ToolBar
   AEVAL(::aItems,{|btn| ::disableBtn(btn:oStruct:idCommand)})
RETURN(self)


//-----------------------------------------------------------------------------------------------

METHOD enableall() CLASS ToolBar
   AEVAL(::aItems,{|btn| ::enableBtn(btn:oStruct:idCommand)})
RETURN(self)

//-----------------------------------------------------------------------------------------------

METHOD UpdateMenu( lShow ) CLASS ToolBar
   LOCAL oBtn, oItem, hSysMenu, n, cText, nId, oMdi, nTab, nStyle

   oMdi := ::Form:MDIClient:GetActive()
   IF oMdi != NIL
      nStyle := oMdi:GetWindowLong( GWL_STYLE )
      IF nStyle & WS_MAXIMIZE == WS_MAXIMIZE
         lShow := .T.
      ENDIF
   ENDIF
   IF lShow
      IF ::__lIcon
         AEVAL( ::__aBtns, {|o| o:Visible := .T. } )
         FOR EACH oBtn IN ::__aBtns
             DO CASE
                CASE oBtn:Id == IDM_MDI_ICON
                     oBtn:Visible := oMdi:SysMenu

                CASE oBtn:Id == IDM_MDI_GAP
                     oBtn:Enabled  := .F.

                CASE oBtn:Id == IDM_MDI_MINIMIZE
                     oBtn:Visible  := oMdi:MinimizeBox .AND. oMdi:SysMenu

                CASE oBtn:Id == IDM_MDI_RESTORE
                     oBtn:Visible  := oMdi:MinimizeBox .AND. oMdi:SysMenu

                CASE oBtn:Id == IDM_MDI_CLOSE
                     oBtn:Visible  := oMdi:MinimizeBox .AND. oMdi:SysMenu
             ENDCASE
         NEXT
         ::InvalidateRect()
         RETURN Self
      ENDIF
      ::__lIcon := .T.

      // SysMenu
      oBtn := ToolButton( Self )
      oBtn:Style := TBSTYLE_BUTTON //| TBSTYLE_DROPDOWN
      oBtn:Id    := IDM_MDI_ICON
      oBtn:Width := GetSystemMetrics( SM_CXSMICON )+3
      oBtn:Menu  := MenuPopup( Self )
      oBtn:Visible := oMdi:SysMenu
      oBtn:AutoSize := .F.
      oBtn:Create( 0 )

      AADD( ::__aBtns, oBtn )

      hSysMenu   := GetSystemMenu( oMdi:hWnd, .F. )

      FOR n := 1 TO GetMenuItemCount( hSysMenu )

          nId   := GetMenuItemId( hSysMenu, n-1 )
          cText := GetMenuString( hSysMenu, n-1, MF_BYPOSITION )

          IF EMPTY( ctext )
             ctext := "-"
             nId   := NIL
          ENDIF
          oItem := CMenuItem( oBtn )
          IF ( nTab := AT( CHR(9), cText ) ) == 0
             oItem:Caption := cText
           ELSE
             oItem:Caption := ALLTRIM( LEFT( cText, nTab-1 ) )
             oItem:ShortCutText := SUBSTR( cText, nTab+1 )
          ENDIF
          oItem:Id      := nId
          oItem:Action  := {|o| o:Form:__PrcMdiMenu( o:Id ) }

          oItem:MenuItemInfo:cbSize := oItem:MenuItemInfo:SizeOf()
          oItem:MenuItemInfo:fMask  := MIIM_CHECKMARKS | MIIM_DATA | MIIM_ID | MIIM_STATE | MIIM_SUBMENU | MIIM_TYPE

//          GetMenuItemInfo( hSysMenu, n-1, .T., @oItem:MenuItemInfo )
//          oItem:BmpItem := oItem:MenuItemInfo:hBmpItem
          oItem:Enabled := GetMenuState( hSysMenu, n-1, MF_BYPOSITION ) & MF_DISABLED == 0
          oItem:Create()
      NEXT

      oBtn := ToolButton( Self )
      oBtn:Id       := IDM_MDI_GAP
      oBtn:Width    := ::GetGapWidth()
      oBtn:AutoSize := .F.
      oBtn:Enabled  := .F.
      oBtn:Create()
      AADD( ::__aBtns, oBtn )

      // Buttons
      oBtn := ToolButton( Self )
      oBtn:Id       := IDM_MDI_MINIMIZE
      oBtn:Width    := 16
      oBtn:AutoSize := .F.
      oBtn:Visible  := oMdi:MinimizeBox .AND. oMdi:SysMenu
      oBtn:Create()
      AADD( ::__aBtns, oBtn )

      oBtn := ToolButton( Self )
      oBtn:Id       := IDM_MDI_RESTORE
      oBtn:Width    := 16
      oBtn:AutoSize := .F.
      oBtn:Visible  := oMdi:MinimizeBox .AND. oMdi:SysMenu
      oBtn:Create()
      AADD( ::__aBtns, oBtn )

      oBtn := ToolButton( Self )
      oBtn:Id       := IDM_MDI_CLOSE
      oBtn:Width    := 18
      oBtn:AutoSize := .F.
      oBtn:Visible  := oMdi:MinimizeBox .AND. oMdi:SysMenu
      oBtn:Create()
      AADD( ::__aBtns, oBtn )

      IF ::Owner != NIL
         ::Owner:Chevron := .T.
         //::Owner:SetChevron()
      ENDIF
    ELSE
      IF ::__lIcon
         ::InvalidateRect()
         AEVAL( ::__aBtns, {|o| o:Visible := .F. } )
         IF ::Owner != NIL
            //::Owner:SetChevron()
            ::Owner:Chevron := .T.
         ENDIF
      ENDIF
   ENDIF
RETURN Self

METHOD GetGapWidth() CLASS ToolBar
   LOCAL oBtn, nWidth := 0
   FOR EACH oBtn IN ::aItems
       IF oBtn:Id <= IDM_MDI_ICON
          nWidth += oBtn:Width
       ENDIF
   NEXT
   IF nWidth > 0
      nWidth := ( ::ClientWidth - nWidth - 50 )
   ENDIF
RETURN nWidth

METHOD __OnParentSize( x, y, hDef, lMoveNow, lNoMove ) CLASS ToolBar
   LOCAL oControl
   ::Super:__OnParentSize( x, y, hDef, lMoveNow, lNoMove )
   ::GetClientRect()
   FOR EACH oControl IN ::Children
       IF oControl:__xCtrlName == "ComboBox"
          IF oControl:FitToolBar
             oControl:xTop := 1
             oControl:SetItemHeight( -1, ::ClientHeight-8 )
             oControl:MoveWindow()
           ELSE
             oControl:xTop := ( (::ClientHeight-6)/2 ) - ( oControl:GetItemHeight( -1 )/2 )
             oControl:MoveWindow()
          ENDIF
       ENDIF
   NEXT
RETURN hDef

//-----------------------------------------------------------------------------------------------

METHOD OnSize() CLASS ToolBar
   LOCAL n, tbbi

   IF ::Parent:MdiContainer
      n := ::GetGapWidth()

      IF n > 0
         IF ::ClientWidth-n-50 > 0
            ::SendMessage( WM_SETREDRAW, .F., 0 )
            tbbi := (struct TBBUTTONINFO)
            tbbi:cbSize  := tbbi:sizeof()
            tbbi:dwMask  := TBIF_SIZE
            tbbi:cx      := n

            SendMessage( ::hWnd, TB_SETBUTTONINFO, IDM_MDI_GAP, tbbi )

            ::SendMessage( WM_SETREDRAW, .T., 0 )
            ::ShowMDIButtons := .T.
            ::ValidateRect()

            ::RedrawWindow( , , RDW_NOFRAME + RDW_NOINTERNALPAINT + RDW_INVALIDATE + RDW_UPDATENOW + RDW_ALLCHILDREN )
          ELSE
            ::ShowMDIButtons := .F.
            ::RedrawWindow( , , RDW_NOFRAME + RDW_ERASE + RDW_NOINTERNALPAINT + RDW_INVALIDATE + RDW_UPDATENOW + RDW_ALLCHILDREN )
         ENDIF
         RETURN 0
      ENDIF
   ENDIF
   ::InvalidateRect(,.F.)
RETURN 0

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS ToolButton INHERIT Object

   DATA ToolTip          PUBLISHED
   DATA Message          PUBLISHED

   DATA Menu             EXPORTED

   ACCESS ImageList      INLINE ::Parent:ImageList
   ACCESS HotImageList   INLINE ::Parent:HotImageList


   PROPERTY ContextMenu GET __ChkComponent( Self, ::xContextMenu )

   EXPORTED:
      DATA Id
      DATA AllowUnDock            INIT FALSE
      DATA AllowClose             INIT FALSE
      DATA Active                 INIT .F.
      DATA Action
      DATA State                  INIT TBSTATE_ENABLED
      DATA Cargo
      DATA Parent
      DATA oStruct
      DATA oBtnInfo
      DATA Style
      DATA __PropFilter           INIT {}
      DATA Color
      DATA Left
      DATA Top
      DATA Events                 INIT {  {"General", { { "OnClick"       , "", "" } } } }
      DATA IsMenuItem             INIT .F.
      DATA Children
      DATA hWnd                   INIT 0
      DATA __IsInstance
      DATA __IdeImageIndex        INIT 3
      DATA Height
      DATA MdiMenu                INIT .F.
      DATA EventHandler
      DATA nHScrollPos            INIT 0
      DATA VertScrollPos          INIT 0
      DATA Font
      DATA KeepParentActive       INIT .F.
      DATA ShortCut
   PROPERTY WholeDropDown INDEX BTNS_WHOLEDROPDOWN  READ xWholeDropDown WRITE SetStyle DEFAULT .F.
   PROPERTY DropDown      INDEX BTNS_DROPDOWN       READ xDropDown      WRITE SetStyle DEFAULT .F.

   PROPERTY AutoSize      INDEX TBSTYLE_AUTOSIZE    READ xAutoSize      WRITE SetStyle DEFAULT .T.
   PROPERTY ShowText      INDEX BTNS_SHOWTEXT       READ xShowText      WRITE SetStyle DEFAULT .F.

   PROPERTY Width                                   READ xWidth         WRITE SetWidth DEFAULT 24
   PROPERTY Caption                                 READ xCaption       WRITE SetText

   PROPERTY ImageIndex                              READ xImageIndex    WRITE SetImageIndex DEFAULT 0

   PROPERTY Check         INDEX BTNS_CHECK          READ xCheck         WRITE SetStyle DEFAULT .F.
   PROPERTY CheckGroup    INDEX TBSTYLE_CHECKGROUP  READ xCheckGroup    WRITE __Dummy DEFAULT .F.

   PROPERTY Enabled                                 READ xEnabled       WRITE Enable DEFAULT .T.
   PROPERTY Visible                                 READ xVisible       WRITE Show   DEFAULT .T.

   PROPERTY Wrap          INDEX TBSTATE_WRAP        READ xWrap          WRITE SetState DEFAULT .F.
   PROPERTY Separator     INDEX BTNS_SEP            READ xSeparator     WRITE SetStyle DEFAULT .F.

   ACCESS IsChecked      INLINE ::Parent:GetButtonInfo( ::Id, TBIF_STATE ):fsState & TBSTATE_CHECKED == TBSTATE_CHECKED

   DATA xPosition          EXPORTED
   ACCESS Position         INLINE ::xPosition PERSISTENT
   ASSIGN Position(n)      INLINE ::SetPosition(n)

   DATA __lResizeable          EXPORTED INIT {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
   DATA __lMoveable            EXPORTED INIT .F.
   DATA __lCopyCut             EXPORTED INIT .F.
   DATA __lCreateAfterChildren EXPORTED INIT .F.
   DATA __TempRect             EXPORTED
   DATA xTabOrder              EXPORTED

   DATA lCreated     PROTECTED INIT .F.

   METHOD Init()         CONSTRUCTOR
   METHOD Create()
   METHOD __Dummy()      INLINE Self
   METHOD Refresh()      INLINE Self
   METHOD Disable()      INLINE ::Enabled := .F.
   METHOD Enable()
   METHOD Show()
   METHOD CheckButton(l) INLINE ::Parent:CheckButton( ::oStruct:idCommand, IIF( l==NIL, .T., l ) )
   METHOD Press()
   METHOD Check()
   METHOD Release()
   METHOD GetRect()
   METHOD GetClientRect()
   METHOD GetWindowRect()
   METHOD GetRectangle() INLINE ::GetWindowRect()
   METHOD SetStyle()
   METHOD AddMenuItem()
   METHOD SetWidth()
   METHOD GetBitmap()    INLINE ::Parent:GetButtonBitmap( ::Id )
   METHOD Delete()
   METHOD SetText()
   METHOD MoveWindow()
   METHOD __OnParentSize()   INLINE 0
   METHOD OnParentMove()   INLINE 0
   METHOD InvalidateRect() INLINE 0
   METHOD IsWindowVisible() INLINE .T.
   METHOD Destroy()        INLINE ::Delete()
   METHOD OnClick()        VIRTUAL
   METHOD SetImageIndex()
   METHOD SetState()
   METHOD SetPosition()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS ToolButton
   DEFAULT ::ClsName TO "ToolButton"
   DEFAULT ::__xCtrlName TO "ToolButton"

   ::Children     := {}
   ::EventHandler := Hash()
   ::Parent       := IIF( oParent:ClsName == "OptionBar", oParent:__ToolBar, oParent )
   ::Style        := TBSTYLE_BUTTON

   IF oParent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
      ::__ClassInst:__IsInstance   := .T.
   ENDIF

   ::__CreateProperty()

   ::Id := ::Form:GetNextControlId()
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create( nPos ) CLASS ToolButton
   LOCAL tbbi, aRect, n := -1

   IF LEN( ::Parent:Children ) == 0 .AND. EMPTY( ::xCaption )
      ::xCaption := NIL
   ENDIF

   ::oBtnInfo := (struct TBBUTTONINFO)
   ::oStruct  := (struct TBBUTTON)

   DEFAULT nPos TO ::xPosition
   DEFAULT nPos TO -1
   IF ::Separator
      ::AutoSize  := .T.
      ::Style += TBSTYLE_AUTOSIZE
   ENDIF

   IF ::CheckGroup
      ::Style += TBSTYLE_CHECKGROUP
   ENDIF

   IF ::AutoSize .AND. ::Style != BTNS_SEP .AND. ::Style & TBSTYLE_AUTOSIZE == 0
      ::Style += TBSTYLE_AUTOSIZE
   ENDIF
   IF ::ShowText .AND. ::Style & BTNS_SHOWTEXT == 0
      ::Style += BTNS_SHOWTEXT
   ENDIF

   IF ::Parent:Parent:ClsName == "OptionBar"
      ::AutoSize   := .F.
      ::Width      := NIL
      ::State      := TBSTATE_ENABLED | TBSTATE_WRAP
      ::Parent:Parent:__oPage:RecalSize()
      ::Parent:PostMessage( TB_SETBUTTONWIDTH, 0, MAKELONG( ::Parent:Parent:ClientWidth, ::Parent:Parent:ClientWidth ) )
   ENDIF

   ::oStruct:ibitmap   := IIF( ::xImageIndex <= 0, -2, ::ImageIndex-1 )
   ::oStruct:idCommand := ::Id
   ::oStruct:fsState   := ::State
   ::oStruct:fsStyle   := ::Style

   IF ::xCaption != NIL .AND. !::Parent:__MenuBar .AND. !::Separator
      ::oStruct:iString := GlobalString( ::xCaption )
   ENDIF

   SendMessage( ::Parent:hWnd, TB_INSERTBUTTON, nPos, ::oStruct )

   DEFAULT ::xCaption TO ""

   ::oBtnInfo:cbSize    := ::oBtnInfo:sizeof()
   ::oBtnInfo:dwMask    := TBIF_COMMAND
   ::oBtnInfo:idCommand := ::Id

   IF ::xCaption != NIL
      ::oBtnInfo:dwMask := ::oBtnInfo:dwMask | TBIF_TEXT | TBIF_IMAGE
      ::oBtnInfo:pszText:= ::xCaption
      ::oBtnInfo:iImage := IIF( (::Separator .OR. ::ImageIndex <= 0) .AND. !::IsMenuItem, -2, ::ImageIndex-1 )
   ENDIF

   IF ::Width != NIL .AND. !::AutoSize
      ::oBtnInfo:dwMask := ::oBtnInfo:dwMask | TBIF_SIZE
      ::oBtnInfo:cx     := ::Width
   ENDIF

   ::hWnd := ::Id
   SendMessage( ::Parent:hWnd, TB_SETBUTTONINFO, ::Id, ::oBtnInfo )

   IF nPos < 0
      ::xPosition := LEN(::Parent:aItems)
      AADD( ::Parent:aItems, Self )
      AADD( ::Parent:aReset, Self )
      IF ::Parent:Parent:ClsName == "OptionBar"
         AADD( ::Parent:Parent:Children, Self )
       ELSE
         IF !::IsMenuItem
            AADD( ::Parent:Children, Self )
         ENDIF
      ENDIF
    ELSE
      AINS( ::Parent:aItems, nPos+1, Self, .T. )
      AINS( ::Parent:aReset, nPos+1, Self, .T. )

      IF ::Parent:Parent:ClsName == "OptionBar"
         AINS( ::Parent:Parent:Children, nPos+1, Self, .T. )
       ELSE
         IF !::IsMenuItem
            AINS( ::Parent:Children, nPos+1, Self, .T. )
         ENDIF
      ENDIF

      FOR n := 1 TO LEN( ::Parent:aItems )
          ::Parent:aReset[n]:xPosition := n-1
          ::Parent:aItems[n]:xPosition := n-1
          IF !::IsMenuItem
             TRY
                IF ::Parent:Parent:ClsName == "OptionBar"
                   ::Parent:Parent:Children[n]:xPosition := n-1
                 ELSE
                   ::Parent:Children[n]:xPosition := n-1
                ENDIF
             CATCH
             END
          ENDIF
      NEXT
      ::xPosition := nPos
   ENDIF

   aRect := ::GetWindowRect()
   tbbi := ::Parent:GetButtonInfo( ::Id, TBIF_SIZE )

   ::Left    := aRect[1]
   ::Top     := aRect[2]
   ::Width   := tbbi:cx
   ::Height  := aRect[4]

   IF !::xEnabled
      ::Disable()
   ENDIF
   IF !::xVisible
      ::Show( .F. )
   ENDIF
   ::lCreated := .T.

   IF ( ::xWholeDropDown .OR. ::xDropDown ) .AND. ::Menu == NIL
      IF ::ContextMenu == NIL
         ::Menu := MenuPopup( ::Parent )
       ELSE
         ::Menu := ::ContextMenu:Menu
      ENDIF
   ENDIF
   IF ::__ClassInst == NIL .AND. ::ShortCut != NIL
      ::Form:AddAccelerator( ::ShortCut[1], ::ShortCut[2], ::Id )
   ENDIF
   IF ::__ClassInst != NIL
      ::Application:ObjectTree:Set( Self )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetPosition(n) CLASS ToolButton
   (n)
//   LOCAL cName, x, oButton
//   IF ::lCreated
//      cName := ::xName
//      ::Delete()
//      ::Create(n)
//      ::Name := cName
//   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD MoveWindow() CLASS ToolButton
   ::SetWidth( ::Width )
RETuRN Self

//-----------------------------------------------------------------------------------------------
METHOD Enable( lEnable ) CLASS ToolButton
   IF ::oStruct != NIL //.AND. ::__ClassInst == NIL
      DEFAULT lEnable TO .T.
      ::Parent:SendMessage( TB_ENABLEBUTTON, ::oStruct:idCommand, lEnable )
   ENDIF
   ::xEnabled := lEnable
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Show( lShow ) CLASS ToolButton
   IF ::oStruct != NIL .AND. ::__ClassInst == NIL
      DEFAULT lShow TO .T.
      ::Parent:SendMessage( TB_HIDEBUTTON, ::oStruct:idCommand, !lShow )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetText( c ) CLASS ToolButton
   LOCAL aRect, tbbi := (struct TBBUTTONINFO)

   ::xCaption   := c

   IF ::lCreated
      tbbi:cbSize  := tbbi:sizeof()
      tbbi:dwMask  := TBIF_TEXT
      tbbi:pszText := c

      SendMessage( ::Parent:hWnd, TB_SETBUTTONINFO, ::Id, tbbi )

      //tbbi:dwMask  := TBIF_SIZE
      //SendMessage( ::Parent:hWnd, TB_GETBUTTONINFO, ::Id, @tbbi )

      aRect := ::GetClientRect()
      ::Left    := aRect[1]
      ::Top     := aRect[2]
      ::Width   := aRect[3]//tbbi:cx
      ::Height  := aRect[4]

   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetWidth( nWidth ) CLASS ToolButton
   LOCAL tbbi

   IF ::lCreated
      tbbi := (struct TBBUTTONINFO)
      tbbi:cbSize  := tbbi:sizeof()
      tbbi:dwMask  := TBIF_SIZE

      IF !::AutoSize
         tbbi:cx      := nWidth
         SendMessage( ::Parent:hWnd, TB_SETBUTTONINFO, ::Id, tbbi )
       ELSE
         SendMessage( ::Parent:hWnd, TB_GETBUTTONINFO, ::Id, @tbbi )
         ::xWidth := tbbi:cx
      ENDIF
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD Delete( lChildren, lDelProp ) CLASS ToolButton
   LOCAL aRect, tbbi, n := -1
   DEFAULT lDelProp  TO .T.
   DEFAULT lChildren TO .T.
   aEval( ::Children, {|o| IIF( o == NIL, , o:Delete() ) } )
   SendMessage( ::Parent:hWnd, TB_DELETEBUTTON, ::xPosition, 0 )
   ADEL( ::Parent:Children, ::xPosition+1, .T. )
   ADEL( ::Parent:aItems, ::xPosition+1, .T. )
   ADEL( ::Parent:aReset, ::xPosition+1, .T. )
   IF ::Parent:Parent:ClsName == "OptionBar"
      ADEL( ::Parent:Parent:Children, ::xPosition+1, .T. )
   ENDIF
   FOR n := 1 TO LEN( ::Parent:aItems )
      ::Parent:aItems[n]:xPosition := n-1
      ::Parent:aReset[n]:xPosition := n-1
      IF !::IsMenuItem
         IF ::Parent:Parent:ClsName == "OptionBar"
            ::Parent:Parent:Children[n]:xPosition := n-1
          ELSEIF ::Parent:Children != NIL
            TRY
               ::Parent:Children[n]:xPosition := n-1
            CATCH
            END
         ENDIF
      ENDIF
      aRect := ::Parent:aItems[n]:GetWindowRect()
      tbbi := ::Parent:GetButtonInfo( ::Parent:aItems[n]:Id, TBIF_SIZE )

      ::Parent:aItems[n]:Left    := aRect[1]
      ::Parent:aItems[n]:Top     := aRect[2]
      ::Parent:aItems[n]:Width   := tbbi:cx
      ::Parent:aItems[n]:Height  := aRect[4]
   NEXT
   IF lDelProp .AND. ::Name != NIL
      TRY
         HDel( ::Form:Property, ::Name )
      CATCH
      END
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD Press() CLASS ToolButton

   LOCAL tbbi := (struct TBBUTTONINFO)

   tbbi:cbSize  := tbbi:sizeof()
   tbbi:dwMask  := TBIF_STATE
   tbbi:fsState := TBSTATE_ENABLED + TBSTATE_PRESSED

   SendMessage( ::Parent:hWnd, TB_SETBUTTONINFO, ::Id, tbbi )

RETURN SELF

METHOD Check( lCheck ) CLASS ToolButton
   LOCAL tbbi := (struct TBBUTTONINFO)
   tbbi:cbSize  := tbbi:sizeof()
   tbbi:dwMask  := TBIF_STATE
   tbbi:fsState := TBSTATE_ENABLED
   IF lCheck
      tbbi:fsState += TBSTATE_CHECKED
   ENDIF
   SendMessage( ::Parent:hWnd, TB_SETBUTTONINFO, ::Id, tbbi )
RETURN SELF

METHOD SetState( nState, lSet ) CLASS ToolButton
   LOCAL tbbi
   IF lSet
      ::State := ::State + nState
    ELSE
      ::State := ::State - nState
   ENDIF
   IF ::Parent:hWnd != NIL
      tbbi := (struct TBBUTTONINFO)
      tbbi:cbSize  := tbbi:sizeof()
      tbbi:dwMask  := TBIF_STATE
      tbbi:fsState := ::State
      SendMessage( ::Parent:hWnd, TB_SETBUTTONINFO, ::Id, tbbi )
   ENDIF
RETURN SELF


//-----------------------------------------------------------------------------------------------

METHOD Release() CLASS ToolButton

   LOCAL tbbi := (struct TBBUTTONINFO)

   tbbi:cbSize  := tbbi:sizeof()
   tbbi:dwMask  := TBIF_STATE
   tbbi:fsState := TBSTATE_ENABLED

   SendMessage( ::Parent:hWnd, TB_SETBUTTONINFO, ::Id, tbbi )

RETURN SELF


//-----------------------------------------------------------------------------------------------
METHOD GetClientRect() CLASS ToolButton
   LOCAL rc := ::GetRect()
RETURN { rc:Left, rc:Top, rc:Right-rc:Left, rc:Bottom-rc:Top }

METHOD GetWindowRect() CLASS ToolButton
   LOCAL rc := ::GetRect()
RETURN { rc:Left, rc:Top, rc:Right-rc:Left, rc:Bottom-rc:Top }

METHOD GetRect() CLASS ToolButton
   LOCAL rc := (struct RECT)
   SendMessage( ::Parent:hWnd, TB_GETITEMRECT, ::xPosition, @rc )
RETURN rc

//-----------------------------------------------------------------------------------------------
METHOD SetImageIndex( nInd ) CLASS ToolButton
   LOCAL tbbi
   IF ::lCreated
      tbbi := (struct TBBUTTONINFO)
      tbbi:cbSize  := tbbi:sizeof()
      tbbi:dwMask  := TBIF_IMAGE
      tbbi:iImage  := nInd-1
      SendMessage( ::Parent:hWnd, TB_SETBUTTONINFO, ::Id, tbbi )
   ENDIF
RETURN self

//-----------------------------------------------------------------------------------------------
METHOD SetStyle( nStyle, lAdd ) CLASS ToolButton
   LOCAL tbbi, n

   DEFAULT lAdd TO .T.

   IF lAdd
      ::Style := ::Style | nStyle
      IF nStyle == BTNS_SEP
         ::Caption := ""
         IF ::lCreated .AND. !::__ClassInst != NIL
            n := ::xPosition
            ::Delete()
            ::Create()
            RETURN Self
         ENDIF
      ENDIF
    ELSE
      ::Style := ::Style & NOT( nStyle )
   ENDIF

   IF ::lCreated
      tbbi := (struct TBBUTTONINFO)
      tbbi:cbSize  := tbbi:sizeof()
      tbbi:dwMask  := TBIF_STYLE

      IF ::Separator
         ::Style := ::Style | TBSTYLE_AUTOSIZE
         ::xAutoSize  := .T.
      ENDIF
      tbbi:fsStyle := ::Style

      SendMessage( ::Parent:hWnd, TB_SETBUTTONINFO, ::Id, tbbi )

      tbbi:dwMask  := TBIF_SIZE
      SendMessage( ::Parent:hWnd, TB_GETBUTTONINFO, ::Id, @tbbi )

      ::xWidth := tbbi:cx
   ENDIF

RETURN self


//-----------------------------------------------------------------------------------------------

METHOD AddMenuItem( cCaption, nId, bnAction, nImgInd, cAccel ) CLASS ToolButton

   LOCAL oItem := CMenuItem( Self )
   oItem:Id        := nId
   oItem:Caption   := cCaption
   oItem:ImageIndex:= nImgInd
   oItem:Action    := bnAction
   oItem:ShortCutText := cAccel

RETURN oItem
//-----------------------------------------------------------------------------------------------

FUNCTION __SysVxhMenuHook( nCode, nwParam, nlParam )

   LOCAL oBar, n,  o, nMenu, Item, pt, ms, nPushPos, hSubMenu, oApp := __GetApplication()
   static hMenu
   oBar := oApp:__CurCoolMenu
   IF oBar == NIL
      RETURN CallNextHookEx( oApp:__hMenuHook, nCode, nwParam, nlParam)
   ENDIF
   ms := (struct MSG*) nlParam
   pt := (struct POINT)
   DO CASE
      CASE ms:message == WM_MENUSELECT
      
           IF oBar:Chevron == NIL
              oBar:SelPopup:= HiWord( ms:wParam ) & MF_POPUP != 0

              hMenu     := ms:lParam
              //nMenu := oBar:nPressed+1
              
              nMenu := SendMessage( oBar:hWnd, TB_GETHOTITEM, 0, 0 )+1
              IF nMenu > 0
                 s_nMenu := nMenu
              ENDIF

              hSubMenu := GetSubMenu( ms:lParam, LoWord( ms:wParam ) )
              s_oItem := NIL
              
              IF s_nMenu != NIL .AND. s_nMenu > 0
                 IF oBar:aItems[s_nMenu]:Menu != NIL
                    IF oBar:SelPopup
                       FOR EACH Item IN oBar:aItems[s_nMenu]:Menu:aItems
                           IF hSubMenu == Item:hMenu
                              o := Item
                              EXIT
                           ENDIF
                           IF ( o := Item:GetMenuByHandle( hSubMenu ) ) != NIL
                              EXIT
                           ENDIF
                       NEXT
                     ELSE
                       o := oBar:aItems[s_nMenu]:Menu:GetItem( LoWord( ms:wParam ) )
                    ENDIF
                    IF o!= NIL
                       s_oItem := o
                    ENDIF
                    IF o!= NIL .AND. o:Message != NIL .AND. oBar:Parent:StatusBar != NIL
                       oBar:Parent:StatusBar:SendMessage( SB_SIMPLE, .T., 0 )
                       oBar:Parent:StatusBar:SendMessage( SB_SETTEXT, SB_SIMPLEID + SBT_NOBORDERS, " "+o:Message )
                     ELSEIF oBar:Parent:StatusBar != NIL
                       oBar:Parent:StatusBar:SendMessage( SB_SIMPLE, .F., 0 )
                    ENDIF
                 ENDIF
               ELSEIF oBar:Parent:StatusBar != NIL
                 oBar:Parent:StatusBar:SendMessage( SB_SIMPLE, .F., 0 )
              ENDIF
              IF oBar:__ClassInst != NIL .AND. s_oItem != NIL .AND. s_oItem:Caption != "[ Add New Item ]"
                 oApp:Project:CurrentForm:SelectControl( s_oItem, .F. )
                 oApp:ObjectTree:Set( s_oItem )
              ENDIF
            ELSE
              IF oBar:Parent:StatusBar != NIL
                 n := ASCAN( oBar:Chevron:aItems, {|o|o:Id == LoWord( ms:wParam ) } )
                 IF n > 0 .AND. oBar:Chevron:aItems[n]:Message != NIL
                    oBar:Parent:StatusBar:SendMessage( SB_SIMPLE, .T., 0 )
                    oBar:Parent:StatusBar:SendMessage( SB_SETTEXT, SB_SIMPLEID + SBT_NOBORDERS, " "+oBar:Chevron:aItems[n]:Message )
                  ELSE
                    oBar:Parent:StatusBar:SendMessage( SB_SIMPLE, .F., 0 )
                 ENDIF
              ENDIF
           ENDIF

      CASE ms:message == WM_LBUTTONDOWN
           pt:x := LoWord(ms:lparam)
           pt:y := HiWord(ms:lparam)
           IF oBar:Chevron == NIL
              oBar:lKeyboard := .F.
              IF oBar:hWndHook != NIL .AND. MenuItemFromPoint( oBar:hWndHook, oBar:MenuWnd, pt ) < 0 .AND. MenuItemFromPoint( oBar:hWndHook, hMenu, pt ) < 0
                 oBar:nPressed := -1
                 SendMessage( oBar:hWndHook, WM_CANCELMODE, 0, 0 )
                 SendMessage( oBar:hWnd, TB_SETHOTITEM, oBar:nPressed, 0 )
                 oApp:__CurCoolMenu := NIL
              ENDIF
            ELSE
              IF ( nPushPos := MenuItemFromPoint( oBar:hWndHook, oBar:Chevron:hMenu, pt ) )>= 0
                  oBar:PushPos := { LoWord(ms:lparam), HiWord(ms:lparam) }
                  oBar:Chevron:aItems[nPushPos+1]:SetText( oBar:Chevron:aItems[nPushPos+1]:Caption )
              ENDIF
              IF nPushPos < 0
                 oBar:nPressed := -1
                 oApp:__CurCoolMenu := NIL
                 SendMessage( oBar:hWnd, WM_CANCELMODE, 0, 0 )
                 SendMessage( oBar:hWnd, TB_SETHOTITEM, oBar:nPressed, 0 )
                 RETURN 1
              ENDIF
           ENDIF

      CASE ms:message == WM_LBUTTONUP
           IF oBar:Chevron == NIL
              IF oBar:__ClassInst != NIL /*.AND. oBar:nPressed >= 0*/ .AND. s_oItem != NIL
                 IF s_oItem:Caption == "[ Add New Item ]"
                    oBar:__AddNewItemMenuItem( s_oItem:Parent )
                    RETURN 1
                 ENDIF
              ENDIF
              oBar:nPressed := -1
            ELSE
              pt:x := LoWord(ms:lparam)
              pt:y := HiWord(ms:lparam)
              IF MenuItemFromPoint( ms:hWnd, oBar:Chevron:hMenu, pt ) < 0
                 RETURN 1
              ENDIF
              oBar:nPressed := -1
           ENDIF

      CASE ms:message == WM_MOUSEMOVE
           IF oBar:Chevron == NIL
              IF oBar:__ClassInst == NIL .OR. ms:wParam & MK_LBUTTON == MK_LBUTTON
                 oBar:lKeyboard := .F.
                 pt:x := LoWord(ms:lparam)
                 pt:y := HiWord(ms:lparam)
                 ScreenToClient( ms:hwnd, @pt )

                 IF pt:x == oBar:pRect:x .AND. pt:y == oBar:pRect:y
                    RETURN 1
                 ENDIF
                 oBar:pRect := pt
                 n := SendMessage( ms:hwnd, TB_HITTEST, 0, pt)
                 IF n!=oBar:nPressed .AND. n >= 0 .AND. n < SendMessage( oBar:hWndHook, TB_BUTTONCOUNT )
                    IF !( oBar:oMenu == oBar:aItems[n+1]:Menu )
                       IF oBar:aItems[n+1]:Id < IDM_MDI_GAP .AND. oBar:__ClassInst == NIL
                          SendMessage( oBar:hWndHook, WM_CANCELMODE, 0, 0 )
                          oBar:nPressed := n
                          PostMessage( oBar:hWndHook, MBM_POPUPMENU, pt:x, pt:y )
                          RETURN 1
                       ENDIF
                    ENDIF
      //            ELSEIF oBar:nPressed >= 0
      //              oBar:nPressed := -1
      //              oApp:__CurCoolMenu := NIL
                 ENDIF
              ENDIF
           ENDIF

      CASE ms:message == WM_KEYDOWN
           IF oBar:Chevron == NIL .AND. oBar:__MenuBar
              oBar:lKeyboard := .T.
              IF VALTYPE( oBar:__ClassInst ) == "O" .AND. oBar:__MenuBar
                 DO CASE
                    CASE ms:wParam == VK_RETURN
                         IF oBar:hWndHook != NIL .AND. s_oItem != NIL
                            oBar:nPressed := -1
                            IF s_oItem:Caption != "[ Add New Item ]"
                               oApp:Project:CurrentForm:SelectControl( s_oItem, .F. )
                             ELSE
                               oBar:__AddNewItemMenuItem( s_oItem:Parent )
                            ENDIF
                            RETURN 1
                         ENDIF

                 ENDCASE
              ENDIF
              DO CASE
                 CASE ms:wParam == VK_LEFT
                      IF oBar:nPressed < 0 .OR. ( s_oItem != NIL .AND. s_oItem:Parent:__xCtrlName == "CMenuItem" ) .OR. oBar:__ClassInst != NIL
                         RETURN 0
                      ENDIF
                      oBar:nPressed--
                      IF oBar:hWndHook != NIL
                         SendMessage( oBar:hWndHook, TB_SETHOTITEM, -1, 0 )
                         SendMessage( oBar:hWndHook, WM_CANCELMODE, 0, 0 )
                         PostMessage( oBar:hWndHook, MBM_POPUPMENU)
                       ELSE
                         IF oBar:nPressed < 0
                            n := ASCAN( oBar:aItems, {|o| o:Id == IDM_MDI_GAP } )
                            oBar:nPressed := IIF( n > 0, n-1, LEN( oBar:aItems )-1 )
                         ENDIF
                         SendMessage( oBar:hWnd, TB_SETHOTITEM, oBar:nPressed, 0 )
                      ENDIF
                      RETURN 1

                 CASE ms:wParam == VK_RIGHT
                      IF oBar:nPressed < 0 .OR. oBar:SelPopup .OR. oBar:__ClassInst != NIL
                         RETURN 0
                      ENDIF
                      IF oBar:hWndHook != NIL
                         SendMessage( oBar:hWndHook, TB_SETHOTITEM, -1, 0 )
                         SendMessage( oBar:hWndHook, WM_CANCELMODE, 0, 0 )
                         oBar:nPressed++
                         IF oBar:nPressed >= 0 .AND. oBar:nPressed+1 <= LEN( oBar:aItems ) .AND. oBar:aItems[oBar:nPressed+1]:Id == IDM_MDI_GAP
                            oBar:nPressed := 0
                         ENDIF
                         PostMessage( oBar:hWndHook, MBM_POPUPMENU)
                       ELSE
                         oBar:nPressed++
                         IF oBar:nPressed > LEN( oBar:aItems )-1 .OR. oBar:aItems[oBar:nPressed+1]:Id == IDM_MDI_GAP
                            oBar:nPressed := 0
                         ENDIF
                         SendMessage( oBar:hWnd, TB_SETHOTITEM, oBar:nPressed, 0 )
                      ENDIF
                      RETURN 1

                 CASE ms:wParam == VK_DOWN
                      IF oBar:nPressed >= 0 .AND. oBar:hWndHook == NIL
                         PostMessage( oBar:hWnd, MBM_POPUPMENU)
                         RETURN 1
                      ENDIF

                 CASE ms:wParam == VK_ESCAPE
                      IF oBar:nPressed < 0
                         RETURN 0
                      ENDIF
                      IF oBar:hWndHook != NIL
                         SendMessage( oBar:hWndHook, WM_CANCELMODE, 0, 0 )
                         oBar:SelPopup := .F.
                       ELSE
                         oBar:nPressed := -1
                         oApp:__CurCoolMenu := NIL
                      ENDIF
                      SendMessage( oBar:hWnd, TB_SETHOTITEM, oBar:nPressed, 0 )
                      RETURN 1

              ENDCASE
           ENDIF
   ENDCASE

RETURN CallNextHookEx( oApp:__hMenuHook, nCode, nwParam, nlParam)
