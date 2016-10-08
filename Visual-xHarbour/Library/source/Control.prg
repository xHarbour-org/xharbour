/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Control.prg                                                                                          *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

#xcommand ODEFAULT <v> TO <x> [, <vN> TO <xN>]         ;
       => IF <v> == nil .OR. VALTYPE( <v> ) == "O"; <v> := <x> ; END            ;
          [; IF <vN> == nil ; <vN> := <xN> ; END]

//-----------------------------------------------------------------------------------------------

CLASS Control INHERIT Window

   // Object Manager properties ----------------------------------------------------------------------------------------------------------------------------
   PROPERTY Dock           ROOT "Position"
   PROPERTY Anchor         ROOT "Position"
   PROPERTY Transparent    ROOT "Appearance" DEFAULT .F.
   PROPERTY ContextMenu    ROOT "Behavior"   GET __ChkComponent( Self, @::xContextMenu )
   PROPERTY TabStop        ROOT "Behavior"   SET ::SetStyle( WS_TABSTOP, v )          DEFAULT .T. PROTECTED
   PROPERTY Enabled        ROOT "Behavior"   SET ::__Enable(v)                        DEFAULT .T.
   PROPERTY Text           ROOT "Appearance" GET IIF( ! ::IsWindow() .OR. ::__IsInstance, ::xText, _GetWindowText( ::hWnd ) ) SET ::SetWindowText( v ) DEFAULT ""
   PROPERTY AllowMaximize  ROOT "Behavior"   DEFAULT .F.
   PROPERTY BorderColor    ROOT "Colors" DEFAULT 0
   PROPERTY Border         ROOT "Appearance" SET ::__SetBorder(v)      DEFAULT 0

   PROPERTY When           ROOT "Behavior"

   DATA EnumBorder       EXPORTED INIT { { "None", "Single", "Sunken", "Fixed3D" }, { 0, WS_BORDER, WS_EX_STATICEDGE, WS_EX_CLIENTEDGE } }


   // Compatibility
   ACCESS StaticEdge    INLINE ::Border == WS_EX_STATICEDGE
   ASSIGN StaticEdge(l) INLINE ::Border := IIF(l,WS_EX_STATICEDGE,0)

   ACCESS ClientEdge    INLINE ::Border == WS_EX_CLIENTEDGE
   ASSIGN ClientEdge(l) INLINE ::Border := IIF(l,WS_EX_CLIENTEDGE,0)

   ACCESS xCaption       INLINE ::xText
   ASSIGN xCaption(c)    INLINE ::xText := c

   ACCESS Caption        INLINE ::Text
   ASSIGN Caption(c)     INLINE ::Text := c

   DATA ImageList         EXPORTED
   DATA IsContainer       EXPORTED INIT .F.
   DATA Value             EXPORTED
   DATA Options           EXPORTED
   DATA Data              EXPORTED
   DATA Validating        EXPORTED INIT FALSE
   DATA IsValid           EXPORTED INIT TRUE
   DATA EmptyLeft         EXPORTED INIT 0
   DATA ToolBarPos        EXPORTED INIT 1
   DATA ShowMode          EXPORTED INIT 1
   DATA Modal             EXPORTED INIT .F.
   DATA __IdeImageIndex   EXPORTED INIT 3
   DATA __BackMargin      EXPORTED INIT 0
   DATA OnWMUnDock        EXPORTED
   DATA OnWMReDock        EXPORTED
   DATA bSetValue         EXPORTED
   DATA bGetValue         EXPORTED

   DATA SmallCaption      EXPORTED INIT .T.
   DATA bOnSetValue       EXPORTED

   DATA __hParBrush       PROTECTED
   DATA BackInfo          PROTECTED
   DATA __DockParent      PROTECTED

   ACCESS Child           INLINE (::Style & WS_CHILD) != 0
   ACCESS ControlParent   INLINE (::ExStyle & WS_EX_CONTROLPARENT) != 0
   ACCESS MdiContainer    INLINE ::xMdiContainer
   ASSIGN MdiContainer(l) INLINE ::xMdiContainer := l
   ACCESS DesignMode      INLINE IIF( ::Parent != NIL, ::Parent:DesignMode, .F. )
   ACCESS Right           INLINE ::Left + ::Width
   ACCESS IsDocked        INLINE ::__Docked

   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD Disable()             INLINE ::Enabled := .F.
   METHOD Enable()              INLINE ::Enabled := .T.
   METHOD OnSize()
   METHOD OnMove()
   METHOD OnSysKeyDown()
   METHOD OnEnterSizeMove()
   METHOD OnExitSizeMove()

   METHOD Redraw( aRect ) INLINE ::RedrawWindow( aRect, , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT | RDW_ALLCHILDREN) ),::UpdateWindow()
   METHOD __Enable( lEnable )
   METHOD GetBkBrush()
   METHOD OnDestroy()          INLINE IIF( ::__hParBrush != NIL, DeleteObject( ::__hParBrush ),), Super:OnDestroy()
   METHOD OnNCPaint()
   METHOD DrawArrow()
   METHOD __SetBorder()
   METHOD Show()
   METHOD Hide()
   METHOD Center()
ENDCLASS

METHOD __SetBorder( nBorder ) CLASS Control
   LOCAL nExStyle, nStyle
   IF VALTYPE( nBorder ) == "L" // backward compatibility
      nBorder := IIF( nBorder, WS_BORDER, 0 )
   ENDIF
   IF VALTYPE( nBorder ) == "L"
      nBorder := IIF( ! nBorder, 0, WS_BORDER )
   ENDIF
   nStyle   := (::Style & NOT( WS_BORDER ))
   nExStyle := (::ExStyle & NOT( WS_EX_STATICEDGE ))
   nExStyle := (nExStyle & NOT( WS_EX_CLIENTEDGE ))

   IF nBorder <> 0
      IF nBorder == WS_BORDER
         nStyle := (nStyle | WS_BORDER)
      ELSE
         nExStyle := (nExStyle | nBorder)
      ENDIF
   ENDIF
   ::Style := nStyle
   ::ExStyle := nExStyle

   ::SetWindowLong( GWL_STYLE, ::Style )
   ::SetWindowLong( GWL_EXSTYLE, ::ExStyle )
   IF ::IsWindowVisible()
      ::SetWindowPos(,0,0,0,0,(SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER))
      ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW ))
   ENDIF
RETURN nBorder

METHOD __Enable( lEnable ) CLASS Control
   IF ::hWnd != NIL .AND. ::__xCtrlName != "Button"
      EnableWindow( ::hWnd, lEnable )
      ::InvalidateRect( , .F. )
      ::UpdateWindow()
   ENDIF
RETURN lEnable

//---------------------------------------------------------------------------------------------------

METHOD Init( oParent, lInitValues ) CLASS Control
   ::__IsControl  := .T.
   ::__IsStandard := .T.
   ::Super:Init( oParent, lInitValues )
   ::Id := ::Form:ControlId++
   oParent := NIL
   __SetInitialValues( Self, "Border", ::xBorder )
RETURN Self

//---------------------------------------------------------------------------------------------------

METHOD Create( hParent ) CLASS Control
   LOCAL pt, rc, bEnabled, cWhen

   ::__SetBorder( ::xBorder )

   IF ::IsContainer
      ::__IdeImageIndex := 1
   ENDIF

   IF ::Parent:__xCtrlName == "ToolBar" .AND. ::Parent:IsWindow()
      ::Parent:AddControl( Self, ::ToolBarPos )
   ENDIF

   IF ::Parent:__oDlg != NIL
      pt := (struct POINT)
      pt:x := ::xLeft
      pt:y := ::xTop

      rc := (struct RECT)
      rc:left   := ::Parent:__oDlg:Left
      rc:top    := ::Parent:__oDlg:Top
      rc:right  := ::Parent:__oDlg:Width
      rc:bottom := ::Parent:__oDlg:Top + ::Parent:__oDlg:Height

      IF PtInRect( rc, pt )
         hParent := ::Parent:__oDlg:hWnd
         ::xTop  -= ::Parent:VertScrollTopMargin
         ::Parent:__oDlg:OriginalRect[4] := Max( ::xTop + ::xHeight + ( (GetSystemMetrics( SM_CYFRAME )*2) + GetSystemMetrics( SM_CYSMCAPTION ) ), ::Parent:__oDlg:OriginalRect[4] )
         ::Parent:__oDlg:OriginalRect[3] := Max( ::xLeft + ::xWidth + ( GetSystemMetrics( SM_CXFRAME )*2), ::Parent:__oDlg:OriginalRect[3] )
      ENDIF
   ENDIF
   ::Super:Create( hParent )

   IF ! Empty(::When) .AND. ! ::DesignMode
      cWhen    := ::When
      bEnabled := &("{|Self|" + cWhen + "}")
      EnableWindow( ::hWnd, Eval( bEnabled, Self ) )
   ENDIF

   IF ::__OnInitCanceled
      RETURN NIL
   ENDIF

   IF ::DesignMode
      __SetInitialValues( Self, "Id" )
      __DeleteEvents( ::Events, { "OnLoad",;
                                  "OnChangeCbChain",;
                                  "OnDrawClipboard",;
                                  "OnCancel",;
                                  "OnCommand",;
                                  "OnOk",;
                                  "OnSysCommand",;
                                  "OnToolTipNotify",;
                                  "OnCtlColorDlg",;
                                  "OnDrawItem",;
                                  "OnChildChar",;
                                  "OnChildGetDlgCode",;
                                  "OnChildKeyDown",;
                                  "OnHotKey",;
                                  "OnGetMinMaxInfo",;
                                  "OnMeasureItem"  ,;
                                  "OnCancelMode",;
                                  "OnEnterMenuLoop",;
                                  "OnExitMenuLoop",;
                                  "OnInitMenuPopup",;
                                  "OnMenuChar",;
                                  "OnMenuCommand",;
                                  "OnMenuSelect",;
                                  "OnNextMenu",;
                                  "OnTimer",;
                                  "OnInitDialog" } )
   ENDIF
   IF ::Parent:ClsName == "StatusBarPanel"
      ::Parent:Parent:SetPanels()
      ::__lResizeable := {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
      ::__lMoveable   := .F.
   ENDIF

   IF ::Parent:ClsName == "ToolStrip"
      ::Parent:__UpdateWidth()
   ENDIF
   IF ::__xCtrlName != "Button"
      IF !::xEnabled
         EnableWindow( ::hWnd, .F.  )
      ENDIF
   ENDIF

RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Center() CLASS Control
   LOCAL aRect
   IF ::Parent == NIL .OR. ::hWnd == NIL .OR. ::DesignMode
      RETURN Self
   ENDIF
   ::GetWindowRect()

   aRect := _GetClientRect( ::Parent:hWnd )
   ::xLeft := ( ( aRect[3] - ::xWidth ) / 2 )
   ::xTop  := ( ( aRect[4] - ::xHeight ) / 2 )
   ::MoveWindow()
RETURN Self

//------------------------------------------------------------------------------------------------------
METHOD Hide() CLASS Control
   IF ::__DockParent != NIL
      RETURN ::__DockParent:Hide()
   ENDIF
   IF ::hWnd != NIL
      ShowWindow( ::hWnd, SW_HIDE )
      ::UpdateWindow()
   ENDIF
   IF ::LeftSplitter != NIL
      ::LeftSplitter:Hide()
   ENDIF
   IF ::TopSplitter != NIL
      ::TopSplitter:Hide()
   ENDIF
   IF ::RightSplitter != NIL
      ::RightSplitter:Hide()
   ENDIF
   IF ::BottomSplitter != NIL
      ::BottomSplitter:Hide()
   ENDIF
   ::Style := (::Style & NOT( WS_VISIBLE ))
   IF ::hWnd != NIL
      ::Parent:SendMessage( WM_SIZE, 0, MAKELONG( ::Parent:ClientWidth, ::Parent:ClientHeight ) )
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------
METHOD Show( nShow ) CLASS Control
   DEFAULT nShow TO SW_SHOW
   IF ::__DockParent != NIL
      RETURN ::__DockParent:Show( nShow )
   ENDIF

   IF nShow == SW_HIDE
      RETURN ::Hide()
   ENDIF

   IF ::hWnd != NIL
      ShowWindow( ::hWnd, IIF( ! ::DesignMode, nShow, SW_SHOW ) )
      ::UpdateWindow()
   ENDIF
   IF ::LeftSplitter != NIL
      ::LeftSplitter:Show()
   ENDIF
   IF ::TopSplitter != NIL
      ::TopSplitter:Show()
   ENDIF
   IF ::RightSplitter != NIL
      ::RightSplitter:Show()
   ENDIF
   IF ::BottomSplitter != NIL
      ::BottomSplitter:Show()
   ENDIF
   ::Style := (::Style | WS_VISIBLE)
   ::Parent:SendMessage( WM_SIZE, 0, MAKELONG( ::Parent:ClientWidth, ::Parent:ClientHeight ) )
RETURN Self

//---------------------------------------------------------------------------------------------------

METHOD DrawArrow( hDC, aRect ) CLASS Control
   LOCAL nShadow, nColor, hPenShadow, hPenLight, hOldPen, z, i, n, x, y, nArrow := 1, nH := 5, nBackColor

   nBackColor := GetSysColor( COLOR_ACTIVECAPTION )

   nColor  := LightenColor( nBackColor, 100 )
   nShadow := DarkenColor( nBackColor, 100 )

   hPenShadow := CreatePen( PS_SOLID, 0, nShadow )
   hPenLight  := CreatePen( PS_SOLID, 0, nColor )

   hOldPen := SelectObject( hDC, hPenLight )
   z := 1
   FOR i := 1 TO 2
       FOR n := 1 TO nH
           x := IIF( nArrow == 1,n,nH-n+1)
           y := (aRect[4]-nH)/2

           MoveTo( hDC, aRect[3] - (15-x), y+n+z )
           LineTo( hDC, aRect[3] - ( 4+x), y+n+z )
       NEXT
       SelectObject( hDC, hPenShadow )
       z := 0
       aRect[3]--
   NEXT
   SelectObject( hDC, hOldPen )
   DeleteObject( hPenShadow )
   DeleteObject( hPenLight )
RETURN Self

//---------------------------------------------------------------------------------------------------
METHOD OnSize( nwParam, nlParam ) CLASS Control
   LOCAL x, y
   IF ::Super:OnSize( nwParam, nlParam ) == NIL
      x := LOWORD( nlParam )
      y := HIWORD( nlParam )
      ::ClientWidth  := x
      ::ClientHeight := y

      IF ::LeftSplitter != NIL
         ::LeftSplitter:__OnParentSize( x, y )
      ENDIF
      IF ::TopSplitter != NIL
         ::TopSplitter:__OnParentSize( x, y )
      ENDIF
      IF ::RightSplitter != NIL
         ::RightSplitter:__OnParentSize( x, y )
      ENDIF
      IF ::BottomSplitter != NIL
         ::BottomSplitter:__OnParentSize( x, y )
      ENDIF

   ENDIF
   x := NIL
   y := NIL
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnEnterSizeMove() CLASS Control
   IF ::LeftSplitter != NIL
      ::LeftSplitter:Visible := .F.
   ENDIF
   IF ::TopSplitter != NIL
      ::TopSplitter:Visible := .F.
   ENDIF
   IF ::RightSplitter != NIL
      ::RightSplitter:Visible := .F.
   ENDIF
   IF ::BottomSplitter != NIL
      ::BottomSplitter:Visible := .F.
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnExitSizeMove() CLASS Control
   IF ::LeftSplitter != NIL
      ::LeftSplitter:Visible := .T.
   ENDIF
   IF ::TopSplitter != NIL
      ::TopSplitter:Visible := .T.
   ENDIF
   IF ::RightSplitter != NIL
      ::RightSplitter:Visible := .T.
   ENDIF
   IF ::BottomSplitter != NIL
      ::BottomSplitter:Visible := .T.
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnMove( x, y ) CLASS Control
   IF ::Super:OnMove( x, y ) == NIL
      IF ::LeftSplitter != NIL
         ::LeftSplitter:OnParentMove( x, y )
      ENDIF
      IF ::TopSplitter != NIL
         ::TopSplitter:OnParentMove( x, y )
      ENDIF
      IF ::RightSplitter != NIL
         ::RightSplitter:OnParentMove( x, y )
      ENDIF
      IF ::BottomSplitter != NIL
         ::BottomSplitter:OnParentMove( x, y )
      ENDIF
   ENDIF
   IF ::DesignMode
      ::InvalidateRect()
      RETURN 0
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------
METHOD OnSysKeyDown( nwParam, nlParam ) CLASS Control
   LOCAL oCtrl, n
   IF nwParam != 18 .AND. LoWord( nlParam )== MOD_ALT .AND. ::Super:OnSysKeyDown( nwParam, nlParam ) == NIL
      FOR EACH oCtrl IN ::Parent:Children
          IF VALTYPE( oCtrl:Caption ) == "C" .AND. AT( "&"+UPPER( CHR( nwParam ) ), UPPER( oCtrl:Caption ) ) > 0
             RETURN NIL
          ENDIF
      NEXT
      IF ::Parent:Parent != NIL .AND. ::Parent:Parent:ClsName == "SysTabControl32"
         ::Parent:Parent:PostMessage( WM_SYSKEYDOWN, nwParam, nlParam )
       ELSE
         IF ( n:=ASCAN( ::Parent:Children, {|o|o:ClsName == "SysTabControl32" } ) ) > 0
            IF ! (::Parent:Children[n] == Self)
               ::Parent:Children[n]:PostMessage( WM_SYSKEYDOWN, nwParam, nlParam )
            ENDIF
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

METHOD GetBkBrush() CLASS Control
   LOCAL hBkGnd
   DEFAULT hBkGnd TO ::BkBrush
   DEFAULT hBkGnd TO ::Parent:BkBrush
   DEFAULT hBkGnd TO GetSysColorBrush( COLOR_BTNFACE )
RETURN hBkGnd

METHOD OnNCPaint() CLASS Control
   LOCAL hOldBrush, hPen, hDC
   ::CallWindowProc()
   IF VALTYPE( ::Border ) == "L"
      ::Border := IIF( ::Border, WS_BORDER, 0 )
   ENDIF
   IF ::BorderColor <> 0 .AND. ::Border <> 0
      hDC       := GetWindowDC( ::hWnd )
      hOldBrush := SelectObject( hDC, GetStockObject( NULL_BRUSH ) )
      hPen      := SelectObject( hDC, CreatePen( PS_SOLID, 0, ::BorderColor ) )

      Rectangle( hDC, 0, 0, ::Width, ::Height )

      DeleteObject( SelectObject( hDC, hPen ) )
      SelectObject( hDC, hOldBrush )
      ReleaseDC( ::hWnd, hDC )
   ENDIF
RETURN 0

//---------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------

CLASS CommonControls INHERIT Control
   PROPERTY CCS_Adjustable    SET ::SetProperty( CCS_ADJUSTABLE, v )    DEFAULT .F.
   PROPERTY CCS_NoDevider     SET ::SetProperty( CCS_NODIVIDER, v )     DEFAULT .F.
   PROPERTY CCS_Left          SET ::SetProperty( CCS_LEFT, v )          DEFAULT .F.
   PROPERTY CCS_Top           SET ::SetProperty( CCS_TOP, v )           DEFAULT .F.
   PROPERTY CCS_Right         SET ::SetProperty( CCS_RIGHT, v )         DEFAULT .F.
   PROPERTY CCS_Bottom        SET ::SetProperty( CCS_BOTTOM, v )        DEFAULT .F.
   PROPERTY CCS_NoMoveX       SET ::SetProperty( CCS_NOMOVEX, v )       DEFAULT .F.
   PROPERTY CCS_NoMoveY       SET ::SetProperty( CCS_NOMOVEY, v )       DEFAULT .F.
   PROPERTY CCS_NoResize      SET ::SetProperty( CCS_NORESIZE, v )      DEFAULT .F.
   PROPERTY CCS_NoParentAlign SET ::SetProperty( CCS_NOPARENTALIGN, v ) DEFAULT .F.
   PROPERTY CCS_Vert          SET ::SetProperty( CCS_VERT, v )          DEFAULT .F.

   METHOD SetProperty( nProp, lSet ) INLINE  ::Style := IIF( lSet, (::Style | nProp), (::Style & NOT( nProp )) )

ENDCLASS

CLASS UserControl INHERIT Control
   METHOD Init() CONSTRUCTOR
ENDCLASS

METHOD Init( oParent ) CLASS UserControl
   DEFAULT ::__xCtrlName TO "UserControl"
   DEFAULT ::ClsName     TO "UserControl"
   ::Style := (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   DEFAULT ::xLeft   TO 0
   DEFAULT ::xTop    TO 0
   DEFAULT ::xWidth  TO 200
   DEFAULT ::xHeight TO 200
   ::Super:Init( oParent )
   ::__IsStandard := .F.
RETURN Self


//---------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------

CLASS TitleControl INHERIT Control

   PROPERTY TitleHeight SET IIF( ::DesignMode, ::ResetFrame(),) DEFAULT 21
   PROPERTY Text        SET IIF( ::DesignMode, ::ResetFrame(),) DEFAULT ""
   PROPERTY AllowUnDock                    DEFAULT FALSE
   PROPERTY AllowClose                     DEFAULT FALSE
   PROPERTY MenuArrow                      DEFAULT .F.

   PROPERTY TextColor   ROOT "Colors"  GET IIF( ::xTextColor == NIL, ::__SysTextColor, ::xTextColor ) SET ::InValidateRect()

   DATA __SysTextColor       EXPORTED  INIT GetSysColor( COLOR_WINDOW )
   DATA __lActive            EXPORTED  INIT .F.
   DATA OnWMClose            EXPORTED

   DATA __aCaptionRect       PROTECTED
   DATA __lPinPushed         PROTECTED INIT .F.
   DATA __lPinHover          PROTECTED INIT .F.
   DATA __aPinRect           PROTECTED
   DATA __lClosePushed       PROTECTED INIT .F.
   DATA __lCloseHover        PROTECTED INIT .F.
   DATA __aCloseRect         PROTECTED
   DATA __aArrowRect         PROTECTED
   DATA __nBtnHeight         PROTECTED INIT 17

   METHOD Create()
   METHOD OnNCCalcSize()
   METHOD OnNCMouseleave()
   METHOD OnNCPaint()
   METHOD OnNCHitTest()
   METHOD OnNCLButtonDown()
   METHOD OnNCLButtonUp()
   METHOD Undock()
   METHOD Redock()
   METHOD DrawClose()
   METHOD DrawPin()
   METHOD SetActive( l ) INLINE IIF( ::__lActive != l, ( ::__lActive := l, ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT) ), ::UpdateWindow() ), )
   METHOD ResetFrame() VIRTUAL
   METHOD __OnClose()
   METHOD OnPaint()
ENDCLASS

METHOD Create() CLASS TitleControl
   ::Super:Create()
   IF ::__nCaptionHeight == 0 .AND. ! EMPTY( ::xText ) .AND. ::xTitleHeight > 0
      ::ResetFrame()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __OnClose( nwParam, nlParam ) CLASS TitleControl
   LOCAL nRet := ExecuteEvent( "OnClose", Self )
   ODEFAULT nRet TO ::OnClose( nwParam )
   ODEFAULT nRet TO __Evaluate( ::OnWMClose, Self, nwParam, nlParam )
   ::Application:DoEvents()
RETURN nRet

//---------------------------------------------------------------------------------------------------

METHOD OnNCCalcSize( nwParam, nlParam ) CLASS TitleControl
   LOCAL nccs
   (nwParam)
   ::__nCaptionHeight := IIF( EMPTY( ::xText ), 0, ::xTitleHeight )
   IF ::__nCaptionHeight > 0
      nccs := (struct NCCALCSIZE_PARAMS)
      nccs:Pointer( nlParam )
      nccs:rgrc[1]:Left += ::EmptyLeft
      nccs:rgrc[1]:Top  += ::__nCaptionHeight
      nccs:CopyTo( nlParam )
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------

METHOD OnNCMouseLeave() CLASS TitleControl
   ::Super:OnNCMouseLeave()
   IF ::__nCaptionHeight > 0 .AND. ( ::AllowClose .OR. ::AllowUnDock )
      ::__lCloseHover  := .F.
      ::__lClosePushed := .F.
      ::__lPinHover    := .F.
      ::__lPinPushed   := .F.
      ::OnNCPaint()
      RETURN 0
   ENDIF
RETURN NIL

//---------------------------------------------------------------------------------------------------

METHOD OnNCPaint( nwParam, nlParam ) CLASS TitleControl
   LOCAL hOldBrush, hOldPen, hdc, hOldFont, nWidth, n:=0, nLeft, nTop
   ::CallWindowProc()
   IF nwParam != NIL
      ::Super:OnNCPaint( nwParam, nlParam )
   ENDIF
   IF ::__nCaptionHeight > 0
      hdc := GetWindowDC( ::hWnd )

      IF (::ExStyle & WS_EX_CLIENTEDGE) == WS_EX_CLIENTEDGE
         n += 2
      ENDIF
      IF (::ExStyle & WS_EX_STATICEDGE) == WS_EX_STATICEDGE
         n += 1
      ENDIF
      ::__aCaptionRect := { n, n, ::xWidth - n, ::__nCaptionHeight + n + IIF( (::Style & WS_BORDER) == WS_BORDER, 1, 0 ) }

      IF ::BorderColor != NIL .AND. ::BorderColor <> 0
         hOldBrush := CreateSolidBrush( ::BorderColor )
         _FillRect( hDC, ::__aCaptionRect, hOldBrush )
      ELSE
         hOldPen   := SelectObject( hDC, ::ColorScheme:Pen:TitleBackColorActive )
         hOldBrush := SelectObject( hDC, IIF( ! ::__lActive, ::ColorScheme:Brush:TitleBackColorInactive, ::ColorScheme:Brush:TitleBackColorActive ) )
         Rectangle( hDC, ::__aCaptionRect[1], ::__aCaptionRect[2], ::__aCaptionRect[3], ::__aCaptionRect[4] )
      ENDIF

      IF ::BorderColor != NIL .AND. ::BorderColor <> 0
         DeleteObject( hOldBrush )
      ELSE
         SelectObject( hDC, hOldBrush )
         SelectObject( hDC, hOldPen )
      ENDIF


      SetTextColor( hDC, ::TextColor )

      hOldFont := SelectObject( hDC, ::System:hFont )
      SetBkMode( hDC, TRANSPARENT )

      _DrawText( hDC, ::xText, { IIF( ::MenuArrow .AND. ::__aArrowRect != NIL, ::__aArrowRect[3]+2, ::__aCaptionRect[1]+5 ), ::__aCaptionRect[2], ::__aCaptionRect[3], ::__aCaptionRect[4] }, (DT_LEFT | DT_SINGLELINE | DT_VCENTER | DT_WORD_ELLIPSIS) )

      nWidth := 0
      IF ::AllowClose
         nTop  := Int( ( ::__aCaptionRect[4]-::__aCaptionRect[2]-::__nBtnHeight ) / 2 ) + n
         nLeft := Int( ( ::__aCaptionRect[3]-(::__nBtnHeight+2) ) )
         ::__aCloseRect := { nLeft, nTop, nLeft + ::__nBtnHeight + 1, nTop + ::__nBtnHeight }

         ::DrawClose( hDC, n )
         nWidth := ::__aCloseRect[3]-::__aCloseRect[1]+1
      ENDIF
      IF ::AllowUnDock
         nTop  := Int( ( ::__aCaptionRect[4]-::__aCaptionRect[2]-::__nBtnHeight ) / 2 ) + n
         nLeft := Int( ( ::__aCaptionRect[3]-(::__nBtnHeight+2)-IIF( ::AllowClose, (::__nBtnHeight+2), 0) ) )
         ::__aPinRect := { nLeft, nTop, nLeft + ::__nBtnHeight + 1, nTop + ::__nBtnHeight }

         ::DrawPin( hDC, n )
      ENDIF

      n := IIF( ! ::IsChild, 1, 0 )

      IF ::MenuArrow
         ::__aArrowRect := { 0, ::__aCaptionRect[2]+n+1, 20, ::__aCaptionRect[4]-2 }
         ::DrawArrow( hDC, ::__aArrowRect )
      ENDIF

      SelectObject( hDC, hOldFont )
      ReleaseDC(::hWnd, hdc)
   ENDIF

RETURN 0

//---------------------------------------------------------------------------------------------------

METHOD OnNCHitTest( x, y ) CLASS TitleControl
   LOCAL nRes, aPt, hRegion, hdc, n
   IF !EMPTY(::__aCaptionRect) .AND. ::__nCaptionHeight > 0 .AND. ::Super:OnNCHitTest( x, y ) == NIL
      aPt := { x, y }
      _ScreenToClient( ::hWnd, @aPt )
      aPt[2]+=::__aCaptionRect[4]
      n := 0
      IF !::IsChild
         n := 1
      ENDIF
      IF ::AllowClose
         // Check the close button
         IF _PtInRect( ::__aCloseRect, aPt )
            nRes := HTCLOSE
            ::__lCloseHover  := .T.
          ELSE
            ::__lCloseHover  := .F.
            IF ::__lClosePushed
               nRes := HTCLOSE
            ENDIF
         ENDIF
         hRegion := CreateRectRgn( ::__aCloseRect[1], ::__aCloseRect[2], ::__aCloseRect[3], ::__aCloseRect[4] )
         hdc := GetDCEx( ::hWnd, hRegion, DCX_WINDOW + DCX_PARENTCLIP + DCX_CLIPSIBLINGS + DCX_VALIDATE )
         ::DrawClose( hDC, n )
         ReleaseDC(::hWnd, hdc)
         DeleteObject( hRegion )
      ENDIF
      IF ::AllowUnDock
         // Check the pin button
         IF _PtInRect( ::__aPinRect, aPt )
            nRes := HTBORDER
            ::__lPinHover  := .T.
          ELSE
            ::__lPinHover  := .F.
            IF ::__lPinPushed
               nRes := HTBORDER
            ENDIF
         ENDIF
         hRegion := CreateRectRgn( ::__aPinRect[1], ::__aPinRect[2], ::__aPinRect[3], ::__aPinRect[4] )
         hdc := GetDCEx( ::hWnd, hRegion, DCX_WINDOW + DCX_PARENTCLIP + DCX_CLIPSIBLINGS + DCX_VALIDATE )
         ::DrawPin( hDC, n )
         ReleaseDC(::hWnd, hdc)
         DeleteObject( hRegion )
      ENDIF
      IF ::MenuArrow
         IF _PtInRect( ::__aArrowRect, aPt )
            nRes := HTMENU
         ENDIF
      ENDIF
      IF !::AllowUnDock .AND. !::AllowClose .AND. !::AllowMaximize .AND. nRes == NIL .AND. _PtInRect( ::__aCaptionRect, aPt )
         RETURN HTNOWHERE
      ENDIF

   ENDIF
RETURN nRes

//---------------------------------------------------------------------------------------------------

METHOD OnNCLButtonDown( nwParam, nlParam ) CLASS TitleControl
   LOCAL hRegion, hdc, aRect, n

   ::Super:OnNCLButtonDown( nwParam, nlParam )

   IF nwParam == HTCAPTION
      IF (::Style & WS_CHILD) == WS_CHILD
         RETURN 0
      ENDIF
   ENDIF
   IF nwParam == HTCLOSE .OR. nwParam == HTBORDER
      IF ::__lCloseHover .AND. ::__aCloseRect != NIL
         ::__lClosePushed := .T.
         ::__lPinPushed   := .F.
         aRect := ACLONE( ::__aCloseRect )
       ELSEIF ::__lPinHover .AND. ::__aPinRect != NIL
         ::__lPinPushed := .T.
         ::__lClosePushed := .F.
         aRect := ACLONE( ::__aPinRect )
      ENDIF
      IF aRect != NIL
         hRegion := CreateRectRgn( aRect[1], aRect[2], aRect[3], aRect[4] )
         hdc := GetDCEx( ::hWnd, hRegion, (DCX_WINDOW | DCX_PARENTCLIP | DCX_CLIPSIBLINGS | DCX_VALIDATE ))
         n := 0
         IF !::IsChild
            n := 1
         ENDIF
         IF ::__lCloseHover
            ::DrawClose( hDC, n )
          ELSEIF ::__lPinHover
            ::DrawPin( hDC, n )
         ENDIF
         ReleaseDC(::hWnd, hdc)
         DeleteObject( hRegion )
         RETURN nwParam
      ENDIF
   ENDIF
RETURN NIL //nwParam

//---------------------------------------------------------------------------------------------------

METHOD OnNCLButtonUp( nwParam, nlParam ) CLASS TitleControl
   LOCAL pt, aPt := {LOWORD( nlParam ),HIWORD( nlParam )}

   ::Super:OnNCLButtonUp( nwParam, nlParam )

   ::__lCloseHover  := .F.
   ::__lPinHover    := .F.
   IF nwParam == HTCLOSE .OR. nwParam == HTBORDER .AND. !EMPTY( ::__aCaptionRect )
      _ScreenToClient( ::hWnd, @aPt )
      aPt[2]+=::__aCaptionRect[4]
      IF ::ClsName == "DLGEDT"
         aPt[1] += ::RullerWeight
         aPt[2] += ::RullerWeight
      ENDIF
      IF ::__aCloseRect != NIL .AND. _PtInRect( ::__aCloseRect, aPt ) .AND. ::__lClosePushed
         ::PostMessage( WM_CLOSE )
       ELSEIF ::__aPinRect != NIL .AND. _PtInRect( ::__aPinRect, aPt ) .AND. ::__lPinPushed
         ::Undock()
      ENDIF
    ELSEIF nwParam == HTMENU
      _ScreenToClient( ::hWnd, @aPt )
      aPt[2]+=::__aCaptionRect[4]
      IF ::__aArrowRect != NIL .AND. _PtInRect( ::__aArrowRect, aPt )
         IF ::ContextMenu != NIL
            pt := (struct POINT)
            pt:x := ::Left
            pt:y := ::Top+::__nCaptionHeight
            ClientToScreen( ::Parent:hWnd, @pt )
            ::ContextMenu:Show( pt:x, pt:y )
         ENDIF
      ENDIF
   ENDIF
   ::__lClosePushed := .F.
   ::__lPinPushed := .F.
RETURN nwParam

//---------------------------------------------------------------------------------------------------

METHOD Redock() CLASS TitleControl
   LOCAL oControl, hDef, o
   IF __Evaluate( ::OnWMRedock,  Self ) == NIL .AND. ::AllowUnDock
      ::__Docked := .T.

      ::xText := ::BackInfo[15]

      ::Hide()

      SetParent( ::hWnd, ::BackInfo[13] )
      ::xLeft        := ::BackInfo[ 5]
      ::xTop         := ::BackInfo[ 6]
      ::xWidth       := ::BackInfo[ 7]
      ::xHeight      := ::BackInfo[ 8]

      ::ClientWidth := ::BackInfo[ 9]
      ::ClientHeight:= ::BackInfo[10]

      IF ::BackInfo[ 1] != NIL
         o := Splitter( ::Parent )
         o:Owner    := Self
         o:Weight   := ::BackInfo[1]:Weight
         o:Position := ::BackInfo[1]:Position
         o:Create()
      ENDIF
      IF ::BackInfo[ 2] != NIL
         o := Splitter( ::Parent )
         o:Owner    := Self
         o:Weight   := ::BackInfo[2]:Weight
         o:Position := ::BackInfo[2]:Position
         o:Create()
      ENDIF
      IF ::BackInfo[ 3] != NIL
         o := Splitter( ::Parent )
         o:Owner    := Self
         o:Weight   := ::BackInfo[3]:Weight
         o:Position := ::BackInfo[3]:Position
         o:Create()
      ENDIF
      IF ::BackInfo[ 4] != NIL
         o := Splitter( ::Parent )
         o:Owner    := Self
         o:Weight   := ::BackInfo[4]:Weight
         o:Position := ::BackInfo[4]:Position
         o:Create()
      ENDIF
      ::SetWindowPos(, ::xLeft, ::xTop, ::xWidth, ::xHeight, (SWP_DRAWFRAME | SWP_FRAMECHANGED | SWP_NOSENDCHANGING) )

      IF ! Empty( ::Parent:__aDock )
         hDef := BeginDeferWindowPos( LEN( ::Parent:__aDock ) )
         FOR EACH oControl IN ::Parent:__aDock
             oControl:__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight, @hDef )
             oControl:InvalidateRect()//, .F. )
         NEXT
         EndDeferWindowPos( hDef )
      ENDIF

      ::__DockParent := NIL

      TRY
         IF ::Parent:MDIClient != NIL
            ::Parent:MDIClient:AlignLeft   := ::BackInfo[14][1]
            ::Parent:MDIClient:AlignTop    := ::BackInfo[14][2]
            ::Parent:MDIClient:AlignRight  := ::BackInfo[14][3]
            ::Parent:MDIClient:AlignBottom := ::BackInfo[14][4]
         ENDIF
       CATCH
      END
      ::Dock:Left   := ::BackInfo[16][1]
      ::Dock:Top    := ::BackInfo[16][2]
      ::Dock:Right  := ::BackInfo[16][3]
      ::Dock:Bottom := ::BackInfo[16][4]

      ::BackInfo := NIL

      ::Show()
      ExecuteEvent( "OnRedock", Self )
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------

METHOD Undock() CLASS TitleControl
   LOCAL hDef, oChild, pt

   IF __Evaluate( ::OnWMUnDock,  Self ) == NIL .AND. ::AllowUnDock
      ::__Docked := .F.
      ::BackInfo := { ::LeftSplitter, ::TopSplitter, ::RightSplitter, ::BottomSplitter,;
                      ::xLeft, ::xTop, ::xWidth, ::xHeight, ::ClientWidth, ::ClientHeight,;
                      ::Style, ::ExStyle, ::Parent:hWnd,, ::xText, { ::Dock:Left, ::Dock:Top, ::Dock:Right, ::Dock:Bottom } }

      ::xText := ""
      ::ResetFrame()
      IF ::Parent:MDIContainer .AND. ::Parent:MDIClient != NIL
         ::BackInfo[14] := { ::Parent:MDIClient:AlignLeft, ::Parent:MDIClient:AlignTop, ::Parent:MDIClient:AlignRight, ::Parent:MDIClient:AlignBottom }
         IF ::Parent:MDIClient:AlignLeft == Self
            ::Parent:MDIClient:AlignLeft := ::Dock:Left
         ENDIF
         IF ::Parent:MDIClient:AlignBottom == Self
            ::Parent:MDIClient:AlignBottom := ::Dock:Bottom
         ENDIF
         IF ::Parent:MDIClient:AlignRight == Self
            ::Parent:MDIClient:AlignRight := NIL
         ENDIF
      ENDIF

      IF ::LeftSplitter != NIL
         ::LeftSplitter:Destroy()
         ::LeftSplitter := NIL
      ENDIF
      IF ::TopSplitter != NIL
         ::TopSplitter:Destroy()
         ::TopSplitter := NIL
      ENDIF
      IF ::RightSplitter != NIL
         ::RightSplitter:Destroy()
         ::RightSplitter := NIL
      ENDIF
      IF ::BottomSplitter != NIL
         ::BottomSplitter:Destroy()
         ::BottomSplitter := NIL
      ENDIF

      pt := (struct POINT)
      pt:x := ::left
      pt:y := ::top

      ClientToScreen( ::Form:hWnd, @pt )

      ::__DockParent := WinForm( ::Form )
      ::__DockParent:Cargo      := Self
      ::__DockParent:Caption    := ::xText
      ::__DockParent:Left       := pt:x
      ::__DockParent:Top        := pt:y
      ::__DockParent:Width      := ::Width  + ( GetSystemMetrics( SM_CXFRAME ) + 2 )
      ::__DockParent:Height     := ::Height + ( GetSystemMetrics( SM_CYFRAME ) + 2 ) + GetSystemMetrics( SM_CYSMCAPTION )
      ::__DockParent:ToolWindow := .T.
      ::__DockParent:SysMenu    := .T. //::AllowClose
      ::__DockParent:ThickFrame := .T.
      ::__DockParent:AutoClose  := .T.
      ::__DockParent:Create()

      SetParent( ::hWnd, ::__DockParent:hWnd )

      MoveWindow( ::hWnd, 0, 0, ::Width, ::Height )
      ::__DockParent:Show()
      ::__DockParent:OnWMClose   := {|| IIF( ::IsDocked, 0, ::Redock() ) }

      IF ! Empty( ::Parent:__aDock )
         hDef := BeginDeferWindowPos( LEN( ::Parent:__aDock ) )
         FOR EACH oChild IN ::Parent:__aDock
             IF oChild:hWnd != ::hWnd .AND. oChild:Dock != NIL .AND. ( oChild:Dock:Left   != NIL .OR.;
                                                                       oChild:Dock:Top    != NIL .OR.;
                                                                       oChild:Dock:Right  != NIL .OR.;
                                                                       oChild:Dock:Bottom != NIL )
                oChild:__OnParentSize( ::Parent:Width, ::Parent:Height,hDef )
             ENDIF
         NEXT
         EndDeferWindowPos( hDef )
      ENDIF

      ::__DockParent:OnWMSize := {|o,nwParam,nlParam| (o,nwParam), ::MoveWindow(0,0,LOWORD(nlParam),HIWORD(nlParam),.T.) }

      ::ResetFrame()
      ExecuteEvent( "OnUndock", Self )
   ENDIF

RETURN Self

//---------------------------------------------------------------------------------------------------

METHOD DrawClose( hDC ) CLASS TitleControl
   LOCAL hOld
   LOCAL aRect  := ::__aCloseRect

   hOld := SelectObject( hDC, ::ColorScheme:Pen:TitleBackColorActive ) //::System:TitleBorderPen )
   IF ::__lCloseHover
      SelectObject( hDC, IIF( !::__lClosePushed, ::Application:ColorTable:Brush:MenuItemSelected, ::Application:ColorTable:Brush:MenuItemSelectedGradientEnd ) )
      Rectangle( hDC, aRect[1], aRect[2], aRect[3], aRect[4] )
    ELSE
      _FillRect( hDC, aRect, IIF( ! ::__lActive, ::ColorScheme:Brush:TitleBackColorInactive, ::ColorScheme:Brush:TitleBackColorActive ) )
      SelectObject( hDC, GetStockObject( IIF( ! ::__lActive, WHITE_PEN, BLACK_PEN ) ) )
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

   aRect[1]-=4
   aRect[2]-=4
   aRect[3]+=4
   aRect[4]+=4
   SelectObject( hDC, hOld )

RETURN Self

METHOD DrawPin( hDC, n ) CLASS TitleControl
   LOCAL hOld, nLeft, nRight, nBottom
   LOCAL aRect  := ::__aPinRect

   hOld := SelectObject( hDC, ::ColorScheme:Pen:TitleBackColorActive ) //::System:TitleBorderPen )
   IF ::__lPinHover
      SelectObject( hDC, IIF( ! ::__lPinPushed, ::Application:ColorTable:Brush:MenuItemSelected, ::Application:ColorTable:Brush:MenuItemSelectedGradientEnd ) )
      Rectangle( hDC, aRect[1], aRect[2], aRect[3], aRect[4] )
    ELSE
      SelectObject( hDC, IIF( ! ::__lActive, ::ColorScheme:Brush:TitleBackColorInactive, ::ColorScheme:Brush:TitleBackColorActive ) )
      _FillRect( hDC, aRect, IIF( ! ::__lActive, ::ColorScheme:Brush:TitleBackColorInactive, ::ColorScheme:Brush:TitleBackColorActive ) )
      SelectObject( hDC, GetStockObject( IIF( ! ::__lActive, WHITE_PEN, BLACK_PEN ) ) )
   ENDIF

   aRect[1] += 4
   aRect[2] += (3-n)
   aRect[3] -= 4
   aRect[4] -= 4

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
   aRect[1]-=4
   aRect[2]-=(3-n)
   aRect[3]+=4
   aRect[4]+=4
   SelectObject( hDC, hOld )

RETURN Self

METHOD OnPaint() CLASS TitleControl
   LOCAL hDC, hMemDC, hBitmap, hOldBitmap
   IF ::DesignMode .AND. ::IsContainer
      hDC        := ::BeginPaint()

      hMemDC     := CreateCompatibleDC( hDC )
      hBitmap    := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hBitmap )

      _FillRect( hMemDC, { 0,0,::ClientWidth, ::ClientHeight }, IIF( ::bkBrush != NIL, ::bkBrush, GetSysColorBrush( COLOR_BTNFACE ) ) )

      IF ::Application:ShowGrid == 1
         DrawGrid( hMemDC, ::Form:CtrlMask:xGrid, ::Form:CtrlMask:yGrid, ::ClientWidth, ::ClientHeight, RGB(0,0,0) )
      ENDIF

      BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )

      SelectObject( hMemDC, hOldBitmap )
      DeleteObject( hBitmap )
      DeleteDC( hMemDC )

      ::EndPaint()
      RETURN 0
   ENDIF
RETURN Super:OnPaint()
