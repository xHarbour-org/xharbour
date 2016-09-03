/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Label.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

#define EP_EDITBORDER_NOSCROLL 6
#define EPSN_NORMAL            1
#define EPSN_HOT               2
#define EPSN_FOCUSED           3
#define EPSN_DISABLED          4

//-----------------------------------------------------------------------------------------------

CLASS Label INHERIT Control
   PROPERTY Alignment                     SET ::Redraw(v)      DEFAULT DT_LEFT
   PROPERTY NoPrefix                      SET ::Redraw(v)      DEFAULT .F.
   PROPERTY VertCenter                    SET ::Redraw(v)      DEFAULT .F.
   PROPERTY TextShadowColor ROOT "Colors" SET ::InvalidateRect()
   PROPERTY BlinkColor      ROOT "Colors" SET ::__SetBlinkColor(v)
   PROPERTY BorderColor     ROOT "Colors"                      DEFAULT __GetSystem():Color:Gray
   PROPERTY Padding                       SET ::InvalidateRect() DEFAULT 0
   PROPERTY WrapText        ROOT "Behavior" DEFAULT .F.

   DATA EnumAlignment    EXPORTED INIT { { "Left", "Center", "Right" }, { DT_LEFT, DT_CENTER, DT_RIGHT } }
   DATA EnumBorder       EXPORTED INIT { { "None", "Flat", "Sunken", "Risen" }, { 0, -1, BDR_SUNKENINNER, BDR_RAISEDINNER } }

   DATA TabStop          EXPORTED INIT .F.
   DATA xTabStop         EXPORTED INIT .F.

   // Backward compatibility
   ACCESS CenterText    INLINE ::Alignment == DT_CENTER
   ASSIGN CenterText(l) INLINE ::Alignment := IIF( l, DT_CENTER, DT_LEFT )

   ACCESS RightAlign    INLINE ::Alignment == DT_RIGHT
   ASSIGN RightAlign(l) INLINE ::Alignment := IIF( l, DT_RIGHT, DT_LEFT )

   ACCESS Sunken        INLINE ::Border == BDR_SUNKENINNER
   ASSIGN Sunken(l)     INLINE ::Border := IIF( l, BDR_SUNKENINNER, 0 )

   ASSIGN SunkenText(l)  INLINE IIF( l, ::xTextShadowColor := RGB( 255, 255, 255 ),)

   DATA __CurColor     PROTECTED

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD SetParent( oParent ) INLINE ::Super:SetParent( oParent ), ::RedrawWindow( , , ( RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW ) )
   METHOD OnEraseBkGnd()       INLINE 1
   METHOD OnPaint()
   METHOD SetWindowText(cText) INLINE Super:SetWindowText(cText), IIF( ::IsWindowVisible(), ( ::InvalidateRect(), ::UpdateWindow() ), )
   //METHOD __SetTransp(lSet)    INLINE IIF( lSet, ::Parent:__RegisterTransparentControl( Self ), ::Parent:__UnregisterTransparentControl( Self ) )
   METHOD __SetBlinkColor()
   METHOD OnSize(w,l)          INLINE Super:OnSize( w, l ), ::Redraw(), NIL
   METHOD OnLButtonUp()
   METHOD OnTimer()
   METHOD SetForeColor()
   METHOD Redraw( aRect )      INLINE ::RedrawWindow( aRect, , ( RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT ) ),::UpdateWindow()
   METHOD __SetBorder()        INLINE ::Refresh()
   METHOD __Enable( lEnable )  INLINE ::xEnabled := lEnable, EnableWindow( ::hWnd, lEnable ), ::InvalidateRect(), ::UpdateWindow(), lEnable

ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Label
   DEFAULT ::__xCtrlName TO "Label"
   ::Style        := (WS_CHILD | WS_VISIBLE | BS_OWNERDRAW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::ClsName      := "Label"
   ::Super:Init( oParent )
   ::__IsStandard := .F.
   ::xWidth       := 80
   ::xHeight      := 16
   ::Events       := ;
          { ;
            {"Command",     {;
                            { "OnClick"            , "", "" } } },;
            {"Layout",      { ;
                            { "OnEnterSizeMove"    , "", "" },;
                            { "OnExitSizeMove"     , "", "" },;
                            { "OnMove"             , "", "" },;
                            { "OnSize"             , "", "" } } },;
            {"Mouse",       {;
                            { "OnLButtonDblClk"    , "", "" },;
                            { "OnLButtonDown"      , "", "" },;
                            { "OnLButtonUp"        , "", "" },;
                            { "OnMButtonDown"      , "", "" },;
                            { "OnMButtonUp"        , "", "" },;
                            { "OnMouseHover"       , "", "" },;
                            { "OnMouseLeave"       , "", "" },;
                            { "OnMouseMove"        , "", "" },;
                            { "OnRButtonDown"      , "", "" },;
                            { "OnRButtonUp"        , "", "" } } },;
            {"Control",     {;
                            { "OnCreate"           , "", "" },;
                            { "OnDestroy"          , "", "" },;
                            { "OnEnable"           , "", "" },;
                            { "OnSetText"          , "", "" } } } }
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create()  CLASS Label
   //IF ::Parent:__xCtrlName IN {"TabPage","GroupBox"} .AND. ! ::xTransparent .AND. ::BackColor == ::__SysBackColor
   //   ::Transparent := .T.
   //ENDIF
   ::Parent:Refresh()
   Super:Create()
   ::__SetBlinkColor()
RETURN Self
//-----------------------------------------------------------------------------------------------

METHOD SetForeColor( nColor, lRepaint ) CLASS Label
   DEFAULT lRepaint TO .T.
   ::xForeColor := nColor
   ::__CurColor := nColor
   IF lRepaint .AND. ::IsWindowVisible()
      ::Refresh()
   ENDIF
   IF ::IsWindowVisible()
      ::InValidateRect()
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------
METHOD __SetBlinkColor() CLASS Label
   IF ! ::DesignMode
      IF ::BlinkColor != NIL
         ::SetTimer( 512, 500 )
       ELSE
         ::KillTimer( 512 )
         ::__CurColor := ::ForeColor
         ::RedrawWindow( , , (RDW_INTERNALPAINT | RDW_UPDATENOW | RDW_INVALIDATE) )
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnTimer( nID ) CLASS Label
   IF nID == 512
      ::KillTimer( 512 )
      DEFAULT ::__CurColor TO ::ForeColor
      IF ::__CurColor == ::ForeColor
         ::__CurColor := ::BlinkColor
      ELSE
         ::__CurColor := ::ForeColor
      ENDIF
      ::RedrawWindow( , , (RDW_INTERNALPAINT | RDW_UPDATENOW | RDW_INVALIDATE) )
      ::SetTimer( 512, 500 )
   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------
METHOD OnLButtonUp() CLASS Label
   LOCAL nRet
   nRet := __Evaluate( ::Action, Self,,, nRet )
   ExecuteEvent( "OnClick", Self )
RETURN nRet

//-----------------------------------------------------------------------------------------------
METHOD OnPaint() CLASS Label
   LOCAL hOldPen, nFlags, cText, hBrush, hFont, hBkGnd, aRect := {0,0,::xWidth,::xHeight}
   LOCAL hMemDC, hMemBitmap, hOldBitmap, hDC, rc

   hDC        := ::BeginPaint()

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   hBkGnd := ::BkBrush
   IF hBkGnd == NIL
      hBkGnd := ::Parent:BkBrush
      SetBrushOrgEx( hMemDC, ::Parent:ClientWidth-::Left, ::Parent:ClientHeight-::Top )
   ENDIF
   DEFAULT hBkGnd TO GetSysColorBrush( COLOR_BTNFACE )

   DEFAULT ::__CurColor TO ::ForeColor

   DEFAULT hBkGnd TO GetSysColorBrush( COLOR_BTNFACE )
   _FillRect( hMemDC, aRect, IIF( ::Enabled, hBkGnd, GetSysColorBrush( COLOR_BTNFACE ) ) )

   // compatibility
   IF VALTYPE( ::xBorder ) == "L"
      ::xBorder := IIF( ! ::xBorder, 0, -1 )
   ENDIF

   IF ::xBorder <> 0
      IF ::xBorder == -1
         hOldPen := SelectObject( hMemDC, CreatePen( PS_SOLID, 0, IIF( ::Enabled, ::BorderColor, ::System:Colors:GrayText ) ) )
         hBrush  := SelectObject( hMemDC, GetStockObject( NULL_BRUSH ) )
         Rectangle( hMemDC, aRect[1], aRect[2], aRect[3], aRect[4] )
         SelectObject( hMemDC, hBrush )
         DeleteObject( SelectObject( hMemDC, hOldPen ) )

       ELSE
         _DrawEdge( hMemDC, aRect, ::xBorder, BF_RECT )
      ENDIF
      aRect := {1,1,::xWidth-1,::xHeight-1}
   ENDIF

   SetBkMode( hMemDC, TRANSPARENT )
   hFont  := SelectObject( hMemDC, ::Font:Handle )
   nFlags := ::Alignment
   IF ::WrapText
      nFlags := (::Alignment | DT_WORDBREAK)
   ENDIF
   IF ::NoPrefix
      nFlags := (nFlags | DT_NOPREFIX)
   ENDIF

   cText := ::xText
   DEFAULT cText TO ""

   IF ::Padding <> 0
      //_GetTextExtentExPoint( hMemDC, Space(::Margin), ::Margin, @iLen )
      //VIEW iLen
      IF ::Alignment == DT_LEFT
         cText := Space(::Padding)+cText
      ELSEIF ::Alignment == DT_RIGHT
         cText += Space(::Padding)
      ENDIF
   ENDIF

   IF ::VertCenter
      rc := (struct RECT)
      rc:left   := aRect[1]
      rc:top    := aRect[2]
      rc:right  := aRect[3]
      rc:bottom := aRect[4]
      DrawText( hMemDC, cText, @rc, (DT_CALCRECT|DT_CENTER|IIF( ::WrapText, DT_WORDBREAK, 0 )) )
      aRect[2]  := ( aRect[4]-rc:bottom ) / 2
      aRect[4]  := aRect[2] + rc:bottom
   ENDIF

   IF ::xTextShadowColor != NIL .AND. ::Enabled
      aRect[1] += 1
      aRect[2] += 1
      aRect[3] += 1
      aRect[4] += 1
      SetTextColor( hMemDC, ::xTextShadowColor )
      _DrawText( hMemDC, cText, aRect, nFlags )
      aRect[1] -= 1
      aRect[2] -= 1
      aRect[3] -= 1
      aRect[4] -= 1
   ENDIF
   SetTextColor( hMemDC, IIF( ::Enabled, ::__CurColor, ::System:Colors:GrayText ) )
   _DrawText( hMemDC, cText, aRect, nFlags )
   SelectObject( hMemDC, hFont )

   BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )
   ::EndPaint()
RETURN 0



CLASS Line INHERIT CONTROL
   PROPERTY Lenght         SET ::__SetLenght(v)   DEFAULT 150
   PROPERTY Sunken         SET ::__SetSunken(v)   DEFAULT .T.
   PROPERTY Vertical       SET ::__SetVertical(v) DEFAULT .F.
   PROPERTY Color  ROOT "Colors"  DEFAULT  GetSysColor( COLOR_BTNSHADOW )

   ACCESS Width            INLINE ::xWidth
   ACCESS Height           INLINE ::xHeight

   DATA xText              EXPORTED  INIT ""
   DATA Text               EXPORTED  INIT ""

   DATA Weight             EXPORTED  INIT 2
   DATA Border             EXPORTED
   DATA IndexOrder         EXPORTED  INIT 0

   DATA Font               EXPORTED
   DATA TabStop            EXPORTED
   DATA BackColor          EXPORTED
   DATA ForeColor          EXPORTED
   DATA AllowClose         EXPORTED
   DATA AllowUndock        EXPORTED
   DATA ClipChildren       EXPORTED
   DATA ClipSiblings       EXPORTED
   DATA OwnerDraw          EXPORTED  INIT .F.
   //DATA Transparent        EXPORTED

   DATA AcceptFiles        EXPORTED  INIT .F.
   DATA AnimationStyle     EXPORTED  INIT 0
   DATA ContextMenu        EXPORTED
   DATA AllowMaximize      EXPORTED  INIT .F.
   DATA Enabled            EXPORTED  INIT .T.
   DATA TabOrder           EXPORTED  INIT 0
   DATA NoActivate         EXPORTED  INIT .F.
   DATA Theming            EXPORTED  INIT .F.

   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD OnEraseBkGnd()
   METHOD OnSize()

   METHOD __SetVertical()
   METHOD __SetSunken()
   METHOD __SetLenght()
ENDCLASS

METHOD Init( oParent ) CLASS Line
   DEFAULT ::__xCtrlName TO "Line"
   ::ClsName       := "VxhLine"
   ::Style         := (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::__lResizeable := {.F.,.T.,.F.,.F.,.F.,.T.,.F.,.F.}
   ::Super:Init( oParent )
   ::__IsStandard  := .F.
   ::Width         := ::Lenght
   ::Height        := ::Weight
   ::Events        := ;
          { ;
            {"Layout",      { ;
                            { "OnEnterSizeMove"    , "", "" },;
                            { "OnExitSizeMove"     , "", "" },;
                            { "OnMove"             , "", "" },;
                            { "OnSize"             , "", "" } } },;
            {"Control",     {;
                            { "OnCreate"           , "", "" },;
                            { "OnDestroy"          , "", "" },;
                            { "OnEnable"           , "", "" } } } }
RETURN Self

METHOD Create() CLASS Line
   Super:Create()
RETURN Self

METHOD __SetVertical( lSet ) CLASS Line
   ::__lResizeable := {.F.,!lSet,.F.,lSet,.F.,!lSet,.F.,lSet}
   IF lSet
      ::xHeight := ::xLenght
      ::xWidth  := ::Weight
    ELSE
      ::xWidth  := ::xLenght
      ::xHeight := ::Weight
   ENDIF
   IF ::hWnd != NIL
      ::MoveWindow()
   ENDIF
RETURN NIL

METHOD __SetSunken( lSet ) CLASS Line
   IF ::xSunken != lSet
      ::Weight := IIF( lSet, 2, 1 )
      IF ::xVertical
         ::xWidth := ::Weight
       ELSE
         ::xHeight := ::Weight
      ENDIF
   ENDIF
   IF ::hWnd != NIL
      ::MoveWindow()
   ENDIF
RETURN NIL

METHOD __SetLenght( nLen ) CLASS Line
   IF ::xLenght != nLen
      IF ::xVertical
         ::xHeight := nLen
       ELSE
         ::xWidth  := nLen
      ENDIF
      IF ::hWnd != NIL
         ::MoveWindow()
      ENDIF
   ENDIF
RETURN NIL

METHOD OnSize( nwParam, nlParam ) CLASS Line
   Super:OnSize( nwParam, nlParam )
   IF ::DesignMode
      ::xLenght := IIF( ::xVertical, ::Height, ::Width )
   ENDIF
RETURN NIL

METHOD OnEraseBkGnd( hDC ) CLASS Line
   LOCAL lVert := ::xVertical, hBrush := CreateSolidBrush( ::Color )
   _FillRect( hDC, { 0, 0, IIF( lVert, 1, ::Width ), IIF( lVert, ::Height, 1 ) }, hBrush )
   DeleteObject( hBrush )
   IF ::Sunken
      _FillRect( hDC, { IIF( lVert, 1, 0 ), IIF( lVert, 0, 1 ), IIF( lVert, 2, ::Width ), IIF( lVert, ::Height, 2 ) }, GetStockObject( WHITE_BRUSH ) )
   ENDIF
RETURN 1
