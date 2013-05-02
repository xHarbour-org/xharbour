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

//-----------------------------------------------------------------------------------------------

CLASS Label INHERIT Control
   DATA Transparent  PUBLISHED INIT .F.

   DATA ImageList    EXPORTED
   DATA ImageIndex   PROTECTED
   DATA AllowUnDock  EXPORTED INIT .F.
   DATA AllowClose   EXPORTED INIT .F.

   PROPERTY CenterText   INDEX SS_CENTER      READ xCenterText WRITE SetStyle  DEFAULT .F. PROTECTED
   PROPERTY RightAlign   INDEX SS_RIGHT       READ xRightAlign WRITE SetStyle  DEFAULT .F. PROTECTED
   PROPERTY Sunken       INDEX SS_SUNKEN      READ xSunken     WRITE SetStyle  DEFAULT .F. PROTECTED
   PROPERTY Simple       INDEX SS_SIMPLE      READ xSimple     WRITE SetStyle  DEFAULT .F. PROTECTED
   PROPERTY Noprefix     INDEX SS_NOPREFIX    READ xNoprefix   WRITE SetStyle  DEFAULT .F. PROTECTED
   PROPERTY Border       INDEX WS_BORDER      READ xBorder     WRITE SetStyle  DEFAULT .F. PROTECTED

   METHOD Init()  CONSTRUCTOR
   METHOD OnCtlColorStatic()
   METHOD SetParent( oParent ) INLINE IIF( ::__hBrush != NIL, ( DeleteObject( ::__hBrush ), ::__hBrush := NIL ), ), ::Super:SetParent( oParent ), ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
   METHOD OnSize(w,l)  INLINE Super:OnSize( w, l ), ::InvalidateRect(, .F. ), NIL
   METHOD Create()     INLINE IIF( ::Transparent, ::Parent:__RegisterTransparentControl( Self ), ), Super:Create()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Label
   DEFAULT ::__xCtrlName TO "Label"
   ::ClsName    := "static"
   ::Style      := WS_CHILD | WS_VISIBLE | SS_NOTIFY | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Super:Init( oParent )
   ::Width      := 80
   ::Height     := 16
   ::Events     := ;
          { ;
            {"Command",     {;
                            { "OnClick"            , "", "" } } },;
            {"Color",       {;
                            { "OnCtlColorStatic"   , "", "" } } },;
            {"Drawing",     {;
                            { "OnEraseBkGnd"       , "", "" },;
                            { "OnPaint"            , "", "" } } },;
            {"Layout",      { ;
                            { "OnEnterSizeMove"    , "", "" },;
                            { "OnExitSizeMove"     , "", "" },;
                            { "OnMove"             , "", "" },;
                            { "OnSize"             , "", "" } } },;
            {"Parent",      {;
                            { "OnParentDrawItem"   , "", "" } } },;
            {"Mouse",       {;
                            { "OnLButtonDblClk"    , "", "" },;
                            { "OnLButtonDown"      , "", "" },;
                            { "OnLButtonUp"        , "", "" },;
                            { "OnMButtonDown"      , "", "" },;
                            { "OnMButtonUp"        , "", "" },;
                            { "OnMouseActivate"    , "", "" },;
                            { "OnMouseHover"       , "", "" },;
                            { "OnMouseLeave"       , "", "" },;
                            { "OnMouseMove"        , "", "" },;
                            { "OnRButtonDown"      , "", "" },;
                            { "OnRButtonUp"        , "", "" },;
                            { "OnMouseWheel"       , "", "" } } },;
            {"Control",     {;
                            { "OnCreate"           , "", "" },;
                            { "OnDestroy"          , "", "" },;
                            { "OnEnable"           , "", "" },;
                            { "OnSetText"          , "", "" } } } }
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD OnCtlColorStatic( nwParam ) CLASS Label
   LOCAL hBkGnd := ::GetBkBrush()

   IF ::ForeColor != NIL
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
      RETURN GetStockObject( NULL_BRUSH )
   ENDIF

   IF ::Parent:ClsName == "ToolBarWindow32" .OR. ::Transparent
      SetBkMode( nwParam, TRANSPARENT )
      RETURN GetStockObject( NULL_BRUSH )
   ENDIF
RETURN NIL


CLASS Line INHERIT CONTROL
   PROPERTY Lenght         READ xLenght   WRITE __SetLenght   DEFAULT 150 INVERT
   PROPERTY Sunken         READ xSunken   WRITE __SetSunken   DEFAULT .T. INVERT
   PROPERTY Vertical       READ xVertical WRITE __SetVertical DEFAULT .F.

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
   DATA ClientEdge         EXPORTED
   DATA ClipChildren       EXPORTED
   DATA ClipSiblings       EXPORTED
   DATA OwnerDraw          EXPORTED  INIT .F.
   DATA StaticEdge         EXPORTED
   DATA Transparent        EXPORTED

   DATA HorzScrollSize     EXPORTED  INIT 0
   DATA VertScrollSize     EXPORTED  INIT 0
   DATA AcceptFiles        EXPORTED  INIT .F.
   DATA AnimationStyle     EXPORTED  INIT 0
   DATA ContextMenu        EXPORTED
   DATA AllowMaximize      EXPORTED  INIT .F.
   DATA Enabled            EXPORTED  INIT .T.
   DATA TabOrder           EXPORTED  INIT 0  
   DATA NoActivate         EXPORTED  INIT .F.
   DATA Theming            EXPORTED  INIT .F.

   DATA Color              PUBLISHED INIT GetSysColor( COLOR_BTNSHADOW )

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
   ::Style         := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
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
   IF ::hWnd != NIL .AND. ::xSunken != lSet
      ::Weight := IIF( lSet, 2, 1 )
      IF ::xVertical
         ::Width := ::Weight
       ELSE
         ::Height := ::Weight
      ENDIF
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
   IF ::__ClassInst != NIL
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
