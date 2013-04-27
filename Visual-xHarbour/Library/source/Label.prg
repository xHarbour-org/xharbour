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
   PROPERTY Sunken  READ xSunken     WRITE __SetSunken  DEFAULT .T. PROTECTED
   DATA xText      EXPORTED INIT ""
   DATA Text       EXPORTED INIT ""
   DATA xHeight    EXPORTED INIT 2

   DATA Border             EXPORTED
   DATA IndexOrder         EXPORTED  INIT 0

   DATA Font               EXPORTED
   DATA ToolTip            EXPORTED
   DATA Id                 EXPORTED
   DATA BackColor          EXPORTED
   DATA ForeColor          EXPORTED
   DATA AllowClose         EXPORTED
   DATA AllowUndock        EXPORTED
   DATA Dock               EXPORTED
   DATA Width              EXPORTED
   DATA Height             EXPORTED
   DATA ClientEdge         EXPORTED
   DATA ClipChildren       EXPORTED
   DATA ClipSiblings       EXPORTED
   DATA OwnerDraw          EXPORTED  INIT .F.
   DATA StaticEdge         EXPORTED
   DATA Transparent        EXPORTED
   DATA Visible            EXPORTED

   ACCESS Height  INLINE ::xHeight
   
   METHOD Init() CONSTRUCTOR
   METHOD __SetSunken(l) INLINE ::SetStyle( WS_BORDER, ! l ),;
                                ::SetStyle( SS_SUNKEN, l ),;
                                ::xHeight := IIF( l, 2, 1 ),;
                                ::MoveWindow(),;
                                ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER),;
                                ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT ),;
                                ::UpdateWindow()
ENDCLASS

METHOD Init( oParent ) CLASS Line
   DEFAULT ::__xCtrlName TO "Label"
   ::ClsName    := "static"
   ::Style      := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | SS_SUNKEN
   ::__lResizeable := {.F.,.T.,.F.,.F.,.F.,.T.,.F.,.F.}
   ::Super:Init( oParent )
   ::Width      := 150
   ::Events     := ;
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
