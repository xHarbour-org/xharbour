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
   LOCAL hBkGnd := ::BkBrush
   DEFAULT hBkGnd TO ::Parent:BkBrush

   IF ::ForeColor != NIL
      SetTextColor( nwParam, ::ForeColor )
   ENDIF

   IF ::__hBrush != NIL
      SetBkMode( nwParam, TRANSPARENT )
      RETURN ::__hBrush
   ENDIF

   IF ::Parent:ClsName == "ToolBarWindow32" .OR. ::Transparent
      SetBkMode( nwParam, TRANSPARENT )
      RETURN GetStockObject( NULL_BRUSH )
   ENDIF

   IF hBkGnd != NIL .OR. ::Transparent
      SetBkMode( nwParam, TRANSPARENT )
      RETURN hBkGnd
    ELSEIF ::ForeColor != NIL .AND. ::ForeColor != ::ForeSysColor
      SetBkMode( nwParam, TRANSPARENT )
      IF ::BackColor == ::BackSysColor
         RETURN GetSysColorBrush( COLOR_BTNFACE )
      ENDIF
      RETURN GetStockObject( NULL_BRUSH )
   ENDIF
RETURN NIL

