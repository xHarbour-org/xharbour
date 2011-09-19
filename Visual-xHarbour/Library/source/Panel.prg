/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Panel.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"

CLASS Panel FROM Control
   DATA xImageList     EXPORTED
   ACCESS ImageList    INLINE ::xImageList PERSISTENT
   ASSIGN ImageList(o) INLINE ::xImageList := __ChkComponent( Self, o )

   DATA ImageIndex PROTECTED
   DATA oLastFocus EXPORTED
   DATA Border     EXPORTED INIT .F.
   DATA Transparent PUBLISHED INIT .F.

   PROPERTY SmallCaption                         READ xSmallCaption WRITE __SetSmallCaption DEFAULT .T. PROTECTED

   DATA VertScroll              INIT .F. PUBLISHED
   DATA HorzScroll              INIT .F. PUBLISHED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnLButtonUp() INLINE IIF( HGetPos( ::EventHandler, "OnClick" ) != 0, ::Form:&( ::EventHandler[ "OnClick" ] )( Self ), )

   METHOD OnSetFocus()// INLINE IIF( !EMPTY( ::Children ), (::Children[1]:SetFocus(),0), NIL )
   METHOD OnEraseBkGnd()
ENDCLASS

METHOD OnEraseBkGnd( hDC ) CLASS Panel
   IF ::Transparent
      IF ::__hBrush != NIL
         _FillRect( hDC, _GetClientRect( ::hWnd ), ::__hBrush )
         RETURN 1
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Panel
   DEFAULT ::__xCtrlName TO "Panel"
   ::ClsName      := "PanelBox"
   ::StaticEdge   := .F.
   ::Style        := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Super:Init( oParent )
   ::Width        := 80
   ::Height       := 80
   ::__IsStandard   := .F.
   __DeleteEvents( ::Events,{ /*"OnClick",*/;
                              "OnCtlColorBtn",;
                              "OnCtlColorEdit",;
                              "OnCtlColorListBox",;
                              "OnCtlColorScrollBar",;
                              "OnCtlColorStatic",;
                              "OnSysColorChange",;
                              "OnClear",;
                              "OnCopy",;
                              "OnCut",;
                              "OnPaste",;
                              "OnUndo" } )

RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS Panel
   ::ControlParent := .T.
   ::Super:Create()
   ::IsContainer  := .T.
   IF !EMPTY( ::Caption ) .AND. ::SmallCaption
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
   ENDIF
RETURN Self

METHOD OnSetFocus( nwParam ) CLASS Panel
   LOCAL n
   IF ::__Docked .AND. !EMPTY( ::Children )
      IF ::oLastFocus != NIL
         ::oLastFocus:SetFocus()
       ELSE
         IF ( n := ASCAN( ::Children, {|o| o:hWnd == nwParam} ) ) == 0
            n := 1
         ENDIF
         ::Children[n]:SetFocus()
      ENDIF
      RETURN 0
   ENDIF
RETURN NIL

