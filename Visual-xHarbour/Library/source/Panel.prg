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

CLASS Panel FROM TitleControl
   PROPERTY VertScroll                                  DEFAULT .F.
   PROPERTY HorzScroll                                  DEFAULT .F.
   PROPERTY ScrollOnChildFocus                          DEFAULT .F.
   PROPERTY Transparent                                 DEFAULT .F.
   PROPERTY VertScrollSize SET ::__SetVertScrollSize(v) DEFAULT 0
   PROPERTY HorzScrollSize SET ::__SetHorzScrollSize(v) DEFAULT 0
   PROPERTY ImageList      GET __ChkComponent( Self, @::xImageList )

   DATA oLastFocus EXPORTED

   DATA ImageIndex PROTECTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnLButtonUp() INLINE IIF( HGetPos( ::EventHandler, "OnClick" ) != 0, ::Form:&( ::EventHandler[ "OnClick" ] )( Self ), )

   METHOD OnEraseBkGnd()
   METHOD ResetFrame() INLINE ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER)
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS Panel
   DEFAULT ::__xCtrlName TO "Panel"
   ::ClsName      := "PanelBox"
   ::Style        := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ExStyle      := WS_EX_CONTROLPARENT
   ::Super:Init( oParent )
   ::Width        := 80
   ::Height       := 80
   ::__IsStandard   := .F.
   IF ! ::__ClassInst != NIL
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
   ENDIF
   ::IsContainer  := .T.
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS Panel
   Super:Create()
   IF ::VertScrollSize > 0
      ::OriginalRect[4] := ::VertScrollSize
    ELSE
      ::VertScrollSize := ::ClientHeight
      IF ::__ClassInst != NIL
         ::__ClassInst:VertScrollSize := ::ClientHeight
      ENDIF
   ENDIF
   IF ::HorzScrollSize > 0
      ::OriginalRect[3] := ::HorzScrollSize
    ELSE
      ::HorzScrollSize := ::ClientWidth
      IF ::__ClassInst != NIL
         ::__ClassInst:HorzScrollSize := ::ClientWidth
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnEraseBkGnd( hDC ) CLASS Panel
   IF ::Transparent
      IF ::__hBrush != NIL
         _FillRect( hDC, _GetClientRect( ::hWnd ), ::__hBrush )
         RETURN 1
      ENDIF
   ENDIF
RETURN NIL

