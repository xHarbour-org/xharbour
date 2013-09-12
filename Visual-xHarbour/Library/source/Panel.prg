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
   PROPERTY VertScrollSize READ xVertScrollSize WRITE __SetVertScrollSize DEFAULT 0
   PROPERTY HorzScrollSize READ xHorzScrollSize WRITE __SetHorzScrollSize DEFAULT 0

   DATA xImageList     EXPORTED
   ACCESS ImageList    INLINE ::xImageList PERSISTENT
   ASSIGN ImageList(o) INLINE ::xImageList := __ChkComponent( Self, o )

   DATA ImageIndex PROTECTED
   DATA oLastFocus EXPORTED
   //DATA Border     EXPORTED INIT .F.
   DATA Transparent PUBLISHED INIT .F.

   DATA VertScroll              INIT .F. PUBLISHED
   DATA HorzScroll              INIT .F. PUBLISHED

   DATA ScrollOnChildFocus      PUBLISHED INIT .F.

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnLButtonUp() INLINE IIF( HGetPos( ::EventHandler, "OnClick" ) != 0, ::Form:&( ::EventHandler[ "OnClick" ] )( Self ), )

   METHOD OnEraseBkGnd()
   METHOD OnGetDlgCode()  INLINE dview("OnGetDlgCode")
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

