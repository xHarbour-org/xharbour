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
   METHOD OnLButtonUp() INLINE IIF( HGetPos( ::EventHandler, "OnClick" ) != 0, ::Form:&( ::EventHandler[ "OnClick" ] )( Self ), )

   METHOD OnEraseBkGnd()
   METHOD OnGetDlgCode()  INLINE dview("OnGetDlgCode")
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
