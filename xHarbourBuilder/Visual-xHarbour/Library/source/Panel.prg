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

   ACCESS bChanged INLINE ::Parent:bChanged

   DATA oLastFocus EXPORTED

   DATA ImageIndex PROTECTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnEraseBkGnd()
   METHOD ResetFrame() INLINE ::SetWindowPos(,0,0,0,0,(SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER))
   METHOD __CreateBkBrush()
   METHOD OnGetDlgCode()     INLINE (DLGC_WANTMESSAGE | DLGC_WANTALLKEYS)
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS Panel
   DEFAULT ::__xCtrlName TO "Panel"
   ::ClsName      := "PanelBox"
   ::Style        := (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::ExStyle      := WS_EX_CONTROLPARENT
   ::Super:Init( oParent )
   ::xWidth       := 80
   ::xHeight      := 80
   ::__IsStandard := .F.
   IF ! ::DesignMode
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
      IF ::DesignMode
         __SetInitialValues( Self, "VertScrollSize", ::ClientHeight )
      ENDIF
   ENDIF
   IF ::HorzScrollSize > 0
      ::OriginalRect[3] := ::HorzScrollSize
    ELSE
      ::HorzScrollSize := ::ClientWidth
      IF ::DesignMode
         __SetInitialValues( Self, "HorzScrollSize", ::ClientWidth )
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __CreateBkBrush( hDC ) CLASS Panel
   LOCAL hMemBitmap, hOldBitmap, hMemDC, nBorder, nLeftBorder
   IF ::xBackColor == NIL .AND. ::Transparent
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
      nBorder    := (::Height - ( ::ClientHeight + IIF( ! Empty(::Text), ::TitleHeight, 0 ) ) ) / 2
      nLeftBorder:= (::Width-::ClientWidth)/2

      SetBrushOrgEx( hMemDC, ::Parent:ClientWidth-::Left-nLeftBorder, ::Parent:ClientHeight-::Top-IIF( ! Empty(::Text), ::TitleHeight, 0 )-nBorder )
      _FillRect( hMemDC, { 0, 0, ::ClientWidth, ::ClientHeight }, IIF( ::Parent:BkBrush != NIL, ::Parent:BkBrush, GetSysColorBrush( COLOR_BTNFACE ) ) )

      IF ::BkBrush != NIL
         DeleteObject( ::BkBrush )
      ENDIF
      ::BkBrush   := CreatePatternBrush( hMemBitmap )

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnEraseBkGnd( hDC ) CLASS Panel
   ::__CreateBkBrush( hDC )
   _FillRect( hDC, { 0, 0, ::Width, ::Height }, ::BkBrush )
RETURN 1

