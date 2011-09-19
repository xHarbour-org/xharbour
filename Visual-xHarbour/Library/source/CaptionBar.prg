/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// CaptionBar.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"


//-----------------------------------------------------------------------------------------------

CLASS CaptionBar INHERIT Control
   DATA TextWidth  PROTECTED
   DATA lLeave     PROTECTED INIT .T.
   DATA Pushed     PROTECTED INIT .F.
   DATA Icon       EXPORTED
   DATA IconSize   EXPORTED
   METHOD Init()    CONSTRUCTOR
   METHOD Create()
   METHOD OnPaint()
   METHOD OnEraseBkGnd() INLINE 1
   METHOD OnMouseMove()
   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD PaintText()
   METHOD OnNCHitTest()
   METHOD __WindowDestroy()
   METHOD OnTimer()
   METHOD OnMove()  INLINE ::InvalidateRect(, .F.),0
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight, nStyle, lCreate ) CLASS CaptionBar

   DEFAULT lCreate TO .F.

   DEFAULT ::__xCtrlName TO "CaptionBar"
   ::ClsName   := "CaptionBar"
   ::ClassStyle:= NIL

   ::Super:Init( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight, nStyle )
   ::Style     := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS

   ::__IsStandard:= .F.
   ::ClassStyle:=CS_VREDRAW+CS_HREDRAW
   ::Left      := 0
   ::Top       := 0
   ::Width     := 50
   ::Height    := 23
   ::Font:Height         := -MulDiv(14, ::Parent:Drawing:GetDeviceCaps( LOGPIXELSY ), 72 )
   ::Font:PitchAndFamily := VARIABLE_PITCH + FF_SWISS
   ::Font:Weight         := FW_BOLD
   ::Font:Quality        := ANTIALIASED_QUALITY
   
   ::ClassBrush:= GetSysColorBrush( COLOR_BTNSHADOW )
   ::ForeColor := ::System:Colors:Window
   
   ::LeftMargin:= 5
   IF lCreate
      ::Create()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS CaptionBar
   IF VALTYPE( ::Icon ) == "C"
      ::Icon := LoadIcon( ::AppInstance, ::Icon )
   ENDIF
   IF ::Icon != NIL
      ::IconSize := __GetIconSize( ::Icon )
      ::Height := MAX( ::IconSize[2], ::Height )
      ::LeftMargin += ::IconSize[1]+5
   ENDIF
  ::Super:Create()
RETURN Self

//-----------------------------------------------------------------------------------------------------

METHOD PaintText() CLASS CaptionBar
   ::Drawing:FillRect()
   ::GetClientRect()
   ::Drawing:SelectObject( ::Font:Handle )
   ::Drawing:SetBkMode( TRANSPARENT )
   ::Drawing:SetTextColor( ::ForeColor )
   ::Drawing:DrawText( ::Caption, {::LeftMargin,0,::ClientWidth,::ClientHeight}, DT_SINGLELINE + DT_LEFT + DT_VCENTER )
   DEFAULT ::TextWidth TO ::Drawing:GetTextExtentPoint32( ::Caption )[1]
RETURN Self

//-----------------------------------------------------------------------------------------------------

METHOD OnPaint() CLASS CaptionBar
   LOCAL y := (::Height/2)-(::IconSize[2]/2)
   ::PaintText()
   ::Drawing:DrawIcon( 5, 0, ::Icon )
   ::Drawing:DrawSpecialChar( {::LeftMargin+::TextWidth, 10, ::LeftMargin+::TextWidth+20,::ClientHeight}, ASC("u"), .F. )
   IF !::lLeave
      IF !::Pushed
         ::Drawing:Draw3dRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, GetSysColor(COLOR_3DHIGHLIGHT), GetSysColor(COLOR_3DDKSHADOW))
       ELSE
         ::Drawing:Draw3dRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, GetSysColor(COLOR_3DDKSHADOW), GetSysColor(COLOR_3DHIGHLIGHT) )
      ENDIF
   ENDIF
   //::UpdateWindow()
RETURN 0

//-----------------------------------------------------------------------------------------------------

METHOD OnNCHitTest(x,y)
   LOCAL aPt := {x,y}
   _ScreenToClient( ::hWnd, @aPt )
   IF !_PtInRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, aPt )
      RETURN HTCLIENT
   ENDIF
   ::SetTimer( 1, 100 )
RETURN NIL

//-----------------------------------------------------------------------------------------------------

METHOD OnTimer() CLASS CaptionBar
   LOCAL aPt, nPos
   ::KillTimer( 1 )

   aPt := _GetCursorPos()
   _ScreenToClient( ::hWnd, @aPt )
   IF !_PtInRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, aPt )
      ::Pushed := .F.
      ::InvalidateRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, .F. )
      ::lLeave := .T.
      ::UpdateWindow()
    ELSE
      nPos := GetMessagePos()
      ::SendMessage( WM_NCHITTEST, 0, nPos )
   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------------

METHOD OnMouseMove(nwParam,x) CLASS CaptionBar
   (nwParam)
   IF x <= ::LeftMargin+::TextWidth+20
      IF !::Pushed
         ::Drawing:Draw3dRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, GetSysColor(COLOR_3DHIGHLIGHT), GetSysColor(COLOR_3DDKSHADOW))
       ELSE
         ::Drawing:Draw3dRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, GetSysColor(COLOR_3DDKSHADOW), GetSysColor(COLOR_3DHIGHLIGHT) )
      ENDIF
      ::lLeave := .F.
    ELSEIF !::lLeave
      ::Pushed := .F.
      ::InvalidateRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, .F. )
      ::lLeave := .T.
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------------

METHOD OnLButtonDown(nwParam,x) CLASS CaptionBar
   (nwParam)
   IF x <= ::LeftMargin+::TextWidth+20
      ::Pushed := !::Pushed
      ::InvalidateRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, .F. )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------------

METHOD OnLButtonUp() CLASS CaptionBar
   ::InvalidateRect( {0, 0, ::LeftMargin+::TextWidth+20,::ClientHeight}, .F. )
RETURN NIL

//-----------------------------------------------------------------------------------------------------

METHOD __WindowDestroy() CLASS CaptionBar
  ::Super:__WindowDestroy()
   IF ::Icon != NIL
      DeleteObject( ::Icon )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------------

