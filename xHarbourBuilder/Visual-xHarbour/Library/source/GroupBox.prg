/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// GroupBox.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

#define BP_GROUPBOX              4
#define TMT_CAPTIONTEXT 1610
#define GBS_NORMAL   1

/* Color rendering properties */
#define TMT_BORDERCOLOR             3801
#define TMT_FILLCOLOR               3802
#define TMT_TEXTCOLOR               3803
#define TMT_EDGELIGHTCOLOR          3804
#define TMT_EDGEHIGHLIGHTCOLOR      3805
#define TMT_EDGESHADOWCOLOR         3806
#define TMT_EDGEDKSHADOWCOLOR       3807
#define TMT_EDGEFILLCOLOR           3808
#define TMT_TRANSPARENTCOLOR        3809
#define TMT_GRADIENTCOLOR1          3810
#define TMT_GRADIENTCOLOR2          3811
#define TMT_GRADIENTCOLOR3          3812
#define TMT_GRADIENTCOLOR4          3813
#define TMT_GRADIENTCOLOR5          3814
#define TMT_SHADOWCOLOR             3815
#define TMT_GLOWCOLOR               3816
#define TMT_TEXTBORDERCOLOR         3817
#define TMT_TEXTSHADOWCOLOR         3818
#define TMT_GLYPHTEXTCOLOR          3819
#define TMT_GLYPHTRANSPARENTCOLOR   3820
#define TMT_FILLCOLORHINT           3821
#define TMT_BORDERCOLORHINT         3822
#define TMT_ACCENTCOLORHINT         3823


//-----------------------------------------------------------------------------------------------

CLASS GroupBox INHERIT Control
   PROPERTY ImageList   GET __ChkComponent( Self, @::xImageList )

   DATA ImageIndex PROTECTED

   ACCESS __SysForeColor INLINE ::GetSysColor()


   METHOD Init()  CONSTRUCTOR
   METHOD OnEraseBkGnd()       INLINE 1
   METHOD OnPaint()
   METHOD OnSize(w,l)          INLINE ::__CreateBkBrush(), Super:OnSize(w,l), ::InvalidateRect(), NIL
   METHOD GetSysColor()
   METHOD SetWindowText(cText) INLINE Super:SetWindowText(cText), ::InvalidateRect()
   METHOD __CreateBkBrush()
   METHOD Refresh()            INLINE ::__CreateBkBrush(), Super:Refresh()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS GroupBox
   ::__xCtrlName := "GroupBox"
   ::ClsName   := "GroupBox"
   DEFAULT ::Style TO (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::Super:Init( oParent )
   ::Width         := 150
   ::Height        := 100
   ::IsContainer   := .T.
   ::ControlParent := .T.
   ::__IsStandard  := .F.
   ::__IsControl   := .T.
   IF ::DesignMode
      ::__PropFilter := { "ALLOWMAXIMIZE", "ALLOWCLOSE", "ALLOWUNDOCK" }
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD GetSysColor() CLASS GroupBox
   LOCAL nColor
   IF ::Application:IsThemedXP .AND. ::Theming
      nColor := GetThemeColor( ::System:hButtonTheme, BP_GROUPBOX, GBS_NORMAL, TMT_TEXTCOLOR )
    ELSE
      nColor := GetSysColor( COLOR_BTNTEXT )
   ENDIF
RETURN nColor

//-----------------------------------------------------------------------------------------------
METHOD __CreateBkBrush() CLASS GroupBox
   LOCAL aRect, hDC, hMemBitmap, hOldBitmap, hMemDC, hBrush
   IF ::xBackColor == NIL
      ::GetClientRect()
      aRect      := _GetClientRect( ::Parent:hWnd )
      hDC        := GetDC( ::hWnd )

      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)

      hBrush     := ::Parent:BkBrush
      DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )

      SetBrushOrgEx( hMemDC, aRect[3]-::Left, aRect[4]-::Top )
      _FillRect( hMemDC, { 0, 0, ::ClientWidth, ::ClientHeight }, hBrush )

      IF ::BkBrush != NIL
         DeleteObject( ::BkBrush )
      ENDIF
      ::BkBrush   := CreatePatternBrush( hMemBitmap )

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )

      ReleaseDC( ::hWnd, hDC )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnPaint() CLASS GroupBox
   LOCAL hDC, hFont, hBrush, hMemDC, hMemBitmap, hOldBitmap, sz := (struct SIZE), aRect

   ::__CreateBkBrush()

   aRect      := { 0, 0, ::ClientWidth, ::ClientHeight }
   hBrush     := ::BkBrush

   hDC        := ::BeginPaint()

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   _FillRect( hMemDC, { 0, 0, ::Width, ::Height }, hBrush )

   hFont := SelectObject( hMemDC, ::Font:Handle )
   GetTextExtentPoint32( hMemDC, ::Text, @sz )

   aRect[2]    := sz:cy / 2
   IF ::Theming .AND. ::Application:IsThemedXP
      DrawThemeBackground( ::System:hButtonTheme, hMemDC, BP_GROUPBOX, 0, aRect, aRect )
    ELSE
      _DrawEdge( hMemDC, aRect, EDGE_ETCHED, BF_RECT )
   ENDIF

   IF ! Empty( ::Text )
      _FillRect( hMemDC, {7,0,sz:cx + 12,sz:cy}, hBrush )

      IF ::ForeColor != NIL
         SetTextColor( hMemDC, ::ForeColor )
      ENDIF
      aRect[2] := 0
      aRect[1] := 10

      SetBkMode( hMemDC, TRANSPARENT )
      _DrawText( hMemDC, ::Text, aRect, (DT_LEFT | DT_SINGLELINE) )
   ENDIF

   SelectObject( hMemDC, hFont )

   BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )

   ::EndPaint()
RETURN 0


