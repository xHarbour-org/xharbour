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
   PROPERTY Transparent SET ::__SetTransp(v) DEFAULT .F.
   PROPERTY ImageList   GET __ChkComponent( Self, @::xImageList )

   DATA ImageIndex PROTECTED

   ACCESS SysForeColor INLINE ::GetSysColor()

   
   METHOD Init()  CONSTRUCTOR
   METHOD Create()             INLINE IIF( ::Parent:__xCtrlName IN {"TabPage","GroupBox"} .AND. ! ::xTransparent, ::__SetTransp(.T.), ), Super:Create()
   METHOD OnDestroy()          INLINE Super:OnDestroy(), ::CloseThemeData(), NIL
   METHOD OnEraseBkGnd()       INLINE IIF( LEN( ::Children ) == 0, ::SetWindowPos( HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE ), ), 1
   METHOD OnPaint()
   METHOD OnSize(w,l)          INLINE Super:OnSize(w,l),::InvalidateRect(), NIL
   METHOD GetSysColor()
   METHOD SetWindowText(cText) INLINE Super:SetWindowText(cText), ::InvalidateRect()
   METHOD __SetTransp(lSet)    INLINE IIF( lSet, ::Parent:__RegisterTransparentControl( Self ), ::Parent:__UnregisterTransparentControl( Self ) )
ENDCLASS

METHOD Init( oParent ) CLASS GroupBox
   ::__xCtrlName := "GroupBox"
   ::ClsName   := "GroupBox"
   DEFAULT ::Style TO WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Super:Init( oParent )
   ::Width         := 150
   ::Height        := 100
   ::IsContainer   := .T.
   ::ControlParent := .T.
   ::__IsStandard  := .F.
   ::__IsControl   := .T.
   ::ThemeName     := "button"
   ::OpenThemeData()
   IF ::__ClassInst != NIL
      ::__PropFilter := { "ALLOWMAXIMIZE", "ALLOWCLOSE", "ALLOWUNDOCK" }
   ENDIF
RETURN Self

METHOD GetSysColor() CLASS GroupBox
   LOCAL nColor
   IF ::Application:IsThemedXP .AND. ::Theming
      nColor := GetThemeColor( ::hTheme, BP_GROUPBOX, GBS_NORMAL, TMT_TEXTCOLOR )
    ELSE
      nColor := GetSysColor( COLOR_BTNTEXT )
   ENDIF
RETURN nColor

METHOD OnPaint( hDC, hMemDC ) CLASS GroupBox
   LOCAL lDC, hFont, hBrush, hMemBitmap, hOldBitmap, rc := (struct RECT), rcalc := (struct RECT), sz := (struct SIZE)
   IF !::IsWindow()
      RETURN 0
   ENDIF

   hBrush := ::GetBkBrush()

   rc:left   := 0
   rc:top    := 0
   rc:right  := ::Width
   rc:bottom := ::Height

   lDC := ( hDC == NIL .OR. hDC == 0 )
   IF lDC
      hDC := ::BeginPaint()
   ENDIF

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   FillRect( hMemDC, rc, hBrush )

   hFont := SelectObject( hMemDC, ::Font:Handle )
   GetTextExtentPoint32( hMemDC, ::Text, @sz )

   rc:top    := sz:cy / 2 
   IF ::Theming .AND. ::Application:IsThemedXP
      DrawThemeBackground( ::hTheme, hMemDC, BP_GROUPBOX, 0, rc:Array, rc:Array )
    ELSE
      DrawEdge( hMemDC, rc, EDGE_ETCHED, BF_RECT )
   ENDIF

   IF ! Empty( ::Text )
      rcalc:left   := 7
      rcalc:top    := 0
      rcalc:right  := sz:cx + 12
      rcalc:bottom := sz:cy

      FillRect( hMemDC, rcalc, hBrush )

      IF ::ForeColor != NIL
         SetTextColor( hMemDC, ::ForeColor )
      ENDIF
      rc:top    := 0
      rc:left   := 10

      SetBkMode( hMemDC, TRANSPARENT )
      DrawText( hMemDC, ::Text, rc, DT_LEFT | DT_SINGLELINE )
   ENDIF
   SelectObject( hMemDC, hFont )
  
   BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )

   ::__SetTransparentChildren( hDC, hMemDC )

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )

   IF lDC
      ::EndPaint()
   ENDIF
RETURN 0


