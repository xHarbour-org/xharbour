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
   DATA ImageIndex PROTECTED
   DATA Transparent PUBLISHED INIT .F.
   
   ACCESS ForeSysColor INLINE ::GetSysColor()

   PROPERTY ImageList  GET __ChkComponent( Self, ::xImageList )
   
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD __WindowDestroy()    INLINE ::Super:__WindowDestroy(), ::CloseThemeData(), Self
   METHOD OnEraseBkGnd()       INLINE IIF( LEN( ::Children ) == 0, ::SetWindowPos( HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE ), ), 1
   METHOD OnPaint()
   METHOD OnSize()             INLINE ::InvalidateRect(), NIL
   METHOD GetSysColor()
   METHOD SetWindowText(cText) INLINE Super:SetWindowText(cText), ::InvalidateRect()
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
   ::CaptionHeight := 0
   ::ThemeName     := "button"
   ::OpenThemeData()
   IF ::__ClassInst != NIL
      ::__PropFilter := { "HIGHLIGHTCAPTION", "SMALLCAPTION", "ALLOWMAXIMIZE", "ALLOWCLOSE", "ALLOWUNDOCK" }
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

METHOD Create() CLASS GroupBox
   ::Super:Create()
RETURN Self

METHOD OnPaint( hDC, hMemDC ) CLASS GroupBox
   LOCAL oChild, hFont, hMemDC1, hOldBitmap1, hBrush, hMemBitmap, hOldBitmap, rc := (struct RECT), rcalc := (struct RECT), sz := (struct SIZE)
   IF !::IsWindow()
      RETURN 0
   ENDIF
   hBrush := ::BkBrush
   DEFAULT hBrush TO ::Parent:BkBrush
   DEFAULT hBrush TO ::__hBrush

   rc:left   := 0
   rc:top    := 0
   rc:right  := ::Width
   rc:bottom := ::Height

   IF hDC != NIL
      hMemDC     := CreateCompatibleDC( hDC )
      IF hBrush == NIL .AND. ::Parent:__xCtrlName == "TabPage" .AND. GetParent( ::hWnd ) == ::Parent:hWnd
         hMemBitmap := CreateCompatibleBitmap( hDC, ::Parent:ClientWidth, ::Parent:ClientHeight )
         hOldBitmap := SelectObject( hMemDC, hMemBitmap)
         SendMessage( ::Parent:hWnd, WM_PRINT, hMemDC, PRF_CLIENT | PRF_ERASEBKGND )
         BitBlt( hDC, ::Left, ::Top, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )
       ELSE
         hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
         hOldBitmap := SelectObject( hMemDC, hMemBitmap)
         DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )
         FillRect( hMemDC, rc, hBrush )
      ENDIF
   ENDIF

   hFont := SelectObject( hMemDC, ::Font:Handle )

   GetTextExtentPoint32( hMemDC, ::Caption, @sz )

   rc:top    := sz:cy / 2 
   IF ::Theming .AND. ::Application:IsThemedXP
      DrawThemeBackground( ::hTheme, hMemDC, BP_GROUPBOX, 0, rc:Array, rc:Array )
    ELSE
      DrawEdge( hMemDC, rc, EDGE_ETCHED, BF_RECT )
   ENDIF
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


   DrawText( hMemDC, ::Caption, rc, DT_LEFT | DT_SINGLELINE )
   SelectObject( hMemDC, hFont )
   IF hMemBitmap != NIL
      FOR EACH oChild IN ::Children
          IF oChild:__hBrush != NIL
             DeleteObject( oChild:__hBrush )
          ENDIF

          DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )

          hMemDC1      := CreateCompatibleDC( hDC )
          hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )

          BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin, SRCCOPY )

          oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )

          SelectObject( hMemDC1,  hOldBitmap1 )
          DeleteDC( hMemDC1 )

      NEXT
   ENDIF

   IF hDC != NIL
      BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )

      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF
RETURN 0


