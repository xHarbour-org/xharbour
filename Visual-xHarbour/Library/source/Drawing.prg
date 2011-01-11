/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Drawing.prg                                                                                          *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

//------------------------------------------------------------------------------------------------

CLASS Drawing
   DATA Parent      EXPORTED
   DATA xhDC        EXPORTED
   DATA cPaint      EXPORTED
   DATA __aFonts     PROTECTED   
   
   ACCESS hDC       INLINE IIF( ::xhDC == NIL, ::RefreshDC(),), ::xhDC

   METHOD Init() CONSTRUCTOR
   
   METHOD Destroy()
   METHOD RefreshDC()
   METHOD GetTextExtentPoint32()
   METHOD GetTextExtentExPoint()
   METHOD DrawFrameControl()
   METHOD FillRect()
   METHOD Rectangle()
   METHOD EnumFonts()
   METHOD EnumProc()
   METHOD BeginPaint()
   METHOD GetDC()                                        INLINE ::xhDC := GetDC( ::Parent:hWnd ), Self
   METHOD ReleaseDC()                                    INLINE ReleaseDC( ::Parent:hWnd, ::xhDC ),  ::xhDC := NIL, Self
   METHOD EndPaint()                                     INLINE _EndPaint( ::Parent:hWnd, ::cPaint), ::xhDC := NIL, ::cPaint := NIL, Self
   METHOD SelectObject( hObj )                           INLINE SelectObject( ::hDC, hObj )
   METHOD SetPixel( x, y, nColor )                       INLINE SetPixel( ::hDC, x, y, nColor )
   METHOD GetPixel( x, y )                               INLINE GetPixel( ::hDC, x, y )
   METHOD SetBkColor( nColor )                           INLINE SetBkColor( ::hDC, nColor )
   METHOD SetTextColor( nColor )                         INLINE SetTextColor( ::hDC, nColor )
   METHOD SetTextAlign( nAlign )                         INLINE SetTextAlign( ::hDC, nAlign )
   METHOD ExtTextOut( x, y, nFlags, aRect, cText, aDx )  INLINE _ExtTextOut( ::hDC, x, y, nFlags, aRect, cText, aDx )
   METHOD PolyLine( aReg )                               INLINE _PolyLine( ::hDC, aReg )
   METHOD DrawFocusRect( aRect )                         INLINE _DrawFocusRect( ::hDC, aRect )
   METHOD GetClipBox()
   METHOD GetDeviceCaps( nFlags )                        INLINE GetDeviceCaps( ::hDC, nFlags )
   METHOD SetBkMode( nMode )                             INLINE SetBkMode( ::hDC, nMode )
   METHOD DrawText( cText, aRect, nFlags )               INLINE _DrawText( ::hDC, cText, aRect, nFlags )
   METHOD GetTextMetrics()
   METHOD DrawEdge( aRect, nFlags, nState )              INLINE _DrawEdge( ::hDC, aRect, nFlags, nState )
   METHOD DrawThemeParentBackground( aRect )             INLINE DrawThemeParentBackground( ::Parent:hWnd, ::hDC, aRect )
   METHOD DrawIcon( nLeft, nTop, hIcon )                 INLINE DrawIcon( ::hDC, nLeft, nTop, hIcon )
   METHOD DrawSpecialChar( aRect, nSign, lBold, nPoint ) INLINE __DrawSpecialChar( ::hDC, aRect, nSign, lBold, nPoint )

   METHOD DrawThemeBackground( hTheme, nPartId, nStateId, aRect, aClipRect ) INLINE DrawThemeBackground( hTheme, ::hDC, nPartId, nStateId, aRect, aClipRect )
   METHOD Draw3DRect( rcItem, oTopLeftColor, oBottomRightColor )             INLINE __Draw3DRect( ::hDC, rcItem, oTopLeftColor, oBottomRightColor )
ENDCLASS

//------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Drawing
   ::Parent := oParent
RETURN Self

//------------------------------------------------------------------------------------------------

METHOD RefreshDC() CLASS Drawing
   IF ::xhDC == NIL
      ::GetDC()
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------

METHOD GetTextExtentPoint32( cText ) CLASS Drawing
   LOCAL hFont, aExt
   DEFAULT cText TO ::Parent:Caption
   hFont := SelectObject( ::hDC, ::Parent:Font:Handle )
   aExt := _GetTextExtentPoint32( ::hDC, cText )
   ::SelectObject( hFont )
RETURN aExt

//------------------------------------------------------------------------------------------------

METHOD GetTextExtentExPoint( cText, nMaxWidth, nFit ) CLASS Drawing
   LOCAL hFont, aExt
   hFont := SelectObject( ::hDC, ::Parent:Font:Handle )
   aExt := _GetTextExtentExPoint( ::hDC, cText, nMaxWidth, @nFit )
   ::SelectObject( hFont )
RETURN aExt

//------------------------------------------------------------------------------------------------

METHOD GetTextMetrics() CLASS Drawing
   LOCAL cBuffer, tm := (struct TEXTMETRIC)
   cBuffer := GetTextMetrics( ::hDC, @tm )
RETURN tm

//------------------------------------------------------------------------------------------------

METHOD GetClipBox( aRect ) CLASS Drawing
   LOCAL nRet
   DEFAULT aRect TO {,,,}
   nRet := GetClipBox( ::hDC, @aRect )
RETURN nRet

//------------------------------------------------------------------------------------------------

METHOD Destroy() CLASS Drawing
   IF ::xhDC != NIL .AND. ::cPaint == NIL
      ::ReleaseDC()
      ::xhDC := NIL
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------

METHOD BeginPaint() CLASS Drawing
   LOCAL cPaint
   ::Destroy()
   ::xhDC := _BeginPaint( ::Parent:hWnd, @cPaint )
   ::cPaint := cPaint
RETURN Self

//------------------------------------------------------------------------------------------------

METHOD FillRect( aRect, hBrush ) CLASS Drawing
   DEFAULT aRect  TO { 0, 0, ::Parent:Width, ::Parent:Height }
   DEFAULT hBrush TO IIF( ::Parent:BkBrush != NIL, ::Parent:BkBrush, ::Parent:ClassBrush )
   _FillRect( ::hDC, aRect, hBrush )
RETURN Self

//------------------------------------------------------------------------------------------------

METHOD Rectangle( nLeft, nTop, nRight, nBottom, nColor, hBrush, nPen ) CLASS Drawing
   LOCAL hOldBrush, hOldPen, hPen, aRect := ::Parent:GetRectangle()

   DEFAULT nLeft     TO aRect[1]
   DEFAULT nTop      TO aRect[2]
   DEFAULT nRight    TO aRect[3]
   DEFAULT nBottom   TO aRect[4]
   
   IF nColor != NIL
      DEFAULT nPen TO 1
      hPen    := CreatePen( PS_SOLID, nPen, nColor )
      hOldPen := SelectObject( ::hDC, hPen )
   ENDIF
   
   IF hBrush != NIL
      hOldBrush := SelectObject( ::hDC, hBrush )
   ENDIF

   Rectangle( ::hDC, nLeft, nTop, nRight, nBottom )
   
   IF hOldBrush != NIL
      SelectObject( ::hDC, hOldBrush )
   ENDIF
   IF hOldPen != NIL
      SelectObject( ::hDC, hOldPen )
      DeleteObject( hPen )
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------

METHOD DrawFrameControl( aRect, nStyle, nFlags ) CLASS Drawing
   DEFAULT aRect  TO { 0, 0, ::Parent:Width, ::Parent:Height }
   _DrawFrameControl( ::hDC, aRect, nStyle, nFlags )
RETURN Self


METHOD EnumFonts( cFaceName ) CLASS Drawing
   ::__aFonts := {}
   EnumFonts( ::hDC, cFaceName, WinCallBackPointer( HB_ObjMsgPtr( Self, "EnumProc" ), Self ), NIL )
RETURN ::__aFonts

METHOD EnumProc( plf, ptm, nType, nlParam ) CLASS Drawing
   LOCAL lf, tm
   lf := (struct LOGFONT*) plf
   tm := (struct TEXTMETRIC*) ptm
   AADD( ::__aFonts, { lf, tm } )
RETURN 1

