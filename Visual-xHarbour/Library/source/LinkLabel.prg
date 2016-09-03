/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Link.prg                                                                                             *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

//-----------------------------------------------------------------------------------------------

CLASS LinkLabel INHERIT Control

   DATA Group         INIT .F.
   DATA OwnerDraw     INIT .F.
   DATA DefaultButton INIT .F.
   DATA Cursor
   DATA ForeColor
   DATA AllowUnDock   INIT FALSE
   DATA AllowClose    INIT FALSE
   DATA __SysLinkColor  INIT RGB(0,0,255)
   DATA EnumAlignment EXPORTED  INIT { { "Left", "Center", "Right" }, {1,2,3} }

   PROPERTY Url
   PROPERTY ActiveLinkColor ROOT "Colors"         DEFAULT RGB(255,0,0)
   PROPERTY FocusRect                             DEFAULT .T.
   PROPERTY LinkVisited  SET ::InvalidateRect()   DEFAULT .F.
   PROPERTY LinkColor    ROOT "Colors" SET ::SetLinkColor(v)    DEFAULT RGB(0,0,255)
   PROPERTY VisitedColor ROOT "Colors" SET ::SetVisitedColor(v) DEFAULT RGB(128,0,128)
   PROPERTY AutoSize     SET ::SetWindowText(v)   DEFAULT .T.
   PROPERTY ImageIndex   SET ::SetImageIndex(v)   DEFAULT 0
   PROPERTY Alignment    SET ::Redraw()           DEFAULT 1
   PROPERTY SelBackColor ROOT "Colors" SET ::SetSelColor(v)

   DATA __lSelected     PROTECTED INIT .F.
   DATA __lFocused      PROTECTED INIT .F.

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD SetLinkColor()
   METHOD SetVisitedColor()
   METHOD SetWindowText()
   METHOD SetSelColor()
   METHOD OnLButtonDown()      INLINE ::__lSelected := .T., ::InvalidateRect(), NIL
   METHOD OnLButtonUp()        INLINE ::__lSelected := .F., ::InvalidateRect(), NIL
   METHOD OnSetFocus()         INLINE ::__lFocused  := .T., ::InvalidateRect(), NIL
   METHOD OnKillFocus()        INLINE ::__lFocused  := .F., ::InvalidateRect(), NIL
   METHOD OnEraseBkGnd( hDC )  INLINE ::PaintLabel( hDC ), 1
   METHOD OnPaint()
   METHOD SetParent( oParent ) INLINE ::Super:SetParent( oParent ), ::RedrawWindow( , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
   METHOD SetImageIndex()
   METHOD PaintLabel()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS LinkLabel
   ::__xCtrlName := "LinkLabel"
   ::Style := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_OWNERDRAW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::ClsName := "button"
   ::xCursor := IDC_HAND
   ::Super:Init( oParent )
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS LinkLabel
   LOCAL aSize

   Super:Create()

   //::Parent:__RegisterTransparentControl( Self )

   IF ::AutoSize
      ::__lResizeable   := {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
      aSize := ::Drawing:GetTextExtentPoint32( ::Text )
      ::xWidth := aSize[1]+4
      ::xHeight := aSize[2]+2
      IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
         ::xWidth += ::Parent:ImageList:IconWidth + 1
         ::xHeight := MAX( ::xHeight, ::Parent:ImageList:IconHeight )
      ENDIF
      ::MoveWindow()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetWindowText( cText ) CLASS LinkLabel
   LOCAL aSize
   IF VALTYPE( cText ) == "C"
      ::xText := cText
   ENDIF
   IF ::hWnd != NIL
      SetWindowText( ::hWnd, cText )
      IF ::AutoSize
         ::__lResizeable := {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}

         aSize := ::Drawing:GetTextExtentPoint32( cText )

         ::xWidth  := aSize[1]+4
         ::xHeight := aSize[2]+2

         IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
            ::xWidth += ::Parent:ImageList:IconWidth + 1
         ENDIF

         ::MoveWindow()
         IF ! (::Parent:ClsName IN {"StatusBarPanel"})
            ::__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight, NIL, .T. )
            ::Parent:DockControls()
         ENDIF
         ::UpdateWindow()

       ELSE
         ::__lResizeable   := {.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.}
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD PaintLabel( hDC ) CLASS LinkLabel
   LOCAL nColor, hBrush := ::BkBrush
   LOCAL rc := (struct RECT)

   IF hBrush == NIL .AND. ! (::Parent:ClsName IN {"StatusBarPanel"})
      hBrush := ::Parent:BkBrush
      IF hBrush != NIL
         SetBrushOrgEx( hDC, ::Parent:ClientWidth-::Left, ::Parent:ClientHeight-::Top )
      ENDIF
   ENDIF
   DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )
   _FillRect( hDC, {0,0,::Width,::Height}, hBrush )

   GetClientRect( ::hWnd, @rc )
   IF ! (::Parent:ClsName IN {"StatusBarPanel"}) .AND. ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
      IF ::__lFocused
         ::Parent:ImageList:DrawImage( hDC, ::ImageIndex, 0, 0, (ILD_TRANSPARENT | ILD_FOCUS), ::BackColor )
       ELSE
         ::Parent:ImageList:DrawImage( hDC, ::ImageIndex, 0, 0, ILD_TRANSPARENT )
      ENDIF
      rc:Bottom := MAX( rc:Bottom-rc:Left, ::Parent:ImageList:IconHeight )
      rc:Left   := ::Parent:ImageList:IconWidth + 1
   ENDIF

   SelectObject( hDC, ::Font:Handle )
   SetBkMode( hDC, TRANSPARENT )
   nColor := IIF( !::xLinkVisited, ::xLinkColor, ::xVisitedColor )
   nColor := IIF( ::__lSelected, ::ActiveLinkColor, nColor )

   IF ( ! ::IsWindowEnabled() .OR. ! ::Parent:IsWindowEnabled() ) .AND. ! (::Parent:ClsName IN {"StatusBarPanel"})
      nColor := ::System:Colors:GrayText
   ENDIF

   SetTextColor( hDC, nColor )
   rc:Left+=2

   DrawText( hDC, ::Text, rc, ((::Alignment-1)|DT_WORDBREAK) )
   IF ::__lFocused .AND. ::FocusRect
      rc:Left-=2
      DrawFocusRect( hDC, rc)
   ENDIF
RETURN NIL

METHOD OnPaint() CLASS LinkLabel
   LOCAL hDC, hMemDC, hMemBitmap, hOldBitmap

   hDC        := ::BeginPaint()

   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, ::Width, ::Height )
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)

   ::PaintLabel( hMemDC )

   BitBlt( hDC, 0, 0, ::Width, ::Height, hMemDC, 0, 0, SRCCOPY )

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )

   ::EndPaint()
RETURN 0

METHOD SetSelColor( nColor, lRepaint ) CLASS LinkLabel
   DEFAULT lRepaint TO TRUE
   ::xSelBackColor := nColor
   IF ::SelBkBrush != NIL
      DeleteObject( ::SelBkBrush )
      ::SelBkBrush := NIL
   ENDIF
   IF nColor != NIL
      ::SelBkBrush := CreateSolidBrush( nColor )
   ENDIF
   IF lRepaint .AND. ::IsWindowVisible() .AND. GetFocus() == ::hWnd
      ::InvalidateRect()
   ENDIF
RETURN SELF

METHOD SetLinkColor( nColor, lRepaint ) CLASS LinkLabel
   DEFAULT lRepaint TO .T.
   ::xForeColor := nColor
   IF ::IsWindowVisible()
      ::Redraw()
   ENDIF
RETURN SELF

METHOD SetVisitedColor( nColor, lRepaint ) CLASS LinkLabel
   DEFAULT lRepaint TO .T.
   ::xVisitedColor := nColor
   IF ::IsWindowVisible()
      ::Redraw()
   ENDIF
RETURN SELF

METHOD SetImageIndex() CLASS LinkLabel
   LOCAL aSize
   IF ::AutoSize
      ::__lResizeable   := {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
      aSize := ::Drawing:GetTextExtentPoint32( ::Text )
      ::xWidth := aSize[1]+4
      ::xHeight := aSize[2]+2
      IF ! (::Parent:ClsName IN {"StatusBarPanel"}) .AND. ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
         ::xWidth += ::Parent:ImageList:IconWidth + 1
         ::xHeight := MAX( ::xHeight, ::Parent:ImageList:IconHeight )
      ENDIF
      ::MoveWindow()
   ENDIF
RETURN Self