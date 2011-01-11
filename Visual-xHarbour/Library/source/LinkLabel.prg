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
   DATA LinkSysColor  INIT RGB(0,0,255)

   PROPERTY LinkVisited  READ xLinkVisited  WRITE InvalidateRect  DEFAULT .F.            PROTECTED
   PROPERTY LinkColor    READ xLinkColor    WRITE SetLinkColor    DEFAULT RGB(0,0,255)   PROTECTED
   PROPERTY VisitedColor READ xVisitedColor WRITE SetVisitedColor DEFAULT RGB(128,0,128) PROTECTED
   PROPERTY AutoSize     READ xAutoSize     WRITE SetWindowText   DEFAULT .T.            PROTECTED
   PROPERTY ImageIndex   READ xImageIndex   WRITE SetImageIndex   DEFAULT 0              PROTECTED

   DATA __Alignments                   EXPORTED  INIT { "Left", "Center", "Right" }
   ACCESS Alignment                    INLINE ::xAlignment PERSISTENT
   ASSIGN Alignment(n)                 INLINE ::xAlignment := n, ::Refresh()

   DATA ActiveLinkColor  PUBLISHED INIT RGB(255,0,0)
   DATA Url              PUBLISHED
   DATA FocusRect        PUBLISHED INIT .T.
   
   DATA xSelBackColor       EXPORTED
   ACCESS SelBackColor      INLINE ::xSelBackColor PERSISTENT
   ASSIGN SelBackColor( n ) INLINE ::xSelBackColor := n, ::SetSelColor( ::SelBackColor )

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
   METHOD OnEraseBkGnd() //INLINE 1

   METHOD SetParent( oParent ) INLINE IIF( ::__hBrush != NIL, ( DeleteObject( ::__hBrush ), ::__hBrush := NIL ), ), ::Super:SetParent( oParent ), ::__lReqBrush := .T., ::RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW )
   METHOD OnNCDestroy()        INLINE IIF( ::__hBrush != NIL, DeleteObject( ::__hBrush ), ), NIL
   METHOD SetImageIndex()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS LinkLabel
   ::__xCtrlName := "LinkLabel"
   ::Style := WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_OWNERDRAW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ClsName := "button"
   ::xCursor := IDC_HAND
   ::Super:Init( oParent )
RETURN Self

METHOD Create() CLASS LinkLabel
   LOCAL aSize, hDC

   IF ::Parent != NIL .AND. ::Parent:ClsName == "StatusBarPanel"
      hDC := ::Parent:Parent:Drawing:hDC
    ELSE
      hDC := ::Parent:Drawing:hDC
   ENDIF
   DEFAULT ::Parent:__hMemBitmap TO CreateCompatibleBitmap( hDC, ::Parent:ClientWidth, ::Parent:ClientHeight )

   ::__lReqBrush := .T.

   Super:Create()
   IF ::AutoSize
      ::__lResizeable   := {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
      aSize := ::Drawing:GetTextExtentPoint32( ::Caption )
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
      ::xCaption := cText
   ENDIF
   IF ::hWnd != NIL .AND. !::__IsInstance
      IF VALTYPE( cText ) == "C"
         SetWindowText( ::hWnd, cText )
      ENDIF
      IF ::AutoSize
         ::__lResizeable   := {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
         aSize := ::Drawing:GetTextExtentPoint32( ::Caption )
         ::xWidth := aSize[1]+4
         ::xHeight := aSize[2]+2
         IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
            ::xWidth += ::Parent:ImageList:IconWidth + 1
         ENDIF
         ::MoveWindow()
       ELSE
         ::__lResizeable   := {.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.}
      ENDIF
   ENDIF
RETURN Self

METHOD OnEraseBkGnd( hDC, nlParam ) CLASS LinkLabel
   LOCAL nState, hMemDC, hMemDC1, hMemBitmap, hMemBitmap1, hOldBitmap, hOldBitmap1, pWi, rc := (struct RECT)
   LOCAL nColor, lFocus, hBrush := ::BkBrush

   lFocus := ::__lFocused

   IF lFocus
      hBrush := ::SelBkBrush
   ENDIF
   ::Parent:UpdateWindow()
   DEFAULT hBrush TO ::__hBrush
   DEFAULT hBrush TO ::Parent:BkBrush
   DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )

   _FillRect( hDC, {0,0,::ClientWidth,::ClientHeight}, hBrush )

   GetClientRect( ::hWnd, @rc )

   IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
      IF lFocus
         ::Parent:ImageList:DrawImage( hDC, ::ImageIndex, 0, 0, ILD_TRANSPARENT | ILD_FOCUS, ::BackColor )
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
   
   IF !::IsWindowEnabled() .OR. !::Parent:IsWindowEnabled()
      nColor := ::System:Colors:GrayText
   ENDIF
   
   SetTextColor( hDC, nColor )
   rc:Left+=2
   
   DrawText( hDC, ::Caption, rc, (::Alignment-1)|DT_WORDBREAK )
   IF lFocus .AND. ::FocusRect
      rc:Left-=2
      DrawFocusRect( hDC, rc)
   ENDIF
RETURN 1

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
      IF lRepaint
         ::Refresh()
       ELSE
         ::InValidateRect()
      ENDIF
   ENDIF
RETURN SELF

METHOD SetVisitedColor( nColor, lRepaint ) CLASS LinkLabel
   DEFAULT lRepaint TO .T.
   ::xVisitedColor := nColor
   IF ::IsWindowVisible()
      IF lRepaint
         ::Refresh()
       ELSE
         ::InValidateRect()
      ENDIF
   ENDIF
RETURN SELF

METHOD SetImageIndex() CLASS LinkLabel
   LOCAL aSize
   IF ::AutoSize
      ::__lResizeable   := {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
      aSize := ::Drawing:GetTextExtentPoint32( ::Caption )
      ::xWidth := aSize[1]+4
      ::xHeight := aSize[2]+2
      IF ::Parent:ImageList != NIL .AND. ::ImageIndex > 0
         ::xWidth += ::Parent:ImageList:IconWidth + 1
         ::xHeight := MAX( ::xHeight, ::Parent:ImageList:IconHeight )
      ENDIF
      ::MoveWindow()
   ENDIF
RETURN Self