/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Splitter.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"
#include "colors.ch"

//--------------------------------------------------------------------------------------------------------------

CLASS Splitter INHERIT Control

   DATA lDown          PROTECTED INIT .F.
   DATA InvRect        PROTECTED
   DATA hWinDC         PROTECTED

   DATA MoveWhileDrag  EXPORTED INIT .F. // backward compatibility

   DATA Weight         EXPORTED  INIT 3
   DATA Cursor         EXPORTED
   DATA MinTop         EXPORTED
   DATA MaxBottom      EXPORTED
   DATA lSizing        EXPORTED  INIT .F.
   DATA TabOrder       EXPORTED

   PROPERTY Left       SET ::__SetSizePos( 1, v ) NOTPUBLIC
   PROPERTY Top        SET ::__SetSizePos( 2, v ) NOTPUBLIC
   PROPERTY Width      SET ::__SetSizePos( 3, v ) NOTPUBLIC
   PROPERTY Height     SET ::__SetSizePos( 4, v ) NOTPUBLIC

   PROPERTY Owner      GET __ChkComponent( Self, @::xOwner )

   PROPERTY ShowDragging DEFAULT .F.
   PROPERTY Position


   PROPERTY MinPos     DEFAULT 0
   PROPERTY MaxPos     DEFAULT 0

   DATA MaxBottom      EXPORTED

   EXPORTED:
      DATA ToolTip
      DATA Font
      DATA Border
      DATA Caption
      DATA Theming        INIT .T.
      DATA HorzScroll     INIT .F.
      DATA VertScroll     INIT .F.
      DATA BorderColor    INIT 0
      DATA ForeColor
      DATA Noresize
      DATA NoParentAlign
      DATA TopMost
      DATA AllowClose
      DATA AllowUndock
      DATA Dock
      DATA Anchor
      DATA TabStop
      DATA Visible
      DATA ClipSiblings
      DATA Transparent
      DATA ClipChildren
      DATA ContextMenu
      DATA Enabled
      DATA __lResizeable    INIT {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
      DATA __lMoveable      INIT .F.

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD __OnParentSize()
   METHOD OnParentMove()
   METHOD OnMouseMove()
   METHOD SplitOn()
   METHOD OnLButtonUp()
   METHOD GetSizes()
   METHOD SetSizes()
   METHOD OnLButtonDown()
   METHOD OnEraseBkGnd( hDC )  INLINE IIF( ::BackColor != NIL, Super:OnEraseBkGnd( hDC ), 1 )
   METHOD __GetOwner()
ENDCLASS

//--------------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS Splitter
   ::__xCtrlName    := "Splitter"
   ::Super:Init( oParent )
   ::ClassBrush   := GetStockObject( NULL_BRUSH )
   ::Weight       := 3
   ::Style        := (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::ExStyle      := WS_EX_TRANSPARENT
   ::__IsStandard := .F.
   ::Events       := {  {"General", { { "OnPosChanged", "", "" } } } }
RETURN Self

METHOD __GetOwner() CLASS Splitter
   ::xOwner := __ChkComponent( Self, ::xOwner )
RETURN ::xOwner

//--------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS Splitter
   LOCAL n
   DEFAULT ::Position TO 10 - ::Owner:Dock:Type

   ::Style    := (WS_CHILD | WS_VISIBLE)
   ::ExStyle  := WS_EX_TRANSPARENT

   ::ClsName  := "Splitter"+ALLTRIM( STR( ::Position ) )
   ::__hCursor := LoadCursor(, IIF( ::Position == 1 .OR. ::Position == 3, IDC_SIZEWE, IDC_SIZENS ) )
   IF ::Owner == NIL
      RETURN NIL
   ENDIF
   SWITCH ::Position
      CASE 1
         ::Owner:LeftSplitter   := Self
         EXIT
      CASE 2
         ::Owner:TopSplitter    := Self
         EXIT
      CASE 3
         ::Owner:RightSplitter  := Self
         IF ::Owner:MinWidth > 0
            ::MinLeft := ::Owner:Left + ::Owner:MinWidth
         ENDIF
         EXIT
      CASE 4
         ::Owner:BottomSplitter := Self
         EXIT
      DEFAULT
         RETURN NIL
   END
   ::GetSizes()

   ::Style    := (WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)

   TRY
      IF ::Owner:Visible
         ::Style := (::Style | WS_VISIBLE)
      ENDIF
    CATCH
      OutputDebugString( ::Owner:ClassName )
   END
   Super:Create()
   ::Dock := NIL

   FOR n := 1 TO LEN( ::Parent:__aDock )
       ::Parent:__aDock[n]:__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight,, .T. )
   NEXT
RETURN Self

//----------------------------------------------------------------------------------------------------------------

METHOD __OnParentSize() CLASS Splitter
   LOCAL aRect
   IF !::lSizing
      ::GetSizes()
      SetWindowPos( ::hWnd, , ::xLeft, ::xTop, ::xWidth, ::xHeight, (SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER | SWP_DEFERERASE) )
      aRect := { ::xLeft, ::xTop, ::xLeft+::xWidth, ::xTop+::xHeight }
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------------------------------------

METHOD OnParentMove() CLASS Splitter
   IF !::lSizing .AND. ::Owner != NIL .AND. ::Owner:IsWindow()
      ::GetSizes()
      SetWindowPos( ::hWnd, , ::xLeft, ::xTop, ::xWidth, ::xHeight, (SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER | SWP_DEFERERASE) )
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------------------------------------

METHOD SplitOn( x, y, lDirect )
   LOCAL pt, nOwnerTop, nOwnerHeight, n
   (x,y)
   DEFAULT lDirect TO .F.

   IF ::lDown .OR. lDirect
      pt := (struct POINT)

      GetCursorPos( @pt )
      ScreenToClient( ::Parent:hWnd, @pt )

      IF ::Position == 1
         IF ::MaxPos > 0
            pt:x := Min( pt:x, ::Parent:ClientWidth - ::MaxPos )
         ENDIF
         IF ::MinPos > 0
            pt:x := Max( pt:x, ::Parent:ClientWidth - ::MinPos )
         ENDIF
      ELSEIF ::Position == 3
         IF ::MaxPos > 0
            pt:x := Min( pt:x, ::MaxPos )
         ENDIF
         IF ::MinPos > 0
            pt:x := Max( pt:x, ::MinPos )
         ENDIF
       ELSE
         IF ::MaxPos > 0
            pt:y := Min( pt:y, ::MaxPos )
         ENDIF
         IF ::MinPos > 0
            pt:y := Max( pt:y, ::MinPos )
         ENDIF
      ENDIF

      SWITCH ::Position
         CASE 1
              IF pt:x + ::Weight <> ::Owner:xLeft
                 ::MoveWindow( pt:x, ::xTop, ::xWidth, ::xHeight, .f. )
                 ::SetSizes()
              ENDIF
              EXIT

         CASE 2
              pt:y :=  Min( ::Parent:ClientHeight - ::Weight, pt:y )
              IF ::Owner:MinHeight > 0
                 nOwnerHeight := ::Owner:xHeight
                 n := ::Owner:xTop
                 nOwnerTop := pt:y + ::Weight
                 nOwnerHeight -= ( nOwnerTop -n )
                 IF nOwnerHeight < ::Owner:MinHeight
                    pt:y -= (::Owner:MinHeight-nOwnerHeight)
                 ENDIF
              ENDIF
              IF ::Owner:MaxHeight > 0
                 nOwnerHeight := ::Owner:xHeight
                 n := ::Owner:xTop
                 nOwnerTop := pt:y + ::Weight
                 nOwnerHeight -= ( nOwnerTop - n )
                 IF nOwnerHeight > ::Owner:MaxHeight
                    pt:y -= (::Owner:MaxHeight-nOwnerHeight)
                 ENDIF
              ENDIF
              IF pt:y + ::Weight <> ::Owner:Top
                 MoveWindow( ::hWnd, ::xLeft, pt:y, ::xWidth, ::xHeight, .f. )
                 ::SetSizes()
              ENDIF
              EXIT

         CASE 3
              pt:x := Max( ::Owner:xLeft-::Weight, Min( ::Parent:ClientWidth - ::Weight, pt:x ) )
              ::MoveWindow( pt:x, ::xTop, ::xWidth, ::xHeight, .F. )
              ::SetSizes()
              EXIT

         CASE 4
              pt:y :=  Min( ::Parent:ClientHeight - ::Weight, pt:y )
              IF ::MaxBottom != NIL
                 pt:y :=  MIN( pt:y, ::MaxBottom )
              ENDIF
              IF ::MinTop != NIL
                 pt:y :=  MAX( pt:y, ::MinTop )
              ENDIF
              MoveWindow( ::hWnd, ::xLeft, pt:y, ::xWidth, ::xHeight, .f. )
              ::SetSizes()
              EXIT
      END
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------------------

METHOD OnMouseMove( nwParam, nlParam ) CLASS Splitter
   LOCAL pt, nPos, pt2, rc, x, y

   ::Super:OnMouseMove( nwParam, nlParam )

   x := LOWORD( nlParam )
   y := HIWORD( nlParam )

   IF ::ShowDragging
      ::SplitOn( x, y )
      RETURN 0
    ELSEIF ::lDown
      ::hWinDC := GetDCEx( ::Parent:hWnd,, DCX_PARENTCLIP )

      pt := (struct POINT)
      pt2 := (struct POINT)
      GetCursorPos( @pt )
      ScreenToClient( ::Parent:hWnd, @pt )

      IF pt:x < ::MinLeft
         pt:x := ::MinLeft
      ENDIF

      IF ::InvRect != NIL
         _InvertRect( ::hWinDC, ::InvRect )
      ENDIF

      GetWindowRect( ::hWnd, @rc )
      pt2:x := rc:left
      pt2:y := rc:Top
      ScreenToClient( ::Parent:hWnd, @pt2 )

      ::InvRect := Array(4)

      nPos := ::Position
      IF nPos > 2
         nPos -= 2
      ENDIF
      SWITCH nPos
         CASE 1
              IF ::Position == 1
                 IF ::MaxPos > 0
                    pt:x := Min( pt:x, ::Parent:ClientWidth - ::MaxPos )
                 ENDIF
                 IF ::MinPos > 0
                    pt:x := Max( pt:x, ::Parent:ClientWidth - ::MinPos )
                 ENDIF
              ELSE
                 IF ::MaxPos > 0
                    pt:x := Min( pt:x, ::MaxPos )
                 ENDIF
                 IF ::MinPos > 0
                    pt:x := Max( pt:x, ::MinPos )
                 ENDIF
              ENDIF
              ::InvRect[1] := MAX( MIN( pt:x, ::Parent:ClientWidth - ::xWidth ), 0 )
              ::InvRect[2] := pt2:y
              ::InvRect[3] := ::InvRect[1] + ::xWidth
              ::InvRect[4] := ::InvRect[2] + ::xHeight
              EXIT
         CASE 2
              ::InvRect[1] := pt2:x
              ::InvRect[2] := MAX( MIN( pt:y, ::Parent:ClientHeight - ::xHeight ), 0 )
              ::InvRect[3] := ::InvRect[1] + ::xWidth
              ::InvRect[4] := ::InvRect[2] + ::xHeight
              EXIT
      END
      _InvertRect( ::hWinDC, ::InvRect )
      ReleaseDC( ::hWnd, ::hWinDC )

      RETURN 0
    ELSEIF ::InvRect != NIL
      ::hWinDC := GetDCEx( ::Parent:hWnd,, DCX_PARENTCLIP )
      _InvertRect( ::hWinDC, ::InvRect )
      ReleaseDC( ::hWnd, ::hWinDC )
      ::InvRect := NIL
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------------------------------------------

METHOD OnLButtonDown() CLASS Splitter
   ::lDown := .T.
   SetCapture( ::hWnd )

   IF !::ShowDragging
      ::OnMouseMove()
   ENDIF
RETURN 0

//----------------------------------------------------------------------------------------------------------------

METHOD OnLButtonUp(nwParam,x,y) CLASS Splitter
   (nwParam)
   ReleaseCapture( ::hWnd )
   ::lDown := .F.
   IF !::ShowDragging
      ::OnMouseMove()
      ::SplitOn( x, y, .T. )
   ENDIF
RETURN 0

//----------------------------------------------------------------------------------------------------------------

METHOD GetSizes() CLASS Splitter
   IF ::Owner == NIL
      RETURN Self
   ENDIF
   SWITCH ::Position
      CASE 1
         ::xLeft        := ::Owner:xLeft - ::Weight
         ::xTop         := ::Owner:xTop
         ::xWidth       := ::Weight
         ::xHeight      := ::Owner:xHeight
         EXIT
      CASE 2
         ::xLeft        := ::Owner:xLeft
         ::xTop         := ::Owner:xTop - ::Weight - 1
         ::xWidth       := ::Owner:xWidth
         ::xHeight      := ::Weight
         EXIT
      CASE 3
         ::xLeft        := ::Owner:xLeft + ::Owner:xWidth
         ::xTop         := ::Owner:xTop
         ::xWidth       := ::Weight
         ::xHeight      := ::Owner:xHeight
         EXIT
      CASE 4
         ::xLeft        := ::Owner:xLeft
         ::xTop         := ::Owner:xTop + ::Owner:xHeight
         ::xWidth       := ::Owner:xWidth
         ::xHeight      := ::Weight
         EXIT
   END
RETURN Self

//----------------------------------------------------------------------------------------------------------------

METHOD SetSizes() CLASS Splitter
   LOCAL nOld, lMove := .F., n

   ::Owner:aPrevSize := { ::Owner:xLeft, ::Owner:xTop, ::Owner:xWidth, ::Owner:xHeight }
   SWITCH ::Position
      CASE 1
         IF ::Owner:xLeft != ::xLeft + ::Weight
            n := ::Owner:xLeft
            ::Owner:xLeft := ::xLeft + ::Weight
            IF ::Owner:Dock:Right != NIL
               ::Owner:Width += ( n - ::Owner:xLeft )
            ENDIF
            lMove := .T.
         ENDIF
         EXIT

      CASE 2
         n := ::Owner:xTop
         ::Owner:xTop := ::xTop + ::Weight
         ::Owner:xHeight -= ( ::Owner:xTop -n )
         lMove := .T.
         EXIT

      CASE 3
         nOld := ::Owner:xWidth
         ::Owner:xWidth := ::xLeft - ::Owner:xLeft
         IF ::Owner:xWidth != nOld
            ::Owner:__WidthPerc := ( ::Owner:xWidth / ( ::Owner:Parent:xWidth-::Owner:xLeft ) )
            IF ::Owner:Dock != NIL .AND. ::Owner:Dock:__nWidthPerc != NIL
               ::Owner:Dock:__nWidthPerc := ( ::Owner:xWidth  / ( ::Owner:Parent:ClientWidth-::Owner:xLeft ) )
            ENDIF
            lMove := .T.
         ENDIF
         EXIT

      CASE 4
         nOld := ::Owner:xHeight
         ::Owner:xHeight := ::xTop - ::Owner:xTop
         IF ::Owner:xHeight != nOld
            ::Owner:__HeightPerc := ( ::Owner:xHeight / ( ::Owner:Parent:xHeight-::Owner:xTop ) )
            lMove := .T.
         ENDIF
         EXIT
   END
   IF lMove
      ::Owner:__Splitting := .T.
      ::Parent:__Splitting := .T.
      IF ::ShowDragging
         ::Hide()
      ENDIF
      ::lSizing := .T.

      n := ::Owner:__OnParentSize()

      ::Parent:SendMessage( WM_SIZE, 0, MAKELONG( ::Parent:ClientWidth, ::Parent:ClientHeight ) )
      ::Parent:UpdateWindow()

      IF n == NIL
         ::Owner:MoveWindow()
      ENDIF

      ::lSizing := .F.
//      ::Owner:UpdateWindow()
      IF ::ShowDragging
         ::Show()
      ENDIF
      ::Owner:__Splitting := .F.
      ::Parent:__Splitting := .F.
      ExecuteEvent( "OnPosChanged", Self )
   ENDIF
RETURN Self

