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
   DATA MoveWhileDrag  EXPORTED INIT .F. // backward compatibility
   DATA ShowDragging   PUBLISHED INIT .F.
   DATA Position       PUBLISHED  //INIT 1

   DATA Weight         EXPORTED  INIT 3
   DATA Cursor         EXPORTED
   DATA MinTop         EXPORTED
   DATA MaxBottom      EXPORTED
   DATA lDown          PROTECTED INIT .F.
   DATA lSizing        EXPORTED INIT .F.
   DATA InvRect        PROTECTED
   DATA hWinDC         PROTECTED
   DATA TabOrder       EXPORTED

   PROPERTY Left    INDEX 1 READ xLeft   WRITE __SetSizePos HIDDEN
   PROPERTY Top     INDEX 2 READ xTop    WRITE __SetSizePos HIDDEN
   PROPERTY Width   INDEX 3 READ xWidth  WRITE __SetSizePos HIDDEN
   PROPERTY Height  INDEX 4 READ xHeight WRITE __SetSizePos HIDDEN
   
   PROPERTY Owner GET __ChkComponent( Self, ::xOwner )

   EXPORTED:
      DATA ToolTip
      DATA Font
      DATA Border
      DATA Caption
      DATA SmallCaption   INIT .F.
      DATA Theming        INIT .T.
      DATA HorzScroll     INIT .F.
      DATA VertScroll     INIT .F.
      DATA BackColor
      DATA ForeColor
      DATA Noresize
      DATA NoParentAlign
      DATA TopMost
      DATA AllowClose
      DATA AllowUndock
      DATA Dock
      DATA Anchor
      DATA TabStop
      DATA StaticEdge
      DATA Visible
      DATA ClipSiblings
      DATA ClientEdge
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
   METHOD OnEraseBkGnd()  INLINE 1
   METHOD __GetOwner()
ENDCLASS

//--------------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS Splitter
   LOCAL aProperties, aProperty, cProp
   ::__xCtrlName    := "Splitter"
   ::Super:Init( oParent )
   ::ClassBrush   := GetStockObject( NULL_BRUSH )
   ::Weight       := 3
   ::Style        := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ExStyle      := WS_EX_TRANSPARENT
   ::__IsStandard   := .F.
   ::Events       := {}
RETURN Self

METHOD __GetOwner() CLASS Splitter
   ::xOwner := __ChkComponent( Self, ::xOwner )
RETURN ::xOwner

//--------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS Splitter
   LOCAL n
   DEFAULT ::Position TO 10 - ::Owner:Dock:Type

   ::Style    := WS_CHILD | WS_VISIBLE
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

   ::Style    := WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   
   TRY
      IF ::Owner:Visible
         ::Style := ::Style | WS_VISIBLE 
      ENDIF
    CATCH
      OutputDebugString( ::Owner:ClassName )
   END
   Super:Create()
   ::Dock := NIL

   FOR n := 1 TO LEN( ::Parent:Children )
       ::Parent:Children[n]:__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight,, .T. )
   NEXT
RETURN Self

//----------------------------------------------------------------------------------------------------------------

METHOD __OnParentSize(nW, nH, hDef ) CLASS Splitter
   LOCAL aRect
   ::OnParentSize(nW, nH, hDef )
   IF !::lSizing
      ::GetSizes()
      SetWindowPos( ::hWnd, , ::xLeft, ::xTop, ::xWidth, ::xHeight, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER | SWP_DEFERERASE )
      aRect := { ::xLeft, ::xTop, ::xLeft+::xWidth, ::xTop+::xHeight }
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------------------------------------

METHOD OnParentMove() CLASS Splitter
   IF !::lSizing .AND. ::Owner != NIL .AND. ::Owner:IsWindow()
      ::GetSizes()
      SetWindowPos( ::hWnd, , ::xLeft, ::xTop, ::xWidth, ::xHeight, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER | SWP_DEFERERASE )
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------------------------------------

METHOD SplitOn( x, y, lDirect )
   LOCAL aPos, oCtrl, rc, pt, nOwnerTop, nOwnerHeight, n

   DEFAULT lDirect TO .F.

   IF ::lDown .OR. lDirect
      pt := (struct POINT)

      GetCursorPos( @pt )
      ScreenToClient( ::Parent:hWnd, @pt )

      IF pt:x < ::MinLeft
         pt:x := ::MinLeft
      ENDIF

      SWITCH ::Position
         CASE 1
              //aPos[1] :=  Max( ::Parent:LeftMargin, Min( ::Parent:ClientWidth - ::Parent:RightMargin - ::Weight, aPos[1] ) )
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

METHOD OnMouseMove( n, x, y, lDirect ) CLASS Splitter
   LOCAL pt, hDC, nPos, pt2, rc
   IF ::ShowDragging
      ::SplitOn( x, y, lDirect )
      RETURN 0
    ELSEIF ::lDown

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
      RETURN 0
    ELSEIF ::InvRect != NIL

      _InvertRect( ::hWinDC, ::InvRect )
      ::InvRect := NIL
   ENDIF

RETURN NIL

//----------------------------------------------------------------------------------------------------------------

METHOD OnLButtonDown() CLASS Splitter
   LOCAL rc

   ::lDown := .T.
   SetCapture( ::hWnd )

   IF !::ShowDragging

      ::hWinDC := GetDCEx( ::Parent:hWnd,, DCX_PARENTCLIP )

      //::hWinDC  := GetWindowDC( ::Parent:hWnd )

      ::OnMouseMove()
   ENDIF
RETURN 0

//----------------------------------------------------------------------------------------------------------------

METHOD OnLButtonUp(n,x,y) CLASS Splitter
   ReleaseCapture( ::hWnd )
   ::lDown := .F.
   IF !::ShowDragging
      ::OnMouseMove()
      ReleaseDC( ::hWnd, ::hWinDC )
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
   LOCAL aRect, aPt, oChild, nOld, lMove := .F., hWnd, hDef, n

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
   ENDIF
RETURN Self

