/*
 * $Id: CtrlMask.Prg,v 1.30 2002/10/28 12:04:18 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * xIDE FormEditor masking module
 *
 * Copyright 2002 Andy Wos [andrwos@aust1.net] Augusto Infante [systems@quesoro.com] Ron Pinkas [ron@ronpinkas.com]
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 */

/*
TODO:

- add lSnap flag
- revise (improve) snapping
  - snap only the mouse drag position
    ( and only if moved )
  - snap draw steps only - position of the control
  - separate method for control snapping
  - preserve custom grid offset and custom control size
- revise min size protection for controls
- add notifications for object inspector
- add external object (selective+group) selection/deselection
- add object tabbing                                <- done
- add send to the back/front
- mouse right click - trackpopup
- tooltip with position coords
- add UNDO ( only last operation as in Delphi ).
- direct text editing ( on char ? pass keystrokes to selected control ?)
- Use methods instead of API calls
- menu editor
- parent form must disable processing commands in editor mode.
- improve visibility of the selection rectangle
  ( replace focus rect with your own. )

*/

*************************************************


#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
#include "accel.ch"

#define ASWORD( nUword ) if( ( nUword > 32768 ),(nUword-65536),nUword )

GLOBAL EXTERNAL hThisInstance
GLOBAL EXTERNAL FormEdit
GLOBAL EXTERNAL MainFrame

// selected controls array elements

#define SEL_HANDLE   1
#define SEL_OLDRECT  2
#define SEL_ANCHORX  3
#define SEL_ANCHORY  4
#define SEL_HOTAREAS 5
#define SEL_NEWRECT  6

// min control sixe

#define MINX 5
#define MINY 5

// resizing handle size

#define HANDLESIZE 5

*-----------------------------------------------------------------------------*

CLASS oCtrlMask FROM TWinControl

   DATA aSelected
   DATA aHot
   DATA aSiblings
   DATA moving
   DATA sizing
   DATA selecting
   DATA creating
   DATA nClick
   DATA MouseDown
   DATA aEditRect
   DATA aClickPt
   DATA Cursor
   DATA lHintActive INIT .T.
   DATA lChanged    INIT .F.
   
   METHOD New() CONSTRUCTOR
   METHOD OnGetDlgCode() INLINE DLGC_WANTARROWS + DLGC_WANTTAB
   METHOD OnRButtonDown()
   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD OnPaint()
   METHOD OnKeyDown()
   METHOD OnKeyUp()
   METHOD GetSiblings()
   METHOD OnMouseMove()
   METHOD MoveCtrl()
   METHOD Refresh()
   METHOD ChangeInspector()
   METHOD OnUserMsg()
ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent ) CLASS oCtrlMask

   ::left      := 0
   ::top       := 0
   ::width     := oParent:width
   ::height    := oParent:height
   ::style     := WS_CHILD + WS_VISIBLE
   ::WinClass  := "CTRLMASK"
   ::ControlName := "CTRLMASK"
   ::color     := GetStockObject( NULL_BRUSH )
   ::ExStyle   := WS_EX_TRANSPARENT
   ::Cursor    := 0
   ::Caption   := ""
   ::lRegister := .T.
   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .F.
   ::FormType  := 1
   ::aHot      := {}
   ::aSiblings := {}
   ::moving    := .F.
   ::sizing    := .F.
   ::selecting := .F.
   ::creating  := .F.
   ::mousedown := .F.
   ::nClick    := HTCLIENT
   ::aEditRect := NIL
   ::aSelected := {}
   ::aClickPt  := {}

   RETURN( super:new( oParent ) )


*-----------------------------------------------------------------------------*


METHOD OnRButtonDown()

   messagebox( ::Parent:Handle, "TrackPopupMenu comes here" )
   SetFocus( ::Handle )

   RETURN( NIL )



// no snapping to grid with keyboard - precision movements

// Ctrl+arrows => moving
// Shift+arrows => sizing
// Esc => unselect
// Del => remove


// ALT key to swich over to main window
// Check in OnSysKeyDown

// TAB next control ( even if none selected )


METHOD OnKeyDown( wParam, lParam )

     LOCAL o
     LOCAL cKbd
     LOCAL lShift
     LOCAL lCtrl
     LOCAL a
     LOCAL aRect
     LOCAL acRect
     LOCAL lOnBorder := .T.
     LOCAL n := 0
     LOCAL l := 0
     LOCAL t := 0
     LOCAL b := 0
     LOCAL r := 0

     cKbd   := GetKeyboardState()
     lShift := ( cKbd[ VK_SHIFT + 1 ]   < 0  )
     lCtrl  := ( cKbd[ VK_CONTROL + 1 ] < 0  )

     DO CASE
        CASE wParam == VK_RETURN
            ::Parent:MsgBox( "Popup menu ? text editing ? oBj inspector ?" )
            ::SetFocus()
            RETURN( NIL )

         CASE wParam == VK_DELETE
            FOR EACH a IN ::aSelected()
               IF !Empty( o := ::Parent:ChildFromHandle( a[1] ) )
                  o:Delete()
               ENDIF
            NEXT
            ::aSelected := {}
            ::GetSiblings()
            ::nClick := HTCLIENT
            ::SetFocus()
            RETURN( NIL )

         CASE wParam == VK_ESCAPE
            ::aSelected := {}
            ::nClick    := HTCLIENT
            InvalidateRect( ::Parent:Handle,, .F. )
            ::SetFocus()
            RETURN( NIL )

         CASE wParam == VK_TAB
            IF Len( ::aSelected ) == 0 .OR.;
               (n := aScan( ::aSiblings,{|h| h == ::aSelected[1][SEL_HANDLE] } ) ) == 0

               IF len( ::aSiblings ) > 0
                  n := 1
               ENDIF

            ENDIF
            IF n > 0

               IF lShift
                  n := IF( n==1, len( ::aSiblings ), Max( 1, n-1 ) )
                 ELSE
                  n := IF( len( ::aSiblings ) = n, 1, Min( Len( ::aSiblings ), n + 1 ) )
               ENDIF

               IF Len( ::aSelected )==0 .OR.;
                  ( ::aSiblings[n] <> ::aSelected[1][SEL_HANDLE] ) .OR. len( ::aSelected ) > 1

                  aRect := GetWindowRect( ::aSiblings[n] )
                  RectScreenToClient( ::Handle, aRect )
                  ::aSelected := {{ ::aSiblings[n], aclone( aRect ), 0, 0, {}, {} } }   // handle,rect,anchorX,anchorY,aHot
                  InvalidateRect( ::Parent:Handle ,, .F. )

                  ::PostMessage( WM_USER + 1, 0 )

               ENDIF
            ENDIF
            RETURN( NIL )

           // select next control


         CASE ( !lCtrl .AND. !lShift) .OR. ( lShift .AND. lCtrl ) .OR. Len( ::aSelected ) == 0
            RETURN(NIL)

         CASE wParam==VK_DOWN
         
            lOnBorder := .F.
            acRect    := GetClientRect(::Handle)
            b         := 1
            IF lShift
               t := 1
               ::moving := .T.
            ELSE
               ::sizing := .T.
            ENDIF
            
            FOR EACH a IN ::aSelected
            
               aRect := GetWindowRect( a[ SEL_HANDLE ] )
               RectScreenToClient( ::Handle, aRect )
               a[ SEL_OLDRECT ] := aClone( aRect )
               
               IF a[ SEL_OLDRECT][4] >= acRect[4]
                  lOnBorder := .T.
                  EXIT
               ENDIF
               
            NEXT

         CASE wParam==VK_UP //up
            lOnBorder := .F.
            acRect    := GetClientRect(::Handle)
            b         := -1
            IF lShift
               t := -1
               ::moving := .T.
            ELSE
               ::sizing := .T.
            ENDIF
            
            FOR EACH a IN ::aSelected
               aRect := GetWindowRect( a[ SEL_HANDLE ] )
               RectScreenToClient( ::Handle, aRect )
               a[ SEL_OLDRECT ] := aClone( aRect )
               
               IF ( lCtrl .AND. ( a[ SEL_OLDRECT ][ 2 ] <= 0 ) ) .OR.;
                  ( lShift .AND. ( a[ SEL_OLDRECT ][ 4 ] - a[ SEL_OLDRECT ][ 2 ] <= MINY ) )
                  lOnBorder:=.T.
                  EXIT
               ENDIF
               
            NEXT

         CASE wParam == VK_LEFT // left
            lOnBorder := .F.
            acRect    := GetClientRect(::Handle)
            r         := -1
            IF lShift
               l := -1
               ::moving := .T.
            ELSE
               ::sizing := .T.
            ENDIF
            
            FOR EACH a IN ::aSelected
               aRect := GetWindowRect( a[ SEL_HANDLE ] )
               RectScreenToClient( ::Handle, aRect )
               a[ SEL_OLDRECT ] := aClone( aRect )
               
               IF ( lCtrl .AND. ( a[ SEL_OLDRECT ][ 1 ] <= 0 ) ) .OR.;
                  (lShift .AND. ( a[ SEL_OLDRECT ][ 3 ] - a[ SEL_OLDRECT ][ 1 ] <= MINX ) )
                  lOnBorder := .T.
                  EXIT
               ENDIF
               
            NEXT

         CASE wParam == VK_RIGHT
            lOnBorder := .F.
            acRect    := GetClientRect(::Handle)
            r         := 1
            IF lShift
               l := 1
               ::moving := .T.
            ELSE
               ::sizing := .T.
            ENDIF
            FOR EACH a IN ::aSelected
               aRect := GetWindowRect( a[ SEL_HANDLE ] )
               RectScreenToClient( ::Handle, aRect )
               a[ SEL_OLDRECT ] := aClone( aRect )
               
               IF a[ SEL_OLDRECT ][ 3 ] >= acRect[ 3 ]
                  lOnBorder := .T.
                  EXIT
               ENDIF
               
            NEXT

      ENDCASE

      IF !lOnBorder
         FOR EACH a IN ::aSelected

            InvalidateRect( ::Parent:Handle, a[ SEL_OLDRECT ] , .F. )
            a[ SEL_OLDRECT ][ 1 ] += l
            a[ SEL_OLDRECT ][ 2 ] += t
            a[ SEL_OLDRECT ][ 3 ] += r
            a[ SEL_OLDRECT ][ 4 ] += b
            MoveWindow( a[ SEL_HANDLE ], a[ SEL_OLDRECT ][ 1 ], a[ SEL_OLDRECT ][ 2 ],;
                        a[ SEL_OLDRECT ][ 3 ]-a[ SEL_OLDRECT ][ 1 ], a[ SEL_OLDRECT ][ 4 ] - a[ SEL_OLDRECT ][ 2 ], .T. )
         NEXT
      ENDIF

     RETURN(NIL)


*-----------------------------------------------------------------------------*

 METHOD OnKeyUp( wParam, lParam )

   LOCAL a
   LOCAL oObj

   IF wParam == VK_RIGHT .OR. wParam == VK_LEFT .OR. ;
      wParam == VK_UP    .OR. wParam == VK_DOWN .OR. ;
      wParam == VK_SHIFT .OR. wParam == VK_CONTROL

      IF ::sizing .OR. ::moving
         ::sizing := .F.
         ::moving := .F.
         
         FOR EACH a IN ::aSelected
            oObj := ::Parent:ChildFromHandle( a[ SEL_HANDLE ] )
            oObj:Parent:Parent:ObjInspect:SetBrowserData( oObj, 1)
         NEXT
         
      ENDIF
   ENDIF

   RETURN( NIL )


*-----------------------------------------------------------------------------*

METHOD OnPaint( hDC )
   LOCAL a
   LOCAL n:=0
   IF !empty( ::aSelected )
   
      IF ! ::mousedown
         FOR EACH a In ::aSelected
           n++
           ::aSelected[ n ][ SEL_HOTAREAS ] := DrawHandles( hDC, a[ SEL_OLDRECT ],;
                                                          if( len(::aSelected) > 1,;
                                                              COLOR_BTNSHADOW + 1,;
                                                              COLOR_WINDOWTEXT + 1 ) )
         NEXT
      ENDIF
      
   ENDIF

   RETURN(nil)

*-----------------------------------------------------------------------------*

METHOD Refresh()

   LOCAL aRect
   LOCAL a

   FOR EACH a IN ::aSelected
      aRect := GetWindowRect( a[ SEL_HANDLE ] )
      RectScreenToClient( ::Handle, aRect )
      a[ SEL_OLDRECT ] := aClone( aRect )
   NEXT

   InvalidateRect( ::Parent:Handle,, .T. )
   ::BringToTop()
   ::SetFocus()

   RETURN( NIL )

*-----------------------------------------------------------------------------*

// client area coordinates

METHOD OnLButtonDown( wParam, x, y )

   LOCAL aPt := {ASWORD(x), ASWORD(y)}
   LOCAL hChild
   LOCAL aRect
   LOCAL aWRect
   LOCAL lFound := .F.
   LOCAL nPos
   LOCAL hDC
   LOCAL a
   LOCAL aOldSelected := aClone( ::aSelected )
   local Item
   ClientToScreen( ::Handle, aPt )

   IF Empty( ::Cursor )
      
      ::lChanged := .F.

      FOR EACH hChild IN ::aSiblings
         aWRect := GetWindowRect( hChild )
         IF PtInRect( aWRect, aPt )
            
            lFound := .T.
            IF aScan( ::aSelected, {|a| a[ SEL_HANDLE ] == hChild } ) == 0
               aRect := aClone( aWRect )
               RectScreenToClient( ::Handle, aRect )
               aEval( ::aSelected, {|a| ShowWindow( a[ SEL_HANDLE ], SW_HIDE ),;
               RedrawWindow( ::Parent:Handle, aRect,, RDW_INVALIDATE + RDW_UPDATENOW + RDW_ALLCHILDREN),;
               ShowWindow( a[ SEL_HANDLE ], SW_SHOW ) } )
               ::aSelected := { { hChild, aclone( aRect ), x - aRect[1], y - aRect[2], {}, aclone( aRect ) } }   // handle,rect,anchorX,anchorY,aHot
            ENDIF
            
            IF Len( ::aSelected ) == 1 .and. ( nPos := aScan( ::aSelected[ 1, SEL_HOTAREAS ],{|a| ptInRect( a[1], {x,y} ) } ) ) > 0 // <= aHot
               ::nClick := ::aSelected[ 1, SEL_HOTAREAS, nPos, 2 ]
               WinSetCursor( LoadCursor(, ::aSelected[ 1, SEL_HOTAREAS, nPos, 3 ] ) )
               ::sizing := .T.
              ELSE
               ::nClick := HTCAPTION
               WinSetCursor( LoadCursor( hThisInstance, "FIST" ) )
               FOR EACH a IN ::aSelected
                   a[ SEL_ANCHORX ] := x - a[ SEL_OLDRECT, 1 ] // anchor X
                   a[ SEL_ANCHORY ] := y - a[ SEL_OLDRECT, 2 ] // anchor Y
               NEXT
               ::moving := .T.
            ENDIF
            ::aEditRect := ::MoveCtrl( ::Handle, { x, y } )

            EXIT
         ENDIF
      NEXT
    ELSE
      ::creating :=.T.
      ::cursor   :=NIL
      ::lChanged := .T.
   ENDIF

   IF !lFound
      FOR EACH hChild in ::aSelected
         aWRect := GetWindowRect( hChild )
         aRect  := aClone( aWRect )
         RectScreenToClient( ::Handle, aRect )
         aEval( ::aSelected, {|a| ShowWindow( a[ SEL_HANDLE ], SW_HIDE ),;
                RedrawWindow( ::Parent:Handle, aRect,, RDW_INVALIDATE + RDW_UPDATENOW + RDW_ALLCHILDREN ),;
                ShowWindow( a[ SEL_HANDLE ], SW_SHOW ) } )
      NEXT

      ::aSelected := {}
      ::nClick    := HTCLIENT
      WinSetCursor( LoadCursor(, IDC_SIZEALL ) )
      ::aEditRect := { x, y, x, y }
      ::selecting := .T.
      ::aClickPt  := { x, y }
   ENDIF

 //  InvalidateRect(::Handle,,.F.)
 //  RedrawWindow(::Parent:Handle,GetClientRect(::Parent:Handle),,RDW_INVALIDATE+RDW_ERASENOW+RDW_ALLCHILDREN)

   // ToolTips and Inspector selection
   IF lFound
      ::HintWindow:Activate(1)
      ::HintWindow:TrackHint(1)
      ::lHintActive := .T.
      ::PostMessage( WM_USER + 1, 0 )
     ELSE
      ::PostMessage( WM_USER + 1, 1 )
   ENDIF


   hDC := GetDC( ::Handle )
   SetTextColor( hDC, RGB( 255, 255, 128 ) )
   SetBkColor( hDC, RGB( 0, 0, 125 ) )
   IF len( ::aSelected ) > 1
      FOR EACH a In ::aSelected
         DrawFocusRect( hDC, a[ SEL_NEWRECT ] )
      NEXT
     ELSE
      DrawFocusRect( hDC, ::aEditRect )
   ENDIF
   ReleaseDc( ::Handle, hDC )

   ::mousedown := .T.
   SetCapture( ::Handle )
   UpdateWindow( ::Parent:handle )

   RETURN( NIL )

*-----------------------------------------------------------------------------*

METHOD OnUserMsg( nMsg, nwParam, nlParam)
   
   IF nMsg == WM_USER + 1
      ::ChangeInspector( nwParam )
   ENDIF
   
RETURN( nil )

*-----------------------------------------------------------------------------*

METHOD ChangeInspector( n )

   LOCAL Item, SubItem, hTreeItem
   static nSelObj
            
   IF n == 1 
  
      FOR EACH Item IN MainFrame:ObjTree:TreeView1:Items

          IF Item:cargo == ::Parent:handle
             Item:Select()
             nSelObj := ::handle
             EXIT
          ENDIF

      NEXT
      RETURN( nil )
      
   ENDIF
   
   IF Len( ::aSelected ) == 1 .AND. ( nSelObj == NIL .OR. nSelObj != ::aSelected[ 1 ][ SEL_HANDLE ] )
      // need to tell the inspector
      FOR EACH Item IN MainFrame:ObjTree:TreeView1:Items
             
          // Check TreeView subitems
          FOR EACH SubItem IN Item:Items
              IF SubItem:cargo == ::aSelected[ 1 ][ SEL_HANDLE ]
                 hTreeItem := SubItem:handle
                 EXIT
              ENDIF
          NEXT

          // Check TreeView Root items
          IF hTreeItem != NIL
             EXIT
            ELSEIF Item:cargo == ::aSelected[ 1 ][ SEL_HANDLE ]
             hTreeItem := Item:handle
             EXIT
          ENDIF

      NEXT
         
      // Select the TreeView item
      IF hTreeItem != NIL
         MainFrame:ObjTree:TreeView1:SelectItem( hTreeItem )
         nSelObj := ::aSelected[ 1 ][ SEL_HANDLE ]
      ENDIF
            
   ENDIF

RETURN( nil )

*-----------------------------------------------------------------------------*

// client coordinates

METHOD OnMouseMove( aParam, x, y )

   LOCAL aPt := {ASWORD(x),ASWORD(y)}
   LOCAL hChild
   LOCAL lFound := .F.
   LOCAL nPos
   LOCAL n
   LOCAL hDC
   LOCAL a,oObj, aTipRect
   static oLastObj
   
   IF ::mousedown

      aTipRect := ::aEditRect
      
      ::MoveCtrl( ::Handle, aPt )
      if !aTipRect[1] == ::aEditRect[1].or.;
         !aTipRect[2] == ::aEditRect[2].or.;
         !aTipRect[3] == ::aEditRect[3].or.;
         !aTipRect[4] == ::aEditRect[4]
         
         ::lChanged := .T.
         
         hDC := GetDC( ::Handle )
         SetTextColor( hDC, RGB( 255, 255, 128 ) )
         SetBkColor( hDC, RGB( 0, 0, 128 ) )
      
         // old
         IF len( ::aSelected ) > 1
            FOR EACH a In ::aSelected
               DrawFocusRect( hDC, a[ SEL_NEWRECT ] )
            NEXT
         ELSE
           DrawFocusRect( hDC, aTipRect ) //::aEditRect )
         ENDIF

         ::HintWindow:SetTitle(0,"")
         ::HintWindow:SetText( alltrim( str( ::aEditRect[ 1 ] ) ) + "," + alltrim( str( ::aEditRect[ 2 ] ) ) + "  " +;
                               alltrim( str( ::aEditRect[ 3 ] - ::aEditRect[ 1 ] ) ) + "," + alltrim( str( ::aEditRect[ 4 ] - ::aEditRect[ 2 ] ) ) )

         UpdateWindow( ::Parent:handle )

      
         //new
         IF len( ::aSelected ) > 1
            ::MoveCtrl( ::Handle, aPt )
            FOR EACH a In ::aSelected
              DrawFocusRect( hDC, a[ SEL_NEWRECT ] )
            NEXT
         ELSE
            ::MoveCtrl( ::Handle, aPt )
            DrawFocusRect( hDC, ::aEditRect )
         ENDIF
         ReleaseDc( ::Handle, hDC )


      ENDIF

   ELSE
      IF Empty( ::Cursor )
         ClientToScreen( ::Handle, aPt )
         FOR EACH hChild IN ::aSiblings
            IF PtInRect( GetWindowRect( hChild ), aPt )
               IF Len( ::aSelected ) == 1 .and. ::aSelected[ 1, SEL_HANDLE ] == hChild
                  IF (nPos := aScan( ::aSelected[ 1, SEL_HOTAREAS ], {|a| ptInRect( a[1], {x,y} ) } ) ) > 0
                     WinSetCursor( LoadCursor(, ::aSelected[ 1, 5, nPos, 3 ] ) )
                  ELSE
                     WinSetCursor( LoadCursor( hThisInstance, "HAND" ) )
                  ENDIF
               ELSEIF len( ::aSelected ) > 1
                  WinSetCursor( LoadCursor( hThisInstance, "HAND" ) )
               ELSE
                  WinSetCursor( LoadCursor( hThisInstance, "FINGER" ) )
               ENDIF
               lFound := .T.
               oObj   := ::Parent:ChildFromHandle( hChild )
               IF oLastObj == NIL .OR. hChild != oLastObj:handle .OR. ::lHintActive
                  ::HintWindow:Activate(1)
                  ::HintWindow:SetTitle( 2, "Class: " + oObj:Classname() )
                  ::HintWindow:SetText( "Object: " + oObj:name )
                  ::lHintActive := .F.
                  oLastObj      := oObj
               endif
               
               EXIT
            ENDIF
         NEXT
         IF !lFound
            oLastObj := NIL
            ::HintWindow:Activate(0)
            WinSetCursor( LoadCursor(, IDC_ARROW ) )
         ENDIF
      ELSE
         WinSetCursor( ::Cursor )
      ENDIF
   ENDIF

   RETURN( nil )


*-----------------------------------------------------------------------------*

// client coordinates

METHOD OnLButtonUp( wParam, x, y )

   LOCAL aPt := { ASWORD(x), ASWORD(y) }
   LOCAL aTemp
   LOCAL hChild
   LOCAL aWRect
   LOCAL aRect
   LOCAL hDC
   LOCAL a
   LOCAL oObj

   IF !::mousedown
      RETURN( NIL )
   ENDIF

   IF ::creating
      ::cursor := NIL
   ENDIF
   IF ::moving .OR. ::sizing
      UpdateWindow( ::Parent:handle )
   ENDIF
    
   ::mousedown := .F.
   hDC := GetDC( ::Handle )

   IF ::moving .OR. ::sizing
      IF len( ::aSelected ) > 1
         FOR EACH a In ::aSelected
            DrawFocusRect( hDC, a[ SEL_NEWRECT ] )
         NEXT
      ELSE
        DrawFocusRect( hDC, ::aEditRect )
      ENDIF
      ::HintWindow:Activate(0)
   ENDIF

   IF ::moving
      ::moving := .F.
   ENDIF
   IF ::sizing
      ::sizing := .F.
   ENDIF
   ::MoveCtrl( , aPt )
   FOR EACH a IN ::aSelected

       ShowWindow( a[ SEL_HANDLE ], SW_HIDE )

       MoveWindow( a[ SEL_HANDLE ], a[ SEL_NEWRECT, 1 ],a[ SEL_NEWRECT , 2 ],;
                   a[ SEL_NEWRECT, 3 ]- a[ SEL_NEWRECT, 1 ], a[ SEL_NEWRECT, 4 ] - a[ SEL_NEWRECT, 2 ], .F. )

       ShowWindow( a[ SEL_HANDLE ], SW_SHOW )
       
       aRect := GetWindowRect( a[1] )
       RectScreenToClient( ::Handle, aRect )
       a[ SEL_OLDRECT ] := aClone( aRect )
      
       if ::lChanged .AND. a[ SEL_HANDLE ] == ::Parent:Parent:ObjInspect:CurObject:handle
          oObj := ::Parent:ChildFromHandle( a[ SEL_HANDLE ] )
          oObj:Parent:Parent:ObjInspect:SetBrowserData( oObj, 1 )
       endif

   NEXT

   IF ::selecting

      ::selecting := .F.
      WinSetCursor( LoadCursor(, IDC_ARROW ) )
      DrawFocusRect( hDC, ::aEditRect )

      IF ::creating
         ::creating := .F.

         oObj := ::Parent:CreateControl( { ::aEditRect[ 1 ],;
                                           ::aEditRect[ 2 ],;
                                           IF( ::aEditRect[ 3 ] - ::aEditRect[ 1 ] < MINX, NIL,;
                                               ::aEditRect[ 3 ] - ::aEditRect[ 1 ] ),;
                                           IF( ::aEditRect[ 4 ] - ::aEditRect[ 2 ] < MINX, NIL,;
                                               ::aEditRect[ 4 ] - ::aEditRect[ 2 ] ) } )

         IF ! Empty(oObj)
            aWRect := GetWindowRect( oObj:Handle )
            aRect  := aClone( aWRect )
            RectScreenToClient( ::Handle, aRect )
            ::aSelected:={ { oObj:Handle, aclone( aRect ), 0, 0, {}, {} } } // handle,rect,anchorX,anchorY,aHot
         ENDIF

      ELSE

         FOR EACH hChild IN ::aSiblings
            aWRect := GetWindowRect( hChild )
            aRect  := aClone( aWRect )
            RectScreenToClient( ::Handle, aRect )
            IF !empty( IntersectRect( aRect, ::aEditRect ) )
               aAdd( ::aSelected, { hChild, aclone( aRect ), 0, 0, {}, {} } ) // handle,rect,anchorX,anchorY,aHot
            ENDIF
         NEXT

      ENDIF
      InvalidateRect( ::Handle,, .F. )
   ENDIF
   ReleaseDc( ::Handle, hDC )


  // InvalidateRect(::Handle,,.F.)
  // RedrawWindow(::Parent:Handle,GetClientRect(::Parent:Handle),,RDW_INVALIDATE+RDW_UPDATENOW+RDW_ALLCHILDREN)
   ReleaseCapture()
   ::BringToTop()
   ::SetFocus()
   RETURN( NIL )

*-----------------------------------------------------------------------------*

// to be revised
METHOD MoveCtrl( hWnd, aPt )

   LOCAL aRect
   LOCAL AnchorX
   LOCAL AnchorY
   LOCAL aChild
   LOCAL aOriginal
   local oObj
   FOR EACH aChild IN ::aSelected
      AnchorX   := aChild[ SEL_ANCHORX ]
      AnchorY   := aChild[ SEL_ANCHORY ]
      aOriginal := SnapRect( aChild[ SEL_OLDRECT ], ::Parent:xGrid, ::Parent:yGrid )

      DO CASE
      CASE ::nClick == HTCAPTION
         aRect := SnapRect( { aPt[1] - AnchorX, aPt[2] - AnchorY,;
                              aPt[1] + aOriginal[3] - aOriginal[1] - AnchorX,;
                              aPt[2] + aOriginal[4] - aOriginal[2] - AnchorY },;
                              ::Parent:xGrid, ::Parent:yGrid )

      CASE ::nClick == HTTOP
         aPt[2] := Snap( Min( aPt[2], aOriginal[4] - MINY ), ::Parent:yGrid )
         aRect  := { aOriginal[1], aPt[2], aOriginal[3], aOriginal[4] }

      CASE ::nClick == HTBOTTOM
         aPt[2] := Snap( Max( aPt[2], aOriginal[2] + MINY ), ::Parent:yGrid )
         aRect  := { aOriginal[1], aOriginal[2], aOriginal[3], aPt[2] }

      CASE ::nClick == HTLEFT
         aPt[1] := Snap( Min( aPt[1], aOriginal[3] - MINX), ::Parent:xGrid )
         aRect  := { aPt[1], aOriginal[2], aOriginal[3], aOriginal[4] }

      CASE ::nClick == HTRIGHT
         aPt[1] := Snap( Max( aPt[1], aOriginal[1] + MINX), ::Parent:xGrid )
         aRect  := { aOriginal[1], aOriginal[2], aPt[1], aOriginal[4] }

      CASE ::nClick == HTTOPLEFT
         aPt[1] := Snap( Min( aPt[1], aOriginal[3] - MINX), ::Parent:xGrid )
         aPt[2] := Snap( Min( aPt[2], aOriginal[4] - MINY), ::Parent:yGrid )
         aRect  := { aPt[1], aPt[2], aOriginal[3], aOriginal[4] }

      CASE ::nClick == HTBOTTOMLEFT
         aPt[1] := Snap( Min( aPt[1], aOriginal[3] - MINX ), ::Parent:xGrid )
         aPt[2] := Snap( Max( aPt[2], aOriginal[2] + MINY ), ::Parent:yGrid )
         aRect:={ aPt[1], aOriginal[2], aOriginal[3], aPt[2] }

      CASE ::nClick == HTTOPRIGHT
         aPt[1] := Snap( Max( aPt[1], aOriginal[1] + MINX ), ::Parent:xGrid )
         aPt[2] := Snap( Min( aPt[2], aOriginal[4] - MINY ), ::Parent:yGrid )
         aRect  := { aOriginal[1], aPt[2], aPt[1], aOriginal[4] }

      CASE ::nClick == HTBOTTOMRIGHT
         aPt[1] := Snap( Max( aPt[1], aOriginal[1] + MINX ), ::Parent:xGrid )
         aPt[2] := Snap( Max( aPt[2], aOriginal[2] + MINY ), ::Parent:yGrid )
         aRect  := { aOriginal[1], aOriginal[2], aPt[1], aPt[2] }

      ENDCASE

      aChild[6] := aclone( aRect )

   NEXT

   IF ::nClick == HTCLIENT
      aRect :={ Min( aPt[1], ::aClickPt[1] ), Min( aPt[2], ::aClickPt[2] ),;
                Max( aPt[1], ::aClickPt[1] ), Max( aPt[2], ::aClickPt[2] ) }

   ENDIF
   ::aEditRect := aRect

   RETURN(aRect)

*-----------------------------------------------------------------------------*
// in reversed order

METHOD GetSiblings()
   LOCAL hWnd  := GetWindow( ::Parent:Handle, GW_CHILD )
   ::aSiblings := {}
   
   DO WHILE !empty( hWnd )
      IF hWnd <> ::Handle
         aIns( ::aSiblings, 1, hWnd, .T.)
      ENDIF
      hWnd := GetWindow( hWnd, GW_HWNDNEXT )
   ENDDO
   RETURN( ::aSiblings )

*-----------------------------------------------------------------------------*

FUNCTION Snap( x, nGrain )
   RETURN( if( nGrain == 0, x, round( ( x / nGrain ), 0) * nGrain ) )

*-----------------------------------------------------------------------------*

FUNCTION SnapRect( aRect, xGrain, yGrain )

   aRect[1] := Snap( aRect[1], xGrain )
   aRect[2] := Snap( aRect[2], yGrain )
   aRect[3] := Snap( aRect[3], xGrain )
   aRect[4] := Snap( aRect[4], yGrain )

   RETURN(aRect)

*-----------------------------------------------------------------------------*

FUNCTION DrawHandles( hDC, aRect, hBrush )

   LOCAL nTop
   LOCAL nLeft
   LOCAL aHot := array(8)

   aHot[1] := { { aRect[1], aRect[2], aRect[1] + HANDLESIZE, aRect[2] + HANDLESIZE }, HTTOPLEFT, IDC_SIZENWSE }
   aHot[2] := { { aRect[3] - HANDLESIZE, aRect[2], aRect[3], aRect[2] + HANDLESIZE }, HTTOPRIGHT, IDC_SIZENESW }
   aHot[3] := { { aRect[1], aRect[4] - HANDLESIZE, aRect[1] + HANDLESIZE, aRect[4] }, HTBOTTOMLEFT, IDC_SIZENESW }
   aHot[4] := { { aRect[3] - HANDLESIZE, aRect[4] - HANDLESIZE, aRect[3], aRect[4] }, HTBOTTOMRIGHT, IDC_SIZENWSE }

   nLeft   := ( aRect[1] + aRect[3] ) / 2 - 2
   nTop    := aRect[2]
   aHot[5] := { {nLeft, nTop, nLeft + HANDLESIZE, nTop + HANDLESIZE }, HTTOP, IDC_SIZENS }

   nLeft   := ( aRect[1] + aRect[3] ) / 2 - 2
   nTop    := aRect[4] - HANDLESIZE
   aHot[6] := { { nLeft, nTop , nLeft + HANDLESIZE, nTop + HANDLESIZE }, HTBOTTOM, IDC_SIZENS }

   nLeft   := aRect[1]
   nTop    := ( aRect[2] + aRect[4] ) / 2 - 2
   aHot[7] := { { nLeft, nTop, nLeft + HANDLESIZE, nTop + HANDLESIZE }, HTLEFT, IDC_SIZEWE }

   nLeft   := aRect[3] - HANDLESIZE
   nTop    := ( aRect[2] + aRect[4] ) / 2 - 2
   aHot[8] := { { nLeft, nTop, nLeft + HANDLESIZE, nTop + HANDLESIZE }, HTRIGHT, IDC_SIZEWE }

   aEval( aHot, {|a| FillRect( hDC, a[1], hBrush ) } )

   RETURN(aHot)

*-----------------------------------------------------------------------------*

FUNCTION RectScreenToClient( hWnd, aRect )

   LOCAL aPt

   aPt := { aRect[ 1 ], aRect[ 2 ] }
   ScreenToClient( hWnd, aPt )
   aRect[ 1 ] := aPt[ 1 ]
   aRect[ 2 ] := aPt[ 2 ]

   aPt := { aRect[ 3 ], aRect[ 4 ] }
   ScreenToClient( hWnd, aPt )
   aRect[ 3 ] := aPt[ 1 ]
   aRect[ 4 ] := aPt[ 2 ]

   RETURN( aRect )

*-----------------------------------------------------------------------------*

FUNCTION RectClientToScreen( hWnd, aRect )

   LOCAL aPt

   aPt := { aRect[ 1 ], aRect[ 2 ] }
   ClientToScreen( hWnd, aPt )
   aRect[ 1 ] := aPt[ 1 ]
   aRect[ 2 ] := aPt[ 2 ]

   aPt := { aRect[ 3 ], aRect[ 4 ] }
   ClientToScreen( hWnd, aPt )
   aRect[ 3 ] := aPt[ 1 ]
   aRect[ 4 ] := aPt[ 2 ]

   RETURN( aRect )

*-----------------------------------------------------------------------------*

FUNCTION RaScan( a, b, s, n )  // scans array backwards

  LOCAL i := 0
  LOCAL block
  LOCAL e
  LOCAL r := 0

  IF ValType( b ) # 'B'
    block := {|x| x == b }
  ELSE
    block := b
  ENDIF

  IF ValType( s ) # 'N'
    s := Len( a )
  ELSE
    s := Min( Max( 1, s ), Len( a ) )
  ENDIF

  IF ValType( n ) # 'N'
    n := Len( a )
  ENDIF
  
  n := Min( Max( 0, n), s )
  e := s - n + 1

  FOR i := s To e STEP -1
    IF Eval( block, a[i] )
      r := i
      EXIT
    ENDIF
  NEXT

  RETURN( r )

*------------------------------------------------------------------------------*

FUNCTION RaEval( a, b, s, n)  // traverses array backwards

  LOCAL i
  LOCAL block
  LOCAL e

  IF ValType( s ) # 'N'
    s := Len( a )
  ELSE
    s := Min( Max( 1, s ), Len( a ) )
  ENDIF

  IF ValType( n ) # 'N'
    n := Len( a )
  ENDIF
  
  n := Min( Max( 0, n ), s )
  e := s - n + 1

  FOR i := s To e STEP -1
    Eval( b, a[ i ], i )
  NEXT

  RETURN(a)



