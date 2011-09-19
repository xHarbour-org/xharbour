/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// MDIClient.prg                                                                                        *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"

#define WM_MDICHILDSIZED      4500

CLASS MDIClient INHERIT Window
   DATA WindowMenu      EXPORTED
   DATA FirstChild      EXPORTED
   DATA Parent          EXPORTED
   DATA Left            EXPORTED
   DATA Top             EXPORTED
   DATA Width           EXPORTED
   DATA Height          EXPORTED
   DATA Style           EXPORTED INIT WS_CHILD | WS_HSCROLL | WS_VSCROLL | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   DATA ExStyle         EXPORTED
   DATA ClsName         EXPORTED
   DATA Name            EXPORTED
   DATA DisableParent   EXPORTED INIT .F.
   DATA WindowMenu      EXPORTED
   DATA Center          EXPORTED
   DATA MDIClient       EXPORTED
   DATA WindowStyle     EXPORTED
   DATA Tooltip         EXPORTED
   DATA __ClientStruct  EXPORTED
   DATA TabStop         EXPORTED INIT .F.
   DATA Caption         EXPORTED
   DATA Enabled         EXPORTED INIT .T.
   DATA Theming         EXPORTED INIT .T.
   DATA Cursor          EXPORTED
   DATA TabOrder        EXPORTED
   DATA ForeColor       EXPORTED
   DATA ContextMenu     EXPORTED
   DATA BackSysColor     EXPORTED INIT GetSysColor( COLOR_APPWORKSPACE )
   DATA ForeSysColor     EXPORTED INIT GetSysColor( COLOR_BTNTEXT )

   DATA xBackColor       EXPORTED
   ACCESS BackColor      INLINE IIF( ::xBackColor == NIL, ::BackSysColor, ::xBackColor ) PERSISTENT
   ASSIGN BackColor( n ) INLINE ::xBackColor := n, ::SetBackColor( n )

   DATA xForeColor       EXPORTED
   ACCESS ForeColor      INLINE IIF( ::xForeColor == NIL, ::ForeSysColor, ::xForeColor ) PERSISTENT
   ASSIGN ForeColor( n ) INLINE ::xForeColor := n, IIF( ::IsWindow() .AND. ::IsWindowVisible(), ::InvalidateRect(), NIL )

   DATA BkBrush          EXPORTED
   DATA __IsInstance       EXPORTED INIT .F.


   DATA Font             EXPORTED
   ACCESS StaticEdge       INLINE ::ExStyle & WS_EX_STATICEDGE != 0
   ACCESS MDIChild         INLINE ::ExStyle & WS_EX_MDICHILD != 0
   ACCESS ControlParent    INLINE ::ExStyle & WS_EX_CONTROLPARENT != 0
   ACCESS Transparent      INLINE ::ExStyle & WS_EX_TRANSPARENT != 0
   ACCESS Visible          INLINE IIF( ::IsWindow(), ::IsWindowVisible(), ::Style & WS_VISIBLE != 0 )
   ACCESS ClipChildren     INLINE ::Style & WS_CLIPCHILDREN != 0
   ACCESS ClipSiblings     INLINE ::Style & WS_CLIPSIBLINGS != 0

   ACCESS Border         INLINE ::Style & WS_BORDER != 0
   ACCESS PopUp          INLINE ::Style & WS_POPUP != 0
   ACCESS ThickFrame     INLINE ::Style & WS_THICKFRAME != 0
   ACCESS MaximizeBox    INLINE ::Style & WS_MAXIMIZEBOX != 0
   ACCESS MinimizeBox    INLINE ::Style & WS_MINIMIZEBOX != 0
   ACCESS CaptionBar     INLINE ::Style & WS_CAPTION != 0
   ACCESS SysMenu        INLINE ::Style & WS_SYSMENU != 0

   PROPERTY ClientEdge INDEX WS_EX_CLIENTEDGE READ xClientEdge WRITE SetExStyle DEFAULT .T. PROTECTED

   DATA AlignLeft   PUBLISHED
   DATA AlignTop    PUBLISHED
   DATA AlignRight  PUBLISHED
   DATA AlignBottom PUBLISHED

   DATA WindowsMenu PUBLISHED INIT .T.

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD MoveWindow()
   METHOD IsWindowVisible()     INLINE IsWindowVisible( ::hWnd )
   METHOD Destroy()             INLINE DeleteObject( ::BkBrush ), DestroyWindow( ::hWnd )
   METHOD SetBackColor()
   METHOD SetExStyle()
   METHOD __ControlProc()
   METHOD GetActive()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS MDIClient

   ::Left   := 0
   ::Top    := 0
   ::Width  := 0
   ::Height := 0
   ::WindowMenu := 0
   ::FirstChild := 100

   ::Parent      := oParent
   ::ClsName        := "MDIClient"
   ::ExStyle     := WS_EX_CLIENTEDGE
   ::WindowStyle := 4
   //::Form        := oParent
   ::__ClientStruct := (struct CLIENTCREATESTRUCT)
   ::__ClientStruct:hWindowMenu  := ::WindowMenu
   ::__ClientStruct:idFirstChild := ::FirstChild

RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS MDIClient
   ::ControlParent := .T.
   IF ::Parent != NIL .AND.::Parent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
      ::__ClassInst:__IsInstance   := .T.
   ENDIF
   ::ClipChildren := .T.
   ::hWnd := CreateWindowEx( ::ExStyle, ::ClsName, , ::Style, ::Left, ::Top, ::Width, ::Height, ::Parent:hWnd, 0, ::Parent:Instance, ::__ClientStruct )
   ShowWindow( ::hWnd, SW_SHOW )
   ::__pCallBackPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, "__ControlProc" ), Self )
   ::__nProc := SetWindowLong( ::hWnd, GWL_WNDPROC, ::__pCallBackPtr )

   IF ::BkBrush != NIL
      SetClassLong( ::hWnd, GCL_HBRBACKGROUND, ::BkBrush )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD MoveWindow( x, y, w, h, lRep ) CLASS MDIClient
   DEFAULT x    TO ::Left
   DEFAULT y    TO ::Top
   DEFAULT w    TO ::Width
   DEFAULT h    TO ::Height
   DEFAULT lRep TO ::IsWindowVisible()
   ::Left  := x
   ::Top   := y
   ::width := w
   ::height:= h

   MoveWindow( ::hWnd, ::Left, ::Top, ::Width, ::Height, lRep )
RETURN Self

METHOD SetBackColor( nColor, lRepaint ) CLASS MDIClient

   DEFAULT lRepaint TO .T.
   ::xBackColor := nColor

   IF ::BkBrush != NIL
      DeleteObject( ::BkBrush )
   ENDIF

   IF nColor != NIL .AND. nColor != ::BackSysColor
      ::BkBrush := CreateSolidBrush( nColor )
      IF ::hWnd != NIL
         SetClassLong( ::hWnd, GCL_HBRBACKGROUND, ::BkBrush )
      ENDIF
    ELSE
      IF ::hWnd != NIL
         SetClassLong( ::hWnd, GCL_HBRBACKGROUND, GetSysColorBrush( COLOR_APPWORKSPACE ) )
      ENDIF
   ENDIF
   IF IsWindowVisible( ::hWnd )
      InValidateRect( ::hWnd )
   ENDIF
RETURN Self

METHOD SetExStyle(nStyle,lAdd) CLASS MDIClient
   DEFAULT lAdd TO .T.
   IF !::__IsInstance
      IF IsWindow( ::hWnd )
         ::ExStyle := GetWindowLong( ::hWnd, GWL_EXSTYLE )
      ENDIF
      IF lAdd
         ::ExStyle := ::ExStyle | nStyle
       ELSE
         ::ExStyle := ::ExStyle & NOT( nStyle )
      ENDIF
      IF IsWindow( ::hWnd )
         SetWindowLong( ::hWnd, GWL_EXSTYLE, ::ExStyle )
         SetWindowPos( ::hWnd,,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
         RedrawWindow( ::hWnd, , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
      ENDIF
   ENDIF
RETURN Self

METHOD __ControlProc( hWnd, nMsg, nwParam, nlParam ) CLASS MDIClient
   LOCAL lShow

   SWITCH nMsg
      CASE WM_MDICHILDSIZED
           lShow := ::GetWindowLong( GWL_STYLE ) & WS_MAXIMIZE != 0
           ::Parent:Children[nwParam]:UpdateMenu( lShow )
           RETURN 1
   END
RETURN CallWindowProc( ::__nProc, hWnd, nMsg, nwParam, nlParam )

METHOD GetActive() CLASS MDIClient
   LOCAL n, hWnd := SendMessage( ::hWnd, WM_MDIGETACTIVE )
   IF( n := ASCAN( ::Children, {|o|o:hWnd == hWnd} ) ) > 0
      RETURN ::Children[n]
   ENDIF
RETURN NIL
