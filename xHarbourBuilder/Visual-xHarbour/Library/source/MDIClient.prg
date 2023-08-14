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

   PROPERTY AlignLeft    ROOT "Layout"
   PROPERTY AlignTop     ROOT "Layout"
   PROPERTY AlignRight   ROOT "Layout"
   PROPERTY AlignBottom  ROOT "Layout"
   PROPERTY WindowsMenu  DEFAULT .T.
   PROPERTY BackColor    ROOT "Colors" GET IIF( ::xBackColor == NIL, ::__SysBackColor, ::xBackColor ) SET ::SetBackColor(v)
   PROPERTY ForeColor    ROOT "Colors" GET IIF( ::xForeColor == NIL, ::__SysForeColor, ::xForeColor );
                         SET IIF( ::IsWindow() .AND. ::IsWindowVisible(), ::InvalidateRect(), NIL )

   PROPERTY Border         ROOT "Appearance" SET ::__SetBorder(v)      DEFAULT 0
   DATA EnumBorder       EXPORTED INIT { { "None", "Single", "Sunken", "Fixed3D" }, { 0, WS_BORDER, WS_EX_STATICEDGE, WS_EX_CLIENTEDGE } }

   PROPERTY Margins      SET ::SetMargins(@v)

   DATA Left               EXPORTED
   DATA Top                EXPORTED
   DATA Width              EXPORTED
   DATA Height             EXPORTED

   DATA FirstChild         EXPORTED
   DATA Parent             EXPORTED
   DATA Style              EXPORTED INIT (WS_CHILD | WS_HSCROLL | WS_VSCROLL | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   DATA ExStyle            EXPORTED INIT 0
   DATA ClsName            EXPORTED
   DATA Name               EXPORTED
   DATA Center             EXPORTED
   DATA MDIClient          EXPORTED
   DATA WindowStyle        EXPORTED
   DATA Tooltip            EXPORTED
   DATA TabStop            EXPORTED INIT .F.
   DATA Text               EXPORTED

   ACCESS Caption     INLINE ::Text
   ASSIGN Caption(c)  INLINE ::Text := c

   DATA Enabled            EXPORTED INIT .T.
   DATA Theming            EXPORTED INIT .T.
   DATA Cursor             EXPORTED
   DATA TabOrder           EXPORTED
   DATA ForeColor          EXPORTED
   DATA ContextMenu        EXPORTED
   DATA __SysBackColor     EXPORTED INIT GetSysColor( COLOR_APPWORKSPACE )
   DATA __SysForeColor     EXPORTED INIT GetSysColor( COLOR_BTNTEXT )
   DATA BkBrush            EXPORTED
   DATA __IsInstance       EXPORTED INIT .F.

   DATA LeftMargin         EXPORTED INIT 0
   DATA TopMargin          EXPORTED INIT 0
   DATA RightMargin        EXPORTED INIT 0
   DATA BottomMargin       EXPORTED INIT 0

   DATA Font               EXPORTED
   ACCESS MDIChild         INLINE (::ExStyle & WS_EX_MDICHILD) != 0
   ACCESS ControlParent    INLINE (::ExStyle & WS_EX_CONTROLPARENT) != 0
   ACCESS Transparent      INLINE (::ExStyle & WS_EX_TRANSPARENT) != 0
   ACCESS Visible          INLINE IIF( ::IsWindow(), ::IsWindowVisible(), (::Style & WS_VISIBLE) != 0 )
   ACCESS ClipChildren     INLINE (::Style & WS_CLIPCHILDREN) != 0
   ACCESS ClipSiblings     INLINE (::Style & WS_CLIPSIBLINGS) != 0

   ACCESS PopUp          INLINE (::Style & WS_POPUP) != 0
   ACCESS ThickFrame     INLINE (::Style & WS_THICKFRAME) != 0
   ACCESS MaximizeBox    INLINE (::Style & WS_MAXIMIZEBOX) != 0
   ACCESS MinimizeBox    INLINE (::Style & WS_MINIMIZEBOX) != 0
   ACCESS CaptionBar     INLINE (::Style & WS_CAPTION) != 0
   ACCESS SysMenu        INLINE (::Style & WS_SYSMENU) != 0

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD MoveWindow()
   METHOD IsWindowVisible()     INLINE IsWindowVisible( ::hWnd )
   METHOD Destroy()             INLINE DeleteObject( ::BkBrush ), DestroyWindow( ::hWnd )
   METHOD SetBackColor()
   METHOD SetExStyle()
   METHOD __ControlProc()
   METHOD __SetBorder()
   METHOD GetActive()
   METHOD SetMargins()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS MDIClient
   ::Left   := 0
   ::Top    := 0
   ::Width  := 0
   ::Height := 0

   ::Parent      := oParent
   ::ClsName     := "MDIClient"
   ::Border      := WS_EX_CLIENTEDGE
   ::__SetBorder( ::xBorder )
   ::WindowStyle := 4

   IF ::Parent != NIL .AND.::Parent:DesignMode
      __SetInitialValues( Self )
   ENDIF

RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS MDIClient
   LOCAL ccs := (struct CLIENTCREATESTRUCT)
   ccs:hWindowMenu  := 0
   ccs:idFirstChild := 10001

   ::ControlParent := .T.
   ::ClipChildren := .T.
   ::hWnd := CreateWindowEx( ::ExStyle, ::ClsName, , ::Style, ::Left, ::Top, ::Width, ::Height, ::Parent:hWnd, 0, ::Parent:Instance, ccs )
   ShowWindow( ::hWnd, SW_SHOW )
   ::__pCallBackPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, "__ControlProc" ), Self )
   ::__nProc := SetWindowLong( ::hWnd, GWL_WNDPROC, ::__pCallBackPtr )

   IF ::BkBrush != NIL
      SetClassLong( ::hWnd, GCL_HBRBACKGROUND, ::BkBrush )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD __SetBorder( nBorder ) CLASS MDIClient
   LOCAL nExStyle, nStyle

   IF VALTYPE( nBorder ) == "L" // backward compatibility
      nBorder := IIF( nBorder, WS_BORDER, 0 )
   ENDIF

   nStyle   := (::Style & NOT( WS_BORDER ))
   nExStyle := (::ExStyle & NOT( WS_EX_STATICEDGE ))
   nExStyle := (nExStyle & NOT( WS_EX_CLIENTEDGE ))

   IF nBorder <> 0
      IF nBorder == WS_BORDER
         nStyle := (nStyle | WS_BORDER)
      ELSE
         nExStyle := (nExStyle | nBorder)
      ENDIF
   ENDIF
   ::Style := nStyle
   ::ExStyle := nExStyle
RETURN nBorder

METHOD SetBackColor( nColor, lRepaint ) CLASS MDIClient

   DEFAULT lRepaint TO .T.
   ::xBackColor := nColor

   IF ::BkBrush != NIL
      DeleteObject( ::BkBrush )
   ENDIF

   IF nColor != NIL .AND. nColor != ::__SysBackColor
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
         ::ExStyle := (::ExStyle | nStyle)
       ELSE
         ::ExStyle := (::ExStyle & NOT( nStyle ))
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
           lShow := (::GetWindowLong( GWL_STYLE ) & WS_MAXIMIZE) != 0
           ::Parent:Children[nwParam]:UpdateMenu( lShow )
           RETURN 1

      CASE WM_ERASEBKGND
           IF ::Parent:__PaintBakgndImage( nwParam, IIF( ::BkBrush == NIL .AND. ::Parent:BkBrush == NIL, GetSysColorBrush( COLOR_APPWORKSPACE ), ::BkBrush ) ) == 1
              RETURN 1
           ENDIF
           EXIT

      CASE WM_SIZE
           IF ::Parent:BackgroundImage != NIL .AND. !EMPTY( ::Parent:BackgroundImage:ImageName )
              ::CallWindowProc()
              ::InvalidateRect()
              ::RedrawWindow( , , (RDW_UPDATENOW | RDW_INTERNALPAINT | RDW_ALLCHILDREN) )
           ENDIF
   END
RETURN CallWindowProc( ::__nProc, hWnd, nMsg, nwParam, nlParam )

METHOD GetActive() CLASS MDIClient
   LOCAL n, hWnd := SendMessage( ::hWnd, WM_MDIGETACTIVE )
   IF( n := ASCAN( ::Children, {|o|o:hWnd == hWnd} ) ) > 0
      RETURN ::Children[n]
   ENDIF
RETURN NIL

METHOD SetMargins( cMargins ) CLASS MDIClient
   LOCAL n, aMargins := hb_atokens( cMargins, "," )
   ::LeftMargin   := 0
   ::TopMargin    := 0
   ::RightMargin  := 0
   ::BottomMargin := 0

   IF LEN( aMargins ) == 1
      ASIZE( aMargins, 4 )
      FOR n := 2 TO 4
          IF aMargins[n] == NIL
             aMargins[n] := aMargins[1]
          ENDIF
      NEXT
    ELSE
      ASIZE( aMargins, 4 )
      FOR n := 1 TO 4
          IF aMargins[n] == NIL
             aMargins[n] := "0"
          ENDIF
      NEXT
   ENDIF

   ::LeftMargin   := VAL( aMargins[1] )
   ::TopMargin    := VAL( aMargins[2] )
   ::RightMargin  := VAL( aMargins[3] )
   ::BottomMargin := VAL( aMargins[4] )

   cMargins := Alltrim( Str( ::LeftMargin ) )+ "," + Alltrim( Str( ::TopMargin ) ) + "," + Alltrim( Str( ::RightMargin ) ) + "," + Alltrim( Str( ::BottomMargin ) )

   IF ::DesignMode .AND. ::Application:ObjectManager != NIL
      ::Application:ObjectManager:PostMessage( WM_USER + 4765 )
      ::MoveWindow()
   ENDIF
RETURN cMargins

METHOD MoveWindow( nLeft, nTop, nWidth, nHeight ) CLASS MDIClient
   LOCAL n

   DEFAULT nLeft   TO 0
   DEFAULT nTop    TO 0
   DEFAULT nWidth  TO ::Parent:ClientWidth
   DEFAULT nHeight TO ::Parent:ClientHeight

   IF VALTYPE( ::AlignLeft ) == "C" .AND. ( n := ASCAN( ::Parent:Children, {|o| o:Name == ::AlignLeft } ) ) > 0
      ::AlignLeft := ::Parent:Children[n]
   ENDIF
   IF VALTYPE( ::AlignTop ) == "C" .AND. ( n := ASCAN( ::Parent:Children, {|o| o:Name == ::AlignTop } ) ) > 0
      ::AlignTop := ::Parent:Children[n]
   ENDIF
   IF VALTYPE( ::AlignRight ) == "C" .AND. ( n := ASCAN( ::Parent:Children, {|o| o:Name == ::AlignRight } ) ) > 0
      ::AlignRight := ::Parent:Children[n]
   ENDIF
   IF VALTYPE( ::AlignBottom ) == "C" .AND. ( n := ASCAN( ::Parent:Children, {|o| o:Name == ::AlignBottom } ) ) > 0
      ::AlignBottom := ::Parent:Children[n]
   ENDIF

   IF ::IsWindow()
      IF VALTYPE( ::AlignLeft ) == "O"
         nLeft   := ::AlignLeft:Left + ::AlignLeft:Width + IIF( ::AlignLeft:RightSplitter != NIL, ::AlignLeft:RightSplitter:Weight, 0 )
      ENDIF
      IF VALTYPE( ::AlignTop ) == "O"
         nTop    := ::AlignTop:Top + ::AlignTop:Height + IIF( ::AlignTop:BottomSplitter != NIL, ::AlignTop:BottomSplitter:Weight, 0 )
      ENDIF
      IF VALTYPE( ::AlignRight ) == "O"
         nWidth  := ::AlignRight:Left - IIF( ::AlignRight:LeftSplitter != NIL, ::AlignRight:LeftSplitter:Weight, 0 )
      ENDIF
      IF VALTYPE( ::AlignBottom ) == "O"
         nHeight := ::AlignBottom:Top - IIF( ::AlignBottom:TopSplitter != NIL, ::AlignBottom:TopSplitter:Weight, 0 )
      ENDIF

      MoveWindow( ::hWnd, nLeft + ::LeftMargin, nTop + ::TopMargin,;
                          nWidth - nLeft - ::RightMargin - ::LeftMargin, nHeight - nTop - ::BottomMargin - ::TopMargin, .T. )
   ENDIF
RETURN Self
