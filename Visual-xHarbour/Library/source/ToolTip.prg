/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// ToolTip.prg                                                                                          *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"
#Include "colors.ch"

CLASS ToolTip INHERIT Window
   PROPERTY BackColor     ROOT "Colors" SET IIF( ::hWnd != NIL .AND. v != NIL, ::SendMessage( TTM_SETTIPBKCOLOR, v, 0 ), )   DEFAULT GetSysColor( COLOR_INFOBK )
   PROPERTY ForeColor     ROOT "Colors" SET IIF( ::hWnd != NIL .AND. v != NIL, ::SendMessage( TTM_SETTIPTEXTCOLOR, v, 0 ), ) DEFAULT GetSysColor( COLOR_INFOTEXT )

   PROPERTY Text          SET ::SetText( v )                                                 DEFAULT ""
   PROPERTY CloseButton   SET ::SetStyle( TTS_CLOSE, v )                                     DEFAULT .F.
   PROPERTY Balloon       SET ( ::SetStyle( TTS_BALLOON, v ), ::SetStyle( WS_BORDER, .F. ) ) DEFAULT .F.
   PROPERTY Animate       SET ::SetStyle( TTS_NOANIMATE, v )                                 DEFAULT .F.
   PROPERTY Title         SET ::SetTitle( ::Icon, v  )                                       DEFAULT ""
   PROPERTY Track         SET ::SetTTStyle( TTF_TRACK, v )                                   DEFAULT .F.
   PROPERTY Transparent   SET ::SetTTStyle( TTF_TRANSPARENT, v )                             DEFAULT .F.
   PROPERTY CenterTip     SET ::SetTTStyle( TTF_CENTERTIP, v )                               DEFAULT .F.
   PROPERTY Absolute      SET ::SetTTStyle( TTF_ABSOLUTE, v )                                DEFAULT .T.
   PROPERTY Icon                                                                             DEFAULT 0
   PROPERTY CloseOnClick                                                                     DEFAULT .F.

   DATA TabOrder         EXPORTED
   DATA Theming          EXPORTED  INIT .T.
   DATA ContextMenu      EXPORTED
   DATA Cursor           EXPORTED
   DATA Top              EXPORTED
   DATA Dock             EXPORTED
   DATA Left             EXPORTED
   DATA Width            EXPORTED
   DATA Height           EXPORTED


   // disable persistants from class Window
   DATA ClsName          EXPORTED
   DATA Caption          EXPORTED
   DATA Name             EXPORTED
   DATA AutoClose        EXPORTED INIT .T.
   DATA ShowMode         EXPORTED INIT 1

   DATA ToolTip          EXPORTED


   ACCESS Center         INLINE ::CenterTip
   ASSIGN Center(l)      INLINE ::CenterTip := l

   ACCESS Visible        INLINE (::Style & WS_VISIBLE) != 0
   ACCESS Transparent    INLINE (::ExStyle & WS_EX_TRANSPARENT) != 0

   ACCESS Child          INLINE (::Style & WS_CHILD) != 0
   ACCESS ControlParent  INLINE (::ExStyle & WS_EX_CONTROLPARENT) != 0
   ACCESS MdiContainer   INLINE ::xMdiContainer
   ACCESS TabStop        INLINE (::Style & WS_TABSTOP) != 0
   ACCESS TopMost        INLINE (::ExStyle & WS_EX_TOPMOST) != 0
   //------------------------------------------

   DATA MDIClient         PROTECTED
   DATA __SysBackColor    EXPORTED INIT GetSysColor( COLOR_INFOBK )
   DATA __SysForeColor    EXPORTED INIT GetSysColor( COLOR_INFOTEXT )

   ACCESS ClipChildren    INLINE (::Style & WS_CLIPCHILDREN) != 0
   ASSIGN ClipChildren(l) INLINE ::SetStyle( WS_CLIPCHILDREN, l )

   ACCESS ClipSiblings    INLINE (::Style & WS_CLIPSIBLINGS) != 0
   ASSIGN ClipSiblings(l) INLINE ::SetStyle( WS_CLIPSIBLINGS, l )

   DATA Tip               EXPORTED AS OBJECT
   DATA TTStyle           EXPORTED INIT (TTF_SUBCLASS | TTF_IDISHWND | TTF_ABSOLUTE)
   DATA xMdiContainer     EXPORTED INIT .F.

   ACCESS MdiContainer    INLINE    ::xMdiContainer

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Destroy()
   METHOD TrackActivate()
   METHOD Pop()                  INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_POP, 0, 0 ), NIL )
   METHOD PopUp()                INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_POPUP, 0, 0 ), NIL )
   METHOD Activate(lAct)         INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_ACTIVATE, lAct, 0), NIL )
   METHOD SetTitle(nIcon,cTitle) INLINE ::Icon := IFNIL( nIcon, ::Icon, nIcon ),;
                                        ::xTitle := cTitle,;
                                        IIF( ::hWnd != NIL, ::SendMessage( TTM_SETTITLE, ::Icon, ::xTitle ), NIL )

   METHOD SetTTStyle()
   METHOD GetMargin()
   METHOD SetDelayTime(nmSec)    INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_SETDELAYTIME, TTDT_INITIAL,   nmSec ), NIL )
   METHOD SetAutoPop(nmSec)      INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_SETDELAYTIME, TTDT_AUTOPOP,   nmSec ), NIL )
   METHOD SetReShow(nmSec)       INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_SETDELAYTIME, TTDT_RESHOW,    nmSec ), NIL )
   METHOD SetAutomatic(nmSec)    INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_SETDELAYTIME, TTDT_AUTOMATIC, nmSec ), NIL )
   METHOD SetMaxTipWidth(nMax)   INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_SETMAXTIPWIDTH, 0, nMax ), NIL )
   METHOD TrackPosition(x,y)     INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_TRACKPOSITION , 0, MAKELONG( x, y ) ), NIL )
   METHOD Update()               INLINE IIF( ::hWnd != NIL, ::SendMessage( TTM_UPDATE, 0, 0 ), NIL )
   METHOD SetNewRect()
   METHOD SetText()
   METHOD OnLButtonDown()
   METHOD AlignToParent()        INLINE SetParent( ::hWnd, ::Parent:hWnd ), ::Update()
   METHOD ShowAt( x, y )         INLINE ::Parent:SendMessage( WM_MOUSEMOVE, 0, MAKELONG( x, y ), ::Popup() )
   METHOD Show()                 INLINE NIL
   METHOD OnParentNotify()
   METHOD OnTimer()
ENDCLASS

METHOD Init( oParent ) CLASS ToolTip
   ::__xCtrlName    := "ToolTip"
   ::ClsName      := TOOLTIPS_CLASS
   ::Super:Init( oParent )
   ::Style        := (WS_POPUP | WS_BORDER | TTS_NOPREFIX | TTS_ALWAYSTIP)
   ::ExStyle      := WS_EX_TOPMOST
   ::Left         := CW_USEDEFAULT
   ::Top          := CW_USEDEFAULT
   ::Width        := CW_USEDEFAULT
   ::Height       := CW_USEDEFAULT
   oParent := NIL
RETURN( self )

METHOD Create() CLASS ToolTip
   ::SetChildren := .F.
   IF ! ::DesignMode
      ::Super:Create()

      ::Tip             := (struct TOOLINFO)
      ::Tip:cbSize      := ::Tip:sizeof()
      ::Tip:uFlags      := ::TTStyle
      ::Tip:hwnd        := ::Parent:hWnd
      ::Tip:uId         := ::Parent:hWnd
      ::Tip:lpszText    := ::Text

      ::Tip:rect:left   := ::Left
      ::Tip:rect:top    := ::Top
      ::Tip:rect:right  := ::Width
      ::Tip:rect:bottom := ::Height

      SendMessage( ::hWnd, TTM_ADDTOOL, 0, ::Tip )

      IF !EMPTY( ::Title )
         ::SetTitle( ::Icon, ::xTitle )
      ENDIF

      ::BackColor       := ::xBackColor
      ::ForeColor       := ::xForeColor
      ::Show( SW_SHOWNOACTIVATE )
   ENDIF
RETURN( Self )

METHOD SetTTStyle( nStyle, lSet ) CLASS ToolTip
   IF lSet
      ::TTStyle := (::TTStyle | nStyle)
    ELSE
      ::TTStyle := (::TTStyle & NOT( nStyle ))
   ENDIF
   IF ::hWnd != NIL
      ::Tip:uFlags := ::TTStyle
      SendMessage( ::hWnd, TTM_SETTOOLINFO, 0, ::Tip )
   ENDIF
RETURN Self

METHOD Destroy() CLASS ToolTip
   SendMessage( ::hWnd, TTM_DELTOOL, 0, ::Tip )
   ::Tip := NIL
RETURN Super:Destroy()

METHOD GetMargin() CLASS ToolTip
   LOCAL rc := (struct RECT)
   SendMessage( ::hWnd, TTM_GETMARGIN, 0, rc )
   rc:Scatter()
RETURN rc

//------------------------------------------------------------------------------------------------

METHOD SetNewRect( aRect ) CLASS ToolTip
   ::Tip:rect:left   := aRect[1]
   ::Tip:rect:top    := aRect[2]
   ::Tip:rect:right  := aRect[3]
   ::Tip:rect:bottom := aRect[4]
   SendMessage( ::hWnd, TTM_NEWTOOLRECT, 0, ::Tip )
RETURN( Self )

METHOD TrackActivate( lSet ) CLASS ToolTip
   local ti := (struct TOOLINFO)
   DEFAULT lSet TO .T.
   ti:cbSize      := ti:sizeof()
   ti:uFlags      := ::TTStyle
   ti:hwnd        := ::Parent:hWnd
   ti:uId         := ::Parent:hWnd
   SendMessage( ::hWnd, TTM_TRACKACTIVATE, lSet, Ti )
RETURN Self

METHOD SetText(c) CLASS ToolTip
   ::xText := c
   IF IsWindow( ::hWnd )
      ::tip:lpszText := ::xText
      SendMessage( ::hWnd, TTM_UPDATETIPTEXT , 0, ::tip )
   ENDIF
RETURN Self

METHOD OnLButtonDown( n, x, y ) CLASS ToolTip
   LOCAL pt
   IF (::Tip:uflags & TTF_TRACK) == TTF_TRACK .AND. ::CloseOnClick
      ::TrackActivate( .F. )
      ::PopUp()

      pt := (struct POINT)
      pt:x := x
      pt:y := y
      ClientToScreen( ::hWnd, @pt )
      ScreenToClient( ::Parent:hWnd, @pt )
      PostMessage( ::Parent:hWnd, WM_LBUTTONDOWN, n, MAKELONG( pt:x, pt:y ) )
      RETURN 0
   ENDIF
RETURN NIL

METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS ToolTip
   (nwParam, nlParam)
   IF hdr:code == TTN_POP
      ::Parent:__lPopTip := .T.
      RETURN 0
   ENDIF
RETURN NIL

METHOD OnTimer( nTimer ) CLASS ToolTip
   IF nTimer == 25
      ::KillTimer( 25 )
      ::Popup()
      RETURN 0
   ENDIF
RETURN NIL
