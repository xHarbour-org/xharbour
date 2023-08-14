/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// ProgressBar.prg                                                                                      *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

#define TBPF_NOPROGRESS    0x00000000
#define TBPF_INDETERMINATE 0x00000001
#define TBPF_NORMAL        0x00000002
#define TBPF_ERROR         0x00000004
#define TBPF_PAUSED        0x00000008

#define PBS_MARQUEE        0x08
#define PBM_SETMARQUEE WM_USER + 10

CLASS ProgressBar FROM Control
   PROPERTY Marquee         SET ::SetStyle( PBS_MARQUEE, v )  DEFAULT .F.
   PROPERTY MarqueeSeconds  SET ::__SetMarqueeSeconds(v)      DEFAULT 0
   PROPERTY TaskBarProgress                                   DEFAULT .F.
   PROPERTY MinRange        SET ::SetRange(v,::xMaxRange)     DEFAULT 0
   PROPERTY MaxRange        SET ::SetRange(::xMinRange,v)     DEFAULT 100
   PROPERTY Position        SET ::SetPosition( v )            DEFAULT 0
   PROPERTY Smooth          SET ::SetStyle( PBS_SMOOTH, v )   DEFAULT .F.
   PROPERTY ForeColor       ROOT "Colors" SET ::SetBarColor( v )            DEFAULT GetSysColor( COLOR_HIGHLIGHT )
   PROPERTY BackColor       ROOT "Colors" SET ::SetBkColor( v )             DEFAULT GetSysColor( COLOR_BTNFACE )
   PROPERTY Step            SET ::SetStep( v )                DEFAULT 10
   PROPERTY Vertical        SET ::SetStyle( PBS_VERTICAL, v ) DEFAULT .F.

   DATA AllowUnDock     EXPORTED INIT FALSE
   DATA AllowClose      EXPORTED INIT FALSE
   DATA ImageList       EXPORTED
   DATA ImageIndex      PROTECTED

   DATA PropParent  PROTECTED

   METHOD Init()    CONSTRUCTOR
   METHOD Create()
   METHOD DeltaPos( n )    INLINE ::SendMessage( PBM_DELTAPOS, n, 0 )

   METHOD SetRange(x,y)    INLINE IIF( ::IsWindow(), ::SendMessage( PBM_SETRANGE, 0, MAKELONG( x, y ) ), NIL )
   METHOD SetPosition()
   METHOD SetBarColor( n ) INLINE ::xForeColor := n, IIF( ::IsWindow(), ::SendMessage( PBM_SETBARCOLOR, 0, n ), NIL )
   METHOD StepIt()         INLINE ::SendMessage( PBM_STEPIT, 0, 0 )

   METHOD SetBkColor( n )  INLINE ::xBackColor := n, IIF( ::IsWindow(), ::SendMessage( PBM_SETBKCOLOR, 0, IIF(n==NIL,GetSysColor( COLOR_BTNFACE ),n) ), NIL )
   METHOD SetStep( n )     INLINE ::xStep      := n, IIF( ::IsWindow(), ::SendMessage( PBM_SETSTEP, n, 0 ), NIL )
   METHOD OnDestroy()
   METHOD __SetMarqueeSeconds()
   METHOD OnUserMsg()
   METHOD SetMarquee( l )   INLINE ::Marquee := l, ::SendMessage( PBM_SETMARQUEE, l, ::xMarqueeSeconds ), ::Redraw()
   METHOD OnTimer()         INLINE 0
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ProgressBar
   LOCAL aRect
   DEFAULT ::__xCtrlName TO "ProgressBar"
   ::ClsName   := PROGRESS_CLASS
   ::ThemeName := "progress"
   ::Style     := (WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
   ::Super:Init( oParent )
   ::Width     := 150
   ::Height    := 20
   IF oParent:__xCtrlName == "StatusBarPanel"
      aRect    := oParent:GetRect()
      ::Left   := aRect[1]
      ::Top    := aRect[2]
      ::Width  := aRect[3]-aRect[1]
      ::Height := aRect[4]-aRect[2]
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS ProgressBar
   ::Super:Create()

   ::SetRange( ::xMinRange,::xMaxRange )
   ::SetPosition( ::Position )
   ::SetBarColor( ::ForeColor )
   ::__SetMarqueeSeconds( ::xMarqueeSeconds )
   IF ::Step != NIL
      ::SetStep( ::Step )
   ENDIF
   IF ::BackColor != NIL
      ::SetBkColor( ::BackColor )
   ENDIF
   IF ! ::DesignMode .AND. ::TaskBarProgress
      ::PostMessage( WM_USER + 555 )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD __SetMarqueeSeconds( nSecs ) CLASS ProgressBar
   IF ! ::DesignMode .AND. ::IsWindow()
      ::SendMessage( PBM_SETMARQUEE, ::xMarquee, nSecs )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetPosition( n ) CLASS ProgressBar
   ::xPosition  := n
   IF ::IsWindow()
      ::SendMessage( PBM_SETPOS, n, 0 )
      IF ::DesignMode .AND. ::Application:OsVersion:dwMajorVersion > 5
         sleep(500)
         ::SendMessage( PBM_SETPOS, n+1, 0 )
         ::SendMessage( PBM_SETPOS, n, 0 )
      ENDIF
      IF ! ::DesignMode .AND. ::TaskBarProgress
         IF ! ::xMarquee
            TaskBarProgressValue( IIF( ::Form:Parent != NIL, ::Form:Parent:hWnd, ::Form:hWnd ), n, ::xMaxRange )
          ELSE
            TaskBarProgressState( IIF( ::Form:Parent != NIL, ::Form:Parent:hWnd, ::Form:hWnd ), TBPF_INDETERMINATE )
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnDestroy() CLASS ProgressBar
   IF ! ::DesignMode .AND. ::TaskBarProgress
      TaskBarProgressState( IIF( ::Form:Parent != NIL, ::Form:Parent:hWnd, ::Form:hWnd ), TBPF_NOPROGRESS )
   ENDIF
   Super:OnDestroy()
RETURN NIL

METHOD OnUserMsg( hWnd, nMsg ) CLASS ProgressBar
   (hWnd)
   IF nMsg == WM_USER + 555
      IF ::xMarquee
         TaskBarProgressState( IIF( ::Form:Parent != NIL, ::Form:Parent:hWnd, ::Form:hWnd ), TBPF_INDETERMINATE )
       ELSE
         TaskBarProgressValue( IIF( ::Form:Parent != NIL, ::Form:Parent:hWnd, ::Form:hWnd ), ::Position, ::xMaxRange )
      ENDIF
   ENDIF
RETURN NIL
