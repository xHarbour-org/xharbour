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

CLASS ProgressBar FROM Control
   DATA AllowUnDock          EXPORTED INIT FALSE
   DATA AllowClose           EXPORTED INIT FALSE
   DATA ImageList  EXPORTED
   DATA ImageIndex PROTECTED
   DATA xRange      PROTECTED INIT { 0, 100 }

   ACCESS Range         INLINE ::xRange
   ASSIGN Range( a )    INLINE ::SetRange( a )

   ACCESS MinRange      INLINE ::xRange[1] PERSISTENT
   ASSIGN MinRange( n ) INLINE ::xRange[1]:=n, ::SetRange( ::xRange )

   ACCESS MaxRange      INLINE ::xRange[2] PERSISTENT
   ASSIGN MaxRange( n ) INLINE ::xRange[2]:=n, ::SetRange( ::xRange )

   DATA xPosition   PROTECTED INIT 0
   ACCESS Position      INLINE ::xPosition PERSISTENT
   ASSIGN Position( n ) INLINE ::SetPosition( n )

   DATA xSmooth     PROTECTED INIT .F.
   ACCESS Smooth        INLINE ::xSmooth PERSISTENT
   ASSIGN Smooth( l )   INLINE ::xSmooth := IIF( l == NIL, .F., l ), ::SetStyle( PBS_SMOOTH, ::xSmooth )

   DATA xForeColor   EXPORTED INIT GetSysColor( COLOR_HIGHLIGHT )
   ACCESS ForeColor      INLINE ::xForeColor PERSISTENT
   ASSIGN ForeColor( n ) INLINE ::SetBarColor( n )

   DATA xBackColor    EXPORTED INIT GetSysColor( COLOR_BTNFACE )
   ACCESS BackColor       INLINE ::xBackColor PERSISTENT
   ASSIGN BackColor( n )  INLINE ::SetBkColor( n )

   DATA xStep       PROTECTED  INIT 10
   ACCESS Step          INLINE ::xStep PERSISTENT
   ASSIGN Step( n )     INLINE ::SetStep( n )

   DATA xVertical   PROTECTED INIT .F.
   ACCESS Vertical      INLINE ::xVertical PERSISTENT
   ASSIGN Vertical( l ) INLINE ::xVertical := IIF( l == NIL, .F., l ), ::SetStyle( PBS_VERTICAL, ::xVertical )
 
   ACCESS Border           INLINE ::Style & WS_BORDER != 0 PERSISTENT
   ASSIGN Border(l)        INLINE ::SetStyle( WS_BORDER, l ), ::SetPosition( ::xPosition )

   DATA PropParent  PROTECTED
   
   METHOD Init()    CONSTRUCTOR
   METHOD Create()

   METHOD DeltaPos( n )    INLINE ::SendMessage( PBM_DELTAPOS, n, 0 )

   METHOD SetRange( a )    INLINE ::xRange     := a, IIF( ::IsWindow(), ::SendMessage( PBM_SETRANGE, 0, MAKELONG( a[1], a[2] ) ), NIL )
   METHOD SetPosition()
   METHOD SetBarColor( n ) INLINE ::xForeColor := n, IIF( ::IsWindow(), ::SendMessage( PBM_SETBARCOLOR, 0, n ), NIL )
   METHOD StepIt()         INLINE ::SendMessage( PBM_STEPIT, 0, 0 )

   METHOD SetBkColor( n )  INLINE ::xBackColor := n, IIF( ::IsWindow(), ::SendMessage( PBM_SETBKCOLOR, 0, IIF(n==NIL,GetSysColor( COLOR_BTNFACE ),n) ), NIL )
   METHOD SetStep( n )     INLINE ::xStep      := n, IIF( ::IsWindow(), ::SendMessage( PBM_SETSTEP, n, 0 ), NIL )
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS ProgressBar
   LOCAL aRect
   DEFAULT ::__xCtrlName TO "ProgressBar"
   ::ClsName   := PROGRESS_CLASS
   ::ThemeName := "progress"
   ::Style     := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
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

   ::SetRange( ::Range )
   ::SetPosition( ::Position )
   ::SetBarColor( ::ForeColor )
   IF ::Step != NIL
      ::SetStep( ::Step )
   ENDIF
   IF ::BackColor != NIL
      ::SetBkColor( ::BackColor )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetPosition( n ) CLASS ProgressBar
   ::xPosition  := n
   IF ::IsWindow()
      ::SendMessage( PBM_SETPOS, n, 0 )
      IF ::__ClassInst != NIL .AND. ::Application:OsVersion:dwMajorVersion > 5
         sleep(500)
         ::SendMessage( PBM_SETPOS, n+1, 0 )
         ::SendMessage( PBM_SETPOS, n, 0 )
      ENDIF
   ENDIF
RETURN NIL