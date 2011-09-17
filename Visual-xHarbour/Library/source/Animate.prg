/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Animation.prg                                                                                        *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"
#include "colors.ch"

//-----------------------------------------------------------------------------------------------
// 150 = Shell Move File Flying papers
CLASS Animation INHERIT Control

   PROPERTY ImageName                         READ xImageName   WRITE __SetImageName INVERT
   PROPERTY Centered    INDEX ACS_CENTER      READ xCentered    WRITE SetStyle DEFAULT .F. PROTECTED
   PROPERTY Transparent INDEX ACS_TRANSPARENT READ xTransparent WRITE SetStyle DEFAULT .F. PROTECTED
   PROPERTY AutoPlay    INDEX ACS_AUTOPLAY    READ xAutoPlay    WRITE SetStyle DEFAULT .F. PROTECTED
   PROPERTY SystemAnimation READ xSystemAnimation WRITE Open     DEFAULT 0   PROTECTED

   DATA FromFrame EXPORTED INIT 0
   DATA ToFrame   EXPORTED INIT -1
   DATA Repeat    PUBLISHED INIT -1

   // 161
   DATA SysAnimations EXPORTED INIT {"FileMove", "FileCopy", "FileRecycle", "EmptyRecycle", "FileDel", "FileNuke", "Web-Search" }
   EXPORTED:
      DATA ImageList
      DATA ImageIndex
      DATA ClipChildren
      DATA ClipSiblings
      DATA TabStop
      DATA Border
      DATA __ExplorerFilter INIT {;
                                  { "Video for Windows (*.avi)", "*.avi" };
                                  }

   METHOD Init()             CONSTRUCTOR
   METHOD Play()             INLINE SendMessage( ::hWnd, ACM_PLAY, ::Repeat, MAKELONG( ::FromFrame, ::ToFrame ) )
   METHOD Stop()             INLINE SendMessage( ::hWnd, ACM_STOP, 0, 0 )
   METHOD Seek( n )          INLINE ::FromFrame := n, ::ToFrame := n, ::Play()
   METHOD Close()            INLINE ::ImageName := NIL
   METHOD __WindowDestroy()  INLINE IIF( ::ImageName != NIL, ::ImageName := NIL,), ::Super:__WindowDestroy()
   METHOD OnCtlColorStatic()
   METHOD OnEraseBkGnd()
   METHOD Create()
   METHOD Open()
   METHOD __SetImageName()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Animation
   DEFAULT ::__xCtrlName TO "Animation"
   ::Style     := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ClsName   := ANIMATE_CLASS
   ::Super:Init( oParent )
   ::Width     := 40
   ::Height    := 40
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Open( cFile ) CLASS Animation
   LOCAL hAvi
   ::xImageName := cFile
   IF ::hWnd != NIL
      IF !EMPTY( ::xImageName )
         SendMessage( ::hWnd, ACM_OPEN, ::AppInstance, ::xImageName )
       ELSEIF ::SystemAnimation >= 0
         hAvi := LoadLibrary( "Shell32.dll" )
         SendMessage( ::hWnd, ACM_OPEN, hAvi, ::SystemAnimation )
         FreeLibrary( hAvi )
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD __SetImageName( cFile ) CLASS Animation
   LOCAL cType, cPrev
   IF VALTYPE( cFile ) == "A"
      cFile := IIF( ::__ClassInst != NIL .AND. VALTYPE( cFile[1] ) == "C", cFile[1], cFile[2] )
   ENDIF
   cPrev := ::xImageName
   
   IF !EMPTY( cFile )
      ::Open( cFile )
    ELSE
      ::xImageName := NIL
   ENDIF

   IF ::__ClassInst != NIL 
      IF !EMPTY( cPrev )
         ::Application:Project:RemoveImage( cPrev, Self )
      ENDIF
      IF !EMPTY( cFile )
         DEFAULT cType TO RIGHT( UPPER( cFile ), 3 )
         ::Application:Project:AddImage( cFile, cType, Self )
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS Animation
   IF ::__ClassInst != NIL
      ::Style := ::Style & NOT( ACS_AUTOPLAY )
   ENDIF
   ::Super:Create()
   IF ::ImageName != NIL .OR. ::SystemAnimation >= 0
      ::Open( ::xImageName )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD OnCtlColorStatic( nwParam ) CLASS Animation
   LOCAL hBrush := ::BkBrush
   DEFAULT hBrush TO ::Parent:BkBrush
   DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )
   SetBkColor( nwParam, ::BackColor )
RETURN hBrush

METHOD OnEraseBkGnd( nwParam ) CLASS Animation
   LOCAL hBrush := ::BkBrush
   DEFAULT hBrush TO ::Parent:BkBrush
   DEFAULT hBrush TO GetSysColorBrush( COLOR_BTNFACE )
   ::GetClientRect()
   _FillRect( nwParam, { 0, 0, ::ClientWidth, ::ClientHeight }, hBrush )
RETURN 1
