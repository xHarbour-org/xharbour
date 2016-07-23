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

static hAvi

EXIT PROCEDURE __CleanUpAnimation
   IF hAvi != NIL
      FreeLibrary( hAvi )
   ENDIF
RETURN

//-----------------------------------------------------------------------------------------------
// 150 = Shell Move File Flying papers
CLASS Animation INHERIT Control

   PROPERTY ImageName       ROOT "Appearance" SET ::__SetImageName(@v)
   PROPERTY Transparent     ROOT "Appearance" SET ::SetStyle( ACS_TRANSPARENT, v ) DEFAULT .F. PROTECTED
   PROPERTY AutoPlay        ROOT "Behavior"   SET ::SetStyle( ACS_AUTOPLAY, v )    DEFAULT .F. PROTECTED
   PROPERTY Centered        ROOT "Behavior"   SET ::SetStyle( ACS_CENTER, v )      DEFAULT .F. PROTECTED
   PROPERTY SystemAnimation ROOT "Behavior"   SET ::Open(v)                        DEFAULT 0   PROTECTED
   PROPERTY Repeat          ROOT "Behavior"                                        DEFAULT -1

   DATA FromFrame EXPORTED INIT 0
   DATA ToFrame   EXPORTED INIT -1

   DATA EnumSystemAnimation EXPORTED INIT { {"None", "Search flashlight",;
                                                     "Search documents",;
                                                     "Search computer",;
                                                     "Move files",;
                                                     "Copy files",;
                                                     "Delete files",;
                                                     "Empty trash",;
                                                     "Empty folder",;
                                                     "Check files",;
                                                     "Search internet",;
                                                     "Move files",;
                                                     "Copy files",;
                                                     "Empty folder",;
                                                     "Download files"},;
                                            {0, 150,151,152,160,161,162,163,164,165,166,167,168,169,170} }
   EXPORTED:
      DATA ImageList
      DATA ImageIndex
      DATA ClipChildren
      DATA ClipSiblings
      DATA __ExplorerFilter INIT {;
                                  { "Video for Windows (*.avi)", "*.avi" };
                                  }

   METHOD Init()             CONSTRUCTOR
   METHOD Play()             INLINE SendMessage( ::hWnd, ACM_PLAY, ::Repeat, MAKELONG( ::FromFrame, ::ToFrame ) )
   METHOD Stop()             INLINE SendMessage( ::hWnd, ACM_STOP, 0, 0 )
   METHOD Seek( n )          INLINE ::FromFrame := n, ::ToFrame := n, ::Play()
   METHOD Close()            INLINE ::ImageName := NIL
   METHOD OnDestroy()        INLINE IIF( ::ImageName != NIL, ::ImageName := NIL,), ::Super:OnDestroy()
   METHOD OnCtlColorStatic()
   METHOD OnEraseBkGnd()
   METHOD Create()
   METHOD Open()
   METHOD __SetImageName()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS Animation
   DEFAULT ::__xCtrlName TO "Animation"
   ::Style     := ( WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS )
   ::ClsName   := ANIMATE_CLASS
   ::Super:Init( oParent )
   ::Width     := 40
   ::Height    := 40
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD Open( cFile ) CLASS Animation
   IF ::hWnd != NIL
      IF !EMPTY( cFile ) .AND. VALTYPE( cFile ) == "C"
         SendMessage( ::hWnd, ACM_OPEN, ::AppInstance, cFile )
       ELSEIF ::SystemAnimation >= 0
         DEFAULT hAvi TO LoadLibrary( "Shell32.dll" )
         SendMessage( ::hWnd, ACM_OPEN, hAvi, MAKEINTRESOURCE(::SystemAnimation) )
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD __SetImageName( cFile ) CLASS Animation
   LOCAL cType, cPrev
   IF VALTYPE( cFile ) == "A"
      cFile := IIF( ::DesignMode .AND. VALTYPE( cFile[1] ) == "C", cFile[1], cFile[2] )
   ENDIF
   cPrev := ::xImageName

   IF !EMPTY( cFile )
      ::Open( cFile )
    ELSE
      ::xImageName := NIL
   ENDIF

   IF ::DesignMode
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
   IF ::DesignMode
      ::Style := ( ::Style & NOT( ACS_AUTOPLAY ) )
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
