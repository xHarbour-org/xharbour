/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// TrackBar.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

#define TRBN_FIRST (0-1501)
#define TRBN_THUMBPOSCHANGING (TRBN_FIRST-1)
#define TBS_TRANSPARENTBKGND 0x1000
#define TBS_NOTIFYBEFOREMOVE 0x800

//-----------------------------------------------------------------------------------------------

CLASS TrackBar INHERIT Control
   METHOD Init()  CONSTRUCTOR
   METHOD OnParentNotify()
ENDCLASS

METHOD Init( oParent ) CLASS TrackBar
   DEFAULT ::__xCtrlName TO "TrackBar"
   ::ClsName := TRACKBAR_CLASS
   ::Super:Init( oParent )
   ::Style  := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TBS_AUTOTICKS | TBS_ENABLESELRANGE | TBS_NOTIFYBEFOREMOVE)
   ::Width  := 100
   ::Height := 28
   ::Events := ;
            { ;
            {"Position", {;
                          { "OnPosChange", "", "nPosition, nReason" };
            } } }
RETURN Self

METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS TrackBar
   local tpc
   (nwParam)
   IF hdr:code == TRBN_THUMBPOSCHANGING
      tpc := (struct NMTRBTHUMBPOSCHANGING *) nlParam
      ExecuteEvent( "OnPosChange", Self, tpc:dwPos, tpc:nReason )
   ENDIF
RETURN NIL
