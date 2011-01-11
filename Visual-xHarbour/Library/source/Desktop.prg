/*
 * $Id$
 */
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"



//------------------------------------------------------------------------------------------------

CLASS DeskTop INHERIT Window

   DATA TaskBar EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD GetClientRect()

ENDCLASS

//------------------------------------------------------------------------------------------------

METHOD Init() CLASS DeskTop

   ::ClsName := "DeskTop"
   ::TaskBar := TaskBar( Self )
   ::hWnd := GetDeskTopWindow()
   ::GetClientRect()
   ::GetWindowRect()

RETURN Self

//------------------------------------------------------------------------------------------------

METHOD GetClientRect() CLASS DeskTop

   ::Super:GetClientRect()
   ::TaskBar:OnWindowPosChanged()

   SWITCH ::TaskBar:BarData:uEdge
      CASE 0
           ::Left := ::TaskBar:BarData:rc:right
           ::ClientWidth -= ::TaskBar:BarData:rc:right
      CASE 1
           ::Top := ::TaskBar:BarData:rc:top
           ::ClientHeight -= ::Top
      CASE 2
           ::Width := ::TaskBar:BarData:rc:Left
           ::ClientWidth := ::TaskBar:BarData:rc:Left
      CASE 3
           ::Height := ::TaskBar:BarData:rc:top
           ::ClientHeight := ::TaskBar:BarData:rc:top
   END

RETURN SELF

//------------------------------------------------------------------------------------------------


