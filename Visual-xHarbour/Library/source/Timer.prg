/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Radio.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

//-----------------------------------------------------------------------------------------------

CLASS Timer INHERIT Component
   PROPERTY Delay READ xDelay  WRITE SetDelay DEFAULT 1000 PROTECTED

   DATA lRunning      EXPORTED INIT .F.
   DATA AutoRun       PUBLISHED INIT .T.

   DATA Id            EXPORTED
   DATA ClsName       EXPORTED  INIT "Timer"
   DATA Events        EXPORTED  INIT {  {"General", { { "OnTimeOut"       , "", "" } } } }

   DATA hProc         PROTECTED
   ACCESS Parent      INLINE ::Owner

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD SetDelay()
   METHOD Start()     INLINE ::lRunning := .T., ::Owner:SetTimer( ::Id, ::Delay, ::hProc )
   METHOD Stop()      INLINE ::lRunning := .F., IIF( ::Owner != NIL, ::Owner:KillTimer( ::Id ),)
   METHOD TimeProc()
   METHOD OnTimeOut() VIRTUAL
   METHOD Destroy()   INLINE ::Stop(), ::Super:Destroy()
ENDCLASS

METHOD Init( oOwner ) CLASS Timer
   ::__xCtrlName := "Timer"
   ::ComponentType := "Timer"
   ::Super:Init( oOwner )
   ::Id := oOwner:__Timers ++
RETURN Self

METHOD Create() CLASS Timer
   ::hProc := WinCallBackPointer( HB_ObjMsgPtr( Self, "TimeProc" ), Self )
   ::lCreated := .T.
RETURN Self

METHOD SetDelay(n) CLASS Timer
   ::xDelay := n
   IF ::lRunning
      ::Stop()
      ::Start()
   ENDIF
RETURN Self

METHOD TimeProc() CLASS Timer
   LOCAL nRet := 0
   IF ::__ClassInst == NIL
      ::OnTimeOut()
      nRet := ExecuteEvent( "OnTimeOut", Self )
   ENDIF
RETURN nRet
