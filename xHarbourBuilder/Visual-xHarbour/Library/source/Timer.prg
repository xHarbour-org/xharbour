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
   PROPERTY Delay   SET ::SetDelay(v) DEFAULT 1000
   PROPERTY AutoRun                   DEFAULT .T.

   DATA bWhen         EXPORTED INIT {||.T.}
   DATA bTrace        EXPORTED

   DATA Running       EXPORTED INIT .F.
   DATA Id            EXPORTED
   DATA ClsName       EXPORTED  INIT "Timer"
   DATA Events        EXPORTED  INIT {  {"General", { { "OnTimeOut"       , "", "" } } } }

   DATA hProc         PROTECTED

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD SetDelay()
   METHOD Start()
   METHOD Stop()
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

METHOD Start() CLASS Timer
   IF ! ::Running .AND. Eval( ::bWhen )
      IF ::Owner != NIL
         ::Running := .T.
         ::Owner:SetTimer( ::Id, ::Delay, ::hProc )
         IF ::bTrace != NIL
            Eval( ::bTrace, ::Running )
         ENDIF
      ENDIF
   ENDIF
RETURN Self

METHOD Stop() CLASS Timer
   IF ::Running .AND. Eval( ::bWhen )
      ::Running := .F.
      IF ::Owner != NIL
         ::Owner:KillTimer( ::Id )
         IF ::bTrace != NIL
            Eval( ::bTrace, ::Running )
         ENDIF
      ENDIF
   ENDIF
RETURN Self


METHOD SetDelay(n) CLASS Timer
   ::xDelay := n
   IF ::Running
      ::Stop()
      ::Start()
   ENDIF
RETURN Self

METHOD TimeProc() CLASS Timer
   LOCAL nRet := 0
   IF ! ::DesignMode
      ::OnTimeOut()
      nRet := ExecuteEvent( "OnTimeOut", Self )
   ENDIF
RETURN nRet
