/* $Id$ */

#include "hbclass.ch"

CLASS XHDebugTimer
  DATA bCallback
  DATA oWindow
  DATA hProc
  METHOD New( oWindow, bCallback )
  METHOD OnTimeOut()
  METHOD Start()
  METHOD Stop()
ENDCLASS


METHOD New( oWindow, bCallback ) CLASS XHDebugTimer
  ::bCallback := bCallback
  ::oWindow := oWindow
  ::hProc := WinCallBackPointer( HB_ObjMsgPtr( Self, "OnTimeOut" ), Self ) 
RETURN Self


METHOD OnTimeOut() CLASS XHDebugTimer
  Eval( ::bCallback )
RETURN Self


METHOD Start() CLASS XHDebugTimer
  ::oWindow:SetTimer( 0, 150, ::hProc )
RETURN Self


METHOD Stop() CLASS XHDebugTimer
  ::oWindow:KillTimer( 0 )
RETURN Self

