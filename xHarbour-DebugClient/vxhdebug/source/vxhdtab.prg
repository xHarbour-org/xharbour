/*
 * $Id$
 */

#include "vxh.ch"

CLASS XHDebuggerTab FROM TabControl
  DATA oDebugger
  METHOD Init( oParent, oDebugger ) CONSTRUCTOR
  METHOD OnSysCommand(n) INLINE If( n == SC_CLOSE, ( ::Hide(), ::oDebugger:Stop(), 0 ), )
ENDCLASS


METHOD Init( oParent, oDebugger ) CLASS XHDebuggerTab
  ::Super:Init( oParent )
  ::__xCtrlName := "Debug"
  ::AllowClose := .T.
  ::BoldSelection := .T.
  ::Flat := .T.
  ::Width := 680
  ::Height := 400
  ::AllowUndock := .T.
  ::oDebugger := oDebugger
RETURN Self
