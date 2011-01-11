/* $Id$ */

#include "hbclass.ch"

CLASS XHDebugTimer
  DATA nSource
  DATA bCallback
  METHOD New( oWindow, bCallback )
  METHOD Start()
  METHOD Stop()
ENDCLASS


METHOD New( oWindow, bCallback ) CLASS XHDebugTimer
  ::bCallback := bCallback
RETURN Self


METHOD Start() CLASS XHDebugTimer
  ::nSource := g_timeout_add( 150, ::bCallback )
RETURN Self


METHOD Stop() CLASS XHDebugTimer
  IF ValType( ::nSource ) == 'N'
    g_source_remove( ::nSource )
    ::nSource := NIL
  ENDIF
RETURN Self

