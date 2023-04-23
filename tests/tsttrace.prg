#include "set.ch"

PROCEDURE Main()
   
   LOCAL cFile, nLevel

   TraceLog( 1 )

   cFile  := Set( _SET_TRACEFILE, "error.log" ) 
   nLevel := Set( _SET_TRACESTACK, HB_SET_TRACESTACK_ALL )

   TraceLog( "Error", 1 )

   // The .T. sets the mode to APPEND vs OVERRIDE.
   Set( _SET_TRACEFILE, cFile, .T. )
   Set( _SET_TRACESTACK, nLevel )

   TraceLog( 2 )
   
   cFile  := Set( _SET_TRACEFILE, "error.log", .T. )
   nLevel := Set( _SET_TRACESTACK, HB_SET_TRACESTACK_ALL )

   TraceLog( "Error", 2 )      

   Set( _SET_TRACEFILE, cFile, .T. ) 
   Set( _SET_TRACESTACK, nLevel )

   TraceLog( 3 )

   Alert( "Done" )

RETURN   
