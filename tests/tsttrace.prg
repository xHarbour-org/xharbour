#include "set.ch"

PROCEDURE Main()

   Nested()

RETURN      
   
PROCEDURE Nested()   
   LOCAL cFile, nLevel

   TraceLog( 1 )

   cFile  := Set( _SET_TRACEFILE, "error.log" ) 
   nLevel := Set( _SET_TRACESTACK, HB_SET_TRACESTACK_ALL )

   TraceLog( "Error", 1, cFile )

   DirChange( ".." )

   /* 
      Because the default "trace.log" does not have absolute path nor EXPLICIT RELATIVE PATH
      the output should be directed to the same file used before changing directory!
    */

   // The .T. sets the mode to APPEND vs OVERRIDE.
   Set( _SET_TRACEFILE, cFile, .T. )
   Set( _SET_TRACESTACK, nLevel )

   TraceLog( 2 )
   
   /*
      Because "./" indicates a RELATIVE path this trace will be in explictly the current directory
      ignoring possible prior different path.
    */
   cFile  := Set( _SET_TRACEFILE, "./error.log", .T. )
   nLevel := Set( _SET_TRACESTACK, HB_SET_TRACESTACK_ALL )

    SubProc()

   Set( _SET_TRACEFILE, cFile, .T. )
   // No Call Stack 
   Set( _SET_TRACESTACK, HB_SET_TRACESTACK_CURRENT )

   OtherProc()

   Alert( "Done" )

RETURN

PROCEDURE SubProc()
   TraceLog( "Error", 2 )      
RETURN

PROCEDURE OtherProc()
   TraceLog( 3 )
RETURN   
