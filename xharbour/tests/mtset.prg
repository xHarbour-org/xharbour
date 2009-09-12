/* $Id$ */

/* This is a test for thread-safety of Set() */

#include "set.ch"

PROCEDURE main()
  LOCAL result1, result2

  StartThread( @task(), .T., @result1 )
  StartThread( @task(), .F., @result2 )
  ? "Threads started, please wait..."
  WaitForThreads()
  ? "result1 = ", result1, If( result1 != 0, " FAILED, should be 0!", "OK" )
  ? "result2 = ", result2, If( result2 != 100000, " FAILED, should be 100000!", "OK" )
RETURN

PROCEDURE task( lSet, nCounter )
  LOCAL i, s := "aa"

  nCounter := 0
  FOR i := 1 TO 100000
    Set( _SET_EXACT, lSet )
    IF s = "a"
      nCounter++
    ENDIF
  NEXT
RETURN
