#include "hbclass.ch"
#include "hbmemory.ch"

PROCEDURE Main()

  LOCAL nStart := Seconds()

  CLEAR SCREEN

  // 1st param is the Startup Function, 2nd. is Self if 1st param is a Method or NIL otherwise,
  // rest are paramaters to be passed to the Function/Method.
  StartThread ( @MyThreadFunc(), 2, "1st Thread:",     0,  5000 )
  StartThread ( @MyThreadFunc(), 4, "2nd Thread:",  5000, 10000 )
  StartThread ( @MyThreadFunc(), 6, "3rd Thread:", 10000, 15000 )

  WaitForThreads()
  @ 8, 0 SAY "Threads Time:" + Str( Seconds() - nStart )

RETURN

PROCEDURE MyThreadFunc( nRow, cName, nStart, nMax )

  LOCAL i, aVar

  FOR i := nStart TO nMax
     @ nRow, 10 SAY cName + Str( i )

     IF nRow < 10
        aVar := { 1 }
        aVar[1] := Array( 50 )
        aVar[1][1] := aVar
        aVar := NIL

        @ nRow, 40 SAY "Before:" + Str( Memory( HB_MEM_USED ) )
        //HB_GCALL( .T. )
        @ nRow, 60 SAY "After:" + Str( Memory( HB_MEM_USED ) )
     ENDIF
  NEXT

RETURN

