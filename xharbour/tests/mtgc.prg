#include "hbclass.ch"
#include "hbmemory.ch"

PROCEDURE Main()

  LOCAL nStart := Seconds()
  LOCAL oMyObject := MyClass()
  LOCAL MethodPtr := HB_ObjMsgPtr( oMyObject, "Count" )

  CLEAR SCREEN

  // 1st param is the Startup Function, 2nd. is Self if 1st param is a Method or NIL otherwise,
  // rest are paramaters to be passed to the Function/Method.
  StartThread ( @MyThreadFunc(), NIL, 2, "1st Thread:",     0,  5000 )
  StartThread ( @MyThreadFunc(), NIL, 4, "2nd Thread:",  5000, 10000 )
  StartThread ( @MyThreadFunc(), NIL, 6, "3rd Thread:", 10000, 15000 )

  WaitForThreads()
  @ 8, 0 SAY "Threads Time:" + Str( Seconds() - nStart )

  nStart := Seconds()

  StartThread ( MethodPtr, oMyObject, 10, "1st Thread:",     0,  5000 )
  StartThread ( MethodPtr, oMyObject, 12, "2nd Thread:",  5000, 10000 )
  StartThread ( MethodPtr, oMyObject, 14, "3rd Thread:", 10000, 15000 )

  WaitForThreads()
  @ 16, 0 SAY "[METHODS] Threads Time:" + Str( Seconds() - nStart )

  nStart := Seconds()

  MyThreadFunc( 18, "1st Run:",     0,  5000 )
  MyThreadFunc( 20, "2nd Run:",  5000, 10000 )
  MyThreadFunc( 22, "3rd Run:", 10000, 15000 )

  @ 24, 0 SAY  "Sequential Time:" + Str( Seconds() - nStart )

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
        HB_GCALL( .T. )
        @ nRow, 60 SAY "After:" + Str( Memory( HB_MEM_USED ) )
     ENDIF
  NEXT

RETURN

CLASS MyClass
    METHOD Count( nRow, cName, nStart, nMax )
ENDCLASS

METHOD Count( nRow, cName, nStart, nMax ) CLASS MyClass
  LOCAL i

  FOR i := nStart TO nMax
     @ nRow, 10 SAY cName + Str( i )
  NEXT

RETURN NIL
