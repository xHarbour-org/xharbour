#include "hbclass.ch"

PROCEDURE Main()

  LOCAL nStart := Seconds()
  LOCAL oMyObject := MyClass()
  LOCAL MethodPtr := HB_ObjMsgPtr( oMyObject, "Count" )
  LOCAL xThread

  CLEAR SCREEN

  // 1st param is the Startup Function, 2nd. is Self if 1st param is a Method or NIL otherwise,
  // rest are paramaters to be passed to the Function/Method.
  StartThread ( @MyThreadFunc(), 2, "1st Thread:",     0,  5000 )
  StartThread ( @MyThreadFunc(), 4, "2nd Thread:",  5000, 10000 )
  StartThread ( @MyThreadFunc(), 6, "3rd Thread:", 10000, 15000 )

  WaitForThreads()
  @ 8, 0 SAY "Threads Time:" + Str( Seconds() - nStart )

  nStart := Seconds()

  // StartThread() for methods can be called using an already available
  // Method Pointer or using a method name
  StartThread ( oMyObject, "Count", 10, "1st Thread:",     0,  5000 )
  StartThread ( oMyObject, "Count", 12, "2nd Thread:",  5000, 10000 )
  StartThread ( oMyObject, MethodPtr, 14, "3rd Thread:", 10000, 15000 )

  WaitForThreads()
  @ 16, 0 SAY "[METHODS] Threads Time:" + Str( Seconds() - nStart )

  nStart := Seconds()

  MyThreadFunc( 18, "1st Run:",     0,  5000 )
  MyThreadFunc( 20, "2nd Run:",  5000, 10000 )
  MyThreadFunc( 22, "3rd Run:", 10000, 15000 )

  @ 24, 0 SAY  "Sequential Time:" + Str( Seconds() - nStart )

  Inkey(0)
RETURN

PROCEDURE MyThreadFunc( nRow, cName, nStart, nMax )

  LOCAL i

  FOR i := nStart TO nMax
     @ nRow, 10 SAY cName + Str( i )
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
