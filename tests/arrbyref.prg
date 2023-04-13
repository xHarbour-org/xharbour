#include "hbclass.ch"


PROCEDURE Main()

   LOCAL aArray := { 1, 2, 3 }

   ? Test( @aArray[1] )

   ? aArray[1]

RETURN

FUNCTION Test( xByRef )

RETURN xByRef := 10
