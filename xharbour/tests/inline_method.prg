#include "hbclass.ch"

PROCEDURE Main()

   ? MyClass():MyMethod( 2, 3 )

RETURN

CLASS MyClass

   INLINE METHOD MyMethod( x, y )
      LOCAL MyLocal := 17

      RETURN MyLocal + 7 + x + 7 * y
   ENDMETHOD

ENDCLASS
