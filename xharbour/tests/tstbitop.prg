PROCEDURE Main()

   LOCAL x := 0, y := 0, cMacro := "x"

   IF 0 & 0
      ? "Error"
   ELSE
      ? "Ok"
   ENDIF

   IF 0 | 0
      ? "Error"
   ELSE
      ? "Ok"
   ENDIF

   IF 0 ^^ 0
      ? "Error"
   ELSE
      ? "Ok"
   ENDIF

   ? 256 >> 8
   ? 256 << 8

   IF x & y
      ? "Error"
   ELSE
      ? "Ok"
   ENDIF

   IF x | y
      ? "Error"
   ELSE
      ? "Ok"
   ENDIF

   IF x ^^ y
      ? "Error"
   ELSE
      ? "Ok"
   ENDIF

   x := 256
   y := 8

   ? x >> y
   ? x << y

   & ( "x" ) := "Macro OK"
   ? & ( "x" )
   ? &cMacro

RETURN
