PROCEDURE Main()

   LOCAL x := 8, y := 0, cMacro := "x"

   ? 8 & 0
   ? 8 | 0
   ? 8 ^^ 0
   ? 256 >> 8
   ? 256 << 8

   ? x & y
   ? x | y
   ? x ^^ y

   x := 256
   y := 8

   ? x >> y
   ? x << y

   & ( "x" ) := "Macro OK"
   ? & ( "x" )
   ? &cMacro

RETURN
