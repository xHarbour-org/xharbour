Procedure Main()

   LOCAL cMacro := "1, 2, 3"
   PRIVATE Var

   /*
   USE TEST
   INDEX ON FIRST TO FIRST

   DBSEEK( "John", "Mary", .t. )
   */

   Var := Test( &cMacro + 4 )
   ? Var

return

Function Test( p1, p2, p3 )

   ? p1, p2, p3

return p3
