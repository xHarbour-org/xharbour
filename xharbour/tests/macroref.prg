//#translate { <Property1> \=> <Value1> [, <PropertyN> \=> <ValueN> ] } => TAssociativeArray( { { <Property1>, <Value1> } [, { <PropertyN> \=> <ValueN> } ] } )

PROCEDURE Main()

   LOCAL cMacro := "cVar", aVar, xByRef
   MEMVAR cVar1

   PRIVATE cVar1 := "OOps!"
   Test( @&cMacro.1 )
   ? cVar1

   cVar1 := "Oops!"
   // Macro Refernce may be embeded in Arrays.
   aVar := { @&( cMacro + "1" ) } // Macro Expressions supported too.
   aVar[1] := "Again :-)"
   ? cVar1

RETURN

FUNCTION Test( xParam )

   xParam := "Ok :-)"

RETURN NIL

