//#translate { <Property1> \=> <Value1> [, <PropertyN> \=> <ValueN> ] } => TAssociativeArray( { { <Property1>, <Value1> } [, { <PropertyN> \=> <ValueN> } ] } )

PROCEDURE Main()

   LOCAL cMacro := "cVar"
   MEMVAR cVar

   PRIVATE cVar := "OOps!"

   Test( @&cMacro )

   ? cVar

RETURN

FUNCTION Test( xParam )

   xParam := "Ok :-)"

RETURN NIL

