#include "hbclass.ch"

PROCEDURE Main()

   LOCAL aVar := {}
   LOCAL bDisplay := {|xElement| Alert( xElement:AsString ) }
   LOCAL pPointer := IIF( .T., @Main(), )

   ENABLE TYPE CLASS ALL

   CLS

   aVar:Init( 2 )
   aVar:AtPut( 1, "One" )
   aVar:AtPut( 2, 2 )

   aVar:InsertAt( 3, "Three" )

   Alert( "Found at pos: " + aVar:IndexOf( "Three" ):AsString )

   aVar:Do( bDisplay )

   aVar:AddAll( { 4, "Five", 6 } )

   ? aVar:AsString
   ? bDisplay:AsString
   ? pPointer:AsString

RETURN

