#include "hbclass.ch"

PROCEDURE Main()

   LOCAL aVar := {}

   ENABLE TYPE CLASS ARRAY, CHARACTER, NUMERIC

   CLS

   aVar:Init( 2 )
   aVar:AtPut( 1, "One" )
   aVar:AtPut( 2, 2 )

   aVar:InsertAt( 3, "Three" )

   Alert( "Found at pos: " + aVar:IndexOf( "Three" ):AsString )

   aVar:Do( {|xElement| Alert( xElement:AsString ) } )

   aVar:AddAll( { 4, "Five", 6 } )

   ? aVar:AsString

RETURN

