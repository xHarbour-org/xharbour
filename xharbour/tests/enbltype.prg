#include "hbclass.ch"

PROCEDURE Main()

   LOCAL aVar := {}
   LOCAL bDisplayArray := {|xElement| Alert( xElement:AsString ) }
   LOCAL bDisplayHash := {|xKey, xValue| QOut( xKey, xValue ) }
   LOCAL pPointer := (  @Main() )
	 LOCAL hHash := Hash()
	 LOCAL xVal

   ENABLE TYPE CLASS ALL

   CLS

   aVar:Init( 2 )
   aVar:AtPut( 1, "One" )
   aVar:AtPut( 2, 2 )

   aVar:InsertAt( 3, "Three" )

   Alert( "Found at pos: " + aVar:IndexOf( "Three" ):AsString )

   aVar:Do( bDisplayArray )

   aVar:AddAll( { 4, "Five", 6 } )

   hHash:Init( 3 )
   hHash:AtPut( 1, "One" )
   hHash:AtPut( 2, 2 )
   hHash:AtPut( 3, "Three" )

   Alert( "Found: " + hHash:AtIndex( hHash:IndexOf( "Three" ) ):AsString() )

   hHash:Do( bDisplayHash )

   hHash:AddAll( { 4 => "Four", 5 => "Five", 6 => "Six" } )

	 ?
	 FOR EACH xVal IN hHash:Collect( {|| .T.} )
			? "  ", xVal
	 NEXT

	 ?
   ? aVar:AsString
   ? bDisplayArray:AsString
   ? pPointer:AsString

RETURN

