PROCEDURE Main()

   LOCAL hHash := Hash()
   LOCAL hTest := { "Test" => Date(), "Other" => Date()+1 }
   LOCAL n
   LOCAL xKey, xVal
   
   IF HSetAACompatibility( hHash, .T. )
      ? "Ok, empty hash can be seted as Associative Array compatible"
   ENDIF
   
   hHash[ "One" ]   := 1
   hHash[ "Two" ]   := 2
   hHash[ "Three" ] := 3
   hHash[ "Four" ]  := 4
   hHash[ "Five" ]  := 5
   
   ?
   ? "List in hash order"
   For n := 1 To Len( hHash )
      ? Pad(HGetKeyAt( hHash, n ),5), HGetValueAt( hHash, n )
   Next
   
   ?
   ? "List in hash order"
   FOR EACH xKey IN hHash:Keys
      ? xKey, hHash[ xKey ]
   NEXT
   
   ?
   ? "Scan values in associative array order"
   FOR EACH xVal IN hHash
      ? HB_EnumIndex(), xVal
   NEXT
   
   ?
   ? "List in associative array order"
   For n := 1 To Len( hHash )
      ? Pad(HAAGetKeyAt( hHash, n ),5), hHash[ n ] // or HAAGetValueAt( hHash, n )
   Next
   
   ?
   
   TRY
      HSetAACompatibility( hTest, .T. )
   CATCH
      ? "Only empty Hash can set as Associative Array compatible."
   END
   
   
   ? hHash[ "One" ] += 10
   ? hHash[ 1 ] += 10

RETURN