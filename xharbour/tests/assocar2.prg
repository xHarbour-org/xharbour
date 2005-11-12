
Function Main()

Local hHash := Hash()
Local hTest := { "Test" => Date(), "Other" => Date()+1 }

if HSetAACompatibility( hHash, .T. )
   ? "Ok, empty hash can be seted as Associative Array compatible"
endif

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
