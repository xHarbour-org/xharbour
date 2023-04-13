Procedure Main

    cVar := "Hello"
    ? cVar[1]      // H

    // Auto conversion to NUMERIC due to math context.
    ? cVar[1] - 1  //         71
    ? cVar[1] + 1  //         73
    ? - cVar[1]    //        -72

    cVar[1] := 'h'
    ? cVar         // hello

    // Auto conversion from NUMERIC due to STRING assignment context.
    cVar[1] := 72
    ? cVar         // Hello

    SimpleCode( @cVar )

    ? cVar

    SimpleDeCode( @cVar )

    ? cVar

Return

Function SimpleCode( cString )

   LOCAL nLen := Len( cString )

   FOR Counter := 1 TO nLen
      cString[ Counter ] += Counter
   NEXT

Return

Function SimpleDeCode( cString )

   LOCAL nLen := Len( cString )

   FOR Counter := 1 TO nLen
      cString[ Counter ] -= Counter
   NEXT

Return
