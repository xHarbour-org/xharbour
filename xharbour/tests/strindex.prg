Procedure Main

   LOCAL Counter, cVar := "Some test string to decode..."

   SimpleCode( @cVar )
   SimpleDeCode( @cVar )

   ? cVar

Return

Procedure SimpleCode( cString )

   LOCAL Counter, nLen := Len( cString )

   FOR Counter := 1 TO nLen
      #ifdef __XHARBOUR__
         cString[ Counter ] += Counter
      #else
         cString := Left( cString, Counter - 1 ) + Chr( Asc( SubStr( cString, Counter, 1 ) ) + Counter ) + SubStr( cString, Counter + 1 )
      #endif
   NEXT

Return

Procedure SimpleDeCode( cString )

   LOCAL Counter, nLen := Len( cString )

   FOR Counter := 1 TO nLen
      #ifdef __XHARBOUR__
         cString[ Counter ] -= Counter
      #else
         cString := Left( cString, Counter - 1 ) + Chr( Asc( SubStr( cString, Counter, 1 ) ) - Counter ) + SubStr( cString, Counter + 1 )
      #endif
   NEXT

Return
