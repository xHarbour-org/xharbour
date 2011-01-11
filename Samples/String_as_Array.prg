PROCEDURE Main()

   LOCAL cVar := Encript( "Hello" )

   ? "Encripted:", cVar

   cVar := Decript( cVar )

   ? "Decripted:", cVar

   // Here we demonstrate String as Array, as well as NEGATIVE Indexing (Right to Left).
   ? "Last letter is:", cVar[-1]

   wait

RETURN

FUNCTION Encript( cStr )

   LOCAL cChar, cReturn := ""

   // Here we demonstrate FOR EACH (most efficent) 
   FOR EACH cChar IN cStr
      cReturn += Chr( 256 - Asc(cChar) )
   NEXT

RETURN cReturn

FUNCTION Decript( cStr )

   LOCAL nIndex, nLen := Len( cStr ), cReturn := ""

   // Here we demonstrate String as Array
   // Fast then SubStr() but slower then above FOR EACH sample.
   FOR nIndex := 1 TO nLen
      cReturn += Chr( 256 - Asc(cStr[ nIndex]) )
   NEXT

RETURN cReturn