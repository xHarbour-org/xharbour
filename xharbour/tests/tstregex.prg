FUNCTION Main()

   LOCAL Start, Len
   LOCAL lCaseSensitive := .T.

   LOCAL cString := "This is the string to search into"

   // Find a sequence starting by a space followed by 's' followed by 4 to 8 NON space charcters, and terminated with a space.
   LOCAL cRegEx  := " [^ ]{4,8} "

   ? "Found:", HB_AtX( cRegEx, cString, lCaseSensitive, @Start, @Len ), Start, Len
   ? "Should have found: ' string '"
   ?

   // Now search only from the 15th position.
   Start := 15
   Len := NIL
   ? "Found:",HB_AtX( cRegEx, cString, lCaseSensitive, @Start, @Len ), Start, Len
   ? "Should have found: ' search '"
   ?

RETURN 0
