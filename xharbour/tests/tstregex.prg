FUNCTION Main()

   LOCAL Start, Len
   LOCAL lCaseSensitive := .T.

   LOCAL cString := "This is the string to search into"

   // Find a WORD starting with <word boundary> starting with 's' followed by 4 to 8 NON space charcters, and terminated at <word boundary>.
   LOCAL cRegEx  := "\bs[^ ]{4,8}\b"

   ? "Found: <" + HB_AtX( cRegEx, cString, lCaseSensitive, @Start, @Len ) + "> At:", Start, "Len:", Len
   ? "Should have found: ' string '"
   ?

   // Now search only from the 15th position.
   Start := 15
   Len := NIL
   ? "Found: <" + HB_AtX( cRegEx, cString, lCaseSensitive, @Start, @Len ) + "> At:", Start, "Len:", Len
   ? "Should have found: ' search '"
   ?

RETURN 0
