FUNCTION Main()

   LOCAL Start, Len
   LOCAL lCaseSensitive := .T.

   LOCAL cString := "This is the string to search into"

   // Find a WORD (<word boundary>) starting with 's' followed by 4 to 8 NON space characters, and terminated at <word boundary>.
   LOCAL cRegEx  := HB_RegExComp( "\bs[^ ]{4,8}\b" )

   ? "Found: <" + HB_AtX( cRegEx, cString, lCaseSensitive, @Start, @Len ) + "> At:", Start, "Len:", Len
   ? "Should have found: 'string'"
   ?

   // Now search only from the 14th position.
   Start := 14
   Len := 28
   ? "Found: <" + HB_AtX( cRegEx, cString, lCaseSensitive, @Start, @Len ) + "> At:", Start, "Len:", Len
   ? "Should have found: 'search'"
   ?

RETURN 0
