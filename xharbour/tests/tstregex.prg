FUNCTION Main()

   LOCAL Start, Len
   LOCAL cString := "This is the string to search into"
   LOCAL cRegEx  := "s[^ ]*"
   LOCAL lCaseSensitive := .T.

   ? HB_AtX( cRegEx, cString, lCaseSensitive, @Start, @Len ), Start, Len

   // Now search only a portion.
   Start := 8
   Len := 10
   ? HB_AtX( cRegEx, cString, lCaseSensitive, @Start, @Len ), Start, Len

RETURN 0
