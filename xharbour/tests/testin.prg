PROCEDURE Main()

   LOCAL nStart, Counter

   IF "Hello" IN "He said Hello"
      ? "Found 'Hello'."
   ENDIF

   nStart := Seconds()
   FOR Counter := 1 TO 100000
      IF 7 IN { 13, 7, 9 }
         //? "Found 7."
      ENDIF
   NEXT
   ? Seconds() - nStart

   nStart := Seconds()
   FOR Counter := 1 TO 100000
      IF aScan( { 13, 7, 9 }, 7 ) > 0
         //? "Found 7."
      ENDIF
   NEXT
   ? Seconds() - nStart

RETURN
