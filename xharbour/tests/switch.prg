PROCEDURE Main()

   LOCAL cVar := 'F'
   LOCAL Counter
   LOCAL nStart

   nStart := Seconds()
   FOR Counter := 1 TO 100000
      SWITCH cVar
         CASE 'A'
         CASE 'B'
         CASE 'C'
         CASE 'D'
           ? "A-D"
           EXIT

         CASE 'E'
         CASE 'F'
         CASE 'G'
         CASE 'H'
           //? "E-H"
           EXIT

         DEFAULT
           ? "Other"
      END
   NEXT
   ? "Time:", Seconds() - nStart

   nStart := Seconds()
   FOR Counter := 1 TO 100000
      DO CASE
         CASE cVar == 'A' .OR. cVar == 'B' .OR. cVar == 'C' .OR. cVar == 'D'
            ? "A-D"

         CASE cVar == 'E' .OR. cVar == 'F' .OR. cVar == 'G' .OR. cVar == 'H'
            //? "E-H"

         OTHERWISE
           ? "Other"
      END
   NEXT
   ? "Time:", Seconds() - nStart

RETURN
