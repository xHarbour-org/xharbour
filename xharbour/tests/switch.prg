#define LOOPS_COUNT 100000

PROCEDURE Main()

   LOCAL cVar := 'F', cResult
   LOCAL Counter
   LOCAL nStart

   nStart := Seconds()
   FOR Counter := 1 TO LOOPS_COUNT
      SWITCH cVar
         CASE 'A'
         CASE 'B'
         CASE 'C'
         CASE 'D'
           cResult := "A-D"
           EXIT

         CASE 'E'
         CASE 'F'
         CASE 'G'
         CASE 'H'
           cResult := "E-H"
           EXIT

         DEFAULT
           cResult := "Other"
      END
   NEXT
   ? "SWITCH Time:", Seconds() - nStart, "Result:", cResult

   nStart := Seconds()
   FOR Counter := 1 TO LOOPS_COUNT
      DO CASE
         CASE cVar == 'A' .OR. cVar == 'B' .OR. cVar == 'C' .OR. cVar == 'D'
           cResult := "A-D"

         CASE cVar == 'E' .OR. cVar == 'F' .OR. cVar == 'G' .OR. cVar == 'H'
           cResult := "E-H"

         OTHERWISE
           cResult := "Other"
      END
   NEXT
   ? "CASE Time  :", Seconds() - nStart, "Result:", cResult

   nStart := Seconds()
   FOR Counter := 1 TO LOOPS_COUNT
      IF cVar == 'A' .OR. cVar == 'B' .OR. cVar == 'C' .OR. cVar == 'D'
         cResult := "A-D"

      ELSEIF cVar == 'E' .OR. cVar == 'F' .OR. cVar == 'G' .OR. cVar == 'H'
         cResult := "E-H"

      ELSE
         cResult := "Other"
      END
   NEXT
   ? "IF Time    :", Seconds() - nStart, "Result:", cResult

RETURN
