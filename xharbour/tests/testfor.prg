//
// $Id: testfor.prg,v 1.1 1999/10/04 18:46:32 vszel Exp $
//

FUNCTION MAIN

   LOCAL i

   FOR i := 1 TO 10

      qout( i )

      IF i = 4 .AND. .T.
         __Accept("")
         qout(i)
         i := 9
         qout(i)
         __Accept("")
      ENDIF

   NEXT

   RETURN( NIL )
