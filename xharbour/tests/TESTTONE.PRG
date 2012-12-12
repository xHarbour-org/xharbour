//
// $Id$
// Test Program for HB_TONE for Windows
//

PROCEDURE MAIN()

   LOCAL n := 0
   LOCAL i

   FOR i := 1 TO 10
      n += 10
      OutStd( "Volume="+str(n,,,.T.) + chr(10) )
      HB_TONE(850,2,.T.,n)
      HB_TONE(900,5,.T.,n)
      HB_TONE(800,2,.T.,n)
      HB_TONE(950,10,.T.,n)
      HB_TONE(700,2,.T.,n)
      HB_TONE(1200,2,.T.,n)
      HB_TONE(800,2,.T.,n)
      HB_TONE(900,8,.T.,n)
   NEXT

   RETURN
