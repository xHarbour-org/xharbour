* Random.prg
* $Id$
* Test for the hb_random function
*
* Giancarlo Niccolai

PROCEDURE Main()
   LOCAL i

   CLEAR SCREEN

   @2,5 say "Natural random"
   FOR i := 3 to 21
      @i,3 SAY Str( HB_Random() )
   NEXT

   @0,15 SAY "X H A R B O U R - Random number generator test"

   @2,25 SAY "Zero to ten"
   FOR i := 3 to 21
      @i,23 SAY Str( HB_Random( 10 ) )
   NEXT

   @2,45 say "Five dot three to nine dot eighty"
   FOR i := 3 to 21
      @i,43 SAY Str( HB_Random( 5.3, 9.80) )
   NEXT

   @23, 25 SAY "Press a key to continue"
   Inkey(0)
RETURN