***********************************************************
* Bitfield operations test
*
* (C) 2003 Giancarlo Niccolai
*
* $Id: bitfield.prg,v 1.1 2003/07/08 06:05:40 jonnymind Exp $
*

PROCEDURE Main()
   LOCAL nVal1, nVal2

   SET COLOR TO w+/b
   CLEAR SCREEN
   @1,20 SAY "X H A R B O U R - Bitfield operation tests"
   @3,0

   ? "2  AND 8  =", HB_BitAnd( 2 , 8 )
   ? "15 AND 6  =", HB_BitAnd( 15, 6 )
   ? "8  OR  4  =", HB_BitOr ( 8 , 4 )
   ? "15 XOR 6  =", HB_BitXor( 15, 6 )
   ?

   ? "Using now variables:"
   nVal1 := 34521
   nVal2 := 99321

   ? nVal1, "AND", nVal2, "=", HB_BitAnd( nVal1 , nVal2 )
   ? nVal1, "OR ", nVal2, "=", HB_BitOR ( nVal1 , nVal2 )
   ? nVal1, "XOR", nVal2, "=", HB_BitXOR( nVal1 , nVal2 )
   ? "NOT", nVal1, "=", HB_BitNot( nVal1 )
   ?

   ? "Bitfields"
   ? "Setting fuorth bit into 12", HB_BitSet( 12, 4 )
   ? "Resetting second bit into 14", HB_BitReset( 14, 2 )
   ? "Shifting 64 two bit left", HB_BitShift( 64, 2 )
   ? "Shifting 64 two bit right", HB_BitShift( 64, -2 )
   ? "Is bit 0 set in 15?", HB_BitIsSet( 15, 0 )
   ? "Is bit 5 set in 15?", HB_BitIsSet( 15, 5 )
   ?

   ? "Program terminated, press a key"

   Inkey(0)
   SET COLOR TO w/n
   CLEAR SCREEN

RETURN
