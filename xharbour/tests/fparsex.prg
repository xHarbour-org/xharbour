*
* $Id: fparse.prg,v 1.1 2004/02/15 03:12:50 andijahja Exp $
*
* testing program for parsing delimited text file
*
* Andi Jahja
*
//---------------------------------
procedure main()

   local aparse, i, n := 0
   local aline, ctext := '"Jones,Mr",Male,"Oklahoma","IL",20041231,"Director,President"'

   OutStd( "Parsing Text: " + cText, chr(10) )
   OutStd( "Press a key ...", chr(10) )
   Inkey(0)
   CLEAR SCREEN
   aLine := FParseLine( cText,,@n )
   i := 0
   for each ctext in aline
      outstd( str(++i,,,.T.), ctext, chr(10) )
   next
   OutStd( "Number of parsed text:", str(n,,,.T.), chr(10), chr(10) )

   OutStd( "Parsing File: test1.csv", chr(10) )
   OutStd( "Press a key ...", chr(10) )
   Inkey(0)
   CLEAR SCREEN
   aparse := fparseEX("test1.csv")
   for each aline in aparse
      for each ctext in aline
         outstd( ctext + " || " )
      next
      outstd(chr(10))
   next

   return
