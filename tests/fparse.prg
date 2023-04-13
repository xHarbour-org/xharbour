*
* $Id$
*
* testing program for parsing delimited text file
*
* Andi Jahja
*
//---------------------------------
procedure main()

   local aparse := fparse("test.csv")
   local aline, ctext

   for each aline in aparse
      for each ctext in aline
         outstd( ctext + " " )
      next
      outstd(chr(10))
   next

   return
