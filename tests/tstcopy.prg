* $Id$
*
* test program for copy to cTextFile delimited
*
* Andi Jahja
*
*
* 2004-03-10
*
* In my machine this previously took approximately 0.38 seconds
* After optimization id is 0.11 seconds
*
//----------------------------------------------------------------------------//
proc main

   local n := seconds()

   use test new
   copy to myfile.txt delimited
   outstd( seconds() - n, chr(10) )

return
