* $Id$
*
* test program for append from cTextFile delimited
*
* Andi Jahja
*
* provided to test FPARSEEX() functions
*
* 2004-03-10
*
* In my machine this previously took approximately 0.80 seconds
* Using FParseEX() is it now 0.25 seconds
*
//----------------------------------------------------------------------------//
procedure main()

   local astruct, n := seconds()

   use test alias test new
   astruct := test->(dbstruct())
   test->(dbclosearea())

   dbcreate( "newfile.dbf", astruct )
   use newfile alias newfile new

   append from test.csv delimited with ","

   ? seconds() - n

return
