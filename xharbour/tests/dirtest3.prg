* $Id$
*
* Test program for extended DirectoryRecurse()
*
* Syntax : DirectoryRecurse( <cPath> )
*
* cPath = wild card skleton including path name
*
* Andi Jahja
*

//------------------------------------------------
   proc main()

   // Must Supply Path
   ShowIt( DIRECTORYRECURSE( "C:\WINDOWS\*.*" ) )

   return

//------------------------------------------------
static procedure showit( adir )

   local cnewline := hb_osnewline()
   local x
   for x := 1 to len(adir)
      outstd(cNewLine)
      outstd(padr(adir[x,1], 34), "|", ;
             transform(adir[x,2], "999,999,999,999"), "|", ;
             adir[x,3], "|", ;
             adir[x,4], "|", ;
             adir[x,5])
   next x
   outstd( cnewline )
   outstd( cnewline )
   outstd( str(len(adir),,,.T.) + " files listed.")
   outstd( cnewline )
   return
