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

   OutStd( "Will get ALL *.EXE file in C:\WINDOWS. Press any key ...." + hb_osnewline())
   inkey(0)
   OutStd( "Working ..." + hb_osnewline())
   Showit( DIRECTORYRECURSE( "C:\WINDOWS\*.EXE" ) )
   OutStd( hb_osnewline())

   OutStd( "Will get ALL *.DLL file in C:\WINDOWS. Press any key ...." + hb_osnewline())
   inkey(0)
   OutStd( "Working ..." + hb_osnewline())
   ShowIt( DIRECTORYRECURSE( "C:\WINDOWS\*.DLL" ) )

   OutStd( "Will get ALL *.OCX file in C:\WINDOWS. Press any key ...." + hb_osnewline())
   inkey(0)
   OutStd( "Working ..." + hb_osnewline())
   ShowIt( DIRECTORYRECURSE( "C:\WINDOWS\*.OCX" ) )

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
