* $Id$
*
* Test program for extended Directory()
*
* Syntax : Directory( <cPath>, [<cAttrinute>], [<lDirOnly>], [<lFullPath>] )
*
* Extensions:
* lDirOnly,  LOGICAL, return only diretory entry names
* lFullPath, LOGICAL, return full path names
*
* Andi Jahja
*

#define CMASK "C:\WINDOWS\*.*"
//------------------------------------------------
proc main()

   local cnewline := hb_osnewline()

   outstd("Returning directory only, WITHOUT path. Press a key ..." + hb_osnewline() )
   inkey(0)
   showit( directory( CMASK,,.T.) )
   outstd( cnewline)

   outstd("Returning directory only, WITH path. Press a key ..." + cnewline )
   inkey(0)
   showit( directory( CMASK,,.T.,.T.) )
   outstd( cnewline)

   outstd("Returning all entries, WITH path. Press a key ..." + cnewline )
   inkey(0)
   showit( directory( CMASK,"D",,.T. ) )
   outstd( cnewline)

   outstd("The classic operation. Press a key ..." + cnewline )
   inkey(0)
   showit( directory( CMASK,"D") )

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
   outstd( cnewline)
   return
