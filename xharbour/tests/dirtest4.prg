* $Id: dirtest4.prg,v 1.1 2004/03/01 05:16:43 andijahja Exp $
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
PROCEDURE Main()

   ? "Will get ALL *.EXE file in C:\WINDOWS and SUB folders. Press any key ...."
   Inkey(0)
   ? "Working ..."
   Showit( DIRECTORYRECURSE( "C:\WINDOWS\*.EXE" ) )
   ?

   ? "Will get ALL *.DLL file in C:\WINDOWS and SUB folders. Press any key ...."
   Inkey(0)
   ? "Working ..."
   ShowIt( DIRECTORYRECURSE( "C:\WINDOWS\*.DLL" ) )

   ? "Will get ALL *.OCX files in C:\WINDOWS and SUB folders. Press any key ...."
   Inkey(0)
   ? "Working ..."
   ShowIt( DIRECTORYRECURSE( "C:\WINDOWS\*.OCX" ) )

   ? "Will get ALL *.PRG file in CURRENT and SUB folders. Press any key ...."
   Inkey(0)
   ? "Working ..."
   ShowIt( DIRECTORYRECURSE( "*.PRG" ) )

RETURN

//------------------------------------------------
STATIC PROCEDURE ShowIt( aDir )

   LOCAL aFile

   FOR EACH aFile IN aDir
      ? PadR( aFile[1], 38 ), Transform( aFile[2], "999,999,999,999" ), aFile[3], aFile[4], aFile[5]
   NEXT

   ?
   ? Len( aDir), "files listed."
   ?

RETURN
