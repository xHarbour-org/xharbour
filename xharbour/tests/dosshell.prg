//NOTEST             // It is very frustrating if this one is auto-tested
//
// $Id: dosshell.prg,v 1.1 1999/10/04 18:46:28 vszel Exp $
//

//
// DosShell
//
// This program shell to DOS
//
// Warning : DOS only
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://www.harbour-project.org
//
// Placed in the public domain
//

function Main()

   local cOs    := Upper( OS() )
   local cShell := GetEnv("COMSPEC")

   if at( "WINDOWS", cOs ) != 0 .or. at( "DOS", cOs ) != 0 ;
   .or. at( "OS/2", cOs ) != 0
      ? "About to shell to DOS.."
      ! (cShell)
      ? "Hey, I am back !"
   else
      ? "Sorry this program is for Windows, DOS, and OS/2 only"
   endif
return nil
