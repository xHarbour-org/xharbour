//
// $Id: fornext.prg,v 1.1 1999/10/04 18:46:29 vszel Exp $
//

// Testing Harbour For Next loops
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://www.harbour-project.org
//
// Placed in the public domain
//

function Main()

   local n := 1

   QOut( "Testing Harbour For Next loops. Going up quick" )

   for n:=1 to 10 step 4
     QOut( n )
   next n

   QOut( "Going down" )

   for n:=10 to 1 step -1
     QOut( n )
   next n

   QOut( "No step" )

   for n:=1 to 10
     QOut( n )
   next n

   QOut( "No production" )

   for n:=1 to 10 step -1
     QOut( n )
   next n

   QOut( "Ok!" )

return nil
