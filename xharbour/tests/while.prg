//
// $Id: while.prg,v 1.1 1999/10/04 18:46:33 vszel Exp $
//

// while loop test

function Main()

   local x := 0

   while x++ < 1000
      QOut( x )
   end

return nil
