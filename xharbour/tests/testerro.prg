//
// $Id: testerro.prg,v 1.1 1999/10/04 18:46:32 vszel Exp $
//

// Testing Harbour Error system

function Main()

   local n

   QOut( "We are running and now an error will raise" )

   n++      // an error should raise here

return nil
