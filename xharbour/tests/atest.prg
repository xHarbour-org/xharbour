//
// $Id: atest.prg,v 1.1 1999/10/04 18:46:26 vszel Exp $
//

// releasing arrays test

function Main()

   local a := { 1 }

   a[ 1 ] = a
   a[ 1 ] = nil

   QOut( "The array will try to be released now..." )

return nil
