//
// $Id: ifinline.prg,v 1.1 1999/10/04 18:46:29 vszel Exp $
//

// Testing Harbour If inline

function Main()

   local n := 1

   QOut( "Testing Harbour If inline" )

   If( n == 1, QOut( 1 ), QOut( 2 ) )

   IIf( n <> nil, QOut( "not nil" ),)

   QOut( "Now changing n to 2" )

   n = 2

   If( n == 1, QOut( 1 ), QOut( 2 ) )

   IIf( n <> nil, QOut( "not nil" ),)

   QOut( "ok!" )

return nil
