//
// $Id: statics2.prg,v 1.1 1999/10/04 18:46:31 vszel Exp $
//

// Statics overlapped!
//
// Compile statics1.prg, statics2.prg and link both files

static uA, uB

function Test()

   QOut( "INSIDE STATICS2.PRG" )
   QOut( "   static uA, uB" )
   QOut( "" )
   QOut( "   ValType( uA ), ValType( uB ) =>", ValType( uA ), ",", ValType( uB ) )
   QOut( "   uA, uB =>", uA, ",", uB )
   uA := "a"
   uB := "b"
   QOut( '   uA := "a"' )
   QOut( '   uB := "b"' )
   QOut( "   uA, uB =>", uA, ",", uB )
   QOut( "" )

return nil
