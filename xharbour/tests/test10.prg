//NOTEST
//
// $Id: test10.prg,v 1.1 1999/10/04 18:46:32 vszel Exp $
//

// compile this using Harbour /10 flag

Function Main()

   QOut( MyReplicatZZ( 'a', 10 ) )

return NIL

Function MyReplicator( cChar, nLen )

return Replicate( cChar, nLen )
