//
// $Id: arrindex.prg,v 1.1 1999/10/04 18:46:26 vszel Exp $
//

Function Main

   local a, b , c

   a := { {,} }

   a [ 1, 2 ] := [Hello]

   c := { 1 }

   b := a [ c [1] ] [ val( [2] ) ]

   QOut( b )

return NIL
