//
// $Id: strdelim.prg,v 1.1 1999/10/04 18:46:32 vszel Exp $
//

procedure main()

   local aArray := {{NIL}}

   aArray  [ 1 /*first*/ ][ 1 /* second */ ] := [Hello]

   QOut( aArray[1][1] )

   QOut( 'World "Peace[!]"' )

   QOut( "Harbour 'Power[!]'" )

   QOut( [King 'Clipper "!"'] )

return


