//
// $Id: test.prg,v 1.1.1.1 2001/12/21 10:46:29 ronpinkas Exp $
//

//

procedure main()

   local s := " " + chr(0) + "  mab  " + chr(0) + " "

   QOut( s )

   qout( '"' + ltrim(s) + '"' )
   qout( '"' + rtrim(s) + '"' )
   qout( '"' + alltrim(s) + '"' )

return
