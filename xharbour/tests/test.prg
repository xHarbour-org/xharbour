//
// $Id: test.prg,v 1.4 2000/04/19 09:35:17 vszel Exp $
//

//

procedure main()

   local s := " " + chr(0) + "  mab  " + chr(0) + " "

   StrDump( s )
   QOut( s )

   qout( '"' + ltrim(s) + '"' )
   qout( '"' + rtrim(s) + '"' )
   qout( '"' + alltrim(s) + '"' )

return
