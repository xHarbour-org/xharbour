//
// $Id: returns.prg,v 1.1 1999/10/04 18:46:31 vszel Exp $
//

// Testing multiple returns into a function

function Main()

   QOut( "From Main()" )

   Two( 1 )

   QOut( "back to Main()" )

   Two( 2 )

   QOut( "back to Main()" )

return nil

function Two( n )

   do case
      case n == 1
           QOut( "n == 1" )
           return nil

      case n == 2
           QOut( "n == 2" )
           return nil
  endcase

  QOut( "This message should not been seen" )

return nil
