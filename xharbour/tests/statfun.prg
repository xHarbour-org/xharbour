//
// $Id: statfun.prg,v 1.1 1999/10/04 18:46:31 vszel Exp $
//

// Testing a static function call

function Main()

   QOut( "From Main()" )

   SecondOne()

   QOut( "From Main() again" )

return nil

static function SecondOne()

   QOut( "From Second()" )

return nil
