//
// $Id: calling.prg,v 1.1 1999/10/04 18:46:27 vszel Exp $
//

// Calling different PRG functions

function Main()

   SecondOne()

   DO Fourth WITH "from Fourth() function"    // Testing the old fashion DO ...

   QOut( "Ok, back to Main()" )

return nil

function SecondOne()

   QOut( "Ok, this is from Second() function call" )

   Third()

return nil

function Third()

   QOut( "Ok, this is from Third() function call" )

return nil

function Fourth( cText )

   QOut( cText )

return nil
