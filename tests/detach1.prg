PROCEDURE Main()
   LOCAL nTest
   LOCAL bBlock1 := MakeBlock()
   LOCAL bBlock2 := {|| Modify( @nTest ), QOut("From Main: ", nTest ) }

   Eval( bBlock1 )
   Eval( bBlock2 )
RETURN

Function MakeBlock()
   LOCAL nTest
RETURN  {|| Modify( @nTest ), QOut("From MakeBlock: ", nTest ) }

Function Modify( n )
   n := 10
RETURN NIL

