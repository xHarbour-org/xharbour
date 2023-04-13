
//----------------------------------------------------------------------------//

PROCEDURE MAIN()

   LOCAL bBlock
   LOCAL pPointer
   
   ?  {|| "Block:Exec() Work" }:Exec()
   ?  ( @PointerTest() ):Exec()

   ( @QOUT() ):Exec( "Pointer:Exec() With Params Work", "Good" )
   
   ?  {|| "Block:Eval Work()" }:Eval
   
   //Fixed Now ... Before It Was Working, now Not
   ?  {|| "Block:Eval Work()" }:Evals // Error!!!

   RETURN


FUNCTION PointerTest()
   RETURN "Pointer:Exec() Work"
