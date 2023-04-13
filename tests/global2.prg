GLOBAL EXTERNAL MyGlobal1
GLOBAL MyGlobal2

Procedure Global2( bBlock, x )

   ? "In Global2() Before assignment:", MyGlobal1, MyGlobal2, Eval( bBlock )

   MyGlobal1 := ProcName()
   MyGlobal2 := ProcName()
   x := 1

Return
