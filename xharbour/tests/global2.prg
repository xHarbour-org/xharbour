GLOBAL EXTERNAL MyGlobal1
GLOBAL MyGlobal2

Procedure Global2()

   ? "In Global2() Before assignment:", MyGlobal1, MyGlobal2

   MyGlobal1 := ProcName()
   MyGlobal2 := ProcName()

Return
