GLOBAL MyGlobal1
GLOBAL EXTERNAL MyGlobal2

Procedure Global1()

   ? "In Global1() Before assignment:", MyGlobal1, MyGlobal2

   MyGlobal1 := ProcName()
   MyGlobal2 := ProcName()

   Global2()

   ? "In Global1() After Global2():", MyGlobal1, MyGlobal2

Return
