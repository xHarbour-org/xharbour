Procedure Main()

   LOCAL aVar := { { 1, "Hello", 3 } }, aVar2 := { "@!" }

   PRIVATE cVar1 := { { "Macro", "Macro1" } }, cMacro := "cVar", cMacro1 := "cVar1"

   CLS

   @ 10,10 GET aVar[1][2] PICTURE aVar2[1]

   GetList[1]:Cargo := { { 2, 3, 4 } }

   @ 12,10 GET GetList[1]:Cargo[1][1]

   @ 14,10 GET GetList[1]:Cargo[1][2]

   @ 16,10 GET GetList[1]:Cargo[1][3]

   @ 18,10 GET &cMacro.1[1,1]

   @ 20,10 GET &cMacro1[1,2]

Return
