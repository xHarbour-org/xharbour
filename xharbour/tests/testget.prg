Procedure Main()

   LOCAL   GetList := {}, cVar := "Hello"
   MEMVAR  aVar, nIndex, cMacro
   PRIVATE aVar := { "World", "Again" }, nIndex := 1, cMacro := "cEarly", cEarly := {"Early"}, cLate := "Late!", cEarly2 := {"Early2"}

   CLS

   ? "2nd GET should say 'Early'."

   @ 10,10 SAY "cVar            :" GET cVar PICTURE "@K!"

   @ 12,10 SAY "cMacro[1]       :" GET &cMacro[1]

   @ 14,10 SAY "cMacro.2[1]     :" GET &cMacro.2[1]

   @ 16,10 SAY "cEarly[1]       :" GET cEarly[1]

#ifdef __HARBOUR__
   @ 18,10 SAY "cMacro          :" GET &(cMacro)[1]
#endif

   nIndex := 2

   @ 20,10 SAY "aVar            :" GET aVar[nIndex]

   @ 22,10 SAY "Picture of GET-1:" GET GetList[1]:Picture

   nIndex := 3

   cMacro := "cLate"

   READ

   CLS

#ifdef __HARBOUR__
   // Clipper Error "Get contains complex macro"
   ? "This GET should say 'Late!'??? (No, all gets are early. Codeblock is an implementation detail)"
   cMacro := "cEarly[1]"
   @ 10,10 SAY "cMacro          :" GET &(cMacro)
   cMacro := "cLate"
   READ
#endif

RETURN

