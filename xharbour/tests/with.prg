Procedure Main()

   LOCAL cVar := "Hello", cVar2 := "World", GetList := {}, Counter
   LOCAL cName, cName2, cVal, cVal2
   LOCAL cMAcro

   CLS

   @ 10,10 GET cVar
   @ 12,10 GET cVar2

   WITH OBJECT GetList[1]
     cName := :Name
     :VarPut( "From WITH" )

     // Nested.
     WITH OBJECT GetList[2]
        cName2 := :Name
        :VarPut( "From Nested WITH" )
        cVal2 := :VarGet()
     END

     // Resume outer level.
     cVal := :VarGet()
   END

   ? cName, cVal
   ? cName2, cVal2

   ?
   ? "MACROS"
   ?

   // MACROS
   WITH OBJECT GetList[1]
     cMacro := ":Name"
     cName := &cMacro

     cMacro := ':VarPut( "From WITH (MACRO)" )'
     &( cMacro )

     // Nested.
     WITH OBJECT GetList[2]
        cMacro := ":Name"
        cName2 := &cMacro

        cMacro := ':VarPut( "From Nested WITH (MACRO)" )'
        &cMacro
        cMacro := ':VarGet()'
        cVal2 := &cMacro
     END

     // Resume outer level.
     cVal := :VarGet()
   END

   ? cName, cVal
   ? cName2, cVal2

Return

