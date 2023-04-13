MEMVAR var, macro

PROC MAIN()

   PRIVATE macro
   PRIVATE var

   macro := "(var := 7, var)"
   ? &macro

   var :=1
   macro :="IIF((var%5)==0, '===', STR(var,3))"

   Test( {|| &macro } )
   Test( {|| &(macro) } )

RETURN

PROC TEST( bBlock )

   LOCAL i

   macro := "'Late'"

   FOR i:=1 TO 20
      ? EVAL( bBlock )
      var++
   NEXT

RETURN
