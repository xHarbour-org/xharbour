PROCEDURE Test()

   LOCAL aArray1 := { 1, 2, 3, { NIL, 20, 30 }, NIL, NIL }
   LOCAL aArray2

   aArray1[4][1] := aArray1
   aArray1[5]    := aArray1
   aArray1[6]    := aArray1[4]

   aArray2 := aClone( aArray1 )

RETURN
