PROCEDURE Main()

   LOCAL aArray := {}, bBlock := {|| Nil}, hHash := {=>}, pPointer := (@Main())
   LOCAL aArray2 := {}, bBlock2 := {|| Nil}, hHash2 := {=>}, pPointer2 := (@Main())

   ? aArray   == aArray2, "Should be .F."
   ? bBlock   == bBlock2, "Should be .F."
   ? hHash    == hHash2, "Should be .F."
   ? pPointer == pPointer2, "Should be .T."

   aArray2   := aArray
   bBlock2   := bBlock
   hHash2    := hHash
   pPointer2 := pPointer

   ?
   ? aArray   == aArray2, "Should be .T."
   ? bBlock   == bBlock2, "Should be .T."
   ? hHash    == hHash2, "Should be .T."
   ? pPointer == pPointer2, "Should be .T."

RETURN
   