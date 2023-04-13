PROCEDURE Test()

   LOCAL aArray1 := { 1, 2, 3, { NIL, 20, 30 }, NIL, NIL }, ;
         aArray2, aArray3

   aArray1[4][1] := aArray1
   aArray1[5]    := aArray1
   aArray1[6]    := aArray1[4]

   aArray2 := aClonePrg( aArray1 )
   aArray3 := aClone( aArray1 )

   ? aArray1[4][1] == aArray1
   ? aArray1[5]    == aArray1
   ? aArray1[6]    == aArray1[4]

   ? aArray2[4][1] == aArray2
   ? aArray2[5]    == aArray2
   ? aArray2[6]    == aArray2[4]

   ? aArray3[4][1] == aArray3
   ? aArray3[5]    == aArray3
   ? aArray3[6]    == aArray3[4]

   ? aArray1[1], aArray1[2], aArray1[3], aArray1[4][1][1], aArray1[4][1][2], aArray1[4][1][3], aArray1[4][2], aArray1[4][3], ;
     aArray1[5][1], aArray1[5][2], aArray1[5][3], aArray1[6][1][1], aArray1[6][1][2], aArray1[6][1][3]

   ? aArray2[1], aArray2[2], aArray2[3], aArray2[4][1][1], aArray2[4][1][2], aArray2[4][1][3], aArray2[4][2], aArray2[4][3], ;
     aArray2[5][1], aArray2[5][2], aArray2[5][3], aArray2[6][1][1], aArray2[6][1][2], aArray2[6][1][3]

   ? aArray3[1], aArray3[2], aArray3[3], aArray3[4][1][1], aArray3[4][1][2], aArray3[4][1][3], aArray3[4][2], aArray3[4][3], ;
     aArray3[5][1], aArray3[5][2], aArray3[5][3], aArray3[6][1][1], aArray3[6][1][2], aArray3[6][1][3]

RETURN

FUNCTION aClonePrg( aSource, aCloned )  // --> aResult

   LOCAL nLen, aResult, Counter, nAt

   IF ValType( aSource ) != 'A'
      RETURN NIL
   ENDIF

   nLen    := Len( aSource )
   aResult := Array( nLen )

   if aCloned == NIL
      aCloned := { { aSource, aResult } }
   else
      aAdd( aCloned, { aSource, aResult } )
   endif

   FOR Counter = 1 to nLen

      IF ValType( aSource[ Counter ] ) == "A"

         IF ( nAt := aScan( aCloned, { | a | a[ 1 ] == aSource[ Counter ] } ) ) != 0
            aResult[ Counter ] := aCloned[ nAt ][ 2 ]
         ELSE
            aResult[ Counter ] := aClonePrg( aSource[ Counter ], @aCloned )
         ENDIF

      ELSE
         aResult[ Counter ] := aSource[ Counter ]
      ENDIF

   NEXT

RETURN aResult
