#include "common.ch"

PROCEDURE WG_aGrow( aArray, xValue, nEle )
   DEFAULT nEle TO 1
   aAdd( aArray, NIL )          // Aggiunge un elemento all'array
   aIns( aArray, nEle )         // Inserisce Nil in posiz. nEle shiftando adestra
   aArray[nEle] := xValue       // sostituisce nEle con xValue
RETURN


FUNCTION WG_aShrink( aArray, nEle )
   DEFAULT nEle TO Len( aArray )
   aDel( aArray, nEle )
   aSize( aArray, Len( aArray ) - 1 )
RETURN aArray



// ATOC.PRG - conversion from array to char and back
/*
       of course it only has a limited use. codblocks and objects cannot
       be stored and the entire length can be no longer than 64K.
*/

FUNCTION WG_AToC( aArray )
   LOCAL i, nLen := Len( aArray )
   LOCAL cType, cElement, cArray := ""
   FOR i := 1 TO nLen
      cElement := WG_Char( aArray[ i ] )
      IF ( cType := ValType( aArray[ i ] ) ) == "A"
         cArray += cElement
      ELSE
         cArray += Left( cType, 1) + I2Bin( Len( cElement ) ) + cElement
      ENDIF
   ENDFOR
RETURN "A" + I2Bin( Len( cArray ) ) + cArray

FUNCTION WG_CToA( cArray )
   LOCAL cType, nLen, aArray := {}
   cArray := SubStr( cArray, 4 )    // strip off array and length
   WHILE Len( cArray ) > 0
      nLen := Bin2I( SubStr( cArray, 2, 2 ) )
      IF ( cType := Left( cArray, 1 ) ) == "A"
         AAdd( aArray, WG_CToA( SubStr( cArray, 1, nLen + 3 ) ) )
      ELSE
         AAdd( aArray, WG_Value( SubStr( cArray, 4, nLen ), cType ) )
      ENDIF
      cArray := SubStr( cArray, 4 + nLen )
   END
RETURN aArray


// EOF

