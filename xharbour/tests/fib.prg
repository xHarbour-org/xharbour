//
// $Id: fib.prg,v 1.1.1.1 2001/12/21 10:45:46 ronpinkas Exp $
//

PROCEDURE Main( cNumber )
  Local n

  IF cNumber == NIL
    n := 10
  ELSE
    n := Val( cNumber )
  ENDIF

  ? "Fib sum of: ", n
   
  ? "Recursive:", FibR( n )
  ? "Inplace  :", FibI( n ) 
   
RETURN

Function FibR( n )

  IF n <= 1
     RETURN 1
  ENDIF
      
RETURN FibR( n - 2 ) + FibR( n - 1 )

Function FibI( n )
   Local nFirst := 0
   Local nSecond := 1
   LOCAL Counter
   LOCAL nTemp

   FOR Counter := 1 TO n 
      nTemp := nSecond
      nSecond += nFirst     
      nFirst := nTemp
   NEXT

RETURN nSecond
