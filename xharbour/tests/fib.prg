//
// $Id: fib.prg,v 1.1 1999/10/04 18:46:28 vszel Exp $
//

Function Main()
Local n

  For n := 1 To 20
     QOut( FibR( n ) )
     QOut( FibI( n ) )
  Next

Return( NIL )

Function FibR( n )
Local nFib

   If n < 2
      nFib := n
   Else
      nFib := FibR( n - 2 ) + FibR( n - 1 )
   EndIf

Return( nFib )

Function FibI( n )
Local nFibMin1  := 1
Local nFibMinN1 := 0
Local i         := 1
Local nFib

   If n < 2
      nFib := n
   Else
      Do While i < n
         nFib      := nFibMin1 + nFibMinN1
         nFibMinN1 := nFibMin1
         nFibMin1  := nFib
         ++i
      EndDo
   EndIf

Return( nFib )

