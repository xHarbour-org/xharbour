Function Main()
 Local Program := { , }, Condition := 1, body := 2, Counter := 1, TheEnd := 1000000, stop, start

 Program[ condition] := { || Counter == TheEnd }
 Program[      body] := { || Counter++ /*:= xxx(Counter)*/ }
 ? start := Second()

 While !Eval( Program[ condition]) ; Eval( Program[ body] )
 End
 ? stop := Second()
 ? '==============='
 ? stop - start
 Return NIL

FUNCTION xxx( Counter )
RETURN Counter+1


