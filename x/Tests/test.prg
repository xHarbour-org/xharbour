init Function Main()

   Local nVar := nVar2 := nVar3 := 3.1234
   LOCAL cMacro := "Par1"

   QOut( '"' + LTrim( Str( nVar3 ) ) + '"' )

   QOut( M->nVar2 )

   IIF( nVar > 1, Alert( "Yes" ), Alert( "No" ) )

   PRIVATE n := 2

   &cMacro = 1

   PUBLIC SomePub

   PARAMETERS Par1

   QOut( n, SomePub, Par1 )

Return

