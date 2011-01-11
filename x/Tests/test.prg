init Function Main()

   Local nVar := nVar2 := nVar3 := 3.1234
   LOCAL cMacro := "Par1"

   ? '"' + LTrim( Str( nVar3 ) ) + '"'

   ? M->nVar2

   IIF( nVar > 1, Alert( "Yes" ), Alert( "No" ) )

   PRIVATE n := 2

   &cMacro = 1

   PUBLIC SomePub

   PARAMETERS Par1

   ? n, SomePub, Par1

Return

