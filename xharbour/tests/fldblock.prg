#translate \[ Hello ] => WasHello

Procedure Main()

   LOCAL b1, b2

   A := [ Hello ]

   //USE TEST

   b1 := FieldBlock( 'First' )
   b2 := FieldWBlock( 'Last', 1 )

   USE TEST

   ? Eval( b1 ), Eval( b2 )

return

/*
Function FieldBlock( cFldName )

   IF ValType( cFldName ) == 'C'
      RETURN {|xSet, nPos| nPos := FieldPos( cFldName ), IIF( nPos > 0, IIF( xSet == NIL, FieldGet( nPos ), FieldPut( nPos, xSet ) ), &( "FIELD->" + cFldName ) ) }
   ENDIF

RETURN NIL

Function FieldWBlock( cFldName, nWa )

   IF ValType( cFldName ) == 'C' .AND. ValType( nWa ) == 'N'
      RETURN {|xSet, nPos| nPos := FieldPos( cFldName ), IIF( nPos > 0, IIF( xSet == NIL, (nWa)->( FieldGet( nPos ) ), (nWa)->( FieldPut( nPos, xSet ) ) ), &( "FIELD->" + cFldName ) ) }
   ENDIF

RETURN NIL

/*
Function FieldWBlock( cFldName, nWA )

    local bField

    if ValType( nWA ) == "N" .and. FieldPos( ( nWA )->( cFldName ) ) > 0
        cFldName := AllTrim( str( nWA ) ) + "->" + cFldName
        bField := &("{|x| if(x==nil,"+cFldName+","+cFldName+":=x)}")
    endif

return( bField )
*/
