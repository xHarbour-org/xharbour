PROCEDURE Main()

    STATIC s_nOuter := 12
    LOCAL bBlock, nOuter := 7

    bBlock := < | x, y |
                     LOCAL InlineLocal := IIF( x > y .AND. x > nOuter, .T., .F. )

                     IF InlineLocal
                        Alert( "Yes" )
                        RETURN 33 + s_nOuter
                     ENDIF

                     RETURN 77 + s_nOuter
              >

    ? Eval( bBlock, 8, 3 )

RETURN
