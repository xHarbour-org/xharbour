#include "prg2c.ch"
#define EXPLICIT_BLOCK

STATIC FUNCTION NextExp( sLine, cType, aWords, aExp, sNextAnchor, bX )

  LOCAL  sExp, sTemp, Counter, sPad, sToken, sList
  LOCAL  sNextLine, sNextToken, sLastToken, sJustToken, sJustNext, cLastChar
  LOCAL  s1, s2, s4, s5, sNext1, sNext2, sNext4, sNext5, nLen, nNextLen
  LOCAL  sWorkLine, sPrimaryStopper, nStoppers, nStopper, sStopLine, sStopper, ;
         sMultiStopper, nSpaceAt, sNextStopper, cChar

  IF Empty( sLine )
     RETURN NIL
  ENDIF

  //TraceLog( "*** Start", cType, sLine, sNextAnchor )

  DO CASE
     CASE cType == '<'
        /* No prep needed */

     CASE cType == 'A'
        IF aExp == NIL
           aExp := {}
        ENDIF

     CASE cType == ','
        sList := ""

     CASE cType == ':'
        sWorkLine       := sLine
        sPrimaryStopper := NextToken( @sWorkLine )

        IF sPrimaryStopper == NIL
           //? "No primary", sPrimaryStopper
           RETURN NIL
        ELSE
           sPrimaryStopper := Upper( RTrim( sPrimaryStopper ) )

           /* Is it a stopper (the anchor of another acceptable match) ? */
           IF bDbgExp
              ? "Stopper?: '" + sPrimaryStopper +"'"
           ENDIF

           nStoppers := Len( aWords )
           FOR nStopper := 1 TO nStoppers

              sStopLine := sWorkLine
              sToken    := sPrimaryStopper
              sStopper  := aWords[ nStopper ]

              sMultiStopper := ""
              WHILE ( nSpaceAt := At( ' ', sStopper ) ) > 0
                 sNextStopper := Left( sStopper, nSpaceAt - 1 )

                 IF bX
                    nLen := 64
                 ELSE
                    nLen := Max( 4, Len( sToken ) )
                 ENDIF

                 //? "Next Stopper: " + sNextStopper, sToken
                 IF Left( sNextStopper, nLen ) == sToken
                    sMultiStopper += sNextStopper
                    sStopper      := SubStr( sStopper, nSpaceAt )
                    sMultiStopper += ExtractLeadingWS( @sStopper )
                    sToken        := NextToken( @sStopLine )
                    sToken        := Upper( RTrim( sToken ) )
                 ELSE
                    EXIT
                 ENDIF
              ENDDO

              IF bX
                 nLen := 64
              ELSE
                 nLen := Max( 4, Len( sToken ) )
              ENDIF

              IF Left( sStopper, nLen ) == sToken
                 sMultiStopper += sStopper
                 EXIT
              ENDIF
           NEXT

           IF nStopper <= nStoppers
              sLine := sStopLine
              //TraceLog( sMultiStopper, sStopLine )
              RETURN sMultiStopper
           ELSE
              sLine := sWorkLine
              RETURN NIL
           ENDIF
        ENDIF

     CASE cType == '*'
        sExp  := sLine
        sLine := ""
        //? "EXP <*>: " + sExp
        RETURN sExp

     CASE cType == '(' .AND. Left( sLine, 1 ) != '('
        nSpaceAt := At( ' ', sLine )

        IF nSpaceAt = 0
           sExp  := sLine
           sLine := ""
        ELSE
           sExp  := Left( sLine, nSpaceAt - 1 )
           sLine := SubStr( sLine, nSpaceAt )
           sExp  += ExtractLeadingWS( @sLine )
        ENDIF

        //? "EXP <(>: " + sExp
        RETURN sExp

     CASE cType == '!'
        IF IsAlpha( cChar := Left( sLine, 1 ) ) .OR. cChar == '_'
           RETURN NextToken( @sLine )
        ELSE
           RETURN NIL
        ENDIF

     CASE cType == NIL
        RETURN "-"
  ENDCASE

  sExp := ""
  DO WHILE .T.
     sToken := NextToken( @sLine )

     IF sToken == NIL
        EXIT
     ENDIF

     TraceLog( sToken )

     sJustToken := RTrim( sToken )
     IF sNextAnchor != NIL  .AND. sJustToken == sNextAnchor
        // Clipper give preference to ',' in list expression.
        IF ! ( sNextAnchor $ ',' .AND. cType $ ",A" )
           //TraceLog( "Anchor: '" + sNextAnchor + "' found!" )
           sLine := sToken + sLine
           EXIT
        ENDIF
     ENDIF

     nLen := Len( sJustToken )
     s1 := Left( sJustToken, 1 )
     s2 := s4 := s5 := ""
     IF nLen == 2
        s2 := sJustToken
     ELSEIF nLen == 4
        s4 := Upper( sJustToken )
     ELSEIF nLen == 5
        s5 := Upper( sJustToken )
     ENDIF

     IF Empty( sLine )
        sNextToken := ""
        sJustNext  := ""
        sNext1     := ""
     ELSE
        sNextLine := sLine
        sNextToken := NextToken( @sNextLine, .T. )
        IF sNextToken == NIL
           sNextToken := ""
           sJustNext  := ""
           sNext1     := ""
        ELSE
           sJustNext := RTrim( sNextToken )
           sNext1    := Left( sJustNext, 1 )
        ENDIF
     ENDIF

     // ------------------
     // 1st. Level.
     // ------------------

     IF bDbgExp
        ? "1st. Level - Token: '" + sToken + "' Next: '" + sNextToken + "'"
        WAIT
     ENDIF

     //TraceLog( "Token: '" + sToken + "' Len: " + Str( nLen ) + " Next: '" + sNextToken + "'" )

     IF nLen == 1

        IF s1 $ "-+!:@|" // *** Very ODD Clipper consider '|' a continuation token !!!
           sExp += sToken
           LOOP
        ELSEIF s1 == "&"
           sExp += sToken
           IF sNext1 == '('
              LOOP
           ELSE
              IF IsAlpha( sNext1 ) .OR. sNext1 == '_'
                 sExp           += sNextToken
                 sLastToken     := sJustNext
                 sLine          := sNextLine
                 #ifdef __HARBOUR__
                    SetArrayPrefix( .T. )
                 #else
                    s_bArrayPrefix := .T.
                 #endif
                 sNextToken     := NextToken( @sNextLine, .T. )
                 IF sNextToken != NIL .AND. Left( sNextToken, 1 ) == '.'
                    // Get the macro terminator.
                    sExp           += sNextToken
                    sLastToken     := "."
                    sLine          := sNextLine
                    #ifdef __HARBOUR__
                       SetArrayPrefix( .T. )
                    #else
                       s_bArrayPrefix := .T.
                    #endif
                    IF sNextToken == '.' //(Last Token) No space after Macro terminator, so get the suffix.
                       sNextToken := NextToken( @sNextLine, .T. )
                       IF sNextToken != NIL
                          sNext1 := Left( sNextToken, 1 )
                          IF IsAlpha( sNext1 ) .OR. IsDigit( sNext1 ) .OR. sNext1 == '_'
                             // Get the macro sufix.
                             sExp           += sNextToken
                             sLastToken     := RTrim( sNextToken )
                             sLine          := sNextLine
                             #ifdef __HARBOUR__
                                SetArrayPrefix( .T. )
                             #else
                                s_bArrayPrefix := .T.
                             #endif
                          ENDIF
                       ENDIF
                    ENDIF
                 ENDIF
              ELSE
                 Alert( "ERROR! Invalid '&' at: " + sExp + sNextToken )
                 EXIT
              ENDIF
           ENDIF

            sLastToken := RTrim( sLastToken )
           // Continue  2nd level checks below.
        ELSEIF s1 == '('
           sExp += sToken
           IF Left( sNext1, 1 ) == ')'
              sExp           += sNextToken
              sLine          := sNextLine
              #ifdef __HARBOUR__
                 SetArrayPrefix( .T. )
              #else
                 s_bArrayPrefix := .T.
              #endif
           ELSE
              //TraceLog( "Content from: " + sLine )
              sTemp := NextExp( @sLine, ',', NIL, NIL, NIL ) // Content - Ignoring sNextAnchor !!!
              IF sTemp == NIL
                 TraceLog( "ERROR!(1) No content at: '" + sLine + "' After: " + sExp, sLine  )
                 Alert( "ERROR!(1) No content at: '" + sLine + "' After: " + sExp  )
                 EXIT
              ELSE
                 sExp +=  sTemp
                 //TraceLog( "Content: '" + sTemp + "'", sExp, sLine )
              ENDIF

              sToken := NextToken( @sLine ) // Close
              IF sToken == NIL
                 TraceLog( "ERROR!(2) Unbalanced '(' at: " + sExp, sLine )
                 Alert( "ERROR!(2) Unbalanced '(' at: " + sExp )
                 EXIT
              ELSEIF Left( sToken, 1 ) == ')'
                 sExp += sToken
              ELSE
                 sLine := sToken + sLine
                 TraceLog( "ERROR!(3) Unbalanced '(' Found: '" +  sToken + "' at: " + sExp, sLine )
                 Alert( "ERROR!(3) Unbalanced '(' Found: '" +  sToken + "' at: " + sExp )
                 EXIT
              ENDIF
           ENDIF

           sLastToken := ")"
           // Continue  2nd level checks below.
        ELSEIF s1 == '{'
           sExp  += sToken

         #ifdef EXPLICIT_BLOCK

           IF sNext1 == '|'
              /* Literal block */
              sExp           += sNextToken
              sLine          := sNextLine
              #ifdef __HARBOUR__
                 SetArrayPrefix( .F. )
              #else
                 s_bArrayPrefix := .F.
              #endif
              sNextToken     := NextToken( @sNextLine, .T. )
              IF sNextToken != NIL .AND. Left( sNextToken, 1 ) == '|'
                 sExp           += sNextToken
                 sLine          := sNextLine
                 #ifdef __HARBOUR__
                    SetArrayPrefix( .F. )
                 #else
                    s_bArrayPrefix := .F.
                 #endif
              ELSE
                 sTemp := NextExp( @sLine, ',', NIL, NIL, NIL ) // Content - Ignoring sNextAnchor !!!
                 IF sTemp == NIL
                    TraceLog( "ERROR! Unbalanced '{|...' at: " + sExp )
                    Alert( "ERROR! Unbalanced '{|...' at: " + sExp )
                    EXIT
                 ELSE
                    sExp += sTemp
                 ENDIF

                 /* sLine was changed by NextExp()! */
                 sNextLine  := sLine
                 sNextToken := NextToken( @sNextLine, .T. )
                 IF sNextToken != NIL .AND. Left( sNextToken, 1 ) == '|'
                    sExp           += sNextToken
                    sLine          := sNextLine
                    #ifdef __HARBOUR__
                       SetArrayPrefix( .F. )
                    #else
                       s_bArrayPrefix := .F.
                    #endif
                 ELSE
                    TraceLog( "ERROR! Unbalanced '{|...|' at: " + sExp, sNextToken, sNextLine )
                    Alert( "ERROR! Unbalanced '{|...|' at: " + sExp )
                    EXIT
                 ENDIF
              ENDIF

              sTemp := NextExp( @sLine, ',', NIL, NIL, NIL ) // Content - Ignoring sNextAnchor !!!
              IF sTemp == NIL
                 TraceLog( "ERROR! Empty '{||'" )
                 Alert( "ERROR! Empty '{||'" )
                 EXIT
              ELSE
                 sExp +=  sTemp
              ENDIF

              sToken := NextToken( @sLine ) // Close
              IF sToken == NIL
                 TraceLog( "ERROR! Unbalanced '{' at: " + sExp )
                 Alert( "ERROR! Unbalanced '{' at: " + sExp )
                 EXIT
              ELSEIF Left( sToken, 1 ) == '}'
                 sExp += sToken
              ELSE
                 sLine := sToken + sLine
                 TraceLog( "ERROR! Unbalanced '{' at: " + sExp )
                 Alert( "ERROR! Unbalanced '{' at: " + sExp )
                 EXIT
              ENDIF
           ELSE

         #endif

              /* Literal array */
              IF sNext1 == '}'
                 sExp           += sNextToken
                 sLine          := sNextLine
                 #ifdef __HARBOUR__
                    SetArrayPrefix( .T. )
                 #else
                    s_bArrayPrefix := .T.
                 #endif
              ELSE
                 sTemp := NextExp( @sLine, ',', NIL, NIL, NIL ) // Content - Ignoring sNextAnchor !!!
                 IF sTemp == NIL
                    TraceLog( "ERROR! Unbalanced '{...'", sLine )
                    Alert( "ERROR! Unbalanced '{...'" )
                    EXIT
                 ELSE
                    sExp +=  sTemp
                 ENDIF

                 sToken := NextToken( @sLine ) // Close
                 IF sToken == NIL
                    TraceLog( "ERROR! Unbalanced '{' at: " + sExp )
                    Alert( "ERROR! Unbalanced '{' at: " + sExp )
                    EXIT
                 ELSEIF Left( sToken, 1 ) == '}'
                    sExp += sToken
                 ELSE
                    sLine := sToken + sLine
                    TraceLog( "ERROR! Unbalanced '{' at: " + sExp )
                    Alert( "ERROR! Unbalanced '{' at: " + sExp )
                    EXIT
                 ENDIF
              ENDIF

         #ifdef EXPLICIT_BLOCK
           ENDIF
         #endif

           sLastToken := "}"
           // Continue  2nd level checks below.
        ELSEIF s1 == "["
           sExp  += sToken
           sTemp := NextExp( @sLine, ',', NIL, NIL, NIL ) // Content - Ignoring sNextAnchor !!!
           IF sTemp == NIL
              Alert( "ERROR! Unbalanced '[' at: " + sExp )
              EXIT
           ELSE
              sExp += sTemp
           ENDIF

           sToken := NextToken( @sLine ) // Close
           IF sToken == NIL
              Alert( "ERROR! Unbalanced '[' at: " + sExp )
              EXIT
           ELSEIF Left( sToken, 1 ) == ']'
              sExp += sToken
           ELSE
              sLine := sToken + sLine
              Alert( "ERROR! Unbalanced '[' at: " + sExp )
              EXIT
           ENDIF

           sLastToken := "]"
           // Continue  2nd level checks below.
        ELSEIF s1 $ ".*/=^><!$%#)}]?"
           sLine := sToken + sLine
           EXIT
        ELSEIF s1 == ","
           IF cType == ","
              sList += ( sExp + sToken )
              sExp  := ""
              LOOP
           ELSEIF cType == "A"
              aAdd( aExp, sExp )
              sExp  := ""
              LOOP
           ELSE
              //? "DONT CONTINUE: " + sLine
              sLine := sToken + sLine
              EXIT
           ENDIF
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSEIF nLen == 2

        IF s2 $ '++\--'
           sExp += sToken
           LOOP
        ELSEIF s2 $ "->\:=\==\!=\<>\>=\<=\+=\-=\*=\^=\**\/=\%=\??"
           sLine := sToken + sLine
           EXIT
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSEIF nLen == 4

        IF s4 == '.OR.'
           sLine := sToken + sLine
           EXIT
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSEIF nLen == 5

        IF s5 == '.AND.'
           sLine := sToken + sLine
           EXIT
        /* .NOT. is being translated to ! at NextToken() !!!
        ELSEIF s5 == ".NOT."
           sExp       += sToken
           LOOP
        */
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSE

        sExp       += sToken
        sLastToken := sJustToken

     ENDIF

     // ------------------
     // 2nd. Level.
     // ------------------

     //TraceLog( sExp, sLastToken, sLine, nLen, sToken, sNextToken )

     IF sLastToken == NIL .OR. Right( sLastToken, 1 ) == ' '
        TraceLog( sExp, sLastToken, sLine, nLen, sToken, sNextToken )
        Alert( "??? " + sExp )
        EXIT
     ENDIF

     nLen := Len( sLastToken )
     cLastChar := Right( sLastToken, 1 )

     IF Empty( sLine )
        EXIT
     ELSE
        sNextLine  := sLine
        sNextToken := NextToken( @sNextLine, .T. )
        IF sNextToken == NIL
           sNextToken := ""
        ENDIF
     ENDIF

     sJustNext := RTrim( sNextToken )
     nNextLen := Len( sJustNext )
     sNext1 := Left( sJustNext, 1 )
     sNext2 := sNext4 := sNext5 := ""
     IF nNextLen == 2
        sNext2 := sJustNext
     ELSEIF nNextLen == 4
        sNext4 := Upper( sJustNext )
     ELSEIF nNextLen == 5
        sNext5 := sJustNext
     ENDIF

     IF bDbgExp
        ? "2nd. Level - Token: '" + sToken + "' Next: '" + sNextToken + "'"
        WAIT
     ENDIF

     IF sNextAnchor != NIL  .AND. sJustNext == sNextAnchor
        // Clipper give preference to ',' in list expression.
        IF ! ( sNextAnchor == ',' .AND. cType $ ",A" )
           EXIT
        ENDIF
     ENDIF

     //TraceLog( sExp, sToken, sJustToken, nLen, sNextToken, sJustNext, nNextLen, sLastToken, cLastChar, sNextAnchor )

     IF nNextLen == 1

        IF sNext1 == '(' .AND. ( IsAlpha( cLastChar ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "_."  )
           LOOP
        ELSEIF sNext1 == '[' // No need to check prefix because NextToken() already has the logic.
           LOOP
        ELSEIF sNext1 $ "+-*/:=^!><!$%#|" // *** Very ODD Clipper consider '|' a continuation token !!!
           sExp           += sNextToken
           sLine          := sNextLine
           #ifdef __HARBOUR__
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           LOOP
        ENDIF

     ELSEIF nNextLen == 2

        IF sNext2 $ "--\++"
           IF IsAlpha( cLastChar  ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "_.]"
              sExp           += sNextToken
              sLine          := sNextLine
              #ifdef __HARBOUR__
                 SetArrayPrefix( .F. )
              #else
                 s_bArrayPrefix := .F.
              #endif
           ENDIF
        ELSEIF sNext2 $ "->\:=\==\!=\<>\>=\<=\+=\-=\*=\/=\^=\**\%="
           sExp           += sNextToken
           sLine          := sNextLine
           #ifdef __HARBOUR__
              SetArrayPrefix( .T. )
           #else
              s_bArrayPrefix := .T.
           #endif
           LOOP
        ENDIF

     ELSEIF nNextLen == 4

        IF sNext4 == ".OR."
           sExp           += sNextToken
           sLine          := sNextLine
           #ifdef __HARBOUR__
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           LOOP
        ENDIF

     ELSEIF nNextLen == 5

        IF sNext5 == ".AND."
           sExp           += sNextToken
           sLine          := sNextLine
           s_bArrayPrefix := .F.
           #ifdef __HARBOUR__
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           LOOP
        /* .NOT. is being translated to ! at NextToken() !!!
        ELSEIF sNext5 == ".NOT."
           sExp           += sNextToken
           sLine          := sNextLine
           s_bArrayPrefix := .F.
           #ifdef __HARBOUR__
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif */
        ENDIF

     ENDIF

     // ------------------
     // 3rd. Level.
     // ------------------

     IF sNext1 == ','
        IF cType == ","
           sList          += ( sExp + sNextToken )
           sLine          := sNextLine
           #ifdef __HARBOUR__
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           sExp           := ""
        ELSEIF cType == "A"
           aAdd( aExp, sExp )
           sLine          := sNextLine
           #ifdef __HARBOUR__
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
           sExp           := ""
        ELSE
           //? "DONT CONTINUE: " + sLine
           EXIT
        ENDIF
     ELSE
        //? "DONT CONTINUE: " + sLine
        EXIT
     ENDIF
  ENDDO

  IF cType == 'A'
     IF sExp == ""
        IF Len( aExp ) == 0
           aExp := NIL
        ENDIF
     ELSE
        aAdd( aExp, sExp )
     ENDIF

     IF bDbgExp
        IF ! ( ProcName(1) == "NEXTEXP" )
           ? "List Exp: " + '{'
           FOR Counter := 1 TO Len( aExp )
              ?? aExp[Counter]
              IF Counter < Len( aExp )
                 ?? ','
              ENDIF
           NEXT
           ?? '}'
        ENDIF
     ENDIF
  ELSEIF cType == ','
     IF sExp == ""
        IF sList == ""
           sExp := NIL
        ELSE
           sExp := sList
        ENDIF
     ELSE
        sExp := ( sList + sExp )
     ENDIF

     IF bDbgExp
        ? "List =", sExp, " Next:", sLine
     ENDIF
  ELSE
     IF sExp == ""
        sExp := NIL
     ENDIF
     IF bDbgExp
        ? "Exp =", sExp, " Next:", sLine
     ENDIF
  ENDIF

  IF bDbgExp
     WAIT
  ENDIF

  //TraceLog( "*** Finish", cType, aExp, sExp, sLine, sNextAnchor )

RETURN IIF( cType == 'A', aExp, sExp )
