Procedure Main( sMsg )

   LOCAL sText, sPPed, asPPed, oInterpreter, oErr, xResult, bDefErrBlock := ErrorBlock()

   TRY
      oInterpreter := TInterpreter():New()

      oInterpreter:AddLine( "/* Sample Script for embedded PP "   )
      oInterpreter:AddLine( " have fun... */"                     )
      oInterpreter:AddLine( ""                                    )
      oInterpreter:AddLine( "Procedure Test( cMacroVar, xValue )" )
      oInterpreter:AddLine( ""                                    )
      oInterpreter:AddLine( "   Local cVar := 'Hi There'"         )
      oInterpreter:AddLine( ""                                    )
      oInterpreter:AddLine( "   &cMacroVar := xValue"             )
      oInterpreter:AddLine( ""                                    )
      oInterpreter:AddLine( "   ? cVar, &cMacroVar, ;"            )
      oInterpreter:AddLine( "     xValue"                         )
      oInterpreter:AddLine( ""                                    )
      oInterpreter:AddLine( "   WHILE Inkey() == 0 // WAIT"       )
      oInterpreter:AddLine( "   ENDDO"                            )
      oInterpreter:AddLine( ""                                    )
      oInterpreter:AddLine( "RETURN LastKey()"                    )

      // Will automatically perform a ::Compile() first.
      xResult := oInterpreter:Run( "Param1", "Param2" )

      IF xResult:ClassName == "ERROR"
         Eval( bDefErrBlock, xResult )
      ELSE
         Alert( xResult )
      ENDIF
   CATCH oErr
      Eval( bDefErrBlock, oErr )
   END

   TRY
      sText := "/* Sample Script for embedded PP     " + Chr(10)
      sText += " have fun... */                      " + Chr(10)
      sText += "                                     " + Chr(10)
      sText += "Procedure Test( cMacroVar, xValue )  " + Chr(10)
      sText += "                                     " + Chr(10)
      sText += "   Local cVar := 'Hi There'          " + Chr(10)
      sText += "                                     " + Chr(10)
      sText += "   &cMacroVar := xValue              " + Chr(10)
      sText += "                                     " + Chr(10)
      sText += "   ? cVar, &cMacroVar,  ;            " + Chr(10)
      sText += "     xValue                          " + Chr(10)
      sText += "                                     " + Chr(10)
      sText += "   WHILE Inkey() == 0 // WAIT        " + Chr(10)
      sText += "   ENDDO                             " + Chr(10)
      sText += "                                     " + Chr(10)
      sText += "RETURN LastKey()                     "

      xResult := PP_RunText( sText, .T., { "Private_1", 1000 } )

      IF xResult:ClassName == "ERROR"
         Eval( bDefErrBlock, xResult )
      ELSE
         Alert( xResult )
      ENDIF
   CATCH oErr
      Eval( bDefErrBlock, oErr )
   END

   //OR ...
   TRY
      sPPed := PP_PreProText( sText )
      xResult := PP_RunText( sPPed, .F., { "Private_2", 2000 } )

      IF xResult:ClassName == "ERROR"
         Eval( bDefErrBlock, xResult )
      ELSE
         Alert( xResult )
      ENDIF
   CATCH oErr
      Eval( bDefErrBlock, oErr )
   END

   //OR ...
   TRY
      asPPed := {}
      PP_PreProText( sText, asPPed )

      xResult := PP_RunArray( asPPed, { "Private_3", 3000 } )

      IF xResult:ClassName == "ERROR"
         Eval( bDefErrBlock, xResult )
      ELSE
         Alert( xResult )
      ENDIF
   CATCH oErr
      Eval( bDefErrBlock, oErr )
   END

   // Now let's have some real fun...
   IF sMsg == "Recursively running self"
      Alert( "Let's STOP this madness..." )
      RETURN "Stopped"
   ENDIF

   TRY
      //PP_Run( "prgscrpt.prg", { "Recursively running self" } )
      xResult := oInterpreter:RunFile( "prgscrpt.prg", { "Recursively running self" }, ".pp", .F. ) // Output PreProcessed extension .pp wirhout blanks.

      IF xResult:ClassName == "ERROR"
         Eval( bDefErrBlock, xResult )
      ELSE
         Alert( xResult )
      ENDIF
   CATCH oErr
      Eval( bDefErrBlock, oErr )
   END

RETURN
