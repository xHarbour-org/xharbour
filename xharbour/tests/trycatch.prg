FUNCTION Main()

   LOCAL e, bPrevious

   bPrevious := ErrorBlock( {|oErr| QOut( "Outer Handler:", oErr:Operation, oErr:Description ) } )

   TRY
      ? "start"

      // Test using BEGIN SEQUENCE nested inside a TRY section.
      bPrevious := ErrorBlock( {|oErr| QOut( "Inner Handler:",  oErr:Operation, oErr:Description ) } )

      BEGIN SEQUENCE
         Eval( ErrorBlock(), ErrorNew( "MySys", 0, 1001, "MyOperation", "My Description", { "MyArg" } ) )
      END

      ErrorBlock( bPrevious )

      Throw( ErrorNew( "MySys", 0, 1001, "MyOperation", "My Description", { "MyArg" } ) )

      ? "Success"
   CATCH e
     ? "Caught", e:SubSystem, e:SubCode, e:Operation, e:Description, ValToPrg( e:Args )
   END

   Eval( ErrorBLock(), ErrorNew( "MySys", 0, 1001, "MyOperation", "My Description", { "MyArg" } ) )

   ? "Done"

RETURN 0
