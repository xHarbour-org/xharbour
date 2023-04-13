FUNCTION Main()

   LOCAL e, bPrevious

   bPrevious := ErrorBlock( {|oErr| QOut( "Outer Handler:", oErr:Operation, oErr:Description ), Break() } )

   TRY
      ? "start"

      // Test using BEGIN SEQUENCE with custom ErrorBlock() inside a TRY section.
      bPrevious := ErrorBlock( {|oErr| IIF( oErr:CanSubStitute, 0, Break() ) } )

      BEGIN SEQUENCE
         ? 3 / 0
      END

      ErrorBlock( bPrevious )

      Throw( ErrorNew( "MySys", 0, 1001, "MyOperation", "My Description", { "MyArg" } ) )

      ? "Oops"
   CATCH e
     ? "Caught", e:SubSystem, e:SubCode, e:Operation, e:Description, ValToPrg( e:Args )
   END

   Eval( ErrorBLock(), ErrorNew( "MySys", 0, 1001, "MyOperation", "My Description", { "MyArg" } ) )

   ? "Oops!!!"

RETURN 0
