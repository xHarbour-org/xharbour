FUNCTION Main()

   LOCAL e

   TRY
      ? "start"

      Throw( ErrorNew( "MySys", 1001, "MyOperation", "My Description", { "MyArg" } ) )

      ? "Success"
   CATCH e
     ? "Caught", e:SubSystem, e:SubCode, e:Operation, e:Description, ValToPrg( e:Args )
   END

   ? "Done"

RETURN 0
