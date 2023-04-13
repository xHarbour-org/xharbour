PROCEDURE Main()

   LOCAL oErr

   TRY
      ? "Test 1"
      Test1()
      ?

      ? "Test 2"
      Test2()
      ?

      ? "Test 3"
      Test3()
      ?

      ? "Test 4"
      Test4()
      ?
   CATCH oErr
      ? "Outer Caught:", oErr:Operation
      ?

      TRY
         ? "Test 5"
         Test5()
         ?
      CATCH oErr
         ? "Outer Caught:", oErr:Operation
      END
   END

RETURN

PROCEDURE Test1()

   LOCAL oErr

   TRY
      ? "Trying"
   CATCH oErr
      ? "Oops:", oErr:Operation
   FINALLY
      ? "Finalized"
   END

   ? "Tested OK"

RETURN

PROCEDURE Test2()

   LOCAL oErr

   TRY
      ? "Trying"
   FINALLY
      ? "Finalized"
   END

   ? "Tested OK"

RETURN

PROCEDURE Test3()

   LOCAL oErr

   TRY
      ? "Trying"
      ? "Throwing"
      Throw( ErrorNew( "Finalize Test", 0, 0, "Forced Error" ) )
   CATCH oErr
      ? "Caought:", oErr:Operation

      ? "Return should be deffered"
      RETURN
   FINALLY
      ? "Finalized"
   END

   ? "Oops, should have returned after the FINALLY."

RETURN

PROCEDURE Test4()

   LOCAL oErr

   TRY
      ? "Trying"
      ? "Throwing"
      Throw( ErrorNew( "Finalize Test", 0, 0, "Forced Error" ) )
   CATCH oErr
      ? "Caought:", oErr:Operation

      ? "Throwing to outer, should be deffered"
      Throw( oErr )
   FINALLY
      ? "Finalized"
   END

   ? "Oops, should have Re-Throw, after the FINALLY."

RETURN

PROCEDURE Test5()

   TRY
      ? "Trying"
      ? "Throwing to outer, should be deffered"
      Throw( ErrorNew( "Finalize Test", 0, 0, "Forced Error" ) )
   FINALLY
      ? "Finalized"
   END

   ? "Oops, should have Re-Throw, after the FINALLY."

RETURN
