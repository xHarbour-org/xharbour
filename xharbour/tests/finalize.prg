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
   CATCH oErr
      ? "Outer Caught:", oErr:Operation
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
      ? "Throwing"
      Throw( ErrorNew( "Finalize Test", 0, 0, "Forced Error" ) )
   CATCH oErr
      ? "Caought:", oErr:Operation

      ? "Return should be deffered"
      RETURN
   FINALLY
      ? "Finalized"
   END

   ? "Oops, should have returned after Finalize."

RETURN

PROCEDURE Test3()

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

   ? "Oops, should have returned after Re-Throw."

RETURN
