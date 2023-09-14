PROCEDURE Main()
   LOCAL MyStr := "123456789", oErr

   ? Test( "123456789", SubStr( MyStr, 0 ) )
   ? Test( ""         , SubStr( MyStr, 3, 0 ) )
   ? Test( "3456789"  , SubStr( MyStr, 3 ) )
   ? Test( "34567"    , SubStr( MyStr, 3, -3 ) )
   ? Test( "123456789", SubStr( MyStr, -15 ) )
   ? Test( ""         , SubStr( MyStr, -3, 0 ) )
   ? Test( "7"        , SubStr( MyStr, -3, -3 ) )
   ? Test( "1234567"  , SubStr( MyStr, -9, -3 ) )
   TRY
      ? Test( "Error!"   , SubStr( MyStr, -3, .F. ) )
   CATCH oErr
       ? "Ok: ", oErr:Description    
   FINALLY
      ? "Please make sure that all of the above 9 lines statrt with 'Ok:'"   
   END     
   
RETURN

FUNCTION Test( sExpected, sResult )
   IF sExpected == sResult
      RETURN "Ok: '" + sResult + "'"
   ENDIF
RETURN  "Expected: '" + sExpected + "' Got: '" + sResult + "'"
