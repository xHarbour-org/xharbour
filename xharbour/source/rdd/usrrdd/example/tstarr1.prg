REQUEST ARRAYRDD
REQUEST DBTCDX

PROC MAIN()
   LOCAL aStruct, n, xVal

   SET DATE TO ITALIAN
   SET CENTURY ON
   SET DELETED OFF
   CLEAR SCREEN

   SetUnhandledExceptionFilter( @GpfHandler() )

   //AltD()

   ? "Open a existent dbf"
   USE test.dbf VIA "DBTCDX" NEW

   ? "Get structure"
   aStruct := dbStruct()

   ? "Creating a dbf in memory"
   dbCreate( "arrtest.dbf", aStruct, "ARRAYRDD" )

   ? "Using it"
   USE arrtest.dbf VIA "ARRAYRDD" NEW

   ? "Close real dbf to append from (need to be closed)"
   CLOSE test

   SELECT arrtest

   ? "Appending all dbf from read databse in memory"
   APPEND FROM test VIA "DBFCDX"

   ? "Show it - Please don't press any key except movement keys and ESC"
   ? "          to exit from browse(), otherwise you will get an error"
   ? "          due to missing index support"
   WAIT

   GO TOP
   BROWSE()
   CLEAR SCREEN

   ? "Now delete 10 records starting from RECNO 15, and set DELETE ON"
   SET DELETED ON

   GOTO 15
   FOR n := 1 TO 10
       DELETE
       SKIP
   NEXT
   WAIT

   ? "Show it - look at RECNO value at top right, after RecNo 14 it have to jump to 25"
   WAIT
   GO TOP
   BROWSE()
   CLEAR SCREEN

   ? "Now ZAP entire database (note that as real dbf, this command not erase file in memory)"
   GO TOP

   ZAP

   ? "Show it"
   WAIT
   BROWSE()
   CLEAR SCREEN

   WAIT

   ? "Try to create newly same database, I have to get an error"
   TRY
      dbCreate( "arrtest.dbf", aStruct, "ARRAYRDD" )
   CATCH
      ? "ERROR: database already exists"
   END
   WAIT

   /*
     EraseArrayRdd() function is equivalent of FErase() function, but works in memory
   */

   ? " Delete memory file - getting an error because dabatase is in use"
   TRY
      EraseArrayRdd( "arrtest.dbf" )
   CATCH
      ? "ERROR: database in use"
   END
   WAIT

   ? "Close database"
   CLOSE arrtest

   ? "Delete memory file - now have to be ok"
   TRY
      EraseArrayRdd( "arrtest.dbf" )
      ? "OK, deleted"
   CATCH
      ? "ERROR: database in use"
   END

   WAIT

RETURN

#include "hbexcept.ch"

Function GpfHandler( Exception )
   LOCAL cMsg, nCode
   //TraceLog( "GPF:", Exception )

   IF Exception <> NIL
      nCode := Exception:ExceptionRecord:ExceptionCode
      SWITCH nCode
         CASE EXCEPTION_ACCESS_VIOLATION
            cMsg := "EXCEPTION_ACCESS_VIOLATION"
            EXIT

         CASE EXCEPTION_DATATYPE_MISALIGNMENT
            cMsg := "CASE EXCEPTION_DATATYPE_MISALIGNMENT"
            EXIT

         CASE EXCEPTION_ARRAY_BOUNDS_EXCEEDED
            cMsg := "CASE EXCEPTION_ARRAY_BOUNDS_EXCEEDED"
            EXIT

         CASE EXCEPTION_FLT_DENORMAL_OPERAND
            cMsg := "CASE EXCEPTION_FLT_DENORMAL_OPERAND"
            EXIT

         CASE EXCEPTION_FLT_DIVIDE_BY_ZERO
            cMsg := "CASE EXCEPTION_FLT_DIVIDE_BY_ZERO"
            EXIT

         CASE EXCEPTION_FLT_INEXACT_RESULT
            cMsg := "CASE EXCEPTION_FLT_INEXACT_RESULT"
            EXIT

         CASE EXCEPTION_FLT_INVALID_OPERATION
            cMsg := "CASE EXCEPTION_FLT_INVALID_OPERATION"
            EXIT

         CASE EXCEPTION_FLT_OVERFLOW
            cMsg := "CASE EXCEPTION_FLT_OVERFLOW"
            EXIT

         CASE EXCEPTION_FLT_STACK_CHECK
            cMsg := "CASE EXCEPTION_FLT_STACK_CHECK"
            EXIT

         CASE EXCEPTION_FLT_UNDERFLOW
            cMsg := "CASE EXCEPTION_FLT_UNDERFLOW"
            EXIT

         CASE EXCEPTION_INT_DIVIDE_BY_ZERO
            cMsg := "CASE EXCEPTION_INT_DIVIDE_BY_ZERO"
            EXIT

         CASE EXCEPTION_INT_OVERFLOW
            cMsg := "CASE EXCEPTION_INT_OVERFLOW"
            EXIT

         CASE EXCEPTION_PRIV_INSTRUCTION
            cMsg := "CASE EXCEPTION_PRIV_INSTRUCTION"
            EXIT

         CASE EXCEPTION_IN_PAGE_ERROR
            cMsg := "CASE EXCEPTION_IN_PAGE_ERROR"
            EXIT

         CASE EXCEPTION_ILLEGAL_INSTRUCTION
            cMsg := "CASE EXCEPTION_ILLEGAL_INSTRUCTION"
            EXIT

         CASE EXCEPTION_NONCONTINUABLE_EXCEPTION
            cMsg := "CASE EXCEPTION_NONCONTINUABLE_EXCEPTION"
            EXIT

         CASE EXCEPTION_STACK_OVERFLOW
            cMsg := "CASE EXCEPTION_STACK_OVERFLOW"
            EXIT

         CASE EXCEPTION_INVALID_DISPOSITION
            cMsg := "EXCEPTION_INVALID_DISPOSITION"
            EXIT

         CASE EXCEPTION_GUARD_PAGE
            cMsg := "CASE EXCEPTION_GUARD_PAGE"
            EXIT

         CASE EXCEPTION_INVALID_HANDLE
            cMsg := "EXCEPTION_INVALID_HANDLE"
            EXIT

         DEFAULT
            cMsg := "UNKNOWN EXCEPTION (" + cStr( Exception:ExceptionRecord:ExceptionCode ) + ")"
      END

      //IF cMsg <> NIL
         //Tracelog( "GPF Intercepted!", cMsg )
         //Alert( "GPF Intercepted!" + CRLF + cMsg )
      //ENDIF
   ENDIF

   Throw( ErrorNew( "GPFHANDLER", 0, 0, ProcName(), "GPF INTERCEPTED", { cMsg, Exception, nCode } ) )

RETURN EXCEPTION_EXECUTE_HANDLER
