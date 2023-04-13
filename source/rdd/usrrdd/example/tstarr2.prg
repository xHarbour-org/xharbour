REQUEST ARRAYRDD
REQUEST DBTCDX

PROC MAIN()
   LOCAL aStruct, n, xVal
   LOCAL nSecs

   SET DATE TO ITALIAN
   SET CENTURY ON
   SET DELETED OFF
   CLEAR SCREEN

   SetUnhandledExceptionFilter( @GpfHandler() )

   //AltD()

   ? "Speed test skipping all records 100 times"

   ? "Real DBF file"
   USE test.dbf VIA "DBTCDX" NEW

   nSecs := Seconds()
   FOR n := 1 TO 100
       GO TOP
       DO WHILE !Eof()
          SKIP
       ENDDO
   NEXT
   ? Seconds() - nSecs

   aStruct := dbStruct()

   dbCreate( "arrtest.dbf", aStruct, "ARRAYRDD" )

   USE arrtest.dbf VIA "ARRAYRDD" NEW

   CLOSE test

   SELECT arrtest

   APPEND FROM test VIA "DBFCDX"

   GO TOP

   ? "Memory DBF file"
   nSecs := Seconds()
   FOR n := 1 TO 100
       GO TOP
       DO WHILE !Eof()
          SKIP
       ENDDO
   NEXT
   ? Seconds() - nSecs

   CLOSE arrtest

   ? "Show using 2 workareas of same database in memory"
   USE arrtest.dbf VIA "ARRAYRDD" NEW ALIAS "array1" SHARED

   USE arrtest.dbf VIA "ARRAYRDD" NEW ALIAS "array2" SHARED

   array1->( dbGoto( 200 ) )
   array2->( dbGoto( 300 ) )

   ? array1->first
   ? array2->first

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
