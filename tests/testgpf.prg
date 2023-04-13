#include "hbexcept.ch"

//----------------------------------------------------------------------------//
PROCEDURE Main()

   // Register the address of the new GPF Handler function.
   SetUnhandledExceptionFilter( @GpfHandler() )

   // Now let's test...
   ForceGpf()

RETURN

//----------------------------------------------------------------------------//
Function GpfHandler( Exception )

   TraceLog( Exception )

   SWITCH Exception:ExceptionRecord:ExceptionCode
      CASE EXCEPTION_ACCESS_VIOLATION
         ? "EXCEPTION_ACCESS_VIOLATION"
         EXIT

      CASE EXCEPTION_DATATYPE_MISALIGNMENT
         ? "CASE EXCEPTION_DATATYPE_MISALIGNMENT"
         EXIT

      CASE EXCEPTION_ARRAY_BOUNDS_EXCEEDED
         ? "CASE EXCEPTION_ARRAY_BOUNDS_EXCEEDED"
         EXIT

      CASE EXCEPTION_FLT_DENORMAL_OPERAND
         ? "CASE EXCEPTION_FLT_DENORMAL_OPERAND"
         EXIT

      CASE EXCEPTION_FLT_DIVIDE_BY_ZERO
         ? "CASE EXCEPTION_FLT_DIVIDE_BY_ZERO"
         EXIT

      CASE EXCEPTION_FLT_INEXACT_RESULT
        ? "CASE EXCEPTION_FLT_INEXACT_RESULT"
         EXIT

      CASE EXCEPTION_FLT_INVALID_OPERATION
        ? "CASE EXCEPTION_FLT_INVALID_OPERATION"
         EXIT

      CASE EXCEPTION_FLT_OVERFLOW
         ? "CASE EXCEPTION_FLT_OVERFLOW"
         EXIT

      CASE EXCEPTION_FLT_STACK_CHECK
         ? "CASE EXCEPTION_FLT_STACK_CHECK"
         EXIT

      CASE EXCEPTION_FLT_UNDERFLOW
         ? "CASE EXCEPTION_FLT_UNDERFLOW"
         EXIT

      CASE EXCEPTION_INT_DIVIDE_BY_ZERO
         ? "CASE EXCEPTION_INT_DIVIDE_BY_ZERO"
         EXIT

      CASE EXCEPTION_INT_OVERFLOW
         ? "CASE EXCEPTION_INT_OVERFLOW"
         EXIT

      CASE EXCEPTION_PRIV_INSTRUCTION
         ? "CASE EXCEPTION_PRIV_INSTRUCTION"
         EXIT

      CASE EXCEPTION_IN_PAGE_ERROR
         ? "CASE EXCEPTION_IN_PAGE_ERROR"
         EXIT

      CASE EXCEPTION_ILLEGAL_INSTRUCTION
         ? "CASE EXCEPTION_ILLEGAL_INSTRUCTION"
         EXIT

      CASE EXCEPTION_NONCONTINUABLE_EXCEPTION
         ? "CASE EXCEPTION_NONCONTINUABLE_EXCEPTION"
         EXIT

      CASE EXCEPTION_STACK_OVERFLOW
         ? "CASE EXCEPTION_STACK_OVERFLOW"
         EXIT

      CASE EXCEPTION_INVALID_DISPOSITION
         ? "EXCEPTION_INVALID_DISPOSITION"
         EXIT

      CASE EXCEPTION_GUARD_PAGE
         ? "CASE EXCEPTION_GUARD_PAGE"
         EXIT

      CASE EXCEPTION_INVALID_HANDLE
         ? "EXCEPTION_INVALID_HANDLE"
         EXIT

      DEFAULT
         ? "UNKNOWN EXCEPTION", Exception:ExceptionRecord:ExceptionCode
   END

   Alert( "GPF Intercepted!" )

RETURN EXCEPTION_EXECUTE_HANDLER

//----------------------------------------------------------------------------//
#pragma BEGINDUMP
HB_FUNC( FORCEGPF )
{
   char gpf[1];

   strcpy( gpf, "Intentionally attempting to force a gpf" );
}

//----------------------------------------------------------------------------//
#pragma ENDDUMP

