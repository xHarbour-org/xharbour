REQUEST ARRAYRDD

PROC MAIN()
   LOCAL aStruct

   SET DATE TO ITALIAN
   SET CENTURY ON
   SET DELETED OFF
   CLEAR SCREEN

   SetUnhandledExceptionFilter( @GpfHandler() )

   //AltD()

   ? "Create a new dbf in memory using dbCreate() command"
   aStruct := { ;
                { "NAME"     , "C", 40, 0 } ,;
                { "ADDRESS"  , "C", 40, 0 } ,;
                { "BIRTHDAY" , "D",  8, 0 } ,;
                { "AGE"      , "N",  3, 0 }  ;
              }

   dbCreate( "arrtest.dbf", aStruct, "ARRAYRDD" )

   ? "Open it"
   USE arrtest.dbf VIA "ARRAYRDD"

   ? "Show structure"
   ? ValToPrg( dbStruct() )
   WAIT

   ? "ALIAS", ALIAS(), "RECNO", RECNO(), ;
     "BOF", BOF(), "EOF", EOF(), "LASTREC", LASTREC()
   ? RECNO(), '"' + FIELD->NAME + '"'
   DBGOBOTTOM()
   ? RECNO(), '"' + FIELD->NAME + '"'
   DBGOTOP()
   ? RECNO(), '"' + FIELD->NAME + '"'
   WAIT

   ? "Adding some data"
   dbAppend()
   field->name     := "Giudice Francesco Saverio"
   field->address  := "Main Street 10"
   field->birthday := CToD( "03/01/1967" )
   field->age      := 39

   ? RECNO(), '"' + FIELD->NAME + '"'

   dbAppend()
   field->name     := "Mouse Mickey"
   field->address  := "Main Street 20"
   field->birthday := CToD( "01/01/1940" )
   field->age      := 66

   WHILE !EOF()
      ? RECNO(), '"' + FIELD->NAME + '"'
      IF RECNO()==20
         INKEY(0)
      ENDIF
      DBSKIP()
   ENDDO
   ? "ALIAS", ALIAS(), "RECNO", RECNO(), ;
     "BOF", BOF(), "EOF", EOF(), "LASTREC", LASTREC()
   WAIT
   DBGOBOTTOM()
   ? "ALIAS", ALIAS(), "RECNO", RECNO(), ;
     "BOF", BOF(), "EOF", EOF(), "LASTREC", LASTREC()
   WAIT
   WHILE !BOF()
      ? RECNO(), '[' + FIELD->NAME + ']'
      IF RECNO()==LASTREC()-20
         INKEY(0)
      ENDIF
      DBSKIP(-1)
   ENDDO
   ? "ALIAS", ALIAS(), "RECNO", RECNO(), ;
     "BOF", BOF(), "EOF", EOF(), "LASTREC", LASTREC()
   WAIT

   ? "Show it - Please don't press any key except movement keys and ESC"
   ? "          to exit from browse(), otherwise you will get an error"
   ? "          due to missing index support"
   WAIT
   BROWSE()

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
