PROCEDURE Main

   MEMVAR cName, Counter

   FIELD first

   LOCAL   cLocal
   PRIVATE cName := "Private", Counter

   CLEAR SCREEN

   #ifdef HELLO
     Alert( "Defined" )
   #endif

   Alert( "Testing PP as Interpreter... " )

   ? cName

   USE ..\..\tests\test
   IF ! File( "test" + IndexExt() )
      INDEX on FIELD->First TO First
   ELSE
      SET INDEX TO First
   ENDIF

   GO TOP

   cName := FIELD->First + FIELD->Last

   IF cName == FIELD->First + FIELD->Last
      ? "Ok"
   ELSE
      ? "Err"
   ENDIF

   DO CASE
      CASE cName == First // Not exact!
        ? "Err"

      CASE cName = First // But still equal
        ? "Ok"

      OTHERWISE
        ? "Err"
   ENDCASE

   FOR Counter := 1 TO 10
     ? Counter
     IF Counter == 1
        #ifdef __HARBOUR__
           Counter++
        #else
           Counter := Counter + 1
        #endif
        LOOP
     ENDIF

     IF Counter == 6
        EXIT
     ENDIF
   NEXT

   REPLACE First WITH "From PP"

   ? FIELD->First

   cLocal := "in main"
   ? Test( cName, 1, .T. )
   ? cLocal
   ? cName

   ? cFromTest
   ? Type( "TestPrv" )

RETURN

static FUNCTION Test( P1, P2 )

    MEMVAR cName

    PRIVATE TestPrv
    PUBLIC cFromTest

    ? P1, P2

    ? cName
    ? Type( "cLocal" ) // U

    M->TestPrv := "Private of Test"
    Test2()

RETURN ProcName()

PROCEDURE Test2

   ? ProcName(), ProcLine(), M->testPrv

RETURN

INIT Procedure TestInit

   Alert( "In TestInit()" )

RETURN

EXIT Procedure TestExit

   Alert( "In TestExit()" )

RETURN
