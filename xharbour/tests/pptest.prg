PROCEDURE Main

   LOCAL   cLocal
   PRIVATE cName := "Private"

   CLEAR SCREEN

   Alert( "Testing PP as Interpreter... " )

   ? cName

   USE test
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

   REPLACE First WITH "From PP"

   ? FIELD->First

   //DO t6.prg

   cLocal := "in main"
   ? Test( cName, 1, .T. )
   ? cLocal
   ? cName

   ? cFromTest
   ? TestPrv

RETURN

static FUNCTION Test( P1, P2 )

    PRIVATE TestPrv
    PUBLIC cFromTest

    ? P1, P2

    ? cName
    ? cLocal

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
