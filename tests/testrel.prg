/* $Id$ */

/* Test for SET RELATION and friends */

FUNCTION Main()
  DbCreate( "testrel1.dbf", { { "FIELD1", "N", 5, 0 } } )
  DbCreate( "testrel2.dbf", { { "REL1", "N", 5, 0 } } )
  USE testrel2
  APPEND BLANK
  FIELD->REL1 := 3
  APPEND BLANK
  FIELD->REL1 := 2
  APPEND BLANK
  FIELD->REL1 := 1
  INDEX ON FIELD->REL1 TO testrel2
  USE testrel1 NEW
  APPEND BLANK
  FIELD->FIELD1 := 1
  APPEND BLANK
  FIELD->FIELD1 := 2
  APPEND BLANK
  FIELD->FIELD1 := 3
  GO TOP
  DbSetRelation( "TESTREL2", {|| FIELD->FIELD1 }, "FIELD->FIELD1" )
  DO WHILE !Eof()
    ? "1: RecNo() ", RecNo(), " Value ", FIELD->FIELD1, "; 2: RecNo() ", TESTREL2->( RecNo() ), " Value ", TESTREL2->REL1
    SKIP
  ENDDO
RETURN 0
