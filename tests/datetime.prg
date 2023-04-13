REQUEST DBFCDX

PROCEDURE Main()
   LOCAL nItem

   RDDSETDEFAULT( "DBFCDX" )

   SET TIME FORMAT "hh:mm"

   DbCreate( "TestDT", { { "STRINGDT",  "C", 21, 0 },;
                         { "STRINGT",   "C", 21, 0 },;
                         { "DATE",      "D",  8, 0 },;
                         { "DATETIME",  "T",  8, 0 },;
                         { "TIMESTAMP", "@",  8, 0 } } )
   USE TESTDT
   INDEX ON FIELD->DATE TAG ONE TO TESTDT
   INDEX ON FIELD->DATETIME TAG TWO TO TESTDT
   INDEX ON FIELD->TIMESTAMP TAG TREE TO TESTDT

   FOR nItem := 1 TO 100
      DBAPPEND()
      FIELD->DATE     := Date() - nItem
      FIELD->DATETIME := DateTime() - nItem
      FIELD->STRINGDT := TTOC( FIELD->DATETIME )
      DbSkip( 0 ) /* To force TIME STAMP update */
      FIELD->STRINGT  := TTOC( FIELD->TIMESTAMP )
   NEXT

   OrdSetFocus( 'TWO' )

   BROWSE()

RETURN

#ifndef __XHARBOUR__
STATIC FUNCTION DATETIME( D )
RETURN HB_DATETIME( D )

STATIC FUNCTION TTOC( D )
RETURN HB_TTOC( D )
#endif



