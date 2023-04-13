//
// $Id$
//

request DBFCDX

function Main()
   local fh, size, data
   local aStruct := { { "CHARACTER", "C", 25, 0 }, ;
                      { "NUMERIC",   "N",  8, 0 }, ;
                      { "DOUBLE",    "N",  8, 2 }, ;
                      { "DATE",      "D",  8, 0 }, ;
                      { "MEMO",      "M", 10, 0 }, ;
                      { "LOGICAL",   "L",  1, 0 } }

   CLS
   dbUseArea( .T., "DBFCDX", "test", "TESTDBF", .T., .F. )
   dbCreate( "testcdx", aStruct, "DBFCDX", .T., "TESTCDX" )

   ? "RddName:", RddName()
//   ? "Press any key to continue..."
//   InKey( 0 )
   Select( "TESTDBF" )
   SET FILTER TO TESTDBF->SALARY > 140000
   TESTDBF->( dbGoTop() )
   fh := fopen( "test.jpg" )
   FILESTATS( "test.jpg", 0, @size )
   ? size
   inkey(0)
   data := ""
   fh := fopen( "test.jpg" )
   fread( fh, @data, size )
   ? ferror()
   fclose(fh)
   while !TESTDBF->( Eof() )

      TESTCDX->( dbAppend() )
      TESTCDX->CHARACTER = TESTDBF->FIRST
      TESTCDX->NUMERIC = TESTDBF->SALARY
      TESTCDX->MEMO = data
      ? len( data )
      Inkey(0)
      TESTDBF->( dbSkip() )
   end

   ? TESTCDX->( RecCount() )
   TESTCDX->( dbGoTop() )
   ? TESTCDX->( Eof() )
   while !TESTCDX->( Eof() )
      ? TESTCDX->( RecNo() ), TESTCDX->NUMERIC
      fh = fcreate( "test1.jpg" )
      fwrite( fh, TESTCDX->MEMO )
      fclose( fh )
      TESTCDX->( dbSkip() )
//      ? "Press any key to continue..."
//      InKey( 0 )
   end

   FErase( "testcdx.cdx" )

   Select( "TESTCDX" )
   OrdCreate( "testcdx", "Character", "CHARACTER", FIELD->CHARACTER, .F. )


return nil