#include "sql.ch"

#xcommand GET COLUMN <nCol> INTO <cVar> => ;
  <cVar> := space( 128 ) ;;
  SQLGetData( hStmt, <nCol>, SQL_CHAR, len( <cVar> ), @<cVar> )

FUNCTION Main()

  LOCAL hEnv       := 0
  LOCAL hDbc       := 0
  LOCAL hStmt      := 0
  LOCAL cConstrin  := "DBQ=grocertogo.mdb;Driver={Microsoft Access Driver (*.mdb)}"
  LOCAL cConstrout := SPACE(1024)
  LOCAL nRows      := 0
  LOCAL cField1, cField2, cField3, cField4

  CLS

  ? padc( "*** ODBC ACCESS TEST ***", 80 )
  ?
  ? "Allocating environment... "
  SQLAllocEn( @hEnv )

  ?
  ? "Allocating connection... "
  SQLAllocCo( hEnv, @hDbc )

  ?
  ? "Connecting to driver " + cConstrin + "... "
  SQLDriverC( hDbc, cConstrin, @cConstrout )

  ? "Allocating statement... "
  SQLAllocSt( hDbc, @hStmt )

  ?
  ? "SQL: SELECT * FROM Products"
  SQLExecDir( hStmt, "select * from Products" )

  ?

  WHILE SQLFetch( hStmt ) == 0
     nRows++
     GET COLUMN 1 INTO cField1
     GET COLUMN 2 INTO cField2
     GET COLUMN 3 INTO cField3
     GET COLUMN 4 INTO cField4

     ? cField1, cField2, cField3, cField4
  ENDDO

  ? "------------------------------------------------------------------------------"
  ? Str( nRows, 4 ), " Row(s) found."

  SQLFreeStm( hStmt, SQL_DROP )
  SQLDisconn( hDbc )
  SQLFreeCon( hDbc )
  SQLFreeEnv( hEnv )

  RETURN( NIL )

