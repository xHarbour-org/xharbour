/*
 * $Id$
 */

#include "simpleio.ch"
#include "hbrddsql.ch"

REQUEST SDDODBC, SQLMIX

PROC main()
   LOCAL cpath

   HB_FNAMESPLIT( EXENAME(), @cPath )
   RDDSETDEFAULT( "SQLMIX" )
   SET( _SET_DATEFORMAT, "yyyy-mm-dd" )
   ? "Connect:", RDDINFO( RDDI_CONNECT, { "ODBC", "DBQ=" + cPath + "..\..\hbodbc\tests\test.mdb;Driver={Microsoft Access Driver (*.mdb)}" } )
   ? "Use:", DBUSEAREA( .T.,, "select * from test", "test" )
   ? "Alias:", ALIAS()
   ? "DB struct:", VALTOPRG( DBSTRUCT() )
   INKEY( 0 )
   BROWSE()

   INDEX ON FIELD->SALARY TO salary
   DBGOTOP()
   BROWSE()
   DBCLOSEAREA()
RETURN
