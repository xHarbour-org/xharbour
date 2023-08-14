/*
* SQLRDD namespace demo
* Copyright (c) 2008 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "sqlrdd.ch"
#include "dbinfo.ch"

#define RECORDS_IN_TEST                    100
#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

FUNCTION MAIN( cRdd, cDsn )

   local aStruct := {{"CODE_ID","C",8,0 },{"CARDID","C",1,0},{"DESCR","C",50,0},{"PERCENT","N",8,2},{"DAYS","N",6,0},{"DATE_LIM","D",8,0},{"ENABLE","L",1,0},{"OBS","M",10,0}, {"VALUE","N",18,6}}

   local cComm, apCode, cOut
   local nErr, nPos
   LOCAL vEmp := {}
   Local nCnn, s, i, oSql

   Connect( @cRDD, cDSN )    // see connect.prg

   ? "Connected to ", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )
   ? "Creating table"

   oSql   := SR_GetConnection()

   dbCreate( "TEST_TABLE3", aStruct, cRDD )
   USE TEST_TABLE3 via cRDD
   INDEX ON CODE_ID TO TBL3_INDX
   INDEX ON CODE_ID TAG CODE_ID FOR DAYS < 20

   For i = 1 to RECORDS_IN_TEST
      Append Blank
      Replace CODE_ID  with strZero( i, 5 )
      Replace DESCR    with dtoc( date() ) + " - " + time()
      Replace DAYS     with (RECORDS_IN_TEST - i)
      Replace DATE_LIM with date()
      Replace ENABLE   with .T.
      Replace OBS      with "This is a memo field. Seconds since midnight : " + alltrim(str(seconds()))
   Next

   dbGoTop()

   i := select()

   ? "RDD Version     :", dbInfo( DBI_RDD_VERSION )
   ? "RDD Build       :", dbInfo( DBI_RDD_BUILD )

   // Now lets play with NAMESPACES

   ? "SR_FILE()       :", sr_file( "TEST_TABLE3" )
   ? "FILE()          :", file( "TEST_TABLE3" )  // There is no phisical file with such name, should return .F.
   ? "SQLRDD.FILE()   :", sqlrdd.file( "TEST_TABLE3" )
   ? "Standard table structure"
   ? sr_showVector( dbStruct() )

   WITH NAMESPACE SQLRDD
   ? "FILE()          :", file( "TEST_TABLE3" )  // now it will use SQLRDD's file(), should return .T.
   ? "GLOBAL.FILE()   :", global.file( "TEST_TABLE3" )  // back to original xHB file(), should return .F.
   ? "Extended table structure"
   ? sr_showVector( dbStruct() )    // SQLRDD internal dbStruct() extended function
   END

   wait

Return

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/
