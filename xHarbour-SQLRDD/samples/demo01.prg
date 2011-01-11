/*
* SQLRDD Test
* Copyright (c) 2003 - Marcelo Lombardo  <marcelo@xharbour.com.br>
* All Rights Reserved
*/

#include "sqlrdd.ch"

#define RECORDS_IN_TEST                   1000
#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

/*------------------------------------------------------------------------*/

Function Main( cRDD, cDSN )

   local aStruct := {{"CODE_ID","C",8,0 },;
                     {"CARDID","C",1,0},;
                     {"DESCR","C",50,0},;
                     {"PERCENT","N",10,2},;
                     {"DAYS","N",8,0},;
                     {"DATE_LIM","D",8,0},;
                     {"ENABLE","L",1,0},;
                     {"OBS","M",10,0},;
                     {"VALUE","N",18,6}}
   local nCnn, i

   ? ""
   ? "demo01.exe"
   ? ""
   ? "Small SQLRDD demo"
   ? "(c) 2003 - Marcelo Lombardo"
   ? ""

   ? "Connecting to database..."

   Connect( @cRDD, cDSN )    // see connect.prg

   ? "Connected to        :", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )
   ? "RDD in use          :", cRDD
   ? "Creating table      :", dbCreate( "TEST_TABLE", aStruct, cRDD )

   USE "TEST_TABLE" EXCLUSIVE VIA cRDD

   ? "Table opened. Alias :", select(), alias(), RddName()
   ? "Fieldpos( CODE_ID ) :", Fieldpos( "CODE_ID" )
   ? "Fieldpos( DESCR )   :", Fieldpos( "DESCR" )

   ? "Creating 02 indexes..."

   Index on CODE_ID+DESCR            to TEST_TABLE_IND01
   Index on str(DAYS)+dtos(DATE_LIM) to TEST_TABLE_IND02

   ? "Appending " + alltrim(str(RECORDS_IN_TEST)) + " records.."

   s := seconds()

   For i = 1 to RECORDS_IN_TEST
      Append Blank
      Replace CODE_ID  with strZero( i, 5 )
      Replace DESCR    with dtoc( date() ) + " - " + time()
      Replace DAYS     with (RECORDS_IN_TEST - i)
      Replace DATE_LIM with date()
      Replace ENABLE   with .T.
      Replace OBS      with "This is a memo field. Seconds since midnight : " + alltrim(str(seconds()))
   Next

   ? "dbClearIndex()      :", dbClearIndex()
   ? "dbCloseArea()       :", dbCloseArea()

   USE "TEST_TABLE" SHARED VIA cRDD

   ? "Opening Indexes"
   SET INDEX TO TEST_TABLE_IND01
   SET INDEX TO TEST_TABLE_IND02 ADDITIVE

   ? "Set Order to 1      :", OrdSetFocus(1)
   ? "Seek                :", dbSeek( "00002" )

   ? "found()             :", found()
   ? "Recno(),bof(),eof() :", recno(), bof(), eof()
   ? "dbUnLock()          :", dbUnLock()
   ? "RLock(), dbRLockList:", rlock(), sr_showVector( dbRLockList() )
   ? "Writes to the WA    :", FIELD->DESCR := "Hello, SQL!", FIELD->PERCENT  := 23.55
   ? "dbCommit()          :", dbCommit()
   ? " "
   ? "Press any key to browse()"

   inkey(0)
   clear

   browse(row()+1,1,row()+20,80)

   clear

   ? "Order 2, key is     :", OrdSetFocus(2), ordKey()

   OrdScope( 0, str(RECORDS_IN_TEST / 4,8) )
   OrdScope( 1, str(RECORDS_IN_TEST / 2,8) )

   ? "TOP Scope           :", OrdScope( 0 )
   ? "BOTTOM Scope        :", OrdScope( 1 )

   ? "Press any key to browse() with another index and scope"
   inkey(0)
   dbGoTop()
   clear
   browse(row()+1,1,row()+20,80)

   SET SCOPE TO

   ? "Scope removed"
   ? "Press any key to browse()"
   inkey(0)

   dbGoTop()
   clear
   browse(row()+1,1,row()+20,80)

Return NIL

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/
