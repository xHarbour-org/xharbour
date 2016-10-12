/*
* SQLRDD Test
* Copyright (c) 2003 - Marcelo Lombardo  <marcelo@xharbour.com.br>
* All Rights Reserved
*/

#include "sqlrdd.ch"

#define RECORDS_IN_TEST                    100
#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18
#define TABLE_NAME               "TEST_STRUCT"

/*------------------------------------------------------------------------*/

Function Main( cRDD, cDSN )

   /* CODE_IS is the primary key 1st key. See SQLRDD.CH for details about structure array */

   local aStruct := {{"CODE_ID","C",8,0,.F.,,,,,1 },{"CARDID","C",1,0},{"DESCR","C",50,0},{"PERCENT","N",10,2},{"DAYS","N",8,0},{"DATE_LIM","D",8,0},{"ENABLE","L",1,0},{"OBS","M",10,0}}
   local nCnn, i, s, r, c

   ? ""
   ? "ChgStruct.exe"
   ? ""
   ? "Small SQLRDD demo"
   ? "(c) 2005 - Marcelo Lombardo"
   ? ""

   cRDD := "SQLRDD"

   SET AUTOPEN ON

   Connect( @cRDD, cDSN )    // see connect.prg

   ? "Connected to        :", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )
   ? "RDD in use          :", cRDD

   // SR_SetSyntheticIndex(.t.)

   ? "Creating table      :", dbCreate( TABLE_NAME, aStruct, cRDD )

   USE TABLE_NAME EXCLUSIVE VIA cRDD

   ? "Table opened. Alias :", select(), alias(), RddName()
   ? "Fieldpos( CODE_ID ) :", Fieldpos( "CODE_ID" )
   ? "Fieldpos( DESCR )   :", Fieldpos( "DESCR" )

   ? "Creating 02 indexes..."

   s := seconds()

   Index on CODE_ID+DESCR to TEST_STRUC_IND01
   Index on str(DAYS)+dtos(DATE_LIM) to TEST_STRUC_IND02

   ? "Done, Elapsed time  :", seconds() - s, "seconds"
   ? ""

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

   ? "Done, Elapsed time  :", seconds() - s, "seconds"
   ? ""

   ? "dbClearIndex()      :", dbClearIndex()
   ? "dbCloseArea()       :", dbCloseArea()

   USE TABLE_NAME SHARED VIA cRDD

   ? "Opening Indexes"
   SET INDEX TO TEST_STRUC_IND01
   SET INDEX TO TEST_STRUC_IND02 ADDITIVE

   SET ORDER TO 1
   dbGoTo(35)

   ? " "
   ? "************ Old structure ************"
   ? " "
   ? sr_ShowVector( dbStruct() )
   ? "OrdCount()   :", OrdCount()
   wait
   r = row()
   c = col()
   browse()
   setpos( r,c)

   aStruct := {{"CODE_ID","C",8,0,.F.,,,,,1 },{"CARDID","C",4,0},{"DESCR","C",60,0},{"PERCENT","N",18,4}, {"NEW_FIELD_HERE","C",8,0},{"DATE_LIM","D",8,0},{"ENABLE","L",1,0},{"OBS","M",10,0}}

   SR_ChangeStruct( TABLE_NAME, aStruct )

   ? " "
   ? "************ New structure ************"
   ? " "
   ? sr_ShowVector( dbStruct() )
   ? "OrdCount()   :", OrdCount()
   ? "Indexord()   :", Indexord()
   ? "Please note both indexes were automatically dropped since columns has changed"
   wait
   r = row()
   c = col()
   browse()
   setpos( r,c)

   wait

Return NIL

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/
