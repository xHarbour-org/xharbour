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
SR_SETSQL2008NEWTYPES(.t.)
   ? ""
   ? "demo01.exe"
   ? ""
   ? "Small SQLRDD demo"
   ? "(c) 2003 - Marcelo Lombardo"
   ? ""

   ? "Connecting to database..."
   SR_SetMininumVarchar2Size( 2 ) 
   SR_SetOracleSyntheticVirtual( .F. )

   sr_usedeleteds(.f.)
   Connect( @cRDD, cDSN )    // see connect.prg

   ? "Connected to        :", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )
   ? "RDD in use          :", cRDD
   altd()
   ? "Creating table      :", dbCreate( "test_table4", aStruct, cRDD )
   ? "Creating table      :", dbCreate( "test_table41", aStruct, cRDD )
   USE "test_table41" NEW EXCLUSIVE VIA cRDD
   Index on CODE_ID+DESCR            tAG test_table4_IND01
   Index on str(DAYS)+dtos(DATE_LIM) tAG test_table4_IND02
   index on code_id+str(DAYS)+dtos(DATE_LIM) tAG test_table4_IND03


   USE "test_table4" NEW EXCLUSIVE VIA cRDD

   ? "Table opened. Alias :", select(), alias(), RddName()
   ? "Fieldpos( CODE_ID ) :", Fieldpos( "CODE_ID" )
   ? "Fieldpos( DESCR )   :", Fieldpos( "DESCR" )

   ? "Creating 02 indexes..."

   Index on CODE_ID+DESCR            to test_table4_IND01
   Index on str(DAYS)+dtos(DATE_LIM) to test_table4_IND02
   index on code_id+str(DAYS)+dtos(DATE_LIM) to test_table4_IND03
   index on dtos(date_lim) to test_table4_IND04


   ? "Appending " + alltrim(str(RECORDS_IN_TEST)) + " records.."

   s := seconds()

   For i = 1 to RECORDS_IN_TEST
      Append Blank
      Replace CODE_ID  with strZero( i, 5 )
      Replace DESCR    with dtoc( date() ) + " - " + time()
      Replace DAYS     with (RECORDS_IN_TEST - i)
      Replace DATE_LIM with date() + if(i%2 == 0,4,if(i%3==0,12,3))
      Replace ENABLE   with .T.
      Replace OBS      with "This is a memo field. Seconds since midnight : " + alltrim(str(seconds()))
   Next

   ? "dbClearIndex()      :", dbClearIndex()
   ? "dbCloseArea()       :", dbCloseArea()

   USE "test_table4" SHARED VIA cRDD

   ? "Opening Indexes"
   SET INDEX TO test_table4_IND01
   SET INDEX TO test_table4_IND02 ADDITIVE
   SET INDEX TO test_table4_IND03 ADDITIVE
   SET INDEX TO test_table4_IND04 ADDITIVE
altd()
set order to 4
?"dbseek( dtos(date()),.t.)   ",dbseek( dtos(date()),.t.),date_lim
?"dbseek( dtos(date()+4),.t.) ",dbseek( dtos(date()+4),.t.),date_lim
?"dbseek( dtos(date()+5),.t.) ",dbseek( dtos(date()+5),.t.),date_lim
?"dbseek( dtos(date()+12),.t.)",dbseek( dtos(date()+12),.t.),date_lim
?"dbseek( dtos(date()))       ",dbseek( dtos(date())),date_lim
?"dbseek( dtos(date()+4))     ",dbseek( dtos(date()+4)),date_lim
?"dbseek( dtos(date()+5))     ",dbseek( dtos(date()+5)),date_lim
?"dbseek( dtos(date()+12))    ",dbseek( dtos(date()+12)),date_lim
inkey(0)
   sr_starttrace()
set order to 1
go top
BROWSE()
set order to 2
go top
BROWSE()
set order to 3
go top
BROWSE()

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
