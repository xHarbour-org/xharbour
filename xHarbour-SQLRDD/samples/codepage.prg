/*
* SQLRDD Test
* Copyright (c) 2009 - Miguel Angel Marchuet <miguelangel@marchuet.net>
* All Rights Reserved
*/

#include "sqlrdd.ch"
#include "dbinfo.ch"

#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

/*------------------------------------------------------------------------*/

Function Main( cDSN, lLog, cRdd )

   local aStruct := {{"DESCR","C",1,0}}
   local nCnn, i

   IF Empty( cRdd )
      cRDD := "SQLRDD"
   ENDIF

   REQUEST HB_CODEPAGE_PLWIN,HB_CODEPAGE_PL852

   ? ""
   ? "codepage.exe"
   ? ""
   ? "CODEPAGE demo"
   ? "(c) 2009 - Miguel Angel Marchuet"
   ? ""

   ? "Connecting to database..."

   Connect( cDSN )    // see connect.prg

   ? "Connected to                    :", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )
   ? "Creating table                  :", dbCreate( "TEST_CODEPAGE", aStruct, cRDD )

   ? ""
   hb_SetCodePage( "PLWIN" )
   ? "SetCodePage aplicattion to PLWIN:"
   ? ""

   USE "TEST_CODEPAGE" EXCLUSIVE VIA ( cRDD ) CODEPAGE "PL852"
   ? "Creating 01 index..."
   Index on DESCR            to TEST_CODEPAGE_IND01
   ? "Appending records.."

   s := seconds()

   For i = 33 to 128
      Append Blank
      Replace DESCR    with Chr( i )
   Next

   ? "dbCloseArea()       :", dbCloseArea()

   USE "TEST_CODEPAGE" SHARED VIA ( cRDD )
   ? 'Open table with codepage ' + DbInfo( DBI_CPID, "PL852" )
   ? "Opening Indexes"
   SET INDEX TO TEST_CODEPAGE_IND01

   ? " "
   ? "Press any key to browse()"

   inkey(0)
   clear

   browse(row()+1,1,row()+20,80)

   clear

   DbCloseAll()

Return NIL

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/
