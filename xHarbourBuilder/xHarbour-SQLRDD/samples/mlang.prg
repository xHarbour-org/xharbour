/*
* SQLRDD Test
* Copyright (c) 2003 - Marcelo Lombardo  <marcelo@xharbour.com.br>
* All Rights Reserved
*/

#include "sqlrdd.ch"
#include "msg.ch"

#define RECORDS_IN_TEST       1000
#define SQL_DBMS_NAME           17
#define SQL_DBMS_VER            18

/*------------------------------------------------------------------------*/

Function Main( cDSN, lLog )

   /* CODE_IS is the primary key 1st key. See SQLRDD.CH for details about structure array */

   local aStruct := {{"LI","N",2,0},;
                     {"CODE_ID","C",8,0 },;
                     {"CARDID","C",1,0},;
                     {"DESCR2","C",20,0,,,MULTILANG_FIELD_ON},;
                     {"DESCR","C",50,0},;
                     {"PERCENT","N",10,2},;
                     {"DAYS","N",8,0},;
                     {"DATE_LIM","D",8,0},;
                     {"ENABLE","L",1,0},;
                     {"OBS","M",10,0},;
                     {"VALUE","N",18,6}}

   local nCnn, i, s

   ? ""
   ? "mlang.exe"
   ? ""
   ? "SQLRDD multilanguage demo "
   ? "(c) 2005 - Marcelo Lombardo"
   ? ""

   Connect( cDSN )    // see connect.prg

   ? "Connected to        :", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )

   // Sets engine on, or no magic wll show
   SR_SetMultiLang( .t. )

   If lLog != NIL
      ? "Starting LOG", SR_GetActiveConnection(), SR_StartLog()
   endif

   ? "Creating table      :", dbCreate( "TEST_MLANG", aStruct, "SQLRDD" )

   USE "TEST_MLANG" SHARED VIA "SQLRDD"
   Index on CODE_ID TO TEST_MLANG_IND01

   ? "Appending " + alltrim(str(RECORDS_IN_TEST)) + " records and pushing 4 languages at same time in DESCR2 field.."

   s := seconds()

   For i = 1 to RECORDS_IN_TEST
      Append Blank

      Replace CODE_ID  with strZero( i, 5 )
      Replace DESCR    with dtoc( date() ) + " - " + time()
      Replace DAYS     with (RECORDS_IN_TEST - i)
      Replace DATE_LIM with date()
      Replace ENABLE   with .T.
      Replace OBS      with "This is a memo field. Seconds since midnight : " + alltrim(str(seconds()))

      // DESCR2 is setted as MULTILANG in dbCreate() array

      SR_SetBaseLang( LANG_EN_US )
      Replace DESCR2   with "Hello, friends"

      SR_SetBaseLang( LANG_PT_BR )
      Replace DESCR2   with "Ola, amigos"

      SR_SetBaseLang( LANG_DE_DE )
      Replace DESCR2   with "Hallo, Freunde"

      SR_SetBaseLang( LANG_ES_ES )
      Replace DESCR2   with "Hola, amigos"
   Next

   commit
   dbGoTop()

   clear screen

   SR_SetBaseLang( LANG_EN_US )
   alert( "Browse in English" )
   browse(1,1,20,80)

   SR_SetBaseLang( LANG_PT_BR )
   alert( "Browse in Portuguese" )
   browse(1,1,20,80)

   alert( "Browse in German" )
   SR_SetBaseLang( LANG_DE_DE )
   browse(1,1,20,80)

   SR_SetBaseLang( LANG_ES_ES )
   alert( "Browse in Spanish" )
   browse(1,1,20,80)

   Use

   oSql   := SR_GetConnection()

   SR_SetBaseLang( LANG_EN_US )
   nErr := oSql:exec( "SELECT A.CODE_ID, A.DESCR2 FROM TEST_MLANG A WHERE A.CODE_ID < '00005'",,.t.,,"test.dbf" )
   alert( "Query in English" )
   browse(1,1,20,80)
   use

   SR_SetBaseLang( LANG_PT_BR )
   nErr := oSql:exec( "SELECT A.CODE_ID, A.DESCR2 FROM TEST_MLANG A WHERE A.CODE_ID < '00005'",,.t.,,"test.dbf" )
   alert( "Query in Portuguese" )
   browse(1,1,20,80)
   use

   SR_SetBaseLang( LANG_DE_DE )
   nErr := oSql:exec( "SELECT A.CODE_ID, A.DESCR2 FROM TEST_MLANG A WHERE A.CODE_ID < '00005'",,.t.,,"test.dbf" )
   alert( "Query in German" )
   browse(1,1,20,80)
   use

   SR_SetBaseLang( LANG_ES_ES )
   nErr := oSql:exec( "SELECT A.CODE_ID, A.DESCR2 FROM TEST_MLANG A WHERE A.CODE_ID < '00005'",,.t.,,"test.dbf" )
   alert( "Query in Spanish" )
   browse(1,1,20,80)
   use

Return NIL

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/
