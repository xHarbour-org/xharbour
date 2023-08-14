/*
* SQLRDD Test
* Copyright (c) 2003 - Marcelo Lombardo  <marcelo@xharbour.com.br>
* All Rights Reserved
*/

#include "sqlrdd.ch"

#xcommand DEFAULT <uVar1> := <uVal1> ;
               [, <uVarN> := <uValN> ] => ;
                  <uVar1> := If( <uVar1> == nil, <uVal1>, <uVar1> ) ;;
                [ <uVarN> := If( <uVarN> == nil, <uValN>, <uVarN> ); ]

#define RECORDS_IN_TEST                  10000
#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

/*------------------------------------------------------------------------*/

Function Main( nRDD, cPath, lRecreate, cDSN )

   local aStruct := {;
                     {"CODE_ID","C",8,0 },;
                     {"DESCR","C",50,0},;
                     {"CARDID","C",1,0},;
                     {"PERCENT","N",10,2},;
                     {"DAYS","N",8,0},;
                     {"ENABLE","L",1,0},;
                     {"VALUE","N",18,6},;
                     {"OBS","M",10,0},;
                     {"DATE_LIM","D",8,0};
                     }

   local nCnn, i, cRDD, oSql, nMinTime, nIdealBuff, nTime, xVal, s
   Public cRDDName

//   REQUEST DBFNSX

   ? ""
   ? "tstRDD.exe [nRdd] [cPath]"
   ? "           1 => SQLRDD Extreme (default)"
   ? "           2 => SQLRDD"
   ? "           3 => DBFNTX"
   ? "           4 => DBFCDX"
   ? "           5 => DBFNSX"
   ? ""
   ? "RDD performance test"
   ? "(c) 2008 - Marcelo Lombardo"
   ? ""

   default nRdd := "1"

   If nRDD == NIL
      Quit
   EndIf

   DEFAULT cPath := "z:\temp\"      // We suggest a network drive to have a fair comparison
   DEFAULT lRecreate := .T.

   sr_useDeleteds( .f. )

   if valtype( lRecreate ) == "C"
      lRecreate := lRecreate $ "sSyY"
   EndIf

   SWITCH Val( nRdd )
   CASE 2
      cRDD  := "SQLRDD"
      cPath := ''
      EXIT
   CASE 3
      cRDD  := "DBFNTX"
      EXIT
   CASE 4
      cRDD  := "DBFCDX"
      EXIT
   CASE 5
      cRDD  := "DBFNSX"
      EXIT
   DEFAULT
      cRDD  := "SQLEX"
      cPath := ''
   END SWITCH

   IF Val( nRdd ) < 3
      ? "Connecting to database..."

      Connect( @cRDD, cDSN )    // see connect.prg
      ? "Connected to        :", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )

      oSql := sr_GetConnection()
   ENDIF

   ? " "
   ? "Current RDD:", cRDD
   ? " "

   IF (! If( "RDD" $ cRDD,;
            SQLRDD.File( cPath + "TEST_TABLE_RDD_" + cRDD ),;
            File( cPath + "TEST_TABLE_RDD_" + cRDD + ".dbf" ) ) ) .or. lRecreate

      ? "Creating table      :", dbCreate( cPath + "TEST_TABLE_RDD_" + cRDD, aStruct, cRDD )

      USE (cPath + "TEST_TABLE_RDD_" + cRDD) EXCLUSIVE VIA cRDD

      ? "Appending " + alltrim(str(RECORDS_IN_TEST)) + " records.."

      s := seconds() * 100

      For i = 1 to RECORDS_IN_TEST
         Append Blank
         Replace CODE_ID  with strZero( i, 5 )
         Replace DESCR    with dtoc( date() ) + " - " + strZero( i, 5 ) + " - " + time()
         Replace PERCENT  with i/(RECORDS_IN_TEST/10)
         Replace DAYS     with (RECORDS_IN_TEST - i)
         Replace DATE_LIM with date()
         REPLACE CARDID   with "X"
         Replace ENABLE   with .T.
         Replace VALUE    with i/10
         Replace OBS      with "This is a memo field. Seconds since midnight : " + alltrim(str(seconds())) + " record " + strZero( i, 5 )
      Next

      ? "Append performed in ", ((seconds()*100) - s)/100
      ? "Creating 02 indexes..."

      If "CDX" $ cRDD
         Index on CODE_ID+DESCR            TAG IND01
         Index on str(DAYS)+dtos(DATE_LIM) TAG IND02
         Index on CODE_ID+DESCR            TAG IND03 DESCEND
      Else
         Index on CODE_ID+DESCR         to (cPath + "TEST_TABLE_RDD_IND01")
         Index on str(DAYS)             to (cPath + "TEST_TABLE_RDD_IND02")  //+dtos(DATE_LIM)
         Index on CODE_ID+DESCR         to (cPath + "TEST_TABLE_RDD_IND03") DESCEND
      EndIf
      ? "dbClearIndex()      :", dbClearIndex()
      ? "dbCloseArea()       :", dbCloseArea()

   Else
      ? "Reusing existing table"
   EndIf

   nMinTime   := 99999999999
   nIdealBuff := 0

   s := seconds() * 100

   For nBuffer = 1 to 1

      USE (cPath + "TEST_TABLE_RDD_" + cRDD) SHARED VIA cRDD
      If "CDX" $ cRDD
         SET INDEX TO (cPath + "TEST_TABLE_RDD_" + cRDD)
      Else
         SET INDEX TO (cPath + "TEST_TABLE_RDD_IND01" )
         SET INDEX TO (cPath + "TEST_TABLE_RDD_IND02" ) ADDITIVE
         SET INDEX TO (cPath + "TEST_TABLE_RDD_IND03" ) ADDITIVE
      EndIf

      For j = 1 to 2

         ? "Starting pass:", j, "Elapsed time:", ((seconds()*100) - s)/100
         DoTest( j )

      Next

      USE

   Next

   ? " performing in ", ((seconds()*100) - s)/100

Return NIL

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/

Function DoTest( nLoop )

   Private lError := .F.
   Private s := seconds() * 100

   // DBGOTOP() / DBGOBOTTOM() test

   dbGotop()
   ChekStatusFlags3()

   dbGoBottom()
   ChekStatusFlags4()

   dbGotop()
   ChekStatusFlags3()

   dbGoBottom()
   ChekStatusFlags4()


   // DBGOTO() test

   DbGoTo( 1 )
   ChekStatusFlags1(1)

   DbGoTo( 2 )
   ChekStatusFlags1(2)

   DbGoTo( 3 )
   ChekStatusFlags1(3)

   DbGoTo( 900 )
   ChekStatusFlags1(900)

   DbGoTo( 901 )
   ChekStatusFlags1(901)

   DbGoTo( 911 )
   ChekStatusFlags1(911)

   DbGoTo( 921 )
   ChekStatusFlags1(921)

   DbSkip( 1 )
   ChekStatusFlags1(922)

   DbGoTo( RECORDS_IN_TEST + 1 )

   DbSkip( -1 )
   ChekStatusFlags1( RECORDS_IN_TEST )

   DbGoTo( RECORDS_IN_TEST + 1 )
   ChekStatusFlags2()

   DbGoTo( 0 )
   ChekStatusFlags2()

   DbGoTo( RECORDS_IN_TEST )
   ChekStatusFlags1(RECORDS_IN_TEST)


   // SKIP permormance test

   For i = 1 to fcount() - 7
      xVal := FieldGet(i)
   Next

   Set order to 1

   dbGoTop()

   For i = 1 to RECORDS_IN_TEST - 2

      dbSkip()

      if recno() != i + 1
         ? "Skip para o registro errado()", i + 1, recno(), code_id
         lError := .t.
      endif

      if recno() != val( code_id )
         ? "Erro em dbSkip()", i, recno(), code_id
         lError := .t.
      endif
      if bof()
         ? "Erro - nao deveria estar em BOF()", recno()
         lError := .t.
      endif
      if eof()
         ? "Erro - nao deveria estar em EOF()", recno()
         lError := .t.
      endif

      if lError
         WAIT
         exit
      endif
   Next

   dbGoBottom()

   For i = RECORDS_IN_TEST to 2 STEP -1

      dbSkip(-1)
      if recno() != i - 1
         ? "Skip para o registro errado", i - 1, recno(), code_id
         lError := .t.
      endif

      if recno() != val( code_id )
         ? "Erro em dbSkip()", i, recno(), code_id
         lError := .t.
      endif
      if bof()
         ? "Erro - nao deveria estar em BOF()", recno()
         lError := .t.
      endif
      if eof()
         ? "Erro - nao deveria estar em EOF()", recno()
         lError := .t.
      endif

      if lError
         WAIT
         exit
      endif
   Next

   Set order to 0  // Natural order

   dbGoTop()

   For i = 1 to RECORDS_IN_TEST - 2

      dbSkip()

      if recno() != i + 1
         ? "Skip para o registro errado()", i + 1, recno(), code_id
         lError := .t.
      endif

      if recno() != val( code_id )
         ? "Erro em dbSkip()", i, recno(), code_id
         lError := .t.
      endif
      if bof()
         ? "Erro - nao deveria estar em BOF()", recno()
         lError := .t.
      endif
      if eof()
         ? "Erro - nao deveria estar em EOF()", recno()
         lError := .t.
      endif

      if lError
         WAIT
         exit
      endif
   Next

   dbGoBottom()

   For i = RECORDS_IN_TEST to 2 STEP -1
      dbSkip(-1)
      if recno() != i - 1
         ? "Skip para o registro errado()", i - 1, recno(), code_id
         lError := .t.
      endif

      if recno() != val( code_id )
         ? "Erro em dbSkip()", i, recno(), code_id
         lError := .t.
      endif
      if bof()
         ? "Erro - nao deveria estar em BOF()", recno()
         lError := .t.
      endif
      if eof()
         ? "Erro - nao deveria estar em EOF()", recno()
         lError := .t.
      endif

      if lError
         WAIT
         exit
      endif
   Next

   // Seek test

   SET ORDER TO 1

   For i = 1 to RECORDS_IN_TEST

      If !dbSeek( strZero( i, 5 ) )
         ? "dbSeek() deveria retornar .t."
         lError := .t.
      endif

      If !found()
         ? "found() deveria retornar .t."
         lError := .t.
      endif

      If eof()
         ? "não deveria estar em eof()"
         lError := .t.
      endif

      If bof()
         ? "não deveria estar em bof()"
         lError := .t.
      endif

      if recno() != i
         ? "Posição de registro errado", i, recno(), code_id
         lError := .t.
      endif

      if code_id != strZero( i, 5 )
         ? "Posição de registro errado", i, recno(), code_id
         lError := .t.
      endif

      if lError
         WAIT
         exit
      endif

   Next

   // Replace test

   dbGoTop()

   For i = 1 to RECORDS_IN_TEST - 2

      REPLACE VALUE WITH nLoop + i

      dbSkip()

      if recno() != i + 1
         ? "Skip para o registro errado após update", i + 1, recno(), code_id
         lError := .t.
      endif

      if recno() != val( code_id )
         ? "Erro em dbSkip() apos update", i, recno(), code_id
         lError := .t.
      endif
      if bof()
         ? "Erro - nao deveria estar em BOF() apos update", recno()
         lError := .t.
      endif
      if eof()
         ? "Erro - nao deveria estar em EOF() apos update", recno()
         lError := .t.
      endif

      if lError
         WAIT
         exit
      endif
   Next

   // Reverse Index test

   Set order to 3

   dbGoBottom()

   For i = 1 to RECORDS_IN_TEST - 2

      dbSkip(-1)

      if recno() != i + 1
         ? "Skip Reverse para o registro errado()", i + 1, recno(), code_id
         lError := .t.
      endif

      if recno() != val( code_id )
         ? "Erro em dbSkip(-1) on reverse index", i, recno(), code_id
         lError := .t.
      endif
      if bof()
         ? "Erro - nao deveria estar em BOF()", recno()
         lError := .t.
      endif
      if eof()
         ? "Erro - nao deveria estar em EOF()", recno()
         lError := .t.
      endif

      if lError
         WAIT
         exit
      endif
   Next

   Set Order to 1

Return ((seconds()*100) - s)/100

/*------------------------------------------------------------------------*/

Function ChekStatusFlags1( nExpected )    // Check navigation not hitting TOP or BOTTOM

   Local lError := .F.

   if recno() != nExpected
      ? "Error: recno() in ChekStatusFlags1 is not the expected", recno(), nExpected
      lError := .t.
   endif

   if recno() != val( code_id )
      ? "Error: CODE_ID in ChekStatusFlags1 is not the expected", recno(), code_id
      lError := .t.
   endif
   if bof()
      ? "Error: Should not be at BOF() in ChekStatusFlags1", recno()
      lError := .t.
   endif
   if eof()
      ? "Error: Should not be at EOF() in ChekStatusFlags1", recno()
      lError := .t.
   endif

   if lError
      WAIT
   endif

Return lError

/*------------------------------------------------------------------------*/

Function ChekStatusFlags2()      // Check phantom record condition for invalid DBGOTO()

   Local lError := .F.

   if recno() != RECORDS_IN_TEST + 1
      ? "Error: Should be at phantom record", recno(), code_id
      lError := .t.
   endif
   if !bof()
      ? "Should be at BOF() with phantom record", recno()
      lError := .t.
   endif
   if !eof()
      ? "Should be at EOF() with phantom record", recno()
      lError := .t.
   endif

   if lError
      WAIT
   endif

Return lError

/*------------------------------------------------------------------------*/

Function ChekStatusFlags3()    // Check for TOP condition

   Local lError    := .F.
   Local nExpected := 1

   if recno() != nExpected
      ? "Error: Should be on first record, but is at", recno()
      lError := .t.
   endif

   if recno() != val( code_id )
      ? "Error: Invalid value for CODE_ID in first record:", code_id
      lError := .t.
   endif
   if bof()
      ? "Error: should not be in BOF() at top", recno()
      lError := .t.
   endif
   if eof()
      ? "Error: should not be in EOF() at top", recno()
      lError := .t.
   endif

   if lError
      WAIT
   endif

Return lError

/*------------------------------------------------------------------------*/

Function ChekStatusFlags4()    // Check for BOTTOM condition

   Local lError    := .F.
   Local nExpected := RECORDS_IN_TEST

   if recno() != nExpected
      ? "Error: Should be at last record, but is at", recno()
      lError := .t.
   endif

   if recno() != val( code_id )
      ? "Error: Invalid value for CODE_ID in last record:", code_id
      lError := .t.
   endif
   if bof()
      ? "Error: should not be in BOF() at bottom", recno()
      lError := .t.
   endif
   if eof()
      ? "Error: should not be in EOF() at bottom", recno()
      lError := .t.
   endif

   if lError
      WAIT
   endif

Return lError

