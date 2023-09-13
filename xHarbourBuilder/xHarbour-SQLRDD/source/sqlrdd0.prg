/* $CATEGORY$SQLRDD/Utils$FILES$sql.lib$
* SQLRDD Startup
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "error.ch"
#include "rddsys.ch"
#include "hbclass.ch"
#include "compat.ch"
#include "sqlrdd.ch"
#include "sqlodbc.ch"
#include "msg.ch"
#define HB_FALSE 0
#define HB_TRUE 1
#ifndef __XHARBOUR__
   REQUEST XHB_LIB
#endif
request HB_Deserialize,HB_Serialize
#define DEMO_NOTICE    "54686973206170706C69636174696F6E20776173206275696C64207769746820612064656D6F206F662053514C5244442C2064697374726962757465642062792078486172626F75722E636F6D2E0D0A0D0A457374612061706C69636163616F20E920666569746120636F6D20756D612076657273616F2044454D4F4E5354524143414F20646F2053514C5244442C2065207365752075736F20E9207065726D697469646F204150454E415320504152412046494E53204445204156414C4941C7C34F2E204120646973747269627569E7E36F20656D2070726F6475E7E36F2064657374612061706C696361E7E36F20E92070726F696269646120652073756A6569746120E0732070656E616C6964616465732070726576697374617320656D206C65692E204D61697320696E666F726D61E7F5657320656D207777772E78686172626F75722E636F6D2E62722E0D0A"

/* Need this modules linked */
REQUEST SR_WORKAREA

Static aConnections, nActiveConnection

Static lTblMgmnt        := .F.
Static EvalFilters      := .F.
Static RecnoName        := "SR_RECNO"
Static DeletedName      := "SR_DELETED"
Static cCollation       := ""
Static cToolsOwner      := ""
Static cIntenalID       := NIL
Static lFastOpenWA      := .T.
Static nMaxRowCache     := 1000
Static nFetchSize       := 10
Static lSyntheticInd    := .F.
Static cSynthetiVInd    := ""
Static lCheckMgmntInd   := .T.
Static cRDDTemp         := "DBFCDX"

Static cTblSpaceData    := ""
Static cTblSpaceIndx    := ""
Static cTblSpaceLob     := ""

Static hMultilangColumns
Static nSyntheticIndexMinimun      := 3

Static lErrorOnGotoToInvalidRecord := .F.

Static lUseNullsFirst              := .T.



/*------------------------------------------------------------------------*/

Procedure SR_Init

   #pragma TEXTHIDDEN(1)

   #ifdef SQLRDD_DEMO
      #ifdef __PLATFORM__Windows
         SR_BINDBYVALUE( hextostr(DEMO_NOTICE) )
      #else
         SQLBINDBYVAL( hextostr(DEMO_NOTICE) )
      #endif

   #endif

   #ifdef SQLRDD_TIMEBOMBED
      if date() > stod( "20090801" )
      #ifdef __PLATFORM__Windows
         SR_BINDBYVALUE( "SQLRDD Demo expired. Please contact your local resseler." )
         Quit
      #else
         SQLBINDBYVAL("SQLRDD Demo expired. Please contact your local resseler.")
         Quit
      #endif
         quit
      endif
   #endif

   #pragma TEXTHIDDEN(0)

Return

/*------------------------------------------------------------------------*/

Function SR_GetCnn( nConnection )
   DEFAULT nActiveConnection := 0
   DEFAULT nConnection := nActiveConnection
   If SR_CheckCnn( nConnection )
      DEFAULT aConnections  := {}
      Return aConnections[ nConnection ]
   EndIf
Return  NIL

/*------------------------------------------------------------------------*/

Function SR_CheckCnn( nConnection )
   DEFAULT nActiveConnection := 0
   DEFAULT nConnection := nActiveConnection
   DEFAULT aConnections  := {}
   If nConnection > len( aConnections ) .or. nConnection == 0
      Return .F.
   EndIf
Return .T.

/*------------------------------------------------------------------------*/

Function SR_GetConnection( nConnection )
   DEFAULT nActiveConnection := 0
   DEFAULT nConnection  := nActiveConnection
   DEFAULT aConnections := {}
   SR_CheckConnection( nConnection )
Return aConnections[ nConnection ]

/*------------------------------------------------------------------------*/

Function SR_CheckConnection( nConnection )
   DEFAULT nActiveConnection := 0
   DEFAULT nConnection := nActiveConnection
   DEFAULT aConnections  := {}
   If nConnection > len( aConnections ) .or. nConnection == 0 .or. nConnection < 0
      Return SR_RuntimeErr( "SR_CheckConnection()", SR_Msg(7) )
   EndIf
Return aConnections[ nConnection ]

/*------------------------------------------------------------------------*/

Function SR_SetNextQuery( cSql )
   local cOld

   DEFAULT aConnections  := {}
   DEFAULT nActiveConnection := 0

   SR_CheckConnection( nActiveConnection )
   cOld := aConnections[ nActiveConnection ]:cNextQuery

   If cSql != NIL
      aConnections[ nActiveConnection ]:cNextQuery := cSql
   EndIf

Return cOld

/*------------------------------------------------------------------------*/

Function SR_GetSyntheticIndexMinimun()

   Local nRet :=  nSyntheticIndexMinimun

   DEFAULT aConnections  := {}
   DEFAULT nActiveConnection := 0

   Switch aConnections[ nActiveConnection ]:nSystemID
   Case SYSTEMID_POSTGR
   Case SYSTEMID_ORACLE
      Exit
   DEFAULT
      nRet := 10
   End

Return nRet

/*------------------------------------------------------------------------*/

Function SR_SetSyntheticIndexMinimun(nSet)
   Local nOld := nSyntheticIndexMinimun
   If valtype(nSet) == "N"
      nSyntheticIndexMinimun := Min( nSet, 10 )
   EndIf
Return nOld

/*------------------------------------------------------------------------*/

Function SR_CheckMgmntInd(nSet)
   Local nOld := lCheckMgmntInd
   If valtype(nSet) == "L"
      lCheckMgmntInd := nSet
   EndIf
Return nOld

/*------------------------------------------------------------------------*/

Function SR_SetSyntheticIndex(lSet)
   Local lOld := lSyntheticInd
   If valtype(lSet) == "L"
      lSyntheticInd := lSet
   EndIf
Return lOld

/*------------------------------------------------------------------------*/

Function SR_GetSyntheticIndex()

Return lSyntheticInd

/*------------------------------------------------------------------------*/

Function SR_SetSVIndex(cSet)
   Local cOld := cSynthetiVInd
   If valtype(cSet) == "C"
      If len(cSet) != 3 .or. " " $ cSet .or. "." $ cSet
         SR_RuntimeErr( "SR_SetSVIndex()", "Invalid parameter: " + cSet )
      EndIf
      cSynthetiVInd := cSet
   EndIf
Return cOld

/*------------------------------------------------------------------------*/

Function SR_GetSVIndex()

   Local cRet     := cSynthetiVInd
   cSynthetiVInd  := ""

Return cRet

/*------------------------------------------------------------------------*/

Function SR_SetFastOpen( lSet )
   Local lOld := lFastOpenWA
   If valtype(lSet) == "L"
      lFastOpenWA := lSet
   EndIf
Return lOld

/*------------------------------------------------------------------------*/

Function SR_SetExclusiveManagement( lSet )
   Local lOld := !lFastOpenWA
   If valtype(lSet) == "L"
      lFastOpenWA := !lSet
   EndIf
Return lOld

/*------------------------------------------------------------------------*/

Function SR_SetTblSpaceData( cSet )
   Local cOld := cTblSpaceData
   Local oSql

   If valtype(cSet) == "C"
      cTblSpaceData := cSet
   ElseIf Empty( cTblSpaceData )
      oSql := SR_GetConnection()
      If !Empty( oSql:cDsnTblData )
         Return oSql:cDsnTblData
      EndIf
   EndIf

Return cOld

/*------------------------------------------------------------------------*/

Function SR_SetTblSpaceIndx( cSet )
   Local cOld := cTblSpaceIndx
   Local oSql

   If valtype(cSet) == "C"
      cTblSpaceIndx := cSet
   ElseIf Empty( cTblSpaceIndx )
      oSql := SR_GetConnection()
      If !Empty( oSql:cDsnTblIndx )
         Return oSql:cDsnTblIndx
      EndIf
   EndIf

Return cOld

/*------------------------------------------------------------------------*/

Function SR_SetTblSpaceLob( cSet )
   Local cOld := cTblSpaceLob
   Local oSql

   If valtype(cSet) == "C"
      cTblSpaceLob := cSet
   ElseIf Empty( cTblSpaceLob )
      oSql := SR_GetConnection()
      If !Empty( oSql:cDsnTblLob )
         Return oSql:cDsnTblLob
      EndIf
   EndIf

Return cOld

/*------------------------------------------------------------------------*/

Function SR_SetRDDTemp( cSet )
   Local cOld := cRDDTemp
   If valtype(cSet) == "C"
      cRDDTemp := cSet
   EndIf
Return cOld

/*------------------------------------------------------------------------*/

Function SR_GetFastOpen()

Return lFastOpenWA

/*------------------------------------------------------------------------*/

Function SR_GetActiveConnection()
   DEFAULT nActiveConnection := 0
Return nActiveConnection

/*------------------------------------------------------------------------*/

Function SR_SetActiveConnection( nCnn )
   Local nOld
   DEFAULT nActiveConnection := 0
   nOld := nActiveConnection
   DEFAULT nCnn := 1
   DEFAULT aConnections  := {}

   If nCnn != 0 .and. nCnn <= len( aConnections )
      nActiveConnection := nCnn
   Else
      Return -1
   EndIf
Return nOld

/*------------------------------------------------------------------------*/

Function SR_AddConnection( nType, cDSN, cUser, cPassword, cOwner, lCounter, lAutoCommit, lNoSetEnv, nTimeout )

   Local nRet := -1, oConnect, oConnect2

   DEFAULT nType        := CONNECT_ODBC
   DEFAULT lAutoCommit  := .F.
   DEFAULT lCounter     := .F.
   DEFAULT cOwner       := ""
   DEFAULT lNoSetEnv    := .F.
   DEFAULT aConnections  := {}
   DEFAULT nActiveConnection := 0

   #pragma TEXTHIDDEN(1)

   #ifdef SQLRDD_DEMO
      #ifdef __PLATFORM__Windows
         SR_BINDBYVALUE( hextostr(DEMO_NOTICE) )
      #else
         SQLBINDBYVAL( hextostr(DEMO_NOTICE) )
      #endif

   #endif

   #pragma TEXTHIDDEN(0)

   /* The macro execution is used to NOT link the connection class if we don't need it
      The programmer MUST declare the needed connection class using REQUEST in PRG source */

   Switch nType
   Case CONNECT_ODBC
   Case CONNECT_ODBC_NOEXLOCK
#ifndef MYSQLRDD
      oConnect  := &( "SR_ODBC()" )
      oConnect2 := &( "SR_ODBC()" )
#endif
      Exit
   Case CONNECT_MYSQL
   Case CONNECT_MYSQL_NOEXLOCK
      oConnect  := &( "SR_MYSQL()" )
      oConnect2 := &( "SR_MYSQL()" )
      Exit
   Case CONNECT_POSTGRES
   Case CONNECT_POSTGRES_NOEXLOCK
#ifndef MYSQLRDD
      oConnect  := &( "SR_PGS()" )
      oConnect2 := &( "SR_PGS()" )
#endif
      Exit
   Case CONNECT_ORACLE
   Case CONNECT_ORACLE_NOEXLOCK
#ifndef MYSQLRDD
      oConnect  := &( "SR_ORACLE()" )
      oConnect2 := &( "SR_ORACLE()" )
#endif
      Exit
   Case CONNECT_ORACLE2
   Case CONNECT_ORACLE2_NOEXLOCK
#ifndef MYSQLRDD
      oConnect  := &( "SR_ORACLE2()" )
      oConnect2 := &( "SR_ORACLE2()" )
#endif
      Exit

   Case CONNECT_FIREBIRD
   Case CONNECT_FIREBIRD_NOEXLOCK
#ifndef MYSQLRDD
      oConnect  := &( "SR_FIREBIRD()" )
      oConnect2 := &( "SR_FIREBIRD()" )
#endif
      Exit
   Case CONNECT_FIREBIRD3
   Case CONNECT_FIREBIRD3_NOEXLOCK
#ifndef MYSQLRDD
      oConnect  := &( "SR_FIREBIRD3()" )
      oConnect2 := &( "SR_FIREBIRD3()" )
#endif
      Exit

   Case CONNECT_MARIA
   Case CONNECT_MARIA_NOEXLOCK
#ifndef MYSQLRDD
      oConnect  := &( "SR_MARIA()" )
      oConnect2 := &( "SR_MARIA()" )
#endif
      Exit


   Case CONNECT_ODBC_QUERY_ONLY
#ifndef MYSQLRDD
      oConnect  := &( "SR_ODBC()" )
      oConnect:lQueryOnly := .T.
#endif
      Exit
   Case CONNECT_ORACLE_QUERY_ONLY
#ifndef MYSQLRDD
      oConnect  := &( "SR_ORACLE()" )
      oConnect:lQueryOnly := .T.
#endif
      Exit
   Case CONNECT_ORACLE2_QUERY_ONLY
#ifndef MYSQLRDD
      oConnect  := &( "SR_ORACLE2()" )
      oConnect:lQueryOnly := .T.
#endif
      Exit

   Case CONNECT_MYSQL_QUERY_ONLY
      oConnect  := &( "SR_MYSQL()" )
      oConnect:lQueryOnly := .T.
      Exit
   Case CONNECT_POSTGRES_QUERY_ONLY
#ifndef MYSQLRDD
      oConnect  := &( "SR_PGS()" )
      oConnect:lQueryOnly := .T.
#endif
      Exit
   Case CONNECT_FIREBIRD_QUERY_ONLY
#ifndef MYSQLRDD
      oConnect  := &( "SR_FIREBIRD()" )
      oConnect:lQueryOnly := .T.
#endif
      Exit
   Case CONNECT_MARIA_QUERY_ONLY
#ifndef MYSQLRDD
      oConnect  := &( "SR_MARIA()" )
      oConnect:lQueryOnly := .T.
#endif
      Exit

   DEFAULT
      SR_MsgLogFile( "Invalid connection type in SR_AddConnection() :" + str( nType ) )
      Return -1
   End

   If valtype( oConnect ) == "O"

      oConnect:Connect( "", cUser, cPassword, 1, cOwner, 4000, .F.,;
                     cDSN, 50, "ANSI", 0, 0, 0,;
                     lCounter, lAutoCommit, nTimeout )
   Else
      SR_MsgLogFile( "Invalid connection type in SR_AddConnection() :" + str( nType ) )
      Return -1
   EndIf

   If oConnect:nSystemID != 0 .and. oConnect:nSystemID != NIL

      oConnect:nConnectionType := nType


      /* Create other connections to the database */

      If nType < CONNECT_NOEXLOCK
         oConnect:oSqlTransact := oConnect2:Connect( "", cUser, cPassword, 1, cOwner, 4000, .F.,;
                                  cDSN, 50, "ANSI", 0, 0, 0, .T., lAutoCommit, nTimeout )
         oConnect2:nConnectionType := nType
      ElseIf nType < CONNECT_QUERY_ONLY
         lNoSetEnv := .F.
      Else
         lNoSetEnv := .T.
      EndIf

      // ToDo: Add MUTEX here
      aadd( aConnections, oConnect )
      nRet := len( aConnections )

      If nActiveConnection == NIL .or. nActiveConnection == 0
         nActiveConnection := nRet
      EndIf

      oConnect:nID := nRet

      If !lNoSetEnv
         If empty( SR_SetEnvSQLRDD( oConnect ) )
            Return -1
         EndIf
      Else
         If empty( SR_SetEnvMinimal( oConnect ) )
            Return -1
         EndIf
      EndIf

      SR_ReloadFieldModifiers( oConnect )

      If !( "DB2/400" $ oConnect:cSystemName )
         If !lAutoCommit
            oConnect:SetOptions( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF )
            If nType < CONNECT_QUERY_ONLY
               oConnect2:SetOptions( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF )
            EndIf
         EndIf
      Else
         oConnect:SetOptions( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON )
         If nType < CONNECT_QUERY_ONLY
            oConnect2:SetOptions( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON )
         EndIf
      EndIf

   EndIf

Return nRet

/*------------------------------------------------------------------------*/

Function SR_ReloadFieldModifiers( oConnect )

#ifdef SQLRDD_TOPCONN

   Local aRet := {}, aFlds := {}, aField, cLast

   If oConnect:exec( "SELECT FIELD_TABLE, FIELD_NAME, FIELD_TYPE, FIELD_PREC, FIELD_DEC FROM TOP_FIELD WHERE FIELD_TYPE != 'X' ORDER BY FIELD_TABLE, FIELD_NAME", .F., .T., @aRet ) == SQL_SUCCESS
      oConnect:aFieldModifier := { => }
      HAllocate( oConnect:aFieldModifier, 10000 )
      oConnect:nTCCompat := 2
      If len( aRet ) > 0
         cLast := aRet[1,1]
         For each aField in aRet
            If aField[1] != cLast
               If "." $ cLast
                  cLast := Upper(SubSTr( cLast, At( ".", cLast ) + 1 ))
               EndIf
               oConnect:aFieldModifier[alltrim(cLast)] := aFlds
               aFlds := {}
               cLast := aField[1]
            EndIf
            aadd( aFlds, { aField[2], aField[3], aField[4], aField[5] } )
         Next
         If "." $ cLast
            cLast := Upper(SubSTr( cLast, At( ".", cLast ) + 1 ))
         EndIf
         oConnect:aFieldModifier[alltrim(cLast)] := aFlds
      EndIf
   Else
      oConnect:Commit()
   EndIf
#else
   (oConnect)
#endif

Return NIL

/*------------------------------------------------------------------------*/

Static Function SR_SetEnvSQLRDD( oConnect )

   Local aRet := {}, cRet := "", i, oCnn, cStartingVersion
   Local cSql, lOld

   For i = 1 to 2

      If i == 1
         oCnn := oConnect
      ElseIf i == 2
         If oConnect:oSqlTransact == NIL
            Exit
         EndIf
         oCnn := oConnect:oSqlTransact
      EndIf

      Switch oCnn:nSystemID
      Case SYSTEMID_ORACLE
         If SR_UseSequences() .and. i == 1
            aRet := {}
            oCnn:exec( "SELECT SEQUENCE_NAME FROM USER_SEQUENCES WHERE SEQUENCE_NAME='SQ_NRECNO'",.F.,.T.,@aRet )
         EndIf
         oCnn:exec( "ALTER SESSION SET NLS_LANGUAGE=AMERICAN",.F. )
         oCnn:exec( "ALTER SESSION SET NLS_SORT=BINARY",.F. )
         oCnn:exec( "ALTER SESSION SET NLS_NUMERIC_CHARACTERS='.,'",.F. )
         oCnn:exec( "alter session set SESSION_CACHED_CURSORS=10000",.F. )
         If oCnn:cCharSet != NIL
            oCnn:exec( "ALTER SESSION SET NLS_CHARACTERSET="+oCnn:cCharSet ,.F. )
         EndIf
         If oCnn:lNative
            oCnn:exec( "ALTER SESSION SET NLS_DATE_FORMAT='yyyymmdd'",.F. )
            oCnn:exec( "ALTER SESSION SET NLS_TIMESTAMP_FORMAT='yyyymmdd HH.MI.SSXFF AM'",.F. )
         EndIf
         /* Locking system housekeeping */

         aRet := {}
         oCnn:exec( "select sid from " + If(oCnn:lCluster, "g", "" ) + "v$session where AUDSID = sys_context('USERENV','sessionid')", .T., .T., @aRet )

         If len( aRet ) > 0
            oCnn:uSid := val(str(aRet[1,1],8,0))
         EndIf

         oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + [SR_MGMNTLOCKS WHERE SPID_ = ] + str( oCnn:uSid ) + [ OR SPID_ NOT IN (select "AUDSID" from ] + If(oCnn:lCluster, "g", "" ) + [v$session)], .F. )
         oCnn:Commit()
         Exit

      Case SYSTEMID_INGRES
         oCnn:Commit()
         oCnn:exec( "set lockmode session where readlock=nolock,level=row" )
         Exit

      Case SYSTEMID_IBMDB2
         /*
         If SR_UseSequences() .and. i == 1
            aRet := {}
            oCnn:exec( "VALUES NEXTVAL FOR N_RECNO",.F.,.T.,@aRet )
            If len(aRet) == 0
               oCnn:exec( "CREATE SEQUENCE N_RECNO START WITH 1 INCREMENT BY 1 NOMAXVALUE NOCYCLE" )
            EndIf
         EndIf
         */
         Exit

      Case SYSTEMID_SYBASE
//         oCnn:commit()
//         oCnn:exec( "SET CHAINED ON" )
         oCnn:commit()
         oCnn:exec( "SET QUOTED_IDENTIFIER ON" )
         oCnn:exec( "SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED" )
         Exit

      Case SYSTEMID_MSSQL7
      Case SYSTEMID_MSSQL6
      Case SYSTEMID_AZURE
         oCnn:Commit()
         oCnn:exec( "SET QUOTED_IDENTIFIER ON" )
         oCnn:exec( "SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED" )
         /* Locking system housekeeping */
         aRet := {}
         oCnn:exec( "SELECT convert( char(30), login_time, 21 ) FROM MASTER.DBO.SYSPROCESSES where SPID = @@SPID",.F.,.T.,@aRet )

         If len( aRet ) > 0
            oCnn:cLoginTime   := alltrim(aRet[1,1])
         EndIf

         oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLOCKS WHERE SPID_ = @@SPID OR convert( CHAR(10), SPID_ ) + convert( CHAR(23), LOGIN_TIME_, 21 ) NOT IN (SELECT convert( CHAR(10), SPID) + CONVERT( CHAR(23), LOGIN_TIME, 21 ) FROM MASTER.DBO.SYSPROCESSES)", .F. )
         oCnn:Commit()
         Exit

      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
         oCnn:exec( "set session autocommit=0;" )
         oCnn:exec( "set session sql_mode = 'PIPES_AS_CONCAT'" )
         oCnn:exec( "SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED" )
         Exit

      Case SYSTEMID_POSTGR
         If SR_UseSequences() .and. i == 1
            aRet := {}
            oCnn:exec( "SELECT * FROM SQ_NRECNO",.F.,.T.,@aRet )
            oCnn:Commit()
         EndIf
         oCnn:exec( "SET CLIENT_ENCODING to 'LATIN1'",.F.,.T.,@aRet )
         oCnn:exec( "SET xmloption to 'DOCUMENT'",.F.,.T.,@aRet )
         /* Locking system housekeeping */
         oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLOCKS WHERE SPID_ = (select pg_backend_pid()) OR SPID_ NOT IN (select pg_stat_get_backend_pid(pg_stat_get_backend_idset()))", .F. )
         oCnn:Commit()
         Exit

      Case SYSTEMID_FIREBR
      Case SYSTEMID_FIREBR3
//         oCnn:exec( "SET TERM !@¨§;", .f. )
         oCnn:Commit()
         Exit

      Case SYSTEMID_CACHE
         oCnn:exec( "SET OPTION SUPPORT_DELIMITED_IDENTIFIERS=TRUE" )

         aRet := {}
         oCnn:exec( "select * from " + SR_GetToolsOwner() + "dual",.F.,.T.,@aRet )
         if len(aRet) == 0
            oConnect:exec( "create table " + SR_GetToolsOwner() + "dual (dummy char(1))",.F. )
            oConnect:exec( "insert into " + SR_GetToolsOwner() + "dual (dummy) values (0)",.F. )
            oConnect:commit()

            cSql := "create function " + SR_GetToolsOwner() + e"NEXTVAL(sequenceName VARCHAR(50))\r\n for " + SR_GetToolsOwner() + e"SequenceControler \r\n"
            cSql += e"returns INT \r\n LANGUAGE OBJECTSCRIPT \r\n {\r\n New nextVal \r\n Set nextVal = $Increment(^" + SR_GetToolsOwner() + 'SequenceControler("Sequences",sequenceName))' + e"\r\n"
            cSql += e"Set ^CacheTemp.SequenceControler($Job,sequenceName)=nextVal \r\n Quit nextVal \r\n }"

            oConnect:exec( cSql,.T. )
            oConnect:commit()

            cSql := "create function " + SR_GetToolsOwner() + e"CURRVAL(sequenceName VARCHAR(50))\r\n for " + SR_GetToolsOwner() + e"SequenceControler \r\n  returns INT \r\n LANGUAGE OBJECTSCRIPT \r\n  { \r\n  Quit $Get(^CacheTemp.SequenceControler($Job,sequenceName)) \r\n  }"

            oConnect:exec( cSql,.T. )
            oConnect:commit()

            cSql := "create procedure " + SR_GetToolsOwner() + e"RESET(sequenceName VARCHAR(50))\r\n for " + SR_GetToolsOwner() + e"SequenceControler \r\n LANGUAGE OBJECTSCRIPT \r\n  { \r\n  Kill ^" + SR_GetToolsOwner() + 'SequenceControler("Sequences",sequenceName)' + e" \r\n  }"

            oConnect:exec( cSql,.T. )
            oConnect:commit()

            cSql := "create function " + SR_GetToolsOwner() + e"JOB()\r\n for " + SR_GetToolsOwner() + e"JOB \r\n  returns INT \r\n LANGUAGE OBJECTSCRIPT \r\n  { \r\n  Quit $Job \r\n  }"
            oConnect:exec( cSql,.T. )
            oConnect:commit()

            cSql := "create function " + SR_GetToolsOwner() + e"JOBLIST()\r\n for " + SR_GetToolsOwner() + e"JOB \r\n  returns %String \r\n LANGUAGE OBJECTSCRIPT \r\n  { \r\n"
            cSql += [Set lista=""] + e"\r\n Do \r\n { \r\n" + [Set rs=##class(%ResultSet).%New("%SYSTEM.Process:CONTROLPANEL")] + e"\r\n" + [Set ok=rs.Execute("")] + e"\r\n"
            cSql += e"If 'ok Quit\r\n \r\n Set i=1\r\n  While rs.Next() \r\n{\r\n Set pid=rs.GetData(2) \r\n"
            cSql += [If pid>0 Set $Piece(lista,",",i)=pid] + e"\r\n Set i=i+1 \r\n } \r\n Do rs.Close() \r\n } While 0 \r\n"
            cSql += e"\r\n  Quit lista\r\n}\r\n"

            oConnect:exec( cSql,.T. )
            oConnect:commit()

         EndIf

         oCnn:Commit()
         Exit

      End
   Next

   /* check for the control tables */
   aRet := {}
   oConnect:exec( "SELECT VERSION_, SIGNATURE_ FROM " + SR_GetToolsOwner() + "SR_MGMNTVERSION",.F.,.T.,@aRet )

   If len(aRet) > 0
      cStartingVersion := alltrim(aRet[1,1])
   Else
      cStartingVersion := ""
   EndIf

   If cStartingVersion < "MGMNT 1.02"
      /* Only use BASIC types and commands to be 100% darabase independent */
      oConnect:commit()
      oConnect:exec( "DROP TABLE " + SR_GetToolsOwner() + "SR_MGMNTVERSION",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTVERSION (VERSION_ CHAR(20), SIGNATURE_ CHAR(20))",.F. )
      oConnect:commit()

      If oConnect:nSystemID == SYSTEMID_AZURE

         oConnect:exec( "CREATE CLUSTERED INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTVERSION01 ON " + SR_GetToolsOwner() + "SR_MGMNTVERSION ( VERSION_ )",.F. )
         oConnect:commit()

      Endif

      oConnect:exec( "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTVERSION (VERSION_, SIGNATURE_) VALUES ('" + HB_SR__MGMNT_VERSION + "', '" + DTOS(DATE()) + " " + TIME() + "')" ,.T. )
      oConnect:commit()
      oConnect:exec( "DROP TABLE " + SR_GetToolsOwner() + "SR_MGMNTINDEXES",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTINDEXES (TABLE_ CHAR(50), SIGNATURE_ CHAR(20), IDXNAME_ CHAR(64), PHIS_NAME_ CHAR(64), IDXKEY_ VARCHAR(254), IDXFOR_ VARCHAR(254), IDXCOL_ CHAR(3), TAG_ CHAR(30), TAGNUM_ CHAR(6) )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE " + If(oConnect:nSystemID == SYSTEMID_AZURE," CLUSTERED " ," ") + " INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTINDEX01 ON " + SR_GetToolsOwner() + "SR_MGMNTINDEXES ( TABLE_ )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTINDEX02 ON " + SR_GetToolsOwner() + "SR_MGMNTINDEXES ( IDXNAME_ )",.F. )
      oConnect:commit()
      oConnect:exec( "DROP TABLE " + SR_GetToolsOwner() + "SR_MGMNTTABLES",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTTABLES (TABLE_ CHAR(50), SIGNATURE_ CHAR(20), CREATED_ CHAR(20), TYPE_ CHAR(30), REGINFO_ CHAR(15))",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE " + If(oConnect:nSystemID == SYSTEMID_AZURE," CLUSTERED " ," ") + " INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTTABLES01 ON " + SR_GetToolsOwner() + "SR_MGMNTTABLES ( TABLE_ )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS ( SOURCETABLE_ CHAR(50), TARGETTABLE_ CHAR(50), CONSTRNAME_ CHAR(50), CONSTRTYPE_ CHAR(2) )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE " + If(oConnect:nSystemID == SYSTEMID_AZURE," CLUSTERED " ," ") + " INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTCONSTRAINTS01 ON " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS ( SOURCETABLE_, CONSTRNAME_ )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTCONSTRSRCCOLS ( SOURCETABLE_ CHAR(50), CONSTRNAME_ CHAR(50), ORDER_ CHAR(02), SOURCECOLUMN_ CHAR(50) )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE " + If(oConnect:nSystemID == SYSTEMID_AZURE," CLUSTERED " ," ") + " INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTCONSTRSRCCOLS01 ON " + SR_GetToolsOwner() + "SR_MGMNTCONSTRSRCCOLS ( SOURCETABLE_, CONSTRNAME_, ORDER_ )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTCONSTRTGTCOLS ( SOURCETABLE_ CHAR(50), CONSTRNAME_ CHAR(50), ORDER_ CHAR(02), TARGETCOLUMN_ CHAR(50) )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE " + If(oConnect:nSystemID == SYSTEMID_AZURE," CLUSTERED " ," ") + " INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTCONSTRTGTCOLS01 ON " + SR_GetToolsOwner() + "SR_MGMNTCONSTRTGTCOLS ( SOURCETABLE_, CONSTRNAME_, ORDER_ )",.F. )

      // Caché - should add dual table ,like Oracle

      cRet := HB_SR__MGMNT_VERSION

   ElseIf cStartingVersion < "MGMNT 1.03"

      oConnect:commit()
      oConnect:exec( "DROP TABLE " + SR_GetToolsOwner() + "SR_MGMNTTABLES",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTTABLES (TABLE_ CHAR(50), SIGNATURE_ CHAR(20), CREATED_ CHAR(20), TYPE_ CHAR(30), REGINFO_ CHAR(15))",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTTABLES01 ON " + SR_GetToolsOwner() + "SR_MGMNTTABLES ( TABLE_ )",.F. )
      oConnect:commit()
      oConnect:exec( "UPDATE " + SR_GetToolsOwner() + "SR_MGMNTVERSION SET VERSION_ = '" + HB_SR__MGMNT_VERSION + "'" )
      oConnect:commit()

      cRet := HB_SR__MGMNT_VERSION
   Else
      cRet := aRet[1,1]
   EndIf

   If cStartingVersion < "MGMNT 1.65"

      oConnect:commit()
      oConnect:exec( "DROP TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOCKS",.F. )
      oConnect:commit()
      oConnect:exec( "DROP TABLE " + SR_GetToolsOwner() + "SR_MGMNTLTABLES",.F. )   // Table REMOVED from SQLRDD catalogs
      oConnect:commit()

      Switch oConnect:nSystemID
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_AZURE
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOCKS (LOCK_ CHAR(250) NOT NULL UNIQUE, WSID_ CHAR(250) NOT NULL, SPID_ NUMERIC(6), LOGIN_TIME_ DATETIME )",.F. )
         Exit
      Case SYSTEMID_POSTGR
      Case SYSTEMID_SYBASE
      Case SYSTEMID_CACHE
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOCKS (LOCK_ CHAR(250) NOT NULL UNIQUE, WSID_ CHAR(250) NOT NULL, SPID_ NUMERIC(6), LOGIN_TIME_ TIMESTAMP )",.F. )
         Exit
      Case SYSTEMID_ORACLE
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOCKS (LOCK_ CHAR(250) NOT NULL UNIQUE, WSID_ CHAR(250) NOT NULL, SPID_ NUMBER(8), LOGIN_TIME_ TIMESTAMP )",.F. )
         Exit
      Case SYSTEMID_IBMDB2
      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
      Case SYSTEMID_ADABAS
      Case SYSTEMID_INGRES
      Case SYSTEMID_INFORM
      Case SYSTEMID_FIREBR
      Case SYSTEMID_FIREBR3
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOCKS (LOCK_ CHAR(250) NOT NULL UNIQUE, WSID_ CHAR(250) NOT NULL, SPID_ DECIMAL(8), LOGIN_TIME_ TIMESTAMP )",.F. )
         Exit
      DEFAULT
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOCKS (LOCK_ CHAR(250) NOT NULL UNIQUE, WSID_ CHAR(250) NOT NULL, SPID_ CHAR(10), LOGIN_TIME_ CHAR(50))",.F. )
      End

      oConnect:commit()
      oConnect:exec( "CREATE " + If(oConnect:nSystemID == SYSTEMID_AZURE," CLUSTERED " ," ") + " INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTLOCKS01 ON " + SR_GetToolsOwner() + "SR_MGMNTLOCKS ( LOCK_, WSID_ )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTLOCKS02 ON " + SR_GetToolsOwner() + "SR_MGMNTLOCKS ( WSID_, LOCK_ )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTLOCKS03 ON " + SR_GetToolsOwner() + "SR_MGMNTLOCKS ( SPID_ )",.F. )
      oConnect:commit()

      oConnect:exec( "UPDATE " + SR_GetToolsOwner() + "SR_MGMNTVERSION SET VERSION_ = '" + HB_SR__MGMNT_VERSION + "'" )
      oConnect:commit()

      cRet := HB_SR__MGMNT_VERSION
   EndIf

   If cStartingVersion < "MGMNT 1.50"

      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLANG ( TABLE_ CHAR(50), COLUMN_ CHAR(50), TYPE_ CHAR(1), LEN_ CHAR(8), DEC_ CHAR(8) )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE " + If(oConnect:nSystemID == SYSTEMID_AZURE," CLUSTERED " ," ") + " INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTLANG01 ON " + SR_GetToolsOwner() + "SR_MGMNTLANG ( TABLE_, COLUMN_ )",.F. )
      oConnect:commit()

      oConnect:exec( "UPDATE " + SR_GetToolsOwner() + "SR_MGMNTVERSION SET VERSION_ = '" + HB_SR__MGMNT_VERSION + "'" )
      oConnect:commit()

      cRet := HB_SR__MGMNT_VERSION
   EndIf

   If cStartingVersion < "MGMNT 1.60"

      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS ( SOURCETABLE_ CHAR(50), TARGETTABLE_ CHAR(50), CONSTRNAME_ CHAR(50), CONSTRTYPE_ CHAR(2) )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTCONSTRAINTS01 ON " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS ( SOURCETABLE_, CONSTRNAME_ )",.F. )

      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTCONSTRSRCCOLS ( SOURCETABLE_ CHAR(50), CONSTRNAME_ CHAR(50), ORDER_ CHAR(02), SOURCECOLUMN_ CHAR(50) )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTCONSTRSRCCOLS01 ON " + SR_GetToolsOwner() + "SR_MGMNTCONSTRSRCCOLS ( SOURCETABLE_, CONSTRNAME_, ORDER_ )",.F. )

      oConnect:commit()
      oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTCONSTRTGTCOLS ( SOURCETABLE_ CHAR(50), CONSTRNAME_ CHAR(50), ORDER_ CHAR(02), TARGETCOLUMN_ CHAR(50) )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTCONSTRTGTCOLS01 ON " + SR_GetToolsOwner() + "SR_MGMNTCONSTRTGTCOLS ( SOURCETABLE_, CONSTRNAME_, ORDER_ )",.F. )

      oConnect:commit()
      oConnect:exec( "UPDATE " + SR_GetToolsOwner() + "SR_MGMNTVERSION SET VERSION_ = '" + HB_SR__MGMNT_VERSION + "'" )
      oConnect:commit()

      cRet := HB_SR__MGMNT_VERSION

   EndIf

   If cStartingVersion < "MGMNT 1.67"
      If oConnect:nSystemID == SYSTEMID_MSSQL7 .or. oConnect:nSystemID == SYSTEMID_AZURE
         oConnect:exec( "DROP FUNCTION dbo.trim", .F. )
         oConnect:commit()
         oConnect:exec( "CREATE FUNCTION dbo.trim( @p1 AS CHAR  ) RETURNS CHAR BEGIN RETURN ltrim(rtrim( @p1 )) END", .F. )
         oConnect:commit()
         oConnect:exec( "UPDATE " + SR_GetToolsOwner() + "SR_MGMNTVERSION SET VERSION_ = '" + HB_SR__MGMNT_VERSION + "'" )
         oConnect:commit()
      EndIf
      cRet := HB_SR__MGMNT_VERSION
   EndIf

   // MISSING: Add columns to SR_MGMNTINDEXES ( ordfor(), ordkey() )

   If cStartingVersion < "MGMNT 1.72"

      lOld := SR_UseDeleteds( .F. )

      dbCreate( "SR_MGMNTLOGCHG", { { "SPID_",        "N", 12, 0 },;
                                    { "WPID_",        "N", 12, 0 },;
                                    { "TYPE_",        "C",  2, 0 },;
                                    { "APPUSER_",     "C", 50, 0 },;
                                    { "TIME_",        "C", 25, 0 },;
                                    { "QUERY_",       "M", 10, 0 },;
                                    { "CALLSTACK_",   "C", 1000, 0 },;
                                    { "SITE_",        "C", 20, 0 },;
                                    { "CONTROL_",     "C", 50, 0 },;
                                    { "COST_",        "N", 12, 0 } }, "SQLRDD" )

      USE SR_MGMNTLOGCHG EXCLUSIVE VIA "SQLRDD" NEW
//      ordCreate( "SR_MGMNTLOGCHG", "001", "SPID_ + WPID_" )
//      ordCreate( "SR_MGMNTLOGCHG", "002", "APPUSER_ + WPID_" )
//      ordCreate( "SR_MGMNTLOGCHG", "003", "TIME_ + SITE_" )
//      ordCreate( "SR_MGMNTLOGCHG", "004", "TYPE_ + SPID_" )
      USE

      SR_UseDeleteds( lOld )

/*

      Switch oConnect:nSystemID
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_SYBASE
      Case SYSTEMID_CACHE
      Case SYSTEMID_POSTGR
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG (SPID_ NUMERIC(12) NOT NULL, WPID_ NUMERIC(12), TYPE_ CHAR(2), APPUSER_ CHAR(50), TIME_ CHAR(16), QUERY_ TEXT, CALLSTACK_ TEXT, SITE_ CHAR(10), FREE1_ CHAR(50) )",.F. )
         Exit
      Case SYSTEMID_ORACLE
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG (SPID_ NUMERIC(12) NOT NULL, WPID_ NUMERIC(12), TYPE_ CHAR(2), APPUSER_ CHAR(50), TIME_ CHAR(16), QUERY_ TEXT, CALLSTACK_ TEXT, SITE_ CHAR(10), FREE1_ CHAR(50) )",.F. )
         Exit
      Case SYSTEMID_IBMDB2
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG (SPID_ DECIMAL(12) NOT NULL, WPID_ DECIMAL(12), TYPE_ CHAR(2), APPUSER_ CHAR(50), TIME_ CHAR(16), QUERY_ CLOB (64000) " + If( "DB2/400" $ oCOnnect:cSystemName, "",  " NOT LOGGED COMPACT") + ", CALLSTACK_ CLOB (4000) " + If( "DB2/400" $ oCOnnect:cSystemName, "",  " NOT LOGGED COMPACT") + ", SITE_ CHAR(10), FREE1_ CHAR(50) )",.F. )
         Exit
      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG (SPID_ BIGINT(12) NOT NULL, WPID_ BIGINT(12), TYPE_ CHAR(2), APPUSER_ CHAR(50), TIME_ CHAR(16), QUERY_ MEDIUMBLOB, CALLSTACK_ MEDIUMBLOB, SITE_ CHAR(10), FREE1_ CHAR(50) )",.F. )
         Exit
      Case SYSTEMID_ADABAS
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG (SPID_ BIGINT(12) NOT NULL, WPID_ BIGINT(12), TYPE_ CHAR(2), APPUSER_ CHAR(50), TIME_ CHAR(16), QUERY_ LONG, CALLSTACK_ LONG, SITE_ CHAR(10), FREE1_ CHAR(50) )",.F. )
         Exit
      Case SYSTEMID_INGRES
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG (SPID_ BIGINT(12) NOT NULL, WPID_ BIGINT(12), TYPE_ CHAR(2), APPUSER_ CHAR(50), TIME_ CHAR(16), QUERY_ long varchar, CALLSTACK_ long varchar, SITE_ CHAR(10), FREE1_ CHAR(50) )",.F. )
         Exit
      Case SYSTEMID_INFORM
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG (SPID_ DECIMAL(12) NOT NULL, WPID_ DECIMAL(12), TYPE_ CHAR(2), APPUSER_ CHAR(50), TIME_ CHAR(16), QUERY_ TEXT, CALLSTACK_ TEXT, SITE_ CHAR(10), FREE1_ CHAR(50) )",.F. )
         Exit
      Case SYSTEMID_FIREBR
         oConnect:exec( "CREATE TABLE " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG (SPID_ DECIMAL(12) NOT NULL, WPID_ DECIMAL(12), TYPE_ CHAR(2), APPUSER_ CHAR(50), TIME_ CHAR(16), QUERY_ BLOB SUB_TYPE 1, CALLSTACK_ BLOB SUB_TYPE 1, SITE_ CHAR(10), FREE1_ CHAR(50) )",.F. )
         Exit
      End

      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTLOGCHG01 ON " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG ( SPID_, WPID_ )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTLOGCHG02 ON " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG ( APPUSER_, SPID_ )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTLOGCHG03 ON " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG ( TIME_, SITE_ )",.F. )
      oConnect:commit()
      oConnect:exec( "CREATE INDEX " + if( oConnect:nSystemID != SYSTEMID_ORACLE, "", SR_GetToolsOwner() ) + "SR_MGMNTLOGCHG04 ON " + SR_GetToolsOwner() + "SR_MGMNTLOGCHG ( TYPE_, TIME_ )",.F. )
      oConnect:commit()

*/

      oConnect:exec( "UPDATE " + SR_GetToolsOwner() + "SR_MGMNTVERSION SET VERSION_ = '" + HB_SR__MGMNT_VERSION + "'" )
      oConnect:commit()

      cRet := HB_SR__MGMNT_VERSION

   EndIf

   oConnect:cMgmntVers := cRet

   // Setup multilang hash

   SR_ReloadMLHash( oConnect )

Return cRet

/*------------------------------------------------------------------------*/

Static Function SR_SetEnvMinimal( oConnect )

   Local aRet := {}, cRet := "0", oCnn

   oCnn := oConnect

   Switch oCnn:nSystemID
   Case SYSTEMID_ORACLE
      oCnn:exec( "ALTER SESSION SET NLS_LANGUAGE=AMERICAN",.F. )
      oCnn:exec( "ALTER SESSION SET NLS_SORT=BINARY",.F. )
      oCnn:exec( "ALTER SESSION SET NLS_NUMERIC_CHARACTERS='.,'",.F. )
      If oCnn:cCharSet != NIL
         oCnn:exec( "ALTER SESSION SET NLS_CHARACTERSET=" + oCnn:cCharSet ,.F. )
      EndIf
      If oCnn:lNative
         oCnn:exec( "ALTER SESSION SET NLS_DATE_FORMAT='yyyymmdd'",.F. )
         oCnn:exec( "ALTER SESSION SET NLS_TIMESTAMP_FORMAT='yyymmdd HH.MI.SSXFF AM'",.F. )
      EndIf
      Exit

   Case SYSTEMID_INGRES
      oCnn:Commit()
      oCnn:exec( "set lockmode session where readlock=nolock,level=row" )
      Exit

   Case SYSTEMID_SYBASE
//      oCnn:commit()
//      oCnn:exec( "SET CHAINED ON" )
      oCnn:commit()
      oCnn:exec( "SET QUOTED_IDENTIFIER ON" )
      oCnn:exec( "SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED" )
      Exit

   Case SYSTEMID_MSSQL7
   Case SYSTEMID_MSSQL6
   Case SYSTEMID_AZURE
      oCnn:Commit()
      oCnn:exec( "SET QUOTED_IDENTIFIER ON" )
      oCnn:exec( "SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED" )
      oCnn:Commit()
      Exit

   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB
      oCnn:exec( "set session autocommit=0;", .F. )
      oCnn:exec( "SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED", .F. )
      Exit

   Case SYSTEMID_CACHE
      oCnn:exec( "SET OPTION SUPPORT_DELIMITED_IDENTIFIERS=TRUE" )
      oCnn:Commit()
      Exit

   Case SYSTEMID_POSTGR
      oCnn:exec( "SET CLIENT_ENCODING to 'SQL_ASCII'",.F.,.T.,@aRet )
      oCnn:Commit()
      Exit

   End

Return cRet

/*------------------------------------------------------------------------*/

Function SR_ReloadMLHash( oConnect )

   Local aRet := {}, aCol

   oConnect:exec( "SELECT TABLE_ , COLUMN_, TYPE_, LEN_, DEC_ FROM " + SR_GetToolsOwner() + "SR_MGMNTLANG",.F.,.T.,@aRet )
   oConnect:commit()

   hMultilangColumns := Hash()
   HAllocate( hMultilangColumns, max(10, len( aRet ) ) )

   For each aCol in aRet
      hMultilangColumns[ aCol[1] + aCol[2] ] := aCol
   Next

Return NIL

/*------------------------------------------------------------------------*/

Function AddToMLHash( aField )

   hMultilangColumns[ PadR(aField[1],50) + PadR(aField[2],50) ] := aField

Return NIL

/*------------------------------------------------------------------------*/

Function GetMLHash( cTab, cCol )

   Local cKey := PadR( upper( cTab ), 50 ) + PadR( upper( cCol ), 50 )
   Local nPos := HGetPos( hMultilangColumns, cKey )

   If nPos > 0
      Return HGetValueAt( hMultilangColumns, nPos )
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_ErrorOnGotoToInvalidRecord( l )
   Local lOld  := lErrorOnGotoToInvalidRecord
   If l != NIL
      lErrorOnGotoToInvalidRecord := l
   EndIf
Return lOld

Function SR_UseNullsFirst( l )
   Local lOld  := lUseNullsFirst
   If l != NIL
      lUseNullsFirst := l
   EndIf
Return lOld



/*------------------------------------------------------------------------*/

Function SR_TblMgmnt()
Return lTblMgmnt

/*------------------------------------------------------------------------*/

Function SR_SetTblMgmnt(lOpt)
   Local lOld := lTblMgmnt
   lTblMgmnt := lOpt
Return lOld

/*------------------------------------------------------------------------*/

Function SR_EvalFilters( lEval )
   Local lOld  := EvalFilters
   If lEval != NIL
      EvalFilters := lEval
   EndIf
Return lOld

/*------------------------------------------------------------------------*/

Function SR_RecnoName( cName )
   Local cOld  := RecnoName
   If cName != NIL
      RecnoName := Upper(Alltrim(cName))
   EndIf
Return cOld

/*------------------------------------------------------------------------*/

Function SR_MaxRowCache( n )
   Local cOld  := nMaxRowCache
   If n != NIL
      nMaxRowCache := n
   EndIf
Return cOld

/*------------------------------------------------------------------------*/

Function SR_FetchSize( n )
   Local cOld  := nFetchSize
   If n != NIL
      nFetchSize := n
   EndIf
Return cOld

/*------------------------------------------------------------------------*/

Function SR_DeletedName( cName )
   Local cOld  := DeletedName
   If cName != NIL
      DeletedName := Upper(Alltrim(cName))
   EndIf
Return cOld

/*------------------------------------------------------------------------*/

Function SR_ExistTable( cTableName, cOwner, oCnn )

   Local cFileName, nRet, aRet

   DEFAULT oCnn := SR_GetConnection()
   aRet := eval( SR_GetTableInfoBlock(), cTableName )

   If cOwner == NIL
      cOwner := aRet[ TABLE_INFO_OWNER_NAME ]
      If !Empty( SR_GetGlobalOwner() )
         cOwner := alltrim( SR_GetGlobalOwner() )
      ElseIf !Empty( oCnn:cOwner )
         cOwner := alltrim(oCnn:cOwner)
      ElseIf cOwner == NIL
         cOwner := ""
      EndIf
   EndIf

   If (!Empty( cOwner )) .and. cOwner[-1] != "."
      cOwner += "."
   EndIf

   cFileName := SR_ParseFileName( alltrim( aRet[ TABLE_INFO_TABLE_NAME ] ) )

   If oCnn:oSqlTransact == NIL
      nRet := oCnn:exec( "SELECT * FROM " + cOwner + SR_DBQUALIFY( cFileName , oCnn:nSystemID) + " WHERE 0 = 2",.F. )
      oCnn:Commit()
   Else
      nRet := oCnn:oSqlTransact:exec( "SELECT * FROM " + cOwner + SR_DBQUALIFY( cFileName , oCnn:nSystemID) + " WHERE 0 = 2",.F. )
      oCnn:oSqlTransact:Commit()
   EndIf

   If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO .or. nRet == SQL_NO_DATA_FOUND
      Return .T.
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

Function SR_ExistIndex( cIndexName, cOwner )

   Local oCnn, nRet, aRet := {}

   (cOwner)

   oCnn := SR_GetConnection()

   aRet := eval( SR_GetIndexInfoBlock(), cIndexName )
   aSize( aRet, TABLE_INFO_SIZE )

   cIndexName  := SR_ParseFileName( aRet[ TABLE_INFO_TABLE_NAME ] )

   aRet := {}
   nRet := oCnn:exec( "SELECT * FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE IDXNAME_ = '" + Upper(Alltrim(cIndexName)) + "'" ,.F.,.T.,@aRet )

   If (nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO) .and. len(aRet) > 0
      Return .T.
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

Function SR_File( cTableName )

   Local cTbl := lower( cTableName )

   Do Case
   Case ".dbf" $ cTbl .or. ".dbt" $ cTbl
      Return SR_ExistTable( cTableName )
   Case ".ntx" $ cTbl .or. ".cdx" $ cTbl
      Return SR_ExistIndex( cTableName )
   EndCase

Return SR_ExistTable( cTableName ) .or. SR_ExistIndex( cTableName )

/*------------------------------------------------------------------------*/

Function SR_EndConnection( nConnection )

   Local oCnn, uRet

   DEFAULT nActiveConnection := 0
   DEFAULT nConnection := nActiveConnection
   DEFAULT aConnections  := {}

   #pragma TEXTHIDDEN(1)

   #ifdef SQLRDD_DEMO
      #ifdef __PLATFORM__Windows
         SR_BINDBYVALUE( hextostr(DEMO_NOTICE) )
      #else
         SQLBINDBYVAL( hextostr(DEMO_NOTICE) )
      #endif

   #endif

   #pragma TEXTHIDDEN(0)

   SR_CheckConnection( nConnection )

   If nConnection > len( aConnections ) .or. nConnection == 0 .or. nConnection < 0
      Return NIL
   EndIf

   oCnn := aConnections[ nConnection ]

   If nConnection == len( aConnections )
      aSize( aConnections, len(aConnections) - 1 )
   Else
      aConnections[ nConnection ] := NIL
   EndIf

   If oCnn != NIL
      If oCnn:oSqlTransact != NIL
         oCnn:oSqlTransact:RollBack()
         oCnn:oSqlTransact:end()
      EndIf
      uRet := oCnn:end()
   EndIf

   nActiveConnection := len( aConnections )

Return uRet

/*------------------------------------------------------------------------*/

Function SR_GetConnectionInfo( nConnection, nInfo )

   Local oCnn
   DEFAULT nActiveConnection := 0
   DEFAULT nConnection := nActiveConnection
   SR_CheckConnection( nConnection )
   oCnn := SR_GetConnection()

   Switch nInfo
   Case SQL_DBMS_NAME
      Return oCnn:cTargetDB
      //Exit
   Case SQL_DBMS_VER
      Return oCnn:cSystemVers
      //Exit
   End

Return ""

/*------------------------------------------------------------------------*/

Function SR_StartLog( nConnection )

   DEFAULT nActiveConnection := 0
   DEFAULT nConnection := nActiveConnection
   DEFAULT aConnections  := {}
   SR_CheckConnection( nConnection )
   aConnections[ nConnection ]:lTraceToDBF := .T.
   If aConnections[ nConnection ]:oSqlTransact != NIL
      aConnections[ nConnection ]:oSqlTransact:lTraceToDBF := .T.
   EndIf

Return .T.

/*------------------------------------------------------------------------*/

Function SR_StartTrace( nConnection )

   DEFAULT nActiveConnection := 0
   DEFAULT nConnection := nActiveConnection
   DEFAULT aConnections  := {}
   SR_CheckConnection( nConnection )
   aConnections[ nConnection ]:lTraceToScreen := .T.
   If aConnections[ nConnection ]:oSqlTransact != NIL
      aConnections[ nConnection ]:oSqlTransact:lTraceToScreen := .T.
   EndIf

Return .T.

/*------------------------------------------------------------------------*/

Function SR_StopLog( nConnection )

   DEFAULT nActiveConnection := 0
   DEFAULT nConnection := nActiveConnection
   DEFAULT aConnections  := {}
   SR_CheckConnection( nConnection )
   aConnections[ nConnection ]:lTraceToDBF := .F.
   If aConnections[ nConnection ]:oSqlTransact != NIL
      aConnections[ nConnection ]:oSqlTransact:lTraceToDBF := .F.
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_StopTrace( nConnection )

   DEFAULT nActiveConnection := 0
   DEFAULT nConnection   := nActiveConnection
   DEFAULT aConnections  := {}
   SR_CheckConnection( nConnection )
   aConnections[ nConnection ]:lTraceToScreen := .F.
   If aConnections[ nConnection ]:oSqlTransact != NIL
      aConnections[ nConnection ]:oSqlTransact:lTraceToScreen := .F.
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_SetTimeTrace( nConnection, nMilisseconds )

   Local nOld
   DEFAULT nActiveConnection := 0
   DEFAULT nConnection   := nActiveConnection
   DEFAULT aConnections  := {}
   SR_CheckConnection( nConnection )
   DEFAULT nMilisseconds := aConnections[ nConnection ]:nTimeTraceMin
   nOld := aConnections[ nConnection ]:nTimeTraceMin
   aConnections[ nConnection ]:nTimeTraceMin := nMilisseconds

Return NIL

/*------------------------------------------------------------------------*/

Procedure SR_End()
   DEFAULT aConnections  := {}
   While len( aConnections ) > 0
      SR_EndConnection(len( aConnections ))
   EndDo
Return

/*------------------------------------------------------------------------*/

Function _SR_UnRegister(oWA)

   Local aActiveWAs, n
   aActiveWAs := oWa:oSql:oHashActiveWAs:Find(oWA:cFileName)

   If valtype( aActiveWAs ) == "A"
      While (n := aScan( aActiveWAs, { |x| x:nThisArea == oWA:nThisArea } ) ) > 0
         aDel( aActiveWAs, n )
         aSize( aActiveWAs, len(aActiveWAs)-1 )
      EndDo
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function _SR_Register(oWA)

   Local aActiveWAs

   aActiveWAs := oWa:oSql:oHashActiveWAs:Find(oWA:cFileName)

   If valtype( aActiveWAs ) == "A"
      aadd( aActiveWAs, oWA )
   Else
      oWa:oSql:oHashActiveWAs:Insert( oWA:cFileName, { oWA } )
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function _SR_ScanExec(oWA, bExpr)

   Local aActiveWAs := oWa:oSql:oHashActiveWAs:Find(oWA:cFileName)

   If valtype( aActiveWAs ) == "A"
      aEval( aActiveWAs, bExpr )
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function _SR_ScanExecAll( bExpr )

   SR_GetConnection():oHashActiveWAs:Haeval( bExpr )

Return NIL

/*------------------------------------------------------------------------*/

Function SR_GetUniqueSystemID()

   Local i1, i2

   i1 := HB_RANDOMINT(1, 99999999 )
   i2 := HB_RANDOMINT(1, 99999999 )

Return alltrim( SR_Val2Char( SR_GetCurrInstanceID() ) ) + "__" + strZero( i1, 8 ) + "__" + strZero( i2, 8 )

/*------------------------------------------------------------------------*/

Function SR_GetInternalID()

   Local i1, i2

   If cIntenalID != NIL
      Return cIntenalID
   EndIf

   i1 := HB_RANDOMINT(1, 99999999 )
   i2 := HB_RANDOMINT(1, 99999999 )

   cIntenalID := alltrim( SR_Val2Char( SR_GetCurrInstanceID() ) ) + "__" + strZero( i1, 8 ) + "__" + strZero( i2, 8 )

Return cIntenalID

/*------------------------------------------------------------------------*/

Function SR_SetCollation( cName )

   Local cOld := cCollation

   If valtype( cName ) == "C"
      cCollation := cName
   EndIf

Return cOld

/*------------------------------------------------------------------------*/

Function SR_DropIndex( cIndexName, cOwner )

   Local oCnn, cFileName, nRet, aRet := {}, cPhisicalName, aIndex, cIdxName
   Local oWA
   Local lTag := .F.,cIndex
   Local ctempIndex := ""
   Local nSelect

   oCnn := SR_GetConnection()

   aRet := eval( SR_GetIndexInfoBlock(), cIndexName )
   ctempIndex    := cIndexName
   cIndexName := SR_ParseFileName( alltrim( aRet[ TABLE_INFO_TABLE_NAME ] ) )

   If cOwner == NIL
      cOwner := aRet[ TABLE_INFO_OWNER_NAME ]
      If !Empty( SR_GetGlobalOwner() )
         cOwner := alltrim( SR_GetGlobalOwner() )
      ElseIf !Empty( oCnn:cOwner )
         cOwner := alltrim(oCnn:cOwner)
      ElseIf cOwner == NIL
         cOwner := ""
      EndIf
   EndIf

   If (!Empty( cOwner )) .and. cOwner[-1] != "."
      cOwner += "."
   EndIf

   aRet := {}
   nRet := oCnn:exec( "SELECT TABLE_, PHIS_NAME_, IDXNAME_, IDXCOL_, IDXFOR_, IDXKEY_ FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE IDXNAME_ = '" + Upper(Alltrim(cIndexName)) + "'" ,.F.,.T.,@aRet )
   If (nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO) .and. len(aRet) == 0
      // Index does not exist
      aRet := {}
      nRet := oCnn:exec( "SELECT TABLE_, PHIS_NAME_, IDXNAME_, IDXCOL_, IDXFOR_, IDXKEY_ FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE PHIS_NAME_ = '" + Upper(Alltrim(cIndexName)) + "'" ,.F.,.T.,@aRet )
      If (nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO) .and. len(aRet) == 0
          aRet := {}
          nRet := oCnn:exec( "SELECT TABLE_, PHIS_NAME_, IDXNAME_, IDXCOL_, IDXFOR_, IDXKEY_ FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TAG_ = '" + Alltrim(ctempIndex) + "'" ,.F.,.T.,@aRet )
         If (nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO) .and. len(aRet) == 0
            Return .F.
         ELSE
            lTag := .T.
         ENDIF
      endif
   EndIf

   cFileName   := rtrim( aRet[1,1] )
   cIndex      := rtrim( aRet[1,2] )
   cIdxName    := rtrim( aRet[1,3] )
   If lTag
      oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + cFileName + "' AND PHIS_NAME_ = '" + cIndex + "'" + if(oCnn:lComments," /* Wipe index info */",""), .F. )
   ELSE
      oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + cFileName + "' AND IDXNAME_ = '" + cIdxName + "'" + if(oCnn:lComments," /* Wipe index info */",""), .F. )
   ENDIF
   oCnn:Commit()

   For each aIndex in aRet
      cPhisicalName := rtrim( aIndex[2] )

      Switch oCnn:nSystemID
      Case SYSTEMID_MSSQL6
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_SYBASE
      Case SYSTEMID_AZURE
         oCnn:exec( "DROP INDEX " + cOwner + SR_DBQUALIFY( cFileName, oCnn:nSystemID ) + "." + cPhisicalName, .F. )
         Exit
      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
         oCnn:exec( "DROP INDEX " + cPhisicalName + " ON " + cOwner + SR_DBQUALIFY( cFileName, oCnn:nSystemID ) + if(oCnn:lComments," /* DROP Index */",""), .F. )
         Exit
      Case SYSTEMID_ORACLE
         If len(aIndex[6]) > 4 .and. aIndex[6][4] == "@"
            oCnn:exec( "DROP INDEX " + cOwner + "A$" + SubStr(aIndex[6],1,3) + SubStr(cFileName,1,25) + if(oCnn:lComments," /* Drop VIndex */",""), .F. )
            oCnn:Commit()
            oCnn:exec( "DROP INDEX " + cOwner + "D$" + SubStr(aIndex[6],1,3) + SubStr(cFileName,1,25) + if(oCnn:lComments," /* Drop VIndex */",""), .F. )
            oCnn:Commit()
         EndIf
         oCnn:exec( "DROP INDEX " + cPhisicalName + if(oCnn:lComments," /* DROP Index */",""), .F. )
         Exit
      DEFAULT
         oCnn:exec( "DROP INDEX " + cPhisicalName + if(oCnn:lComments," /* DROP Index */",""), .F. )
      End
      
      oCnn:Commit()  //prevent hanging on MsSQL

      If (!Empty( aIndex[4] )) .or. aIndex[5][1] == "#"
         nSelect:=select()
         
         USE (cFileName) NEW VIA "SQLRDD" ALIAS "TEMPDROPCO" exclusive
         oWA := TEMPDROPCO->( dbInfo( DBI_INTERNAL_OBJECT ) )

         If !Empty( aIndex[4] )
            oWA:DropColumn( "INDKEY_" + alltrim(aIndex[4]), .F. )
         EndIf
         If aIndex[5][1] == "#"
            oWA:DropColumn( "INDFOR_" + substr(aIndex[5],2,3), .F. )
         EndIf

         TEMPDROPCO->( dbCLoseArea() )
         
         select(nSelect)
      EndIf
   Next

//   oCnn:Commit()                 //moved up to prevent hanging on MsSQL
   SR_CleanTabInfoCache()

Return .T.

/*------------------------------------------------------------------------*/

Function SR_DropTable( cFileName, cOwner )

   Local oCnn, lRet, aRet := {}

   oCnn := SR_GetConnection()

   aRet := eval( SR_GetTableInfoBlock(), cFileName )
   cFileName := SR_ParseFileName( alltrim( aRet[ TABLE_INFO_TABLE_NAME ] ) )

   If cOwner == NIL
      cOwner := aRet[ TABLE_INFO_OWNER_NAME ]
      If !Empty( SR_GetGlobalOwner() )
         cOwner := alltrim( SR_GetGlobalOwner() )
      ElseIf !Empty( oCnn:cOwner )
         cOwner := alltrim(oCnn:cOwner)
      ElseIf cOwner == NIL
         cOwner := ""
      EndIf
   EndIf

   If (!Empty( cOwner )) .and. cOwner[-1] != "."
      cOwner += "."
   EndIf

   /* Drop the table */

   lRet := oCnn:exec( "DROP TABLE " + cOwner + SR_DBQUALIFY( cFileName, oCnn:nSystemID ) + if( oCnn:nSystemID == SYSTEMID_ORACLE, " CASCADE CONSTRAINTS","") + if(oCnn:lComments," /* drop table */",""), .t. ) == SQL_SUCCESS
   oCnn:Commit()

   If lRet
      oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + UPPER(cFileName) + "'" + if(oCnn:lComments," /* Wipe index info */",""), .F. )
      oCnn:Commit()
      oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTTABLES WHERE TABLE_ = '" + UPPER(cFileName) + "'" + if(oCnn:lComments," /* Wipe table info */",""), .F. )
      oCnn:Commit()
      oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLANG WHERE TABLE_ = '" + UPPER(cFileName) + "'" + if(oCnn:lComments," /* Wipe table info */",""), .F. )
      oCnn:Commit()
      oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS WHERE TABLE_ = '" + UPPER(cFileName) + "'" + if(oCnn:lComments," /* Wipe table info */",""), .F. )
      oCnn:Commit()
      oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS WHERE SOURCETABLE_ = '" + UPPER(cFileName) + "'" + if(oCnn:lComments," /* Wipe table info */",""), .F. )
      oCnn:Commit()
      oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRTGTCOLS WHERE SOURCETABLE_ = '" + UPPER(cFileName) + "'" + if(oCnn:lComments," /* Wipe table info */",""), .F. )
      oCnn:Commit()
      oCnn:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRSRCCOLS WHERE SOURCETABLE_ = '" + UPPER(cFileName) + "'" + if(oCnn:lComments," /* Wipe table info */",""), .F. )
      oCnn:Commit()
   EndIf

Return lRet

/*------------------------------------------------------------------------*/

Function SR_ListIndex( cFilename )

   Local oCnn, nRet, aRet := {}, i

   oCnn := SR_GetConnection()

   aRet := eval( SR_GetIndexInfoBlock(), cFilename )
   cFilename := SR_ParseFileName( alltrim( aRet[ TABLE_INFO_TABLE_NAME ] ) )

   aRet := {}
   nRet := oCnn:exec( "SELECT IDXNAME_,PHIS_NAME_,IDXKEY_,IDXFOR_,IDXCOL_,TAG_,TAGNUM_ FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + alltrim(upper(cFilename)) + "'" ,.F.,.T.,@aRet )

   For i = 1 to len( aRet )
      aRet[i,1] := alltrim( aRet[i,1] )
   Next

Return aRet

/*------------------------------------------------------------------------*/

Function SR_RenameTable( cTable, cNewName, cOwner )

   Local oCnn, nRet, aRet := {}, lOk := .F.

   oCnn := SR_GetConnection()

   aRet   := eval( SR_GetTableInfoBlock(), cTable )
   cTable := SR_ParseFileName( alltrim( aRet[ TABLE_INFO_TABLE_NAME ] ) )

   If cOwner == NIL
      cOwner := aRet[ TABLE_INFO_OWNER_NAME ]
      If !Empty( SR_GetGlobalOwner() )
         cOwner := alltrim( SR_GetGlobalOwner() )
      ElseIf !Empty( oCnn:cOwner )
         cOwner := alltrim(oCnn:cOwner)
      ElseIf cOwner == NIL
         cOwner := ""
      EndIf
   EndIf

   If (!Empty( cOwner )) .and. cOwner[-1] != "."
      cOwner += "."
   EndIf

   aRet     := eval( SR_GetTableInfoBlock(), cNewName )
   cNewName := SR_ParseFileName( alltrim( aRet[ TABLE_INFO_TABLE_NAME ] ) )

   aRet := {}
   nRet := oCnn:exec( "SELECT * FROM " + SR_GetToolsOwner() + "SR_MGMNTTABLES WHERE TABLE_ = '" + upper(cTable) + "'" ,.F.,.T.,@aRet )
   If (nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO) .and. len(aRet) == 0
      // Table does not exist
      Return .F.
   EndIf

   aRet := {}
   nRet := oCnn:exec( "SELECT * FROM " + SR_GetToolsOwner() + "SR_MGMNTTABLES WHERE TABLE_ = '" + upper(cNewName) + "'" ,.F.,.T.,@aRet )
   If len(aRet) > 0
      // Destination EXISTS !!
      Return .F.
   EndIf

   Switch oCnn:nSystemID
   Case SYSTEMID_MSSQL7
   Case SYSTEMID_AZURE
      nRet := oCnn:exec( "exec sp_rename " + cOwner + cTable + ", " + cOwner + cNewName, .F. )
      If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO
         lOk := .T.
      EndIf
      Exit
   Case SYSTEMID_POSTGR
   Case SYSTEMID_ORACLE
   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB

      IF oCnn:nSystemID == SYSTEMID_POSTGR
         nRet := oCnn:exec( "ALTER TABLE " + cOwner +SR_DBQUALIFY(cTable+"_sq",oCnn:nSystemID) + " RENAME TO " + cOwner + SR_DBQUALIFY(cNewName+"_sq",oCnn:nSystemID), .F. )
      ENDIF

      nRet := oCnn:exec( "ALTER TABLE " + cOwner + SR_DBQUALIFY(cTable,oCnn:nSystemID) + " RENAME TO " + cOwner + SR_DBQUALIFY(cNewName,oCnn:nSystemID), .F. )
      If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO
         lOk := .T.
      EndIf

      IF oCnn:nSystemID == SYSTEMID_POSTGR
         nRet := oCnn:exec( "ALTER TABLE "+cOwner + SR_DBQUALIFY(cNewName,oCnn:nSystemID) +" ALTER COLUMN " + SR_RecnoName()+ " SET DEFAULT nextval('"+ lower(cNewName)+"_sq'::regclass)")
      ENDIF
      IF oCnn:nSystemID == SYSTEMID_ORACLE
         nRet := oCnn:exec( "RENAME " + cOwner +cTable+"_sq" + " TO " + cOwner + cNewName+"_sq", .F. )
      ENDIF
      Exit
   End

   If lOk
      oCnn:exec( "UPDATE " + SR_GetToolsOwner() + "SR_MGMNTINDEXES SET TABLE_ = '" + cNewName + "' WHERE TABLE_ = '" + cTable + "'" , .F. )
      oCnn:exec( "UPDATE " + SR_GetToolsOwner() + "SR_MGMNTTABLES SET TABLE_ = '" + cNewName + "' WHERE TABLE_ = '" + cTable + "'" , .F. )
      oCnn:Commit()
   EndIf

Return lOk

/*------------------------------------------------------------------------*/

Function SR_ListTables( cOwner )

Return (SR_GetConnection()):ListCatTables( cOwner )

/*------------------------------------------------------------------------*/

Function SR_ListCreatedTables()

   Local oCnn, nRet, aRet := {}, aRet2 := {}

   oCnn := SR_GetConnection()
   nRet := oCnn:exec( "SELECT TABLE_ FROM " + SR_GetToolsOwner() + "SR_MGMNTTABLES" ,.F.,.T.,@aRet )

   aEval( aRet, {|x| aadd( aRet2, alltrim(x[1]) ) } )

Return aRet2

/*------------------------------------------------------------------------*/

Function SR_SetToolsOwner( cOwner )

   Local cOld := cToolsOwner
   Local oSql

   If cOwner != NIL
      cToolsOwner := cOwner
      If (!Empty(cOwner)) .and. cToolsOwner[-1] != "."
         cToolsOwner += "."
      EndIf
   Else
      If Empty(cToolsOwner)
         oSql := SR_GetConnection()
         If !Empty( oSql:cOwner )
            Return oSql:cOwner
         EndIf
      EndIf
   EndIf

Return cOld

/*------------------------------------------------------------------------*/

Function SR_GetToolsOwner()

   Local oSql

   If Empty(cToolsOwner)
      oSql := SR_GetConnection()
      If !Empty( oSql:cOwner )
         Return oSql:cOwner
      EndIf
   EndIf

Return cToolsOwner

/*------------------------------------------------------------------------

SQLRDD Exclusive Lock management and Lock behavior
--------------------------------------------------

We use internally 2 different connections to manage locks.
Connection information:

1 - Transactional connection (regular queries)
2 - Autocommit connection (used for counters and "one query" transactions)

SetLocks function steps, using SQLRDD system table SR_MGMNTLOCKS:

* For Microsoft SQL Server:
---------------------------

At application startup, perform the following housekeeping routine:

"DELETE FROM SR_MGMNTLOCKS WHERE SPID_ = @@SPID OR SPID_ + LOGIN_TIME_ NOT IN (SELECT SPID + LOGIN_TIME FROM MASTER.DBO.SYSPROCESSES)"

1 - Try to INSERT the string to be locked using connection 2; Commit;
2 - If success, LOCK is acquired
3 - If it fails, run housekeeping "DELETE FROM SR_MGMNTLOCKS WHERE SPID_ NOT IN (SELECT SPID FROM MASTER.DBO.SYSPROCESSES)"; Commit;
4 - Try INSERT again;
5 - If succefull, lock is acquired;
6 - If not succefull, lock is denied;

* For Oracle:
-------------

Housekeeping expression is:
"DELETE FROM SR_MGMNTLOCKS WHERE SPID_ = sys_context('USERENV','sessionid') OR SPID_ NOT IN (select "AUDSID" from v$session)"

Step 3 query is:
"DELETE FROM SR_MGMNTLOCKS WHERE SPID_ NOT IN (select "AUDSID" from v$session)"; Commit;

* For Postgres:
---------------

Housekeeping expression is:
"DELETE FROM SR_MGMNTLOCKS WHERE SPID_ = (select pg_backend_pid()) OR SPID_ NOT IN (select pg_stat_get_backend_pid(pg_stat_get_backend_idset()))"

Step 3 query is:
"DELETE FROM SR_MGMNTLOCKS WHERE SPID_ NOT IN (select pg_stat_get_backend_pid(pg_stat_get_backend_idset()))"; Commit;


ReleaseLocks function steps:

1 - Delete the line using connection 2; Commit;

------------------------------------------------------------------------*/

Function SR_SetLocks( uLocks, oCnn, nRetries )

   Local lRet := .T., aLocks, cSql, cValue, aAdded := {}, nRet
   Local cIns, cDel

   DEFAULT oCnn     := SR_GetConnection()
   DEFAULT nRetries := 0

   If oCnn:oSqlTransact == NIL
      Return .T.
   EndIf

   Do Case
   Case valtype( uLocks ) == "C"
      aLocks := { uLocks }
   Case valtype( uLocks ) == "A"
      aLocks := uLocks
   OtherWise
      aLocks := { SR_Val2Char( uLocks ) }
   EndCase

   For each cValue in aLocks

      cValue := SR_Val2Char( cValue )

      Switch oCnn:nSystemID
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_AZURE
         cIns := "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTLOCKS ( LOCK_, WSID_, SPID_, LOGIN_TIME_ ) VALUES ( '" + cValue + "', '" + SR_GetInternalID() + "', @@SPID, '" + oCnn:oSqlTransact:cLoginTime + "' )"
         cDel := "DELETE FROM SR_MGMNTLOCKS WHERE convert( CHAR(10), SPID_ ) + convert( CHAR(23), LOGIN_TIME_, 21 ) NOT IN (SELECT convert( CHAR(10), SPID) + CONVERT( CHAR(23), LOGIN_TIME, 21 ) FROM MASTER.DBO.SYSPROCESSES)"
         Exit
      Case SYSTEMID_ORACLE
         cIns := "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTLOCKS ( LOCK_, WSID_, SPID_ ) VALUES ( '" + cValue + "', '" + SR_GetInternalID() + "', " + str( oCnn:uSid ) + " )"
         cDel := [DELETE FROM SR_MGMNTLOCKS WHERE SPID_ NOT IN (select "AUDSID" from ] + If(oCnn:lCluster, "g", "" ) + [v$session)]
         Exit
      Case SYSTEMID_POSTGR
         cIns := "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTLOCKS ( LOCK_, WSID_, SPID_ ) VALUES ( '" + cValue + "', '" + SR_GetInternalID() + "', (select pg_backend_pid()) )"
         cDel := "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLOCKS WHERE SPID_ NOT IN (select pg_stat_get_backend_pid(pg_stat_get_backend_idset()))"
         Exit
      Case SYSTEMID_IBMDB2
         Exit
      End

      nRet := oCnn:oSqlTransact:exec( cIns, .F. )
      oCnn:oSqlTransact:Commit()

      If nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
         oCnn:oSqlTransact:exec( cDel, .F. )
         oCnn:oSqlTransact:Commit()
         nRet := oCnn:oSqlTransact:exec( cIns, .F. )
         oCnn:oSqlTransact:Commit()

         If nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
            While nRetries > 0
               oCnn:oSqlTransact:exec( cDel, .F. )
               oCnn:oSqlTransact:Commit()
               nRet := oCnn:oSqlTransact:exec( cIns, .F. )
               oCnn:oSqlTransact:Commit()

               If nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
                  Inkey(.5)
                  nRetries--
               Else
                  Exit
               EndIf
            EndDo
         EndIf
      EndIf

      If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO
         aadd( aAdded, cValue )
      Else
         lRet := .F.
         Exit
      EndIf
   Next

   If !lRet
      For each cValue in aAdded
         Switch oCnn:nSystemID
         Case SYSTEMID_MSSQL7
         Case SYSTEMID_ORACLE
         Case SYSTEMID_POSTGR
         Case SYSTEMID_IBMDB2
         Case SYSTEMID_AZURE
            cSql := "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLOCKS WHERE LOCK_ = '" + cValue + "' AND WSID_ = '" + SR_GetInternalID() + "'"
            Exit
         End
         oCnn:oSqlTransact:exec( cSql, .F. )
         oCnn:oSqlTransact:Commit()
      Next
   EndIf

Return lRet

/*------------------------------------------------------------------------*/

Function SR_ReleaseLocks( uLocks, oCnn )

   Local lRet := .T., aLocks, cValue, cSql

   DEFAULT oCnn := SR_GetConnection()

   If oCnn:oSqlTransact == NIL
      Return .F.
   EndIf

   Do Case
   Case valtype( uLocks ) == "C"
      aLocks := { uLocks }
   Case valtype( uLocks ) == "A"
      aLocks := uLocks
   OtherWise
      aLocks := { SR_Val2Char( uLocks ) }
   EndCase

   For each cValue in aLocks
      cValue := SR_Val2Char( cValue )
      Switch oCnn:nSystemID
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_ORACLE
      Case SYSTEMID_POSTGR
      Case SYSTEMID_IBMDB2
      Case SYSTEMID_AZURE
         cSql := "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLOCKS WHERE LOCK_ = '" + cValue + "' AND WSID_ = '" + SR_GetInternalID() + "'"
         Exit
      End

      oCnn:oSqlTransact:exec( cSql, .T. )
      oCnn:oSqlTransact:Commit()
   Next

Return lRet

/*------------------------------------------------------------------------*/

Function SR_ListLocks( oCnn, lAll )
   Local aLocks := {}

   DEFAULT oCnn := SR_GetConnection()
   DEFAULT lAll := .F.

   If oCnn:oSqlTransact == NIL
      Return {}
   EndIf

   // Housekeeping to avoid wrong info

   Switch oCnn:oSqlTransact:nSystemID
   Case SYSTEMID_ORACLE
      oCnn:oSqlTransact:exec( "DELETE FROM " + SR_GetToolsOwner() + [SR_MGMNTLOCKS WHERE SPID_ NOT IN (select "SID" from ] + If(oCnn:lCluster, "g", "" ) + [v$session)], .F. )
      Exit
   Case SYSTEMID_INGRES
      Exit
   Case SYSTEMID_IBMDB2
      Exit
   Case SYSTEMID_SYBASE
      Exit
   Case SYSTEMID_MSSQL7
   Case SYSTEMID_MSSQL6
   Case SYSTEMID_AZURE
      oCnn:oSqlTransact:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLOCKS WHERE convert( CHAR(10), SPID_ ) + convert( CHAR(23), LOGIN_TIME_, 21 ) NOT IN (SELECT convert( CHAR(10), SPID) + CONVERT( CHAR(23), LOGIN_TIME, 21 ) FROM MASTER.DBO.SYSPROCESSES)",.F. )
      Exit
   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB
      Exit
   Case SYSTEMID_POSTGR
      oCnn:oSqlTransact:exec( "DELETE FROM  " + SR_GetToolsOwner() + "SR_MGMNTLOCKS WHERE SPID_ NOT IN (select pg_stat_get_backend_pid(pg_stat_get_backend_idset()))", .F. )
      Exit
   End

   oCnn:oSqlTransact:Commit()
   oCnn:oSqlTransact:exec( "SELECT LOCK_, WSID_, SPID_ FROM " + SR_GetToolsOwner() + "SR_MGMNTLOCKS" + If( lAll, " WHERE WSID_ = '" + SR_GetInternalID() + "'", ""),.F.,.T.,@aLocks )

Return aLocks

/*------------------------------------------------------------------------*/

Function DetectDBFromDSN( cConnect )

Return SR_DetectDBFromDSN( cConnect )

/*------------------------------------------------------------------------*/

Function SR_DetectDBFromDSN( cConnect )

   Local aItem, cBuff, aToken
   Local aCon := hb_atokens(cConnect,";")

   For each aItem in aCon
      aToken := hb_atokens(aItem,"=")
      cBuff = Upper( aToken[1] )
      Do Case
      Case cBuff == "OCI"
         Return CONNECT_ORACLE
      Case cBuff == "OCI2"
         Return CONNECT_ORACLE2
      Case cBuff == "PGS"
         Return CONNECT_POSTGRES
      Case cBuff == "MYSQL"
         Return CONNECT_MYSQL
      Case cBuff == "MARIA"
         Return CONNECT_MARIA
      Case cBuff == "FB" .or. cBuff == "FIREBIRD" .or. cBuff == "IB"
         Return CONNECT_FIREBIRD
      Case cBuff == "FB3" .or. cBuff == "FIREBIRD3"
         Return CONNECT_FIREBIRD3
      Case cBuff == "DSN" .or. cBuff == "DRIVER"
         Return CONNECT_ODBC
      EndCase
   Next

Return SYSTEMID_UNKNOW


/*------------------------------------------------------------------------*/

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapiitm.h"
#include "compat.h"
#if defined(HB_OS_WIN)
#include <windows.h>
#endif
static HB_BOOL s_fMultiLang               = HB_FALSE;
static HB_BOOL s_fShutDown                = HB_FALSE;
static HB_BOOL s_fGoTopOnScope            = HB_TRUE;
static HB_BOOL s_fSerializedAsString      = HB_FALSE;
static HB_BOOL s_fHideRecno               = HB_TRUE;
static HB_BOOL s_fHideHistoric            = HB_FALSE;
static HB_BOOL s_fUseDeleteds             = HB_TRUE;
/* Culik added new global to tell if we will serialize arrays as json or xml */
static HB_BOOL s_fSerializeArrayAsJson    = HB_FALSE;
/* Culik added new global to tell if we are using sqlverser 2008 or newer */
static HB_BOOL s_fSql2008newTypes         = HB_FALSE;

static HB_BOOL s_iOldPgsBehavior          = HB_FALSE;
static HB_BOOL s_fShortasNum              = HB_FALSE;

HB_BOOL HB_EXPORT sr_isMultilang( void )
{
   return s_fMultiLang;
}

HB_FUNC( SR_SETMULTILANG )
{
   hb_retl( s_fMultiLang );
   if( HB_ISLOG( 1 ) )
      s_fMultiLang = hb_parl( 1 );
}

HB_BOOL HB_EXPORT sr_isShutdownProcess( void )
{
   return s_fShutDown;
}

HB_FUNC( SR_SETSHUTDOWN )
{
   hb_retl( s_fShutDown );
   if( HB_ISLOG( 1 ) )
      s_fShutDown = hb_parl( 1 );
}

HB_BOOL HB_EXPORT sr_GoTopOnScope( void )
{
   return s_fGoTopOnScope;
}

HB_FUNC( SR_SETGOTOPONSCOPE )
{
   hb_retl( s_fGoTopOnScope );
   if( HB_ISLOG( 1 ) )
      s_fGoTopOnScope = hb_parl( 1 );
}

HB_BOOL HB_EXPORT sr_lSerializedAsString( void )
{
   return s_fSerializedAsString;
}

HB_FUNC( SR_SETSERIALIZEDSTRING )
{
   hb_retl( s_fSerializedAsString );
   if( HB_ISLOG( 1 ) )
      s_fSerializedAsString = hb_parl( 1 );
}

HB_BOOL HB_EXPORT sr_lHideRecno( void )
{
   return s_fHideRecno;
}

HB_FUNC( SR_SETHIDERECNO )
{
   hb_retl( s_fHideRecno );
   if( HB_ISLOG( 1 ) )
      s_fHideRecno = hb_parl( 1 );
}

HB_BOOL HB_EXPORT sr_lHideHistoric( void )
{
   return s_fHideHistoric;
}

HB_FUNC( SR_SETHIDEHISTORIC )
{
   hb_retl( s_fHideHistoric );
   if( HB_ISLOG( 1 ) )
      s_fHideHistoric = hb_parl( 1 );
}

HB_BOOL HB_EXPORT sr_UseDeleteds( void )
{
   return s_fUseDeleteds;
}

HB_FUNC( SR_USEDELETEDS )
{
   hb_retl( s_fUseDeleteds );
   if( HB_ISLOG( 1 ) )
      s_fUseDeleteds = hb_parl( 1 );
}

HB_BOOL HB_EXPORT sr_lSerializeArrayAsJson( void )
{
   return s_fSerializeArrayAsJson;
}

HB_FUNC( SR_SETSERIALIZEARRAYASJSON )
{
   hb_retl( s_fSerializeArrayAsJson );
   if( HB_ISLOG( 1 ) )
      s_fSerializeArrayAsJson = hb_parl( 1 );
}

BOOL HB_EXPORT sr_lsql2008newTypes( void )
{
   return s_fSql2008newTypes;
}

BOOL HB_EXPORT sr_iOldPgsBehavior( void )
{
   return s_iOldPgsBehavior ;
}

HB_FUNC( SR_GETSQL2008NEWTYPES )
{
   hb_retl( s_fSql2008newTypes );
}

HB_FUNC( SR_SETSQL2008NEWTYPES )
{
   hb_retl( s_fSql2008newTypes );
   if( HB_ISLOG( 1 ) )
      s_fSql2008newTypes = hb_parl( 1 );
}

HB_FUNC(SETPGSOLDBEHAVIOR)
{
   int iOld = s_iOldPgsBehavior;
   if (ISLOG( 1 ) )
       s_iOldPgsBehavior= hb_parl( 1 ) ;
    hb_retl( iOld ) ;
}


BOOL HB_EXPORT  sr_fShortasNum( void )
{
return s_fShortasNum;
}
HB_FUNC(SETFIREBIRDUSESHORTASNUM)
{
   int iOld = s_fShortasNum;
   if (ISLOG( 1 ) )
       s_fShortasNum= hb_parl( 1 ) ;
    hb_retl( iOld ) ;

}
#pragma ENDDUMP

/*------------------------------------------------------------------------*/

Function SR_Version()

Return HB_SR__VERSION_STRING + ", Build " + alltrim(strzero(HB_SQLRDD_BUILD,4)) + ", " + HB_SR__MGMNT_VERSION

/*------------------------------------------------------------------------*/

Exit Proc SQLRDD_ShutDown()
   sr_setShutDown( .T. )
Return

/*------------------------------------------------------------------------*/
