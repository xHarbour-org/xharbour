/* $CATEGORY$SQLRDD/Connection$FILES$sql.lib$
* Main Connection Class
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*
* This class does not work alone. It should be superclass of
* database-specific connection class
*/

#include "hbclass.ch"
#include "common.ch"
#include "compat.ch"
#include "sqlodbc.ch"
#include "sqlrdd.ch"
#include "error.ch"
#include "msg.ch"
#include "sqlrddsetup.ch"

#define DEBUGSESSION     .F.

Static lNwgOldCompat  := .F.

/*------------------------------------------------------------------------*/

CLASS SR_CONNECTION

   CLASSDATA lFreezed         AS LOGICAL     INIT .F.
   DATA cLowLevLogFile   AS CHARACTER   INIT "sqltrace.log"

   DATA nConnectionType
   DATA nSizeMaxBuff, nPreFetch
   DATA cOwner, cQueryOwner, lTrace, oSqlTransact
   DATA oCache, nSelMeth, nEmptyMode, nDateMode
   DATA oSql, cNextQuery
   DATA sSiteName, cSQLError, cResult, uSid, cLockWait

   DATA cTargetDB, cSystemName, nSystemID, cSystemVers   READONLY
   DATA nFields, aFields, hEnv, hDbc, nRetCode, nVersion READONLY
   // CULIK 18/10/2010 Adicionado para indicar se o indice contem cluster
   DATA lClustered AS LOGICAL INIT .F. READONLY
   //culik 30/12/2011 adicionado para indicar se e  sqlserver versao 2008 ou superior
   DATA lSqlServer2008 AS LOGICAL INIT .F. 
   DATA lPostgresql8   AS LOGICAL INIT .F.  // do we have postgressql >= 8.3
   DATA lPostgresql83  AS LOGICAL INIT .F.  // do we have postgressql >= 8.3
   DATA lMariaDb       AS LOGICAL INIT .F.  // do we have mariadb 
   DATA oHashActiveWAs

   DATA aTableInfo      INIT { => }
   DATA aFieldModifier

   DATA cDsnTblData, cDsnTblIndx, cDsnTblLob

   DATA lNative         AS LOGICAL INIT .T.
   DATA lComments       AS LOGICAL INIT .T.
   DATA lQueryOnly      AS LOGICAL INIT .F.
   DATA lLowLevSqlDbg   AS LOGICAL INIT .F.
   DATA lAutoCommit     AS LOGICAL INIT .F.
   DATA lUseRepl        AS LOGICAL INIT .F.
   DATA lCounter        AS LOGICAL INIT .F.
   DATA lLogDateTime    AS LOGICAL INIT .F.
   DATA lCluster        AS LOGICAL INIT .F.
   DATA lCompress       AS LOGICAL INIT .F. // to enable mysql compression

//   DATA nParallel
//   DATA cLastLine
//   DATA lWantCommit AS LOGICAL INIT .F.
//   DATA lLocks      AS LOGICAL INIT .F.
//   DATA SimulateLL  AS LOGICAL INIT .F.
//   DATA cTempDir         AS CHARACTER   INIT ""

   DATA lSetNext         AS LOGICAL    INIT .F.
   DATA lResultSet       AS LOGICAL    INIT .T.
   DATA lAllInCache      AS LOGICAL    INIT .F.  // Handles whether derived workareas are ALL_IN_CACHE or not
   DATA cLastComm        AS CHARACTER  INIT ""
   DATA cMgmntVers       AS CHARACTER  INIT ""
   DATA cLoginTime       AS CHARACTER  INIT ""
   DATA cAppUser         AS CHARACTER  INIT ""
   DATA cSite            AS CHARACTER  INIT ""
   DATA lHistEnable      AS LOGICAL    INIT .T.
   DATA lTraceToDBF      AS LOGICAL    INIT .F.
   DATA lTraceToScreen   AS LOGICAL    INIT .F.
   DATA nTimeTraceMin    AS NUMERIC    INIT 1000
   DATA nLogMode         AS NUMERIC    INIT SQLLOGCHANGES_NOLOG
   DATA hStmt            //AS NUMERIC    INIT 0
   DATA nConnID          AS NUMERIC    INIT 0
   DATA nID              AS NUMERIC    INIT 0
   DATA nTransacCount    AS NUMERIC    INIT 0
   DATA nMaxTextLines    AS NUMERIC    INIT 100
   DATA nLockWaitTime    AS NUMERIC    INIT 1
   DATA lShowTxtMemo     AS LOGICAL    INIT .F.
   DATA nIteractions     AS NUMERIC    INIT 0
   DATA nAutoCommit      AS NUMERIC    INIT 0
   DATA lUseSequences    AS LOGICAL    INIT .T.
   DATA nTCCompat        AS NUMERIC    INIT 0      // TopConnect compatibility mode

   DATA cTempFile
   DATA oODBCTemp, oOdbc
   DATA nSetOpt, nSetValue, nMiliseconds
   DATA cPort, cHost, oSock, cDBS, cDrv, cDTB, cHandle       /* RPC stuff */
   DATA cCharSet, cNetLibrary
   DATA cApp
   // Culik Added to tell to postgresql use ssl
   DATA sslcert
   DATA sslkey
   DATA sslrootcert
   DATA sslcrl   
   // CULIK 21/3/2011 Adicionado para indicar se o indice contem cluster
   DATA lClustered AS LOGICAL INIT .F. READONLY
   //culik 30/12/2011 adicionado para indicar se e  sqlserver versao 2008 ou superior
   DATA lSqlServer2008 AS LOGICAL INIT .F. 
   DATA lOracle12      AS LOGICAL INIT .F.  // do we have Oracle >= 12.0
   
   DATA lBind Init .f.
   DATA cSqlPrepare INIT ""
   DATA aBindParameters INIT {}
   
   PROTECTED:

   DATA cConnect, cDSN, cUser, cPassword

   EXPORTED:

   METHOD Connect( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace, cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit, nTimeout ) CONSTRUCTOR
   METHOD ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace, cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit, nTimeout ) VIRTUAL
   METHOD DetectTargetDb()
   METHOD RPCTalk( cSend )
   METHOD End()
   METHOD GetInfo( nType )
   METHOD SetOptions( nType, uBuffer )
   METHOD GetOptions( nType )
   METHOD LastError()
   METHOD Commit()
   METHOD RollBack()
   METHOD IniFields( lReSelect, cTable, cCommand, lLoadCache, cWhere, cRecnoName, cDeletedName, lRefCursor ) VIRTUAL
   METHOD ExecuteRaw() VIRTUAL
   METHOD Execute( cCommand, lErrMsg, nLogMode, cType )
   //METHOD Exec( cCommand, lMsg, lFetch, aArray, cFile, cAlias, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate, nLogMode, cType )
   METHOD Exec( cCommand, lMsg, lFetch, aArray, cFile, cAlias, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate, nLogMode, cType , cCodePage )
   METHOD AllocStatement()
   METHOD SetStmtOptions( nType, uBuffer )
   METHOD RuntimeErr( cOperation, cErr )
   METHOD FreeStatement() VIRTUAL
   METHOD Fetch( aLine, lTranslate, aFields )
   METHOD FetchRaw( lTranslate, aFields ) VIRTUAL
   METHOD FieldGet( nField, aField, lTranslate ) VIRTUAL
   METHOD SetNextOpt( nSet, nOpt )
   METHOD MoreResults() VIRTUAL
   METHOD Getline( aFields, lTranslate, aArray )
   METHOD GetStruct( cTable )
   METHOD WriteMemo( hDbc, cFileName, nRecno, cRecnoName, aColumnsAndData ) VIRTUAL
   METHOD ListCatTables( cOwner )
   METHOD DriverCatTables() VIRTUAL
   METHOD FetchMultiple( lTranslate, aFields, aCache, nCurrentFetch, aInfo, nDirection, hnRecno, lFetchAll, aFetch, uRecord, nPos ) VIRTUAL
   METHOD LogQuery( cSql, cType )

   METHOD SQLType( nType, cName, nLen )
   METHOD SQLLen( nType, nLen, nDec )

   METHOD GetConnectionID() INLINE (::uSid)
   METHOD KillConnectionID( nID ) VIRTUAL
   METHOD ExecSPRC(  cComm, lMsg, lFetch, aArray, cFile, cAlias, cVar, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate, nLogMode  ) VIRTUAL
   METHOD ExecSP( cComm, aReturn, nParam ) VIRTUAL
   METHOD GetAffectedRows() Virtual

   
ENDCLASS

/*------------------------------------------------------------------------*/

METHOD LogQuery( cCommand, cType, nLogMode, nCost )    CLASS SR_CONNECTION

   Local cSql, cMode, oSql, cStack

   DEFAULT cType    := SQLLOGCHANGES_TYPE_DML
   DEFAULT nLogMode := ::nLogMode
   DEFAULT nCost    := 0

   cMode := StrZero( nLogMode, SQLLOGCHANGES_SIZE )

   If ::cQueryOwner == NIL
      If !Empty( SR_GetGlobalOwner() )
         ::cQueryOwner := alltrim( SR_GetGlobalOwner() )
      ElseIf !Empty( ::oSql:cOwner )
         ::cQueryOwner := alltrim(::cOwner)
      EndIf

      If (!Empty( ::cQueryOwner )) .and. ::cQueryOwner[-1] != "."
         ::cQueryOwner += "."
      EndIf

      If Empty( ::cQueryOwner )
         ::cQueryOwner := ""
      EndIf

   EndIf

   If cMode[4] == "1" .or. ::oSqlTransact == NIL
      oSql := Self
   Else
      oSql := ::oSqlTransact
   EndIf

   If cMode[1] == "1"
      cStack := sr_cDbValue( SR_GetStack(), ::nSystemID )
   Else
      cStack := " NULL"
   EndIf

   cSql := "INSERT INTO " + ::cQueryOwner + "SR_MGMNTLOGCHG (SPID_, WPID_, TYPE_, APPUSER_, TIME_, QUERY_, CALLSTACK_, SITE_, CONTROL_, COST_ ) VALUES ( " +;
           str(::uSid) + "," + str( SR_GetCurrInstanceID() ) + ", '" + cType + "','" + ::cAppUser + "','" + dtos(date()) + time() + strzero( seconds() * 1000, 8 ) + "'," + sr_cDbValue( cCommand, ::nSystemID ) + "," + cStack + ",'" + ::cSite + "', NULL, " + str(nCost) + " )"
   oSql:execute( cSql,,,.T. )
   oSql:FreeStatement()

   If cMode[4] != "1"
      oSql:Commit()
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD ListCatTables( cOwner )   CLASS SR_CONNECTION

   Local aRet := {}, aRet2 := {}, i

   DEFAULT cOwner := SR_SetGlobalOwner()

   If cOwner[-1] == "."
      cOwner := SubStr( cOwner, 1, len( cOwner) -1 )
   EndIf

   Switch ::nSystemID
   Case SYSTEMID_MSSQL7
   Case SYSTEMID_SYBASE
      If empty( cOwner )
         ::exec( "select name from sysobjects where type = N'U' order by name", .T., .T., @aRet )
      Else
         ::exec( "select name from sysobjects where type = N'U' and user_name(uid) = '" + cOwner + "' order by name", .T., .T., @aRet )
      EndIf
      Exit
   Case SYSTEMID_POSTGR
      If empty( cOwner )
         ::exec( "select tablename from pg_tables where schemaname = 'public' order by tablename", .T., .T., @aRet )
      Else
         ::exec( "select tablename from pg_tables where schemaname = '" + cOwner + "' order by tablename", .T., .T., @aRet )
      EndIf
      Exit
   Case SYSTEMID_ORACLE
      If empty( cOwner )
         ::exec( "select table_name from user_tables order by TABLE_NAME", .T., .T., @aRet )
      Else
         ::exec( "select TABLE_NAME from all_tables where owner = '" + cOwner + "' order by TABLE_NAME", .T., .T., @aRet )
      EndIf
      Exit
   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB
      ::exec( "show tables", .T., .T., @aRet )
      Exit
   Case SYSTEMID_ADABAS
   Case SYSTEMID_IBMDB2
   Case SYSTEMID_CACHE
   Case SYSTEMID_INGRES
      aRet := ::DriverCatTables()
      exit
   Case SYSTEMID_FIREBR
   Case SYSTEMID_FIREBR3   
      If empty( cOwner )
         ::exec( "select RDB$RELATION_NAME from RDB$RELATIONS where RDB$FLAGS = 1 order by RDB$RELATION_NAME", .T., .T., @aRet )
      Else
         ::exec( "select RDB$RELATION_NAME from RDB$RELATIONS where RDB$FLAGS = 1 AND RDB$OWNER_NAME = '" + cOwner + "' order by RDB$RELATION_NAME", .T., .T., @aRet )
      EndIf
      Exit
   End

   aRet2 := array( len( aRet ) )
   For i = 1 to len(aRet)
      aRet2[i] := upper(rtrim(aRet[i,1]))
   Next

Return aRet2

/*------------------------------------------------------------------------*/

METHOD Fetch( aLine, lTranslate, aFields ) CLASS SR_CONNECTION

   Local lResults := valtype( aLine ) == "A"
   Local i, nRet := ::FetchRaw( lTranslate, aFields )

   If nRet == SQL_SUCCESS .and. lResults
      aSize( aLine, ::nFields )
      For i = 1 to ::nFields
         aLine[i] := ::FieldGet( i, ::aFields, lTranslate )
      Next
   EndIf

Return nRet

/*------------------------------------------------------------------------*/

METHOD GetStruct( cTable ) CLASS SR_CONNECTION

Return ::oSql:IniFields( .T., cTable,, .F. )

/*------------------------------------------------------------------------*/

METHOD Getline( aFields, lTranslate, aArray )  CLASS SR_CONNECTION
   Local i

   If aArray == NIL
      aArray := Array(len( aFields ))
   ElseIf len( aArray ) < len( aFields )
      aSize( aArray, len( aFields ) )
   EndIf

   For i = 1 to len( aFields )
      aArray[i] := ::FieldGet( i, aFields, lTranslate )
   Next

Return aArray

/*------------------------------------------------------------------------*/

METHOD SetNextOpt( nSet, nOpt )  CLASS SR_CONNECTION
   ::lSetNext  := .T.
   ::nSetOpt   := nSet
   ::nSetValue := nOpt
Return NIL




//METHOD Exec( cCommand, lMsg, lFetch, aArray, cFile, cAlias, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate, nLogMode, cType ) CLASS SR_CONNECTION
METHOD Exec( cCommand, lMsg, lFetch, aArray, cFile, cAlias, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate, nLogMode, cType, cCodePage ) CLASS SR_CONNECTION

   local nRet := 0, i, j, n, cEste, cCampo, aDb, nFieldRec
   local aFields, nBlocks, nAllocated := 0, nLenMemo, nLinesMemo, aMemo
   local cFileTemp

   DEFAULT nLogMode  := ::nLogMode
   DEFAULT cType     := SQLLOGCHANGES_TYPE_DML

   If ::lTraceToDBF
      SR_WriteDbLog(cCommand, Self)
   EndIf
   If ::lTraceToScreen
      Alert( cCommand )
   EndIf
   If ::lLowLevSqlDbg
      SR_LogFile( ::cLowLevLogFile, { cCommand }, ::lLogDateTime )
   EndIf

   If ::lFreezed
      Return SQL_SUCCESS
   EndIf

   DEFAULT lMsg          := .T.
   DEFAULT lFetch        := .F.
   DEFAULT nMaxRecords   := 99999999999999
   DEFAULT lNoRecno      := .F.
   DEFAULT cRecnoName    := SR_RecnoName()
   DEFAULT cDeletedName  := SR_DeletedName()
   DEFAULT cCodePage  := HB_SetCodePage()

   If !Empty( cFile )
      HB_FNameSplit( cFile,, @cFileTemp )
      DEFAULT cAlias        := cFileTemp
   EndIf

   if cCommand == nil
      Return SQL_ERROR
   else
      ::cLastComm := cCommand

      If nLogMode > 0 .and. StrZero( nLogMode, SQLLOGCHANGES_SIZE )[6] == "1" .and. ((!Upper( SubStr( ltrim( cCommand ), 1, 6 ) ) $ "SELECT,") .or. cType == SQLLOGCHANGES_TYPE_LOCK )
         ::LogQuery( cCommand, cType, nLogMode )
      EndIf

      ::AllocStatement()
      ::nMiliseconds := Seconds() * 100
      nRet := ::ExecuteRaw( cCommand )

      If nLogMode > 0 .and. StrZero( nLogMode, SQLLOGCHANGES_SIZE )[5] == "1" .and. ((!Upper( SubStr( ltrim( cCommand ), 1, 6 ) ) $ "SELECT,") .or. cType == SQLLOGCHANGES_TYPE_LOCK )
         ::LogQuery( cCommand, cType, nLogMode )
      EndIf

      lFetch := lFetch .and. ::lResultSet

      if nRet != SQL_SUCCESS  .and. nRet != SQL_SUCCESS_WITH_INFO .and. (!( ("DELETE FROM " $ upper(cCommand) .OR. "UPDATE " $ upper(left(cCommand,7)) ) .and. nRet == SQL_NO_DATA_FOUND))

         ::nRetCode  = nRet
         ::cSQLError = ''
         if lMsg
            If len(cCommand) > 10000
               ::RunTimeErr("", "SQLExecDirect Error" + ;
                        CRLF + ::LastError() + CRLF + "Command sent to database : " + CRLF + SubStr(cCommand,1,2000) + " ... (command too long to display here)" )
            Else
               ::RunTimeErr("", "SQLExecDirect Error" + ;
                        CRLF + ::LastError() + CRLF + "Command sent to database : " + CRLF + cCommand )
            EndIf
         Else
            ::cSQLError := ::LastError()
         EndIf
      Else

         ::nRetCode     := nRet
         ::nMiliseconds := (Seconds()*100) - ::nMiliseconds

         If ::nMiliseconds > ::nTimeTraceMin
            SR_WriteTimeLog(cCommand, Self, ::nMiliseconds)
         EndIf

         If lFetch
            If !Empty( cFile )

               aFields := ::IniFields(.F.,,,,,cRecnoName, cDeletedName)
               aDb := {}
               If lNoRecno
                  For i = 1 to len( aFields )
                     If aFields[i,1] != cRecnoName
                        AADD( aDb, aFields[i] )
                     Else
                        nFieldRec := i
                     EndIf
                  Next
               EndIf

               if Select( cAlias ) == 0
//                  aDb := {}
                  If lNoRecno
/*                     For i = 1 to len( aFields )
                        If aFields[i,1] != cRecnoName
                           AADD( aDb, aFields[i] )
                        Else
                           nFieldRec := i
                        EndIf
                     Next*/
                     dbCreate( cFile, SR_AdjustNum(aDb), SR_SetRDDTemp() )
                  Else
                     dbCreate( cFile, SR_AdjustNum(aFields), SR_SetRDDTemp() )
                  EndIf

                  //dbUseArea( .t., SR_SetRDDTemp(), cFile, cAlias, .F. )
                  dbUseArea( .t., SR_SetRDDTemp(), cFile, cAlias, .F.,, cCodePage )
               else
                  dbSelectArea( cAlias )
               EndIf

               n := 1

               While n <= nMaxRecords .and. ((::nRetCode := ::Fetch( ,lTranslate )) == SQL_SUCCESS )

                  Append Blank

                  If nFieldRec == NIL
                     For i = 1 to len( aFields )
                        FieldPut( i, ::FieldGet( i, aFields, lTranslate ) )
                     Next
                  Else
                     For i = 1 to len( aFields )
                        Do Case
                        Case i = nFieldRec
                           ::FieldGet( i, aFields, lTranslate )
                        Case i > nFieldRec
                           FieldPut( i-1, ::FieldGet( i, aFields, lTranslate ) )
                        Case i < nFieldRec
                           FieldPut( i, ::FieldGet( i, aFields, lTranslate ) )
                        EndCase
                     Next
                  EndIf

                  n ++

               EndDo

               dbGoTop()

            ElseIf aArray == NIL

               ::cResult := ""
               n         := 0
               aFields   := ::IniFields(.F.,,,,,cRecnoName, cDeletedName)

               For i = 1 to len(aFields)
                  ::cResult += PadR( aFields[i,1], If( aFields[i,2] == "M", Max( len( aFields[i,1] ), if( ::lShowTxtMemo, 79, 30 ) ) , Max( len( aFields[i,1] ), aFields[i,3] ) ), "-" ) + " "
               Next

               ::cResult += chr(13) + chr(10)
               aMemo     := Array( len( aFields ) )

               While n <= ::nMaxTextLines .and. ((::nRetCode := ::Fetch( ,lTranslate )) == SQL_SUCCESS )

                  cEste      := ""
                  nLenMemo   := 0
                  nLinesMemo := 0

                  For i = 1 to len( aFields )
                     cCampo := ::FieldGet( i, aFields, lTranslate )
                     If aFields[i,2] == "M"
                        nLenMemo   := Max( len( aFields[i,1] ), if( ::lShowTxtMemo, 79, 30 ) )
                        nLinesMemo := Max( mlCount( cCampo, nLenMemo ), nLinesMemo )
                        cEste += memoline(cCampo,nLenMemo,1) + " "
                        aMemo[i] := cCampo
                     Else
                        cEste += PadR( SR_Val2Char( cCampo ), Max( len( aFields[i,1] ), aFields[i,3] ) ) + " "
                     EndIf
                  Next

                  ::cResult += cEste + chr(13) + chr(10)
                  n ++

                  If ::lShowTxtMemo .and. nLinesMemo > 1
                     For j = 2 to nLinesMemo
                        cEste    := ""
                        For i = 1 to len( aFields )
                           If aFields[i,2] == "M"
                              cEste += memoline(aMemo[i],nLenMemo,j) + " "
                           Else
                              cEste += Space( Max( len( aFields[i,1] ), aFields[i,3] ) ) + " "
                           EndIf
                        Next
                        ::cResult += cEste + chr(13) + chr(10)
                        n ++
                     Next
                  EndIf

               EndDo

            Else      && Retorno deve ser para Array !

               AsizeAlloc( aArray, 300 )

               If valtype( aArray ) == "A"
                  If len( aArray ) = 0
                     aSize( aArray, ARRAY_BLOCK1 )
                     nAllocated := ARRAY_BLOCK1
                  Else
                     nAllocated := len( aArray )
                  EndIf
               Else
                  aArray  := Array(ARRAY_BLOCK1)
                  nAllocated := ARRAY_BLOCK1
               EndIf

               nBlocks := 1
               n       := 0
               aFields := ::IniFields( .F.,,,,,cRecnoName, cDeletedName )

               While (::nRetCode := ::Fetch( ,lTranslate )) = SQL_SUCCESS
                  n ++
                  If n > nAllocated
                     Switch nAllocated
                     Case ARRAY_BLOCK1
                        nAllocated := ARRAY_BLOCK2
                        Exit
                     Case ARRAY_BLOCK2
                        nAllocated := ARRAY_BLOCK3
                        Exit
                     Case ARRAY_BLOCK3
                        nAllocated := ARRAY_BLOCK4
                        Exit
                     Case ARRAY_BLOCK4
                        nAllocated := ARRAY_BLOCK5
                        Exit
                     Default
                        nAllocated += ARRAY_BLOCK5
                     End

                     aSize( aArray, nAllocated )
                  EndIf

                  aArray[n] := array(len( aFields ))
                  For i = 1 to len( aFields )
                     aArray[n,i] := ::FieldGet( i, aFields, lTranslate )
                  Next
                  If n > nMaxRecords
                     Exit
                  EndIf
               EndDo
               aSize( aArray, n )
            EndIf
         Endif

         If ::nAutoCommit > 0 .and. Upper( SubStr( ltrim( cCommand ), 1, 6 ) ) $ "UPDATE,INSERT,DELETE"
            If (++::nIteractions) >= ::nAutoCommit .and. ::nTransacCount == 0
               ::Commit()
            EndIf
         EndIf

      EndIf

      ::FreeStatement()

   endif

return nRet

/*------------------------------------------------------------------------*/

METHOD AllocStatement() CLASS SR_CONNECTION

return SQL_SUCCESS

/*------------------------------------------------------------------------*/

METHOD Execute( cCommand, lErrMsg, nLogMode, cType, lNeverLog ) CLASS SR_CONNECTION

   local nRet := 0

   DEFAULT lErrMsg   := .T.
   DEFAULT lNeverLog := .F.
   DEFAULT nLogMode  := ::nLogMode
   DEFAULT cType     := SQLLOGCHANGES_TYPE_DML

   If ::lTraceToDBF
      SR_WriteDbLog(cCommand, Self)
   EndIf
   If ::lTraceToScreen
      Alert( cCommand )
   EndIf
   If ::lLowLevSqlDbg
      SR_LogFile( ::cLowLevLogFile, { cCommand }, ::lLogDateTime )
   EndIf

   If ::lFreezed
      Return 0
   EndIf

   if cCommand == nil
      nRet := SQL_ERROR
   else
      if Len( cCommand ) < 1
         ::nRetCode := nRet := 2
      else
         ::cLastComm := cCommand

         If nLogMode > 0 .and. StrZero( nLogMode, SQLLOGCHANGES_SIZE )[6] == "1" .and. ((!Upper( SubStr( ltrim( cCommand ), 1, 6 ) ) $ "SELECT,") .or. cType == SQLLOGCHANGES_TYPE_LOCK ) .and. (!lNeverLog)
            ::LogQuery( cCommand, cType, nLogMode )
         EndIf

         ::AllocStatement()
         ::nMiliseconds := Seconds() * 100

         nRet := ::ExecuteRaw( cCommand )
         ::nRetCode = nRet
         ::nMiliseconds := (Seconds()*100) - ::nMiliseconds

         If nLogMode > 0 .and. StrZero( nLogMode, SQLLOGCHANGES_SIZE )[5] == "1" .and. ((!Upper( SubStr( ltrim( cCommand ), 1, 6 ) ) $ "SELECT,") .or. cType == SQLLOGCHANGES_TYPE_LOCK ) .and. (!lNeverLog)
            ::LogQuery( cCommand, cType, nLogMode, ::nMiliseconds )
         EndIf

         If ::nMiliseconds > ::nTimeTraceMin
            SR_WriteTimeLog(cCommand, Self, ::nMiliseconds)
         EndIf

         if lErrMsg .and. nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO .and. (!( ("DELETE FROM " $ upper(cCommand) .OR. "UPDATE " $ upper(left(cCommand,7)) ) .and. nRet == SQL_NO_DATA_FOUND))

            ::RunTimeErr( "","SQLExecDirect Error" + ;
                     CRLF + ::LastError() + CRLF +;
                     "Command : " + cCommand + CRLF + ;
                     "hStmt   : " + SR_Val2Char( ::hStmt ) )
         endif

         If ::nAutoCommit > 0 .and. Upper( SubStr( ltrim( cCommand ), 1, 6 ) ) $ "UPDATE,INSERT,DELETE"
            If (++::nIteractions) >= ::nAutoCommit .and. ::nTransacCount == 0
               ::Commit()
            EndIf
         EndIf

      endif
   endif

return nRet

/*------------------------------------------------------------------------*/

METHOD LastError() CLASS SR_CONNECTION

return ""

/*------------------------------------------------------------------------*/

METHOD RPCTalk( cSend ) CLASS SR_CONNECTION

   (cSend)

Return ""

/*------------------------------------------------------------------------*/

METHOD DetectTargetDb() CLASS SR_CONNECTION

   Local cTargetDB   := Upper( ::cSystemName )
   Local aVers

   ::nSystemID := SYSTEMID_UNKNOW

   Do Case
   Case "ORACLE" $ cTargetDB
      ::nSystemID := SYSTEMID_ORACLE
   Case ("MICROSOFT" $ cTargetDB .and. "SQL" $ cTargetDB .and. "SERVER" $ cTargetDB .and.( "10.25" $ ::cSystemVers) )
      ::nSystemID := SYSTEMID_AZURE
   Case "MICROSOFT" $ cTargetDB .and. "SQL" $ cTargetDB .and. "SERVER" $ cTargetDB .and. "6.5" $ ::cSystemVers
      ::nSystemID := SYSTEMID_MSSQL6
   Case ("SQL Server" $ cTargetDB .and. "00.53.0000" $ ::cSystemVers) .or. ("MICROSOFT SQL SERVER" $ cTargetDB)
      ::nSystemID := SYSTEMID_MSSQL7
      aVers := hb_atokens( ::cSystemVers , '.' ) 
      IF val(aVers[1]) >= 8
         ::lClustered := .T.
      ENDIF         
         //culik 30/12/2011 adicionado para indicar se e  sqlserver versao 2008 ou superior
      IF val(aVers[1]) >= 10   
         ::lSqlServer2008 := .T.
      ENDIF   

   Case ("MICROSOFT" $ cTargetDB .and. "SQL" $ cTargetDB .and. "SERVER" $ cTargetDB .and.( "7.0" $ ::cSystemVers .or. "8.0" $ ::cSystemVers .or. "9.0" $ ::cSystemVers .or. "10.00" $ ::cSystemVers .or. "10.50" $ ::cSystemVers  .or. "11.00" $ ::cSystemVers   )) //.or. ( "SQL SERVER" $ cTargetDB .and. !("SYBASE" $ cTargetDB))
      ::nSystemID := SYSTEMID_MSSQL7
      aVers := hb_atokens( ::cSystemVers , '.' ) 
      IF val(aVers[1]) >= 8
         ::lClustered := .T.
      ENDIF         
   Case "ANYWHERE" $ cTargetDB
      ::nSystemID := SYSTEMID_SQLANY
   Case "SYBASE" $ cTargetDB .or. "SQL SERVER" $ cTargetDB
      ::nSystemID := SYSTEMID_SYBASE
   Case "ACCESS" $ cTargetDB
      ::nSystemID := SYSTEMID_ACCESS
   Case "INGRES" $ cTargetDB
      ::nSystemID := SYSTEMID_INGRES
   Case "SQLBASE" $ cTargetDB
      ::nSystemID := SYSTEMID_SQLBAS
   Case "INFORMIX" $ cTargetDB
      ::nSystemID := SYSTEMID_INFORM
   Case "ADABAS" $ cTargetDB
      ::nSystemID := SYSTEMID_ADABAS
      ::lComments := .F.
   Case "POSTGRESQL" $ cTargetDB
      ::nSystemID := SYSTEMID_POSTGR
   Case "DB2" $ cTargetDB .or. "SQLDS/VM" $ cTargetDB
      ::nSystemID := SYSTEMID_IBMDB2
      ::lComments := .F.
      If "05.03" $ ::cSystemVers       // Detects AS/400 from Win98 ODBC
         ::cSystemName := "DB2/400"
         cTargetDB     := "DB2/400"
      EndIf
   Case "MYSQL" $ cTargetDB .and.  SubStr( alltrim(::cSystemVers), 1, 3 ) >= "4.1"
      ::nSystemID := SYSTEMID_MYSQL
   Case "MARIADB" $ cTargetDB
      ::nSystemID := SYSTEMID_MARIADB
   Case "FIREBIRD" $ cTargetDb .or. "INTERBASE" $ cTargetdb
      ::nSystemID := SYSTEMID_FIREBR 
      aVers := hb_atokens( ::cSystemVers , '.' ) 
      IF val(aVers[1]) >= 3
         ::nSystemID := SYSTEMID_FIREBR3
      Endif
   Case "INTERSYSTEMS CACHE" $ cTargetDb
      ::nSystemID := SYSTEMID_CACHE
      ::lComments := .F.
   Case "OTERRO" $ cTargetDb
      ::nSystemID := SYSTEMID_OTERRO
      ::lComments := .F.
   Case "PERVASIVE.SQL" $ cTargetDb
      ::nSystemID := SYSTEMID_PERVASIVE
      ::lComments := .F.
   OtherWise
      If !::lQueryOnly
         ::RuntimeErr( , SR_Msg(22) + cTargetDB + " " + ::cSystemVers )
      EndIf
      ::lComments := .F.
   EndCase

   ::cTargetDB    = cTargetDB

Return NIL

/*------------------------------------------------------------------------*/

METHOD End() CLASS SR_CONNECTION

   ::hEnv  = nil
   ::hDbc  = nil

return nil

/*------------------------------------------------------------------------*/

METHOD GetInfo( nType ) CLASS SR_CONNECTION
   (nType)
return ""

/*------------------------------------------------------------------------*/

METHOD GetOptions( nType ) CLASS SR_CONNECTION
   (nType)
return ""

/*------------------------------------------------------------------------*/

METHOD SetOptions( nType, uBuffer ) CLASS SR_CONNECTION
   (nType)
   (uBuffer)
Return SQL_SUCCESS

/*------------------------------------------------------------------------*/

METHOD SetStmtOptions( nType, uBuffer ) CLASS SR_CONNECTION
   (nType)
   (uBuffer)
Return SQL_SUCCESS

/*------------------------------------------------------------------------*/

METHOD Commit( lNoLog ) CLASS SR_CONNECTION

   DEFAULT lNoLog := .F.

   If ::lTraceToDBF
      SR_WriteDbLog("COMMIT", Self)
   EndIf
   If ::lTraceToScreen
      Alert( "COMMIT" )
   EndIf
   If ::lLowLevSqlDbg
      SR_LogFile( ::cLowLevLogFile, { "COMMIT" }, ::lLogDateTime )
   EndIf

   ::nIteractions := 0

   If ::nLogMode > 0 .and. StrZero( ::nLogMode, SQLLOGCHANGES_SIZE )[2] == "1" .and. (!lNoLog)
      If ::cQueryOwner == NIL
         If !Empty( SR_GetGlobalOwner() )
            ::cQueryOwner := alltrim( SR_GetGlobalOwner() )
         ElseIf !Empty( ::oSql:cOwner )
            ::cQueryOwner := alltrim(::cOwner)
         EndIf
         If (!Empty( ::cQueryOwner )) .and. ::cQueryOwner[-1] != "."
            ::cQueryOwner += "."
         EndIf
         If Empty( ::cQueryOwner )
            ::cQueryOwner := ""
         EndIf
      EndIf

      If StrZero( ::nLogMode, SQLLOGCHANGES_SIZE )[4] == "1" .or. ::oSqlTransact == NIL
         Self:execute( "DELETE FROM " + ::cQueryOwner + "SR_MGMNTLOGCHG WHERE SPID_ = " + str(::uSid),,,,.T. )
      Else
         ::oSqlTransact:execute( "DELETE FROM " + ::cQueryOwner + "SR_MGMNTLOGCHG WHERE SPID_ = " + str(::uSid),,,,.T. )
         ::oSqlTransact:FreeStatement()
         ::oSqlTransact:commit( .T. )
      EndIf
   EndIf

Return SQL_SUCCESS

/*------------------------------------------------------------------------*/

METHOD RollBack() CLASS SR_CONNECTION

   If ::lTraceToDBF
      SR_WriteDbLog("ROLLBACK", Self)
   EndIf
   If ::lTraceToScreen
      Alert( "ROLLBACK" )
   EndIf
   If ::lLowLevSqlDbg
      SR_LogFile( ::cLowLevLogFile, { "ROLLBACK" }, ::lLogDateTime )
   EndIf

   ::nIteractions := 0

   If ::nLogMode > 0 .and. StrZero( ::nLogMode, SQLLOGCHANGES_SIZE )[2] == "1"
      If ::cQueryOwner == NIL
         If !Empty( SR_GetGlobalOwner() )
            ::cQueryOwner := alltrim( SR_GetGlobalOwner() )
         ElseIf !Empty( ::oSql:cOwner )
            ::cQueryOwner := alltrim(::cOwner)
         EndIf
         If (!Empty( ::cQueryOwner )) .and. ::cQueryOwner[-1] != "."
            ::cQueryOwner += "."
         EndIf
         If Empty( ::cQueryOwner )
            ::cQueryOwner := ""
         EndIf
      EndIf

      If StrZero( ::nLogMode, SQLLOGCHANGES_SIZE )[4] == "1" .or. ::oSqlTransact == NIL
         Self:execute( "DELETE FROM " + ::cQueryOwner + "SR_MGMNTLOGCHG WHERE SPID_ = " + str(::uSid),,,,.T. )
      Else
         ::oSqlTransact:execute( "DELETE FROM " + ::cQueryOwner + "SR_MGMNTLOGCHG WHERE SPID_ = " + str(::uSid),,,,.T. )
         ::oSqlTransact:FreeStatement()
         ::oSqlTransact:commit( .T. )
      EndIf
   EndIf

Return SQL_SUCCESS

/*------------------------------------------------------------------------*/

METHOD RuntimeErr( cOperation, cErr ) CLASS SR_CONNECTION

   Local oErr := ErrorNew()
   Local cDescr

   DEFAULT cOperation := ::ClassName()
   DEFAULT cErr := "RunTimeError"

   cDescr := alltrim(cErr) + CRLF +;
             "Steatment handle  : " + SR_Val2Char( ::hStmt )+CRLF+;
             "Connection handle : " + SR_Val2Char( ::hDbc )+CRLF+;
             "RetCode           : " + SR_Val2Char( ::nRetCode )+CRLF

   ::RollBack()

   oErr:genCode      := 99
   oErr:CanDefault   := .F.
   oErr:Severity     := ES_ERROR
   oErr:CanRetry     := .T.
   oErr:CanSubstitute:= .F.
   oErr:Description  := cDescr + " - RollBack executed."
   oErr:subSystem    := ::ClassName()
   oErr:operation    := cOperation
   oErr:OsCode       := 0

   SR_LogFile( "sqlerror.log", { cDescr, SR_GetStack() } )

   Throw( oErr )

Return NIL

/*------------------------------------------------------------------------*/

Function SR_AdjustNum(a)

   local b := aClone(a)
   local i

   For i = 1 to len(b)

      If lNwgOldCompat
         If b[i,2] = "N"
            b[i,3] ++
         EndIf
      EndIf

      If b[i,2] = "N" .and. b[i,3] > 18
         b[i,3] := 19
      EndIf

      If lNwgOldCompat
         If b[i,2] = "N" .and. b[i,4] >= (b[i,3] - 1)
            b[i,4] := abs(b[i,3] - 2)
         EndIf
      EndIf

      If b[i,2] = "M"
         b[i,3] := 10
      EndIf

   Next

Return b

/*------------------------------------------------------------------------*/

METHOD Connect( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace,;
            cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit, nTimeout ) CLASS SR_CONNECTION

   local hEnv := nil, hDbc := nil
   local cVersion := "", cSystemVers := "", cBuff := ""
   local aCon, aItem, aToken

   DEFAULT nVersion := 1, lTrace := .F., nPreFetch := 0
   DEFAULT cDSN := ""
   DEFAULT lCounter    := .f.
   DEFAULT lAutoCommit := .F.  /* by default support transactions */

   ::lAutoCommit  = lAutoCommit
   ::nVersion     = nVersion
   ::cOwner       = cOwner
   ::lCounter     = lCounter
   ::nRetCode     = 0
   ::nSizeMaxBuff = nSizeMaxBuff
   ::nPreFetch    = nPrefetch
   ::lTrace       = lTrace
   ::nSelMeth     = nSelMeth
   ::nEmptyMode   = nEmptyMode      && ( 0 = Grava NULLS, 1 = Grava o campo no próprio tamanho )
   ::nDateMode    = nDateMode       && ( 0 = Utiliza o padrão do banco, como DATETIME ou TIMESTAMP, 1 = grava em Char(8) )
   ::cDsn         = ""
   ::cUser        = ""
   ::cPassword    = ""
   ::cPort        = nil
   ::cHost        = ""
   ::cDBS         = ""
   ::cHost        = ""
   ::cDrv         = ""
   ::cDTB         = ""
   ::cNetLibrary  = ""
   ::oOdbc        = Self            && NewAge backwards compatible...
   ::oSql         = Self            && NewAge backwards compatible...
   ::cCharSet     = NIL             && should be NIL or FB does not work
   ::lCluster     = .F.
   ::lClustered   := .F.   

   If ::lCounter
      ::lLowLevSqlDbg   = (!Empty( GetEnv( "QUERYDEBUGCOUNTER" ) )) .and. upper(GetEnv( "QUERYDEBUGCOUNTER" )) $ "Y,S,TRUE"
   Else
      ::lLowLevSqlDbg   = (!Empty( GetEnv( "QUERYDEBUG" ) )) .and. upper(GetEnv( "QUERYDEBUG" )) $ "Y,S,TRUE"
   EndIf

   ::oHashActiveWAs  = SqlFastHash():new()

   if cConnect == NIL .or. empty( cConnect )
      SR_MsgLogFile( "Invalid connection string : " + SR_Val2Char(cConnect) )
      ::nRetCode = SQL_ERROR
      Return Self
   Else
      aCon := hb_atokens(cConnect,";")

      For each aItem in aCon

         If Empty( aItem )
            Loop
         EndIf

         aToken := hb_atokens(aItem,"=")
         cBuff = alltrim(Upper( aToken[1] ))
         If len( aToken ) = 1
            aadd( aToken, "" )
         EndIf
         Do Case
         Case cBuff == "UID" .or. cBuff == "UIID" .or. cBuff == "USR"
            ::cUser   += aToken[2]
         Case cBuff == "PWD"
            ::cPassword   += aToken[2]
         Case cBuff == "DSN"
            ::cDSN   += aToken[2]
         Case cBuff == "DBS"
            ::cDBS   += aToken[2]
         Case cBuff == "HST" .or. cBuff == "OCI" .or. cBuff == "MYSQL" .or. cBuff == "PGS" .or. cBuff == "SERVER" .or. cBuff == "MARIA"
            ::cHost   += aToken[2]
         Case cBuff == "PRT"
            ::cPort   := Val(sr_val2char(aToken[2]))
         Case cBuff == "DRV" .or. cBuff == "DRIVER"
            ::cDRV   += aToken[2]
         Case cBuff == "CHARSET"
            ::cCharSet := aToken[2]
         Case cBuff == "AUTOCOMMIT"
            ::nAutoCommit := Val( aToken[2] )
         Case cBuff == "DTB" .or. cBuff == "FB" .or. cBuff == "FIREBIRD" .or. cBuff == "FB3" .or. cBuff == "FIREBIRD3" .or. cBuff == "IB" .or. cBuff == "TNS" .or. cBuff == "DATABASE"
            ::cDTB   += aToken[2]
         Case cBuff == "TABLESPACE_DATA"
            ::cDsnTblData := aToken[2]
         Case cBuff == "TABLESPACE_INDEX"
            ::cDsnTblIndx := aToken[2]
         Case cBuff == "TABLESPACE_LOB"
            ::cDsnTblLob := aToken[2]
         Case cBuff == "CLUSTER"
            ::lCluster  := Upper( aToken[2] ) $ "Y,S,TRUE"
         Case cBuff == "OWNER" //.and. empty( ::cOwner )
            ::cOwner := aToken[2]
            If !Empty( ::cOwner ) .and. ::cOwner[-1] != "."
               ::cOwner += "."
            EndIf
         Case cBuff == "NETWORK" .or. cBuff == "LIBRARY" .or. cBuff == "NETLIBRARY"
            ::cNetLibrary := aToken[2]
         CASE cBuff == "APP"   
            ::cApp :=  aToken[2]
         CASE cBuff == "SSLCERT"      
            ::sslcert := aToken[2]
         CASE cBuff == "SSLKEY"      
            ::sslkey := aToken[2]
         CASE cBuff == "SSLROOTCERT"      
            ::sslrootcert := aToken[2]
         CASE cBuff == "SSLCRL"      
            ::sslcrl    := aToken[2]   
         case cBuff == "COMPRESS"
            ::lCompress := Upper( aToken[2] ) $ "Y,S,TRUE"
//         OtherWise
//            SR_MsgLogFile( "Invalid connection string entry : " + cBuff + " = " + SR_Val2Char(aToken[2]) )
         EndCase
      Next
   EndIf

   ::nSystemID := SYSTEMID_UNKNOW

   ::ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace, alltrim( cConnect ), nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit, nTimeout )

   Switch ::nSystemID
   Case SYSTEMID_ORACLE
      ::cLockWait := " WAIT " + str( int(::nLockWaitTime) )
      Exit
   Default
      ::cLockWait := ""
   End

Return Self

/*------------------------------------------------------------------------*/

METHOD SQLType( nType, cName, nLen ) CLASS SR_CONNECTION

   local cType := "U"

   (cName)

   DEFAULT nLen := 0

   do case
   case (nType == SQL_CHAR .or. nType == SQL_VARCHAR .or. nType == SQL_NVARCHAR .or. nType == SQL_GUID) .and. If(lNwgOldCompat, nLen != 4000 .and. nLen != 2000, .T. )
      cType = "C"
   Case nType == SQL_SMALLINT .or. nType == SQL_TINYINT
      If ::lQueryOnly
         cType := "N"
      Else
         cType := "L"
      EndIf
   case nType == SQL_BIT
      cType = "L"
   case nType == SQL_NUMERIC .OR. nType == SQL_DECIMAL .OR. ;
      nType == SQL_INTEGER .OR. nType == SQL_BIGINT .OR. ;
      nType == SQL_FLOAT .OR. nType == SQL_REAL .OR. ;
      nType == SQL_DOUBLE
      cType = "N"
   //case nType == SQL_DATE .or. nType == SQL_TIMESTAMP .or. nType == SQL_TYPE_TIMESTAMP .or. nType == SQL_TYPE_DATE
   case nType == SQL_DATE .or. nType == SQL_TYPE_DATE
      cType = "D"
   case nType == SQL_TIME
      if (::nSystemID == SYSTEMID_POSTGR .or. ::nSystemID == SYSTEMID_MYSQL  .or. ::nSystemID == SYSTEMID_MARIADB .or. ::nSystemID == SYSTEMID_FIREBR .or. ::nSystemID == SYSTEMID_FIREBR3  )
         cType := "T"
      else
         cType := "C"
      endif
   case nType == SQL_LONGVARCHAR .or.  nType == SQL_DB2_CLOB .or. nType == SQL_FAKE_LOB .or.  ntype == SQL_LONGVARBINARY .or. (nType == SQL_VARBINARY .and. ::nSystemID != SYSTEMID_MSSQL7) 
      cType := "M"
   case nType == SQL_VARBINARY  .and. ::nSystemID == SYSTEMID_MSSQL7
      cType := "V"  
   case nType == SQL_TIMESTAMP .or. nType == SQL_TYPE_TIMESTAMP  .or. nType == SQL_DATETIME
      cType := 'T'   
   endcase

   if cType == "U"
      SR_MsgLogFile( SR_Msg(2) + SR_Val2CharQ( nType ) )
   EndIf

return cType

/*------------------------------------------------------------------------*/

METHOD SQLLen( nType, nLen, nDec )  CLASS SR_CONNECTION

   local cType := "U"

   DEFAULT nDec := -1

   do case
   case (nType == SQL_CHAR .or. nType == SQL_VARCHAR .or. nType == SQL_NVARCHAR) .and. If(lNwgOldCompat, nLen != 4000 .and. nLen != 2000, .T. )

   Case nType == SQL_SMALLINT .or. nType == SQL_TINYINT
      If ::lQueryOnly
         nLen := 10
      Else
         nLen := 1
      EndIf

   case nType == SQL_BIT
        nLen := 1

   case nType == SQL_NUMERIC  .or. nType == SQL_DECIMAL  .OR. ;
        nType == SQL_INTEGER  .OR. ;
        nType == SQL_FLOAT    .or. nType == SQL_REAL     .OR. ;
        nType == SQL_DOUBLE

      If nLen > 19 .and. nDec > 10 .and. !( nLen = 38 .and. nDec = 0 )
         nLen := 20
         nDec := 6
      EndIf

      If !( nLen = 38 .and. nDec = 0 )
         nLen := min( nLen, 20 )
         nLen := max( nLen, 1 )
      EndIf

   case nType == SQL_DATE .or. nType == SQL_TIMESTAMP .or. nType == SQL_TYPE_TIMESTAMP .or. nType == SQL_TYPE_DATE .or. ntype == SQL_DATETIME
     nLen := 8

   case nType == SQL_TIME
     nLen := 8

   case nType == SQL_LONGVARCHAR .or. nType == SQL_LONGVARBINARY .or. nType == SQL_FAKE_LOB .or. nType == SQL_VARBINARY
     nLen := 10

   Case nType == SQL_GUID
      nLen := 36

   endcase

return nLen

/*------------------------------------------------------------------------*/

Function SR_SetNwgCompat(l)

   Local lOld := lNwgOldCompat

   If l != NIL
      lNwgOldCompat := l
   EndIf

Return lOld

/*------------------------------------------------------------------------*/

Function SR_AutoCommit( nSet )

   local nOld, oSql

   oSql := SR_GetConnection()

   nOld := oSql:nAutoCommit

   If valtype( nSet ) == "N"
      oSql:nAutoCommit := nSet
   EndIf

Return nOld

/*------------------------------------------------------------------------*/

Function SR_AllInCache( lSet )

   local lOld, oSql

   oSql := SR_GetConnection()

   lOld := oSql:lAllInCache

   If valtype( lSet ) == "L"
      oSql:lAllInCache := lSet
   EndIf

Return lOld

/*------------------------------------------------------------------------*/


function SR_SetTraceLog(cLog) 
   Local cOld, oSql

   oSql := SR_GetConnection()

   cOld := oSql:cLowLevLogFile

   If valtype( cLog ) == "C"
      oSql:cLowLevLogFile := cLog
   EndIf

Return cOld