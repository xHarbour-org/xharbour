/* $CATEGORY$SQLRDD/ODBC$FILES$sql.lib$HIDE$
* SQLRDD ODBC Connection Class
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
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
#define ARRAY_BLOCK      500

#define SQL_LONGDATA_COMPAT          1253
#define SQL_ATTR_LONGDATA_COMPAT    SQL_LONGDATA_COMPAT
#define SQL_LD_COMPAT_YES            1

/*------------------------------------------------------------------------*/

CLASS SR_ODBC FROM SR_CONNECTION

   Data aCurrLine

   METHOD ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace, cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit )
   METHOD End()
   METHOD GetInfo( nType )
   METHOD SetOptions( nType, uBuffer )
   METHOD GetOptions( nType )
   METHOD LastError()
   METHOD Commit()
   METHOD RollBack()
   METHOD IniFields( lReSelect, cTable, cCommand, lLoadCache, cWhere, cRecnoName, cDeletedName )
   METHOD ExecuteRaw( cCommand )
   METHOD AllocStatement()
   METHOD SetStmtOptions( nType, uBuffer )
   METHOD FreeStatement()
   METHOD FetchRaw( lTranslate, aFields )
   METHOD FieldGet( nField, aFields, lTranslate )
   METHOD MoreResults( aArray )
   METHOD WriteMemo( cFileName, nRecno, cRecnoName, aColumnsAndData )
   METHOD DriverCatTables()
   METHOD Getline( aFields, lTranslate, aArray )
   METHOD FetchMultiple( lTranslate, aFields, aCache, nCurrentFetch, aInfo, nDirection, nBlockPos, hnRecno, lFetchAll, aFetch, uRecord, nPos )

ENDCLASS

/*------------------------------------------------------------------------*/

METHOD FetchMultiple( lTranslate, aFields, aCache, nCurrentFetch, aInfo, nDirection, hnRecno, lFetchAll, aFetch, uRecord, nPos ) CLASS SR_ODBC

   DEFAULT lTranslate := .T.

Return SR_ODBCGETLINES( ::hStmt, 4096, aFields, aCache, ::nSystemID, lTranslate, nCurrentFetch, aInfo, nDirection, hnRecno, lFetchAll, aFetch, uRecord, nPos )

/*------------------------------------------------------------------------*/

METHOD Getline( aFields, lTranslate, aArray )  CLASS SR_ODBC

   Local i

   DEFAULT lTranslate := .T.

   If aArray == NIL
      aArray := Array(len( aFields ))
   ElseIf len( aArray ) < len( aFields )
      aSize( aArray, len( aFields ) )
   EndIf

   If ::aCurrLine == NIL
      SR_ODBCLINEPROCESSED( ::hStmt, 4096, aFields, ::lQueryOnly, ::nSystemID, lTranslate, aArray )
      ::aCurrLine := aArray
      Return aArray
   EndIf

   For i = 1 to len( aArray )
      aArray[i] := ::aCurrLine[ i ]
   Next

Return aArray

/*------------------------------------------------------------------------*/

METHOD DriverCatTables()  CLASS SR_ODBC

   Local nRet, aArray := Array(ARRAY_BLOCK1)
   Local nAllocated, nBlocks, aFields, n := 0

   ::AllocStatement()
   nRet  := SR_Tables( ::hStmt )

   If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO

      nAllocated := ARRAY_BLOCK1
      nBlocks    := 1
      n          := 0
      aFields    := ::IniFields( .F.,,,,,, )

      While (::nRetCode := ::Fetch()) = SQL_SUCCESS

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

         aArray[n] := { ::FieldGet( 3, aFields, .F. ) }
      EndDo
   EndIf

   ::FreeStatement()
   aSize( aArray, n )

Return aArray

/*------------------------------------------------------------------------*/

METHOD MoreResults( aArray, lTranslate )  CLASS SR_ODBC

   local nRet, i, n
   Static aFieldsMore

   DEFAULT lTranslate := .T.

   nRet := SR_MoreResults(::hStmt)

   If nRet == SQL_SUCCESS

      DEFAULT aArray := {}
      n := 1
      If aFieldsMore == NIL
         aFieldsMore := ::IniFields(.F.,,,,,SR_RecnoName(), SR_DeletedName())
      EndIf

      While (::nRetCode := ::FetchRaw( lTranslate, aFieldsMore )) = SQL_SUCCESS
         AADD( aArray, Array(len( aFieldsMore )) )
         For i = 1 to len( aFieldsMore )
            aArray[n,i] := ::FieldGet( i, aFieldsMore, lTranslate )
         Next
         n ++
      EndDo

   EndIf

Return nRet

/*------------------------------------------------------------------------*/

METHOD FieldGet( nField, aFields, lTranslate ) CLASS SR_ODBC

   If ::aCurrLine == NIL
      DEFAULT lTranslate := .T.
      ::aCurrLine := array( LEN( aFields ) )
      SR_ODBCLINEPROCESSED( ::hStmt, 4096, aFields, ::lQueryOnly, ::nSystemID, lTranslate, ::aCurrLine )
   EndIf

return ::aCurrLine[nField]

/*------------------------------------------------------------------------*/

METHOD FetchRaw( lTranslate, aFields ) CLASS SR_ODBC

   ::nRetCode := SQL_ERROR
   DEFAULT aFields    := ::aFields
   DEFAULT lTranslate := .T.

   If ::hStmt != NIL
      ::nRetCode := SR_Fetch( ::hStmt )
      ::aCurrLine := NIL
   Else
      ::RunTimeErr("", "SQLFetch - Invalid cursor state" + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
   EndIf

Return ::nRetCode

/*------------------------------------------------------------------------*/

METHOD FreeStatement() CLASS SR_ODBC

   if !empty( ::hStmt ) //  != NIL //!= 0
      if SR_FreeStm( ::hStmt, SQL_DROP ) != SQL_SUCCESS
         ::RunTimeErr("", "SQLFreeStmt [DROP] error" + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
      endif
      ::hStmt := nil
   endif

Return NIL

/*------------------------------------------------------------------------*/

METHOD AllocStatement() CLASS SR_ODBC

   local hStmtLocal := nil, nRet := 0

   ::FreeStatement()

   if ( nRet := SR_AllocSt( ::hDbc, @hStmtLocal ) ) == SQL_SUCCESS
      ::hStmt = hStmtLocal
   else
      ::nRetCode = nRet
      ::RunTimeErr("", "SQLAllocStmt [NEW] Error" + CRLF + CRLF + ::LastError() + CRLF + CRLF+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
      return nil
   endif

   If ::lSetNext .and. nRet == SQL_SUCCESS
      ::lSetNext  := .F.
      nRet := ::SetStmtOptions( ::nSetOpt, ::nSetValue )
      If nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
         SR_MsgLogFile( SR_Msg(23) + " (" + alltrim(str(nRet)) + ") : " + ::LastError() )
      EndIf
   EndIf

return nRet

/*------------------------------------------------------------------------*/

METHOD IniFields(lReSelect, cTable, cCommand, lLoadCache, cWhere, cRecnoName, cDeletedName) CLASS SR_ODBC

   local n, nFields := 0
   local nType := 0, nLen := 0, nNull := 0, cName
   local _nLen, _nDec
   local cType, nLenField, nNameLen
   local aFields := {}
   local nDec := 0, nSoma, nRet, cVlr := "" /*, nBfLn, nOut*/

   DEFAULT lReSelect    := .T.
   DEFAULT lLoadCache   := .F.
   DEFAULT cWhere       := ""
   DEFAULT cRecnoName   := SR_RecnoName()
   DEFAULT cDeletedName := SR_DeletedName()

   If lReSelect
      If !Empty( cCommand )
         nRet := ::Execute( cCommand + if(::lComments," /* Open Workarea with custom SQL command */",""), .F. )
      Else
         nRet := ::Execute( "SELECT A.* FROM " + cTable + " A " + if(lLoadCache, cWhere + " ORDER BY A." + cRecnoName, " WHERE 1 = 0") + if(::lComments," /* Open Workarea */",""), .F. )
      EndIf

      If nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
         return nil
      EndIf
   EndIf

   if ( ::nRetCode := SR_NumRes( ::hStmt, @nFields ) ) != SQL_SUCCESS
      ::RunTimeErr("", "SqlNumResultCols Error" + chr(13)+chr(10)+ chr(13)+chr(10)+;
               "Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
      return nil
   endif

   aFields   := Array( nFields )
   ::nFields := nFields

   for n = 1 to nFields

      nDec := 0
      nSoma:= 0

      if ( ::nRetCode := SR_Describ( ::hStmt, n, @cName, 255, @nNameLen, @nType, @nLen, @nDec, @nNull,::nSystemID ) ) != SQL_SUCCESS
         ::RunTimeErr("", "SQLDescribeCol Error" + chr(13)+chr(10)+ ::LastError() + chr(13)+chr(10)+;
                          "Last command sent to database : " + ::cLastComm )
         return nil
      else
         _nLen := nLen
         _nDec := nDec
         if ( nType == SQL_DOUBLE .or. nType == SQL_FLOAT ) .and. nDec == 0
            nDec = 6
            nSoma= 6
         endif

         If (nLen == 2000 .or. nLen == 4000) .and. SR_SetNwgCompat()
            nType := SQL_FAKE_LOB
         EndIf

         if  ::nSystemID == SYSTEMID_ORACLE  .and. nLen == 19 .and. (nType == SQL_TIMESTAMP .or. nType == SQL_TYPE_TIMESTAMP  .or. nType == SQL_DATETIME)
             nType := SQL_DATE
         ENDIF
         if ::nsystemId ==  SYSTEMID_MSSQL7
            if ( ntype == SQL_TYPE_DATE ) .and.  SR_GETSQL2008NEWTYPES() .and.  ::lSqlServer2008
               nType := SQL_DATE
            elseif ( nType == SQL_TIMESTAMP .or. nType == SQL_TYPE_TIMESTAMP  .or. nType == SQL_DATETIME ) .and.  SR_GETSQL2008NEWTYPES() .and.  ::lSqlServer2008

            elseif  (nType == SQL_TIMESTAMP .or. nType == SQL_TYPE_TIMESTAMP  .or. nType == SQL_DATETIME) .and. !SR_GETSQL2008NEWTYPES() //.and.   !::lSqlServer2008
            nType := SQL_DATE
         endif
         endif

         cName     := upper(alltrim( cName ))
         cType     := ::SQLType( nType, cName, nLen )
         nLenField := ::SQLLen( nType, nLen, @nDec ) + nSoma
         If ::nSystemID == SYSTEMID_ORACLE .and. (!::lQueryOnly) .and. cType == "N" .and. nLenField == 38 .and. nDec == 0
            cType     := "L"
            nLenField := 1
         EndIf
/*
         If ::nSystemID == SYSTEMID_POSTGR
            nRet := SR_ColAttribute( ::hStmt, n, SQL_DESC_NULLABLE, @cVlr, 64, @nBfLn, @nOut )
            nNull := nOut
         EndIf
*/
         If cType == "U"
            ::RuntimeErr( "", SR_Msg(21) + cName + " : " + str( nType ) )
         Else
            aFields[n] := { cName, cType, nLenField, if(cType=="D", 0, nDec), nNull >= 1 , nType, nLen, n, _nDec }
         EndIf

      endif
   next

   ::aFields := aFields

   If lReSelect .and. !lLoadCache
      ::FreeStatement()
   EndIf

return aFields

/*------------------------------------------------------------------------*/

METHOD LastError() CLASS SR_ODBC

   local cClassError := space(200), nType := 0, cMsgError := space(200), nRealLen := 0

   SR_Error( ::hEnv, ::hDbc, ::hStmt, @cClassError, @nType, @cMsgError, 256, nRealLen )

return SR_Val2Char(cClassError) + " - " + AllTrim( SR_Val2Char( nType ) ) + " - " + SR_Val2Char(cMsgError)

/*------------------------------------------------------------------------*/

METHOD ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace,;
            cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit ) CLASS SR_ODBC

   local hEnv := nil, hDbc := nil
   local nret, cVersion := "", cSystemVers := "", cBuff := "", aRet := {}

   ( cDSN)
   (cUser)
   (cPassword)
   (nVersion)
   (cOwner)
   (nSizeMaxBuff)
   (lTrace)
   (nPrefetch)
   (nselMeth)
   (nEmptyMode)
   (nDateMode)
   (lCounter)
   (lAutoCommit)

   ::lNative := .F.

   if ( nRet := SR_AllocEn( @hEnv ) ) == SQL_SUCCESS
      ::hEnv = hEnv
   else
      ::nRetCode:=nRet
      SR_MsgLogFile( "SQLALLOCENV Error" + str( nRet ) )
      return Self
   endif

   if (nret := SR_ALLOCCO( hEnv, @hDbc )) == SQL_SUCCESS
      ::hDbc = hDbc
   else
      ::nRetCode:=nRet
      SR_MsgLogFile( "SQLALLOCCONNECT Error" + str( nRet ) )
      return Self
   endif

   If !Empty( ::cDTB )
      SR_SetCOnnectAttr( hDbc, SQL_ATTR_CURRENT_CATALOG, ::cDTB, len( ::cDTB ) )
   EndIf

   cConnect := alltrim( cConnect )
   nRet := SR_DriverC( hDbc, @cConnect )

   if nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
      ::nRetCode = nRet
      SR_MsgLogFile( "SQLDriverConnect Error: No ODBC connection established " + ::LastError() )
      Return Self
   else
      ::cConnect = cConnect
      SR_GetInfo( hDbc, SQL_DBMS_NAME, @cTargetDB )
      SR_GetInfo( hDbc, SQL_DBMS_VER , @cSystemVers )
   EndIf

   ::cSystemName := cTargetDB
   ::cSystemVers := cSystemVers

   ::DetectTargetDb()

   Switch ::nSystemID
   Case SYSTEMID_IBMDB2
      SR_SetConnectAttr( hDbc, SQL_ATTR_LONGDATA_COMPAT, SQL_LD_COMPAT_YES )
      Exit
   Case SYSTEMID_MSSQL7
   Case SYSTEMID_AZURE
      ::exec( "select cast( @@spid as numeric )", .T., .T., @aRet )
      If len( aRet ) > 0
         ::uSid := val(str(aRet[1,1],8,0))
      EndIf
      Exit
   End

return Self

/*------------------------------------------------------------------------*/

METHOD End() CLASS SR_ODBC

   Local nRet

   ::Commit( .T. )

   If ( nRet := SR_Disconn( ::hDbc )) != SQL_SUCCESS
      SR_MsgLogFile( "Error disconnecting : " + str(nRet) + CRLF + ::LastError() )
   Else
      If ( nRet := SR_FreeCon( ::hDbc )) != SQL_SUCCESS
         SR_MsgLogFile( "Error in SR_FreeCon() : " + str(nRet) + CRLF + ::LastError() )
      Else
         If ( nRet := SR_FreeEnv( ::hEnv )) != SQL_SUCCESS
            SR_MsgLogFile( "Error in SR_FreeEnv() : " + str(nRet) + CRLF + ::LastError() )
         EndIf
      EndIf
   EndIf

return ::Super:End()

/*------------------------------------------------------------------------*/

METHOD GetInfo( nType ) CLASS SR_ODBC

   local cBuffer := Space( 256 )
   ::nRetCode := SR_GetInfo( ::hDbc, nType, @cBuffer )

return cBuffer

/*------------------------------------------------------------------------*/

METHOD GetOptions( nType ) CLASS SR_ODBC

  local cBuffer:=space(256)

   ::nRetCode := SR_GETCONNECTOPTION( ::hDbc, nType, @cBuffer )

return cBuffer

/*------------------------------------------------------------------------*/

METHOD SetOptions( nType, uBuffer ) CLASS SR_ODBC

Return ( ::nRetCode := SR_SetConnectOption( ::hDbc, nType, uBuffer ) )

/*------------------------------------------------------------------------*/

METHOD SetStmtOptions( nType, uBuffer ) CLASS SR_ODBC

Return ( ::nRetCode := SR_SetStmtOption( ::hStmt, nType, uBuffer ) )

/*------------------------------------------------------------------------*/

METHOD Commit( lNoLog ) CLASS SR_ODBC
   ::Super:Commit( lNoLog )
Return ( ::nRetCode := SR_Commit( ::hEnv, ::hDbc ) )

/*------------------------------------------------------------------------*/

METHOD RollBack() CLASS SR_ODBC
   ::Super:RollBack()
Return ( ::nRetCode := SR_RollBack( ::hEnv, ::hDbc ) )

/*------------------------------------------------------------------------*/

METHOD ExecuteRaw( cCommand ) CLASS SR_ODBC

Return SR_ExecDir( ::hStmt, cCommand )

/*------------------------------------------------------------------------*/

METHOD WriteMemo( cFileName, nRecno, cRecnoName, aColumnsAndData )  CLASS SR_ODBC

   ::FreeStatement()

Return SR_ODBCWriteMemo( ::hDbc, cFileName, nRecno, cRecnoName, aColumnsAndData )

/*------------------------------------------------------------------------*/
