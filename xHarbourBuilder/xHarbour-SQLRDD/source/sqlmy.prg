/* $CATEGORY$SQLRDD/MySQL$FILES$sql.lib$HIDE$
* SQLRDD MySQL Native Connection Class
* Copyright (c) 2003 - Marcelo Lombardo  <marcelo@xharbour.com.br>
* Copyright (c) 2003 - Luiz Rafal Culik Guimarães <luiz@xharbour.com.br>
* All Rights Reserved
*/

#include "hbclass.ch"
#include "common.ch"
#include "compat.ch"
#include "sqlodbc.ch"
#include "sqlrdd.ch"
#include "error.ch"
#include "msg.ch"
#include "mysql.ch"
#include "sqlrddsetup.ch"

#define  DEBUGSESSION                .F.
#define ARRAY_BLOCK                  500
#define MINIMAL_MYSQL_SUPPORTED  40105

/*------------------------------------------------------------------------*/

CLASS SR_MYSQL FROM SR_CONNECTION

   DATA aCurrLine

   METHOD ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace, cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit, nTimeout )
   METHOD End()
   METHOD LastError()
   METHOD Commit( lNoLog )
   METHOD RollBack()
   METHOD IniFields( lReSelect, cTable, cCommand, lLoadCache, cWhere, cRecnoName, cDeletedName )
   METHOD ExecuteRaw( cCommand )
   METHOD FreeStatement()
   METHOD FetchRaw( lTranslate, aFields )
   METHOD FieldGet( nField, aFields, lTranslate )
   METHOD MoreResults( aArray, lTranslate )
   METHOD Getline( aFields, lTranslate, aArray )
   METHOD KillConnectionID( nID ) INLINE MYSKILLCONNID( ::hDbc, nID )
   METHOD GetAffectedRows()
ENDCLASS

/*------------------------------------------------------------------------*/

METHOD MoreResults( aArray, lTranslate )  CLASS SR_MYSQL
   local nRet
   (aArray)
   (lTranslate)
   nRet := -1
Return nRet

/*------------------------------------------------------------------------*/

METHOD Getline( aFields, lTranslate, aArray )  CLASS SR_MYSQL

   Local i

   DEFAULT lTranslate := .T.

   If aArray == NIL
      aArray := Array(len( aFields ))
   ElseIf len( aArray ) < len( aFields )
      aSize( aArray, len( aFields ) )
   EndIf

   If ::aCurrLine == NIL
      MYSLINEPROCESSED( ::hDbc, 4096, aFields, ::lQueryOnly, ::nSystemID, lTranslate, aArray )
      ::aCurrLine := aArray
      Return aArray
   EndIf

   For i = 1 to len( aArray )
      aArray[i] := ::aCurrLine[ i ]
   Next

Return aArray

/*------------------------------------------------------------------------*/

METHOD FieldGet( nField, aFields, lTranslate ) CLASS SR_MYSQL
   If ::aCurrLine == NIL
      DEFAULT lTranslate := .T.
      ::aCurrLine := array( LEN( aFields ) )
      MYSLINEPROCESSED( ::hDbc, 4096, aFields, ::lQueryOnly, ::nSystemID, lTranslate, ::aCurrLine )
   EndIf

return ::aCurrLine[nField]

/*------------------------------------------------------------------------*/

METHOD FetchRaw( lTranslate, aFields ) CLASS SR_MYSQL

   ::nRetCode := SQL_ERROR
   DEFAULT aFields    := ::aFields
   DEFAULT lTranslate := .T.

   If ::hStmt != NIL
      ::nRetCode := MYSFetch( ::hDbc )
      ::aCurrLine := NIL
   Else
      ::RunTimeErr("", "MySQLFetch - Invalid cursor state" + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
   EndIf

Return ::nRetCode

/*------------------------------------------------------------------------*/

METHOD FreeStatement() CLASS SR_MYSQL
   If ::hStmt != NIL
      MYSClear ( ::hDbc )
   EndIf
   ::hStmt := NIL
Return NIL

/*------------------------------------------------------------------------*/

METHOD IniFields( lReSelect, cTable, cCommand, lLoadCache, cWhere, cRecnoName, cDeletedName ) CLASS SR_MYSQL

   local nType := 0, nLen := 0, nNull := 0
   local aFields := {}
   local nDec := 0, nRet, cVlr := ""
   local aFld,aPks :={},nPos

   DEFAULT lReSelect    := .T.
   DEFAULT lLoadCache   := .F.
   DEFAULT cWhere       := ""
   DEFAULT cRecnoName   := SR_RecnoName()
   DEFAULT cDeletedName := SR_DeletedName()

   If lReSelect
      If !Empty( cCommand )
         nRet := ::Execute( cCommand + if(::lComments," /* Open Workarea with custom SQL command */",""), .F. )
      Else
	     ::exec( [SELECT column_name, ordinal_position FROM   information_schema.key_column_usage WHERE  table_schema = schema() AND    constraint_name = 'PRIMARY' AND    table_name = ']+ StrTran( cTable, [`], [] ) + ['], .F., .T., @aPks )
         nRet := ::Execute( "SELECT A.* FROM " + cTable + " A " + if(lLoadCache, cWhere + " ORDER BY A." + cRecnoName, " WHERE 1 = 0") + if(::lComments," /* Open Workarea */",""), .F. )
      EndIf
      If nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
         return nil
      EndIf
   EndIf

   If MYSResultStatus( ::hDbc ) != SQL_SUCCESS
      ::RunTimeErr("", "SqlNumResultCols Error" + chr(13)+chr(10)+ chr(13)+chr(10)+;
               "Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
      return nil
   endif

   ::nFields   := MYSCols( ::hDbc )

//   If (!Empty( cTable )) .and. empty( cCommand )
//      cTbl := cTable
//      aFields := MYSTableAttr( ::hDbc, cTbl )
//   Else
      aFields := MYSQueryAttr( ::hDbc )
//   EndIf

   ::aFields := aFields

   For each aFld in ::aFields
      aFld[ FIELD_ENUM ] = hb_enumIndex()
	  aFld[ FIELD_PRIMARY_KEY ] := 0
	  nPos := ascan( aPks, { |x| Alltrim( Upper( x[ 1 ] ) ) == Alltrim( Upper( aFld[ FIELD_NAME ] ) ) } )
	  if nPos>0
	     aFld[ FIELD_PRIMARY_KEY ] := aPks[ nPos, 2 ]
	  endif

   Next

   If lReSelect .and. !lLoadCache
      ::FreeStatement()
   EndIf

return aFields

/*------------------------------------------------------------------------*/

METHOD LastError() CLASS SR_MYSQL

   If ::hStmt != NIL
      Return "(" + alltrim(str( ::nRetCode ) ) + ") " + MYSResStatus( ::hDbc ) + " - " + MYSErrMsg( ::hDbc )
   EndIf

Return "(" + alltrim(str( ::nRetCode ) ) + ") " + MYSErrMsg( ::hDbc )

/*------------------------------------------------------------------------*/

METHOD ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace,;
            cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit, nTimeout ) CLASS SR_MYSQL

   local hEnv := 0, hDbc := 0
   local nret, cVersion := "", cSystemVers := "", cBuff := ""

   Local nVersionp


   (cDSN)
   (cUser)
   (cPassword)
   (nVersion)
   (cOwner)
   (nSizeMaxBuff)
   (lTrace)
   (nPrefetch)
   (nSelMeth)
   (nEmptyMode)
   (nDateMode)
   (lCounter)
   (lAutoCommit)

   hDbc := MYSConnect( ::cHost,::cUser,::cPassWord,::cDtb,::cPort,::cDtb, nTimeout,::lCompress )
   nRet := MYSStatus( hDbc )

   if nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
      ::nRetCode = nRet
      ::nSystemID := 0
      SR_MsgLogFile( "Connection Error" )
      nVersionp := MINIMAL_MYSQL_SUPPORTED -100
      Return Self
   else
      ::cConnect  = cConnect
      ::hStmt     = NIL
      ::hDbc      = hDbc
      cTargetDB   = "MySql Native"
      cSystemVers = alltrim( str( MYSVERS( hDbc ) ) )
      nVersionp  := MYSVERS( hDbc )

   EndIf

   If (!::lQueryOnly) .and. nVersionp < MINIMAL_MYSQL_SUPPORTED
      SR_MsgLogFile( "Connection Error: MySQL version not supported : " + cSystemVers + " / minimun is " + str(MINIMAL_MYSQL_SUPPORTED) )
      ::End()
      ::nSystemID := 0
      ::nRetCode  := -1
      Return Self
   EndIf

   ::cSystemName := cTargetDB
   ::cSystemVers := cSystemVers
   ::nSystemID   := SYSTEMID_MYSQL
   ::cTargetDB   := Upper( cTargetDB )
   ::uSid        := MYSGETCONNID( hDbc )


return Self

/*------------------------------------------------------------------------*/

METHOD End() CLASS SR_MYSQL

   ::Commit( .T. )
   ::FreeStatement()

   If !Empty( ::hDbc )
      MYSFinish( ::hDbc )
   EndIf

return ::Super:End()

/*------------------------------------------------------------------------*/

METHOD Commit( lNoLog ) CLASS SR_MYSQL
   ::Super:Commit( lNoLog )
Return ( ::nRetCode := MYSCommit( ::hDbc ) )

/*------------------------------------------------------------------------*/

METHOD RollBack() CLASS SR_MYSQL
   ::Super:RollBack()
Return ( ::nRetCode := MYSRollBack( ::hDbc ) )

/*------------------------------------------------------------------------*/

METHOD ExecuteRaw( cCommand ) CLASS SR_MYSQL

   If upper(left(ltrim(cCommand), 6)) == "SELECT" .or. upper(left(ltrim(cCommand), 5)) == "SHOW "
      ::lResultSet := .T.
   Else
      ::lResultSet := .F.
   EndIf

   ::hStmt := MYSExec( ::hDbc, cCommand )
Return MYSResultStatus( ::hDbc )

/*------------------------------------------------------------------------*/

METHOD GetAffectedRows() CLASS SR_MYSQL
return MYSAFFECTEDROWS( ::hDbc )