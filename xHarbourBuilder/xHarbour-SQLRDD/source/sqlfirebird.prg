/* $CATEGORY$SQLRDD/Firebird$FILES$sql.lib$HIDE$
* SQLRDD Firebird Connection Class
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
#include "firebird.ch"
#include "sqlrddsetup.ch"

#define DEBUGSESSION     .F.
#define ARRAY_BLOCK      500

/*------------------------------------------------------------------------*/

CLASS SR_FIREBIRD FROM SR_CONNECTION

   Data aCurrLine

   METHOD ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace, cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit )
   METHOD End()
   METHOD LastError()
   METHOD Commit()
   METHOD RollBack()
   METHOD IniFields( lReSelect, cTable, cCommand, lLoadCache, cWhere, cRecnoName, cDeletedName )
   METHOD ExecuteRaw( cCommand )
   METHOD AllocStatement()
   METHOD FetchRaw( lTranslate, aFields )
   METHOD FieldGet( nField, aFields, lTranslate )
   METHOD Getline( aFields, lTranslate, aArray )

ENDCLASS

/*------------------------------------------------------------------------*/

METHOD Getline( aFields, lTranslate, aArray )  CLASS SR_FIREBIRD

   Local i

   DEFAULT lTranslate := .T.

   If aArray == NIL
      aArray := Array(len( aFields ))
   ElseIf len( aArray ) != len( aFields )
      aSize( aArray, len( aFields ) )
   EndIf

   If ::aCurrLine == NIL
      FBLINEPROCESSED( ::hEnv, 4096, aFields, ::lQueryOnly, ::nSystemID, lTranslate, aArray )
      ::aCurrLine := aArray
      Return aArray
   EndIf

   For i = 1 to len( aArray )
      aArray[i] := ::aCurrLine[ i ]
   Next

Return aArray

/*------------------------------------------------------------------------*/

METHOD FieldGet( nField, aFields, lTranslate ) CLASS SR_FIREBIRD

   If ::aCurrLine == NIL
      DEFAULT lTranslate := .T.
      ::aCurrLine := array( LEN( aFields ) )
      FBLINEPROCESSED( ::hEnv, 4096, aFields, ::lQueryOnly, ::nSystemID, lTranslate, ::aCurrLine )
   EndIf

return ::aCurrLine[nField]

/*------------------------------------------------------------------------*/

METHOD FetchRaw( lTranslate, aFields ) CLASS SR_FIREBIRD

   ::nRetCode := SQL_ERROR
   DEFAULT aFields    := ::aFields
   DEFAULT lTranslate := .T.

   If ::hEnv != NIL
      ::nRetCode := FBFetch( ::hEnv )
      ::aCurrLine := NIL
   Else
      ::RunTimeErr("", "FBFetch - Invalid cursor state" + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
   EndIf

Return ::nRetCode

/*------------------------------------------------------------------------*/

METHOD AllocStatement() CLASS SR_FIREBIRD

   If ::lSetNext
      If ::nSetOpt == SQL_ATTR_QUERY_TIMEOUT
         // To do.
      EndIf
      ::lSetNext  := .F.
   EndIf

return SQL_SUCCESS

/*------------------------------------------------------------------------*/

METHOD IniFields(lReSelect, cTable, cCommand, lLoadCache, cWhere, cRecnoName, cDeletedName) CLASS SR_FIREBIRD

   local n, nFields := 0
   local nType := 0, nLen := 0, nNull := 0, cName
   local _nLen, _nDec, nPos
   local cType, nLenField
   local aFields := {}
   local nDec := 0, nRet, cVlr := "", aLocalPrecision := {},aPks := {}

   DEFAULT lReSelect    := .T.
   DEFAULT lLoadCache   := .F.
   DEFAULT cWhere       := ""
   DEFAULT cRecnoName   := SR_RecnoName()
   DEFAULT cDeletedName := SR_DeletedName()

   If lReSelect
      If !Empty( cCommand )
         nRet := ::Execute( cCommand + if(::lComments," /* Open Workarea with custom SQL command */",""), .F. )
      Else
         // DOON'T remove "+0"
         ::Exec( [select a.rdb$field_name, b.rdb$field_precision + 0 from rdb$relation_fields a, rdb$fields b where a.rdb$relation_name = '] + StrTran( cTable, ["], [] ) + [' and a.rdb$field_source = b.rdb$field_name] , .F., .T., @aLocalPrecision )
         ::exec( [select sg.rdb$field_name as field_name, rc.rdb$relation_name as table_name,  (sg.RDB$FIELD_POSITION + 1) AS field_position from rdb$indices ix left join rdb$index_segments sg on ix.rdb$index_name = sg.rdb$index_name left join rdb$relation_constraints rc on rc.rdb$index_name = ix.rdb$index_name where rc.rdb$constraint_type = 'PRIMARY KEY'		  and rc.rdb$relation_name = '] + StrTran( cTable, ["], [] ) + ['] , .F., .T., @aPks )
		 
         nRet := ::Execute( "SELECT A.* FROM " + cTable + " A " + if(lLoadCache, cWhere + " ORDER BY A." + cRecnoName, " WHERE 1 = 0") + if(::lComments," /* Open Workarea */",""), .F. )
      EndIf
      If nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
         return nil
      EndIf
   EndIf

   if ( ::nRetCode := FBNumResultCols( ::hEnv, @nFields ) ) != SQL_SUCCESS
      ::RunTimeErr("", "FBNumResultCols Error" + chr(13)+chr(10)+ chr(13)+chr(10)+;
               "Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
      return nil
   endif

   aFields   := Array( nFields )
   ::nFields := nFields

   for n = 1 to nFields

      nDec := 0

      if ( ::nRetCode := FBDescribeCol( ::hEnv, n, @cName, @nType, @nLen, @nDec, @nNull ) ) != SQL_SUCCESS
         ::RunTimeErr("", "FBDescribeCol Error" + chr(13)+chr(10)+ ::LastError() + chr(13)+chr(10)+;
                          "Last command sent to database : " + ::cLastComm )
         return nil
      else
         _nLen := nLen
         _nDec := nDec

         cName     := upper(alltrim( cName ))
         nPos := aScan( aLocalPrecision, { |x| rtrim(x[1]) == cName } )
         cType     := ::SQLType( nType, cName, nLen )
         nLenField := ::SQLLen( nType, nLen, @nDec )
         If nPos > 0 .and. aLocalPrecision[nPos,2] > 0
            nLenField := aLocalPrecision[nPos,2]
         ElseIf ( nType == SQL_DOUBLE .or. nType == SQL_FLOAT .or. nType == SQL_NUMERIC )
            nLenField := 19
         EndIf
         nPos := aScan( aPks, { |x| rtrim(x[1]) == cName } )  

         If cType == "U"
            ::RuntimeErr( "", SR_Msg(21) + cName + " : " + str( nType ) )
         Else
		 if nPos >0
		    aFields[n] := { cName, cType, nLenField, nDec, nNull >= 1 , nType,, n, _nDec, aPks[nPos,3], }
		 else
            aFields[n] := { cName, cType, nLenField, nDec, nNull >= 1 , nType,, n, _nDec,0, }
		 endif	
         EndIf

      endif
   next

   ::aFields := aFields

   If lReSelect .and. !lLoadCache
      ::FreeStatement()
   EndIf

return aFields

/*------------------------------------------------------------------------*/

METHOD LastError() CLASS SR_FIREBIRD

   local cMsgError, nType := 0
   cMsgError := FBError( ::hEnv, @nType )

return alltrim(cMsgError) + " - Native error code " + AllTrim( str( nType ) )

/*------------------------------------------------------------------------*/

METHOD ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace,;
            cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit ) CLASS SR_FIREBIRD

   local nRet, hEnv, cSystemVers

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

   nRet := FBConnect( ::cDtb, ::cUser, ::cPassword, ::cCharSet, @hEnv )

   if nRet != SQL_SUCCESS
      ::nRetCode = nRet
      SR_MsgLogFile( "Connection Error: " + alltrim(str(nRet)) + " (check fb.log) - Database: " + ::cDtb + " - Username : " + ::cUser + " (Password not shown for security)" )
      Return Self
   else
      ::cConnect  := cConnect
      cTargetDB   := StrTran( FBVERSION(hEnv), "(access method)", "" )
      cSystemVers := SubStr( cTargetDB, at( "Firebird ", cTargetDB ) + 9, 3 )
   EndIf

   nRet := FBBeginTransaction( hEnv )

   if nRet != SQL_SUCCESS
      ::nRetCode = nRet
      SR_MsgLogFile( "Transaction Start error : " + alltrim(str(nRet)) )
      Return Self
   EndIf

   ::hEnv         := hEnv
   ::cSystemName  := cTargetDB
   ::cSystemVers  := cSystemVers

   ::DetectTargetDb()

return Self

/*------------------------------------------------------------------------*/

METHOD End() CLASS SR_FIREBIRD

   ::Commit()
   FBClose( ::hEnv )

return ::Super:End()

/*------------------------------------------------------------------------*/

METHOD Commit() CLASS SR_FIREBIRD
   ::Super:Commit()
   ::nRetCode := FBCOMMITTRANSACTION(::hEnv )
Return ( ::nRetCode := FBBeginTransaction( ::hEnv ) )

/*------------------------------------------------------------------------*/

METHOD RollBack() CLASS SR_FIREBIRD
   ::Super:RollBack()
Return ( ::nRetCode := FBRollBackTransaction( ::hEnv ) )

/*------------------------------------------------------------------------*/

METHOD ExecuteRaw( cCommand ) CLASS SR_FIREBIRD
   local nRet

   If upper(left(ltrim(cCommand), 6)) == "SELECT"
      nRet := FBExecute( ::hEnv, cCommand, IB_DIALECT_CURRENT )
      ::lResultSet := .T.
   Else
      nRet := FBExecuteImmediate( ::hEnv, cCommand, IB_DIALECT_CURRENT )
      ::lResultSet := .F.
   EndIf

Return nRet

/*------------------------------------------------------------------------*/
