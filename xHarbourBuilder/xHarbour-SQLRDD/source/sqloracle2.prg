/* $CATEGORY$SQLRDD/Oracle$FILES$sql.lib$HIDE$
* SQLRDD Oracle Native Connection Class
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* Copyright (c) 2003 - Luiz Rafal Culik Guimarães <luiz@xharbour.com.br>
* All Rights Reserved
*/

#include "hbclass.ch"
#include "common.ch"
#include "compat.ch"
#include "sqlora.ch"
#include "sqlrdd.ch"
#include "error.ch"
#include "msg.ch"
#include "sqlrddsetup.ch"

#define DEBUGSESSION     .F.
#define ARRAY_BLOCK      500

/*------------------------------------------------------------------------*/

CLASS SR_ORACLE2 FROM SR_CONNECTION

   DATA hdbc
   DATA nParamStart  INIT 0

   Data Is_logged_on,is_Attached
   Data aBinds
   Data aCurrLine

   METHOD ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace, cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit ) CONSTRUCTOR
   METHOD End()
   METHOD LastError()
   METHOD Commit()
   METHOD RollBack()
   METHOD IniFields( lReSelect, cTable, cCommand, lLoadCache, cWhere, cRecnoName, cDeletedName )
   METHOD ExecuteRaw( cCommand )
   METHOD AllocStatement()
   METHOD FreeStatement()
   METHOD FetchRaw( lTranslate, aFields )
   METHOD FieldGet( nField, aField, lTranslate )
   METHOD MoreResults( aArray, lTranslate )
   METHOD BINDPARAM(lStart,lIn,cRet,nLen)
   METHOD ConvertParams( c )
   METHOD WriteMemo( cFileName, nRecno, cRecnoName, aColumnsAndData )
   METHOD Getline( aFields, lTranslate, aArray )
   METHOD ExecSPRC(  cComm, lMsg, lFetch, aArray, cFile, cAlias, cVar, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate, nLogMode  ) 
   METHOD ExecSP( cComm, aReturn, nParam )
   
   
ENDCLASS

/*------------------------------------------------------------------------*/

METHOD MoreResults( aArray, lTranslate ) CLASS SR_ORACLE2
   (aArray)
   (lTranslate)
Return -1

/*------------------------------------------------------------------------*/

METHOD Getline( aFields, lTranslate, aArray )  CLASS SR_ORACLE2

   Local i

   DEFAULT lTranslate := .T.

   If aArray == NIL
      aArray := Array(len( aFields ))
   ElseIf len( aArray ) < len( aFields )
      aSize( aArray, len( aFields ) )
   EndIf

   If ::aCurrLine == NIL
      SQLO2_LINEPROCESSED( ::hDbc, 4096, aFields, ::lQueryOnly, ::nSystemID, lTranslate, aArray )
      ::aCurrLine := aArray
      Return aArray
   EndIf

   For i = 1 to len( aArray )
      aArray[i] := ::aCurrLine[ i ]
   Next

Return aArray

/*------------------------------------------------------------------------*/

METHOD FieldGet( nField, aFields, lTranslate ) CLASS SR_ORACLE2

   If ::aCurrLine == NIL
      DEFAULT lTranslate := .T.
      ::aCurrLine := array( LEN( aFields ) )
      SQLO2_LINEPROCESSED( ::hDbc, 4096, aFields, ::lQueryOnly, ::nSystemID, lTranslate, ::aCurrLine )
   EndIf

return ::aCurrLine[nField]

/*------------------------------------------------------------------------*/

METHOD FetchRaw( lTranslate, aFields ) CLASS SR_ORACLE2

   ::nRetCode := SQL_ERROR
   DEFAULT aFields    := ::aFields
   DEFAULT lTranslate := .T.

   If ::hDBC != NIL
      ::nRetCode := SQLO2_FETCH( ::hDBC )
      ::aCurrLine := NIL
   Else
      ::RunTimeErr("", "SQLO2_FETCH - Invalid cursor state" + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
   EndIf

Return ::nRetCode

/*------------------------------------------------------------------------*/

METHOD FreeStatement() CLASS SR_ORACLE2

   if ::hDBC != NIL .and. ::hstmt != NIL
      if SQLO2_CLOSESTMT( ::hDBC ) != SQL_SUCCESS
         ::RunTimeErr("", "SQLO2_CLOSESTMT error" + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
      endif
      ::hstmt := NIL
   endif

Return NIL

/*------------------------------------------------------------------------*/

METHOD AllocStatement() CLASS SR_ORACLE2

   local hStmtLocal := 0, nRet := 0

   ::FreeStatement()

   If ::lSetNext
      ::lSetNext  := .F.
      nRet := ::SetStmtOptions( ::nSetOpt, ::nSetValue )
      If nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
         SR_MsgLogFile( SR_Msg(23) + " (" + alltrim(str(nRet)) + ") : " + ::LastError() )
      EndIf
   EndIf

Return nRet

/*------------------------------------------------------------------------*/

METHOD IniFields(lReSelect, cTable, cCommand, lLoadCache, cWhere, cRecnoName, cDeletedName) CLASS SR_ORACLE2

   Local n
   Local nType := 0, nLen := 0, nNull := 0, nDec := 0, cName
   Local _nLen, _nDec
   Local cType, nLenField
   Local aFields := {}
   Local nRet, cVlr := ""

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

   ::nFields := SQLO2_NUMCOLS( ::hDBC )

   If ::nFields < 0
         ::RunTimeErr("", "SQLO2_NUMCOLS Error" + chr(13)+chr(10)+ str(::nFields) + chr(13)+chr(10)+;
                          "Last command sent to database : " + ::cLastComm )

      Return NIL
   EndIf

   aFields   := Array( ::nFields )

   For n = 1 to ::nFields

      if ( ::nRetCode := SQLO2_DESCRIBECOL( ::hDBC, n, @cName, @nType, @nLen, @nDec, @nNull ) ) != SQL_SUCCESS
         ::RunTimeErr("", "SQLDescribeCol Error" + chr(13)+chr(10)+ ::LastError() + chr(13)+chr(10)+;
                          "Last command sent to database : " + ::cLastComm )
        return nil
      else

         _nLen := nLen
         _nDec := nDec
         cName := Upper(alltrim(cName))

         If (nLen == 2000 .or. nLen == 4000) .and. SR_SetNwgCompat()
            nType := SQL_FAKE_LOB
         EndIf

         nLenField := ::SQLLen( nType, nLen, @nDec )
         cType     := ::SQLType( nType, cName, nLen )

         If (!::lQueryOnly) .and. cType == "N" .and. nLenField == 38 .and. nDec == 0
            cType     := "L"
            nLenField := 1
            nType     := SQL_BIT
         EndIf

         If cType == "U"
            ::RuntimeErr( "", SR_Msg(21) + cName + " : " + str( nType ) )
         Else
            aFields[n] := { cName, cType, nLenField, nDec, nNull, nType, , n, , , }
         EndIf

      endif
   next

   ::aFields := aFields

   If lReSelect .and. !lLoadCache
      ::FreeStatement()
   EndIf

return aFields

/*------------------------------------------------------------------------*/

METHOD LastError() CLASS SR_ORACLE2

return SQLO2_GETERRORDESCR( ::hDBC ) + " retcode: " + sr_val2Char( ::nRetCode) + " - " + AllTrim( str( SQLO2_GETERRORCODE( ::hDBC ) ) )

/*------------------------------------------------------------------------*/

METHOD ConnectRaw( cDSN, cUser, cPassword, nVersion, cOwner, nSizeMaxBuff, lTrace,;
            cConnect, nPrefetch, cTargetDB, nSelMeth, nEmptyMode, nDateMode, lCounter, lAutoCommit ) CLASS SR_ORACLE2

   local hEnv := 0, hDbc := 0
   local nret, cVersion := "", cSystemVers := "", cBuff := ""
   Local aRet := {}
   Local aVersion
   Local cmatch,nstart,nlen,s_reEnvVar := HB_RegexComp( "(\d+\.\d+\.\d+)" )
//   Local cString


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

   
   ::hStmt := NIL
*    nret    :=  SQLO2_CONNECT( ::cUser + "/" + ::cPassWord + "@" + ::cDtb, @hDbc )
   if ::cApp != NIL
      nret    :=  SQLO2_CONNECT( ::cDtb,::cUser , ::cPassWord , @hDbc, .T. )
   else
      nret    :=  SQLO2_CONNECT( ::cDtb,::cUser , ::cPassWord , @hDbc, .F. )
   endif
   if nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
      ::nRetCode = nRet
      ::hDbc     := hDbc
      SR_MsgLogFile( "Connection Error: " + ::lastError() + " - Connection string: " + ::cUser + "/" + Replicate("*", len(::cPassWord) ) + "@" + ::cDtb )
      Return Self
   else
      ::cConnect  := cConnect
      ::hDbc      := hDbc
      cTargetDB   := "Oracle"
      cSystemVers := SQLO2_DBMSNAME( hDbc )
   EndIf

   ::cSystemName  := cTargetDB
   ::cSystemVers  := cSystemVers
   ::nSystemID    := SYSTEMID_ORACLE
   ::cTargetDB    := Upper( cTargetDB )

   aRet :={{cSystemVers}}
   cMatch := HB_AtX( s_reEnvVar, cSystemVers, , @nStart, @nLen )         
   if !empty(cMatch )
      aVersion      := hb_atokens( cMatch, "." )
   else
      aVersion      := hb_atokens( strtran(Upper(aRet[1,1]),"ORACLE ",""), "." )
   endif
   
   
   ::exec( "select sid from " + If(::lCluster, "g", "" ) + "v$session where AUDSID = sys_context('USERENV','sessionid')", .T., .T., @aRet )

   If len( aRet ) > 0
      ::uSid := val(str(aRet[1,1],8,0))
   EndIf
   SQLO2_SETSTATEMENTCACHESIZE(hdbc,50)
   ::lOracle12 :=( Val( aversion[ 1 ] ) == 12 ) 

Return Self

/*------------------------------------------------------------------------*/

METHOD End() CLASS SR_ORACLE2

   Local nRet

   IF !Empty( ::hDbc )
     IF  ( nRet := SQLO2_DISCONNECT( ::hDbc )) != SQL_SUCCESS
        SR_MsgLogFile( "Error disconnecting : " + str(nRet) + CRLF + ::LastError() )
     EndIf
   ENDIF

   ::hEnv  = 0
   ::hDbc  = NIL

return nil

/*------------------------------------------------------------------------*/

METHOD Commit( lNoLog ) CLASS SR_ORACLE2
   Super:Commit( lNoLog )
RETURN (::nRetcode := SQLO2_COMMIT( ::hdbc ) )

/*------------------------------------------------------------------------*/

METHOD RollBack() CLASS SR_ORACLE2
   Super:RollBack()
Return ( ::nRetCode := SQLO2_ROLLBACK( ::hDbc ) )

/*------------------------------------------------------------------------*/

METHOD ExecuteRaw( cCommand ) CLASS SR_ORACLE2
   local nRet, i

   If upper(left(ltrim(cCommand), 6)) == "SELECT"
      ::hStmt := ::hDBC
      
      if !empty( ::cSqlPrepare) .and. len( ::aBindParameters ) > 0 .and. ":1" in ::cSqlPrepare
      
         ::lBind := .F.
         
          ORACLEPREPARE2(::hDBC,::cSqlPrepare,.t.)
          ORACLEBINDALLOC2( ::hDBC, len( ::aBindParameters )  )
          for i :=1 to len (::aBindParameters )
          if valtype(::aBindParameters[ i ]) == "A"
             if valtype(::aBindParameters[i,2]) == "C"
                ORACLEINBINDPARAM2( ::hDBC,i,-1,::aBindParameters[i,3],0,::aBindParameters[i,2],.t.)          
             elseif valtype(::aBindParameters[i,2]) == "D"
                ORACLEINBINDPARAM2( ::hDBC,i,8,::aBindParameters[i,3],0,::aBindParameters[i,2],.t.)                          
             elseif valtype(::aBindParameters[i]) == "L"
                ORACLEINBINDPARAM2( ::hDBC,i,3,::aBindParameters[i,3],0,::aBindParameters[i,2],.t.)                                          
             else
                ORACLEINBINDPARAM2( ::hDBC,i,2,15,0,::aBindParameters[i,2],.t.)          
             endif
          else
             if valtype(::aBindParameters[i]) == "C"
                ORACLEINBINDPARAM2( ::hDBC,i,-1,len(::aBindParameters[i]),0,::aBindParameters[i],.t.)          
             elseif valtype(::aBindParameters[i]) == "D"
                ORACLEINBINDPARAM2( ::hDBC,i,8,::aBindParameters[i],0,::aBindParameters[i],.t.)                          
             elseif valtype(::aBindParameters[i]) == "L"
                ORACLEINBINDPARAM2( ::hDBC,i,3,::aBindParameters[i],0,::aBindParameters[i],.t.)                                          
             else
                ORACLEINBINDPARAM2( ::hDBC,i,2,15,0,::aBindParameters[i],.t.)          
             endif
          
          endif   
          next
          nRet := SQLO2_EXECUTE( ::hDBC, ::cSqlPrepare ,.t.)

          ORACLEFREEBIND2(::hDbc)
          ::aBindParameters :={}
          ::cSqlPrepare := ""
      else              
      nRet := SQLO2_EXECUTE( ::hDBC, cCommand )
   endif
      ::lResultSet := .T.
   Else
      ::hStmt := NIL
      nRet := SQLO2_EXECDIRECT( ::hDBC, cCommand )
      ::lResultSet := .F.
   EndIf

Return nRet

/*------------------------------------------------------------------------*/

Static Function ProcessParams( cSql, nBound )
   Local nPos
   Local cTemp := SubStr( cSql,1, AT( "?" , cSql) -1 )
   Local lHasParen := Rat( ")", cSql ) > 0
   Local lHasPointComma := Rat( ";", cSql ) > 0
   Local aItens
   Local cOriginal := cTemp +" "
   Local xParam
   Local nParamBound := 0

   cSql := StrTran(cSql,cTemp,"")
   aItens := hb_aTokens("?",",")

   FOR EACH xParam IN aItens
      nPos := hB_enumIndex()
      cOriginal += alltrim(":P"+StrZero(nPos,3)) +" "
      nParamBound ++
   NEXT

  IF lhasParen
     cOriginal += ")"
  ENDIF

  IF lHasPointComma
     cOriginal += ";"
  ENDIF

  nBound := nParamBound

RETURN cOriginal

/*------------------------------------------------------------------------*/

METHOD BINDPARAM( lStart, lIn, nLen, cRet, nLenRet )  CLASS SR_ORACLE2
   DEFAULT lIn to .F.
   DEFAULT lStart to .f.
   
   (nLen)
   (cRet)
   (nLenRet)
   
   IF lStart
      ::AllocStatement()
      ::nParamStart  := 1
   ELSE
      ::nParamStart ++
   ENDIF

//   OracleinBindParam( ::hdbc, ::nParamStart, SQL_LONGVARCHAR, nLen, 0,@cRet, @nLenRet, lIn )

Return self

/*------------------------------------------------------------------------*/

METHOD ConvertParams( c )  CLASS SR_ORACLE2
   Local nBound
   local cRet := ProcessParams( c ,@nBound)
RETURN cRet

/*------------------------------------------------------------------------*/

METHOD WriteMemo( cFileName, nRecno, cRecnoName, aColumnsAndData )  CLASS SR_ORACLE2

Return OracleWriteMemo2( ::hDbc, cFileName, nRecno, cRecnoName, aColumnsAndData )

/*------------------------------------------------------------------------*/


METHOD ExecSP( cComm, aReturn, nParam, aType )  CLASS SR_ORACLE2
   Local i, n
   Local nError := 0
   
   DEFAULT aReturn to {}
   DEFAULT aType to   {}
   DEFAULT nParam to   1
   
   oracleprePARE( ::hdbc, cComm )
   
   oraclebindalloc( ::hdbc, nParam )
   
   For i:= 1 to nParam 
      n := -1
      If Len( aType ) > 0
         If aType[i]=="N"
            n  := 5
         EndIf
      EndIF      
      OracleinBindParam( ::hdbc, i, n, 12, 0 )      
   Next
    
   TRY
      nError := OracleExecDir( ::hDbc )
   CATCH 
      nerror := - 1
   End
   
   If nError < 0
      ::RunTimeErr("", str( SQLO2_GETERRORCODE( ::hDbc ), 4 ) + " - " + SQLO2_GETERRORDESCR( ::hDbc ) ) 
   Else
   //If nError >= 0
   	  
   	  
      For i:=1 to nParam
        AADD( aReturn, ORACLEGETBINDDATA( ::hdbc, i ) )          
      Next
   EndIf      

   ORACLEFREEBIND( ::hdbc )
   CLOSECURSOR( ::hDbc )
   
Return nError
   
/*------------------------------------------------------------------------*/
METHOD ExecSPRC(  cComm, lMsg, lFetch, aArray, cFile, cAlias, cVar, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate, nLogMode  ) CLASS SR_ORACLE2
   Local i
   Local n, nAllocated := 0
   Local nBlocks   
   Local nError
   Local aFields
   Local nCols 
   Local aDb
   Local nFieldRec
   Local aMemo
   Local cFileTemp      
   Local cEste     
   Local nLenMemo  
   Local nLinesMemo        
   Local cCampo
   Local j 
   
   DEFAULT nMaxRecords TO 999999999999
   DEFAULT cVar To ":c1"
   (ncols)
   (nlogmode)
     
   ::AllocStatement()

   DEFAULT lMsg          := .T.
   DEFAULT lFetch        := .F.
   DEFAULT nMaxRecords   := 99999999999999
   DEFAULT lNoRecno      := .F.
   DEFAULT cRecnoName    := SR_RecnoName()
   DEFAULT cDeletedName  := SR_DeletedName()
   
   TRY
      nError := ORACLE_PROCCURSOR2( ::hDbc, cComm, cVar )  
      //nError := ORACLE_BINDCURSOR( ::hDbc, cComm, cVar )
      ::cLastComm := cComm
   CATCH  
      nError := - 1
   End               
   
   If nError < 0
      If lFetch
       //  ::RunTimeErr("", "SQLExecDirect Error Erro na STORE PROCEDURE" ) 
       ::RunTimeErr("", str( SQLO2_GETERRORCODE( ::hDbc ), 4 ) + " - " + SQLO2_GETERRORDESCR( ::hDbc ) + ::cLastComm ) 
      EndIf  
   EndIf      
    
   If !Empty( cFile )
      HB_FNameSplit( cFile,, @cFileTemp )
      DEFAULT cAlias        := cFileTemp
   EndIf

   //nCols := SQLO2_NUMCOLS( ::hDbc )
 
   //For i := 1 to nCols
   //   ORACLEBINDALLOC( ::hDbc, i  )
   //Next

   aFields := ::iniFields(.f.) 

   If lFetch
      If !Empty( cFile )
       
         aFields := ::IniFields(.F.,,,,,cRecnoName, cDeletedName )

         if Select( cAlias ) == 0
            aDb := {}
            If lNoRecno
               For i = 1 to len( aFields )
                  If aFields[i,1] != cRecnoName
                     AADD( aDb, aFields[i] )
                  Else
                     nFieldRec := i
                  EndIf
               Next
               dbCreate( cFile, SR_AdjustNum(aDb), SR_SetRDDTemp() )
            Else
               dbCreate( cFile, SR_AdjustNum(aFields), SR_SetRDDTemp() )
            EndIf

            dbUseArea( .t., SR_SetRDDTemp(), cFile, cAlias, .F. )
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
         aFields   := ::IniFields(.F.,,,,,cRecnoName, cDeletedName,.t.)
 
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
 
   nerror:=SQLO2_CLOSESTMT( ::hDbc )
   
   If nError < 0
      If lFetch
         ::RunTimeErr("", "SQLExecDirect Error in close cursor Statement" ) 
      EndIf      
   endif   

  ::freestatement()
 
return  0  

function  ExecuteSP2( cComm, aReturn  ) 
*    Local i, n
   Local nError := 0
   local oConn := SR_GetConnection()
   
   DEFAULT aReturn to {}
   
   oracleprePARE( oConn:hdbc, cComm )
   
   oraclebindalloc( oConn:hdbc, 1 )
   
   OracleinBindParam( oConn:hdbc, 1, -1, 12, 0 )      
 
   TRY
      nError := OracleExecDir( oConn:hDbc )
   CATCH 
      nerror := - 1
   End
   
   if nError >=0
      AADD( aReturn, ORACLEGETBINDDATA( oConn:hdbc, 1 ) )           
   EndIf
   
  
   ORACLEFREEBIND( oConn:hdbc )
   CLOSECURSOR( oConn:hDbc )
    
Return nError
/*
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
*/