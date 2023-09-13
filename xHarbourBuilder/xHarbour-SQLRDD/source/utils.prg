/* $CATEGORY$SQLRDD/Utils$FILES$sql.lib$
* SQLRDD Utilities
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "hbclass.ch"
#include "common.ch"
#include "compat.ch"
#include "sqlodbc.ch"
#include "sqlrdd.ch"
#include "fileio.ch"
#include "msg.ch"
#include "error.ch"
#include "sqlrddsetup.ch"
request HB_Deserialize,HB_DeserialNext
#define FH_ALLOC_BLOCK     32

Static DtAtiv, lHistorico
Static _nCnt := 1
Static lCreateAsHistoric := .F.

#ifdef HB_C52_UNDOC
STATIC s_lNoAlert
#endif

/*------------------------------------------------------------------------*/

Function SR_GoPhantom()

   If IS_SQLRDD
      Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):sqlGoPhantom()
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

FUNCTION SR_WorkareaFileName()

   if empty( alias() )
      Return ""
   endif

   if ! IS_SQLRDD
      Return ""
   endif

Return dbInfo( DBI_INTERNAL_OBJECT ):cFileName

/*------------------------------------------------------------------------*/

FUNCTION SR_dbStruct()

   if empty( alias() )
      Return {}
   endif

   if ! IS_SQLRDD
      Return {}
   endif

Return aclone( dbInfo( DBI_INTERNAL_OBJECT ):aFields )

/*------------------------------------------------------------------------*/

Function SR_MsgLogFile( uMsg, p1,p2,p3,p4,p5,p6,p7,p8 )
   SR_LogFile( "sqlerror.log", { uMsg, p1,p2,p3,p4,p5,p6,p7,p8 } )
Return NIL

/*------------------------------------------------------------------------*/

Function SR_Val2Char(a,n1,n2)
   Do Case
   Case HB_ISSTRING(a)
      Return a
   Case HB_ISNUMERIC(a) .and. n1 != NIL .and. n2 != NIL
      Return Str(a,n1,n2)
   Case HB_ISNUMERIC(a)
      Return Str(a)
   Case HB_ISDATE(a)
      Return dtoc(a)
   Case HB_ISLOGICAL(a)
      Return if(a,".T.",".F.")
   EndCase
Return ""

/*------------------------------------------------------------------------*/

Function SR_LogFile( cFileName, aInfo, lAddDateTime )

   local hFile, cLine, n

   Default lAddDatetime := .T.

   If lAddDateTime

      cLine := DToC( Date() ) + " " + Time() + ": "

   Else

      cLine := ""

   Endif

   for n = 1 to Len( aInfo )
      If aInfo[ n ] == NIL
         Exit
      EndIf
      cLine += SR_Val2CharQ( aInfo[ n ] ) + Chr( 9 )
   next

   cLine += CRLF

   IF sr_phFile( cFileName )
      hFile := FOpen( cFileName, 1 )
   ELSE
      hFile := FCreate( cFileName )
   ENDIF

   FSeek( hFile, 0, 2 )
   FWrite( hFile, alltrim( cLine ) )
   FClose( hFile )

return nil

/*------------------------------------------------------------------------*/

Function SR_FilterStatus(lEnable)

   If IS_SQLRDD
      If HB_ISLOGICAL( lEnable )
         Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):lDisableFlts := !lEnable
      Else
         Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):lDisableFlts
      EndIf
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

Function SR_CreateConstraint( aSourceColumns, cTargetTable, aTargetColumns, cConstraintName )

   If IS_SQLRDD
      Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):CreateConstraint(dbInfo( DBI_INTERNAL_OBJECT ):cFileName,aSourceColumns, cTargetTable, aTargetColumns, cConstraintName)
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_DropConstraint( cConstraintName, lFKs )

   If IS_SQLRDD
      Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):DropConstraint(dbInfo( DBI_INTERNAL_OBJECT ):cFileName,cConstraintName, lFKs)
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_ChangeStruct( cTableName, aNewStruct )

   Local oWA, lOk := .T., aToDrop := {}, aToFix := {}
   Local i, n, cAlias, nReg, cTblName, nAlias, nOrd
   Local aDirect := {}

   If select() == 0
      SR_RuntimeErr( , "SR_ChengeStructure: Workarea not in use." )
   EndIf

   If len( aNewStruct ) < 1 .or. valtype( aNewStruct) != "A" .or. ValType( aNewStruct[1] ) != "A"
      SR_RuntimeErr( , "SR_ChengeStructure: Invalid arguments [2]." )
   EndIf

   If IS_SQLRDD

      oWA := dbInfo( DBI_INTERNAL_OBJECT )

      If (!Empty(cTableName)) .and. oWA:cOriginalFN != upper(alltrim(cTableName))
         SR_RuntimeErr( , "SR_ChengeStructure: Invalid arguments [1]: " + cTableName )
      EndIf

      cAlias   := alias()
      nAlias   := select()
      cTblName := oWA:cFileName
      nOrd     := IndexOrd()
      nReg     := recno()

      dbSetOrder(0)

      SR_LogFile( "changestruct.log", { oWA:cFileName, "Original Structure:", e"\r\n" + sr_showVector(oWA:aFields)  } )
      SR_LogFile( "changestruct.log", { oWA:cFileName, "New Structure:", e"\r\n" + sr_showVector(aNewStruct)  } )

      For i = 1 to len( aNewStruct )
         aNewStruct[i,1] := Upper(alltrim(aNewStruct[i,1]))
         If (n := aScan( oWA:aFields, {|x| x[1] == aNewStruct[i,1] } ) ) > 0

            aSize( aNewStruct[i], max( len(aNewStruct[i] ), 5 ) )

            If aNewStruct[i, 2] == oWA:aFields[n, 2] .and. aNewStruct[i, 3] == oWA:aFields[n, 3] .and. aNewStruct[i, 4] == oWA:aFields[n, 4]
               // Structure is identical. Only need to check for NOT NULL flag.
               If aNewStruct[i, FIELD_NULLABLE] != NIL .and. valtype(aNewStruct[i, FIELD_NULLABLE]) == "L" .and. aNewStruct[i, FIELD_NULLABLE] !=  oWA:aFields[n, FIELD_NULLABLE]
                  If aNewStruct[i, FIELD_NULLABLE]
                     SR_LogFile( "changestruct.log", { oWA:cFileName, "Changing to nullable:", aNewStruct[i,1]} )
                     oWA:DropRuleNotNull( aNewStruct[i,1] )
                  Else
                     SR_LogFile( "changestruct.log", { oWA:cFileName, "Changing to not null:", aNewStruct[i,1]} )
                     oWA:AddRuleNotNull( aNewStruct[i,1] )
                  EndIf
               EndIf
            ElseIf oWA:oSql:nSystemID == SYSTEMID_IBMDB2
               SR_LogFile( "changestruct.log", { oWA:cFileName, "Column cannot be changed:", aNewStruct[i,1], " - Operation not supported by back end database" } )
            ElseIf aNewStruct[i, 2] == "M" .and. oWA:aFields[n, 2] == "C"
               aadd( aToFix, aClone( aNewStruct[i] ) )
               SR_LogFile( "changestruct.log", { oWA:cFileName, "Will Change data type of field:", aNewStruct[i,1], "from", oWA:aFields[n, 2], "to", aNewStruct[i, 2]} )
            ElseIf aNewStruct[i, 2] == "C" .and. oWA:aFields[n, 2] == "M"
               aadd( aToFix, aClone( aNewStruct[i] ) )
               SR_LogFile( "changestruct.log", { oWA:cFileName, "Warning: Possible data loss changing data type:", aNewStruct[i,1], "from", oWA:aFields[n, 2], "to", aNewStruct[i, 2]} )
            ElseIf aNewStruct[i, 2] != oWA:aFields[n, 2]
               IF aNewStruct[i, 2] $"CN" .and. oWA:aFields[n, 2] $"CN" .and. oWA:oSql:nSystemID == SYSTEMID_POSTGR

*                   IF "8.4" $ oWA:oSql:cSystemVers .or. "9.0" $ oWA:oSql:cSystemVers
                  IF oWA:oSql:lPostgresql8 .and. !oWA:oSql:lPostgresql83
                     aadd( aDirect, aClone( aNewStruct[i] ) )
                  else
                     aadd( aToFix, aClone( aNewStruct[i] ) )
                  ENDIF
                  SR_LogFile( "changestruct.log", { oWA:cFileName, "Warning: Possible data loss changing field types:", aNewStruct[i,1], "from", oWA:aFields[n, 2], "to", aNewStruct[i, 2]} )
               ELSE
                  SR_LogFile( "changestruct.log", { oWA:cFileName, "ERROR: Cannot convert data type of field:", aNewStruct[i,1], " from", oWA:aFields[n, 2], "to", aNewStruct[i, 2] } )
               ENDIF
            ElseIf aNewStruct[i, 3] >= oWA:aFields[n, 3] .and. oWA:aFields[n, 2] $ "CN"

               aadd( aDirect, aClone( aNewStruct[i] ) )
               SR_LogFile( "changestruct.log", { oWA:cFileName, "Will Change field size:", aNewStruct[i,1], "from", oWA:aFields[n, 3], "to", aNewStruct[i, 3] } )
            ElseIf aNewStruct[i, 3] < oWA:aFields[n, 3] .and. oWA:aFields[n, 2] $ "CN"
               aadd( aToFix, aClone( aNewStruct[i] ) )
               SR_LogFile( "changestruct.log", { oWA:cFileName, "Warning: Possible data loss changing field size:", aNewStruct[i,1], "from", oWA:aFields[n, 3], "to", aNewStruct[i, 3]} )
            Else
               SR_LogFile( "changestruct.log", { oWA:cFileName, "Column cannot be changed:", aNewStruct[i,1] } )
            EndIf
         Else
            aadd( aToFix, aClone( aNewStruct[i] ) )
            SR_LogFile( "changestruct.log", { oWA:cFileName, "Will add column:", aNewStruct[i,1] } )
         EndIf
      Next

      For i = 1 to len( oWA:aFields )
         If (n := aScan( aNewStruct, {|x| x[1] == oWA:aFields[i,1] } ) ) == 0
            If (!oWA:aFields[i,1] == oWA:cRecnoName) .and. (!oWA:aFields[i,1] == oWA:cDeletedName ) .and. oWA:oSql:nSystemID != SYSTEMID_IBMDB2
               aadd( aToDrop, aClone( oWA:aFields[i] ) )
               SR_LogFile( "changestruct.log", { oWA:cFileName, "Will drop:", oWA:aFields[i,1] } )
            EndIf
         EndIf
      Next
      IF Len( aDirect ) > 0 .and.;
       ( oWA:oSql:nSystemID == SYSTEMID_FIREBR .or. ;
         oWA:oSql:nSystemID == SYSTEMID_FIREBR3 .or. ;
         oWA:oSql:nSystemID == SYSTEMID_MYSQL  .or. ;
         oWA:oSql:nSystemID == SYSTEMID_MARIADB  .or. ;
         oWA:oSql:nSystemID == SYSTEMID_ORACLE .or. ;
         oWA:oSql:nSystemID == SYSTEMID_MSSQL6 .or. ;
         oWA:oSql:nSystemID == SYSTEMID_MSSQL7 .or. ;
         oWA:oSql:nSystemID == SYSTEMID_CACHE  .or. ;
         oWA:oSql:nSystemID == SYSTEMID_POSTGR )

         oWA:AlterColumnsDirect( aDirect, .T., .F., @aTofix )
      ENDIF

      If len( aToFix ) > 0
         oWA:AlterColumns( aToFix, .T. )
      EndIf

      For i = 1 to len( aToDrop )
         If aToDrop[i,1] == "BACKUP_"
            oWA:DropColumn( aToDrop[i,1], .F. )
         Else
            oWA:DropColumn( aToDrop[i,1], .T. )
         EndIf
      Next

      SELECT (nALias)
      dbCloseArea()

      SR_CleanTabInfoCache()

      // recover table status

      SELECT (nAlias)
      dbUseArea( .F., "SQLRDD", cTblName, cAlias )
      If OrdCount() >= nOrd
         dbSetOrder( nOrd )
      EndIf
      dbGoTo( nReg )

   Else
      SR_RuntimeErr( , "SR_ChengeStructure: Not a SQLRDD workarea." )
   EndIf

Return lOk

/*------------------------------------------------------------------------*/

Function SR_SetCurrDate(d)

   If IS_SQLRDD
      d := (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):SetCurrDate( d )
      If d == NIL
         d := SR_GetActiveDt()
      EndIf
   EndIf

Return d

/*------------------------------------------------------------------------*/

Function SR_QuickAppend(l)

   If IS_SQLRDD
      l := (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):SetQuickAppend(l)
   EndIf

Return l

/*------------------------------------------------------------------------*/

Function SR_SetColPK( cColName )

   If IS_SQLRDD
      (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):SetColPK( cColName )
      If cColName == NIL
         cColName := (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):cColPK
      EndIf
   EndIf

Return cColName

/*------------------------------------------------------------------------*/

Function SR_IsWAHist()

   If IS_SQLRDD
      Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):lHistoric
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

Function SR_SetReverseIndex( nIndex, lSet )

   Local lOldSet

   If IS_SQLRDD .and. nIndex > 0 .and. nIndex <= len( (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):aIndex )
      lOldSet := (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):aIndex[ nIndex, DESCEND_INDEX_ORDER ]
      If HB_ISLOGICAL( lSet )
         (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):aIndex[ nIndex, DESCEND_INDEX_ORDER ] := lSet
      EndIf
   EndIf

Return lOldSet

/*------------------------------------------------------------------------*/

Function SR_SetNextDt(d)

   If IS_SQLRDD
      (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):SetNextDt(d)
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_DisableHistoric()

   If IS_SQLRDD
      (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):DisableHistoric()
      (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):Refresh()
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_EnableHistoric()

   If IS_SQLRDD
      (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):EnableHistoric()
      (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):Refresh()
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_GetActiveDt()

Return DtAtiv

/*------------------------------------------------------------------------*/

Function SR_SetActiveDt(d)

   DEFAULT d := date()

Return DtAtiv := d

/*------------------------------------------------------------------------*/

Function SR_SetActiveDate(d)

   Local dOld := DtAtiv

   If d != NIL
      DtAtiv := d
   EndIf

Return dOld

/*------------------------------------------------------------------------*/

Init Procedure SR_IniDtAtiv()

   DtAtiv := date()

Return

/*------------------------------------------------------------------------*/

Function SR_SetCreateAsHistoric( l )

   Local lOld := lCreateAsHistoric

   If HB_ISLOGICAL( l )
      lCreateAsHistoric := l
   EndIf

Return lCreateAsHistoric

/*------------------------------------------------------------------------*/

Function SR_HasHistoric()

Return (lHistorico := .t.)

/*------------------------------------------------------------------------*/

Function SR_cDBValue( uData, nSystemID )

   default nSystemID := SR_GetConnection():nSystemID

return SR_SubQuoted( valtype( uData ), uData, nSystemID )

/*------------------------------------------------------------------------*/

Static Function SR_SubQuoted( cType, uData, nSystemID )

   local cRet
   lOCAL cOldSet := SET(_SET_DATEFORMAT)

   Do Case
   Case cType $ "CM" .and. nSystemID == SYSTEMID_ORACLE
      return ['] + rtrim(strtran(uData,"'","'||"+"CHR(39)"+"||'")) + [']
   Case cType $ "CM" .and. nSystemID == SYSTEMID_MSSQL7
      return ['] + rtrim(strtran(uData,"'",[']+['])) + [']
   Case cType $ "CM" .and. nSystemID == SYSTEMID_POSTGR
      return [E'] + strtran(rtrim(strtran(uData,"'",[']+['])), "\","\\") + [']
   Case cType $ "CM"
      return ['] + rtrim(strtran(uData,"'","")) + [']
   Case cType == "D" .and. nSystemID == SYSTEMID_ORACLE
      return [TO_DATE('] + rtrim(DtoS(uData)) + [','YYYYMMDD')]
    Case cType == "D" .and. (nSystemID == SYSTEMID_IBMDB2 .or. nSystemID == SYSTEMID_ADABAS )
        return [']+transform(DtoS(uData) ,'@R 9999-99-99')+[']
   Case cType == "D" .and. nSystemID == SYSTEMID_SQLBAS
      return ['] + SR_dtosDot(uData) + [']
   Case cType == "D" .and. nSystemID == SYSTEMID_INFORM
      return ['] + SR_dtoUS(uData) + [']
   Case cType == "D" .and. nSystemID == SYSTEMID_INGRES
      return ['] + SR_dtoDot(uData) + [']
   Case cType == "D" .and. (nSystemID == SYSTEMID_FIREBR .or. nSystemID == SYSTEMID_FIREBR3)
      return [']+transform(DtoS(uData) ,'@R 9999/99/99')+[']

   Case cType == "D" .and. nSystemID == SYSTEMID_CACHE
      return [{d ']+transform(DtoS(if(year(uData)<1850,stod("18500101"),uData)) ,'@R 9999-99-99')+['}]
   Case cType == "D"
      return ['] + dtos(uData) + [']
   Case cType == "N"
      return ltrim(str(uData))
   Case cType == "L" .and. (nSystemID == SYSTEMID_POSTGR .or. nSystemID == SYSTEMID_FIREBR3 )
      return if(uData,"true","false")
   Case cType == "L" .and. nSystemID == SYSTEMID_INFORM
      return if(uData,"'t'","'f'")
   Case cType == "L"
      return if(uData,"1","0")
   case ctype == "T"  .and. nSystemID == SYSTEMID_POSTGR
      IF Empty( uData)
         RETURN 'NULL'
      ENDIF

      return ['] + transform(ttos(uData), '@R 9999-99-99 99:99:99') + [']
   case ctype == "T" .and. nSystemID == SYSTEMID_ORACLE
      IF Empty( uData)
         RETURN 'NULL'
      ENDIF
      return [ TIMESTAMP '] + transform(ttos(uData), '@R 9999-99-99 99:99:99') + [']
   Case cType == 'T'
      IF Empty( uData)
         RETURN 'NULL'
      ENDIF
      Set( _SET_DATEFORMAT,  "yyyy-mm-dd")
      cRet := ttoc( uData )
      Set( _SET_DATEFORMAT,cOldSet)
      RETURN [']+cRet+[']

   OtherWise
      cRet := SR_STRTOHEX(HB_Serialize( uData ))
      return SR_SubQuoted( "C", SQL_SERIALIZED_SIGNATURE + str(len(cRet),10) + cRet, nSystemID )
   EndCase

Return ""

/*------------------------------------------------------------------------*/

Function SR_WriteTimeLog(cComm, oCnn, nLimisencos)

   Local nAlAtual := Select()

   Local TRACE_STRUCT := { ;
                            { "USUARIO",    "C", 10, 0 },;
                            { "DATA",       "D", 08, 0 },;
                            { "HORA",       "C", 08, 0 },;
                            { "CONTADOR",   "C", 01, 0 },;
                            { "TRANSCOUNT", "N", 10, 0 },;
                            { "COMANDO",    "M", 10, 0 },;
                            { "CUSTO",      "N", 12, 0 } ;
                         }

   (oCnn) // to remove warning

   Try

      If !sr_PhFile( "long_qry.dbf" )
         dbCreate( "long_qry.dbf", TRACE_STRUCT, "DBFNTX" )
      EndIf

      While .t.
         dbUseArea( .T., "DBFNTX", "long_qry.dbf", "LONG_QRY", .T., .F. )
         If !NetErr()
            exit
         EndIf
         ThreadSleep( 500 )
      EndDo

      LONG_QRY->( dbAppend() )
      Replace LONG_QRY->DATA         with Date()
      Replace LONG_QRY->HORA         with Time()
      Replace LONG_QRY->COMANDO      with cComm
      Replace LONG_QRY->CUSTO        with nLimisencos
      LONG_QRY->( dbCloseArea() )

   Catch

   End

   dbSelectArea( nAlAtual )

Return NIL

/*------------------------------------------------------------------------*/

Function SR_uCharToVal( cVal, cType, nLen )

   Do Case
   Case cType == "C"
      If nLen == NIL
         Return cVal
      Else
         Return PadR( cVal, nLen )
      EndIf
   Case cType == "M"
      Return cVal
   Case cType == "D"
      Return ctod( cVal )
   Case cType == "N"
      Return val( cVal )
   Case cType == "L"
      Return cVal $ "1.T.SYsy.t."
   EndCase

   Return ""

/*------------------------------------------------------------------------*/

Function SR_WriteDbLog(cComm, oCnn)

   Local nAlAtual := Select()

   Local TRACE_STRUCT   := { ;
                              { "USUARIO",    "C", 10, 0 },;
                              { "DATA",       "D", 08, 0 },;
                              { "HORA",       "C", 08, 0 },;
                              { "CONTADOR",   "C", 01, 0 },;
                              { "TRANSCOUNT", "N", 10, 0 },;
                              { "COMANDO",    "M", 10, 0 } ;
                           }

   (oCnn) // To remove warning

   DEFAULT cComm := ""

   Try

      If !sr_phFile( "sqllog.dbf" )
         dbCreate( "sqllog.dbf", TRACE_STRUCT, "DBFNTX" )
      EndIf

      While .T.
         dbUseArea( .T., "DBFNTX", "sqllog.dbf", "SQLLOG", .T., .F. )
         If !NetErr()
            exit
         EndIf
         ThreadSleep( 500 )
      EndDo

      SQLLOG->( dbAppend() )
      Replace SQLLOG->DATA         with Date()
      Replace SQLLOG->HORA         with Time()
      Replace SQLLOG->COMANDO      with cComm
      SQLLOG->( dbCloseArea() )

   Catch

   End

   dbSelectArea( nAlAtual )

Return NIL

/*------------------------------------------------------------------------*/

Function SR_ShowVector( a )

   local cRet := "", i

   If HB_ISARRAY(a)

      cRet := "{"

      For i = 1 to len(a)

         If HB_ISARRAY(a[i])
            cRet += SR_showvector(a[i]) + if( i == len(a), "", "," ) + CRLF
         Else
            cRet += SR_Val2CharQ(a[i]) + if( i == len(a), "", "," )
         EndIf

      Next

      cRet += "}"

   Else

      cRet += SR_Val2CharQ(a)

   EndIf

Return cRet

/*------------------------------------------------------------------------*/

Function SR_Val2CharQ( uData )

   local cType := valtype( uData )

   Do Case
   Case cType == "C"
      //Return (["] + uData + ["])
      Return AllTrim(uData)
   Case cType == "N"
      Return alltrim(Str(uData))
   Case cType == "D"
      Return dtoc(uData)
   Case cType == "T"
      Return ttoc(uData)
   Case cType == "L"
      Return if(uData,".T.",".F.")
   Case cType == "A"
      Return "{Array}"
   Case cType == "O"
      Return "{Object}"
   Case cType == "B"
      Return "{||Block}"
   OtherWise
      Return "NIL"
   EndCase

Return "NIL"

/*------------------------------------------------------------------------*/

Function SR_BlankVar( cType, nLen, nDec )
Local nVal

   (nDec) // To remove warning

   Do Case
   Case cType $ "MC"
      Return Space(nLen)
   Case cType = "L"
      Return .F.
   Case cType = "D"
      Return ctod('')
   Case cType = "N"
      if nDec >0
         switch ndec
         case 1
            nVal := 0.0
            exit
         case 2
         nVal := 0.00
         exit
         case 3
         nVal :=0.000
         exit
         case 4
         nVal :=0.0000
         exit
         case 5
         nVal :=0.00000
         exit
         case 6
         nVal :=0.000000
         exit
         default

         nVal := 0.00
         exit
         end
         return nVal
      endif
      return 0
   Case cType == 'T'
      return datetime(0,0,0,0,0,0,0)
   EndCase

Return ""

/*------------------------------------------------------------------------*/

Function SR_HistExpression(n, cTable, cPK, CurrDate, nSystem)

   local cRet, cAl1, cAl2, cAlias, oCnn

   oCnn := SR_GetConnection()

   cAlias := "W" + StrZero(++_nCnt,3)
   cAl1   := "W" + StrZero(++_nCnt,3)
   cAl2   := "W" + StrZero(++_nCnt,3)

   If _nCnt >= 995
      _nCnt := 1
   EndIf

   DEFAULT CurrDate := SR_GetActiveDt()
   DEFAULT n := 0
   DEFAULT nSystem := oCnn:nSystemID

   cRet := "SELECT " + cAlias + ".* FROM " + cTable + " " + cAlias + " WHERE " + CRLF

   cRet += "(" + cAlias + ".DT__HIST = (SELECT" + if(n=3," MIN(", " MAX(") + cAl1 + ".DT__HIST) FROM "
   cRet += cTable + " " + cAl1 + " WHERE " + cAlias + "." + cPK + "="
   cRet += cAl1 + "." + cPk

   If n = 0
      cRet += " AND " + cAl1 + ".DT__HIST <= " + SR_cDBValue( CurrDate )
   endif

   cRet += "))"

Return cRet

/*------------------------------------------------------------------------*/

Function SR_HistExpressionWhere(n, cTable, cPK, CurrDate, nSystem, cAlias)

   local cRet, cAl1, cAl2, oCnn

   oCnn := SR_GetConnection()

   cAl1   := "W" + StrZero(++_nCnt,3)
   cAl2   := "W" + StrZero(++_nCnt,3)

   If _nCnt >= 995
      _nCnt := 1
   EndIf

   DEFAULT CurrDate := SR_GetActiveDt()
   DEFAULT n := 0
   DEFAULT nSystem := oCnn:nSystemID

   cRet := ""

   cRet += "(" + cAlias + ".DT__HIST = (SELECT" + if(n=3," MIN(", " MAX(") + cAl1 + ".DT__HIST) FROM "
   cRet += cTable + " " + cAl1 + " WHERE " + cAlias + "." + cPK + "="
   cRet += cAl1 + "." + cPk

   If n = 0
      cRet += " AND " + cAl1 + ".DT__HIST <= " + SR_cDBValue( CurrDate )
   endif

   cRet += "))"

Return  cRet

/*------------------------------------------------------------------------*/

Function SR_SetNextSvVers( lVers )

   If IS_SQLRDD
      (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):lVers := lVers
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_GetRddName( nArea )

   DEFAULT nArea := Select()

   Do Case
   Case Empty( Alias( nArea ) )
      Return "    "
   OtherWise
      Return ( nArea )->( RddName() )
   EndCase

Return ""

/*------------------------------------------------------------------------*/

Function IsSQLWorkarea()

Return "*" + SR_GetRddName() + "*" $ "*SQLRDD*ODBCRDD*SQLEX*"

/*------------------------------------------------------------------------*/

Function SR_OrdCondSet( cForSql, cForxBase )

   If IS_SQLRDD
      (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):OrdSetForClause( cForSql, cForxBase )
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_SetJoin( nAreaTarget, cField, nAlias, nOrderTarget )

   (nAreaTarget)
   (cField)
   (nAlias)
   (nOrderTarget)

   SR_RuntimeErr( , "SR_SetJoin() is no longer supported" )

Return NIL

/*------------------------------------------------------------------------*/

Function SR_AddRuleNotNull( cCol )

   local lRet

   If IS_SQLRDD
      lRet := (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):AddRuleNotNull( cCol )
      SR_CleanTabInfoCache()
      Return lRet
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

Function SR_Deserialize( uData )
* Local ctemp,cdes,chex
* cTemp := udata
* altd()
* cHex := SR_HEXTOSTR(SubStr( uData, 21, val( substr(uData,11,10) ) ) )
* cdes := sr_Deserialize1( cHex)
* tracelog(udata,chex,cdes)
* return cdes
Return SR_Deserialize1( SR_HEXTOSTR(SubStr( uData, 21, val( substr(uData,11,10) ) ) ) )

/*------------------------------------------------------------------------*/

Function SR_DropRuleNotNull( cCol )

   local lRet

   If IS_SQLRDD
      lRet := (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):DropRuleNotNull( cCol )
      SR_CleanTabInfoCache()
      Return lRet
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

Function SR_LastSQLError()

   If IS_SQLRDD
      Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):oSql:cSQLError
   EndIf

Return ""

/*------------------------------------------------------------------------*/

Function SR_SetFilter( cFlt )

   Local oWA, uRet

   If IS_SQLRDD
      oWA := (Select())->(dbInfo( DBI_INTERNAL_OBJECT ))
      uRet := oWA:cFilter
      If !Empty( cFlt )
         oWA:cFilter := cFlt
         oWA:Refresh()
      ElseIf HB_ISSTRING( cFlt )
         oWA:cFilter := ''
      EndIf
   EndIf

Return uRet

/*------------------------------------------------------------------------*/

Function SR_ResetStatistics()

   If IS_SQLRDD
      (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):ResetStatistics()
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_GetnConnection()

   If IS_SQLRDD
      Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT ):oSql:nID )
   EndIf

Return 0

/*------------------------------------------------------------------------*/

Function SR_HasFilters()

   If IS_SQLRDD
      Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):HasFilters()
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

Function SR_dbRefresh()

   Local oWA

   If IS_SQLRDD
      oWA := (Select())->(dbInfo( DBI_INTERNAL_OBJECT ))
      oWA:Refresh()
      If !oWA:aInfo[ AINFO_EOF ]
         oWA:sqlGoTo( oWA:aInfo[ AINFO_RECNO ] )
      Else
         oWA:sqlGoPhantom()
      EndIf
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

CLASS SqlFastHash

   DATA hHash, nPartSize

   METHOD New( nPartSize )
   METHOD Insert( uHashKey, xValue )
   METHOD Find( uHashKey, nIndex, nPart )    /* nIndex and nPart by ref */
   METHOD Delete( uHashKey )
   METHOD Update( uHashKey, uValue )
   METHOD UpdateIndex( nPos, nPart, uValue )
   METHOD Haeval( bExpr )
ENDCLASS

/*------------------------------------------------------------------------*/

METHOD Haeval( bExpr ) CLASS SqlFastHash

Return Heval( ::hHash, bExpr )

/*------------------------------------------------------------------------*/

METHOD New( nPartSize ) CLASS SqlFastHash

   ::nPartSize := nPartSize
   ::hHash := {=>}
   If nPartSize != NIL
      HSetPartition( ::hHash, nPartSize )
   EndIf

RETURN Self

/*------------------------------------------------------------------------*/

METHOD Insert( uHashKey, xValue ) CLASS SqlFastHash

   If len(::hHash) > HASH_TABLE_SIZE
      ::hHash := { => }          /* Reset hash table */
      HB_GCALL(.T.)              /* Release memory blocks */
   EndIf

   ::hHash[uHashKey] := xValue

RETURN .T.

/*------------------------------------------------------------------------*/

METHOD Find( uHashKey, nIndex, nPart ) CLASS SqlFastHash
   LOCAL aData

   nIndex := HGetPos(::hHash,uHashKey)

   If nIndex > 0
      aData := HGetValueAt( ::hHash, nIndex )
   EndIf

   nPart := 1     /* Compatible with old version */

RETURN aData

/*------------------------------------------------------------------------*/

METHOD Delete( uHashKey ) CLASS SqlFastHash
   LOCAL nIndex := 0

   nIndex := HGetPos(::hHash,uHashKey)

   If nIndex > 0
      HDelAt( ::hHash, nIndex )
      Return .T.
   EndIf

RETURN .F.

/*------------------------------------------------------------------------*/

METHOD Update( uHashKey, uValue ) CLASS SqlFastHash
   LOCAL nIndex := 0

   nIndex := HGetPos(::hHash,uHashKey)

   If nIndex > 0
      HSetValueAt( ::hHash, nIndex, uValue )
      Return .T.
   EndIf

RETURN .F.

/*------------------------------------------------------------------------*/

METHOD UpdateIndex( nPos, nPart, uValue ) CLASS SqlFastHash
   /* nPart not used - Compatible with old version */
   (nPart)
   HSetValueAt( ::hHash, nPos, uValue )
RETURN .F.

/*------------------------------------------------------------------------*/

Function SR_BeginTransaction(nCnn)

   local oCnn

   If HB_ISOBJECT( nCnn )
      oCnn := nCnn
   Else
      oCnn := SR_GetConnection( nCnn )
   EndIf

   If oCnn != NIL
      If oCnn:nTransacCount == 0       // Commit any changes BEFORE Begin Transaction
         oCnn:Commit()
      EndIf
      oCnn:nTransacCount ++

      If oCnn:nSystemID == SYSTEMID_CACHE
         oCnn:exec( "START TRANSACTION %COMMITMODE EXPLICIT ISOLATION LEVEL READ COMMITTED" )
//         oCnn:exec( "START TRANSACTION %COMMITMODE EXPLICIT" )
      EndIf

   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_CommitTransaction(nCnn)

   local oCnn

   If HB_ISOBJECT( nCnn )
      oCnn := nCnn
   Else
      oCnn := SR_GetConnection( nCnn )
   EndIf

   If oCnn != NIL
      If (oCnn:nTransacCount - 1) == 0
         oCnn:Commit()
         oCnn:nTransacCount := 0
      ElseIf (oCnn:nTransacCount - 1) > 0
         oCnn:nTransacCount --
      EndIf
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_SetAppSite( nCnn, cSite )

   local oCnn, cOld

   If HB_ISOBJECT( nCnn )
      oCnn := nCnn
   Else
      oCnn := SR_GetConnection( nCnn )
   EndIf

   If oCnn != NIL
      cOld := oCnn:cSite
      If cSite != NIL
         oCnn:cSite := cSite
      EndIf
   EndIf

Return cOld

/*------------------------------------------------------------------------*/

Function SR_SetConnectionLogChanges( nCnn, nOpt )

   local oCnn, nOld

   If HB_ISOBJECT( nCnn )
      oCnn := nCnn
   Else
      oCnn := SR_GetConnection( nCnn )
   EndIf

   If oCnn != NIL
      nOld := oCnn:nLogMode
      If nOpt != NIL
         oCnn:nLogMode := nOpt
      EndIf
   EndIf

Return nOld

/*------------------------------------------------------------------------*/

Function SR_SetAppUser( nCnn, cUsername )

   local oCnn, cOld

   If HB_ISOBJECT( nCnn )
      oCnn := nCnn
   Else
      oCnn := SR_GetConnection( nCnn )
   EndIf

   If oCnn != NIL
      cOld := oCnn:cAppUser
      If cUsername != NIL
         oCnn:cAppUser := cUsername
      EndIf
   EndIf

Return cOld

/*------------------------------------------------------------------------*/

Function SR_SetALockWait( nCnn, nSeconds )

   local oCnn, nOld

   If HB_ISOBJECT( nCnn )
      oCnn := nCnn
   Else
      oCnn := SR_GetConnection( nCnn )
   EndIf

   If oCnn != NIL
      nOld := oCnn:nLockWaitTime
      oCnn:nLockWaitTime := nSeconds
   EndIf

Return nOld

/*------------------------------------------------------------------------*/

Function SR_RollBackTransaction(nCnn)

   local oCnn

   If HB_ISOBJECT( nCnn )
      oCnn := nCnn
   Else
      oCnn := SR_GetConnection( nCnn )
   EndIf

   If oCnn != NIL
      If oCnn:nTransacCount >  0
         oCnn:nTransacCount := 0
         // Should CLEAN UP ALL workareas BEFORE issue the ROLLBACK
         _SR_ScanExecAll( { |y,x| (y), aeval( x, { |z| z:Refresh( .F. ) } ) } )
         oCnn:RollBack()
      EndIf
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_TransactionCount(nCnn)

   local oCnn

   If HB_ISOBJECT( nCnn )
      oCnn := nCnn
   Else
      oCnn := SR_GetConnection( nCnn )
   EndIf

   If oCnn != NIL
      Return oCnn:nTransacCount
   EndIf

Return 0

/*------------------------------------------------------------------------*/

Function SR_EndTransaction(nCnn)

   local oCnn

   If HB_ISOBJECT( nCnn )
      oCnn := nCnn
   Else
      oCnn := SR_GetConnection( nCnn )
   EndIf

   If oCnn != NIL
      If oCnn:nTransacCount >  0
         oCnn:Commit()
         oCnn:nTransacCount := 0
      EndIf
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

Function SR_RuntimeErr( cOperation, cErr )

   Local oErr := ErrorNew()
   Local cDescr

   DEFAULT cOperation := "SQLRDD"
   DEFAULT cErr := "RunTimeError"

   cDescr := alltrim( cErr )

   oErr:genCode       := 99
   oErr:CanDefault    := .F.
   oErr:Severity      := ES_ERROR
   oErr:CanRetry      := .T.
   oErr:CanSubstitute := .F.
   oErr:Description   := cDescr + " - RollBack executed."
   oErr:subSystem     := "SQLRDD"
   oErr:operation     := cOperation
   oErr:OsCode        := 0

   SR_LogFile( "sqlerror.log", { cDescr } )

   Throw( oErr )

Return NIL

/*------------------------------------------------------------------------*/

Function dbCount()

   If IS_SQLRDD
      Return (Select())->(dbInfo( DBI_INTERNAL_OBJECT )):KeyCount()
   EndIf

Return 0

/*------------------------------------------------------------------------*/

Function SR_GetStack()

   Local i := 1, cErrorLog := ""

   while ( i < 70 )
       If ! Empty( ProcName( i ) )
          cErrorLog += CRLF + Trim( ProcName( i ) ) + "     Linha : " + alltrim(str(ProcLine(i)))
       EndIf
       i++
   end

Return  cErrorLog

/*------------------------------------------------------------------------*/

/*

Alert() copied as SQLBINDBYVAL() -> DEMO banner protection

*/

//#include "hbsetup.ch"
#include "box.ch"
#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"

/* TOFIX: Clipper defines a clipped window for Alert() [vszakats] */

/* NOTE: Clipper will return NIL if the first parameter is not a string, but
         this is not documented. This implementation converts the first
         parameter to a string if another type was passed. You can switch back
         to Clipper compatible mode by defining constant
         HB_C52_STRICT. [vszakats] */

/* NOTE: Clipper handles these buttons { "Ok", "", "Cancel" } in a buggy way.
         This is fixed. [vszakats] */

/* NOTE: nDelay parameter is a Harbour extension. */

#define INRANGE( xLo, xVal, xHi )       ( xVal >= xLo .AND. xVal <= xHi )


FUNCTION SQLBINDBYVAL( xMessage, aOptions, cColorNorm, nDelay )

   LOCAL nChoice
   LOCAL aSay, nPos, nWidth, nOpWidth, nInitRow, nInitCol
   LOCAL nKey, aPos, nCurrent, aHotkey, aOptionsOK, cEval
   LOCAL cColorHigh

   LOCAL nOldRow
   LOCAL nOldCol
   LOCAL nOldCursor
   LOCAL cOldScreen

   LOCAL nOldDispCount
   LOCAL nCount
   LOCAL nLen, sCopy
   LOCAL lWhile

   LOCAL cColorStr,cColorPair1,cColorPair2,cColor11,cColor12,cColor21,cColor22
   LOCAL nCommaSep,nSlash

#ifdef HB_COMPAT_C53
   LOCAL nMRow, nMCol
#endif

   /* TOFIX: Clipper decides at runtime, whether the GT is linked in,
             if it is not, the console mode is choosen here. [vszakats] */
   LOCAL lConsole := .F.

#ifdef HB_C52_UNDOC

   DEFAULT s_lNoAlert TO hb_argCheck( "NOALERT" )

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

   aSay := {}

#ifdef HB_C52_STRICT

   IF !ISCHARACTER( xMessage )
      RETURN NIL
   ENDIF

   DO WHILE ( nPos := At( ';', xMessage ) ) != 0
      AAdd( aSay, Left( xMessage, nPos - 1 ) )
      xMessage := SubStr( xMessage, nPos + 1 )
   ENDDO
   AAdd( aSay, xMessage )

#else

   IF PCount() == 0
      RETURN NIL
   ENDIF

   IF ISARRAY( xMessage )

      FOR EACH cEval IN xMessage
         IF ISCHARACTER( cEval )
            AAdd( aSay, cEval )
         ENDIF
      NEXT

   ELSE

      SWITCH ValType( xMessage )
         CASE "C"
         CASE "M"
            EXIT

         CASE "N"
            xMessage := LTrim( Str( xMessage ) )
            EXIT

         CASE "D"
            xMessage := DToC( xMessage )
            EXIT

         CASE "T"
            xMessage := TToC( xMessage )
            EXIT

         CASE "L"
            xMessage := iif( xMessage, ".T.", ".F." )
            EXIT

         CASE "O"
            xMessage := xMessage:className + " Object"
            EXIT

         CASE "B"
            xMessage := "{||...}"
            EXIT

         DEFAULT
            xMessage := "NIL"
      END

      DO WHILE ( nPos := At( ';', xMessage ) ) != 0
         AAdd( aSay, Left( xMessage, nPos - 1 ) )
         xMessage := SubStr( xMessage, nPos + 1 )
      ENDDO
      AAdd( aSay, xMessage )

      FOR EACH xMessage IN aSay

         IF ( nLen := Len( xMessage ) ) > 58
            FOR nPos := 58 TO 1 STEP -1
               IF xMessage[nPos] $ ( " " + Chr( 9 ) )
                  EXIT
               ENDIF
            NEXT

            IF nPos == 0
               nPos := 58
            ENDIF

            sCopy := xMessage
            aSay[ HB_EnumIndex() ] := RTrim( Left( xMessage, nPos ) )

            IF Len( aSay ) == HB_EnumIndex()
               aAdd( aSay, SubStr( sCopy, nPos + 1 ) )
            ELSE
               aIns( aSay, HB_EnumIndex() + 1, SubStr( sCopy, nPos + 1 ), .T. )
            ENDIF
        ENDIF
      NEXT

   ENDIF

#endif

   IF !ISARRAY( aOptions )
      aOptions := {}
   ENDIF

   IF !ISCHARACTER( cColorNorm ) .or. EMPTY( cColorNorm )
      cColorNorm := "W+/R" // first pair color (Box line and Text)
      cColorHigh := "W+/B" // second pair color (Options buttons)
   ELSE

      /* NOTE: Clipper Alert does not handle second color pair properly.
               If we inform the second color pair, xHarbour alert will consider it.
               if we not inform the second color pair, then xHarbour alert will behave
               like Clipper.  2004/Sep/16 - Eduardo Fernandes <modalsist> */

      cColor11 := cColor12 := cColor21 := cColor22 := ""

      cColorStr := alltrim( StrTran( cColorNorm," ","") )
      nCommaSep := At(",",cColorStr)

      if nCommaSep > 0 // exist more than one color pair.
         cColorPair1 := SubStr( cColorStr, 1, nCommaSep - 1 )
         cColorPair2 := SubStr( cColorStr, nCommaSep + 1 )
      else
         cColorPair1 := cColorStr
         cColorPair2 := ""
      endif

      nSlash := At("/",cColorPair1)

      if nSlash > 1
         cColor11 := SubStr( cColorPair1,1,nSlash-1)
         cColor12 := SubStr( cColorPair1,nSlash+1)
      else
         cColor11 := cColorPair1
         cColor12 := "R"
      endif

      if ColorValid(cColor11) .and. ColorValid(cColor12)

        // if color pair is passed in numeric format, then we need to convert for
        // letter format to avoid blinking in some circumstances.
        if IsDigit( cColor11 )
           cColor11 := COLORLETTER( cColor11 )
        endif

        cColorNorm := cColor11

        if !empty(cColor12)

            if IsDigit( cColor12 )
               cColor12 := COLORLETTER( cColor12 )
            endif

            cColorNorm := cColor11+"/"+cColor12

        endif

      else
         cColor11 := "W+"
         cColor12 := "R"
         cColorNorm := cColor11+"/"+cColor12
      endif


      // if second color pair exist, then xHarbour alert will handle properly.
      if !empty( cColorPair2 )

         nSlash := At("/",cColorPair2)

         if nSlash > 1
            cColor21 := SubStr( cColorPair2,1,nSlash-1)
            cColor22 := SubStr( cColorPair2,nSlash+1)
         else
            cColor21 := cColorPair2
            cColor22 := "B"
         endif

         if ColorValid(cColor21) .and. ColorValid(cColor22)

            if IsDigit( cColor21 )
               cColor21 := COLORLETTER( cColor21 )
            endif

            cColorHigh := cColor21

            if !empty(cColor22)

                if IsDigit( cColor22 )
                   cColor22 := COLORLETTER( cColor22 )
                endif

                // extracting color attributes from background color.
                cColor22 := StrTran( cColor22, "+", "" )
                cColor22 := StrTran( cColor22, "*", "" )
                cColorHigh := cColor21+"/"+cColor22

            endif

         else
            cColorHigh := "W+/B"
         endif

      else // if does not exist the second color pair, xHarbour alert will behave like Clipper
         if empty(cColor11) .or. empty(cColor12)
            cColor11 := "B"
            cColor12 := "W+"
         else
            cColor11 := StrTran( cColor11, "+", "" )
            cColor11 := StrTran( cColor11, "*", "" )
         endif
         cColorHigh := cColor12+"/"+cColor11
      endif

   ENDIF

   IF nDelay == NIL
      nDelay := 0
   ENDIF

   /* The longest line */
   nWidth := 0
   AEval( aSay, {| x | nWidth := Max( Len( x ), nWidth ) } )

   /* Cleanup the button array */
   aOptionsOK := {}
   FOR EACH cEval IN aOptions
      IF ISCHARACTER( cEval ) .AND. !Empty( cEval )
         AAdd( aOptionsOK, cEval )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { 'Ok' }
#ifdef HB_C52_STRICT
   /* NOTE: Clipper allows only four options [vszakats] */
   ELSEIF Len( aOptionsOK ) > 4
      aSize( aOptionsOK, 4 )
#endif
   ENDIF

   /* Total width of the botton line (the one with choices) */
   nOpWidth := 0
   AEval( aOptionsOK, {| x | nOpWidth += Len( x ) + 4 } )

   /* what's wider ? */
   nWidth := Max( nWidth + 2 + iif( Len( aSay ) == 1, 4, 0 ), nOpWidth + 2 )

   /* box coordinates */
   nInitRow := Int( ( ( MaxRow() - ( Len( aSay ) + 4 ) ) / 2 ) + .5 )
   nInitCol := Int( ( ( MaxCol() - ( nWidth + 2 ) ) / 2 ) + .5 )

   /* detect prompts positions */
   aPos := {}
   aHotkey := {}
   nCurrent := nInitCol + Int( ( nWidth - nOpWidth ) / 2 ) + 2
   AEval( aOptionsOK, {| x | AAdd( aPos, nCurrent ), AAdd( aHotKey, Upper( Left( x, 1 ) ) ), nCurrent += Len( x ) + 4 } )

   nChoice := 1

   IF lConsole

      nCount := Len( aSay )
      FOR EACH cEval IN aSay
         OutStd( cEval )
         IF HB_EnumIndex() < nCount
            OutStd( hb_OSNewLine() )
         ENDIF
      NEXT

      OutStd( " (" )
      nCount := Len( aOptionsOK )
      FOR EACH cEval IN aOptionsOK
         OutStd( cEval )
         IF HB_EnumIndex() < nCount
            OutStd( ", " )
         ENDIF
      NEXT
      OutStd( ") " )

      /* choice loop */
      lWhile := .T.
      DO WHILE lWhile

         nKey := Inkey( nDelay, INKEY_ALL )

         SWITCH nKey
            CASE 0
               lWhile := .F.
               EXIT

            CASE K_ESC

               nChoice := 0
               lWhile  := .F.
               EXIT

            DEFAULT
               IF Upper( Chr( nKey ) ) $ aHotkey
                  nChoice := aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } )
                  lWhile  := .F.
               ENDIF

         END

      ENDDO

      OutStd( Chr( nKey ) )

   ELSE

      /* PreExt */
      nCount := nOldDispCount := DispCount()

      DO WHILE nCount-- != 0
         DispEnd()
      ENDDO

      /* save status */
      nOldRow := Row()
      nOldCol := Col()
      nOldCursor := SetCursor( SC_NONE )
      cOldScreen := SaveScreen( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1 )

      /* draw box */
      DispBox( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1, B_SINGLE + ' ', cColorNorm )

      FOR EACH cEval IN aSay
         DispOutAt( nInitRow + HB_EnumIndex(), nInitCol + 1 + Int( ( ( nWidth - Len( cEval ) ) / 2 ) + .5 ), cEval, cColorNorm )
      NEXT

      /* choice loop */
      lWhile := .T.
      DO WHILE lWhile

         nCount := Len( aSay )
         FOR EACH cEval IN aOptionsOK
            DispOutAt( nInitRow + nCount + 2, aPos[ HB_EnumIndex() ], " " + cEval + " ", cColorNorm )
         NEXT
         DispOutAt( nInitRow + nCount + 2, aPos[ nChoice ], " " + aOptionsOK[ nChoice ] + " ", cColorHigh )

         nKey := Inkey( nDelay, INKEY_ALL )

         SWITCH nKey
            CASE K_ENTER
            CASE K_SPACE
            CASE 0
               lWhile := .F.
               EXIT

            CASE K_ESC
               nChoice := 0
               lWhile  := .F.
               EXIT

#ifdef HB_COMPAT_C53

            CASE K_LBUTTONDOWN

               nMRow  := MRow()
               nMCol  := MCol()
               nPos   := 0
               nCount := Len( aSay )

               FOR EACH cEval IN aOptionsOK
                  IF nMRow == nInitRow + nCount + 2 .AND. ;
                       INRANGE( aPos[ HB_EnumIndex() ], nMCol, aPos[ HB_EnumIndex() ] + Len( cEval ) + 2 - 1 )
                     nPos := HB_EnumIndex()
                     EXIT
                  ENDIF
               NEXT

               IF nPos > 0
                  nChoice := nPos
                  lWhile := .F.
               ENDIF

               EXIT

#endif

            CASE K_LEFT
            CASE K_SH_TAB
               IF Len( aOptionsOK ) > 1

                  nChoice--
                  IF nChoice == 0
                     nChoice := Len( aOptionsOK )
                  ENDIF

                  nDelay := 0
               ENDIF
               EXIT

            CASE K_RIGHT
            CASE K_TAB
               IF Len( aOptionsOK ) > 1

                  nChoice++
                  IF nChoice > Len( aOptionsOK )
                     nChoice := 1
                  ENDIF

                  nDelay := 0
               ENDIF
               EXIT

            DEFAULT
               IF Upper( Chr( nKey ) ) $ aHotkey

                  nChoice := aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } )
                  lWhile  := .F.
               ENDIF

         END

      ENDDO

      /* Restore status */
      RestScreen( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1, cOldScreen )
      SetCursor( nOldCursor )
      SetPos( nOldRow, nOldCol )

      /* PostExt */
      DO WHILE nOldDispCount-- != 0
         DispBegin()
      ENDDO

   ENDIF

RETURN nChoice

//-----------------------------------//
// 2004/Setp/15 - Eduardo Fernandes
// Convert number color format to character color format.
STATIC FUNCTION COLORLETTER( cColor )

Local nColor

  if !IsCharacter( cColor )
     cColor:=""
  endif

  cColor := StrTran( cColor, " ","")
  cColor := StrTran( cColor, "*","")
  cColor := StrTran( cColor, "+","")

  nColor := Abs( Val( cColor ) )


  if nColor=0
     cColor:="N"
  elseif nColor=1
     cColor:="B"
  elseif nColor=2
     cColor:="G"
  elseif nColor=3
     cColor:="BG"
  elseif nColor=4
     cColor:="R"
  elseif nColor=5
     cColor:="RB"
  elseif nColor=6
     cColor:="GR"
  elseif nColor=7
     cColor:="W"
  elseif nColor=8
     cColor:="N+"
  elseif nColor=9
     cColor:="B+"
  elseif nColor=10
     cColor:="G+"
  elseif nColor=11
     cColor:="BG+"
  elseif nColor=12
     cColor:="R+"
  elseif nColor=13
     cColor:="RB+"
  elseif nColor=14
     cColor:="GR+"
  elseif nColor=15
     cColor:="W+"
  else
     cColor:="W+" // 15 is the max.
  endif

RETURN ( cColor )

//-----------------------------------//
// 2004/Setp/15 - Eduardo Fernandes
// Test vality of the color string
STATIC FUNCTION COLORVALID( cColor )

if !IsCharacter( cColor )
   Return .F.
endif

cColor := StrTran( cColor, " ","" )
cColor := StrTran( cColor, "*","" )
cColor := StrTran( cColor, "+","" )
cColor := Upper( cColor )

if cColor=="0"  .or.;
   cColor=="1"  .or.;
   cColor=="2"  .or.;
   cColor=="3"  .or.;
   cColor=="4"  .or.;
   cColor=="5"  .or.;
   cColor=="6"  .or.;
   cColor=="7"  .or.;
   cColor=="8"  .or.;
   cColor=="9"  .or.;
   cColor=="10" .or.;
   cColor=="11" .or.;
   cColor=="12" .or.;
   cColor=="13" .or.;
   cColor=="14" .or.;
   cColor=="15" .or.;
   cColor=="B"  .or.;
   cColor=="BG" .or.;
   cColor=="G"  .or.;
   cColor=="GR" .or.;
   cColor=="N"  .or.;
   cColor=="R"  .or.;
   cColor=="RB" .or.;
   cColor=="W"

   Return .T.

ENDIF

Return .F.

#PRAGMA BEGINDUMP

#include "compat.h"

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"

#ifndef HB_PATH_MAX
#define HB_PATH_MAX     264 /* with trailing 0 byte */
#endif

/* TODO: Xbase++ has an extension where the second parameter can specify
         the required attribute. */

HB_FUNC( SR_PHFILE )
{
   PHB_ITEM pFile = hb_param( 1, HB_IT_STRING );
   hb_retl( ( pFile && hb_itemGetCLen( pFile ) < HB_PATH_MAX - 1 ) ? hb_spFile( hb_itemGetCPtr( pFile ), NULL ) : HB_FALSE );
}

#PRAGMA ENDDUMP

function sr_AddToFilter( nRecNo )
   Local oWA

   If IS_SQLRDD
      oWA := (Select())->(dbInfo( DBI_INTERNAL_OBJECT ))

      If !Empty( oWA:cFilter )
         aadd( oWA:aRecnoFilter, nRecno )
         oWA:Refresh()
      EndIf
   EndIf

Return nil

FUNCTION sr_clearFilter()
   Local oWa
   If IS_SQLRDD
      oWA := (Select())->(dbInfo( DBI_INTERNAL_OBJECT ))

      If !Empty( oWA:cFilter )
         oWA:aRecnoFilter := {}
         oWA:Refresh()
      EndIf
   EndIf

Return nil


FUNCTION SR_SetFieldDefault( cTable, cField, cDefault )
   LOCAL oCnn
   LOCAL cSql := "ALTER TABLE "+ cTable + " ALTER COLUMN " +cField +" SET DEFAULT "
   oCnn := SR_GetConnection(  )
   IF HB_ISNUMERIC( cDefault )
      cSql += Alltrim( str( cDefault ) )
   ELSEIF HB_ISSTRING( cDefault )
      IF Empty( cDefault)
         cSql += "''"
      ELSE
         cSql +="'"+cDefault+"'"
      ENDIF
   ENDIF
   IF oCnn:nSystemId==SYSTEMID_POSTGR
      oCnn:exec( cSql,,.f.)
      oCnn:Commit()
   ENDIF
RETURN NIL






FUNCTION SR_Deserialize1( cSerial, nMaxLen, lRecursive, aObj, aHash, aArray, aBlock )
return HB_Deserialize( cSerial, nMaxLen, lRecursive, aObj, aHash, aArray, aBlock )