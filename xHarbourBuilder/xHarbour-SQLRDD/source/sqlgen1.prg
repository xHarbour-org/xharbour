/* $CATEGORY$SQLRDD/Parser$FILES$sql.lib$
* SQL Code Generator
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "compat.ch"
#include "error.ch"
#include "sqlrdd.ch"
#include "msg.ch"
#include "hbsql.ch"
#include "sqlrddsetup.ch"

/*
* Readble Macros
*/

#define cJoinWords( nType, nSystemID )    aJoinWords[nSystemID,nType]

#define  SKIPFWD            nIP++;uData:=apCode[nIP]
#define  PARAM_SOLV         if(valtype(aParam[uData+1])=="B",eval(aParam[uData+1]),aParam[uData+1])
#define  RECURSIVE_CALL     nIP++;cSql+=SR_SQLCodeGen2(apCode,aParam,nSystemId,lIdent,@nIP,nContext,@nSpaces,lParseTableName);Exit
#define  GETPARAM           cSql+=if(uData+1<=len(aParam),PARAM_SOLV,"##PARAM_"+strzero(uData+1,3)+"_NOT_SUPPLIED##");nIP++;Exit
#define  GETPARAM_QUOTED    cSql+=if(uData+1<=len(aParam),SR_DBQUALIFY( PARAM_SOLV, nSystemID ),"##PARAM_"+strzero(uData+1,3)+"_NOT_SUPPLIED##");nIP++;Exit
#define  GETPARAM_VALUE     cSql+=if(uData+1<=len(aParam),SR_SQLQuotedString(PARAM_SOLV,nSystemID),"##PARAM_"+strzero(uData+1,3)+"_NOT_SUPPLIED##");nIP++;Exit
#define  GETPARAM_VAL_2     uData:=if(uData+1<=len(aParam),SR_DBQUALIFY(PARAM_SOLV,nSystemID),"##PARAM_"+strzero(uData+1,3)+"_NOT_SUPPLIED##")
#define  GETPARAM_VALNN     cSql+=if(uData+1<=len(aParam),SR_SQLQuotedString(PARAM_SOLV,nSystemID,.T.),"##PARAM_"+strzero(uData+1,3)+"_NOT_NULL_NOT_SUPPLIED##");nIP++;Exit
#define  FIX_PRE_WHERE      if(nContext==SQL_CONTEXT_SELECT_PRE_WHERE,(nContext:=SQL_CONTEXT_SELECT_WHERE,cSql+=" WHERE "),if(nContext==SQL_CONTEXT_SELECT_PRE_WHERE2,(nContext:=SQL_CONTEXT_SELECT_WHERE,cSql+=" AND "),))
#define  PASSTHROUGH        nIP++;Exit
#define  IDENTSPACE         space(nSpaces)
//#define  TABLE_OPTIMIZER    if(nSystemId==SYSTEMID_MSSQL7,if(lLocking," WITH (UPDLOCK)", " WITH (NOLOCK)"),"")
#define  TABLE_OPTIMIZER    if(nSystemId==SYSTEMID_MSSQL7,if(lLocking," WITH (UPDLOCK)", ""),"")
#define  COMMAND_OPTIMIZER  if(nSystemId==SYSTEMID_SYBASE,if(lLocking,"", " AT ISOLATION READ UNCOMMITTED "),"")
#define  SELECT_OPTIMIZER1  ""
#define  SELECT_OPTIMIZER2  if(nSystemId==SYSTEMID_ORACLE,if(lLocking," FOR UPDATE", ""),"")
#define  NEWLINE            if(lIdent,CRLF,"")

#xtranslate Default( <Var>, <xVal> ) => IIF( <Var> == NIL, <Var> := <xVal>, )

Static bTableInfo, bIndexInfo

Static nRecordNum := 0
Static bNextRecord
Static aJoinWords

/*
* SQL Code generation
*/

Function SR_SQLCodeGen( apCode, aParam, nSystemId, lIdent, lParseTableName )
Return   SR_SQLCodeGen2( apCode, aParam, nSystemId, lIdent, , , , lParseTableName )

Static Function SR_SQLCodeGen2( apCode, aParam, nSystemId, lIdent, nIP, nContext, nSpaces, lParseTableName )

   local cSql, nCommand, uData, nDepht, nErrorId, aRet, nFlt, cTmp
   local cAtual, cAtual2, nPos, cTbl, outer
   local aFilters  := {}
   local lLocking  := .F.
   local nLen      := len( apCode )
   local bError    := Errorblock()
   local aLJoins   := {}             /* A, B, Expression */
   local aTables   := {}             /* TableName */
   local aQualifiedTables   := {}             /* Owner.TableName */
   local aAlias    := {}
   local aOuters   := {}
   local cSqlCols  := ""
   local cTrailler := ""

   Default( nSystemId, SR_GetConnection():nSystemID )
   Default( nIP, 1 )
   Default( aParam, {} )
//   Default( nContext, SQL_CONTEXT_RESET )
   Default( lIdent, .T. )
   Default( lParseTableName, .T. )

   nContext := SQL_CONTEXT_RESET

   If nSpaces == NIL
      nSpaces := 0
   Elseif lIdent
      nSpaces += 2
   EndIf

   cSql      := ""
   nDepht    := 0

   BEGIN SEQUENCE

      WHILE .t.

         If nIp > nLen .or. nDepht < 0    /* nDepht controls recursivity */
            Exit
         EndIf

         nCommand := apCode[nIP]

         SWITCH nCommand
         CASE SQL_PCODE_SELECT
            cSql += "SELECT" + SELECT_OPTIMIZER1 + NEWLINE + IDENTSPACE + "  "
            nContext := SQL_CONTEXT_SELECT_LIST
            PASSTHROUGH
         CASE SQL_PCODE_UPDATE
            cSql += "UPDATE " + NEWLINE + IDENTSPACE + "  "
            nContext := SQL_CONTEXT_UPDATE
            PASSTHROUGH
         CASE SQL_PCODE_LOCK
            lLocking := .T.
            PASSTHROUGH
         CASE SQL_PCODE_NOLOCK
            lLocking := .F.
            PASSTHROUGH
         CASE SQL_PCODE_INSERT
            cSql += "INSERT INTO "
            nContext := SQL_CONTEXT_INSERT
            PASSTHROUGH
         CASE SQL_PCODE_INSERT_VALUES
            cSql += NEWLINE + " VALUES" + NEWLINE + IDENTSPACE + "  "
            PASSTHROUGH
         CASE SQL_PCODE_DELETE
            cSql += "DELETE FROM "
            nContext := SQL_CONTEXT_DELETE
            PASSTHROUGH
         CASE SQL_PCODE_COLUMN_NAME
            SKIPFWD
            FIX_PRE_WHERE
            cSql += SR_DBQUALIFY( uData, nSystemID )
            PASSTHROUGH
         CASE SQL_PCODE_COLUMN_BY_VALUE
            SKIPFWD
            FIX_PRE_WHERE
            cSql += SR_SQLQuotedString( uData, nSystemID, .t. )
            PASSTHROUGH
         CASE SQL_PCODE_COLUMN_PARAM
            SKIPFWD
            FIX_PRE_WHERE
            GETPARAM_VALUE
         CASE SQL_PCODE_COLUMN_PARAM_NOTNULL
            SKIPFWD
            FIX_PRE_WHERE
            GETPARAM_VALNN
         CASE SQL_PCODE_COLUMN_NAME_PARAM
            SKIPFWD
            FIX_PRE_WHERE
            GETPARAM_QUOTED
         CASE SQL_PCODE_COLUMN_BINDVAR
            SKIPFWD
            FIX_PRE_WHERE
            cSql += SR_SQLQuotedString( &uData, nSystemID, .t. )
            PASSTHROUGH
         CASE SQL_PCODE_COLUMN_NAME_BINDVAR
            SKIPFWD
            FIX_PRE_WHERE
            cSql += SR_DBQUALIFY( &uData, nSystemID )
            PASSTHROUGH
         CASE SQL_PCODE_COLUMN_ALIAS
            SKIPFWD
            FIX_PRE_WHERE
            cSql += SR_DBQUALIFY( uData, nSystemID  ) + "."
            PASSTHROUGH
         CASE SQL_PCODE_COLUMN_NO_AS
            PASSTHROUGH
         CASE SQL_PCODE_COLUMN_AS
            SKIPFWD
            cSql += [ AS ] + SR_DBQUALIFY( uData, nSystemID )
            PASSTHROUGH
         CASE SQL_PCODE_NO_WHERE
            If len( aFilters ) > 0
               cSql += NEWLINE + IDENTSPACE + "WHERE"
               For nFlt = 1 to len( aFilters )
                  cSql +=  NEWLINE + IDENTSPACE + if(nFlt > 1, " AND ", "  ") + aFilters[nFlt]
               Next
               cSql += " "
            EndIf
            nContext := SQL_CONTEXT_RESET
            PASSTHROUGH
         CASE SQL_PCODE_WHERE
            If len( aFilters ) > 0
               cSql += NEWLINE + IDENTSPACE + "WHERE "
               For nFlt = 1 to len( aFilters )
                  cSql += NEWLINE + IDENTSPACE + if(nFlt > 1, " AND ", "  ") + aFilters[nFlt]
               Next
               nContext := SQL_CONTEXT_SELECT_PRE_WHERE2
            Else
               nContext := SQL_CONTEXT_SELECT_PRE_WHERE
            EndIf
            PASSTHROUGH
         CASE SQL_PCODE_TABLE_NAME
            SKIPFWD
            if lParseTableName
               aRet := eval( bTableInfo, uData, nSystemId )
               aadd( aTables, aRet[TABLE_INFO_TABLE_NAME] )
               aadd( aQualifiedTables, aRet[TABLE_INFO_QUALIFIED_NAME] )
               If nContext == SQL_CONTEXT_UPDATE
                  cSql += aRet[TABLE_INFO_QUALIFIED_NAME]
                  SR_SolveFilters(aFilters,aRet,,nSystemID)
                  cSql +=  NEWLINE + " SET" + NEWLINE + "  "
               EndIf
               If nContext == SQL_CONTEXT_DELETE
                  cSql += aRet[TABLE_INFO_QUALIFIED_NAME]
                  SR_SolveFilters(aFilters,aRet,,nSystemID)
               EndIf
               If nContext == SQL_CONTEXT_INSERT
                  cSql += aRet[TABLE_INFO_QUALIFIED_NAME]
                  cSql +=  NEWLINE + IDENTSPACE + "  "
               EndIf
            Else
               aadd( aTables, uData )
               aadd( aQualifiedTables, uData )
               If nContext == SQL_CONTEXT_UPDATE
                  cSql += uData
                  cSql +=  NEWLINE + " SET" + NEWLINE + "  "
               EndIf
               If nContext == SQL_CONTEXT_DELETE
                  cSql += uData
               EndIf
               If nContext == SQL_CONTEXT_INSERT
                  cSql += uData
                  cSql +=  NEWLINE + IDENTSPACE + "  "
               EndIf
            EndIf

            PASSTHROUGH
         CASE SQL_PCODE_TABLE_NO_ALIAS
            aadd( aAlias, "" )
            PASSTHROUGH
         CASE SQL_PCODE_TABLE_ALIAS
            SKIPFWD
            aadd( aAlias, SR_DBQUALIFY( uData, nSystemID  ) )
            PASSTHROUGH
         CASE SQL_PCODE_TABLE_PARAM
            SKIPFWD
            if lParseTableName
               aRet := eval( bTableInfo, if(uData+1<=len(aParam),if(valtype(aParam[uData+1])=="B",eval(aParam[uData+1]),aParam[uData+1]),"##PARAM_"+strzero(uData+1,3)+"_NOT_SUPPLIED##"), nSystemId )
               If nContext != SQL_CONTEXT_SELECT_FROM
                  cSql += aRet[TABLE_INFO_QUALIFIED_NAME]
               Else
                  aadd( aTables, aRet[TABLE_INFO_TABLE_NAME] )
                  aadd( aQualifiedTables, aRet[TABLE_INFO_QUALIFIED_NAME] )
               EndIf
               If nContext == SQL_CONTEXT_UPDATE
                  SR_SolveFilters(aFilters,aRet,,nSystemID)
                  cSql +=  NEWLINE + " SET" + NEWLINE + "  "
               EndIf
               If nContext == SQL_CONTEXT_DELETE
                  SR_SolveFilters(aFilters,aRet,,nSystemID)
               EndIf
               If nContext == SQL_CONTEXT_INSERT
                  cSql +=  NEWLINE + IDENTSPACE + "  "
               EndIf
            Else
               uData := if(uData+1<=len(aParam),if(valtype(aParam[uData+1])=="B",eval(aParam[uData+1]),aParam[uData+1]),"##PARAM_"+strzero(uData+1,3)+"_NOT_SUPPLIED##")
               If nContext != SQL_CONTEXT_SELECT_FROM
                  cSql += uData
               Else
                  aadd( aTables, uData )
                  aadd( aQualifiedTables, uData )
               EndIf
               If nContext == SQL_CONTEXT_UPDATE
                  cSql +=  NEWLINE + " SET" + NEWLINE + "  "
               EndIf
               If nContext == SQL_CONTEXT_INSERT
                  cSql +=  NEWLINE + IDENTSPACE + "  "
               EndIf
            EndIf
            PASSTHROUGH
         CASE SQL_PCODE_TABLE_BINDVAR
            SKIPFWD
            if lParseTableName
               aRet := eval( bTableInfo, &uData, nSystemId )
               aadd( aTables, aRet[TABLE_INFO_TABLE_NAME] )
               aadd( aQualifiedTables, aRet[TABLE_INFO_QUALIFIED_NAME] )
               If nContext == SQL_CONTEXT_UPDATE
                  SR_SolveFilters(aFilters,aRet,,nSystemID)
                  cSql +=  NEWLINE + " SET" + NEWLINE + "  "
               EndIf
               If nContext == SQL_CONTEXT_DELETE
                  SR_SolveFilters(aFilters,aRet,,nSystemID)
               EndIf
               If nContext == SQL_CONTEXT_INSERT
                  cSql +=  NEWLINE + IDENTSPACE + "  "
               EndIf
            Else
               uData := &uData
               aadd( aTables, uData )
               aadd( aQualifiedTables, uData )
               If nContext == SQL_CONTEXT_UPDATE
                  cSql +=  NEWLINE + " SET" + NEWLINE + "  "
               EndIf
               If nContext == SQL_CONTEXT_INSERT
                  cSql +=  NEWLINE + IDENTSPACE + "  "
               EndIf
            EndIf
            PASSTHROUGH
         CASE SQL_PCODE_COLUMN_LIST_SEPARATOR
            If nContext != SQL_CONTEXT_SELECT_FROM
               cSql += "," + NEWLINE + IDENTSPACE + "  "
            EndIf
            PASSTHROUGH
         CASE SQL_PCODE_START_EXPR
            If nContext != SQL_CONTEXT_SELECT_FROM
               FIX_PRE_WHERE
               cSql += "("
               RECURSIVE_CALL
            Else
               SKIPFWD
               uData := "(" + SR_SQLCodeGen2(apCode,aParam,nSystemId,lIdent,@nIP,nContext,@nSpaces,lParseTableName)
               aadd( aTables, uData )
               aadd( aQualifiedTables, uData )
               Exit
            EndIf
         CASE SQL_PCODE_STOP_EXPR
            cSql += ")"
            nDepht--
            PASSTHROUGH
         CASE SQL_PCODE_NOT_EXPR
            FIX_PRE_WHERE
            cSql += " NOT "
            PASSTHROUGH
         CASE SQL_PCODE_FUNC_DATE
            SWITCH nSystemId
            CASE SYSTEMID_MSSQL7
               cSql += "getdate() "
               Exit
            CASE SYSTEMID_ORACLE
               cSql += "SYSDATE "
               Exit
            CASE SYSTEMID_IBMDB2
            CASE SYSTEMID_FIREBR
            CASE SYSTEMID_FIREBR3
            CASE SYSTEMID_POSTGR
               cSql += "CURRENT_DATE "
               Exit
            CASE SYSTEMID_MYSQL
            Case SYSTEMID_MARIADB
               cSql += "CURDATE() "
               Exit
            DEFAULT
               cSql += "CURRENT_DATE "
            END
            PASSTHROUGH
         CASE SQL_PCODE_FUNC_COUNT
            cSql += "COUNT("
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_COUNT_AST
            cSql += "COUNT(*"
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_ABS
            FIX_PRE_WHERE
            cSql += "ABS("
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_AVG
            FIX_PRE_WHERE
            SWITCH nSystemId
            CASE SYSTEMID_ORACLE
            CASE SYSTEMID_MSSQL7
               cSql += "AVG("
               Exit
            DEFAULT
               cSql += "AVERAGE("
            END
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_ISNULL
            FIX_PRE_WHERE
            SWITCH nSystemId
            CASE SYSTEMID_MSSQL7
               cSql += "ISNULL("
               Exit
            CASE SYSTEMID_ORACLE
               cSql += "NVL("
               Exit
            CASE SYSTEMID_IBMDB2
               cSql += "VALUE("
               Exit
            CASE SYSTEMID_POSTGR
            CASE SYSTEMID_MYSQL
            Case SYSTEMID_MARIADB
               cSql += "COALESCE("
               Exit
            DEFAULT
               cSql += "ISNULL("
            END
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_MAX
            FIX_PRE_WHERE
            cSql += "MAX("
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_MIN
            FIX_PRE_WHERE
            cSql += "MIN("
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_POWER
            FIX_PRE_WHERE
            cSql += "POWER("
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_ROUND
            FIX_PRE_WHERE
            cSql += "ROUND("
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_SUBSTR
            FIX_PRE_WHERE
            SWITCH nSystemId
            CASE SYSTEMID_MSSQL7
            CASE SYSTEMID_SYBASE
               cSql += "SUBSTRING("
               Exit
            DEFAULT
               cSql += "SUBSTR("
            END
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_SUBSTR2
            FIX_PRE_WHERE
            SWITCH nSystemId
            CASE SYSTEMID_MSSQL7
            CASE SYSTEMID_SYBASE
               cSql += "SUBSTRING("
               Exit
            DEFAULT
               cSql += "SUBSTR("
            END
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_SUM
            FIX_PRE_WHERE
            cSql += "SUM("
            RECURSIVE_CALL
         CASE SQL_PCODE_FUNC_TRIM
            FIX_PRE_WHERE
            SWITCH nSystemId
            CASE SYSTEMID_MSSQL7
               cSql += "dbo.trim("
               Exit
            DEFAULT
               cSql += "TRIM("
            END
            RECURSIVE_CALL
         CASE SQL_PCODE_SELECT_ITEM_ASTERISK
            cSql += "*"
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_ITEM_ALIAS_ASTER
            SKIPFWD
            cSql += SR_DBQUALIFY( uData, nSystemID  ) + ".*"
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_ALL
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_DISTINCT
            cSql += "DISTINCT "
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_NO_LIMIT
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_LIMIT
            SKIPFWD
            SWITCH nSystemId
            Case SYSTEMID_MSSQL7
            Case SYSTEMID_CACHE
               cSql += "TOP " + ltrim(str(uData)) + " "
               Exit
            Case SYSTEMID_FIREBR
            Case SYSTEMID_FIREBR3
            Case SYSTEMID_INFORM
               cSql += "FIRST " + ltrim(str(uData)) + " "
               Exit
            Case SYSTEMID_MYSQL
            Case SYSTEMID_MARIADB
            Case SYSTEMID_POSTGR
               cTrailler := " LIMIT " + ltrim(str(uData)) + " "
               Exit
            Case SYSTEMID_IBMDB2
               cTrailler := " fetch first " + ltrim(str(uData)) + " rows only"
               Exit
            END
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_ORDER_ASC
            SKIPFWD
            If uData == SQL_PCODE_COLUMN_ALIAS
               SKIPFWD
               cSql += SR_DBQUALIFY( uData, nSystemID  ) + "."
               SKIPFWD
            EndIf
            Switch uData
            Case SQL_PCODE_COLUMN_NAME_BINDVAR
               SKIPFWD
               cSql += SR_DBQUALIFY( &uData, nSystemID  ) + " ASC"
               If nSystemId == SYSTEMID_ORACLE
                  cSql += " NULLS FIRST"
               EndIf
               Exit
            Case SQL_PCODE_COLUMN_NAME_PARAM
               SKIPFWD
               GETPARAM_VAL_2
               cSql += SR_DBQUALIFY( uData, nSystemID  ) + " ASC"
               If nSystemId == SYSTEMID_ORACLE
                  cSql += " NULLS FIRST"
               EndIf
               Exit
            Default
               SKIPFWD
               cSql += SR_DBQUALIFY( uData, nSystemID  ) + " ASC"
               If nSystemId == SYSTEMID_ORACLE
                  cSql += " NULLS FIRST"
               EndIf
            End
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_ORDER_DESC
            SKIPFWD
            If uData == SQL_PCODE_COLUMN_ALIAS
               SKIPFWD
               cSql += SR_DBQUALIFY( uData, nSystemID  ) + "."
               SKIPFWD
            EndIf
            Switch uData
            Case SQL_PCODE_COLUMN_NAME_BINDVAR
               SKIPFWD
               cSql += SR_DBQUALIFY( &uData, nSystemID  ) + " DESC"
               If nSystemId == SYSTEMID_ORACLE
                  cSql += " NULLS FIRST"
               EndIf
               Exit
            Case SQL_PCODE_COLUMN_NAME_PARAM
               SKIPFWD
               GETPARAM_VAL_2
               cSql += SR_DBQUALIFY( uData, nSystemID  ) + " DESC"
               If nSystemId == SYSTEMID_ORACLE
                  cSql += " NULLS FIRST"
               EndIf
               Exit
            Default
               SKIPFWD
               cSql += SR_DBQUALIFY( uData, nSystemID  ) + " DESC"
               If nSystemId == SYSTEMID_ORACLE
                  cSql += " NULLS FIRST"
               EndIf
            End
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_ORDER
            cSql += NEWLINE + IDENTSPACE + " ORDER BY "
            nContext := SQL_CONTEXT_SELECT_ORDER
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_NO_ORDER
            nContext := SQL_CONTEXT_SELECT_ORDER
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_NO_GROUPBY
            nContext := SQL_CONTEXT_SELECT_GROUP
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_GROUPBY
            cSql += NEWLINE + IDENTSPACE + " GROUP BY "
            nContext := SQL_CONTEXT_SELECT_GROUP
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_FROM
            cSqlCols := cSql
            cSql     := ""
            nContext := SQL_CONTEXT_SELECT_FROM
            PASSTHROUGH
         CASE SQL_PCODE_SELECT_UNION

            /* FROM and JOIN will be included now */

            If !Empty( cSqlCols )

               cTmp := cSql
               cSql := cSqlCols
               cSql += NEWLINE + IDENTSPACE + "FROM" + NEWLINE + IDENTSPACE + "  "
               cAtual  := "$"
               cAtual2 := "$"

               aSort( aOuters,,, { |x,y| x[1] > y[1] .and. x[2] > y[2] } )

               For each outer in aOuters
                  If outer[1] != cAtual .and. hb_enumIndex() > 1
                     cSql += ", "
                     cSql += CRLF
                  ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
                     cSql += " AND "
                  ElseIf  hb_enumIndex() > 1
                     cSql += CRLF
                  EndIf

                  If outer[1] != cAtual
                     nPos := aScan( aAlias, SR_DBQUALIFY( outer[1], nSystemID  ) )
                     If nPos == 0
                        nPos := aScan( aTables, outer[1] )
                     EndIf
                     cSql += aQualifiedTables[nPos] + " " + aAlias[nPos] + NEWLINE + IDENTSPACE
                     nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID  ) )
                     If nPos == 0
                        nPos := aScan( aTables, outer[2] )
                     EndIf
                     cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos] + " " + aAlias[nPos] + " ON " + outer[3]
                  ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
                     cSql += outer[3]
                  Else
                     nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID  ) )
                     If nPos == 0
                        nPos := aScan( aTables, outer[2] )
                     EndIf
                     cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos]  + " " + aAlias[nPos] + " ON " + outer[3]
                  EndIf
                  cAtual  := outer[1]
                  cAtual2 := outer[2]
               Next

               For each outer in aOuters
                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID  ) )

                  If nPos == 0
                     nPos := aScan( aTables, outer[2] )
                  EndIf

                  If nPos > 0
                     aDel( aTables, nPos )
                     aDel( aQualifiedTables, nPos )
                     aSize( aTables, len(aTables)-1 )
                     aSize( aQualifiedTables, len(aQualifiedTables)-1 )
                     aDel( aAlias, nPos )
                     aSize( aAlias, len(aAlias)-1 )
                  EndIf

                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[1], nSystemID  ) )

                  If nPos == 0
                     nPos := aScan( aTables, outer[1] )
                  EndIf

                  If nPos > 0
                     aDel( aTables, nPos )
                     aSize( aTables, len(aTables)-1 )
                     aDel( aQualifiedTables, nPos )
                     aSize( aQualifiedTables, len(aQualifiedTables)-1 )
                     aDel( aAlias, nPos )
                     aSize( aAlias, len(aAlias)-1 )
                  EndIf

               Next

               If len( aTables ) > 0 .and. len( aOuters ) > 0
                  cSql += ", " + CRLF
               EndIf

               For each cTbl in aQualifiedTables
                  cSql += cTbl + " " + aAlias[hb_enumIndex()] + " " + if(left(cTbl,1)!= "(", TABLE_OPTIMIZER, "") + if( hb_enumIndex() < len( aTables ), "," + NEWLINE + IDENTSPACE + "  ", "" )
               Next

               cSql += cTmp + cTrailler
               cTrailler := ""

            EndIf

            cSql += NEWLINE + IDENTSPACE + " UNION" + NEWLINE + IDENTSPACE + "  "

            aLJoins   := {}             /* A, B, Expression */
            aTables   := {}             /* TableName */
            aQualifiedTables := {}
            aAlias    := {}
            aOuters   := {}
            cSqlCols  := ""

            nContext := SQL_CONTEXT_SELECT_UNION
            PASSTHROUGH

         CASE SQL_PCODE_SELECT_UNION_ALL

            /* FROM and JOIN will be included now */

            If !Empty( cSqlCols )

               If nSystemId == SYSTEMID_ORACLE

                  cTmp := cSql
                  cSql := cSqlCols
                  cSql += NEWLINE + IDENTSPACE + "FROM" + NEWLINE + IDENTSPACE + "  "
                  cAtual  := "$"
                  cAtual2 := "$"
      //            aSort( aOuters,,, { |x,y| x[1] > y[1] .and. x[2] > y[2] } )

                  For each outer in aOuters
      //               If outer[1] != cAtual .and. hb_enumIndex() > 1
      //                  cSql += ", "
      //                  cSql += CRLF
      //               ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
                     If outer[1] = cAtual .and. outer[2] = cAtual2
                        cSql += " AND "
                     ElseIf  hb_enumIndex() > 1
                        cSql += CRLF
                     EndIf

                     If hb_enumIndex() = 1
      //               If outer[1] != cAtual
                        nPos := aScan( aAlias, SR_DBQUALIFY( outer[1], nSystemID ) )
                        If nPos == 0
                           nPos := aScan( aTables, outer[1] )
                        EndIf
                        cSql += aQualifiedTables[nPos] + " " + aAlias[nPos] + NEWLINE + IDENTSPACE
                        nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )
                        If nPos == 0
                           nPos := aScan( aTables, outer[2] )
                        EndIf
                        cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos] + " " + aAlias[nPos] + " ON " + outer[3]
                     ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
                        cSql += outer[3]
                     Else
                        nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )
                        If nPos == 0
                           nPos := aScan( aTables, outer[2] )
                        EndIf
                        cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos]  + " " + aAlias[nPos] + " ON " + outer[3]
                     EndIf
                     cAtual  := outer[1]
                     cAtual2 := outer[2]
                  Next

               Else

                  cTmp := cSql
                  cSql := cSqlCols
                  cSql += NEWLINE + IDENTSPACE + "FROM" + NEWLINE + IDENTSPACE + "  "
                  cAtual  := "$"
                  cAtual2 := "$"

                  aSort( aOuters,,, { |x,y| x[1] > y[1] .and. x[2] > y[2] } )

                  For each outer in aOuters
                     If outer[1] != cAtual .and. hb_enumIndex() > 1
                        cSql += ", "
                        cSql += CRLF
                     ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
                        cSql += " AND "
                     ElseIf  hb_enumIndex() > 1
                        cSql += CRLF
                     EndIf

                     If outer[1] != cAtual
                        nPos := aScan( aAlias, SR_DBQUALIFY( outer[1], nSystemID ) )
                        If nPos == 0
                           nPos := aScan( aTables, outer[1] )
                        EndIf
                        cSql += aQualifiedTables[nPos] + " " + aAlias[nPos] + NEWLINE + IDENTSPACE
                        nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )
                        If nPos == 0
                           nPos := aScan( aTables, outer[2] )
                        EndIf
                        cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos] + " " + aAlias[nPos] + " ON " + outer[3]
                     ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
                        cSql += outer[3]
                     Else
                        nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )
                        If nPos == 0
                           nPos := aScan( aTables, outer[2] )
                        EndIf
                        cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos]  + " " + aAlias[nPos] + " ON " + outer[3]
                     EndIf
                     cAtual  := outer[1]
                     cAtual2 := outer[2]
                  Next

               EndIf

               For each outer in aOuters
                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )

                  If nPos == 0
                     nPos := aScan( aTables, outer[2] )
                  EndIf

                  If nPos > 0
                     aDel( aTables, nPos )
                     aDel( aQualifiedTables, nPos )
                     aSize( aTables, len(aTables)-1 )
                     aSize( aQualifiedTables, len(aQualifiedTables)-1 )
                     aDel( aAlias, nPos )
                     aSize( aAlias, len(aAlias)-1 )
                  EndIf

                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[1], nSystemID ) )

                  If nPos == 0
                     nPos := aScan( aTables, outer[1] )
                  EndIf

                  If nPos > 0
                     aDel( aTables, nPos )
                     aSize( aTables, len(aTables)-1 )
                     aDel( aQualifiedTables, nPos )
                     aSize( aQualifiedTables, len(aQualifiedTables)-1 )
                     aDel( aAlias, nPos )
                     aSize( aAlias, len(aAlias)-1 )
                  EndIf

               Next

               If len( aTables ) > 0 .and. len( aOuters ) > 0
                  cSql += ", " + CRLF
               EndIf

               For each cTbl in aQualifiedTables
                  cSql += cTbl + " " + aAlias[hb_enumIndex()] + " " + if(left(cTbl,1)!= "(", TABLE_OPTIMIZER, "") + if( hb_enumIndex() < len( aTables ), "," + NEWLINE + IDENTSPACE + "  ", "" )
               Next

               cSql += cTmp + cTrailler
               cTrailler := ""

            EndIf

            aLJoins   := {}             /* A, B, Expression */
            aTables   := {}             /* TableName */
            aQualifiedTables := {}
            aAlias    := {}
            aOuters   := {}
            cSqlCols  := ""

            cSql += NEWLINE + IDENTSPACE + " UNION ALL" + NEWLINE + IDENTSPACE + "  "
            nContext := SQL_CONTEXT_SELECT_UNION
            PASSTHROUGH
         CASE SQL_PCODE_INSERT_NO_LIST
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_BASE
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_IN
            cSql += " IN "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_NOT_IN
            cSql += " NOT IN "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_IS_NULL
            cSql += " IS NULL "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_IS_NOT_NULL
            cSql += " IS NOT NULL "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_AND
            if nIP + 8 <= len(apCode) .and. apCode[nIP+1] == SQL_PCODE_OPERATOR_BASE .and.;
                                            apCode[nIP+2] == SQL_PCODE_COLUMN_ALIAS .and.;
                                            apCode[nIP+4] == SQL_PCODE_COLUMN_NAME .and. ;
                                            SR_IsComparOp( apCode[nIP+6] ) .and. ;
                                            if(!SR_IsComparNullOp( apCode[nIP+6] ), apCode[nIP+7] == SQL_PCODE_COLUMN_BY_VALUE, .T.) .and.;
                                            (nFlt := aScan( aOuters, {|x| upper(x[2]) == upper(apCode[nIP+3])} )) > 0

               aOuters[nFlt,3] += " AND " + SR_DBQUALIFY( apCode[nIP+3], nSystemID ) + "." + SR_DBQUALIFY( apCode[nIP+5], nSystemID ) + SR_ComparOpText(apCode[nIP+6]) + if(!SR_IsComparNullOp( apCode[nIP+6] ), SR_SQLQuotedString( apCode[nIP+8], nSystemID ), "" )
               nIP += if( SR_IsComparNullOp( apCode[nIP+6] ), 7, 9 )
               Exit
            ElseIf nIP + 1 <= len(apCode) .and. apCode[nIP+1] == SQL_PCODE_OPERATOR_LEFT_OUTER_JOIN
               PASSTHROUGH
            Else
               If nContext == SQL_CONTEXT_SELECT_PRE_WHERE .or. nContext == SQL_CONTEXT_SELECT_PRE_WHERE2
                  FIX_PRE_WHERE
               Else
                  If nContext == SQL_CONTEXT_SELECT_WHERE
                     nContext := SQL_CONTEXT_SELECT_PRE_WHERE2
                  Else
                     cSql += " AND "
                  EndIf
               EndIf
               PASSTHROUGH
            EndIf
         CASE SQL_PCODE_OPERATOR_OR
            If nContext == SQL_CONTEXT_SELECT_PRE_WHERE .or. nContext == SQL_CONTEXT_SELECT_PRE_WHERE2
               BREAK SQL_SINTAX_ERROR_OUTER_JOIN_OR
            Else
               cSql += " OR "
            EndIf
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_EQ
            cSql += " = "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_NE
            cSql += " != "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_GT
            cSql += " > "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_GE
            cSql += " >= "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_LT
            cSql += " < "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_LE
            cSql += " <= "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_LIKE
            cSql += " LIKE "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_NOT_LIKE
            cSql += " NOT LIKE "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_PLUS
            cSql += " + "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_MINUS
            cSql += " - "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_MULT
            cSql += " * "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_DIV
            cSql += " / "
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_CONCAT
            SWITCH nSystemId
            CASE SYSTEMID_MSSQL7
               cSql += " + "
               Exit
            CASE SYSTEMID_ORACLE
            CASE SYSTEMID_POSTGR
            CASE SYSTEMID_MYSQL
            Case SYSTEMID_MARIADB
               cSql += " || "
               Exit
            END
            PASSTHROUGH
         CASE SQL_PCODE_OPERATOR_JOIN
            SWITCH nSystemId
            CASE SYSTEMID_MSSQL7
               cSql += " = "
               Exit
            CASE SYSTEMID_ORACLE
               cSql += " = "
               Exit
            END
            PASSTHROUGH

         CASE SQL_PCODE_OPERATOR_LEFT_OUTER_JOIN
            SKIPFWD
            If uData == SQL_PCODE_COLUMN_ALIAS
               SKIPFWD
               aadd( aOuters, { uData,,,1 } )
               SKIPFWD
            Else
               BREAK SQL_SINTAX_ERROR_OUTER_JOIN
            EndIf

            If uData == SQL_PCODE_COLUMN_NAME
               SKIPFWD
               aOuters[-1,3] := SR_DBQUALIFY( aOuters[-1,1], nSystemID ) + "." + SR_DBQUALIFY( uData, nSystemID )
               SKIPFWD
            Else
               BREAK SQL_SINTAX_ERROR_OUTER_JOIN
            EndIf

            aOuters[-1,3] += " = "

            If uData == SQL_PCODE_COLUMN_ALIAS
               SKIPFWD
               aOuters[-1,2] := uData
               SKIPFWD
            Else
               BREAK SQL_SINTAX_ERROR_OUTER_JOIN
            EndIf

            If uData == SQL_PCODE_COLUMN_NAME
               SKIPFWD
               aOuters[-1,3] += SR_DBQUALIFY( aOuters[-1,2], nSystemID ) + "." + SR_DBQUALIFY( uData, nSystemID )
               SKIPFWD
            Else
               BREAK SQL_SINTAX_ERROR_OUTER_JOIN
            EndIf

            Exit

         CASE SQL_PCODE_OPERATOR_RIGHT_OUTER_JOIN
            SKIPFWD
            If uData == SQL_PCODE_COLUMN_ALIAS
               SKIPFWD
               aadd( aOuters, { uData,,,2 } )
               SKIPFWD
            Else
               BREAK SQL_SINTAX_ERROR_OUTER_JOIN
            EndIf

            If uData == SQL_PCODE_COLUMN_NAME
               SKIPFWD
               aOuters[-1,3] := SR_DBQUALIFY( aOuters[-1,1], nSystemID ) + "." + SR_DBQUALIFY( uData, nSystemID )
               SKIPFWD
            Else
               BREAK SQL_SINTAX_ERROR_OUTER_JOIN
            EndIf

            aOuters[-1,3] += " = "

            If uData == SQL_PCODE_COLUMN_ALIAS
               SKIPFWD
               aOuters[-1,2] := uData
               SKIPFWD
            Else
               BREAK SQL_SINTAX_ERROR_OUTER_JOIN
            EndIf

            If uData == SQL_PCODE_COLUMN_NAME
               SKIPFWD
               aOuters[-1,3] += SR_DBQUALIFY( aOuters[-1,2], nSystemID ) + "." + SR_DBQUALIFY( uData, nSystemID )
               SKIPFWD
            Else
               BREAK SQL_SINTAX_ERROR_OUTER_JOIN
            EndIf

            Exit

         DEFAULT
            nIP++
         END

      ENDDO

      cSql += SELECT_OPTIMIZER2

      /* FROM and JOIN will be included now */

      If !Empty( cSqlCols )

         If nSystemId == SYSTEMID_ORACLE

            cTmp := cSql
            cSql := cSqlCols
            cSql += NEWLINE + IDENTSPACE + "FROM" + NEWLINE + IDENTSPACE + "  "
            cAtual  := "$"
            cAtual2 := "$"
//            aSort( aOuters,,, { |x,y| x[1] > y[1] .and. x[2] > y[2] } )

            For each outer in aOuters
//               If outer[1] != cAtual .and. hb_enumIndex() > 1
//                  cSql += ", "
//                  cSql += CRLF
//               ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
               If outer[1] = cAtual .and. outer[2] = cAtual2
                  cSql += " AND "
               ElseIf  hb_enumIndex() > 1
                  cSql += CRLF
               EndIf

               If hb_enumIndex() = 1
//               If outer[1] != cAtual
                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[1], nSystemID ) )
                  If nPos == 0
                     nPos := aScan( aTables, outer[1] )
                  EndIf
                  cSql += aQualifiedTables[nPos] + " " + aAlias[nPos] + NEWLINE + IDENTSPACE
                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )
                  If nPos == 0
                     nPos := aScan( aTables, outer[2] )
                  EndIf
                  cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos] + " " + aAlias[nPos] + " ON " + outer[3]
               ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
                  cSql += outer[3]
               Else
                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )
                  If nPos == 0
                     nPos := aScan( aTables, outer[2] )
                  EndIf
                  cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos]  + " " + aAlias[nPos] + " ON " + outer[3]
               EndIf
               cAtual  := outer[1]
               cAtual2 := outer[2]
            Next

         Else

            cTmp := cSql
            cSql := cSqlCols
            cSql += NEWLINE + IDENTSPACE + "FROM" + NEWLINE + IDENTSPACE + "  "
            cAtual  := "$"
            cAtual2 := "$"

            aSort( aOuters,,, { |x,y| x[1] > y[1] .and. x[2] > y[2] } )

            For each outer in aOuters
               If outer[1] != cAtual .and. hb_enumIndex() > 1
                  cSql += ", "
                  cSql += CRLF
               ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
                  cSql += " AND "
               ElseIf  hb_enumIndex() > 1
                  cSql += CRLF
               EndIf

               If outer[1] != cAtual
                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[1], nSystemID ) )
                  If nPos == 0
                     nPos := aScan( aTables, outer[1] )
                  EndIf
                  cSql += aQualifiedTables[nPos] + " " + aAlias[nPos] + NEWLINE + IDENTSPACE
                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )
                  If nPos == 0
                     nPos := aScan( aTables, outer[2] )
                  EndIf
                  cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos] + " " + aAlias[nPos] + " ON " + outer[3]
               ElseIf outer[1] = cAtual .and. outer[2] = cAtual2
                  cSql += outer[3]
               Else
                  nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )
                  If nPos == 0
                     nPos := aScan( aTables, outer[2] )
                  EndIf
                  cSql += cJoinWords( outer[4], nSystemID ) + aQualifiedTables[nPos]  + " " + aAlias[nPos] + " ON " + outer[3]
               EndIf
               cAtual  := outer[1]
               cAtual2 := outer[2]
            Next

         EndIf

         For each outer in aOuters
            nPos := aScan( aAlias, SR_DBQUALIFY( outer[2], nSystemID ) )

            If nPos == 0
               nPos := aScan( aTables, outer[2] )
            EndIf

            If nPos > 0
               aDel( aTables, nPos )
               aSize( aTables, len(aTables)-1 )
               aDel( aQualifiedTables, nPos )
               aSize( aQualifiedTables, len(aQualifiedTables)-1 )
               aDel( aAlias, nPos )
               aSize( aAlias, len(aAlias)-1 )
            EndIf

            nPos := aScan( aAlias, SR_DBQUALIFY( outer[1], nSystemID ) )

            If nPos == 0
               nPos := aScan( aTables, outer[1] )
            EndIf

            If nPos > 0
               aDel( aTables, nPos )
               aSize( aTables, len(aTables)-1 )
               aDel( aQualifiedTables, nPos )
               aSize( aQualifiedTables, len(aQualifiedTables)-1 )
               aDel( aAlias, nPos )
               aSize( aAlias, len(aAlias)-1 )
            EndIf

         Next

         If len( aTables ) > 0 .and. len( aOuters ) > 0
            cSql += ", " + CRLF
         EndIf

         For each cTbl in aQualifiedTables
            cSql += cTbl + " " + aAlias[hb_enumIndex()] + " " + if(left(cTbl,1)!= "(", TABLE_OPTIMIZER, "") + if( hb_enumIndex() < len( aTables ), "," + NEWLINE + IDENTSPACE + "  ", "" )
         Next

         cSql += cTmp + cTrailler
         cTrailler := ""

      EndIf

   RECOVER USING nErrorId

      If valtype(nErrorId) == "O"
         Eval( bError, nErrorId )
      Else
         SR_SQLParseError( , , "", nErrorId,,bError )
      EndIf

   END SEQUENCE

   If lIdent
      nSpaces -= 2
   EndIf

   Return cSQL

/*
* Quoting xBase DataTypes
*/

Function SR_SQLQuotedString( uData, nSystemID, lNotNull )

   local cType := valtype(uData), uElement, cRet := ""

   Default( lNotNull, .F. )

   If (!lNotNull) .and. empty( uData )
      Return "NULL"
   Endif

   If lNotNull .and. empty( uData ) .and. cType $ "CM"
      Return ['] + " " + [']
   Endif

   Do Case
   Case cType $ "CM" .and. nSystemID == SYSTEMID_POSTGR
      return [E'] + rtrim(SR_ESCAPESTRING(uData, nSystemID)) + [']
   Case cType $ "CM"
      return ['] + rtrim(SR_ESCAPESTRING(uData, nSystemID)) + [']
   Case cType == "D" .and. nSystemID == SYSTEMID_ORACLE
      return ([TO_DATE('] + rtrim(DtoS(uData)) + [','YYYYMMDD')])
   Case cType == "D" .and. (nSystemID == SYSTEMID_IBMDB2 .or. nSystemID == SYSTEMID_ADABAS )
      return ([']+transform(DtoS(uData) ,'@R 9999-99-99')+['])
   Case cType == "D" .and. nSystemID == SYSTEMID_SQLBAS
      return (['] + SR_dtosdot(uData) + ['])
   Case cType == "D" .and. nSystemID == SYSTEMID_INFORM
      return (['] + SR_dtoUS(uData) + ['])
   Case cType == "D" .and. nSystemID == SYSTEMID_INGRES
      return (['] + SR_dtoDot(uData) + ['])
   Case cType == "D" .and. (nSystemID == SYSTEMID_FIREBR .or. nSystemID == SYSTEMID_FIREBR3)
      return [']+transform(DtoS(uData) ,'@R 9999/99/99')+[']
   Case cType == "D" .and. nSystemID == SYSTEMID_CACHE
      return [{d ']+transform(DtoS(if(year(uData)<1850,stod("18500101"),uData)) ,'@R 9999-99-99')+['}]
   Case cType == "D" .and. ( nSystemID == SYSTEMID_MYSQL .or. nSystemID == SYSTEMID_MARIADB )
      return ([str_to_date( '] + dtos(uData) + [', '%Y%m%d' )])
   Case cType == "D"
      return (['] + dtos(uData) + ['])
   Case cType == "N"
      return ltrim(str(uData))
   Case cType == "L" .and. ( nSystemID == SYSTEMID_POSTGR .or. nSystemID == SYSTEMID_FIREBR3)
      return if(uData,"true","false")
   Case cType == "L" .and. nSystemID == SYSTEMID_INFORM
      return if(uData,"'t'","'f'")
   Case cType == "L"
      return if(uData,"1","0")
   Case cType == "A"
      For each uElement in uData
         cRet += if(empty(cRet),"",", ") + SR_SQLQuotedString( uElement, nSystemID, lNotNull )
      Next
      return cRet
   Case cType == "O"
      cRet := SR_STRTOHEX(HB_Serialize( uData ))
      Return SR_SQLQuotedString( SQL_SERIALIZED_SIGNATURE + str(len(cRet),10) + cRet, nSystemID, lNotNull )
   EndCase

Return "NULL"

/*
*   SQLBASE date format
*/

Function SR_dtosdot( dData )

   Local cData := dtos( dData )

Return SubStr( cData,1,4) + "-" + subStr( cData,5,2 ) + "-" + subStr( cData,7,2 )

/*
*  YYYY.MM.DD
*/

Function SR_dtoDot( dData )

   Local cData := dtos( dData )

Return SubStr( cData,1,4) + "." + subStr( cData,5,2 ) + "." + subStr( cData,7,2 )

/*
*  MMDDYYYY
*/

Function SR_dtous( dData )

   Local cData := dtos( dData )

Return subStr( cData,5,2 ) + subStr( cData,7,2 ) + SubStr( cData,1,4)

/*
* Error Handler
*/

Static Function SR_SQLParseError( arg1, arg2, cDescr, nCode, cOper, oError )

   local uRet
   local oErr := ErrorNew()

   If arg1 != NIL .and. arg2 != NIL
      oErr:Args          := { arg1, arg2 }
   EndIf

   Default( nCode, 0 )
   Default( cDescr, "SQLPARSER Error" )

   oErr:GenCode       := 20000 + nCode
   oErr:CanDefault    := .F.
   oErr:Severity      := ES_ERROR
   oErr:CanRetry      := .T.
   oErr:CanSubstitute := .F.
   oErr:Description   := cDescr
   oErr:Filename      := ""
   oErr:SubSystem     := "SQLCodGen"
   oErr:Operation     := cOper
   oErr:OsCode          := 0

   uRet := Eval( oError, oErr )

Return uRet

/*
* SQL Filters
*
*  Expected filter format example:  "<ALIAS>.ColumnName IS NULL"
*
*/

Static Function SR_SolveFilters(aFilters,aRet,cAlias,nSystemID)

   Local i

   If !(valtype(aRet) == "A" .and. len(aRet) >= 2 .and. valtype(aRet[1]) == "C")
      Return .F.
   EndIf

   Default( nSystemID, SR_GetConnection():nSystemID )
   Default( cAlias, SR_DBQUALIFY( aRet[TABLE_INFO_TABLE_NAME], nSystemID  ) )

   For i = 1 to len( aRet[TABLE_INFO_FILTERS] )
      If SR_EvalFilters()
         aadd( aFilters, &( StrTran(aRet[TABLE_INFO_FILTERS,i], "<ALIAS>", cAlias ) ) )
      Else
         aadd( aFilters, StrTran(aRet[TABLE_INFO_FILTERS,i], "<ALIAS>", cAlias ) )
      EndIf
   Next

Return .T.

/*
* Startup settings
*/

Procedure __SR_StartSQL()
   bTableInfo  := { |cTableName,nSystemID| SR_TableAttr( cTableName, nSystemID ) }
   bIndexInfo  := { |cIndexName,nSystemID| SR_IndexAttr( cIndexName, nSystemID ) }
   bNextRecord := { || ++ nRecordNum }
   aJoinWords  := Array( SUPPORTED_DATABASES )
   aFill( aJoinWords, { " LEFT OUTER JOIN ", " RIGHT OUTER JOIN ", " LEFT JOIN ", " RIGHT JOIN ", " JOIN " } )

Return

/*------------------------------------------------------------------------*/

Function SR_SetTableInfoBlock( b )

   If valtype( b ) != "B"
      Return .F.
   ENdIf

   bTableInfo := b

Return .T.

/*------------------------------------------------------------------------*/

Function SR_SetIndexInfoBlock( b )

   If valtype( b ) != "B"
      Return .F.
   ENdIf

   bIndexInfo := b

Return .T.

/*------------------------------------------------------------------------*/

Function SR_GetTableInfoBlock()

Return bTableInfo

/*------------------------------------------------------------------------*/

Function SR_GetIndexInfoBlock()

Return bIndexInfo

/*------------------------------------------------------------------------*/

Function SR_SetNextRecordBlock( b )

   If valtype( b ) != "B"
      Return .F.
   ENdIf

   bNextRecord := b

Return .T.

/*------------------------------------------------------------------------*/

Function SR_GetNextRecordBlock()

Return bNextRecord

/*
* Version Report
*/

Function SR_ParserVersion()

   local nVers := 1

Return nVers


/*
*
*   Debug Functions
*
*/

Function SR_pCodeDescr(nCode)

   local nFound

   Static apCode := {;
      { "SQL_PCODE_SELECT",                     0 },;
      { "SQL_PCODE_INSERT",                     1 },;
      { "SQL_PCODE_UPDATE",                     2 },;
      { "SQL_PCODE_DELETE",                     3 },;
      { "SQL_PCODE_COLUMN_NAME",                4 },;
      { "SQL_PCODE_COLUMN_BY_VALUE",            5 },;
      { "SQL_PCODE_COLUMN_PARAM",               6 },;
      { "SQL_PCODE_COLUMN_BINDVAR",             7 },;
      { "SQL_PCODE_COLUMN_ALIAS",               8 },;
      { "SQL_PCODE_COLUMN_NO_AS",               9 },;
      { "SQL_PCODE_COLUMN_AS",                  10 },;
      { "SQL_PCODE_COLUMN_NAME_BINDVAR",        11 },;
      { "SQL_PCODE_COLUMN_NAME_PARAM",          12 },;
      { "SQL_PCODE_COLUMN_PARAM_NOTNULL",       13 },;
      { "SQL_PCODE_LOCK",                       20 },;
      { "SQL_PCODE_NOLOCK",                     21 },;
      { "SQL_PCODE_NO_WHERE",                   22 },;
      { "SQL_PCODE_WHERE",                      23 },;
      { "SQL_PCODE_TABLE_NAME",                 25 },;
      { "SQL_PCODE_TABLE_NO_ALIAS",             26 },;
      { "SQL_PCODE_TABLE_ALIAS",                27 },;
      { "SQL_PCODE_TABLE_PARAM",                28 },;
      { "SQL_PCODE_TABLE_BINDVAR",              29 },;
      { "SQL_PCODE_COLUMN_LIST_SEPARATOR",      80 },;
      { "SQL_PCODE_START_EXPR",                 100 },;
      { "SQL_PCODE_STOP_EXPR",                  101 },;
      { "SQL_PCODE_NOT_EXPR",                   102 },;
      { "SQL_PCODE_FUNC_COUNT_AST",             200 },;
      { "SQL_PCODE_FUNC_COUNT",                 201 },;
      { "SQL_PCODE_FUNC_ABS",                   202 },;
      { "SQL_PCODE_FUNC_AVG",                   203 },;
      { "SQL_PCODE_FUNC_ISNULL",                204 },;
      { "SQL_PCODE_FUNC_MAX",                   205 },;
      { "SQL_PCODE_FUNC_MIN",                   206 },;
      { "SQL_PCODE_FUNC_POWER              ",   207 },;
      { "SQL_PCODE_FUNC_ROUND              ",   208 },;
      { "SQL_PCODE_FUNC_SUBSTR             ",   209 },;
      { "SQL_PCODE_FUNC_SUBSTR2            ",   210 },;
      { "SQL_PCODE_FUNC_SUM                ",   211 },;
      { "SQL_PCODE_FUNC_TRIM               ",   212 },;
      { "SQL_PCODE_FUNC_DATE               ",   213 },;
      { "SQL_PCODE_SELECT_ITEM_ASTERISK    ",   300 },;
      { "SQL_PCODE_SELECT_ITEM_ALIAS_ASTER ",   301 },;
      { "SQL_PCODE_SELECT_ALL              ",   302 },;
      { "SQL_PCODE_SELECT_DISTINCT         ",   303 },;
      { "SQL_PCODE_SELECT_NO_LIMIT         ",   304 },;
      { "SQL_PCODE_SELECT_LIMIT            ",   305 },;
      { "SQL_PCODE_SELECT_ORDER_ASC        ",   306 },;
      { "SQL_PCODE_SELECT_ORDER_DESC       ",   307 },;
      { "SQL_PCODE_SELECT_ORDER            ",   308 },;
      { "SQL_PCODE_SELECT_NO_ORDER         ",   309 },;
      { "SQL_PCODE_SELECT_NO_GROUPBY       ",   310 },;
      { "SQL_PCODE_SELECT_GROUPBY          ",   311 },;
      { "SQL_PCODE_SELECT_FROM             ",   312 },;
      { "SQL_PCODE_SELECT_UNION            ",   313 },;
      { "SQL_PCODE_SELECT_UNION_ALL        ",   314 },;
      { "SQL_PCODE_INSERT_NO_LIST          ",   400 },;
      { "SQL_PCODE_INSERT_VALUES           ",   401 },;
      { "SQL_PCODE_OPERATOR_BASE           ",   1000 },;
      { "SQL_PCODE_OPERATOR_IN             ",   1002 },;
      { "SQL_PCODE_OPERATOR_NOT_IN         ",   1003 },;
      { "SQL_PCODE_OPERATOR_IS_NULL        ",   1004 },;
      { "SQL_PCODE_OPERATOR_IS_NOT_NULL    ",   1005 },;
      { "SQL_PCODE_OPERATOR_AND            ",   1006 },;
      { "SQL_PCODE_OPERATOR_OR             ",   1007 },;
      { "SQL_PCODE_OPERATOR_EQ             ",   1008 },;
      { "SQL_PCODE_OPERATOR_NE             ",   1009 },;
      { "SQL_PCODE_OPERATOR_GT             ",   1010 },;
      { "SQL_PCODE_OPERATOR_GE             ",   1011 },;
      { "SQL_PCODE_OPERATOR_LT             ",   1012 },;
      { "SQL_PCODE_OPERATOR_LE             ",   1013 },;
      { "SQL_PCODE_OPERATOR_LIKE           ",   1014 },;
      { "SQL_PCODE_OPERATOR_NOT_LIKE       ",   1020 },;
      { "SQL_PCODE_OPERATOR_PLUS           ",   1015 },;
      { "SQL_PCODE_OPERATOR_MINUS          ",   1016 },;
      { "SQL_PCODE_OPERATOR_MULT           ",   1017 },;
      { "SQL_PCODE_OPERATOR_DIV            ",   1018 },;
      { "SQL_PCODE_OPERATOR_CONCAT         ",   1019 },;
      { "SQL_PCODE_OPERATOR_JOIN           ",   1100 },;
      { "SQL_PCODE_OPERATOR_LEFT_OUTER_JOIN",   1101 },;
      { "SQL_PCODE_OPERATOR_RIGHT_OUTER_JOIN",  1102 };
   }

   if valtype( nCode ) != "N"
      Return nCode
   endif

   nFound := aScan( apCode, {|x| x[2] == nCode } )

   If nFound > 0
      Return apCode[nFound,1]
   EndIf

Return nCode

/*------------------------------------------------------------------------*/

Function SR_TableAttr( cTableName, nSystemID )

   /* Translates "c:\data\accounts\chart.dbf" to "DATA_ACCONTS_CHART" */

   local aRet, cOwner := "",  cSlash

   if cTableName[2] == ":"
      /* Remove drive letter */
      cTableName := SubStr( cTableName, 3 )
   endif

   if "\" $ cTableName .or. "/" $ cTableName    // This may keep compatible with xHB 1.2
      if "/" $ cTableName
        cSlash := "/"
      Else
        cSlash := "\"
      EndIf
      if SubStr( cTableName, 2, rat( cSlash, cTableName )-2 ) == CurDir()
         cTableName := SubStr( cTableName, Rat( cSlash, cTableName ) + 1 )
      EndIf
   EndIf

   cTableName := strtran( alltrim(lower(cTableName)), ".dbf", "_dbf" )
   cTableName := strtran( cTableName, ".ntx", "" )
   cTableName := strtran( cTableName, ".cdx", "" )
   cTableName := strtran( cTableName, "\", "_" )
   if cTableName[1] == "/"
      cTableName := SubStr( cTableName, 2 )
   endif
   cTableName := strtran( cTableName, "/", "_" )
   cTableName := strtran( cTableName, ".", "_" )
   cTableName := alltrim( cTableName )

   if len( cTableName ) > 30
      cTableName := SubStr( cTableName, len( cTableName ) - 30 + 1 )
   endif

   cOwner := SR_SetGlobalOwner()
   If (!Empty(cOwner)) .and. cOwner[-1] != "."
      cOwner += "."
   EndIf

   aRet := { upper(cTableName),;
             {},;
             "",;
             TABLE_INFO_RELATION_TYPE_OUTER_JOIN,;
             SR_SetGlobalOwner(),;
             NIL,;
             "",;
             .T.,;
             .T.,;
             .T.,;
             .F.,;
             ,;
             ,;
             ,;
             cOwner + SR_DBQUALIFY(cTableName, nSystemID) }

Return aRet

/*------------------------------------------------------------------------*/

Function SR_IndexAttr( cTableName, nSystemID )

   /* Translates "c:\data\accounts\chart.dbf" to "DATA_ACCONTS_CHART" */

   local aRet, cSlash

   (nSystemID)

   if cTableName[2] == ":"
      /* Remove drive letter */
      cTableName := SubStr( cTableName, 3 )
   endif

   if "\" $ cTableName .or. "/" $ cTableName    // This may keep compatible with xHB 1.2
      if "/" $ cTableName
        cSlash := "/"
      Else
        cSlash := "\"
      EndIf
      if SubStr( cTableName, 2, rat( cSlash, cTableName )-2 ) == CurDir()
         cTableName := SubStr( cTableName, Rat( cSlash, cTableName ) + 1 )
      EndIf
   EndIf

   cTableName := strtran( alltrim(lower(cTableName)), ".dbf", "_dbf" )
   cTableName := strtran( cTableName, ".ntx", "" )
   cTableName := strtran( cTableName, ".cdx", "" )
   cTableName := strtran( cTableName, "\", "_" )
   if cTableName[1] == "/"
      cTableName := SubStr( cTableName, 2 )
   endif
   cTableName := strtran( cTableName, "/", "_" )
   cTableName := strtran( cTableName, ".", "_" )
   cTableName := alltrim( cTableName )

   if len( cTableName ) > 30
      cTableName := SubStr( cTableName, len( cTableName ) - 30 + 1 )
   endif

   aRet := { upper(cTableName), {}, "", TABLE_INFO_RELATION_TYPE_OUTER_JOIN,;
             SR_SetGlobalOwner(), .F., "", .T., .T., .T., .F.,, }

Return aRet

/*------------------------------------------------------------------------*/

Static Function SR_IsComparOp( nOp )

   SWITCH nOp
   CASE SQL_PCODE_OPERATOR_EQ
   CASE SQL_PCODE_OPERATOR_NE
   CASE SQL_PCODE_OPERATOR_GT
   CASE SQL_PCODE_OPERATOR_GE
   CASE SQL_PCODE_OPERATOR_LT
   CASE SQL_PCODE_OPERATOR_LE
   CASE SQL_PCODE_OPERATOR_LIKE
   CASE SQL_PCODE_OPERATOR_NOT_LIKE
   CASE SQL_PCODE_OPERATOR_IS_NULL
   CASE SQL_PCODE_OPERATOR_IS_NOT_NULL
      RETURN .T.
   END SWITCH

Return .F.

/*------------------------------------------------------------------------*/

Static Function SR_IsComparNullOp( nOp )

   SWITCH nOp
   CASE SQL_PCODE_OPERATOR_IS_NULL
   CASE SQL_PCODE_OPERATOR_IS_NOT_NULL
      RETURN .T.
   END SWITCH

Return .F.

/*------------------------------------------------------------------------*/

Static Function SR_ComparOpText( nOp )

   local cSql := ""

   Switch nOp
   CASE SQL_PCODE_OPERATOR_EQ
      cSql += " = "
      Exit
   CASE SQL_PCODE_OPERATOR_NE
      cSql += " != "
      Exit
   CASE SQL_PCODE_OPERATOR_GT
      cSql += " > "
      Exit
   CASE SQL_PCODE_OPERATOR_GE
      cSql += " >= "
      Exit
   CASE SQL_PCODE_OPERATOR_LT
      cSql += " < "
      Exit
   CASE SQL_PCODE_OPERATOR_LE
      cSql += " <= "
      Exit
   CASE SQL_PCODE_OPERATOR_LIKE
      cSql += " LIKE "
      Exit
   CASE SQL_PCODE_OPERATOR_NOT_LIKE
      cSql += " NOT LIKE "
      Exit
   CASE SQL_PCODE_OPERATOR_IS_NULL
      cSql += " IS NULL "
      Exit
   CASE SQL_PCODE_OPERATOR_IS_NOT_NULL
      cSql += " IS NOT NULL "
      Exit
   End

Return cSql

/*------------------------------------------------------------------------*/
