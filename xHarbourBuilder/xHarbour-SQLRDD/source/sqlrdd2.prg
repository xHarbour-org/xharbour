/* $CATEGORY$HIDE$FILES$HIDE$
* SQLRDD Support Classes
* WorkArea abstract class
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "hbclass.ch"
#include "sqlrdd.ch"
#include "compat.ch"
#include "sqlodbc.ch"
#include "error.ch"
#include "ord.ch"
#include "msg.ch"
#include "set.ch"
#include "dbinfo.ch"
#include "sqlrddsetup.ch"
#include "hbxml.ch" // Culik added to support arrays as xml
#define DUPL_IND_DETECT                .F.
#define SQLRDD_LEARNING_REPETITIONS     5



static aFather :={}
static nStartId :=0
static aPos :={}
static nPosData :=0
static lUseXmlField := .F.
STATIC lUseJSONField := .F.
#if 0
Static ItP11, ItP14, ItP2, ItP3
#endif
Static lGoTopOnFirstInteract := .T.
Static lUseDTHISTAuto        := .F.
Static nLineCountResult      := 0
Static cGlobalOwner          := ""
Static nOperat               := 0
Static cMySqlMemoDataType    := "MEDIUMBLOB"
Static cMySqlNumericDataType := "REAL"
Static lUseDBCatalogs        := .F.
Static lAllowRelationsInIndx := .F.
Static ____lOld
static nMininumVarchar2Size  := 31
Static lOracleSyntheticVirtual  := .T.

/*------------------------------------------------------------------------*/

CLASS SR_WORKAREA

   CLASSDATA nCnt
   CLASSDATA cWSID
   CLASSDATA aExclusive       AS ARRAY    INIT {}

   DATA aInfo         AS ARRAY INIT { .T., .T., .F., 0, 0, 0, .F., .F., 0, 0, .F.,.F., 0, 0, .T., 0, .F., 0, .F., 0, 0, 0, 0, 0 }  // See sqlrdd.ch, AINFO_*
   DATA aLocked       AS ARRAY INIT {}
   DATA aIndex        AS ARRAY INIT {}
   DATA aIndexMgmnt   AS ARRAY INIT {}
   DATA aConstrMgmnt  AS ARRAY INIT {}
   DATA aCache        AS ARRAY INIT Array( CAHCE_PAGE_SIZE * 3 )
   DATA aLocalBuffer  AS ARRAY INIT {}
   DATA aOldBuffer    AS ARRAY INIT {}
   DATA aEmptyBuffer  AS ARRAY INIT {}
   DATA aSelectList   AS ARRAY INIT {}

   DATA nThisArea     AS NUMERIC INIT 0
   DATA nFetchSize    AS NUMERIC INIT SR_FetchSize()

   DATA cOwner        AS CHARACTER INIT ""
   DATA cColPK        AS CHARACTER INIT ""
   DATA cFor          AS CHARACTER INIT ""
   DATA cScope        AS CHARACTER INIT ""
   DATA cOriginalFN   AS CHARACTER INIT ""
   DATA cRights       AS CHARACTER INIT ""
   DATA cRecnoName    AS CHARACTER INIT ""
   DATA cDeletedName  AS CHARACTER INIT ""

   DATA cQualifiedTableName  AS CHARACTER
   DATA lTableIsSelect       INIT .F.

   DATA Optmizer_1s
   DATA Optmizer_1e
   DATA Optmizer_ns
   DATA Optmizer_ne

   DATA nCurrentFetch   AS NUMERIC INIT SR_FetchSize()
   DATA nSkipCount      AS NUMERIC INIT 0
   DATA nLastRecordAded AS NUMERIC INIT -1
   DATA nLastRefresh    AS NUMERIC INIT 0
   DATA hnRecno         AS NUMERIC INIT 0
   DATA hnDeleted       AS NUMERIC INIT 0

   DATA cLastMove       AS CHARACTER INIT ""
   DATA cLastComm       AS CHARACTER INIT ""

   DATA lStable        AS LOGICAL INIT .T.
   DATA lOrderValid    AS LOGICAL INIT .F.
   DATA lTableLocked   AS LOGICAL INIT .F.
   DATA lHistoric      AS LOGICAL INIT .F.
   DATA lHistEnable    AS LOGICAL INIT .T.
   DATA lNoData        AS LOGICAL INIT .F.
   DATA lEmptyTable    AS LOGICAL INIT .F.
   DATA lVers          AS LOGICAL INIT .T.
   DATA lDisableFlts   AS LOGICAL INIT .F.
   DATA lSharedLock    AS LOGICAL INIT .F.
   DATA lOpened        AS LOGICAL INIT .T.
   DATA lCreating      AS LOGICAL INIT .F.
   DATA lQuickAppend   AS LOGICAL INIT .F.
   DATA lUseSequences  AS LOGICAL INIT .T.

   DATA lCollectingBehavior  AS LOGICAL INIT .T.
   DATA lAllColumnsSelected  AS LOGICAL INIT .F.

   DATA nTCCompat      AS NUMERIC INIT 0      // TopConnect compatibility mode

   DATA nSequencePerTable  AS NUMERIC INIT SEQ_NOTDEFINED

   DATA bScope    AS CODEBLOCK INIT {|| .T. }
   DATA bFilter   AS CODEBLOCK INIT {|| .T. }

   DATA oSql      AS OBJECT

   DATA cFileName, aFields, aIniFields, aNames, aNamesLower, nPosColPK, cAlias, aFilters
   DATA nFields, CurrDate, cFltUsr, cFilter, nLogMode
   DATA lCanSel, lCanUpd, lCanIns, lCanDel, nRelacType, lISAM, cCustomSQL
   DATA nLastRec, lGoTopOnFirstInteract
   DATA aLastOrdCond

   DATA lFetchAll AS LOGICAL INIT .F.
   DATA aFetch    AS ARRAY   INIT {}

   DATA cDel, cUpd, cIns
   DATA nPosDtHist
   DATA dNextDt                              /* Date value for next INSERT with Historic */

   DATA aPosition, aQuoted, aDat, nPartialDateSeek

   // For Self recno filter
   Data aRecnoFilter AS ARRAY INIT {}

   /* SQL Methods */

   METHOD ResetStatistics() INLINE (::nCurrentFetch := SR_FetchSize(), ::aInfo[ AINFO_SKIPCOUNT ] := 0, ::cLastMove := "OPEN" )
   METHOD GetNextRecordNumber()

   METHOD IniFields(lReSelect, lLoadCache, aInfo)
   METHOD Refresh()
   METHOD GetBuffer( lClean, nCache )
   METHOD SolveSQLFilters( cAliasSQL )
   METHOD SolveRestrictors()
   METHOD Default()
   METHOD UpdateCache( aResultSet )
   METHOD lCanICommitNow()
   METHOD WriteBuffer( lInsert, aBuffer )
   METHOD QuotedNull(uData, trim, nLen, nDec, nTargetDB, lNull, lMemo )
   METHOD Quoted(uData,trim, nLen, nDec, nTargetDB, lSynthetic )
   METHOD CheckCache( oWorkArea )
   METHOD WhereEqual()
   METHOD RuntimeErr( cOperation, cErr, nOSCode, nGenCode, SubCode )
   METHOD Normalize( nDirection )
   METHOD SkipRawCache( nToSkip )
   METHOD Stabilize()
   METHOD FirstFetch( nDirection )
   METHOD OrderBy( nOrder, lAscend )
   METHOD ReadPage( nDirection )
   METHOD WhereMajor()       // Retrieves an SQL/WHERE Major or equal the currente record
   METHOD WhereMinor()       // Retrieves an SQL/WHERE Minor or equal the currente record
   METHOD WhereVMajor()       // Retrieves an SQL/WHERE Major or equal the currente record (Synthetic Virtual Index)
   METHOD WhereVMinor()       // Retrieves an SQL/WHERE Minor or equal the currente record (Synthetic Virtual Index)
   METHOD WherePgsMajor( aQuotedCols )    // Retrieves an SQL/WHERE Major or equal the currente record
   METHOD WherePgsMinor( aQuotedCols )    // Retrieves an SQL/WHERE Minor or equal the currente record
   /* METHOD sqlKeyCompare( uKey )                       C level implemented - reads from ::aInfo */
   METHOD ParseIndexColInfo( cSQL )
   METHOD HasFilters()
   METHOD ParseForClause( cFor )
   METHOD OrdSetForClause( cFor, cForxBase )
   METHOD SetColPK( cColName )
   METHOD ConvType( cData, cType, lPartialSeek, nThis, lLike )

   METHOD LoadRegisteredTags()

   METHOD LockTable( lCheck )
   METHOD UnlockTable()

   METHOD FCount()       INLINE ::nFields
   METHOD SetNextDt( d ) INLINE ::dNextDt := d
   METHOD SetQuickAppend( l ) INLINE ( ____lOld := ::lQuickAppend, ::lQuickAppend := l, ____lOld )

   /* Table maintanance stuff */

   METHOD AlterColumns( aCreate, lDisplayErrorMessage, lBakcup )
   //This is an new method for direct alter column
   METHOD AlterColumnsDirect( aCreate, lDisplayErrorMessage, lBakcup,aRemove )
   METHOD DropColumn( cColumn, lDisplayErrorMessage )
   METHOD DropColRules( cColumn, lDisplayErrorMessage, aDeletedIndexes )
   METHOD AddRuleNotNull( cColumn )
   METHOD DropRuleNotNull( cColumn )

   METHOD DropConstraint( cTable, cConstraintName, lFKs, cConstrType )
   METHOD CreateConstraint( cSourceTable, aSourceColumns, cTargetTable, aTargetColumns, cConstraintName )

   /* Historic functionality specific methods */

   METHOD HistExpression(cAlias, cAlias)
   METHOD DisableHistoric()
   METHOD EnableHistoric()
   METHOD SetCurrDate(d)    INLINE if(d==NIL, ::CurrDate, ::CurrDate := d)

   METHOD LineCount()
   METHOD CreateOrclFunctions( cOwner, cFileName )

   METHOD sqlOpenAllIndexes()
   METHOD IncludeAllMethods()

   /* Workarea methods reflexion */

   /* METHOD sqlBof                       C level implemented - reads from ::aInfo */
   /* METHOD sqlEof                       C level implemented - reads from ::aInfo */
   /* METHOD qlFound                      C level implemented - reads from ::aInfo */
   METHOD sqlGoBottom()
   METHOD sqlGoPhantom()
   METHOD sqlGoTo( uRecord, lNoOptimize )
   /* METHOD sqlGoToId                    C level implemented - maps to sqlGoTo() */
   METHOD sqlGoTop()
   METHOD sqlSeek( uKey, lSoft, lLast )
   /* METHOD sqlSkip                      C level implemented */
   /* METHOD sqlSkipFilter                Superclass does the job */
   /* METHOD sqlSkipRaw                   C level implemented */
   /* METHOD sqlAddField                  Superclass does the job */
   /* METHOD sqlAppend()                  C level implemented */
   /* METHOD sqlCreateFields              Superclass does the job */
   METHOD sqlDeleteRec()
   /* METHOD sqlDeleted                   C level implemented - reads from ::aInfo */
   /* METHOD sqlFieldCount                C level implemented - reads from ::aInfo */
   /* METHOD sqlFieldDisplay              Superclass does the job */
   /* METHOD sqlFieldInfo                 Superclass does the job */
   /* METHOD sqlFieldName                 Superclass does the job */
   METHOD sqlFlush()
   /* METHOD sqlGetRec                    Superclass does the job */
   METHOD sqlGetValue( nField )
   /* METHOD sqlGetVarLen                 C level implemented - reads from aLocalBuffer */
   METHOD sqlGoCold()                     /* NOT called from SQLRDD1.C */
   /* METHOD sqlGoHot                     C level implemented - writes to ::aInfo */
   /* METHOD sqlPutRec                    Superclass does the job */
   /* METHOD sqlPutValue                  C level implemented - writes to aLocalBuffer */
   METHOD sqlRecall()
   /* METHOD sqlRecCount                  C level implemented - reads from ::aInfo */
   /* METHOD sqlRecInfo                   Superclass does the job */
   /* METHOD sqlRecNo                     C level implemented - reads from ::aInfo */
   /* METHOD sqlSetFieldExtent            Superclass does the job */
   /* METHOD sqlAlias                     Superclass does the job */
   METHOD sqlClose()
   METHOD sqlCreate( aStruct, cFileName )
   /* METHOD sqlInfo                      C level implemented - reads from ::aInfo */
   /* METHOD sqlNewArea                   Superclass does the job */
   METHOD sqlOpenArea( cFileName, nArea, lShared, lReadOnly, cAlias ) /* the constructor */
   /* METHOD sqlRelease                   Superclass does the job */
   /* METHOD sqlStructSize                C level implemented */
   /* METHOD sqlSysName                   C level implemented */
   /* METHOD sqlEval                      Superclass does the job */
   METHOD sqlPack()
   /* METHOD sqlPackRec                   Superclass does the job */
   /* METHOD sqlSort                      Superclass does the job - UNSUPPORTED */
   /* METHOD sqlTrans                     Superclass does the job */
   /* METHOD sqlTransRec                  Superclass does the job */
   METHOD sqlZap()
   /* METHOD sqlChildEnd                  C level implemented */
   /* METHOD sqlChildStart                C level implemented */
   /* METHOD sqlChildSync                 C level implemented */
   /* METHOD sqlSyncChildren              C level implemented */
   /* METHOD sqlClearRel                  C level implemented */
   /* METHOD sqlForceRel                  C level implemented */
   /* METHOD sqlRelArea                   Superclass does the job */
   /* METHOD sqlRelEval                   Superclass does the job */
   /* METHOD sqlRelText                   Superclass does the job */
   /* METHOD sqlSetRel                    C level implemented */
   METHOD sqlOrderListAdd( cBagName, cTag )
   METHOD sqlOrderListClear()
   /* METHOD sqlOrderListDelete           Superclass does the job */
   METHOD sqlOrderListFocus( uOrder )
   METHOD sqlOrderListNum( uOrder )       /* Used by sqlOrderInfo */
   /* METHOD sqlOrderListRebuild          Superclass does the job - UNSUPPORTED */
   METHOD sqlOrderCondition( cFor, cWhile, nStart, nNext, uRecord, lRest, lDesc )
   METHOD sqlOrderCreate( cIndexName, cColumns, cTag )
   METHOD sqlOrderDestroy( uOrder, cBag )
   /* METHOD sqlOrderInfo                 C level implemented - reads from ::aInfo and ::aIndex */
   METHOD sqlClearFilter()
   /* METHOD sqlClearLocate               Superclass does the job */
   METHOD sqlClearScope()
   /* METHOD sqlCountScope                Superclass does the job */
   METHOD sqlFilterText()
   /* METHOD sqlScopeInfo                 C level implemented */
   METHOD sqlSetFilter( cFilter )
   /* METHOD sqlSetLocate                 Superclass does the job */
   METHOD sqlSetScope( nType, uValue )
   /* METHOD sqlSkipScope                 Superclass does the job */
   /* METHOD sqlCompile                   Superclass does the job */
   /* METHOD sqlError                     Superclass does the job */
   /* METHOD sqlEvalBlock                 Superclass does the job */
   /* METHOD sqlRawLock                   Superclass does the job */
   METHOD sqlLock( nType, uRecord )
   METHOD sqlUnLock( uRecord )
   /* METHOD sqlCloseMemFile              Superclass does the job - UNSUPPORTED */
   /* METHOD sqlCreateMemFile             Superclass does the job - UNSUPPORTED */
   /* METHOD sqlGetValueFile              Superclass does the job - UNSUPPORTED */
   /* METHOD sqlOpenMemFile               Superclass does the job - UNSUPPORTED */
   /* METHOD sqlPutValueFile              Superclass does the job - UNSUPPORTED */
   /* METHOD sqlReadDBHeader              Superclass does the job - UNSUPPORTED */
   /* METHOD sqlWriteDBHeader             Superclass does the job - UNSUPPORTED */
   /* METHOD sqlExit                      Superclass does the job */
   METHOD sqlDrop()
   METHOD sqlExists()
   /* METHOD sqlWhoCares                  Superclass does the job */

   METHOD SetBOF()
   METHOD sqlKeyCount()
   METHOD sqlRecSize()
   METHOD GetSyntheticVirtualExpr( aExpr )
   METHOD GetSelectList()
   METHOD RecnoExpr()   // add recno filters
   // DESTRUCTOR WA_ENDED

ENDCLASS

//----------------------------------------------------------------------------//
//
// PROCEDURE WA_ENDED  CLASS SR_WORKAREA
//
//   ? "Cleanup:", "WORKAREA", ::cFileName
//
// RETURN
//
/*------------------------------------------------------------------------*/

METHOD sqlSetFilter( cFilter )    CLASS SR_WORKAREA

   Local cExpr
#ifdef NG_DEVELOPMENT
   Local oParser, oTranslator
#endif

   cExpr := ::ParseForClause( cFilter )

   // Try it

   If ::oSql:oSqlTransact:exec( "SELECT A.* FROM " + ::cQualifiedTableName + " A WHERE 0 = 1 AND (" + cExpr + ")" , .F. ) = SQL_SUCCESS
      ::cFilter := cExpr
      ::Refresh()
      ::oSql:oSqlTransact:commit()
      Return SQL_SUCCESS
   EndIf

#ifdef NG_DEVELOPMENT
   // Try with Maxime parser
   oParser := ConditionParser():New( ::cAlias )
   oTranslator := MSSQLExpressionTranslator():New( ::cAlias, .F., .T. )
   cExpr := oTranslator:GetTranslation( oParser:Parse( cFilter ) ):cSQLCondition

   If ::oSql:oSqlTransact:exec( "SELECT A.* FROM " + ::cQualifiedTableName + " A WHERE 0 = 1 AND (" + cExpr + ")" , .F. ) = SQL_SUCCESS
      ::cFilter := cExpr
      ::Refresh()
      ::oSql:oSqlTransact:commit()
      Return SQL_SUCCESS
   EndIf
#endif

   ::oSql:oSqlTransact:commit()

Return SQL_ERROR

/*------------------------------------------------------------------------*/

METHOD sqlClearFilter()    CLASS SR_WORKAREA

   ::cFilter := ""
   ::Refresh()

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlFilterText()    CLASS SR_WORKAREA

   If ::cFilter == NIL
      Return ""
   EndIf

Return ::cFilter

/*------------------------------------------------------------------------*/

METHOD GetSelectList()    CLASS SR_WORKAREA

   local i, nLen := len( ::aFields ), cSelectList := " ", nFeitos := 0
   local aInd

   If ::lCollectingBehavior .or. ::lAllColumnsSelected
      IF  (::osql:nsystemID == SYSTEMID_POSTGR .and. SR_getUseXmlField())
      ELSE
      aEval( ::aFields, { |x,i| (x),::aFields[i, FIELD_ENUM] := i } )
      Return " A.* "
      ENDIF
   EndIf

   If ::hnDeleted > 0
      ::aSelectList[::hnDeleted] := 1
   EndIf
   ::aSelectList[::hnRecno] := 1

   // Current order fields should be added to select list

   If ::aInfo[ AINFO_INDEXORD ] > 0
      aInd := ::aIndex[::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS]
      For i = 1 to len( aInd )
         ::aSelectList[ aInd[i,2] ] := 1
      Next
   EndIf

   For i = 1 to nLen
      If ::aSelectList[i] == 1
         nFeitos++
         cSelectList += if(nFeitos > 1, ", A.", "A.") + SR_DBQUALIFY( ::aNames[i], ::oSql:nSystemID )
         IF ::osql:nsystemID == SYSTEMID_POSTGR .and. ::aFields[ i, FIELD_DOMAIN ] == SQL_LONGVARCHARXML
            cSelectList += "::varchar"
         ENDIF

         ::aFields[ i, FIELD_ENUM ] := nFeitos
      Else
         ::aFields[ i, FIELD_ENUM ] := 0
      EndIf
   Next

   If nFeitos == nLen
      if (::osql:nsystemID == SYSTEMID_POSTGR .and. SR_getUseXmlField())
      else
      cSelectList := " A.* "
      ::lAllColumnsSelected := .T.
      endif
   EndIf

Return cSelectList + " "

/*------------------------------------------------------------------------*/

METHOD sqlGetValue( nField ) CLASS SR_WORKAREA

   local aRet := { NIL }, lOldIndex

   If ::aInfo[ AINFO_DETECT1_COUNT ] > SQLRDD_LEARNING_REPETITIONS
      ::lAllColumnsSelected := .T.
      ::sqlGoTo( ::aInfo[ AINFO_RECNO ], .T. )
      Return ::aLocalBuffer[nField]
   EndIf

   If ::oSql:Execute( "SELECT " + SR_DBQUALIFY( ::aNames[nField], ::oSql:nSystemID ) +;
                      " FROM " +;
                      ::cQualifiedTableName + ::WhereEqual() ) == SQL_SUCCESS

      lOldIndex := ::aFields[nField, FIELD_ENUM]
      ::aFields[nField, FIELD_ENUM] := 1

      If ::oSql:Fetch( NIL, .F., { ::aFields[nField] } ) == SQL_SUCCESS
         ::oSql:GetLine( { ::aFields[nField] }, .F., @aRet )
         If aRet[1] != NIL
            ::aLocalBuffer[nField] := aRet[1]
            If ::aInfo[ AINFO_NPOSCACHE ] > 0 .and. ::aCache[::aInfo[ AINFO_NPOSCACHE ]] != NIL .and. len(::aCache[::aInfo[ AINFO_NPOSCACHE ]]) > 0
               ::aCache[::aInfo[ AINFO_NPOSCACHE ], nField] := aRet[1]
               If ::aInfo[ AINFO_DETECT1_LASTRECNO ] != ::aInfo[ AINFO_RECNO ]
                  ::aInfo[ AINFO_DETECT1_COUNT ] ++
                  ::aInfo[ AINFO_DETECT1_LASTRECNO ] := ::aInfo[ AINFO_RECNO ]
               EndIf
            EndIf
         EndIf
      EndIf

      ::aFields[nField, FIELD_ENUM] := lOldIndex

   EndIf

Return aRet[1]

/*------------------------------------------------------------------------*/

METHOD sqlRecSize() CLASS SR_WORKAREA

   Local i := 0, aCol

   For each aCol in ::aFields
      i += aCol[3]
   Next

Return i

/*------------------------------------------------------------------------*/

METHOD SolveRestrictors()  CLASS SR_WORKAREA

   Local cRet := ""

   If !empty(::cFor)
      cRet += "(" + ::cFor + ")"
   EndIf

   If !::lDisableFlts
      If !empty(::cFilter)
         cRet := "(" + ::cFilter + ")"
      EndIf
      If !empty(::cScope)
         if !empty(cRet)
            cRet += " AND "
         EndIf
         cRet += "(" + ::cScope + ")"
      EndIf
      If !empty(::cFltUsr)
         if !empty(cRet)
            cRet += " AND "
         EndIf
         cRet += "(" + ::cFltUsr + ")"
      EndIf
      If ::aInfo[ AINFO_INDEXORD ] > 0 .and. (!Empty( ::aIndex[::aInfo[ AINFO_INDEXORD ], SCOPE_SQLEXPR] ))
         if !empty(cRet)
            cRet += " AND "
         EndIf
         cRet += "(" + ::aIndex[::aInfo[ AINFO_INDEXORD ], SCOPE_SQLEXPR] + ") "
      EndIf
      If ::lHistoric .and. ::lHistEnable
         if !empty(cRet)
            cRet += " AND "
         EndIf
         cRet += ::HistExpression()
      EndIf
     if len(::aRecnoFilter ) > 0
         if !empty(cRet)
            cRet += " OR  "
         EndIf
         cRet += ::RecnoExpr()
     endif
   EndIf
   /*
   if  SR_UseDeleteds() .and. set( _SET_DELETED )
      if !empty( cRet )
         cRet += " AND "
      ENDIF
      cRet += " (" + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " IS NULL  OR "  + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID )  + " != " + if(::nTCCompat > 0, "'*'", "'T'") +" ) "
   endif
   */
Return cRet

/*------------------------------------------------------------------------*/

METHOD GetSyntheticVirtualExpr( aExpr, cAlias )  CLASS SR_WORKAREA

   Local cRet := "", cColName, nPos


   DEFAULT cAlias := ""

   If !Empty(cAlias)
      cAlias += "."
   EndIf

   For each cColName in aExpr
      nPos := aScan( ::aNames, {|x| x == upper( cColName )} )
      If nPos <= 0
         ::RunTimeErr("31", SR_Msg(31) + cColName + " Table : " + ::cFileName )
         Exit
      Else
         If !Empty(cRet)
            cRet+="||"
         EndiF

         Do Case
         Case ::aFields[nPos,2] == "C"
            cRet += "RPAD(NVL("+cAlias+::aFields[nPos,1]+",' '),"+alltrim(str(::aFields[nPos,3],5))+")"
         Case ::aFields[nPos,2] == "D"
            cRet += "TO_CHAR("+cAlias+::aFields[nPos,1]+",'YYYYMMDD')"
         Case ::aFields[nPos,2] == "N"
            cRet += "SUBSTR(TO_CHAR(NVL("+cAlias+::aFields[nPos,1]+",0),'"+replicate("9",::aFields[nPos,3]-::aFields[nPos,4]-1 -if(::aFields[nPos,4]>0,1,0))+"0"+if(::aFields[nPos,4]>0,"."+replicate("9",::aFields[nPos,4]),"")+"'),2," + str(::aFields[nPos,3])+")"
         OtherWise
            ::RunTimeErr("31", SR_Msg(31) + cColName + "(2) Table : " + ::cFileName )
         EndCase
      EndIf
   Next
Return cRet

/*------------------------------------------------------------------------*/

METHOD LoadRegisteredTags()  CLASS SR_WORKAREA

   Local aInd, cLast := "##", lCDXCompat := .F., aRet, aThisIndex, aCols, i, cind, nPos, cItem

   aSize( ::aIndexMgmnt, 0 )
   ::oSql:exec( "SELECT TABLE_,SIGNATURE_,IDXNAME_,IDXKEY_,IDXFOR_,IDXCOL_,TAG_,TAGNUM_ FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + UPPER(::cFileName) + "' ORDER BY IDXNAME_, TAGNUM_"  ,.F., .T., @::aIndexMgmnt )

   For each aInd in ::aIndexMgmnt
      aSize( aInd, INDEXMAN_SIZE )
      If aInd[INDEXMAN_IDXKEY][4] == "@"
         If ::oSql:nSystemID == SYSTEMID_ORACLE
            aInd[INDEXMAN_VIRTUAL_SYNTH] := SubStr(aInd[INDEXMAN_IDXKEY],1,3)+SubStr(::cFileName,1,25)
         EndIf
         aInd[INDEXMAN_IDXKEY]  := SubStr( aInd[INDEXMAN_IDXKEY], 5 )
      EndIf
      if !Empty( aInd[INDEXMAN_COLUMNS] )

         aInd[INDEXMAN_KEY_CODEBLOCK] := &( "{|| SR_Val2Char(" + alltrim( aInd[INDEXMAN_IDXKEY] ) + ") + str(Recno(),15) }" )
         aInd[INDEXMAN_SYNTH_COLPOS]  := aScan( ::aNames, "INDKEY_" + aInd[ INDEXMAN_COLUMNS] )     // Make life easier in odbcrdd2.c
      Else

         aInd[INDEXMAN_KEY_CODEBLOCK] := &( "{|| " + alltrim( aInd[INDEXMAN_IDXKEY] ) + " }" )
      EndIf
      aInd[INDEXMAN_IDXNAME] := alltrim( aInd[INDEXMAN_IDXNAME] )
      aInd[INDEXMAN_TAG] := alltrim( aInd[INDEXMAN_TAG] )
      If aInd[INDEXMAN_FOR_EXPRESS][1] == "#"
         aInd[INDEXMAN_FOR_CODEBLOCK] := &( "{|| if(" + alltrim( SubStr(aInd[INDEXMAN_FOR_EXPRESS],5) ) + ",'T','F') }" )     // FOR clause codeblock
         aInd[INDEXMAN_FOR_COLPOS]    := aScan( ::aNames, "INDFOR_" + SubStr( aInd[INDEXMAN_FOR_EXPRESS],2,3) )   // Make life easier in odbcrdd2.c
      EndIf
      // If there is no more than one occourrence of same index bag name,
      // for sure we are dealing with CDX compatible application
      If cLast == aInd[INDEXMAN_IDXNAME]
         lCDXCompat := .T.
      EndIf
      cLast := aInd[INDEXMAN_IDXNAME]
   Next

   If lUseDBCatalogs
      aRet := {}
      Switch ::oSql:nSystemID
      Case SYSTEMID_IBMDB2
         ::oSql:exec( "SELECT NAME, COLNAMES FROM SYSIBM.SYSINDEXES WHERE CREATOR != 'SYSIBM' AND TBNAME = '" + ::cFileName + "' ORDER BY 1", .F., .T., @aRet )
         For each aInd in aRet
            aInd[1] := Upper(rtrim( aInd[1] ))
            If right( aInd[1], 4 ) == "_UNQ"
               Loop
            EndIf
            aCols := hb_atokens( alltrim(aInd[2]), "+" )
            aDel( aCols, 1 )
            aSize( aCols, len( aCols ) - 1 )    // Remove first "+"
            aThisIndex := Array( INDEXMAN_SIZE )
            aThisIndex[INDEXMAN_TABLE] := ::cFileName
            aThisIndex[INDEXMAN_SIGNATURE] := "DBCATALOG"
            aThisIndex[INDEXMAN_IDXNAME] := aInd[1]
            aThisIndex[INDEXMAN_IDXKEY] := ""
            For i = 1 to len(aCols)
               If alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_" .and. len(aCols) == 1
                  Exit
               EndIf
               aThisIndex[INDEXMAN_IDXKEY] += if(i>1,',"','"')+alltrim(aCols[i])+'"'
               If alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_"
                  Exit
               EndIf
            Next
            If Empty( aThisIndex[INDEXMAN_IDXKEY] )
               Exit
            EndIf
            aThisIndex[INDEXMAN_FOR_EXPRESS] := ""
            aThisIndex[INDEXMAN_COLUMNS] := ""
            aThisIndex[INDEXMAN_TAG] := aInd[1]
            aThisIndex[INDEXMAN_TAGNUM] := StrZero( HB_EnumIndex(), 6 )
            aThisIndex[INDEXMAN_KEY_CODEBLOCK] := &( "{|| " + aThisIndex[INDEXMAN_IDXKEY] + " }" )
            aThisIndex[INDEXMAN_SYNTH_COLPOS]  := 0
            aThisIndex[INDEXMAN_FOR_COLPOS]    := 0

            aadd( ::aIndexMgmnt, aThisIndex )
         Next

         Exit
      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
         aRet:= {}
         ::oSql:exec( "show index from "+ ::cFileName , .F., .T., @aRet )

         aCols := {}
         For EACH aInd IN aRet
           If ( npos := Ascan( aCols , { |x| x[1] == aInd[3] } ) )== 0
              AADD(aCols ,{aInd[3],""})
           EndIf
         Next

         For EACH aInd IN aCols

           clast := aInd[1]
           cind  := ""

           For EACH citem IN aret
             If citem[3]==clast
                cind += Alltrim(citem[5]) + ","
             EndIf
           Next

           cind    := SubStr( cind,1,LEn(cind)-1) // remove ","
           aInd[2] := Upper(cind)

         Next

         For each aInd in aCols

            If asc(alltrim(aInd[2])[-1]) == 0
               aInd[2] := SubStr( aInd[2], 1, len(alltrim(aInd[2]))-1 )
            EndIf

            aCols := hb_atokens( alltrim(aInd[2]), "," )

            aThisIndex := Array( INDEXMAN_SIZE )
            aThisIndex[INDEXMAN_TABLE] := ::cFileName
            aThisIndex[INDEXMAN_SIGNATURE] := "DBCATALOG"
            aThisIndex[INDEXMAN_IDXNAME] := rtrim( aInd[1] )
            aThisIndex[INDEXMAN_IDXKEY] := ""
            For i = 1 to len(aCols)
               If ( alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_" ) .and. len(aCols) == 1
                  Exit
               EndIf
               aThisIndex[INDEXMAN_IDXKEY] += if(i>1,',"','"')+alltrim(aCols[i])+'"'
               If alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_"
                  Exit
               EndIf
            Next
            If Empty( aThisIndex[INDEXMAN_IDXKEY] ) .and. len( ::aIndexMgmnt ) > 0
               Exit
            EndIf
            If (!Empty( aThisIndex[INDEXMAN_IDXKEY] ))
               aThisIndex[INDEXMAN_FOR_EXPRESS] := ""
               aThisIndex[INDEXMAN_COLUMNS] := ""
               aThisIndex[INDEXMAN_TAG] := rtrim( aInd[1] )
               aThisIndex[INDEXMAN_TAGNUM] := StrZero( HB_EnumIndex(), 6 )
               aThisIndex[INDEXMAN_KEY_CODEBLOCK] := &( "{|| " + aThisIndex[INDEXMAN_IDXKEY] + " }" )
               aThisIndex[INDEXMAN_SYNTH_COLPOS]  := 0
               aThisIndex[INDEXMAN_FOR_COLPOS]    := 0
               aadd( ::aIndexMgmnt, aThisIndex )
            EndIf
         Next

         Exit

      Case SYSTEMID_SYBASE
         Exit

      Case SYSTEMID_POSTGR
         ::oSql:oSqlTransact:exec( "SELECT DISTINCT cls.oid, cls.relname as idxname FROM pg_index idx JOIN pg_class cls ON cls.oid=indexrelid JOIN pg_class tab ON tab.oid=indrelid WHERE tab.relname = '" + lower( ::cFileName ) + "' order by idxname", .F., .T., @aRet )
         ::oSql:oSqlTransact:Commit()
         For each aInd in aRet
            aThisIndex := Array( INDEXMAN_SIZE )
            aThisIndex[INDEXMAN_TABLE] := ::cFileName
            aThisIndex[INDEXMAN_SIGNATURE] := "DBCATALOG"
            aThisIndex[INDEXMAN_IDXNAME] := Upper( rtrim( aInd[2] ) )

            If right( aThisIndex[INDEXMAN_IDXNAME], 4 ) == "_UNQ"
               Loop
            EndIf

            aThisIndex[INDEXMAN_IDXKEY] := ""
            aCols := {}
            For i = 1 to 20
               ::oSql:oSqlTransact:exec( "SELECT pg_get_indexdef(" + str(aInd[1]) + "," + str(i,2) + ",true)", .F., .T., @aCols )
               ::oSql:oSqlTransact:Commit()
               If len( aCols ) > 0 .and. !Empty( aCols[1,1] )
                  aCols[1,1] := Upper(alltrim(aCols[1,1]))
                  If aCols[1,1] == ::cRecnoName .or. aCols[1,1] == "R_E_C_N_O_" .and. i == 1
                     Exit
                  EndIf
                  aThisIndex[INDEXMAN_IDXKEY] += if(i>1,',"','"')+aCols[1,1]+'"'
                  If aCols[1,1] == ::cRecnoName .or. aCols[1,1] == "R_E_C_N_O_"
                     Exit
                  EndIf
               Else
                  Exit
               EndIf
            Next
            If !Empty( aThisIndex[INDEXMAN_IDXKEY] )
               aThisIndex[INDEXMAN_FOR_EXPRESS] := ""
               aThisIndex[INDEXMAN_COLUMNS] := ""
               aThisIndex[INDEXMAN_TAG] := Upper( rtrim( aInd[2] ) )
               aThisIndex[INDEXMAN_TAGNUM] := StrZero( HB_EnumIndex(), 6 )
               aThisIndex[INDEXMAN_KEY_CODEBLOCK] := &( "{|| " + aThisIndex[INDEXMAN_IDXKEY] + " }" )
               aThisIndex[INDEXMAN_SYNTH_COLPOS]  := 0
               aThisIndex[INDEXMAN_FOR_COLPOS]    := 0
               aadd( ::aIndexMgmnt, aThisIndex )
            EndIf
         Next
         Exit

      Case SYSTEMID_ORACLE
         ::oSql:exec( "select index_name, column_name, column_position from user_ind_columns where table_name = '" + ::cFileName + "' and index_name not like 'X_WA_Sys%' order by 1, 3", .F., .T., @aRet )
         If len( aRet ) > 0
            cLast := aRet[1,1]
            aCols := {}
            For each aInd in aRet
               If aInd[1] == cLast
                  aadd( aCols, aInd[2] )
                  Loop
               EndIf
               aThisIndex := Array( INDEXMAN_SIZE )
               aThisIndex[INDEXMAN_TABLE] := ::cFileName
               aThisIndex[INDEXMAN_SIGNATURE] := "DBCATALOG"
               aThisIndex[INDEXMAN_IDXNAME] := rtrim( cLast )
               aThisIndex[INDEXMAN_IDXKEY] := ""
               For i = 1 to len(aCols)
                  If alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_" .and. len(aCols) == 1
                     Exit
                  EndIf
                  aThisIndex[INDEXMAN_IDXKEY] += if(i>1,',"','"')+alltrim(aCols[i])+'"'
                  If alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_"
                     Exit
                  EndIf
               Next
               If !Empty( aThisIndex[INDEXMAN_IDXKEY] ) .and. right( rtrim( cLast ), 4 ) != "_UNQ"
                  aThisIndex[INDEXMAN_FOR_EXPRESS] := ""
                  aThisIndex[INDEXMAN_COLUMNS] := ""
                  aThisIndex[INDEXMAN_TAG] := rtrim( cLast )
                  aThisIndex[INDEXMAN_TAGNUM] := StrZero( HB_EnumIndex(), 6 )
                  aThisIndex[INDEXMAN_KEY_CODEBLOCK] := &( "{|| " + aThisIndex[INDEXMAN_IDXKEY] + " }" )
                  aThisIndex[INDEXMAN_SYNTH_COLPOS]  := 0
                  aThisIndex[INDEXMAN_FOR_COLPOS]    := 0
                  aadd( ::aIndexMgmnt, aThisIndex )
               EndIf
               cLast := aInd[1]
               aCols := { aInd[2] }
            Next

            aThisIndex := Array( INDEXMAN_SIZE )
            aThisIndex[INDEXMAN_TABLE] := ::cFileName
            aThisIndex[INDEXMAN_SIGNATURE] := "DBCATALOG"
            aThisIndex[INDEXMAN_IDXNAME] := rtrim( cLast )
            aThisIndex[INDEXMAN_IDXKEY] := ""
            For i = 1 to len(aCols)
               If alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_" .and. len(aCols) == 1
                  Exit
               EndIf
               aThisIndex[INDEXMAN_IDXKEY] += if(i>1,',"','"')+alltrim(aCols[i])+'"'
               If alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_"
                  Exit
               EndIf
            Next
            If !Empty( aThisIndex[INDEXMAN_IDXKEY] ) .and. right( rtrim( cLast ), 4 ) != "_UNQ"
               aThisIndex[INDEXMAN_FOR_EXPRESS] := ""
               aThisIndex[INDEXMAN_COLUMNS] := ""
               aThisIndex[INDEXMAN_TAG] := rtrim( cLast )
               aThisIndex[INDEXMAN_TAGNUM] := StrZero( HB_EnumIndex(), 6 )
               aThisIndex[INDEXMAN_KEY_CODEBLOCK] := &( "{|| " + aThisIndex[INDEXMAN_IDXKEY] + " }" )
               aThisIndex[INDEXMAN_SYNTH_COLPOS]  := 0
               aThisIndex[INDEXMAN_FOR_COLPOS]    := 0
               aadd( ::aIndexMgmnt, aThisIndex )
            EndIf

         EndIf
         Exit
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_AZURE
         ::oSql:exec( "sp_helpindex " + ::cFileName, .F., .T., @aRet )

         For each aInd in aRet
            If asc(alltrim(aInd[3])[-1]) == 0
               aInd[3] := SubStr( aInd[3], 1, len(alltrim(aInd[3]))-1 )
            EndIf
            aCols := hb_atokens( alltrim(aInd[3]), "," )

            aThisIndex := Array( INDEXMAN_SIZE )
            aThisIndex[INDEXMAN_TABLE] := ::cFileName
            aThisIndex[INDEXMAN_SIGNATURE] := "DBCATALOG"
            aThisIndex[INDEXMAN_IDXNAME] := rtrim( aInd[1] )
            aThisIndex[INDEXMAN_IDXKEY] := ""
            For i = 1 to len(aCols)
               If ( alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_" ) .and. len(aCols) == 1
                  Exit
               EndIf
               aThisIndex[INDEXMAN_IDXKEY] += if(i>1,',"','"')+alltrim(aCols[i])+'"'
               If alltrim(aCols[i]) == ::cRecnoName .or. alltrim(aCols[i]) == "R_E_C_N_O_"
                  Exit
               EndIf
            Next
            If Empty( aThisIndex[INDEXMAN_IDXKEY] ) .and. len( ::aIndexMgmnt ) > 0
               Exit
            EndIf
            If (!Empty( aThisIndex[INDEXMAN_IDXKEY] )) .and. right( aThisIndex[INDEXMAN_IDXNAME], 4 ) != "_UNQ" .and. right( aThisIndex[INDEXMAN_IDXNAME], 3 ) != "_PK"
               aThisIndex[INDEXMAN_FOR_EXPRESS] := ""
               aThisIndex[INDEXMAN_COLUMNS] := ""
               aThisIndex[INDEXMAN_TAG] := rtrim( aInd[1] )
               aThisIndex[INDEXMAN_TAGNUM] := StrZero( HB_EnumIndex(), 6 )
               aThisIndex[INDEXMAN_KEY_CODEBLOCK] := &( "{|| " + aThisIndex[INDEXMAN_IDXKEY] + " }" )
               aThisIndex[INDEXMAN_SYNTH_COLPOS]  := 0
               aThisIndex[INDEXMAN_FOR_COLPOS]    := 0
               aadd( ::aIndexMgmnt, aThisIndex )
            EndIf
         Next
         Exit
      End
   EndIf

   If !lCDXCompat
      // If not CDX, orders should be added by creation order
      aSort( ::aIndexMgmnt,,,{ |x,y| x[INDEXMAN_TAGNUM] < y[INDEXMAN_TAGNUM] } )
   EndIf

//   ::aConstrMgmnt := {}
//   ::oSql:exec( "SELECT SOURCETABLE_ , SOURCECOLUMNS_, CONSTRTYPE_, TARGETTABLE_, TARGETCOLUMNS_, CONSTRNAME_ FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS WHERE SOURCETABLE_ = '" + UPPER(::cFileName) + "' ORDER BY CONSTRNAME_"  ,.F., .T., @::aConstrMgmnt )

Return NIL

/*------------------------------------------------------------------------*/

METHOD SetColPK( cColName )  CLASS SR_WORKAREA

   Local nPos := aScan( ::aNames, {|x| x == upper( cColName )} )

   If nPos > 0
      ::nPosCOlPK := nPos
      ::cColPK    := Upper( cColName )
   EndIf

Return ::cColPK

/*------------------------------------------------------------------------*/

METHOD DisableHistoric()  CLASS SR_WORKAREA
   Local i
   ::lHistEnable := .F.
   For i = 1 to len( ::aIndex )
      ::aIndex[i, ORDER_SKIP_UP ]   := NIL
      ::aIndex[i, ORDER_SKIP_DOWN ] := NIL
   Next
Return NIL

/*------------------------------------------------------------------------*/

METHOD EnableHistoric()  CLASS SR_WORKAREA
   Local i
   ::lHistEnable := .T.
   For i = 1 to len( ::aIndex )
      ::aIndex[i, ORDER_SKIP_UP ]   := NIL
      ::aIndex[i, ORDER_SKIP_DOWN ] := NIL
   Next
Return NIL

/*------------------------------------------------------------------------*/

METHOD GetNextRecordNumber()  CLASS SR_WORKAREA

   Local nRet, aRet

   If ::lQuickAppend
      Return ::aInfo[ AINFO_RCOUNT ] + 1
   EndIf

   If ((!::oSql:lUseSequences) .or. (!::lUseSequences))
      nRet := eval( SR_GetNextRecordBlock(), Self )
   Else
      Switch ::oSql:nSystemID
      Case SYSTEMID_INGRES
         aRet := {}
         ::oSql:exec( "SELECT SQ_"+::cFileName+".nextval",.F.,.T.,@aRet )
         If len(aRet) > 0
            nRet := aRet[1,1]
         EndIf
         Exit
      Case SYSTEMID_FIREBR
         aRet := {}
         ::oSql:exec( "SELECT gen_id("+::cFileName+",1) FROM RDB$DATABASE",.F.,.T.,@aRet )
         If len(aRet) > 0
            nRet := aRet[1,1]
         EndIf
         Exit
      Default
         nRet := ::aInfo[ AINFO_RCOUNT ] + 1
      End
   EndIf

Return nRet

/*------------------------------------------------------------------------*/

METHOD ParseIndexColInfo( cSQL )  CLASS SR_WORKAREA

   Local i, nLen, aQuot, cOut := "", nIndexCol, cFieldName, cType, lNull

   If ::aInfo[ AINFO_INDEXORD ] == 0
      Return cSQL
   EndIf

   nLen  := len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] )
   aQuot := array(nLen)

   For i = 1 to nLen
      aQuot[i] := ::QuotedNull(::aLocalBuffer[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] ],.t.,,,,::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],5])
   Next

   nLen := len( cSql )
   cOut := left( cSql, 11 )

   For i = 12 to nLen
      If cSql[i] == "@"

         nIndexCol := val(cSql[i+2])+1  // This is ZERO-base

         If aQuot[nIndexCol] == "NULL"  // This 90% of the problem from 1% of the cases

            cType     := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,nIndexCol,2 ], 2 ]

            If cType == "N"

               cFieldName := "A." + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,nIndexCol,2 ]], ::oSql:nSystemID )

               Switch cSql[i+1]
               Case "1"  // >
                  cOut := ShiftLeftAddParentesis(cOut) + " IS NOT NULL AND " + cFieldName + " > 0 )"
                  Exit
               Case "2"  // =
                  cOut := ShiftLeftAddParentesis(cOut) + " IS NULL OR " + cFieldName + " = 0 )"
                  Exit
               Case "3"  // >=
                  cOut := ShiftLeftAddParentesis(cOut) + " IS NULL OR " + cFieldName + " >= 0 )"
                  Exit
               Case "4"  // <
                  cOut := ShiftLeftAddParentesis(cOut) + " IS NOT NULL AND " + cFieldName + " < 0 )"
                  Exit
               Case "6"  // <=
                  cOut := ShiftLeftAddParentesis(cOut) + " IS NULL OR " + cFieldName + " < 0 )"
                  Exit
               End
            Else
               Switch cSql[i+1]
               Case "1"  // >
                  cOut += "IS NOT NULL"
                  Exit
               Case "2"  // =
                  cOut += "IS NULL"
                  Exit
               Case "3"  // >=
                  cOut := ShiftLeft( cOut ) + " 1 = 1 "
                  Exit
               Case "4"  // <
                  cOut := ShiftLeft( cOut ) + " 1 = 0 "
                  Exit
               Case "6"  // <=
                  cOut += " IS NULL"
                  Exit
               End
            EndIf
         Else
            lNull     := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ], INDEX_FIELDS, nIndexCol, 2 ], 5]

            Switch cSql[i+1]
            Case "1"  // >
               cOut += " > " + aQuot[nIndexCol]
               Exit
            Case "2"  // =
               cOut += " = " + aQuot[nIndexCol]
               Exit
            Case "3"  // >=
               cOut += " >= " + aQuot[nIndexCol]
               Exit
            Case "4"  // <
               If lNull
                  cFieldName := "A." + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,nIndexCol,2 ]], ::oSql:nSystemID )
                  cOut := ShiftLeftAddParentesis(cOut) + " < " + aQuot[nIndexCol] + " OR " + cFieldName + " IS NULL )"
               Else
                  cOut += " < " + aQuot[nIndexCol]
               EndIf
               Exit
            Case "6"  // <=
               If lNull
                  cFieldName := "A." + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,nIndexCol,2 ] ], ::oSql:nSystemID )
                  cOut := ShiftLeftAddParentesis(cOut) + " <= " + aQuot[nIndexCol] + " OR " + cFieldName + " IS NULL )"
               Else
                  cOut += " <= " + aQuot[nIndexCol]
               EndIf
               Exit
            End
         EndIf
         i += 2
      Else
         cOut += cSql[i]
      EndIf
   Next

Return cOut

/*------------------------------------------------------------------------*/

Static Function ShiftLeft( cSql )

   Local i := len( cSql )

   While cSql[i] == " "
      i--
   EndDo
   While cSql[i] != " "
      i--
   EndDo
   cSql := SubStr( cSql, 1, i )

Return cSql

/*------------------------------------------------------------------------*/

Static Function ShiftLeftAddParentesis( cSql )

   Local i := len( cSql )

   While cSql[i] == " "
      i--
   EndDo
   While cSql[i] != " "
      i--
   EndDo
   cSql := SubStr( cSql, 1, i ) + "(" + SubStr( cSql, i )
   i := len( cSql )
   While cSql[i] == " "
      i--
   EndDo

Return cSql

/*------------------------------------------------------------------------*/

METHOD sqlKeyCount( lFilters )  CLASS SR_WORKAREA

   Local nRet := 0, aRet := {}
   Local lDeleteds, cSql, cRet := ""

   DEFAULT lFilters := .T.

   If ::lISAM

      lDeleteds := (!Empty(::hnDeleted)) .and. set( _SET_DELETED )

      cSql := "SELECT COUNT(" + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + ") FROM " + ::cQualifiedTableName + " A " +;
               if( lDeleteds, " WHERE " + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " != " + if(::nTCCompat > 0, "'*'", "'T'"), "" )

      If lFilters
         cRet := ::SolveRestrictors()
         If len( cRet ) > 0
            If !lDeleteds
               cRet := " WHERE" + cRet
            Else
               cRet := " AND " + cRet
            EndIf
            cSql += cRet
         EndIf
      EndIf

      cSql += if(::oSql:lComments," /* dbCount() */","")
      ::oSql:exec( cSql,,.t.,@aRet )

      If len(aRet) > 0
         nRet := aRet[1,1]
      EndIf
   Else
      nRet := len( ::aCache )
   EndIf

Return nRet

/*------------------------------------------------------------------------*/

METHOD IncludeAllMethods()  CLASS SR_WORKAREA

   /* Any methods referenced by startSQLRDDSymbols() should be added here */

   ::sqlGetValue()
   ::READPAGE()
   ::SQLGOBOTTOM()
   ::SQLGOTO()
   ::SQLGOTOP()
   ::SQLSEEK()
   ::SETBOF()
   ::SQLDELETEREC()
   ::SQLFLUSH()
   ::SQLRECALL()
   ::SQLCLOSE()
   ::SQLCREATE()
   ::SQLOPENAREA()
   ::SQLOPENALLINDEXES()
   ::SQLPACK()
   ::SQLZAP()
   ::SQLORDERLISTADD()
   ::SQLORDERLISTCLEAR()
   ::SQLORDERLISTFOCUS()
   ::SQLORDERDESTROY()
   ::SQLORDERCREATE()
   ::SQLORDERCONDITION()
   ::SQLORDERLISTNUM()
   ::SQLSETSCOPE()
   ::SQLLOCK()
   ::SQLUNLOCK()
   ::SQLDROP()
   ::SQLEXISTS()
   ::WRITEBUFFER()
   ::SQLKEYCOUNT()
   ::SQLRECSIZE()
   ::DROPCONSTRAINT()
   ::CREATECONSTRAINT()
   ::GETSYNTHETICVIRTUALEXPR()
   ::SQLSETFILTER()
   ::SQLCLEARFILTER()
   ::SQLFILTERTEXT()

   SR_Serialize1()

Return NIL

/*------------------------------------------------------------------------*/

METHOD LockTable( lCheck4ExcLock, lFLock ) CLASS SR_WORKAREA

   Local lRet := .T., aVet := {}
   Local aResultSet := {}, i

   If aScan( ::aExclusive, { |x| x[2] == ::cFileName } ) > 0    // Table already exclusive by this application instance
      Return .T.
   EndIf

   DEFAULT lCheck4ExcLock := .T.
   DEFAULT lFLock := .F.

   Switch ::oSql:nSystemID
   Case SYSTEMID_IBMDB2
   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB
   Case SYSTEMID_SYBASE
      Exit

   Case SYSTEMID_MSSQL7
   Case SYSTEMID_ORACLE
   Case SYSTEMID_POSTGR
   Case SYSTEMID_AZURE

      For i = 1 to LOCKTABLE_TRIES

         If lCheck4ExcLock

            /* Step 1: Try to create a LOCK to check If someone have this table EXCLUSIVE
               Lock format is: EXCLUSIVE_TABLE_LOCK_SIGN + TableName */

            lRet := SR_SetLocks( EXCLUSIVE_TABLE_LOCK_SIGN + UPPER(::cFileName), ::oSql, 4 )

            /* Step 2: If LOCK acquired, RELEASE IT and return TRUE, else LOOP */

            If lRet
               // Ok, nobody have it EXCLUSIVE !
               SR_ReleaseLocks( EXCLUSIVE_TABLE_LOCK_SIGN + UPPER(::cFileName), ::oSql )
            EndIf

         Else

            /* Try to create a LOCK to this table
               Lock format is: EXCLUSIVE_TABLE_LOCK_SIGN + TableName
               If LOCK acquired, return TRUE. Lock MUST be removed at workarea close */

            If lFLock
               lRet := SR_SetLocks( FLOCK_TABLE_LOCK_SIGN + UPPER(::cFileName), ::oSql, 4 )
            Else
               lRet := SR_SetLocks( { FLOCK_TABLE_LOCK_SIGN + UPPER(::cFileName),;
                                  EXCLUSIVE_TABLE_LOCK_SIGN + UPPER(::cFileName),;
                                     SHARED_TABLE_LOCK_SIGN + UPPER(::cFileName) }, ::oSql, 4 )
            EndIf

         EndIf

         If lRet
            Exit
         EndIf

         Inkey( 0.5 )         // wait .5 seconds before trying again
      Next

      Exit

   End

   If (!lCheck4ExcLock) .and. lRet
      ::lTableLocked := .T.
      aadd( ::aExclusive, { ::nThisArea, ::cFileName } )
   EndIf

Return lRet

/*------------------------------------------------------------------------*/

METHOD UnlockTable(lClosing) CLASS SR_WORKAREA

   Local lRet := .T., aVet := {}
   Local aResultSet := {}, nPos

   If aScan( ::aExclusive, { |x| x[1] == ::nThisArea } ) == 0
      Return .T.
   EndIf

   DEFAULT lClosing := .F.

   If (!lClosing) .and. (!::aInfo[ AINFO_SHARED ])
      Return .F.  // USE EXCLUSIVE cannot be released until file is closed
   EndIf

   Switch ::oSql:nSystemID
   Case SYSTEMID_IBMDB2
   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB
   Case SYSTEMID_SYBASE
      Exit

   Case SYSTEMID_MSSQL7
   Case SYSTEMID_ORACLE
   Case SYSTEMID_POSTGR
   Case SYSTEMID_AZURE
      SR_ReleaseLocks( { EXCLUSIVE_TABLE_LOCK_SIGN + UPPER(::cFileName), FLOCK_TABLE_LOCK_SIGN + UPPER(::cFileName) } , ::oSql )
      Exit
   End

   ::lTableLocked := .F.
   nPos := aScan( ::aExclusive, { |x| x[1] == ::nThisArea } )

   If nPos > 0
      aDel( ::aExclusive, nPos )
      aSize( ::aExclusive, len( ::aExclusive ) - 1 )
   EndIf

Return lRet

/*------------------------------------------------------------------------*/

METHOD LineCount( lMsg ) CLASS SR_WORKAREA

   Local nRet := 0, aRet := {}

   DEFAULT lMsg := .T.

   If ::lISAM

      If nLineCountResult == 0
         Switch ::oSql:nSystemID
         Case SYSTEMID_POSTGR
            ::oSql:exec( "SELECT " + SR_DBQUALIFY( ::cRecnoName, SYSTEMID_POSTGR) + " FROM " + ::cQualifiedTableName + " ORDER BY " + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " DESC LIMIT 1" + if(::oSql:lComments," /* Counting Records */",""),lMsg,.t.,@aRet )
            Exit
         Case SYSTEMID_FIREBR
            ::oSql:exec( "SELECT gen_id("+::cFileName+",0) FROM RDB$DATABASE",.F.,.T.,@aRet )
            Exit
         Case SYSTEMID_CACHE
            ::oSql:exec( "SELECT TOP 1 " + SR_DBQUALIFY(::cRecnoName, SYSTEMID_CACHE) + " FROM " + ::cOwner + ::cFileName + " ORDER BY " + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " DESC", lMsg, .t., @aRet )
            Exit
         Default
           ::oSql:exec( "SELECT MAX( " + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " ) FROM " + ::cQualifiedTableName + if(::oSql:lComments," /* Counting Records */",""),lMsg,.t.,@aRet )
         End

         If Len(aRet) > 0 .and. valtype( aRet[1,1] ) != "N"
           ::oSql:exec( "SELECT COUNT( " + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " ) FROM " + ::cQualifiedTableName + if(::oSql:lComments," /* Counting Records */",""),lMsg,.t.,@aRet )
         EndIf

         If Len(aRet) > 0
            ::aInfo[ AINFO_RCOUNT ] := aRet[1,1]
            nRet := aRet[1,1]
         ElseIf ::oSql:nRetCode != SQL_SUCCESS .and. ::oSql:nRetCode != SQL_NO_DATA_FOUND
            nRet := -1     // Error
         EndIf
      Else
         nRet := nLineCountResult
         ::aInfo[ AINFO_RCOUNT ] := nRet
      EndIf
   Else
      nRet := len( ::aCache )
   EndIf

   If Empty( nRet )
      ::lEmptyTable           := .T.
      ::aInfo[ AINFO_RECNO ]  := 1
      ::aInfo[ AINFO_RCOUNT ] := 0
      ::aInfo[ AINFO_BOF ]    := .T.
      ::aInfo[ AINFO_EOF ]    := .T.
   EndIf

Return nRet

/*------------------------------------------------------------------------*/

METHOD sqlOpenAllIndexes() CLASS SR_WORKAREA

   Local i, aCols
   Local cOrdName
   Local nPos, nInd
   Local cXBase, cCol, nPosAt
   Local cSqlA, cSqlD
   Local cColumns
   Local lSyntheticVirtual := .F., cPhysicalVIndexName

   aSize( ::aIndex, len( ::aIndexMgmnt ) )

   For nInd = 1 to len( ::aIndexMgmnt )

      If !Empty( ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] )
         aCols := { "INDKEY_" + ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] }
      Else
         aCols := &( "{" + ::aIndexMgmnt[nInd, INDEXMAN_IDXKEY] + "}" )
//         aCols := HB_ATokens( StrTran( ::aIndexMgmnt[nInd, INDEXMAN_IDXKEY], '"', "" ), "," )
      endif

      cOrdName := ::aIndexMgmnt[nInd, INDEXMAN_TAG]
      cColumns := ::aIndexMgmnt[nInd, INDEXMAN_IDXKEY]
      cSqlA  := " ORDER BY "
      cSqlD  := " ORDER BY "
      cXBase := ""

      If Empty(cOrdName)
         cOrdName := ""
      EndIf

      If ::aIndexMgmnt[nInd, INDEXMAN_VIRTUAL_SYNTH] != NIL
         lSyntheticVirtual    := .T.
         cPhysicalVIndexName  := ::aIndexMgmnt[nInd, INDEXMAN_VIRTUAL_SYNTH]
      Else
         cPhysicalVIndexName  := NIL
      EndIf

      ::aIndex[nInd] := { "","",{},"","","", NIL, NIL, cOrdName, cColumns,,,,,,0, ::aIndexMgmnt[nInd, INDEXMAN_SIGNATURE][19] == "D", cPhysicalVIndexName,,, ::aIndexMgmnt[nInd, INDEXMAN_IDXNAME] }
      ::aIndex[nInd,INDEX_FIELDS] := Array( len( aCols ) )

      For i = 1 to len( aCols )

         nPosAt := At( aCols[i], " " )

         If nPosAt = 0
            cCol := aCols[i]
         Else
            cCol := SubStr( aCols[i], 1, nPosAt )
         EndIf

         Switch ::oSql:nSystemID
         Case SYSTEMID_ORACLE
         Case SYSTEMID_FIREBR
         Case SYSTEMID_FIREBR3
            cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ NULLS FIRST,]
            cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC NULLS LAST,]
            exit
         Case SYSTEMID_IBMDB2
            If "08.0" $ ::oSql:cSystemVers .and. (!"08.00" $ ::oSql:cSystemVers)
               cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ NULLS FIRST,]
               cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC NULLS LAST,]
            Else
               cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [,]
               cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC,]
            EndIf
            exit
         Case  SYSTEMID_POSTGR
            if ::osql:lPostgresql8
               cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ NULLS FIRST,]
               cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC NULLS LAST,]
            else

               cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [,]
               cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC,]
            endif
            exit
         Default
            cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [,]
            cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC,]
         End

         If (nPos := aScan( ::aNames, {|x| x == cCol } )) != 0
            If ::aNames[nPos] != ::cRecnoName

               ::aIndex[nInd, SYNTH_INDEX_COL_POS] := nPos

               Do Case
               Case ::aFields[nPos,2] = "C" .and. ::aNames[nPos] == ::cDeletedName
                  cXBase += "Deleted() + "
               Case ::aFields[nPos,2] = "C"
                  cXBase += ::aNames[nPos] + " + "
               Case ::aFields[nPos,2] = "D"
                  cXBase += "DTOS(" + ::aNames[nPos] + ") + "
               Case ::aFields[nPos,2] = "T"
                  cXBase += "TTOS(" + ::aNames[nPos] + ") + "
               Case ::aFields[nPos,2] = "N"
                  cXBase += "STR(" + ::aNames[nPos] + ", " + alltrim(str(::aFields[nPos,3])) + ", " +;
                            alltrim(str(::aFields[nPos,4])) + ") + "
               Case ::aFields[nPos,2] = "L"
                  cXBase += "Sr_cdbvalue("+ ::aNames[nPos]+ ")" + " + "
               EndCase
            EndIf
         Else
            ::RunTimeErr("18", SR_Msg(18) + cCol + " Table : " + ::cFileName )
            Return 0       /* error exit */
         EndIf

         ::aIndex[nInd,INDEX_FIELDS,i] := { aCols[i], nPos }

      Next

      cXBase := left( cXBase,len(cXBase)-2)
      cSqlA  := left( cSqlA, len(cSqlA)-1 ) + " "
      cSqlD  := left( cSqlD, len(cSqlD)-1 ) + " "

      ::aIndex[ nInd, ORDER_ASCEND ] := cSqlA
      ::aIndex[ nInd, ORDER_DESEND ] := cSqlD
      ::aIndex[ nInd, INDEX_KEY ]    := rtrim(if( nInd > 0 .and. (!Empty(::aIndexMgmnt[nInd, INDEXMAN_COLUMNS])), ::aIndexMgmnt[nInd, INDEXMAN_IDXKEY], cXBase ))
      If !Empty( ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] )

         IF RDDNAME() =="SQLEX"
            ::aIndex[ nInd, INDEX_KEY_CODEBLOCK ] := &( "{|| " + cXBase + " }")  //aScan( ::aNames, "INDKEY_" + ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] )
         ELSE
            ::aIndex[ nInd, INDEX_KEY_CODEBLOCK ] := aScan( ::aNames, "INDKEY_" + ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] )
         ENDIF
      Else
         ::aIndex[ nInd, INDEX_KEY_CODEBLOCK ] := &( "{|| " + cXBase + " }")
      EndIf
      If ::aIndexMgmnt[nInd, INDEXMAN_FOR_EXPRESS][1] != "#"
         ::aIndex[ nInd, FOR_CLAUSE ]   := rtrim(::aIndexMgmnt[nInd, INDEXMAN_FOR_EXPRESS])
      Else
         ::aIndex[ nInd, FOR_CLAUSE ]   := "INDFOR_" + SubStr(::aIndexMgmnt[nInd, INDEXMAN_FOR_EXPRESS],2,3) + " = 'T'"
      EndIf
      ::aIndex[ nInd, STR_DESCENDS ] := ""
      ::aIndex[ nInd, SYNTH_INDEX_COL_POS] := if( nInd > 0 .and. (!Empty(::aIndexMgmnt[nInd, INDEXMAN_COLUMNS])), ::aIndex[ nInd, SYNTH_INDEX_COL_POS], 0 )

      If lSyntheticVirtual
         ::aIndex[ nInd, VIRTUAL_INDEX_EXPR] := ::GetSyntheticVirtualExpr(aCols, "A")
      EndIf

   Next

   ::lStable := .F.

   If len( ::aIndexMgmnt ) > 0 .and. set(_SET_AUTORDER) <= len( ::aIndexMgmnt )
      //Return ::sqlOrderListFocus(1)
      Return ::sqlOrderListFocus( set(_SET_AUTORDER) )
   endif

Return 0

/*------------------------------------------------------------------------*/

METHOD OrderBy( nOrder, lAscend, lRec ) CLASS SR_WORKAREA

   DEFAULT nOrder := ::aInfo[ AINFO_INDEXORD ]
   DEFAULT lRec   := .T.

   lAscend := if( ::aInfo[ AINFO_REVERSE_INDEX ], !lAscend, lAscend )

   If lRec
      If (nOrder == 0 .or. nOrder > len(::aIndex))
         return " ORDER BY A." + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + if(lAscend, " ", " DESC " )
      endif
      Return ::aIndex[nOrder,if(lAscend,ORDER_ASCEND,ORDER_DESEND)]
   Else
      If (nOrder == 0 .or. nOrder > len(::aIndex))
         return " "
      endif
      Return strtran( ::aIndex[nOrder,if(lAscend,ORDER_ASCEND,ORDER_DESEND)], ", A." + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ), "" )
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD FirstFetch( nDirection ) CLASS SR_WORKAREA

   Local uRecord, nFecth, nBlockPos := 0, nPos
   Local nOldBg, nOldEnd, lCacheIsEmpty

   DEFAULT nDirection := ORD_INVALID

   ::oSql:nRetCode := ::oSql:Fetch( , .F., ::aFields )

   If ::lFetchAll
      nDirection := ORD_DIR_FWD
      ::nCurrentFetch := Max( ::nCurrentFetch, 50 )
   EndIf

   Switch ::oSql:nRetCode
   Case SQL_SUCCESS

      ::lNoData            := .F.
      ::aInfo[ AINFO_EOF ] := .F.
      ::aInfo[ AINFO_BOF ] := .F.
      ::lEmptyTable        := .F.
      nOldBg               := ::aInfo[ AINFO_NCACHEBEGIN ]
      nOldEnd              := ::aInfo[ AINFO_NCACHEEND ]
      lCacheIsEmpty        := (nOldBg == nOldEnd) .and. nOldEnd == 0

      If nDirection = ORD_DIR_FWD
         If (::aInfo[ AINFO_NPOSCACHE ]+1) > (CAHCE_PAGE_SIZE * 3)
            nBlockPos := 1
         Else
            nBlockPos := ::aInfo[ AINFO_NPOSCACHE ]+1
         EndIf
         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NPOSCACHE ] + ::nCurrentFetch
         If nOldBg == nOldEnd
            If ::aInfo[ AINFO_NCACHEEND ] > CAHCE_PAGE_SIZE * 3   // 3 pages...
               ::aInfo[ AINFO_NCACHEEND ] -= (CAHCE_PAGE_SIZE * 3)
            EndIf
            ::aInfo[ AINFO_EOF_AT ]   := 0
         ElseIf nOldBg < nOldEnd
            If ::aInfo[ AINFO_NCACHEEND ] > CAHCE_PAGE_SIZE * 3   // 3 pages...
               ::aInfo[ AINFO_NCACHEEND ] -= (CAHCE_PAGE_SIZE * 3)
               If ::aInfo[ AINFO_NCACHEEND ] >= (::aInfo[ AINFO_NCACHEBEGIN ] - 2)
                  ::aInfo[ AINFO_NCACHEBEGIN ] := ::aInfo[ AINFO_NCACHEEND ] + 2
               EndIf
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
         Else
            If ::aInfo[ AINFO_NCACHEEND ] >= (::aInfo[ AINFO_NCACHEBEGIN ] - 2)
               ::aInfo[ AINFO_NCACHEBEGIN ] := ::aInfo[ AINFO_NCACHEEND ] + 2
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
            If ::aInfo[ AINFO_NCACHEEND ] > CAHCE_PAGE_SIZE * 3   // 3 pages...
               ::aInfo[ AINFO_NCACHEEND ] -= (CAHCE_PAGE_SIZE * 3)
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
            If ::aInfo[ AINFO_NCACHEBEGIN ] > CAHCE_PAGE_SIZE * 3   // 3 pages...
               ::aInfo[ AINFO_NCACHEBEGIN ] -= (CAHCE_PAGE_SIZE * 3)
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf

         EndIf
         If ::aInfo[ AINFO_NCACHEBEGIN ] == 0
            ::aInfo[ AINFO_NCACHEBEGIN ] := 1
         EndIf
         If ::aInfo[ AINFO_NPOSCACHE ] == 0
            ::aInfo[ AINFO_NPOSCACHE ] := 1
         EndIf
         If ::aCache[nBlockPos] == NIL
            ::aCache[nBlockPos] := Array( len( ::aLocalBuffer ) )
         EndIf
         ::oSql:GetLine( ::aFields, .F., @::aCache[ nBlockPos ] )
         uRecord := ::aCache[ nBlockPos, ::hnRecno ]
         If ::aInfo[ AINFO_NPOSCACHE ] == 0
            ::aInfo[ AINFO_NPOSCACHE ] := 1
         EndIf
         nPos := ::aInfo[ AINFO_NPOSCACHE ] + if( lCacheIsEmpty, 0, 1 )
         If nPos > (CAHCE_PAGE_SIZE * 3)
            nPos := 1
         EndIf

         If ::lFetchAll
            aadd( ::aFetch, uRecord )
         EndIf

      ElseIf nDirection = ORD_DIR_BWD

         If (::aInfo[ AINFO_NPOSCACHE ]-1) < 1
            nBlockPos := CAHCE_PAGE_SIZE * 3
         Else
            nBlockPos := ::aInfo[ AINFO_NPOSCACHE ]-1
         EndIf
         ::aInfo[ AINFO_NCACHEBEGIN ] := ::aInfo[ AINFO_NPOSCACHE ] - ::nCurrentFetch
         If nOldBg == nOldEnd
            If ::aInfo[ AINFO_NCACHEBEGIN ] < 1
               ::aInfo[ AINFO_NCACHEBEGIN ] += (CAHCE_PAGE_SIZE * 3)
            EndIf
            ::aInfo[ AINFO_EOF_AT ]   := 0

         ElseIf nOldBg < nOldEnd
            If ::aInfo[ AINFO_NCACHEBEGIN ] < 1
               ::aInfo[ AINFO_NCACHEBEGIN ] += (CAHCE_PAGE_SIZE * 3)
               If (::aInfo[ AINFO_NCACHEEND ] + 2) >= ::aInfo[ AINFO_NCACHEBEGIN ]
                  ::aInfo[ AINFO_NCACHEEND ]  := ::aInfo[ AINFO_NCACHEBEGIN ] - 2
               EndIf
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
         Else
            If (::aInfo[ AINFO_NCACHEEND ] + 2) >= ::aInfo[ AINFO_NCACHEBEGIN ]
               ::aInfo[ AINFO_NCACHEEND ]  := ::aInfo[ AINFO_NCACHEBEGIN ] - 2
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
            If ::aInfo[ AINFO_NCACHEBEGIN ] < 1
               ::aInfo[ AINFO_NCACHEBEGIN ] += (CAHCE_PAGE_SIZE * 3)
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
            If ::aInfo[ AINFO_NCACHEEND ] < 1
               ::aInfo[ AINFO_NCACHEEND ] += (CAHCE_PAGE_SIZE * 3)
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
         EndIf

         If ::aInfo[ AINFO_NCACHEEND ] == 0
            ::aInfo[ AINFO_NCACHEEND ] := (CAHCE_PAGE_SIZE * 3)
         EndIf
         If ::aInfo[ AINFO_NPOSCACHE ] == 0
            ::aInfo[ AINFO_NPOSCACHE ] := (CAHCE_PAGE_SIZE * 3)
         EndIf
         If ::aCache[nBlockPos] == NIL
            ::aCache[nBlockPos] := Array( len( ::aLocalBuffer ) )
         EndIf
         ::oSql:GetLine( ::aFields, .F., @::aCache[nBlockPos] )
         uRecord := ::aCache[ nBlockPos, ::hnRecno ]
         nPos := ::aInfo[ AINFO_NPOSCACHE ] - if( lCacheIsEmpty, 0, 1 )
         If nPos < 1
            nPos := (CAHCE_PAGE_SIZE * 3)
         EndIf

      Else
         ::aInfo[ AINFO_NPOSCACHE ] := ::aInfo[ AINFO_NCACHEBEGIN ] := ::aInfo[ AINFO_NCACHEEND ] := nBlockPos := 1
         ::oSql:GetLine( ::aFields, .F., @::aCache[1] )
         uRecord   := ::aCache[ nBlockPos, ::hnRecno ]
      EndIf

      If nDirection = ORD_DIR_FWD .or. nDirection = ORD_DIR_BWD
         For nFecth = 1 to ::nCurrentFetch
            ::oSql:nRetCode := ::oSql:Fetch( NIL, .F., ::aFields )
            If ::oSql:nRetCode != SQL_SUCCESS
               If ::oSql:nRetCode == SQL_ERROR
                  DEFAULT ::cLastComm := ::oSql:cLastComm
                  ::RunTimeErr("999", "[FetchLine Failure][" + alltrim(str(::oSql:nRetCode)) + "] " + ::oSql:LastError() + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
               EndIf
               If nDirection = ORD_DIR_FWD
                  ::aInfo[ AINFO_EOF_AT ]    := uRecord
                  ::aInfo[ AINFO_NCACHEEND ] := nPos
               Else
                  ::aInfo[ AINFO_BOF_AT ]      := uRecord
                  ::aInfo[ AINFO_NCACHEBEGIN ] := nPos
               EndIf

               exit
            Endif
            If nDirection = ORD_DIR_FWD
               nPos ++
               If nPos > (CAHCE_PAGE_SIZE * 3)
                  nPos -= (CAHCE_PAGE_SIZE * 3)
               EndIf
            Else
               nPos --
               If nPos < 1
                  nPos += (CAHCE_PAGE_SIZE * 3)
               EndIf
            EndIf

            ::oSql:GetLine( ::aFields, .F., @::aCache[nPos] )
            uRecord := ::aCache[nPos,::hnRecno]
            If ::lFetchAll
               aadd( ::aFetch, uRecord )
            EndIf
         Next
      EndIf
      If ::aCache[ ::aInfo[ AINFO_NPOSCACHE ] ] != NIL
         ::GetBuffer()     // Loads current cache position to record buffer
      EndIf
      Exit

   Case SQL_NO_DATA_FOUND

      ::lNoData := .T.

      If nDirection = ORD_DIR_BWD
         ::aInfo[ AINFO_BOF ]    := .T.
         ::aInfo[ AINFO_BOF_AT ] := ::aInfo[ AINFO_RECNO ]
      ElseIf nDirection = ORD_DIR_FWD
         ::aInfo[ AINFO_EOF_AT ] := ::aInfo[ AINFO_RECNO ]
         ::GetBuffer(.T.)         // Clean Buffer
      Else
         ::GetBuffer(.T.)         // Clean Buffer
      EndIf

      Exit
   Default
      ::lNoData := .T.
      DEFAULT ::cLastComm := ::oSql:cLastComm
      ::RunTimeErr("999", "[Fetch Failure/First][" + alltrim(str(::oSql:nRetCode)) + "] " + ::oSql:LastError() + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
   End

Return NIL

/*------------------------------------------------------------------------*/

METHOD Stabilize() CLASS SR_WORKAREA

   Local nLen, nPos, nRec, aPos, i, nLast := 0

   If ::lStable .or. ::aInfo[ AINFO_INDEXORD ] == 0 .or. len( ::aIndex ) == 0 .and. len( ::aCache ) > 0
      Return NIL
   EndIf

   /* Stabilize means re-order the workarea cache */

   nLen := len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] ) /* - 1      && This "-1" is to removes the NRECNO column */
   nRec := ::aLocalBuffer[::hnRecno]

   If nLen == 1      && One field index is easy and fast !
      nPos := ::aIndex[ ::aInfo[ AINFO_INDEXORD ], INDEX_FIELDS, 1,2 ]
      aSort( ::aCache, , , { |x,y| x[nPos] < y [nPos] } )
   Else
      aPos := Array(nLen)
      For i = 1 to nLen
         aPos[i] := ::aIndex[::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ]
      Next
      aSort( ::aCache, , , { |x,y| aOrd( x, y, aPos ) } )
   EndIf

   ::aInfo[ AINFO_NPOSCACHE ] := aScan( ::aCache, { |x| x[::hnRecno] == nRec } )

   If ::aInfo[ AINFO_NPOSCACHE ] == 0
      ::aInfo[ AINFO_NPOSCACHE ] := len( ::aCache )
   EndIf

   ::GetBuffer( .F., ::aInfo[ AINFO_NPOSCACHE ] )
   ::lStable   := .T.
   ::aInfo[ AINFO_FOUND ]    := .F.

   ::aInfo[ AINFO_BOF ] := len(::aCache) == 0
   ::aInfo[ AINFO_EOF ] := len(::aCache) == 0

   ::lNoData    := .F.

Return NIL

/*------------------------------------------------------------------------*/

METHOD Normalize( nDirection ) CLASS SR_WORKAREA

   Local nRet := 1

   /*
   * Returns : 0 - Nothing, 1 Ok, 2 GoOut()
   */

   DEFAULT nDirection := 1

   /* Can the actual record pass the filters ?_*/

   While !( eval( ::bScope, ::aLocalBuffer ) .and. eval( ::bFilter, ::aLocalBuffer ) )
      nRet := ::SkipRawCache( nDirection )
      If nRet != 1
         /* EOF or BOF */
         Exit
      EndIf
   EndDo

   Do Case
   Case nDirection < 0 .and. nRet == 0      /* BOF */
      nDirection := 1
      While !( eval( ::bScope, ::aLocalBuffer ) .and. eval( ::bFilter, ::aLocalBuffer ) )
         nRet := ::SkipRawCache( nDirection )
         If nRet != 1
            ::GetBuffer(.T.)
            Exit
         EndIf
      EndDo
      ::aInfo[ AINFO_BOF ] := .T.
   EndCase

Return nRet

/*------------------------------------------------------------------------*/

METHOD SkipRawCache( nToSkip ) CLASS SR_WORKAREA

   Local nRet := 0

   DEFAULT nToSkip := 1

   Do Case
   Case ::aInfo[ AINFO_NPOSCACHE ] + nToSkip > 0 .and. ::aInfo[ AINFO_NPOSCACHE ] + nToSkip <= len( ::aCache ).and. nToSkip != 0
      ::GetBuffer( .F., ::aInfo[ AINFO_NPOSCACHE ] + nToSkip )
      Return 1
   Case ::aInfo[ AINFO_NPOSCACHE ] + nToSkip < 1
      ::GetBuffer( .F., 1 )
      Return 0
   Case ::aInfo[ AINFO_NPOSCACHE ] + nToSkip > len( ::aCache )
      ::aInfo[ AINFO_BOF ] := .F.
      ::GetBuffer(.T.)
   EndCase

return 0

/*------------------------------------------------------------------------*/

METHOD RuntimeErr( cOperation, cErr, nOSCode, nGenCode, SubCode ) CLASS SR_WORKAREA

   Local oErr := ErrorNew()
   Local cDescr

   DEFAULT cOperation := RddName()  // ::ClassName()
   DEFAULT nOSCode    := 0
   DEFAULT nGenCode   := 99
   DEFAULT SubCode    := Val(cOperation)

   If SubCode > 0 .and. SubCode <= SR_GetErrMessageMax()
      DEFAULT cErr       := SR_Msg( SubCode )
   Else
      DEFAULT cErr       := "RunTime Error"
   EndIf

   cDescr := alltrim( cErr )

   ::oSql:RollBack()

   oErr:genCode       := nGenCode
   oErr:subCode       := SubCode
   oErr:CanDefault    := .F.
   oErr:Severity      := ES_ERROR
   oErr:CanRetry      := .T.
   oErr:CanSubstitute := .F.
   oErr:Description   := cDescr + " - RollBack executed."
   oErr:subSystem     := RddName()  // ::ClassName()
   oErr:operation     := cOperation
   oErr:OsCode        := nOSCode
   oErr:FileName      := ::cFileName

   SR_LogFile( "sqlerror.log", { cDescr } )

   Throw( oErr )

Return NIL

/*------------------------------------------------------------------------*/

METHOD CheckCache( oWorkArea ) CLASS SR_WORKAREA

   Local nRecno := ::aLocalBuffer[::hnRecno]

   If oWorkArea:aInfo[ AINFO_EOF ]
      oWorkArea:aInfo[ AINFO_NCACHEEND ] := oWorkArea:aInfo[ AINFO_NCACHEBEGIN ] := 0
      oWorkArea:aInfo[ AINFO_NPOSCACHE ] := 0
   Else
      //If oWorkArea:aLocalBuffer[::hnRecno] == ::aLocalBuffer[::hnRecno]
      If valtype(oWorkArea:aLocalBuffer[oWorkArea:hnRecno])="N".and.valtype(::aLocalBuffer[::hnRecno])="N".and.oWorkArea:aLocalBuffer[oWorkArea:hnRecno] == ::aLocalBuffer[::hnRecno]
         aCopy( ::aLocalBuffer, oWorkArea:aLocalBuffer )
      EndIf
      oWorkArea:aInfo[ AINFO_NCACHEEND ] := oWorkArea:aInfo[ AINFO_NCACHEBEGIN ] := 0
      oWorkArea:aInfo[ AINFO_NPOSCACHE ] := 0
   EndIf
   oWorkArea:aInfo[ AINFO_EOF_AT ]   := 0
   oWorkArea:aInfo[ AINFO_BOF_AT ]   := 0

Return NIL

/*------------------------------------------------------------------------*/

METHOD WhereEqual() CLASS SR_WORKAREA

Return " WHERE " + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " = " + ::Quoted( ::aInfo[ AINFO_RECNO ] )

/*------------------------------------------------------------------------*/

METHOD Quoted( uData, trim, nLen, nDec, nTargetDB, lSynthetic )   CLASS SR_WORKAREA

   Local cType := valtype(uData), cRet

   DEFAULT trim := .F.
   DEFAULT nTargetDB  := ::oSql:cTargetDB
   DEFAULT lSynthetic := .F.

   If cType $ "CM" .and. nLen!=NIL
      uData := Left(uData, nLen)
   EndIf

   If ::nTCCompat > 0
      trim := .F.
   EndIf

   If Empty(uData) .and. ::oSql:nSystemID == SYSTEMID_POSTGR .and. cType == "D"
      If lSynthetic
         Return '        '
      Else
//         uData := stod( '17550101' )   // Lowest date allowed by MSSQL Server
         Return "NULL"
      EndIf
   EndIf

   Do Case
   Case cType $ "CM" .and. ::oSql:nSystemID == SYSTEMID_POSTGR .and. (!trim)
      return [E'] + rtrim(SR_ESCAPESTRING(uData, ::oSql:nSystemID)) + [']
   Case cType $ "CM" .and. (!trim)
      return ['] + SR_ESCAPESTRING(uData, ::oSql:nSystemID) + [']
   Case cType $ "CM" .and. ::oSql:nSystemID == SYSTEMID_POSTGR .and. trim
      return [E'] + rtrim(SR_ESCAPESTRING(uData, ::oSql:nSystemID)) + [']
   Case cType $ "CM" .and. trim
      return ['] + rtrim(SR_ESCAPESTRING(uData, ::oSql:nSystemID)) + [']
   Case cType == "D" .and. ::oSql:nSystemID == SYSTEMID_ORACLE
      return [TO_DATE('] + rtrim(DtoS(uData)) + [','YYYYMMDD')]
   Case cType == "D" .and. ::oSql:nSystemID == SYSTEMID_INFORM
      return ['] + SR_dtoUS(uData) + [']
   Case cType == "D" .and. ::oSql:nSystemID == SYSTEMID_SQLBAS
      return ['] + SR_dtosdot(uData) + [']
    Case cType == "D" .and. (::oSql:nSystemID == SYSTEMID_IBMDB2 .or. ::oSql:nSystemID == SYSTEMID_ADABAS )
      return [']+transform(DtoS(uData) ,'@R 9999-99-99')+[']
   Case cType == "D" .and. (::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
      return [']+transform(DtoS(uData) ,'@R 9999/99/99')+[']
   Case cType == "D" .and. ::oSql:nSystemID == SYSTEMID_INGRES
      return ['] + SR_dtoDot(uData) + [']
   Case cType == "D" .and. ::oSql:nSystemID == SYSTEMID_CACHE
      return [{d ']+transform(DtoS(if(year(uData)<1850,stod("18500101"),uData)) ,'@R 9999-99-99')+['}]
   Case cType == "D"
      return ['] + dtos(uData) + [']
   case ctype == "T"  .and. ( ::oSql:nSystemID == SYSTEMID_POSTGR   .or.  ::oSql:nSystemID == SYSTEMID_MYSQL  .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
      if nLen == 4
         return ['] + HB_TSTOSTR( udata, .t.) + [']
      endif

      //return ['] + transform(ttos(uData), '@R 9999-99-99 99:99:99') + [']
      return ['] + HB_TSTOSTR( uData ) + [']
   case ctype == "T" .and.  ::oSql:nSystemID == SYSTEMID_ORACLE
      return [ TIMESTAMP '] + transform(ttos(uData), '@R 9999-99-99 99:99:99') + [']
   Case cType == "N" .and. nLen == NIL
      return ltrim(str(uData))
   Case cType == "N" .and. nLen != NIL
      return ltrim(str(uData, nLen, nDec))
   Case cType == "L" .and. ( ::oSql:nSystemID == SYSTEMID_POSTGR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
      return if(uData,"true","false")
   Case cType == "L" .and. ::oSql:nSystemID == SYSTEMID_INFORM
      return if(uData,"'t'","'f'")
   Case cType == "L"
      return if(uData,"1","0")
   OtherWise
      if HB_ISARRAY( uData )  .and. SR_SetSerializeArrayAsJson()
         cRet := hb_jsonencode(uData,.f.)
         return ::Quoted( cRet, trim, nLen, nDec, nTargetDB )
      ENDIF
      cRet := SR_STRTOHEX(HB_Serialize( uData ))
      return ::Quoted( SQL_SERIALIZED_SIGNATURE + str(len(cRet),10) + cRet, trim, nLen, nDec, nTargetDB )
   EndCase

Return ""

/*------------------------------------------------------------------------*/

METHOD QuotedNull( uData, trim, nLen, nDec, nTargetDB, lNull, lMemo )   CLASS SR_WORKAREA

   Local cType := valtype(uData), cRet
   lOCAL cOldSet := SET(_SET_DATEFORMAT)

   DEFAULT trim      := .F.
   DEFAULT nTargetDB := ::oSql:nSystemID
   DEFAULT lNull     := .T.
   DEFAULT lMemo     := .F.

   If empty( uData ) .and. (!cType $ "AOH") .and. ((nTargetDB = SYSTEMID_POSTGR .and. cType $ "DCMNT" .and. cType != "L"  ) .or. ( nTargetDB != SYSTEMID_POSTGR .and. cType != "L" ))


      If lNull
         Return 'NULL'
      Else
         Do Case
         Case cType $ "DT"
            Return 'NULL'
         Case cType $ "CM" .and. ::nTCCompat > 0
            Return "'" + uData + "'"
         Case cType $ "CM"
          if nTargetDB = SYSTEMID_POSTGR
             if SETPGSOLDBEHAVIOR()
                Return "''"
             else
                Return "' '"
             endif
          endif
            Return "' '"
         Case cType == "N"
            Return "0"
         EndCase
      EndIf

   EndIf

   If cType $ "CM" .and. nLen!=NIL
      If ::nTCCompat > 0
         trim := .F.
         uData := PadR( uData, nLen )
      Else
         uData := Left(uData, nLen)
      EndIf
   EndIf

   Do Case
   Case cType $ "CM" .and. nTargetDB == SYSTEMID_POSTGR .and. (!trim)
      return [E'] + SR_ESCAPESTRING(uData, nTargetDB) + [']

   Case cType $ "CM" .and. (!trim)
      return ['] + SR_ESCAPESTRING(uData, nTargetDB) + [']
   Case cType $ "CM" .and. nTargetDB == SYSTEMID_POSTGR .and. (trim)
      return [E'] + rtrim(SR_ESCAPESTRING(uData, nTargetDB)) + [']
   Case cType $ "CM" .and. trim
      return ['] + rtrim(SR_ESCAPESTRING(uData, nTargetDB)) + [']
   Case cType == "D" .and. nTargetDB == SYSTEMID_ORACLE .and. (!lMemo)
      return [TO_DATE('] + rtrim(DtoS(uData)) + [','YYYYMMDD')]
   Case cType == "D" .and. nTargetDB == SYSTEMID_INFORM .and. (!lMemo)
      return ['] + SR_dtoUS(uData) + [']
   Case cType == "D" .and. nTargetDB == SYSTEMID_SQLBAS .and. (!lMemo)
      return ['] + SR_dtosDot(uData) + [']
   Case cType == "D" .and. (nTargetDB == SYSTEMID_IBMDB2 .or. nTargetDB == SYSTEMID_ADABAS ) .and. (!lMemo)
      return [']+transform(DtoS(uData) ,'@R 9999-99-99')+[']
   Case cType == "D" .and. (nTargetDB == SYSTEMID_FIREBR .or. nTargetDB == SYSTEMID_FIREBR3)  .and. (!lMemo)
      return [']+transform(DtoS(uData) ,'@R 9999-99-99')+[']
   Case cType == "D" .and. nTargetDB == SYSTEMID_INGRES .and. (!lMemo)
      return ['] + SR_dtoDot(uData) + [']
   Case cType == "D" .and. nTargetDB == SYSTEMID_CACHE .and. (!lMemo)
      return [{d ']+transform(DtoS(if(year(uData)<1850,stod("18500101"),uData)) ,'@R 9999-99-99')+['}]
   Case cType == "D" .and. (!lMemo)
      return ['] + dtos(uData) + [']
   case ctype == "T"  .and. ( ::oSql:nSystemID == SYSTEMID_POSTGR   .or.  ::oSql:nSystemID == SYSTEMID_MYSQL  .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
      IF Empty( uData)
         RETURN 'NULL'
      ENDIF

      if nLen == 4
         return ['] + HB_TSTOSTR( udata, .t.) + [']
      endif
      //return ['] + transform(ttos(uData), '@R 9999-99-99 99:99:99') + [']
      return ['] + HB_TSTOSTR( uData ) + [']
   case ctype == "T" .and.  ::oSql:nSystemID == SYSTEMID_ORACLE
      IF Empty( uData)
         RETURN 'NULL'
      ENDIF

      return [ TIMESTAMP '] + transform(ttos(uData), '@R 9999-99-99 99:99:99') + [']
   Case cType == "N" .and. nLen != NIL .and. (!lMemo)
      return ltrim(str(uData, nLen+1, nDec))
   Case cType == "N" .and. (!lMemo)
      return ltrim(str(uData))
   Case cType == "L" .and.  (nTargetDB == SYSTEMID_POSTGR .or. nTargetDB == SYSTEMID_FIREBR3).and. (!lMemo)
      return if(uData,"true","false")
   Case cType == "L" .and. nTargetDB == SYSTEMID_INFORM
      return if(uData,"'t'","'f'")
   Case cType == "L" .and. (!lMemo)
      return if(uData,"1","0")
   Case cType == 'T'
      IF Empty( uData)
         RETURN 'NULL'
      ENDIF
      Set( _SET_DATEFORMAT,  "yyyy-mm-dd")
      cRet := ttoc( uData )
      Set( _SET_DATEFORMAT,cOldSet)
      RETURN [']+cRet+[']

   OtherWise
      if HB_ISARRAY( uData )  .and. SR_SetSerializeArrayAsJson()
         cRet := hb_jsonencode(uData,.f.)
         return ::Quoted( cRet, .f., , , nTargetDB )
      ENDIF

      cRet := SR_STRTOHEX( HB_Serialize( uData ) )
      return ::Quoted( SQL_SERIALIZED_SIGNATURE + str(len(cRet),10) + cRet, .f., , , nTargetDB )
   EndCase

Return ""

/*------------------------------------------------------------------------*/

METHOD HistExpression(n, cAlias)   CLASS SR_WORKAREA

   /*
   parameter n +- 0 -> (default) active record at current date
               |- 1 -> record last version
               +- 2 -> record first version
   */

   Local cRet := "", cAl1, cAl2

   If (!::lHistoric) .or. (!::lHistEnable)
      Return ""
   EndIf

   If ::nCnt == NIL
      ::nCnt := 1
   EndIf

   cAl1 := "W" + StrZero(++::nCnt,3)
   cAl2 := "W" + StrZero(++::nCnt,3)

   If ::nCnt >= 997
      ::nCnt := 1
   EndIf

   DEFAULT cAlias := "A"

   If lUseDTHISTAuto
      DEFAULT ::CurrDate := SR_GetActiveDt()
   Else
      ::CurrDate := SR_GetActiveDt()
   EndIf

   DEFAULT n := 0

   cRet += "(" + cAlias + ".DT__HIST = (SELECT" + if(n=3," MIN(", " MAX(") + cAl1 + ".DT__HIST) FROM "
   cRet += ::cQualifiedTableName + " " + cAl1 + " WHERE " + cAlias + "." + ::cColPK + "="
   cRet += cAl1 + "." + ::cColPK

   If n = 0
      cRet += " AND " + cAl1 + ".DT__HIST <= " + SR_cDBValue( ::CurrDate )
   endif

   cRet += "))"

Return cRet

/*------------------------------------------------------------------------*/

METHOD WriteBuffer( lInsert, aBuffer ) CLASS SR_WORKAREA

   Local cRet := "", cVal := "", lFirst := .T., cWh, aRet
   Local nLen, nDec, lNull, lMemo
   Local i, nInd, cKey, nRet
   Local nThisField, cIdent := "", lML, lMustUPD := .f.
   Local aMemos := {}, cMemo
   Local oXml

   DEFAULT lInsert := ::aInfo[ AINFO_ISINSERT ]
   DEFAULT aBuffer := ::aLocalBuffer

   aSize( ::aLocalBuffer, ::nFields )

   If (!lInsert) .and. ::aInfo[ AINFO_EOF ] .and. ::aInfo[ AINFO_BOF ]
      ::RunTimeErr("1")
      Return .F.
   EndIf

   If (!lInsert) .and. Empty(aBuffer[::hnRecno])
      Return .F.
   EndIf

   ::aInfo[ AINFO_ISINSERT ] := .F.
   ::aInfo[ AINFO_HOT ]      := .F.
   ::aInfo[ AINFO_EOF_AT ]   := 0
   ::aInfo[ AINFO_BOF_AT ]   := 0

   If ::lHistoric .and. (!empty( aBuffer[::nPosDtHist] )) .and. (!empty( aBuffer[::hnRecno] ) )
      aRet := {}

      If lUseDTHISTAuto
         ::oSql:exec( "SELECT " + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " FROM " + ::cQualifiedTableName + " WHERE " +;
                      ::cColPK + " = " + SR_cDbValue( aBuffer[::nPosColPK ] ) + " AND DT__HIST = " + SR_cDbValue( aBuffer[::nPosDtHist] ),;
                            .F., .T., @aRet )
      Else
         ::oSql:exec( "SELECT " + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " FROM " + ::cQualifiedTableName + " WHERE " +;
                      ::cColPK + " = " + SR_cDbValue( aBuffer[::nPosColPK ] ) + " AND DT__HIST = " + SR_cDbValue( SR_GetActiveDt() ),;
                            .F., .T., @aRet )
      EndIf

      If len( aRet ) > 0
         lInsert  := .F.
         lMustUPD := .T.
      EndIf
   EndIf

   If ::lHistoric .and. !lMustUPD

      If empty( aBuffer[::nPosDtHist] )
         aBuffer[::nPosDtHist] := SR_GetActiveDt()
      EndIf
      If !lInsert
         If ::lVers
            lInsert := .t.
            If ((!::oSql:lUseSequences) .or. (!::lUseSequences)) .or. (::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_INGRES)
               ::aInfo[ AINFO_RECNO ] := 0
               aBuffer[::hnRecno]     := 0               // This forces NEW recno number
            EndIf
            If ::dNextDt == NIL
               aBuffer[::nPosDtHist] := SR_GetActiveDt()
            Else
               aBuffer[::nPosDtHist] := ::dNextDt
            EndIf
         EndIf
      EndIf
   EndIf

   Do Case
   Case !lInsert
      If ::lCanUpd
         For nThisField = 1 to ::nFields
            If SubStr(::aNames[nThisField],1,7) == "INDKEY_" .or.;
               SubStr(::aNames[nThisField],1,7) == "INDFOR_" .or.;
               aBuffer[nThisField] == NIL
               Loop
            EndIf

            nLen  := ::aFields[nThisField, FIELD_LEN]
            nDec  := ::aFields[nThisField, FIELD_DEC]
            lNull := ::aFields[nThisField, FIELD_NULLABLE]
            lMemo := ::aFields[nThisField, FIELD_TYPE] = "M"
            lML   := ::aFields[nThisField, FIELD_MULTILANG]
            if lMemo .and. ::aFields[nThisField,6] ==SQL_LONGVARCHARXML
               lMemo := .F.
            endif
            If lML .or.;
               (::aOldBuffer[nThisField] == NIL ) .or.;
               ( lMemo .and. (valtype(::aOldBuffer[nThisField]) != "C" .or. valtype( aBuffer[nThisField] ) != "C" )) .or.;
               ( (!::aOldBuffer[nThisField] == aBuffer[nThisField]) .and. ( nThisField != ::hnRecno ) )

               If lML .and. valtype( aBuffer[nThisField] ) $ "CM"
                  aBuffer[nThisField] := Hash( SR_SetBaseLang(), aBuffer[nThisField] )
               EndIf
               If (lMemo .or. lML) .and. (::oSql:nSystemID == SYSTEMID_ORACLE .or. ::oSql:nSystemID == SYSTEMID_ADABAS  .or. ::oSql:nSystemID == SYSTEMID_IBMDB2 ) .and. ::aFields[nThisField,6] != SQL_FAKE_LOB  // .or. ::oSql:nSystemID == SYSTEMID_CACHE
                  If !valtype( aBuffer[nThisField] ) $ "CM"
                     cMemo := SR_STRTOHEX( HB_Serialize( aBuffer[nThisField] ) )
                     cMemo := SQL_SERIALIZED_SIGNATURE + str(len(cMemo),10) + cMemo
                  Else
                     cMemo := aBuffer[nThisField]
                  EndIf
                  aadd( aMemos, { ::aNames[nThisField], cMemo } )
                  loop
#ifdef SQLRDD_TOPCONN
               ElseIf ::aFields[nThisField,6] == SQL_FAKE_DATE
                  cRet += if(!lFirst,", ","") + SR_DBQUALIFY( ::aNames[nThisField], ::oSql:nSystemID ) + " = '" + dtos(aBuffer[nThisField]) + "' "
               ElseIf ::aFields[nThisField,6] == SQL_FAKE_NUM
                  cRet += if(!lFirst,", ","") + SR_DBQUALIFY( ::aNames[nThisField], ::oSql:nSystemID ) + " = " + str(aBuffer[nThisField], nLen, nDec ) + " "
#endif
               ElseIf ::aFields[nThisField,6] != SQL_GUID
                  cRet += if(!lFirst,", ","") + SR_DBQUALIFY( ::aNames[nThisField], ::oSql:nSystemID ) + " = " + ::QuotedNull(aBuffer[nThisField],.t.,If(lMemo, NIL, nLen),nDec,,lNull,lMemo) + " "
               ElseIf ::aFields[nThisField,6] ==SQL_LONGVARCHARXML
                  oXml := sr_arraytoXml( aBuffer[nThisField] )
                  nlen:=len(oxml:tostring(HBXML_STYLE_NONEWLINE))
                  cVal := if(!lFirst,", ","") + SR_DBQUALIFY( ::aNames[nThisField], ::oSql:nSystemID ) + " = " + ::QuotedNull(oxml:tostring(HBXML_STYLE_NONEWLINE),.t.,If(lMemo, NIL, nLen),nDec,,lNull,lMemo)
               elseif ::aFields[nthisField,6] == SQL_VARBINARY .and. ::osql:nsystemID ==SYSTEMID_MSSQL7
                  cVal := '0x'+StrtoHex(aBuffer[nThisField])
               Else
                  Loop
               EndIf
               lFirst := .F.
               ::aOldBuffer[nThisField] := aBuffer[nThisField]
            endif
         Next

         If !lFirst            && Smth has been updated

            /* Write the index columns */

            If !lFirst
               For nInd = 1 to len( ::aIndexMgmnt )
                  If !Empty( ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] )
                     cKey := (::cAlias)->( SR_ESCAPESTRING( eval(::aIndexMgmnt[nInd, INDEXMAN_KEY_CODEBLOCK]), ::oSql:nSystemID ) )
                     if ::osql:nsystemID ==SYSTEMID_POSTGR
                     cRet += ", " + SR_DBQUALIFY( "INDKEY_" + ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS], ::oSql:nSystemID ) + " = E'" + cKey + "' "
                     else
                     cRet += ", " + SR_DBQUALIFY( "INDKEY_" + ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS], ::oSql:nSystemID ) + " = '" + cKey + "' "
                     endif
                     ::aLocalBuffer[ ::aIndexMgmnt[nInd, INDEXMAN_SYNTH_COLPOS] ] := cKey
                  EndIf
                  If !Empty( ::aIndexMgmnt[nInd, INDEXMAN_FOR_CODEBLOCK] )
                     cKey := (::cAlias)->( eval(::aIndexMgmnt[nInd, INDEXMAN_FOR_CODEBLOCK]) )
                     if ::osql:nsystemID ==SYSTEMID_POSTGR
                     cRet += ", " + SR_DBQUALIFY( "INDFOR_" + SubStr(::aIndexMgmnt[nInd, INDEXMAN_FOR_EXPRESS],2,3), ::oSql:nSystemID ) + " = E'" + cKey + "' "
                     else
                     cRet += ", " + SR_DBQUALIFY( "INDFOR_" + SubStr(::aIndexMgmnt[nInd, INDEXMAN_FOR_EXPRESS],2,3), ::oSql:nSystemID ) + " = '" + cKey + "' "
                     endif
                     ::aLocalBuffer[ ::aIndexMgmnt[nInd, INDEXMAN_FOR_COLPOS] ] := cKey
                  EndIf
               Next
            EndIf

            cWh := ::WhereEqual()

            If empty( cWh )
               ::dNextDt := NIL
               ::RuntimeErr( "4" )
               Return .F.
            endif

            If  (::oSql:Execute( ::cUpd + cRet + cWh, , ::nLogMode  ) ) != SQL_SUCCESS
               ::RuntimeErr( "16", SR_Msg(16) + ::oSql:LastError() + chr(13)+chr(10)+ chr(13)+chr(10)+;
                        SR_Msg(14) + chr(13)+chr(10) + ::oSql:cLastComm )
               ::dNextDt := NIL
               Return .F.
            EndIf

            ::oSql:FreeStatement()

         EndIf

         // Write memo fields

         If len( aMemos ) > 0
            ::oSql:WriteMemo( ::cQualifiedTableName, aBuffer[ ::hnRecno ], SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ), aMemos )
         EndIf

         If ::aInfo[ AINFO_NCACHEBEGIN ] == 0 .and. ::aInfo[ AINFO_NCACHEEND ] == 0
            ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 1
            ::aInfo[ AINFO_NPOSCACHE ] := 1
         EndIf

         aSize( ::aCache[ ::aInfo[ AINFO_NPOSCACHE ] ], ::nFields )
         aCopy( ::aLocalBuffer, ::aCache[ ::aInfo[ AINFO_NPOSCACHE ] ] )

         /* Sync with other workareas opening the same table */

         _SR_ScanExec( Self, { |x| If( x:nThisArea != ::nThisArea, ::CheckCache(x),)} )

      Else
         ::RunTimeErr("5", SR_Msg(5) + ::cFileName )
      EndIf

   Case lInsert

      If ::lCanIns

         For i = 1 to ::nFields
            If SubStr( ::aNames[i], 1, 7 ) == "INDKEY_" .or. SubStr( ::aNames[i], 1, 7 ) == "INDFOR_"
               Loop
            EndIf

            nLen  := ::aFields[i,3]
            nDec  := ::aFields[i,4]
            lNull := ::aFields[i,5]
            lMemo := ::aFields[i,FIELD_TYPE] = "M"
            lML   := ::aFields[i,FIELD_MULTILANG]
            if lMemo .and. ::aFields[i,6] ==SQL_LONGVARCHARXML
               lMemo := .F.
            endif

            If lML .and. valtype( aBuffer[i] ) $ "CM"
               aBuffer[i] := { SR_SetBaseLang() => aBuffer[i] }
            EndIf

            If aBuffer[i] == NIL
               aBuffer[i] := ::aEmptyBuffer[i]
            EndIf

            If i == ::hnRecno
               If ((!::oSql:lUseSequences) .or. (!::lUseSequences))
                  If Empty( ::aInfo[ AINFO_RECNO ] )
                     aBuffer[::hnRecno]     := ::GetNextRecordNumber()
                     ::aInfo[ AINFO_RECNO ] := aBuffer[::hnRecno]
                  Else
                     aBuffer[::hnRecno] := ::aInfo[ AINFO_RECNO ]
                  EndIf
                  If ::cIns == NIL
                     cRet += if(!lFirst,", ","( ") + SR_DBQUALIFY( ::aNames[i], ::oSql:nSystemID )
                  EndIf
                  cVal += if(!lFirst,", ","( ") + ::QuotedNull(aBuffer[i],.t.,NIL,nDec,,lNull,lMemo)
                  lFirst := .F.
               ElseIf (!::lQuickAppend) .or. ::oSql:nSystemID == SYSTEMID_INGRES
                  Switch ::oSql:nSystemID
                  Case SYSTEMID_INGRES
                  Case SYSTEMID_FIREBR
                     If Empty( ::aInfo[ AINFO_RECNO ] )
                        aBuffer[::hnRecno] := ::GetNextRecordNumber()
                        ::aInfo[ AINFO_RECNO ] := aBuffer[::hnRecno]
                     Else
                        aBuffer[::hnRecno] := ::aInfo[ AINFO_RECNO ]
                     EndIf
                     If ::cIns == NIL
                        cRet += if(!lFirst,", ","( ") + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )
                     EndIf
                     cVal += if(!lFirst,", ","( ") + ltrim( Str( aBuffer[::hnRecno], 15 ) )
                     lFirst := .F.
                     Exit
                  Case SYSTEMID_INFORM
                  Case SYSTEMID_ORACLE
                  Case SYSTEMID_MSSQL7
                  Case SYSTEMID_POSTGR
                  Case SYSTEMID_MSSQL6
                  Case SYSTEMID_SYBASE
                  Case SYSTEMID_IBMDB2    // Use IDENTITY column (or similar)
                  Case SYSTEMID_AZURE
                     Exit
                  End
               EndIf
            Else
               If (lMemo .or. lML)
                  If !(valtype( aBuffer[i] ) $ "CM")
                     cMemo := SR_STRTOHEX( HB_Serialize( aBuffer[i] ) )
                     cMemo := SQL_SERIALIZED_SIGNATURE + str(len(cMemo),10) + cMemo
                  Else
                     cMemo := aBuffer[i]
                  EndIf

                  If (::oSql:nSystemID == SYSTEMID_ORACLE) .and. len( cMemo ) > 2000  //.or. ::oSql:nSystemID == SYSTEMID_CACHE
                     aadd( aMemos, { ::aNames[i], cMemo } )
                     cMemo := NIL
                  ElseIf (::oSql:nSystemID == SYSTEMID_IBMDB2) .and. len( cMemo ) > 32000
                     aadd( aMemos, { ::aNames[i], cMemo } )
                     cMemo := NIL
                  ElseIf ::oSql:nSystemID == SYSTEMID_ADABAS //.or. ::oSql:nSystemID == SYSTEMID_MSSQL7      // ADABAS always need binding
                     aadd( aMemos, { ::aNames[i], cMemo } )
                     Loop
                  Else
                     cMemo := aBuffer[i]
                  EndIf
               Else
                  cMemo := aBuffer[i]
               EndIf

               If ::cIns == NIL
                  cRet += if(!lFirst,", ","( ") + SR_DBQUALIFY( ::aNames[i], ::oSql:nSystemID )
               EndIf

               Switch ::aFields[i,6]
               Case SQL_GUID
                  cVal += if(!lFirst,", ","( ") + " NEWID() "
                  Exit
#ifdef SQLRDD_TOPCONN
               Case SQL_FAKE_DATE
                  cVal += if(!lFirst,", ","( ") + "'" + dtos(cMemo) + "' "
                  Exit
               Case SQL_FAKE_NUM
                  cVal += if(!lFirst,", ","( ") + str(cMemo, nLen, nDec ) + " "
                  Exit
#endif
               CASE SQL_LONGVARCHARXML
                  oXml := sr_arraytoXml( cMemo )

                  nlen:=len(oxml:tostring(HBXML_STYLE_NONEWLINE))
                  cVal += if(!lFirst,", ","( ") + ::QuotedNull(oXml:tostring(HBXML_STYLE_NONEWLINE),.t.,If(lMemo, NIL, nLen),nDec,,lNull,lMemo)
                  exit
               Case SQL_VARBINARY
                  if ::osql:nsystemID ==SYSTEMID_MSSQL7
                     cVal += if(!lFirst,", ","( ")+ '0x'+StrtoHex(cmemo)
                  else
                     cVal += if(!lFirst,", ","( ") + ::QuotedNull(cMemo,.t.,If(lMemo, NIL, nLen),nDec,,lNull,lMemo)
                  endif
                  exit
               Default
                  cVal += if(!lFirst,", ","( ") + ::QuotedNull(cMemo,.t.,If(lMemo, NIL, nLen),nDec,,lNull,lMemo)
               End
               lFirst := .F.
            EndIf
         Next

         /* Write the index columns */

         For nInd = 1 to len( ::aIndexMgmnt )
            If !Empty( ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] )
               If ::cIns == NIL
                  cRet += if(!lFirst,", ","( ") + SR_DBQUALIFY( "INDKEY_" + ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS], ::oSql:nSystemID )
               EndIf
               cKey := (::cAlias)->( SR_ESCAPESTRING(eval(::aIndexMgmnt[nInd, INDEXMAN_KEY_CODEBLOCK]), ::oSql:nSystemID ) )
               if ::osql:nsystemID ==SYSTEMID_POSTGR
                  cVal += if(!lFirst,", E'","( E'") + cKey + "'"
               else
                  cVal += if(!lFirst,", '","( '") + cKey + "'"
               endif
               ::aLocalBuffer[ ::aIndexMgmnt[nInd, INDEXMAN_SYNTH_COLPOS] ] := cKey
            EndIf
            If !Empty( ::aIndexMgmnt[nInd, INDEXMAN_FOR_CODEBLOCK] )
               If ::cIns == NIL
                  cRet += if(!lFirst,", ","( ") + SR_DBQUALIFY( "INDFOR_" + SubStr(::aIndexMgmnt[nInd, INDEXMAN_FOR_EXPRESS],2,3), ::oSql:nSystemID )
               EndIf
               cKey := (::cAlias)->( eval(::aIndexMgmnt[nInd, INDEXMAN_FOR_CODEBLOCK]) )
               if ::osql:nsystemID ==SYSTEMID_POSTGR
                  cVal += if(!lFirst,", E'","( E'") + cKey + "'"
               else
                  cVal += if(!lFirst,", '","( '") + cKey + "'"
               endif
               ::aLocalBuffer[ ::aIndexMgmnt[nInd, INDEXMAN_FOR_COLPOS] ] := cKey
            EndIf
         Next

         Switch ::oSql:nSystemID
         Case SYSTEMID_MSSQL7
         Case SYSTEMID_AZURE
            If ::oSql:lUseSequences .and. ::lUseSequences
               //cIdent := "; SELECT IDENT_CURRENT('"+ ::cfilename+"');"
               cIdent := "; SELECT " + ::cRecnoName + " FROM @InsertedData;"
            EndIf
            Exit
         Case SYSTEMID_IBMDB2
            If ::oSql:lUseSequences .and. ::lUseSequences .and.(!"DB2/400" $ ::oSql:cSystemName)
               cIdent := "; VALUES IDENTITY_VAL_LOCAL();"
            EndIf
            Exit
         case SYSTEMID_FIREBR3
            If ::oSql:lUseSequences .and. ::lUseSequences
               cIdent := "  RETURNING  " + ::cRecnoName
            EndIf
            Exit
         End

         aRet := {}

         If len( ::aOldBuffer ) == 0
            aSize( ::aOldBuffer, len( ::aLocalBuffer ) )
         EndIf
         aCopy( aBuffer, ::aOldBuffer )

         If ::cIns == NIL
             if ::oSql:nSystemID == SYSTEMID_MSSQL7
               ::cIns    := "Declare @InsertedData table (" + ::cRecnoName+" numeric(15,0) );INSERT INTO " + ::cQualifiedTableName +" " + cRet + " ) OUTPUT Inserted."+ ::cRecnoName + " INTO @InsertedData VALUES "
            else
               ::cIns    := "INSERT INTO " + ::cQualifiedTableName + " " + cRet + " ) VALUES "
            ENDIF
         EndIf

         If  ::oSql:Execute( ::cIns + cVal + " ) " + cIdent, , ::nLogMode ) != SQL_SUCCESS
            ::dNextDt := NIL
            Return .F.
         EndIf



         /* If using sequences, try to find the record number */

         If ::oSql:lUseSequences .and. ::lUseSequences .and. !::lQuickAppend

            Switch ::oSql:nSystemID
            Case SYSTEMID_FIREBR3
               ::oSql:MoreResults( @aRet )
               If len(aRet) > 0
                  aBuffer[::hnRecno] := aRet[1,1]
               Else
                  ::RunTimeErr("11", SR_Msg(11) + ::cFileName + " SQL Statement: " + ::cIns + cVal + " ) " + cIdent  + " " + ::oSql:LastError() )
                  ::dNextDt := NIL
                  Return .F.
               EndIf
            exit
            Case SYSTEMID_MSSQL7
            Case SYSTEMID_AZURE

               ::oSql:MoreResults( @aRet )
               ::oSql:MoreResults( @aRet )
               If len(aRet) > 0
                  aBuffer[::hnRecno] := aRet[1,1]
               Else
                  ::RunTimeErr("11", SR_Msg(11) + ::cFileName + " SQL Statement: " + ::cIns + cVal + " ) " + cIdent  + " " + ::oSql:LastError() )
                  ::dNextDt := NIL
                  Return .F.
               EndIf
               ::oSql:FreeStatement()
               Exit
            Case SYSTEMID_IBMDB2
               If "DB2/400" $ ::oSql:cSystemName
                  ::oSql:FreeStatement()
                  aRet := {}
                  ::oSql:exec( "SELECT IDENTITY_VAL_LOCAL() AS RECORD FROM " + ::cOwner + ::cFileName + " WHERE " + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " = IDENTITY_VAL_LOCAL()" ,.F.,.T.,@aRet )
                  If len(aRet) > 0
                     aBuffer[::hnRecno] := aRet[1,1]
                  Else
                     ::RunTimeErr( "11", SR_Msg(11) + ::cFileName )
                     ::dNextDt := NIL
                     Return .F.
                  EndIf
               Else
                  ::oSql:MoreResults( @aRet )
                  ::oSql:MoreResults( @aRet )
                  If len(aRet) > 0
                     aBuffer[::hnRecno] := aRet[1,1]
                  Else
                     ::RunTimeErr("11", SR_Msg(11) + ::cFileName )
                     ::dNextDt := NIL
                     Return .F.
                  EndIf
                  ::oSql:FreeStatement()
                  Exit
               EndIf
               Exit



            Case SYSTEMID_ORACLE
               ::oSql:FreeStatement()
               aRet := {}
               Switch ::nSequencePerTable
               Case SEQ_PER_TABLE
                  ::oSql:exec( "SELECT " + ::cOwner + LimitLen(::cFileName,3) + "_SQ.CURRVAL FROM DUAL",.T.,.T.,@aRet )
                  If len(aRet) > 0
                     aBuffer[::hnRecno] := aRet[1,1]
                  EndIf
                  Exit
               Case SEQ_NOTDEFINED
                  ::oSql:exec( "SELECT " + ::cOwner + LimitLen(::cFileName,3) + "_SQ.CURRVAL FROM DUAL",.F.,.T.,@aRet )
                  If len(aRet) > 0
                     ::nSequencePerTable := SEQ_PER_TABLE
                     aBuffer[::hnRecno] := aRet[1,1]
                  Else
                     ::nSequencePerTable := SEQ_PER_DATABASE
                     aRet := {}
                     ::oSql:exec( "SELECT " + ::cOwner + "SQ_NRECNO.CURRVAL FROM DUAL",.F.,.T.,@aRet )
                     If len(aRet) > 0
                        aBuffer[::hnRecno] := aRet[1,1]
                     EndIf
                  EndIf
                  Exit
               Case SEQ_PER_DATABASE
                  ::oSql:exec( "SELECT SQ_NRECNO.CURRVAL FROM DUAL",.F.,.T.,@aRet )
                  If len(aRet) > 0
                     aBuffer[::hnRecno] := aRet[1,1]
                  EndIf
                  Exit
               End
               Exit
            Case SYSTEMID_POSTGR
               ::oSql:FreeStatement()
               aRet := {}
               Switch ::nSequencePerTable
               Case SEQ_PER_TABLE
                  ::oSql:exec( "SELECT currval('" + ::cOwner + LimitLen(::cFileName,3) + "_SQ')",.F.,.T.,@aRet )
                  If len(aRet) > 0
                     aBuffer[::hnRecno] := aRet[1,1]
                  EndIf
                  Exit
               Case SEQ_NOTDEFINED
                  ::oSql:exec( "SELECT currval('" + ::cOwner + LimitLen(::cFileName,3) + "_SQ')",.F.,.T.,@aRet )
                  If len(aRet) > 0
                     ::nSequencePerTable := SEQ_PER_TABLE
                     aBuffer[::hnRecno] := aRet[1,1]
                  Else
                     ::oSql:Commit()
                     ::nSequencePerTable := SEQ_PER_DATABASE
                     aRet := {}
                     ::oSql:exec( "SELECT currval('SQ_NRECNO')",.F.,.T.,@aRet )
                     If len(aRet) > 0
                        aBuffer[::hnRecno] := aRet[1,1]
                     EndIf
                  EndIf
                  Exit
               Case SEQ_PER_DATABASE
                  ::oSql:exec( "SELECT currval('SQ_NRECNO')",.F.,.T.,@aRet )
                  If len(aRet) > 0
                     aBuffer[::hnRecno] := aRet[1,1]
                  EndIf
                  Exit
               End
               Exit
            Case SYSTEMID_MYSQL
            Case SYSTEMID_MARIADB
               ::oSql:FreeStatement()
               aRet := {}
               ::oSql:exec( "SELECT LAST_INSERT_ID()",.F.,.T.,@aRet )
               If len(aRet) > 0
                  aBuffer[::hnRecno] := aRet[1,1]
               Else
                  ::RunTimeErr("11", SR_Msg(11) + ::cFileName )
                  ::dNextDt := NIL
                  Return .F.
               EndIf
               Exit

            Case SYSTEMID_INFORM
               ::oSql:FreeStatement()
               aRet := {}
               ::oSql:exec( "select first 1 dbinfo('sqlca.sqlerrd1') from systables" ,.F.,.T.,@aRet )
               If len(aRet) > 0
                  aBuffer[::hnRecno] := aRet[1,1]
               Else
                  ::RunTimeErr( "11", SR_Msg(11) + ::cFileName )
                  ::dNextDt := NIL
                  Return .F.
               EndIf
            End
         EndIf

         // Write memo fields
         If len( aMemos ) > 0
            If (nRet := ::oSql:WriteMemo( ::cQualifiedTableName, aBuffer[ ::hnRecno ], SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ), aMemos )) != 0
               ::RunTimeErr("10", "Error writing LOB info in table " + ::cFileName + ":" + ltrim(str(nRet)) + " " + ::oSql:LastError() )
            EndIf
         EndIf

         /* Record should remain Locked */

         If ::aInfo[ AINFO_SHARED ] .and. len( ::aLocked ) < MAXIMUN_LOCKS
            aadd( ::aLocked, aBuffer[ ::hnRecno ] )
         EndIf

         /* Sets info array */

         ::lEmptyTable := .F.
         ::aInfo[ AINFO_RCOUNT ] := aBuffer[ ::hnRecno ]
         ::nLastRecordAded       := aBuffer[ ::hnRecno ]

         /* Cache insertion is disabled because INSERTED lines usually
         are discarded by cache engine */

         ::aInfo[ AINFO_BOF ]    := .F.
         ::aInfo[ AINFO_EOF ]    := .F.
         ::aInfo[ AINFO_BOF_AT ]    := 0
         ::aInfo[ AINFO_EOF_AT ]    := 0

         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 1
         ::aInfo[ AINFO_NPOSCACHE ] := 1
         If ::aCache[1] == NIL
            ::aCache[1] := Array( len( ::aLocalBuffer ) )
         EndIf

         aSize( ::aCache[1], ::nFields )
         aCopy( ::aLocalBuffer, ::aCache[1] )

         _SR_ScanExec( Self, { |x| If( x:nThisArea != ::nThisArea, ::CheckCache(x),NIL)} )

      Else
         SR_MsgLogFile( SR_Msg(3) + ::cFileName )
      EndIf

   endCase

   ::aInfo[ AINFO_RECNO ]  := aBuffer[::hnRecno]
   ::aInfo[ AINFO_BOF ]    := .F.
   ::aInfo[ AINFO_EOF ]    := .F.
   ::lNoData    := .F.
   ::dNextDt    := NIL

   If ::lCollectingBehavior
      For each i in ::aSelectList
         If i == 1
            ::lCollectingBehavior   := .F.
            Exit
         EndIf
      Next
   EndIf

Return .T.

/*------------------------------------------------------------------------*/

METHOD lCanICommitNow() CLASS SR_WORKAREA

Return ::oSql:nTransacCount == 0 .and. ::aInfo[ AINFO_SHARED ] .and. Empty( ::aLocked )

/*------------------------------------------------------------------------*/

METHOD UpdateCache( aResultSet ) CLASS SR_WORKAREA

   Local uRecord, uVal

   If ::hnRecno == NIL .or. ::hnRecno <= 0 .and. len( aResultSet ) > 0
      Return NIL
   EndIf

   uRecord := aResultSet[ 1, ::hnRecno ]

   If SR_SetMultiLang()
      If ::aInfo[ AINFO_RECNO ] == uRecord
         For each uVal in aResultSet[1]
            If valtype( ::aLocalBuffer[ hb_enumIndex() ] ) == "H"
               If ::aFields[hb_enumIndex(), FIELD_TYPE] $ "CM"
                  (::aLocalBuffer[ hb_enumIndex() ])[SR_SetBaseLang()] := PadR( uVal, ::aFields[hb_enumIndex(), FIELD_LEN] )
               Else
                  (::aLocalBuffer[ hb_enumIndex() ])[SR_SetBaseLang()] := uVal
               EndIf
               ::aOldBuffer[ hb_enumIndex() ] := ::aLocalBuffer[ hb_enumIndex() ]
            Else
               ::aLocalBuffer[ hb_enumIndex() ] := uVal
               ::aOldBuffer[ hb_enumIndex() ]   := uVal
            EndIf
         Next
      EndIf
   Else
      If ::aInfo[ AINFO_RECNO ] == uRecord
         For each uVal in aResultSet[1]
            ::aLocalBuffer[ hb_enumIndex() ] := uVal
            ::aOldBuffer[ hb_enumIndex() ]   := uVal
         Next
      EndIf
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD Default() CLASS SR_WORKAREA

   If len( ::aCache ) == 0
      ::aInfo[ AINFO_BOF ] := .T.
      ::aInfo[ AINFO_RCOUNT ] := 0
      ::aInfo[ AINFO_RECNO ]  := 1
      ::lNoData    := .T.
      ::GetBuffer(.T.)
      ::aInfo[ AINFO_NPOSCACHE ]  := 1
   Else

      ::aInfo[ AINFO_BOF ] := .F.
      ::aInfo[ AINFO_EOF ] := .F.
      ::aInfo[ AINFO_RCOUNT ] := ::nLastRec
      ::lNoData    := .F.
      ::GetBuffer(.F.)                        /* Use the first record */
      if ::hnRecno != NIL .and. ::hnRecno != 0
         ::aInfo[ AINFO_RECNO ]  := ::aLocalBuffer[::hnRecno]
      else
         ::aInfo[ AINFO_RECNO ]  :=  ::aInfo[ AINFO_NPOSCACHE ]
      endif
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD SolveSQLFilters( cAliasSQL ) CLASS SR_WORKAREA

   Local cRet := "", cFlt

   DEFAULT cAliasSQL := ""

   If !Empty( cAliasSQL ) .and. cAliasSQL[-1] != "."
      cAliasSQL += "."
   EndIf
   For each cFlt in ::aFilters
      cFlt := StrTran( cFlt, "<ALIAS>.", cAliasSQL )
      Begin Sequence
         If empty(cRet)
            If SR_EvalFilters()
               cRet := &cFlt
            Else
               cRet := cFlt
            EndIf
         Else
            If SR_EvalFilters()
               cRet += " AND " + &cFlt
            Else
               cRet += " AND " + cFlt
            EndIf
         EndIf
      Recover
         ::RunTimeErr("10", SR_Msg(10) + ::cFileName + " - " + cFlt )
      End
   Next
Return cRet

/*------------------------------------------------------------------------*/

METHOD Refresh( lGoCold ) CLASS SR_WORKAREA

   Local i, nMax := 0
   Local nAllocated, n, lRecnoAdded := .F.

   DEFAULT lGoCold := .T.

   If lGoCold
      ::sqlGoCold()     /* writes any change in the buffer to the database */
   Else
      ::aInfo[ AINFO_HOT ] := .F.
      ::GetBuffer(.T.)         // Clean Buffer
   EndIf

   If ::lISAM

      ::aInfo[ AINFO_BOF_AT ] := 0
      ::aInfo[ AINFO_EOF_AT ] := 0

      If !( ::aInfo[ AINFO_EOF ] .and. ::aInfo[ AINFO_BOF ] )
         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 1
         ::aInfo[ AINFO_NPOSCACHE ] := 1
         aCopy( ::aLocalBuffer, ::aCache[1] )
      Else
         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
         ::aInfo[ AINFO_NPOSCACHE ] := 0
      EndIf

      For i = 1 to len( ::aIndex )
         ::aIndex[i, ORDER_SKIP_UP ]   := NIL
         ::aIndex[i, ORDER_SKIP_DOWN ] := NIL
      Next

   Else

      ::IniFields(.T., .T.)

      /* Load the cache to ::aCache */

      aSize( ::aCache, ARRAY_BLOCK2 )
      nAllocated := ARRAY_BLOCK2
      n          := 0

      If ::hnRecno == NIL
         ::hnRecno := ::nFields + 1
         ::nFields ++
         ::lCanUpd := .F.
         ::lCanIns := .F.
         ::lCanDel := .F.
         lRecnoAdded := .T.
         aadd( ::aNames, ::cRecnoName )
         aadd( ::aFields, { ::cRecnoName, "N", 15, 0 } )
         aadd( ::aEmptyBuffer, 0 )
      EndIf

      While (::oSql:Fetch( , .F., ::aFields )) = SQL_SUCCESS
         n ++
         If n > nAllocated
            Switch nAllocated
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

            aSize( ::aCache, nAllocated )
         EndIf

         ::aCache[n] := Array(::nFields)

         For i = 1 to ::nFields
            If lRecnoAdded .and. i == ::nFields
               ::aCache[n,i] := n
               nMax := n
            Else
               ::aCache[n,i] := ::oSql:FieldGet( i, ::aFields, .F. )
            EndIf
            If (!lRecnoAdded) .and. i == ::hnRecno
               nMax := Max( nMax, ::aCache[n,i] )
            EndIf
         Next
      EndDo

      aSize( ::aCache, n )
      ::nLastRec  := nMax
      ::aInfo[ AINFO_NPOSCACHE ] := min( ::aInfo[ AINFO_NPOSCACHE ], len( ::aCache ) )
      ::aInfo[ AINFO_FCOUNT ] := ::nFields
      ::aInfo[ AINFO_FOUND ]  := .F.
      ::aInfo[ AINFO_RCOUNT ] := nMax

      ::Default()

   EndIf

   ::LineCount()
   ::nLastRefresh := Seconds()

Return NIL

/*------------------------------------------------------------------------*/

METHOD GetBuffer( lClean, nCache ) CLASS SR_WORKAREA

   DEFAULT lClean := .F.
   DEFAULT nCache := ::aInfo[ AINFO_NPOSCACHE ]

   If len( ::aLocalBuffer ) < ::nFields
      aSize( ::aLocalBuffer, ::nFields )
   EndIf

   If (!lClean) .and. (nCache = 0 .or. len( ::aCache ) = 0 .or. nCache > len( ::aCache ) .or. len( ::aCache[nCache] ) < ::nFields)
      lClean := .T.
   EndIf

   If !lClean
      If ::lISAM
         aCopy( ::aCache[nCache], ::aLocalBuffer )
         ::aInfo[ AINFO_RECNO ] := ::aCache[ nCache, ::hnRecno ]
      Else
         aCopy( ::aCache[ nCache ], ::aLocalBuffer )
         if ::hnRecno != NIL .and. ::hnRecno != 0
            ::aInfo[ AINFO_RECNO ] := ::aCache[ nCache, ::hnRecno ]
         else
            ::aInfo[ AINFO_RECNO ] := nCache
         endif
         ::aInfo[ AINFO_NPOSCACHE ] := nCache
      EndIf
      If ::hnDeleted > 0 .and. ::aLocalBuffer[ ::hnDeleted ] != NIL
         ::aInfo[ AINFO_DELETED ] := ::aLocalBuffer[ ::hnDeleted ] $ "T*"
      Else
         ::aInfo[ AINFO_DELETED ] := .F.
      EndIf
   else

      aEval( ::aLocalBuffer, { |x,i| (x),::aLocalBuffer[i] := ::aEmptyBuffer[i] } )
      If !::lISAM
         ::aInfo[ AINFO_NPOSCACHE ] := len( ::aCache ) + 1
      EndIf
      ::aInfo[ AINFO_DELETED ] := .F.
      ::aInfo[ AINFO_RECNO ]   := ::aInfo[ AINFO_RCOUNT ] + 1
      ::aInfo[ AINFO_EOF ]     := .T.
   endIf

   /* Take a picture of the buffer */

   If len( ::aOldBuffer ) == 0
      aSize( ::aOldBuffer, len( ::aLocalBuffer ) )
   EndIf

   aCopy( ::aLocalBuffer, ::aOldBuffer )

Return ::aLocalBuffer

/*------------------------------------------------------------------------*/

METHOD IniFields(lReSelect, lLoadCache, aInfo) CLASS SR_WORKAREA

   Local cName, n, cWhere, lHaveInfoCache, aML
   Local aFlds := {}
#ifdef SQLRDD_TOPCONN
   Local nPos
#endif

   DEFAULT lLoadCache := .F.

   lHaveInfoCache := aInfo != NIL .and. aInfo[ CACHEINFO_AFIELDS ] != NIL

   If !::lISAM
      If Empty( ::cCustomSQL )
         ::cFltUsr := ::SolveSQLFilters( "A" )
         cWhere    := ::SolveRestrictors()
         If !Empty( cWhere )
            cWhere := " WHERE " + cWhere
         EndIf
      EndIf
   EndIf

   ::hnRecno      := 0
   ::hnDeleted    := 0
   ::aIniFields   := {}

   If ::oSql:oSqlTransact == NIL .or. (!::lISAM)
      ::aFields := ::oSql:IniFields( lReSelect, ::cQualifiedTableName, ::cCustomSQL, lLoadCache, cWhere, ::cRecnoName, ::cDeletedName )
   Else
      ::aFields := ::oSql:oSqlTransact:IniFields( lReSelect, ::cQualifiedTableName, ::cCustomSQL, lLoadCache, cWhere, ::cRecnoName, ::cDeletedName )
      ::oSql:oSqlTransact:Commit()
   EndIf

   If ::aFields == NIL
      Return NIL
   EndIf

   ::nFields               := LEN( ::aFields )
   ::aInfo[AINFO_FCOUNT]   := ::nFields
   ::aNames                := Array( ::nFields )
   ::aNamesLower           := Array( ::nFields )

   aSize( ::aEmptyBuffer, ::nFields )
   aSize( ::aSelectList, ::nFields )
   aFill( ::aSelectList, 0 )

   If !::lISAM
      aSize( ::aLocalBuffer, ::nFields )
   EndIf

#ifdef SQLRDD_TOPCONN

   If ::oSQL:nTCCompat > 0

      nPos := HGetPos( ::oSql:aFieldModifier, ::cFileName )

      If nPos > 0
         aFlds := HGetValueAt( ::oSql:aFieldModifier, nPos )
      EndIf

      For n = 1 to ::nFields
         cName := Upper(alltrim(::aFields[n,1]))
         aSize( ::aFields[n], FIELD_INFO_SIZE )
         ::aFields[n, FIELD_MULTILANG ] := .F.
         ::aFields[n, FIELD_ENUM] := n
         ::aFields[n, FIELD_WAOFFSET] := 0

         If ::aFields[n,2] == "M" .and. SR_SetMultiLang()
            aML := GetMLHash( ::cFileName, ::aFields[n,1] )
            If aML != NIL
               ::aFields[n,2] := aML[3]
               ::aFields[n,3] := val(aML[4])
               ::aFields[n, FIELD_MULTILANG ] := .T.
            EndIf
         EndIf

         If cName == ::cRecnoName .or. cName == "SR_RECNO"
            ::hnRecno := n
            If (::cRecnoName != SR_RecnoName()) .or. !SR_SetHideRecno()
               aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
               ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
            EndIf
         ElseIf cName == ::cDeletedName
            ::hnDeleted := n
            ::aFields[n,5] := .F.     /* NOT NULL */
         ElseIf cName == "DT__HIST" .and. ::lISAM
            ::nPosDtHist := n
            ::lHistoric  := .T.

            If !SR_SetHideHistoric()
               aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
               ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
            EndIf
         ElseIf cName == ::cColPK
            ::nPosColPK := n
            aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
            ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
         ElseIf SubStr(cName,1,7)  == "INDKEY_" .or. SubStr( cName,1,7) == "INDFOR_"
            // Ignore these columns
         ElseIf cName == "R_E_C_N_O_"
            ::cRecnoName := "R_E_C_N_O_"
            ::hnRecno    := n
            ::nTCCompat  := 2
            If (::cRecnoName != SR_RecnoName()) .or. !SR_SetHideRecno()
               aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
               ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
            EndIf
            // Fix record numbering to work like TopConnect
            SR_SetNextRecordBlock( { |oWA| SR_TCNextRecord(oWA) } )
            ::lUseSequences := .F.
         ElseIf cName == "D_E_L_E_T_"
            ::cDeletedName := "D_E_L_E_T_"
            ::hnDeleted    := n
         ElseIf cName == "R_E_C_D_E_L_"
            ::nTCCompat  := 4
            // Ignore this column
         Else
            nPos  := aScan( aFlds, { |x| upper(alltrim(x[1])) == cName } )
            If nPos > 0
               If aFlds[nPos, 2] = "P"
                  ::aFields[n, 2] := "N"
                  ::aFields[n, 3] := val(aFlds[nPos, 3])
                  ::aFields[n, 4] := val(aFlds[nPos, 4])
                  ::aFields[n, FIELD_DOMAIN] := SQL_FAKE_NUM
               ElseIf aFlds[nPos, 2] = "D"
                  If ::aFields[n, 2] != "D"
                     ::aFields[n, 2] := "D"
                     ::aFields[n, 3] := 8
                     ::aFields[n, FIELD_DOMAIN] := SQL_FAKE_DATE
                  EndIf
               EndIf
            EndIf
            aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
            ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
         EndIf

         ::aNames[n]       := cName

         If ::aFields[n, FIELD_MULTILANG] .and. SR_SetMultiLang()
            ::aEmptyBuffer[n] := Hash()
         Else
            ::aEmptyBuffer[n] := SR_BlankVar( ::aFields[n,2], ::aFields[n,3], ::aFields[n,4] )
         EndIf

      Next

      If ::nTCCompat > 0
         For n = 1 to ::nFields
            ::aFields[n,5] := .F.
         Next
      EndIf

   Else

#endif

      For n = 1 to ::nFields
         cName := ::aFields[n,1]
         aSize( ::aFields[n], FIELD_INFO_SIZE )
         ::aFields[n, FIELD_MULTILANG ] := .F.
         ::aFields[n, FIELD_ENUM] := n
         ::aFields[n, FIELD_WAOFFSET] := 0

         If ::aFields[n,2] == "M" .and. SR_SetMultiLang()
            aML := GetMLHash( ::cFileName, ::aFields[n,1] )
            If aML != NIL
               ::aFields[n,2] := aML[3]
               ::aFields[n,3] := val(aML[4])
               ::aFields[n, FIELD_MULTILANG ] := .T.
            EndIf
         EndIf

         If cName == ::cRecnoName .or. cName == "SR_RECNO"
            ::cRecnoName := cName
            ::hnRecno    := n
            If (::cRecnoName != SR_RecnoName()) .or. !SR_SetHideRecno()
               aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
               ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
            EndIf
         ElseIf cName == ::cDeletedName
            ::hnDeleted    := n
            ::aFields[n,5] := .F.     /* NOT NULL */
         ElseIf cName == "DT__HIST" .and. ::lISAM
            ::nPosDtHist := n
            ::lHistoric  := .T.

            If !SR_SetHideHistoric()
               aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
               ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
            EndIf
         ElseIf cName == ::cColPK
            ::nPosColPK := n
            aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
            ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
         ElseIf SubStr(cName,1,7)  == "INDKEY_" .or. SubStr( cName,1,7) == "INDFOR_"
            // Ignore these columns
            // Culik are we in sqlex? if yes, we need to return this fields to query also
            if RDDNAME() == "SQLEX"
               aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
               ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
            endif

         Else
            aadd( ::aIniFields, { ::aFields[n,1], ::aFields[n,2], ::aFields[n,3], ::aFields[n,4], n } )
            ::aFields[n, FIELD_WAOFFSET] := len( ::aIniFields )
         EndIf

         ::aNames[n]       := cName

         If ::aFields[n, FIELD_MULTILANG] .and. SR_SetMultiLang()
            ::aEmptyBuffer[n] := Hash()
         Else
            ::aEmptyBuffer[n] := SR_BlankVar( ::aFields[n,2], ::aFields[n,3], ::aFields[n,4] )
         EndIf

      Next

#ifdef SQLRDD_TOPCONN

   EndIf

#endif

   If ::lHistoric .and. ::nPosColPK == NIL
      ::nPosColPK := ::hnRecno
      ::cColPK    := ::cRecnoName
   EndIf

   If ::hnRecno == NIL .and. ::lISAM
      // Let's try some *magic*

      aFlds := {}

      Switch ::oSQL:nSystemID
      Case SYSTEMID_IBMDB2
      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
      Case SYSTEMID_SYBASE
         Exit

      Case SYSTEMID_MSSQL7
      Case SYSTEMID_AZURE
         ::oSql:exec( "sp_pkeys " + ::cFileName, .T., .T., @aFlds )
         If len( aFlds ) == 1
            ::hnRecno    := aScan( ::aFields, {|x| x[1] == alltrim(upper(aflds[1,4]))} )
            ::cRecnoName := aflds[1,4]
         EndIf
         Exit
      Case SYSTEMID_ORACLE
      Case SYSTEMID_POSTGR

      End

      If Empty( ::hnRecno )
         ::RuntimeErr( "24", SR_Msg(24) + ::cRecnoName  + " / " + ::cFileName )
      EndIf
   EndIf

   If !lHaveInfoCache .and. aInfo != NIL
      aInfo[ CACHEINFO_AFIELDS ]     := aClone(::aFields)
      aInfo[ CACHEINFO_ANAMES ]      := aClone(::aNames)
      aInfo[ CACHEINFO_ABLANK ]      := aClone(::aEmptyBuffer)
      aInfo[ CACHEINFO_HNRECNO ]     := ::hnRecno
      aInfo[ CACHEINFO_HNDELETED ]   := ::hnDeleted
      aInfo[ CACHEINFO_INIFIELDS ]   := aClone(::aIniFields)
      aInfo[ CACHEINFO_HNPOSDTHIST ] := ::nPosDtHist
      aInfo[ CACHEINFO_HNCOLPK ]     := ::nPosColPK
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlGoBottom() CLASS SR_WORKAREA

   Local cJoin1, cJoin3, cTemp := "", i

   If ::lCollectingBehavior
      For each i in ::aSelectList
         If i == 1
            ::lCollectingBehavior   := .F.
            Exit
         EndIf
      Next
   EndIf

   ::aInfo[ AINFO_DETECT1_COUNT ] := 0

   If ::lISAM

      cJoin1 := " " + ::cQualifiedTableName + " A "
      cJoin3 := ::GetSelectList()
      ::aInfo[ AINFO_SKIPCOUNT ] := 0

      If !Empty( cTemp := ::SolveRestrictors() )
         cTemp := " WHERE " + cTemp
      EndIf

      ::ResetStatistics()

      If ::aInfo[ AINFO_INDEXORD ] > 0 .and. ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] != NIL
         cTemp += if( " WHERE " $ cTemp, " AND ", " WHERE " ) + "(" + ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_EXPR ] + " <= 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz' AND ROWNUM <= " + str(::nCurrentFetch+1) + ") "
         ::oSql:Execute( 'SELECT /*+ INDEX( A D$' + ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] + ") */ " + cJoin3 + "FROM" + cJoin1 + cTemp + if(::oSql:lComments," /* GoBottom */","") )
      Else
         ::oSql:Execute( "SELECT" + eval( ::Optmizer_ns, ::nCurrentFetch ) + cJoin3 + "FROM" + cJoin1 + cTemp + ::OrderBy( NIL,.F.) + eval( ::Optmizer_ne, ::nCurrentFetch ) + if(::oSql:lComments," /* GoBottom */","") )
      EndIf

      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
      ::aInfo[ AINFO_NPOSCACHE ] := 0

      ::FirstFetch( ORD_DIR_BWD )
      ::oSql:FreeStatement()

      If !::lNoData
         ::aInfo[ AINFO_EOF_AT ] := ::aLocalBuffer[::hnRecno]
      EndIf

      If ::lEmptyTable .or. ::lNoData
         ::aInfo[ AINFO_BOF ] := .T.
         ::aInfo[ AINFO_EOF ] := .T.
      else
         ::aInfo[ AINFO_BOF ] := .F.
         ::aInfo[ AINFO_EOF ] := .F.
         ::aInfo[ AINFO_NPOSCACHE ] := (CAHCE_PAGE_SIZE * 3)
      endif

   Else

      If ::lNoData
         Return NIL
      else
         ::Stabilize()

         ::aInfo[ AINFO_BOF ] := .F.
         ::aInfo[ AINFO_EOF ] := .F.
      endif

      If !::aInfo[ AINFO_REVERSE_INDEX ]
         ::GetBuffer(.F., len( ::aCache ) )
         ::Normalize(-1)
      Else
         ::GetBuffer(.F., 1 )
         ::Normalize(1)
      EndIf

   EndIf

   If ::hnDeleted > 0  .and. ::aLocalBuffer[ ::hnDeleted ] != NIL
     ::aInfo[ AINFO_DELETED ] := ::aLocalBuffer[ ::hnDeleted ] $ "T*"
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlGoCold() CLASS SR_WORKAREA

   If ::aInfo[ AINFO_HOT ] .and. if( ::hnDeleted > 0, .T.,  !::aInfo[ AINFO_DELETED ] )
      ::WriteBuffer( ::aInfo[ AINFO_ISINSERT ], ::aLocalBuffer )
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

METHOD sqlGoTo( uRecord, lNoOptimize ) CLASS SR_WORKAREA

   Local nCache, cGoTo
   Local i
   Local cJoin1, cJoin3

   DEFAULT lNoOptimize := .F.

   ::aInfo[ AINFO_FOUND ]  := .F.

   If ::lCollectingBehavior
      For each i in ::aSelectList
         If i == 1
            ::lCollectingBehavior   := .F.
            Exit
         EndIf
      Next
   EndIf

   // Optimizing dbGoTo( recno() )

   If (!lNoOptimize) .and. (uRecord == ::aInfo[ AINFO_RECNO ] .and. !::aInfo[ AINFO_ISINSERT ] .and. !( ::aInfo[ AINFO_BOF ] .and. ::aInfo[ AINFO_EOF ] ))
//      ::aInfo[ AINFO_EOF ]      := .F.
      ::aInfo[ AINFO_BOF ]      := .F.
      Return NIL
   EndIf

   ::aInfo[ AINFO_SKIPCOUNT ]     := 0
   ::aInfo[ AINFO_DETECT1_COUNT ] := 0

   If Empty( uRecord ) .or. ( valtype( uRecord ) == "N" .and. uRecord == LASTREC_POS + 1 )
      ::GetBuffer(.T.)
      If ::aInfo[ AINFO_ISINSERT ]
         ::aInfo[ AINFO_RECNO ] := ::GetNextRecordNumber()
         ::aLocalBuffer[ ::hnRecno ] := ::aInfo[ AINFO_RECNO ]
      EndIf
      ::aInfo[ AINFO_BOF ]     := .T.     // Bug fixed in feb 2009, BOF should be TRUE as well as EOF
                                          // if dbGoTo( invalidRecord )
      Return NIL
   Else
      If ::lISAM

         If ::aInfo[ AINFO_NCACHEBEGIN ] > 0 .and. ::aInfo[ AINFO_BOF_AT ] > 0 .and. ::aCache[ ::aInfo[ AINFO_NCACHEBEGIN ]] != NIL .and. len(::aCache[ ::aInfo[ AINFO_NCACHEBEGIN ]] ) > 0
            If ::aCache[ ::aInfo[ AINFO_NCACHEBEGIN ], ::hnRecno ] == uRecord
               ::aInfo[ AINFO_NPOSCACHE ] := ::aInfo[ AINFO_NCACHEBEGIN ]
               ::GetBuffer()
               ::aInfo[ AINFO_EOF ]      := .F.
               ::aInfo[ AINFO_BOF ]      := .F.
               Return NIL
            EndIf
         EndIf

         cJoin1 := " " + ::cQualifiedTableName + " A "
         cJoin3 := ::GetSelectList()
         cGoTo  := "SELECT" + ::Optmizer_1s + cJoin3 + "FROM" + cJoin1 + " WHERE A." + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " = "

         ::oSql:Execute( cGoTo + ::Quoted( uRecord,,18,0) + " " + if(::oSql:lComments," /* GoTo */","") )
         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
         ::aInfo[ AINFO_NPOSCACHE ] := 0

         ::FirstFetch()
         ::oSql:FreeStatement()

         If ::lNoData
            If SR_ErrorOnGotoToInvalidRecord()
               ::RuntimeErr( "6", SR_Msg(6) + ::Quoted(uRecord,,15,0) )
            Else
               ::GetBuffer(.T.)
               ::aInfo[ AINFO_BOF ]     := .T.     // Bug fixed in feb 2009, BOF should be TRUE as well as EOF
                                                   // if dbGoTo( invalidRecord )
            EndIf
         Else
            ::aInfo[ AINFO_EOF ] := .F.
            ::aInfo[ AINFO_BOF ] := .F.
         EndIf
      Else
         nCache := aScan( ::aCache, {|x| x[::hnRecno ] == uRecord } )
         If nCache == 0 .and. SR_ErrorOnGotoToInvalidRecord()
            ::RuntimeErr( "6", SR_Msg(6) + ::Quoted(uRecord,,15,0) )
         Else
            ::GetBuffer( .F., nCache )
            ::aInfo[ AINFO_EOF ] := .F.
            ::aInfo[ AINFO_BOF ] := .F.
         EndIf
      EndIf
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlGoTop() CLASS SR_WORKAREA

   Local cJoin1, cJoin3, cTemp, i
   If ::lCollectingBehavior
      For each i in ::aSelectList
         If i == 1
            ::lCollectingBehavior   := .F.
            Exit
         EndIf
      Next
   EndIf

   ::aInfo[ AINFO_DETECT1_COUNT ] := 0

   If ::lISAM
      cJoin1 := " " + ::cQualifiedTableName + " A "
      cJoin3 := ::GetSelectList()
      ::aInfo[ AINFO_SKIPCOUNT ] := 0

      If !Empty( cTemp := ::SolveRestrictors() )
         cTemp := " WHERE " + cTemp
      EndIf

      ::ResetStatistics()

      If ::aInfo[ AINFO_INDEXORD ] > 0 .and. ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] != NIL
         cTemp += if( " WHERE " $ cTemp, " AND ", " WHERE " ) + "(" + ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_EXPR ] + " >= ' ' AND ROWNUM <= " + str(::nCurrentFetch+1) + ") "
         ::oSql:Execute( 'SELECT /*+ INDEX( A A$' + ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] + ") */ " + cJoin3 + "FROM" + cJoin1 + cTemp + if(::oSql:lComments," /* GoTop */","") )
      Else
         ::oSql:Execute( "SELECT" + eval( ::Optmizer_ns, ::nCurrentFetch ) + cJoin3 + "FROM" + cJoin1 + cTemp + ::OrderBy( NIL,.T. ) + eval( ::Optmizer_ne, ::nCurrentFetch ) + if(::oSql:lComments," /* GoTop */","") )
      EndIf

      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
      ::aInfo[ AINFO_NPOSCACHE ] := 0
      ::FirstFetch( ORD_DIR_FWD )

      ::oSql:FreeStatement()

      If !::lNoData
         ::aInfo[ AINFO_BOF_AT ] := ::aLocalBuffer[::hnRecno]
      EndIf

     If ::lEmptyTable .or. ::lNoData
         ::aInfo[ AINFO_BOF ] := .t.
         ::aInfo[ AINFO_EOF ] := .t.
      else
         ::aInfo[ AINFO_BOF ] := .F.
         ::aInfo[ AINFO_EOF ] := .F.
         ::aInfo[ AINFO_NPOSCACHE ] := 1
      endif

   Else

      /* ALL_IN_CACHE */

      If ::lNoData
         Return NIL
      else
         ::Stabilize()
         ::aInfo[ AINFO_BOF ] := .F.
         ::aInfo[ AINFO_EOF ] := .F.
      endif

      If !::aInfo[ AINFO_REVERSE_INDEX ]
         ::GetBuffer(.F., 1 )
         ::Normalize(1)
      Else
         ::GetBuffer(.F., len( ::aCache ) )
         ::Normalize(-1)
      EndIf

   EndIf

   If ::hnDeleted > 0 .and. ::aLocalBuffer[ ::hnDeleted ] != NIL
      ::aInfo[ AINFO_DELETED ] := ::aLocalBuffer[ ::hnDeleted ] $ "T*"
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlGoPhantom()  CLASS SR_WORKAREA

   ::sqlGoCold()
   ::GetBuffer(.T.)
   ::aInfo[ AINFO_FOUND ]  := .F.

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlSeek( uKey, lSoft, lLast ) CLASS SR_WORKAREA

   Local nLenKey, cPart, nCons, nLen
   Local i, j
   Local cType := ""
   Local lPartialSeek := .F.
   Local cRet := "", nFDec, nFLen
   Local nThis, cSep, cSql
   Local c1 := "", cQot, cNam, nSimpl
   Local nFeitos, aTemp, cTemp
   Local cJoin1, cJoin3, lNull, uSet
   Local lBlockSearch := .T.
   Local cField,nfieldPos
   Local lLikeSep   := .F.
   Local cKeyValue
   Local lIsIndKey := .F.

   (lLast) // to remove warning

   If ::lCollectingBehavior
      For each i in ::aSelectList
         If i == 1
            ::lCollectingBehavior   := .F.
            Exit
         EndIf
      Next
   EndIf

   ::nPartialDateSeek := 0
   ::sqlGoCold()

   DEFAULT lSoft := .F.

   ::aInfo[ AINFO_FOUND ]   := .F.
   ::aInfo[ AINFO_DETECT1_COUNT ] := 0


   /* reset static data */

   ::aQuoted   := {}
   ::aDat      := {}
   ::aPosition := {}

   If ::lNoData .and. (!::lISAM)
      Return NIL
   EndIf

   If ::aInfo[ AINFO_INDEXORD ] == 0
      ::RuntimeErr( "20" )
      Return NIL
   EndIf

   ::aInfo[ AINFO_SKIPCOUNT ] := 0
   uSet := Set( _SET_EXACT, .f. )

   If lSoft .and. ::lISAM .and. ::oSql:nSystemID == SYSTEMID_ORACLE .and. ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] != NIL .and. valtype(uKey) == "C"

         nLen      := Max( len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] ) - 1, 1 )      && Esse -1 é para remover o NRECNO que SEMPRE faz parte do indice !
         nCons     := 0
         nLenKey   := Len(uKey)
         cPart     := ""

         For i = 1 to nLen

            nThis := ::aFields[ ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ], FIELD_LEN ]
            cPart := SubStr( uKey, nCons+1, nThis )

            AADD( ::aPosition, ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] )

            cType := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],2]
            lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],5]
            nFDec := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],4]
            nFLen := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],3]

            If i == 1 .and. nThis >= len( uKey )
               If uKey == ""
                  Exit
               EndIf
            Else
               If len( cPart ) = 0
                  Exit
               EndIf
            EndIf

            AADD( ::aQuoted, ::QuotedNull(::ConvType( cPart, cType, @lPartialSeek, nThis, ::aInfo[ AINFO_REVERSE_INDEX ] ),!lSoft,,,,lNull  )) // Reverse Index should add % to end of string or seek will never find current record if partial
            AADD( ::aDat,    ::ConvType( if( lSoft, cPart, rtrim(cPart) ), cType, , nThis ) )

            nCons += nThis

            If nLenKey < nCons
               Exit
            endif

         Next

      cJoin1 := " " + ::cQualifiedTableName + " A "
      cJoin3 := ::GetSelectList()

      if ::aInfo[ AINFO_REVERSE_INDEX ] .or. lLast
         cSql  := 'SELECT /*+ INDEX( A ' + 'D$' + ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] + ") */ " + cJoin3 + "FROM" + cJoin1 + ::WhereVMinor( uKey ) + " AND ROWNUM <= 1"
      Else
         cSql  := 'SELECT /*+ INDEX( A ' + 'A$' + ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] + ") */ " + cJoin3 + "FROM" + cJoin1 + ::WhereVMajor( uKey ) + " AND ROWNUM <= 1"
      EndIf

      ::oSql:Execute( cSql + if(::oSql:lComments," /* SoftSeek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )

      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
      ::aInfo[ AINFO_NPOSCACHE ] := 0
      ::FirstFetch()

      If ::lNoData .or. ::aInfo[ AINFO_EOF ]
         ::aInfo[ AINFO_EOF ]   := .T.
         ::aInfo[ AINFO_FOUND ] := .F.
      Else

         If valtype( uKey ) == "C" .and. uKey == ""
            ::aInfo[ AINFO_FOUND ] := .T.
         Else
            For i = 1 to len( ::aQuoted )
               Do Case
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "C" .and. valtype( ::aDat[i] ) == "C" .and. (::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) .or. ::oSql:nSystemID == SYSTEMID_AZURE)
                  ::aInfo[ AINFO_FOUND ] := ( Upper(::aLocalBuffer[::aPosition[i]]) = Upper(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "N" .and. valtype( ::aDat[i] ) == "C"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = val(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "C" .and. valtype( ::aDat[i] ) == "N"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = str(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "D" .and. valtype( ::aDat[i] ) == "C"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = stod(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "D" .and. valtype( ::aDat[i] ) == "D" .and. ::nPartialDateSeek > 0 .and. i == len( ::aQuoted )
                  ::aInfo[ AINFO_FOUND ] := ( Left( dtos( ::aLocalBuffer[::aPosition[i]]), ::nPartialDateSeek ) == Left( dtos(::aDat[i]), ::nPartialDateSeek ) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "D" .and. valtype( ::aDat[i] ) == "D" .and. ::nPartialDateSeek == 0 .and. i == len( ::aQuoted )  .and. lsoft
                  ::aInfo[ AINFO_FOUND ] := (  dtos( ::aLocalBuffer[::aPosition[i]]) >= dtos(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "T" .and. valtype( ::aDat[i] ) == "T" .and. ::nPartialDateSeek > 0 .and. i == len( ::aQuoted )
                  ::aInfo[ AINFO_FOUND ] := ( Left( ttos( ::aLocalBuffer[::aPosition[i]]), ::nPartialDateSeek ) == Left( ttos(::aDat[i]), ::nPartialDateSeek ) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "T" .and. valtype( ::aDat[i] ) == "T" .and. ::nPartialDateSeek == 0 .and. i == len( ::aQuoted )     .and. lsoft
                  ::aInfo[ AINFO_FOUND ] := ( ttos( ::aLocalBuffer[::aPosition[i]]) >=  ttos(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) != valtype( ::aDat[i] )
                  ::RuntimeErr( "28" )
                  Set( _SET_EXACT, uSet )
                  Return NIL
               OtherWise
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = ::aDat[i] )
               EndCase

               If !::aInfo[ AINFO_FOUND ]
                  Exit
               EndIf
            Next
         EndIf
      EndIf

      If ::aInfo[ AINFO_FOUND ]
         ::aInfo[ AINFO_EOF ]   := .F.
         ::aInfo[ AINFO_BOF ]   := .F.
         ::aInfo[ AINFO_RECNO ]  := ::aLocalBuffer[::hnRecno]
      Else
         ::aInfo[ AINFO_FOUND ] := .F.
         If lSoft .and. ::lNoData
            ::GetBuffer(.T.)
            ::aInfo[ AINFO_RECNO ]      := ::aInfo[ AINFO_RCOUNT ] + 1
            ::aLocalBuffer[ ::hnRecno ] := ::aInfo[ AINFO_RECNO ]
         ElseIf lSoft
            ::aInfo[ AINFO_EOF ]   := .F.
            ::aInfo[ AINFO_RECNO ]  := ::aLocalBuffer[::hnRecno]
         Else
            ::GetBuffer(.T.)
            ::aInfo[ AINFO_RECNO ]      := ::aInfo[ AINFO_RCOUNT ] + 1
            ::aLocalBuffer[ ::hnRecno ] := ::aInfo[ AINFO_RECNO ]
         EndIf
      EndIf

      ::oSql:FreeStatement()

   ElseIf ::lISAM .and. ::oSql:nSystemID != SYSTEMID_POSTGR

      If valtype(uKey) $ "NDLT"       /* One field seek, piece of cake! */

         lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],5]
         nFDec := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],4]
         nFLen := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],3]

         cRet  := " WHERE (( "

         If ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] == ::hnDeleted .and. valtype(uKey) == "L"
            If ::nTCCompat > 0
               cQot := if( uKey, "'*'", "' '" )
               AADD( ::aDat, if( uKey, '*', ' ' ) )
            Else
               cQot := if( uKey, "'T'", "' '" )
               AADD( ::aDat, if( uKey, 'T', ' ' ) )
            EndIf
         Else
            if "INDKEY_" $ ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] ] .and. valtype( uKey ) == "N"
               cField :=  UPPER( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_KEY ] )
               IF "VAL(" $ CFIELD

                  CfIELD := STRTRAN( CfIELD, 'VAL(', '' )
                  CfIELD := STRTRAN( CfIELD, ')', '' )
                  nfieldPos := ASCAN( ::aFields, { | x | x[ 1 ] == cField } )
                  IF nFieldPos >0
                     cKeyValue := Str( uKey, ::aFields[ nFieldPos, 3 ] ) + "%"
                     lLikeSep := .T.
                  ENDIF
               ENDIF
            ENDIF
            if !empty( cKeyValue )
               cQot  := ::QuotedNull(cKeyValue,, nFLen, nFDec,, lNull)
            else
               cQot := ::QuotedNull(uKey,, nFLen, nFDec,, lNull)
            endif
            IF lLikeSep
               AADD( ::aDat, Str( uKey,::aFields[nFieldPos,3] ) )
            ELSE
               AADD( ::aDat, uKey )
            ENDIF
         EndIf

         If ::aInfo[ AINFO_REVERSE_INDEX ]  .or. lLast
            cSep  := if( cQot == "NULL", " IS ", if( lSoft, " <= ", IF( lLikeSep, " Like ", " = " ) ) )
         Else
            cSep  := if( cQot == "NULL", " IS ", if( lSoft, " >= ", IF( lLikeSep, " Like ", " = " ) ) )
         EndIf
         cNam  := [A.] + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] ], ::oSql:nSystemID )

         AADD( ::aPosition, ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] )
         AADD( ::aQuoted, ::Quoted(uKey) )

         && If Null, we don't need WHERE clause on Soft Seeks
         If (!IsNull( cQot )) .or. (!lSoft)
            cRet  += cNam + cSep + cQot + " "
         EndIf

      ElseIf ValType(uKey) == "C"

         nLen      := Max( len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] ) - 1, 1 )      && Esse -1 é para remover o NRECNO que SEMPRE faz parte do indice !
         nCons     := 0
         nLenKey   := Len(uKey)
         cPart     := ""

         For i = 1 to nLen

            nThis := ::aFields[ ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ], FIELD_LEN ]
            cPart := SubStr( uKey, nCons+1, nThis )

            AADD( ::aPosition, ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] )

            cType := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],2]
            lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],5]
            nFDec := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],4]
            nFLen := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],3]

            If i == 1 .and. nThis >= len( uKey )
               If uKey == ""
                  Exit
               EndIf
            Else
               If len( cPart ) = 0
                  Exit
               EndIf
            EndIf

            AADD( ::aQuoted, ::QuotedNull(::ConvType( cPart, cType, @lPartialSeek, nThis, ::aInfo[ AINFO_REVERSE_INDEX ] ),!lSoft,,,,lNull  )) // Reverse Index should add % to end of string or seek will never find current record if partial
            AADD( ::aDat,    ::ConvType( if( lSoft, cPart, rtrim(cPart) ), cType, , nThis ) )

            nCons += nThis

            If nLenKey < nCons
               Exit
            endif

         Next

         cRet := " WHERE (( "
         nLen := Min( nLen, Len( ::aQuoted ) )

         If lSoft
            nSimpl := nLen
         Else
            nSimpl := 1
         EndIf

         For j = 1 to nSimpl

            If j >= 2
               cRet += e" ) OR \r\n ( "
            endif

            nFeitos := 0

            For i = 1 to ( nLen - j + 1 )

               cQot := ::aQuoted[i]
               cNam := [A.] + SR_DBQUALIFY( ::aNames[::aIndex[::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2]], ::oSql:nSystemID )

               If lPartialSeek .and. i = nLen
                  If ::aInfo[ AINFO_REVERSE_INDEX ]
                     cSep := if( lSoft, if(j=1, " <= ", " < "), if(cQot == "NULL", " IS ", " LIKE " ))
                  ElseIf ::nPartialDateSeek > 0  // Partial date seek
                     cSep := if(j=1, " >= ", " > ")
                  Else
                     cSep := if( lSoft, if(j=1, " >= ", " > "), if(cQot == "NULL", " IS ", " LIKE " ))
                     If cQot != "NULL" .and. !lSoft .AND. ! 'TO_DATE(' $ cQot
                        cTemp := SubStr(cQot,1,len(cQot)-1)
                        Switch ::oSql:nSystemID
                        Case SYSTEMID_MSSQL7
                        Case SYSTEMID_AZURE
                           cTemp := StrTran( cTemp, "%", "!%" )
                           Exit
                        Case SYSTEMID_MYSQL
                        Case SYSTEMID_MARIADB
                           cTemp := StrTran( cTemp, "%", "\%" )
                           cTemp := StrTran( cTemp, "_", "\_" )
                           Exit
                        End
                        cQot  := cTemp + "%'"
                     EndIf
                  EndIf
               Else
                  If ::aInfo[ AINFO_REVERSE_INDEX ]  .or. lLast
                     cSep := if( lSoft, if(i != nLen - j + 1 .or. j == 1, " <= ", " < "), if(cQot == "NULL", " IS ", " = " ))
                  Else
                     cSep := if( lSoft, if(i != nLen - j + 1 .or. j == 1, " >= ", " > "), if(cQot == "NULL", " IS ", " = " ))
                  EndIf
               EndIf

               && When using >=, If the quoted value is NULL, this column does't mind to this query.

               If (cSep == " >= " .or. cSep == " <= " ) .and. IsNull( ::aQuoted[i] )
                  Loop
               endif

               If cSep == " > " .and. ::aQuoted[i] == "NULL"
                  cSep := " IS NOT "
               ElseIf cSep == " < " .and. ::aQuoted[i] == "NULL"
                  Loop        // ToDo: less than NULL does not exist (or negative if numeric field)
               EndIf

               nFeitos ++

               cRet += if(nFeitos>1," AND ","") + cNam + cSep + cQot + if( cSep == " LIKE " .and. (::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_AZURE), " ESCAPE '!' ", " " )

            Next

            If j == 1 .and. nFeitos == 0
               cRet := " WHERE (( 1 = 1 "
               Exit
            EndIf

         Next

      Else
         ::RuntimeErr( "26" )
         Set( _SET_EXACT, uSet )
         Return NIL
      EndIf

      cTemp := ::SolveRestrictors()

      If cRet == " WHERE (( "
         If Empty(cTemp)
            cRet := ""
         Else
            cRet := " WHERE " + cTemp
         EndIf
      Else
         cRet += ")) "
         If !Empty(cTemp)
            cRet += " AND " + cTemp
         EndIf
      EndIf

      cJoin1 := " " + ::cQualifiedTableName + " A "
      cJoin3 := ::GetSelectList()

      If ::lFetchAll
         //::oSql:Execute( "SELECT" + eval( ::Optmizer_ns, max(::nCurrentFetch,50) ) + cJoin3 + "FROM" + cJoin1 + cRet + ::OrderBy(NIL,.T.) + eval( ::Optmizer_ne, max(::nCurrentFetch,50) ) + if(::oSql:lComments," /* " + if(lSoft,"Soft", "" ) + "Seek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )
         ::oSql:Execute( "SELECT" + eval( ::Optmizer_ns, max(::nCurrentFetch,50) ) + cJoin3 + "FROM" + cJoin1 + cRet + ::OrderBy(NIL,if(lLast,.F.,.T.)) + eval( ::Optmizer_ne, max(::nCurrentFetch,50) ) + if(::oSql:lComments," /* " + if(lSoft,"Soft", "" ) + "Seek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )
      Else
         //::oSql:Execute( "SELECT" + ::Optmizer_1s + cJoin3 + "FROM" + cJoin1 + cRet + ::OrderBy(NIL,.T.) + ::Optmizer_1e + if(::oSql:lComments," /* " + if(lSoft,"Soft", "" ) + "Seek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )
         ::oSql:Execute( "SELECT" + ::Optmizer_1s + cJoin3 + "FROM" + cJoin1 + cRet + ::OrderBy(NIL,if(lLast,.F.,.T.)) + ::Optmizer_1e + if(::oSql:lComments," /* " + if(lSoft,"Soft", "" ) + "Seek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )
      EndIf

      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
      ::aInfo[ AINFO_NPOSCACHE ] := 0
      ::FirstFetch()

      If ::lNoData .or. ::aInfo[ AINFO_EOF ]
         ::aInfo[ AINFO_EOF ]   := .T.
         ::aInfo[ AINFO_FOUND ] := .F.
      Else

         If valtype( uKey ) == "C" .and. uKey == ""
            ::aInfo[ AINFO_FOUND ] := .T.
         Else
            For i = 1 to len( ::aQuoted )
               Do Case
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "C" .and. valtype( ::aDat[i] ) == "C" .and. (::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) .or. ::oSql:nSystemID == SYSTEMID_AZURE)
                  ::aInfo[ AINFO_FOUND ] := ( Upper(::aLocalBuffer[::aPosition[i]]) = Upper(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "N" .and. valtype( ::aDat[i] ) == "C"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = val(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "C" .and. valtype( ::aDat[i] ) == "N"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = str(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "D" .and. valtype( ::aDat[i] ) == "C"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = stod(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "D" .and. valtype( ::aDat[i] ) == "D" .and. ::nPartialDateSeek > 0 .and. i == len( ::aQuoted )
                  ::aInfo[ AINFO_FOUND ] := ( Left( dtos( ::aLocalBuffer[::aPosition[i]]), ::nPartialDateSeek ) == Left( dtos(::aDat[i]), ::nPartialDateSeek ) )
                Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "D" .and. valtype( ::aDat[i] ) == "D" .and. ::nPartialDateSeek == 0 .and. i == len( ::aQuoted )  .and. lsoft
                  ::aInfo[ AINFO_FOUND ] := (  dtos( ::aLocalBuffer[::aPosition[i]]) >= dtos(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "T" .and. valtype( ::aDat[i] ) == "T" .and. ::nPartialDateSeek > 0 .and. i == len( ::aQuoted )
                  ::aInfo[ AINFO_FOUND ] := ( Left( ttos( ::aLocalBuffer[::aPosition[i]]), ::nPartialDateSeek ) == Left( ttos(::aDat[i]), ::nPartialDateSeek ) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "T" .and. valtype( ::aDat[i] ) == "T" .and. ::nPartialDateSeek == 0 .and. i == len( ::aQuoted )    .and. lsoft
                  ::aInfo[ AINFO_FOUND ] := ( ttos( ::aLocalBuffer[::aPosition[i]]) >=  ttos(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) != valtype( ::aDat[i] )
                  ::RuntimeErr( "28" )
                  Set( _SET_EXACT, uSet )
                  Return NIL
               OtherWise
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = ::aDat[i] )
               EndCase

               If !::aInfo[ AINFO_FOUND ]
                  Exit
               EndIf
            Next
         EndIf
      EndIf

      If ::aInfo[ AINFO_FOUND ]
         ::aInfo[ AINFO_EOF ]   := .F.       /* 06/01/2004 - fixing skip after dbseek */
         ::aInfo[ AINFO_BOF ]   := .F.
         ::aInfo[ AINFO_RECNO ]  := ::aLocalBuffer[::hnRecno]
      Else
         ::aInfo[ AINFO_FOUND ] := .F.
         If lSoft .and. ::lNoData
            ::GetBuffer(.T.)
            ::aInfo[ AINFO_RECNO ]      := ::aInfo[ AINFO_RCOUNT ] + 1
            ::aLocalBuffer[ ::hnRecno ] := ::aInfo[ AINFO_RECNO ]
         ElseIf lSoft
            ::aInfo[ AINFO_EOF ]   := .F.
            ::aInfo[ AINFO_RECNO ]  := ::aLocalBuffer[::hnRecno]
         Else
            ::GetBuffer(.T.)
            ::aInfo[ AINFO_RECNO ]      := ::aInfo[ AINFO_RCOUNT ] + 1
            ::aLocalBuffer[ ::hnRecno ] := ::aInfo[ AINFO_RECNO ]
         EndIf
      EndIf

      ::oSql:FreeStatement()

   ElseIf ::lISAM .and. ::oSql:nSystemID == SYSTEMID_POSTGR

      If valtype(uKey) $ "NDLT"       /* One field seek, piece of cake! */

         lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],5]
         nFDec := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],4]
         nFLen := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],3]

         cRet  := " WHERE (( "

         if "INDKEY_" $ ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] ] .and. valtype( uKey ) == "N"
            cField :=  UPPER( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_KEY ] )
            IF "VAL(" $ CFIELD

               CfIELD := STRTRAN(CfIELD,'VAL(','')
               CfIELD := STRTRAN(CfIELD,')','')
               nfieldPos := ASCAN(::aFields, {|x| x[1] == cField})
               IF nFieldPos >0
                  cKeyValue := Str( uKey,::aFields[nFieldPos,3] ) +"%"
                  lLikeSep := .T.
               ENDIF
            ENDIF
         ENDIF
         if !empty( cKeyValue )
            cQot  := ::QuotedNull(cKeyValue,, nFLen, nFDec,, lNull)
         else
            cQot  := ::QuotedNull(uKey,, nFLen, nFDec,, lNull)
         endif

         If ::aInfo[ AINFO_REVERSE_INDEX ]  .or. lLast
            cSep  := if( cQot == "NULL", " IS ", if( lSoft, " <= ", IF( lLikeSep, " Like ", " = " ) ) )
         Else
            cSep  := if( cQot == "NULL", " IS ", if( lSoft, " >= ", IF( lLikeSep, " Like ", " = " ) ) )
         EndIf

         cNam  := [A.] + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] ], ::oSql:nSystemID )

         AADD( ::aPosition, ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] )
         AADD( ::aQuoted, ::QuotedNull(uKey) )
         IF lLikeSep
            AADD( ::aDat, Str( uKey,::aFields[nFieldPos,3] ) )
         ELSE
            AADD( ::aDat, uKey )
         ENDIF

         && If Null, we don't need WHERE clause on Soft Seeks
         // culik, we have an indkey_xxxx and seek value is number, we convert to string
         If (!IsNull( cQot )) .or. (!lSoft)
            cRet  += cNam + cSep + cQot +  " "
         EndIf

         cTemp := ::SolveRestrictors()

         If cRet == " WHERE (( "
            If Empty(cTemp)
               cRet := ""
            Else
               cRet := " WHERE " + cTemp
            EndIf
         Else
            cRet += ")) "
            If !Empty(cTemp)
               cRet += " AND " + cTemp
            EndIf
         EndIf

         cJoin1 := " " + ::cQualifiedTableName + " A "
         cJoin3 := ::GetSelectList()

         If ::lFetchAll
            //::oSql:Execute( "SELECT" + eval( ::Optmizer_ns, max(::nCurrentFetch,50) ) + cJoin3 + "FROM" + cJoin1 + cRet + ::OrderBy(NIL,.T.) + eval( ::Optmizer_ne, max(::nCurrentFetch,50) ) + if(::oSql:lComments," /* " + if(lSoft,"Soft", "" ) + "Seek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )
            ::oSql:Execute( "SELECT" + eval( ::Optmizer_ns, max(::nCurrentFetch,50) ) + cJoin3 + "FROM" + cJoin1 + cRet + ::OrderBy(NIL,if(lLast,.F.,.T.)) + eval( ::Optmizer_ne, max(::nCurrentFetch,50) ) + if(::oSql:lComments," /* " + if(lSoft,"Soft", "" ) + "Seek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )
         Else
            //::oSql:Execute( "SELECT" + ::Optmizer_1s + cJoin3 + "FROM" + cJoin1 + cRet + ::OrderBy(NIL,.T.) + ::Optmizer_1e + if(::oSql:lComments," /* " + if(lSoft,"Soft", "" ) + "Seek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )
            ::oSql:Execute( "SELECT" + ::Optmizer_1s + cJoin3 + "FROM" + cJoin1 + cRet + ::OrderBy(NIL,if(lLast,.F.,.T.)) + ::Optmizer_1e + if(::oSql:lComments," /* " + if(lSoft,"Soft", "" ) + "Seek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )
         EndIf

      ElseIf ValType(uKey) == "C"

         nLen      := Max(len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] ) - 1, 1)      && Esse -1 é para remover o NRECNO que SEMPRE faz parte do indice !
         nCons     := 0
         nLenKey   := Len(uKey)
         cPart     := ""

         For i = 1 to nLen

            nThis := ::aFields[ ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ], FIELD_LEN ]
            cPart := SubStr( uKey, nCons+1, nThis )

            AADD( ::aPosition, ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] )

            cType := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],2]
            lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],5]
            nFDec := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],4]
            nFLen := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],3]

            If i == 1 .and. nThis >= len( uKey )
               If uKey == ""
                  Exit
               EndIf
            Else
               If len( cPart ) = 0
                  Exit
               EndIf
            EndIf

            // Ajuste abaixo - pgs é tudo NOT NULL, nao deve dar trim se for indice sintetico

            If ::aIndex[ ::aInfo[ AINFO_INDEXORD ], SYNTH_INDEX_COL_POS ] > 0
               AADD( ::aQuoted, ::Quoted(::ConvType( cPart, cType, @lPartialSeek, nThis ),.F.,,,, .T. ) )
            Else

               AADD( ::aQuoted, ::Quoted(::ConvType( cPart, cType, @lPartialSeek, nThis ),!lSoft,,,, .F. ) )
            EndIf
            AADD( ::aDat,    ::ConvType( cPart, cType, , nThis ) )

            nCons += nThis

            If nLenKey < nCons
               Exit
            endif

         Next

         cJoin1 := " " + ::cQualifiedTableName + " A "
         cJoin3 := ::GetSelectList()

         If ::aInfo[ AINFO_REVERSE_INDEX ] .or. lLast
            aTemp := ::WherePgsMinor( ::aQuoted, lPartialSeek .or. lSoft )
         Else
            aTemp := ::WherePgsMajor( ::aQuoted, lPartialSeek .or. lSoft )
         EndIf

         cTemp := "SELECT" + eval( ::Optmizer_ns, ::nCurrentFetch ) + cJoin3 + "FROM" + cJoin1
         cSql  := ""

         If len( aTemp ) > 0
            For i = 1 to len( aTemp )
               //cSql += "SELECT * FROM (" + cTemp + " WHERE " + aTemp[i] + ::OrderBy(NIL,.t.) + eval( ::Optmizer_ne, ::nCurrentFetch ) + " ) TMP" + alltrim(str(i))
               //test fix for seek last
               cSql += "SELECT * FROM (" + cTemp + " WHERE " + aTemp[i] + ::OrderBy(NIL,if(lLast,.F.,.T.)) + eval( ::Optmizer_ne, ::nCurrentFetch ) + " ) TMP" + alltrim(str(i))
               If i != len( aTemp )
                  cSql += CRLF + "UNION" + CRLF
               EndIf
            Next
            //cSql += strtran(::OrderBy(NIL, .t. ), "A.", "" ) + eval( ::Optmizer_ne, ::nCurrentFetch )
            //test fix for seek last
            cSql += strtran(::OrderBy(NIL, if(lLast,.F.,.T.) ), "A.", "" ) + eval( ::Optmizer_ne, ::nCurrentFetch )
         Else
            //test fix for seek last
            //cSql := "SELECT" + eval( ::Optmizer_ns, ::nCurrentFetch ) + cJoin3 + "FROM" + cJoin1 + ::OrderBy(NIL, .t. ) + eval( ::Optmizer_ne, ::nCurrentFetch )
            cSql := "SELECT" + eval( ::Optmizer_ns, ::nCurrentFetch ) + cJoin3 + "FROM" + cJoin1 + ::OrderBy(NIL, if(lLast,.F.,.T.) ) + eval( ::Optmizer_ne, ::nCurrentFetch )
         EndIf

         ::oSql:Execute( cSql + if(::oSql:lComments," /* " + if(lSoft,"Soft", "" ) + "Seek " + str(::aInfo[ AINFO_INDEXORD ]) + " */","")  )

      Else
         ::RuntimeErr( "26" )
         Set( _SET_EXACT, uSet )
         Return NIL
      EndIf

      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
      ::aInfo[ AINFO_NPOSCACHE ] := 0
      ::FirstFetch()

      If ::lNoData .or. ::aInfo[ AINFO_EOF ]
         ::aInfo[ AINFO_EOF ]   := .T.
         ::aInfo[ AINFO_FOUND ] := .F.
      Else

         If valtype( uKey ) == "C" .and. uKey == ""
            ::aInfo[ AINFO_FOUND ] := .T.
         Else
            For i = 1 to len( ::aQuoted )
               Do Case
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "N" .and. valtype( ::aDat[i] ) == "C"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = val(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "C" .and. valtype( ::aDat[i] ) == "N"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = str(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "D" .and. valtype( ::aDat[i] ) == "C"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = stod(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "T" .and. valtype( ::aDat[i] ) == "C"
                  ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = stot(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "D" .and. valtype( ::aDat[i] ) == "D" .and. ::nPartialDateSeek > 0 .and. i == len( ::aQuoted )
                  ::aInfo[ AINFO_FOUND ] := ( Left( dtos( ::aLocalBuffer[::aPosition[i]]), ::nPartialDateSeek ) == Left( dtos(::aDat[i]), ::nPartialDateSeek ) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "D" .and. valtype( ::aDat[i] ) == "D" .and. ::nPartialDateSeek == 0 .and. i == len( ::aQuoted ) .and. lsoft
                  ::aInfo[ AINFO_FOUND ] := (  dtos( ::aLocalBuffer[::aPosition[i]]) >= dtos(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "T" .and. valtype( ::aDat[i] ) == "T" .and. ::nPartialDateSeek > 0 .and. i == len( ::aQuoted )
                  ::aInfo[ AINFO_FOUND ] := ( Left( ttos( ::aLocalBuffer[::aPosition[i]]), ::nPartialDateSeek ) == Left( ttos(::aDat[i]), ::nPartialDateSeek ) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) == "T" .and. valtype( ::aDat[i] ) == "T" .and. ::nPartialDateSeek == 0 .and. i == len( ::aQuoted )  .and. lsoft
                  ::aInfo[ AINFO_FOUND ] := ( ttos( ::aLocalBuffer[::aPosition[i]]) >=  ttos(::aDat[i]) )
               Case valtype( ::aLocalBuffer[::aPosition[i]] ) != valtype( ::aDat[i] )
                  ::RuntimeErr( "28" )
                  Set( _SET_EXACT, uSet )
                  Return NIL
               OtherWise
                  If valtype( ::aLocalBuffer[::aPosition[i]] ) == "C"
                     ::aInfo[ AINFO_FOUND ] := ( left(::aLocalBuffer[::aPosition[i]],len(::aDat[i])) == ::aDat[i] )
                  Else
                     ::aInfo[ AINFO_FOUND ] := ( ::aLocalBuffer[::aPosition[i]] = ::aDat[i] )
                  EndIf
               EndCase

               If !::aInfo[ AINFO_FOUND ]
                  Exit
               EndIf
            Next
         EndIf
      EndIf

      If ::aInfo[ AINFO_FOUND ]
         ::aInfo[ AINFO_EOF ]   := .F.
         ::aInfo[ AINFO_BOF ]   := .F.
         ::aInfo[ AINFO_RECNO ]  := ::aLocalBuffer[::hnRecno]
      Else
         ::aInfo[ AINFO_FOUND ] := .F.
         If lSoft .and. ::lNoData
            ::GetBuffer(.T.)
            ::aInfo[ AINFO_RECNO ]      := ::aInfo[ AINFO_RCOUNT ] + 1
            ::aLocalBuffer[ ::hnRecno ] := ::aInfo[ AINFO_RECNO ]
         ElseIf lSoft
            ::aInfo[ AINFO_EOF ]   := .F.
            ::aInfo[ AINFO_RECNO ]  := ::aLocalBuffer[::hnRecno]
         Else
            ::GetBuffer(.T.)
            ::aInfo[ AINFO_RECNO ]      := ::aInfo[ AINFO_RCOUNT ] + 1
            ::aLocalBuffer[ ::hnRecno ] := ::aInfo[ AINFO_RECNO ]
         EndIf
      EndIf

      ::oSql:FreeStatement()

   EndIf

   Set( _SET_EXACT, uSet )

Return NIL

/*------------------------------------------------------------------------*/

Method ConvType( cData, cType, lPartialSeek, nThis, lLike )

   Local dRet, cD := "19600101"
   Local cD1 := "19600101 000000"

   DEFAULT lLike := .F.

   Do Case
   Case cType = "C"
      If len(cData) < nThis //.and. cData[-1] != " "
         lPartialSeek := .T.
      EndIf
   Case cType = "N"
      Return val( cData )
   Case cType = "D"
      If len(cData) < 8 .and. !empty( cData )
         ::nPartialDateSeek := len(cData)
         cData += SubStr( cD, len(cData) + 1, 8 - len(cData) )
         lPartialSeek     := .T.
         lLike        := .F.
      Else
         lPartialSeek := .F.
         lLike        := .F.
      EndIf
      dRet := stod( cData )
      Return dRet
   case ctype =="T"
   If len(cData) < 15 .and. !empty( cData )
         ::nPartialDateSeek := len(cData)
         cData += SubStr( cD1, len(cData) + 1, 15 - len(cData) )
         lPartialSeek     := .T.
         lLike        := .F.
      Else
         lPartialSeek := .F.
         lLike        := .F.
      EndIf
      dRet := stot( cData )
      Return dRet

   Case cType = "L"
      Return cData $ "SY.T."
   EndCase

Return If( lLike .and. lPartialSeek, rtrim( cData + "%"), cData )

/*------------------------------------------------------------------------*/

Static Function IsNull( cPar )

   If cPar == "NULL" .or. cPar == "0" .or. cPar == " "
      Return .T.
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

METHOD SetBOF() CLASS SR_WORKAREA

   ::aInfo[ AINFO_BOF ]   := .T.

Return NIL

/*------------------------------------------------------------------------*/

METHOD ReadPage( nDirection, lWasDel ) CLASS SR_WORKAREA

   Local i
   Local cJoin1, cJoin3, cTemp, aTemp, cSql := ""
   Local uRecord, nFecth, nBlockPos := 0, nPos
   Local nOldBg, nOldEnd, lCacheIsEmpty

   //-------- Paging cache

   If ::lCollectingBehavior
      For each i in ::aSelectList
         If i == 1
            ::lCollectingBehavior   := .F.
            Exit
         EndIf
      Next
   EndIf

   If abs(::aInfo[ AINFO_SKIPCOUNT ]) >= (::nCurrentFetch)
      If ::nCurrentFetch <= 16
         ::nCurrentFetch := Max( 60, ::nCurrentFetch * ::nCurrentFetch )
      Else
         ::nCurrentFetch += abs(::aInfo[ AINFO_SKIPCOUNT ]) * 3
      EndIf
      ::nCurrentFetch := min( ::nCurrentFetch, 500 )
   Else
      ::lCollectingBehavior   := .F.
      ::nCurrentFetch := Max( abs(::aInfo[ AINFO_SKIPCOUNT ]), 30 )
   EndIf

   //-------- Paging cache

   cJoin1 := " " + ::cQualifiedTableName + " A "
   cJoin3 := ::GetSelectList()

   Switch ::oSql:nSystemID
   Case SYSTEMID_INGRES
   Case SYSTEMID_INFORM
   Case SYSTEMID_IBMDB2
      if ::aInfo[ AINFO_REVERSE_INDEX ]
         cTemp := if( nDirection != ORD_DIR_FWD, ::WhereMajor(), ::WhereMinor() )
      Else
         cTemp := if( nDirection == ORD_DIR_FWD, ::WhereMajor(), ::WhereMinor() )
      EndIf
      cSql  := "SELECT" + eval( ::Optmizer_ns, ::nCurrentFetch ) + cJoin3 + "FROM" + cJoin1 + cTemp + ::OrderBy(NIL,nDirection == ORD_DIR_FWD) + eval( ::Optmizer_ne, ::nCurrentFetch ) +;
               if(::oSql:lComments," /* Skip " + if( nDirection == ORD_DIR_FWD,"FWD","BWD") + " */","")
      cSql := ::ParseIndexColInfo( cSQL )
      Exit

   Case SYSTEMID_POSTGR
      if ::aInfo[ AINFO_REVERSE_INDEX ]
         aTemp := if( nDirection != ORD_DIR_FWD, ::WherePgsMajor(), ::WherePgsMinor() )
      Else
         aTemp := if( nDirection == ORD_DIR_FWD, ::WherePgsMajor(), ::WherePgsMinor() )
      EndIf

      cTemp := "SELECT" + eval( ::Optmizer_ns, ::nCurrentFetch ) + cJoin3 + "FROM" + cJoin1

      For i = 1 to len( aTemp )
         cSql += "SELECT * FROM (" + cTemp + " WHERE " + aTemp[i] + ::OrderBy(NIL,nDirection == ORD_DIR_FWD) + eval( ::Optmizer_ne, ::nCurrentFetch ) + " ) TMP" + alltrim(str(i))
         If i != len( aTemp )
            cSql += CRLF + "UNION" + CRLF
         EndIf
      Next
      cSql := cSql + strtran(::OrderBy(NIL, nDirection == ORD_DIR_FWD ), "A.", "" ) + eval( ::Optmizer_ne, ::nCurrentFetch ) +;
              if(::oSql:lComments," /* Skip " + if( nDirection == ORD_DIR_FWD,"FWD","BWD") + " */","")
      Exit

   Case SYSTEMID_ORACLE
      If len(::aIndex) > 0 .and. ::aInfo[ AINFO_INDEXORD ] > 0 .and. ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] != NIL
         if ::aInfo[ AINFO_REVERSE_INDEX ]
            cTemp := if( nDirection != ORD_DIR_FWD, ::WhereVMajor(), ::WhereVMinor() )
            cSql  := 'SELECT /*+ INDEX( A ' + if( nDirection != ORD_DIR_FWD, 'A$', 'D$') + ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] + ") */ " + cJoin3 + "FROM" + cJoin1 + cTemp + " AND ROWNUM <= " + str(::nCurrentFetch+2)+ ' '+;
                    ::OrderBy(NIL,nDirection == ORD_DIR_FWD) +  if(::oSql:lComments," /* Skip " + if( nDirection == ORD_DIR_FWD,"FWD","BWD") + " */","")
         Else
            cTemp := if( nDirection == ORD_DIR_FWD, ::WhereVMajor(), ::WhereVMinor() )
            cSql  := 'SELECT /*+ INDEX( A ' + if( nDirection == ORD_DIR_FWD, 'A$', 'D$') + ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_NAME ] + ") */ " + cJoin3 + "FROM" + cJoin1 + cTemp + " AND ROWNUM <= " + str(::nCurrentFetch+2)+ ' '+;
                  ::OrderBy(NIL,nDirection == ORD_DIR_FWD) +    if(::oSql:lComments," /* Skip " + if( nDirection == ORD_DIR_FWD,"FWD","BWD") + " */","")
         EndIf
         Exit  // Leave this exist HERE !!!!
      EndIf

   Default
      if ::aInfo[ AINFO_REVERSE_INDEX ]
         cTemp := if( nDirection != ORD_DIR_FWD, ::WhereMajor(), ::WhereMinor() )
      Else
         cTemp := if( nDirection == ORD_DIR_FWD, ::WhereMajor(), ::WhereMinor() )
      EndIf
      cSql  := "SELECT" + eval( ::Optmizer_ns, ::nCurrentFetch ) + cJoin3 + "FROM" + cJoin1 + cTemp + ::OrderBy(NIL,nDirection == ORD_DIR_FWD) + eval( ::Optmizer_ne, ::nCurrentFetch ) +;
               if(::oSql:lComments," /* Skip " + if( nDirection == ORD_DIR_FWD,"FWD","BWD") + " */","")
      cSql := ::ParseIndexColInfo( cSQL )

   End

  ::oSql:Execute( cSql )

   If !(lWasDel .and. (!SR_UseDeleteds()))
      If (::oSql:nRetCode := ::oSql:Fetch( NIL, .F., ::aFields )) != SQL_SUCCESS
         Return NIL
      EndIf
   else
   EndIf

   ::oSql:nRetCode := ::oSql:Fetch( , .F., ::aFields )

   Switch ::oSql:nRetCode
   Case SQL_SUCCESS

      nOldBg               := ::aInfo[ AINFO_NCACHEBEGIN ]
      nOldEnd              := ::aInfo[ AINFO_NCACHEEND ]
      lCacheIsEmpty        := (nOldBg == nOldEnd) .and. nOldEnd == 0

      If nDirection = ORD_DIR_FWD

         nBlockPos := SR_FIXCACHEPOINTER( ::aInfo[ AINFO_NPOSCACHE ]+1 )

         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NPOSCACHE ] + ::nCurrentFetch

         If nOldBg == nOldEnd
            If ::aInfo[ AINFO_NCACHEEND ] > CAHCE_PAGE_SIZE * 3   // 3 pages...
               ::aInfo[ AINFO_NCACHEEND ] -= (CAHCE_PAGE_SIZE * 3)
            EndIf
            ::aInfo[ AINFO_EOF_AT ]   := 0
         ElseIf nOldBg < nOldEnd
            If ::aInfo[ AINFO_NCACHEEND ] > CAHCE_PAGE_SIZE * 3   // 3 pages...
               ::aInfo[ AINFO_NCACHEEND ] -= (CAHCE_PAGE_SIZE * 3)
               If ::aInfo[ AINFO_NCACHEEND ] >= (::aInfo[ AINFO_NCACHEBEGIN ] - 2)
                  ::aInfo[ AINFO_NCACHEBEGIN ] := ::aInfo[ AINFO_NCACHEEND ] + 2
               EndIf
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
         Else
            If ::aInfo[ AINFO_NCACHEEND ] >= (::aInfo[ AINFO_NCACHEBEGIN ] - 2)
               ::aInfo[ AINFO_NCACHEBEGIN ] := ::aInfo[ AINFO_NCACHEEND ] + 2
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
            If ::aInfo[ AINFO_NCACHEEND ] > CAHCE_PAGE_SIZE * 3   // 3 pages...
               ::aInfo[ AINFO_NCACHEEND ] -= (CAHCE_PAGE_SIZE * 3)
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
            If ::aInfo[ AINFO_NCACHEBEGIN ] > CAHCE_PAGE_SIZE * 3   // 3 pages...
               ::aInfo[ AINFO_NCACHEBEGIN ] -= (CAHCE_PAGE_SIZE * 3)
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf

         EndIf
         If ::aInfo[ AINFO_NCACHEBEGIN ] == 0
            ::aInfo[ AINFO_NCACHEBEGIN ] := 1
         EndIf
         If ::aInfo[ AINFO_NPOSCACHE ] == 0
            ::aInfo[ AINFO_NPOSCACHE ] := 1
         EndIf
         If ::aCache[nBlockPos] == NIL
            ::aCache[nBlockPos] := Array( len( ::aLocalBuffer ) )
         EndIf
         ::oSql:GetLine( ::aFields, .F., @::aCache[ nBlockPos ] )
         uRecord := ::aCache[ nBlockPos, ::hnRecno ]
         If ::aInfo[ AINFO_NPOSCACHE ] == 0
            ::aInfo[ AINFO_NPOSCACHE ] := 1
         EndIf
         nPos := ::aInfo[ AINFO_NPOSCACHE ] + if( lCacheIsEmpty, 0, 1 )
         If nPos > (CAHCE_PAGE_SIZE * 3)
            nPos := 1
         EndIf

      ElseIf nDirection = ORD_DIR_BWD

         nBlockPos := SR_FIXCACHEPOINTER( ::aInfo[ AINFO_NPOSCACHE ] - 1 )

         ::aInfo[ AINFO_NCACHEBEGIN ] := ::aInfo[ AINFO_NPOSCACHE ] - ::nCurrentFetch
         If nOldBg == nOldEnd
            If ::aInfo[ AINFO_NCACHEBEGIN ] < 1
               ::aInfo[ AINFO_NCACHEBEGIN ] += (CAHCE_PAGE_SIZE * 3)
            EndIf
            ::aInfo[ AINFO_EOF_AT ]   := 0

         ElseIf nOldBg < nOldEnd
            If ::aInfo[ AINFO_NCACHEBEGIN ] < 1
               ::aInfo[ AINFO_NCACHEBEGIN ] += (CAHCE_PAGE_SIZE * 3)
               If (::aInfo[ AINFO_NCACHEEND ] + 2) >= ::aInfo[ AINFO_NCACHEBEGIN ]
                  ::aInfo[ AINFO_NCACHEEND ]  := ::aInfo[ AINFO_NCACHEBEGIN ] - 2
               EndIf
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
         Else
            If (::aInfo[ AINFO_NCACHEEND ] + 2) >= ::aInfo[ AINFO_NCACHEBEGIN ]
               ::aInfo[ AINFO_NCACHEEND ]  := ::aInfo[ AINFO_NCACHEBEGIN ] - 2
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
            If ::aInfo[ AINFO_NCACHEBEGIN ] < 1
               ::aInfo[ AINFO_NCACHEBEGIN ] += (CAHCE_PAGE_SIZE * 3)
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
            If ::aInfo[ AINFO_NCACHEEND ] < 1
               ::aInfo[ AINFO_NCACHEEND ] += (CAHCE_PAGE_SIZE * 3)
               ::aInfo[ AINFO_EOF_AT ]   := 0
               ::aInfo[ AINFO_BOF_AT ]   := 0
            EndIf
         EndIf

         If ::aInfo[ AINFO_NCACHEEND ] == 0
            ::aInfo[ AINFO_NCACHEEND ] := (CAHCE_PAGE_SIZE * 3)
         EndIf
         If ::aInfo[ AINFO_NPOSCACHE ] == 0
            ::aInfo[ AINFO_NPOSCACHE ] := (CAHCE_PAGE_SIZE * 3)
         EndIf
         If ::aCache[nBlockPos] == NIL
            ::aCache[nBlockPos] := Array( len( ::aLocalBuffer ) )
         EndIf
         ::oSql:GetLine( ::aFields, .F., @::aCache[nBlockPos] )
         uRecord := ::aCache[ nBlockPos, ::hnRecno ]
         nPos := ::aInfo[ AINFO_NPOSCACHE ] - if( lCacheIsEmpty, 0, 1 )
         If nPos < 1
            nPos := (CAHCE_PAGE_SIZE * 3)
         EndIf

      Else
         ::aInfo[ AINFO_NPOSCACHE ] := ::aInfo[ AINFO_NCACHEBEGIN ] := ::aInfo[ AINFO_NCACHEEND ] := nBlockPos := 1
         ::oSql:GetLine( ::aFields, .F., @::aCache[1] )
         uRecord   := ::aCache[ nBlockPos, ::hnRecno ]
      EndIf

      If nDirection = ORD_DIR_FWD .or. nDirection = ORD_DIR_BWD
         For nFecth = 1 to ::nCurrentFetch
            ::oSql:nRetCode := ::oSql:Fetch( NIL, .F., ::aFields )
            If ::oSql:nRetCode != SQL_SUCCESS
               If ::oSql:nRetCode == SQL_ERROR
                  DEFAULT ::cLastComm := ::oSql:cLastComm
                  ::RunTimeErr("999", "[FetchLine Failure][" + alltrim(str(::oSql:nRetCode)) + "] " + ::oSql:LastError() + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
               EndIf
               If nDirection = ORD_DIR_FWD
                  ::aInfo[ AINFO_EOF_AT ]    := uRecord
                  ::aInfo[ AINFO_NCACHEEND ] := nPos
               Else
                  ::aInfo[ AINFO_BOF_AT ]      := uRecord
                  ::aInfo[ AINFO_NCACHEBEGIN ] := nPos
               EndIf

               exit
            Endif
            If nDirection = ORD_DIR_FWD
               nPos ++
               If nPos > (CAHCE_PAGE_SIZE * 3)
                  nPos -= (CAHCE_PAGE_SIZE * 3)
               EndIf
            Else
               nPos --
               If nPos < 1
                  nPos += (CAHCE_PAGE_SIZE * 3)
               EndIf
            EndIf

            ::oSql:GetLine( ::aFields, .F., @::aCache[nPos] )
            uRecord := ::aCache[nPos,::hnRecno]
            If ::lFetchAll
               aadd( ::aFetch, uRecord )
            EndIf
         Next
      EndIf
      If ::aCache[ ::aInfo[ AINFO_NPOSCACHE ] ] != NIL
         ::GetBuffer()     // Loads current cache position to record buffer
      EndIf
      Exit

   Case SQL_NO_DATA_FOUND

      ::lNoData := .T.

      If nDirection = ORD_DIR_BWD
         ::aInfo[ AINFO_BOF_AT ] := ::aInfo[ AINFO_RECNO ]
      ElseIf nDirection = ORD_DIR_FWD
         ::aInfo[ AINFO_EOF_AT ] := ::aInfo[ AINFO_RECNO ]
         ::GetBuffer(.T.)         // Clean Buffer
      Else
         ::GetBuffer(.T.)         // Clean Buffer
      EndIf

      Exit
   Default
      ::lNoData := .T.
      DEFAULT ::cLastComm := ::oSql:cLastComm
      ::RunTimeErr("999", "[Fetch Failure/First][" + alltrim(str(::oSql:nRetCode)) + "] " + ::oSql:LastError() + chr(13)+chr(10)+ chr(13)+chr(10)+"Last command sent to database : " + chr(13)+chr(10) + ::cLastComm )
   End

   ::oSql:FreeStatement()

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlRecall() CLASS SR_WORKAREA

   Local nRecno := ::aInfo[ AINFO_RECNO ]

   ::sqlGoCold()

   If ::lCanDel .and. SR_UseDeleteds()
      If ::hnDeleted > 0
         If ::nTCCompat >= 4
            If  (::oSql:Execute( ::cUpd + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " = ' ', R_E_C_D_E_L_ = 0 " + ::WhereEqual(), , ::nLogMode ) ) != SQL_SUCCESS
               ::RuntimeErr( "13", SR_Msg(13) + ::oSql:LastError() + chr(13)+chr(10)+;
                        SR_Msg(14) + chr(13)+chr(10) + ::oSql:cLastComm )
               Return .F.
            EndIf
         Else
            If  (::oSql:Execute( ::cUpd + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " = ' ' " + ::WhereEqual(), , ::nLogMode ) ) != SQL_SUCCESS
               ::RuntimeErr( "13", SR_Msg(13) + ::oSql:LastError() + chr(13)+chr(10)+;
                        SR_Msg(14) + chr(13)+chr(10) + ::oSql:cLastComm )
               Return .F.
            EndIf
         EndIf

         ::oSql:FreeStatement()
         ::aInfo[ AINFO_DELETED ]             := .F.
         ::aLocalBuffer[ ::hnDeleted ]        := " "

         ::aCache[ ::aInfo[ AINFO_NPOSCACHE ], ::hnDeleted ] := " "
         _SR_ScanExec( Self, { |x| If( x:nThisArea != ::nThisArea, ::CheckCache(x),)} )
      EndIf
   Else
      SR_MsgLogFile( SR_Msg(12) + ::cFileName )
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlPack() CLASS SR_WORKAREA

   local nRet

   ::sqlGoCold()

   If ::lCanDel
      If ::hnDeleted > 0
         If ::nTCCompat >= 2
            nRet := ::oSql:Execute( ::cDel + " WHERE " + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " = '*'", , ::nLogMode )
            If  nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO .and. nRet != SQL_NO_DATA_FOUND
               ::RuntimeErr( "13", SR_Msg(13) + ::oSql:LastError() + chr(13)+chr(10)+;
                        SR_Msg(14) + chr(13)+chr(10) + ::oSql:cLastComm )
               Return .F.
            EndIf
         Else
            nRet := ::oSql:Execute( ::cDel + " WHERE " + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " = 'T'", , ::nLogMode )
            If  nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO .and. nRet != SQL_NO_DATA_FOUND
               ::RuntimeErr( "13", SR_Msg(13) + ::oSql:LastError() + chr(13)+chr(10)+;
                        SR_Msg(14) + chr(13)+chr(10) + ::oSql:cLastComm )
               Return .F.
            EndIf
         EndIf

         ::oSql:FreeStatement()
         ::Refresh()

      EndIf
   Else
      SR_MsgLogFile( SR_Msg(12) + ::cFileName )
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlDeleteRec() CLASS SR_WORKAREA

   Local nRecno := ::aInfo[ AINFO_RECNO ],nRet

   ::sqlGoCold()

   If ::lCanDel
      If !::aInfo[ AINFO_DELETED ]
         If ::hnDeleted > 0 .and. SR_UseDeleteds()
            If ::nTCCompat >= 2
               If ::nTCCompat >= 4
                  If  (nRet := ::oSql:Execute( ::cUpd + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " = '*', R_E_C_D_E_L_ = R_E_C_N_O_ " + ::WhereEqual(), , ::nLogMode ) ) != SQL_SUCCESS .and.;
                      nRet != SQL_NO_DATA_FOUND .and. nRet != SQL_SUCCESS_WITH_INFO
                     ::RuntimeErr( "13", SR_Msg(13) + ::oSql:LastError() + chr(13)+chr(10)+;
                              SR_Msg(14) + chr(13)+chr(10) + ::oSql:cLastComm )
                     Return .F.
                  EndIf
               Else
                  If  (nRet := ::oSql:Execute( ::cUpd + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " = '*' " + ::WhereEqual(), , ::nLogMode ) ) != SQL_SUCCESS .and.;
                      nRet != SQL_NO_DATA_FOUND .and. nRet != SQL_SUCCESS_WITH_INFO
                     ::RuntimeErr( "13", SR_Msg(13) + ::oSql:LastError() + chr(13)+chr(10)+;
                              SR_Msg(14) + chr(13)+chr(10) + ::oSql:cLastComm )
                     Return .F.
                  EndIf
               EndIf
            Else
               If  (nRet := ::oSql:Execute( ::cUpd + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " = 'T' " + ::WhereEqual(), , ::nLogMode ) ) != SQL_SUCCESS .and.;
                   nRet != SQL_NO_DATA_FOUND .and. nRet != SQL_SUCCESS_WITH_INFO
                  ::RuntimeErr( "13", SR_Msg(13) + ::oSql:LastError() + chr(13)+chr(10)+;
                           SR_Msg(14) + chr(13)+chr(10) + ::oSql:cLastComm )
                  Return .F.
               EndIf
            EndIf

            ::oSql:FreeStatement()
            ::aInfo[ AINFO_DELETED ]      := .T.
            ::aLocalBuffer[ ::hnDeleted ] := if( ::nTCCompat > 0, "*", "T" )

            If ::aInfo[ AINFO_NPOSCACHE ] > 0
               ::aCache[ ::aInfo[ AINFO_NPOSCACHE ], ::hnDeleted ] :=  ::aLocalBuffer[ ::hnDeleted ]
               _SR_ScanExec( Self, { |x| If( x:nThisArea != ::nThisArea, ::CheckCache(x),)} )
            EndIf

         Else
            If  (::oSql:Execute( ::cDel + ::WhereEqual(), , ::nLogMode ) ) != SQL_SUCCESS
               ::RuntimeErr( "13", SR_Msg(13) + ::oSql:LastError() + chr(13)+chr(10)+;
                        SR_Msg(14) + chr(13)+chr(10) + ::oSql:cLastComm )
               Return .F.
            EndIf

            ::oSql:FreeStatement()

            If nRecno == ::aInfo[ AINFO_EOF_AT ]
/*
               nPos := ::aInfo[ AINFO_NPOSCACHE ] - 1
               If nPos > CAHCE_PAGE_SIZE
                  nPos := 1
               EndIf
               If ( ::aInfo[ AINFO_NCACHEBEGIN ] < ::aInfo[ AINFO_NCACHEEND ] .and. ( nPos < ::aInfo[ AINFO_NCACHEEND ] .and. nPos >= ::aInfo[ AINFO_NCACHEBEGIN ] ) ) .or.;
                  ( ::aInfo[ AINFO_NCACHEBEGIN ] > ::aInfo[ AINFO_NCACHEEND ] .and. ( nPos >= ::aInfo[ AINFO_NCACHEBEGIN ] .or. nPos < ::aInfo[ AINFO_NCACHEEND ] ))
                  ::aInfo[ AINFO_EOF_AT ] := ::aCache[nPos, ::hnRecno]
               EndIf
*/
               ::aInfo[ AINFO_EOF_AT ] := 0
            EndIf

            If nRecno == ::aInfo[ AINFO_BOF_AT ]
/*
               nPos := ::aInfo[ AINFO_NPOSCACHE ] + 1
               If nPos > CAHCE_PAGE_SIZE
                  nPos := 1
               EndIf
               If ( ::aInfo[ AINFO_NCACHEBEGIN ] < ::aInfo[ AINFO_NCACHEEND ] .and. ( nPos <= ::aInfo[ AINFO_NCACHEEND ] .and. nPos > ::aInfo[ AINFO_NCACHEBEGIN ] ) ) .or.;
                  ( ::aInfo[ AINFO_NCACHEBEGIN ] > ::aInfo[ AINFO_NCACHEEND ] .and. ( nPos > ::aInfo[ AINFO_NCACHEBEGIN ] .or. nPos <= ::aInfo[ AINFO_NCACHEEND ] ))
                  ::aInfo[ AINFO_BOF_AT ] := ::aCache[nPos, ::hnRecno]
               EndIf
*/
               ::aInfo[ AINFO_EOF_AT ] := 0

            EndIf

            If ::hnDeleted > 0
               ::aLocalBuffer[ ::hnDeleted ] := if( ::nTCCompat > 0, "*", "T" )
            EndIf

            If ::aInfo[ AINFO_NPOSCACHE ] != 0
               ::aCache[ ::aInfo[ AINFO_NPOSCACHE ] ] := NIL
            EndIf

            ::aInfo[ AINFO_DELETED ]      := .T.

         EndIf
      EndIf
   Else
      SR_MsgLogFile( SR_Msg(12) + ::cFileName )
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlFlush() CLASS SR_WORKAREA
   ::sqlGoCold()
   If ::lCanICommitNow()
      ::oSql:Commit()
   EndIf
Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlClose() CLASS SR_WORKAREA
   If ::oSql != NIL
      ::sqlFlush()      /* commit when close WA */
      If ::nThisArea > 0
         If (!::lCreating) //
            _SR_UnRegister(Self)
         EndIf
         If ::lTableLocked
            ::UnlockTable(.t.)
         EndIf
         If ::lSharedLock .and. ::lOpened
            SR_ReleaseLocks( SHARED_TABLE_LOCK_SIGN + UPPER(::cFileName), ::oSql )
         EndIf
      EndIf
   EndIf

   If ++nOperat > 100
      hb_gcAll(.t.)
      nOperat := 0
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlCreate( aStruct, cFileName, cAlias, nArea ) CLASS SR_WORKAREA

   Local i, nConnection, aRet, aRec, aPK, aCreate, cSql, cField, lNotNull
   Local lPrimary, lRet, cTblName, nRet, nRowSize := 0, cRowSize, aMultilang := {}, aField
   Local cLobs := ""
   Local lRecnoAdded := .F., lShared := .F.
   Local aCacheInfo  := Array( CACHEINFO_LEN ), nPos
   Local nMax := 0

   ::cRecnoName   := SR_RecnoName()
   ::cDeletedName := SR_DeletedName()

   AsizeAlloc( ::aFetch, 50 )

   If ::cWSID == NIL
      ::cWSID := SR_GetUniqueSystemID()
   EndIf

   aRet := eval( SR_GetTableInfoBlock(), cFileName )

   aSize( aRet, TABLE_INFO_SIZE )

   If aRet[ TABLE_INFO_CONNECTION ] != NIL
      nConnection := aRet[ TABLE_INFO_CONNECTION ]
   EndIf

   If ::nCnt == NIL
      ::nCnt := 1
   EndIf

   DEFAULT cAlias := "SQLRDD_SYS_WA_" + StrZero(++::nCnt,3)

   If ::nCnt >= 998
      ::nCnt := 1
   EndIf

   ::nThisArea  := nArea
   ::cAlias     := cAlias
   ::aInfo[ AINFO_SHARED ]   := .F.
   ::cOriginalFN := upper(alltrim(cFileName))
   ::lGoTopOnFirstInteract := lGoTopOnFirstInteract

   If !::aInfo[ AINFO_SHARED ]
      ::lQuickAppend := .T.
   EndIf

   If SR_GetFastOpen()
      ::aInfo[ AINFO_SHARED ] := .T.
   EndIf

   ::cFileName  := SR_ParseFileName( aRet[ TABLE_INFO_TABLE_NAME ] )
   ::aFilters   := aRet[ TABLE_INFO_FILTERS ]
   ::cColPK     := alltrim(upper(aRet[ TABLE_INFO_PRIMARY_KEY ]))
   ::nRelacType := aRet[ TABLE_INFO_RELATION_TYPE ]
   ::cOwner     := aRet[ TABLE_INFO_OWNER_NAME ]
   ::cCustomSQL := aRet[ TABLE_INFO_CUSTOM_SQL ]

   ::lHistoric  := aRet[ TABLE_INFO_HISTORIC ] .or. SR_SetCreateAsHistoric()

   ::lCanUpd := aRet[ TABLE_INFO_CAN_UPDATE ]
   ::lCanIns := aRet[ TABLE_INFO_CAN_INSERT ]
   ::lCanDel := aRet[ TABLE_INFO_CAN_DELETE ]

   ::lOpened   := .T.
   ::lCreating := .T.

   If aRet[ TABLE_INFO_RECNO_NAME ] != NIL
      ::cRecnoName := Upper(alltrim(aRet[ TABLE_INFO_RECNO_NAME ]))
   EndIf

   If aRet[ TABLE_INFO_DELETED_NAME ] != NIL
      ::cDeletedName := Upper(alltrim(aRet[ TABLE_INFO_DELETED_NAME ]))
   EndIf

   ::oSql    := SR_GetConnection( nConnection )

   If aRet[ TABLE_INFO_ALL_IN_CACHE ] == NIL
      aRet[ TABLE_INFO_ALL_IN_CACHE ] := ::oSql:lAllInCache
   EndIf

   ::lISAM      := .T.     // !aRet[ TABLE_INFO_ALL_IN_CACHE ]

   // NewAge compatible rights string...

   ::cRights := "S"+if(::lCanUpd,"U","")+if(::lCanIns,"I","")+if(::lCanDel,"D","")+ltrim(strzero(::nRelacType,1))

   If !Empty( cGlobalOwner )
      ::cOwner := alltrim( cGlobalOwner )
   ElseIf !Empty( ::oSql:cOwner )
      ::cOwner := alltrim(::oSql:cOwner)
   EndIf

   If (!Empty( ::cOwner )) .and. ::cOwner[-1] != "."
      ::cOwner += "."
   EndIf

   ::cQualifiedTableName := ::cOwner + SR_DBQUALIFY( ::cFileName, ::oSql:nSystemID )

   //nPos := HGetPos( ::oSql:aTableInfo, ::cOriginalFN )
   //If nPos > 0
      // REMOVE from cache
      //HDelAt( ::oSql:aTableInfo, nPos )
   //EndIf

   If len( ::cFileName ) > MAX_TABLE_NAME_LENGHT
      cTblName := subStr( ::cFileName, 1, MAX_TABLE_NAME_LENGHT )
      ::cFileName := cTblName
   Else
      cTblName := ::cFileName
   EndIf
   
   nPos := HGetPos( ::oSql:aTableInfo, ::cFileName ) //dbcopy probably pass wrong second parameter - cFileName
   //nPos := HGetPos( ::oSql:aTableInfo, ::cOriginalFN )
      
   If nPos > 0
      // REMOVE from cache
      HDelAt( ::oSql:aTableInfo, nPos )
   EndIf      

   ::lCanSel := .T.

   ::cDel    := "DELETE FROM " + ::cQualifiedTableName + " "
   ::cUpd    := "UPDATE " + ::cQualifiedTableName + " SET "

   // Release any pending transaction before a DML command

   ::oSql:Commit()
   ::oSql:nTransacCount := 0

   If ::oSql:nSystemID == SYSTEMID_SYBASE
      ::oSql:SetOptions( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON )
   EndIf

   /* Drop the table */

   If ::oSql:nSystemID == SYSTEMID_POSTGR
      ::oSql:exec( "DROP TABLE " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + " CASCADE" + if(::oSql:lComments," /* create table */",""), .F. )
   Else
      ::oSql:exec( "DROP TABLE " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + if( ::oSql:nSystemID == SYSTEMID_ORACLE, " CASCADE CONSTRAINTS","") + if(::oSql:lComments," /* create table */",""), .F. )
   EndIf
   ::oSql:Commit()

   /* Catalogs cleanup */

   ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + UPPER(::cFileName) + "'" + if(::oSql:lComments," /* Wipe index info */",""), .F. )
   ::oSql:Commit()
   ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTTABLES WHERE TABLE_ = '" + UPPER(::cFileName) + "'" + if(::oSql:lComments," /* Wipe table info */",""), .F. )
   ::oSql:Commit()
   ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLANG WHERE TABLE_ = '" + UPPER(::cFileName) + "'" + if(::oSql:lComments," /* Wipe table info */",""), .F. )
   ::oSql:Commit()
   ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS WHERE TABLE_ = '" + UPPER(::cFileName) + "'" + if(::oSql:lComments," /* Wipe table info */",""), .F. )
   ::oSql:Commit()
   ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS WHERE SOURCETABLE_ = '" + UPPER(::cFileName) + "'" + if(::oSql:lComments," /* Wipe table info */",""), .F. )
   ::oSql:Commit()
   ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRTGTCOLS WHERE SOURCETABLE_ = '" + UPPER(::cFileName) + "'" + if(::oSql:lComments," /* Wipe table info */",""), .F. )
   ::oSql:Commit()
   ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRSRCCOLS WHERE SOURCETABLE_ = '" + UPPER(::cFileName) + "'" + if(::oSql:lComments," /* Wipe table info */",""), .F. )
   ::oSql:Commit()

   If ::oSql:exec( "SELECT * FROM " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + if(::oSql:lComments," /* check dropped table */",""), .F. ) = SQL_SUCCESS
      ::oSql:commit()
      ::oSql:exec( "DROP TABLE " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + if( ::oSql:nSystemID == SYSTEMID_ORACLE, " CASCADE CONSTRAINTS","") + if(::oSql:lComments," /* create table */",""), .T. )
      ::oSql:Commit()
      If ::oSql:exec( "SELECT * FROM " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + if(::oSql:lComments," /* check dropped table */",""), .F. ) = SQL_SUCCESS
         SR_MsgLogFile( "Could not drop existing table " + cTblName + " in dbCreate()" )
         ::lOpened := .F.
         If ::oSql:nSystemID == SYSTEMID_SYBASE
            ::oSql:SetOptions( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF )
         EndIf
         Return Self
      EndIf
   EndIf
   ::oSql:Commit()

   /* If Postgres, create SEQUENCE per table */

   If ::oSql:nSystemID == SYSTEMID_POSTGR
      ::oSql:Commit()
      ::oSql:exec( "DROP SEQUENCE " + ::cOwner + LimitLen(::cFileName,3) + "_SQ", .F. )
      ::oSql:Commit()
      ::oSql:exec( "CREATE SEQUENCE " + ::cOwner + LimitLen(::cFileName,3) + "_SQ START 1" )
      ::oSql:Commit()
   EndIf

   /* Create the new table */

   aPK     := {}
   aCreate := {}

   For each aRec in aStruct

      aSize( aRec, FIELD_INFO_SIZE )

      aRec[FIELD_NAME] := alltrim( upper( aRec[FIELD_NAME] ) )
      aRec[FIELD_TYPE] := alltrim( upper( aRec[FIELD_TYPE] ) )

      If aRec[FIELD_NULLABLE] == NIL .or. valtype( aRec[FIELD_NULLABLE] ) != "L"
         aRec[FIELD_NULLABLE]    := .T.
      EndIf
      If aRec[FIELD_UNIQUE] == NIL
         aRec[FIELD_UNIQUE]      := .F.
      EndIf
      If aRec[FIELD_PRIMARY_KEY] == NIL
         aRec[FIELD_PRIMARY_KEY] := 0
      EndIf
      If aRec[FIELD_MULTILANG] == NIL
         aRec[FIELD_MULTILANG]   := MULTILANG_FIELD_OFF
      EndIf

      If !("*" + aRec[FIELD_NAME] + "*") $ "*"+::cRecnoName+"*" //DT__HIST*"
         If aRec[ FIELD_PRIMARY_KEY ] != 0
            aadd( aPk, { aRec[ FIELD_PRIMARY_KEY ], aRec[FIELD_NAME] } )
         EndIf
         aadd( aCreate, aRec )
      EndIf
   Next

   AADD( aCreate, { ::cRecnoName, "N", 15, 0, .F., , MULTILANG_FIELD_OFF, , , 0, .F. } )

   If SR_UseDeleteds()
      AADD( aCreate, { ::cDeletedName, "C", 1, 0, if( ::oSql:nSystemID == SYSTEMID_ORACLE, .T., .F. ), , MULTILANG_FIELD_OFF, , , 0, .F. } )
   EndIf



   If ::lHistoric
      AADD( aCreate, { "DT__HIST", "D", 8, 0, .T., , MULTILANG_FIELD_OFF, , , 0, .F. } )
   EndIf

   cSql := "CREATE TABLE " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + " ( "

   For i = 1 to len( aCreate )

      cField   := Upper(Alltrim(aCreate[ i, FIELD_NAME ]))
      lPrimary := aCreate[ i, FIELD_PRIMARY_KEY ] > 0
      nRowSize += aCreate[ i, FIELD_LEN]

      If aCreate[i,FIELD_MULTILANG] .and. aCreate[i,FIELD_TYPE] $ "MC" .and. SR_SetMultiLang()
         aadd( aMultilang, aClone( aCreate[i] ) )
         aCreate[i,FIELD_TYPE] := "M"
      EndIf

      cSql := cSql + "   " + SR_DBQUALIFY( cField, ::oSql:nSystemID )
      cSql += " "

      //lNotNull := (!aCreate[i,FIELD_NULLABLE]) .or. lPrimary
      lNotNull := (aCreate[i,FIELD_NULLABLE] !=NIL .and. valtype(aCreate[i,FIELD_NULLABLE]) == "L" .and. !aCreate[i,FIELD_NULLABLE]) .or. lPrimary

      Do Case
      Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
         If (aCreate[i,FIELD_LEN] > 30)
               cSql := cSql + "VARCHAR2(" + ltrim(str(min(aCreate[i,FIELD_LEN],4000),9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
         Else
               if aCreate[i,FIELD_LEN] > nMininumVarchar2Size .and.  nMininumVarchar2Size < 30
                  cSql := cSql + "VARCHAR(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
               else
               cSql := cSql + "CHAR(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
         EndIf
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "C" .or. aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_SQLBAS
         If (aCreate[i,FIELD_LEN] > 254) .or. aCreate[i,FIELD_TYPE] == "M"
            cSql := cSql + "LONG VARCHAR"
         Else
            cSql := cSql + "VARCHAR(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lPrimary, " NOT NULL", "" )
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "C") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7  .OR. ::oSql:nSystemID == SYSTEMID_AZURE .or. ::oSql:nSystemID == SYSTEMID_POSTGR .or. ::oSql:nSystemID == SYSTEMID_CACHE  .or. ::oSql:nSystemID == SYSTEMID_ADABAS )
         If ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_AZURE
               IF  ::OSQL:lSqlServer2008 .AND. SR_Getsql2008newTypes()
                  IF  aCreate[i,FIELD_LEN] > 10
                     cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + if(!Empty(SR_SetCollation()), "COLLATE " + SR_SetCollation() + " " , "")  + IF(lNotNull, " NOT NULL", "")
                  ELSE
                     cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + if(!Empty(SR_SetCollation()), "COLLATE " + SR_SetCollation() + " " , "")  + IF(lNotNull, " NOT NULL", "")
                  ENDIF
               ELSE
                  cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + if(!Empty(SR_SetCollation()), "COLLATE " + SR_SetCollation() + " " , "")  + IF(lNotNull, " NOT NULL", "")
               ENDIF
         ElseIf ::oSql:nSystemID == SYSTEMID_POSTGR .and. aCreate[i,FIELD_LEN] > nMininumVarchar2Size -1 //10
            cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
         Else
            cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
         EndIf

         If ::oSql:nSystemID == SYSTEMID_POSTGR .and. cField == ::cDeletedName
            cSql += " default ' '"
         EndIf

      Case aCreate[i,FIELD_TYPE] == "C" .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
         If aCreate[i,FIELD_LEN] > 255
            cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
         Else
            cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "C") .and. (::oSql:nSystemID == SYSTEMID_IBMDB2)
         If aCreate[i,FIELD_LEN] > 128
            cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
         Else
            cSql := cSql + "CHARACTER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_INGRES
         cSql := cSql + "varchar(" + LTrim(Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lPrimary, "NOT NULL PRIMARY KEY", IF(lNotNull, " NOT NULL", ""))

      Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_INFORM
         cSql := cSql + "CHARACTER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lPrimary, "NOT NULL PRIMARY KEY", IF(lNotNull, " NOT NULL", ""))

      Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_ACCESS
         If (aCreate[i,FIELD_LEN] > 254)
            cSql := cSql + "TEXT"
         Else
            cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")"
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
         If (aCreate[i,FIELD_LEN] > 254)
            cSql := cSql + "LONG VARCHAR "
         Else
            cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + "  " + IF(lNotNull, " NOT NULL", "")
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "C") .and. (::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
         If (aCreate[i,FIELD_LEN] > 254)
            cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF( !Empty( ::oSql:cCharSet ), " CHARACTER SET " + ::oSql:cCharSet, "") + IF(lNotNull, " NOT NULL", "")
         Else
            cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF( !Empty( ::oSql:cCharSet ), " CHARACTER SET " + ::oSql:cCharSet, "")  + IF(lNotNull, " NOT NULL", "")
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
         If (aCreate[i,FIELD_LEN] > 30)
            cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
         Else
            cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "D") .and. (::oSql:nSystemID == SYSTEMID_ORACLE .or. ::oSql:nSystemID == SYSTEMID_SQLBAS .or. ::oSql:nSystemID == SYSTEMID_INFORM .or. ::oSql:nSystemID == SYSTEMID_INGRES .or. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) .or. ::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3 .or. ::oSql:nSystemID == SYSTEMID_CACHE)
         cSql := cSql + "DATE"

      Case (aCreate[i,FIELD_TYPE] == "D") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
         cSql := cSql + "DATETIME"

      Case (aCreate[i,FIELD_TYPE] == "D") .and. (::oSql:nSystemID == SYSTEMID_IBMDB2 .or. ::oSql:nSystemID == SYSTEMID_POSTGR  .or. ::oSql:nSystemID == SYSTEMID_ADABAS)
         cSql := cSql + "DATE"

      Case (aCreate[i,FIELD_TYPE] == "D") .and. (::oSql:nSystemID == SYSTEMID_ACCESS .or. ::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_AZURE)
         IF (::oSql:nSystemID == SYSTEMID_MSSQL7  )  .AND. ::OSQL:lSqlServer2008 .AND. SR_Getsql2008newTypes()
            cSql := cSql + 'DATE NULL '
         ELSE

         cSql := cSql + "DATETIME NULL"
         ENDIF

      Case (aCreate[i,FIELD_TYPE] == "D") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
         cSql := cSql + "TIMESTAMP"

      Case (aCreate[i,FIELD_TYPE] == "L") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_AZURE .or. ::oSql:nSystemID == SYSTEMID_CACHE)
         cSql := cSql + "BIT"

      Case (aCreate[i,FIELD_TYPE] == "L") .and. (::oSql:nSystemID == SYSTEMID_POSTGR  .or. ::oSql:nSystemID == SYSTEMID_ADABAS .or. ::oSql:nSystemID == SYSTEMID_FIREBR3 )
         cSql := cSql + "BOOLEAN"

      Case (aCreate[i,FIELD_TYPE] == "L") .and. (( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) )
         cSql := cSql + "TINYINT"

      Case (aCreate[i,FIELD_TYPE] == "L") .and. (::oSql:nSystemID == SYSTEMID_IBMDB2 .or. ::oSql:nSystemID == SYSTEMID_FIREBR)
         cSql := cSql + "SMALLINT"

      Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
         cSql := cSql + "BIT NOT NULL"

      Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
         cSql := cSql + "NUMERIC (1) NULL"

      Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
         cSql := cSql + "SMALLINT"

      Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_INFORM
         cSql := cSql + "BOOLEAN"

      Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_INGRES
         cSql := cSql + "tinyint"

      Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
         cSql := cSql + "CLOB"
         cLobs += if(empty(cLobs),"",",") + SR_DBQUALIFY( cField, ::oSql:nSystemID )

      Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_IBMDB2
         cSql := cSql + "CLOB (256000) " + If( "DB2/400" $ ::oSql:cSystemName, "",  " NOT LOGGED COMPACT" )

      Case (aCreate[i,FIELD_TYPE] == "M") .and. (::oSql:nSystemID == SYSTEMID_POSTGR  .and. aCreate[i,FIELD_LEN] == 4)
         cSql := cSql + 'XML'


      Case (aCreate[i,FIELD_TYPE] == "M") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7  .or. ::oSql:nSystemID == SYSTEMID_AZURE .OR. ::oSql:nSystemID == SYSTEMID_POSTGR .OR. ::oSql:nSystemID == SYSTEMID_INFORM .or. ::oSql:nSystemID == SYSTEMID_CACHE)
         cSql := cSql + "TEXT"

      Case (aCreate[i,FIELD_TYPE] == "M") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
         cSql := cSql + cMySqlMemoDataType

      Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_ADABAS
         cSql := cSql + "LONG"

      Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_INGRES
         cSql := cSql + "long varchar"

      Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_ACCESS
         cSql := cSql + "TEXT NULL"

      Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
         cSql := cSql + "TEXT"

      Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
         cSql := cSql + "LONG VARCHAR"

      Case (aCreate[i,FIELD_TYPE] == "M") .and. (::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
         cSql := cSql + "BLOB SUB_TYPE 1" + IF( !Empty( ::oSql:cCharSet ), " CHARACTER SET " + ::oSql:cCharSet, "")

      Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .OR. ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_AZURE) .and. cField == ::cRecnoName
         If ::oSql:lUseSequences
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") IDENTITY"
         Else
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL UNIQUE"
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_CACHE .and. cField == ::cRecnoName
         cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") UNIQUE " + [default objectscript '##class(] + SR_GetToolsOwner() + [SequenceControler).NEXTVAL("] + ::cFileName + [")']

      Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .OR. ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_CACHE .or. ::oSql:nSystemID == SYSTEMID_AZURE)
         cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str (aCreate[i,FIELD_DEC])) + ") " + IF(lNotNull, " NOT NULL ", "")

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_POSTGR .and. cField == ::cRecnoName
         cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") default (nextval('" + ::cOwner + LimitLen(::cFileName,3) + "_SQ')) NOT NULL UNIQUE"
      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_POSTGR
         cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str (aCreate[i,FIELD_DEC])) + ")  default 0 " + IF(lNotNull, " NOT NULL ", "")

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) .and. cField == ::cRecnoName
         cSql := cSql + "BIGINT (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") NOT NULL UNIQUE AUTO_INCREMENT "
      Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
         cSql := cSql + cMySqlNumericDataType + " (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str (aCreate[i,FIELD_DEC])) + ") " + IF(lNotNull, " NOT NULL ", "")

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ORACLE .and. cField == ::cRecnoName
         cSql := cSql + "NUMBER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" +;
                 IF(lNotNull, " NOT NULL UNIQUE USING INDEX ( CREATE INDEX " + ::cOwner + LimitLen(::cFileName,3) + "_UK ON " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + "( " + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + ")" +;
                 IF(Empty(SR_SetTblSpaceIndx()), "", " TABLESPACE " + SR_SetTblSpaceIndx( ) ) , "") + ")"
      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
         cSql := cSql + "NUMBER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_IBMDB2 .and. cField == ::cRecnoName
         If ::oSql:lUseSequences
            cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1, NO CACHE)"
         Else
            cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL"
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ADABAS .and. cField == ::cRecnoName
         cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL DEFAULT SERIAL"

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_IBMDB2  .or. ::oSql:nSystemID == SYSTEMID_ADABAS )
         cSql := cSql + "DECIMAL(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_INGRES .and. cField == ::cRecnoName
         cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL UNIQUE"

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_INFORM .and. cField == ::cRecnoName
         cSql := cSql + "SERIAL NOT NULL UNIQUE"

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_FIREBR3 .and. cField == ::cRecnoName
        cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") GENERATED BY DEFAULT AS IDENTITY  NOT NULL UNIQUE "

      Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_INFORM .or. ::oSql:nSystemID == SYSTEMID_INGRES)
         cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") " + IF(lPrimary, "NOT NULL PRIMARY KEY ", IF(lNotNull, " NOT NULL", ""))

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_SQLBAS
         If aCreate[i,FIELD_LEN] > 15
            cSql := cSql + "NUMBER" + IF(lPrimary, " NOT NULL", " " )
         Else
            cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" + IF(lPrimary, " NOT NULL", " " )
         EndIf

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
         cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ACCESS
         cSql := cSql + "NUMERIC"

      Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_FIREBR .and. cField == ::cRecnoName
        cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL UNIQUE "

      Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
         If aCreate[i,FIELD_LEN] > 18
            cSql := cSql + "DOUBLE PRECISION" + IF(lPrimary .or. lNotNull, " NOT NULL", " " )
         Else
            cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) +  ")" + IF(lPrimary .or. lNotNull, " NOT NULL", " " )
         EndIf
      // including xml data type
      // postgresql datetime
      Case (aCreate[i,FIELD_TYPE] == "T") .and. (::oSql:nSystemID == SYSTEMID_POSTGR )
         if aCreate[i,FIELD_LEN] == 4
             cSql := cSql + 'time  without time zone '
         else
             cSql := cSql + 'timestamp  without time zone '
         endif
      Case (aCreate[i,FIELD_TYPE] == "T") .and. ( ::osql:nSystemID == SYSTEMID_MYSQL  .or. ::osql:nSystemID == SYSTEMID_MARIADB )
         if aCreate[i,FIELD_LEN] == 4
             cSql := cSql + 'time '
         else
             cSql := cSql + 'DATETIME '
         endif

      // oracle datetime
      Case (aCreate[i,FIELD_TYPE] == "T") .and. (::oSql:nSystemID == SYSTEMID_ORACLE   .or. ::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
         cSql := cSql + 'TIMESTAMP '
      CASE (aCreate[i,FIELD_TYPE] == "T") .and. (::oSql:nSystemID == SYSTEMID_MSSQL7  ) // .AND. ::OSQL:lSqlServer2008 .AND. SR_Getsql2008newTypes()
         cSql := cSql + 'DATETIME NULL '
      CASE (aCreate[i,FIELD_TYPE] == "T") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
         cSql := cSql + 'DATETIME '
      CASE (aCreate[i,FIELD_TYPE] == "V") .and. (::oSql:nSystemID == SYSTEMID_MSSQL7  )
            cSql := cSql + ' VARBINARY(MAX) '

      OtherWise
         SR_MsgLogFile(  SR_Msg(9)+cField+" ("+aCreate[i,FIELD_TYPE]+")" )

      EndCase

      If i != len( aCreate )
         cSql += ", " + CR_LF
      Else
         cSql += CR_LF
      EndIf

   Next

   cSql += " )"

   //If ::oSql:nSystemID == SYSTEMID_MYSQL
        //cSql += " Type=InnoDb "
   //ENDIF

   IF ::oSql:nSystemID == SYSTEMID_MARIADB
      cSql += " Engine=InnoDb "
   ENDIF

   IF ::oSql:nSystemID == SYSTEMID_MYSQL

      if Val( Substr( ::oSql:cSystemVers, 1, 3 ) ) < 505
         cSql += " Type=InnoDb "
      else
         cSql += " Engine=InnoDb "
      Endif

   ENDIF

   If ::oSql:nSystemID == SYSTEMID_ORACLE .and. !Empty( SR_SetTblSpaceData() )
        cSql += " TABLESPACE " + SR_SetTblSpaceData()
   ENDIF

   If ::oSql:nSystemID == SYSTEMID_ORACLE .and. (!Empty( SR_SetTblSpaceLob() )) .and. (!Empty( cLobs ))
      cSql += " LOB (" + cLobs + ") STORE AS (TABLESPACE " + SR_SetTblSpaceLob() + ")"
   EndIf

   If ::oSql:nSystemID == SYSTEMID_INGRES
      nRowSize += 1000     // prevent INDKEY_...
      Do Case
//      Case nRowSize < 3988
//         cRowSize := "4096"
      Case nRowSize < 8084
         cRowSize := "8192"
//      Case nRowSize < 16276
//         cRowSize := "16384"
      Case nRowSize < 32660
         cRowSize := "32768"
      OtherWise
         cRowSize := "65536"
      EndCase
//      cSql += " structure=btree"
//      cSql += " with page_size= " + cRowSize + " structure=btree"
   ENDIF

   ::oSql:commit()
   nRet := ::oSql:exec( cSql, .T. )
   lRet := nRet  == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO
   ::oSql:Commit()

   If lRet .and. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_POSTGR .or. ::oSql:nSystemID == SYSTEMID_ADABAS .or. ::oSql:nSystemID == SYSTEMID_AZURE //  .or. ::oSql:nSystemID == SYSTEMID_CACHE /* Create SR_RECNO INDEX */
   // Culik 18/10/2010 se o server suporta clustered index, adicionamos o mesmo na criacao
      cSql := "CREATE " + If(::oSql:lClustered," CLUSTERED " ," ") + "INDEX " + LimitLen(::cFileName,3) + "_SR ON " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + "(" + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + ") " + if(::oSql:lComments," /* Unique Index */", "" )

      lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS
      ::oSql:Commit()
   EndIf

   If lRet .and. ::oSql:nSystemID == SYSTEMID_ORACLE .and. ::oSql:lUseSequences  /* Create RECNO Trigger */
      ::oSql:exec( "DROP SEQUENCE " + ::cOwner + LimitLen(::cFileName,3) + "_SQ", .f. )
      ::oSql:commit()
      ::oSql:exec( "CREATE SEQUENCE " + ::cOwner + LimitLen(::cFileName,3) + "_SQ START WITH 1" )
      ::CreateOrclFunctions( ::cOwner, ::cFileName )
   EndIf

   If lRet .and. ::oSql:nSystemID == SYSTEMID_CACHE
      ::oSql:Commit()
      ::oSql:exec( "call " +  SR_GetToolsOwner() + [RESET("] + ::cFileName + [")], .F. )
      ::oSql:Commit()
   EndIf

   If lRet .and. ::oSql:nSystemID == SYSTEMID_INGRES .and. ::oSql:lUseSequences  /* Create RECNO Trigger */
      ::oSql:Commit()
      ::oSql:exec( "modify " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + " to btree with page_size = " + cRowSize , .t. ) // + "unique on " + ::cRecnoName
      ::oSql:Commit()
      ::oSql:exec( "DROP SEQUENCE "  + ::cOwner + "SQ_" + cTblName, .f. )
      ::oSql:Commit()
      ::oSql:exec( "CREATE SEQUENCE " + ::cOwner + "SQ_" + cTblName + " AS BIGINT START WITH 1" )
      ::oSql:commit()
   EndIf

   If lRet .and. ::oSql:nSystemID == SYSTEMID_FIREBR .and. ::oSql:lUseSequences  /* Create RECNO Trigger */
      ::oSql:Commit()

      If ::oSql:exec( "SELECT gen_id( " + ::cOwner+cTblName+", 1) FROM RDB$DATABASE",.F. ) != SQL_SUCCESS
         ::oSql:exec( "CREATE GENERATOR " + ::cOwner + cTblName )
         ::oSql:commit()
      EndIf

      ::oSql:exec( "SET GENERATOR " + ::cOwner + cTblName + " TO 0" )
      ::oSql:commit()

      cSql := "CREATE TRIGGER " + ::cOwner + cTblName + "_SR  FOR " + ::cOwner + cTblName +;
              " ACTIVE BEFORE INSERT POSITION 0 " +;
              " as " +;
              "begin " +;
              "If (new."+ ::cRecnoName + " is null) then "+;
              "new."+::cRecnoName +" = gen_id( " + ::cOwner+cTblName+", 1); "+;
              "end"
      lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS
      ::oSql:Commit()

   EndIf

   If lRet .and. len(aPk) > 0     /* Creates the primary key */

      aSort( aPk,,,{ |x,y| x[1] < y[1] } )
      cSql := ""

      Do Case
      Case (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7  .OR. ::oSql:nSystemID == SYSTEMID_FIREBR .OR. ::oSql:nSystemID == SYSTEMID_FIREBR3  .OR. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) .OR. ::oSql:nSystemID ==SYSTEMID_POSTGR .or. ::oSql:nSystemID == SYSTEMID_AZURE)
         cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + " ADD CONSTRAINT " + cTblName + "_PK PRIMARY KEY ("
         For i = 1 to len( aPk )
            cSql += if( i == 1, "", ", " )
            cSql += aPk[i,2]
         Next
         cSql += ")"
      Case ::oSql:nSystemID == SYSTEMID_ORACLE
         cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + " ADD CONSTRAINT " + cTblName + "_PK PRIMARY KEY ("
         For i = 1 to len( aPk )
            cSql += if( i == 1, "", ", " )
            cSql += aPk[i,2]
         Next
         cSql += ") USING INDEX ( CREATE INDEX "+ ::cOwner  + LimitLen(::cFileName,3) + "_PK ON "+ ::cOwner  + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + "( "
         For i = 1 to len( aPk )
            cSql += if( i == 1, "", ", " )
            cSql += aPk[i,2]
         Next
         cSql += ")" + IF(Empty(SR_SetTblSpaceIndx()), "", " TABLESPACE " + SR_SetTblSpaceIndx( ) ) + ")"

      EndCase

      If len( cSql ) > 0
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS
         ::oSql:Commit()
      EndIf
   EndIf

   // Add multilang columns in catalog

   For each aField in aMultilang
      ::oSql:exec( "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTLANG ( TABLE_ , COLUMN_, TYPE_, LEN_, DEC_ ) VALUES ( '" + UPPER(::cFileName) + "','" + aField[FIELD_NAME] + "', '" + aField[FIELD_TYPE] + "','" + alltrim(str(aField[FIELD_LEN],8)) + "','" + alltrim(str(aField[FIELD_DEC],8)) + "' )" )
      ::oSql:Commit()
   Next

   // Update SQLRDD catalogs

   ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTTABLES WHERE TABLE_ = '" + UPPER(::cFileName) + "'" , .F. )
   ::oSql:Commit()

   ::oSql:exec( "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTTABLES ( TABLE_ , SIGNATURE_, CREATED_, TYPE_, REGINFO_ ) VALUES ( '" + UPPER(::cFileName) + "','" + HB_SR__MGMNT_VERSION + "', '" + dtos(date()) + time() + "','TABLE',' ' )" )
   ::oSql:Commit()

   If len( aMultilang ) > 0
      SR_ReloadMLHash( ::oSql )
   EndIf

   If ::oSql:nSystemID == SYSTEMID_SYBASE
      ::oSql:SetOptions( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF )
   EndIf

   If ::LineCount(.F.) == -1
      ::lOpened := .F.
      ::RuntimeErr( "27", SR_Msg(27) , 2, EG_OPEN, ESQLRDD_OPEN )
      Return Self
   EndIf

   ::IniFields(.T.,,@aCacheInfo)

//   aFill( ::aCache, Array( len( ::aLocalBuffer ) ) )
   aEval( ::aCache, { |x,i| (x), ::aCache[i] := array(len(::aLocalBuffer)) } )

   ::GetBuffer(.T.)         /* Clean Buffer */
   ::aInfo[ AINFO_BOF ]    := .T.

   ::nCurrentFetch  := ::nFetchSize
   ::aInfo[ AINFO_SKIPCOUNT ]     := 0
   ::cLastMove      := "OPEN"
   ::Optmizer_1e    := ""
   ::Optmizer_1s    := ""
   ::Optmizer_ne    := { || "" }
   ::Optmizer_ns    := { || "" }

   Switch ::oSql:nSystemID
   Case SYSTEMID_MSSQL7
   Case SYSTEMID_AZURE
   Case SYSTEMID_CACHE
      ::Optmizer_1s  := " TOP 1"
      ::Optmizer_ns  := { |x| " TOP " + str(x+2,5) }
      Exit
   Case SYSTEMID_FIREBR
      If ::oSql:cSystemVers >= "2.0"
         ::Optmizer_1e  := " ROWS 1"
         ::Optmizer_ne  := { |x| " ROWS " + str(x+2,5) }
      Else
         ::Optmizer_1s  := " FIRST 1"
         ::Optmizer_ns  := { |x| " FIRST " + str(x+2,5) }
      EndIf
      Exit
   Case SYSTEMID_INFORM
      ::Optmizer_1s  := " FIRST 1"
      ::Optmizer_ns  := { |x| " FIRST " + str(x+2,5) }
      Exit
   Case SYSTEMID_ORACLE
      //::Optmizer_1s  := " /*+ FIRST_ROWS(1) */ "
      //::Optmizer_ns  := { |x| " /*+ FIRST_ROWS(" + str(x+2,5) + ") */ " }
      IF OracleMinVersion( ::oSql:cSystemVers ) < 9
         ::Optmizer_1s  := " /* + FIRST_ROWS(1) */ "
         ::Optmizer_ns  := { |x| " /* + FIRST_ROWS(" + str(x+2,5) + ") */" }
      ELSE
         ::Optmizer_1s  := " /* + FIRST_ROWS_1 */ "
         ::Optmizer_ns  := { |x| " /* + FIRST_ROWS_" + ALLTRIM(str(x+2,5)) + " */ " }
      ENDIF

      Exit
   Case SYSTEMID_POSTGR
   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB
      ::Optmizer_1e  := " LIMIT 1"
      ::Optmizer_ne  := { |x| " LIMIT " + str(x+2,5) }
      Exit
   Case SYSTEMID_IBMDB2
      If !("DB2/400" $ ::oSql:cSystemName .or. "SQLDS/VM" $ ::oSql:cSystemName)
         ::Optmizer_1e  := " fetch first 1 row only"
         ::Optmizer_ne  := { |x| " fetch first " + str(x+2,5) + " rows only" }
      EndIf
      Exit
   Case SYSTEMID_SYBASE
      If "12.53" $ ::oSql:cSystemVers
         ::Optmizer_1s  := " TOP 1"
         ::Optmizer_ns  := { |x| " TOP " + str(x+2,5) }
      EndIf
      Exit
   End

   ::aInfo[ AINFO_HNRECNO ]   := ::hnRecno
   ::aInfo[ AINFO_HNDELETED ] := ::hnDeleted

   aSize( ::aIndexMgmnt, 0 )
   If SR_CheckMgmntInd()
      ::LoadRegisteredTags()
   EndIf

   If !SR_GetFastOpen()

      If !::LockTable( .F. )
         ::lOpened := .F.
      EndIf

      ::lSharedLock := .T.     // EXCLUSIVE open have EXC and SHARED locks.

      If ::lOpened
         _SR_Register( Self )
      EndIf
   Else
      _SR_Register( Self )
   EndIf

   aRet[ TABLE_INFO_HISTORIC ] := ::lHistoric

   aCacheInfo[ CACHEINFO_TABINFO ]    := aRet
   aCacheInfo[ CACHEINFO_TABNAME ]    := ::cFileName
   aCacheInfo[ CACHEINFO_CONNECT ]    := ::oSql
   aCacheInfo[ CACHEINFO_INDEX ]      := ::aIndexMgmnt
   ::oSql:aTableInfo[ ::cOriginalFN ] := aCacheInfo

Return Self

/*------------------------------------------------------------------------*/

METHOD sqlOpenArea( cFileName, nArea, lShared, lReadOnly, cAlias, nDBConnection ) CLASS SR_WORKAREA

   Local i, nConnection, aRet, nMax := 0, nPos := 0, aCacheInfo
   Local nAllocated, n, lRecnoAdded := .F.

   If ::cWSID == NIL
      ::cWSID := SR_GetUniqueSystemID()
   EndIf

   AsizeAlloc( ::aFetch, 50 )

   ::nThisArea               := nArea
   ::cAlias                  := cAlias
   ::aInfo[ AINFO_SHARED ]   := lShared
   ::cOriginalFN             := upper(alltrim(cFileName))
   ::lGoTopOnFirstInteract   := lGoTopOnFirstInteract

   ::cRecnoName   := SR_RecnoName()
   ::cDeletedName := SR_DeletedName()
   ::nFetchSize   := SR_FetchSize()

   If !::aInfo[ AINFO_SHARED ]
      ::lQuickAppend := .T.
   EndIf

   If SR_GetFastOpen()
      ::aInfo[ AINFO_SHARED ] := .T.
   EndIf

   If nDBConnection == 0
      nDBConnection := NIL
   EndIf

   ::oSql := SR_GetConnection(nDBConnection)

   If (::oSql:cNextQuery != NIL) .or. (Upper( Left( cFileName, 6 ) ) == "SELECT" .and. cFileName[7] $ " " + chr(9) + chr(13) + chr(10))
      If ::oSql:cNextQuery != NIL
         ::cFileName  := ::oSql:cNextQuery
      else
         ::cFileName  := cFileName
      EndIf
      aRet         := Array( TABLE_INFO_SIZE )
      aRet[ TABLE_INFO_RELATION_TYPE ] := TABLE_INFO_RELATION_TYPE_SELECT
      aRet[ TABLE_INFO_ALL_IN_CACHE ]  := .t.
      aRet[ TABLE_INFO_CUSTOM_SQL ]    := ::cFileName
      aRet[ TABLE_INFO_HISTORIC ]      := .f.
      aRet[ TABLE_INFO_OWNER_NAME ]    := ""
      aRet[ TABLE_INFO_CAN_UPDATE ]    := .f.
      aRet[ TABLE_INFO_CAN_INSERT ]    := .f.
      aRet[ TABLE_INFO_CAN_DELETE ]    := .f.
      aRet[ TABLE_INFO_PRIMARY_KEY ]   := ""
      ::oSql:cNextQuery := NIL                     // Reset Next query
   Else
      nPos := HGetPos( ::oSql:aTableInfo, ::cOriginalFN )
      // We created the table and index on same module then close for reopen open later in share mode 
      If nPos >0
        If EMPTY(aClone(HGetValueAt( ::oSql:aTableInfo, nPos ))[CACHEINFO_INDEX])
           nPos := 0 
        EndIf
     EndIf
            
      If nPos > 0
         aCacheInfo  := aClone(HGetValueAt( ::oSql:aTableInfo, nPos ))
         aRet        := aCacheInfo[CACHEINFO_TABINFO]
         ::cFileName := aCacheInfo[CACHEINFO_TABNAME]
         ::oSql      := aCacheInfo[CACHEINFO_CONNECT]
      Else
         aCacheInfo  := Array( CACHEINFO_LEN )
         aRet := eval( SR_GetTableInfoBlock(), cFileName )
         ::cFileName  := SR_ParseFileName( aRet[ TABLE_INFO_TABLE_NAME ] )
      EndIf

      If len( ::cFileName ) > MAX_TABLE_NAME_LENGHT
         ::cFileName := subStr( ::cFileName, 1, MAX_TABLE_NAME_LENGHT )
      EndIf

   EndIf

   aSize( aRet, TABLE_INFO_SIZE )
   If aRet[ TABLE_INFO_CONNECTION ] != NIL
      nConnection := aRet[ TABLE_INFO_CONNECTION ]
      ::oSql := SR_GetConnection(nConnection)
   EndIf

   If aRet[ TABLE_INFO_NO_TRANSAC ] != NIL .and. aRet[ TABLE_INFO_NO_TRANSAC ]
      ::oSql := ::oSql:oSqlTransact
   EndIf

   If aRet[ TABLE_INFO_ALL_IN_CACHE ] == NIL
      aRet[ TABLE_INFO_ALL_IN_CACHE ] := ::oSql:lAllInCache
   EndIf

   ::aFilters   := aRet[ TABLE_INFO_FILTERS ]

   ::cColPK     := alltrim(upper(aRet[ TABLE_INFO_PRIMARY_KEY ]))
   ::nRelacType := aRet[ TABLE_INFO_RELATION_TYPE ]
   ::cOwner     := aRet[ TABLE_INFO_OWNER_NAME ]
   ::lISAM      := .T.                             // !aRet[ TABLE_INFO_ALL_IN_CACHE ]
   ::cCustomSQL := aRet[ TABLE_INFO_CUSTOM_SQL ]

   ::lHistoric  := aRet[ TABLE_INFO_HISTORIC ]

   If lReadOnly .or. (!Empty( ::cCustomSQL ))
      ::lCanUpd := .F.
      ::lCanIns := .F.
      ::lCanDel := .F.
   Else
      ::lCanUpd := aRet[ TABLE_INFO_CAN_UPDATE ]
      ::lCanIns := aRet[ TABLE_INFO_CAN_INSERT ]
      ::lCanDel := aRet[ TABLE_INFO_CAN_DELETE ]
   EndIf

   // NewAge compatible rights string...

   ::cRights := "S"+if(::lCanUpd,"U","")+if(::lCanIns,"I","")+if(::lCanDel,"D","")+ltrim(strzero(::nRelacType,1))

   If aRet[ TABLE_INFO_RECNO_NAME ] != NIL
     ::cRecnoName := aRet[ TABLE_INFO_RECNO_NAME ]
   EndIf

   If aRet[ TABLE_INFO_DELETED_NAME ] != NIL
      ::cDeletedName := aRet[ TABLE_INFO_DELETED_NAME ]
   EndIf

   If !Empty( ::cCustomSQL )      /* Custom SQL Commands requires ALL_DATA_IN_CACHE workarea */
      ::lISAM := .F.
   EndIf

   If !Empty( cGlobalOwner )
      ::cOwner := alltrim( cGlobalOwner )
   ElseIf !Empty( ::oSql:cOwner )
      ::cOwner := alltrim(::oSql:cOwner)
   EndIf

   If (!Empty( ::cOwner )) .and. ::cOwner[-1] != "."
      ::cOwner := alltrim(::cOwner) + "."
   EndIf

   ::lCanSel := .T.
   IF Upper( Substr( ::cFileName, 1, 6 ) ) == "SELECT"
      ::lTableIsSelect := .T.
   ENDIF
   ::cQualifiedTableName := ::cOwner + SR_DBQUALIFY( ::cFileName, ::oSql:nSystemID )

   ::cDel    := "DELETE FROM " + ::cQualifiedTableName + " "
   ::cUpd    := "UPDATE " + ::cQualifiedTableName + " SET "

   /* Search the registered indexes and creates KEY codeblock */

   If Empty( ::cCustomSQL )
      If nPos > 0
         ::aIndexMgmnt           := aCacheInfo[ CACHEINFO_INDEX ]
         ::aFields               := aCacheInfo[ CACHEINFO_AFIELDS ]
         ::aNames                := aCacheInfo[ CACHEINFO_ANAMES ]
         ::aNamesLower           := aCacheInfo[ CACHEINFO_ANAMES_LOWER ]
         ::hnRecno               := aCacheInfo[ CACHEINFO_HNRECNO ]
         ::hnDeleted             := aCacheInfo[ CACHEINFO_HNDELETED ]
         ::aIniFields            := aCacheInfo[ CACHEINFO_INIFIELDS ]
         ::nPosDtHist            := aCacheInfo[ CACHEINFO_HNPOSDTHIST ]
         ::nPosColPK             := aCacheInfo[ CACHEINFO_HNCOLPK ]
         ::nFields               := LEN( ::aFields )
         ::aInfo[ AINFO_FCOUNT ] := ::nFields
         If ::hnRecno != NIL
            ::cRecnoName            := ::aFields[ ::hnRecno, 1 ]
         EndIf
         If ::hnDeleted > 0
            ::cDeletedName       := ::aFields[ ::hnDeleted, 1 ]
         EndIf
         aSize( ::aEmptyBuffer, ::nFields )
         aEval( ::aEmptyBuffer, { |x,i| (x), ::aEmptyBuffer[i] := aCacheInfo[ CACHEINFO_ABLANK ][i] } )
         aSize( ::aSelectList, ::nFields )
//         aFill( ::aCache, Array( len( ::aLocalBuffer ) ) )
         aEval( ::aCache, { |x,i| (x), ::aCache[i] := array(len(::aLocalBuffer)) } )
      Else
         aSize( ::aIndexMgmnt, 0 )
      EndIf
      ::cFltUsr := ::SolveSQLFilters( "A" )
   Else
      ::cFltUsr := ""
      aSize( ::aIndexMgmnt, 0 )
   EndIf

   If !::lISAM    /* load the result set to memory */
      ::lGoTopOnFirstInteract := .T.
      ::IniFields(.T., .T.,@aCacheInfo)

      If ::aFields == NIL
         ::lOpened := .F.
         ::RuntimeErr( "32", SR_Msg(32) , 2, EG_OPEN, ESQLRDD_OPEN )
         Return Self
      EndIf

      /* Load the cache to ::aCache */

      aSize( ::aCache, ARRAY_BLOCK2 )
      nAllocated := ARRAY_BLOCK2
      n          := 0

      If ::hnRecno == NIL .or. ::hnRecno == 0
         ::hnRecno := ::nFields + 1
         ::nFields ++
         ::lCanUpd := .F.
         ::lCanIns := .F.
         ::lCanDel := .F.
         lRecnoAdded := .T.
         aadd( ::aNames, ::cRecnoName )
         aadd( ::aFields, { ::cRecnoName, "N", 15, 0 } )
         aadd( ::aEmptyBuffer, 0 )
      EndIf

      While (::oSql:Fetch( , .F., ::aFields )) = SQL_SUCCESS
         n ++
         If n > nAllocated
            Switch nAllocated
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

            aSize( ::aCache, nAllocated )
         EndIf

         ::aCache[n] := Array(::nFields)

         For i = 1 to ::nFields
            If lRecnoAdded .and. i == ::nFields
               ::aCache[n,i] := n
               nMax := n
            Else
               ::aCache[n,i] := ::oSql:FieldGet( i, ::aFields, .F. )
            EndIf
            If (!lRecnoAdded) .and. i == ::hnRecno
               nMax := Max( nMax, ::aCache[n,i] )
            EndIf
         Next
      EndDo

      aSize( ::aCache, n )
      ::nLastRec := nMax
      ::aInfo[ AINFO_RCOUNT ] := nMax
      ::aInfo[ AINFO_FCOUNT ] := ::nFields
      ::aInfo[ AINFO_FOUND ]  := .F.
      ::aInfo[ AINFO_NPOSCACHE ] := 1

      ::Default()

      ::oSql:FreeStatement()

   Else

      If nPos < 1
         ::IniFields(.T.,,@aCacheInfo)
      EndIf

      If ::aFields == NIL
         ::lOpened := .F.
         ::RuntimeErr( "32", SR_Msg(32) + ": " + ::cFileName , 2, EG_OPEN, ESQLRDD_OPEN )
         Return Self
      EndIf

      If ::LineCount(.F.) == -1
         ::lOpened := .F.
         ::RuntimeErr( "27", SR_Msg(27) , 2, EG_OPEN, ESQLRDD_OPEN )
         Return Self
      EndIf

      If Empty( ::cCustomSQL ) .and. nPos <= 0
         If SR_CheckMgmntInd()
            ::LoadRegisteredTags()
         EndIf
      EndIf

      aEval( ::aCache, { |x,i| (x), ::aCache[i] := array(len(::aLocalBuffer)) } )

      ::GetBuffer(.T.)         /* Clean Buffer */
      ::aInfo[ AINFO_BOF ]    := .T.

      ::nCurrentFetch  := ::nFetchSize
      ::aInfo[ AINFO_SKIPCOUNT ]     := 0
      ::cLastMove      := "OPEN"
      ::Optmizer_1e    := ""
      ::Optmizer_1s    := ""
      ::Optmizer_ne    := { || "" }
      ::Optmizer_ns    := { || "" }

      Switch ::oSql:nSystemID
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_AZURE
      Case SYSTEMID_CACHE
         ::Optmizer_1s  := " TOP 1"
         ::Optmizer_ns  := { |x| " TOP " + str(x+2,5) }
         Exit
      Case SYSTEMID_FIREBR
         If ::oSql:cSystemVers >= "2.0"
            ::Optmizer_1e  := " ROWS 1"
            ::Optmizer_ne  := { |x| " ROWS " + str(x+2,5) }
         Else
            ::Optmizer_1s  := " FIRST 1"
            ::Optmizer_ns  := { |x| " FIRST " + str(x+2,5) }
         EndIf
         Exit
      Case SYSTEMID_INFORM
         ::Optmizer_1s  := " FIRST 1"
         ::Optmizer_ns  := { |x| " FIRST " + str(x+2,5) }
         Exit
      Case SYSTEMID_ORACLE
         //::Optmizer_1s  := " /*+ FIRST_ROWS(1) */ "
         //::Optmizer_ns  := { |x| " /*+ FIRST_ROWS(" + str(x+2,5) + ") */ " }
         IF OracleMinVersion( ::oSql:cSystemVers ) < 9
            ::Optmizer_1s  := " /* + FIRST_ROWS(1) */ "
            ::Optmizer_ns  := { |x| " /* + FIRST_ROWS(" + str(x+2,5) + ") */" }
         ELSE
            ::Optmizer_1s  := " /* + FIRST_ROWS_1 */ "
            ::Optmizer_ns  := { |x| " /* + FIRST_ROWS_" + ALLTRIM(str(x+2,5)) + " */ " }
         ENDIF

         Exit
      Case SYSTEMID_POSTGR
      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
         ::Optmizer_1e  := " LIMIT 1"
         ::Optmizer_ne  := { |x| " LIMIT " + str(x+2,5) }
         Exit
      Case SYSTEMID_IBMDB2
         If !("DB2/400" $ ::oSql:cSystemName .or. "SQLDS/VM" $ ::oSql:cSystemName)
            ::Optmizer_1e  := " fetch first 1 row only"
            ::Optmizer_ne  := { |x| " fetch first " + str(x+2,5) + " rows only" }
         EndIf
         Exit
      Case SYSTEMID_SYBASE
         If "12.53" $ ::oSql:cSystemVers
            ::Optmizer_1s  := " TOP 1"
            ::Optmizer_ns  := { |x| " TOP " + str(x+2,5) }
         EndIf
         Exit
      End
   EndIf

   ::aInfo[ AINFO_HNRECNO ]   := ::hnRecno
   ::aInfo[ AINFO_HNDELETED ] := ::hnDeleted

   ::nLogMode  := ::oSql:nLogMode

   If !SR_GetFastOpen()

      If !lShared
         If !::LockTable( .F. )
            ::lOpened := .F.
         EndIf
         ::lSharedLock := .T.     // EXCLUSIVE open have EXC and SHARED locks.
      Else
        If !::LockTable( .T. )   /* Test If can Lock the file for a moment in current ID */
            ::lOpened := .F.
         Else
            ::lSharedLock := SR_SetLocks( SHARED_TABLE_LOCK_SIGN + UPPER(::cFileName), ::oSql )
         EndIf
      EndIf

      If ::lOpened
         _SR_Register( Self )
      EndIf
   Else
      _SR_Register( Self )
   EndIf

   If nPos <= 0 .and. Empty( ::cCustomSQL )
      aCacheInfo[ CACHEINFO_TABINFO ]     := aRet
      aCacheInfo[ CACHEINFO_TABNAME ]     := ::cFileName
      aCacheInfo[ CACHEINFO_CONNECT ]     := ::oSql
      aCacheInfo[ CACHEINFO_INDEX ]       := ::aIndexMgmnt
      ::oSql:aTableInfo[ ::cOriginalFN ]  := aCacheInfo
   EndIf

   If aRet[ TABLE_INFO_ALL_IN_CACHE ]
      ::lGoTopOnFirstInteract := .T.
   EndIf

Return Self

/*------------------------------------------------------------------------*/

METHOD CreateOrclFunctions( cOwner, cFileName ) CLASS SR_WORKAREA

   local lRet, cTblName, cSql

   If len( cFileName ) > MAX_TABLE_NAME_LENGHT
      cTblName := subStr( cFileName, 1, MAX_TABLE_NAME_LENGHT )
      cFileName := cTblName
   Else
      cTblName := cFileName
   EndIf

   ::oSql:exec( "CREATE OR REPLACE FUNCTION " + cOwner + LimitLen(cFileName,3) + "_SP RETURN NUMBER AS ID_R NUMBER; BEGIN SELECT " + cOwner + LimitLen(cFileName,3) + "_SQ.NEXTVAL INTO ID_R FROM DUAL; RETURN ID_R; END;" )

   cSql := "CREATE OR REPLACE TRIGGER " + cOwner + LimitLen(cFileName,3) + "_SR BEFORE INSERT ON " +;
           cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + " FOR EACH ROW DECLARE v_seq " +;
           SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + "." + ::cRecnoName + "%TYPE; BEGIN If :OLD." +;
           ::cRecnoName + " IS NULL THEN SELECT " + cOwner + LimitLen(cFileName,3) + "_SQ.NEXTVAL INTO v_seq FROM DUAL; :NEW." +;
           ::cRecnoName + " := v_seq; END IF; END;"
   lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS
   ::oSql:Commit()

Return lRet

/*------------------------------------------------------------------------*/

METHOD sqlZap() CLASS SR_WORKAREA

   local nRet

   ::sqlGoCold()

   If ::lCanDel

      nRet := ::oSql:execute( ::cDel  + if(::oSql:lComments," /* Zap */",""), .F., ::nLogMode )

      ::oSql:FreeStatement()

      If  nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO .and. nRet != SQL_NO_DATA_FOUND
         Return .F.
      EndIf
     
      if (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .OR. ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_AZURE);
         .and.::oSql:lUseSequences.and.::lUseSequences
         
         ::oSql:Commit()
         ::oSql:exec( "DBCC CHECKIDENT ('"+::cFileName+"', RESEED, 0)")
         ::oSql:Commit()
      endi     

      If ::oSql:nSystemID == SYSTEMID_FIREBR .and. ::oSql:lUseSequences .and. ::lUseSequences
         ::oSql:Commit()
         ::oSql:exec( "SET GENERATOR " + ::cFileName + " TO 0" )
         ::oSql:Commit()
      EndIf
      If ::oSql:nSystemID == SYSTEMID_POSTGR .and. ::oSql:lUseSequences .and. ::lUseSequences
         ::oSql:Commit()
         ::oSql:exec( "select setval('" +::cOwner + LimitLen(::cFileName,3) + "_SQ'  , 1)" )
         ::oSql:Commit()
      EndIf

/*
      If ::oSql:nSystemID == SYSTEMID_ORACLE .and.  .and. ::lUseSequences
         ::oSql:commit()
         ::oSql:exec( "DROP SEQUENCE " + ::cOwner + LimitLen(::cFileName,3) + "_SQ", .f. )
         ::oSql:commit()
         ::oSql:exec( "CREATE SEQUENCE " + ::cOwner + LimitLen(::cFileName,3) + "_SQ START WITH 1" )
         ::oSql:commit()
      EndIf
*/
      ::sqlFlush()

      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
      ::aInfo[ AINFO_NPOSCACHE ] := 0
      ::aInfo[ AINFO_EOF_AT ]    := 0
      ::aInfo[ AINFO_BOF_AT ]    := 0
      ::aInfo[ AINFO_EOF ]       := .T.
      ::aInfo[ AINFO_BOF ]       := .T.
     ::Refresh()
   Else
      SR_MsgLogFile( SR_Msg(12) + ::cFileName )
   endif

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlOrderListAdd( cBagName, cTag ) CLASS SR_WORKAREA

   Local i, c, cWord := "", aCols := {}
   Local cList := ""
   Local cOrdName
   Local nPos, nInd, aInd := {}
   Local nLen, cXBase := "", cCol, nPosAt
   Local cSqlA, cSqlD, aRet
   Local lSyntheticVirtual := .F., cPhysicalVIndexName, cVInd

   If !Empty( cVInd := SR_GetSVIndex() )
      lSyntheticVirtual   := ::oSql:nSystemID == SYSTEMID_ORACLE
      cPhysicalVIndexName := SubStr(cVInd,1,3)+SubStr(::cFileName,1,25)
   ElseIf len(cBagName) > 0 .and. cBagName[4] == "@"
      lSyntheticVirtual   := ::oSql:nSystemID == SYSTEMID_ORACLE
      cPhysicalVIndexName := SubStr(cBagName,1,3)+SubStr(::cFileName,1,25)
      cBagName            := SubStr(cBagName,5)
   Else
      aRet := eval( SR_GetIndexInfoBlock(), cBagName )
      aSize( aRet, TABLE_INFO_SIZE )
      cBagName  := SR_ParseFileName( aRet[ TABLE_INFO_TABLE_NAME ] )

      If cTag == NIL .or. Empty( alltrim(cTag) )

         /* Check If the index is already open */

         nInd := aScan( ::aIndex, { |x| alltrim(upper(x[10])) == alltrim(upper(cBagName)) } )

         If nInd > 0 .and. DUPL_IND_DETECT
            /* Index already opened */
            Return nInd
         EndIf

         /* Not opened, than try find it in the orders management */

         nInd := aScan( ::aIndexMgmnt, { |x| alltrim(upper(x[INDEXMAN_IDXNAME])) == alltrim(upper( cBagName )) } )
         If nInd > 0
            aadd(aInd, nInd)
            nInd ++
            While nInd <= len( ::aIndexMgmnt )
               If alltrim(upper(::aIndexMgmnt[nInd,INDEXMAN_IDXNAME])) == alltrim(upper( cBagName ))
                  aadd(aInd, nInd)
               Else
                  exit
               Endif
               nInd ++
            EndDo
         EndIf
         cTag := NIL
      Else
         nInd := aScan( ::aIndexMgmnt, { |x| alltrim(upper(x[INDEXMAN_IDXNAME])) == alltrim(upper( cBagName )) .and. alltrim(upper(x[INDEXMAN_TAG])) == alltrim(upper( cTag )) } )
         If nInd > 0
            /* Index already opened */
            aadd(aInd, nInd)
         EndIf
      EndIf
   EndIf

   /* Check if BagName is a list of fields or a real index name */

   cCol := ""

   If len(aInd) == 0
      For i = 1 to len( cBagName )
         c := cBagName[i]
         If IsDigit( c ) .or. IsAlpha( c ) .or. c == "_"  .or. c == " "
            cWord += c
         EndIf
         If c $ "|-;+-/*" .and. !Empty(cWord)
            aadd( aCols, upper(cWord) )
            cCol += if( len( cCol ) != 0, ",", "" ) + upper(cWord)
            cWord := ""
         EndIf
      Next
      If len( cWord ) > 0
         aadd( aCols, upper(cWord) )
         cCol += if( len( cCol ) != 0, ",", "" ) + upper(cWord)
      EndIf

      If ::lHistoric
         aadd( aCols, "DT__HIST" )
         cCol += if( len( cCol ) != 0, ",", "" ) + "DT__HIST"
      EndIf

      aadd( aCols, ::cRecnoName )
      cCol += if( len( cCol ) != 0, ",", "" ) + ::cRecnoName

      aadd( ::aIndexMgmnt, { "","","Direct",aCols,"","","","",&( "{|| " + alltrim( cBagName ) + " }" ),,if(lSyntheticVirtual,cPhysicalVIndexName,NIL) } )

//      If Empty( ::cCustomSQL )
//         aInf := ::oSql:aTableInfo[ ::cOriginalFN ]
//         aInf[ CACHEINFO_INDEX ] := ::aIndexMgmnt
//      EndIf

      aadd( aInd, len( ::aIndexMgmnt ) )

   EndIf

   For each nInd in aInd

      If !Empty( ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] )
         aCols := { "INDKEY_" + ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] }
      Else
         If valtype( ::aIndexMgmnt[nInd, INDEXMAN_IDXKEY] ) == "C"
            aCols := &( "{" + ::aIndexMgmnt[nInd, INDEXMAN_IDXKEY] + "}" )
         Else
            aCols := ::aIndexMgmnt[nInd, INDEXMAN_IDXKEY]
         EndIf
      endif
      cOrdName     := ::aIndexMgmnt[nInd, INDEXMAN_TAG]

      If Empty(cOrdName)
         cOrdName := ""
      EndIf

      If ::aIndexMgmnt[nInd, INDEXMAN_VIRTUAL_SYNTH] != NIL
         lSyntheticVirtual    := .T.
         cPhysicalVIndexName  := ::aIndexMgmnt[nInd, INDEXMAN_VIRTUAL_SYNTH]
      Else
         cPhysicalVIndexName  := NIL
      EndIf

      Switch ::oSql:nSystemID
      Case SYSTEMID_IBMDB2
         If "08.0" $ ::oSql:cSystemVers .and. (!"08.00" $ ::oSql:cSystemVers)
            cSqlA  := " ORDER BY row_number() over( ORDER BY "
            cSqlD  := " ORDER BY row_number() over( ORDER BY "
         Else
            cSqlA  := " ORDER BY "
            cSqlD  := " ORDER BY "
         EndIf
         Exit
      Default
         cSqlA  := " ORDER BY "
         cSqlD  := " ORDER BY "
      End

      cXBase := ""

      AADD( ::aIndex, { "","",{},"","","", NIL, NIL, cOrdName, cBagName,,,,,,0, ::aIndexMgmnt[nInd, INDEXMAN_SIGNATURE][19] == "D", cPhysicalVIndexName,,, ::aIndexMgmnt[nInd, INDEXMAN_IDXNAME] } )
      nLen := Len( ::aIndex )

      For i = 1 to len( aCols )

         nPosAt := At( aCols[i], " " )

         If nPosAt = 0
            cCol := aCols[i]
         Else
            cCol := SubStr( aCols[i], 1, nPosAt )
         EndIf

         Switch ::oSql:nSystemID
         Case SYSTEMID_ORACLE
         Case SYSTEMID_FIREBR
       Case SYSTEMID_FIREBR3
            cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ NULLS FIRST,]
            cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC NULLS LAST,]
            exit
         case SYSTEMID_POSTGR
         if ::osql:lPostgresql8
            cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ NULLS FIRST,]
            cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC NULLS LAST,]
         else
            cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [,]
            cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC,]

         endif
         exit
         Case SYSTEMID_IBMDB2
            If "08.0" $ ::oSql:cSystemVers .and. (!"08.00" $ ::oSql:cSystemVers)
               cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ NULLS FIRST,]
               cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC NULLS LAST,]
            Else
               cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [,]
               cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC,]
            EndIf
            exit
         Default
            cSqlA += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [,]
            cSqlD += [ A.] + SR_DBQUALIFY( cCol, ::oSql:nSystemID ) + [ DESC,]
         End

         If (nPos := aScan( ::aNames, {|x| x == cCol } )) != 0
            If ::aNames[nPos] != ::cRecnoName

               ::aIndex[nLen, SYNTH_INDEX_COL_POS] := nPos

               Do Case
               Case ::aFields[nPos,2] = "C" .and. ::aFields[nPos,2] == ::cDeletedName
                  cXBase += "Deleted() + "
               Case ::aFields[nPos,2] = "C"
                  cXBase += ::aNames[nPos] + " + "
               Case ::aFields[nPos,2] = "D"
                  cXBase += "DTOS(" + ::aNames[nPos] + ") + "
               Case ::aFields[nPos,2] = "T"
                  cXBase += "TTOS(" + ::aNames[nPos] + ") + "
               Case ::aFields[nPos,2] = "N"
                  cXBase += "STR(" + ::aNames[nPos] + ", " + alltrim(str(::aFields[nPos,3])) + ", " +;
                            alltrim(str(::aFields[nPos,4])) + ") + "
               Case ::aFields[nPos,2] = "L"
                  cXBase += "Sr_cdbvalue("+ ::aNames[nPos]+ ")" + " + "
               EndCase
            EndIf
         Else
            ::RunTimeErr("18", SR_Msg(18) + cCol + " Table : " + ::cFileName )
            Return 0       /* error exit */
         EndIf

         AADD( ::aIndex[nLen,INDEX_FIELDS], { aCols[i], nPos } )

      Next

      cXBase := left( cXBase,len(cXBase)-2)

      Switch ::oSql:nSystemID
      Case SYSTEMID_IBMDB2
         If "08.0" $ ::oSql:cSystemVers .and. (!"08.00" $ ::oSql:cSystemVers)
            cSqlA  := left( cSqlA, len(cSqlA)-1 ) + " ) "
            cSqlD  := left( cSqlD, len(cSqlD)-1 ) + " ) "
         Else
            cSqlA  := left( cSqlA, len(cSqlA)-1 ) + " "
            cSqlD  := left( cSqlD, len(cSqlD)-1 ) + " "
         EndIf
         Exit
      Default
         cSqlA  := left( cSqlA, len(cSqlA)-1 ) + " "
         cSqlD  := left( cSqlD, len(cSqlD)-1 ) + " "
      End

      ::aIndex[ nLen, ORDER_ASCEND ] := cSqlA
      ::aIndex[ nLen, ORDER_DESEND ] := cSqlD
      ::aIndex[ nLen, INDEX_KEY ]    := rtrim(if( nInd > 0 .and. (!Empty(::aIndexMgmnt[nInd, INDEXMAN_COLUMNS])), ::aIndexMgmnt[nInd, INDEXMAN_IDXKEY], cXBase ))
      If !Empty( ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] )

         IF RDDNAME() =="SQLEX"
            ::aIndex[ nInd, INDEX_KEY_CODEBLOCK ] := &( "{|| " + cXBase + " }")  //aScan( ::aNames, "INDKEY_" + ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS] )
         ELSE
            ::aIndex[ nLen, INDEX_KEY_CODEBLOCK ] := aScan( ::aNames, "INDKEY_" + ::aIndexMgmnt[nInd, INDEXMAN_COLUMNS]  )
         ENDIF
      Else
         ::aIndex[ nLen, INDEX_KEY_CODEBLOCK ] := &( "{|| " + cXBase + " }")
      EndIf

      If ::aIndexMgmnt[nInd, INDEXMAN_FOR_EXPRESS][1] != "#"
         ::aIndex[ nLen, FOR_CLAUSE ]   := rtrim(::aIndexMgmnt[nInd, INDEXMAN_FOR_EXPRESS])
      Else
         ::aIndex[ nLen, FOR_CLAUSE ]   := "INDFOR_" + SubStr(::aIndexMgmnt[nInd, INDEXMAN_FOR_EXPRESS],2,3) + " = 'T'"
      EndIf

      ::aIndex[ nLen, STR_DESCENDS ] := ""
      ::aIndex[ nLen, SYNTH_INDEX_COL_POS] := if( nInd > 0 .and. (!Empty(::aIndexMgmnt[nInd, INDEXMAN_COLUMNS])), ::aIndex[nLen, SYNTH_INDEX_COL_POS], 0 )

      If lSyntheticVirtual
         ::aIndex[ nLen, VIRTUAL_INDEX_EXPR] := ::GetSyntheticVirtualExpr(aCols, "A")
      EndIf

   Next

   ::lStable   := .F.
   ::lOrderValid := .T.

   If ::aInfo[ AINFO_INDEXORD ] == 0
      If cTag == NIL
         ::aInfo[ AINFO_INDEXORD ] := 1
         ::aInfo[ AINFO_REVERSE_INDEX ] := ::aIndex[ 1, DESCEND_INDEX_ORDER ]
         ::sqlOrderListFocus(1)
      Else
         ::aInfo[ AINFO_INDEXORD ] := len(::aIndex)
         ::aInfo[ AINFO_REVERSE_INDEX ] := ::aIndex[ -1, DESCEND_INDEX_ORDER ]
         ::sqlOrderListFocus(len(::aIndex))
      EndIf
   EndIf

Return ::aInfo[ AINFO_INDEXORD ]   // len(::aIndex) Controlling order should not be changed.

/*------------------------------------------------------------------------*/

METHOD sqlOrderListClear() CLASS SR_WORKAREA

   ::aInfo[ AINFO_FOUND ]   := .F.
   aSize( ::aIndex, 0 )
   ::cFor   := ""
   ::aInfo[ AINFO_INDEXORD ]   = 0
   ::lStable     = .T.
   ::lOrderValid = .F.

Return .T.

/*------------------------------------------------------------------------*/

METHOD sqlOrderListFocus( uOrder, cBag ) CLASS SR_WORKAREA

   Local nOrder := 0, i, aInd

   (cBag) // to remove warning

   If valtype( uOrder ) == "C"      /* TAG order */
      nOrder := aScan( ::aIndex, {|x| upper(alltrim(x[ORDER_TAG])) == upper(alltrim(uOrder)) } )
      If nOrder == 0 .or. nOrder > len( ::aIndex )
         ::cFor        := ""
         ::aInfo[ AINFO_INDEXORD ]   := 0
         ::RuntimeErr( "19", SR_Msg(19) + SR_Val2Char( uOrder ) )
         Return 0 /* error exit */
      EndIf
   ElseIf valtype( uOrder ) == "N"
      nOrder := uOrder
   EndIf

   /*If nOrder == ::aInfo[ AINFO_INDEXORD ]
      Return nOrder
   EndIf*/

   If nOrder == 0 .or. nOrder > len( ::aIndex )

      ::cFor        := ""
      ::aInfo[ AINFO_INDEXORD ]   := 0

      If nOrder > len( ::aIndex )
         ::RuntimeErr( "19", SR_Msg(19) + alltrim(SR_Val2Char( uOrder )) + ", " + ::cAlias )
      EndIf

      ::aInfo[ AINFO_EOF_AT ] := 0
      ::aInfo[ AINFO_BOF_AT ] := 0

//      If (!( ::aInfo[ AINFO_EOF ] .and. ::aInfo[ AINFO_BOF ] )) .and. !::aInfo[ AINFO_DELETED ]
//         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 1
//         ::aInfo[ AINFO_NPOSCACHE ] := 1
//         aCopy( ::aLocalBuffer, ::aCache[ 1 ] )
//      Else
         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
         ::aInfo[ AINFO_NPOSCACHE ] := 0
//      EndIf

      Return 0

   EndIf

   ::cFor  := ::aIndex[nOrder,FOR_CLAUSE]

   If !( nOrder == ::aInfo[ AINFO_INDEXORD ] .and. ::lStable )
      ::lStable     := .F.
   EndIf

   ::aInfo[ AINFO_INDEXORD ]   := nOrder
   ::aInfo[ AINFO_REVERSE_INDEX ] := ::aIndex[nOrder, DESCEND_INDEX_ORDER ]
   ::lOrderValid := .T.
   ::aInfo[ AINFO_EOF_AT ] := 0
   ::aInfo[ AINFO_BOF_AT ] := 0

   If (!( ::aInfo[ AINFO_EOF ] .and. ::aInfo[ AINFO_BOF ] )) .and. (!::aInfo[ AINFO_DELETED ]) .and. ::aInfo[ AINFO_NPOSCACHE ] > 0
      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 1
      ::aInfo[ AINFO_NPOSCACHE ] := 1
      aCopy( ::aLocalBuffer, ::aCache[1] )
   Else
      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
      ::aInfo[ AINFO_NPOSCACHE ] := 0
   EndIf

   aInd := ::aIndex[::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS]
   For i = 1 to len( aInd )
      ::aSelectList[ aInd[i,2] ] := 1
   Next

Return nOrder

/*------------------------------------------------------------------------*/

METHOD sqlOrderDestroy( uOrder, cBag ) CLASS SR_WORKAREA

   Local nOrder := 0 //, i , aInd

   (cBag) // to remove warning
   //(uOrder)


   If valtype( uOrder ) == "C"      // TAG order
      nOrder := aScan( ::aIndex, {|x| upper(alltrim(x[ORDER_TAG])) == upper(alltrim(uOrder)) } )
      If nOrder == 0 .or. nOrder > len( ::aIndex )
         ::cFor        := ""
         ::aInfo[ AINFO_INDEXORD ]   := 0
         ::RuntimeErr( "19", SR_Msg(19) + SR_Val2Char( uOrder ) )
         Return 0
      else
         SR_DropIndex( ::aIndex[ nOrder, ORDER_TAG ] )
         aDel( ::aIndex, 12 ,.T.)
         return 0
      EndIf
   ElseIf valtype( uOrder ) == "N"
      nOrder := uOrder
      If nOrder == 0 .or. nOrder > len( ::aIndex )
         ::cFor        := ""
         ::aInfo[ AINFO_INDEXORD ]   := 0
         ::RuntimeErr( "19", SR_Msg(19) + SR_Val2Char( uOrder ) )
         Return 0
      else
         SR_DropIndex(::aIndex[ nOrder, ORDER_TAG ] )
         aDel (::aIndex, 12, .T. )
         return 0
      EndIf

   EndIf
/*
   If nOrder == 0 .or. nOrder > len( ::aIndex )

      ::cFor        := ""
      ::aInfo[ AINFO_INDEXORD ]   := 0

      If nOrder > len( ::aIndex )
         ::RuntimeErr( "19", SR_Msg(19) + alltrim(SR_Val2Char( uOrder )) + ", " + ::cAlias )
      EndIf

      ::aInfo[ AINFO_EOF_AT ] := 0
      ::aInfo[ AINFO_BOF_AT ] := 0

//      If (!( ::aInfo[ AINFO_EOF ] .and. ::aInfo[ AINFO_BOF ] )) .and. !::aInfo[ AINFO_DELETED ]
//         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 1
//         ::aInfo[ AINFO_NPOSCACHE ] := 1
//         aCopy( ::aLocalBuffer, ::aCache[ 1 ] )
//      Else
         ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
         ::aInfo[ AINFO_NPOSCACHE ] := 0
//      EndIf

      Return 0

   EndIf

   ::cFor  := ::aIndex[nOrder,FOR_CLAUSE]

   If !( nOrder == ::aInfo[ AINFO_INDEXORD ] .and. ::lStable )
      ::lStable     := .F.
   EndIf

   ::aInfo[ AINFO_INDEXORD ]   := nOrder
   ::aInfo[ AINFO_REVERSE_INDEX ] := ::aIndex[nOrder, DESCEND_INDEX_ORDER ]
   ::lOrderValid := .T.
   ::aInfo[ AINFO_EOF_AT ] := 0
   ::aInfo[ AINFO_BOF_AT ] := 0

   If (!( ::aInfo[ AINFO_EOF ] .and. ::aInfo[ AINFO_BOF ] )) .and. (!::aInfo[ AINFO_DELETED ]) .and. ::aInfo[ AINFO_NPOSCACHE ] > 0
      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 1
      ::aInfo[ AINFO_NPOSCACHE ] := 1
      aCopy( ::aLocalBuffer, ::aCache[1] )
   Else
      ::aInfo[ AINFO_NCACHEEND ] := ::aInfo[ AINFO_NCACHEBEGIN ] := 0
      ::aInfo[ AINFO_NPOSCACHE ] := 0
   EndIf

   aInd := ::aIndex[::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS]
   For i = 1 to len( aInd )
      ::aSelectList[ aInd[i,2] ] := 1
   Next
*/
Return nOrder

/*------------------------------------------------------------------------*/

METHOD sqlOrderListNum( uOrder ) CLASS SR_WORKAREA

   Local nOrder := 0

   If valtype( uOrder ) == "C"      /* TAG order */
      nOrder := aScan( ::aIndex, {|x| upper(alltrim(x[ORDER_TAG])) == upper(alltrim(uOrder)) } )
      If nOrder == 0 .or. nOrder > len( ::aIndex )
         Return 0 /* error exit */
      EndIf
   ElseIf valtype( uOrder ) == "N"
      nOrder := uOrder
   Else
      nOrder := ::aInfo[ AINFO_INDEXORD ]
   EndIf

   If nOrder == 0 .or. nOrder > len( ::aIndex )
      Return 0 /* error exit */
   EndIf

Return nOrder

/*------------------------------------------------------------------------*/

METHOD sqlOrderCondition( cFor, cWhile, nStart, nNext, uRecord, lRest, lDesc )

   ::aLastOrdCond := { cFor, cWhile, nStart, nNext, uRecord, lRest, lDesc }

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlOrderCreate( cIndexName, cColumns, cTag, cConstraintName, cTargetTable, aTargetColumns, lEnable ) CLASS SR_WORKAREA

   Local i, c, cWord := "", aCols := {}, cNextTagNum, cFor, cForDb := "", cColFor, aConstraintCols := { }
   Local cList := "", cList2 := "", cListConstraint := "", cListConstraint_Source := "", lHaveTag, nNewTag, lTagFound, cPhisicalName
   Local cSql, lRet, lSyntheticIndex := .F., lInFunction := .F., cColIndx, bIndexKey, bIndexFor
   Local lInParams := .F., nLenTag, nOldOrd, aRet, lDesc := .F.
   Local lSyntheticVirtual := .F., cPhysicalVIndexName, cPrevPhysicalVIndexName, nVI, lOK, cVInd
   Local aOldPhisNames := {}, cName
   Local nKeySize :=0

   (lEnable)

   If valtype( aTargetColumns ) == "A" .and. len( aTargetColumns ) > 0 .and. valtype( aTargetColumns[1] ) == "A"
      aTargetColumns := aTargetColumns[1]
   EndIf

   lHaveTag := !Empty(cTag)

   // Release any pending transaction before a DDL command

   ::sqlGoCold()

   If ::oSql:nTransacCount >  0
      ::oSql:Commit()
      ::oSql:nTransacCount := 0
   EndIf

   If cConstraintName == NIL
      cConstraintName := ""
   Endif

   If !Empty( cVInd := SR_GetSVIndex() )
      lSyntheticVirtual   := ::oSql:nSystemID == SYSTEMID_ORACLE
      cPhysicalVIndexName := SubStr(cVInd,1,3)+SubStr(::cFileName,1,25)
   ElseIf len( cColumns ) > 4 .and. cColumns[4] == "@"
      lSyntheticVirtual   := ::oSql:nSystemID == SYSTEMID_ORACLE
      cPhysicalVIndexName := SubStr(cColumns,1,3)+SubStr(::cFileName,1,25)
      cColumns            := SubStr(cColumns,5)
   EndIf

   If !SR_GetSyntheticIndex()
      For i = 1 to len( cColumns )
         c := cColumns[i]
         If lInFunction .or. lInParams
            If c == ")"
               lInFunction := .F.
               lInParams   := .F.
               If !Empty( cWord )
                  aadd( aCols, upper(cWord) )
                  cWord := ""
               EndIf
            ElseIf c $ "," .and. !Empty(cWord)
               aadd( aCols, upper(cWord) )
               cWord := ""
               lInParams := .T.
            ElseIf c $ ","
               lInParams := .T.
            ElseIf c $ "("       // dtos( otherfunc() ) not allowed
               lSyntheticIndex := .T.
               Exit
            ElseIf c == "-" .and. cColumns[i+1] == ">"
               If lAllowRelationsInIndx
                  lSyntheticIndex := .T.
                  Exit
               EndIf
               i ++
               cWord := ""
            ElseIf IsAlpha( c ) .or. c == "_" .or. (IsDigit( c ) .and. (IsAlpha( left(cWord,1) ) .or. left(cWord,1) == "_" ))
               cWord += c
            EndIf
         Else
            If IsDigit( c ) .or. IsAlpha( c ) .or. c == "_"
               cWord += c
            EndIf
            If c == "("
               If upper( cWord ) == "STR"
                  cWord       := ""
                  lInFunction := .T.
               ElseIf upper( cWord ) == "DTOS"
                  cWord       := ""
                  lInFunction := .T.
               ElseIf upper( cWord ) == "DELETED" .and. ::hnDeleted > 0
                  aadd( aCols, ::cDeletedName )
                  cWord := ""
               ElseIf upper( cWord ) == "RECNO"
                  cWord       := ""
                  lInFunction := .T.
               Else
                  lSyntheticIndex   := .T.
                  lSyntheticVirtual := .F.
                  Exit
               EndIf
            else
               If c $ "|-;+-/*" .and. !Empty(cWord)
                  If c == "-" .and. cColumns[i+1] == ">"
                     If lAllowRelationsInIndx
                        lSyntheticIndex := .T.
                        Exit
                     EndIf
                     i ++
                  Else
                     aadd( aCols, upper(cWord) )
                  EndIf
                  cWord := ""
               EndIf
            endif
         EndIf
      Next
   Else
      lSyntheticIndex   := .T.
      lSyntheticVirtual := .F.
   EndIf

   If !lSyntheticIndex
      If len( cWord ) > 0
         aadd( aCols, upper(cWord) )
      EndIf
   EndIf

   If !Empty(AllTrim(cConstraintName))
      aConstraintCols := aClone(aCols)
   Endif

   If !lSyntheticIndex

      If ::lHistoric
         aadd( aCols, "DT__HIST" )
      EndIf

      If ((!::lHistoric .and. Len(aCols) == 1) .or. (::lHistoric .and. Len(aCols) == 2))
         If AllTrim(aCols[1]) <> ::cRecnoName   //minor hack for indexes with only recno (or history) column....
            aadd( aCols, ::cRecnoName )
         Endif
      Else
         aadd( aCols, ::cRecnoName )
      Endif
   EndIf

   If (!lSyntheticIndex) .and. len( aCols ) > SR_GetSyntheticIndexMinimun()
      If ::oSql:nSystemID != SYSTEMID_ORACLE
         lSyntheticIndex   := .T.
         lSyntheticVirtual := .F.
      Else     // Oracle can workaround with SinthetycVirtualIndex

         IF !SR_GetOracleSyntheticVirtual() // if set to false, use normal index, dont created the function based indexes
            lSyntheticIndex   := .T.
            lSyntheticVirtual := .F.
         ELSE
         lSyntheticVirtual   := .T.
         ::LoadRegisteredTags()

         nVI := 1

         While nVI <= 999     // Determine an automatic name for SVI (SyntheticVirtualIndex)
            lOk := .T.
            For i = 1 to len( ::aIndexMgmnt )
               If ::aIndexMgmnt[i, INDEXMAN_VIRTUAL_SYNTH] != NIL .and. subStr(::aIndexMgmnt[i, INDEXMAN_VIRTUAL_SYNTH],1,3) == StrZero(nVI,3)
                  nVI++
                  lOk := .F.
                  Exit
               EndIf
            Next
            If lOK
               cPhysicalVIndexName := StrZero(nVI,3) + SubStr(::cFileName,1,25)
               Exit
            EndIf
         EndDo
      EndIf
   EndIf
   EndIf

   aRet := eval( SR_GetIndexInfoBlock(), cIndexName )
   aSize( aRet, TABLE_INFO_SIZE )

   cIndexName  := SR_ParseFileName( aRet[ TABLE_INFO_TABLE_NAME ] )

   If empty( cIndexName )
      cIndexName := ::cFileName
   EndIf

   If empty( cTag ) .and. (!empty( cIndexName )) .and. (!lSyntheticIndex)
      cTag := cIndexName
   EndIf

   If ::oSql:nSystemID == SYSTEMID_SYBASE
      ::oSql:SetOptions( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON )
   EndIf

   /* Removes from the control structures */

   aSize( ::aIndexMgmnt, 0 )
   ::oSql:exec( "SELECT TABLE_,SIGNATURE_,IDXNAME_,IDXKEY_,IDXFOR_,IDXCOL_,TAG_,TAGNUM_,PHIS_NAME_ FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES  WHERE TABLE_ = '" + UPPER(::cFileName) + "' AND IDXNAME_ = '" + Upper(Alltrim(cIndexName)) + "'" + if(lHaveTag, " AND TAG_ = '" + cTag + "'", "") + " ORDER BY IDXNAME_, TAGNUM_", .T., .T., @::aIndexMgmnt )

   For i = 1 to len( ::aIndexMgmnt )
      aadd( aOldPhisNames, alltrim(::aIndexMgmnt[i, 9]) )
      If !Empty( ::aIndexMgmnt[i,INDEXMAN_COLUMNS] )
         ::DropColumn( "INDKEY_" + alltrim(::aIndexMgmnt[i,INDEXMAN_COLUMNS]), .F. )
      EndIf
      If ::aIndexMgmnt[i,INDEXMAN_FOR_EXPRESS][1] == "#"
         ::DropColumn( "INDFOR_" + substr(::aIndexMgmnt[i,INDEXMAN_FOR_EXPRESS],2,3), .F., .T. )
      EndIf
      If len( ::aIndexMgmnt[i,INDEXMAN_IDXKEY] ) > 4 .and. ::aIndexMgmnt[i,INDEXMAN_IDXKEY][4] == "@"
         cPrevPhysicalVIndexName := SubStr(::aIndexMgmnt[i,INDEXMAN_IDXKEY],1,3)+SubStr(::cFileName,1,25)
      EndIf
   Next

   If valtype( ::aLastOrdCond ) == "A" .and. len( ::aLastOrdCond ) > 1
      If ::aLastOrdCond[1] != NIL
         // Try to create an easy xBase => SQL translation
         cFor := ::ParseForClause( ::aLastOrdCond[1] )

         // Try FOR clause in SQL
         If ::oSql:exec( "SELECT A.* FROM " + ::cQualifiedTableName + " A WHERE 0 = 1 AND (" + cFor + ")" , .F. ) = SQL_SUCCESS
            cForDb := cFor
         Else
            i := 1
            While aScan( ::aNames, { |x| upper(x) == "INDFOR_" + strZero( i, 3 ) } ) > 0
               i++
            EndDo
            cForDB  := "#" + strZero( i,3 ) + ::aLastOrdCond[1]
            cColFor := "INDFOR_" + strZero( i, 3 )
            bIndexFor := &( "{|| " + alltrim( ::aLastOrdCond[1] ) + " }" )
            ::AlterColumns( {{ cColFor, "C", 1, 0, , SQL_CHAR }}, .F. )
            ::Refresh()
         Endif
      EndIf

      If ::aLastOrdCond[7] != NIL
         lDesc := ::aLastOrdCond[7]
      EndIf
   EndIf

   ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + UPPER(::cFileName) + "' AND IDXNAME_ = '" + Upper(Alltrim(cIndexName)) + "'" + if(lHaveTag, " AND TAG_ = '" + cTag + "'", "") + if(::oSql:lComments," /* Wipe index info 01 */",""), .F. )
   ::oSql:Commit()

   aSize( ::aIndexMgmnt, 0 )
   ::oSql:exec( "SELECT TABLE_,SIGNATURE_,IDXNAME_,IDXKEY_,IDXFOR_,IDXCOL_,TAG_,TAGNUM_ FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + UPPER(::cFileName) + "' ORDER BY IDXNAME_, TAGNUM_"  ,.F., .T., @::aIndexMgmnt )
   ::oSql:Commit()

   cNextTagNum := StrZero( Val( if( len( ::aIndexMgmnt ) == 0, "0", ::aIndexMgmnt[-1,INDEXMAN_TAGNUM] ) ) + 1, 6 )

   /* Create the index */

   ::oSql:Commit()      /* All Locks should be released to INDEX a table */

   nLenTag       := len( if( lHaveTag, "_" + cTag, "_" + cNextTagNum ) )
   cPhisicalName := cIndexName + if( lHaveTag, "_" + cTag, "_" + cNextTagNum )

   If len(cIndexName) + nLenTag > 30
      cPhisicalName := Left( cIndexName, 30 - nLenTag ) + if( lHaveTag, "_" + cTag, "_" + cNextTagNum )
   EndIf
   If ::oSql:nSystemID == SYSTEMID_ORACLE .and. len( cPhisicalName ) > 30     /* Oracle sucks! */
      cPhisicalName := right( cPhisicalName, 30 )
   EndIf
   If ::oSql:nSystemID == SYSTEMID_IBMDB2 .and. len( cPhisicalName ) > 18     /* DB2 sucks! */
      cPhisicalName := right( cPhisicalName, 18 )
   EndIf
   If cPhisicalName[1] == "_"
      cPhisicalName[1] := "I"
   EndIf

   If lSyntheticIndex

      lTagFound := .F.

      If len( cColumns ) > 4 .and. cColumns[4] == "@"
         cColumns := SubStr( cColumns, 5 )
      EndIf

      /* look for an empty tag column */

      For nNewTag = 1 to 120
         lTagFound := .T.
         For i = 1 to len( ::aIndexMgmnt )
            If substr(::aIndexMgmnt[i,INDEXMAN_COLUMNS],1,3) == strzero(nNewTag,3)
               lTagFound := .F.
               Exit
            EndIf
         Next
         If lTagFound .or. len( ::aIndexMgmnt ) == 0
            Exit
         EndIf
      Next

      cColIndx := "INDKEY_" + strZero( nNewTag, 3 )

      aCols := { cColIndx }

      bIndexKey := &( "{|| " + alltrim( cColumns ) + " }" )

      /* Update all records with the index key */

      nOldOrd := (::cAlias)->( indexOrd() )

      (::cAlias)->( dbSetOrder(0) )
      (::cAlias)->( dbGoTop() )

      nKeySize := len(SR_Val2Char( eval( bIndexKey ) )) +15

      /* Create the index column in the table and add it to aCols */
      If ::oSql:nSystemID == SYSTEMID_FIREBR
         ::AlterColumns( {{ cColIndx, "C", min(nKeySize,180), 0, , SQL_CHAR }}, .F., .F. )
      Else
         ::AlterColumns( {{ cColIndx, "C", min(nKeysize,254), 0, , SQL_CHAR }}, .F., .F. )
      EndIf

      ::Refresh()
*       bIndexKey := &( "{|| " + alltrim( cColumns ) + " }" )
*
*       /* Update all records with the index key */
*
*       nOldOrd := (::cAlias)->( indexOrd() )
*
*       (::cAlias)->( dbSetOrder(0) )
*       (::cAlias)->( dbGoTop() )

      If cColFor == NIL
         While !(::cAlias)->( eof() )
            IF::oSql:nSystemID == SYSTEMID_POSTGR
               ::oSql:exec( ::cUpd + cColIndx + " = E'" + SR_ESCAPESTRING( SR_Val2Char( eval( bIndexKey ) ) + str( recno(), 15 ), ::oSql:nSystemID ) + "' WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + str( (::cAlias)->( recno() ) ) )
            else
            ::oSql:exec( ::cUpd + cColIndx + " = '" + SR_ESCAPESTRING( SR_Val2Char( eval( bIndexKey ) ) + str( recno(), 15 ), ::oSql:nSystemID ) + "' WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + str( (::cAlias)->( recno() ) ) )
            ENDIF
            (::cAlias)->( dbSkip() )
         EndDo
      Else
         While !(::cAlias)->( eof() )
            IF ::oSql:nSystemID == SYSTEMID_POSTGR
               ::oSql:exec( ::cUpd + cColIndx + " = E'" + SR_ESCAPESTRING( SR_Val2Char( eval( bIndexKey ) ) + str( recno(), 15 ), ::oSql:nSystemID ) + "' WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + str( (::cAlias)->( recno() ) ) )
            ELSE
            ::oSql:exec( ::cUpd + cColIndx + " = '" + SR_ESCAPESTRING( SR_Val2Char( eval( bIndexKey ) ) + str( recno(), 15 ), ::oSql:nSystemID ) + "' WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + str( (::cAlias)->( recno() ) ) )
            ENDIF
            ::oSql:exec( ::cUpd + cColFor  + " = '" + if(eval( bIndexFor ),"T","F") + "' WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + str( (::cAlias)->( recno() ) ) )
            (::cAlias)->( dbSkip() )
         EndDo
         If len(aCols) < 9    // If index has 9 or more columns... no sense to optimize!
            aadd( aCols, cColFor )
         EndIf
      EndIf

      ::oSql:Commit()
      (::cAlias)->( dbSetOrder( nOldOrd ) )

      If ::lHistoric
         If len(aCols) < 9    // If index has 9 or more columns... no sense to optimize!
            aadd( aCols, "DT__HIST" )
         EndIf
      EndIf

      If ::oSql:nSystemID == SYSTEMID_POSTGR .and.  ::osql:lPostgresql8
               // PGS 8.3 will use it once released
         For i = 1 to len( aCols )
            cList += SR_DBQUALIFY( aCols[i], ::oSql:nSystemID ) + " NULLS FIRST"
            cList += if( i == len( aCols ), "", "," )
            cList2 += ["] + aCols[i] + ["]
            cList2 += if( i == len( aCols ), "", "," )
         Next
      Else
         For i = 1 to len( aCols )
            cList += SR_DBQUALIFY( aCols[i], ::oSql:nSystemID )
            cList += if( i == len( aCols ), "", "," )
            cList2 += ["] + aCols[i] + ["]
            cList2 += if( i == len( aCols ), "", "," )
         Next
      EndIf


      /* Drop the index */

      If !Empty(AllTrim(cConstraintName))
         If ::oSql:nSystemID == SYSTEMID_ORACLE .or. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            ::DropConstraint(::cFileName,AllTrim(cConstraintName),.T.)
         Endif
      Endif

      If aScan( aOldPhisNames, cPhisicalName ) == 0
         aadd( aOldPhisNames, cPhisicalName )
      EndIf

      Switch ::oSql:nSystemID
      Case SYSTEMID_MSSQL6
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_AZURE
      Case SYSTEMID_SYBASE
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + ::cQualifiedTableName + "." + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create synthetic Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + cName + " ON " + ::cQualifiedTableName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create synthetic Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      CASE SYSTEMID_ORACLE
         If cPhysicalVIndexName != NIL
            ::oSql:exec( "DROP INDEX " + ::cOwner + "A$" + cPhysicalVIndexName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
            ::oSql:exec( "DROP INDEX " + ::cOwner + "D$" + cPhysicalVIndexName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
         EndIf
         If cPrevPhysicalVIndexName != NIL
            ::oSql:exec( "DROP INDEX " + ::cOwner + "A$" + cPrevPhysicalVIndexName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
            ::oSql:exec( "DROP INDEX " + ::cOwner + "D$" + cPrevPhysicalVIndexName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
         EndIf
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + ::cOwner + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + ::cOwner + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql += IF(Empty(SR_SetTblSpaceIndx()), "", " TABLESPACE " + SR_SetTblSpaceIndx() )
         cSql +=  + if(::oSql:lComments," /* Create synthetic Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      CASE SYSTEMID_IBMDB2
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + ::cOwner + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + ::cOwner + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create synthetic Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      CASE SYSTEMID_FIREBR
     CASE SYSTEMID_FIREBR3
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
            ::oSql:exec( "DROP INDEX " + cName + "R" + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
         Next
         cSql := "CREATE INDEX " + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create regular Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         cSql := "CREATE DESCENDING INDEX " + cPhisicalName + "R ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create regular Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      Default
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create synthetic Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
      End

      If lRet
         ::oSql:Commit()
         cSql := "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTINDEXES (TABLE_,SIGNATURE_,IDXNAME_,PHIS_NAME_,IDXKEY_,IDXFOR_,IDXCOL_,TAG_,TAGNUM_) VALUES ( '" + UPPER(::cFileName) + "','" + DTOS(DATE()) + " " + TIME() + if(lDesc," D"," A") + "','"
         cSql += Upper(alltrim(cIndexName)) + "','" + cPhisicalName + "','" + SR_ESCAPESTRING(cColumns, ::oSql:nSystemID) + "','" + SR_ESCAPESTRING(cForDB, ::oSql:nSystemID) + "','" + strZero( nNewTag,3) + "','" + cTag + "','" + cNextTagNum + "' )"
         ::oSql:exec( cSql,.T. )
      EndIf

   Else     // Not a Synthetic Index

      If cColFor != NIL
         nOldOrd := (::cAlias)->( indexOrd() )
         (::cAlias)->( dbSetOrder(0) )
         (::cAlias)->( dbGoTop() )
         While !(::cAlias)->( eof() )
            ::oSql:exec( ::cUpd + cColFor  + " = '" + if(eval( bIndexFor ),"T","F") + "' WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + str( (::cAlias)->( recno() ) ) )
            (::cAlias)->( dbSkip() )
         EndDo
         ::oSql:Commit()
         (::cAlias)->( dbSetOrder( nOldOrd ) )
      EndIf

      If ::oSql:nSystemID == SYSTEMID_POSTGR .and.  ::osql:lPostgresql8
               // PGS 8.3 will use it once released
         For i = 1 to len( aCols )
            cList += SR_DBQUALIFY( aCols[i], ::oSql:nSystemID ) + " NULLS FIRST"
            cList += if( i == len( aCols ), "", "," )
            cList2 += ["] + aCols[i] + ["]
            cList2 += if( i == len( aCols ), "", "," )
         Next
      Else
         For i = 1 to len( aCols )
            cList += SR_DBQUALIFY( aCols[i], ::oSql:nSystemID )
            cList += if( i == len( aCols ), "", "," )
            cList2 += ["] + aCols[i] + ["]
            cList2 += if( i == len( aCols ), "", "," )
         Next
      EndIf

      /* Drop the index */

      If !Empty(AllTrim(cConstraintName))
         If ::oSql:nSystemID == SYSTEMID_ORACLE .or. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            ::DropConstraint(::cFileName,AllTrim(cConstraintName),.T.)
         Endif
      Endif

      If aScan( aOldPhisNames, cPhisicalName ) == 0
         aadd( aOldPhisNames, cPhisicalName )
      EndIf

      Switch ::oSql:nSystemID
      Case SYSTEMID_MSSQL6
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_AZURE
      Case SYSTEMID_SYBASE
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + ::cQualifiedTableName + "." + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create regular Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + cName + " ON " + ::cQualifiedTableName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create regular Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      Case SYSTEMID_ORACLE
         If cPhysicalVIndexName != NIL
            ::oSql:exec( "DROP INDEX " + ::cOwner + "A$" + cPhysicalVIndexName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
            ::oSql:exec( "DROP INDEX " + ::cOwner + "D$" + cPhysicalVIndexName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
         EndIf
         If cPrevPhysicalVIndexName != NIL
            ::oSql:exec( "DROP INDEX " + ::cOwner + "A$" + cPrevPhysicalVIndexName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
            ::oSql:exec( "DROP INDEX " + ::cOwner + "D$" + cPrevPhysicalVIndexName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
         EndIf
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + ::cOwner + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + ::cOwner + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql += IF(Empty(SR_SetTblSpaceIndx()), "", " TABLESPACE " + SR_SetTblSpaceIndx() )
         cSql +=  + if(::oSql:lComments," /* Create regular Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      CASE SYSTEMID_IBMDB2
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + ::cOwner + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + ::cOwner + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create regular Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      CASE SYSTEMID_FIREBR
     CASE SYSTEMID_FIREBR3
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
            ::oSql:exec( "DROP INDEX " + cName + "R" + if(::oSql:lComments," /* Create Index */",""), .F. )
            ::oSql:Commit()
         Next
         cSql := "CREATE INDEX " + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create regular Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         cSql := "CREATE DESCENDING INDEX " + cPhisicalName + "R ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create regular Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         Exit
      Default
         For Each cName in aOldPhisNames
            ::oSql:exec( "DROP INDEX " + cName + if(::oSql:lComments," /* Create Index */",""), .F. )
         Next
         cSql := "CREATE INDEX " + cPhisicalName + " ON " + ::cQualifiedTableName + " (" + cList + ")"
         ::oSql:Commit()
         cSql +=  + if(::oSql:lComments," /* Create regular Index */","")
         lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
      End


      If lRet
         ::oSql:Commit()

         If lSyntheticVirtual    // Should we create the Virtual Index too ?
            Switch ::oSql:nSystemID
            Case SYSTEMID_ORACLE
               cSql := "CREATE INDEX " + ::cOwner + "A$" + cPhysicalVIndexName + " ON " + ::cQualifiedTableName + " (" + ::GetSyntheticVirtualExpr( aCols ) + ")" +IF(Empty(SR_SetTblSpaceIndx()), "", " TABLESPACE " + SR_SetTblSpaceIndx() )
               lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
               ::oSql:Commit()
               If lRet
                  cSql := "CREATE INDEX " + ::cOwner + "D$" + cPhysicalVIndexName + " ON " + ::cQualifiedTableName + " (" + ::GetSyntheticVirtualExpr( aCols ) + " DESC )" +IF(Empty(SR_SetTblSpaceIndx()), "", " TABLESPACE " + SR_SetTblSpaceIndx() )
                  lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
                  ::oSql:Commit()
               EndIf
               Exit
            End
         EndIf

         If lRet
            cSql := "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTINDEXES (TABLE_,SIGNATURE_,IDXNAME_,PHIS_NAME_,IDXKEY_,IDXFOR_,IDXCOL_,TAG_,TAGNUM_) VALUES ( '" + UPPER(::cFileName) + "','" + DTOS(DATE()) + " " + TIME() + if(lDesc," D"," A") + "','"
            cSql += Upper(alltrim(cIndexName)) + "','" + cPhisicalName + "','" + If(lSyntheticVirtual, SubStr(cPhysicalVIndexName,1,3)+"@", "") + SR_ESCAPESTRING(cList2, ::oSql:nSystemID) + "','" + SR_ESCAPESTRING(cForDB, ::oSql:nSystemID) + "',NULL,'" + cTag + "','" + cNextTagNum  + "' )"
            ::oSql:exec( cSql,.T. )
            ::oSql:Commit()
         EndIf
      EndIf
   EndIf

   If !Empty(AllTrim(cConstraintName))

      cConstraintName := Upper(Alltrim(cConstraintName))
      cTargetTable    := Upper(Alltrim(cTargetTable))

      For i = 1 to Len( aTargetColumns )

         cListConstraint += aTargetColumns[i]
         cListConstraint += if( i == len( aTargetColumns ), "", "," )

         cListConstraint_Source  += aConstraintCols[i]
         cListConstraint_Source  += if( i == len( aTargetColumns ), "", "," )

      Next

      If Len(aTargetColumns) > Len(aConstraintCols)

         ::RunTimeErr("29", SR_Msg(29) + " Table: " + ::cFileName + " Index columns list: " + Upper(AllTrim(cListConstraint_Source)) + " Constraint columns list: " + Upper(AllTrim(cListConstraint)) )

      Endif

      aSize(aConstraintCols,Len(aTargetColumns))

      ::CreateConstraint(::cFileName, aConstraintCols, cTargetTable, aTargetColumns, cConstraintName)

   Endif

   ::oSql:Commit()
   ::LoadRegisteredTags()

   /*aInf := ::oSql:aTableInfo[ ::cOriginalFN ]   //What is it?
   aInf[ CACHEINFO_INDEX ] := ::aIndexMgmnt*/

   If ::oSql:nSystemID == SYSTEMID_SYBASE
      ::oSql:SetOptions( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF )
   EndIf

   ::aLastOrdCond := NIL

Return ::sqlOrderListAdd( cIndexName, cTag )

/*------------------------------------------------------------------------*/

METHOD sqlClearScope() CLASS SR_WORKAREA

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlSetScope( nType, uValue ) CLASS SR_WORKAREA

   Local uKey, nLenKey, cPart, nCons, nLen, cSep2, cExpr, aNulls, aNotNulls
   Local i, j, cType := ""
   Local lPartialSeek := .F.
   Local cRet, cRet2, nFDec, nFLen, nScoping, nSimpl
   Local nThis, cSep, cQot, cNam, nFeitos, lNull

   If len(::aIndex) > 0 .and. ::aInfo[ AINFO_INDEXORD ] > 0

      If valtype(uValue) == "B"
         uKey := eval( uValue )
      Else
         uKey := uValue
         If valtype( uKey ) == "C"
            If len( uKey ) == 0
               uKey := NIL
            EndIf
         EndIf
      EndIf

      Switch nType
      Case TOPSCOPE
         ::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE]    := uKey
         Exit
      Case BOTTOMSCOPE
         ::aIndex[::aInfo[ AINFO_INDEXORD ], BOTTOM_SCOPE] := uKey

         IF ::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE] == ::aIndex[::aInfo[ AINFO_INDEXORD ], BOTTOM_SCOPE]
            IF valtype(uKey) == "C"
               ::aIndex[::aInfo[ AINFO_INDEXORD ], BOTTOM_SCOPE] := uKey+"|"
            endif
         endif
         Exit
      Case TOP_BOTTOM_SCOPE
         ::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE]    := uKey
         ::aIndex[::aInfo[ AINFO_INDEXORD ], BOTTOM_SCOPE] := uKey
         Exit
      Default
         Return -1         /* Error */
      End

      /* Create the SQL expression based on the scope data */

      ::aIndex[::aInfo[ AINFO_INDEXORD ], SCOPE_SQLEXPR ]    := NIL
      ::aIndex[::aInfo[ AINFO_INDEXORD ], ORDER_SKIP_UP ]   := NIL
      ::aIndex[::aInfo[ AINFO_INDEXORD ], ORDER_SKIP_DOWN ] := NIL

      If nType = TOP_BOTTOM_SCOPE .or. (::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE] != NIL .and. ::aIndex[::aInfo[ AINFO_INDEXORD ], BOTTOM_SCOPE] != NIL .and.;
         ::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE] == ::aIndex[::aInfo[ AINFO_INDEXORD ], BOTTOM_SCOPE])

         nLen := Max( len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] ) - 1, 1 )      /* -1 to remove RECNO from index key */

         If valtype(uKey) $ "NDL"       /* One field, piece of cake! */

            lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],5]
            nFDec := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],4]
            nFLen := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],3]

            cQot  := ::QuotedNull(::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE],, nFLen, nFDec,, lNull)
            cSep  := if( cQot == "NULL", " IS ", " = " )
            cNam  := [A.] + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] ], ::oSql:nSystemID )

            cRet  := " " + cNam + cSep + cQot + " "

            If cQot == "NULL"
               If valtype( uKey ) == "N"
                  cRet := "( " + cRet + " OR " + cNam + " = 0 )"
               ElseIf HB_ISDATE( uKey ) //valtype( uKey ) == "D"
                  cRet := "( " + cRet + " OR " + cNam + " <= " + ::QuotedNull(stod("19000101"),, nFLen, nFDec,, lNull) + " )"
               EndIf
            EndIf

            If !empty(cRet)
               ::aIndex[::aInfo[ AINFO_INDEXORD ], SCOPE_SQLEXPR] := " ( " + cRet + " ) "
            EndIf

         ElseIf ValType(uKey) == "C"

            ::aQuoted   := {}
            ::aDat      := {}
            ::aPosition := {}
            nCons     := 0
            nLenKey   := Len( uKey )
            cPart     := ""

            /* First, split uKey in fields and values according to current index */

            For i = 1 to nLen

               nThis := ::aFields[ ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ], FIELD_LEN ]
               cPart := SubStr( uKey, nCons+1, nThis )

               If alltrim( cPart ) == "%"
                  Exit
               EndIf

               AADD( ::aPosition, ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] )

               cType := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],2]
               lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],5]
               nFDec := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],4]
               nFLen := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],3]

               If i == 1 .and. nThis >= len( uKey )
                  If uKey == ""
                     Exit
                  EndIf
               Else
                  If len( cPart ) = 0
                     Exit
                  EndIf
               EndIf

               AADD( ::aQuoted, ::QuotedNull(::ConvType( cPart, cType, @lPartialSeek, nThis, SR_SetGoTopOnScope() ),.t.,,,,lNull  ))  // This SR_SetGoTopOnScope() here is a NewAge issue.
               AADD( ::aDat,    ::ConvType( rtrim(cPart), cType, , nThis ) )

               nCons += nThis

               If nLenKey < nCons
                  Exit
               endif

            Next

            cRet    := ""
            nLen    := Min( nLen, Len( ::aQuoted ) )
            nFeitos := 0

            For i = 1 to nLen
               cQot := ::aQuoted[i]
               cNam := [A.] + SR_DBQUALIFY( ::aNames[::aIndex[::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2]], ::oSql:nSystemID )

               If lPartialSeek .and. i = nLen
                  cSep := " >= "
               Else
                  cSep := " = "
               EndIf

               If ::aQuoted[i] == "NULL"
                  cSep := if( cSep == " = ", " IS ", " IS NOT " )
               EndIf

               If i = nLen .and. "%" $ cQot
                  cSep := " LIKE "
               EndIf

               nFeitos ++
               cRet += if(nFeitos>1," AND ","") + cNam + cSep + cQot + " "
            Next

            If !empty(cRet)
               ::aIndex[::aInfo[ AINFO_INDEXORD ], SCOPE_SQLEXPR] := " ( " + cRet + " ) "
            EndIf

         ElseIf ValType(uKey) == "U"
            /* Clear scope */

         Else
            ::RuntimeErr( "26" )
            Return -1
         EndIf

      ElseIf ::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE] != NIL .or. ::aIndex[::aInfo[ AINFO_INDEXORD ], BOTTOM_SCOPE] != NIL

         nLen      := Max( len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] ) - 1, 1 )      /* -1 to remove RECNO from index key */
         aNulls    := {}
         aNotNulls := {}

         For nScoping = TOPSCOPE to BOTTOMSCOPE

            uKey      := ::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE + nScoping]
            IF HB_ISSTRING( uKey ) //ValType( uKey ) == "C"
               if uKey[ -1 ] == "|"
                  uKey := Left( uKey, len(uKey)-1)
               endif
            endif

            If valtype(uKey) $ "NDL"       /* One field, piece of cake! */

               lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],5]
               nFDec := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],4]
               nFLen := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ],3]

               If ::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE] != NIL
                  cQot  := ::QuotedNull(::aIndex[::aInfo[ AINFO_INDEXORD ], TOP_SCOPE],, nFLen, nFDec,, lNull)
                  cSep  := if( cQot == "NULL", " IS ", " >= " )
                  cNam  := [A.] + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] ], ::oSql:nSystemID )

                  cRet  := " " + cNam + cSep + cQot + " "

                  If cQot == "NULL"
                     If valtype( uKey ) == "N"
                        cRet := "( " + cRet + " OR " + cNam + " >= 0 )"
                     ElseIf HB_ISDATE( uKey ) //valtype( uKey ) == "D"
                        cRet := "( " + cRet + " OR " + cNam + " >= " + ::QuotedNull(stod("19000101"),, nFLen, nFDec,, lNull) + " )"
                     EndIf
                  EndIf
               EndIf

               If ::aIndex[::aInfo[ AINFO_INDEXORD ], BOTTOM_SCOPE] != NIL
                  cQot  := ::QuotedNull(::aIndex[::aInfo[ AINFO_INDEXORD ], BOTTOM_SCOPE],, nFLen, nFDec,, lNull)
                  cSep  := if( cQot == "NULL", " IS ", " <= " )
                  cNam  := [A.] + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,1,2 ] ], ::oSql:nSystemID )

                  cRet2 := " " + cNam + cSep + cQot + " "

                  If cQot == "NULL"
                     If valtype( uKey ) == "N"
                        cRet2 := "( " + cRet2 + " OR " + cNam + " <= 0 )"
                     ElseIf HB_ISDATE( uKey ) //valtype( uKey ) == "D"
                        cRet2 := "( " + cRet2 + " OR " + cNam + " <= " + ::QuotedNull(stod("19000101"),, nFLen, nFDec,, lNull) + " )"
                     EndIf
                  EndIf

                  If empty(cRet)
                     cRet := cRet2
                  else
                     cRet := cRet + " and " + cRet2
                  endif
               EndIf

            ElseIf HB_ISSTRING( ukey ) //ValType(uKey) == "C"

               ::aQuoted   := {}
               ::aDat      := {}
               ::aPosition := {}
               nCons     := 0
               nLenKey   := Len( uKey )
               cPart     := ""
               cSep2     := if( nScoping == TOPSCOPE, ">", "<" )

               /* First, split uKey in fields and values according to current index */

               For i = 1 to nLen

                  nThis := ::aFields[ ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ], FIELD_LEN ]
                  cPart := SubStr( uKey, nCons+1, nThis )

                  If len( alltrim( cPart ) ) < nThis .and. nScoping == BOTTOMSCOPE
                     If empty(alltrim(cPart))
                        cPart := " " + LAST_CHAR
                     Else
                        cPart := alltrim(cPart) + LAST_CHAR
                     EndIf
                  EndIf

                  AADD( ::aPosition, ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] )

                  cType := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],2]
                  lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],5]
                  nFDec := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],4]
                  nFLen := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],3]

                  If i == 1 .and. nThis >= len( uKey )
                     If uKey == ""
                        Exit
                     EndIf
                  Else
                     If len( cPart ) = 0
                        Exit
                     EndIf
                  EndIf

                  AADD( ::aQuoted, ::QuotedNull(::ConvType( cPart, cType, @lPartialSeek, nThis ),.t.,,,,lNull  ))
                  AADD( ::aDat,    ::ConvType( rtrim(cPart), cType, , nThis ) )

                  nCons += nThis

                  If nLenKey < nCons
                     Exit
                  endif

               Next

               cRet := if(empty(cRet), " (   ( ", cRet + " AND (   ( " )
               nLen := Min( nLen, Len( ::aQuoted ) )

               nSimpl := nLen
               nSimpl := Min( nSimpl, 2 )      && 3-key SET SCOPE not allowed.

               For j = 1 to nSimpl

                  nFeitos      := 0
                  cExpr        := ""

                  For i = 1 to ( nLen - j + 1 )

                     cQot := ::aQuoted[i]
                     cNam := [A.] + SR_DBQUALIFY( ::aNames[::aIndex[::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2]], ::oSql:nSystemID )

                     If lPartialSeek .and. i = nLen
                        cSep := if(j=1, " " + cSep2 + "= ", " " + cSep2 + " ")
                     Else
                        cSep := if(j=1, " " + cSep2 + "= ", " " + cSep2 + " ")
                     EndIf

                     nFeitos ++

                     If IsNull( ::aQuoted[i] )
                        If (cSep == " >= " .or. cSep == " <= ")
                           cExpr += if(nFeitos>1," AND ","") + cNam + cSep + " ' " + if( cSep == " <= ", LAST_CHAR, "" ) + "' "
                           aadd( aNulls, cNam )
                        ElseIf cSep == " = "
                           aadd( aNulls, cNam )
                        Else
                           cExpr += if(nFeitos>1," AND ","") + cNam + cSep + " ' " + LAST_CHAR + "' "
                           aadd( aNotNulls, cNam )
                        EndIf
                     Else

*                         IF ::oSql:nSystemID == SYSTEMID_POSTGR
*                            IF 'INDKEY_' IN UPPER( cNam )
*                            altd()
*                               cnam := "substr( " + cNam + ",1,"+str(len(cQot)-3) +")"
*                           ENDIF
*                         ENDIF

                        cExpr += if(nFeitos>1," AND ","") + cNam + cSep + cQot + " "
                     EndIf

                  Next

                  If j < nSimpl
                     cRet += cExpr + e" ) OR \r\n ( "
                  Else
                     cRet += cExpr
                  endif

               Next

               cRet += " )   )"

            ElseIf ValType(uKey) == "U"
               /* Clear scope */
            Else
               ::RuntimeErr( "26" )
               Return -1
            EndIf

         Next

         If len( aNulls ) > 0 .or. len( aNotNulls ) > 0
            cRet := "(" + cRet + ") ) "
            For i = 1 to len( aNulls )
               cRet := " " + aNulls[i] + " IS NULL OR " + cRet
            Next
            For i = 1 to len( aNotNulls )
               cRet := " " + aNotNulls[i] + " IS NOT NULL OR " + cRet
            Next
            cRet := " (" + cRet
         EndIf

         If !empty(cRet)
            ::aIndex[::aInfo[ AINFO_INDEXORD ], SCOPE_SQLEXPR] := cRet
         EndIf

      EndIf

      ::Refresh()

      Return 0    /* Success */

   EndIf

Return -1         /* Failure */

/*------------------------------------------------------------------------*/

METHOD sqlLock( nType, uRecord ) CLASS SR_WORKAREA

   Local lRet := .T., aVet := {}
   Local aResultSet := {}

   ::sqlGoCold()

   If nType < 3 .and. ::aInfo[ AINFO_SHARED ]
      If uRecord == NIL .or. Empty( uRecord ) .or. ascan( ::aLocked, uRecord ) > 0 .or. ::aInfo[ AINFO_ISINSERT ]
         Return .T.
      EndIf
      If nType != 2 .and. len( ::aLocked ) > 0
         ::sqlUnlock()
      EndIf
   Else
      aSize( ::aLocked, 0 )
   EndIf

   /* Sets the timeout to LOCK_TIMEOUT seconds */

   ::oSql:SetNextOpt( SQL_ATTR_QUERY_TIMEOUT, LOCK_TIMEOUT )

   Switch ::oSql:nSystemID
   Case SYSTEMID_ORACLE
   Case SYSTEMID_POSTGR

      If nType < 3
         If ::oSql:Exec( "SELECT * FROM " + ::cQualifiedTableName + if( nType < 3, " WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + ::Quoted(uRecord,,15,0), "" ) + " FOR UPDATE" + ::oSql:cLockWait ;
                                + if(::oSql:lComments," /* Line Lock */",""), .F., .T., @aResultSet,,,,, ::cRecnoName, ::cDeletedName, , ::nLogMode, SQLLOGCHANGES_TYPE_LOCK ) != SQL_SUCCESS
            lRet := .F.
         EndIf

         If Len( aResultSet ) > 0 .and. lret
            ::UpdateCache( aResultSet )
         Else
            lRet := .F.
         EndIf
      Else
         If !::LockTable( .F., .T. )
            lRet := .F.
         EndIf
      EndIf
      Exit

      /*

      Commented 2005/02/04 - It's better to wait forever on a lock than have a corrupt transaction

      If ::oSql:nSystemID == SYSTEMID_POSTGR .and. !lRet
         // This will BREAK transaction control, but it's the only way to have Postgres responding again
         If ::oSql:nTransacCount >  0
            ::oSql:Commit()
            ::oSql:nTransacCount := 0
         EndIf
         ::oSql:commit()
      EndIf

      */

   Case SYSTEMID_FIREBR
   Case SYSTEMID_FIREBR3

      If nType < 3
         If ::oSql:Exec( "SELECT * FROM " + ::cQualifiedTableName + if( nType < 3, " WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + ::Quoted( uRecord,, 15, 0 ), "" ) + " FOR UPDATE WITH LOCK" ;
                                + if(::oSql:lComments," /* Line Lock */",""), .F., .T., @aResultSet,,,,, ::cRecnoName, ::cDeletedName, , ::nLogMode, SQLLOGCHANGES_TYPE_LOCK ) != SQL_SUCCESS
            lRet := .F.
         EndIf

         If Len( aResultSet ) > 0 .and. lret
            ::UpdateCache( aResultSet )
         Else
            lRet := .F.
         EndIf
      Else
         If !::LockTable( .F., .T. )
            lRet := .F.
         EndIf
      EndIf

      Exit

   Case SYSTEMID_IBMDB2
   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB

      If nType < 3
         If ::oSql:Exec( "SELECT * FROM " + ::cQualifiedTableName + if( nType < 3, " WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + ::Quoted( uRecord,, 15, 0 ), "" ) + " FOR UPDATE" + ::oSql:cLockWait ;
                                + if(::oSql:lComments," /* Line Lock */",""), .F., .T., @aResultSet,,,,, ::cRecnoName, ::cDeletedName, , ::nLogMode, SQLLOGCHANGES_TYPE_LOCK ) != SQL_SUCCESS
            lRet := .F.
         EndIf
         If Len( aResultSet ) > 0 .and. lret
            ::UpdateCache( aResultSet )
         Else
            lRet := .F.
         EndIf
      Else
         If !::LockTable( .F., .T. )
            lRet := .F.
         EndIf
      EndIf
      Exit

   Case SYSTEMID_INGRES

      If ::oSql:Exec( "SELECT * FROM " + ::cQualifiedTableName + if( nType < 3, " WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + ::Quoted( uRecord,, 15, 0 ), "");
                             + if(::oSql:lComments," /* Lock */",""), .F., .T., @aResultSet,,,,, ::cRecnoName, ::cDeletedName, , ::nLogMode, SQLLOGCHANGES_TYPE_LOCK )  != SQL_SUCCESS
         lRet := .F.
      EndIf

      If nType < 3
         If Len( aResultSet ) > 0 .and. lRet
            ::UpdateCache( aResultSet )
         Else
            lRet := .F.
         EndIf
      EndIf
      Exit

   Case SYSTEMID_MSSQL7
   Case SYSTEMID_AZURE

      If nType < 3
         If ::oSql:Exec( "SELECT * FROM " + ::cQualifiedTableName  + " WITH (UPDLOCK) WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + ::Quoted( uRecord,, 15, 0 );
                                + if(::oSql:lComments," /* Lock row */",""), .F., .T., @aResultSet,,,,, ::cRecnoName, ::cDeletedName, , ::nLogMode, SQLLOGCHANGES_TYPE_LOCK ) != SQL_SUCCESS
            lRet := .F.
         EndIf
         If Len( aResultSet ) > 0 .and. lRet
            ::UpdateCache( aResultSet )
         Else
            lRet := .F.
         EndIf
      Else
         If !::LockTable( .F., .T. )
            lRet := .F.
         EndIf
      EndIf
      Exit

   Case SYSTEMID_SYBASE

      If ::oSql:Exec( "UPDATE " + ::cQualifiedTableName + " SET "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + ::Quoted( uRecord,,15, 0 ) + if( nType < 3, " WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + ::Quoted( uRecord,, 15, 0 ), "") +;
                            + if(::oSql:lComments," /* Lock */",""), .F., .T., @aResultSet,,,,, ::cRecnoName, ::cDeletedName, , ::nLogMode, SQLLOGCHANGES_TYPE_LOCK ) != SQL_SUCCESS
         lRet := .F.
      EndIf

      If nType < 3
         If ::oSql:Exec( "SELECT * FROM " + ::cQualifiedTableName + " WHERE "+SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" = " + ::Quoted( uRecord,, 15, 0 );
                                + if(::oSql:lComments," /* Lock */",""), .F., .T., @aResultSet,,,,, ::cRecnoName, ::cDeletedName, , ::nLogMode, SQLLOGCHANGES_TYPE_LOCK ) != SQL_SUCCESS
            lRet := .F.
         EndIf

         If Len( aResultSet ) > 0 .and. lRet
            ::UpdateCache( aResultSet )
         Else
//            SR_MsgLogFile( SR_Msg(8) + alltrim(Str(uRecord,15)) + " + " + ::cOwner + alltrim(::cFileName) + " + " + SR_Val2Char(::oSql:nRetCode) )
            lRet := .F.
         EndIf
      EndIf
      Exit

   CASE SYSTEMID_CACHE

/*
drop function newage.LOCK
drop function newage.UNLOCK

create function newage.LOCK(lockName VARCHAR(50) default 'noname',
timeout int default 4)
for newage.LockControler
returns INT
LANGUAGE OBJECTSCRIPT
{
Set lockName="^"_lockName
Lock +@lockName:timeout
Else Quit 0
Quit 1
}
*/

   End

   If ::aInfo[ AINFO_SHARED ] .and. lRet .and. nType < 3
      aadd( ::aLocked, uRecord )
   EndIf

   If lRet .and. nType == 3
      ::lTableLocked := .T.
   EndIf

   /* Reset stmt timeout */
   ::oSql:SetNextOpt( SQL_ATTR_QUERY_TIMEOUT, 0 )

Return lRet

/*------------------------------------------------------------------------*/

METHOD sqlUnLock( uRecord ) CLASS SR_WORKAREA

   (uRecord)

   ::sqlGoCold()

   If ::aInfo[ AINFO_SHARED ]
      If len( ::aLocked ) > 0 .or. ::lTableLocked
         aSize( ::aLocked, 0 )
         If ::lCanICommitNow()
            ::oSql:Commit()      /* This will release all Locks in SQL database */
         EndIf
      EndIf
      If ::lTableLocked
         ::UnlockTable()
         ::lTableLocked := .F.
      EndIf
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD sqlDrop( cFileName ) CLASS SR_WORKAREA

   If SR_ExistTable( cFileName )
      SR_DropTable( cFileName )
   ElseIf SR_ExistIndex( cFileName )
      SR_DropIndex( cFileName )
   Else
      Return .F.
   EndIf

Return .T.

/*------------------------------------------------------------------------*/

METHOD sqlExists( cFileName ) CLASS SR_WORKAREA

Return SR_File( cFileName )

/*------------------------------------------------------------------------*/

Static Function aOrd( x, y, aPos )

   Local i, cStr1 := ""
   Local cStr2 := ""

   For i = 1 to len( aPos )
      If HB_ISDATE( x[aPos[i]] )
         cStr1 += dtos( x[aPos[i]] )
         cStr2 += dtos( y[aPos[i]] )
      Else
         cStr1 += HB_VALTOSTR( x[aPos[i]] )
         cStr2 += HB_VALTOSTR( y[aPos[i]] )
      EndIf
   Next

Return cStr1 < cStr2

/*------------------------------------------------------------------------*/
#if 0
Static Function aScanIndexed( aVet, nPos, uKey, lSoft, nLen, lFound )

   Local nRet := 0
   Local first, last, mid, closest, icomp, exec, nRegress

   exec   := valtype( nPos ) == "B"
   first  := 1
   last   := len( aVet )
   mid    := int( (first+last)/2)
   lFound := .T.

   closest := mid

   While last > 0

      ItP11 := nPos
      ItP14 := nPos
      ItP2  := uKey
      ItP3  := nLen

      icomp := ItemCmp( if(exec,eval(ItP11, mid, aVet), aVet[mid,ItP14]), ItP2, ItP3 )

      If icomp == 0
         nRegress := mid
         While --nRegress > 0
            If ItemCmp( if(exec,eval(ItP11, nRegress, aVet), aVet[nRegress,ItP14]), ItP2, ItP3 ) != 0
               Exit
            EndIf
         EndDo
         Return (++nRegress)
      Else
         If first == last
            Exit
         ElseIf first == (last - 1)

            ItP11 := nPos
            ItP14 := nPos
            ItP2  := uKey
            ItP3  := nLen

            If ItemCmp( if(exec,eval(ItP11, last, aVet), aVet[last,ItP14]), ItP2, ItP3 ) == 0
               nRegress := last
               While --nRegress > 0
                  If ItemCmp( if(exec,eval(ItP11, nRegress, aVet), aVet[nRegress,ItP14]), ItP2, ItP3 ) != 0
                     Exit
                  EndIf
               EndDo
               Return (++nRegress)
            endif
            exit
         EndIf

         If icomp > 0
            last = mid
            closest := mid
         else
            first = mid
            closest := first
         endif

         mid = int(( last + first )/2)

      EndIf

   EndDo

   If lSoft .and. len( aVet ) > 0
      lFound := .F.
      if len( aVet ) > mid
         nRet   := mid + 1    // Soft seek should stop at immediatelly superior item
      Else
         nRet   := mid
      EndIf
   EndIf

Return nRet
#endif
/*------------------------------------------------------------------------*/

METHOD WhereMajor() CLASS SR_WORKAREA

   Local i, cRet := "", nLen, cSep, cNam
   Local c1 := "", c2 := "", c3 := "", c4 := "", cRet2 := "", j

   Local aQuot := {}

   If ::aInfo[ AINFO_INDEXORD ] = 0
      cRet2 := ::SolveRestrictors()
      If !Empty( cRet2 )
         cRet2 := " AND " + cRet2
      EndIf
      Return " WHERE A." + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" >= " + ::QuotedNull(::aLocalBuffer[::hnRecno],.t.,,,,.F.) + cRet2
   endif

   If (!::lOrderValid)
      Return ""
   endif

   If ::aIndex[ ::aInfo[ AINFO_INDEXORD ], ORDER_SKIP_UP ] != NIL
      Return ::aIndex[ ::aInfo[ AINFO_INDEXORD ], ORDER_SKIP_UP ]
   EndIf

   nLen := len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ], INDEX_FIELDS ] )

   For i = 1 to nLen
      c1 += if(!empty(c1)," AND ","") + "A." + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] ], ::oSql:nSystemID ) + " @3" + str(i-1,1)
   Next

   cRet := "( " + c1 + ") "

   For j = (nLen-1) to 1 STEP -1
      c2 := ""
      For i = 1 to j
         cNam := "A." + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] ], ::oSql:nSystemID )
         Do Case
         Case i = j
            cSep := " @1"  // " > "
         Case i + 1 = j
            cSep := " @2"  // " = "
         OtherWise
            cSep := " @3"  // " >= "
         EndCase
         c2 += if(!empty(c2)," AND ","") + cNam + cSep + str(i-1,1) + " "
      Next

      If !empty(c2)
         cRet += "OR ( " + c2 + ") "
      EndIf
   Next

   cRet2 := ::SolveRestrictors()

   If !Empty( cRet2 )
      cRet2 := " AND " + cRet2
   EndIf

   cRet := " WHERE ( " + cRet + " )" + cRet2
   ::aIndex[ ::aInfo[ AINFO_INDEXORD ], ORDER_SKIP_UP ] := cRet

Return cRet

/*------------------------------------------------------------------------*/

METHOD WhereVMajor( cQot ) CLASS SR_WORKAREA

   Local cRet := ""
   Local c1 := "", c2 := "", c3 := "", c4 := "", cRet2 := ""
   Local aQuot := {}

   If ::aInfo[ AINFO_INDEXORD ] = 0
      cRet2 := ::SolveRestrictors()
      If !Empty( cRet2 )
         cRet2 := " AND " + cRet2
      EndIf
      Return " WHERE A." + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" >= " + ::QuotedNull(::aLocalBuffer[::hnRecno],.t.,,,,.F.) + cRet2
   endif

   If (!::lOrderValid)
      Return ""
   endif

   DEFAULT cQot := (::cAlias)->( &( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_KEY ] ) ) + str( ::aInfo[ AINFO_RECNO ], 15 )

   cRet  := ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_EXPR ] + " >= '" + SR_ESCAPESTRING( cQot, ::oSql:nSystemID ) + "'"
   cRet2 := ::SolveRestrictors()

   If !Empty( cRet2 )
      cRet2 := " AND " + cRet2
   EndIf

   cRet := " WHERE ( " + cRet + " )" + cRet2

Return cRet

/*------------------------------------------------------------------------*/

METHOD WherePgsMajor( aQuotedCols, lPartialSeek ) CLASS SR_WORKAREA

   Local i, aRet := {}, nLen, cSep, cQot, cNam, lNull
   Local c1 := "", c2 := "", c3 := "", c4 := "", cRet := "", cRet2 := "", j
   Local aQuot := {}

   DEFAULT lPartialSeek := .T.

   If ::aInfo[ AINFO_INDEXORD ] = 0
      cRet2 := ::SolveRestrictors()
      If !Empty( cRet2 )
         cRet2 := " AND " + cRet2
      EndIf
      aRet := { "A." + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " >= " + ::Quoted(::aLocalBuffer[::hnRecno],.t.,,,,.F.) + cRet2 }
   Else

      If empty( ::aInfo[ AINFO_INDEXORD ] )
         Return { }
      endif

      If (!::lOrderValid)
         Return { }
      endif

      If aQuotedCols == NIL
         nLen  := len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] )
         For i = 1 to nLen
            AADD( aQuot, ::Quoted(::aLocalBuffer[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] ],.t.,,,,lNull) )
         Next
      Else
         nLen  := len( aQuotedCols )
         aQuot := aQuotedCols
      EndIf

      For j = nLen to 1 STEP -1

         c2 := ""

         For i = 1 to j
            lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],5]
            cQot  := aQuot[ i ]
            cNam  := "A." + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] ], ::oSql:nSystemID )

            Do Case
            Case !lPartialSeek
               cSep := " = "
            Case j == nLen .and. cQot == "NULL"
               Loop
            Case j == nLen
               cSep := " >= "
            Case i = j .and. cQot == "NULL"
               cSep := " IS NOT "
            Case i = j
               cSep := " > "
            Case i + 1 = j
               cSep := " = "
            OtherWise
               If cQot == "NULL"
                  Loop
               Else
                  cSep := " >= "
               EndIf
            EndCase

            c2 += if(!empty(c2)," AND ","") + cNam + cSep + cQot + " "

         Next

         If !empty(c2)
            aadd( aRet, "( " + c2 + ") " )
         EndIf

         If !lPartialSeek
            Exit
         EndIf

      Next

   EndIf

   cRet := ::SolveRestrictors()

   If !Empty( cRet )
      cRet := " AND ( " + cRet + " ) "
   EndIf

   aEval( aRet, { |x,i| (x), aRet[i] += cRet } )

Return aRet

/*------------------------------------------------------------------------*/

METHOD WhereMinor() CLASS SR_WORKAREA

   Local i, cRet := "", nLen, cSep, cNam
   Local c1 := "", c2 := "", c3 := "", c4 := "", cRet2 := "", j

   Local aQuot := {}

   If ::aInfo[ AINFO_INDEXORD ] = 0 .and. ::aLocalBuffer[::hnRecno] != 0
      cRet2 := ::SolveRestrictors()
      If !Empty( cRet2 )
         cRet2 := " AND " + cRet2
      EndIf
      Return " WHERE A." + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" <= " + ::QuotedNull(::aLocalBuffer[::hnRecno],.t.,,,,.F.) + cRet2
   endif

   If (!::lOrderValid)
      Return ""
   endif

   If ::aIndex[ ::aInfo[ AINFO_INDEXORD ], ORDER_SKIP_DOWN ] != NIL
      Return ::aIndex[ ::aInfo[ AINFO_INDEXORD ], ORDER_SKIP_DOWN ]
   EndIf

   nLen := len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] )

   For i = 1 to nLen
      c1 += if(!empty(c1)," AND ","") + "A." + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] ], ::oSql:nSystemID ) + " @6" + str(i-1,1)
   Next

   cRet += "( " + c1 + ") "

   For j = (nLen-1) to 1 STEP -1
      c2 := ""
      For i = 1 to j
         cNam := [A.] + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] ], ::oSql:nSystemID )
         Do Case
         Case i = j
            cSep := " @4"  // " < "
         Case i + 1 = j
            cSep := " @2"  // " = "
         OtherWise
            cSep := " @6"  // " <= "
         EndCase
         c2 += if(!empty(c2)," AND ","") + cNam + cSep + str(i-1,1) + " "
      Next

      If !empty(c2)
         cRet += if(empty(cRet),"( ","OR ( ") + c2 + ") "
      EndIf
   Next

   cRet2 := ::SolveRestrictors()

   If !Empty( cRet2 )
      cRet2 := " AND " + cRet2
   EndIf

   cRet := " WHERE ( " + cRet + " )" + cRet2
   ::aIndex[ ::aInfo[ AINFO_INDEXORD ], ORDER_SKIP_DOWN ] := cRet

Return cRet

/*------------------------------------------------------------------------*/

METHOD WhereVMinor( cQot ) CLASS SR_WORKAREA

   Local cRet := ""
   Local c1 := "", c2 := "", c3 := "", c4 := "", cRet2 := ""

   Local aQuot := {}

   If ::aInfo[ AINFO_INDEXORD ] = 0 .and. ::aLocalBuffer[::hnRecno] != 0
      cRet2 := ::SolveRestrictors()
      If !Empty( cRet2 )
         cRet2 := " AND " + cRet2
      EndIf
      Return " WHERE A." + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID )+" <= " + ::QuotedNull(::aLocalBuffer[::hnRecno],.t.,,,,.F.) + cRet2
   endif

   If (!::lOrderValid)
      Return ""
   endif

   DEFAULT cQot := (::cAlias)->( &( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_KEY ] ) ) + str( ::aInfo[ AINFO_RECNO ], 15 )

   cRet := ::aIndex[ ::aInfo[ AINFO_INDEXORD ],VIRTUAL_INDEX_EXPR ] + " <= '" + SR_ESCAPESTRING( cQot, ::oSql:nSystemID ) + "'"

   cRet2 := ::SolveRestrictors()

   If !Empty( cRet2 )
      cRet2 := " AND " + cRet2
   EndIf

   cRet := " WHERE ( " + cRet + " )" + cRet2

   ::aIndex[ ::aInfo[ AINFO_INDEXORD ], ORDER_SKIP_DOWN ] := cRet

Return cRet

/*------------------------------------------------------------------------*/

METHOD WherePgsMinor( aQuotedCols ) CLASS SR_WORKAREA

   Local i, aRet := {}, nLen, cSep, cQot, cNam, lNull
   Local c1 := "", c2 := "", c3 := "", c4 := "", cRet := "", cRet2 := "", j

   Local aQuot := {}

   If ::aInfo[ AINFO_INDEXORD ] = 0
      cRet2 := ::SolveRestrictors()
      If !Empty( cRet2 )
         cRet2 := " AND " + cRet2
      EndIf
      aRet := { "A." + SR_DBQUALIFY( ::cRecnoName, ::oSql:nSystemID ) + " <= " + ::QuotedNull(::aLocalBuffer[::hnRecno],.t.,,,,.F.) + cRet2 }
   Else

      If empty( ::aInfo[ AINFO_INDEXORD ] )
         Return { }
      endif

      If (!::lOrderValid)
         Return { }
      endif

      If aQuotedCols == NIL
         nLen  := len( ::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS ] )
         For i = 1 to nLen
            cQot  := ::Quoted(::aLocalBuffer[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] ],.t.,,,,lNull)
            AADD( aQuot, cQot )
         Next
      Else
         nLen  := len( aQuotedCols )
         aQuot := aQuotedCols
      EndIf

      For j = nLen to 1 STEP -1

         c2 := ""

         For i = 1 to j

            lNull := ::aFields[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ],5]
            cQot  := aQuot[ i ]
            cNam := "A." + SR_DBQUALIFY( ::aNames[::aIndex[ ::aInfo[ AINFO_INDEXORD ],INDEX_FIELDS,i,2 ] ], ::oSql:nSystemID )

            Do Case
            Case j == nLen .and. cQot == "NULL"
               cSep := " IS "
            Case j == nLen
               cSep := " <= "
*                if 'INDKEY_' IN UPPER(CNAM)
*                altd()
*                   cnam := "substr( " + cnam + ",1,"+str(len(cQot)-3) +")"
*                ENDIF

            Case i = j
               cSep := " < "
            Case i + 1 = j
               cSep := " = "
            OtherWise
               If  cQot == "NULL"
                  cSep := " IS "
               Else
                  cSep := " <= "
*                   if 'INDKEY_' IN UPPER(CNAM)
*                   altd()
*                      cnam := "substr( " + cnam + ",1,"+str(len(cQot)-3) +")"
*                   ENDIF
               EndIf
            EndCase

            c2 += CatSep(if(!empty(c2)," AND ",""), cNam, cSep, cQot ) + " "

         Next

         If !empty(c2)
            aadd( aRet, "( " + c2 + ") " )
         EndIf
      Next

   EndIf

   cRet := ::SolveRestrictors()

   If !Empty( cRet )
      cRet := " AND ( " + cRet + " ) "
   EndIf

   aEval( aRet, { |x,i| (x), aRet[i] += cRet } )

Return aRet

/*------------------------------------------------------------------------*/

METHOD DropColRules( cColumn, lDisplayErrorMessage, aDeletedIndexes )  CLASS SR_WORKAREA

   Local aInd, i, cPhisicalName, aIndexes, nRet := SQL_SUCCESS
   Local cPhysicalVIndexName

   cColumn := Upper( Alltrim( cColumn ) )

   aIndexes := {}
   ::oSql:Commit()
   ::oSql:exec( "SELECT PHIS_NAME_, IDXCOL_, IDXKEY_, IDXNAME_ ,TAGNUM_, TAG_ FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + UPPER(::cFileName) + "' ORDER BY IDXNAME_, TAGNUM_"  ,.T., .T., @aIndexes )

   // Check if column is used by any index and drop the index, if necessary.

   For i = 1 to len( aIndexes )

      If at( ["]+cColumn+["], aIndexes[i,3] ) > 0 .or. at([+]+cColumn+[+], [+]+alltrim(aIndexes[i,3])+[+] ) > 0 .or.;
         (Left( cColumn, 7) == "INDKEY_" .and. SubStr( cColumn, 8, 3 ) == SubStr( aIndexes[i,2], 1, 3) )

         // Drop the index

         If len(aIndexes) >= i

            cPhisicalName := alltrim(aIndexes[i,1])

            If aIndexes[i,4][4] == "@"
               cPhysicalVIndexName := SubStr(aInd[4],1,3)+SubStr(::cFileName,1,25)
            Else
               cPhysicalVIndexName := NIL
            EndIf

            Switch ::oSql:nSystemID
            Case SYSTEMID_MSSQL6
            Case SYSTEMID_MSSQL7
            Case SYSTEMID_AZURE
            Case SYSTEMID_SYBASE
               nRet := ::oSql:exec( "DROP INDEX " + ::cQualifiedTableName + "." + cPhisicalName + if(::oSql:lComments," /* Drop index before drop column */",""), lDisplayErrorMessage )
               Exit
            Case SYSTEMID_MYSQL
            Case SYSTEMID_MARIADB
               nRet := ::oSql:exec( "DROP INDEX " + cPhisicalName + " ON " + ::cQualifiedTableName + if(::oSql:lComments," /* Drop index before drop column */",""), lDisplayErrorMessage )
               Exit
            CASE SYSTEMID_ORACLE
               If cPhysicalVIndexName != NIL
                  ::oSql:exec( "DROP INDEX " + ::cOwner + "A$" + cPhysicalVIndexName + if(::oSql:lComments," /* Drop VIndex before drop column */",""), .F. )
                  ::oSql:Commit()
                  ::oSql:exec( "DROP INDEX " + ::cOwner + "D$" + cPhysicalVIndexName + if(::oSql:lComments," /* Drop VIndex before drop column */",""), .F. )
                  ::oSql:Commit()
               EndIf
               nRet := ::oSql:exec( "DROP INDEX " + ::cOwner + cPhisicalName + if(::oSql:lComments," /* Drop index before drop column */",""), lDisplayErrorMessage )
               Exit
            CASE SYSTEMID_FIREBR
         CASE SYSTEMID_FIREBR3
               nRet := ::oSql:exec( "DROP INDEX " + cPhisicalName + if(::oSql:lComments," /* Drop index before drop column */",""), lDisplayErrorMessage )
               ::oSql:Commit()
               // DELETED THE DESCENDING INDEX
               nRet := ::oSql:exec( "DROP INDEX " + cPhisicalName +"R"+ if(::oSql:lComments," /* Drop index before drop column */",""), lDisplayErrorMessage )
               ::oSql:Commit()
               Exit
            Default
               nRet := ::oSql:exec( "DROP INDEX " + cPhisicalName + if(::oSql:lComments," /* Drop index before drop column */",""), lDisplayErrorMessage )
            End

            ::oSql:Commit()

            /* Remove from catalogs */

            If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO

               nRet := ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTINDEXES WHERE TABLE_ = '" + UPPER(::cFileName) + "' AND IDXNAME_ = '" + Upper(Alltrim(aIndexes[i,4])) + "' AND TAG_ = '" + Upper(Alltrim(aIndexes[i,6])) + "'" + if(::oSql:lComments," /* Wipe index info */",""), .F. )

               ::oSql:Commit()

               If (nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO .or. nRet == SQL_NO_DATA_FOUND) .and. aDeletedIndexes <> NIL
                  AADD(aDeletedIndexes,aClone(aIndexes[i]))
               Endif

            EndIf

            If (!lDisplayErrorMessage) .and. nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO .and. nRet != SQL_NO_DATA_FOUND
               SR_LogFile( "changestruct.log", { ::cFileName, "Warning: DROP INDEX:", cPhisicalName, ::oSql:cSQLError } )
            EndIf

         EndIf

         ::oSql:Commit()

      EndIf
   Next

Return nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO .or. nRet == SQL_NO_DATA_FOUND

/*------------------------------------------------------------------------*/

METHOD DropColumn( cColumn, lDisplayErrorMessage, lRemoveFromWA )  CLASS SR_WORKAREA

   Local i
   Local nRet := SQL_SUCCESS

   DEFAULT lRemoveFromWA := .F.

   cColumn := Upper( Alltrim( cColumn ) )

   ::DropColRules( cColumn, .F. )

   Switch ::oSql:nSystemID
   Case SYSTEMID_ORACLE
   Case SYSTEMID_MSSQL6
   Case SYSTEMID_MSSQL7
   Case SYSTEMID_SQLANY
   Case SYSTEMID_SYBASE
   Case SYSTEMID_ACCESS
   Case SYSTEMID_INGRES
   Case SYSTEMID_SQLBAS
   Case SYSTEMID_ADABAS
   Case SYSTEMID_INFORM
   Case SYSTEMID_IBMDB2
   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB
   Case SYSTEMID_POSTGR
   Case SYSTEMID_CACHE
   Case SYSTEMID_AZURE
      nRet := ::oSql:exec( "ALTER TABLE " + ::cQualifiedTableName + " DROP COLUMN " + SR_DBQUALIFY( cColumn, ::oSql:nSystemID ), lDisplayErrorMessage )
      ::oSql:Commit()
      Exit
   Case SYSTEMID_FIREBR
   Case SYSTEMID_FIREBR3
      nRet := ::oSql:exec( "ALTER TABLE " + ::cQualifiedTableName + " DROP " + SR_DBQUALIFY( cColumn, ::oSql:nSystemID ), lDisplayErrorMessage )
      ::oSql:Commit()
      Exit
   End

   If (!lDisplayErrorMessage) .and. nRet != SQL_SUCCESS .and. nRet != SQL_SUCCESS_WITH_INFO
      SR_LogFile( "changestruct.log", { ::cFileName, "Warning: DROP COLUMN:", cColumn, ::oSql:cSQLError } )
   EndIf

   If lRemoveFromWA
      If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO
         If (i := aScan(::aNames, cColumn )) > 0
            aDel( ::aNames, i )
            aSize( ::aNames, len(::aNames) - 1 )
            aDel( ::aFields, i )
            aSize( ::aFields, len(::aFields) - 1 )
            ::nFields := LEN( ::aFields )
         EndIf
      EndIf
   EndIf

Return nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO .or. nRet == SQL_NO_DATA_FOUND

/*------------------------------------------------------------------------*/

METHOD AlterColumns( aCreate, lDisplayErrorMessage, lBakcup ) CLASS SR_WORKAREA

   Local lRet := .T., i, cSql, cField, lPrimary, lNotNull, lRet2, aInfo, aMultilang := {}, aField
   Local nPos_, aBack, lDataInBackup := .F.
   Local cLobs := "", cTblName
   Local lCurrentIsMultLang := .f.

   DEFAULT lBakcup := .t.

   // Release any pending transaction before a DML command

   ::sqlGoCold()

   ::oSql:Commit()
   ::oSql:nTransacCount := 0

   DEFAULT lDisplayErrorMessage := .T.

   /* Check existing column */

   For i = 1 to len( aCreate )

      aSize( aCreate[i], FIELD_INFO_SIZE )
      cLobs := ""

      DEFAULT aCreate[i,FIELD_PRIMARY_KEY] := 0
      DEFAULT aCreate[i,FIELD_NULLABLE]    := .T.
      DEFAULT aCreate[i,FIELD_MULTILANG]   := MULTILANG_FIELD_OFF

      aCreate[ i, FIELD_NAME ] := Upper( Alltrim( aCreate[ i, FIELD_NAME ] ) )
      cField   := aCreate[ i, FIELD_NAME ]
      lPrimary := aCreate[ i, FIELD_PRIMARY_KEY ] > 0

      If (nPos_ := aScan( ::aNames, {|x| alltrim(upper( x ) ) == cField} )) > 0
         // Column exists

         lCurrentIsMultLang := ::aFields[nPos_,FIELD_MULTILANG]

         If lBakcup .and. (!lCurrentIsMultLang)
            // Create backup column
            aBack := { aClone( ::aFields[nPos_] ) }
            aBack[ 1, 1 ] := "BACKUP_"
            ::AlterColumns( aBack, lDisplayErrorMessage, .F. )
            ::oSql:exec( "UPDATE " + ::cQualifiedTableName + " SET BACKUP_ = " + SR_DBQUALIFY( ::aFields[nPos_,1], ::oSql:nSystemID ), lDisplayErrorMessage )
            ::oSql:Commit()
            lDataInBackup := .T.
         EndIf
      Else
         lCurrentIsMultLang := .F.
      EndIf

      // DROP the column

      If !( lCurrentIsMultLang .and. aCreate[i,FIELD_TYPE] $ "MC" )

         ::DropColumn( cField, .F. )   // It may be a new column or not - don't care.

         cSql := "ALTER TABLE " + ::cQualifiedTableName
         cSql := cSql + " ADD " + if(::oSql:nSystemID == SYSTEMID_POSTGR, "COLUMN ", "" ) + SR_DBQUALIFY( alltrim( cField  ), ::oSql:nSystemID )
         cSql += " "

         // lNotNull := (!aCreate[i,FIELD_NULLABLE]) .or. lPrimary
         lNotNull := .F.

      EndIf

      If lCurrentIsMultLang .and. aCreate[i,FIELD_TYPE] $ "MC" .and. SR_SetMultiLang()

         aField := aClone( aCreate[i] )
         aadd( aMultilang, aClone( aCreate[i] ) )
         ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLANG WHERE TABLE_ = '" + UPPER(::cFileName) + "' AND COLUMN_ = '" + aField[FIELD_NAME] + "'" )
         ::oSql:Commit()
         SR_LogFile( "changestruct.log", { ::cFileName, "Removing MLANG column:", aField[FIELD_NAME] } )
         lRet2 := .F.

      Else

         If aCreate[i,FIELD_MULTILANG] .and. aCreate[i,FIELD_TYPE] $ "MC" .and. SR_SetMultiLang()
            aadd( aMultilang, aClone( aCreate[i] ) )
            aCreate[i,FIELD_TYPE] := "M"
         EndIf

         Do Case

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
            If (aCreate[i,FIELD_LEN] > 30)
               cSql := cSql + "VARCHAR2(" + ltrim(str(min(aCreate[i,FIELD_LEN],4000),9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
            Else
               if aCreate[i,FIELD_LEN] > nMininumVarchar2Size .and.  nMininumVarchar2Size < 30
                  cSql := cSql + "VARCHAR2(" + ltrim(str(min(aCreate[i,FIELD_LEN],4000),9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
               else
               cSql := cSql + "CHAR(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
            EndIf
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C" .or. aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_SQLBAS
            If (aCreate[i,FIELD_LEN] > 254) .or. aCreate[i,FIELD_TYPE] == "M"
               cSql := cSql + "LONG VARCHAR"
            Else
               cSql := cSql + "VARCHAR(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lPrimary, " NOT NULL", "" )
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_POSTGR .or. ::oSql:nSystemID == SYSTEMID_CACHE  .or. ::oSql:nSystemID == SYSTEMID_ADABAS .or. ::oSql:nSystemID == SYSTEMID_AZURE )
            If ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_AZURE
               IF  ::OSQL:lSqlServer2008 .AND. SR_Getsql2008newTypes()
                  IF  aCreate[i,FIELD_LEN] > 10
                     cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + if(!Empty(SR_SetCollation()), "COLLATE " + SR_SetCollation() + " " , "")  + IF(lNotNull, " NOT NULL", "")
                  ELSE
                     cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + if(!Empty(SR_SetCollation()), "COLLATE " + SR_SetCollation() + " " , "")  + IF(lNotNull, " NOT NULL", "")
                  ENDIF
               ELSE
                  cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + if(!Empty(SR_SetCollation()), "COLLATE " + SR_SetCollation() + " " , "")  + IF(lNotNull, " NOT NULL", "")
               ENDIF
            ElseIf ::oSql:nSystemID == SYSTEMID_POSTGR .and. aCreate[i,FIELD_LEN] > nMininumVarchar2Size -1 //10
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case aCreate[i,FIELD_TYPE] == "C" .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            If aCreate[i,FIELD_LEN] > 255
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. (::oSql:nSystemID == SYSTEMID_IBMDB2)
            If aCreate[i,FIELD_LEN] > 255
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHARACTER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_INGRES
            cSql := cSql + "varchar(" + LTrim(Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lPrimary, "NOT NULL PRIMARY KEY", IF(lNotNull, " NOT NULL", ""))

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_INFORM
            cSql := cSql + "CHARACTER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lPrimary, "NOT NULL PRIMARY KEY", IF(lNotNull, " NOT NULL", ""))

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_ACCESS
            If (aCreate[i,FIELD_LEN] > 254)
               cSql := cSql + "TEXT"
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")"
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
            If (aCreate[i,FIELD_LEN] > 254)
               cSql := cSql + "LONG VARCHAR "
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + "  " + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. (::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID ==SYSTEMID_FIREBR3)
            If (aCreate[i,FIELD_LEN] > 254)
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF( !Empty( ::oSql:cCharSet ), " CHARACTER SET " + ::oSql:cCharSet, "") + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF( !Empty( ::oSql:cCharSet ), " CHARACTER SET " + ::oSql:cCharSet, "")  + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
            If (aCreate[i,FIELD_LEN] > 30)
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "D") .and. (::oSql:nSystemID == SYSTEMID_ORACLE .or. ::oSql:nSystemID == SYSTEMID_SQLBAS .or. ::oSql:nSystemID == SYSTEMID_INFORM .or. ::oSql:nSystemID == SYSTEMID_INGRES .or. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) .or. ::oSql:nSystemID == SYSTEMID_FIREBR  .or. ::oSql:nSystemID ==SYSTEMID_FIREBR3 .or. ::oSql:nSystemID == SYSTEMID_CACHE)
            cSql := cSql + "DATE"

         Case (aCreate[i,FIELD_TYPE] == "D") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
            cSql := cSql + "DATETIME"

         Case (aCreate[i,FIELD_TYPE] == "D") .and. (::oSql:nSystemID == SYSTEMID_IBMDB2 .or. ::oSql:nSystemID == SYSTEMID_POSTGR  .or. ::oSql:nSystemID == SYSTEMID_ADABAS)
            cSql := cSql + "DATE"

         Case (aCreate[i,FIELD_TYPE] == "D") .and. (::oSql:nSystemID == SYSTEMID_ACCESS .or. ::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_AZURE)
            cSql := cSql + "DATETIME NULL"

         Case (aCreate[i,FIELD_TYPE] == "D") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
            cSql := cSql + "TIMESTAMP"

         Case (aCreate[i,FIELD_TYPE] == "L") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_CACHE .or. ::oSql:nSystemID == SYSTEMID_AZURE)
            cSql := cSql + "BIT"

         Case (aCreate[i,FIELD_TYPE] == "L") .and. (::oSql:nSystemID == SYSTEMID_POSTGR  .or. ::oSql:nSystemID == SYSTEMID_ADABAS  .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
            cSql := cSql + "BOOLEAN"

         Case (aCreate[i,FIELD_TYPE] == "L") .and. (( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) )
            cSql := cSql + "TINYINT"

         Case (aCreate[i,FIELD_TYPE] == "L") .and. (::oSql:nSystemID == SYSTEMID_IBMDB2 .or. ::oSql:nSystemID == SYSTEMID_FIREBR)
            cSql := cSql + "SMALLINT"

         Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
            cSql := cSql + "BIT NOT NULL"

         Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
            cSql := cSql + "NUMERIC (1) NULL"

         Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
            cSql := cSql + "SMALLINT"

         Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_INFORM
            cSql := cSql + "BOOLEAN"

         Case (aCreate[i,FIELD_TYPE] == "L") .and. ::oSql:nSystemID == SYSTEMID_INGRES
            cSql := cSql + "tinyint"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
            cSql := cSql + "CLOB"
            cLobs += if(empty(cLobs),"",",") + SR_DBQUALIFY( alltrim( cField  ), ::oSql:nSystemID )

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_IBMDB2
            cSql := cSql + "CLOB (256000) " + If( "DB2/400" $ ::oSql:cSystemName, "",  " NOT LOGGED COMPACT" )

         Case (aCreate[i,FIELD_TYPE] == "M") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7  .OR. ::oSql:nSystemID == SYSTEMID_POSTGR .OR. ::oSql:nSystemID == SYSTEMID_INFORM .OR. ::oSql:nSystemID == SYSTEMID_CACHE .or. ::oSql:nSystemID == SYSTEMID_AZURE)
            cSql := cSql + "TEXT"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            cSql := cSql + cMySqlMemoDataType

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_ADABAS
            cSql := cSql + "LONG"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_INGRES
            cSql := cSql + "long varchar"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_ACCESS
            cSql := cSql + "TEXT NULL"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
            cSql := cSql + "TEXT"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
            cSql := cSql + "LONG VARCHAR"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. (::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID ==SYSTEMID_FIREBR3)
            cSql := cSql + "BLOB SUB_TYPE 1" + IF( !Empty( ::oSql:cCharSet ), " CHARACTER SET " + ::oSql:cCharSet, "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .OR. ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_AZURE) .and. cField == ::cRecnoName
            If ::oSql:lUseSequences
               cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") IDENTITY"
            Else
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL UNIQUE"
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_CACHE .and. cField == ::cRecnoName
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") UNIQUE " + [default objectscript '##class(] + SR_GetToolsOwner() + [SequenceControler).NEXTVAL("] + ::cFileName + [")']

         Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .OR. ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_CACHE .or. ::oSql:nSystemID == SYSTEMID_AZURE)
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str (aCreate[i,FIELD_DEC])) + ") " + IF(lNotNull, " NOT NULL ", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_POSTGR .and. cField == ::cRecnoName
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") default (nextval('" + ::cOwner + LimitLen(::cFileName,3) + "_SQ')) NOT NULL UNIQUE"
         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_POSTGR
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str (aCreate[i,FIELD_DEC])) + ")  default 0 " + IF(lNotNull, " NOT NULL ", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) .and. cField == ::cRecnoName
            cSql := cSql + "BIGINT (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") NOT NULL UNIQUE AUTO_INCREMENT "
         Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            cSql := cSql + cMySqlNumericDataType + " (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str (aCreate[i,FIELD_DEC])) + ") " + IF(lNotNull, " NOT NULL ", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ORACLE .and. cField == ::cRecnoName
            cSql := cSql + "NUMBER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" +;
                 IF(lNotNull, " NOT NULL UNIQUE USING INDEX ( CREATE INDEX " + ::cOwner + LimitLen(::cFileName,3) + "_UK ON " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + "( " + ::cRecnoName + ")" +;
                 IF(Empty(SR_SetTblSpaceIndx()), "", " TABLESPACE " + SR_SetTblSpaceIndx( ) ) , "") + ")"
         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
            cSql := cSql + "NUMBER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_IBMDB2 .and. cField == ::cRecnoName
            If ::oSql:lUseSequences
               cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1, NO CACHE)"
            Else
               cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL"
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ADABAS .and. cField == ::cRecnoName
            cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL DEFAULT SERIAL"

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_IBMDB2  .or. ::oSql:nSystemID == SYSTEMID_ADABAS )
               cSql := cSql + "DECIMAL(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_INGRES .and. cField == ::cRecnoName
            cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL UNIQUE "

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_INFORM .and. cField == ::cRecnoName
            cSql := cSql + "SERIAL NOT NULL UNIQUE"

         Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_INFORM .or. ::oSql:nSystemID == SYSTEMID_INGRES)
            cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") " + IF(lPrimary, "NOT NULL PRIMARY KEY ", IF(lNotNull, " NOT NULL", ""))

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_SQLBAS
            If aCreate[i,FIELD_LEN] > 15
               cSql := cSql + "NUMBER" + IF(lPrimary, " NOT NULL", " " )
            Else
               cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" + IF(lPrimary, " NOT NULL", " " )
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ACCESS
            cSql := cSql + "NUMERIC"

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_FIREBR .and. cField == ::cRecnoName
           cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL UNIQUE "

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_FIREBR3 .and. cField == ::cRecnoName
           cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") GENERATED BY DEFAULT AS IDENTITY  NOT NULL UNIQUE "


         Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
            If aCreate[i,FIELD_LEN] > 18
               cSql := cSql + "DOUBLE PRECISION" + IF(lPrimary .or. lNotNull, " NOT NULL", " " )
            Else
               cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) +  ")" + IF(lPrimary .or. lNotNull, " NOT NULL", " " )
            EndIf
         // including xml data type
         // postgresql datetime
         Case (aCreate[i,FIELD_TYPE] == "T") .and. (::oSql:nSystemID == SYSTEMID_POSTGR )
            if aCreate[i,FIELD_LEN] == 4
               cSql := cSql + 'time  without time zone '
            else
               cSql := cSql + 'timestamp  without time zone '
            endif
         Case (aCreate[i,FIELD_TYPE] == "T") .and. ( ::osql:nSystemID == SYSTEMID_MYSQL  .or. ::osql:nSystemID == SYSTEMID_MARIADB )
         if aCreate[i,FIELD_LEN] == 4
             cSql := cSql + 'time '
         else
             cSql := cSql + 'DATETIME '
         endif

         // oracle datetime
         Case (aCreate[i,FIELD_TYPE] == "T") .and. (::oSql:nSystemID == SYSTEMID_ORACLE   .or. ::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
            cSql := cSql + 'TIMESTAMP '
         CASE (aCreate[i,FIELD_TYPE] == "T") .and. (::oSql:nSystemID == SYSTEMID_MSSQL7  ) // .AND. ::OSQL:lSqlServer2008 .AND. SR_Getsql2008newTypes()
            cSql := cSql + 'DATETIME NULL '
         CASE (aCreate[i,FIELD_TYPE] == "T") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            cSql := cSql + 'DATETIME '
         CASE (aCreate[i,FIELD_TYPE] == "V") .and. (::oSql:nSystemID == SYSTEMID_MSSQL7  )
            cSql := cSql + ' VARBINARY(MAX) '

         OtherWise
            SR_MsgLogFile(  SR_Msg(9)+cField+" ("+aCreate[i,FIELD_TYPE]+")" )

         EndCase

         If ::oSql:nSystemID == SYSTEMID_ORACLE .and. (!Empty( SR_SetTblSpaceLob() )) .and. (!Empty( cLobs ))
            cSql += " LOB (" + cLobs + ") STORE AS (TABLESPACE " + SR_SetTblSpaceLob() + ")"
         EndIf

         lRet2 := ::oSql:exec( cSql, lDisplayErrorMessage ) == SQL_SUCCESS
         ::oSql:Commit()

         lRet := lRet .and. lRet2

      EndIf

      If nPos_ > 0
         ::aFields[nPos_] := aClone( aCreate[i] )
         ::aNames[nPos_]  := aCreate[i,1]
         ::aEmptyBuffer[nPos_] := SR_BlankVar( ::aFields[-1,2], ::aFields[-1,3], ::aFields[-1,4] )
      Else
         aadd( ::aFields, aClone( aCreate[i] ) )
         aadd( ::aNames, aCreate[i,1] )
         aadd( ::aEmptyBuffer, SR_BlankVar( ::aFields[-1,2], ::aFields[-1,3], ::aFields[-1,4] ) )
         nPos_ := len( ::aFields )
      EndIf

      ::nFields := LEN( ::aFields )
      aSize( ::aLocalBuffer, ::nFields )
      aSize( ::aSelectList, ::nFields )
      aFill( ::aSelectList, 1 )           // Next SELECT should include ALL columns

      aInfo := ::oSql:aTableInfo[ ::cOriginalFN ]
      aInfo[ CACHEINFO_AFIELDS ] := ::aFields
      aInfo[ CACHEINFO_ANAMES ]  := ::aNames
      aInfo[ CACHEINFO_ABLANK ]  := ::aEmptyBuffer

      // Add multilang columns in catalog

      For each aField in aMultilang
         ::oSql:exec( "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTLANG ( TABLE_ , COLUMN_, TYPE_, LEN_, DEC_ ) VALUES ( '" + UPPER(::cFileName) + "','" + aField[FIELD_NAME] + "', '" + aField[FIELD_TYPE] + "','" + alltrim(str(aField[FIELD_LEN],8)) + "','" + alltrim(str(aField[FIELD_DEC],8)) + "' )" )
         ::oSql:Commit()
         SR_LogFile( "changestruct.log", { ::cFileName, "Adding MLANG column:", "'" + aField[FIELD_NAME] + "', '" + aField[FIELD_TYPE] + "','" + alltrim(str(aField[FIELD_LEN],8)) + "'" } )
         SR_ReloadMLHash( ::oSql )
      Next

      aMultilang := {}

      If lDataInBackup
         // Put data back in column
         If ::oSql:nSystemID == SYSTEMID_ORACLE .and. ::aFields[nPos_, 2] $ "CM"
            ::oSql:exec( "UPDATE " + ::cQualifiedTableName + " SET " + SR_DBQUALIFY( ::aFields[nPos_,1], ::oSql:nSystemID ) + " = RTRIM( BACKUP_ )", lDisplayErrorMessage )
         Else
            if ::oSql:nSystemID == SYSTEMID_POSTGR  .AND. ::aFields[nPos_, 2] != aBack[ 1, 2 ]
               IF ::aFields[nPos_, 2] =="N" .AND. aBack[ 1, 2 ] == "C"
                  ::oSql:exec( "UPDATE " + ::cQualifiedTableName + " SET " + SR_DBQUALIFY( ::aFields[nPos_,1], ::oSql:nSystemID ) + " = BACKUP_::text::numeric::integer", lDisplayErrorMessage )
               //ELSEif ::aFields[nPos_, 2] =="C" .AND. aBack[ 1, 2 ] == "N"
                  //::oSql:exec( "UPDATE " + ::cQualifiedTableName + " SET " + SR_DBQUALIFY( ::aFields[nPos_,1], ::oSql:nSystemID ) + " = BACKUP_::numeric::integer::text", lDisplayErrorMessage )
               ELSE
                  ::oSql:exec( "UPDATE " + ::cQualifiedTableName + " SET " + SR_DBQUALIFY( ::aFields[nPos_,1], ::oSql:nSystemID ) + " = BACKUP_", lDisplayErrorMessage )
               ENDIF
            ELSE
            ::oSql:exec( "UPDATE " + ::cQualifiedTableName + " SET " + SR_DBQUALIFY( ::aFields[nPos_,1], ::oSql:nSystemID ) + " = BACKUP_", lDisplayErrorMessage )
            ENDIF
         EndIf
         ::oSql:Commit()
         // Drop backup
         ::DropColumn( "BACKUP_", lDisplayErrorMessage )
         lDataInBackup := .F.
      EndIf

      If lRet2 .and. aCreate[i,FIELD_NULLABLE] !=NIL .and. valtype(aCreate[i,FIELD_NULLABLE]) == "L" .and. (!aCreate[i,FIELD_NULLABLE])  // Column should be NOT NULL
         ::AddRuleNotNull( aCreate[i,FIELD_NAME] )
      EndIf

   Next

Return lRet


METHOD AlterColumnsDirect( aCreate, lDisplayErrorMessage, lBakcup,aRemove ) CLASS SR_WORKAREA

   Local lRet := .T., i, cSql, cField, lPrimary, lNotNull, lRet2, aInfo, aMultilang := {}, aField
   Local nPos_, aBack, lDataInBackup := .F.
   Local cLobs := "", cTblName
   Local lCurrentIsMultLang := .f.
   Local nPos
   Local cSql2 := ""
   Local cSql3 := ""

   DEFAULT lBakcup := .t.

   // Release any pending transaction before a DML command

   ::sqlGoCold()

   ::oSql:Commit()
   ::oSql:nTransacCount := 0

   DEFAULT lDisplayErrorMessage := .T.

   /* Check existing column */

   For i = 1 to len( aCreate )

      aSize( aCreate[i], FIELD_INFO_SIZE )
      cLobs := ""

      DEFAULT aCreate[i,FIELD_PRIMARY_KEY] := 0
      DEFAULT aCreate[i,FIELD_NULLABLE]    := .T.
      DEFAULT aCreate[i,FIELD_MULTILANG]   := MULTILANG_FIELD_OFF

      aCreate[ i, FIELD_NAME ] := Upper( Alltrim( aCreate[ i, FIELD_NAME ] ) )
      cField   := aCreate[ i, FIELD_NAME ]
      lPrimary := aCreate[ i, FIELD_PRIMARY_KEY ] > 0

      If (nPos_ := aScan( ::aNames, {|x| alltrim(upper( x ) ) == cField} )) > 0
         // Column exists

         lCurrentIsMultLang := ::aFields[nPos_,FIELD_MULTILANG]

         If lBakcup .and. (!lCurrentIsMultLang)
            // Create backup column
            aBack := { aClone( ::aFields[nPos_] ) }
            aBack[ 1, 1 ] := "BACKUP_"
            ::AlterColumns( aBack, lDisplayErrorMessage, .F. )
            ::oSql:exec( "UPDATE " + ::cQualifiedTableName + " SET BACKUP_ = " + SR_DBQUALIFY( ::aFields[nPos_,1], ::oSql:nSystemID ), lDisplayErrorMessage )
            ::oSql:Commit()
            lDataInBackup := .T.
         EndIf
      Else
         lCurrentIsMultLang := .F.
      EndIf

      // DROP the column

      If !( lCurrentIsMultLang .and. aCreate[i,FIELD_TYPE] $ "MC" )

         cSql := "ALTER TABLE " + ::cQualifiedTableName
         IF ::oSql:nSystemID == SYSTEMID_POSTGR .or. ::oSql:nSystemID == SYSTEMID_FIREBR      .or. ::oSql:nSystemID == SYSTEMID_FIREBR3
            cSql := cSql + " ALTER " + SR_DBQUALIFY( alltrim( cField  ), ::oSql:nSystemID ) + " TYPE "

         elseif ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            cSql := cSql + " MODIFY COLUMN " + SR_DBQUALIFY( alltrim( cField  ), ::oSql:nSystemID )
         ELSEIF ::oSql:nSystemID == SYSTEMID_ORACLE
            cSql := cSql + " MODIFY (" + SR_DBQUALIFY( alltrim( cField  ), ::oSql:nSystemID )
         ELSEIF ::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .OR. ::oSql:nsystemid == SYSTEMID_CACHE .or. ::oSql:nSystemID == SYSTEMID_AZURE
            cSql := cSql + " ALTER COLUMN " + SR_DBQUALIFY( alltrim( cField  ), ::oSql:nSystemID )
         ENDIF
         cSql += " "

         // lNotNull := (!aCreate[i,FIELD_NULLABLE]) .or. lPrimary
         lNotNull := .F.

      EndIf

      If lCurrentIsMultLang .and. aCreate[i,FIELD_TYPE] $ "MC" .and. SR_SetMultiLang()

         aField := aClone( aCreate[i] )
         aadd( aMultilang, aClone( aCreate[i] ) )
         ::oSql:exec( "DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTLANG WHERE TABLE_ = '" + UPPER(::cFileName) + "' AND COLUMN_ = '" + aField[FIELD_NAME] + "'" )
         ::oSql:Commit()
         SR_LogFile( "changestruct.log", { ::cFileName, "Removing MLANG column:", aField[FIELD_NAME] } )
         lRet2 := .F.

      Else

         If aCreate[i,FIELD_MULTILANG] .and. aCreate[i,FIELD_TYPE] $ "MC" .and. SR_SetMultiLang()
            aadd( aMultilang, aClone( aCreate[i] ) )
            aCreate[i,FIELD_TYPE] := "M"
         EndIf

         Do Case
         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
            If (aCreate[i,FIELD_LEN] > 30)
               cSql := cSql + "VARCHAR2(" + ltrim(str(min(aCreate[i,FIELD_LEN],4000),9,0)) + ")" + IF(lNotNull, " NOT NULL )", ") ")
            Else
               if aCreate[i,FIELD_LEN] > nMininumVarchar2Size .and.  nMininumVarchar2Size < 30
                  cSql := cSql + "VARCHAR2(" + ltrim(str(min(aCreate[i,FIELD_LEN],4000),9,0)) + ")" + IF(lNotNull, " NOT NULL )", ") ")
               ELSE
               cSql := cSql + "CHAR(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL )", ")")
            EndIf
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C" .or. aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_SQLBAS
            If (aCreate[i,FIELD_LEN] > 254) .or. aCreate[i,FIELD_TYPE] == "M"
               cSql := cSql + "LONG VARCHAR"
            Else
               cSql := cSql + "VARCHAR(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lPrimary, " NOT NULL", "" )
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_POSTGR .or. ::oSql:nSystemID == SYSTEMID_CACHE  .or. ::oSql:nSystemID == SYSTEMID_ADABAS .or. ::oSql:nSystemID == SYSTEMID_AZURE)
            If ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_AZURE
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + if(!Empty(SR_SetCollation()), "COLLATE " + SR_SetCollation() + " " , "")  + IF(lNotNull, " NOT NULL", "")
            ElseIf ::oSql:nSystemID == SYSTEMID_POSTGR .and. aCreate[i,FIELD_LEN] > 10
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case aCreate[i,FIELD_TYPE] == "C" .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            If aCreate[i,FIELD_LEN] > 255
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. (::oSql:nSystemID == SYSTEMID_IBMDB2)
            If aCreate[i,FIELD_LEN] > 255
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHARACTER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_INGRES
            cSql := cSql + "varchar(" + LTrim(Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lPrimary, "NOT NULL PRIMARY KEY", IF(lNotNull, " NOT NULL", ""))

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_INFORM
            cSql := cSql + "CHARACTER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") " + IF(lPrimary, "NOT NULL PRIMARY KEY", IF(lNotNull, " NOT NULL", ""))

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_ACCESS
            If (aCreate[i,FIELD_LEN] > 254)
               cSql := cSql + "TEXT"
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")"
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
            If (aCreate[i,FIELD_LEN] > 254)
               cSql := cSql + "LONG VARCHAR "
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + "  " + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ( ::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
            If (aCreate[i,FIELD_LEN] > 254)
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF( !Empty( ::oSql:cCharSet ), " CHARACTER SET " + ::oSql:cCharSet, "") + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF( !Empty( ::oSql:cCharSet ), " CHARACTER SET " + ::oSql:cCharSet, "")  + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "C") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
            If (aCreate[i,FIELD_LEN] > 30)
               cSql := cSql + "VARCHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
            Else
               cSql := cSql + "CHAR (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
            cSql := cSql + "CLOB"
            cLobs += if(empty(cLobs),"",",") + SR_DBQUALIFY( alltrim( cField  ), ::oSql:nSystemID )

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_IBMDB2
            cSql := cSql + "CLOB (256000) " + If( "DB2/400" $ ::oSql:cSystemName, "",  " NOT LOGGED COMPACT" )

         Case (aCreate[i,FIELD_TYPE] == "M") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7  .OR. ::oSql:nSystemID == SYSTEMID_POSTGR .OR. ::oSql:nSystemID == SYSTEMID_INFORM .OR. ::oSql:nSystemID == SYSTEMID_CACHE .or. ::oSql:nSystemID == SYSTEMID_AZURE)
            cSql := cSql + "TEXT"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            cSql := cSql + cMySqlMemoDataType

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_ADABAS
            cSql := cSql + "LONG"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_INGRES
            cSql := cSql + "long varchar"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_ACCESS
            cSql := cSql + "TEXT NULL"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_SYBASE
            cSql := cSql + "TEXT"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
            cSql := cSql + "LONG VARCHAR"

         Case (aCreate[i,FIELD_TYPE] == "M") .and. (::oSql:nSystemID == SYSTEMID_FIREBR .or. ::oSql:nSystemID == SYSTEMID_FIREBR3)
            cSql := cSql + "BLOB SUB_TYPE 1" + IF( !Empty( ::oSql:cCharSet ), " CHARACTER SET " + ::oSql:cCharSet, "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .OR. ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_AZURE) .and. cField == ::cRecnoName
            If ::oSql:lUseSequences
               cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") IDENTITY"
            Else
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL UNIQUE"
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_CACHE .and. cField == ::cRecnoName
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") UNIQUE " + [default objectscript '##class(] + SR_GetToolsOwner() + [SequenceControler).NEXTVAL("] + ::cFileName + [")']

         Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_MSSQL6 .OR. ::oSql:nSystemID == SYSTEMID_MSSQL7 .OR. ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_CACHE .or. ::oSql:nSystemID == SYSTEMID_AZURE)
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str (aCreate[i,FIELD_DEC])) + ") " + IF(lNotNull, " NOT NULL ", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_POSTGR .and. cField == ::cRecnoName
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") default (nextval('" + ::cOwner + LimitLen(::cFileName,3) + "_SQ')) NOT NULL UNIQUE"
         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_POSTGR

            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str (aCreate[i,FIELD_DEC])) + ")" //
            nPos := ascan( ::aFields, {|x| Alltrim( UPPER( x[1] ))  == allTrim( UPPER(cField) )})
            if nPos >0
               if ::aFields[nPos,FIELD_TYPE] == "C"
                  cSql += " using "+cField+"::numeric "
               ENDIF
            ENDIF
            cSql2 := "ALTER TABLE " + ::cQualifiedTableName
            cSql2 := cSql2 + " ALTER " + SR_DBQUALIFY( alltrim( cField  ), ::oSql:nSystemID ) + " SET DEFAULT 0"

            if lNotNull
               cSql3 := "ALTER TABLE " + ::cQualifiedTableName
               cSql3 := cSql3 + " ALTER " + SR_DBQUALIFY( alltrim( cField  ), ::oSql:nSystemID ) + " SET "
               cSql3 := cSql3 + " NOT NULL "
            endif

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB ) .and. cField == ::cRecnoName
            cSql := cSql + "BIGINT (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + ") NOT NULL UNIQUE AUTO_INCREMENT "
         Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB )
            cSql := cSql + cMySqlNumericDataType + " (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str (aCreate[i,FIELD_DEC])) + ") " + IF(lNotNull, " NOT NULL ", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ORACLE .and. cField == ::cRecnoName
            cSql := cSql + "NUMBER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" +;
                 IF(lNotNull, " NOT NULL UNIQUE USING INDEX ( CREATE INDEX " + ::cOwner + LimitLen(::cFileName,3) + "_UK ON " + ::cOwner + SR_DBQUALIFY( cTblName, ::oSql:nSystemID ) + "( " + ::cRecnoName + ")" +;
                 IF(Empty(SR_SetTblSpaceIndx()), "", " TABLESPACE " + SR_SetTblSpaceIndx( ) ) , "") + ")"
         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ORACLE
            cSql := cSql + "NUMBER (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_IBMDB2 .and. cField == ::cRecnoName
            If ::oSql:lUseSequences
               cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1, NO CACHE)"
            Else
               cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL"
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ADABAS .and. cField == ::cRecnoName
            cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL DEFAULT SERIAL"

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_IBMDB2  .or. ::oSql:nSystemID == SYSTEMID_ADABAS )
               cSql := cSql + "DECIMAL(" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" + IF(lNotNull, " NOT NULL", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_INGRES .and. cField == ::cRecnoName
            cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL UNIQUE "

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_INFORM .and. cField == ::cRecnoName
            cSql := cSql + "SERIAL NOT NULL UNIQUE"

         Case (aCreate[i,FIELD_TYPE] == "N") .and. (::oSql:nSystemID == SYSTEMID_INFORM .or. ::oSql:nSystemID == SYSTEMID_INGRES)
            cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") " + IF(lPrimary, "NOT NULL PRIMARY KEY ", IF(lNotNull, " NOT NULL", ""))

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_SQLBAS
            If aCreate[i,FIELD_LEN] > 15
               cSql := cSql + "NUMBER" + IF(lPrimary, " NOT NULL", " " )
            Else
               cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ")" + IF(lPrimary, " NOT NULL", " " )
            EndIf

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_SQLANY
            cSql := cSql + "NUMERIC (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") " + IF(lNotNull, " NOT NULL", "")

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_ACCESS
            cSql := cSql + "NUMERIC"

          Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_FIREBR3 .and. cField == ::cRecnoName
           cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") GENERATED BY DEFAULT AS IDENTITY  NOT NULL UNIQUE "

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ::oSql:nSystemID == SYSTEMID_FIREBR .and. cField == ::cRecnoName
           cSql := cSql + "DECIMAL (" + ltrim (str (aCreate[i,FIELD_LEN])) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) + ") NOT NULL UNIQUE "

         Case (aCreate[i,FIELD_TYPE] == "N") .and. ( ::oSql:nSystemID == SYSTEMID_FIREBR .or.  ::oSql:nSystemID == SYSTEMID_FIREBR3)
            If aCreate[i,FIELD_LEN] > 18
               cSql := cSql + "DOUBLE PRECISION" + IF(lPrimary .or. lNotNull, " NOT NULL", " " )
            Else
               cSql := cSql + "DECIMAL (" + LTrim( Str(aCreate[i,FIELD_LEN],9,0)) + "," + LTrim( Str(aCreate[i,FIELD_DEC],9,0)) +  ")" + IF(lPrimary .or. lNotNull, " NOT NULL", " " )
            EndIf

         OtherWise
            SR_MsgLogFile(  SR_Msg(9)+cField+" ("+aCreate[i,FIELD_TYPE]+")" )

         EndCase

         If ::oSql:nSystemID == SYSTEMID_ORACLE .and. (!Empty( SR_SetTblSpaceLob() )) .and. (!Empty( cLobs ))
            cSql += " LOB (" + cLobs + ") STORE AS (TABLESPACE " + SR_SetTblSpaceLob() + ")"
         EndIf

         lRet2 := ::oSql:exec( cSql, lDisplayErrorMessage ) == SQL_SUCCESS
         ::oSql:Commit()
         if !empty( cSql2 ) .and. ::oSql:nSystemID == SYSTEMID_POSTGR
             ::osql:Exec( cSql2, lDisplayErrorMessage )
             ::osql:commit()
         endif
         if !empty( cSql3 ) .and. ::oSql:nSystemID == SYSTEMID_POSTGR
             ::osql:Exec( cSql3, lDisplayErrorMessage  )
             ::osql:commit()
         endif

         lRet := lRet .and. lRet2

      EndIf

      If nPos_ > 0
         ::aFields[nPos_] := aClone( aCreate[i] )
         ::aNames[nPos_]  := aCreate[i,1]
         ::aEmptyBuffer[nPos_] := SR_BlankVar( ::aFields[-1,2], ::aFields[-1,3], ::aFields[-1,4] )
      Else
         aadd( ::aFields, aClone( aCreate[i] ) )
         aadd( ::aNames, aCreate[i,1] )
         aadd( ::aEmptyBuffer, SR_BlankVar( ::aFields[-1,2], ::aFields[-1,3], ::aFields[-1,4] ) )
         nPos_ := len( ::aFields )
      EndIf

      ::nFields := LEN( ::aFields )
      aSize( ::aLocalBuffer, ::nFields )
      aSize( ::aSelectList, ::nFields )
      aFill( ::aSelectList, 1 )           // Next SELECT should include ALL columns

      aInfo := ::oSql:aTableInfo[ ::cOriginalFN ]
      aInfo[ CACHEINFO_AFIELDS ] := ::aFields
      aInfo[ CACHEINFO_ANAMES ]  := ::aNames
      aInfo[ CACHEINFO_ABLANK ]  := ::aEmptyBuffer

      // Add multilang columns in catalog

      For each aField in aMultilang
         ::oSql:exec( "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTLANG ( TABLE_ , COLUMN_, TYPE_, LEN_, DEC_ ) VALUES ( '" + UPPER(::cFileName) + "','" + aField[FIELD_NAME] + "', '" + aField[FIELD_TYPE] + "','" + alltrim(str(aField[FIELD_LEN],8)) + "','" + alltrim(str(aField[FIELD_DEC],8)) + "' )" )
         ::oSql:Commit()
         SR_LogFile( "changestruct.log", { ::cFileName, "Adding MLANG column:", "'" + aField[FIELD_NAME] + "', '" + aField[FIELD_TYPE] + "','" + alltrim(str(aField[FIELD_LEN],8)) + "'" } )
         SR_ReloadMLHash( ::oSql )
      Next

      aMultilang := {}

      If lDataInBackup
         // Put data back in column
         If ::oSql:nSystemID == SYSTEMID_ORACLE .and. ::aFields[nPos_, 2] $ "CM"
            ::oSql:exec( "UPDATE " + ::cQualifiedTableName + " SET " + SR_DBQUALIFY( ::aFields[nPos_,1], ::oSql:nSystemID ) + " = RTRIM( BACKUP_ )", lDisplayErrorMessage )
         Else
            ::oSql:exec( "UPDATE " + ::cQualifiedTableName + " SET " + SR_DBQUALIFY( ::aFields[nPos_,1], ::oSql:nSystemID ) + " = BACKUP_", lDisplayErrorMessage )
         EndIf
         ::oSql:Commit()
         // Drop backup
         ::DropColumn( "BACKUP_", lDisplayErrorMessage )
         lDataInBackup := .F.
      EndIf

      If lRet2 .and. aCreate[i,FIELD_NULLABLE] !=NIL .and. valtype(aCreate[i,FIELD_NULLABLE]) == "L" .and. !aCreate[i,FIELD_NULLABLE]  // Column should be NOT NULL
         ::AddRuleNotNull( aCreate[i,FIELD_NAME] )
      EndIf

      IF lRet2
         nPos := ascan( aRemove, {|x| Alltrim( UPPER( x[1] ))  == allTrim( UPPER(cField) )})
         IF nPos >0
            aDel(aRemove, nPos, .T.)
         ENDIF
      ENDIF


   Next

Return lRet



/*------------------------------------------------------------------------*/

METHOD OrdSetForClause( cFor, cForxBase ) CLASS SR_WORKAREA

   Local i, cOut := "", cWord := "", cWordUpper

   (cForxBase)

   cFor := alltrim( cFor )

   If ::aInfo[ AINFO_INDEXORD ] > 0

      For i = 1 to len( cFor )
         If (!IsDigit(cFor[i])) .and. (!IsAlpha(cFor[i])) .and. (cFor[i] != "_")
            cWordUpper := Upper( cWord )
            If len(cWord) > 0 .and. aScan( ::aNames, {|x| x == cWordUpper} ) > 0
               cOut += "A." + SR_DBQUALIFY( cWordUpper, ::oSql:nSystemID ) + cFor[i]
            Else
               cOut += cWord + cFor[i]
            EndIf
            cWord := ""
         ElseIf IsDigit(cFor[i]) .or. IsAlpha(cFor[i]) .or. cFor[i] = "_"
            cWord += cFor[i]
         Else
           cOut += cFor[i]
         EndIf
      Next

      If len(cWord) > 0
         cWordUpper := Upper( cWord )
         If aScan( ::aNames, {|x| x == cWordUpper} ) > 0
            cOut += "A." + SR_DBQUALIFY( cWordUpper, ::oSql:nSystemID ) + cFor[i]
         Else
            cOut += cWord + cFor[i]
         EndIf
      EndIf

      ::cFor  := cOut
      ::aIndex[::aInfo[ AINFO_INDEXORD ],FOR_CLAUSE] := cOut
   EndIf

Return NIL

/*------------------------------------------------------------------------*/

METHOD ParseForClause( cFor ) CLASS SR_WORKAREA

   Local i, cOut := "", cWord := "", cWordUpper

   cFor := alltrim( cFor )

   For i = 1 to len( cFor )

      If cFor[i] == '"'
         cFor[i] := "'"
      EndIf

      If (!IsDigit(cFor[i])) .and. (!IsAlpha(cFor[i])) .and. (cFor[i] != "_")

         If cFor[i] == "-" .and. cFor[i+1] == ">"        // Remove ALIAS
            cWord := ""
            i ++
            Loop
         EndIf

         cWordUpper := Upper( cWord )
         If len(cWord) > 0 .and. aScan( ::aNames, {|x| x == cWordUpper} ) > 0
            cOut += "A." + SR_DBQUALIFY( cWordUpper, ::oSql:nSystemID )
         Else
            cOut += cWord
         EndIf

         If cFor[i] == "." .and. lower(cFor[i+1]) $ "aon"       // .and. .or.
            If lower(SubStr( cFor, i, 5 )) == ".and."
               cOut += " AND "
               i += 4
               Loop
            EndIf
            If lower(SubStr( cFor, i, 5 )) == ".not."
               cOut += " ! "
               i += 4
               Loop
            EndIf
            If lower(SubStr( cFor, i, 4 )) == ".or."
               cOut += " OR "
               i += 3
               Loop
            EndIf
         EndIf

         cOut += cFor[i]
         cWord := ""
      ElseIf IsDigit(cFor[i]) .or. IsAlpha(cFor[i]) .or. cFor[i] = "_"
         cWord += cFor[i]
      Else
         cOut += cFor[i]
      EndIf
   Next

   If len(cWord) > 0
      cWordUpper := Upper( cWord )
      If aScan( ::aNames, {|x| x == cWordUpper} ) > 0
         cOut += "A." + SR_DBQUALIFY( cWordUpper, ::oSql:nSystemID ) + cFor[i]
      Else
         cOut += cWord + cFor[i]
      EndIf
   EndIf

   If upper(strTran(cOut, " ", "" )) == "!DELETED()"
      cOut := "A." + SR_DBQUALIFY( ::cDeletedName, ::oSql:nSystemID ) + " = ' '"
   EndIf

Return  cOut

/*------------------------------------------------------------------------*/

METHOD HasFilters() CLASS SR_WORKAREA

   If (!Empty( ::cFilter )) .or. (!Empty( ::cFltUsr )) .or. (!Empty( ::cFor )) .or. (!Empty( ::cScope )) .or. (::lHistoric .and. ::lHistEnable)
      Return .T.
   EndIf

Return .F.

/*------------------------------------------------------------------------*/

METHOD AddRuleNotNull( cColumn ) CLASS SR_WORKAREA

   Local lOk := .T., nCol, nRet := SQL_ERROR, uVal, cType

   nCol := aScan( ::aNames, {|x| alltrim(upper( x ) ) == alltrim(upper( cColumn ))} )

   If nCol > 0
      Switch ::aFields[nCol,2]
      Case 'C'
         uVal := "' '"
         exit
      Case 'D'
         uVal := SR_cDBValue( stod( '17550101' ) )
         exit
      Case 'N'
         uVal := '0'
         exit
      Default
         lOk := .F.
//         ::RunTimeErr("", "Cannot change NULL constraint to datatype: " + ::aFields[nCol,2] )
         exit
      End

      ::oSql:Commit()

      If !lOk
         Return .F.
      EndIf

      lOk := .F.

      nRet := ::oSql:exec( "UPDATE " +  ::cQualifiedTableName + " SET " + cColumn + " = " + uVal + " WHERE " + cColumn + " IS NULL", .F. )

      If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO .or. nRet == SQL_NO_DATA_FOUND

         ::oSql:Commit()

         Do Case
         Case ::oSql:nSystemID == SYSTEMID_IBMDB2

            If ::AlterColumns( {{ ::aFields[nCol,1], ::aFields[nCol,2], ::aFields[nCol,3], ::aFields[nCol,4], .F. }}, .T. )
               nRet := SQL_SUCCESS
            Else
               nRet := SQL_ERROR
            EndIf
         Case ::oSql:nSystemID == SYSTEMID_MYSQL .or. ::oSql:nSystemID == SYSTEMID_MARIADB

            If ::aFields[nCol,2] == "C"
               If ::aFields[nCol,3] > 255
                  cType := "VARCHAR (" + str(::aFields[nCol,3],3) + ")"
               Else
                  cType := "CHAR (" + str(::aFields[nCol,3],3) + ")"
               EndIf
            ElseIf ::aFields[nCol,2] == "N"
               cType := cMySqlNumericDataType + " (" + str(::aFields[nCol,3],4) + "," + str(::aFields[nCol,4],3) + ")"
            ElseIf ::aFields[nCol,2] == "D"
               cType = "DATE"
            EndIf
            nRet  := ::oSql:exec( "ALTER TABLE " +  ::cQualifiedTableName  + " MODIFY " + cColumn + " " + cType + " NOT NULL" , .F. )

         Case ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_AZURE

            If ::aFields[nCol,2] == "C"
               cType := "CHAR (" + str(::aFields[nCol,3],3) + ")"
            ElseIf ::aFields[nCol,2] == "N"
               cType := "NUMERIC (" + str(::aFields[nCol,3],4) + "," + str(::aFields[nCol,4],3) + ")"
            ElseIf ::aFields[nCol,2] == "D"
               cType = "DATETIME"
            EndIf
            nRet  := ::oSql:exec( "ALTER TABLE " +  ::cQualifiedTableName  + " ALTER COLUMN " + cColumn + " " + cType + " NOT NULL" , .F. )

         Case ::oSql:nSystemID == SYSTEMID_ORACLE
            nRet  := ::oSql:exec( "ALTER TABLE " +  ::cQualifiedTableName  + " MODIFY " + cColumn + " NOT NULL" , .F. )

         Case ::oSql:nSystemID == SYSTEMID_POSTGR
            nRet  := ::oSql:exec( "ALTER TABLE " +  ::cQualifiedTableName  + " ALTER COLUMN " + cColumn + " SET NOT NULL" , .F. )

         Case ::oSql:nSystemID == SYSTEMID_FIREBR
            nRet  := ::oSql:exec( "update RDB$RELATION_FIELDS set RDB$NULL_FLAG = 1 where (RDB$FIELD_NAME = '" + cColumn + "') and (RDB$RELATION_NAME = '" + ::cFileName + "')" , .T. )
         Case ::oSql:nSystemID == SYSTEMID_FIREBR3
            nRet  := ::oSql:exec( "ALTER TABLE " +  ::cQualifiedTableName  + " ALTER " + cColumn + " NOT NULL" , .F. )

         EndCase
      EndIf
   EndIf

   If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO .or. nRet == SQL_NO_DATA_FOUND
      lOk := .T.
      ::aFields[nCol,FIELD_NULLABLE] := .F.
   EndIf

Return lOk

/*------------------------------------------------------------------------*/

METHOD DropRuleNotNull( cColumn ) CLASS SR_WORKAREA

   Local lOk := .T., nCol, nRet := SQL_ERROR, cType

   nCol := aScan( ::aNames, {|x| alltrim(upper( x ) ) == alltrim(upper( cColumn ))} )

   If nCol > 0

      ::oSql:Commit()

      Do Case
      Case ::oSql:nSystemID == SYSTEMID_IBMDB2

      Case ::oSql:nSystemID == SYSTEMID_MYSQL .or.  ::oSql:nSystemID == SYSTEMID_MARIADB

         If ::aFields[nCol,2] == "C"
            cType := "CHAR (" + str(::aFields[nCol,3],3) + ")"
         ElseIf ::aFields[nCol,2] == "N"
            cType := cMySqlNumericDataType + " (" + str(::aFields[nCol,3],4) + "," + str(::aFields[nCol,4],3) + ")"
         ElseIf ::aFields[nCol,2] == "D"
            cType = "DATE"
         EndIf
         nRet  := ::oSql:exec( "ALTER TABLE " +  ::cQualifiedTableName  + " MODIFY " + cColumn + " " + cType + " NULL" , .F. )

      Case ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_AZURE

         If ::aFields[nCol,2] == "C"
            cType := "CHAR (" + str(::aFields[nCol,3],3) + ")"
         ElseIf ::aFields[nCol,2] == "N"
            cType := "NUMERIC (" + str(::aFields[nCol,3],4) + "," + str(::aFields[nCol,4],3) + ")"
         ElseIf ::aFields[nCol,2] == "D"
            cType = "DATETIME"
         EndIf
         nRet  := ::oSql:exec( "ALTER TABLE " +  ::cQualifiedTableName  + " ALTER COLUMN " + cColumn + " " + cType + " NULL" , .F. )

      Case ::oSql:nSystemID == SYSTEMID_ORACLE
         nRet  := ::oSql:exec( "ALTER TABLE " +  ::cQualifiedTableName  + " MODIFY " + cColumn + " NULL" , .F. )

      Case ::oSql:nSystemID == SYSTEMID_POSTGR
         nRet  := ::oSql:exec( "ALTER TABLE " +  ::cQualifiedTableName  + " ALTER COLUMN " + cColumn + " SET NULL" , .F. )

      EndCase
   EndIf

   If nRet == SQL_SUCCESS .or. nRet == SQL_SUCCESS_WITH_INFO .or. nRet == SQL_NO_DATA_FOUND
      lOk := .T.
      ::aFields[nCol,FIELD_NULLABLE] := .F.
   EndIf

Return lOk

/*------------------------------------------------------------------------*/

METHOD DropConstraint( cTable, cConstraintName, lFKs, cConstrType ) CLASS SR_WORKAREA

   Local lOk := .T., cSql, aLine := {}, aRet := {}, aRet2 := {}

   cTable          := Upper(AllTrim(cTable))
   cConstraintName := Upper(AllTrim(cConstraintName))

   DEFAULT lFKs := .T.

   If cConstrType == NIL
      cConstrType := ""
   Endif

   cSql := "SELECT "
   cSql += "   A.SOURCETABLE_ , A.TARGETTABLE_, A.CONSTRNAME_, A.CONSTRTYPE_ "
   cSql += "FROM "
   cSql +=     SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS A "
   cSql += "WHERE "
   cSql += "       A.SOURCETABLE_ = '" + cTable          + "' "
   cSql += "   AND A.CONSTRNAME_  = '" + cConstraintName + "' "

   aRet := {}
   ::oSql:Exec(cSql,.T.,.T.,@aRet)

   If Len(aRet) == 1

      If lFKs .and. AllTrim(aRet[1,4]) == "PK"

         cSql := "SELECT "
         cSql += "   A.SOURCETABLE_ , A.TARGETTABLE_, A.CONSTRNAME_, A.CONSTRTYPE_ "
         cSql += "FROM "
         cSql +=    SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS A "
         cSql += "WHERE "
         cSql += "       A.TARGETTABLE_   = '" + cTable + "' "
         cSql += "   AND A.CONSTRTYPE_    = 'FK' "
         cSql += "ORDER BY "
         cSql += "   A.SOURCETABLE_ , A.CONSTRNAME_ "

         aRet2 := {}
         ::oSql:Exec(cSql,.T.,.T.,@aRet2)

         For each aLine in aRet2

            ::DropConstraint(AllTrim(aLine[1]),AllTrim(aLine[3]),.F.,AllTrim(aLine[4]))

         Next

         ::DropConstraint(cTable,cConstraintName,.F.,"PK")

      Else

         Switch ::oSql:nSystemID
         Case SYSTEMID_MSSQL6
         Case SYSTEMID_MSSQL7
         Case SYSTEMID_SYBASE
         Case SYSTEMID_AZURE
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cTable,::oSql:nSystemID) + " DROP CONSTRAINT " + cConstraintName + if(::oSql:lComments," /* Create Constraint */","")
            Exit
         Case SYSTEMID_MYSQL
         Case SYSTEMID_MARIADB
            If AllTrim(cConstrType) == "PK"
               cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cTable,::oSql:nSystemID) + " DROP PRIMARY KEY " + if(::oSql:lComments," /* Create Constraint */","")
            Else
               cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cTable,::oSql:nSystemID) + " DROP FOREIGN KEY " + cConstraintName + if(::oSql:lComments," /* Create Constraint */","")
            Endif
            Exit
         Case SYSTEMID_ORACLE
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cTable,::oSql:nSystemID) + " DROP CONSTRAINT " + cConstraintName + if(::oSql:lComments," /* Create Constraint */","")
            Exit
         Default
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cTable,::oSql:nSystemID) + " DROP CONSTRAINT " + cConstraintName + if(::oSql:lComments," /* Create Constraint */","")
         End

         lOk := ::oSql:exec(cSql,.T.) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO
         ::oSql:Commit()

         If lOk
            ::oSql:exec("DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS   WHERE SOURCETABLE_ = '" + cTable + "' AND CONSTRNAME_ = '" + cConstraintName + "'" + if(::oSql:lComments," /* Wipe constraint info 01 */",""), .T. )
            ::oSql:Commit()
            ::oSql:Exec("DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRSRCCOLS WHERE SOURCETABLE_ = '" + cTable + "' AND CONSTRNAME_ = '" + cConstraintName + "'" + if(::oSql:lComments," /* Wipe constraint info 01 */",""),.T.)
            ::oSql:Commit()
            ::oSql:Exec("DELETE FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRTGTCOLS WHERE SOURCETABLE_ = '" + cTable + "' AND CONSTRNAME_ = '" + cConstraintName + "'" + if(::oSql:lComments," /* Wipe constraint info 01 */",""),.T.)
            ::oSql:Commit()
         Endif

      Endif

   Endif

Return lOk

/*------------------------------------------------------------------------*/

METHOD CreateConstraint( cSourceTable, aSourceColumns, cTargetTable, aTargetColumns, cConstraintName ) CLASS SR_WORKAREA

   Local i, cSql, lRet, aRet := {}, nCol, lPk := .F., aRecreateIndex := {}
   Local cSourceColumns := "", cTargetColumns := "", cCols

   cSourceTable    := Upper(AllTrim(cSourceTable))
   cTargetTable    := Upper(AllTrim(cTargetTable))
   cConstraintName := Upper(AllTrim(cConstraintName))

   // You can pass constraint KEY as a list or as array
   If valtype( aTargetColumns ) == "A" .and. len( aTargetColumns ) > 0 .and. valtype( aTargetColumns[1] ) == "A"
      aTargetColumns := aTargetColumns[1]
   EndIf

   // check if the constraint already exists....
   ::oSql:exec( "SELECT A.CONSTRNAME_ FROM " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS A WHERE A.SOURCETABLE_ = '" + cSourceTable + "' AND A.CONSTRNAME_ = '" + cConstraintName + "'", .F., .T., @aRet )

   If Len(aRet) == 0
      For i = 1 to len( aSourceColumns )
         cSourceColumns   += SR_DBQUALIFY( aSourceColumns[i], ::oSql:nSystemID )
         cSourceColumns   += if( i == len( aSourceColumns ), "", "," )
      Next

      For i = 1 to len( aTargetColumns )
         cTargetColumns   += SR_DBQUALIFY( aTargetColumns[i], ::oSql:nSystemID )
         cTargetColumns   += if( i == len( aTargetColumns ), "", "," )
      Next

      If ::oSql:nSystemID == SYSTEMID_ORACLE .and. Len(cConstraintName) > 30     /* Oracle sucks! */
         cConstraintName := Right(cConstraintName, 30)
      EndIf

      If ::oSql:nSystemID == SYSTEMID_IBMDB2 .and. Len(cConstraintName) > 18     /* DB2 sucks! */
         cConstraintName := Right(cConstraintName, 18)
      EndIf
      // ? "OK", AllTrim(::cFileName) , AllTrim(cTargetTable) , Upper(AllTrim(cSourceColumns)) , Upper(AllTrim(cTargetColumns))
      lPk := (AllTrim(::cFileName) == AllTrim(cTargetTable) .and. Upper(AllTrim(cSourceColumns)) == Upper(AllTrim(cTargetColumns)))

      If lPk   /* primary key, so lets perform an alter column setting not null property first... */

         For i = 1 to Len(aTargetColumns)

            nCol := aScan( ::aNames, { |x| Upper(Alltrim(x)) == Upper(Alltrim( aTargetColumns[i])) } )

            If nCol > 0 .and. ::aFields[nCol,FIELD_NULLABLE]
               If ::oSql:nSystemID == SYSTEMID_MSSQL6 .or. ::oSql:nSystemID == SYSTEMID_MSSQL7 .or. ::oSql:nSystemID == SYSTEMID_SYBASE .or. ::oSql:nSystemID == SYSTEMID_AZURE
                  If !::DropColRules( aTargetColumns[i], .F., @aRecreateIndex )
                     ::RunTimeErr("30", SR_Msg(30) + " Table: " + ::cFileName + " Column: " + aTargetColumns[i] )
                  Endif
               Endif
               If !::AddRuleNotNull( aTargetColumns[i] )
                  ::RunTimeErr("30", SR_Msg(30) + " Table: " + ::cFileName + " Column: " + aTargetColumns[i] )
               Endif
            Endif

         Next

         For i = 1 to Len(aRecreateIndex)
            cCols := StrTran(StrTran(StrTran( AllTrim(aRecreateIndex[i,3]),::cRecnoName,""),["],""),",","+") // this comment fixes my stupid text editor's colorizer"
            If cCols[Len(cCols)] == "+"
               cCols := Left(cCols,(Len(cCols)-1))
            Endif
            ::sqlOrderCreate( AllTrim(aRecreateIndex[i,4]), cCols , AllTrim(aRecreateIndex[i,6]) )
         Next

      Endif

      Switch ::oSql:nSystemID
      Case SYSTEMID_MSSQL6
      Case SYSTEMID_MSSQL7
      Case SYSTEMID_SYBASE
      Case SYSTEMID_MYSQL
      Case SYSTEMID_MARIADB
      Case SYSTEMID_AZURE
         If lPk
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cSourceTable,::oSql:nSystemID) + " ADD CONSTRAINT " + cConstraintName + " PRIMARY KEY (" + cTargetColumns + ")"
         Else
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cSourceTable,::oSql:nSystemID) + " ADD CONSTRAINT " + cConstraintName + " FOREIGN KEY (" + cSourceColumns + ") REFERENCES " + ::cOwner + SR_DBQUALIFY(cTargetTable,::oSql:nSystemID) + " (" + cTargetColumns + ")"
         Endif
         Exit
      Case SYSTEMID_ORACLE
         If lPk
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cSourceTable,::oSql:nSystemID) + " ADD CONSTRAINT " + cConstraintName + " PRIMARY KEY (" + cTargetColumns + ")"
         Else
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cSourceTable,::oSql:nSystemID) + " ADD CONSTRAINT " + cConstraintName + " FOREIGN KEY (" + cSourceColumns + ") REFERENCES " + ::cOwner + SR_DBQUALIFY(cTargetTable,::oSql:nSystemID) + " (" + cTargetColumns + ") "
         Endif
         Exit
      Case SYSTEMID_FIREBR
      Case SYSTEMID_FIREBR3
         If lPk
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cSourceTable,::oSql:nSystemID) + " ADD CONSTRAINT " + cConstraintName + " PRIMARY KEY (" + cTargetColumns + ")"
         Else
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cSourceTable,::oSql:nSystemID) + " ADD CONSTRAINT " + cConstraintName + " FOREIGN KEY (" + cSourceColumns + ") REFERENCES " + ::cOwner + SR_DBQUALIFY(cTargetTable,::oSql:nSystemID) + " (" + cTargetColumns + ")"
         Endif
         Exit
      CASE SYSTEMID_POSTGR
         cSourceColumns:= strtran(cSourceColumns,'"',"")
         cTargetColumns:= strtran(cTargetColumns,'"',"")
         If lPk
            cSql := "ALTER TABLE " + ::cOwner + strtran(SR_DBQUALIFY(cSourceTable,::oSql:nSystemID),'"','') + " ADD CONSTRAINT " + cConstraintName + " PRIMARY KEY (" + cTargetColumns + ")"
         Else
            cSql := "ALTER TABLE " + ::cOwner + strtran(SR_DBQUALIFY(cSourceTable,::oSql:nSystemID),'"','') + " ADD CONSTRAINT " + cConstraintName + " FOREIGN KEY (" + cSourceColumns + ") REFERENCES " + ::cOwner + strtran(SR_DBQUALIFY(cTargetTable,::oSql:nSystemID),'"','') + " (" + cTargetColumns + ")"
         Endif
         exit
      Default
         If lPk
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cSourceTable,::oSql:nSystemID) + " ADD CONSTRAINT " + cConstraintName + " PRIMARY KEY (" + cTargetColumns + ")"
         Else
            cSql := "ALTER TABLE " + ::cOwner + SR_DBQUALIFY(cSourceTable,::oSql:nSystemID) + " ADD CONSTRAINT " + cConstraintName + " FOREIGN KEY (" + cSourceColumns + ") REFERENCES " + ::cOwner + SR_DBQUALIFY(cTargetTable,::oSql:nSystemID) + " (" + cTargetColumns + ")"
         Endif
      End

      If ::oSql:nSystemID == SYSTEMID_ORACLE .and. lPK
         cSql += If(Empty(SR_SetTblSpaceIndx()), "", " USING INDEX TABLESPACE " + SR_SetTblSpaceIndx() )
      EndIf

      cSql +=  + if(::oSql:lComments," /* Create constraint */","")

      ::oSql:Commit()
      lRet := ::oSql:exec( cSql, .T. ) == SQL_SUCCESS .or. ::oSql:nRetCode == SQL_SUCCESS_WITH_INFO

      If lRet

         ::oSql:Commit()

         cSql := "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTCONSTRAINTS "
         cSql += "   (SOURCETABLE_ , TARGETTABLE_, CONSTRNAME_, CONSTRTYPE_) "
         cSql += "VALUES "
         cSql += "   ('" + cSourceTable + "','" + cTargetTable + "', '" + cConstraintName + "','"
         cSql +=     If(lPk,"PK","FK") + "')"
         ::oSql:exec( cSql,.T. )

         // aSourceColumns and aTargetColumns arrays has the same size (does it? sure???), so we can use just one loop...
         For i = 1 to Len(aSourceColumns)

            cSql := "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTCONSTRSRCCOLS "
            cSql += "   (SOURCETABLE_, CONSTRNAME_, ORDER_, SOURCECOLUMN_) "
            cSql += "VALUES "
            cSql += " ('" + cSourceTable + "','" + cConstraintName + "','" + StrZero(i,2) + "','" + aSourceColumns[i] + "')"
            ::oSql:exec( cSql,.T. )

            cSql := "INSERT INTO " + SR_GetToolsOwner() + "SR_MGMNTCONSTRTGTCOLS "
            cSql += "   (SOURCETABLE_, CONSTRNAME_, ORDER_, TARGETCOLUMN_) "
            cSql += "VALUES "
            cSql += " ('" + cSourceTable + "','" + cConstraintName + "','" + StrZero(i,2) + "','" + aTargetColumns[i] + "')"
            ::oSql:exec( cSql,.T. )

         Next

         ::oSql:Commit()

      EndIf

   Endif

Return NIL



/*------------------------------------------------------------------------*/

Function SR_ParseFileName( cInd )

   Local i, cRet := ""

   For i = len( cInd ) to 1 STEP -1
      If cInd[i] == "."
         cRet := ""
         Loop
      endif
      If cInd[i] $ "\/:"
         Exit
      EndIf
      cRet := cInd[i] + cRet
   Next

Return alltrim( cRet )

/*------------------------------------------------------------------------*/

Static Function CatSep( cP, cNam, cSep, cQot )

   Local cRet := ""

   If cQot == "NULL"
      Do Case
      Case cSep == " >= "
         cRet := ""
      Case cSep == " = " .or. cSep == " <= "
         cRet := cP + cNam + " IS NULL"
      Case cSep == " > "
         cRet := cP + cNam + " IS NOT NULL"
      Case cSep == " < "      && Query is dead (Killed)
         cRet := cP + " 0 = 1"
      EndCase
   Else
      cRet := cP + cNam + cSep + cQot
   EndIf

Return cRet

/*------------------------------------------------------------------------*/

Function SR_CleanTabInfoCache()
   local oCnn := SR_GetConnection()

   If HB_ISOBJECT( oCnn )
      oCnn:aTableInfo := { => }
   EndIf
Return NIL

/*------------------------------------------------------------------------*/

Function SR_SetlGoTopOnFirstInteract( l )

   Local lOld := lGoTopOnFirstInteract

   If l != NIL
      lGoTopOnFirstInteract := l
   EndIf

Return lOld

/*------------------------------------------------------------------------*/

Function SR_SetnLineCountResult( l )

   Local lOld := nLineCountResult

   If l != NIL
      nLineCountResult := l
   EndIf

Return lOld

/*------------------------------------------------------------------------*/

Function SR_SetUseDTHISTAuto( l )

   Local lOld := lUseDTHISTAuto

   If l != NIL
      lUseDTHISTAuto := l
   EndIf

Return lOld

/*------------------------------------------------------------------------*/

Static Function LimitLen( cStr, nLen )

   If len( cStr ) > (MAX_TABLE_NAME_LENGHT - nLen)
      Return SubStr( cStr, 1, MAX_TABLE_NAME_LENGHT - nLen )
   EndIf

Return cStr

/*------------------------------------------------------------------------*/

Function SR_GetGlobalOwner()

Return cGlobalOwner

/*------------------------------------------------------------------------*/

Function SR_SetGlobalOwner( cOwner )

   local cOld := cGlobalOwner
   local oSql

   If cOwner != NIL
      cGlobalOwner := cOwner
   Else
      If Empty(cGlobalOwner)
         oSql := SR_GetCnn()
         If valtype( oSql ) == "O" .and. (!Empty( oSql:cOwner ))
            Return oSql:cOwner
         EndIf
      EndIf
   EndIf

Return cOld

/*------------------------------------------------------------------------*/

Function SR_UseSequences( oCnn )

   DEFAULT oCnn := SR_GetConnection()

   If HB_ISOBJECT( oCnn )
      Return oCnn:lUseSequences
   EndIf

Return .T.

/*------------------------------------------------------------------------*/

Function SR_SetUseSequences( lOpt, oCnn )

   local lOld := .T.
   DEFAULT oCnn := SR_GetConnection()

   If HB_ISOBJECT( oCnn )
      lOld := oCnn:lUseSequences
      oCnn:lUseSequences := lOpt
   EndIf

Return lOld

/*------------------------------------------------------------------------*/

Function SR_SetMySQLMemoDataType(cOpt)
   local cOld := cMySqlMemoDataType
   If cOpt != NIL
      cMySqlMemoDataType := cOpt
   EndIf
Return cOld

/*------------------------------------------------------------------------*/

Function SR_SetMySQLNumericDataType(cOpt)
   local cOld := cMySqlNumericDataType
   If cOpt != NIL
      cMySqlNumericDataType := cOpt
   EndIf
Return cOld

/*------------------------------------------------------------------------*/

Function SR_TCNextRecord( oWA )

   Local aRet := {}

   Switch oWA:oSQL:nSystemID
   Case SYSTEMID_SYBASE
   Case SYSTEMID_MSSQL7
   Case SYSTEMID_AZURE
      oWA:exec( "SELECT isnull(max(R_E_C_N_O_),0) + 1 AS R_E_C_N_O_ FROM " + SR_DBQUALIFY( oWA:cFileName, oWA:oSql:nSystemID ),.F.,.T.,@aRet )
      Exit
   Case SYSTEMID_ORACLE
      oWA:exec( "SELECT nvl(max(R_E_C_N_O_),0) + 1 AS R_E_C_N_O_ FROM " + SR_DBQUALIFY( oWA:cFileName, oWA:oSql:nSystemID ),.F.,.T.,@aRet )
      Exit
   Case SYSTEMID_IBMDB2
      oWA:exec( "SELECT value(max(R_E_C_N_O_),0) + 1 AS R_E_C_N_O_ FROM " + SR_DBQUALIFY( oWA:cFileName, oWA:oSql:nSystemID ),.F.,.T.,@aRet )
      Exit
   Case SYSTEMID_POSTGR
   Case SYSTEMID_MYSQL
   Case SYSTEMID_MARIADB
      oWA:exec( "SELECT coalesce(max(R_E_C_N_O_),0) + 1 AS R_E_C_N_O_ FROM " + SR_DBQUALIFY( oWA:cFileName, oWA:oSql:nSystemID ),.F.,.T.,@aRet )
      Exit
   End

Return if(len(aRet)>0,aRet[1,1],0)

/*------------------------------------------------------------------------*/

Function SR_SetlUseDBCatalogs( lSet )
   local lOld := lUseDBCatalogs
   If lSet != NIL
      lUseDBCatalogs := lSet
   EndIf
Return lOld

/*------------------------------------------------------------------------*/

Function SR_SetAllowRelationsInIndx( lSet )
   local lOld := lAllowRelationsInIndx
   If lSet != NIL
      lAllowRelationsInIndx := lSet
   EndIf
Return lOld

/*------------------------------------------------------------------------*/

Function SR_Serialize1( uVal )
   Local cMemo := SR_STRTOHEX( HB_Serialize( uVal ) )
Return SQL_SERIALIZED_SIGNATURE + str(len(cMemo),10) + cMemo

/*------------------------------------------------------------------------*/

STATIC FUNCTION OracleMinVersion( cString )

STATIC s_reEnvVar
LOCAL cMatch, nStart, nLen
Local cTemp := cString

   IF s_reEnvVar == NIL
      s_reEnvVar := HB_RegexComp( "(([2]([0-4][0-9]|[5][0-5])|[0-1]?[0-9]?[0-9])[.]){3}(([2]([0-4][0-9]|[5][0-5])|[0-1]?[0-9]?[0-9]))" )
   ENDIF

   cMatch := HB_AtX( s_reEnvVar, cString , @nStart, @nLen )

RETURN IF( EMPTY( cMatch ), 0, Val( hb_atokens( cMatch ,'.')[ 1 ] ) )

METHOD RecnoExpr()
Local cRet := ""

Local aItem
cRet +=  "( " +::cRecnoname  + " IN ( "
For each aItem in  ::aRecnoFilter
     cRet += Alltrim( str( aItem ) ) +","
next
cRet := Substr( cRet, 1, Len( cRet ) - 1 ) + " ) ) "
return cRet

REQUEST SR_FROMXML,SR_arraytoXml,SR_DESERIALIZE

function SR_arraytoXml( a )
* Local cItem
Local hHash
LOCAL oXml := TXmlDocument():new() // Cria um objeto Xml
Local oNode
* Local oNode1
Local aItem
nPosData:=0
hhash := hash()

hHash[ "version" ] := '1.0'

hHash[ "encoding"] := "utf-8"
oNode := tXMLNode(  ):New( HBXML_TYPE_PI, "xml", hHash,[version="1.0" encoding="utf-8"]  )
oXml:oRoot:Addbelow( oNode)
hhash:=hash()
hhash['Type'] := valtype( a )
hhash['Len']  := Alltrim( Str( Len( a ) ) )
hHash['Id' ]  :=alltrim(str(nStartId))
hHash['FatherId' ]  :=alltrim('-1')
oNode := tXMLNode(  ):New( HBXML_TYPE_TAG, "Array",hhash )
For each aItem in a
    addNode( aItem,ONode )
next
hhash:={}
oXml:oRoot:addBelow(oNode)
return oXml


static function AddNode( a,oNode)
Local oNode1
Local hHash := Hash()
* Local oNode2
Local aItem
* local theData
hhash['Type'] := valtype( a )

if HB_ISARRAY( a )
   hhash['Len']  := Alltrim( Str( Len( a ) ) )
   hhash['Type'] := valtype( a )
   aadd(aFather, nStartId)
   ++nStartId
   hHash['Id' ]  :=alltrim(str(nStartId))
   hHash['FatherId' ]  :=alltrim(str(aFather[-1]))
   hHash['Pos'] := alltrim(Str(++nPosData))
   aadd( aPos ,nPosData )

   nPosData :=0
   oNode1:= tXMLNode(  ):New( HBXML_TYPE_TAG, "Array",hhash )
   For each aItem in a
      AddNode( aItem,oNode1)
      //oNode1:addbelow( onode2 )
   next
   nStartId := aFather[ -1 ]
   nPosData := aPos[-1]
   adel(aFather,len( aFather ),.t.)
   adel(aPos,len( aPos ),.t.)
   oNode:addbelow(oNode1)
else
   if HB_ISNUMERIC( a )
      hHash['Value']:= Alltrim( str( a ) )
   elseif HB_ISLOGICAL( a )
      hHash['Value']:= if(a,'T','F')
   elseif HB_ISDATE( a )
      hHash['Value']:= dtos(a)
   ELSE
      hHash['Value']:= a
   endif
   hHash['Pos'] := alltrim(Str(++nPosData))
   hHash['Id' ]  :=alltrim(str(nStartId))
   oNode1 := tXMLNode(  ):New( HBXML_TYPE_TAG, "Data",hhash )
   oNode:addBelow(oNode1)
endif
return  nil


function SR_fromXml(oDoc, aRet,nLen,c)
Local oNode
* Local Curnode
* Local nId
Local CurPos
Local nStart :=0


  if nLen == -1 .and. !'<Array' $ c
     aRet :={}
     return {}
  ENDIF
  if nLen == -1 .and. !"<?xml" $ c
  c:=[<?xml version="1.0" encoding="utf-8"?>]+c
  endif
  if oDoc == NIL
     oDoc := txmldocument():new(c)
  ENDIF

  oNode := oDoc:CurNode
  oNode:=oDoc:Next()
  DO WHILE oNode != NIL
      if oNode:nType ==6 .or. oNode:nType == 2
         oNode := oDoc:Next()
         loop
      endif

      oNode := oDoc:CurNode
      if oNode:cName == "Array"
         if Val(oNode:AATTRIBUTES['Id'] ) == 0    .and. Val(oNode:AATTRIBUTES['FatherId']) == -1
            aRet := Array(Val(oNode:AATTRIBUTES['Len'] ))
         ELSEIF Val(oNode:AATTRIBUTES['Id'] ) == 0
            CurPos := Val(oNode:AATTRIBUTES['Pos'] )
            aRet[CurPos] := Array(Val(oNode:AATTRIBUTES['Len'] ))
            SR_fromXml(@oDoc,@aRet[CurPos],Val(oNode:AATTRIBUTES['Len'] ))
         ELSE
            CurPos := Val(oNode:AATTRIBUTES['Pos'] )
            aRet[CurPos] := Array(Val(oNode:AATTRIBUTES['Len'] ))
            SR_fromXml(@oDoc,@aRet[CurPos],Val(oNode:AATTRIBUTES['Len'] ))
         endif
      endif
      if oNode:cName == "Data"
         //IF Val(oNode:AATTRIBUTES['Id'] ) == 0

            CurPos := Val(oNode:AATTRIBUTES['Pos'] )
               if oNode:AATTRIBUTES['Type'] == "C"
                  aRet[CurPos] :=  oNode:AATTRIBUTES['Value']
               elseif oNode:AATTRIBUTES['Type'] == "L"
                  aRet[CurPos] :=  IF(oNode:AATTRIBUTES['Value']=="F", .F., .T.)
               elseif oNode:AATTRIBUTES['Type'] == "N"
                  aRet[CurPos] := val(oNode:AATTRIBUTES['Value'])
               elseif oNode:AATTRIBUTES['Type'] == "D"
                  aRet[CurPos] := Stod(oNode:AATTRIBUTES['Value'])
               endif
         //ELSE
         //endif
      endif
      nStart++
      If nStart == nLen
         exit
      end
      oNode := oDoc:Next()

  enddo
return aret

FUNCTION SR_getUseXmlField()
RETURN lUseXmlField

FUNCTION SR_SetUseXmlField( l )
   lUseXmlField := l
RETURN NIL

FUNCTION SR_getUseJSON()
RETURN lUseJSONField

FUNCTION SR_SetUseJSON( l )
   lUseJSONField := l
RETURN NIL

FUNCTION SR_SetMininumVarchar2Size( n )
   nMininumVarchar2Size := n
RETURN NIL
FUNCTION SR_SetOracleSyntheticVirtual( l )
   lOracleSyntheticVirtual := l
RETURN NIL

FUNCTION SR_GetOracleSyntheticVirtual(  )

RETURN lOracleSyntheticVirtual