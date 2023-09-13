request ADS

#include "sqlrdd.ch"
#include "sqlodbc.ch"
#include "common.ch"
#include "hbclass.ch"
#include "ads.ch"
#ifndef __XHARBOUR__
   #include "hbcompat.ch"
   #include "xhbcls.ch"
#endif
static aOraclipHash := {=>}
Static aOraClipCursors := {=>}
static nIdCursor := 1
static hplVars := {}
static nlasterror := 0
static lReleaseBind := .f.
static aDestroyFiles :={}

function OraExecSql( n,c,adata)
Local cbind,i
(n)
if aData != nil .and. valtype(adata) == "A"
for i:= len( aData )  to  1 step -1
      cBind := ":"+alltrim(str( i))
      c := strtran( c,cBind,   sr_cdbvalue(adata[i]) )
next
endif


return sr_getconnection():exec(c,,.f.)


function cssel1(n,aret,csql,adata)
return OraSel1(n,@aret,csql,adata)

function  OraSeek( nCursor, aData, cTable , cWhere , aVarSust )
Local cSql := "select * from " + cTable
Local nRet
if !empty(cWhere )
cSql += " where " + cWhere
endif
aOraClipCursors[nCursor]["oraseek"] := .T.
nRet := OraSel1(nCursor,@aData,csql,aVarSust)
return nret


FUNCTION OraSel1(n,aret,csql,adata)

Local oSql := sr_getconnection()
Local nError
Local i
Local cBind := ""
Local aTemp := {}
//Local aDataRet
Local cursor
Local lRowId :=.f.

Local cTmpFile
Local nArea := SelecT()
Local aTmp
local ncount :=0
set server local
SR_SetRDDTemp("ADT")
closecursor(n)
aOraClipCursors[n]["cursoropen"] := .f.
if adata == nil

   if ( "ROWID" in csql )
      aOraClipCursors[n]["data"]:={}
   nError:= sr_getconnection():exec(csql,,.t.,@aret)
      lRowId :=.t.

   else
   nError := ExecuteSql( csql, @cursor, n )//sr_getconnection():exec(csql,,.t.,@aret)
*    aOraClipCursors[n]["ret"] := aOraClipCursors[n]["data"]
   endif

   if nError == 0
      aOraClipCursors[n]["cursoropen"] := .T.
      aOraClipCursors[n]["cursor"] := cursor
      aOraClipCursors[n]["start"] := 1
      aOraClipCursors[n]["len"] := 0
      aOraClipCursors[n]["curpos"] := 1
      aOraClipCursors[n]["error"] := 0
      aOraClipCursors[n]["nrowread"] := -1
      aOraClipCursors[n]["lastsql"] := cSql
      aOraClipCursors[n]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
      if !lRowid
         aOraClipCursors[n]["aFields"] := sr_getconnection():IniFields(.f.) //aFields
         aOraClipCursors[n]["data"] := {}
         aOraClipCursors[n]["completed"] := .F.
         aOraClipCursors[n]["eof"] := .f.

         for each atmp in aOraClipCursors[n]["aFields"]
         /*if "TO_CHAR(" in UPPER(atmp[1])
            atmp[1]:=substr(atmp[1],at("TO_DATE(",upper(atmp[1]))+9)
            atmp[1]:=substr(atmp[1],1,at(",",upper(atmp[1]))-1)
         endif
         if "DECODE(" in UPPER(atmp[1])
            atmp[1]:=substr(atmp[1],at("DECODE(",upper(atmp[1]))+8)
            atmp[1]:=substr(atmp[1],1,at(",",upper(atmp[1]))-1)
            ENDIF
         */
         atmp[1]:="fld"+strzero(ncount++,5)
         next

         fclose(HB_FTEMPCREATE('.','tmp',,@cTmpFile))
         aOraClipCursors[n]["aliastmp"] := StrTran( StrTran( cTmpFile, ".", "" ), "\", "" )
         if file(strtran(cTmpfile,".\",""))
            ferase(strtran(cTmpfile,".\",""))
         endif
         if file(cTmpfile)
            ferase(cTmpfile)
         endif
         aOraClipCursors[n]["tmpfile"] :=cTmpFile
         dbCreate( cTmpFile, SR_AdjustNum(aOraClipCursors[n]["aFields"] ), SR_SetRDDTemp() )
         dbuseArea( .t., SR_SetRDDTemp(), cTmpFile, aOraClipCursors[n]["aliastmp"] )
         cTmpFile := strtran(cTmpfile,".\","")
         OraFetch( n )
         aRet := aOraClipCursors[n]["data"]

      nlasterror:=0
      else
      if len(aRet) >= 1
            aOraClipCursors[n]["data"] := aret[1]
            aret := aclone(aOraClipCursors[n]["data"])
         endif
         aOraClipCursors[n]["completed"] := .F.
         aOraClipCursors[n]["eof"]  :=.F.
         nlasterror:=0
      endif
     /* if len(aRet) >= 1
         aDataRet := aRet[1]
         aret := aclone(aDataRet)
      endif*/
   else
      aOraClipCursors[n]["eof"]  :=.T.
      aOraClipCursors[n]["error"]  = SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[n]["lastsql"] := ""
      aOraClipCursors[n]["rowaffected"] := 0
      nlasterror := SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[n]["aFields"] := {}
   endif
   if ( nArea>0 )
      select(nArea)
   endif
   return nError

endif

for i:=1 to len( aData )
      cBind := ":"+alltrim(str( i))
      cSql := strtran( cSql,cBind,   sr_cdbvalue(adata[i]) )
next
*    nError:= sr_getconnection():exec(csql,,.t.,@aret)
   if ( "ROWID" in csql )
      aOraClipCursors[n]["data"]:={}
   nError:= sr_getconnection():exec(csql,,.t.,@aret)
      lRowId :=.t.

   else
      nError := ExecuteSql( csql, @cursor, n )//sr_getconnection():exec(csql,,.t.,@aret)
   endif

   //aOraClipCursors[n]["ret"] := aclone(aret)
   if nError == 0
      aOraClipCursors[n]["cursoropen"] := .T.
      aOraClipCursors[n]["cursor"] := cursor
      aOraClipCursors[n]["start"] := 1
      aOraClipCursors[n]["len"] := 0
      aOraClipCursors[n]["curpos"] := 0
      aOraClipCursors[n]["error"] := 0
      aOraClipCursors[n]["nrowread"] := -1
      aOraClipCursors[n]["lastsql"] := cSql
      aOraClipCursors[n]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
      if !lRowid
         aOraClipCursors[n]["aFields"] :=  sr_getconnection():IniFields(.f.) //aFields
         aOraClipCursors[n]["completed"] := .F.
         aOraClipCursors[n]["eof"] := .f.
nlasterror :=0
         fclose(HB_FTEMPCREATE('.','tmp',,@cTmpFile))
         aOraClipCursors[n]["aliastmp"] := StrTran( StrTran( cTmpFile, ".", "" ), "\", "" )
         if file(strtran(cTmpfile,".\",""))
            ferase(strtran(cTmpfile,".\",""))
         endif
         if file(cTmpfile)
            ferase(cTmpfile)
         endif
         for each atmp in aOraClipCursors[n]["aFields"]
         /*
         if "TO_CHAR(" in atmp[1]
         atmp[1]:=substr(atmp[1],at("TO_DATE(",upper(atmp[1]))+9)
         atmp[1]:=substr(atmp[1],1,at(",",upper(atmp[1]))-1)
         endif
            if "DECODE(" in UPPER(atmp[1])
            atmp[1]:=substr(atmp[1],at("DECODE(",upper(atmp[1]))+8)
            atmp[1]:=substr(atmp[1],1,at(",",upper(atmp[1]))-1)
            ENDIF
            */
            atmp[1]:="fld"+strzero(ncount++,5)
         next

         aOraClipCursors[n]["tmpfile"] :=cTmpFile
         dbCreate( cTmpFile, SR_AdjustNum(aOraClipCursors[n]["aFields"] ), SR_SetRDDTemp() )
         dbuseArea( .t., SR_SetRDDTemp(), cTmpFile, aOraClipCursors[n]["aliastmp"] )
         cTmpFile := strtran(cTmpfile,".\","")
         orafetch(n)
         aret := aOraClipCursors[n]["data"]
      else
      if len(aRet) >= 1
            aOraClipCursors[n]["data"] := aret[1]
            aret := aclone(aOraClipCursors[n]["data"])
         endif
         aOraClipCursors[n]["completed"] := .F.
         aOraClipCursors[n]["eof"]  :=.F.
         nlasterror:=0
      endif

   else
      aOraClipCursors[n]["eof"]  :=.T.
      aOraClipCursors[n]["error"]  = SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[n]["lastsql"] := ''
      aOraClipCursors[n]["rowaffected"] := 0
      aOraClipCursors[n]["aFields"] := {}
nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
   endif
   if ( nArea>0 )
      select(nArea)
   endif
return nError

function orafound(n)
local lret := .f.
if aOraClipCursors[n]["oraseek"]
   lret := !aOraClipCursors[n]["eof"]
   aOraClipCursors[n]["eof"] :=.f.
   aOraClipCursors[n]["oraseek"]  :=.f.
   return lret

endif
if aOraClipCursors[n]["eof"]  .and. aOraClipCursors[n]["completed"]
return .f.
endif
return len(aOraClipCursors[n]["data"]) >0


function OraDelete( nCursor2,cTabOrgMat,cwhere ,adata )
Local csql := "delete from " +  cTabOrgMat
Local i
Local oSql := sr_getconnection()
Local nError
Local cBind



if pCount() == 3
   cSql +=  " where " +  cwhere
elseif pCount() == 4

   for i:=1 to len( aData )
      cBind := ":"+alltrim(str( i))
      cwhere := strtran( cwhere ,cBind, sr_cdbvalue(adata[i]) )
   next
   cSql += " where " +  cwhere
endif
   nError:= sr_getconnection():exec(csql,,.f.)
   if nError == 0
      aOraClipCursors[nCursor2]["error"] := 0
      aOraClipCursors[nCursor2]["lastsql"] := cSql
      aOraClipCursors[ncursor2]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
      nlasterror :=0
   else
      aOraClipCursors[nCursor2]["error"]  = SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[nCursor2]["lastsql"] := ''
      aOraClipCursors[ncursor2]["rowaffected"] := 0
      nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
   endif

return nError

function OraCommit()
 sr_committransaction()
return  0
function OraBegin()
sr_begintransaction()
return  0
function OraRollBack()
 sr_rollbacktransaction()
return  0
function OraErrMsg()
Local c,e
try
c:=SQLO_GETERRORDESCR( sr_getconnection():hDBC )
catch e
c:= "usuario nao conectado"
end
return c

function OraUpdate( nCursor,cTabAutos,aCols,aDadosAlt,cWhere,aChave )
Local csql := "update " +  cTabAutos + " set "

Local n,i,e
Local oSql := sr_getconnection()
Local nError,cbind
Local nPos

nPos := ascan(acols,{|x| upper(x) == "ROWID"})
for n := 1  to len( aDadosAlt )
   if nPos >0
      if nPos != n
   cSql += acols[n ] + "=" + sr_cdbvalue( aDadosAlt[n] ) + ","
      endif
   else
   cSql += acols[n ] + "=" + sr_cdbvalue( aDadosAlt[n] ) + ","
   endif
next
cSql := substr( csql,1,len(csql)-1)

if pcount() == 5
    csql +=  " where " +  cwhere
elseif pcount() == 6


   for i:= len( aChave)  to 1 step -1

      cBind := ":"+alltrim(str( i))
      cwhere := strtran( cwhere ,cBind, sr_cdbvalue( aChave[i] ) )
   next
   cSql += " where " +  cwhere

endif
   try
      nError:= sr_getconnection():exec(csql,,.f.)
      aOraClipCursors[nCursor]["error"]  := 0
      aOraClipCursors[nCursor]["lastsql"] := cSql
      aOraClipCursors[ncursor]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
      aOraClipCursors[ncursor]["cwhere"] := cwhere
nlasterror :=0
   CATCH e
      nerror := - 1
      aOraClipCursors[nCursor]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[nCursor]["lastsql"] := ''
      aOraClipCursors[ncursor]["rowaffected"] := 0
nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
   End
return nError

function OraOpen(n,ncursor)
nCursor := nIdCursor
aOraclipHash[n] := hash()
aOraclipHash[n][nIdCursor] := hash()
aOraClipCursors[nIdCursor] := hash()
aOraClipCursors[nIdCursor]["oraseek"] := .f.
aOraClipCursors[nIdCursor]["cursoropen"] := .f.
++nIdCursor
return 0

function cslogon(cCnxName, cUser, cPwd, cAlias, nCnxType)
return OraLogon(cCnxName, cUser, cPwd, cAlias, nCnxType)

function ocInitialize( cConexion, cUsuario, cPassword , cAlias )
return OraLogon(cConexion, cUsuario, cPassword , cAlias )

function  OraLogon(cCnxName, cUser, cPwd, cAlias)
Local cString
Local nRet
cString := "UID="+cUSer+";PWD="+cPwd
if !empty(cAlias )
cString += ";TNS="+cAlias
endif
nRet := sr_addconnection(CONNECT_ORACLE,cstring)

if nRet >0
   aOraclipHash[cCnxName] := hash()
   aOraclipHash[cCnxName]["nRet"] := nRet
   aOraclipHash[cCnxName]["alias"] := cAlias
   aOraclipHash[cCnxName]["time"] := dtoc(date())+'-'+time()
   aOraclipHash[cCnxName]["user"] := cUSer
   aOraclipHash[cCnxName]["pwd"] :=  cPwd
endif
return if(nRet>0,0,nret)

function cslogoff(cCnxName)
return  OraLogoff(cCnxName)
function  OraLogoff(cCnxName)
Local e
Local hData
try
   hData := aOraclipHash[cCnxName]
   if !empty(hdata)
       sr_endconnection( hData["nRet"])
       aOraclipHash[cCnxName]["nRet"] := nil
   endif
   catch e
      sr_endconnection()
end
return nil

function oraalias(cCnxName)
return  aOraclipHash[cCnxName]["alias"]

function oralogtime(cCnxName)
return  aOraclipHash[cCnxName]["time"]

function orauser(ccnxname)
return aOraclipHash[cCnxName]["user"]

function orapwd(ccnxname)
return aOraclipHash[cCnxName]["pwd"]

function oraintrans(ccnxname)
return SR_TransactionCount(aOraclipHash[cCnxName]["nRet"] )


function csskip(n,aData,nPos)
return oraskip(n,@aData,nPos)

//Alteracao
function oraskip(n,aData,nPos)
Local i, lPrimeiro := .T.
default nPos to 1
if nPos>0
    if aOraClipCursors[n]["curpos"] == 0
       aOraClipCursors[n]["curpos"] := 1
    endif
    For i:=1 to nPos

       aOraClipCursors[n]["curpos"]++

       OraFetch(n)

    Next

    *if aOraClipCursors[n]["curpos"]+1 <= aOraClipCursors[n]["len"]
      aOraClipCursors[n]["curpos"]++
      //aData := aOraClipCursors[n]["ret"][aOraClipCursors[n]["curpos"]]
      aData := aOraClipCursors[n]["data"]
    *endif
else
    For i:=nPos to 1 STEP -1
        aOraClipCursors[n]["curpos"]--
    Next

   if aOraClipCursors[n]["curpos"]-1 >= aOraClipCursors[n]["start"]
      OraFetch(n)
      //aData := aOraClipCursors[n]["ret"][aOraClipCursors[n]["curpos"]]
      aData := aOraClipCursors[n]["data"]
   endif

endif

return  0
function cseof(n)
return oraeof(n)
function oraeof(n)
local lreturn := .f.
local i
If aOraClipCursors[n]["completed"]
   lreturn := .T. // ( aOraClipCursors[n]["aliastmp"] )->( eof() )
Else
   If !aOraClipCursors[n]["completed"] .and. aOraClipCursors[n]["eof"]
      lreturn:= .T.
   Else
      lreturn:= .F.
   EndIf
EndIf
if lReturn
   for i:=1 to len(  aOraClipCursors[n]["data"] )
       aOraClipCursors[n]["data"][i]:=GerGhost(aOraClipCursors[n]["data"][i])
   next
endif
Return lreturn

//return aOraClipCursors[n]["curpos"] >= aOraClipCursors[n]["len"]

function csbof(n)
return  orabof(n)
function orabof(n)
return aOraClipCursors[n]["curpos"] == aOraClipCursors[n]["start"]

function orazap()
return nil

function orastruct(n,ctable)
Local aStru
Local csql := "select * from " + ctable + " where 1 == 1"
(n)
use (csql) new via "SQLRDD" alias "ZZZZZZZZZZ"
aStru := zzzzzzzzzz->(dbstruct())
zzzzzzzzzz->(dbclosearea())
return astru








function  OraSetPwd(nCursor, cUser, cPassword)
Local cSql := 'alter user '+ cUser + ' identified by ' + cPassword
Local nRet
(nCursor)
nRet := sr_getconnection():exec( cSql,, .f. )
return  nRet


FUNCTION OraSingle(n,csql,adata)

Local oSql := sr_getconnection()
Local nError
Local aRet := {},i,e,cBind

if adata == nil
   nError:= sr_getconnection():exec(csql,,.t.,@aret)
   if nError == 0
      aOraClipCursors[n]["error"]  := 0
      aOraClipCursors[n]["lastsql"] := cSql
      aOraClipCursors[n]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
nlasterror :=0
      aOraClipCursors[n]["aFields"] := sr_Getconnection():aFields
      if len(aret) == 1
         if len( aRet[1]) == 1
            return aret[1,1]
         else
            return aret[1]
         endif
      endif

   else
      aOraClipCursors[n]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
aOraClipCursors[n]["lastsql"] := ''
      aOraClipCursors[n]["rowaffected"] := 0
      aOraClipCursors[n]["aFields"] := {}
   endif
   return aret

endif

   for i:=1 to len( aData )
      cBind := ":"+alltrim(str( i))
      cSql := strtran( cSql,cBind, sr_cdbvalue(adata[i]) )
   next

    TRY
       nError:= sr_getconnection():exec(csql,,.t.,@aret)
    CATCH e
       nerror := - 1
    End


   aOraClipCursors[n]["ret"] := aret
   if nError == 0
      aOraClipCursors[n]["start"] := 1
      aOraClipCursors[n]["len"] := len(aret)
      aOraClipCursors[n]["curpos"] := 1
      aOraClipCursors[n]["error"]  := 0
      aOraClipCursors[n]["nrowread"] := -1
      aOraClipCursors[n]["lastsql"] := cSql
      aOraClipCursors[n]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
      aOraClipCursors[n]["aFields"] := sr_getconnection():aFields
nlasterror :=0
   else
      aOraClipCursors[n]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[n]["lastsql"] := ''
      aOraClipCursors[n]["rowaffected"] := 0
      aOraClipCursors[n]["aFields"] := {}
   endif

return nError



function OraInsert(nCursor, cTable, aCols, aData)
Local cSql := 'insert into ' + ctable
Local cValues := ""
Local i
Local nError,e
Local osql:=sr_getconnection()

if len(acols) > 0
cSql += "( "

for  i:= 1  to len( acols)
if upper(aCols[i]) == "SR_DELETED"
   cSql += aCols[i]+","
   cValues += "' ',"
elseif    upper(aCols[i]) == "ROWID"
else
   cSql += aCols[i]+","
   cValues += sr_cdbvalue(aData[i])+","
endif
next

endif
if len(acols ) == 0 .and. len(adata ) > 0
for  i:= 1  to len( aData )

   cValues += sr_cdbvalue(aData[i])+","
next

endif

if len(acols ) > 0
cSql := substr(cSql,1,len(cSql)-1) + ") VALUES ("
else
csql += " values ( "
endif
cValues := substr(cValues,1,len(cValues)-1) + ")"
cSql += cValues

   TRY
      nError:= sr_getconnection():exec(csql,,.f.,)
      aOraClipCursors[nCursor]["error"]  := 0
      aOraClipCursors[nCursor]["lastsql"] := cSql
      aOraClipCursors[ncursor]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
      aOraClipCursors[ncursor]["acols"] := acols
      aOraClipCursors[ncursor]["avalues"] := aData
nlasterror :=0

   CATCH e
      nerror := - 1
      aOraClipCursors[nCursor]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[nCursor]["lastsql"] := cSql
      aOraClipCursors[ncursor]["rowaffected"] := 0
   End


return nError


function      OraCount(nCursor, cTabela, cWhere, aVarSust)
Local nlen := 0
Local aRet := {}
Local nErro
Local cSql ,i,cbind
if pcount() < 2
   return nil
endif
if pcount() == 2
cSql := "select count(*) from " +cTabela
elseif pCount() == 3
cSql := "select count(*) from " +cTabela + " where " + cwhere
elseif pCount() == 4
   cSql := "select count(*) from " +cTabela
   for i:=1 to len( aVarSust)
      cBind := ":"+alltrim(str( i))
      cwhere := strtran( cwhere ,cBind, sr_cdbvalue( aVarSust[i] ) )
   next
   cSql += " where " +  cwhere

endif
nErro := sr_Getconnection():exec(cSql,,.t.,@aret)
if nErro ==0
   aOraClipCursors[nCursor]["error"]  := 0
   aOraClipCursors[nCursor]["lastsql"] := cSql
*    aOraClipCursors[ncursor]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
nlasterror :=0
   return aRet[1,1]
else
   aOraClipCursors[nCursor]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
   aOraClipCursors[nCursor]["lastsql"] := cSql
*    aOraClipCursors[ncursor]["rowaffected"] := 0
nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
endif
return 0
function oraset()
return nil


function oraerror(n)
if pcount()==0
return nlasterror
endif
return aOraClipCursors[n]["error"]
function   csSetVar(cVarName, cOraType, nLen, nDec, xInitValue)
return OraSetVar(cVarName, cOraType, nLen, nDec, xInitValue)

function   OraSetVar(cVarName, cOraType, nLen, nDec, xInitValue)
default nlen to 1
default ndec to 0

if lReleaseBind
lReleaseBind := .F.
hplvars:={}
endif
aadd(hplVars,  {UPPER(cVarName), UPPER(cOraType), nLen+5, nDec, xInitValue,})
return nil

function csplsql(nCursor, cPLSQL, aVarSust)
return OraPLSQL(nCursor, cPLSQL, aVarSust)
function      OraPLSQL(nCursor, cPLSQL, aVarSust)
Local cBind,i
Local aItem,nerror,e
Local oSql
lReleaseBind := .t.
if valtype(aVarSust) == "A"
   for i:=len( aVarSust ) to 1 step -1
      cBind := ":"+alltrim(str( i))
      cPLSQL := strtran( cPLSQL,cBind,   sr_cdbvalue(aVarSust[i]) )
   next
endif
if len(hplVars ) >0
   oSql := sr_getconnection()
   oracleprePARE( osql:hdbc, cPLSQL )
   oraclebindalloc( oSql:hdbc, len(hplVars ))
   for i:= 1 to len( hplVars )
      aItem := hplVars[ i ]
      if aItem[2] == "VARCHAR" .or. aItem[2] == "CHAR" .or. aItem[2] == "VARCHAR2"
         OracleinBindParam( oSql:hdbc, i, -1, aItem[3], , aItem[5])
      elseif aItem[2] == "NUMBER"
         if aItem[4] >0
            OracleinBindParam( oSql:hdbc, i, 4, 12, , aItem[5] )
         else
            OracleinBindParam( oSql:hdbc, i, 2, 12, , aItem[5])
         endif
      elseif aItem[2] == "DATE"  .or. aItem[2] == "DATA"
        OracleinBindParam( oSql:hdbc, i, 8, 12, , aItem[5])
      endif
   next
   TRY
      nError := OracleExecDir( osql:hDbc )
      aOraClipCursors[nCursor]["error"]  := nError
      aOraClipCursors[nCursor]["lastsql"] := cPlSql

nlasterror  :=0
   CATCH e
      nerror := - 1
      aOraClipCursors[nCursor]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[nCursor]["lastsql"] := ''

nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
   End
   If nerror >= 0
      for i:= 1 to len( hplVars )
         hplVars[ i ,6] := ORACLEGETBINDDATA( osql:hdbc, i )
     next
     Endif
   ORACLEFREEBIND( osql:hdbc )
endif
return nError

function csgetvar(c)
return oragetvar( c )
function oragetvar( c )
Local nPos
nPos := ascan(hplVars,{|x| alltrim(upper(x[1])) == Alltrim(upper(c ))})
if nPos >0
   return hplVars[ nPos ,6]
endif
   lReleaseBind := .T.
return ""

function oraclose()
return nil

function OraResetBind()
hplVars := {}
return nil
function OCTBROWSEDB(a,s,d,f)
return tbrowsedb(a,s,d,f)

function csxerror()
return sr_Getconnection():nretcode
function csxerrmsg()
return SQLO_GETERRORCODE( sr_getconnection():hDBC )

function CSXCLEARFILTER()
(alias())->(sr_setfilter())
return nil

function CSXsetFILTER(a)
Local cFilter
if len(a) == 1
(alias())->(sr_setfilter(a[1]))
elseif len(a) ==2
cFilter := strtran(a[1],':1',sr_cdbvalue(a[2]))
(alias())->(sr_setfilter(a[1]))
endif
return nil


function orarowid()
return (alias())->(recno())
function csxrowid()
return (alias())->(recno())

function csx
return nil

function CSOPENTMP(n,ncursor)
return OraOpentmp(n,@ncursor)

function OraOpentmp(n,ncursor)
nCursor := nIdCursor
aOraclipHash[n] := hash()
aOraclipHash[n][nIdCursor] := hash()
aOraClipCursors[nIdCursor] := hash()
aOraClipCursors[nIdCursor]["cursoropen"] := .f.
++nIdCursor
return 0

function CSCLOSETMP(n)
return oraclosetmp(n)
function oraclosetmp(n)
   closecursor(n)
   hdel(aOraClipCursors,n)
   --nIdCursor
   return nil

// culik 8/12/2012 adicionado funcoes nao existente

function OraMax( nCursor, cTable, cColumn , cWhere , aVarSust )

Local nlen := 0
Local aRet := {}
Local nErro
Local cSql ,i
Local cBind
if pcount() < 3
   return nil
endif
cSql := "select max( " +cColumn + " ) from " +cTable


if pCount() == 4
   cSql +=  " where " + cwhere
elseif pCount() == 5
   for i:=1 to len( aVarSust)
      cBind := ":"+alltrim(str( i))
      cwhere := strtran( cwhere ,cBind, sr_cdbvalue( aVarSust[i] ) )
   next
   cSql += " where " +  cwhere

endif
nErro := sr_Getconnection():exec(cSql,,.t.,@aret)
if nErro ==0
   aOraClipCursors[nCursor]["error"]  := 0
   aOraClipCursors[nCurSor]["lastsql"] := cSql
   aOraClipCursors[ncursor]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
   nlasterror :=0
   return aRet[1,1]
else
   aOraClipCursors[nCursor]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
   nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
   aOraClipCursors[nCursor]["lastsql"] := cSql
   aOraClipCursors[ncursor]["rowaffected"] := 0
endif
return nil

FUNCTION OraSelect(n,aret,csql,adata,nRows)

//Local oSql := sr_getconnection()
Local nError
Local i,nPos
Local cBind := ""
Local aTemp := {}
Local aDataRet := {}
Local cursor
Default nRows to -1
   closecursor(n)
aOraClipCursors[n]["cursoropen"] := .F.
if adata == nil
   //if nrows >F.
   //else
*    nError:= sr_getconnection():exec(csql,,.t.,@aret)
   //endif
   //comentada linha sandro, desnecessario neste ponto, existe abaixo
   //aOraClipCursors[n]["ret"] := aret
   // velho
   nError := ExecuteSql( csql, @cursor, n )//sr_getconnection():exec(csql,,.t.,@aret)
   if nError == 0
aOraClipCursors[n]["cursoropen"] := .T.
      aOraClipCursors[n]["cursor"] := cursor
      aOraClipCursors[n]["start"] := 1
      aOraClipCursors[n]["len"] := 0
      aOraClipCursors[n]["curpos"] := 1
      aOraClipCursors[n]["error"] := 0
      aOraClipCursors[n]["nrowread"] := -1
      aOraClipCursors[n]["lastsql"] := cSql
      aOraClipCursors[n]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
      aOraClipCursors[n]["aFields"] := sr_getconnection():IniFields(.f.) //aFields
      aOraClipCursors[n]["data"] := {}
      aOraClipCursors[n]["completed"] := .F.
      aOraClipCursors[n]["eof"] := .f.
      if nRows > -1
          for nPos :=1 to nRows
             OraFetchSelect(n)
             IF LEN( aOraClipCursors[n]["data"] ) > 0
                 aadd(aRet,aOraClipCursors[n]["data"])
            endif
         next
      else
          while OraFetchSelect(n) == 0
             aadd(aRet,aOraClipCursors[n]["data"])
          enddo

      endif



*    if nError == 0
*
*       aOraClipCursors[n]["start"] := 1
*       aOraClipCursors[n]["len"] := len(aret)
*       aOraClipCursors[n]["curpos"] := 0
*       aOraClipCursors[n]["error"] := 0
*       aOraClipCursors[n]["nrowread"] := nRows
*
*       aOraClipCursors[n]["ret"] := aret
*       aOraClipCursors[n]["lastsql"] := cSql
*       aOraClipCursors[n]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
*       aOraClipCursors[n]["aFields"] := sr_getconnection():aFields
*       nlasterror:=0
*       if nRows > -1
*          for each aTemp in aRet
*             aadd(aDataRet,aTemp)
*             if hb_enumindex() == nRows
*                exit
*             endif
*          next
*          aOraClipCursors[n]["curpos"] := len(aDataRet )+1
*          aRet := aDataRet
*       endif
   else
      aOraClipCursors[n]["error"]  = SQLO_GETERRORCODE( sr_getconnection():hDBC )
      nlasterror := SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[n]["lastsql"] := cSql
      aOraClipCursors[n]["rowaffected"] := 0
      aOraClipCursors[n]["aFields"] := {}
   endif
   return nError
endif

   for i:= len( aData ) TO 1 STEP -1
   cBind := ":"+alltrim(str( i))
   cSql := strtran( cSql,cBind,   sr_cdbvalue(adata[i]) )
next
   nError := ExecuteSql( csql, @cursor, n )//sr_getconnection():exec(csql,,.t.,@aret)
   if nError == 0
      aOraClipCursors[n]["cursoropen"] := .T.
      aOraClipCursors[n]["cursor"] := cursor
      aOraClipCursors[n]["start"] := 1
      aOraClipCursors[n]["len"] := 0
      aOraClipCursors[n]["curpos"] := 1
      aOraClipCursors[n]["error"] := 0
      aOraClipCursors[n]["nrowread"] := -1
      aOraClipCursors[n]["lastsql"] := cSql
      aOraClipCursors[n]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
      aOraClipCursors[n]["aFields"] := sr_getconnection():IniFields(.f.) //aFields
      aOraClipCursors[n]["data"] := {}
      aOraClipCursors[n]["completed"] := .F.
      aOraClipCursors[n]["eof"] := .f.
      if nRows > -1
          for nPos :=1 to nRows
             OraFetchSelect(n)
             IF LEN( aOraClipCursors[n]["data"] ) > 0
                aadd(aRet,aOraClipCursors[n]["data"])
            endif
         next
      else
          while OraFetchSelect(n) == 0
             aadd(aRet,aOraClipCursors[n]["data"])
          enddo
      endif


*    nError:= sr_getconnection():exec(csql,,.t.,@aret)
*    aOraClipCursors[n]["ret"] := aret
//velho
*    if nError == 0
*
*       aOraClipCursors[n]["start"] := 1
*       aOraClipCursors[n]["len"] := len(aret)
*       aOraClipCursors[n]["curpos"] := 0
*       aOraClipCursors[n]["error"] := 0
*       aOraClipCursors[n]["nrowread"] := nRows
*       aOraClipCursors[n]["lastsql"] := cSql
*       aOraClipCursors[n]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
*       aOraClipCursors[n]["aFields"] := sr_getconnection():aFields
*       nlasterror :=0
*       if nRows > -1
*          for each aTemp in aRet
*             aadd(aDataRet,aTemp)
*             if hb_enumindex() == nRows
*                exit
*             endif
*          next
*          aRet := aDataRet
*          aOraClipCursors[n]["curpos"] := len(aDataRet )+1
*       endif
*
   else
      aOraClipCursors[n]["error"]  = SQLO_GETERRORCODE( sr_getconnection():hDBC )
      nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[n]["lastsql"] := ''
      aOraClipCursors[n]["rowaffected"] := 0
      aOraClipCursors[n]["aFields"] := {}
   endif

return nError

function OraUpdIns( nCursor, cTable, aCols, aDataUpd, aDataIns , cWhere , aVarSust )
Local nRet

   if OraCount(nCursor,cTable,cWhere , aVarSust ) > 0
      OraUpdate( nCursor,cTable,aCols,aDataUpd,cWhere,aVarSust )
   else
      return OraInsert(nCursor, cTable, aCols, aDataIns)
   endif
return nRet

/*
function OraUpdIns( nCursor, cTable, aCols, aDataUpd, aDataIns , cWhere , aVarSust )
Local nRet
Local nPos

   nRet := OraUpdate( nCursor,cTable,aCols,aDataUpd,cWhere,aVarSust )
   if nRet == -1
      return OraInsert(nCursor, cTable, aCols, aDataIns)
   endif
return nRet
*/

function Orasum( nCursor, cTable, cColumn , cWhere , aVarSust )

Local nlen := 0
Local aRet := {}
Local nErro
Local cSql
Local cBind,i
if pcount() < 3
   return nil
endif
cSql := "select sum( " +cColumn + " ) from " +cTable


if pCount() == 4
   cSql +=  " where " + cwhere
elseif pCount() == 5
   for i:=1 to len( aVarSust)
      cBind := ":"+alltrim(str( i))
      cwhere := strtran( cwhere ,cBind, sr_cdbvalue( aVarSust[i] ) )
   next
   cSql += " where " +  cwhere

endif
nErro := sr_Getconnection():exec(cSql,,.t.,@aret)
if nErro ==0
   aOraClipCursors[nCursor]["error"]  := 0
   aOraClipCursors[nCursor]["lastsql"] := cSql
   aOraClipCursors[ncursor]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
   nlasterror :=0
   return aRet[1,1]
else
   aOraClipCursors[nCursor]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
   aOraClipCursors[nCursor]["lastsql"] := cSql
   aOraClipCursors[ncursor]["rowaffected"] := 0
   nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
endif
return nil


function csxBegin()
Local oCnn

SR_BeginTransaction()
oCnn := sr_getconnection()
return if(oCnn:nTransacCount>0,0,-1)

function CSXCOMMIT()
sr_committransaction()
return 0
function CSXROLLBACK()
sr_rollbacktransaction()
return 0

function CSINTRANS()
return SR_TransactionCount() > 0

function  OraSelNext( nCursor, aTableData , nRows )
Local e
Local nRet :=0,aDataRet:={}
Local nPos
default nRows to -1
try

    if aOraClipCursors[nCursor]["completed"]
       aOraClipCursors[nCursor]["data"]:={}
       aTableData := {}
      else
      aOraClipCursors[nCursor]["data"]:={}
      if nRows > -1
          for nPos :=1 to nRows
             OraFetchSelect(nCursor)
             IF LEN( aOraClipCursors[nCursor]["data"] ) > 0
                 aadd(aDataRet,aOraClipCursors[nCursor]["data"])
            endif
         next
      else
          while OraFetchSelect(nCursor) == 0
             IF LEN( aOraClipCursors[nCursor]["data"] ) > 0
                aadd(aDataRet,aOraClipCursors[nCursor]["data"])
             ENDIF
          enddo
      endif
         aTableData := aDataRet
      endif


*
*    hData := aOraClipCursors[nCursor]
*    if len( hData["ret"]) > 0
*       aRet := hData["ret"]
*       nStart := hData["curpos"]
*
*       if nRows == -1
*          while nStart <= len(aRet)
*             aadd(aDataRet,aRet[nStart])
*             nStart ++
*          enddo
*          aTableData := aDataRet
*          aOraClipCursors[nCursor]["curpos"] := len(aRet) +1
*
*       else
*
*          nEnd   := hData["curpos"] + nRows
*          ii := 0
*          for nPos := nStart  to nEnd
*             aadd(aDataRet,aRet[nStart])
*             ii++
*             if ii == nRows
*                exit
*             endif
*          next
*          aTableData := aDataRet
*          aOraClipCursors[nCursor]["curpos"] := nEnd +1
*       endif
*
*    endif
catch e
   nRet := -1
end
return nRet

function csExecSQL(nCursor, cSQL, aVarSust)
LOCAL nRet
Local hData,oSql
Local cBind,i, e


Try
hData := aOraClipCursors[nCursor]
osql:=sr_getconnection()
if upper(substr(cSql,6)) =="BEGIN "
   if pCount() == 3

      if valtype(aVarSust) == "A"
         for i := len( aVarSust )  TO 1 STEP -1
            cBind := ":"+alltrim(str( i) )
            cSQL := strtran( cSQL,cBind,   sr_cdbvalue(aVarSust[i]) )
         next
      endif

   endif
   oracleprePARE( osql:hdbc, cSQL )
   nRet := OracleExecDir( osql:hDbc )
   nlasterror := 0
   aOraClipCursors[nCursor]["errormsg"]  := ""
   if nRet == 0
      aOraClipCursors[nCursor]["error"]  := 0
      aOraClipCursors[nCursor]["lastsql"] := cSql
      aOraClipCursors[ncursor]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
   else
      aOraClipCursors[nCursor]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
      nlasterror := SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[nCursor]["errormsg"]  := SQLO_GETERRORDESCR( osql:hDBC )
      aOraClipCursors[nCursor]["lastsql"] := ''
      aOraClipCursors[ncursor]["rowaffected"] := 0
   endif

else
   nRet := osql:exec(cSql,,.f.)
   nlasterror := 0
   aOraClipCursors[nCursor]["errormsg"]  := ""
   if nRet == 0
      aOraClipCursors[nCursor]["error"]  := 0
      aOraClipCursors[nCursor]["lastsql"] := cSql
      aOraClipCursors[ncursor]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
   else
      aOraClipCursors[nCursor]["error"]  := osql:lasterror()
      aOraClipCursors[nCursor]["errormsg"]  := SQLO_GETERRORDESCR( osql:hDBC )
      nlasterror := SQLO_GETERRORCODE( sr_getconnection():hDBC )
      aOraClipCursors[nCursor]["lastsql"] := cSql
      aOraClipCursors[ncursor]["rowaffected"] := 0
   endif
endif
catch e
nret := -1
end
return nRet

function csErrMsg(nCursor)

Local hData,oSql,e
Local cRet := ""
Try
   hData := aOraClipCursors[nCursor]
   osql:=sr_getconnection()
   cRet :=  hData["errormsg"]
catch e
end
return cRet


function CSVALIDCNX( nCursor )
LOCAL lRet := .F.
Local hData,oSql,e
Local cRet := ""
Try
   hData := aOraClipCursors[nCursor]
   osql:=sr_getconnection()
   lRet := hData["nRet"] >0
catch e
lRet := .F.
end
return lRet


function CSUSER( nCursor )

Local hData,oSql,e
Local cRet := ""
Try
   hData := aOraClipCursors[nCursor]
   osql:=sr_getconnection()
   cRet := hData["user"]
catch e
   cRet := ""
end
return cRet


function CSVALIDCURSOR( nCursor)
local lRet
local hDAta
try
hData := aOraClipCursors[nCursor]
lRet :=.t.
catch
lRet:= .F.
end
return lRet

function CSXSETRDD
return nil



function OraExists( nCursor, cTable,  cWhere , aVarSust )
Local nlen := 0
Local aRet := {}
Local nErro
Local cSql ,i
Local cBind
(nCursor)
if pcount() < 3
   return nil
endif
cSql := "select * from " +cTable


if pCount() == 3
   cSql +=  " where " + cwhere
elseif pCount() == 4
   for i:=1 to len( aVarSust)
      cBind := ":"+alltrim(str( i))
      cwhere := strtran( cwhere ,cBind, sr_cdbvalue( aVarSust[i] ) )
   next
   cSql += " where " +  cwhere

endif
nErro := sr_Getconnection():exec(cSql,,.t.,@aret)
if nErro ==0
   return len(aRet) > 0
endif
return .f.


function OraMin( nCursor, cTable, cColumn , cWhere , aVarSust )

Local nlen := 0
Local aRet := {}
Local nErro
Local cSql
Local cBind,i
if pcount() < 3
   return nil
endif
cSql := "select mim( " +cColumn + " ) from " +cTable


if pCount() == 4
   cSql +=  " where " + cwhere
elseif pCount() == 5
   for i:=len( aVarSust) TO 1 STEP -1
      cBind := ":"+alltrim(str( i))
      cwhere := strtran( cwhere ,cBind, sr_cdbvalue( aVarSust[i] ) )
   next
   cSql += " where " +  cwhere

endif
nErro := sr_Getconnection():exec(cSql,,.t.,@aret)
if nErro ==0
   aOraClipCursors[nCursor]["error"]  := 0
   aOraClipCursors[nCursor]["lastsql"] := cSql
   aOraClipCursors[ncursor]["rowaffected"] := GETAFFECTROWS( sr_getconnection():hdbc )
   nlasterror :=0
   return aRet[1,1]
else
   aOraClipCursors[nCursor]["error"]  := SQLO_GETERRORCODE( sr_getconnection():hDBC )
   nlasterror :=SQLO_GETERRORCODE( sr_getconnection():hDBC )
   aOraClipCursors[nCursor]["lastsql"] := cSql
   aOraClipCursors[ncursor]["rowaffected"] := 0
endif
return nil


#define _ORATBROWSE_EXIT             1
#define _ORATBROWSE_REFRESHALL       2
#define _ORATBROWSE_REFRESHCURRENT   3
#define _ORATBROWSE_METHOD           4
#define _ORATBROWSE_SEARCH           5
#define _ORATBROWSE_SEARCH_NEXT      6
#define _ORATBROWSE_FILTER           7
#define _ORATBROWSE_FILTER_BACK      8
#define _ORATBROWSE_FILTER_RESET     9
#define _ORATBROWSE_ORDER_BY        10
#define _ORATBROWSE_ORDER_RESET     11


function  OraTBrNew( nTop,nLeft,nBottom,nRight )
return tbrowsedb( nTop,nLeft,nBottom,nRight )

#define COMPILE(cExp) &("{||"+cExp+"}")
function OraColumnNew( cHeading, bBlock )
return TBColumnNew( cHeading, COMPILE(bBlock ))

function  OraTBrowse( nCursor1,cSql,c,oBrowse,bBLock)
Local cTempFile
Local aReg :={}
Local i
Local aRet := {}
Local lRet := .T.
Local nKey
lOCAL oSql:= sr_getconnection()
Local oCol
(nCursor1,c)
fclose( HB_FTEMPCREATEEX(@cTempFile,,"tmp",".dbf") )

osql:exec(cSql,,.t.,,cTempFile)

while lRet
   obrowse:forcestable()
   aReg := {}
   for i := 1 to fcount()
*       aadd(aReg, eVal(obrowse:GetColumn(i):Block))
      oCol := oBrowse:getcolumn(i)
      aadd(aReg, fieldget(i))
   next
   nKey := inkey( 0 )
   aRet := eval(bBLock,nkey,obrowse,aReg)
   IF Aret == nil
      loop
   endif
   switch aRet[ 1 ]
   case _ORATBROWSE_METHOD
     oBrowse:applykey( nKey )
     exit
  case _ORATBROWSE_REFRESHALL
     zap
     osql:exec(cSql,,.t.,,cTempFile)
     go top
     exit

  case  _ORATBROWSE_EXIT
     lRet := .F.
     exit

   end switch

enddo

return 0



function ORALASTSQL()
RETURN NIL

INIT FUNCTION OVERRIDESIZE
   OVERRIDE METHOD SQLLen IN CLASS SR_CONNECTION WITH MySQLLen
RETURN NIL

static function mySQLLen( nType, nLen, nDec )

   local cType := "U"
   Local Self:=Qself()

   DEFAULT nDec to -1

   do case
   case (nType == SQL_CHAR .or. nType == SQL_VARCHAR .or. nType == SQL_NVARCHAR) .and. If(SR_SetNwgCompat(), nLen != 4000 .and. nLen != 2000, .T. )

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

     If nDec >3 .and. !( nLen = 38 .and. nDec = 0 )
        nLen :=14
      endif
      If !( nLen = 38 .and. nDec = 0 )
         nLen := min( nLen, 20 )
         nLen := max( nLen, 1 )
      EndIf

   case nType == SQL_DATE .or. nType == SQL_TIMESTAMP .or. nType == SQL_TYPE_TIMESTAMP .or. nType == SQL_TYPE_DATE .or. ntype == SQL_DATETIME
     nLen := 8

   case nType == SQL_TIME
     nLen := 8

   case nType == SQL_LONGVARCHAR .or. nType == SQL_LONGVARBINARY .or. nType == SQL_FAKE_LOB
     nLen := 10

   Case nType == SQL_GUID
      nLen := 36

   endcase

return nLen


function orarownum( nCursor )
return aOraClipCursors[nCursor]["curpos"]

function oraGoto( n, aDados, nRow )
Local nRet := 0
Local e
default aDados to {}
   aOraClipCursors[n]["curpos"] := nRow
   try
      aOraClipCursors[n]["curpos"] := nRow
      aDados := aOraClipCursors[n]["ret"][aOraClipCursors[n]["curpos"]]
   catch e
      nRet := -1
   end
return nRet




function ORAROWCOUNT( nCursor )
return aOraClipCursors[nCursor]["rowaffected"]







function OraFName( n, nPos )
Local aTmp
if len( aOraClipCursors[n]["aFields"] ) == 0 .or. nPos <=0
   aOraClipCursors[ n]["errormsg"]  := "vetor vazio ou Posicao <= 0"
   return nil
endif
if len( aOraClipCursors[n]["aFields"] ) >1 .and. nPos <= len( aOraClipCursors[n]["aFields"] )
   aTmp := aOraClipCursors[n]["aFields"]
   return Alltrim( aTmp[nPos,1])
endif

   aOraClipCursors[ n]["errormsg"]  := "Indice do campo invalido"
return nil

Function SR_AdjustNum(a)

   local b := aClone(a)
   local i
   local lNwgOldCompat :=.f.

   For i = 1 to len(b)

      //If lNwgOldCompat
         If b[i,2] = "N"
            b[i,3] ++
         EndIf
      //EndIf

      If b[i,2] = "N" .and. b[i,3] > 18
         b[i,3] := 19
      EndIf

      If lNwgOldCompat
         If b[i,2] = "N" .and. b[i,4] >= (b[i,3] - 1)
            b[i,4] := abs(b[i,3] - 2)
         EndIf
      EndIf
      if b[i,2] == "N" .and. b[i,4]>=5
         b[i,3] +=4
      endif

      If b[i,2] = "M"
         b[i,3] := 10
      EndIf

   Next

Return b


STATIC Function ExecuteSql( csql, cursor )
LOCAL nError
* Try
*   nError := OraclePrepare( SR_GetConnection():hdbc, cSql, .t. )
* Catch e
*   nError := -1
* End
* If nError > 0

    nError := Sqlo_Execute( SR_GetConnection():hdbc, cSql )
*   nError :=sr_Getconnection():executeraw(cSql)

   cursor := GETORAHANDLE( SR_GetConnection():hdbc )


* EndIf

Return nError

STATIC Function OraFetch( n )
Local oSql := sr_getconnection()
Local hDBC := oSql:hdbc, nError
Local hDBCHandle := aOraClipCursors[n]["cursor"]
Local i
Local aArray

Local cAlias := aOraClipCursors[n]["aliastmp"]
Local aDb

SETORAHANDLE(hDBC,hDBCHandle)



If aOraClipCursors[n]["curpos"] <= ( cAlias )->( RecCount() ) .AND.;
   aOraClipCursors[n]["curpos"] <> 0
   ( cAlias )->( dBGoto( aOraClipCursors[n]["curpos"] ) )
Elseif !aOraClipCursors[n]["completed"]

  nError := oSql:Fetch(,aOraClipCursors[n]["aFields"] )

  aOraClipCursors[n]["eof"] := nError <> 0
  aOraClipCursors[n]["data"] := {}
  If nError == 0

     aArray := Array( Len( aOraClipCursors[n]["aFields"] ) )

     ( cAlias )->( dBAppend() )
     For i:= 1 to Len( aOraClipCursors[n]["aFields"] )

         ( cAlias )->( FieldPut( i, oSql:FieldGet( i, aOraClipCursors[n]["aFields"] ) ) )
     Next
     ( cAlias )->( dBUnlock() )
  Else
*       for i:=1 to len(  aOraClipCursors[n]["data"] )
       aDb := aOraClipCursors[n]["aFields"]
       for i:=1 to len( aDb)
         if adb[i,2]=="C"
            aadd(aOraClipCursors[n]["data"],"")
         elseif adb[i,2]=="N"
            aadd(aOraClipCursors[n]["data"],0)
         elseif adb[i,2]=="D"
            aadd(aOraClipCursors[n]["data"],ctod(''))
         elseif adb[i,2]=="L"
            aadd(aOraClipCursors[n]["data"],.f.)
         endif
       next
         aOraClipCursors[n]["completed"] := .T.
         aOraClipCursors[n]["eof"] := .t.

*          oSql:FreeStatement()
      aOraClipCursors[n]["cursoropen"] := .f.
      SQLO_CLOSESTMT( hDBC )

      if select(aOraClipCursors[n]["aliastmp"]) >0
         (aOraClipCursors[n]["aliastmp"])->(dbclosearea())
         ferase(aOraClipCursors[n]["tmpfile"])
      endif

  EndIf

Else
   ( cAlias )->( dBGoto( aOraClipCursors[n]["curpos"] ) )
EndIf
if select(aOraClipCursors[n]["aliastmp"])>0
   For i:= 1 to Len( aOraClipCursors[n]["aFields"] )
      AADD( aOraClipCursors[n]["data"], ( cAlias )->( FieldGet(i) ) )
   Next
endif

Return nError


static Function OraFetchSelect( n  )
Local oSql := sr_getconnection()
Local hDBC := oSql:hdbc, nError
Local hDBCHandle := aOraClipCursors[n]["cursor"]
Local i
Local aArray

local aret := {}

SETORAHANDLE(hDBC,hDBCHandle)

  nError := oSql:Fetch()

  aOraClipCursors[n]["eof"] := nError <> 0

  If nError == 0

      aOraClipCursors[n]["data"] := {}

      aArray := Array( Len( aOraClipCursors[n]["aFields"] ) )


     For i:= 1 to Len( aOraClipCursors[n]["aFields"] )
         aadd(aOraClipCursors[n]["data"],oSql:FieldGet( i, aOraClipCursors[n]["aFields"] ) )
     Next

  Else

      aOraClipCursors[n]["completed"] := .T.
      aOraClipCursors[n]["cursoropen"] := .f.
*       oSql:FreeStatement()
      SQLO_CLOSESTMT( hDBC )

  EndIf


Return nError


static Function GerGhost( uDat )
if valtype( uDat ) == "C"
   Return ""
elseif valtype( uDat ) == "N"
   return 0
elseif valtype( uDat ) == "L"
   return .f.
elseif valtype( uDat ) == "D"
   return ctod("")
endif
return ""

function getOraclipCursor(ncursor)
return aOraClipCursors[ncursor]

static function closecursor(n)
Local osql := sr_getconnection()
Local e, hDBCHandle, hdbc:=osql:hdbc


   try
      hDBCHandle := aOraClipCursors[n]["cursor"]
      if aOraClipCursors[n]["cursoropen"]
         SETORAHANDLE(hDBC,hDBCHandle)
*       oSql:FreeStatement()      // fecha  o cursor antes da tabela
         SQLO_CLOSESTMT( hDBC )
         aOraClipCursors[n]["cursoropen"] := .f.
      endif
      if select(aOraClipCursors[n]["aliastmp"]) >0

         (aOraClipCursors[n]["aliastmp"])->(dbclosearea())
         ferase(aOraClipCursors[n]["tmpfile"])
      endif
   catch e
   end
return nil

#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapiitm.h"
#if defined(HB_OS_WIN)
#include <windows.h>
#endif
typedef  int sqlo_stmt_handle_t;
typedef struct _ORA_BIND_COLS
{
   char * col_name;
   short sVal;
   double  dValue;
   int iType;
   ULONG  ulValue;
   char sDate[ 7 ];
   int iValue;
   char sValue[31];
//    OCIRowId * RowId;
} ORA_BIND_COLS ;


typedef struct _OCI_SESSION
{
   int dbh;                      // Connection handler
   int stmt;                     // Current statement handler
   int status;                   // Execution return value
   int numcols;                  // Result set columns
   char server_version[128];
   //bellow for bind vars
   sqlo_stmt_handle_t stmtParam;
   ORA_BIND_COLS *  pLink;
   unsigned int   ubBindNum;
   sqlo_stmt_handle_t stmtParamRes;
   unsigned int uRows;
} OCI_SESSION;

typedef OCI_SESSION * POCI_SESSION;
HB_FUNC( GETORAHANDLE)
{
   OCI_SESSION* p  = ( OCI_SESSION* ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   if ( p )
      hb_retni(p->stmt);
}

HB_FUNC( SETORAHANDLE)
{
   OCI_SESSION* p  = ( OCI_SESSION* ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if ( p )
   {
      p->stmt = hb_parni(2);
   }
}
#pragma ENDDUMP

