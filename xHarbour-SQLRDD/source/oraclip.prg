#include "sqlrdd.ch"
#include "common.ch"

static aOraclipHash := hash()
Static aOraClipCursors := hash()
static nIdCursor := 1
static hplVars := {}
static nlasterror := 0
static lReleaseBind := .f.
function OraExecSql( n,c)
return sr_getconnection():exec(c,,.f.)


FUNCTION OraSel1(n,aret,csql,adata)

Local oSql := sr_getconnection()
Local nError
Local i,e
Local cBind := ""
Local aTemp := {}
Local aDataRet

if adata == nil
   nError:= sr_getconnection():exec(csql,,.t.,@aret)
   aOraClipCursors[n]["ret"] := aret   
   if nError == 0 
      
      aOraClipCursors[n]["start"] := 1
      aOraClipCursors[n]["len"] := len(aret)
      aOraClipCursors[n]["curpos"] := 0
      aOraClipCursors[n]["error"] := 0   
nlasterror:=0
      if len(aRet) == 1
         aDataRet := aRet[1]
         aret := aclone(aDataRet)
      endif   
   else 
      aOraClipCursors[n]["error"]  = sr_Getconnection():lasterror()
nlasterror := sr_Getconnection():lasterror()
   endif
   return nError
endif

for i:=1 to len( aData ) 
      cBind := ":"+alltrim(str( i))
      cSql := strtran( cSql,cBind,   sr_cdbvalue(adata[i]) )
next
   nError:= sr_getconnection():exec(csql,,.t.,@aret)
   aOraClipCursors[n]["ret"] := aclone(aret)
   if nError == 0 

      aOraClipCursors[n]["start"] := 1
      aOraClipCursors[n]["len"] := len(aret)
      aOraClipCursors[n]["curpos"] := 0
      aOraClipCursors[n]["error"] := 0   
nlasterror :=0
      if len(aRet) >= 1
         aDataRet := aRet[1]
         aret := aclone(aDataRet)

      endif   
   else 
      aOraClipCursors[n]["error"]  = sr_Getconnection():lasterror()
nlasterror :=sr_Getconnection():lasterror()      
   endif
  
return nError
function orafound(n)
return len(aOraClipCursors[n]["ret"]) >0


function OraDelete( nCursor2,cTabOrgMat,cwhere ,adata ) 
Local csql := "delete from " +  cTabOrgMat 
Local i
Local oSql := sr_getconnection()
Local nError,e
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
      nlasterror :=0
   else 
      aOraClipCursors[nCursor2]["error"]  = sr_Getconnection():lasterror()
      nlasterror :=sr_Getconnection():lasterror()      
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
return sr_getconnection():lasterror()




function OraUpdate( nCursor,cTabAutos,aCols,aDadosAlt,cWhere,aChave ) 
Local csql := "update " +  cTabAutos + " set "

Local n,i,e
Local oSql := sr_getconnection()
Local nError,cbind

for n := 1  to len( aDadosAlt ) 
   cSql += acols[n ] + "=" + sr_cdbvalue( aDadosAlt[n] ) + ","
next
cSql := substr( csql,1,len(csql)-1)

if pcount() == 5
    csql +=  " where " +  cwhere 
elseif pcount() == 6


   for i:=1 to len( aChave) 

      cBind := ":"+alltrim(str( i))
      cwhere := strtran( cwhere ,cBind, sr_cdbvalue( aChave[i] ) )
   next
   cSql += " where " +  cwhere 
tracelog(csql)
endif   
   try
      nError:= sr_getconnection():exec(csql,,.f.)
      aOraClipCursors[nCursor]["error"]  := 0
nlasterror :=0
   CATCH e
      nerror := - 1
      aOraClipCursors[nCursor]["error"]  := sr_Getconnection():lasterror()
nlasterror :=sr_Getconnection():lasterror()      
   End
return nError

function OraOpen(n,ncursor)
nCursor := nIdCursor
aOraclipHash[n] := hash()
aOraclipHash[n][nIdCursor] := hash()
aOraClipCursors[nIdCursor] := hash() 

++nIdCursor
return 0

function  OraLogon(cCnxName, cUser, cPwd, cAlias, nCnxType) 
Local cString
Local nRet
cString := "UID="+cUSer+";PWD="+cPwd
if !empty(cAlias ) 
cString += ";TNS="+cAlias
endif
nRet := sr_addconnection(CONNECT_ORACLE_QUERY_ONLY,cstring)

if nRet >0
   aOraclipHash[cCnxName] := hash()
   aOraclipHash[cCnxName]["nRet"] := nRet
   aOraclipHash[cCnxName]["alias"] := cAlias
   aOraclipHash[cCnxName]["time"] := ttoc(datetime())
   aOraclipHash[cCnxName]["user"] := cUSer
   aOraclipHash[cCnxName]["pwd"] :=  cPwd
endif
return nRet

function  OraLogoff(cCnxName) 
Local e
try
sr_endconnection(aOraclipHash[cCnxName]["nRet"])
aOraclipHash[cCnxName]["nRet"] := nil
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


function oraskip(n,aData,nPos)

default nPos to 1
if nPos>0
    if aOraClipCursors[n]["curpos"] == 0
       aOraClipCursors[n]["curpos"] := 1
    endif
   if aOraClipCursors[n]["curpos"]+1 <= aOraClipCursors[n]["len"] 
      aOraClipCursors[n]["curpos"]++
      aData := aOraClipCursors[n]["ret"][aOraClipCursors[n]["curpos"]]
   endif
else
   if aOraClipCursors[n]["curpos"]-1 >= aOraClipCursors[n]["start"] 
      aOraClipCursors[n]["curpos"]--
      aData := aOraClipCursors[n]["ret"][aOraClipCursors[n]["curpos"]]
   endif
endif   
      
return nil

function oraeof(n)
return aOraClipCursors[n]["curpos"] == aOraClipCursors[n]["len"]
function orabof(n)
return aOraClipCursors[n]["curpos"] == aOraClipCursors[n]["start"]

function orazap(n)
return nil
function orastruct(n,ctable)
Local aStru
Local csql := "select * from " + ctable + " where 1 == 1"
use (csql) new via "SQLRDD" alias "ZZZZZZZZZZ"
aStru := zzzzzzzzzz->(dbstruct())
zzzzzzzzzz->(dbclosearea())
return astru








function  OraSetPwd(nCursor, cUser, cPassword)
Local cSql := 'alter user '+ cUser + ' identified by ' + cPassword 
Local nRet,e
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
nlasterror :=0
      if len(aret) == 1 
         if len( aRet[1]) == 1
            return aret[1,1]
         else
            return aret[1]   
         endif   
      endif

   else
      aOraClipCursors[n]["error"]  := sr_Getconnection():lasterror()   
nlasterror :=sr_Getconnection():lasterror()      
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
nlasterror :=0
   else
      aOraClipCursors[n]["error"]  := sr_Getconnection():lasterror()   
nlasterror :=sr_Getconnection():lasterror()      
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
nlasterror :=0
   CATCH e
      nerror := - 1
      aOraClipCursors[nCursor]["error"]  := sr_Getconnection():lasterror()   
nlasterror :=sr_Getconnection():lasterror()      
   End
   
   
return nError


function      OraCount(nCursor, cTabela, cWhere, aVarSust)
Local nlen := 0
Local aRet := {}
Local nErro
Local cSql 
if pcount() < 2 
   return nil
endif
if pcount() == 2
cSql := "select count(*) from " +cTabela
elseif pCount() == 3
cSql := "select count(*) from " +cTabela + " where " + cwhere
elseif pCount() == 4
endif
nErro := sr_Getconnection():exec(cSql,,.t.,@aret)
if nErro ==0 
   aOraClipCursors[nCursor]["error"]  := 0
nlasterror :=0
   return aRet[1,1]
else
   aOraClipCursors[nCursor]["error"]  := sr_Getconnection():lasterror()      
nlasterror :=sr_Getconnection():lasterror()      
endif
return 0   
function oraset()
return nil


function oraerror(n)
if pcount()==0
return nlasterror
endif
return aOraClipCursors[n]["error"]

function   OraSetVar(cVarName, cOraType, nLen, nDec, xInitValue) 
default nlen to 1
default ndec to 0
if lReleaseBind
lReleaseBind := .F.
hplvars:={}
endif
aadd(hplVars,  {UPPER(cVarName), UPPER(cOraType), nLen+5, nDec, xInitValue,})
return nil

function      OraPLSQL(nCursor, cPLSQL, aVarSust)
Local cBind,i
Local aItem,nerror
lReleaseBind := .t.
if valtype(aVarSust) == "A"
   for i:=1 to len( aVarSust ) 
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
         OracleinBindParam( oSql:hdbc, i, -1, aItem[3], , )
      elseif aItem[2] == "NUMBER"
         if aItem[4] >0
            OracleinBindParam( oSql:hdbc, i, 4, 12, , )
         else
            OracleinBindParam( oSql:hdbc, i, 2, 12, , )
         endif
      elseif aItem[2] == "DATE"  .or. aItem[2] == "DATA" 
        OracleinBindParam( oSql:hdbc, i, 8, 12, , )
      endif
   next  
   TRY
      nError := OracleExecDir( osql:hDbc )
      aOraClipCursors[nCursor]["error"]  := nError
nlasterror  :=0
   CATCH e
      nerror := - 1
      aOraClipCursors[nCursor]["error"]  := sr_Getconnection():lasterror()      
nlasterror :=sr_Getconnection():lasterror()      
   End
   If nerror >= 0
      for i:= 1 to len( hplVars ) 
         hplVars[ i ,6] := ORACLEGETBINDDATA( osql:hdbc, i )   
     next 
     Endif
   ORACLEFREEBIND( osql:hdbc )   
endif   
return nError

function oragetvar( c )
Local nPos
nPos := ascan(hplVars,{|x| alltrim(upper(x[1])) == Alltrim(upper(c ))})
if nPos >0
return hplVars[ nPos ,6]
endif
lReleaseBind := .T.
return ""
      
   function oraclose(n)
   return nil
   
function OraResetBind()
hplVars := {}
return nil   
function OCTBROWSEDB(a,s,d,f)
return tbrowsedb(a,s,d,f)

function csxerror()
return sr_Getconnection():nretcode
function csxerrmsg()
return sr_Getconnection():lasterror()

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


function csxrowid()
return (alias())->(recno())
function csx
return nil

function OraOpentmp(n,ncursor)
nCursor := nIdCursor
aOraclipHash[n] := hash()
aOraclipHash[n][nIdCursor] := hash()
aOraClipCursors[nIdCursor] := hash() 

++nIdCursor
return 0

   function oraclosetmp(n)
   hdel(aOraClipCursors,n)
   --nIdCursor
   return nil
