#include "dbinfo.ch"
REQUEST DBFCDX
function main(bld,rdd)
FIELD F1
local nSec, cBlock, cRegex, cPattern
rddsetdefault(iif(empty(rdd),"DBFCDX",rdd))
? rddsetdefault()
set delete on
set exclusive off
if empty(bld)
  aeval(directory("_tst.*"),{|x|ferase(x[1])})
  dbCreate("_tst", {{"F1", "C", 20, 0}})
  USE _tst
  while lastrec()<100000
    dbappend()
    F1 := strzero(recno(),10)+chr(recno()%26+asc("A"))
  enddo
  INDEX ON F1 TAG T1 TO _tst
  dbcommit()
else
  USE _tst
endif
ordsetfocus(1)
? indexkey(), ordsetfocus()
cBLock:={|key,rec|rec%501==0 .and. "Z"$key .and. !deleted()}

nSec:=secondscpu()
dbgotop()
while !eof()
   if eval( cBlock, ordkeyval(), recno() )
      qout( ordkeyval(), recno() )
   endif
   dbskip()
enddo
nSec:=secondscpu()-nSec
? "SKIP:", nSec, "sec."

nSec:=secondscpu()
dbgotop()
while !eof()
   if eval( cBlock, ordkeyval(), recno() )
      qout( ordkeyval(), recno() )
   endif
   dborderinfo(DBOI_SKIPEVAL,,,cBLock)
enddo
nSec:=secondscpu()-nSec
? "SKIPEVAL:", nSec, "sec."

cBLock:={|key,rec|!deleted() .and. rec%501==0 .and. "Z"$key}
nSec:=secondscpu()
dbgotop()
while !eof()
   if eval( cBlock, ordkeyval(), recno() )
      qout( ordkeyval(), recno() )
   endif
   dborderinfo(DBOI_SKIPEVAL,,,cBLock)
enddo
nSec:=secondscpu()-nSec
? "SKIPEVAL:", nSec, "sec."

cRegex:="\b.*1001[D-O].*\b"
cRegex:=HB_REGEXCOMP(cRegex)
nSec:=secondscpu()
dbgotop()
while !eof()
   if ordkeyval() HAS cRegEx
      qout( ordkeyval(), recno() )
   endif
   dborderinfo(DBOI_SKIPREGEX,,,cRegex)
enddo
nSec:=secondscpu()-nSec
? "SKIPREGEX:", nSec, "sec."

cPattern:="*101*A"
nSec:=secondscpu()
dbgotop()
if !eof() .and. ! WildMatch(cPattern, ordkeyval())
   dborderinfo(DBOI_SKIPWILD,,,cPattern)
endif
while !eof()
   if WildMatch(cPattern, ordkeyval())
     qout( ordkeyval(), recno() )
   endif
   dborderinfo(DBOI_SKIPWILD,,,cPattern)
enddo
nSec:=secondscpu()-nSec
? "SKIPWILD:", nSec, "sec."

return nil
