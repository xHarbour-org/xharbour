#define _DBNAME "_tst"
#include "ord.ch"
FIELD FNUM, FSTR
function main()
local n, aDB
RDDSETDEFAULT("DBFCDX")
aeval(directory("./"+_DBNAME+".??x"),{|x|ferase(x[1])})
ferase("./"+_DBNAME+".dbf")
? "RDD: "+rddsetdefault()
? "creating databse and index..."
aDb:={{"FSTR", "C", 10, 0},{"FNUM", "N", 10, 0}}
dbcreate(_DBNAME, aDb)
use _DBNAME shared
for n:=1 to 15
    dbappend()
    replace FNUM with int((n+2)/3)
    replace FSTR with chr(FNUM+48)
next
INDEX on FNUM tag TG_N to _DBNAME
INDEX on FSTR tag TG_C to _DBNAME
ORDSETFOCUS(1)

? "UNIQUE FLAG:", DBORDERINFO(DBOI_UNIQUE), ", DESCEND FLAG:", DBORDERINFO(DBOI_ISDESC)
SKIPTEST()
INKEY(0)

DBORDERINFO(DBOI_UNIQUE,,,.T.)
? "UNIQUE FLAG:", DBORDERINFO(DBOI_UNIQUE), ", DESCEND FLAG:", DBORDERINFO(DBOI_ISDESC)
SKIPTEST()
INKEY(0)

DBORDERINFO(DBOI_ISDESC,,,.T.)
? "UNIQUE FLAG:", DBORDERINFO(DBOI_UNIQUE), ", DESCEND FLAG:", DBORDERINFO(DBOI_ISDESC)
SKIPTEST()
INKEY(0)

DBCLOSEAREA()
? "."
RETURN NIL

FUNCTION SKIPTEST()
DBGOTOP()
? "[TOP]", "RECORD:", RECNO(), ", VAL:", FNUM
WHILE !EOF()
    ? "RECORD:", RECNO(), ", VAL:", FNUM
    DBSKIP()
ENDDO
? "[EOF]", "RECORD:", RECNO(), ", VAL:", FNUM
WHILE !BOF()
    ? "RECORD:", RECNO(), ", VAL:", FNUM
    DBSKIP(-1)
ENDDO
DBGOBOTTOM()
? "[BOTTOM]", "RECORD:", RECNO(), ", VAL:", FNUM
WHILE !BOF()
    ? "RECORD:", RECNO(), ", VAL:", FNUM
    DBSKIP(-1)
ENDDO
RETURN NIL
