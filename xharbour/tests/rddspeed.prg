#define TST_NUM
//#define TST_DAT
//#define TST_CHR

#define TST_SILENT

static nLoops := 100000
static nModulo := 100000
static nRepeat := 1

#define EOL chr(10)
#ifndef EOL
  #define EOL chr(13)+chr(10)
#endif
#xcommand ? [<xx,...>] => outstd(EOL [,<xx>])
#xcommand ?? [<xx,...>] => outstd([<xx>])

#ifdef _SIX_
  #xtranslate ordkeyval([<xx,...>]) => sx_keyData([<xx>])
#endif


field FTST

function main(rdd, xLoops, xModulo, xRepeat, xLateInd, xReUse)
local aDb, n, l, cFi:="_tstdb", s, tm, total:=seconds(), cType:="",;
      lLateInd:=!empty( xLateInd ), lReUse:=!empty(xReUse)

#ifdef __CLIP__
  SET TRANSLATE PATH OFF
#endif
#ifndef FlahShip
  #ifdef _SIX_
    REQUEST SIXCDX
    REQUEST SIXNSX
    REQUEST SIXNTX
  #else
    REQUEST DBFCDX
    REQUEST DBFNTX
  #endif
#endif

? VERSION()+", "+OS()
SET EXCLUSIVE OFF
#ifdef TST_NUM
  cType := "NUMERIC"
  aDb:={{"FTST", "N", 20, 9}}
#endif
#ifdef TST_DAT
  cType :=  "DATE"
  aDb:={{"FTST", "D", 8, 0}}
#endif
#ifdef TST_CHR
  cType :=  "CHARACTER"
  aDb:={{"FTST", "C", 20, 0}}
#endif

if !empty( xLoops )
  nLoops := val( xLoops )
endif
if !empty( xModulo )
  nModulo := val( xModulo )
  if nModulo == 0
    nModulo := nLoops
  endif
endif
if !empty( xRepeat )
  nRepeat := val( xRepeat )
  if nRepeat == 0
    nRepeat := 1
  endif
endif

if empty(rdd)
#ifdef FlahShip
  rdd:="DBFIDX"
#else
  rdd:="DBFCDX"
#endif
endif
rddSetDefault(rdd)
? "RDD: " + rdd + " type: " + cType
? "records: " + ntrim( nLoops ) + ;
  ", repeated: " + ntrim( nRepeat ) + ;
  ", modulo: " + ntrim( nModulo )

if lReUse
  use (cFi)
  set Index To (cFi)
  set Order To "tg1"
else
  aeval(directory("./"+cFi+".??x"),{|x|ferase(x[1])})
  ferase("./"+cFi+".dbf")

  ? padr("creating databse"+iif(lLateInd, "", " and index")+"...", 30)
  tm := seconds()
  dbcreate(cFi, aDb)
  use (cFi)
  if !lLateInd
    index on FTST tag tg1 to (cFi)
  endif
  for n:=1 to nLoops
    dbappend()
    replace FTST with dbval(n)
#ifndef TST_SILENT
    indexar2(nLoops)
#endif
  next
  dbcommit()
  ?? seconds() - tm, "sec."
  if lLateInd
    ? padr("creating index...", 30)
    tm := seconds()
    if "NTX" $ upper(rdd)
#ifndef TST_SILENT
      index on FTST to (cFi) eval indexar()
#else
      index on FTST to (cFi)
#endif
    else
#ifndef TST_SILENT
      index on FTST tag tg1 to (cFi) eval indexar()
#else
      index on FTST tag tg1 to (cFi)
#endif
    endif
    ?? seconds() - tm, "sec."
  endif
endif

testskip()
testrskip()
#ifndef NO_SEEK_TEST
testseek()
#endif

testskip()
testrskip()
#ifndef NO_SEEK_TEST
testseek()
#endif

dbclosearea()

? padr("Total execution time...",30)
?? seconds() - total, "sec."
?
return nil

function testskip()
local tm, vcur, vold, rcur, rold, l:=0
? padr("skip test...", 30)
tm := seconds()
dbgotop()
while !eof()
  ++l
  vcur:=FTST
  rcur:=recno()
  if ordkeyval()!=dbval(recno())
    ? " ordkeyval="+dspval(ordkeyval())+", dbval="+dspval(dbval(recno()))
  endif
  if rold!=nil .and. (vcur < vold .or. (vcur == vold .and. rcur <= rold ))
    ? " skip faild: curr=["+dspval(vcur)+"]["+ntrim(rcur)+"] prev=["+dspval(vold)+"]["+ntrim(rold)+"]"
  endif
  vold:=vcur
  rold:=rold
  dbskip()
enddo
?? seconds() - tm, "sec. count: "+ntrim(l)
return nil

function testrskip()
local tm, vcur, vold, rcur, rold, l:=0
? padr("revers skip test...", 30)
tm := seconds()
dbgobottom()
while !bof()
  ++l
  vcur:=FTST
  rcur:=recno()
  if ordkeyval()!=dbval(recno())
    ? " ordkeyval="+dspval(ordkeyval())+", dbval="+dspval(dbval(recno()))
  endif
  if rold!=nil .and. (vcur > vold .or. (vcur == vold .and. rcur >= rold ))
    ? " skip faild: curr=["+dspval(vcur)+"]["+ntrim(rcur)+"] prev=["+dspval(vold)+"]["+ntrim(rold)+"]"
  endif
  vold:=vcur
  rold:=rold
  dbskip(-1)
enddo
?? seconds() - tm, "sec. count: "+ntrim(l)
return nil

function testseek()
local tm, s, n
? padr("testing seek...", 30)
tm := seconds()
for n:=1 to nLoops
  if !dbseek(dbval(n))
    ? "seek faild: "+dspval(dbval(n))
  elseif ordkeyval()!=dbval(n)
    ? " ordkeyval="+dspval(ordkeyval())+", dbval="+dspval(dbval(n))
  endif
next
?? seconds() - tm, "sec. count: "+ntrim(n-1)
return nil

function ntrim(n)
return ltrim(str(n))

function indexar()
fwrite(2, str(recno()/lastrec()*100,6,2) + repl(chr(8),6))
//fwrite(2, str(100,6,2) + repl(chr(8),6))
return (.t.)

function indexar2(n)
fwrite(2, str(recno()/n*100,6,2) + repl(chr(8),6))
return (.t.)

function baseval(nn)
return ( int( nn / nRepeat ) % nModulo ) - ;
       int( ( int( nLoops / nRepeat ) % nModulo ) / 2 )

#ifdef TST_NUM
    function dbval(nn)
    return round( baseval(nn) / 1000, 4 )
    function dspval(x)
    return ntrim(x)
#endif
#ifdef TST_DAT
    function dbval(nn)
    static dt
    if dt==nil
      dt:=date()
    endif
    return dt + baseval(nn)
    function dspval(x)
    return dtos(x)
#endif
#ifdef TST_CHR
    function dbval(nn)
    local n := int( nn / nRepeat )
    return str( baseval(nn) + nLoops + nLoops )
    function dspval(x)
    return x
#endif
