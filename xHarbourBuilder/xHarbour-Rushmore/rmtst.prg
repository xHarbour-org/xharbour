#define N_REC 1000000

REQUEST RMDBFCDX

#include "dbinfo.ch"

function main(xOpt,xRDD)

field F1, F2, F3

local aDb:={{"F1", "C", 10, 0},;
            {"F2", "D",  8, 0},;
            {"F3", "N", 10, 2}},;
      cFi:="_tstdb", t, r, rr:="", n:=12345, rl, rl2

if !empty(xRDD)
   rddsetdefault(xRDD)
else
   rddsetdefault("RMDBFCDX")
endif
? rddsetdefault()
rddInfo(RDDI_STRUCTORD, .t.)

if !dbExists( cFi )
   dbDrop( cFi )
   ? padr("building databse...",30)
   t:=seconds()
   dbcreate(cFi, aDb)
   use (cFi)
   while lastrec() < N_REC
      dbappend()
      r:=recno() % 10000
      n := n ^^ r
      replace F1 with chr( n % 26 + 65 )
      replace F2 with date() + ( n ^^ 255 ) % 100 - 50 + hb_random(10) - 5
      replace F3 with n % 100 + r / 1000 - r / 10 + r * 3 + hb_random(100) - 50
   enddo
   ?? seconds()-t, "sec."
   dbcommit()
   close
endif
if !dbExists( cFi, cFi )
   use (cFi)
   ? padr("building indexes...",30)
   t:=seconds()
   index on F1 tag tg1 to (cFi) //FOR !DELETED()
   index on F2 tag tg2 to (cFi)
   index on F3 tag tg3 to (cFi)
   ?? seconds()-t, "sec."
   dbcommit()
   close
endif

use (cFi) shared
set index to (cfi)

/*
   if you enable the optimization (it's off by default in xHarbour)
   then RM filters will also help even without query analyzer because
   "bad" records will be tested (read from DB) only once and eliminated
 */
if !empty( xOpt ) .and. upper( xOpt ) = "A"
   set optimize on
else
   set optimize off
endif

t:=seconds()
set filter to F1='B' .and. F2>=date()-3 .and. F2<=date()+3 .and. F3>=1300 .and. F3<=1400 //.and. !deleted()
set order to 2
? dbfilter()
? dbOrderInfo(DBOI_OPTLEVEL)
?
if empty( xOpt ) .or. upper( xOpt ) = "A"
   ? padr("simple filter expression...",30)
   rr:=rlCount( rlGetFilter() )
   r:=ordkeycount()
   dbgotop()
elseif upper( xOpt ) = "Q"
   ? padr("building the RM query...",30)
   rl:=RLNEWQUERY( dbfilter() )
   rr:=rlCount( rl )
   rlSetFilter( rl )
   r:=ordkeycount()
   dbgotop()
else
   ? padr("building the RM filters...",30)
   rl:=rlNew(N_REC)
   rlSetLoHi( rl, 'B', 'B', 1 )
   rl2:=rlNew(N_REC)
   rlSetLoHi( rl2, date()-3, date()+3, 2 )
   rl := rlAnd( rl, rl2 )
   rl2:=rlNew(N_REC)
   rlSetLoHi( rl2, 1300, 1400, 3 )
   rl := rlAnd( rl, rl2 )
   rr:=rlCount( rl )
   rlSetFilter( rl )
   r:=ordkeycount()
   dbgotop()
endif
? seconds()-t, "sec."
? "records: ", r, rr
?

t:=seconds()
dbgotop()
while !eof()
   dbskip()
enddo
? "SKIP:", seconds()-t, "sec."

t:=seconds()
dbgotop()
while !eof()
   deleted()
   dbskip()
enddo
? "SKIP2:", seconds()-t, "sec."
?
? "now let's see browser..."
inkey(0)
browse()
close
? "."
return nil
