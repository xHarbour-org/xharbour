//
// $Id:$
//
// Andi Jahja <andijahja@xharbour.com>
//
// GLOBALs from within dump area
// Must link with global3.prg

GLOBAL EXTERNAL MYGLOBAL, MYGLOBALX
GLOBAL FROMGLOBAL4

PROCEDURE MYTEST
   FROMGLOBAL4 := "DECLARED FROM MYTEST"
   MYGLOBAL := "XHARBOUR ASSIGNED FROM MYTEST"
   MYGLOBALX := "MYGLOBALX ASSIGNED FROM MYTEST"
   OutStd( "MYGLOBAL =" + MYGLOBAL  + hb_osnewline() )
   OutStd( "MYGLOBALX=" + MYGLOBALX + hb_osnewline() )
   RETURN
