/*
 * $Id: dbevalts.prg,v 1.2 2000/11/08 17:28:24 rglab Exp $
 */

FUNCTION Main()
   LOCAL nCount

   USE test

   dbGoto( 4 )
   ? RecNo()
   COUNT TO nCount
   ? RecNo(), nCount
   COUNT TO nCount NEXT 10
   ? RecNo(), nCount

   RETURN NIL

