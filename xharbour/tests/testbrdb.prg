/*
 * $Id: testbrdb.prg,v 1.3 2000/11/08 17:28:24 rglab Exp $
 */

// Testing Browse()

function Main()
   LOCAL cColor

   cColor := SETCOLOR("W+/B")
   CLS

   USE test
   Browse()

   SETCOLOR(cColor)
   CLS

return nil
