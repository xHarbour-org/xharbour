//NOTEST - You'll want to test this with the output redirected to a file!
//
// $Id: longstr2.prg,v 1.1 1999/10/04 18:46:30 vszel Exp $
//


function Main()

   local short := "1234567890"
   local i, long, very_long, cNewLine

   cNewLine := HB_OSNewLine()

   long := short
   for i := 1 TO 12
      long += long
   next

   very_long := long
   for i := 1 to 5
      very_long += very_long
   next

   OutErr (len(short), len(long), len(very_long))
   Qout   (len(short), len(long), len(very_long))

   OutStd (cNewLine)
   OutStd (len(short), len(long), len(very_long))

   OutStd (cNewLine)
   OutStd (cNewLine)
   OutStd (short)

   OutStd (cNewLine)
   OutStd (cNewLine)
   OutStd (long)

   OutStd (cNewLine)
   OutStd (cNewLine)
   OutStd (very_long)

return nil
