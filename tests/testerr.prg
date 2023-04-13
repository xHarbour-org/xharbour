/*
* $Id$
* Sample program to direct error log to designated file name
*/

function main

   LOCAL aLog := SET( _SET_ERRORLOG )

   // SET( _SET_ERRORLOG ) returns a 2-element array
   // array[1] -> error log file name
   // array[2] -> Logical TRUE = append mode, FALSE = overwrite mode

   OutStd ( "Error Directed To: " + aLog[1] + " (DEFAULT)"+ hb_osnewline() )
   OutStd ( "Append Mode: " + If(aLog[2],"Yes","No") + " (DEFAULT)"+ hb_osnewline() + hb_osnewline())

   SET ERRORLOG TO MYERR.LOG ADDITIVE

   aLog := SET( _SET_ERRORLOG )

   OutStd ( "Error Directed To: " + aLog[1] + hb_osnewline() )
   OutStd ( "Append Mode: " + If(aLog[2],"Yes","No") + hb_osnewline() )

   ? "Hello" - 1
return nil
