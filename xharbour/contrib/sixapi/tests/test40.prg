/*
 * $Id$
 */
#include "sixapi.ch"
/*
   test40.prg
   __dx_dbSort( ...... )
*/
#define EOL chr(10)
#command ? => outstd(EOL)
#command ? <xx,...> => outstd(<xx>, EOL)
 
PROCEDURE MAIN

   LOCAL n := seconds()

   ? 'Demo on SORT TO ... command .. Press any key ...'
   PAUSE
   ? 'USE "TEST\TEST" ALIAS TEST EXCLUSIVE'
   ? 'Working ...'
   USE "TEST\TEST" ALIAS TEST EXCLUSIVE
   ? 'SORT TO NEWFILE ON LAST DESCENDING'
   SORT TO NEWFILE ON LAST DESCENDING
   ?
   ? 'Done!'
   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'BROWSE .. Press any key ...'
   PAUSE
   USE "NEWFILE"
   BROWSE
   CLS
   CLOSE DATABASE
   SELECT TEST
   CLOSE INDEXES

   ?
   ? 'SORT with DATE Field ... Press any key ...'
   PAUSE
   n := seconds()
   ? 'SORT TO NEWFILE ON HIREDATE DESCENDING'
   SORT TO NEWFILE ON HIREDATE DESCENDING
   ?
   ? 'Done!'
   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'BROWSE .. Press any key ...'
   PAUSE
   USE "NEWFILE"
   BROWSE

   CLOSE ALL
