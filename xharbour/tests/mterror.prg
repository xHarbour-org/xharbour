***************************************************************
* Demonstration of error conditions and cancelations in MT
*
* Giancarlo Niccolai
* $Id: mtcomplex.prg,v 1.11 2003/10/18 16:20:03 jonnymind Exp $
*
* There are several threads endlessy counting. Pressing F2 cause
* an error randomly in a thread. This test works if error is
* properly reported and VM quits gracefully. Use ALT-C to cancel
* threads.
*

#include "inkey.ch"

GLOBAL bGenerateError

PROCEDURE Main()
   LOCAL i := 0, nKey
   LOCAL bKill := .F.

   bGenerateError := .F.

   SET OUTPUT SAFETY ON

   set color to w+/b
   CLEAR SCREEN
   @1,30 SAY "X H A R B O U R - Multithreading error test. "
   @3,10 SAY "Press any key to terminate in every moment."
   @4,10 SAY "Press ALT-C to perform graceful cancelation."
   @5,10 SAY "Press F2 to rise an error."

   StartThread ( @ThreadFunc(), 10, "1st. Thread" )
   StartThread ( @ThreadFunc(), 11, "2nd. Thread" )
   StartThread ( @ThreadFunc(), 12, "3rd. Thread" )
   StartThread ( @ThreadFunc(), 13, "4th. Thread" )
   StartThread ( @ThreadFunc(), 14, "5th. Thread" )

   DO WHILE ! bKill
      @ 9, 10 SAY 'Main Thread:' + Str( i, 4 )

      ThreadSleep( 100 )
      nKey := Inkey()
      IF nKey != 0
         IF nKey == K_F2
            bGenerateError := .T.
         ELSE
            bKill := .T.
         ENDIF
      ENDIF
      i++
   ENDDO

   @17, 10 SAY 'Killing all threads on user requests      '
   KillAllThreads()

   WaitforThreads()

   @ 19, 10 SAY 'Program over - press a key    '
   Inkey( 0 )

   @ 24, 00
RETURN


PROCEDURE ThreadFunc( nRow, cName )
   LOCAL i := 0

   DO WHILE .T.

      @nRow, 10 SAY cName + Str( i, 4 )
      ThreadSleep( Int(HB_Random(1000)) )
      i++
      IF bGenerateError
         RiseAnError()
      ENDIF
   ENDDO

RETURN

PROCEDURE RiseAnError()
   LOCAL i

   @10,10 SAY i + " "

RETURN
