*
* Complex example of Multi thread usage
*
* Giancarlo Niccolai
* $Id: mtcomplex.prg,v 1.12 2003/10/19 00:57:54 jonnymind Exp $
*
* Here we have a main thread counting, and some secondary
* threads counting too (in different fashons).
* A control thread is notified when each secondary
* thread finishes, and some of the secondary threads
* are killed before they are able to reach the end.
*

PROCEDURE Main()
   LOCAL i
   LOCAL Mutex := CreateMutex()
   LOCAL Thread4Handle, MonitorHandle
   LOCAL bKill := .F.

   SET OUTPUT SAFETY ON

   set color to w+/b
   CLEAR SCREEN
   @1,15 SAY "X H A R B O U R - Complex multithreading test"
   @3,17 SAY "Press any key to terminate in every moment"

   StartThread ( @ThreadFunc(), 10, "1st. Thread", 100, Mutex )
   StartThread ( @ThreadFunc(), 11, "2nd. Thread", 400, Mutex )

   /* Test of the { codeblock } grammar */
   StartThread ( { | nRow, cName, nLoops, Mtx| ThreadFunc(nRow, cName, nLoops, Mtx) } ;
         , 12, "3rd. Thread", 600, Mutex )

   Thread4Handle := StartThread( @ThreadFunc(), 13, "4th. Thread", 700, Mutex )

   /* Notice the "function name" grammar */
   MonitorHandle := StartThread ( "MonitorFunc", Mutex )
   *MonitorHandle := StartThread ( @MonitorFunc() , Mutex )

   StartThread ( @FourthMonitor(), Thread4Handle, Mutex )

   FOR i := 0 TO 500
      @ 14, 10 SAY 'Main Thread:' + Str( i, 4 )

      IF i == 100
         StopThread( Thread4Handle )
         @ 14, 27 SAY "(Killed 4th. Thread!)" 
      ENDIF

      ThreadSleep( 30 )
      IF Inkey() != 0
         bKill := .T.
         EXIT
      ENDIF
   NEXT

   IF bKill
      @17, 10 SAY 'Killing all threads on user requests      '
      KillAllThreads()
   ELSE
      @ 17, 10 SAY 'Cycle over, stopping Monitor     '
      KillThread( MonitorHandle )
      @ 17, 10 SAY 'Cycle over, Monitor Stopped     '
   ENDIF
      
   WaitforThreads()

   @ 19, 10 SAY 'Program over - press a key    '
   Inkey( 0 )

   @ 24, 00

PROCEDURE ThreadFunc( nRow, cName, nMax, Mutex )

   LOCAL i

   FOR i := 0 TO nMax
      @ nRow, 10 SAY cName + Str( i, 4 )
      WaitForThis( nRow ) // calling a proc inside a thread
   NEXT

   Notify( Mutex, cName )
   @ nRow, 10 SAY cName + ': DONE '+ space(20)
RETURN

PROCEDURE WaitForThis( nRow )

   ThreadSleep( 30 - nRow * 2 )

RETURN

PROCEDURE MonitorFunc( Mutex )

   LOCAL cName

   @ 5, 5 SAY "Entering Monitor Thread"
   @ 6, 5 SAY "->> Thread finished:"

   DO WHILE .T.
      cName = Subscribe( Mutex )

      IF cName != NIL
         @ 6, 26 SAY cName
      ELSE
         @ 6, 26 SAY "waiting ...           " 
      ENDIF

      ThreadSleep( 1500 )
      @ 6, 26 SAY "                          "
   ENDDO

RETURN

PROCEDURE FourthMonitor( ThreadHandle, Mutex )

   @ 8, 5 SAY "Waiting for 4th. Thread to finish..."

   JoinThread( ThreadHandle )

   @ 8, 5 SAY "FourthMonitor: 4th. Thread Finished/Killed!"

RETURN

