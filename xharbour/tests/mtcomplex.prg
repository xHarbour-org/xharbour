PROCEDURE Main()

   LOCAL i
   LOCAL Mutex := CreateMutex()
   LOCAL Thread4Handle, MonitorHandle

   CLS

   StartThread ( @ThreadFunc(), 8, "1st. Thread:", 100, Mutex )
   StartThread ( @ThreadFunc(), 9, "2nd. Thread:", 400, Mutex )

   /* Test of the { codeblock } grammar */
   StartThread ( { | nRow, cName, nLoops, Mtx| ThreadFunc(nRow, cName, nLoops, Mtx) } ;
         , 10, "3rd. Thread:", 600, Mutex )

   Thread4Handle := StartThread( @ThreadFunc(), 11, "4th. Thread", 700, Mutex )

   /* Notice the "function name" grammar */
   MonitorHandle := StartThread ( "MonitorFunc", Mutex )
   *MonitorHandle := StartThread ( @MonitorFunc() , Mutex )

   StartThread ( @FourthMonitor(), Thread4Handle, Mutex )

   FOR i := 0 TO 600
      @ 12, 10 SAY 'Main Thread:' + Str( i, 4 )

      IF i == 100
         StopThread( Thread4Handle )
         @ 12, 27 SAY "(Killed 4th. Thread!)"
      ENDIF

      // Be nice (at first).
      IF i < 300
         ThreadSleep( 30 )
      ENDIF
   NEXT

   @ 13, 10 SAY 'Cycle over, stopping Monitor     '

   StopThread( MonitorHandle, Mutex )
   WaitforThreads()
   DestroyMutex( Mutex )
   @ 14, 10 SAY 'Program over     '

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

   @ 2, 5 SAY "Entering Monitor Thread"
   @ 3, 5 SAY "->> Thread finished:"

   DO WHILE .T.
      cName = Subscribe( Mutex )

      IF cName != NIL
         @ 3, 26 SAY cName
      ELSE
         @ 3, 26 SAY "waiting ...           "
      ENDIF

      ThreadSleep( 500 )
   ENDDO

RETURN

PROCEDURE FourthMonitor( ThreadHandle, Mutex )

   @ 5, 5 SAY "Waiting for 4th. Thread to finish..."

   JoinThread( ThreadHandle )

   @ 5, 5 SAY "FourthMonitor: 4th. Thread Finished/Killed!"

RETURN

