PROCEDURE Main()

    LOCAL i
    LOCAL Mutex := CreateMutex()
    LOCAL Thread4Handle, MonitorHandle

    CLS

    StartThread ( @ThreadFunc(), NIL,  8, "1st. Thread:", 200, Mutex )
    StartThread ( @ThreadFunc(), NIL,  9, "2nd. Thread:", 500, Mutex )
    StartThread ( @ThreadFunc(), NIL, 10, "3rd. Thread:", 600, Mutex )
    Thread4Handle := StartThread( @ThreadFunc(), NIL, 11, "4th. Thread", 700, Mutex )

    MonitorHandle := StartThread ( @MonitorFunc(), NIL, Mutex )
    StartThread ( @FourthMonitor(), NIL, Thread4Handle, Mutex )

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

    StopThread( MonitorHandle )

    WaitforThreads()

    @ 24, 00

PROCEDURE ThreadFunc( nRow, cName, nMax, Mutex )

   LOCAL i

   FOR i := 0 TO nMax
      @ nRow, 10 SAY cName + Str( i, 4 )

      WaitForThis( nRow ) // calling a proc inside a thread
   NEXT

   @ nRow, 10 SAY cName + ': DONE '+ space(20)

   Notify( Mutex, cName )

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

      ThreadSleep( 2000 )
   ENDDO

PROCEDURE FourthMonitor( ThreadHandle, Mutex )

   @ 5, 5 SAY "Waiting for 4th. Thread to finish..."

   JoinThread( ThreadHandle )

   @ 5, 5 SAY "FourthMonitor: 4th. Thread Finished!"

RETURN

