
PROCEDURE Main( cAddress, nPort, nTimeout )
   LOCAL Socket, ThreadID, CtlThreadID
   LOCAL nStatus := 0
   LOCAL nDots := 0

   CLEAR SCREEN
   
   InetInit()

   IF cAddress == NIL
      cAddress := "intel.com"
   ENDIF

   IF nPort == NIL
      nPort := "80"
   ENDIF

   IF nTimeout == NIL
      nTimeout := "4"
   ENDIF

   @5, 10 SAY "Connecting to " + cAddress +":"+ nPort
   @6, 10 SAY "Timeout set to " + nTimeout + " seconds."

   nPort := Val( nPort )
   nTimeout := Val( nTimeout )

   ThreadID := StartThread ( @Connect(), cAddress, nPort, @Socket, @nStatus )
   CtlThreadID := StartThread ( @CheckTime(), nTimeout, @nStatus, ThreadID )

   nDots := 0
   WHILE nStatus == 0
      ThreadSleep( 200 ) // always a sleep!
      @7, 10 + nDots SAY "."
      nDots ++
   ENDDO

   // -1: connection timed out
   // 1: connection established or error while connecting

   DO CASE
      CASE nStatus == -1
         @8, 10 say "Connection not established before timeout."

      CASE nStatus == 1
         @8, 10 say "Connection ESTABLISHED. Timeout aborted."
         StopThread( CtlThreadID )
         // this is just not to exit programs before all threads are stopped.

      CASE nStatus == 2
         @8, 10 say "Connection REJECTED. Timeout aborted."
         StopThread( CtlThreadID )
         // this is just not to exit programs before all threads are stopped.
   ENDCASE

   @10,10 SAY "Please, press a key to continue."

   INKEY(0)
   
   InetCleanup()

RETURN 

FUNCTION Connect( cAddress, nPort, Socket, nStatus )

   Socket := InetConnect( cAddress, nPort )
   IF InetErrorCode( Socket ) != 0
      nStatus := 2  // Connection rejected
   ELSE
      nStatus := 1  // Connection done.
   ENDIF

   // we don't need the socket anymore
   InetDestroy( Socket )

RETURN NIL

FUNCTION CheckTime( nTimeout, nStatus, nThread )
   LOCAL nSeconds := 0


   WHILE nSeconds < nTimeout
      ThreadSleep( 1000 )
      nSeconds ++
   ENDDO

   IF nStatus == 0
      StopThread( nThread ) // because gethostbyname is not killed by InetClose (now)
   ENDIF

   nStatus := -1 // connection not done.

RETURN NIL


