*****************************************************
* Demo of timeout checking with INET/THREAD systems
* This file contains also a skeleton of socket
* service management through an async control thread.
*
* Contributed by Giancarlo Niccolai and Charles Kwon
*


* This array contain the socket control objects that
* the control system will use to do it's cleanups

GLOBAL aCntSockets

* When holding this mutex, the services are signaling that
* they don't want to be interrupted by the controller
GLOBAL MutexCnt

PROCEDURE Main( cAddress, nPort, nTimeout )
   LOCAL Socket, ThreadID, CtlThreadID
   LOCAL nStatus := 0
   LOCAL nDots := 0

   CLEAR SCREEN


   /* Requesting Inet system startup */
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

   /* Initializing socket control structure to void */
   aCntSockets := {}
   MutexCnt := CreateMutex()

   /* Now starting the control thread */
   CtlThreadID := StartThread ( @ControlThread(), nTimeout )

   /* Now we can start how many services threads we want */
   StartThread ( @Connect(), cAddress, nPort, nTimeout )

   nDots := 0
   WHILE .T.
      ThreadSleep( 200 ) // always a sleep!
      @7, 10 + nDots SAY "."
      nDots ++
      MutexLock( MutexCnt )
      IF Len( aCntSockets ) > 0 .and. aCntSockets[1][3] != 0
         MutexUnlock( MutexCnt )
         EXIT
      ENDIF
      MutexUnlock( MutexCnt )
   ENDDO

   // -1: connection timed out
   // 1: connection established or error while connecting

   DO CASE
      CASE aCntSockets[1][3] == -2
         @8, 10 say "Server name not resolved before timeout."

      CASE aCntSockets[1][3] == -1
         @8, 10 say "Connection not established before timeout."

      CASE aCntSockets[1][3] == 1
         @8, 10 say "Connection ESTABLISHED. Timeout aborted."

      CASE aCntSockets[1][3] == 2
         @8, 10 say "Connection REJECTED. Timeout aborted."

      CASE aCntSockets[1][3] == 3
         @8, 10 say "Server name not found."

   ENDCASE

   StopThread( CtlThreadID )
   DestroyMutex( MutexCnt )
   @10,10 SAY "Please, press a key to continue."

   INKEY(0)

   InetCleanup()

RETURN


/********************************************
* This function handles async connecton
* It uses another async thread to search the
* server name
*/

FUNCTION Connect( cAddress, nPort, nTimeout )
   LOCAL aServiceData
   LOCAL Socket
   LOCAL MutexDone
   LOCAL thSearcher
   LOCAL aServer
   LOCAL nPos

   /* We create now a vaild service data, without the socket */
   aServiceData := { InetCreate(), ThreadGetCurrent(), 0 , Seconds() }

   /* Locking here to prevent control thread to intervene in the middle
      of our operation */
   MutexLock( MutexCnt )
   AAdd( aCntSockets, aServiceData )
   MutexUnlock( MutexCnt )

   /* request to get the inet address of the server asynchronously */
   MutexDone := CreateMutex()
   thSearcher := StartThread( @SearchForServer(), cAddress, MutexDone )

   /* we'll wait 1/2 of the timeout */
   aServer := Subscribe( MutexDone, 500 * nTimeout )
   /*And if we have not found an answer, we return failure */
   MutexLock( MutexCnt )

   IF aServer == NIL
      aServiceData[3] := -2 /* timed out */
      MutexUnlock( MutexCnt )
      /* Kill that thread, but without waiting for it to be done*/
      KillThread( thSearcher )
      RETURN
   ENDIF

   /* But the resolver could also return a failure */
   IF Len( aServer ) == 0
      aServiceData[3] := 3 /* Name not found */
      MutexUnlock( MutexCnt )
      RETURN
   ENDIF

   MutexUnlock( MutexCnt )

   /* now we can be interrupted */

   InetConnectIP( aServer[1], nPort, aServiceData[1] )

   /* now we need to be not interrupted */
   MutexLock( MutexCnt )
   IF InetErrorCode( aServiceData[ 1 ] ) > 0
      aServiceData[ 3 ] := 2  // Connection rejected
   ELSE
      aServiceData[ 3 ] := 1  // Connection done.
   ENDIF

   aServiceData[ 4 ] := Seconds()
   aCntSockets[ 1 ] := aServiceData

   MutexUnlock( MutexCnt )

RETURN NIL


/********************************************
* This is the control thread, that ensures that
* the threads have not reached a dead point
* or a too long blocking condition.
*/

FUNCTION ControlThread( nTimeout )
   LOCAL aTicket

   DO WHILE .T.
      ThreadSleep( 1000 )
      MutexLock( MutexCnt )
      FOR EACH aTicket IN aCntSockets

         /* If status is still connecting ... */
         IF aTicket[ 3 ] == 0 .and. aTicket[ 4 ] + nTimeout < Seconds()
            StopThread( aTicket[ 2 ] )
            aTicket[ 3 ] := -1
            InetDestroy( aTicket[1] )
         ENDIF

         /* A complete app could have more status/timeout relations */

      NEXT
      MutexUnlock( MutexCnt )

      /* In a complete enviroment, this thread should also remove
         unused tickets */

   ENDDO

RETURN NIL


/********************************************
* This function scans asynchronously internet
* for a server name
*/

FUNCTION SearchForServer( cName, MutexDone )
   LOCAL aServer

   aServer := InetGetHosts( cName )
   Notify( MutexDone, aServer )
RETURN
