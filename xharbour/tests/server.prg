GLOBAL g_nUserCount
GLOBAL g_nTotalCount

PROCEDURE Main( cPort)

   LOCAL GetList := {}

   LOCAL socket
   LOCAL Key
   LOCAL cCommand

   LOCAL hView, hAccept

   TraceLog( cPort)

   g_nUserCount  := 0
   g_nTotalCount := 0

   CLEAR SCREEN


   SELECT 1
   use "SITE.DBF" index SITE01.NTX, SITE02.NTX
   SELECT 2
   use "PAGE.DBF" index PAGE01.NTX, PAGE02.NTX


   @ 1, 15 SAY "Welcome to Giancarlo Niccolai's CLIPPER WEB SERVER"

   InetInit()

   Socket := InetServer( val( cPort) )

   @ 3, 10 SAY "Waiting for connections on port " + cport

   * Initializing List of Threads

   hView   := StartThread( @ViewUpdate(), NIL, Socket )
   hAccept := StartThread( @AcceptIncoming(), NIL, Socket )

   DO WHILE .T.
       cCommand := Space( 50 )

       @ 5, 5 SAY "Enter Command: " get cCommand
       READ

       cCommand := Trim( cCommand )

       IF Upper( cCommand ) == "QUIT"
             StopThread( hView )
             StopThread( hAccept )
             EXIT
       ENDIF
   ENDDO

   InetClose( Socket )
   InetDestroy( Socket )
   InetCleanup()

   CLOSE ALL

RETURN

******************************************
* Managing visual updates
*

PROCEDURE ViewUpdate( Socket )

   LOCAL nRow, nCol
   LOCAL nProgress

   TraceLog( Socket )

   nProgress := 0

   DO WHILE .T.
      * Saving cursor status before screen update
      nRow = Row()
      nCol = Col()

      Looping( @nProgress, 10, 5 )

      @ 10, 9 SAY "Looping "
      @ 11, 5 SAY "Main socket status: " + InetErrorDesc( Socket ) + ;
            "(" + trim( str( InetErrorCode( Socket ) ) )+ ")"
      @ 12, 5 SAY "Connected Users: " + str( g_nUserCount )
      @ 13, 5 SAY "No more stats for now..."

      @  nRow, nCol
      ThreadSleep( 100 )
   ENDDO

RETURN

************************************************************
* Server Socket manager
*

PROCEDURE AcceptIncoming( Socket )

   LOCAL com

   TraceLog( Socket )

   DO WHILE .T.
      * Saving cursor status before screen update
      Com = InetAccept( Socket )

      @ 20, 10 SAY "Connection from: " + INetAddress( Com )

     IF InetErrorCode( Com ) == 0
          g_nUserCount++
          g_nTotalCount++
          StartThread( @ServeClient, NIL, Socket )
          ThreadSleep( 1 )
      ELSE
          InetDestroy( Com )
          EXIT
      ENDIF
   ENDDO

RETURN

************************************************************
* Service incoming connection
*

PROCEDURE ServeClient( Socket )
   InetSend( Socket, "Sorry, we are down for works" + chr(13) + chr(10))
   InetDestroy( Socket )
   g_nUserCount--

RETURN

************************************************************
* Girello
*
PROCEDURE Looping( nProgress,  nR, nC )

   LOCAL nRow := Row(), nCol := Col()

   *TraceLog( nProgress,  nR, nC )

   IF nProgress > 3 .or. nProgress < 0
      nProgress := 0
   ENDIF

   @ nR, nC SAY "[ ]"
   @ nRow, nCol

   DO CASE
      CASE nProgress = 0
         @  nR, nC + 1 SAY "-"
      CASE nProgress == 1
         @  nR, nC + 1 SAY "\"
      CASE nProgress == 2
         @  nR, nC + 1 SAY "|"
      CASE nProgress == 3
         @  nR, nC + 1 SAY "/"
   ENDCASE

   nProgress++

   @ nRow, nCol

RETURN
