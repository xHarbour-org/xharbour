// Client:
GLOBAL g_bDone

PROCEDURE Main( cAddress, cPort )

   LOCAL Socket, ThreadID
   LOCAL cText
   LOCAL GetList := {}

   CLS

   IF Empty( cAddress )
      cAddress := "127.0.0.1"
   ENDIF

   IF Empty( cPort )
      cPort := "2000"
   ENDIF

   g_bDone = .F.

   InetInit()

   Socket := InetConnect( cAddress, Val( cPort ) )

   IF InetErrorCode( Socket ) != 0
       ? "Can't connect with " + cAddress +": " + InetErrorDesc( Socket )
       Inkey(0)
       InetDestroy( Socket )
       return
   ENDIF

   ThreadID = StartThread( @ReceivePoll(), Socket );

   DO WHILE InetErrorCode( Socket ) == 0
      cText := Space( 60 )
      @ 1, 2 SAY "Enter cText: " GET cText
      READ
      @ 1, 15

      IF Upper( RTrim( cText ) ) == "QUIT"
          EXIT
      ENDIF

      InetSend( Socket, Trim( cText ) + chr(13) + chr( 10 ) )
   ENDDO

   g_bDone = .T.

   WaitForThreads()

   InetClose( Socket )
   InetDestroy( Socket)
   InetCleanup()

PROCEDURE ReceivePoll( Socket )

   LOCAL nResponse, cResponse
   LOCAL nProgress
   LOCAL nRow := Row(), nCol := Col()

   nProgress := 0

   @ 9, 6 SAY "Waiting for data from: " + InetAddress( Socket )

   @ nRow, nCol

   DO WHILE ! g_bDone
      IF InetDataReady( Socket ) > 0
         cResponse := InetRecvLine( Socket, @nResponse )

         IF nResponse > 0
            @ 10, 0 SAY cResponse
            @ nRow, nCol
         ELSE
            @ 10, 0 SAY "Error: " + Str( nResponse )
         ENDIF
     ENDIF

     Progress( @nProgress )

     ThreadSleep( 100 )
   ENDDO

RETURN

PROCEDURE Progress( nProgress )

   LOCAL nRow := Row(), nCol := Col()

   @ 9, 1 SAY "[ ]"
   @ nRow, nCol

   DO CASE
      CASE nProgress = 0
          @ 9, 2 SAY "-"
      CASE nProgress = 1
          @ 9, 2 SAY "\"
      CASE nProgress = 2
          @ 9, 2 SAY "|"
      CASE nProgress = 3
          @ 9, 2 SAY "/"
   ENDCASE

   nProgress++

   IF nProgress == 4
      nProgress := 0
   ENDIF

   @ nRow, nCol

RETURN
