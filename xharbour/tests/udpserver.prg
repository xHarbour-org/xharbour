*************************************************
* Test UDP server
* Will reply to datagrams having "AYT?" messages
* with a "HERE!" reply. All other messages will
* be just printed to screen
*************************************************

Procedure Main( cPort )

   LOCAL Socket
   LOCAL nResponse, cResponse
   LOCAL nProgress := 0

   CLS

   IF Empty( cPort )
      cPort := "2000"
   ENDIF

   InetInit()

   Socket := InetDGramBind( Val( cPort ) )

   @ 5, 5 SAY "Server listening: ..."
   @9,9 SAY "Waiting. Press a key to stop."

   cResponse := Space( 60 )

   DO WHILE InetErrorCode( Socket )  == 0 .and. Upper( Trim(cResponse )) != "QUIT" .and. INKEY() == 0

      IF InetDataReady( Socket, 150 ) != 0
         nResponse := InetDGramRecv( Socket, cResponse, 60 )
      ELSE
         Progress( @nProgress )
         LOOP
      ENDIF

      @6,5 SAY "Connection from: " + InetAddress( Socket ) + ":" + Str( InetPort( Socket ), 5 )

      IF InetErrorCode( Socket ) != 0
          EXIT
      ENDIF

      @ 7, 5 SAY "Received:"
      @ 8, 5 SAY space(70)
      @ 8, 5 SAY cResponse

      /* If we recive an Are You There code, we must send an HERE response */
      IF SubStr( cResponse, 1, nResponse ) == "AYT?"
         
         /* Recv functions fill the remote address with the latest connection */
         InetDGramSend( Socket, InetAddress( Socket ), InetPort( Socket ), "HERE!" )
      
      ENDIF

      cResponse := Space( 60 )
   ENDDO

   @ 8, 5 SAY Space(70)
   @ 8, 5 SAY "Error code " + Str( InetErrorCode( Socket ) ) + ": " + InetErrorDesc( Socket )
   @ 9, 5 SAY "Any key to quit...                    "

   Inkey(0)

   InetClose( Socket )
   InetDestroy( Socket )

   InetCleanup()

RETURN

PROCEDURE Progress( nProgress )

   LOCAL nRow := Row(), nCol := Col()

   @ 9, 5 SAY "[ ]"
   @ nRow, nCol

   DO CASE
      CASE nProgress = 0
          @ 9, 6 SAY "-"
      CASE nProgress = 1
          @ 9, 6 SAY "\"
      CASE nProgress = 2
          @ 9, 6 SAY "|"
      CASE nProgress = 3
          @ 9, 6 SAY "/"
   ENDCASE

   nProgress++

   IF nProgress == 4
      nProgress := 0
   ENDIF

   @ nRow, nCol

RETURN
