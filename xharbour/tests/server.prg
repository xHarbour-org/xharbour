// SERVER:
Procedure Main( cPort )

   LOCAL Socket, s, nResponse, cResponse

   CLS

   IF Empty( cPort )
      cPort := "2000"
   ENDIF

   InetInit()

   Socket := InetServer( Val( cPort ) )

   @ 5, 5 SAY "Server listening: ..."

   s := InetAccept( Socket )

   @ 6, 5 SAY "Connection from: " + InetAddress( s ) + ":" + Str( InetPort( s ), 5 )

   nResponse := InetSend( s, "Welcome to my server!" + Chr(13) + Chr(10) )

   DO WHILE nResponse >= 0
      cResponse := InetRecvLine( s )

      IF InetErrorCode( s ) != 0
          @ 8, 5 SAY Space(70)
          @ 8, 5 SAY "Error code " + Str( InetErrorCode( s ) ) + ": " + InetErrorDesc( s )
          @ 9, 5 SAY "Any key to quit..."

          Inkey(0)

          QUIT
      ENDIF

      IF cResponse != NIL
         @ 7, 5 SAY "Received:"
         @ 8, 5 SAY space(70)
         @ 8, 5 SAY cResponse

         InetSend( s, "Count: " + Str( Len( cResponse ) ) + " characters" + Chr(13) + Chr(10 ) )
      ENDIF
   ENDDO

   InetClose( s )
   InetClose( Socket )
   InetCleanup()

RETURN
