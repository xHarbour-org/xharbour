***********************************************
* UDP CLIENT TEST
* This clients creates a broadcast socket and
* boadcasts the given network in search of a
* correspoinding UDP SERVER TEST.
* Then, you can enter some text that will be
* sent to server.
* CAN BE CALLED USING A VALID NETWORK BROADCAST
* ADDRESS, or a known server address.
************************************************

PROCEDURE Main( cAddress, cPort )

   LOCAL Socket, ThreadID
   LOCAL cText, nLen
   LOCAL GetList := {}

   CLS

   IF Empty( cAddress )
      cAddress := "127.0.0.255"
   ENDIF

   IF Empty( cPort )
      cPort := "2000"
   ENDIF

   InetInit()

   /* Requesting broadcast capability */
   Socket := InetDGram( .T. )

   IF InetErrorCode( Socket ) != 0
       ? "Can't connect with " + cAddress +": " + InetErrorDesc( Socket )
       Inkey(0)
       InetDestroy( Socket )
       return
   ENDIF

   /* Now we scan for a suitable server in the network */
   @1,2 SAY "Sending 'AYT?' request now"
   InetDGramSend( Socket, cAddress, Val( cPort ), "AYT?" )
   cText := space( 60 )
   @2,2 SAY "Waiting (at max. 10 seconds) for server response now"

   nLen := InetDGRecvTout( Socket, 10000, cText, 60 )

   /* Now, if nLen == 0, the server didn't answered */
   DO CASE
      CASE nLen == 0
         @3,2 SAY "Response timeout; no server found."

      CASE nLen < 0
         @3,2 SAY "Error in InetDGRecvTout function call or in Socket "
         @4,2 SAY "(" + AllTrim( Str( InetErrorCode( Socket ) ) ) + ")" + InetErrorDesc( Socket )

      CASE Substr( cText, 1, nLen ) != "HERE!"
         @3,2 SAY "Error in protocol"
         @4,2 SAY "Expected answer 'HERE!' but received: "
         @5,2 SAY Substr( cText, 1, nLen )
         /* A serious program should try again here, with less timeout at disposal */

      OTHERWISE
         /* InetDGramRecv family functions fills the "remote" field with
            the latest UDP packet sender */

         cAddress := InetAddress( Socket )
         @3,2 SAY "Found a server at " + cAddress

         DO WHILE InetErrorCode( Socket ) == 0
            cText := Space( 60 )
            @ 4, 2 SAY "Text to send: " GET cText
            READ

            InetDGramSend( Socket, cAddress, Val( cPort ), cText, 60 )

            IF Upper( RTrim( cText ) ) == "QUIT"
               EXIT
            ENDIF

         ENDDO
   END CASE

   InetClose( Socket )
   InetDestroy( Socket )

   @6, 2 SAY "Program done: Press a key to continue"
   Inkey( 0 )
   InetCleanup()

RETURN
