***************************************************
* X harbour Inet demo server program
* $Id: server.prg,v 1.7 2003/01/14 02:40:40 jonnymind Exp $
*
* Giancarlo Niccolai
*
* In this program, the server uses just one thread
* to demonstrate how to use timeout sockets.
*

Procedure Main( cPort )
   LOCAL Socket, s
   LOCAL nResponse, cResponse
   LOCAL nTurn := 0, nTurn1 := 0
   LOCAL CRLF := InetCRLF()
   LOCAL bCont := .T.

   CLS

   IF Empty( cPort )
      cPort := "2000"
   ENDIF

   InetInit()

   @ 1, 15 SAY "X H A R B O U R - Inet Api Server Demo"
   @ 2, 5 SAY "Contact this server using telnet or the xHarbour Inet Client demo"
   @ 3, 5 SAY "Press a [KEY] to terminate the program"
   @ 5, 5 SAY "Server listening on port " + cPort + "..."
   Socket := InetServer( Val( cPort ) )
   InetSetTimeout( Socket, 500 )

   DO WHILE bCont

      @ 6, 5 SAY Space( 70 )
      @ 7, 5 SAY Space( 70 )
      @ 8, 5 SAY Space( 70 )
      @ 9, 5 SAY Space( 70 )
      @ 6, 5

      * Accepting a connection
      DO WHILE bCont
         Progress( @nTurn, 5, 39 )
         s := InetAccept( Socket )
         IF InetErrorCode( Socket ) == 0
            EXIT
         ENDIF
         IF Inkey() != 0
            bCont := .f.
         ENDIF
      ENDDO

      IF .not. bCont
         EXIT
      ENDIF

      InetSetTimeout( s, 500 )

      @ 6, 5 SAY "Connection from: " + InetAddress( s ) + ":" + Str( InetPort( s ), 5 )
      @ 7, 5 SAY "Receiving: "
      @ 8, 5

      nResponse := InetSend( s, "Welcome to my server!" + CRLF )

      DO WHILE bCont
         cResponse := InetRecvLine( s, @nResponse )

         DO CASE
            CASE InetErrorCode( s ) == 0
               @ 8, 5 SAY space(70)
               @ 8, 5 SAY cResponse
               cResponse := "Count: " + Str( nResponse ) + " characters" + CRLF
               InetSend( s, cResponse )

            CASE InetErrorCode( s ) == -1
               * idle (timed out)
               Progress( @nTurn1, 7, 17 )

            OTHERWISE
               @7, 5 SAY "Received Error " + Str( InetErrorCode( s ) ) + ": " + InetErrorDesc( s )
               @ 8, 5 SAY space(70)
               @ 9, 5 SAY space(70)
               @ 9, 5 SAY "Press a key to continue"
               InetDestroy( s )
               Inkey( 0 )
               EXIT

         END CASE

         IF Inkey() != 0
            InetDestroy( s )
            bCont := .f.
         ENDIF
      ENDDO
   ENDDO

   InetDestroy( Socket )

   InetCleanup()

RETURN


PROCEDURE Progress( nProgress, nDrow, nDcol )

   LOCAL nRow := Row(), nCol := Col()

   @ nDrow, nDcol SAY "[ ]"

   DO CASE
      CASE nProgress = 0
          @ nDrow, nDcol + 1 SAY "-"
      CASE nProgress = 1
          @ nDrow, nDcol + 1 SAY "\"
      CASE nProgress = 2
          @ nDrow, nDcol + 1 SAY "|"
      CASE nProgress = 3
          @ nDrow, nDcol + 1 SAY "/"
   ENDCASE

   nProgress++

   IF nProgress == 4
      nProgress := 0
   ENDIF

   @ nRow, nCol

RETURN
