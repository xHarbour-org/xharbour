***************************************************
* X harbour Inet demo client program
* $Id: client.prg,v 1.10 2003/01/14 23:46:42 jonnymind Exp $
*
* Giancarlo Niccolai
*
* This program demonstrates Multithreading blocking
* sockets
*

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

   @ 1, 15 SAY "X H A R B O U R - Inet Api Client Demo"
   @ 2, 5 SAY "This client is used to contact the demo server; launch this program"
   @ 3, 5 SAY "using the address of the serever"
   @ 4, 5 SAY "Type 'quit' to terminate this program."
   @ 5, 5 SAY "Searching server at " + cAddress+ ":"+ cPort + "..."

   Socket := InetConnect( cAddress, Val( cPort ) )

   IF InetErrorCode( Socket ) != 0
       @7,5 SAY "Can't connect with " + cAddress +": " + InetErrorDesc( Socket )
       @8,5 SAY "Press a key to terminate the program"
       Inkey(0)
       RETURN
   ENDIF

   ThreadID = StartThread( @ReceivePoll(), Socket );

   DO WHILE InetErrorCode( Socket ) == 0
      cText := Space( 60 )
      HBConsoleLock()
      @ 7, 2 SAY "Send to server: " GET cText
      HBConsoleUnlock()
      
      READ
      InetSend( Socket, Trim( cText ) + chr(13) + chr( 10 ) )

      IF Upper( RTrim( cText ) ) == "QUIT"
          EXIT
      ENDIF

   ENDDO

   g_bDone = .T.

   WaitForThreads()

   InetClose( Socket )
   InetCleanup()

RETURN



PROCEDURE ReceivePoll( Socket )

   LOCAL nResponse, cResponse
   LOCAL nProgress
   LOCAL nRow := Row(), nCol := Col()

   nProgress := 0

   @ 11, 6 SAY "Waiting for data from: " + InetAddress( Socket )

   @ nRow, nCol

   DO WHILE ! g_bDone
      IF InetDataReady( Socket, 100 ) > 0
         cResponse := InetRecvLine( Socket, @nResponse )

         IF nResponse > 0
            @ 12, 6 SAY cResponse
            @ nRow, nCol
         ELSE
            @ 12, 6 SAY "Error: " + Str( nResponse )
         ENDIF
     ENDIF

     Progress( @nProgress, 11, 2 )
   ENDDO

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
