// Client:
GLOBAL RequestTerm

PROCEDURE Main( cAddress, cPort )

   LOCAL Socket, ThreadID
   LOCAL nLen, bFlag
   LOCAL cText
   LOCAL GetList := {}

   CLS

   IF Empty( cAddress )
      cAddress := "127.0.0.1"
   ENDIF

   IF Empty( cPort )
      cPort := "2000"
   ENDIF

   RequestTerm = .F.

   InetInit()

   Socket := InetConnect( cAddress, Val( cPort ) )

   IF InetErrorCode( Socket ) != 0
       ? "Can't connect with " + cAddress +": " + InetErrorDesc( Socket )
       Inkey(0)
       InetDestroy( Socket )
       return
   ENDIF

   ThreadID = StartThread( @ReceivePoll(), NIL, Socket );

   bFlag = .T.
   DO WHILE InetErrorCode( Socket ) == 0 .and. bFlag
      cText := Space( 60 )
      @ 1, 2 SAY "Enter cText: " GET cText
      READ
      @ 1, 15

      IF Upper( RTrim( cText ) ) == "QUIT"
          bFlag := .F.
      ENDIF

      nLen  := InetSend( Socket, Trim( cText ) + chr(13) + chr( 10 ) )
   ENDDO

   RequestTerm = .T.
   InetClose( Socket )
   waitforthreads()

   InetDestroy( Socket)
   InetCleanup()

PROCEDURE ReceivePoll( Socket )

   LOCAL nResponse, cResponse := Space( 80 )
   LOCAL nProgress
   LOCAL nRow := Row(), nCol := Col()

   nProgress := 0

   @ 9, 6 SAY "Waiting for data from: " + InetAddress( Socket )

   @ nRow, nCol

   DO WHILE ! RequestTerm
     IF InetDataReady( Socket ) > 0
        cResponse := InetRecvLine( Socket )

        IF InetErrorCode( Socket ) != 0
           @ 10, 0 SAY Left( cResponse, nResponse )
           @ nRow, nCol
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
