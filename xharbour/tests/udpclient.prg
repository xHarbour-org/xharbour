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

   Socket := InetDGram()

   IF InetErrorCode( Socket ) != 0
       ? "Can't connect with " + cAddress +": " + InetErrorDesc( Socket )
       Inkey(0)
       InetDestroy( Socket )
       return
   ENDIF

   DO WHILE InetErrorCode( Socket ) == 0
      cText := Space( 60 )
      @ 1, 2 SAY "Enter cText: " GET cText
      READ

      InetDGramSend( Socket, cAddress, Val( cPort ), cText, 60 )

      IF Upper( RTrim( cText ) ) == "QUIT"
          EXIT
      ENDIF

   ENDDO

   g_bDone = .T.

   InetClose( Socket )
   InetDestroy( Socket)
   InetCleanup()

