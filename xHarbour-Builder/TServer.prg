#include "hbclass.ch"
#include "directry.ch"
#include "fileio.ch"

#define BUFFER_SIZE 16384

#define MAX_UPDATES 32
#define MAX_RETRIES 32

STATIC s_cRequest

PROCEDURE Main( cPort )

   LOCAL nPort, oServer

   IF ! Empty( cPort )
      nPort := Val( cPort )
   ENDIF

   InetInit()

   oServer := TCPIPServer( nPort )

   oServer:AcceptConnections( @TUpdateServer(), "SamplePassword" )

   InetCleanup()

RETURN

INIT PROCEDURE Init()

   s_cRequest := Space( 65536 )

RETURN

CLASS TCPIPServer

   DATA nPort, cPort, Socket, hView, hAccept

   DATA GetList
   DATA nUserCount  INIT 0
   DATA nTotalCount INIT 0
   DATA MutexCount
   DATA lContinue   INIT .T.

   DATA Server, Cargo

   METHOD New( nPort ) CONSTRUCTOR
   METHOD Status()
   METHOD AcceptConnections( Server, Cargo )
   METHOD LaunchServer()
   METHOD OnServerDone( nResult )
   METHOD Looping( nProgress, nrow, nCol )

ENDCLASS

METHOD New( nPort ) CLASS TCPIPServer

   IF nPort == NIL
      nPort := 8001
   ENDIF

   ::nPort := nPort
   ::cPort := Str( ::nPort, 5, 0 )

RETURN Self

METHOD AcceptConnections( Server, Cargo ) CLASS TCPIPServer

   LOCAL cCommand := space( 40 ), GetList := {}

   CLS

   ::GetList := GetList
   ::Server := Server
   ::Cargo  := Cargo

   TRY
      ::MutexCount := HB_MutexCreate()

      ::Socket := InetServer( ::nPort )

      @ 3, 10 SAY "Waiting for connections on port: " + ::cPort
      @ 5, 5 SAY "Enter Command      : " GET cCommand PICTURE "@K!"

      ::hView   := StartThread( Self, "Status" )
      ::hAccept := StartThread( Self, "LaunchServer" )

      DO WHILE .T.
         READ SAVE

         IF cCommand = "QUIT"
            ::lContinue := .F.
            InetClose( ::Socket )
            StopThread( ::hView )
            EXIT
         ENDIF
      ENDDO
   CATCH
      ::lContinue := .F.
      InetClose( ::Socket )
      StopThread( ::hView )
   END

   WaitForThreads()

   InetClose( ::Socket )
   DestroyMutex( ::MutexCount )

RETURN Self

METHOD LaunchServer() CLASS TCPIPServer

   LOCAL Connection

   INetSetTimeOut( ::Socket, 5000 )

   DO WHILE ::lContinue
      Connection := InetAccept( ::Socket )

      IF Connection != NIL
         HB_MutexLock( ::MutexCount )
         ::nUserCount++
         ::nTotalCount++
         HB_MutexUnlock( ::MutexCount )

         StartThread( ::Server, Connection, ::Cargo, Self, "OnServerDone" )
      ENDIF
   ENDDO

RETURN Self

METHOD Status() CLASS TCPIPServer

   LOCAL nProgress := 0

   @ 6, 5 SAY "[ ]"
   @ 7, 5 SAY "Main socket status : "
   @ 8, 5 SAY "Connected Users    : "
   @ 9, 5 SAY "Total users        : "

   DO WHILE ::lContinue
      ::Looping( @nProgress, 6, 5 )

      @ 7, 27 SAY InetErrorDesc( ::Socket ) + "(" + Trim( Str( InetErrorCode( ::Socket ) ) ) + ")"
      @ 8, 27 SAY ::nUserCount
      @ 9, 27 SAY ::nTotalCount

      SetPos( ::GetList[1]:Row, ::GetList[1]:Col + IIF( ::GetList[1]:Pos == NIL, 0, ::GetList[1]:Pos - 1 ) )

      ThreadSleep( 200 )
   ENDDO

RETURN Self

METHOD OnServerDone( nResult ) CLASS TCPIPServer

   HB_MutexLock( ::MutexCount )
   ::nUserCount--
   HB_MutexUnlock( ::MutexCount )

   HB_GCAll( .T. )

RETURN Self

METHOD Looping( nProgress,  nR, nC ) CLASS TCPIPServer

   LOCAL nRow := Row(), nCol := Col()

   IF nProgress > 3
      nProgress := 0
   ENDIF

   SWITCH nProgress
      CASE 0
         @  nR, nC + 1 SAY "-"
         EXIT

      CASE 1
         @  nR, nC + 1 SAY "\"
         EXIT

      CASE 2
         @  nR, nC + 1 SAY "|"
         EXIT

      CASE 3
         @  nR, nC + 1 SAY "/"
         EXIT
   END

   nProgress++

   SetPos( ::GetList[1]:Row, ::GetList[1]:Col + IIF( ::GetList[1]:Pos == NIL, 0, ::GetList[1]:Pos - 1 ) )

RETURN Self

CLASS TUpdateServer

   DATA cPort, nPort
   DATA Socket
   DATA cPassword
   DATA lAuthenticated INIT .F.

   METHOD SendAvailable( cFolder, cPassword )
   METHOD SendUpdates( aUpdates )
   METHOD New( Socket, cPassword, Server, cOnEnd )
   METHOD Send( cSend, nLen )    INLINE InetSend( ::Socket, cSend, nLen )
   METHOD SendAll( cSend, nLen ) INLINE InetSendAll( ::Socket, cSend, nLen )
   METHOD ProcessRequest( cRequet )
   METHOD GetRequest( nLength )
   METHOD GetRequestAll( nLength )

ENDCLASS

METHOD New( Socket, cPassword, Server, cOnDone ) CLASS TUpdateServer

   LOCAL nResult

   IF cPassword == NIL
      cPassword := ""
   ENDIF

   ::Socket    := Socket
   ::cPassword := cPassword

   nResult := ::ProcessRequest( ::GetRequest() )

   InetClose( ::Socket )

RETURN Server:&cOnDone( nResult )

METHOD GetRequest( nLength ) CLASS TUpdateServer

   ThreadSleep( 5 )

   nLength := InetRecv( ::Socket, @s_cRequest )

   IF nLength < 0
      RETURN ""
   ENDIF

RETURN Left( s_cRequest, nLength )

METHOD GetRequestAll( nLength ) CLASS TUpdateServer

   LOCAL nReceived

   ThreadSleep( 5 )

   nReceived := InetRecvAll( ::Socket, @s_cRequest, nLength )

   IF nReceived != nLength
      TraceLog( "Stream", 1007, ProcName(), "Receive Error.", nReceived, nLength, Date(), Time() )
      RETURN ""
   ENDIF

RETURN Left( s_cRequest, nLength )

METHOD ProcessRequest( cRequest ) CLASS TUpdateServer

   LOCAL cPassword, cFolder, nAt

   //TraceLog( cRequest )

   IF ! ::lAuthenticated
      IF cRequest == ::cPassword
         ::lAuthenticated := .T.
         ::Send( "OK;", 3 )
         RETURN ::ProcessRequest( ::GetRequest() )
      ELSE
         ::Send( "Authentication Failed: " + cRequest )
         RETURN Self
      ENDIF
   ENDIF

   IF Left( cRequest, 12 ) == "GetAvailable"
      cRequest := SubStr( cRequest, 14 )

      nAt := At( ';', cRequest )
      IF nAt > 0
         cPassword := Left( cRequest, nAt - 1 )
         cFolder   := SubStr( cRequest, nat + 1 )
      ELSE
         cPassword := cRequest
         cFolder   := ""
      ENDIF

      ::SendAvailable( cPassword, cFolder )
      RETURN ::ProcessRequest( ::GetRequest() )
   ENDIF

   IF Left( cRequest, 10 ) == "GetUpdates"
      ::Send( "OK;" + SubStr( cRequest, 12 ), 13 )
      ::SendUpdates( &( ::GetRequestAll( Val( SubStr( cRequest, 12 ) ) ) ) )
   ENDIF

RETURN Self

METHOD SendAvailable( cPassword, cFolder ) CLASS TUpdateServer

   LOCAL aUpdates := {}, aFile, cReply

   IF ! Empty( cFolder )
      DirChange( cFolder )
   ENDIF

   IF File( "update.psw" ) .AND. ! MemoRead( "update.psw" ) == cPassword
      TraceLog( "Error reading update.psw", Date(), Time())
      ::Send( "Authentication failed: " + cPassword )
      RETURN Self
   ENDIF

   FOR EACH aFile IN Directory()
      IF Lower( aFile[F_NAME] ) == "update.psw"
         LOOP
      ENDIF

      IF Lower( Right( aFile[F_NAME], 4 ) ) == ".upd"
         LOOP
      ENDIF

      IF aFile[F_SIZE] == 0
         LOOP
      ENDIF

      IF HB_EnumIndex() > MAX_UPDATES
         EXIT
      ENDIF

      aAdd( aUpdates, TUpdate( aFile ) )
   NEXT

   //TraceLog( aUpdates )

   cReply := ValToPrgExp( aUpdates )
   ::Send( Str( Len( cReply ), 10, 0 ), 10 )

   IF ::GetRequest( 13 ) == "OK;" + Str( Len( cReply ), 10, 0 )
      ::SendAll( cReply, Len( cReply )  )
   ENDIF

RETURN Self

METHOD SendUpdates( aUpdates ) CLASS TUpdateServer

   LOCAL oUpdate, oStream

   TraceLog( "Sending:", Len( aUpdates ) )

   FOR EACH oUpdate IN aUpdates
      oStream := TStreamTCPIPSender( oUpdate:cFileName, , Self ):Send()
   NEXT

RETURN Self

CLASS TStreamTCPIPSender FROM TStreamFileReader

   DATA oTCPIPServer

   METHOD New( cFile, nMode, oTCPIPServer ) CONSTRUCTOR

   METHOD Write( sBuffer, nOffset, nCount )

   METHOD Send()

   DESTRUCTOR Finalize

ENDCLASS

METHOD New( cFile, nMode, oTCPIPServer ) CLASS TStreamTCPIPSender

   ::lCanRead := .T.
   ::lCanWrite := .T.

   ::cFile := cFile
   ::oTCPIPServer := oTCPIPServer

   ::Handle := FOpen( cFile, nMode )
   IF ::Handle == -1
      TraceLog( "Stream", 1004, ProcName(), "Open Error.", cFile, nMode, Date(), Time() )
      ::nLength := 0
      RETURN Self
   ENDIF

   ::nPosition := 0
   ::nLength := FSeek( ::Handle, 0, FS_END )

   FSeek( ::Handle, 0, FS_SET )

RETURN Self

PROCEDURE Finalize CLASS TStreamTCPIPSender
   ::Close()
RETURN

METHOD Write( sBuffer, nOffset, nCount ) CLASS TStreamTCPIPSender

   LOCAL nWritten

   //TraceLog( ::cFile, nCount )

   nWritten := ::oTCPIPServer:Send( sBuffer, nCount )

   //TraceLog( nCount, nWritten )

   ::nPosition += nWritten

   IF nWritten != nCount
      TraceLog( "Stream", 1003, ProcName(), "Write failed - written:" + Str( nWritten ) + " bytes", nCount, Date(), Time() )
   ENDIF

RETURN nWritten

PROCEDURE Send() CLASS TStreamTCPIPSender

   LOCAL nBytesToRead := ::nLength
   LOCAL sBuffer := Space( BUFFER_SIZE )
   LOCAL nRead
   LOCAL nPosition
   LOCAL cResponse
   LOCAL nRetries := 0

   // Save.
   nPosition := ::nPosition

   ::Seek( 0, FS_SET )

   //TraceLog( ::cFile, ::nLength )

   WHILE nBytesToRead > 0
      nRead := ::Read( @sBuffer, 0, 1024 )

      IF ::Write( sBuffer, 0, nRead ) == nRead
         cResponse := ::oTCPIPServer:GetRequest( 3 )

         IF cResponse == "!!;"
            TraceLog( "QUIT Response." )
            EXIT
         ELSEIF ! cResponse == "OK;"
            TraceLog( "Bad Response.", cResponse )

            WHILE nRetries++ < MAX_RETRIES .AND. ( cTrace := ::oTCPIPServer:GetRequest( 6 ) ) != "RETRY;"
               ::oTCPIPServer:Send( "RESTARTING;", 11 )
            END

            IF nRetries < MAX_RETRIES
               nRetries := 0
            ELSE
               TraceLog( "Retry Failed", ::cFile, ::nLength, nBytesToRead )
               EXIT
            ENDIF

            ::oTCPIPServer:Send( "RESTARTING;", 11 )
            ::Seek( 0, FS_SET )
            nBytesToRead := ::nLength
            LOOP
         ENDIF

         nBytesToRead -= nRead
      ELSE
         TraceLog( "Write failed." )
         EXIT
      ENDIF
   END

   TraceLog( "Completed: " + ::cFile, ::nLength, nBytesToRead, nRead )

   ::Write( ";transfer_completed;", 0, 20 )

   // Restore.
   ::Seek( nPosition, FS_SET )

RETURN
