#include "hbclass.ch"
#include "fileio.ch"

#define MAX_RETRIES 32

STATIC s_cResponse

PROCEDURE Main( cAddress, cPassword, cPort, cFileAbort )

   LOCAL oErr, oClient, nPort, aUpdates, oUpdate

   IF ! Empty( cPort )
      nPort := Val( cPort )
   ENDIF

   InetInit()

   //TRY
      oClient := TUpdateClient( nPort )

      oClient:Connect( cAddress, cPassword )

      aUpdates := oClient:GetAvailable()

      FOR EACH oUpdate IN aUpdates
          WITH OBJECT oUpdate
             TraceLog( :cFileName, :nFileSize, :dDate, :cTime, :InstallScript )
           END
      NEXT

      oClient:GetUpdates( aUpdates, "updates", { | cFile, nFileSize, nTransfered | QOut( cFile, nFileSize, nTransfered ), IIF( cFile == cFileAbort, .F., .T. ) } )

      oClient:Disconnect()
   //CATCH oErr
   //   ? oErr:ProcName, oErr:ProcLine
   //END

   InetCleanup()

RETURN

INIT PROCEDURE Init()

   s_cResponse := Space( 65536 )

RETURN

CLASS TUpdateClient

   DATA nPort
   DATA Socket

   METHOD New( nPort ) CONSTRUCTOR
   METHOD Send( cSend, nLen )    INLINE InetSend( ::Socket, cSend, nLen )
   METHOD SendAll( cSend, nLen ) INLINE InetSendAll( ::Socket, cSend, nLen )
   METHOD GetResponse( nLengthByRef, nMaxLength )
   METHOD GetResponseAll( nLength )
   METHOD Connect( cIP, cPassword )
   METHOD GetAvailable( cPath, cPassword )
   METHOD GetUpdates( aUpdates, cRoot, bBlock )
   //METHOD ExecuteScripts( aUpdates, cRoot )
   METHOD Disconnect() INLINE INetClose( ::Socket )

ENDCLASS

METHOD New( nPort ) CLASS TUpdateClient

   IF Empty( nPort )
      nPort := 8001
   ENDIF

   ::nPort := nPort

RETURN self

METHOD GetResponse( nLengthByRef, nMaxLength ) CLASS TUpdateClient

   nLengthByRef := InetRecv( ::Socket, @s_cResponse, nMaxLength )

   IF nLengthByRef <= 0
      //Throw( ErrorNew() )
      RETURN ""
   ENDIF

RETURN Left( s_cResponse, nLengthByRef )

METHOD GetResponseAll( nLength ) CLASS TUpdateClient

   IF InetRecvAll( ::Socket, @s_cResponse, nLength ) != nLength
      Throw( ErrorNew() )
      RETURN ""
   ENDIF

RETURN Left( s_cResponse, nLength )

METHOD Connect( cAddress, cPassword ) CLASS TUpdateClient

   IF Empty( cAddress )
      cAddress := "127.0.0.1"
   ENDIF

   IF Empty( cPassword )
      cPassword := "SamplePassword"
   ENDIF

   ::Socket := InetConnect( cAddress, ::nPort )
   IF InetErrorCode( ::Socket ) != 0
      Throw( ErrorNew() )
      RETURN NIL
   ENDIF

   ::Send( cPassword )
   IF ! ::GetResponse( , 3 ) == "OK;"
      Throw( ErrorNew() )
   ENDIF

RETURN Self

METHOD GetAvailable( cPath, cPassword ) CLASS TUpdateClient

   LOCAL cResponse, aUpdates, cSend, nLength

   cSend := "GetAvailable;"

   IF ! Empty( cPassword )
      cSend += cPassword
   ELSEIF ! Empty( cPath )
      cSend += ";"
   ENDIF

   IF ! Empty( cPath )
      cSend += cPath
   ENDIF

   ::Send( cSend )

   cResponse := ::GetResponse( , 10 )
   nLength := Val( cResponse )

   IF nLength > 0
      ::Send( "OK;" + cResponse )
      cResponse := ::GetResponseAll( nLength )
   ENDIF

   //TRY
      aUpdates := &( cResponse )

      /*
      IF ! HB_IsArray( aUpdates )
         Throw( ErrorNew() )
         aUpdates := {}
      ENDIF
      */
   //CATCH
      //aUpdates := {}
   //END

RETURN aUpdates

METHOD GetUpdates( aUpdates, cRoot, bBlock )

   LOCAL cResponse, cSend, oUpdate, oStream, nBytesRead, nTotalBytes, lAgain := .T.
   LOCAL nRetries := 0

   cSend := ValToPrgExp( aUpdates )

   ::Send( "GetUpdates;" + Str( Len( cSend ), 10, 0 ), 21 )

   IF ::GetResponse( , 13 ) == "OK;" + Str( Len( cSend ), 10, 0 )
      ::SendAll( cSend, Len( cSend ) )
   ELSE
      Throw( ErrorNew() )
   ENDIF

   IF cRoot[-1] != '\'
      cRoot += '\'
   ENDIF

   //TraceLog( "Getting:", Len( aUpdates ) )

   WHILE lAgain
      lAgain := .F.

      FOR EACH oUpdate IN aUpdates

         IF oUpdate == NIL
            LOOP
         ENDIF

         //? "Getting:", HB_EnumIndex(), Len( aUpdates ), oUpdate:cFileName, oUpdate:nFileSize
         nTotalBytes := 0

         oStream := TStreamFileWriter():New( cRoot + oUpdate:cFileName )
         oStream:Seek( 0, FS_SET )

         //TraceLog( cRoot, oUpdate:cFileName, oUpdate:nFileSize, oStream:cFile, oStream:Handle )

         //? "Getting:", oUpdate:cFileName, oUpdate:nFileSize, oStream:cFile, oStream:Handle

         WHILE nTotalBytes < oUpdate:nFileSize
            cResponse := ::GetResponse( @nBytesRead, Min( 1024, oUpdate:nFileSize - nTotalBytes ) )

            IF ! Eval( bBlock, oUpdate:cFileName, oUpdate:nFileSize, nTotalBytes )
               ::Send( "!!;", 3 )
               TraceLog( "Aborted", oUpdate:cFileName, oUpdate:nFileSize, nTotalBytes )
               RETURN Self
            ENDIF

            IF ! Empty( cResponse )
               ::Send( "OK;", 3 )
            ELSE
               //? "RE-TRY:", nBytesRead, Min( 1024, oUpdate:nFileSize - nTotalBytes ), oUpdate:cFileName, oUpdate:nFileSize, oStream:cFile, oStream:Handle

               nTotalBytes := 0
               oStream:Seek( 0, FS_SET )

               WHILE nRetries++ < MAX_RETRIES .AND. ( cTrace := ::GetResponse( , 11 ) ) != "RESTARTING;"
                  ::Send( "RETRY;", 6 )
               END

               IF nRetries < MAX_RETRIES
                  nRetries := 0
               ELSE
                  TraceLog( "Failed", oUpdate:cFileName, oUpdate:nFileSize, nTotalBytes )
                  EXIT
               ENDIF

               //? "RE-TRY:", oUpdate:cFileName, oUpdate:nFileSize, oStream:cFile, oStream:Handle
               LOOP
            END

            //TraceLog( nBytesRead )
            nTotalBytes += nBytesRead

            oStream:Write( cResponse, 0, nBytesRead )
         END

         cResponse := ::GetResponse( @nBytesRead, 20 )
         IF cResponse == ";transfer_completed;"
            //TraceLog( "Trancating: " + oStream:cFile, oStream:nPosition )
            //Truncate incase it was a bigger file.
            oStream:Write( "", 0, 0 )
            oUpdate := NIL
         ELSE
            lAgain := .T.
         ENDIF
      NEXT
   END

RETURN Self
