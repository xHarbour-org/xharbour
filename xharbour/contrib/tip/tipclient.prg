/**********************************************
* tIPClient.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipclient.prg,v 1.9 2004/02/07 12:53:55 lculik Exp $
************************************************/
/* 2004-01-13
  Enhaced tip cliente to conenct to secure smtp servers by Luiz Rafael Culik
*/
#include "hbclass.ch"
#include "fileio.ch"
#include "tip.ch"

/**
* Inet Client class
*/

CLASS tIPClient
   CLASSDATA bInitSocks INIT .F.
   CLASSDATA cCRLF INIT InetCRLF()
   DATA oUrl            //url to wich to connect
   DATA oCredentials    //credential needed to access the service
   DATA nStatus         //basic status
   DATA SocketCon

   /* Input stream length */
   DATA nLength
   /* Input stream data read by the app*/
   DATA nRead
   /* Last physical read amount */
   DATA nLastRead

   DATA nDefaultPort
   DATA nConnTimeout
   DATA bInitialized

   DATA cReply
   DATA nAccessMode
   DATA nLastWrite

   DATA bEof

   METHOD New( oUrl, oCredentials )
   METHOD Open()

   METHOD Read( iLen )
   METHOD ReadToFile( cFile, nMode )
   METHOD Write( cData, iLen, bCommit )
   METHOD Commit()
   METHOD WriteFromFile( cFile )
   METHOD Reset()
   METHOD Close()
   METHOD Data( cData )
ENDCLASS


METHOD New( oUrl, oCredentials ) CLASS tIPClient
   LOCAL oRet

   IF .not. ::bInitSocks
      InetInit()
      ::bInitSocks := .T.
   ENDIF

   DO CASE
      CASE oUrl:cProto == "http"
         oRet := tIPClientHTTP():New()
      CASE oUrl:cProto == "pop"
         oRet := tIPClientPOP():New()
      CASE oUrl:cProto == "smtp"
         oRet := tIPClientSMTP():New()
      CASE oUrl:cProto == "ftp"
         oRet := tIPClientFTP():New()
   ENDCASE

   IF Empty( oRet )
      RETURN NIL
   ENDIF

   oRet:oUrl := oUrl
   oRet:oCredentials := oCredentials
   oRet:nStatus := 0
   oRet:bInitialized := .F.
   oRet:nLastWrite := 0
   oRet:nLength := -1
   oRet:nRead := 0
   oRet:nLastRead := 0
   oRet:bEof := .F.

RETURN oRet

METHOD Open( cUrl ) CLASS tIPClient
   LOCAL nPort

   IF HB_IsString( cUrl )
      ::oUrl := tUrl():New( cUrl )
   ENDIF

   IF ::oUrl:nPort == -1
      nPort := ::nDefaultPort
   ELSE
      nPort := ::oUrl:nPort
   ENDIF

   ::SocketCon := InetCreate()

   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   InetConnect( ::oUrl:cServer, nPort, ::SocketCon )
   IF InetErrorCode( ::SocketCon ) != 0
      RETURN .F.
   ENDIF
RETURN .T.

METHOD Close() CLASS tIPClient
   IF .not. Empty( ::SocketCon )
      RETURN InetClose( ::SocketCon )
   ENDIF
RETURN -1

METHOD Reset() CLASS tIPClient
   ::bInitialized := .F.
   ::bEof := .F.
RETURN .T.

METHOD Commit() CLASS tIPClient
RETURN .T.


METHOD Read( nLen ) CLASS tIPClient
   LOCAL cStr, cStr1

   IF ::nLength > 0 .and. ::nLength == ::nRead
      RETURN NIL
   ENDIF

   IF Empty( nLen ) .or. nLen < 0 .or.( ::nLength > 0 .and. nLen > ::nLength - ::nRead )
         nLen := ::nLength - ::nRead
   ENDIF

   IF Empty( nLen ) .or. nLen < 0
      // read till end of stream
      cStr1 := Space( 1024 )
      cStr := ""
      ::nLastRead := InetRecvAll( ::SocketCon, @cStr1, 1024 )
      DO WHILE ::nLastRead > 0
         ::nRead += ::nLastRead
         cStr += Substr( cStr1, 1, ::nLastRead )
         ::nLastRead := InetRecvAll( ::SocketCon, @cStr1, 1024 )
      ENDDO
      ::bEof := .T.
   ELSE
      // read an amount of data
      cStr := Space( nLen )
      ::nLastRead := InetRecvAll( ::SocketCon, @cStr, nLen )
      IF ::nLastRead <= 0
         ::bEof := .T.
         RETURN NIL
      ENDIF
      ::nRead += ::nLastRead
      IF ::nRead == ::nLength
         ::bEof := .T.
      ENDIF
      cStr := Substr( cStr, 1, ::nLastRead )
   ENDIF
RETURN cStr



METHOD ReadToFile( cFile, nMode ) CLASS tIPClient
   LOCAL nFout
   LOCAL cData

   IF Empty ( nMode )
      nMode := FO_CREAT
   ENDIF

   ::nStatus := 1
   DO WHILE InetErrorCode( ::SocketCon ) == 0 .and. .not. ::bEof

      cData := ::Read( 1024 )
      IF HB_IsLogical(cData) .and. cData == .F.
         IF nFout != NIL
            Fclose( nFout )
         ENDIF
         RETURN .F.
      ENDIF

      IF nFout == NIL
         nFout := Fcreate( cFile, nMode )
         IF nFout < 0
            ::nStatus := 0
            RETURN .F.
         ENDIF
      ENDIF

      IF Fwrite( nFout, cData ) < 0
         Fclose( nFout )
         RETURN .F.
      ENDIF
   ENDDO

   ::nStatus := 2
   Fclose( nFout )
RETURN .T.


METHOD WriteFromFile( cFile ) CLASS tIPClient
   LOCAL nFin
   LOCAL cData
   LOCAL nLen

   ::nStatus := 0
   nFin := Fopen( cFile, FO_READ )
   tracelog(nFin,cFile)
   IF nFin < 0
      RETURN .F.
   ENDIF

   ::nStatus := 1
   cData := Space( 1024 )
   nLen := Fread( nFin, @cData, 1024 )
   tracelog(cdata,nlen)
   DO WHILE nLen > 0
      IF ::Write( @cData, nLen ) != nLen
         Fclose( nFin )
         RETURN .F.
      ENDIF
      nLen := Fread( nFin, @cData, 1024 )
   tracelog(cdata,nlen)
   ENDDO
   ::Commit()

   ::nStatus := 2
   Fclose( nFin )
RETURN .T.


METHOD Data( cData ) CLASS tIPClient
   InetSendAll( ::SocketCon, "DATA" + ::cCRLF )
   IF .not. ::GetOk()
      RETURN .F.
   ENDIF
   InetSendAll(::SocketCon, cData + ::cCRLF + "." + ::cCRLF )
RETURN ::GetOk()

METHOD Write( cData, nLen, bCommit ) CLASS tIPClient

   IF Empty( nLen )
      nLen := Len( cData )
   ENDIF

   ::nLastWrite := InetSendAll( ::SocketCon,  cData , nLen )
   IF .not. Empty( bCommit ) .and. bCommit
      ::Commit()
   ENDIF

RETURN ::nLastWrite
