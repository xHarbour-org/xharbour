/**********************************************
* tIPClienthttp.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipclienthttp.prg,v 1.2 2003/02/22 21:26:49 jonnymind Exp $
************************************************/
#include "hbclass.ch"
#include "tip.ch"


/**
* Inet service manager: http
*/

CLASS tIPClientHTTP FROM tIPClient
   DATA cMethod
   DATA nVersion
   DATA nSubversion
   DATA bChunked

   METHOD New()
   METHOD GetRequest( cQuery )
   METHOD PostRequest( cQuery, cPostData )
   METHOD ReadHeaders()
   METHOD Read( nLen )

ENDCLASS

METHOD New() CLASS tIPClientHTTP
   ::nDefaultPort := 80
   ::nConnTimeout := 5000
   ::bChunked := .F.
RETURN Self


METHOD GetRequest( cQuery ) CLASS tIPClientHTTP
   InetSendAll( ::SocketCon, "GET " + cQuery + " HTTP/1.0" + ::cCRLF )
   InetSendAll( ::SocketCon, "Host: " + ::oUrl:cServer + ::cCRLF )
   InetSendAll( ::SocketCon, "Connection: close" + ::cCRLF )
   InetSendAll( ::SocketCon, ::cCRLF )
   IF InetErrorCode( ::SocketCon ) ==  0
      RETURN ::ReadHeaders()
   ENDIF
RETURN .F.


METHOD PostRequest( cQuery, cPostData ) CLASS tIPClientHTTP
   InetSendAll( ::SocketCon, "POST " + cQuery + " HTTP/1.1" + ::cCRLF )
   InetSendAll( ::SocketCon, "Host: " + ::oUrl:cServer + ::cCRLF )
   InetSendAll( ::SocketCon, "Connection: close" + ::cCRLF )
   InetSendAll( ::SocketCon, "Content-Length: " + ::oUrl:cServer + ::cCRLF )
   InetSendAll( ::SocketCon, ::cCRLF )
   IF InetErrorCode( ::SocketCon  ) ==  0
      InetSendAll( ::SocketCon, cPostData )
      RETURN ::ReadHeaders()
   ENDIF
RETURN .F.


METHOD ReadHeaders() CLASS tIPClientHTTP
   LOCAL cLine, nPos, aVersion

   // Now reads the fields and set the content lenght
   cLine := InetRecvLine( ::SocketCon, @nPos, 500 )
   IF Empty( cLine )
      // In case of timeout or error on receiving
      RETURN .F.
   ENDIF

   // Get Protocol version
   aVersion := HB_Regex( "^HTTP/(.)\.(.)", cLine )
   ::cReply := cLine

   IF aVersion == NIL
      ::nVersion := 0
      ::nSubvesion := 9
   ELSE
      ::nVersion := Val(aVersion[2])
      ::nSubversion := Val( aVersion[3] )
   ENDIF

   ::nLength := -1
   ::bChunked := .F.
   DO WHILE InetErrorCode( ::SocketCon ) == 0 .and. Len( cLine ) > 0
      IF At( "content-length:", lower(cLine) ) > 0
         cLine := Substr( cLine, 16 )
         ::nLength := Val( cLine )
      ELSEIF At( "transfer-encoding:", lower(cLine) ) > 0
         IF At( "chunked", lower( cLine ) ) > 0
            ::bChunked := .T.
         ENDIF
      ENDIF
      cLine := InetRecvLine( ::SocketCon, @nPos, 500 )
   ENDDO

   IF InetErrorCode( ::SocketCon ) != 0
      RETURN .F.
   ENDIF
RETURN .T.


METHOD Read( nLen ) CLASS tIPClientHTTP
   LOCAL cData, nPos, cLine

   IF .not. ::bInitialized
      ::bInitialized := .T.
      IF .not. ::GetRequest( ::oUrl:BuildQuery() )
         RETURN NIL
      ENDIF
   ENDIF

   /* On HTTP/1.1 protocol, content lenght can be in hex format before each chunk */
   IF ::nLength == -1 .and. ::nVersion >= 1 .and. ::nSubversion >= 1 .and. ::bChunked
      cLine := InetRecvLine( ::SocketCon, @nPos, 16 )
      IF .not. Empty( cLine )
         IF cLine == "0"
            InetRecvLine( ::SocketCon, @nPos, 16 )
            ::bEof := .T.
            RETURN NIL
         ELSE
            ::nLength := IP_HexToDec( cLine ) + ::nRead
         ENDIF
      ELSE
         RETURN  NIL
      ENDIF
   ENDIF

   cData := ::super:Read( nLen )
   IF ::bEof .and. ::nVersion >= 1 .and. ::nSubversion >= 1 .and. ::bChunked
      /* ...and after a sucessful read, we could have read up to chunk Lenght. */
      ::bEof := .F.
      ::nLength := -1
      /* removing following CRLF */
      InetRecvLine( ::SocketCon, @nPos, 16 )
   ENDIF

RETURN cData
