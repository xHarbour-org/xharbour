/**********************************************
* tIPClienthttp.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipclienthttp.prg,v 1.9 2003/12/10 01:48:33 jonnymind Exp $
************************************************/
#include "hbclass.ch"
#include "tip.ch"


/**
* Inet service manager: http
*/

CLASS tIPClientHTTP FROM tIPClient
   DATA cMethod
   DATA nVersion
   DATA nReplyCode
   DATA cReplyDescr
   DATA nSubversion
   DATA bChunked
   DATA hHeaders     INIT  {=>}
   DATA hCookies     INIT  {=>}
   DATA hFields      INIT  {=>}
   DATA cUserAgent   INIT  "Mozilla/3.0 (compatible XHarbour-Tip/1.0)"

   METHOD New()
   METHOD Get( cQuery )
   METHOD Post( cPostData, cQuery )
   METHOD ReadHeaders()
   METHOD Read( nLen )

HIDDEN:
   METHOD StandardFields()

ENDCLASS

METHOD New() CLASS tIPClientHTTP
   ::nDefaultPort := 80
   ::nConnTimeout := 5000
   ::bChunked := .F.
   HSetCaseMatch( ::hHeaders, .F. )
RETURN Self


METHOD Get( cQuery ) CLASS tIPClientHTTP
   IF .not. HB_IsString( cQuery )
      cQuery := ::oUrl:BuildQuery()
   ENDIF

   InetSendAll( ::SocketCon, "GET " + cQuery + " HTTP/1.1" + ::cCRLF )
   ::StandardFields()
   InetSendAll( ::SocketCon, ::cCRLF )
   IF InetErrorCode( ::SocketCon ) ==  0
      RETURN ::ReadHeaders()
   ENDIF
RETURN .F.


METHOD Post( cPostData, cQuery ) CLASS tIPClientHTTP
   LOCAL cData, nI

   IF HB_IsHash( cPostData )
      cData := ""
      FOR nI := 1 TO Len( cPostData )
         cData += TipEncoderUrl_Encode( AllTrim(CStr(HGetKeyAt( cPostData, nI ))) ) + "=" +;
           TipEncoderUrl_Encode( AllTrim(CStr( HGetValueAt( cPostData, nI  )))) + "&"
      NEXT
      cData[-1] = ""
   ELSEIF HB_IsString( cPostData )
      cData := cPostData
   ELSE
      Alert( "TipClientHTTP_PostRequest: Invalid parameters" )
      RETURN .F.
   ENDIF

   IF .not. HB_IsString( cQuery )
      cQuery := ::oUrl:BuildQuery()
   ENDIF

   InetSendAll( ::SocketCon, "POST " + cQuery + " HTTP/1.1" + ::cCRLF )
   ::StandardFields()

   IF .not. "Content-Type" IN ::hFields
      InetSendAll( ::SocketCon, e"Content-Type: application/x-www-form-urlencoded\r\n" )
   ENDIF

   InetSendAll( ::SocketCon, "Content-Length: " + ;
         LTrim(Str( Len( cData ) ) ) + ::cCRLF )

   // End of header
   InetSendAll( ::SocketCon, ::cCRLF )

   IF InetErrorCode( ::SocketCon  ) ==  0
      InetSendAll( ::SocketCon, cData )
      ::bInitialized := .T.
      RETURN ::ReadHeaders()
   ENDIF
RETURN .F.


METHOD StandardFields() CLASS tIPClientHTTP
   LOCAL iCount

   InetSendAll( ::SocketCon, "Host: " + ::oUrl:cServer + ::cCRLF )
   InetSendAll( ::SocketCon, "User-agent: " + ::cUserAgent + ::cCRLF )
   InetSendAll( ::SocketCon, "Connection: close" + ::cCRLF )

   // send cookies
   IF ! Empty( ::hCookies )
      InetSendAll( ::SocketCon, "Cookie: " )
      FOR iCount := 1 TO Len( ::hCookies ) - 1
         InetSendAll( ::SocketCon, HGetKeyAt( ::hCookies, iCount ) +;
            "=" + HGetValueAt( ::hCookies, iCount ) +"; ")
      NEXT
      iCount = Len( ::hCookies )
      InetSendAll( ::SocketCon, HGetKeyAt( ::hCookies, iCount ) +;
         "=" + HGetValueAt( ::hCookies, iCount ) + ::cCRLF)
   ENDIF

   //Send optional Fields
   FOR iCount := 1 TO Len( ::hFields )
      InetSendAll( ::SocketCon, HGetKeyAt( ::hFields, iCount ) +;
         ": " + HGetValueAt( ::hFields, iCount ) + ::cCRLF )
   NEXT

RETURN .T.



METHOD ReadHeaders() CLASS tIPClientHTTP
   LOCAL cLine, nPos, aVersion
   LOCAL aHead, aCookie, cCookie

   // Now reads the fields and set the content lenght
   cLine := InetRecvLine( ::SocketCon, @nPos, 500 )
   IF Empty( cLine )
      // In case of timeout or error on receiving
      RETURN .F.
   ENDIF

   // Get Protocol version
   aVersion := HB_Regex( "^HTTP/(.)\.(.) ([0-9][0-9][0-9]) +(.*)$", cLine )
   ::cReply := cLine

   IF aVersion == NIL
      ::nVersion := 0
      ::nSubversion := 9
      ::nReplyCode := 0
      ::cReplyDescr := ""
   ELSE
      ::nVersion := Val(aVersion[2])
      ::nSubversion := Val( aVersion[3] )
      ::nReplyCode := val( aVersion[4] )
      ::cReplyDescr := aVersion[5]
   ENDIF

   ::nLength := -1
   ::bChunked := .F.
   cLine := InetRecvLine( ::SocketCon, @nPos, 500 )

   DO WHILE InetErrorCode( ::SocketCon ) == 0 .and. .not. Empty( cLine )
      aHead := HB_RegexSplit( ":", cLine,,, 1 )
      IF aHead == NIL .or. Len( aHead ) != 2
         cLine := InetRecvLine( ::SocketCon, @nPos, 500 )
         LOOP
      ENDIF

      ::hHeaders[ aHead[1] ] := LTrim(aHead[2])

      DO CASE

         CASE lower( aHead[1] ) == "content-length:"
            cLine := Substr( cLine, 16 )
            ::nLength := Val( cLine )

         CASE lower( aHead[1] ) == "transfer-encoding:"
            IF At( "chunked", lower( cLine ) ) > 0
               ::bChunked := .T.
            ENDIF

         CASE lower( aHead[1] ) == "set-cookie:"
            aCookie := HB_RegexSplit( ";", aHead[2] )
            FOR EACH cCookie IN aCookie
               aCookie := HB_RegexSplit( "=", cCookie, 1)
               IF Len( aCookie ) == 2
                  ::hCookie[ aCookie[1] ] := aCookie[2]
               ENDIF
            NEXT

      ENDCASE
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
      IF .not. ::Get()
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
            ::nLength := HB_HexToNum( cLine ) + ::nRead
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
