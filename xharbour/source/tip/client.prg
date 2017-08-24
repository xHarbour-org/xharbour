/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbclass.ch"
#include "error.ch"
#include "fileio.ch"
#include "tip.ch"
#include "common.ch"

#define RCV_BUF_SIZE Int( ::InetRcvBufSize( ::SocketCon ) / 2 )
#define SND_BUF_SIZE Int( ::InetSndBufSize( ::SocketCon ) / 2 )

/**
* Inet Client class
* Abstract super class for TIPClientFTP, TIPClientHTTP, TIPClientPOP and TIPClientSMTP
*/

CLASS TIPClient

   CLASSDATA bInitSocks INIT .F.             //
   CLASSDATA cCRLF      INIT InetCRLF()      //

   DATA oUrl                                 // url to wich to connect
   DATA oCredentials                         // credential needed to access the service
   DATA nStatus                              // basic status
   DATA SocketCon                            //
   DATA lTrace    INIT .F.                   //
   DATA nHandle   INIT - 1                   //

   DATA nDefaultRcvBuffSize                  //
   DATA nDefaultSndBuffSize                  //

   DATA nLength                              // Input stream length
   DATA nRead                                // Input stream data read by the app
   DATA nLastRead                            // Last physical read amount

   DATA nDefaultPort                         //
   DATA nConnTimeout                         //
   DATA bInitialized                         //

   DATA cReply                               //
   DATA nAccessMode                          // defines TIP_*
   DATA nWrite                               //
   DATA nLastWrite                           // Last physical write amount

   DATA bEof                                 //
   DATA isOpen INIT .F.                      //

   DATA exGauge                              // Gauge control; it can be a codeblock or a function pointer

   DATA Cargo                                //
                                             /* Data For proxy connection */
   DATA cProxyHost                           //
   DATA nProxyPort                           //
   DATA cProxyUser                           //
   DATA cProxyPassword                       //

   METHOD New( oUrl, lTrace, oCredentials )
   METHOD Open( cUrl )

   METHOD READ( nLen )
   METHOD ReadToFile( cFile, nMode, nSize )
   METHOD Write( cData, iLen, bCommit )
   METHOD COMMIT()
   METHOD WriteFromFile( cFile )
   METHOD Reset()
   METHOD CLOSE()

   METHOD SetProxy( cProxyHost, nProxyPort, cProxyUser, cProxyPassword )
   METHOD OpenProxy( cServer, nPort, cProxy, nProxyPort, cResp, cUserName, cPassWord, nTimeOut, cUserAgent )
   METHOD ReadHTTPProxyResponse( dwTimeout, sResponse )

   METHOD lastErrorCode() INLINE ::nLastError
   METHOD lastErrorMessage( SocketCon ) INLINE ::InetErrorDesc( SocketCon )

   METHOD InetRcvBufSize( SocketCon, nSizeBuff )
   METHOD InetSndBufSize( SocketCon, nSizeBuff )

   PROTECTED:
   DATA nLastError INIT 0

   /* Methods to log data if needed */
   METHOD InetRecv( SocketCon, cStr1, len )
   METHOD InetRecvLine( SocketCon, nLen, size )
   METHOD InetRecvAll( SocketCon, cStr1, len )
   METHOD InetCount( SocketCon )
   METHOD InetSendAll( SocketCon, cData, nLen )
   METHOD InetErrorCode( SocketCon )
   METHOD InetErrorDesc( SocketCon )
   METHOD InetConnect( cServer, nPort, SocketCon )

   METHOD Log( )

ENDCLASS

METHOD New( oUrl, lTrace, oCredentials ) CLASS TIPClient

   LOCAL oErr

   DEFAULT lTrace TO .F.

   ::lTrace := lTrace

   IF ! ::bInitSocks
      InetInit()
      ::bInitSocks := .T.
   ENDIF

   IF HB_ISSTRING( oUrl )
      oUrl := tUrl():New( oUrl )
   ENDIF

   IF ! oUrl:cProto IN "ftp,http,pop,smtp"
      oErr := ErrorNew()
      oErr:Args          := { Self, oUrl:cProto }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "unsupported protocol"
      oErr:GenCode       := EG_UNSUPPORTED
      oErr:Operation     := ::className() + ":New()"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1081
      oErr:SubSystem     := "BASE"
      Eval( ErrorBlock(), oErr )
   ENDIF

   ::oUrl         := oUrl
   ::oCredentials := oCredentials
   ::nStatus      := 0
   ::bInitialized := .F.
   ::nWrite       := 0
   ::nLastWrite   := 0
   ::nLength      := - 1
   ::nRead        := 0
   ::nLastRead    := 0
   ::bEof         := .F.

   RETURN self

METHOD Open( cUrl ) CLASS TIPClient

   LOCAL nPort
   LOCAL cResp := ""

   IF HB_ISSTRING( cUrl )
      ::oUrl := tUrl():New( cUrl )
   ENDIF

   IF ::oUrl:nPort == - 1
      nPort := ::nDefaultPort
   ELSE
      nPort := ::oUrl:nPort
   ENDIF
   ::SocketCon := InetCreate()
   InetSetTimeout( ::SocketCon, ::nConnTimeout )
   IF ! Empty( ::cProxyHost )
      IF ! ::OpenProxy( ::oUrl:cServer, nPort, ::cProxyHost, ::nProxyPort, @cResp, ::cProxyUser, ::cProxyPassword, ::nConnTimeout, '' )
         RETURN .F.
      ENDIF
   ELSE
      ::InetConnect( ::oUrl:cServer, nPort, ::SocketCon )

      IF ::InetErrorCode( ::SocketCon ) != 0
         RETURN .F.
      ENDIF
   ENDIF
   ::isOpen := .T.

   RETURN .T.

METHOD CLOSE() CLASS TIPClient

   LOCAL nRet := - 1

   IF ! Empty( ::SocketCon )

      nRet := InetClose( ::SocketCon )

      ::SocketCon := NIL
      ::isOpen    := .F.
   ENDIF

   RETURN( nRet )

METHOD Reset() CLASS TIPClient

   ::bInitialized := .F.
   ::bEof         := .F.

   RETURN .T.

METHOD COMMIT() CLASS TIPClient

   RETURN .T.

METHOD READ( nLen ) CLASS TIPClient

   LOCAL cStr0, cStr1

   IF ::nLength > 0 .AND. ::nLength == ::nRead
      RETURN NIL
   ENDIF

   IF Empty( nLen ) .OR. nLen < 0 .OR. ( ::nLength > 0 .AND. nLen > ::nLength - ::nRead )
      nLen := ::nLength - ::nRead
   ENDIF

   IF ::nLength > RCV_BUF_SIZE .AND. !::bChunked  //Empty( nLen ) .OR. nLen < 0
      // read till end of stream
      cStr1 := Space( RCV_BUF_SIZE )
      cStr0 := ""
      ::nLastRead := ::InetRecv( ::SocketCon, @cStr1, RCV_BUF_SIZE )
      DO WHILE ::nLastRead > 0
         ::nRead += ::nLastRead
         cStr0   += SubStr( cStr1, 1, ::nLastRead )
         ::nLastRead := ::InetRecv( ::SocketCon, @cStr1, RCV_BUF_SIZE )
      ENDDO
      ::bEof := .T.
   ELSE
      // read an amount of data
     if Empty( nLen ) .or. nLen < 0
        nLen := RCV_BUF_SIZE
     endif   

      cStr0 := Space( nLen )

      // if len of file is less than RCV_BUF_SIZE InetRecvAll return 0
      // ::nLastRead := InetRecvAll( ::SocketCon, @cStr0, nLen )

      ::InetRecvAll( ::SocketCon, @cStr0, nLen )
      ::nLastRead := ::InetCount( ::SocketCon )
      ::nRead     += ::nLastRead

      IF ::nLastRead != nLen
         ::bEof := .T.
         cStr0 := SubStr( cStr0, 1, ::nLastRead )
         // RETURN NIL
      ENDIF

      IF ::nRead == ::nLength
         ::bEof := .T.
      ENDIF

   ENDIF

   RETURN cStr0

METHOD ReadToFile( cFile, nMode, nSize ) CLASS TIPClient

   LOCAL nFout
   LOCAL cData
   LOCAL nSent

   IF Empty ( nMode )
      nMode := FC_NORMAL
   ENDIF

   nSent := 0

   IF ! Empty( ::exGauge )
      hb_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
   ENDIF

   ::nRead   := 0
   ::nStatus := 1

   DO WHILE ::InetErrorCode( ::SocketCon ) == 0 .AND. ! ::bEof
      cData := ::READ( RCV_BUF_SIZE )
      IF cData == NIL
         IF nFout != NIL
            FClose( nFout )
         ENDIF
         IF ::InetErrorCode( ::SocketCon ) > 0
            RETURN .F.
         ELSE
            RETURN .T.
         ENDIF
      ENDIF
      IF nFout == NIL
         nFout := FCreate( cFile, nMode )
         IF nFout < 0
            ::nStatus := 0
            RETURN .F.
         ENDIF
      ENDIF

      IF FWrite( nFout, cData ) < 0
         FClose( nFout )
         RETURN .F.
      ENDIF

      nSent += Len( cData )
      IF ! Empty( ::exGauge )
         hb_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
      ENDIF

   ENDDO

   IF nSent > 0
      ::COMMIT()
   ENDIF

   ::nStatus := 2
   FClose( nFout )

   RETURN .T.

METHOD WriteFromFile( cFile ) CLASS TIPClient

   LOCAL nFin
   LOCAL cData
   LOCAL nLen
   LOCAL nSize, nSent, nBufSize

   ::nWrite  := 0
   ::nStatus := 0
   nFin := FOpen( cFile, FO_READ )
   IF nFin < 0
      RETURN .F.
   ENDIF
   nSize := FSeek( nFin, 0, 2 )
   FSeek( nFin, 0 )

   nBufSize := SND_BUF_SIZE

   // allow initialization of the gauge
   nSent := 0
   IF ! Empty( ::exGauge )
      hb_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
   ENDIF

   ::nStatus := 1
   cData := Space( nBufSize )
   nLen  := FRead( nFin, @cData, nBufSize )
   DO WHILE nLen > 0
      IF ::Write( @cData, nLen ) != nLen
         FClose( nFin )
         RETURN .F.
      ENDIF
      nSent += nLen
      IF ! Empty( ::exGauge )
         hb_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
      ENDIF
      nLen := FRead( nFin, @cData, nBufSize )
   ENDDO

   // it may happen that the file has lenght 0
   IF nSent > 0
      ::COMMIT()
   ENDIF

   ::nStatus := 2
   FClose( nFin )

   RETURN .T.

METHOD Write( cData, nLen, bCommit ) CLASS TIPClient

   IF Empty( nLen )
      nLen := Len( cData )
   ENDIF

   ::nLastWrite := ::InetSendAll( ::SocketCon, cData, nLen )

   IF ! Empty( bCommit )
      ::COMMIT()
   ENDIF

   ::nWrite += ::nLastWrite

   RETURN ::nLastWrite

METHOD InetSendAll( SocketCon, cData, nLen ) CLASS TIPClient

   LOCAL nRet

   IF Empty( nLen )
      nLen := Len( cData )
   ENDIF

   nRet := InetSendAll( SocketCon, cData, nLen )

   IF ::lTrace
      ::Log( SocketCon, nlen, cData, nRet )
   ENDIF

   RETURN nRet

METHOD InetCount( SocketCon ) CLASS TIPClient

   LOCAL nRet

   nRet := InetCount( SocketCon )

   IF ::lTrace
      ::Log( SocketCon, nRet )
   ENDIF

   RETURN nRet

METHOD InetRecv( SocketCon, cStr1, len ) CLASS TIPClient

   LOCAL nRet

   nRet := InetRecv( SocketCon, @cStr1, len )

   IF ::lTrace
      ::Log( SocketCon, "", len, iif( nRet >= 0, cStr1, nRet ) )
   ENDIF

   RETURN nRet

METHOD InetRecvLine( SocketCon, nLen, size ) CLASS TIPClient

   LOCAL cRet

   cRet := InetRecvLine( SocketCon, @nLen, size )

   IF ::lTrace
      ::Log( SocketCon, "", size, cRet )
   ENDIF

   RETURN cRet

METHOD InetRecvAll( SocketCon, cStr1, len ) CLASS TIPClient

   LOCAL nRet

   nRet := InetRecvAll( SocketCon, @cStr1, len )

   IF ::lTrace
      ::Log( SocketCon, "", len, iif( nRet >= 0, cStr1, nRet ) )
   ENDIF

   RETURN nRet

METHOD InetErrorCode( SocketCon ) CLASS TIPClient

   LOCAL nRet

   ::nLastError := nRet := InetErrorCode( SocketCon )

   IF ::lTrace
      ::Log( SocketCon, nRet )
   ENDIF

   RETURN nRet

METHOD InetErrorDesc( SocketCon ) CLASS TIPClient

   LOCAL cMsg := ""

   DEFAULT SocketCon TO ::SocketCon

   IF ! Empty( SocketCon )

      cMsg := InetErrorDesc( SocketCon )

   ENDIF

   RETURN cMsg

   /* BROKEN, should test number of parameters and act accordingly, see doc\inet.txt */

METHOD InetConnect( cServer, nPort, SocketCon ) CLASS TIPClient

   InetConnect( cServer, nPort, SocketCon )

   IF ! Empty( ::nDefaultSndBuffSize )
      ::InetSndBufSize( SocketCon, ::nDefaultSndBuffSize )
   ENDIF
   
   IF ! Empty( ::nDefaultRcvBuffSize )
      ::InetRcvBufSize( SocketCon, ::nDefaultRcvBuffSize )
   ENDIF

   IF ::lTrace
      ::Log( cServer, nPort, SocketCon )
   ENDIF

   RETURN Nil

   /* Methods to manage buffers */

METHOD InetRcvBufSize( SocketCon, nSizeBuff ) CLASS TIPClient

   IF ! Empty( nSizeBuff )
      InetSetRcvBufSize( SocketCon, nSizeBuff )
   ENDIF

   RETURN InetGetRcvBufSize( SocketCon )

METHOD InetSndBufSize( SocketCon, nSizeBuff ) CLASS TIPClient

   IF ! Empty( nSizeBuff )
      InetSetSndBufSize( SocketCon, nSizeBuff )
   ENDIF

   RETURN InetGetSndBufSize( SocketCon )

/* Called from another method with list of parameters and, as last parameter, return code
   of function being logged.
   Example, I want to log MyFunc( a, b, c ) which returns m,
            ::Log( a, b, c, m )
*/

METHOD Log( ... ) CLASS TIPClient

   LOCAL xVar
   LOCAL cMsg := DToS( Date() ) + "-" + Time() + Space( 2 ) + ;
                 SubStr( ProcName( 1 ), RAt( ":", ProcName( 1 ) ) ) + "( "

   FOR EACH xVar in hb_AParams()

      // Preserves CRLF on result
      IF hb_EnumIndex() < PCount()
         cMsg += StrTran( StrTran( AllTrim( CStr( xVar ) ), Chr( 13 ) ), Chr( 10 ) )
      ELSE
         cMsg += CStr( xVar )
      ENDIF

      cMsg += iif ( hb_EnumIndex() < PCount() - 1, ", ", "" )

      IF hb_EnumIndex() == PCount() - 1
         cMsg += " )" + hb_osNewLine() + ">> "

      ELSEIF hb_EnumIndex() == PCount()
         cMsg += " <<" + hb_osNewLine() + hb_osNewLine()

      ENDIF

   NEXT

   FWrite( ::nHandle, cMsg )

   RETURN Self

METHOD SetProxy( cProxyHost, nProxyPort, cProxyUser, cProxyPassword ) CLASS TIPClient

   ::cProxyHost     := cProxyHost
   ::nProxyPort     := nProxyPort
   ::cProxyUser     := cProxyUser
   ::cProxyPassword := cProxyPassword

   RETURN Self

METHOD OpenProxy( cServer, nPort, cProxy, nProxyPort, cResp, cUserName, cPassWord, nTimeOut, cUserAgent ) ;
       CLASS TIPClient

   LOCAL cLine
   LOCAL cRequest := ""
   LOCAL cPass
   LOCAL cEncoded
   LOCAL lRet := .T.
   LOCAL nResponseCode
   LOCAL sResponseCode, nFirstSpace

   ::InetConnect( cProxy, nProxyPort, ::SocketCon )
   IF ::InetErrorCode( ::SocketCon ) == 0
      Try
         cLine    := sprintf( 'CONNECT %s:%d HTTP/1', cServer, nPort ) + Chr( 13 ) + Chr( 10 )
         cRequest += cLine
         IF ! Empty( cUserName )
            cPass    := sprintf( '%s:%s', cUserName, cPassWord )
            cEncoded := hb_base64( cPass, Len( cPass ) )
            cLine    := sprintf( "Proxy-authorization: Basic %s", cEncoded ) + Chr( 13 ) + Chr( 10 )
            cRequest += cLine
         ENDIF
         IF ! Empty( cUserAgent )
            cLine    := sprintf( "User-Agent: %s", cUserAgent ) + Chr( 13 ) + Chr( 10 )
            cRequest += cLine
         ENDIF
         cRequest += Chr( 13 ) + Chr( 10 )
         ::InetSendAll( ::SocketCon, cRequest )
         cResp := ''
         ::ReadHTTPProxyResponse( nTimeOut, @cResp )
         nFirstSpace := At( " ", cResp )
         IF ( nFirstSpace != 0 )
            sResponseCode := Right( cResp, Len( cResp ) - nFirstSpace )
            nResponseCode := Val( sResponseCode )
            IF ( nResponseCode != 200 )
               Throw( ErrorNew( "INETCONNECTPROXY", 0, 4000, ProcName(), "Connection refused" ) )
            ENDIF
         ENDIF
      catch
         ::CLOSE()
         lRet := .F.
      END
   
   ENDIF

   RETURN lRet

METHOD ReadHTTPProxyResponse( dwTimeout, sResponse ) CLASS TIPClient

   LOCAL bMoreDataToRead := .T.
   LOCAL nLength, nData
   LOCAL szResponse

   HB_SYMBOL_UNUSED( dwTimeout )

   WHILE bMoreDataToRead
  
      szResponse := Space( 1 )
      nData := InetRecv( ::SocketCon, @szResponse, 1 )
      IF ( nData == 0 )
         throw( ErrorNew( "INETCONNECTPROXY", 0, 4000, ProcName(), "Disconnected" ) )
      ENDIF
      sResponse += szResponse

      nLength := Len( sResponse )
      IF nLength >= 4
         bMoreDataToRead := ! ( ( SubStr( sResponse, nLength - 3, 1 ) == Chr( 13 ) ) .AND. ;
                                ( SubStr( sResponse, nLength - 2, 1 ) == Chr( 10 ) ) .AND. ;
                                ( SubStr( sResponse, nLength - 1, 1 ) == Chr( 13 ) ) .AND. ;
                                ( SubStr( sResponse, nLength    , 1 ) == Chr( 10 ) ) )
      ENDIF
   ENDDO

   RETURN NIL
