/*
 * $Id: client.prg,v 1.5 2005/04/30 15:41:57 lculik Exp $
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

/* 2004-01-13
  Enhaced tip cliente to conenct to secure smtp servers by Luiz Rafael Culik
*/

#include "hbclass.ch"
#include "fileio.ch"
#include "tip.ch"
#include "common.ch"
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
   Data lTrace
   Data nHandle

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

   /** Gauge control; it can be a codeblock or a function pointer. */
   DATA exGauge

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
   /* New Methods */
   METHOD InetRecv( SocketCon, cStr1, len)
   METHOD InetRecvLine( SocketCon, nLen, size )
   METHOD InetRecvAll( SocketCon, cStr1, nLen )
   METHOD InetCount( SocketCon )
   METHOD InetSendAll( SocketCon,  cData , nLen )
   METHOD InetErrorCode(SocketCon)
   METHOD InetConnect( cServer, nPort, SocketCon )
ENDCLASS


METHOD New( oUrl, oCredentials,lTrace ) CLASS tIPClient
   LOCAL oRet
   Default lTrace to .F.
   IF .not. ::bInitSocks
      InetInit()
      ::bInitSocks := .T.
   ENDIF
   
   DO CASE
      CASE oUrl:cProto == "http"
         oRet := tIPClientHTTP():New(lTrace)
      CASE oUrl:cProto == "pop"
         oRet := tIPClientPOP():New(lTrace)
      CASE oUrl:cProto == "smtp"
         oRet := tIPClientSMTP():New(lTrace)
      CASE oUrl:cProto == "ftp"
         oRet := tIPClientFTP():New(lTrace)
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
   oRet:lTRace := lTRace
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
   ::InetConnect( ::oUrl:cServer, nPort, ::SocketCon )
   IF ::InetErrorCode( ::SocketCon ) != 0
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
   LOCAL cStr0, cStr1

   IF ::nLength > 0 .and. ::nLength == ::nRead
      RETURN NIL
   ENDIF

   IF Empty( nLen ) .or. nLen < 0 .or.( ::nLength > 0 .and. nLen > ::nLength - ::nRead )
      nLen := ::nLength - ::nRead
   ENDIF

   IF Empty( nLen ) .or. nLen < 0
      // read till end of stream
      cStr1 := Space( 1024 )
      cStr0 := ""
      ::nLastRead := ::InetRecv( ::SocketCon, @cStr1, 1024 )
      DO WHILE ::nLastRead > 0
         ::nRead += ::nLastRead
         cStr0 += Substr( cStr1, 1, ::nLastRead )
         ::nLastRead := ::InetRecv( ::SocketCon, @cStr1, 1024 )
      ENDDO
      ::bEof := .T.
   ELSE
      // read an amount of data
      cStr0 := Space( nLen )

// S.R. if len of file is less than 1024 InetRecvAll return 0
//      ::nLastRead := InetRecvAll( ::SocketCon, @cStr0, nLen )

      ::InetRecvAll( ::SocketCon, @cStr0, nLen )
      ::nLastRead := ::InetCount( ::SocketCon )
      ::nRead += ::nLastRead

      IF ::nLastRead != nLen
         ::bEof := .T.
         cStr0 := Substr( cStr0, 1, ::nLastRead )
// S.R.         RETURN NIL
      ENDIF

      IF ::nRead == ::nLength
         ::bEof := .T.
      ENDIF

   ENDIF
RETURN cStr0



METHOD ReadToFile( cFile, nMode ) CLASS tIPClient
   LOCAL nFout
   LOCAL cData

   IF Empty ( nMode )
      nMode := FO_CREAT
   ENDIF

   ::nStatus := 1
   DO WHILE ::InetErrorCode( ::SocketCon ) == 0 .and. .not. ::bEof
      cData := ::Read( 1024 )
      IF cData == NIL
         IF nFout != NIL
            Fclose( nFout )
         ENDIF
         IF ::InetErrorCode( ::SocketCon ) > 0
            RETURN .F.
         ELSE
            RETURN .T.
         ENDIF
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
   LOCAL nSize, nSent

   ::nStatus := 0
   nFin := Fopen( cFile, FO_READ )
   IF nFin < 0
      RETURN .F.
   ENDIF
   nSize := FSeek( nFin, 0, 2 )
   FSeek( nFin, 0 )


   // allow initialization of the gauge
   nSent := 0
   IF ! Empty( ::exGauge )
      HB_ExecFromArray( ::exGauge, {nSent, nSize, Self} )
   ENDIF

   ::nStatus := 1
   cData := Space( 1024 )
   nLen := Fread( nFin, @cData, 1024 )

   DO WHILE nLen > 0
      IF ::Write( @cData, nLen ) != nLen
         Fclose( nFin )
         RETURN .F.
      ENDIF
      nSent += nLen
      IF ! Empty( ::exGauge )
         HB_ExecFromArray( ::exGauge, {nSent, nSize, Self} )
      ENDIF
      nLen := Fread( nFin, @cData, 1024 )
   ENDDO

   // it may happen that the file has lenght 0
   IF nSent > 0
      ::Commit()
   ENDIF

   ::nStatus := 2
   Fclose( nFin )
RETURN .T.


METHOD Data( cData ) CLASS tIPClient
   ::InetSendall( ::SocketCon, "DATA" + ::cCRLF )
   IF .not. ::GetOk()
      RETURN .F.
   ENDIF
   ::InetSendall(::SocketCon, cData + ::cCRLF + "." + ::cCRLF )
RETURN ::GetOk()

METHOD Write( cData, nLen, bCommit ) CLASS tIPClient

   IF Empty( nLen )
      nLen := Len( cData )
   ENDIF

   ::nLastWrite := ::InetSendall( ::SocketCon,  cData , nLen )
   IF .not. Empty( bCommit ) .and. bCommit
      ::Commit()
   ENDIF

RETURN ::nLastWrite
METHOD InetSendAll( SocketCon, nLen, size ) CLASS tIPClient

Local nRet 

    if ::lTrace
       fWrite(::nHandle, [ Parameters recieved  on function InetSendAll  : SocketCon ] +cStr(SocketCon) + [  size  ] +cStr(size) + [  nLen  ] + cstr(nlen)  +hb_osnewline())
    endif

    nRet := InetSendAll( SocketCon, nLen, size )

    if ::lTrace
       fWrite(::nHandle, [ Data returned from the server on  InetSendAll: nRet  ] +cstr(nRet) +hb_osnewline())
    endif
Return nRet

METHOD InetCount( SocketCon ) CLASS tIPClient
Local nRet

    if ::lTrace
       fWrite(::nHandle, [ Parameters recieved  on function InetCount  : SocketCon ] +cStr(socketcon)+hb_osnewline() )
    endif

    nRet:= InetCount( SocketCon )

    if ::lTrace
       fWrite(::nHandle, [Data returned from the server on  InetCount : nRet  ] +cStr(nRet)+hb_osnewline())
    endif
Return nRet

METHOD InetRecv( SocketCon, cStr1, len) CLASS tIPClient
Local nRet

    if ::lTrace
       fWrite(::nHandle, [Parameters recieved on function InetRecv to recieve data :  : SocketCon ] +cStr(SocketCon)+ [ cStr1 ]+cStr1+[ len ]+cstr(len) +hb_osnewline())
    endif

    nRet:= InetRecv( SocketCon, @cStr1, len )

    if ::lTrace
       fWrite(::nHandle, [ Data returned from the server on  InetRecv:  nRet  ]+ cstr(nret) + [ cStr1 ] + if(valtype( cStr1) == "U","NIL",cstr1)+ hb_osnewline())
    endif
Return nRet


METHOD InetRecvLine( SocketCon, nLen, size ) CLASS tIPClient

Local cRet :=""

    if ::lTrace
       fWrite(::nHandle, [ Parameters recieved on function InetRecvLine to recieve data : SocketCon ] +cstr(SocketCon)+ [ nLen ]+cstr(nlen)+ [ size ]+cstr(size) +hb_osnewline() )
    endif

    cRet := InetRecvLine( SocketCon, @nLen, size )

    if ::lTrace
       fWrite(::nHandle, [ Data returned from the server on  InetRecv : cRet  ] + if(valtype( cRet) == "U","NIL",cRet)  +hb_osnewline())
    endif
Return cRet

METHOD InetRecvAll( SocketCon, cStr1, len) CLASS tIPClient
Local nRet

    if ::lTrace
       fWrite(::nHandle, [Parameters recieved on function InetRecvAll to recieve data:  Parametros recebidos : SocketCon ] +cStr(SocketCon) +  [  cStr1  ]+ cStr1 +[  len  ] +cstr(len) +hb_osnewline())
    endif

     InetRecvAll( SocketCon, @cStr1, len )

    if ::lTrace
       fWrite(::nHandle, [  Data returned from the server on  InetRecvAll : cStr1  ] +if(valtype( cStr1) == "U","NIL",cstr1) +hb_osnewline())
    endif
Return self

METHOD InetErrorCode(SocketCon)
Local nRet := 0

    if ::lTrace
       fWrite(::nHandle, [Parameters recieved on function InetErrorCode:  SocketCon ] +cStr(SocketCon) +hb_osnewline())
    endif

    nRet := InetErrorCode( ::SocketCon )

    if ::lTrace
       fWrite(::nHandle, [ Data returned from the server on  InetErrorCode:  nRet  ]+ cstr(nret) + hb_osnewline())
    endif

Return nRet

METHOD InetConnect( cServer, nPort, SocketCon )
    if ::lTrace
       fWrite(::nHandle, [Parameters recieved on function InetConnect:  Server ] +cServer + [ Port ] +cStr( nPort ) + [SocketCon ] +cStr(SocketCon) + hb_osnewline())
    endif

   InetConnect( cServer, nPort, SocketCon )
Return Nil
