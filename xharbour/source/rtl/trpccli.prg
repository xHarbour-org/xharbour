/*
 * $Id: trpccli.prg,v 1.1 2003/02/16 00:00:17 jonnymind Exp $
 */

/*
 * xHarbour Project source code:
 * Remote Procedure Call code
 * Client class
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
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

CLASS tRPCClient
   CLASSDATA lInit INIT InetInit()

   DATA nStatus
   DATA cServer

   DATA cNetwork
   DATA nUdpPort
   DATA nTcpPort

   DATA skUdp
   DATA skTcp

   DATA aServers
   DATA aFunctions
   DATA oResult

   /* asyncrhonous mode */
   DATA lAsyncMode
   /* block to be called at scan completion */
   DATA bOnScanComplete
   /*block called when there is a progress in the scan */
   DATA bOnScanServersProgress
   DATA bOnScanFunctionsProgress

   /* block to be called at function error */
   DATA bOnFunctionProgress
   /* block to be called at function success */
   DATA bOnFunctionReturn
   /* block to be called at function failure */
   DATA bOnFunctionFail

   DATA nUdpTimeout   INIT 0
   DATA nUdpTimeBegin INIT 0
   DATA thUdpAccept   INIT -1

   DATA nTcpTimeout   INIT 0
   DATA nTcpTimeBegin INIT 0
   DATA thTcpAccept   INIT -1

   DATA mtxBusy INIT CreateMutex()
   DATA oResult

   METHOD New( cNetwork )
   METHOD ScanServers( cName, nTime )
   METHOD ScanFunctions( cName, cSerial, nTime )
   METHOD StartScan()
   METHOD StopScan()

   METHOD UDPAccept()
   METHOD UDPParse( cData, nLen )

   METHOD TCPAccept()
   METHOD TCPParse( cData )

   METHOD Connect( cServer, cUserId, cPassword, nTimeout )

   METHOD Call( cFucntion, aParams, nTimeout )

   METHOD Disconnect()
   METHOD Destroy()
   
   /* Utility functions */
   METHOD GetFunctionName( oId )

   /* event handlers */
   METHOD OnScanComplete()
   METHOD OnScanServersProgress( aLoc )
   METHOD OnScanFunctionsProgress( aLoc )

   METHOD OnFunctionFail( nReason, cReason )
   METHOD OnFunctionReturn( oReturn )
   METHOD OnFunctionProgress( nProgress, oData )
   METHOD SendCall(cFunction,aParams )

ENDCLASS


METHOD New( cNetwork ) CLASS tRPCClient
   ::nStatus := 0 // not connected
   ::cServer := NIL // no server

   ::nUdpPort := 1139
   ::nTcpPort := 1140

   ::skTcp := InetCreate()
   ::skUdp := InetDGram( .T. )
   ::lAsyncMode := .F.
   ::aServers := {}
   ::aFunctions := {}
   ::cNetwork := cNetwork

RETURN Self


METHOD Destroy() CLASS tRPCClient

   MutexLock( ::mtxBusy )
      InetDestroy( ::skTcp )
      InetDestroy( ::skUdp )
      IF ::thUdpAccept > 0
         StopThread( ::thUdpAccept )
      ENDIF
      IF ::thTcpAccept > 0
         StopThread( ::thTcpAccept )
      ENDIF
   MutexUnlock( ::mtxBusy )

   DestroyMutex( ::mtxBusy )

RETURN .T.


METHOD ScanServers(cName, nTime ) CLASS tRPCClient
   // do not allow asynchronous mode without timeout
   IF .not. ::lAsyncMode .and. ( nTime == NIL .or. nTime <= 0 )
      RETURN .F.
   ENDIF

   MutexLock( ::mtxBusy )
   ::aServers = {}
   MutexUnlock( ::mtxBusy )

   InetDGramSend( ::skUDP, ::cNetwork , ::nUdpPort, "XHBR00" + HB_Serialize( cName ) )
   ::StartScan( nTime )

RETURN .F.


METHOD ScanFunctions(cFunc, cSerial, nTime ) CLASS tRPCClient
   // do not allow asynchronous mode without timeout
   IF .not. ::lAsyncMode .and. ( nTime == NIL .or. nTime <= 0 )
      RETURN .F.
   ENDIF

   MutexLock( ::mtxBusy )
   ::aFunctions = {}
   MutexUnlock( ::mtxBusy )

   InetDGramSend( ::skUDP, ::cNetwork, ::nUdpPort,;
         "XHBR01" + HB_Serialize( cFunc ) + HB_Serialize( cSerial ))
   ::StartScan( nTime )

RETURN .F.


METHOD StartScan( nTime )
   // We don't accept sync call without timeout

   IF ::lAsyncMode
      // in async mode, force scanning stop
      ::StopScan()
   ENDIF

   // Now let's reset timeout counters
   IF nTime != NIL
      ::nUDPTimeout := nTime
   ELSE
      ::nUDPTimeout := -1
   ENDIF

   ::nUDPTimeBegin := INT( Seconds() * 1000 )

   // in async mode, just launch the listener
   IF ::lAsyncMode
      MutexLock( ::mtxBusy )
         ::thUdpAccept := StartThread( Self, "UDPAccept" )
      MutexUnlock( ::mtxBusy )
   ELSE
      ::UDPAccept()
   ENDIF

RETURN .T.


METHOD UDPAccept() CLASS tRPCClient
   LOCAL nTime, nDatalen, cData

   cData := Space( 1400 )
   // set default socket timeout
   IF ::nUDPTimeout >= 0
      InetSetTimeout( ::skUDP, ::nUDPTimeout )
   ELSE
      InetClearTimeout( ::skUdp )
   ENDIF

   DO WHILE .T.
      nDatalen := InetDGramRecv( ::skUDP, @cData, 1400 )

      MutexLock( ::mtxBusy )
      IF .not. ::UDPParse( cData, nDatalen )
         MutexUnlock( ::mtxBusy )
         EXIT
      ENDIF
      MutexUnlock( ::mtxBusy )

      IF ::nUDPTimeout >= 0
         nTime := Int( Seconds() * 1000 )
         // a little tollerance must be added for double roundings
         // in the double INT() functions
         IF nTime - ::nUDPTimeBegin >= ::nUdpTimeout - 5
            EXIT
         ENDIF
      ENDIF

   ENDDO

   ::OnScanComplete()
RETURN .T.


METHOD UDPParse( cData, nLen ) CLASS tRPCClient
   LOCAL cCode, cSer, cFunc, cName
   LOCAL aLoc

   IF nLen < 12
      RETURN .F.
   ENDIF

   cCode := Substr( cData, 1, 6 )

   DO CASE
      /* XHRB00 - server scan */
      CASE cCode == "XHBR10"
         cData := Substr( cData, 7 )
         aLoc := { InetAddress( ::skUDP ),  HB_Deserialize( cData ) }
         AAdd( ::aServers, aLoc )
         RETURN ::OnScanServersProgress( aLoc )


      CASE cCode == "XHBR11"
         cData := Substr( cData, 7 )
         cSer := HB_DeserialBegin( cData )
         cName := HB_DeserialNext( cSer )
         cFunc := HB_DeserialNext( cSer )
         aLoc := { InetAddress( ::skUDP ), cName, cFunc }
         AAdd( ::aFunctions, aLoc )
         RETURN ::OnScanFunctionsProgress( aLoc )

   ENDCASE

RETURN .T.


METHOD StopScan() CLASS tRPCClient
   MutexLock( ::mtxBusy )
   IF ::thUDPAccept > 0
      StopThread( ::thUDPAccept )
      ::thUDPAccept := -1
      MutexUnlock( ::mtxBusy )
      ::OnScanComplete()
   ELSE
      MutexUnlock( ::mtxBusy )
   ENDIF
RETURN .T.


METHOD Connect( cServer, cUserId, cPassword, nTimeout ) CLASS tRPCClient
   LOCAL cAuth, cReply := Space(8)

   ::nStatus := 1 // connecting
   IF .not. Empty( nTimeout )
      ::skTcp := InetCreate( nTimeout )
   ELSE
      ::skTcp := InetCreate( )
   ENDIF

   InetConnect( cServer, ::nTcpPort, ::skTcp  )

   IF InetErrorCode( ::skTcp ) == 0
      ::nStatus := 2 // Connected
      cAuth := cUserId + ":" + cPassword
      InetSendAll( ::skTcp, "XHBR90" + HB_CreateLen8( Len( cAuth ) ) + cAuth )
      IF InetErrorCode( ::skTcp ) == 0
         InetRecvAll( ::skTcp, @cReply )
         IF InetErrorCode( ::skTcp ) == 0 .and. cReply == "XHBR91OK"
            ::nStatus := 3 // Logged in
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF

   InetDestroy( ::skTcp )
   ::skTcp := NIL
   ::nStatus := 0
RETURN .F.


METHOD Disconnect() CLASS tRPCClient

   IF ::nStatus >= 3
      MutexLock( ::mtxBusy )
      ::nStatus :=  0
      InetSendAll( ::skTcp, "XHBR92" )
      InetDestroy( ::skTcp )
      MutexUnlock( ::mtxBusy )
      RETURN .T.
   ENDIF

RETURN .F.


METHOD Call( cFunction, aParams, nTime ) CLASS tRPCClient
   LOCAL oRet
   // do not allow asynchronous mode without timeout
   IF .not. ::lAsyncMode .and. ( nTime == NIL .or. nTime <= 0 )
      RETURN .F.
   ENDIF

   MutexLock( ::mtxBusy )
   ::oResult := NIL
   ::nStatus := 4 // waiting for a reply
   // send the call through the socket
   ::SendCall( cFunction, aParams )

   IF .not. Empty( nTime ) .and. nTime >= 0
      ::nTCPTimeout := nTime
      InetSetTimeout( ::skTCP, ::nTCPTimeout )
   ELSE
      ::nTCPTimeout := -1
      InetClearTimeout( ::skUdp )
   ENDIF

   // in async mode, just launch the listener
   IF ::lAsyncMode
      ::thTCPAccept := StartThread( Self, "TCPAccept" )
      MutexUnlock( ::mtxBusy )
   ELSE
      MutexUnlock( ::mtxBusy )
      ::TCPAccept()
      MutexLock( ::mtxBusy )
      // Has something bad happened in the meanwhile?
      IF ::nStatus > 3
         oRet := ::oResult
         ::nStatus := 3 // reply complete
      ENDIF
      MutexUnlock( ::mtxBusy )
   ENDIF
RETURN oRet

METHOD SendCall( cFunction, aParams ) CLASS tRPCClient
   LOCAL cData, nLen

   cData := HB_Serialize( cFunction ) + HB_Serialize( aParams )
   nLen := Len( cData )
   IF nLen > 512
      cData := HB_Compress( cData )
      InetSendAll( ::skTCP, "XHBR21" + HB_CreateLen8( nLen ) + HB_CreateLen8( Len( cData ) ) + cData )
   ELSE
      InetSendAll( ::skTCP, "XHBR20" + HB_CreateLen8( nLen ) + cData )
   ENDIF
RETURN .T.



METHOD TCPAccept() CLASS tRPCClient
   LOCAL nTime
   LOCAL cCode

   cCode := Space(6)
   ::nTCPTimeBegin := INT( Seconds() * 1000 )

   DO WHILE .T.
      IF InetRecvAll( ::skTCP, @cCode, 6 ) < 0
         EXIT
      ENDIF

      IF .not. ::TCPParse( cCode )
         EXIT
      ENDIF

      IF ::nTCPTimeout >= 0
         nTime := Int( Seconds() * 1000 )
         // a little tollerance must be added for double roundings
         // in the double INT() functions
         IF nTime - ::nTCPTimeBegin >= ::nTCPTimeout - 5
            EXIT
         ENDIF
      ENDIF
   ENDDO

   IF InetErrorCode( ::skTCP ) != 0
      ::nStatus := 0
      InetDestroy( ::skTCP )
      ::skTCP := NIL
      MutexUnlock( ::mtxBusy )
   ENDIF

RETURN .T.


METHOD TCPParse( cCode ) CLASS tRPCClient
   LOCAL nDataLen, cData, nOrigLen
   LOCAL cDataLen := Space( 8 ), cOrigLen := Space( 8 )
   LOCAL cProgress := Space( 10 ), nProgress
   LOCAL lContinue := .F.

   DO CASE
      /* Warn error codes */
      CASE cCode == "XHBR40"
         cData := Space(2)
         InetRecvAll( ::skTCP, @cData, 2 )
         ::OnFunctionFail( Val( cData ), "No description for now" )

      /* We have a reply */
      CASE cCode == "XHBR30"
         IF InetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
            nDataLen := HB_GetLen8( cDataLen )
            cData := Space( nDataLen )
            IF InetRecvAll( ::skTCP, @cData, nDataLen ) == nDataLen
               ::oResult := HB_Deserialize( cData )
               ::OnFunctionReturn( ::oResult )
            ENDIF
         ENDIF

      /* We have a reply */
      CASE cCode == "XHBR31"
         IF InetRecvAll( ::skTCP, @cOrigLen ) == Len( cOrigLen )
            nOrigLen = HB_GetLen8( cOrigLen )
            IF InetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
               nDataLen := HB_GetLen8( cDataLen )
               cData := Space( nDataLen )
               IF InetRecvAll( ::skTCP, @cData ) == nDataLen
                  cData := HB_Uncompress( nOrigLen, cData )
                  IF .not. Empty( cData )
                     ::oResult := HB_Deserialize( cData )
                     ::OnFunctionReturn( ::oResult )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

      /* We have a progress */
      CASE cCode == "XHBR33"
         IF InetRecvAll( ::skTCP, @cProgress ) == Len( cProgress )
            nProgress := HB_Deserialize( cProgress )
            lContinue := .T.
            ::OnFunctionProgress( nProgress )
         ENDIF

      /* We have a progress with data*/
      CASE cCode == "XHBR34"
         IF InetRecvAll( ::skTCP, @cProgress ) == Len( cProgress )
            nProgress := HB_Deserialize( cProgress )
            IF InetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
               nDataLen := HB_GetLen8( cDataLen )
               cData := Space( nDataLen )
               IF InetRecvAll( ::skTCP, @cData ) == nDataLen
                  lContinue := .T.
                  ::OnFunctionProgress( nProgress, HB_Deserialize(cData) )
               ENDIF
            ENDIF
         ENDIF

      /* We have a progress with compressed data*/
      CASE cCode == "XHBR35"
         IF InetRecvAll( ::skTCP, @cProgress ) == Len( cProgress )
            nProgress := HB_Deserialize( cProgress )
            IF InetRecvAll( ::skTCP, @cOrigLen ) == Len( cOrigLen )
               nOrigLen = HB_GetLen8( cOrigLen )
               IF InetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
                  nDataLen := HB_GetLen8( cDataLen )
                  cData := Space( nDataLen )
                  IF InetRecvAll( ::skTCP, @cData ) == nDataLen
                     cData := HB_Uncompress( nOrigLen, cData )
                     IF .not. Empty( cData )
                        lContinue := .T.
                        ::OnFunctionProgress( nProgress, HB_Deserialize(cData) )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
   ENDCASE

RETURN lContinue

/***********************************
* Utility functions
************************************/
METHOD GetFunctionName( oData ) CLASS tRpcClient
   LOCAL cData, nPos

   IF ValType( oData ) == "N"
      cData := ::aFunctions[oData][3]
   ELSE
      cData := oData
   ENDIF

   nPos := At( "[", cData )
   cData := Substr( cData, 1, nPos-1 )
RETURN cData


/***********************************
* Event handlers
************************************/

METHOD OnScanComplete() CLASS tRPCClient
   IF ::bOnScanComplete != NIL
      RETURN Eval( ::bOnScanComplete )
   ENDIF
RETURN .T.

METHOD OnScanServersProgress( aLoc ) CLASS tRPCClient
   IF ::bOnScanServersProgress != NIL
      RETURN Eval( ::bOnScanServersProgress, aLoc )
   ENDIF
RETURN .T.

METHOD OnScanFunctionsProgress( aLoc ) CLASS tRPCClient
   IF ::bOnScanFunctionsProgress != NIL
      RETURN Eval( ::bOnScanFunctionsProgress, aLoc )
   ENDIF
RETURN .T.

METHOD OnFunctionFail( nReason, cReason ) CLASS tRPCClient
   IF ::bOnFunctionFail != NIL
      RETURN Eval( ::bOnFunctionFail, nReason, cReason )
   ENDIF
RETURN .T.

METHOD OnFunctionReturn( oReturn ) CLASS tRPCClient
   IF ::bOnFunctionReturn != NIL
      RETURN Eval( ::bOnFunctionReturn, oReturn )
   ENDIF
RETURN .T.

METHOD OnFunctionProgress( nProgress, oData ) CLASS tRPCClient
   IF ::bOnFunctionReturn != NIL
      RETURN Eval( ::bOnFunctionProgress, nProgress, oData )
   ENDIF
RETURN .T.

