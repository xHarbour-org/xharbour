/*
 * $Id$
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
#include "hbrpc.ch"

CLASS tRPCClient

   DATA aServers
   DATA aFunctions

   DATA nUdpPort
   DATA nTcpPort

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


   METHOD New( cNetwork, nTcpPort, nUdpPort ) CONSTRUCTOR
   METHOD Destroy()

   /* Connection */
   METHOD Connect( cServer, cUserId, cPassword )
   METHOD Disconnect()

   /* Network scan functions */
   METHOD ScanServers( cName )
   METHOD ScanFunctions( cName, cSerial )
   METHOD ScanAgain()            INLINE ::UDPAccept()
   METHOD StopScan()

   /* Function call */
   METHOD CheckServer()    //Checks if a server is ready on tcp
   METHOD SetLoopMode( nMethod, xData, nEnd, nStep )
   METHOD Call()  // variable parameters
   METHOD CallAgain()            INLINE ::TCPAccept()
   METHOD StopCall()

   METHOD SetPeriodCallback()
   METHOD ClearPeriodCallback()


   /* Accessors */
   METHOD SetEncryption( cKey )
   METHOD IsEncrypted()             INLINE ::cCryptKey != NIL
   METHOD GetStatus()               INLINE ::nStatus
   METHOD SetTimeout( nTime )
   METHOD GetTimeout()
   METHOD GetResult()               INLINE ::oResult
   METHOD FoundServers()            INLINE Len( ::aServers ) != 0
   METHOD FoundFunctions()          INLINE Len( ::aFunctions ) != 0

   METHOD HasError()                INLINE ::nErrorCode != 0 .OR. ::TcpHasError() .OR. ::UdpHasError()
   METHOD GetErrorCode()            INLINE ::nErrorCode

   METHOD TcpHasError()             INLINE iif( Empty( ::skTCP ), .F. , InetErrorCode( ::skTCP ) > 0 )
   METHOD GetTcpErrorCode()         INLINE iif( Empty( ::skTCP ), 0, InetErrorCode( ::skTCP ) )
   METHOD GetTcpErrorDesc()         INLINE iif( Empty( ::skTCP ), "", InetErrorDesc( ::skTCP ) )

   METHOD UdpHasError()             INLINE iif( Empty( ::skUDP ), .F. , InetErrorCode( ::skUDP ) > 0 )
   METHOD UdpGetErrorCode()         INLINE iif( Empty( ::skUDP ), 0, InetErrorCode( ::skUDP ) )
   METHOD UdpGetErrorDesc()         INLINE iif( Empty( ::skUDP ), "", InetErrorDesc( ::skUDP ) )
   /* Used to retreive data from scans */
   METHOD GetFunctionName( xId )
   METHOD GetServerName( xId )
   METHOD GetServerAddress( xId )

   METHOD UDPAccept()
   METHOD TCPAccept()
   DATA thUdpAccept     INIT NIL
   DATA mtxBusy INIT hb_mutexCreate()
   DATA thTcpAccept     INIT NIL
   DATA skTcp
   DATA nStatus

   HIDDEN:
// Automatic initialization of inet support
   CLASSDATA lInit INIT InetInit()

   /* DATA mtxBusy INIT HB_MutexCreate() */

   /* DATA nStatus */
// This RPC protocol breaking error code
   DATA nErrorCode

   /* Network data */
   DATA cServer
   DATA cNetwork
   DATA skUdp
   /* DATA skTcp */

   /* Timeout system */
   DATA nTimeout        INIT - 1
   DATA nTimeLimit      INIT - 1
   DATA caPerCall

   DATA nUdpTimeBegin   INIT 0
   /* DATA thUdpAccept     INIT NIL */


   DATA nTcpTimeBegin   INIT 0
   /* DATA thTcpAccept     INIT NIL */

   /* XHB RPC Loop system */
   DATA nLoopMode
   DATA aLoopData
   DATA nLoopStart
   DATA nLoopEnd
   DATA nLoopStep

   /* Encryption data */
   DATA bEncrypted
   DATA cCryptKey

   /* Last connection result */
   DATA oResult

   /* Encryption system */
   METHOD Encrypt( cData )
   METHOD Decrypt( cData )
   METHOD BuildChallengePwd( cPassword )
   METHOD ManageChallenge()

   /* Network negotiation system */
   METHOD StartScan()
   /* METHOD UDPAccept() */
   METHOD UDPParse( cData, nLen )
   /* METHOD TCPAccept() */
   METHOD TCPParse( cData )
   METHOD clearTCPBuffer()

   /* internal network send call */
   METHOD SendCall( cFunction, aParams )

   /* event handlers */
   METHOD OnScanComplete()
   METHOD OnScanServersProgress( aLoc )
   METHOD OnScanFunctionsProgress( aLoc )

   METHOD OnFunctionFail( nReason, cReason )
   METHOD OnFunctionReturn( oReturn )
   METHOD OnFunctionProgress( nProgress, oData )

ENDCLASS

METHOD New( cNetwork, nTcpPort, nUdpPort ) CLASS tRPCClient

   ::nStatus := RPC_STATUS_NONE // not connected
   ::nErrorCode := 0 // no RPC error
   ::cServer := NIL // no server

   ::nUdpPort := iif( nUdpPort == NIL, 1139, nUdpPort )
   ::nTcpPort := iif( nTcpPort == NIL, 1140, nTcpPort )

   ::skTcp := InetCreate()
   ::skUdp := InetDGram( .T. )
   ::lAsyncMode := .F.
   ::aServers := {}
   ::aFunctions := {}
   ::cNetwork := cNetwork
   ::bEncrypted := .F.

   ::nLoopMode := RPC_LOOP_NONE

   RETURN Self

METHOD Destroy() CLASS tRPCClient

   hb_mutexLock( ::mtxBusy )

   ::Disconnect()
   HB_TRPCCLIENT_DESTROY( self )

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD SetEncryption( cKey )

   IF .NOT. Empty( cKey )
      ::bEncrypted := .T.
      ::cCryptKey := cKey
   ELSE
      ::bEncrypted := .F.
   ENDIF

   RETURN .T.

METHOD ScanServers( cName ) CLASS tRPCClient

// do not allow asynchronous mode without timeout
   IF .NOT. ::lAsyncMode .AND. ( ::nTimeout == NIL .OR. ::nTimeOut <= 0 )
      RETURN .F.
   ENDIF

   hb_mutexLock( ::mtxBusy )
   ::aServers = {}
   hb_mutexUnlock( ::mtxBusy )

   InetDGramSend( ::skUDP, ::cNetwork , ::nUdpPort, "XHBR00" + hb_Serialize( cName ) )
   ::StartScan()

   RETURN .F.

METHOD CheckServer( cRemote )

   LOCAL cData, skRemote, nLen, cData2

   cData := "XHBR00"
   IF cRemote == NIL
      cRemote := ::cNetwork
   ENDIF
   skRemote := InetConnect( cRemote, ::nTcpPort )
   IF InetErrorCode( skRemote ) == 0
      InetSetTimeout( skRemote, 10000 )
      InetSendAll( skRemote, cData )
      cData := Space( 256 )
      InetRecvAll( skRemote, @cData, 6 + 9 )
      IF InetErrorCode( skRemote ) == 0
         cData2 := Space( 256 )
         nLen := HB_GetLen8( SubStr( cData, 8, 8 ) )
         InetRecvAll( skRemote, @cData2, nLen )
         IF InetErrorCode( skRemote ) == 0
            cData := SubStr( cData + cData2, 7 )
            cData2 := hb_Deserialize( cData )
            AAdd( ::aServers, { InetAddress( skRemote ), cData2 } )
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

METHOD ScanFunctions( cFunc, cSerial ) CLASS tRPCClient

// do not allow asynchronous mode without timeout
   IF .NOT. ::lAsyncMode .AND. ( ::nTimeOut == NIL .OR. ::nTimeOut <= 0 )
      RETURN .F.
   ENDIF

   IF cSerial == NIL
      cSerial := "00000000.0"
   ENDIF

   hb_mutexLock( ::mtxBusy )
   ::aFunctions = {}
   ::aServers = {}
   hb_mutexUnlock( ::mtxBusy )

   InetDGramSend( ::skUDP, ::cNetwork, ::nUdpPort, ;
      "XHBR01" + hb_Serialize( cFunc ) + hb_Serialize( cSerial ) )
   ::StartScan()

   RETURN .F.

METHOD StartScan()

// We don't accept sync call without timeout

   IF ::lAsyncMode
      // in async mode, force scanning stop
      ::StopScan()
   ENDIF

   ::nUDPTimeBegin := Int( Seconds() * 1000 )

// in async mode, just launch the listener
   HB_TRPCCLIENT_STARTSCAN( self )

   RETURN .T.

METHOD UDPAccept() CLASS tRPCClient

   LOCAL nTime, nDatalen, cData

   cData := Space( 1400 )
// set default socket timeout
   IF ::nTimeout >= 0
      InetSetTimeout( ::skUDP, ::nTimeout )
   ELSE
      InetClearTimeout( ::skUdp )
   ENDIF

   DO WHILE .T.
      nDatalen := InetDGramRecv( ::skUDP, @cData, 1400 )

      IF nDataLen > 0 .AND. ::UDPParse( cData, nDatalen )
         EXIT
      ENDIF

      IF ::nTimeout >= 0
         nTime := Int( Seconds() * 1000 )
         // a little tollerance must be added for double roundings
         // in the double INT() functions
         IF nTime - ::nUDPTimeBegin >= ::nTimeout - 5
            EXIT
         ENDIF
      ENDIF

   ENDDO

   ::OnScanComplete()
// signal that this thread is no longer active
   hb_mutexLock( ::mtxBusy )
   ::thUdpAccept := NIL
   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD UDPParse( cData, nLen ) CLASS tRPCClient

   LOCAL cCode, cSer, cFunc, cName
   LOCAL aLoc

   IF nLen < 12
      RETURN .F.
   ENDIF

   cCode := SubStr( cData, 1, 6 )

   DO CASE
      /* XHRB00 - server scan */
   CASE cCode == "XHBR10"
      cData := SubStr( cData, 7 )
      cData := hb_Deserialize( cData, 512 )
      // deserialization error checking
      IF cData != NIL
         aLoc := { InetAddress( ::skUDP ), cData }
         AAdd( ::aServers, aLoc )
         RETURN ::OnScanServersProgress( aLoc )
      ELSE
         RETURN .F.
      ENDIF


   CASE cCode == "XHBR11"
      cData := SubStr( cData, 7 )
      cSer := HB_DeserialBegin( cData )
      cName := HB_DeserialNext( @cSer, 64 )
      cFunc := HB_DeserialNext( @cSer, 64 )
      IF cName != NIL .AND. cFunc != NIL
         aLoc := { InetAddress( ::skUDP ), cName, cFunc }
         AAdd( ::aFunctions, aLoc )
         RETURN ::OnScanFunctionsProgress( aLoc )
      ELSE
         RETURN .F.
      ENDIF

   ENDCASE

   RETURN .F.

METHOD StopScan() CLASS tRPCClient

   HB_TRPCCLIENT_STOPSCAN( self )

   RETURN .T.

METHOD Connect( cServer, cUserId, cPassword ) CLASS tRPCClient

   LOCAL cAuth, cReply := Space( 8 )

   InetConnect( cServer, ::nTcpPort, ::skTcp  )

   IF InetErrorCode( ::skTcp ) == 0
      ::nStatus := RPC_STATUS_CONNECTED // Connected
      IF ::bEncrypted
         cAuth := ::BuildChallengePwd( cPassword )
         cAuth := cUserId + ":" + cAuth
         InetSendAll( ::skTcp, "XHBR93" + HB_CreateLen8( Len( cAuth ) ) + cAuth )
      ELSE
         cAuth := cUserId + ":" + cPassword
         InetSendAll( ::skTcp, "XHBR90" + HB_CreateLen8( Len( cAuth ) ) + cAuth )
      ENDIF

      IF InetErrorCode( ::skTcp ) == 0
         IF .NOT. ::bEncrypted
            InetRecvAll( ::skTcp, @cReply )
            IF InetErrorCode( ::skTcp ) == 0 .AND. cReply == "XHBR91OK"
               ::nStatus := RPC_STATUS_LOGGED // Logged in
               RETURN .T.
            ENDIF
         ELSE
            RETURN ::ManageChallenge()
         ENDIF

      ENDIF
   ENDIF

   ::skTcp := NIL
   ::nStatus := RPC_STATUS_NONE

   RETURN .F.

METHOD BuildChallengePwd( cPassword ) CLASS tRPCClient

   LOCAL nLen, nCount, cRet

   nLen := 10 + Int( hb_Random( 1, 60 ) )

   cRet := ""

   FOR nCount := 1 TO nLen
      cRet += Chr( Int( hb_Random( 2, 254 ) ) )
   NEXT
   cRet += "PASSWORD:" + cPassword + ":"

   DO WHILE Len( cRet ) < 100
      cRet += Chr( Int( hb_Random( 2, 254 ) ) )
   ENDDO

   cRet := ::Encrypt( cRet )

   RETURN cRet

METHOD ManageChallenge() CLASS tRPCClient

   LOCAL cCode, cLen, nLen
   LOCAL cData, nChallenge

   cCode := Space( 6 )
   IF InetRecvAll( ::skTCP, @cCode ) != 6
      RETURN .F.
   ENDIF

   IF cCode != "XHBR94"
      RETURN .F.
   ENDIF

   cLen := Space( 8 )
   IF InetRecvAll( ::skTCP, @cLen ) != 8
      RETURN .F.
   ENDIF

   nLen := HB_GetLen8( cLen )
   cData := Space( nLen )
   IF InetRecvAll( ::skTCP, @cData, nLen ) != nLen
      RETURN .F.
   ENDIF

   cData := HB_Decrypt( cData, ::cCryptKey )
   nChallenge := HB_Checksum( cData )
   InetSendAll( ::skTCP, "XHBR95" + HB_CreateLen8( nChallenge ) )
//IF InetErrorCode( ::skTCP ) != 0
//   RETURN .F.
//ENDIF

   cCode := Space( 8 )
   InetRecvAll( ::skTCP, @cCode )
   IF InetErrorCode( ::skTCP ) != 0 .OR. cCode != "XHBR91OK"
      RETURN .F.
   ENDIF
   /* SUCCESS! */
   ::nStatus := RPC_STATUS_LOGGED

   RETURN .T.

METHOD Disconnect() CLASS tRPCClient

   IF ::nStatus >= RPC_STATUS_LOGGED
      hb_mutexLock( ::mtxBusy )
      ::nStatus :=  RPC_STATUS_NONE
      InetSendAll( ::skTcp, "XHBR92" )
      InetClose( ::skTcp )
      hb_mutexUnlock( ::mtxBusy )
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD SetLoopMode( nMethod, xData, nEnd, nStep ) CLASS tRPCClient

   IF nMethod == RPC_LOOP_NONE
      ::nLoopMode := RPC_LOOP_NONE
      ::aLoopData := NIL
      RETURN .T.
   ENDIF

   IF ValType( xData ) == "A"
      ::aLoopData := xData
   ELSE
      IF ValType( xData ) == "NI"
         // this is to allow garbage collecting
         ::aLoopData := NIL
         ::nLoopStart := xData
         ::nLoopEnd := nEnd
         IF ValType( nStep ) == "NI"
            ::nLoopStep := nStep
         ELSE
            ::nLoopStep := 1
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   ::nLoopMode := nMethod

   RETURN .T.

METHOD ClearTCPBuffer() CLASS tRPCClient

   LOCAL cDummy := Space( 512 )

   IF ::skTCP == NIL .OR. ::nStatus < RPC_STATUS_LOGGED
      RETURN .F.
   ENDIF

   DO WHILE InetDataReady( ::skTCP ) > 0
      // InetRecv reads only the available data
      InetRecv( ::skTCP, @cDummy )
   ENDDO

   RETURN .T.

METHOD Call( ... ) CLASS tRPCClient

   LOCAL oCalling
   LOCAL cFunction, aParams
   LOCAL nCount

   IF PCount() == 0
      RETURN NIL
   ENDIF

   ::oResult := NIL

// do not allow asynchronous mode without timeout
   IF .NOT. ::lAsyncMode .AND. ( ::nTimeOut == NIL .OR. ::nTimeOut <= 0 )
      RETURN NIL
   ENDIF

   oCalling := PValue( 1 )
   IF ValType( oCalling ) == "A"
      cFunction := oCalling[1]
      ADel( oCalling, 1 )
      ASize( oCalling, Len( oCalling ) - 1 )
      aParams := oCalling
   ELSE
      cFunction := oCalling
      aParams := Array( PCount() - 1 )
      FOR nCount := 2 TO PCount()
         aParams[nCount - 1] := PValue( nCount )
      NEXT
   ENDIF

// clear eventual pending data
   ::ClearTcpBuffer()

// The real call
   HB_TRPCCLIENT_CALL( self )

   ::nStatus := RPC_STATUS_WAITING // waiting for a reply

// send the call through the socket
   IF .NOT. ::SendCall( cFunction, aParams )
      RETURN .F.
   ENDIF

// in async mode, just launch the listener
   HB_TRPCCLIENT_ASYNC( self )

   RETURN ::oResult

METHOD SetPeriodCallback( ... ) CLASS tRPCClient

   LOCAL caCalling
   LOCAL nCount

   IF PCount() < 3
      //TODO set an error
      RETURN .F.
   ENDIF

   hb_mutexLock( ::mtxBusy )
   ::nTimeout := PValue( 1 )
   ::nTimeLimit := PValue( 2 )

   caCalling := PValue( 3 )
   IF ValType( caCalling ) != "A"
      caCalling := Array( PCount() - 2 )
      FOR nCount := 3 TO PCount()
         caCalling[nCount - 2] :=  PValue( nCount )
      NEXT
   ENDIF
   ::caPerCall := caCalling

   IF ::skTCP != NIL
      InetSetTimeout( ::skTCP, ::nTimeout )
      InetSetTimeLimit( ::skTCP, ::nTimeLimit )
      InetSetPeriodCallback( ::skTCP, caCalling )
   ENDIF

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD ClearPeriodCallback() CLASS tRPCClient

   hb_mutexLock( ::mtxBusy )

   ::nTimeout := - 1
   ::nTimeLimit := - 1
   ::caPerCall := NIL

   IF ::skTCP != NIL
      InetClearTimeout( ::skTCP )
      InetClearTimeLimit( ::skTCP )
      InetClearPeriodCallback( ::skTCP )
   ENDIF

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD SetTimeout( nTime ) CLASS tRPCClient

   hb_mutexLock( ::mtxBusy )

   ::nTimeout := nTime
   InetSetTimeout( ::skTCP, ::nTimeout )

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD GetTimeout()

   LOCAL nRet

   hb_mutexLock( ::mtxBusy )
   nRet := ::nTimeout
   hb_mutexUnlock( ::mtxBusy )

   RETURN nRet

METHOD StopCall() CLASS tRPCClient

   IF ::nStatus != RPC_STATUS_WAITING
      RETURN .F.
   ENDIF

// clear eventual pending data
   ::ClearTcpBuffer()

// send cancelation request
   InetSendAll( ::skTCP, "XHBR29" );

      //Stops waiting for a result
   HB_TRPCCLIENT_STOPCALL( self )

   RETURN .T.

METHOD SendCall( cFunction, aParams ) CLASS tRPCClient

   LOCAL cData := "", nLen
   LOCAL nReq, cType

   SWITCH ::nLoopMode
   CASE RPC_LOOP_NONE
      nReq = 0
      cType = ""
      EXIT

   CASE RPC_LOOP_ALLDATA
      nReq = 2
      cType = "A"
      EXIT

   CASE RPC_LOOP_SUMMARY
      nReq = 2
      cType = "C"
      EXIT

   CASE RPC_LOOP_CONFIRMATION
      nReq = 2
      cType = "E"
      EXIT
   END

   IF ::aLoopData == NIL .AND. ::nLoopMode > RPC_LOOP_NONE
      cData := hb_Serialize( ::nLoopStart ) + hb_Serialize( ::nLoopEnd ) + ;
         hb_Serialize( ::nLoopStep )
   ENDIF

   cData +=  hb_Serialize( cFunction ) + hb_Serialize( aParams )

   IF ::aLoopData != NIL
      cData += hb_Serialize( ::aLoopData )
      nReq += 2
   ENDIF

   nLen := Len( cData )
   IF nLen > 512
      cData := HB_Compress( cData )
      cData := "XHBR2" + AllTrim( Str( nReq + 1 ) ) + ;
         HB_CreateLen8( nLen ) + HB_CreateLen8( Len( cData ) ) + ;
         cType + ::Encrypt( cData )
   ELSE
      cData := "XHBR2" + AllTrim( Str( nReq ) ) + HB_CreateLen8( nLen ) + ;
         cType + ::Encrypt( cData )
   ENDIF

   InetSendAll( ::skTCP,  cData )

   RETURN ( InetErrorCode( ::skTCP ) == 0 )

METHOD TCPAccept() CLASS tRPCClient

   LOCAL nTime := 0
   LOCAL cCode //, bContinue
   LOCAL nTimeLimit

// TcpAccept can also be called standalone, without the
// support of call(). So, we must set the waiting state.
   hb_mutexLock( ::mtxBusy )

   ::nErrorCode := 0
   ::nStatus := RPC_STATUS_WAITING
   //bContinue := iif( ::caPerCall != NIL, .T. , .F. )

   hb_mutexUnlock( ::mtxBusy )

   cCode := Space( 6 )
   ::nTCPTimeBegin := Int( Seconds() * 1000 )
   nTimeLimit = Max( ::nTimeout, ::nTimeLimit )


   DO WHILE .T.
      IF InetRecvAll( ::skTCP, @cCode, 6 ) <= 0
         EXIT
      ENDIF

      IF .NOT. ::TCPParse( cCode )
         EXIT
      ENDIF

      IF nTimeLimit >= 0
         nTime := Int( Seconds() * 1000 )
         // a little tollerance must be added for double roundings
         // in the double INT() functions
         IF nTime - ::nTCPTimeBegin >= nTimeLimit - 5
            EXIT
         ENDIF
      ENDIF
   ENDDO

   hb_mutexLock( ::mtxBusy )

// NOT waiting anymore
   ::nStatus := RPC_STATUS_LOGGED
   ::thTcpAccept := NIL

   IF ::caPerCall == NIL .AND. InetErrorCode( ::skTCP ) != - 1 .AND. ;
         nTime - nTimeLimit < nTimeLimit - 5
      IF InetErrorCode( ::skTCP ) != 0
         ::nStatus := RPC_STATUS_ERROR
      ENDIF
   ENDIF

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD TCPParse( cCode ) CLASS tRPCClient

   LOCAL nDataLen, cData, nOrigLen
   LOCAL cDataLen := Space( 8 ), cOrigLen := Space( 8 )
   LOCAL cProgress := Space( 10 ), nProgress
   LOCAL lContinue := .F.

   ::nErrorCode := 0

   DO CASE
      /* Warn error codes */
   CASE cCode == "XHBR40"
      cData := Space( 2 )
      InetRecvAll( ::skTCP, @cData, 2 )
      ::nErrorCode := Val( cData )
      ::OnFunctionFail( ::nErrorCode, "No description for now" )

      /* We have a reply */
   CASE cCode == "XHBR30"
      IF InetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
         nDataLen := HB_GetLen8( cDataLen )
         cData := Space( nDataLen )
         IF InetRecvAll( ::skTCP, @cData, nDataLen ) == nDataLen
            ::oResult := hb_Deserialize( ::Decrypt( cData ), nDataLen )
            IF ::oResult != NIL
               ::OnFunctionReturn( ::oResult )
            ENDIF
            // todo: rise an error if ::oResult is nil
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
               cData := HB_Uncompress( nOrigLen, ::Decrypt( cData ) )
               IF .NOT. Empty( cData )
                  ::oResult := hb_Deserialize( cData, nDataLen )
                  IF ::oResult != NIL
                     ::OnFunctionReturn( ::oResult )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      /* We have a progress */
   CASE cCode == "XHBR33"
      IF InetRecvAll( ::skTCP, @cProgress, 10 ) == 10
         nProgress := hb_Deserialize( cProgress, 10 )
         IF nProgress != NIL
            lContinue := .T.
            ::OnFunctionProgress( nProgress )
         ENDIF
      ENDIF

      /* We have a progress with data*/
   CASE cCode == "XHBR34"
      IF InetRecvAll( ::skTCP, @cProgress ) == Len( cProgress )
         nProgress := hb_Deserialize( cProgress, Len( cProgress ) )
         IF nProgress != NIL .AND. InetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
            nDataLen := HB_GetLen8( cDataLen )
            cData := Space( nDataLen )
            IF InetRecvAll( ::skTCP, @cData ) == nDataLen
               ::oResult := hb_Deserialize( ::Decrypt( cData ), nDataLen )
               IF ::oResult != NIL
                  lContinue := .T.
                  ::OnFunctionProgress( nProgress, ::oResult  )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      /* We have a progress with compressed data*/
   CASE cCode == "XHBR35"
      IF InetRecvAll( ::skTCP, @cProgress ) == Len( cProgress )
         nProgress := hb_Deserialize( cProgress, Len( cProgress ) )
         IF nProgress != NIL .AND. InetRecvAll( ::skTCP, @cOrigLen ) == Len( cOrigLen )
            nOrigLen = HB_GetLen8( cOrigLen )
            IF InetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
               nDataLen := HB_GetLen8( cDataLen )
               cData := Space( nDataLen )
               IF InetRecvAll( ::skTCP, @cData ) == nDataLen
                  cData := HB_Uncompress( nOrigLen, cData )
                  IF .NOT. Empty( cData )
                     ::oResult := hb_Deserialize( ::Decrypt( cData ), nDataLen )
                     IF ::oResult != NIL
                        lContinue := .T.
                        ::OnFunctionProgress( nProgress, ::oResult )
                     ENDIF
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

METHOD GetFunctionName( xId ) CLASS tRpcClient

   LOCAL cData, nPos

   IF ValType( xID ) == "A"
      cData := xId[3]
   ELSEIF Len( ::aFunctions ) > 0
      cData := ::aFunctions[xId][3]
   ELSE
      cData := ""
   ENDIF

   IF .NOT. Empty( cData )
      nPos := At( "(", cData )
      cData := SubStr( cData, 1, nPos - 1 )
   ENDIF

   RETURN cData

METHOD GetServerName( xId ) CLASS tRpcClient

   LOCAL cData

   IF ValType( xID ) == "A"
      cData := xId[2]
   ELSE
      IF Len( ::aFunctions ) > 0
         cData := ::aFunctions[xId][2]
      ELSEIF Len( ::aServers ) > 0
         cData := ::aServers[xId][2]
      ELSE
         cData := ""
      ENDIF
   ENDIF

   RETURN cData

METHOD GetServerAddress( xId ) CLASS tRpcClient

   LOCAL cData

   IF ValType( xID ) == "A"
      cData := xId[1]
   ELSE
      IF .NOT. Empty( ::aFunctions )
         cData := ::aFunctions[xId][1]
      ELSEIF .NOT. Empty( ::aServers )
         cData := ::aServers[xId][1]
      ELSE
         cData := ""
      ENDIF
   ENDIF

   RETURN cData

METHOD Encrypt( cDataIn ) CLASS tRPCClient

   IF ::bEncrypted
      RETURN HB_Crypt( cDataIn, ::cCryptKey )
   ENDIF

   RETURN cDataIn

METHOD Decrypt( cDataIn ) CLASS tRPCClient

   IF ::bEncrypted
      RETURN HB_Decrypt( cDataIn, ::cCryptKey )
   ENDIF

   RETURN cDataIn


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

   IF ::bOnFunctionProgress != NIL
      RETURN Eval( ::bOnFunctionProgress, nProgress, oData )
   ENDIF

   RETURN .T.

