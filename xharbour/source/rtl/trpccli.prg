/*
 * $Id: trpccli.prg,v 1.8 2003/02/24 05:55:55 jonnymind Exp $
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

   /* Encryption data */
   DATA bEncrypted
   DATA cCryptKey

   METHOD New( cNetwork ) CONSTRUCTOR
   METHOD ScanServers( cName, nTime )
   METHOD ScanFunctions( cName, cSerial, nTime )
   METHOD StartScan()
   METHOD StopScan()

   METHOD UDPAccept()
   METHOD UDPParse( cData, nLen )

   METHOD TCPAccept()
   METHOD TCPParse( cData )

   METHOD SetEncryption( cKey )
   METHOD Connect( cServer, cUserId, cPassword, nTimeout )

   METHOD Call( cFucntion, aParams, nTimeout )
   METHOD CallLoop( cFucntion, nBegin, nEnd, nStep, aParams, nTimeout )
   METHOD CallLoopSummary( cFucntion, nBegin, nEnd, nStep, aParams, nTimeout )
   METHOD CallLoopEnd( cFucntion, nBegin, nEnd, nStep, aParams, nTimeout )
   METHOD CallForEach( cFucntion, aElements, aParams, nTimeout )
   METHOD CallForEachSummary( cFucntion, aElements, aParams, nTimeout )
   METHOD CallForEachEnd( cFucntion, aElements, aParams, nTimeout )

   METHOD CallMode( cFucntion, aParams, nTimeout, nMode, aElems )

   METHOD Disconnect()
   METHOD Destroy()

   /* Utility functions */
   METHOD GetFunctionName( oId )
   METHOD Encrypt( cData )
   METHOD Decrypt( cData )
   METHOD BuildChallengePwd( cPassword )
   METHOD ManageChallenge()

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
   ::bEncrypted := .F.

RETURN Self


METHOD Destroy() CLASS tRPCClient

   MutexLock( ::mtxBusy )
      ::Disconnect()
      IF .not. Empty( ::skTcp )
         InetDestroy( ::skTcp )
      ENDIF
      IF .not. Empty( ::skUdp )
         InetDestroy( ::skUdp )
      ENDIF
      IF ::thUdpAccept > 0
         KillThread( ::thUdpAccept )
         ::thUdpAccept := -1
      ENDIF
      IF ::thTcpAccept > 0
         KillThread( ::thTcpAccept )
         ::thTcpAccept := -1
      ENDIF
   MutexUnlock( ::mtxBusy )
   DestroyMutex( ::mtxBusy )
RETURN .T.


METHOD SetEncryption( cKey )
   ::bEncrypted := .T.
   ::cCryptKey := cKey
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
   // signal that this thread is no longer active
   MutexLock( ::mtxBusy )
   ::thUdpAccept := -1
   MutexUnlock( ::mtxBusy )

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
      KillThread( ::thUDPAccept )
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
      IF ::bEncrypted
         cAuth := ::BuildChallengePwd( cPassword )
         cAuth := cUserId + ":" + cAuth
         InetSendAll( ::skTcp, "XHBR93" + HB_CreateLen8( Len( cAuth ) ) + cAuth )
      ELSE
         cAuth := cUserId + ":" + cPassword
         InetSendAll( ::skTcp, "XHBR90" + HB_CreateLen8( Len( cAuth ) ) + cAuth )
      ENDIF
      
      IF InetErrorCode( ::skTcp ) == 0
         IF .not. ::bEncrypted
            InetRecvAll( ::skTcp, @cReply )
            IF InetErrorCode( ::skTcp ) == 0 .and. cReply == "XHBR91OK"
               ::nStatus := 3 // Logged in
               RETURN .T.
            ENDIF
         ELSE
            RETURN ::ManageChallenge()
         ENDIF

      ENDIF
   ENDIF

   InetDestroy( ::skTcp )
   ::skTcp := NIL
   ::nStatus := 0
RETURN .F.


METHOD BuildChallengePwd( cPassword ) CLASS tRPCClient
   LOCAL nLen, nCount, cRet

   nLen := 10 + INT( HB_Random( 1, 60 ) )

   cRet := ""

   FOR nCount := 1 TO nLen
      cRet += Chr( Int( HB_Random( 2, 254 ) ) )
   NEXT
   cRet += "PASSWORD:" + cPassword + ":"

   DO WHILE Len( cRet ) < 100
      cRet += Chr( Int( HB_Random( 2, 254 ) ) )
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
   IF InetErrorCode( ::skTCP ) != 0
      RETURN .F.
   ENDIF

   cCode := Space( 8 )
   InetRecvAll( ::skTCP, @cCode )
   IF InetErrorCode( ::skTCP ) != 0 .or. cCode != "XHBR91OK"
      RETURN .F.
   ENDIF
   /* SUCCESS! */
   ::nStatus := 3

RETURN .T.


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
RETURN ::CallMode( cFunction, aParams, nTime, 0 , NIL )

METHOD CallLoop( cFunction, nStart, nEnd, nStep, aParams, nTime ) CLASS tRPCClient
RETURN ::CallMode( cFunction, aParams, nTime, 1 , { nStart, nEnd, nStep } )

METHOD CallLoopSummary( cFunction, nStart, nEnd, nStep, aParams, nTime ) CLASS tRPCClient
RETURN ::CallMode( cFunction, aParams, nTime, 2 , { nStart, nEnd, nStep } )

METHOD CallLoopEnd( cFunction, nStart, nEnd, nStep, aParams, nTime ) CLASS tRPCClient
RETURN ::CallMode( cFunction, aParams, nTime, 3 , { nStart, nEnd, nStep } )

METHOD CallForeach( cFunction, aElems, aParams, nTime ) CLASS tRPCClient
RETURN ::CallMode( cFunction, aParams, nTime, 4 , aElems )

METHOD CallForeachSummary( cFunction, aElems, aParams, nTime ) CLASS tRPCClient
RETURN ::CallMode( cFunction, aParams, nTime, 5 , aElems )

METHOD CallForeachEnd( cFunction, aElems, aParams, nTime ) CLASS tRPCClient
RETURN ::CallMode( cFunction, aParams, nTime, 6 , aElems )


METHOD CallMode( cFunction, aParams, nTime, nMode, aElems ) CLASS tRPCClient
   LOCAL oRet
   // do not allow asynchronous mode without timeout
   IF .not. ::lAsyncMode .and. ( nTime == NIL .or. nTime <= 0 )
      RETURN NIL
   ENDIF

   MutexLock( ::mtxBusy )
   // already active or not already connected
   IF ::thTcpAccept > 0 .or. ::skTCP == NIL .or. ::nStatus < 3
      MutexUnlock( ::mtxBusy )
      RETURN NIL
   ENDIF

   ::oResult := NIL
   ::nStatus := 4 // waiting for a reply
   // send the call through the socket
   IF .not. ::SendCall( cFunction, aParams, nMode, aElems )
      RETURN .F.
   ENDIF

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


METHOD SendCall( cFunction, aParams, nMode, aElems ) CLASS tRPCClient
   LOCAL cData, nLen
   LOCAL nReq, cType

   SWITCH nMode
      CASE 0
         nReq = 0
         cType = ""
         cData := ""
      EXIT

      CASE 1
         nReq = 2
         cType = "A"
         cData = HB_Serialize( aElems[1] ) + HB_Serialize( aElems[2] ) +;
                HB_Serialize( aElems[3] )
      EXIT

      CASE 2
         nReq = 2
         cType = "C"
         cData = HB_Serialize( aElems[1] ) + HB_Serialize( aElems[2] ) +;
                HB_Serialize( aElems[3] )
      EXIT

      CASE 3
         nReq = 2
         cType = "E"
         cData := HB_Serialize( aElems[1] ) + HB_Serialize( aElems[2] ) +;
                HB_Serialize( aElems[3] )
      EXIT

      CASE 4
         nReq = 4
         cType = "A"
         cData = ""
      EXIT

      CASE 5
         nReq = 4
         cType = "C"
         cData = ""
      EXIT

      CASE 6
         nReq = 4
         cType = "E"
         cData = ""
      EXIT
   END

   cData +=  HB_Serialize( cFunction ) + HB_Serialize( aParams )

   IF nMode >= 4
      cData += HB_Serialize( aElems )
   ENDIF

   nLen := Len( cData )
   IF nLen > 512
      cData := HB_Compress( cData )
      cData := "XHBR2" + AllTrim( Str( nReq + 1 ) ) + ;
         HB_CreateLen8( nLen ) + HB_CreateLen8( Len( cData ) ) +;
            cType + ::Encrypt( cData )
   ELSE
      cData := "XHBR2" + AllTrim( Str( nReq ) ) + HB_CreateLen8( nLen ) +;
             cType + ::Encrypt( cData)
   ENDIF

   InetSendAll( ::skTCP,  cData )
RETURN ( InetErrorCode( ::skTCP ) == 0 )


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

   // signal that this thread is no longer active.
   MutexLock( ::mtxBusy )
   ::thTcpAccept := -1
   MutexUnlock( ::mtxBusy )

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
               ::oResult := HB_Deserialize( ::Decrypt( cData ) )
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
                     ::oResult := HB_Deserialize( ::Decrypt( cData ) )
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
                  ::OnFunctionProgress( nProgress, HB_Deserialize(::Decrypt( cData) ) )
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
                        ::OnFunctionProgress( nProgress, HB_Deserialize( ::Decrypt( cData) ) )
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


METHOD Encrypt(cDataIn) CLASS tRPCClient
   IF ::bEncrypted
      RETURN HB_Crypt( cDataIn, ::cCryptKey )
   ENDIF
RETURN cDataIn


METHOD Decrypt(cDataIn) CLASS tRPCClient
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
   IF ::bOnFunctionReturn != NIL
      RETURN Eval( ::bOnFunctionProgress, nProgress, oData )
   ENDIF
RETURN .T.
