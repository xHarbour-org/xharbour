/*
 * $Id: trpc.prg,v 1.2 2003/02/16 03:03:41 jonnymind Exp $
 */

/*
 * xHarbour Project source code:
 * Remote Procedure Call code
 * xHarbour part
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

/*
   XHB Remote procedure call protocol

   NOTES:
   All packets begin with the string "XHBR??" where ?? are 2 readable characters
   containing the packet type. In the protocol, a field in "" means a serialized
   string. A field in {} means a serialized array.

   Serialized strings: 4 chars lenght in network order, and then the string.
   Function serial numbers: "AAAAMMDD.C", where c is a developer defined character.

   UDP requests:
   00 - Server scan
      + "Server Name" ( valid regex or nothing )

   01 - Function scan
     + "Function Name" (valid regex)
     + "Minimum Serial" (00000000.0 for all)

   UDP replies:
   10 - Server available
      + "server name"
   11 - Function available
      +"server name"
      +"function description" as returned by tRPCFunction::Describe()

   12 - Too many functions in function request
      Sending data in compressed format:


   TCP requests:

   20 - Function call
     <LEN8> - raw data length
     "Function name" + Function parameters serialized one after another

   21 - Compressed function call
     <LEN8> - Original data length
     <LEN8> - compressed data length
     * follows compressed data containing serialized name + params


   TCP REPLIES:

   30 - Function result
      + <LEN8> - raw data len
      + Serialized result

   31 - Compressed result
     + <LEN8> - Original data length
     + <LEN8> -compressed data len
     + Compressed data containing serialized result

   33 - Progress
     + Serialized progress number (0 to 100 float)

   34 - Progress with raw data
     + Serialized progress number (0 to 100 float) (10 chars)
     + <LEN8> - raw data len
     + Uncompressed progress data

   35 - Progress with compressed data
     + Serialized progress number (0 to 100 float) (10 chars)
     + <LEN8> - Original data length
     + <LEN8> - compressed data lenght
     + Compressed progress data


   40 - Function call error
      00 - Function not present
      01 - Not enough level
      02 - wrong parameters

      10 - Internal function error
      11 - busy, retry later



   90 - LOGIN
      <LEN8> + USERID:PASSWORD
   91 - LOGIN STATUS
      "OK"
      "NO"
   92 - GOODBYE
*/

#include "hbclass.ch"


/************************************
* RPC FUNCTION
*************************************/

CLASS tRPCFunction
   DATA cName
   DATA aParameters
   DATA cReturn
   DATA cSerial
   DATA nAuthLevel

   CLASSDATA cPattern INIT HB_RegexComp( "^C:[0-9]{1,6}$|^A$|^D$|^N:[0-9]{1,2}(,[0-9]{1,2})?$")

   METHOD New( cFname, cSerial, cFret, aParams )
   METHOD CheckTypes( aParams )
   METHOD CheckParam( cParam )
   METHOD Describe()
   METHOD Run( aParams, skNotify ) VIRTUAL
   METHOD SendProgress( skNotify, nProgress )
ENDCLASS


METHOD New( cFname, cSerial, cFret, aParams, nAuthLevel ) CLASS tRPCFunction
   LOCAL cParam

   ::cName := cFname
   ::cReturn := cFret
   ::CheckParam( ::cReturn )
   IF .not. HB_RegexMatch( "[0-9]{8}\..", cSerial )
      Alert( "Serial value not valid" )
      QUIT
   ENDIF
   
   IF nAuthLevel < 1
      Alert( "Authorization level must be at least 1" )
      QUIT
   ENDIF

   ::cSerial := cSerial
   ::nAuthLevel := nAuthLevel
   ::aParameters := {}
   FOR EACH cParam IN aParams
      ::CheckParam( cParam )
      AAdd( ::aParameters, cParam )
   NEXT

RETURN Self


METHOD CheckParam( cParam ) CLASS tRPCFunction
   IF .not. HB_RegexMatch( ::cPattern, cParam )
      Alert("tRPCFunction:CheckParam() wrong parameter specification:" + cParam )
      QUIT
   ENDIF
RETURN .T.


METHOD CheckTypes( aParams ) CLASS tRPCFunction
   LOCAL oElem, i := 0

   IF ValType( aParams ) != 'A'
      RETURN .F.
   ENDIF

   IF Len( aParams ) != Len( ::aParameters )
      RETURN .F.
   ENDIF

   FOR EACH oElem in ::aParameters
      i++
      IF ValType( aParams[i] ) != oElem[1]
         RETURN .F.
      ENDIF
   NEXT
RETURN .T.


METHOD Describe() CLASS tRPCFunction
   LOCAL cRet := ::cName +"[" + ::cSerial + "]/" + ::cReturn
   LOCAL cVar

   FOR EACH cVar IN ::aParameters
      cRet += "," + cVar
   NEXT
RETURN cRet


METHOD SendProgress( skNotify, nProgress, oData )
   LOCAL cOrigLen, cCompLen, lRet := .T.
   LOCAL cData

   IF Empty( oData )
      InetSendAll( skNotify, "XHBR33" + HB_Serialize( nProgress ) )
   ELSE
      cData := HB_Serialize( oData )
      cOrigLen := HB_CreateLen8( Len( cData ) )
      // do we should compress it ?
      IF Len( cData ) > 512
         cData := HB_Compress( cData )
         cCompLen := HB_CreateLen8( Len( cData ) )
         InetSendAll( skNotify, "XHBR35" + HB_Serialize( nProgress ) +;
                cOrigLen + cCompLen + cData )
      ELSE
         InetSendAll( skNotify, "XHBR34" + HB_Serialize( nProgress ) + cOrigLen + cData )
      ENDIF
   ENDIF

   IF InetErrorCode( skNotify ) != 0
      lRet := .F.
   ENDIF

RETURN lRet

/************************************
* RPC SERVICE
*************************************/

CLASS tRPCService
   DATA cServerName INIT "RPCGenericServer"
   DATA aFunctions
   CLASSDATA lInit INIT InetInit()

   DATA nUdpPort INIT 1139
   DATA nTcpPort INIT 1140
   DATA cBindAddress INIT NIL
   DATA thAccept INIT 0
   DATA thUdp INIT 0
   DATA aServing INIT {}
   DATA mtxBusy INIT CreateMutex()

   DATA skUdp
   DATA skServer

   DATA bAuthorize

   METHOD New() Constructor

   /* Function management */
   METHOD Add( oFunction )
   METHOD Run( cName, aParams )
   METHOD Describe( cName )
   METHOD Find( cName )
   METHOD Remove( cName )

   /* General services */
   METHOD Start( lStartUdp )
   METHOD Stop()

   /* Tcp services */
   METHOD Accept()
   METHOD GetAuth( skIn, nAuthlevel, nAuthCount )
   METHOD GetFunction( skIn, nAuthLevel )
   METHOD GetFunctionComp( skIn, nAuthLevel )
   METHOD LaunchFunction( skIn, cData, nAuthLevel )

   /* UDP services */
   METHOD ServeConnection()
   METHOD UdpListen()
   METHOD UDPParseRequest()

   /* to be overloaded */
   METHOD Authorize( cUserid, cPassword )

ENDCLASS


METHOD New() class tRPCService
   ::aFunctions := {}
RETURN Self


METHOD Add( oFunction )
   LOCAL nElem, lRet := .F.

   MutexLock( ::mtxBusy )
   nElem := AScan( ::aFunctions, {|x| oFunction:cName == x:cName})
   IF nElem == 0
      Aadd( ::aFunctions  , oFunction )
      lRet := .T.
   ENDIF
   MutexUnlock( ::mtxBusy )
RETURN lRet


METHOD Find( cName ) class tRPCService
   LOCAL nElem
   LOCAL oRet := NIL

   MutexLock( ::mtxBusy )
   nElem := AScan( ::aFunctions, {|x| cName == x:cName})
   IF nElem != 0
      oRet := ::aFunctions[ nElem ]
   ENDIF
   MutexUnlock( ::mtxBusy )
RETURN oRet


METHOD Remove( cName ) class tRPCService
   LOCAL nElem
   LOCAL lRet := .F.

   MutexLock( ::mtxBusy )
   nElem := AScan( ::aFunctions, {|x| cName == x:cName})
   IF nElem != 0
      ADel( ::aFunctions, nElem )
      ASize( ::aFunctions, Len( ::aFunctions ) - 1 )
      lRet := .T.
   ENDIF
   MutexUnlock( ::mtxBusy )
RETURN lRet


METHOD Run( cName, aParams ) class tRPCService
   LOCAL oFunc := ::Find( cName )
   LOCAL oRet := NIL

   MutexLock( ::mtxBusy )
   IF ! Empty( oFunc )
      oRet := oFunc:Run( aParams )
   ENDIF
   MutexUnlock( ::mtxBusy )

RETURN oRet


METHOD Describe( cName ) class tRPCService
   LOCAL oFunc := ::Find( cName )
   LOCAL cRet := NIL

   MutexLock( ::mtxBusy )
   IF ! Empty( oFunc )
      cRet := oFunc:Describe()
   ENDIF
   MutexUnlock( ::mtxBusy )

RETURN cRet


METHOD Start( lStartUdp ) CLASS tRPCService

   IF Empty( ::cBindAddress )
      ::skServer := InetServer( ::nTcpPort )
      ::skUdp := InetDGramBind( ::nUdpPort )
   ELSE
      ::skServer := InetServer( ::nTcpPort, ::cBindAddress )
      ::skUdp := InetDGramBind( ::nUdpPort, ::cBindAddress )
   ENDIF

   ::thAccept := StartThread( Self, "Accept" )

   IF lStartUdp != NIL .and. lStartUdp
      ::thUdp := StartThread( Self, "UdpListen" )
   ELSE
      ::thUdp := -1
   ENDIF

RETURN .T.


METHOD Stop() CLASS tRPCService
   LOCAL oElem

   MutexLock( ::mtxBusy )
   IF ::thAccept == 0
      MutexUnlock( ::mtxBusy )
      RETURN .F.
   ENDIF

   StopThread( ::thAccept )
   IF ::thUDP > 0
      StopThread( ::thUdp )
   ENDIF

   // now destroy all the allocated resources
   InetDestroy( ::skServer )
   InetDestroy( ::skUdp )
   FOR EACH oElem IN ::aServing
      StopThread( oElem[1] )
      InetDestroy( oElem[2] )
   NEXT
   ASize( ::aServing, 0 )

   MutexUnlock( ::mtxBusy )

RETURN .T.


METHOD Accept() CLASS tRPCService
   LOCAL skIn, thID

   DO WHILE .T.
      skIn := InetAccept( ::skServer )
      // todo: better sync
      MutexLock( ::mtxBusy )
      thID := StartThread( Self, "ServeConnection", skIn )
      AAdd( ::aServing, { thID, skIN } )
      MutexUnlock( ::mtxBusy )
   ENDDO
RETURN .T.


METHOD ServeConnection( skIn ) CLASS tRPCService
   LOCAL nToken
   LOCAL cCode := Space( 6 )
   LOCAL lBreak := .F.
   LOCAL nAuthLevel := 0
   LOCAL nAuthCount := 0


   DO WHILE InetErrorCode( skIn ) == 0 .and. .not. lBreak

      /* Get the request code */
      InetRecvAll( skIn, @cCode, 6 )
      IF InetErrorCode( skIn ) != 0
         EXIT
      ENDIF

      lBreak := .T.
      DO CASE

         /* Read autorization request */
         CASE cCode == "XHBR90"
            lBreak := ::GetAuth( skIn, @nAuthLevel, @nAuthCount )

         /* Close connection */
         CASE cCode == "XHBR92"
            lBreak := .T.

         /* Execute function */
         CASE cCode == "XHBR20"
            lBreak := ::GetFunction( skIn, nAuthLevel )

         /* Execute function */
         CASE cCode == "XHBR21"
            lBreak := ::GetFunctionComp( skIn, nAuthLevel )
      ENDCASE

   ENDDO


   // signaling termination of this thread
   MutexLock( ::mtxBusy )
   InetDestroy( skIn )
   nToken := AScan( ::aServing, {|x| x[1] == ThreadGetCurrent() } )
   ADel( ::aServing, nToken )
   ASize( ::aServing, Len( ::aServing ) -1 )
   MutexUnlock( ::mtxBusy )
RETURN .T.


METHOD GetAuth( skIn, nAuthLevel, nAuthCount ) CLASS tRPCService
   LOCAL cLength := Space(8), nLen, nPos
   LOCAL cUserID, cPassword
   LOCAL lBreak := .T.
   LOCAL cReadIn := Space(1024)

   IF InetRecvAll( skIn, @cLength, 8 ) == 8
      nLen := HB_GetLen8( cLength )
      IF nLen < 33
         IF InetRecvAll( skIn, @cReadin, nLen ) == nLen
            nPos := At( ":", cReadin )
            IF nPos > 0
               lBreak:= .F.
               cUserID := Substr(cReadin, 1, nPos-1 )
               cPassword := Substr( cReadin, nPos+1, nLen - nPos  )
               nAuthLevel := ::Authorize( cUserid, cPassword )
               IF nAuthLevel == 0
                  nAuthCount ++
                  InetSendAll( skIn, "XHBR91NO" )
                  IF nAuthCount > 2
                     lBreak := .T.
                  ENDIF
               ELSE
                  nAuthCount = 0
                  InetSendAll( skIn, "XHBR91OK" )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
RETURN lBreak


METHOD GetFunction( skIn, nAuthLevel ) CLASS tRPCService
   LOCAL cLength := Space(8), nLen
   LOCAL cData
   LOCAL lBreak := .T.

   IF InetRecvAll( skIn, @cLength, 8 ) == 8
      nLen := HB_GetLen8( cLength )
      IF nLen < 65000
         cData := Space( nLen )
         IF InetRecvAll( skIn, @cData, nLen ) == nLen
            lBreak := .not. ::LaunchFunction( skIn, cData, nAuthLevel )
         ENDIF
      ENDIF
   ENDIF

RETURN lBreak


METHOD LaunchFunction( skIn, cData, nAuthLevel ) CLASS tRPCService
   LOCAL cSer, aParam
   LOCAL cFuncName, oFunc
   LOCAL cOrigLen, cCompLen
   LOCAL oRet

   cSer := HB_DeserialBegin( cData )
   cFuncName := HB_DeserialNext( cSer )
   aParam := HB_DeserialNext( cSer )

   //let's try to run this function.
   oFunc := ::Find( cFuncName )
   IF Empty(oFunc)
      // signal error
      InetSendAll( skIn, "XHBR4000" )
      // request socket closing
      RETURN .F.
   ENDIF

   // check for level
   IF oFunc:nAuthLevel > nAuthLevel
      // signal error
      InetSendAll( skIn, "XHBR4001" )
      // request socket closing
      RETURN .F.
   ENDIF

   //check for parameters
   IF Empty( aParam ) .or. .not. oFunc:CheckTypes( aParam )
      // signal error
      InetSendAll( skIn, "XHBR4002" )
      // request socket closing
      RETURN .F.
   ENDIF

   IF InetErrorCode( skIn ) == 0
      // for now, just run it
      oRet := oFunc:Run( aParam, skIn )

      // Internal error?
      IF oRet == NIL
         InetSendAll( skIn, "XHBR4010" )
      ELSE
         cData := HB_Serialize( oRet )
         cOrigLen := HB_CreateLen8( Len( cData ) )
         // do we should compress it ?
         IF Len( cData ) > 512
            cData := HB_Compress( cData )
            cCompLen := HB_CreateLen8( Len( cData ) )
            InetSendAll( skIn, "XHBR31" + cOrigLen + cCompLen + cData )
         ELSE
            InetSendAll( skIn, "XHBR30" + cOrigLen + cData )
         ENDIF
      ENDIF
   ENDIF

RETURN .T.


METHOD GetFunctionComp( skIn, nAuthLevel ) CLASS tRPCService
   LOCAL cLength := Space(8), cOrigLen := Space(8), nLen, nOrigLen
   LOCAL cData
   LOCAL lBreak := .T.

   IF InetRecvAll( skIn, @cOrigLen, 8 ) == 8
      nOrigLen := HB_GetLen8( cOrigLen )

      IF InetRecvAll( skIn, @cLength, 8 ) == 8
         nLen := HB_GetLen8( cLength )

         IF nLen < 65000
            cData := Space( nLen )
            IF InetRecvAll( skIn, @cData, nLen ) == nLen
               // decompress data
               cData := HB_Uncompress( nOrigLen, cData )
               IF .not. Empty( cData )
                  lBreak := .not. ::LaunchFunction( skIn, cData, nAuthLevel )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
RETURN lBreak


METHOD UDPListen( ) CLASS tRPCService
   LOCAL cData := Space( 1000 )
   LOCAL nPacketLen

   DO WHILE .T.
      nPacketLen := InetDGramRecv( ::skUdp, @cData, 1000 )
      ::UDPParseRequest( cData, nPacketLen )
   ENDDO
RETURN .T.


METHOD UDPParseRequest( cData, nPacketLen ) CLASS tRPCService
   LOCAL cCode, cMatch, cNumber, cSerial
   LOCAL oFunc

   IF nPacketLen <= 6
      RETURN .F.
   ENDIF

   cCode := Substr( cData, 1, 6 )

   DO CASE

      /* XHRB00 - server scan */
      CASE cCode == "XHBR00"
         IF nPacketLen > 6
            cMatch := HB_Deserialize( Substr( cData, 7 ) )
            IF HB_RegexMatch( cMatch, ::cServerName )
               InetDGramSend( ::skUdp, InetAddress( ::skUdp ), InetPort( ::skUdp ), ;
                  "XHBR10"+ HB_Serialize( ::cServerName ) )
            ENDIF
         ELSE
            InetDGramSend( ::skUdp, InetAddress( ::skUdp ), InetPort( ::skUdp ), ;
               "XHBR10"+ HB_Serialize( ::cServerName ) )
         ENDIF

      /* XRB01 - Function scan */
      CASE cCode == "XHBR01"
         /* minimal length to be valid */
         IF nPacketLen > 24
            cSerial := HB_DeserialBegin( Substr( cData, 7 ) )
            cMatch := HB_DeserialNext( cSerial )
            cNumber := NIL
            IF .not. Empty ( cMatch )
               cMatch := HB_RegexComp( cMatch )
               cNumber := HB_DeserialNext( cSerial )
            ELSE
               cMatch := HB_RegexComp( ".*" )
            ENDIF

            IF Empty( cNumber )
               cNumber := "00000000.0"
            ENDIF

            FOR EACH oFunc IN ::aFunctions
               IF HB_RegexMatch( cMatch, oFunc:cName ) .and. cNumber <= oFunc:cSerial
                  InetDGramSend(::skUdp, InetAddress( ::skUdp ), InetPort( ::skUdp ), ;
                     "XHBR11" + HB_Serialize(::cServerName ) + ;
                     HB_Serialize( ofunc:Describe()))
               ENDIF
            NEXT
         ENDIF
   ENDCASE

RETURN .T.


/* Default authorization will ALWAYS return 1 if a bAuthorize block is not provided */
METHOD Authorize( cUserid, cPassword ) CLASS tRPCService
   IF ::bAuthorize != NIL
      RETURN Eval( ::bAuthorize, cUserid, cPassword )
   ENDIF
RETURN 1



