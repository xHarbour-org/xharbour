/*
 * $Id: trpc.prg,v 1.6 2003/02/24 01:58:10 jonnymind Exp $
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
   string. A field in {} means a serialized array. A field in '' means a literal
   set of bytes (characters).

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
     "Function name" + { Param1, ... Param N }

   21 - Compressed function call
     <LEN8> - Original data length
     <LEN8> - compressed data length
     * follows compressed data containing serialized name + params

   22 - Loop Function Call
     <LEN8> - Raw data length
     "A" or "C" or "E": send all results/ send compressed result/
         send confirmation at end
     Numeric BEGIN
     Numeric END
     Numeric STEP
     "Function name" + { Param1, ... Param N }
     Note: the parameter called $1 is the loop indicator

   23 - Loop Function Call / Compressed
     <LEN8> - Original data length
     <LEN8> - compressed data length
     "A" or "C" or "E": send all results/ send compressed result/
         send confirmation at end
     * follows compressed data containing:
     Numeric BEGIN
     Numeric END
     Numeric STEP
     "Function name" + { Param1, ... Param N }
     Note: the parameter called $1 is the loop indicator

   24 - Foreach function call
     <LEN8> - raw data length
     "A" or "C" or "E": send all results/ send compressed result/
         send confirmation at end
     "Function name" + { Param1, ... Param N }
     +Array containing the elements
     Note: the parameter called $1 is substitued with the foreach

   25 - Foreach function call / Compressed
     <LEN8> - Original data length
     <LEN8> - compressed data length
     "A" or "C" or "E": send all results/ send compressed result/
         send confirmation at end
     * follows compressed data containing:
     "Function name" + { Param1, ... Param N }
     +Array containing the elements
     Note: the parameter called $1 is substitued with the foreach


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
      'OK'
      'NO'
   92 - GOODBYE

   93 - Encripted login
      <LEN8> Total length
      'USERID:ENCRYPTED( Random data + PASSWORD:pwd: + Random data)'

   94 - Challenge
      <LEN8> Total length
      'ENCRYPT(CHALLENGE DATA)'

   95 - Challenge reply
      <NUM8> - the CRC32 checksum of challenge.


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
   DATA bGetRawParams

   CLASSDATA cPattern INIT HB_RegexComp( "^C:[0-9]{1,6}$|^A$|^D$|^N:[0-9]{1,2}(,[0-9]{1,2})?$")

   METHOD New( cFname, cSerial, cFret, aParams, nAuthLevel, bGetRawParams ) CONSTRUCTOR
   METHOD CheckTypes( aParams )
   METHOD CheckParam( cParam )
   METHOD Describe()
   METHOD Run( aParams, oClient ) VIRTUAL
ENDCLASS


METHOD New( cFname, cSerial, cFret, aParams, nAuthLevel, bGetRaw ) CLASS tRPCFunction
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

   IF bGetRaw != NIL
      ::bGetRawParams := bGetRaw
   ELSE
      ::bGetRawParams := .T.
   ENDIF

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


/***********************************************************
* Connection manager class; this manages a single connection
************************************************************/

CLASS tRPCServeCon
   /* back reference to the parent to get callback blocks */
   DATA oServer
   
   /* Socket, mutex and thread */
   DATA skRemote
   DATA mtxBusy
   DATA thSelf INIT -1

   /* Assigned authorization level */
   DATA nAuthLevel

   /* User ID */
   DATA cUserId

   /* Allow progress ?*/
   DATA bAllowProgress

   /* Is this connection encrypted? */
   DATA bEncrypted

   /* crc for challenge handshake */
   DATA nChallengeCRC
   /* Temporary supposed user in challenge */
   DATA cChallengeUserid
   DATA cCryptKey

   METHOD New( oCaller, skRemote ) CONSTRUCTOR
   METHOD Destroy()

   /* Managing async */
   METHOD Start()
   METHOD Stop()
   METHOD Run()

   /* Utilty */
   METHOD RecvAuth( lEncrypt )
   METHOD RecvChallenge()
   METHOD RecvFunction( bComp, bMode )
   METHOD FuncCall( cData )
   METHOD FuncLoopCall( cData, cMode )
   METHOD FuncForeachCall( cData, cMode )
   METHOD LaunchChallenge( cUserid, cPassword )
   METHOD LaunchFunction( cFuncName, aParms, nMode, aItems )
   METHOD SendResult( oRet )
   METHOD SendProgress( nProgress, aData )

   METHOD Encrypt(cDataIn)
   METHOD Decrypt(cDataIn)

ENDCLASS


METHOD New( oParent, skIn ) CLASS tRPCServeCon
   ::oServer := oParent
   ::skRemote := skIn
   ::mtxBusy := CreateMutex()
   ::bEncrypted := .F.
   ::nAuthLevel := 0
   ::nChallengeCRC := -1
RETURN Self


METHOD Destroy() CLASS tRPCServeCon
   MutexLock( ::mtxBusy )
   IF ::skRemote != NIL
      InetDestroy( ::skRemote )
      ::skRemote := NIL
   ENDIF
   MutexUnlock( ::mtxBusy )
   DestroyMutex( ::mtxBusy )
RETURN .T.


METHOD Start() CLASS tRPCServeCon
   LOCAL lRet := .F.

   MutexLock( ::mtxBusy )
   IF ::thSelf < 0
      ::thSelf := StartThread( Self, "RUN" )
      lRet := .T.
   ENDIF
   MutexUnlock( ::mtxBusy )

RETURN lRet


METHOD Stop() CLASS tRPCServeCon
   LOCAL lRet := .F.

   MutexLock( ::mtxBusy )
   IF ::thSelf > 0
      StopThread( ::thSelf )
      ::thSelf := -1
      lRet := .T.
   ENDIF
   MutexUnlock( ::mtxBusy )

RETURN lRet


METHOD Run() CLASS tRPCServeCon
   LOCAL cCode := Space( 6 )
   LOCAL lBreak := .F.
   LOCAL aData


   DO WHILE InetErrorCode( ::skRemote ) == 0 .and. .not. lBreak

      /* Get the request code */
      InetRecvAll( ::skRemote, @cCode, 6 )
      IF InetErrorCode( ::skRemote ) != 0
         EXIT
      ENDIF

      DO CASE

         /* Read autorization request */
         CASE cCode == "XHBR90"
            lBreak := .not. ::RecvAuth( .F. )

         /* Read encrypted autorization request */
         CASE cCode == "XHBR93"
            lBreak := .not. ::RecvAuth( .T. )

         /* Challeng reply */
         CASE cCode == "XHBR95"
            lBreak := .not. ::RecvChallenge( )

         /* Close connection */
         CASE cCode == "XHBR92"
            ::oServer:OnClientLogout( Self )
            lBreak := .T.

         /* Execute function */
         CASE cCode == "XHBR20"
            aData := ::RecvFunction( .F., .F. )
            IF .not. Empty( aData )
               lBreak := .not. ::FuncCall( aData[2] )
            ELSE
               lBreak := .T.
            ENDIF

         /* Execute function */
         CASE cCode == "XHBR21"
            aData := ::RecvFunction( .T., .F. )
            IF .not. Empty( aData )
               lBreak := .not. ::FuncCall( aData[2] )
            ELSE
               lBreak := .T.
            ENDIF

         /* Loop function */
         CASE cCode == "XHBR22"
            aData := ::RecvFunction( .F., .T. )
            IF .not. Empty( aData )
               lBreak := .not. ::FuncLoopCall( aData[1], aData[2] )
            ELSE
               lBreak := .T.
            ENDIF

         /* Loop function - compressed */
         CASE cCode == "XHBR23"
            aData := ::RecvFunction( .T., .T. )
            IF .not. Empty( aData )
               lBreak := .not. ::FuncLoopCall( aData[1], aData[2] )
            ELSE
               lBreak := .T.
            ENDIF

         /* Foreach function */
         CASE cCode == "XHBR24"
            aData := ::RecvFunction( .F., .T. )
            IF .not. Empty( aData )
               lBreak := .not. ::FuncForeachCall( aData[1], aData[2] )
            ELSE
               lBreak := .T.
            ENDIF

         /* Foreach function - compressed*/
         CASE cCode == "XHBR25"
            aData := ::RecvFunction( .T., .T. )
            IF .not. Empty( aData )
               lBreak := .not. ::FuncForeachCall( aData[1], aData[2] )
            ELSE
               lBreak := .T.
            ENDIF

         OTHERWISE
            EXIT

      ENDCASE

   ENDDO

   // signaling termination of this thread
   ::oServer:Terminating( Self )
   // Destroy resources just before termination
   ::Destroy()
RETURN .T.


METHOD RecvAuth( lEncrypt ) CLASS tRPCServeCon
   LOCAL cLength := Space(8), nLen, nPos
   LOCAL cUserID, cPassword, cEncId
   LOCAL cReadIn

   IF InetRecvAll( ::skRemote, @cLength, 8 ) != 8
      RETURN .F.
   ENDIF

   nLen := HB_GetLen8( cLength )

   IF (lEncrypt .and. nLen > 128 ) .or. ( .not. lEncrypt .and. nLen > 37 )
      RETURN .F.
   ENDIF

   cReadIn := Space( nLen )
   IF InetRecvAll( ::skRemote, @cReadin, nLen ) != nLen
      RETURN .F.
   ENDIF

   nPos := At( ":", cReadin )
   IF nPos == 0
      RETURN .F.
   ENDIF

   cUserID := Substr(cReadin, 1, nPos-1 )
   cPassword := Substr( cReadin, nPos+1 )

   IF .not. lEncrypt
      ::nAuthLevel := ::oServer:Authorize( cUserid, cPassword )
      IF ::nAuthLevel == 0
         InetSendAll( ::skRemote, "XHBR91NO" )
         RETURN .F.
      ENDIF

      InetSendAll( ::skRemote, "XHBR91OK" )
      IF InetErrorCode( ::skRemote ) != 0
         RETURN .F.
      ENDIF
      ::cUserId := cUserId
      ::oServer:OnClientLogin( Self )
      RETURN .T.
   ENDIF

RETURN ::LaunchChallenge( cUserid, cPassword )


METHOD LaunchChallenge( cUserid, cPassword ) CLASS tRPCServeCon
   LOCAL cChallenge, nCount

   ::cCryptKey := ::oServer:AuthorizeChallenge( cUserid, cPassword )
   IF Empty( ::cCryptKey )
      RETURN .F.
   ENDIF

   ::cChallengeUserid := cUserid

   /* Let's generate the sequence */
   cChallenge := Space( 255 )
   FOR nCount := 1 TO 255
      cChallenge[ nCount ] := Chr( HB_Random(0,255 ) )
   NEXT

   ::nChallengeCRC = HB_Checksum( cChallenge )
   cChallenge := HB_Crypt( cChallenge, ::cCryptKey )

   InetSendAll( ::skRemote, "XHBR94" + HB_CreateLen8( Len( cChallenge ) ) + cChallenge )

   IF InetErrorCode( ::skRemote ) != 0
      RETURN .F.
   ENDIF

RETURN .T.


METHOD RecvChallenge() CLASS tRPCServeCon
   LOCAL cNumber := Space( 8 )
   LOCAL nCount

   IF InetRecvAll( ::skRemote, @cNumber ) != 8
      RETURN .F.
   ENDIF

   IF ::nChallengeCRC != HB_GetLen8( cNumber )
      RETURN .F.
   ENDIF

   InetSendAll( ::skRemote, "XHBR91OK" )
   IF InetErrorCode( ::skRemote ) != 0
      RETURN .F.
   ENDIF

   ::nAuthLevel := ::oServer:Authorize( ::cChallengeUserid )
   /* It is always possible that the user has been deleted in the meanwhile */
   IF ::nAuthLevel == 0
      RETURN .F.
   ENDIF

   ::cUserId := ::cChallengeUserid
   ::bEncrypted := .T.
   ::oServer:OnClientLogin( Self )

RETURN .T.


METHOD RecvFunction( bComp, bMode ) CLASS tRPCServeCon
   LOCAL cLength := Space(8), nLen, nComp
   LOCAL cMode := " "
   LOCAL nBegin, nEnd, nStep
   LOCAL cData
   LOCAL cSer

   /* Original lenght of data */
   IF InetRecvAll( ::skRemote, @cLength, 8 ) != 8
      RETURN .F.
   ENDIF

   nLen := HB_GetLen8( cLength )
   IF nLen > 65000
      RETURN NIL
   ENDIF

   /* compressed lenght */
   IF bComp
      IF InetRecvAll( ::skRemote, @cLength, 8 ) != 8
         RETURN NIL
      ENDIF

      nComp := HB_GetLen8( cLength )
   ELSE
      nComp := nLen
   ENDIF

   /* Mode */
   IF bMode
      IF InetRecvAll( ::skRemote, @cMode ) != 1
         RETURN NIL
      ENDIF
   ENDIF

   /* Get data */
   cData := Space( nComp )
   IF InetRecvAll( ::skRemote, @cData ) != nComp
      RETURN NIL
   ENDIF

   /* Eventually decrypt it */
   IF ::bEncrypted
      cData := ::Decrypt( cData )
   ENDIF

   /* Eventually uncompress it */
   IF bComp
      cData := HB_Uncompress( nLen, cData )
   ENDIF

RETURN { cMode, cData }


METHOD FuncCall( cData ) CLASS tRPCServeCon
   LOCAL cSer, cFuncName, aParams

   /* Deserialize all elements */
   cSer := HB_DeserialBegin( cData )
   cFuncName := HB_DeserialNext( cSer )
   aParams := HB_DeserialNext( cSer )

   IF Empty( aParams )
      RETURN .F.
   ENDIF

   ::oServer:OnClientRequest( Self, 20, { cFuncName, aParams } )
RETURN ::LaunchFunction( cFuncName, aParams, 0 )


METHOD FuncLoopCall( cMode, cData ) CLASS tRPCServeCon
   LOCAL nBegin, nEnd, nStep
   LOCAL cSer
   LOCAL cFuncName, aParams

   /* Deserialize all elements */
   cSer := HB_DeserialBegin( cData )
   nBegin := HB_DeserialNext( cSer )
   nEnd := HB_DeserialNext( cSer )
   nStep := HB_DeserialNext( cSer )
   cFuncName := HB_DeserialNext( cSer )
   aParams := HB_DeserialNext( cSer )

   IF Empty( aParams )
      RETURN .F.
   ENDIF

   ::oServer:OnClientRequest( Self, 22, { cFuncName, aParams, cMode, nBegin, nEnd, nStep } )
RETURN ::LaunchFunction( cFuncName, aParams, 1, { cMode, nBegin, nEnd, nStep } )


METHOD FuncForeachCall( cMode, cData ) CLASS tRPCServeCon
   LOCAL cSer
   LOCAL cFuncName, aParams
   LOCAL aItems

   /* Deserialize all elements */
   cSer := HB_DeserialBegin( cData )
   cFuncName := HB_DeserialNext( cSer )
   aParams := HB_DeserialNext( cSer )
   aItems := HB_DeserialNext( cSer )

   IF Empty( aItems )
      RETURN .F.
   ENDIF

   ::oServer:OnClientRequest( Self, 24, { cFuncName, aParams, aItems } )
RETURN ::LaunchFunction( cFuncName, aParams, 2, { cMode, aItems } )


METHOD LaunchFunction( cFuncName, aParams, nMode, aDesc ) CLASS tRPCServeCon
   LOCAL oFunc, nCount
   LOCAL cOrigLen, cCompLen
   LOCAL oRet, oElem, aRet
   LOCAL aSubst, nSubstPos

   //let's try to run this function.
   oFunc := ::oServer:Find( cFuncName )
   IF Empty(oFunc)
      // signal error
      ::oServer:OnFunctionError( Self,00 )
      InetSendAll( ::skRemote, "XHBR4000" )
      // request socket closing
      RETURN .F.
   ENDIF

   // check for level
   IF oFunc:nAuthLevel > ::nAuthLevel
      // signal error
      ::oServer:OnFunctionError( Self,01 )
      InetSendAll( ::skRemote, "XHBR4001" )
      // request socket closing
      RETURN .F.
   ENDIF

   //check for parameters
   IF Empty( aParams ) .or. .not. oFunc:CheckTypes( aParams )
      // signal error
      ::oServer:OnFunctionError( Self,02 )
      InetSendAll( ::skRemote, "XHBR4002" )
      // request socket closing
      RETURN .F.
   ENDIF

   IF InetErrorCode( ::skRemote ) != 0
      RETURN .F.
   ENDIF

   // allow progress indicator by default
   ::bAllowProgress := .T.

   DO CASE

      CASE nMode == 0  // just run the function
         oRet := oFunc:Run( aParams, Self )

      CASE nMode == 1 // run in loop
         aSubst := AClone( aParams )
         nSubstPos := AScan( aParams, {|x| ValType( x ) == "C" .and. x == "$."} )

         DO CASE
            CASE aDesc[1] == 'A' // all results
               FOR nCount := aDesc[ 2 ] TO aDesc[ 3 ] STEP aDesc[ 4 ]
                  IF nSubstPos > 0
                     aSubst[ nSubstPos ] := nCount
                  ENDIF
                  oRet := oFunc:Run( aSubst, Self )
                  IF .not. ::SendResult( oRet )
                     RETURN .F.
                  ENDIF
               NEXT
               RETURN .T.

            CASE aDesc[1] == 'C' // Vector of all results
               aRet := {}
               ::bAllowProgress = .F.
               FOR nCount := aDesc[ 2 ] TO aDesc[ 3 ] STEP aDesc[ 4 ]
                  IF nSubstPos > 0
                     aSubst[ nSubstPos ] := nCount
                  ENDIF
                  oRet :=  oFunc:Run( aSubst, Self )
                  IF oRet == NIL
                     ::SendResult( NIL )
                     RETURN .F.
                  ENDIF
                  AAdd( aRet, oRet )
               NEXT
               oRet := aRet

            CASE aDesc[1] == 'E' // Just send confirmation at end
               ::bAllowProgress = .F.
               FOR nCount := aDesc[ 2 ] TO aDesc[ 3 ] STEP aDesc[ 4 ]
                  IF nSubstPos > 0
                     aSubst[ nSubstPos ] := nCount
                  ENDIF
                  oRet := oFunc:Run( aSubst, Self )
                  IF oRet == NIL
                     ::SendResult( NIL )
                     RETURN .F.
                  ENDIF
               NEXT
               oRet := "Done"
         ENDCASE

      CASE nMode == 2 // Run in a foreach loop
         aSubst := AClone( aParams )
         nSubstPos := AScan( aParams, {|x| ValType( x ) == "C" .and. x == "$."} )

         DO CASE
            CASE aDesc[1] == 'A' // all results
               FOR EACH oElem IN  aDesc[ 2 ]
                  IF nSubstPos > 0
                     aSubst[ nSubstPos ] := oElem
                  ENDIF
                  oRet := oFunc:Run( aSubst, Self )
                  IF .not. ::SendResult( oRet )
                     RETURN .F.
                  ENDIF
               NEXT
               RETURN .T.

            CASE aDesc[1] == 'C' // Vector of all results
               aRet := {}
               ::bAllowProgress = .F.
               FOR EACH oElem IN  aDesc[ 2 ]
                  IF nSubstPos > 0
                     aSubst[ nSubstPos ] := oElem
                  ENDIF
                  oRet := oFunc:Run( aSubst, Self )
                  IF oRet == NIL
                     ::SendResult( NIL )
                     RETURN .F.
                  ENDIF
                  AAdd( aRet, oRet )
               NEXT
               oRet := aRet

            CASE aDesc[1] == 'E' // Just send confirmation at end
               ::bAllowProgress = .F.
               FOR EACH oElem IN aDesc[ 2 ]
                  IF nSubstPos > 0
                     aSubst[ nSubstPos ] := oElem
                  ENDIF
                  oRet := oFunc:Run( aSubst, Self )
                  IF oRet == NIL
                     ::SendResult( NIL )
                     RETURN .F.
                  ENDIF
               NEXT
               oRet := "Done"
         ENDCASE
   ENDCASE

// Default return
RETURN ::SendResult( oRet )


METHOD SendResult( oRet )
   LOCAL cData, cOrigLen, cCompLen

   IF oRet == NIL
      ::oServer:OnFunctionError( Self, 10 )
      InetSendAll( ::skRemote, "XHBR4010" )
      RETURN .F.
   ELSE
      cData := HB_Serialize( oRet )
      cOrigLen := HB_CreateLen8( Len( cData ) )
      ::oServer:OnFunctionReturn( Self, cData )
      // should we compress it ?

      IF Len( cData ) > 512
         cData := HB_Compress( cData )
         cCompLen := HB_CreateLen8( Len( cData ) )
         InetSendAll( ::skRemote, "XHBR31" + cOrigLen + cCompLen + ::Encrypt( cData ) )
      ELSE
         InetSendAll( ::skRemote, "XHBR30" + cOrigLen + ::Encrypt( cData ) )
      ENDIF
   ENDIF

   IF InetErrorCode( ::skRemote ) != 0
      RETURN .F.
   ENDIF

RETURN .T.


METHOD SendProgress( nProgress, oData ) CLASS tRPCServeCon
   LOCAL cOrigLen, cCompLen, lRet := .T.
   LOCAL cData

   //Ignore if told so
   IF .not. ::bAllowProgress
      RETURN .T.
   ENDIF

   ::oServer:OnFunctionProgress( Self, nProgress, oData )
   IF Empty( oData )
      InetSendAll( ::skRemote, "XHBR33" + HB_Serialize( nProgress ) )
   ELSE
      cData := HB_Serialize( oData )
      cOrigLen := HB_CreateLen8( Len( cData ) )
      // do we should compress it ?
      IF Len( cData ) > 512
         cData := HB_Compress( cData )
         cCompLen := HB_CreateLen8( Len( cData ) )
         InetSendAll(::skRemote, "XHBR35" + HB_Serialize( nProgress ) +;
                cOrigLen + cCompLen + ::Encrypt( cData ) )
      ELSE
         InetSendAll( ::skRemote, "XHBR34" + HB_Serialize( nProgress ) +;
               cOrigLen + ::Encrypt( cData ) )
      ENDIF
   ENDIF

   IF InetErrorCode( ::skRemote ) != 0
      lRet := .F.
   ENDIF

RETURN lRet


METHOD Encrypt(cDataIn) CLASS tRPCServeCon
   IF ::bEncrypted
      RETURN HB_Crypt( cDataIn, ::cCryptKey )
   ENDIF
RETURN cDataIn


METHOD Decrypt(cDataIn) CLASS tRPCServeCon
   IF ::bEncrypted
      RETURN HB_Decrypt( cDataIn, ::cCryptKey )
   ENDIF
RETURN cDataIn

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

   /* Code blocks corresponding to event handlers */
   DATA bAuthorize
   DATA bGetEncryption
   DATA bOnFunctionScan
   DATA bOnServerScan
   DATA bOnClientConnect
   DATA bOnClientLogin
   DATA bOnClientRequest
   DATA bOnFunctionProgress
   DATA bOnFunctionError
   DATA bOnFunctionReturn
   DATA bOnClientLogout
   DATA bOnClientTerminate

   METHOD New() CONSTRUCTOR

   /* Block run on client connection request */
   DATA bConnection

   /* Function management */
   METHOD Add( oFunction )
   METHOD Run( cName, aParams )
   METHOD Describe( cName )
   METHOD Find( cName )
   METHOD Remove( cName )

   /* General services */
   METHOD Start( lStartUdp )
   METHOD Stop()
   METHOD StartService( skIn )
   METHOD Terminating( oConnection )

   /* Tcp services */
   METHOD Accept()

   /* UDP services */
   METHOD UdpListen()
   METHOD UDPParseRequest()

   /* Utility */
   METHOD AuthorizeChallenge( cUserid, cPassword )

   /* to be overloaded */
   METHOD Authorize( cUserid, cPassword )
   /* Provide encryption key for a user */
   METHOD GetEncryption( cUserId )
   METHOD OnFunctionScan()
   METHOD OnServerScan( )
   METHOD OnClientConnect( oClient )
   METHOD OnClientLogin( oClient )
   METHOD OnClientRequest( oClient, nRequest, cData )
   METHOD OnFunctionProgress( oClient, nProgress, aData )
   METHOD OnFunctionError( oClient, nError )
   METHOD OnFunctionReturn( oClient, aData )
   METHOD OnClientLogout( oClient )
   METHOD OnClientTerminate( oClient )

ENDCLASS


METHOD New() class tRPCService
   ::aFunctions := {}
RETURN Self


METHOD Add( oFunction )
   LOCAL nElem, lRet := .F.

   MutexLock( ::mtxBusy )
   nElem := AScan( ::aFunctions, {|x| oFunction:cName == x:cName})
   IF nElem == 0g
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
      StopThread( oElem:thSelf )
      InetDestroy( oElem:skRemote )
   NEXT
   ASize( ::aServing, 0 )

   MutexUnlock( ::mtxBusy )

RETURN .T.


METHOD Accept() CLASS tRPCService
   LOCAL skIn

   DO WHILE .T.
      skIn := InetAccept( ::skServer )
      // todo: better sync
      ::StartService( skIn )
   ENDDO
RETURN .T.


METHOD StartService( skIn ) CLASS tRPCService
   LOCAL oService

   MutexLock( ::mtxBusy )
   oService := tRpcServeCon():New( Self, skIn )
   AAdd( ::aServing, oService )
   oService:Start()
   MutexUnlock( ::mtxBusy )
   ::OnClientConnect( oService )
RETURN .T.

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
         IF .not. ::OnServerScan()
            RETURN .F.
         ENDIF
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
         IF .not. ::OnFunctionScan()
            RETURN .F.
         ENDIF
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


METHOD Terminating( oConnection ) CLASS tRPCService
   LOCAL nToken

   ::OnClientTerminate( oConnection )
   MutexLock( ::mtxBusy )
   nToken := AScan( ::aServing, {|x| x == oConnection } )
   IF nToken > 0
      ADel( ::aServing, nToken )
      ASize( ::aServing, Len( ::aServing ) -1 )
   ENDIF
   MutexUnlock( ::mtxBusy )
RETURN .T.


METHOD AuthorizeChallenge( cUserId, cData ) CLASS tRPCService
   LOCAL cKey, nPos, cMarker := "PASSWORD:"

   cKey := ::GetEncryption( cUserId )
   IF Empty( cKey )
      RETURN NIL
   ENDIF

   cData := HB_Decrypt( cData, cKey )
   nPos := At( cMarker, cData )
   IF nPos == 0
      RETURN NIL
   ENDIF

   cData := Substr( cData, nPos + Len( cMarker ) )
   nPos := At( ":", cData )
   IF nPos == 0
      RETURN NIL
   ENDIF

   cData := Substr( cData, 1, nPos - 1 )

   IF ::Authorize( cUserId, cData ) > 0
      RETURN cKey
   ENDIF
RETURN NIL

/* Default authorization will ALWAYS return 1 if a bAuthorize block is not provided */
/* IF cPassword is NIL, must return the level of the given userid */
METHOD Authorize( cUserid, cPassword ) CLASS tRPCService
   IF ::bAuthorize != NIL
      RETURN Eval( ::bAuthorize, cUserid, cPassword )
   ENDIF
RETURN 1

/* By default, do not provide an encryption key for any user */
METHOD GetEncryption( cUserId ) CLASS tRPCService
   IF ::bGetEncryption != NIL
      RETURN Eval( ::bGetEncryption, cUserId )
   ENDIF
RETURN NIL

METHOD OnFunctionScan() CLASS tRPCService
   IF ::bOnFunctionScan != NIL
      RETURN Eval( ::bOnFunctionScan, Self )
   ENDIF
RETURN .T.

METHOD OnServerScan() CLASS tRPCService
   IF ::bOnServerScan != NIL
      RETURN Eval( ::bOnServerScan, Self )
   ENDIF
RETURN .T.

METHOD OnClientConnect( oClient ) CLASS tRPCService
   IF ::bOnClientConnect != NIL
      RETURN Eval( ::bOnClientConnect, oClient )
   ENDIF
RETURN .T.

METHOD OnClientLogin( oClient ) CLASS tRPCService
   IF ::bOnClientLogin != NIL
      Eval( ::bOnClientLogin, oClient )
   ENDIF
RETURN .T.

METHOD OnClientRequest( oClient, nRequest, cData ) CLASS tRPCService
   IF ::bOnClientRequest != NIL
      RETURN Eval( ::bOnClientRequest, oClient, nRequest, cData )
   ENDIF
RETURN .T.

METHOD OnFunctionProgress( oClient, nProgress, aData ) CLASS tRPCService
   IF ::bOnFunctionProgress != NIL
      RETURN Eval( ::bOnFunctionProgress, oClient, nProgress, aData )
   ENDIF
RETURN .T.

METHOD OnFunctionError( oClient, nError ) CLASS tRPCService
   IF ::bOnFunctionError != NIL
      RETURN Eval( ::bOnFunctionError, nError )
   ENDIF
RETURN .T.

METHOD OnFunctionReturn( oClient, aData ) CLASS tRPCService
   IF ::bOnFunctionReturn != NIL
      RETURN Eval( ::bOnFunctionReturn, oClient, aData )
   ENDIF
RETURN .T.

METHOD OnClientLogout( oClient ) CLASS tRPCService
   IF ::bOnClientLogout != NIL
      RETURN Eval( ::bOnClientLogout, oClient )
   ENDIF
RETURN .T.

METHOD OnClientTerminate( oClient ) CLASS tRPCService
   IF ::bOnClientTerminate != NIL
      RETURN Eval( ::bOnClientTerminate, oClient )
   ENDIF
RETURN .T.
