************************************************************
* rpcserver.prg
* $Id: rpcserver.prg,v 1.8 2003/09/23 15:16:21 jonnymind Exp $
* Test for tRpcServer and tRpcFunction class
*
* YOU NEED THREADS TO RUN THIS
*
* This demonstrates how to use create a demo RPC function
* (with its own server). The function is "Checksum", doing
* a BYTE sum on a given string.
*
* Giancarlo Niccolai
*

#include "hbclass.ch"

PROCEDURE Main()
   LOCAL oSv

   CLEAR SCREEN
   @1,15 SAY "X H A R B O U R - Remote Procedure Call server test."

   oSv := tRPCService():New()
   oSv:cServerName := "CksumTest"
   oSv:Add( "Checksum(C:0)-->C:10", "20030201.A", 1, @CheckSum() )
   oSv:bOnServerScan := {|x| Scans( x ) }
   oSv:bOnFunctionScan := {|x| Scanf( x ) }
   oSv:bOnFunctionError := {|x,y,z| FuncError( x, y, z ) }
   oSv:bOnClientConnect := {|x| Connecting( x ) }
   oSv:bOnClientLogin := {|x| Entering( x ) }
   oSv:bOnClientLogout := {|x| Exiting( x ) }
   oSv:bOnClientTerminate := {|x| Terminating( x ) }
   oSv:bGetEncryption := {|x| HaveEncryptionKey( x ) }

   // server is starting
   oSv:Start( .T. )
   @3,10 SAY "Waiting for connection"
   @4,10 SAY "Press any key to stop"
   Inkey(0) // we have nothing more to do here.
   //ThreadSleep( 60000 )
   @20,10 SAY "Terminating"
   oSv:Stop()
   @21,10 SAY "QUIT"
   @22,10
RETURN

FUNCTION CheckSum( cData, oClient )
   LOCAL nSum, i

   // signal that function is starting (not necessary, just for test)
   oClient:SendProgress( 0 )

   nSum := 0
   FOR i := 1 to Len( cData )
      nSum += asc(cData[i])
      // signal a progress each 50 characters
      IF i % 50 == 0
         oClient:SendProgress( i / Len( cData ) * 100, Str(nSum, 10 ) )
         // simulate some burdensome operation
         ThreadSleep( 200 )
      ENDIF
   NEXT

RETURN Str(nSum, 10)

FUNCTION Scanf( oServer )
   @8, 10 SAY "Function scanning from " + InetAddress( oServer:skUDP )+ "         "
RETURN .T.

FUNCTION Scans( oServer )
   @9, 10 SAY "Server scanning from " + InetAddress( oServer:skUDP )+ "         "
RETURN .T.


FUNCTION Connecting( oClient )
   @10, 10 SAY "Serving connection from " + InetAddress( oClient:skRemote )
RETURN .T.

FUNCTION Entering( oClient )
   @11, 10 SAY "Client " + oClient:cUserID  + " has entered         "
RETURN .T.

FUNCTION Exiting( oClient )
   @12, 10 SAY "Client " + oClient:cUserID  + " has logged out         "
RETURN .T.

FUNCTION Terminating( oClient )
   IF .not. Empty( oClient:cUserID )
      @13, 10 SAY "Client " + oClient:cUserID  + " has terminated operations         "
   ELSE
      @13, 10 SAY "A client failed authentication                              "
   ENDIF
RETURN .T.

FUNCTION FuncError( oServer, cFunc, nError )
   ? "ERROR in function call: ", InetAddress( oServer:skTCP ), nError, cFunc
RETURN .T.

/* This function receives also a parameter: the userid for which the
key is requested */
FUNCTION HaveEncryptionKey()
RETURN "A nice key to be used by servers"

