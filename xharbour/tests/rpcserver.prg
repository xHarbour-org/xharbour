************************************************************
* rpcserver.prg
* $Id: rpcserver.prg,v 1.6 2003/04/16 22:08:13 jonnymind Exp $
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
   LOCAL oProc, params
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
   oSv:Stop()
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

PROCEDURE Scanf( oServer )
   @8, 10 say "Function scanning from " + InetAddress( oServer:skUDP )+ "         "
RETURN .T.

PROCEDURE Scans( oServer )
   @9, 10 say "Server scanning from " + InetAddress( oServer:skUDP )+ "         "
RETURN .T.


PROCEDURE Connecting( oClient )
   @10, 10 say "Serving connection from " + InetAddress( oClient:skRemote )
RETURN .T.

PROCEDURE Entering( oClient )
   @11, 10 say "Client " + oClient:cUserID  + " has entered         "
RETURN .T.

PROCEDURE Exiting( oClient )
   @12, 10 say "Client " + oClient:cUserID  + " has logged out         "
RETURN .T.

PROCEDURE Terminating( oClient )
   IF .not. Empty( oClient:cUserID )
      @13, 10 say "Client " + oClient:cUserID  + " has terminated operations         "
   ELSE
      @13, 10 say "A client failed authentication                              "
   ENDIF
RETURN .T.

PROCEDURE FuncError( oServer, cFunc, nError )
   ? "ERROR in function call: ", nError, cFunc
RETURN

PROCEDURE HaveEncryptionKey( cUserId )
RETURN "A nice key to be used by servers"

