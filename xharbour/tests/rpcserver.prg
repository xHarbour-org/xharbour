************************************************************
* rpcserver.prg
* $Id: rpcserver.prg,v 1.1 2003/02/16 03:03:45 jonnymind Exp $
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

   // creating the procedure: we use a new class
   // serial is today: 20030215
   // return is c:10
   // Param list is just a neverending C:0
   // Auth level is 1 (anyone that has logged in)
   oProc := tRPCFunctionTest():New( "Checksum","20030215.A", "C:10", { "C:0" }, 1  )
   oSv := tRPCService():New()
   oSv:cServerName := "CksumTest"
   oSv:Add( oProc )

   // server is starting
   oSv:Start( .T. )
   @3,10 SAY "Waiting for connection"
   @4,10 SAY "Press any key to stop"
   Inkey(0) // we have nothing more to do here.
   oSv:Stop()
RETURN


CLASS tRpcFunctionTest from tRpcFunction
   // You just need to overrun the RUN method
   Method Run( aParams, skRemote )
   // the socket is needed only if you want to give a progress indicator
ENDCLASS


METHOD Run( aParams, skRemote ) class tRpcFunctionTest
   LOCAL nSum, i

   // signal that function is starting (not necessary, just for test)
   ::SendProgress( skRemote, 0 )

   IF .not. ::CheckTypes( aParams )
      RETURN NIL
   ENDIF

   nSum := 0
   FOR i := 1 to Len( aParams[1] )
      nSum += asc(aParams[1][i])
      // signal a progress each 50 characters
      IF i % 50 == 0
         ::SendProgress( skRemote, i / Len( aParams[1] ) * 100, Str(nSum, 10 ) )
         // simulate some burdensome operation
         ThreadSleep( 200 )
      ENDIF
   NEXT

RETURN Str(nSum, 10)


