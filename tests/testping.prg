//
// $Id$
//

// Test program for PING

// Syntax: HB_PING( cHost, @cResult, iPacketSize, iTimeOut, @cAddress, @cHostName, @iRTT )
//   cHost       = string, hostname of IP address to query
//   cResult     = passed by ref string, string to hold ping response
//   iPacketSize = integer, size of data to send, default 32
//   iTimeOut    = integer, response waiting time, default 5000ms
//   cAddress    = passed by ref string, string to host address as replied
//   cHostName   = passed by ref string, string to host name as replied
//   iRTT        = passed by ref integer, is Round Trip Time

#include "simpleio.ch"

PROCEDURE MAIN( cHostName, nPacket, nTimeOut )

   LOCAL sz
   LOCAL bResponse

   LOCAL cAddress
   LOCAL cName
   LOCAL iRTT

   IF Empty( cHostName )
      cHostName := "yahoo.com"
   ENDIF

   IF Empty( nPacket )
      nPacket := "32"
   ENDIF

   IF Empty( nTimeOut )
      nTimeOut := "5000"
   ENDIF

   bResponse := HB_PING( cHostName, @sz, Val( nPacket ), Val( nTimeOut ), @cAddress, @cName, @iRTT  )

   ? "HB_PING() packet    :", nPacket
   ? "HB_PING() timeout   :", nTimeOut
   ? "HB_PING() returns   :", bResponse
   ? "HB_PING() IP-Address:", cAddress
   ? "HB_PING() Host Name :", cName
   ? "HB_PING() RTT       :", IF( iRTT!=NIL,LTRIM(STR(iRTT)) + "ms",NIL)
   ? "HB_PING() message:", sz

   RETURN
