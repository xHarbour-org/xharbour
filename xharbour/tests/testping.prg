//
// $Id$
//

// Test program for PING

#include "simpleio.ch"

PROCEDURE MAIN( cHostName )

   LOCAL sz

   IF Empty( cHostName )
      cHostName := "yahoo.com"
   ENDIF

   HB_PING( cHostName, @sz )
   ? sz

   RETURN
