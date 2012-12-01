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

   IF HB_PING( cHostName, @sz )
      ? sz
   ENDIF

   RETURN
