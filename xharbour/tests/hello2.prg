//
// $Id:$
//
// Andi Jahja <xharbour@cbn.net.id>
//
// Test program for starting procedure
//

STATIC PROCEDURE MYSTATIC() // Static function should never be startup
   ? "MYSTATIC()"
   RETURN		    // eventhough it is on the toppest of the module

STATIC FUNCTION MYSTATIC_01()
   ? "MYSTATIC_01()"
   RETURN NIL

#pragma begindump
#include "hbapi.h"
HB_FUNC( MAIN ) { printf("MAIN()\n"); } // ___FIRST PUBLIC___
                                        // ___SHOULD BECOME STARTUP___
HB_FUNC( TEST_02 ) {}
#pragma enddump

PROCEDURE MAIN_PRG()   // prg code but not the first public symbol
   ? "MAIN_PRG()"      // should not become startup procedure
   RETURN	       // eventhough it is the first of prg sub-modules

PROCEDURE MAIN_PRG_01()
   ? "MAIN_PRG_01()"
   RETURN

#pragma begindump
#include "hbapi.h"
HB_FUNC( MAIN_01 ) { printf("MAIN_01()\n"); }
HB_FUNC( TEST_01 ) {}
#pragma enddump
