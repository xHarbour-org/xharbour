//
// $Id:$
//
// Sample progam where no PRG codes are used
//
// Andi Jahja <xharbour@cbn.net.id>
//
// Typical welcome message

PROCEDURE MAIN()

   TEST()
   TEST_01()
   RETURN

#pragma begindump
#include "hbapi.h"
HB_FUNC_STATIC ( TEST )
{
   HB_FUNC_EXEC( TEST_01 );
}

HB_FUNC_STATIC ( TEST_01 )
{
   printf("Hello world!\n");
}

HB_FUNC( AGAIN )
{
   EXTERNAL ALERT
}
#pragma enddump
