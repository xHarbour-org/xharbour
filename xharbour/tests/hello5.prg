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
   RETURN

#pragma begindump
#include "hbapi.h"
HB_FUNC_STATIC ( TEST )
{
   PHB_ITEM pChild = hb_itemDoC( "TEST_01", 0 , 0 );

   if( pChild )
      hb_itemRelease( pChild );
   else
      printf( "Failed!\nCannot hb_itemDoC() with static functions\n");
}

HB_FUNC_STATIC ( TEST_01 )
{
   printf("Hello world!\n");
}
#pragma enddump
