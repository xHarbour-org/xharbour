//
// $Id:$
//
// Sample progam where no PRG codes are used
//
// Andi Jahja <xharbour@cbn.net.id>
//
// Typical welcome message

#pragma begindump
#include "hbapi.h"
HB_FUNC( MAIN )
{
   PHB_ITEM pChild = hb_itemDoC( "TEST", 0 , 0 );

   if( pChild )
      hb_itemRelease( pChild );
   else
      printf( "Failed ...\nCannot hb_itemDoC() with static functions\n");
}

HB_FUNC_STATIC( TEST )
{
   printf( "Hello world!" );
}
#pragma enddump
