//
// $Id:$
//
// Sample progam where no PRG codes are used
//
// Andi Jahja <xharbour@cbn.net.id>
//
// Typical welcome message

STATIC PROCEDURE TEST( cTEXT )
   IF PCount() == 0
      OutStd( "Hello World from TEST()" + hb_osnewline() )
   ELSE
      OutStd( cTEXT  + hb_osnewline() )
   ENDIF
   RETURN

#pragma begindump
#include "hbapi.h"
HB_FUNC ( MAIN )
{
   HB_ITEM pText;
   PHB_ITEM pChild;

   (&pText)->type = HB_IT_NIL;
   hb_itemPutC( &pText, "Hello World from MAIN()" );
   pChild = hb_itemDoC( "TEST", 1 , &pText );
   hb_itemClear( &pText );

   if( pChild )
      hb_itemRelease( pChild );
   else
      printf( "Failed ...\n");

   HB_FUNC_EXEC( TEST );
}
#pragma enddump
