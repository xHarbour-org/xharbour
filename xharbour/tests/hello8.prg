//
// $Id:$
//
// Andi Jahja <xharbour@cbn.net.id>
//
// No PRG code, more tests
//

#PRAGMA BEGINDUMP
#include "hbapi.h"

HB_FUNC( MAIN )
{
  PHB_ITEM pWait;
  HB_ITEM pnWait;

  (&pnWait)->type = HB_IT_NIL;
  hb_itemPutNI( &pnWait, 0 );

  printf( "Hello world!\nPress any key ....\n");
  pWait = hb_itemDoC( "INKEY", 1, &pnWait );

  HB_FUNC_EXEC( TESTBOX );

  if( pWait )
  {
     hb_itemRelease( pWait );
  }

  hb_itemClear( &pnWait );
}

HB_FUNC_STATIC( TESTBOX )
{
   HB_ITEM pTop, pLeft, pRight, pBottom, pBoxChar, pColor, pText;
   PHB_ITEM pScroll, pBox, pAlert;

   (&pText)->type = HB_IT_NIL;
   (&pTop)->type = HB_IT_NIL;
   (&pLeft)->type = HB_IT_NIL;
   (&pRight)->type = HB_IT_NIL;
   (&pBottom)->type = HB_IT_NIL;
   (&pColor)->type = HB_IT_NIL;
   (&pBoxChar)->type = HB_IT_NIL;

   REQUEST SCROLL /* a must */

   pScroll = hb_itemDoC( "SCROLL", 0, 0 );

   if( pScroll )
   {
      hb_itemRelease( pScroll );
      hb_itemPutC( &pText, "SCROLL operation success ! ");
      pAlert = hb_itemDoC( "ALERT", 1, &pText );
      hb_itemClear( &pText );
      hb_itemRelease( pAlert );
   }
   else
   {
      hb_itemPutC( &pText, "SCROLL operation failed ! ");
      pAlert = hb_itemDoC( "ALERT", 1, &pText );
      hb_itemClear( &pText );
      hb_itemRelease( pAlert );
   }

   REQUEST DISPBOX

   hb_itemPutNI( &pTop,     5 );
   hb_itemPutNI( &pLeft,    10 );
   hb_itemPutNI( &pRight,   70 );
   hb_itemPutNI( &pBottom,  20 );
   hb_itemPutC(  &pColor,   "W/B+" );
   hb_itemPutC(  &pBoxChar, "ÚÄ¿³ÙÄÀ³ " );

   pBox = hb_itemDoC( "DISPBOX", 6, &pTop, &pLeft, &pBottom, &pRight, &pBoxChar, &pColor );

   if( pBox )
   {
      hb_itemRelease( pBox );
      hb_itemPutC( &pText, "DISPBOX operation success ! ");
      pAlert = hb_itemDoC( "ALERT", 1, &pText );
      hb_itemClear( &pText );
      hb_itemRelease( pAlert );
   }
   else
   {
      hb_itemPutC( &pText, "DISPBOX operation failed ! ");
      pAlert = hb_itemDoC( "ALERT", 1, &pText );
      hb_itemClear( &pText );
      hb_itemRelease( pAlert );
   }

   hb_itemClear( &pTop    );
   hb_itemClear( &pLeft   );
   hb_itemClear( &pRight  );
   hb_itemClear( &pBottom );
   hb_itemClear( &pColor  );
   hb_itemClear( &pBoxChar);
}
#PRAGMA ENDDUMP
