//
// $Id:$
//
// Andi Jahja <xharbour@cbn.net.id>
//
// Testing for proper 'parsing' of HB_FUNCs
//

#PRAGMA BEGINDUMP
#include "hbapi.h"
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

   pScroll = hb_itemDoC( "CLEAR", 0, 0 );

   if( pScroll )
   {
      hb_itemRelease( pScroll );
      hb_itemPutC( &pText, "CLEAR operation success ! ");
      pAlert = hb_itemDoC( "ALERT", 1, &pText );
      hb_itemClear( &pText );
      hb_itemRelease( pAlert );
   }
   else
   {
      hb_itemPutC( &pText, "CLEAR operation failed ! ");
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

PROCEDURE MAIN()

  ? "Hello world!"
  ?
  OutStd( "Press any key ..." )
  inkey(0)
  TestBox()
  RETURN

#PRAGMA BEGINDUMP
#include "hbapi.h"

HB_FUNC_EXTERN ( HB_BUILDINFO );
EXTERNAL DBFCDX, DBFFPT
HB_FUNC_PTR pPtr;
PHB_FUNC pFunc;

HB_FUNC( CLEAR )
{
   HB_FUNCNAME( SCROLL )();
}

HB_FUNC_STATIC( TEST_ME )
{
   hb_retni( 0 );
}
#PRAGMA ENDDUMP
