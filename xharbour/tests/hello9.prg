//
// $Id:$
//
// Sample progam where no PRG codes are used
//
// Andi Jahja <xharbour@cbn.net.id>
//

#PRAGMA BEGINDUMP

#include "hbapi.h"
#include "hbver.ch"

// REQUEST HB_BUILDINFO /* Without this request program will fail */

HB_FUNC( MAIN ) // NO PRG MODULE
{
   /* REQUEST can be written anywhere in dump area */
   EXTERNAL HB_BUILDINFO /* Without this request program will fail */

   PHB_ITEM pSymbol;
   HB_ITEM  pInfo;
   (&pInfo)->type = HB_IT_NIL;

   hb_itemPutNI( &pInfo, _HB_VER_AS_STRING );

   pSymbol = hb_itemDoC("HB_BUILDINFO",1,&pInfo);
   hb_itemClear( &pInfo );

   if( pSymbol != NULL )
   {
      printf("%s\n",pSymbol->item.asString.value);
      hb_itemRelease( pSymbol );
   }
   else
      printf("HB_BUILDINFO does not exist in symbol table\nPlease do a REQUEST\n");

}
   /* REQUEST can be written anywhere in dump area
      EXTERNAL HB_BUILDINFO
   */
#PRAGMA ENDDUMP
