GLOBAL MyGlobal1, MyGlobal3 := 77
GLOBAL EXTERNAL MyGlobal2

Procedure Global1()

   ? "In Global1() Before assignment:", MyGlobal1, MyGlobal2, MyGlobal3

   MyGlobal1 := ProcName()
   MyGlobal2 := ProcName()

   Global2( {|| MyGlobal3 }, MyGlobal3 )

   ? "In Global1() After Global2():", MyGlobal1, MyGlobal2, MyGlobal3

   MyCFunc()

   ? "In Global1() After MyCFun():", MyGlobal1, MyGlobal2, MyGlobal3

Return

#pragma BEGINDUMP

#include "hbfast.h"

HB_FUNC( MYCFUNC )
{
   printf( "\nIn MyCFunc() Before assigment: %s, %s", MYGLOBAL1.item.asString.value, MYGLOBAL1.item.asString.value );

   hb_itemPutCStatic( &MYGLOBAL1, "MyCFunc" );
   hb_itemPutCStatic( &MYGLOBAL2, "MyCFunc" );
}
#pragma ENDDUMP
