//
// $Id:$
//
// Andi Jahja <andijahja@xharbour.com>
//
// GLOBALs from within dump area
// Must link with global4.prg

#pragma begindump
#include "hbapi.h"

GLOBAL MYGLOBAL, MYGLOBALX
GLOBAL EXTERNAL FROMGLOBAL4
// No initialization please, because variables are not passed to lexer

HB_FUNC( MAIN )
{
   hb_itemPutC( &MYGLOBALX,"INITIATED FROM MAIN");
   printf( "MYGLOBALX=%s\n",(&MYGLOBALX)->item.asString.value);

   HB_FUNC_EXEC( MYTEST );

   printf( "MYGLOBAL=%s\n",(&MYGLOBAL)->item.asString.value);
   printf( "FROMGLOBAL4=%s\n",(&FROMGLOBAL4)->item.asString.value);
}
#pragma enddump
