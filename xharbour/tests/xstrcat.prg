* $Id$
*
* test program for hb_xstrcat()
* not very useful in prg level but may be handy in c level
*
* Andi Jahja
*

//---------------
proc main()
local str := testxstrcat( version(), " ", hb_compiler(), " ", os() )
OutStd( str )

//---------------
#PRAGMA BEGINDUMP
#include "hbapi.h"
HB_FUNC( TESTXSTRCAT )
{
   char *szResult = (char*) hb_xgrab( 256 );
   hb_xmemset( szResult, 0, 256 );
   /* must end string merging with NULL */
   hb_xstrcat( szResult, hb_parc(1), hb_parc(2), hb_parc(3), hb_parc(4), NULL );
   hb_retc( szResult );
   hb_xfree( szResult );
}
#PRAGMA ENDDUMP
