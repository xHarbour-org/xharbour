* $Id$
*
* test program for hb_xstrcpy()
* not very useful in prg level but may be handy in c level
*
* Andi Jahja
*

//---------------
proc main()

local str  := testxstrcpy1( version(), " ", hb_compiler(), " ", os() )
local str1 := testxstrcpy2( version(), " ", hb_compiler(), " ", os() )

OutStd( str )
OutStd( hb_osnewline() )
OutStd( str1 )

//---------------
#PRAGMA BEGINDUMP
#include "hbapi.h"

HB_FUNC( TESTXSTRCPY1 )
{
   /* this method is preferable since szResult will be dinamically allocated */
   char *szResult;
   /* must end string merging with NULL */
   szResult = hb_xstrcpy( NULL, hb_parc(1), hb_parc(2), hb_parc(3), hb_parc(4), NULL );
   /* in c level, szResult has to be freed after usage */
   hb_retcAdopt( szResult );
}

HB_FUNC( TESTXSTRCPY2 )
{
   char szResult[256]; /* be very careful to not to be under-flow */
   /* must end string merging with NULL */
   hb_xstrcpy( szResult, hb_parc(1), hb_parc(2), hb_parc(3), hb_parc(4), NULL );
   hb_retc( szResult );
}
#PRAGMA ENDDUMP
