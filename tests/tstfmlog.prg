*
* $Id$
*
* tstfmlog.prg
* testing log file of unreleased memory block
* file created is fm.log
*
* only works when HB_FM_STATISTICS is defined
*
* Andi Jahja
*
//---------------------------------
proc main()
foo()
bar()

//---------------------------------
#PRAGMA BEGINDUMP
#include "hbapi.h"
HB_FUNC( FOO )
{
   char *sz = (char*) hb_xgrab( 42 );
   strcpy( sz,"THIS IS INTENTIONALLY NOT FREED FROM FOO()" );
}

HB_FUNC( BAR )
{
   char *sz = (char*) hb_xgrab( 51 );
   strcpy( sz,"AND THIS IS ALSO INTENTIONALLY NOT FREED FROM BAR()" );
}
#PRAGMA ENDDUMP
