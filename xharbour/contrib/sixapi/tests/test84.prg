/*
 * $Id$
 */
/*
   test84.prg
   sx_Query(), sx_QueryTest(), sx_QueryRecCount()
*/
#include "sixapi.ch"
#define EOL chr(10)
#command ? => outstd(EOL)
#command ? <xx,...> => outstd(<xx>, EOL)

PROCEDURE MAIN()

   USE "TEST/TEST"
   ? 'Creating index ...'
   IF FILE( "TEST.NSX" )
      FERASE( "TEST.NSX" )
   ENDIF
   INDEX ON LAST TO TEST
   ? 'Making Query ...'
   ? [sx_QueryTest( "LAST LIKE '%B%'" ) =], sx_QueryTest( "LAST LIKE '%B%'" )
   ? [sx_Query( "LAST LIKE '%B%'" ) =], sx_Query( "LAST LIKE '%B%'" )
   ? [sx_QueryRecCount() =], sx_QueryRecCount()
   ? 'Now Browse .. Press any key ...'
   PAUSE
   BROWSE
   CLS
   ?
   ? 'Now clear the Query and BROWSE ... Press any key ...'
   PAUSE
   sx_QueryClear()
   BROWSE
