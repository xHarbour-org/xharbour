/*
 * $Id$
 */
/*
   test68.prg
   sx_Count()
*/
#include "sixapi.ch"
#define EOL chr(10)
#command ? => outstd(EOL)
#command ? <xx,...> => outstd(<xx>, EOL)
 
PROCEDURE MAIN()

   USE "test/test"
   ? 'sx_Count() =', sx_Count()
