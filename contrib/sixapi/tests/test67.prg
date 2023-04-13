/*
 * $Id$
 */
/*
   test67.prg
   sx_CopyFile( cFileToCopy, cAlias)
*/
#include "sixapi.ch"
#define EOL chr(10)
#command ? => outstd(EOL)
#command ? <xx,...> => outstd(<xx>, EOL)
 
PROCEDURE MAIN()

   LOCAL cCopy := "mynew.dbf"
   LOCAL lResult

   USE "test/test"

   ? 'lResult := sx_CopyFile( cCopy ) =>', lResult := sx_CopyFile( cCopy )
   IF lResult
      ? 'Copy success ... Press any key ...'
      PAUSE
      CLOSE ALL
      USE "mynew.dbf"
      BROWSE
   ENDIF
