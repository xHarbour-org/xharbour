/*
   $Id$
   Test program with 7Zip Compression
   WARNING: Must link SEVENZIP.LIB
*/

#include "simpleio.ch"

#define ARCTYPE_7Z    1
#define ARCTYPE_ZIP   2

PROCEDURE MAIN()

   LOCAL my7z

   IF File( "mytest.7z" )
      FErase( "mytest.7z" )
   ENDIF

   IF File( "mytest.zip" )
      FErase( "mytest.zip" )
   ENDIF

   WITH OBJECT my7z := T7Zip():New()

      // supposed to be run in tests folder
      // pls change aFiles if required
      :aFiles     := "..\source\*.*"
      :cPassword  := "xharbour"
      :lRecursive := .T. /* .T. = include sub-dirs */

      :cArcName := "mytest.7z"
      :nArctype := ARCTYPE_7Z
      :Create()
      ? '[ARCTYPE_7Z] command:', :cCommand
      ? '[ARCTYPE_7Z] RETURN ERROR:', :nError, :ErrorDescription
      ?

      :cArcName := "mytest.zip"
      :nArctype := ARCTYPE_ZIP
      // nZipCompressionLevel valid 0 - 9, the higher the more compact
      // default = 5 */
      // :nZipCompressionLevel := 9
      :Create()
      ? '[ARCTYPE_ZIP] RETURN ERROR:', :nError, :ErrorDescription
      ? '[ARCTYPE_ZIP] cCommand:', :cCommand
      ?

   END

   RETURN
