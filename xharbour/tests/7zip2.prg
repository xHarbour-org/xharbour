/*
   $Id$
   Test program with 7Zip Compression
   WARNING: Must link SEVENZIP.LIB
*/

#include "simpleio.ch"

#define ARCTYPE_7Z            1
#define ARCTYPE_ZIP           2

#define CMPMETHOD_LZMA        1  // LZ-based algorithm
#define CMPMETHOD_LZMA2       2  // LZMA-based algorithm
#define CMPMETHOD_PPMD        3  // Dmitry Shkarin's PPMdH with small changes
#define CMPMETHOD_BZIP2       4  // BWT algorithm
#define CMPMETHOD_DEFLATE     5  // LZ+Huffman
#define CMPMETHOD_COPY        6  // No compression

PROCEDURE MAIN()

   LOCAL my7z, i, t
   LOCAL aTime    := Array( 6 )
   LOCAL aTest    := {"testLZMA.7z","testLZMA2.7z","testPPMD.7z","testBZIP2.7z","testDEFLATE.7z","testCOPY.7z"}
   LOCAL aMethod  := {CMPMETHOD_LZMA,CMPMETHOD_LZMA2,CMPMETHOD_PPMD,CMPMETHOD_BZIP2,CMPMETHOD_DEFLATE,CMPMETHOD_COPY}
   LOCAL acMethod := {"CMPMETHOD_LZMA","CMPMETHOD_LZMA2","CMPMETHOD_PPMD","CMPMETHOD_BZIP2","CMPMETHOD_DEFLATE","CMPMETHOD_COPY"}

   FOR i := 1 TO 6
      IF File( aTest[ i ] )
         Ferase( aTest[ i ] )
      ENDIF
   NEXT

   WITH OBJECT my7z := T7Zip():New()

      // supposed to be run in tests folder
      // pls change aFiles if required
      :aFiles     := "..\source\*.*"
      :cPassword  := "xharbour"
      :lRecursive := .T. /* .T. = include sub-dirs */

      FOR i := 1 TO 6
         t := seconds()
         :cArcName           := aTest[ i ]
         :nCompressionMethod := aMethod[ i ]
         :Create()
         ? '[ARCTYPE_7Z] command:', :cCommand
         ? '[ARCTYPE_7Z] RETURN ERROR:', :nError, :ErrorDescription
         ? 'Time: ' aTime[ i ] := seconds() - t
      NEXT

   END

   ?
   ? '----------'
   ? 'Benchmark:'
   ? '----------'
   FOR i := 1 TO 6
      IF File( aTest[ i ] )
         ? PADR(aTest[i],20), PADR(acMethod[i],20), TRANSFORM( HB_FSize( aTest[i]), "99,999,999" ), aTime[i], "secs"
      ENDIF
   NEXT

   RETURN
