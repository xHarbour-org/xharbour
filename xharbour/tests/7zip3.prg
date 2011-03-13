/*
   $Id$
   Test program with 7Zip Compression
   WARNING: Must link SEVENZIP.LIB
*/

#include "simpleio.ch"

// Uncomment the following line to test ZIP mode
// #define __ARC_IS_ZIP

// Uncomment the following line to test UNSOLID archive
// #define __NOT_SOLID__

#define ARCTYPE_7Z    1
#define ARCTYPE_ZIP   2
#define KILOBYTES     100000

#ifdef __ARC_IS_ZIP
   #define ARCNAME "7zip3.zip"
#else
   #define ARCNAME "7zip3.7z"
#endif

PROCEDURE MAIN()

   LOCAL my7z, aDir, i := 0, cFile, t := seconds()

   // Erase test files exist
   WHILE FILE( cFile := ARCNAME + '.' + strzero( ++ i, 3 ) )
      ? 'Deleting '+ cFile
      FErase( cFile )
   ENDDO

   ?
   ? 'Working ....'

   WITH OBJECT my7z := T7Zip():New()
      :cArcName      := ARCNAME
      // supposed to be run in tests folder
      // pls change aFiles if required
      :aFiles        := "..\source\*.*"
      :cPassword     := "xharbour"
      :lRecursive    := .T.      /* .T. = include sub-dirs */
      :aExcludeFiles := "*.prg"  /* prg files will not be archived */
      :aVolumes      := 2 * KILOBYTES

#ifdef __ARC_IS_ZIP
      :nArctype      := ARCTYPE_ZIP
#endif

#ifdef __NOT_SOLID__
      :lSolid        := .F. /* for testing only, it is recommended to keep this
                               as .T. (default) */
#endif

      :lMultiCPU     := .T.
      :Create()
      ? '[T7ZIP] command:', :cCommand
      ? '[T7ZIP] RETURN ERROR:', :nError, :ErrorDescription

      aDir := Directory( :cArcName + ".*" )

      ?
      FOR i := 1 TO LEN( aDir )
         ? aDir[i][1], aDir[i][2], aDir[i][3], aDir[i][4]
      NEXT

      ?
      ? 'Time', seconds() -t, "secs"
   END

   RETURN
