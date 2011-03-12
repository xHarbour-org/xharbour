/*
   $Id$
   Test program with 7Zip Compression
   WARNING: Must link SEVENZIP.LIB
*/

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL my7z := T7Zip():New()

   ? 'Version',HB_SevenZipGetVersion()

   my7z:cArcName := "mytest.7z"
   my7z:aFiles   := { "hello.prg", "test.dbf" }
   my7z:cCompressionMethod := "PPMd"

   my7z:Create()

   RETURN
