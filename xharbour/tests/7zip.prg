/*
   $Id$
   Test program with 7Zip Compression
   WARNING: Must link SEVENZIP.LIB
*/

#define MAX_BUFFER 4096
#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL my7z

   WITH OBJECT my7z := T7Zip():New()

      :cArcName := "mytest.7z"
      :aFiles   := { "hello.prg", "test.dbf" }
      :cCompressionMethod := "ppmd"
      :nBuffer := MAX_BUFFER

      :Create()
      ? :cBuffer
      ?

      :List()
      ? :cBuffer
      ?

      :Test()
      ? :cBuffer

      // :nBuffer := MAX_BUFFER
      // :Extract( .T. )
      // ? :cBuffer

   END

   RETURN
