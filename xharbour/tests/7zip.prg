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
      :cBuffer := space(MAX_BUFFER)
      :Create()
      ? :cBuffer
      ?
      :cBuffer := space( MAX_BUFFER )
      :List()
      ? :cBuffer
      ?
      :cBuffer := space( MAX_BUFFER )
      :Test()
      ? :cBuffer
      :cBuffer := space( MAX_BUFFER )
      // :Extract( .T. )
      // ? :cBuffer

   END

   RETURN
