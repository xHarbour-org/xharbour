/*
   $Id$
   Test program with 7Zip Compression
   WARNING: Must link SEVENZIP.LIB
*/

#define MAX_BUFFER 4096
#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL my7z

   IF FILE( "mytest.7z" )
      FErase( "mytest.7z" )
   ENDIF

   WITH OBJECT my7z := T7Zip():New()

      :cArcName := "mytest.7z"
      :aFiles   := { "hello.prg", "test.dbf" }
      // :aFiles   := "test.dbf"
      :cCompressionMethod := "ppmd"
      :nBuffer := MAX_BUFFER

      :Create()
      ? :cBuffer
      ? 'RETURN ERROR:', :nError, :ErrorDescription
      ?

      :List()
      ? :cBuffer
      ? 'RETURN ERROR:', :nError, :ErrorDescription
      ?

      :Test()
      ? :cBuffer
      ? 'RETURN ERROR:', :nError, :ErrorDescription

      // :nBuffer := MAX_BUFFER
      // :Extract( .T. )
      // ? :cBuffer
      // ? 'RETURN ERROR:', :nError, :ErrorDescription

   END

   RETURN
