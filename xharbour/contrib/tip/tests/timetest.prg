/******************************************
* TIP test
* timestamp encoding and decoding
*
* Usage:
* base64test "timestamp"
*
* $Id: base64test.prg,v 1.1 2003/11/30 14:42:03 jonnymind Exp $
*****/

PROCEDURE MAIN( cTimeStamp )

   IF cTimeStamp == NIL
      ? "Now is:", TIP_Timestamp()
      ?
   ENDIF
RETURN
