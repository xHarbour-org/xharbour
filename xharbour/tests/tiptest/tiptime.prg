/******************************************
* TIP test
* timestamp encoding and decoding
*
* Usage:
* base64test "timestamp"
*
* $Id: timetest.prg,v 1.1 2003/12/01 00:20:11 jonnymind Exp $
*****/

PROCEDURE MAIN( cTimeStamp )

   IF cTimeStamp == NIL
      ? "Now is:", TIP_Timestamp()
      ?
   ENDIF
RETURN
