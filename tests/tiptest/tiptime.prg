/******************************************
* TIP test
* timestamp encoding and decoding
*
* Usage:
* base64test "timestamp"
*
* $Id$
*****/

PROCEDURE MAIN( cTimeStamp )

   IF cTimeStamp == NIL
      ? "Now is:", TIP_Timestamp()
      ?
   ENDIF
RETURN
