*****************************************************
* Demo of realtime string compression
*
* Giancarlo Niccolai
*
* $Id$
*

#include "hbcompress.ch"

PROCEDURE MAIN()
   LOCAL cText := "A  text  to  be  compressed         "
   LOCAL cComp
   LOCAL cDecomp
   LOCAL nError, nBufLen


   CLEAR SCREEN
   @1,15 SAY "X H A R B O U R - ZLIB based compression test"

   @2,5 SAY "TEST 1: using on-the-fly Buffer creation"

   cComp := HB_Compress( cText )
   cDecomp := HB_Uncompress( LEN( cText), cComp )

   @3,7 SAY "Uncompressed: " + cText + "<<"
   @4,7 SAY "Compressed ("+Alltrim( Str( Len( cComp ) ) ) +")"+ cComp
   @5,7 SAY "Decompressed: " + cDecomp + "<<"

   @7,5 SAY "TEST 2: using preallocated buffers"

   nBufLen := HB_CompressBuflen( Len( cText ) )
   cComp := Space( nBufLen )
   cDecomp := Space( Len( cText ) )
   HB_Compress( cText, Len( cText ), @cComp, @nBuflen )
   HB_Uncompress( Len( cText ), cComp, nBuflen , @cDecomp )

   @8,7 SAY "Uncompressed: " + cText + "<<"
   @9,7 SAY "Compressed ("+Alltrim( Str( nBuflen ) ) +")"+ cComp
   @10,7 SAY "Decompressed: " + cDecomp + "<<"

   @12,5 SAY "TEST 3: Generating an error"

   nBufLen := HB_CompressBuflen( Len( cText ) )
   cComp := Space( nBufLen )
   cDecomp := Space( Len( cText ) )

   nBuflen := 3
   nError := HB_Compress( cText, Len( cText), @cComp, @nBuflen )
   IF nError != HB_Z_OK
      @13,7 SAY "Error generated ("+Alltrim( Str( Len( cComp ) ) ) +")"+ ;
            HB_CompressErrorDesc( nError )
   ELSE
      @13,7 SAY "NO Error generated ("+Alltrim( Str( Len( cComp ) ) ) +")"+ ;
            HB_CompressErrorDesc( nError )
   ENDIF

   @22,25 SAY "Press a key to terminate"
   INKEY(0)
RETURN

