/******************************************
* TIP test
* BASE64 (and other) encoding
*
* This test writes data to standard output, and is
* compiled only under GTCGI; this allow to test the
* input/output file against other OS encoded/decoded data
*
* Usage:
* base64test < file-to-encode >encoded-file
* base64test -d  < encoded-file  >file-to-decode
*
* $Id: httpadvtest.prg,v 1.1 2003/11/22 15:10:33 jonnymind Exp $
*****/

PROCEDURE MAIN( cOption )
   LOCAL oEncoder
   LOCAL cData := ""
   LOCAL cBuffer := Space( 1024 )
   LOCAL nLen

   IF cOption == NIL
      cOption := ""
   ENDIF

   IF Lower( cOption ) == "-h"
      ? "Usage:"
      ? "base64test < file-to-encode >encoded-file"
      ? "base64test -d  < encoded-file  >file-to-decode"
      ?
      QUIT
   ENDIF

   oEncoder := TIPEncoder():New( "base64" )

   nLen := FRead( 0, @cBuffer, 1024 )
   DO WHILE nLen > 0
      IF nLen < 1024
         cData += Substr( cBuffer, 1, nLen )
      ELSE
         cData += cBuffer
      ENDIF
      nLen := FRead( 0, @cBuffer, 1024 )
   ENDDO

   IF Lower( cOption ) == "-d"
      cData := oEncoder:Decode( cData )
   ELSE
      cData := oEncoder:Encode( cData )
   ENDIF

   FWrite( 1, cData )
RETURN
