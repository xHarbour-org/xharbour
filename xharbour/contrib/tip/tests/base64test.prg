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
* base64test -q [-d]  to use quoted printable encoding/decoding.
* base64test -u [-d]  to use url encoding/decoding.
*
* $Id: base64test.prg,v 1.1 2003/11/30 14:42:03 jonnymind Exp $
*****/

PROCEDURE MAIN( ... )
   LOCAL oEncoder
   LOCAL cData
   LOCAL cBuffer := Space( 1024 )
   LOCAL nLen
   LOCAL cOption, lHelp := .F.,lDecode := .F., lQp := .F., lUrl := .F.

   /* Parameter parsing */
   FOR nLen := 1 TO PCount()
      cData := Lower( HB_PValue( nLen ) )
      DO CASE
         CASE cData == '-h'
            lHelp := .T.

         CASE cData == '-d'
            lDecode := .T.

         CASE cData == '-q'
            lQp := .T.

         CASE cData == '-u'
            lUrl := .T.

         OTHERWISE
            ? "Wrong parameter", cData
            ?
            lHelp := .T.
            EXIT
      ENDCASE
   NEXT

   /* Providing help */
   IF lHelp
      ? "Usage:"
      ? "base64test < file-to-encode >encoded-file"
      ? "base64test -d  < encoded-file  >file-to-decode"
      ? "base64test -q [-d]  to use quoted printable encoding/decoding"
      ? "base64test -u [-d]  to use url encoding/decoding."
      ?
      QUIT
   ENDIF

   /* Selecting the encoder */
   IF lUrl
      oEncoder := TIPEncoder():New( "url" )
   ELSEIF lQp
      oEncoder := TIPEncoder():New( "quoted-printable" )
   ELSE
      oEncoder := TIPEncoder():New( "base64" )
   ENDIF

   /* Reading input stream */
   cData := ""
   nLen := FRead( 0, @cBuffer, 1024 )
   DO WHILE nLen > 0
      IF nLen < 1024
         cData += Substr( cBuffer, 1, nLen )
      ELSE
         cData += cBuffer
      ENDIF
      nLen := FRead( 0, @cBuffer, 1024 )
   ENDDO

   /* Encoding/decoding */
   IF lDecode
      cData := oEncoder:Decode( cData )
   ELSE
      cData := oEncoder:Encode( cData )
   ENDIF

   /* Writing stream */
   FWrite( 1, cData )
RETURN
