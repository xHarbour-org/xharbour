/******************************************
* TIP test
* MIME type test
*
* This test tries to detect the mime type of a give file.
*
* Usage:
* mimetest filename
*
* $Id: mimetest.prg,v 1.1 2004/05/17 15:18:15 jonnymind Exp $
*****/

PROCEDURE MAIN( cFileName )

   IF Empty( cFileName )
      ?
      ? "Usage: mimetest <file to test>"
      ?
      QUIT
   ENDIF
   
   IF ( ! file( cFileName ) )
      ?
      ? "File", cFileName, "is not valid"
      ?
      QUIT
   ENDIF
   
   ? cFileName + ":", Tip_FileMimeType( cFileName )
   ?
   
RETURN
