/*
 * $Id: hbdecode.prg,v 1.3 2004/02/03 22:11:43 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    UUDECODE_FILE()
 *    B64DECODE_FILE()
 *    YYDECODE_FILE()
 *
 * Copyright 2004 Andi Jahja <xharbour@cbn.net.id>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
UUDECODE_FILE( <cFileInput>, [<cFileOutput>] ) -> int
   Description:
      UUDecode a given file
   Parameters:
      cFileInput = string, source filename to be decoded
                   OR
                   array, an array of file chunks arranged in proper order
      cFileOutput = output filename
   Returns:
      Upon succesful decoding the function returns numnber of bytes written

B64DECODE_FILE( <cFileInput>, [<cFileOutput>] ) -> int
   Description:
      Decode a Base64 encoded file
   Parameters:
      cFileInput = string, source filename to be decoded
                   OR
                   array, an array of file chunks arranged in proper order
      cFileOutput = output filename
   Returns:
      Upon succesful decoding the function returns numnber of bytes written

YYDECODE_FILE( <cFileInput>, [<cFileOutput>] ) -> int
   Description:
      YYDecode a given
   Parameters:
      cFileInput = string, source filename to be decoded
                   OR
                   array, an array of file chunks arranged in proper order
      cFileOutput = output filename
   Returns:
      Upon succesful decoding the function returns numnber of bytes written
*/

/*
   IMPORTANT NOTES:

   A. Trick for decoding multi-chunk encoded file:
      1. Create a new file from the file chunks which represent a merge
         of all chunks. They are pure text file so there should be no problem.
         (See static function joinfile for processing)
         a. UUENCODED file: No specialities
         b. B64ENCODED file: No specialities
         c. YYENCODED file:
            -> Use Only the first chunk's header and the last chunk's footer so that
            the merge file will look like :
            =ybegin etc. etc.
            xxxxxxxx ........ // this is the bodies of the chunks
            =yend etc. etc.
      2. Decode that new file

   B. These decoding functions use TFileRead Class from libmisc.
      So libmisc.lib must be linked upon usage.

   C. These decoding functions are intended to only decode files that were
      previously encoded using encoding functions of this library.
*/

#PRAGMA BEGINDUMP
#include "hbapi.h"
#undef LINE_MAX
#define LINE_MAX SHRT_MAX

//----------------------------------------------------------------------------//
BOOL hbcc_file_read ( FILE *fileHandle, char *string )
{
   int ch, cnbr = 0;

   memset (string, ' ', LINE_MAX);

   for (;;)
   {
      ch = fgetc (fileHandle);

      if ( (ch == '\n') ||  (ch == EOF) ||  (ch == 26) )
      {
         string [cnbr] = '\0';
         return (ch == '\n' || cnbr);
      }
      else
      {
         if (cnbr < LINE_MAX)
         {
            if ( ch != '\r' )
               string [cnbr++] = (char) ch;
         }
      }

      if (cnbr >= LINE_MAX)
      {
         string [LINE_MAX] = '\0';
         return (TRUE);
      }
   }
}

#PRAGMA ENDDUMP

#ifdef __USE_PRG_FUNCTIONS__
//----------------------------------------------------------------------------//
FUNCTION UUDECODE_FILE( cEncodedFile, cOutFile )

   LOCAL oFile
   LOCAL nBytesWritten := 0
   LOCAL nByteRead := 0
   LOCAL hOut
   LOCAL cDecoded
   LOCAL cEncoded
   LOCAL lStart := .F.
   LOCAL nAt
   LOCAL cItem
   LOCAL lIsArray := .F.

   IF ValType( cEncodedFile ) == "C"
      IF !File( cEncodedFile )
         RETURN 0
      ENDIF
   ELSEIF ValType( cEncodedFile ) == "A"
      /* file chunks are passed in a one dimension array.
         the array should be arranged in such a way so that elements are in
         proper order. ie. element 1 is the first chunk, element 2 the second
         etc.
      */
      FOR EACH cItem IN cEncodedFile
         IF !File( cItem )
            RETURN 0
         ENDIF
      NEXT
      IF empty( cEncodedFile := JoinSection( cEncodedFile, 1 ) )
         RETURN 0
      ENDIF
      lIsArray := .T.
   ELSE
      RETURN 0
   ENDIF

   oFile := TFileRead():New( cEncodedFile )
   oFile:Open()

   IF ! oFile:Error()

      WHILE oFile:MoreToRead()

         cEncoded := oFile:ReadLine()

         IF !lStart
            lStart := .T.
            IF "BEGIN" IN Upper( cEncoded )
               IF ValType( cOutFile ) != "C"
                  nAt := RAT( " ", cEncoded )
                  cOutFile := SUBSTR( cEncoded, nAT + 1 )
               ENDIF
               IF ( hOut := FCreate( cOutFile ) ) < 0
                  EXIT
               ENDIF
               LOOP
            ENDIF
         ENDIF

         IF ! ( "END" IN Upper( cEncoded ) )
            cDecoded      := HB_UUDECODE( cEncoded )
            nBytesWritten += FWrite( hOut, cDecoded )
         ENDIF

      ENDDO

      IF hOut != -1
         FClose( hOut )
      ENDIF

      oFile:Close()

   ENDIF

   /* delete temporary joint file */
   IF lIsArray .AND. File( cEncodedFile )
      FErase( cEncodedFile )
   ENDIF

   RETURN nBytesWritten

//----------------------------------------------------------------------------//
FUNCTION B64DECODE_FILE( cEncodedFile, cOutFile )

   LOCAL oFile
   LOCAL nBytesWritten := 0
   LOCAL nByteRead := 0
   LOCAL hOut
   LOCAL cDecoded
   LOCAL cEncoded
   LOCAL lStart := .F.
   LOCAL nAt
   LOCAL cItem
   LOCAL lIsArray := .F.

   IF ValType( cEncodedFile ) == "C"
      IF !File( cEncodedFile )
         RETURN 0
      ENDIF
   ELSEIF ValType( cEncodedFile ) == "A"
      /* file chunks are passed in a one dimension array.
         the array should be arranged in such a way so that elements are in
         proper order. ie. element 1 is the first chunk, element 2 the second
         etc.
      */
      FOR EACH cItem IN cEncodedFile
         IF !File( cItem )
            RETURN 0
         ENDIF
      NEXT
      IF empty( cEncodedFile := JoinSection( cEncodedFile, 2 ) )
         RETURN 0
      ENDIF
      lIsArray := .T.
   ELSE
      RETURN 0
   ENDIF

   oFile := TFileRead():New( cEncodedFile )
   oFile:Open()

   IF ! oFile:Error()

      WHILE oFile:MoreToRead()

         cEncoded := oFile:ReadLine()

         IF !lStart
            lStart := Empty( cEncoded )
            IF "CONTENT-TYPE" IN Upper( cEncoded )
               IF ValType( cOutFile ) != "C"
                  nAt := RAT( "=", cEncoded )
                  cOutFile := SUBSTR( cEncoded, nAT + 1 )
                  cOutFile := STRTRAN( cOutFile, '"' )
               ENDIF
               IF ( hOut := FCreate( cOutFile ) ) < 0
                  EXIT
               ENDIF
            ENDIF
            LOOP
         ENDIF

         cDecoded      := HB_B64DECODE( cEncoded )
         nBytesWritten += FWrite( hOut, cDecoded )

      ENDDO

      IF hOut != -1
         FClose( hOut )
      ENDIF

      oFile:Close()

   ENDIF

   /* delete temporary joint file */
   IF lIsArray .AND. File( cEncodedFile )
      FErase( cEncodedFile )
   ENDIF

   RETURN nBytesWritten

//----------------------------------------------------------------------------//
FUNCTION YYDECODE_FILE( cEncodedFile, cOutFile )

   LOCAL oFile
   LOCAL nBytesWritten := 0
   LOCAL nByteRead := 0
   LOCAL hOut
   LOCAL cDecoded
   LOCAL cEncoded
   LOCAL lStart := .F.
   LOCAL nAt
   LOCAL cItem
   LOCAL lIsArray := .F.

   IF ValType( cEncodedFile ) == "C"
      IF !File( cEncodedFile )
         RETURN 0
      ENDIF
   ELSEIF ValType( cEncodedFile ) == "A"
      /* file chunks are passed in a one dimension array.
         the array should be arranged in such a way so that elements are in
         proper order. ie. element 1 is the first chunk, element 2 the second
         etc.
      */
      FOR EACH cItem IN cEncodedFile
         IF !File( cItem )
            RETURN 0
         ENDIF
      NEXT
      IF empty( cEncodedFile := JoinSection( cEncodedFile, 3 ) )
         RETURN 0
      ENDIF
      lIsArray := .T.
   ELSE
      RETURN 0
   ENDIF

   oFile := TFileRead():New( cEncodedFile )
   oFile:Open()

   IF ! oFile:Error()

      WHILE oFile:MoreToRead()

         cEncoded := oFile:ReadLine()

         IF !lStart
            lStart := .T.

            IF "=YBEGIN" IN Upper( cEncoded )
               IF ValType( cOutFile ) != "C"
                  nAt := RAT( "=", cEncoded )
                  cOutFile := SUBSTR( cEncoded, nAT + 1 )
               ENDIF
               IF ( hOut := FCreate( cOutFile ) ) < 0
                  EXIT
               ENDIF
               LOOP
            ENDIF
         ENDIF

         IF ! ( "=YEND" IN Upper( cEncoded ) ) .AND. ! ( "=YPART" IN Upper( cEncoded ) )
            cDecoded      := HB_YYDECODE( cEncoded )
            nBytesWritten += FWrite( hOut, cDecoded )
         ENDIF

      ENDDO

      IF hOut != -1
         FClose( hOut )
      ENDIF

      oFile:Close()

   ENDIF

   /* delete temporary joint file */
   IF lIsArray .AND. File( cEncodedFile )
      FErase( cEncodedFile )
   ENDIF

   RETURN nBytesWritten

//----------------------------------------------------------------------------//
STATIC FUNCTION JoinSection( aFile, nEncoding )

   LOCAL cTempFile := "_" + strtran(ltrim(str(seconds())),".") + ".enc"
   LOCAL hTemp := FCreate( cTempFile )
   LOCAL nFile, nTotalFile := LEN( aFile )
   LOCAL cContent, oFile, nLine := 0

   IF hTemp < 0
      cTempFile := ""
      RETURN ""
   ENDIF

   SWITCH ( nEncoding )

   CASE 1 // UUEncoded
   CASE 2 // Base64
      FOR nFile := 1 TO LEN( aFile )
         oFile := TFileRead():New( aFile[ nFile ] )
         oFile:Open()
         IF ! oFile:Error()
            WHILE oFile:MoreToRead()
               cContent := oFile:ReadLine()
               FWrite( hTemp, cContent + CHR(10) )
            ENDDO
            oFile:Close()
         ELSE
            cTempFile := ""
            FClose( hTemp )
            EXIT
         ENDIF
      NEXT
      FClose( hTemp )
      EXIT
   CASE 3 // YYEncoded
      FOR nFile := 1 TO LEN( aFile )
         oFile := TFileRead():New( aFile[ nFile ] )
         oFile:Open()
         IF ! oFile:Error()
            WHILE oFile:MoreToRead()
               cContent := oFile:ReadLine()
               IF nFile == 1
                  IF ! ( "=YEND" IN Upper( cContent ) )
                     FWrite( hTemp, cContent + CHR(10) )
                  ENDIF
               ELSEIF nFile < nTotalFile
                  IF ! ( "=YEND" IN Upper( cContent ) ) .AND. ! ( "=YBEGIN" IN Upper( cContent ) )
                     FWrite( hTemp, cContent + CHR(10) )
                  ENDIF
               ELSE
                  IF ! ( "=YPART" IN Upper( cContent ) ) .AND. ! ( "=YBEGIN" IN Upper( cContent ) )
                     FWrite( hTemp, cContent + CHR(10) )
                  ENDIF
               ENDIF
            ENDDO
            oFile:Close()
         ELSE
            cTempFile := ""
            FClose( hTemp )
            EXIT
         ENDIF
      NEXT
      FClose( hTemp )
      EXIT
   END

   RETURN cTempFile
#endif
