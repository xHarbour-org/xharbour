**************************************************
* xhbenc.prg
* $Id$
* Test program for file encoding and decoding
* UUEncode, Base64, YYEncode and XXEncode
*
* Andi Jahja
*
* Must link hbcc.lib
//----------------------------------------------------------------------------//

#include "directry.ch"
#include "box.ch"
#include "inkey.ch"

static aOkey := { " Continue " , " Quit " }
static cScr

PROCEDURE MAIN()

   LOCAL nError
   LOCAL nFault := 0
   LOCAL nStart
   LOCAL nOldRow := Row(), nOldCol := Col()
   cScr := savescreen(0,0,maxrow(),maxcol())

   BEGIN SEQUENCE

   SET CURSOR OFF
   CLEAR SCREEN

   IF !File( "pp.prg" )
      __CopyFile( "..\utils\xbscript\xbscript.prg", "pp.prg" )
   ENDIF

   IF Alert( "xHarbour File Encoding Tests", aOkey, "N/W*" ) == 2
      _QuitMe()
   ENDIF

   CLEAR SCREEN
   IF Alert( 'Will UUEncode pp.prg to ~pp.uue.;Syntax: UUENCODE_FILE( "pp.prg", "~pp.uue" )',aOkey,"N/GR*" ) == 2
      _QuitMe()
   ENDIF

   nStart := seconds()
   IF ( nError := UUENCODE_FILE( "pp.prg", "~pp.uue" ) ) == 0 .AND. File( "~pp.uue" )
      ShowResult( "~pp.uue", "UUE", nStart )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   IF Alert( 'Now will Base64_Encode pp.prg to ~pp.b64.;Syntax: B64ENCODE_FILE( "pp.prg", "~pp.b64" )',aOkey, "N/BG*" ) == 2
      _QuitMe()
   ENDIF

   nStart := seconds()
   IF ( nError := B64ENCODE_FILE( "pp.prg", "~pp.b64" ) ) == 0 .AND. File( "~pp.b64" )
      ShowResult( "~pp.b64", "B64", nStart )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   IF Alert( 'Will YYEncode pp.prg to ~pp.yye.;Syntax: YYENCODE_FILE( "pp.prg", "~pp.yye" )',aOkey,"N/GR*" ) == 2
      _QuitMe()
   ENDIF

   nStart := seconds()
   IF ( nError := YYENCODE_FILE( "pp.prg", "~pp.yye" ) ) == 0 .AND. File( "~pp.yye" )
      ShowResult( "~pp.yye", "YYE", nStart )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   IF Alert( 'Will XXEncode pp.prg to ~pp.xxe.;Syntax: XXENCODE_FILE( "pp.prg", "~pp.xxe" )',aOkey,"W+/BG" ) == 2
      _QuitMe()
   ENDIF

   nStart := seconds()
   IF ( nError := XXENCODE_FILE( "pp.prg", "~pp.xxe" ) ) == 0 .AND. File( "~pp.xxe" )
      ShowResult( "~pp.xxe", "XXE", nStart )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   IF ALert( 'Now Will UUEncode pp.prg to ~pp*.uue;with 2000 lines per chunk.;Syntax: UUENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" )',aOkey, "W+/B" ) == 2
      _QuitMe()
   ENDIF

   nStart := seconds()
   IF ( nError := UUENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" ) ) == 0
      ShowResult( "~pp*.uue", "UUE", nStart )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   IF Alert( 'Now Will Base64_Encode pp.prg to ~pp*.b64;with 2000 lines per chunk.;Syntax: B64ENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" )',aOkey,"W+/G") == 2
      _QuitMe()
   ENDIF

   nStart := seconds()
   IF ( nError := B64ENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" ) ) == 0
      ShowResult( "~pp*.b64", "B64", nStart )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   IF Alert( 'Now Will YYEncode pp.prg to ~pp*.yye;with 1000 lines per chunk.;Syntax: YYENCODE_FILE_BY_CHUNK( "pp.prg", 1000, "~pp" )',aOkey,"W+/N") == 2
      _QuitMe()
   ENDIF

   nStart := seconds()
   IF ( nError := YYENCODE_FILE_BY_CHUNK( "pp.prg", 1000, "~pp" ) ) == 0
      ShowResult( "~pp*.yye", "YYE", nStart )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   IF Alert( 'Now Will XXEncode pp.prg to ~pp*.xxe;with 2000 lines per chunk.;Syntax: XXENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" )',aOkey,"N/BG*") == 2
      _QuitMe()
   ENDIF

   nStart := seconds()
   IF ( nError := XXENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" ) ) == 0
      ShowResult( "~pp*.xxe", "XXE", nStart )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   Alert( "File Encoding Tests Completed" + if(nFault>0,". Error encountered : " + ltrim(str(nFault)),". All tests succesfull!"), { " Bye "},"N/W*" )

   END SEQUENCE

   restscreen(0,0,maxrow(),maxcol(),cScr)
   SET CURSOR ON
   SetPos( nOldRow, nOldCol )

   RETURN

//----------------------------------------------------------------------------//
STATIC PROCEDURE ShowResult( cFileMask, cEncoding, nStart )

   LOCAL aFiles, aItem
   LOCAL nDecoded := 0
   LOCAL cSyntax
   LOCAL aDecodedFiles := {}
   LOCAL iChoice

   IF !Empty( aFiles := Directory( cFileMask ) )
      Alert( "Conversion succesful !;Done in " + ltrim(str(seconds()-nStart)) + " seconds", { " Okay " }, "N/W*" )

      AEval( aFiles, { |e| AADD( aDecodedFiles, e[1] ) } )

      FOR EACH aItem IN aFiles
         View( aItem )
      NEXT

      CLEAR SCREEN

      DO CASE
      CASE cEncoding == "UUE"
         cSyntax  := 'UUDECODE_FILE( aDecodedFiles, "result.txt" )'
         IF Alert( "Now will decode the encoded files to 'result.txt';Syntax : " + cSyntax , aOkey, "gr+/b" ) == 2
            _QuitMe()
         ENDIF
         ? "Decoding in progress ......"
         nStart := seconds()
         nDecoded := UUDECODE_FILE( aDecodedFiles, "result.txt" )
      CASE cEncoding == "B64"
         cSyntax  := 'B64DECODE_FILE( aDecodedFiles, "result.txt" )'
         IF Alert( "Now will decode the encoded files to 'result.txt';Syntax : " + cSyntax , aOkey, "gr+/b" ) == 2
            _QuitMe()
         ENDIF
         ? "Decoding in progress ......"
         nStart := seconds()
         nDecoded := B64DECODE_FILE( aDecodedFiles, "result.txt" )
      CASE cEncoding == "YYE"
         cSyntax  := 'YYDECODE_FILE( aDecodedFiles, "result.txt" )'
         IF Alert( "Now will decode the encoded files to 'result.txt';Syntax : " + cSyntax , aOkey, "gr+/b" ) == 2
            _QuitMe()
         ENDIF
         ? "Decoding in progress ......"
         nStart := seconds()
         nDecoded := YYDECODE_FILE( aDecodedFiles, "result.txt" )
      CASE cEncoding == "XXE"
         cSyntax  := 'XXDECODE_FILE( aDecodedFiles, "result.txt" )'
         IF Alert( "Now will decode the encoded files to 'result.txt';Syntax : " + cSyntax , aOkey, "gr+/b" ) == 2
            _QuitMe()
         ENDIF
         ? "Decoding in progress ......"
         nStart := seconds()
         nDecoded := XXDECODE_FILE( aDecodedFiles, "result.txt" )
      ENDCASE

      CLEAR SCREEN

      IF nDecoded > 0
         iChoice := Alert( "Decoding successful;Bytes written = " + ltrim(str(nDecoded)+"; Done in " + ltrim(str(seconds()-nStart))+ " seconds"),{" View ", " Skip "," Quit "},"N/W*" )
	 IF iChoice == 1
            IF !Empty( aDecodedFiles := Directory( "result.txt" ) )
               FOR EACH aItem IN aDecodedFiles
                  View( aItem )
               NEXT
	    ENDIF
	 ELSEIF iChoice == 3
	    _QuitMe()
	 ENDIF
      ELSE
         Alert( "Error in file decoding ...", { " Booo ..."})
      ENDIF

      FOR EACH aItem IN aFiles
         FErase( aItem[F_NAME] )
      NEXT
   ENDIF

   RETURN

//----------------------------------------------------------------------------//
STATIC PROCEDURE ShowError( nFault, nError )

   nFault ++
   Alert( "Error Occured. Return Code: " + ltrim(str(nError)),{" Booo ... " } )

   RETURN

//----------------------------------------------------------------------------//
STATIC PROCEDURE View( aFile )

   LOCAL o := HBEditor():New("",1,1,maxrow()-1,maxcol()-1,.F.,maxcol()-2)
   LOCAL nkey

   dispbox(0,0,maxrow(),maxcol(),B_SINGLE + ' ')
   dispoutat( 0,1, "[ " + aFile[F_NAME] + " (" + LTRIM(HB_ValToStr( aFile[F_SIZE])) + " bytes) ]" )

   o:lWordWrap := .F.
   o:Loadfile( aFile[F_NAME] )
   o:RefreshWindow()

   WHILE ( ( nKey := inkey() ) != K_ESC )
      o:MoveCursor(nKey)
   ENDDO

   RETURN

//----------------------------------------------------------------------------//
STATIC PROCEDURE _QuitMe()
   restscreen(0,0,maxrow(),maxcol(),cScr)
   BREAK
