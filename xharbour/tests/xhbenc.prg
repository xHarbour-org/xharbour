**************************************************
* xhbenc.prg
* $Id$
* Test program for file encoding
* UUEncode, Base64 and YYEncode
*
* Andi Jahja
*
* Must link hbcc.lib
//----------------------------------------------------------------------------//

#include "directry.ch"
#include "box.ch"
#include "inkey.ch"

PROCEDURE MAIN()

   LOCAL cScr := savescreen(0,0,maxrow(),maxcol())
   LOCAL nError
   LOCAL nFault := 0
   LOCAL aOkey := { " Okay " }

   CLEAR SCREEN
   IF Alert( "xHarbour File Encoding Tests",{" Continue "," Quit "},"N/W*" ) == 2
      restscreen(0,0,maxrow(),maxcol(),cScr)
      RETURN
   ENDIF

   CLEAR SCREEN
   Alert( 'Will UUEncode pp.prg to ~pp.uue.;Syntax: UUENCODE_FILE( "pp.prg", "~pp.uue" )',aOkey,"N/GR*" )

   IF ( nError := UUENCODE_FILE( "pp.prg", "~pp.uue" ) ) == 0 .AND. File( "~pp.uue" )
      ShowResult( "~pp.uue" )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   Alert( 'Now will Base64_Encode pp.prg to ~pp.b64.;Syntax: B64ENCODE_FILE( "pp.prg", "~pp.b64" )',aOkey, "N/BG*" )

   IF ( nError := B64ENCODE_FILE( "pp.prg", "~pp.b64" ) ) == 0 .AND. File( "~pp.b64" )
      ShowResult( "~pp.b64" )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   Alert( 'Will YYEncode pp.prg to ~pp.yye.;Syntax: YYENCODE_FILE( "pp.prg", "~pp.yye" )',aOkey,"N/GR*" )

   IF ( nError := YYENCODE_FILE( "pp.prg", "~pp.yye" ) ) == 0 .AND. File( "~pp.yye" )
      ShowResult( "~pp.yye" )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   ALert( 'Now Will UUEncode pp.prg to ~pp*.uue;with 2000 lines per chunk.;Syntax: UUENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" )',aOkey, "W+/B" )

   IF ( nError := UUENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" ) ) == 0
      ShowResult( "~pp*.uue" )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   Alert( 'Now Will Base64_Encode pp.prg to ~pp*.b64;with 2000 lines per chunk.;Syntax: B64ENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" )',aOkey,"W+/G")

   IF ( nError := B64ENCODE_FILE_BY_CHUNK( "pp.prg", 2000, "~pp" ) ) == 0
      ShowResult( "~pp*.b64" )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   Alert( 'Now Will YYEncode pp.prg to ~pp*.yye;with 1000 lines per chunk.;Syntax: YYENCODE_FILE_BY_CHUNK( "pp.prg", 1000, "~pp" )',aOkey,"W+/N")

   IF ( nError := YYENCODE_FILE_BY_CHUNK( "pp.prg", 1000, "~pp" ) ) == 0
      ShowResult( "~pp*.yye" )
   ELSE
      ShowError( @nFault, nError )
   ENDIF

   CLEAR SCREEN
   Alert( "File Encoding Tests Completed" + if(nFault>0,". Error encountered : " + ltrim(str(nFault)),". All tests succesfull!"),aOkey,"N/W*" )

   restscreen(0,0,maxrow(),maxcol(),cScr)

   RETURN

//----------------------------------------------------------------------------//
STATIC PROCEDURE ShowResult( cFileMask )

   LOCAL aFiles, aItem

   IF !Empty( aFiles := Directory( cFileMask ) )
      Alert( "Conversion succesful !", { " Okay " }, "N/W*" )
      FOR EACH aItem IN aFiles
         View( aItem )
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

   WHILE ( ( nKey := inkey() ) != K_ESC )
      o:MoveCursor(nKey)
   ENDDO

   RETURN
