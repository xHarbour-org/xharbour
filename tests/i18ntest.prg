************************************************************
* i18ntest.prg
* $Id$
*
* Test for internationalization system
*
* (C) Giancarlo Niccolai
*

#include "inkey.ch"

Procedure MAIN()
   LOCAL nChoice, nLangs
   LOCAL aLanguages
   LOCAL aLangCodes := { "en_US", "it_IT", "fr_FR", "es_MX", "pl_PL" }

   SET COLOR TO W+/B
   SetMode( 25, 80 )

   nChoice := 1
   nLangs := Len( aLangCodes ) + 1
   DO WHILE nChoice < nLangs .and. nChoice > 0
      aLanguages := { ;
         i18n( "International" ), ;
         i18n( "Italian" ), ;
         i18n( "French" ), ;
         i18n( "Spanish" ), ;
         i18n( "Polish" ), ;
         i18n( "Quit" ) }

      CLEAR SCREEN
      @2,10 SAY i18n( "X H A R B O U R - Internationalization test " )
      @4,10 SAY i18n( "Current language: " ) + HB_I18NGetLanguageName() +;
                  "(" +HB_I18NGetLanguage() +")"
      @6,10 SAY i18n( "This is a test with a plain string")
      @7,10 SAY i18n( "This is a test " + "with a static '+' string" )
      @8,10 SAY i18n( "This is a test using a 'compile time' '" +chr(65)+"'")
      @9,10 SAY i18n( "Test mixing" + e"\tescaped\t")

      IF !isDirectory("i18n")
         SET COLOR TO GR+/B
         @19,10 SAY i18n( "WARNING!" )
         @20,10 SAY i18n( "The .\i18n folder is missing." )
         @21,10 SAY i18n( "It contains the .hit files that contain" )
         @22,10 SAY i18n( "the translations. Please restore it." )
         SET COLOR TO W+/B
      ENDIF

      @12,10 SAY i18n( "Select Language: " )
      MakeBox( 12,40, 13+len(aLanguages), 55 )
      nChoice := Achoice(13, 41, 12+len(aLanguages), 54, aLanguages,,, ;
         Ascan( aLangCodes, { |x| x == HB_I18NGetLanguage() } ) )

      IF nChoice > 0 .and. nChoice < nLangs
         HB_I18NSetLanguage( aLangCodes[ nChoice ] )
      ENDIF
   ENDDO
   @24, 0 SAY ""


RETURN

PROCEDURE MakeBox( nRow, nCol, nRowTo, nColTo )
   @nRow, nCol, nRowTo, nColTo ;
        BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) +;
        Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
RETURN

