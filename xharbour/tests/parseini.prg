******************************************************************
* parseini.prg
*
* $Id: parseini.prg,v 1.2 2003/03/19 13:30:47 jonnymind Exp $
* Test for Ini file reading/writing
*
* Giancarlo Niccolai
*

PROCEDURE Main( cName )
   LOCAL aIni, aSect
   LOCAL cSection
   LOCAL cKey
   LOCAL nRow := 1

   set color to w+/b
   CLEAR SCREEN
   @nRow++,20 SAY "X H A R B O U R - .INI file parser test"
   @nRow++,5 SAY "Call from command line using a .ini filename as the only parameter"
   nRow++

   IF Empty( cName )
      cName := "parseini.ini"
      @nRow++, 5 SAY "Using default parseini.ini file"
   ENDIF

   aIni := HB_ReadIni( cName )

   @nRow, 0

   ? "Content of " + cName
   
   IF Empty( aIni )
      ? "Not a valid .ini file!"
   ELSE
      FOR EACH cSection IN aIni:Keys
         ?
         ? "Section [" + cSection + "]"
         aSect := aIni[ cSection ]

         FOR EACH cKey IN aSect:Keys
            ? cKey + " = " + aSect[ cKey ]
         NEXT
      NEXT
   ENDIF
   
   ?
   ? "Adding section 'Added', with key NEW = new"
   aIni[ "Added" ] := Hash()
   aIni[ "Added" ][ "NEW" ] := "new"

   ? "Writing output to parseini_out.ini"
   IF HB_WriteIni( "parseini_out.ini", aIni, "#Generated file; don't touch", "#End of file")
      ? "File written"
   ELSE
      ? "Can't write file"
   ENDIF
   ?
   ? "Press any key to end"
   Inkey(0)
RETURN
