******************************************************************
* parseini.prg
*
* $Id$
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
   ? "Press any key to next text."
   Inkey(0)
   ?
   ? "REPEATING TESTS WITHOUT AUTOMATIC MAIN SECTION"

   aIni := HB_ReadIni( cName, /*default case*/ , /*Default key indicators */ , .F. )

   @nRow, 0

   ? "Content of " + cName

   IF Empty( aIni )
      ? "Not a valid .ini file!"
   ELSE
      FOR EACH cSection IN aIni:Keys
         /* Now (without automatic main), toplevel options may be in the root hash */
         aSect := aIni[ cSection ]

         IF HB_IsHash( aSect )
            /* It's a section */
            ?
            ? "Section [" + cSection + "]"

            FOR EACH cKey IN aSect:Keys
               ? cKey + " = " + aSect[ cKey ]
            NEXT
         ELSE
            /* It's a toplevel option */
            ? "TOPLEVEL option:", cSection + " = " + aSect
         ENDIF
      NEXT
   ENDIF

   ?
   ? "Adding section 'Added', with key NEW = new"
   aIni[ "Added" ] := Hash()
   aIni[ "Added" ][ "NEW" ] := "new"

   ? "Writing output to parseini_out1.ini"
   IF HB_WriteIni( "parseini_out1.ini", aIni,;
         "#Generated file without main auto section; don't touch", "#End of file",;
         .F. )
      ? "File written"
   ELSE
      ? "Can't write file"
   ENDIF
   ?
   ? "Press any key to next text."
   Inkey(0)

RETURN
