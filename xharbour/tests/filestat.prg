***********************************************************
* filestat.prg
* $Id: gtclose.prg,v 1.2 2004/01/15 00:17:56 fsgiudice Exp $
* Test for filestat (and TimeOfDay) function.
*
* Giancarlo Niccolai
*

#include "inkey.ch"

PROCEDURE Main( cFile )
   LOCAL GetList := {}

   SET COLOR to "w+/b"
   CLEAR SCREEN
   @2,0 SAY Padc( "X H A R B O U R - FileStat() function test", MaxCol() )

   IF Empty( cFile )
      DO WHILE LastKey() != K_ESC
         @4,2 SAY "Insert a file name and you'll get file stats. "
         @5,2 SAY "You can also CALL this program with a name from "
         @6,2 SAY "command line."

         cFile := Space( 255 )
         @8,2 SAY "Filename: " GET cFile PICTURE "@s40"
         READ

         IF LastKey() == K_ESC
            EXIT
         ENDIF

         SET COLOR to "w+/b"
         CLEAR SCREEN
         ShowStats( Trim(cFile) )
         @2,0 SAY Padc( "X H A R B O U R - FileStat() function test", MaxCol() )
      ENDDO
   ELSE
      ShowStats( cFile )
      @20,0 SAY Padc( "Press any key to exit", MaxCol() )
      Inkey( 0 )
   ENDIF

RETURN


PROCEDURE ShowStats( cFile )
   LOCAL cAttrib, nSize, dCDate, nCTime, dMDate, nMTime
   LOCAL cColor

   IF FileStats( cFile, @cAttrib, @nSize, @dCDate, @nCTime, @dMDate, @nMTime )
      @10,2 SAY "File name:  " + cFile
      @11,2 SAY "Attributes: '" + cAttrib + "'"
      @12,2 SAY "File size:  " + Alltrim( Str( nSize ) )
      @13,2 SAY "Created:    " + DtoC( dCDate ) + " " + TimeOfDay( nCTime )
      @14,2 SAY "Modified:   " + DtoC( dMDate ) + " " + TimeOfDay( nMTime )
   ELSE
      cColor = SetColor( "r+/b" )
      @10,2 SAY "NOT FOUND: " + cFile
      SetColor( cColor )
   ENDIF
RETURN
