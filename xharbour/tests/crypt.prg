***********************************************************
* HB_Crypt() demo
*
* (C) 2003 Giancarlo Niccolai & Ron Pinkas
*
* $Id$
*
* Cryptography usin NXS algorithm.
* 
* Consider this experimental.
*


PROCEDURE Main()
   LOCAL cKey:= space(30), cSource:= Space(65), cCrypt
   LOCAL cResult

   set color to w+/b
   CLEAR SCREEN
   @2,25 SAY "X H A R B O U R - Cryptography test."

   @4,10 SAY "Fill the fields below; insert QUIT in one field to quit."

   DO WHILE .T.
      @6,3 SAY "Insert a passphrase (min. 8 char): " GET cKey
      @8,3 SAY "Text: " GET cSource
      READ

      IF Trim( Upper( cKey ) ) == "QUIT" .or. Trim( Upper( cSource ) ) == "QUIT"
         EXIT
      ENDIF
      
      IF Len( Trim(cSource) ) < 8 .or. Len( Trim(cKey) ) < 8
         @10, 3 SAY "Please, insert more than 8 characters in each field (press a key)."
         Inkey( 0 )
         @10, 3 SAY Space( 80 )
         LOOP
      ENDIF

      cCrypt  := HB_Crypt  ( Trim(cSource), Trim(cKey) )
      cResult := HB_Decrypt( cCrypt,  Trim(cKey) )
      cSource := Space( 65 )
      @12,1 SAY "Crypt:   " + Space( 65 )
      @15,1 SAY "Decrypt: " + Space( 65 )
      @12,1 SAY "Crypt:   " + cCrypt
      @15,1 SAY "Decrypt: " + cResult
   ENDDO

   @24,0
RETURN

