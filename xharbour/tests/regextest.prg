***********************************************************
* regextest.prg
* $Id: regextest.prg,v 1.2 2003/08/19 15:33:20 jonnymind Exp $
* Test for regular expression functions -
* This allows to use a finetune regex to use them in programs
*
* Giancarlo Niccolai
*
#include "inkey.ch"

PROCEDURE Main()
   LOCAL ReCompiled
   LOCAL cRegex
   LOCAL cSentence
   LOCAL nRow, eError
   LOCAL aMatch, cMatch
   LOCAL GetList := {}

   SET COLOR TO w+/b
   SET CONFIRM ON
   CLEAR SCREEN

   @2,15 SAY "X H A R B O U R - Regular expression test"

   @4,5 SAY "Insert regular expression(s) and strings to test for."
   @5,5 SAY "Press ESC to exit"

   cRegex := Space(60)
   cSentence := space(120)
   WHILE LastKey() != K_ESC

      @8,5 SAY "REGEX : " GET cRegex PICTURE "@S30"
      @9,5 SAY "PHRASE: " GET cSentence PICTURE "@S60"
      READ
      IF LastKey() != K_ESC
         @12,5 CLEAR TO 24,79
         TRY
            reCompiled := HB_RegexComp( RTrim(cRegex) )
         CATCH eError
            @12,5 SAY "Invalid REGEX expression"
            LOOP
         END

         aMatch := HB_Regex( reCompiled, RTrim(cSentence) )
         IF aMatch != NIL
            @12,5 SAY "MATCHES:"
            nRow := 13
            FOR EACH cMatch in aMatch
               @nRow++, 5 SAY ">"+cMatch
            NEXT
         ELSE
            @12,5 SAY "No matches"
         ENDIF
      ENDIF

   ENDDO

   SET COLOR TO w/n
   CLEAR SCREEN
RETURN
