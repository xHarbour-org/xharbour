************************************************************
* i18ntest.prg
* $Id: xmltest.prg,v 1.4 2003/06/17 10:16:10 jonnymind Exp $
*
* Test for internationalization system
*
* (C) Giancarlo Niccolai
*

Procedure MAIN()

   SET COLOR TO W+/B
   CLEAR SCREEN
   @2,10 SAY i18n( "X H A R B O U R - Internationalization test " )

   @4,10 SAY i18n( "This is a test wiht a plain string")
   @6,10 SAY i18n( "This is a test " + "with a static '+' string" )
   @7,10 SAY i18n( "This is a test using a 'compile time' '" +chr(65)+"'")
   @8,10 SAY i18n( "Test mixing" + e"\tescaped\t")

   @20,20 SAY i18n( "Done. Press any key to continue" )
   Inkey(0)
RETURN

