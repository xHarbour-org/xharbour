* QTTest.prg
* $Id: qttest.prg,v 1.1 2003/01/30 00:46:25 jonnymind Exp $
* A test for gtQTc - QT lib baset graphic terminal console
*
* Giancarlo Niccolai
*
* QT GT works exactly as any other terminal except that
* the main function MUST launch the real program in a
* secondary thread, and then, after all initializations
* are done, it must call HB_QTAppExec()
*


PROCEDURE Main()
    bGoOn := .T.
    StartThread( @RealMain() )

RETURN HB_QTAppExec()

PROCEDURE RealMain()
   LOCAL cName := Space( 15 )
   Local nAge := 0

   SET COLOR TO w+/b

   CLEAR SCREEN

   @7,8, 16,70 BOX "$$$$$$" COLOR "r/b"
   @10,30 SAY "HELLO WORLD"
   @8,20 SAY "X H A R B O U R - QT GT Test"
   @12,20 SAY "More to come in the future"
   @13,10 say "Your name:" get cName
   @14,10 say "Your age:" get nAge
   Read
   @15,10 say "Hello " + RTrim( cName ) +", you are " + LTrim( Str( nAge ) )
   @23,30 say "Press a key to terminate"
   Inkey( 0 )
   HB_QTAppExit( 10 )
RETURN
