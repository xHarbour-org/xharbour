* QTTest.prg
* $Id$
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
    Set cursor off

    bGoOn := .T.
    StartThread( @RealMain() )

    HB_QTAppExec()
    set Cursor on
RETURN

PROCEDURE RealMain()
   
   SET COLOR TO w+/b
   
   CLEAR SCREEN
   
   @10,30 SAY "HELLO WORLD"
   @8,20 SAY "X H A R B O U R - QT GT Test"
   @12,20 SAY "More to come in the future"
   
   Inkey( 0 )
RETURN
