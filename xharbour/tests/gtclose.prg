***********************************************************
* gtclose.prg
* $Id: regextest.prg,v 1.3 2003/08/23 10:48:21 jonnymind Exp $
* Test for gt close callback and handler.
*
* Giancarlo Niccolai
*
#include "inkey.ch"

PROCEDURE Main()
   LOCAL nKey, bAction

   SET COLOR TO w+/b
   SET CONFIRM ON
   CLEAR SCREEN

   @2,0 SAY Padc("X H A R B O U R - GT Close Handler test", Maxcol() )

   @4,5 SAY "This is just a test to demonstrate GT Close Callback capability."
   @5,5 SAY "Pressing a key, you can terminate this program normally."
   @6,5 SAY "If your terminal provides an alternative way to close the program, i.e."
   @7,5 SAY "a [X] mark on a graphical window, the GT will ask you if you want to"
   @8,5 SAY "really quit before to proceed."
   @8,5 SAY "If the GT has not this ablity, you won't see nothing special."

   @11,0 SAY Padc( "TEST1: Test with ALT+C. Now window [X] button acts as ALT+C.", Maxcol() )
   @12,0 SAY Padc( "Press a key to skip this test, or close window to test.", Maxcol() )
   @13,maxcol()/2
   nKey := Inkey(0)

   // Test2: use SetCloseEvent to set for a close handler
   @11,0 SAY Padc( "TEST2: Test with SetKey(). Inkey returns closing event.", Maxcol() )
   @12,0 SAY Padc( "Press a key to skip this test, or close window to test.", Maxcol() )
   @13,maxcol()/2
   SetCloseEvent( 2050 )
   nKey := Inkey(0)

   IF nKey == 2050
      Test1Complete()
   ENDIF


   // Test 2: prepare to intercept GT close request.
   // You may pass a function name, a function ID, a codeblock or an executable array,
   // that is an array containing the function name, the function ID, the codeblock
   // or even an object and a message, followed by its parameters.
   // In this case you see a call with a function ID
   @11,0 SAY Padc( "Now there is a handler for window closing.", Maxcol() )
   @12,0 SAY Padc( "Press a key terminate, or close window to test.", Maxcol() )
   @13,maxcol()/2
   SetGtCloseHandler( @ReallyQuit() )

   Inkey(0)

   SET COLOR TO w/n
   CLEAR SCREEN
RETURN

PROCEDURE ReallyQuit()
   LOCAL cIn

   @15,0 SAY Padc("WARNING: GT [X] button has been pressed, or GT window is required to quit.", Maxcol())
   @16,0 SAY Padc("Really quit? (Y/N)", Maxcol())

   cIn = Upper( Chr(Inkey( 0 )) )
   DO WHILE cIn != "Y" .and. cIn != "N"
      cIn = Upper( Chr(Inkey( 0 )) )
   ENDDO

   IF cIn == "N"
      // Safe! all back to work.
      @15,0 SAY Space( Maxcol() )
      @16,0 SAY Space( Maxcol() )
   ELSE
      //Quit confirmed
      SET COLOR TO w/n
      CLEAR SCREEN
      // Quit must be explicitly called
      __QUIT()
   ENDIF
RETURN

PROCEDURE Test1Complete()
   @11,0 SAY Padc( "TEST2 ( intercepting using SetKey ) COMPLTETE.", Maxcol() )
   @12,0 SAY Padc( "Press a key to proceed to next test.", Maxcol() )
   Inkey(0)
RETURN