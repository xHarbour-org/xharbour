***********************************************************
* bkgtest.prg
*
* $Id: idletest.prg,v 1.1 2003/12/18 14:47:54 jonnymind Exp $
*
* This test demonstrates usage of BACKGROUND functions that are an extension of IDLE functions;
* this is a variant of idle functions that runs only on idle state (as inkey(0) does)
* Background functions run on every vm execution. PLEASE BE CAREFULLY using these functions.
* Inside background functions you have to think that them are executed on EVERY vm call.
* Look at idletest.prg too.
*
* Francesco Saverio Giudice
*

#include "set.ch"
#include "inkey.ch"

PROCEDURE Main()
   LOCAL nId1, nId2, nId3, nId4, n, lRun := Set( _SET_BACKGROUNDTASKS, .T. )
   nId1 := HB_IdleAdd( {||idleFunc( 10, "From Block" )} )
   nId2 := HB_IdleAdd( { @idleFunc(), 11, "FromArray"} )
   nId3 := HB_BackgroundAdd( { @CheckFunc() } )
   nId4 := HB_BackgroundAdd( { @TimerFunc() } )

   SET COLOR TO w+/B
   CLEAR SCREEN
   SET CURSOR OFF
   @1,0 SAY Padc( "X H A R B O U R - Background and Idle Function Test.", MaxCol() )
   @3,10 SAY "In lines 10 and 11, two different idle functions"
   @4,10 SAY "will make some text to flash."
   @5,10 SAY "Background functions show timer and check time elapsed."
   @6,10 SAY "After 20 seconds this program will be forcely quitted."

   @20,10 SAY "Press ESC key to terminate"
   //Inkey( 0 )
   @ 18, 10 SAY "Counter: "
   FOR n := 1 TO 5000000
       IF n % 10 == 0
          @ 18, 19 SAY Str( n )
       ENDIF
       IF Inkey() == K_ESC
          EXIT
       ENDIF
   NEXT

   SET COLOR TO w/n
   CLEAR SCREEN
   @0,0 SAY "Removing " + ValToPrg( HB_IdleDel( nId1 ) )
   @1,0 SAY "Removing " + ValToPrg( HB_IdleDel( nId2 ) )
   @2,0 SAY "Done"
   @4,0
   SET CURSOR ON


PROCEDURE IdleFunc( nRow, cStr )
   @nRow, 10 SAY cStr
   ThreadSleep( 100 )
   @nRow, 10 SAY Space(69)
RETURN

PROCEDURE CheckFunc()
  STATIC nSeconds
  STATIC nSecElapsed
  LOCAL nNow := Seconds()
  LOCAL nElapsed
  IF nSeconds == NIL
     nSeconds := Seconds()
  ENDIF
  nElapsed := nNow - nSeconds
  IF nSecElapsed == NIL .OR. Int( nElapsed ) <> nSecElapsed
     nSecElapsed := Int( nElapsed )
     @ 22, 10 SAY "Seconds Elapsed: " + Str( nSecElapsed )
  ENDIF
  IF nElapsed > 20
     @ 23, 10 SAY "Time elapsed! Quitting"
     QUIT
  ELSEIF Int( nElapsed ) % 5 == 0
     // Run forcely idle functions
     HB_IdleState()
  ENDIF
RETURN

PROCEDURE TimerFunc()
  STATIC cTime
  LOCAL cNow := Time()

  IF cTime == NIL .OR. cNow <> cTime
     cTime := cNow
     @ 15, 60 SAY "Time: " + cTime
  ENDIF

RETURN
