***********************************************************
* bkgtest.prg
*
* $Id: bkgtest.prg,v 1.2 2003/12/19 03:27:52 fsgiudice Exp $
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
   LOCAL nId1, nId2, nId3, nId4, n //, lRun := Set( _SET_BACKGROUNDTASKS, .T. )
   nId1 := HB_IdleAdd( {||idleFunc( 10, "On Idle - From Block" )} )
   nId2 := HB_IdleAdd( { @idleFunc(), 11, "On Idle - From Array"} )
   nId3 := HB_BackgroundAdd( { @CheckFunc() } )
   nId4 := HB_BackgroundAdd( { @TimerFunc() } )
   HB_BackgroundAdd( { @Counter1Func() } )
   HB_BackgroundAdd( { @Counter2Func() } )
   HB_BackgroundAdd( { @Counter3Func() } )

   SET COLOR TO w+/B
   CLEAR SCREEN
   SET CURSOR OFF
   @1,0 SAY Padc( "X H A R B O U R - Background and Idle Function Test.", MaxCol() )

   DispInfo( "Background Tasks defined but not running." )
   SecondsSleep( 3 )

   DispInfo( "Now i'll run a single task manually" )
   SecondsSleep( 3 )

   DispInfo( "Timer in action" )
   FOR n := 1 TO 1000000
       HB_BackgroundRun( nId4 )
   NEXT
   @3,10 SAY "In lines 10 and 11, two different idle functions"
   @4,10 SAY "will make some text to flash."
   DispInfo( "Now you will see idle functions running until you press any key" )
   inkey(0)

   DispInfo( "Now manually force to run all background functions" )
   SecondsSleep( 3 )
   DispInfo( "Background functions running manually" )
   FOR n := 1 TO 10000
       HB_BackgroundRun()
   NEXT
   DispInfo( "Now you will see idle functions running until you press any key" )
   inkey(0)

   DispInfo( "Now i set on background tasks" )
   SecondsSleep( 3 )

   SET BACKGROUND TASKS ON

   @5,10 SAY "Background functions show timer, counters and check time elapsed."
   @6,10 SAY "After 20 seconds this program will be forcely quitted."
   DispInfo( "Program in action with active background tasks" )
   //@20,10 SAY "Press ESC key to terminate"
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

   //SET COLOR TO w/n
   //CLEAR SCREEN
   //@0,0 SAY "Removing " + ValToPrg( HB_IdleDel( nId1 ) )
   //@1,0 SAY "Removing " + ValToPrg( HB_IdleDel( nId2 ) )
   //@2,0 SAY "Done"
   //@4,0
   //SET CURSOR ON

RETURN

PROCEDURE DispInfo( cMsg )
  IF cMsg == NIL
     cMsg := ""
  ENDIF
  @ 23, 0 SAY PadC( cMsg, MaxCol() )
RETURN

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
     DispInfo( "Time elapsed! Quitting. Press any key to exit (note idle running meanwhile)" )
     Inkey(0)
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

PROCEDURE Counter1Func()
  STATIC nCount

  IF nCount == NIL
     nCount := 0
  ENDIF
  //IF nCount % 10 == 0
     @ 16, 60 SAY "Count1: " + Str( nCount )
  //ENDIF
  nCount++

RETURN

PROCEDURE Counter2Func()
  STATIC nCount

  IF nCount == NIL
     nCount := 0
  ENDIF
  //IF nCount % 10 == 0
     @ 17, 60 SAY "Count2: " + Str( nCount )
  //ENDIF
  nCount++

RETURN

PROCEDURE Counter3Func()
  STATIC nCount

  IF nCount == NIL
     nCount := 0
  ENDIF
  //IF nCount % 10 == 0
     @ 18, 60 SAY "Count3: " + Str( nCount )
  //ENDIF
  nCount++

RETURN
