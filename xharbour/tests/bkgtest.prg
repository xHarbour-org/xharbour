***********************************************************
* bkgtest.prg
*
* $Id: bkgtest.prg,v 1.3 2003/12/21 23:35:27 fsgiudice Exp $
*
* This test demonstrates usage of BACKGROUND functions that are an extension of IDLE functions;
* this is a variant of idle functions that runs only on idle state (as inkey(0) does)
* Background functions run on every vm execution. PLEASE BE CAREFULLY using these functions.
* Inside background functions you have to think that them are executed on EVERY vm call.
* Look at idletest.prg too.
*
* (C) 2003 - Francesco Saverio Giudice <info@fsgiudice.com>
*

#include "set.ch"
#include "inkey.ch"

PROCEDURE Main()
   LOCAL nId1, nId2, nId3, nId4, nId5, nId6, nId7, n //, lRun := Set( _SET_BACKGROUNDTASKS, .T. )
   LOCAL lActive := .T.
   LOCAL nSpeed  := 500
   nId1 := HB_IdleAdd( {||idleFunc( 10, "On Idle - From Block" )} )
   nId2 := HB_IdleAdd( { @idleFunc(), 11, "On Idle - From Array"} )
   nId3 := HB_BackgroundAdd( { @CheckFunc() }, 1000, .F. ) //This task is defined but not active
   nId4 := HB_BackgroundAdd( { @TimerFunc() }, 1000 )
   nId5 := HB_BackgroundAdd( { @Counter1Func() } )
   nId6 := HB_BackgroundAdd( { @Counter2Func() }, nSpeed )
   nId7 := HB_BackgroundAdd( { @Counter3Func() }, 100 )
   HB_BackgroundAdd( { @Ticker() }, 100 )

   SET COLOR TO w+/B
   CLEAR SCREEN
   SET CURSOR OFF
   @1,0 SAY Padc( "X H A R B O U R - Background and Idle Function Test.", MaxCol() )

   DispInfo( "Background Tasks defined but not running." )
   SecondsSleep( 3 )

   DispInfo( "Now i'll run a single task manually" )
   SecondsSleep( 3 )

   DispInfo( "Counter in action" )
   FOR n := 1 TO 10000
       HB_BackgroundRun( nId5 )
   NEXT
   @3,5 SAY "In lines 10 and 11, two different idle functions"
   @4,5 SAY "will make some text to flash."
   DispInfo( "Now you will see idle functions running until you press any key" )
   inkey(0)

   DispInfo( "Now manually force to run all background functions" )
   SecondsSleep( 3 )
   DispInfo( "Background functions running manually" )
   FOR n := 1 TO 100000
       HB_BackgroundRun()
       IF Inkey() == K_ESC
          EXIT
       ENDIF
   NEXT
   DispInfo( "Now you will see idle functions running until you press any key" )
   inkey(0)

   DispInfo( "Now i set on background tasks" )
   SecondsSleep( 3 )

   SET BACKGROUND TASKS ON

   @5,5 SAY "Background functions show timer, ticker, counters and check time elapsed."
   @6,5 SAY "After 40 seconds this program will be forcely quitted."
   DispInfo( "Program in action with active background tasks" )

   // Now i make check time on
   HB_BackgroundActive( nId3, .T. )
   @ 18, 10 SAY "Main Program Counter: "
   n := 0
   DO WHILE .T.
       @ 18, 32 SAY ++n
       IF n % 1000 == 0
          IF Inkey() == K_ESC
             EXIT
          ENDIF
       ENDIF
       IF n % 20000 == 0
          lActive := !lActive
          HB_BackgroundActive( nId7, lActive )
          @ 19, 60 SAY "Count3 " + IIF( lActive, "ON ", "OFF" )
       ENDIF
       IF n % 60000 == 0
          IF nSpeed == 500
             nSpeed := 0
          ELSE
             nSpeed := 500
          ENDIF
          HB_BackgroundTime( nId6, nSpeed )
          @ 20, 60 SAY "Count2 Time to " + Str( nSpeed, 3 )
       ENDIF
       IF n == 130000
          HB_BackgroundDel( nId5 )
          @ 16, 60 SAY "Count1: DELETED    "
       ENDIF

   ENDDO

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
  STATIC nElapsed
  IF nSeconds == NIL
     nSeconds := Seconds()
  ENDIF
  nElapsed := Int( Seconds() - nSeconds )
  @ 19, 10 SAY "Seconds Elapsed: " + Str( nElapsed )
  IF nElapsed > 40
     DispInfo( "Time elapsed! Quitting. Press any key to exit (note idle running meanwhile)" )
     Inkey(0)
     QUIT
  ENDIF
RETURN

PROCEDURE TimerFunc()
  @ 15, 60 SAY "Time: " + Time()
RETURN

PROCEDURE Counter1Func()
  STATIC nCount

  IF nCount == NIL
     nCount := 0
  ENDIF
  @ 16, 60 SAY "Count1: " + Str( nCount )
  nCount++

RETURN

PROCEDURE Counter2Func()
  STATIC nCount

  IF nCount == NIL
     nCount := 0
  ENDIF
  @ 17, 60 SAY "Count2: " + Str( nCount )
  nCount++

RETURN

PROCEDURE Counter3Func()
  STATIC nCount

  IF nCount == NIL
     nCount := 0
  ENDIF
  @ 18, 60 SAY "Count3: " + Str( nCount )
  nCount++

RETURN

PROCEDURE Ticker()
  STATIC nPos
  STATIC cText
  LOCAL cString
  IF cText == NIL
     cText := "This is a sample text. You can press ESC in any moment to exit.    Please note the different speed of counters.    "
     nPos   := 1
  ENDIF
  cString := Substr( cText, nPos ) + SubStr( cText, 1, nPos-1 )
  @ 22, 0 SAY PadR( cString , MaxCol() )
  nPos++
  IF nPos > Len( cText )
     nPos := 1
  ENDIF
RETURN
