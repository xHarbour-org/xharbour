***********************************************************
* idletest.prg
*
* $Id: regextest.prg,v 1.3 2003/08/23 10:48:21 jonnymind Exp $
*
* This test demonstrates usage of IDLE functions;
* XHarbour allows to use both codeblocks and executable arrays.
* Executable arrays are faster than codeblocks and can be
* dynamically changed (or queried) by direct access to their
* elements.
*
* Idle callbacks can be explicitly called wit HB_IdleState(),
* but may also be automatically called when the system is
* waiting for user input (so, they will be called while
* Inkey( 0 ) is waiting).
*
* Giancarlo Niccolai
*


PROCEDURE Main()
   LOCAL nId1, nId2
   nId1 := HB_IdleAdd( {||idleFunc( 10, "From Block" )} )
   nId2 := HB_IdleAdd( { @idleFunc(), 11, "FromArray"} )

   SET COLOR TO w+/B
   CLEAR SCREEN
   SET CURSOR OFF
   @1,0 SAY Padc( "X H A R B O U R - Idle Function Test.", MaxCol() )
   @3,10 SAY "In lines 10 and 11, two different idle functions"
   @4,10 SAY "will make some text to flash."

   @20,10 SAY "Press any key to terminate"
   Inkey( 0 )

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
