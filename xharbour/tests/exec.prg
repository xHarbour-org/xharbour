***********************************************************
* exec.prg
* $Id$
*
* Test for indirect execution of functions and methods 
* using arrays as parameter lists
*
* Giancarlo Niccolai
*

#include "hbclass.ch"

PROCEDURE MAIN()
   LOCAL oTest, Mthd

   CLEAR SCREEN

   @3,15 SAY "X H A R B O U R - Exec function from array test"

   hb_execFromArray( @MyFunc(), {10,5, "Function call 1" } )
   /* Using indirect function name notation */
   hb_execFromArray( "MyFunc", {11,5, "Function call 2" } )
   hb_execFromArray( @MyFunc(), {12,5, "Function call 3" } )

   oTest := Test():New()
   hb_execFromArray( oTest, "Hello", {14,5, "Method call 1" } )

   Mthd := HB_ObjMsgPtr( oTest, "Hello" )
   hb_execFromArray( oTest, Mthd, {15,5, "Method call 2 (using prebuilt msg pointer)" } )

   hb_execFromArray( @MyProc(), {} )
   Inkey(0)

RETURN

PROCEDURE MyFunc( nRow, nCol, cText )
   @nRow, nCol SAY cText
RETURN

PROCEDURE MyProc()
  @20, 15 SAY "Text complete - press any key"
RETURN


CLASS Test
   METHOD Hello( nRow, nCol, cText )
ENDCLASS

METHOD Hello( nRow, nCol, cText )
   @nRow, nCol SAY "(FROM METHOD) " + cText
RETURN .T.

