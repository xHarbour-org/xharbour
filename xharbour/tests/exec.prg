***********************************************************
* exec.prg
* $Id: exec.prg,v 1.1 2003/02/18 23:54:57 jonnymind Exp $
*
* Test for indirect execution of functions and methods 
* using arrays as parameter lists
*
* Giancarlo Niccolai
*

#include "hbclass.ch"

PROCEDURE MAIN()
   LOCAL oTest, Mthd
   LOCAL aData
   LOCAL nRes

   CLEAR SCREEN

   @3,15 SAY "X H A R B O U R - Exec function from array test"

   hb_execFromArray( @MyFunc(), {6,5, "Function call 1" } )
   /* Using indirect function name notation */
   hb_execFromArray( "MyFunc", {7,5, "Function call 2" } )
   hb_execFromArray( @MyFunc(), {8,5, "Function call 3" } )

   oTest := Test():New()
   hb_execFromArray( oTest, "Hello", {10,5, "Method call 1" } )

   Mthd := HB_ObjMsgPtr( oTest, "Hello" )
   hb_execFromArray( oTest, Mthd, {11,5, "Method call 2 (using prebuilt msg pointer)" } )

   //Test of complete calls in arrays
   aData := { "MyFunc", 13, 5, "Function call through array 1" }
   hb_execFromArray( aData )
   aData[1] := (@MyFunc())
   aData[2]++
   aData[4] := "Function call through array 2"
   hb_execFromArray( aData )
   aData[1] := oTest
   ASize( aData, 5 )
   AIns( aData, 2 )
   aData[2] := "Hello"
   aData[3] ++
   aData[5] := "Method call through array 1"
   hb_execFromArray( aData )
   aData[2] := Mthd
   aData[3] ++
   aData[5] := "Method call through array 2"
   nRes := hb_execFromArray( aData )

   @aData[3]+1, 5 SAY "Check for return value: " + Ltrim( Str( nRes ) )

   hb_execFromArray( @MyProc(), {} )

RETURN

FUNCTION MyFunc( nRow, nCol, cText )
   @nRow, nCol SAY cText
RETURN (nRow * nCol)

PROCEDURE MyProc()
   @20, 15 SAY "Test complete - press any key"
   Inkey(0)
   CLEAR SCREEN
RETURN


CLASS Test
   METHOD Hello( nRow, nCol, cText )
ENDCLASS

METHOD Hello( nRow, nCol, cText )
   @nRow, nCol SAY "(FROM METHOD) " + cText
RETURN (nRow * nCol)

