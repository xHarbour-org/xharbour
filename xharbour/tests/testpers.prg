//
// $Id: testpers.prg,v 1.4 2003/05/24 00:29:10 ronpinkas Exp $
//
// Class HBPersistent test

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oTest := Test():New()
   LOCAL oTest2 := Test2():New()

   oTest:One   = "hello"
   oTest:Two   = 123
   oTest:Three = { 1, 2, { 10, 20, 30 }, 3 }
   oTest:Four  = oTest2   // We store another persistent object here

   oTest2:Five = "some more text"

   ? oTest:SaveToText()            // We save it to a text

   oTest:SaveToFile( "test.txt" )  // We save it to a file

   oTest := Test():New()
   oTest:LoadFromFile( "test.txt" )

   ? "After LoadFromFile() - should be identical!"
   ?
   ? oTest:SaveToText()            // We save it to a text

   ? "Evaluating Restored Block Property..."
   ? Eval( oTest:Five )

RETURN

CLASS Test FROM HBPersistent

   DATA   One       PROPERTY
   DATA   Two       PROPERTY
   DATA   Three     PROPERTY
   DATA   Four      PROPERTY

   METHOD Another() INLINE { 1, { "One", "Two" }, Date() }
   METHOD More()    VIRTUAL

   // Yes we can even persist Blocks!!!
   DATA   Five      INIT {|| HB_QSelf():One } PROPERTY
ENDCLASS

CLASS Test2 FROM HBPersistent

   DATA Five  PROPERTY

ENDCLASS
