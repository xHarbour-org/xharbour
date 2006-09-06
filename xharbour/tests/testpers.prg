//
// $Id: testpers.prg,v 1.5 2003/05/26 00:19:16 ronpinkas Exp $
//
// Class HBPersistent test

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oTest := Test():New()
   LOCAL oTest2 := Test2():New()
   LOCAL sStream1, sStream2
   
   oTest:One   = "hello"
   oTest:Two   = 123
   oTest:Three = { 1, 2, { 10, 20, 30 }, 3 }
   oTest:Four  = oTest2 // We store another persistent object here

   oTest2:Five = "some more text"

   sStream1 := oTest:SaveToText()  // We save it to a text
   ? sStream1   
   oTest:SaveToFile( "test.txt" )  // We save it to a file

   oTest := Test():New()
   oTest:LoadFromFile( "test.txt" )

   ? "After LoadFromFile() - should be identical!"
   ?
   sStream2 := oTest:SaveToText()  // We save it to a text
   ? sStream2                      // We save it to a text

   IF ! sStream1 == sStream2
      Alert( "Oops! Streams are NOT identical." )      
   ENDIF
   
   ? "Evaluating Restored Block Property..."
   ? Eval( oTest:Five )

RETURN

CLASS Test FROM HBPersistent

   DATA   One       PROPERTY
   DATA   Two       PROPERTY
   DATA   Three     PROPERTY
   DATA   Four      PROPERTY
   
   /* 
      Yes we can persist Blocks too, as long as they don't 
      reference any detached variables, and are restored within
      the same UNMODIFIED application symbol table (or do not
      reference any symbol).
   */
   DATA   Five      PROPERTY INIT {|| SomeFunc() }
      
   METHOD Another() INLINE { 1, { "One", "Two" }, Date() }
   METHOD More()    VIRTUAL

ENDCLASS

CLASS Test2 FROM HBPersistent

   DATA Five  PROPERTY

ENDCLASS

FUNCTION SomeFunc()
RETURN 77   