//
// $Id$
//
// Class HBPersistent test

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oTest := Test():New()
   LOCAL oTest2 := Test2():New()
   LOCAL sStream1, sStream2

   oTest:One   := "Normal String"
   oTest:Two   := "String with '[ and " + '"'
   oTest:Three := { 1, .T., Date(), { "HashKey" => "Hash Value" }, { 10, 20, 30 }, 3 }
   oTest:Four  := oTest2 // We store another persistent object here

   /*
      Yes we can persist Blocks too, as long as they don't
      reference any detached variables, and are restored within
      the same UNMODIFIED application symbol table (or do not
      reference any symbol).
   */
   oTest:Five  := { || Alert( "My persisted codeblock property." ) }

   oTest2:One = "Text with Multiple" + Chr(10) + "Lines"

   sStream1 := oTest:SaveToText()  // We save it to a text
   ? sStream1
   oTest:SaveToFile( "test.txt" )  // We save it to a file

   oTest := Test():New()
   oTest:LoadFromFile( "test.txt" )

   ? "After LoadFromFile() - should be identical!"
   ?
   sStream2 := oTest:SaveToText()  // We save it to a text
   ? sStream2

   IF ! ( sStream1 == sStream2 )
      Alert( "Oops! Streams are NOT identical." )
   ENDIF

   ? "Evaluating Restored Block Property..."
   Eval( oTest:Five )

RETURN

CLASS Test FROM HBPersistent

   DATA   One       PROPERTY
   DATA   Two       PROPERTY
   DATA   Three     PROPERTY
   DATA   Four      PROPERTY
   DATA   Five      PROPERTY

ENDCLASS

CLASS Test2 FROM HBPersistent

   DATA One  PROPERTY

ENDCLASS
