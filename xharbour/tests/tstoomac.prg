PROCEDURE Main()

   LOCAL oErr := ErrorNew(), cMessage := "Description", cPrefix := "Descript"

   oErr:&cMessage := "Assigned from Object Message Macro."

   ? oErr:&cMessage
   ? oErr:&cPrefix.ion
   ? oErr:&( cPrefix + "ion" )

   WITH OBJECT oErr
     :&cMessage := "Assigned from 'WITH OBJECT' Message Macro."

     ? :&cMessage
     ? :&cPrefix.ion
     ? :&( cPrefix + "ion" )
   END

RETURN
