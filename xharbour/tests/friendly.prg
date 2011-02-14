//
// $Id$
//

***********
* 
* This program demonstrates how to implement friends classes
*
* Use: bldtest friendly friendl2
*

#include "hbclass.ch"


PROCEDURE Main()
   LOCAL oOne, oTwo, oThree, e
   
   oOne   := One()
   oTwo   := Two():New(oOne)
   oThree := Three():New(oOne)

   oTwo:Testing()

   TRY
      oThree:Testing()
   CATCH e
      ? "Class: THREE is NOT friend of class: ONE"
   END

   // Now, the class Three is friend of class One
   __ClsFriendly( oOne, oThree )

   oThree:Testing()
   
   TRY
       TestFriend( oOne )
   CATCH
      ? "Function: TESTFRIEND is NOT friend of Class: ONE"
   END   

   // Now, the class Three is friends with One
   __ClsFriendly( oOne, @TestFriend() )

   TestFriend( oOne )

    Trusted( oOne )
Return


CLASS One
   METHOD Test() HIDDEN
   FRIEND FUNCTION Trusted   
ENDCLASS

PROCEDURE Test( cName ) Class One
   ? cName + " is friend of class: " + ::Classname()
RETURN
