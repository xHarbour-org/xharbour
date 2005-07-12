//
// $Id: friendly.prg,v 1.0 2004/03/06 13:20:32 jonnymind Exp $
//

***********
* 
* This program demonstrates how to implement friends classes
*
* Use: bldtest friendly friendl2
*

#include "hbclass.ch"


Function Main()
Local oOne, oTwo, oThree, e
   oOne   := One()
   oTwo   := Two():New(oOne)
   oThree := Three():New(oOne)

   oTwo:Testing()

TRY
   oThree:Testing()
CATCH e
   ? "Class THREE is NOT friends with ONE"
END

  // Now, the class Three is friends with One
  __ClsFriendly( oOne, oThree )

   oThree:Testing()

Return


CLASS One
   METHOD Test() HIDDEN
ENDCLASS

METHOD Test( cName ) Class One
   ? "Class "+cName+" is friends with "+::Classname()
RETURN



