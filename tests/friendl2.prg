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

CLASS Two
   DATA oCall
   METHOD New()  CONSTRUCTOR
   METHOD Testing()
ENDCLASS

METHOD New( oCall ) CLASS Two
   ::oCall := oCall
   // This class is friends with One
   __ClsFriendly( One(), Self )
RETURN Self

PROCEDURE Testing() CLASS Two
   ::oCall:Test( ::Classname() )
RETURN

//----------------------------------------------------

CLASS Three
   DATA oCall
   METHOD New()  CONSTRUCTOR
   METHOD Testing()
ENDCLASS

METHOD New( oCall ) CLASS Three
   ::oCall := oCall
RETURN Self

PROCEDURE Testing() CLASS Three
   ::oCall:Test( "Class: " + ::Classname() )
RETURN

PROCEDURE TestFriend( o )
   o:Test( "Function: " + ProcName() )
RETURN 

PROCEDURE Trusted( o )
   o:Test( "Function: " + ProcName() )
RETURN 