//
// $Id: friendl2.prg,v 1.1 2005/07/12 04:19:31 walito Exp $
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

METHOD Testing() CLASS Two
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

METHOD Testing() CLASS Three
   ::oCall:Test( ::Classname() )
RETURN


