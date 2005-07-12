//
// $Id: friendl2.prg,v 1.0 2004/03/06 13:20:32 jonnymind Exp $
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
RETURN

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
RETURN

METHOD Testing() CLASS Three
   ::oCall:Test( ::Classname() )
RETURN


