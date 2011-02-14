//
// $Id$
//

***********
* 
* This program demonstrates how to use DELEGATE messages in class
*
* Use: bldtest delegate
*

#include "hbclass.ch"

Function Main()

Local oObj

oObj := One():New()

? oObj:Sum( 1, 2, 3 )
? oObj:Sum( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 )



CLASS One
   DATA oCalc

   METHOD New( )

   DELEGATE Sum IS Sum TO oCalc  // oCalc is the name of DATA in class One
ENDCLASS

METHOD New( )  CLASS One
   ::oCalc := Calc()
RETURN Self


CLASS Calc
   METHOD Sum()
ENDCLASS

METHOD Sum(...)  CLASS Calc
Local nSum := 0, n

FOR EACH n IN hb_aParams()
   nSum += n
NEXT
RETURN nSum

