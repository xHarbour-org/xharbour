//
// $Id: classch.prg,v 1.1 1999/10/04 18:46:27 vszel Exp $
//

// Testing Harbour hbclass.ch commands

#include "hbclass.ch"

//--------------------------------------------------------------------//

function Main()

   local o := TTest():New( "one", "two" )

   ? o:ClassName()
   ? o:One
   ? o:Two

   o:Test()

return nil

//--------------------------------------------------------------------//

CLASS TTest INHERIT TParent

   DATA One, Two, Three

   METHOD New( One, Two )

   METHOD Test() INLINE QOut( "Hello" )

ENDCLASS

//--------------------------------------------------------------------//

METHOD New( One, Two ) CLASS TTest

   Super:New()

   ::One = One
   ::Two = Two

return Self

//--------------------------------------------------------------------//

CLASS TParent

   DATA One

   METHOD New()

ENDCLASS

//--------------------------------------------------------------------//

METHOD New() CLASS TParent

   ? "TParent:New()"

return Self

//--------------------------------------------------------------------//
