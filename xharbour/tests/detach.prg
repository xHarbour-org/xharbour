PROCEDURE Main()
   LOCAL bResult, lSomeVar := .T.

   CLS

   Eval( {|lEnd| bResult := SomeStuff( @lEnd ), SomethingElse( @lEnd ) }, @lSomeVar )

   // In clipper it's .T. because Detached Local in SomeStuff() has been severed
   // before its BYREF Source has been modified to .F. in SomethingElse()
   ? Eval( bResult )

   Eval( {|lEnd| SomethingElse( @lEnd ), bResult := SomeStuff( @lEnd ) }, @lSomeVar )
   ? Eval( bResult )            // In clipper it's .F. (Severed Detach, after SomethingElse()).

   // Here is a sample that a Severed Detached is still Detached and Valid.
   Eval( bResult, .T. )
   ? Eval( bResult ) // In clipper it's .T. as expected.
RETURN

FUNCTION SomethingElse( lVar )
   lVar := .F.
RETURN Nil

FUNCTION SomeStuff( lVar )
RETURN {|_1| IIF( PCount() >= 1, lVar := _1, lVar ) }
