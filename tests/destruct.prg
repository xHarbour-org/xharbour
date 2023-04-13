#include "hbclass.ch"

PROCEDURE MAIN()

   LOCAL oVar := TMyClass(7), oVar2 := TMyClass( 8 )

   // Intentional cyclic ref to force test of Destructor from GC.
   oVar:oVar := oVar

   // On Return Destructor of oVar2  should be called
   // On VM Quit Destructor of oVar  should be called
RETURN

CLASS TMyClass

    DATA nVar

    DATA oVar

    METHOD NEW( nVar ) CONSTRUCTOR

    DESTRUCTOR MyDestructor

ENDCLASS

PROCEDURE MyDestructor CLASS TMyClass

   TraceLog()
   ? "Cleanup:", ::nVar

RETURN

METHOD New( nVar ) CLASS TMyClass

   ::nVar := nVar

RETURN Self
