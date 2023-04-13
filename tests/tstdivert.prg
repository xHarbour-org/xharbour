#include "divert.ch"

#define DIVERT_OOP

PROCEDURE Main(p1)
   LOCAL b := { |_1| QOut( _1 ) }

   #ifdef DIVERT_OOP
      LOCAL o := ChildClass( 7 )

      ? o:x
      Eval( b, ProcName() )
   #endif

   DIVERT TO ( @SomeProc() ) FLAGS DIVERT_RESUME
   Eval( b )
RETURN

PROCEDURE SomeProc(p1)
  LOCAL b

  // LOCAL of the DIVERT is SHAREd with it's parent!!!
  Eval( b, ProcName() )
  // Override it!
  b := {|| QOut( "Diverted" ) }
RETURN

#ifdef DIVERT_OOP
  #include "hbclass.ch"

  // To avoid missing external for the (@New()) below - Class messages are created at R/T.
  DYNAMIC New

  CLASS SuperClass
     DATA x
     METHOD New( x ) CONSTRUCTOR
  ENDCLASS

  METHOD New( x ) CLASS SuperClass
     ? "Super", ::ClassName, ProcName()
     ::x++
  RETURN Self

  CLASS ChildClass FROM SuperClass
     METHOD New( x ) CONSTRUCTOR
  ENDCLASS

  METHOD New( x ) CLASS ChildClass
     ? "Child", ProcName(), x
     ::x := x
     TRY
        DIVERT TO ( @New() ) OF ::SuperClass
     FINALLY
        ? "Finally, even without DIVERT_RESUME"
     END

     ? "Should never get here, unless an Error was trapped."
  RETURN Self
#endif
