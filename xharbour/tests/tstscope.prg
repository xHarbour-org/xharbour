#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oChild := TChild():Create()
   LOCAL oErr

   oChild:PublicOfChild := "Public is Ok."

   TRY
      ? oChild:PrivateOfParent
      ? "OOPS", '[' + Str( ProcLine(), 2 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:operation, '[' + Str( ProcLine(), 2 ) + ']'
   END

   TRY
      ? oChild:ProtectedOfParent
      ? "OOPS", '[' + Str( ProcLine(), 2 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 2 ) + ']'
   END

   TRY
      oChild:ProtectedReadOnlyOfParent := "Can NOT assign a READONLY or PROTECTED outside of [DERIVED] Class!"
      ? "OOPS", '[' + Str( ProcLine(), 2 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 2 ) + ']'
   END

   ?
   ? oChild:ReadOnlyOfParent

RETURN

CLASS TParent

   DATA ReadOnlyOfParent READONLY

   PROTECTED:
      DATA ProtectedOfParent
      DATA ProtectedReadOnlyOfParent READONLY

   PRIVATE:
      DATA PrivateOfParent

ENDCLASS

CLASS TChild FROM TParent

   DATA PublicOfChild

   METHOD Create() CONSTRUCTOR

ENDCLASS

METHOD Create() CLASS TChild

   LOCAL oErr

   ::ProtectedOfParent := "Can assign PROTECTED of Parent in a DERIVED Class - Can read anywhere."
   ::ReadOnlyOfParent := "Can assign READONLY of Parent in a DERIVED Class if NOT PROTECTED - Can read anywhere."

   TRY
      ? ::PrivateOfParent
      ? "OOPS", '[' + Str( ProcLine(), 2 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 2 ) + ']'
   END

   TRY
      ::ProtectedReadOnlyOfParent := "Can NOT assign PROTECTED READONLY of Parent in Derived Class!"
      ? "OOPS", '[' + Str( ProcLine(), 2 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 2 ) + ']'
   END

RETURN Self
