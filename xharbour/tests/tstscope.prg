#include "hbclass.ch"

static oChild

PROCEDURE Main()

   LOCAL oErr, oStranger := TSranger()

   oChild := TChild():Create()

   oChild:PublicOfChild := "Public is Ok."

   TRY
      ? oChild:PrivateOfParent
      ? "OOPS", '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:operation, '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      ? oChild:ProtectedOfParent
      ? "OOPS", '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      oChild:ProtectedReadOnlyOfParent := "Can NOT assign a READONLY or PROTECTED outside of [DERIVED] Class!"
      ? "OOPS", '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 3 ) + ']'
   END

   oStranger:TestScope()

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

   DATA ProtectedOfChild PROTECTED

   DATA PublicOfChild

   METHOD Create() CONSTRUCTOR

ENDCLASS

METHOD Create() CLASS TChild

   LOCAL oErr

   ::ProtectedOfParent := "Can assign PROTECTED of Parent in a DERIVED Class - Can read anywhere."
   ::ReadOnlyOfParent := "Can assign READONLY of Parent in a DERIVED Class if NOT PROTECTED - Can read anywhere."

   TRY
      ? ::PrivateOfParent
      ? "OOPS", '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      ::ProtectedReadOnlyOfParent := "Can NOT assign PROTECTED READONLY of Parent in Derived Class!"
      ? "OOPS", '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 3 ) + ']'
   END

RETURN Self

CLASS TSranger

   METHOD TestScope()

ENDCLASS

METHOD TestScope()

   LOCAL oErr

   TRY
      ? oChild:ProtectedOfChild
      ? "OOPS", '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 3 ) + ']'
   END

RETURN NIL
