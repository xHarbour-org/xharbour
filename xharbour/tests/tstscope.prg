#include "hbclass.ch"
#include "classex.ch"

static oChild

PROCEDURE Main()

   LOCAL oErr, oStranger := TStranger()
   LOCAL oParent := TParent()

   oChild := TChild():Create()
   oParent:ChangeReadOnly( 7 )

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

   TRY
      TestByRef( @( oChild:ReadOnlyOfParent ) )
      ? "OOPS", '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 3 ) + ']'
   END

   oStranger:TestScope()

   ?
   ? oChild:ReadOnlyOfParent

   oChild:Property := "Can assign into a property."

   TRY
      ? oChild:FProperty
      ? "OOPS", '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, '[' + Str( ProcLine(), 3 ) + ']'
   END

RETURN

CLASS TParent

   PROPERTY Property READ FProperty WRITE FProperty

   DATA ReadOnlyOfParent READONLY

   PROTECTED:
      DATA ProtectedOfParent
      DATA ProtectedReadOnlyOfParent READONLY

   PRIVATE:
      DATA PrivateOfParent

   PUBLIC:
   METHOD ChangeReadOnly(x) INLINE ::ReadOnlyOfParent := x

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
   ::FProperty := "Can assign property from derived class, otherwise hidden."

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

CLASS TStranger

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

PROCEDURE TestByRef( xByRef )

   ? "OOPS", '[' + Str( ProcLine(), 3 ) + ']'
   xByRef := "New Value"

RETURN
