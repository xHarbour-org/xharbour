/*
   This file is to be used with secondary linked modules scope1.prg and scope2.prg.
 */

#include "hbclass.ch"
#include "classex.ch"

PROCEDURE Main()

   LOCAL oErr, oStranger := TStranger()
   LOCAL oParent := TParent(), oChild := TChild():Create(), oCloseChild := TCloseChild():Create()

   TRY
      // Should be Ok.
      oParent:ChangeReadOnly( 7 )
   CATCH oErr
      ? "OOPS No Violation!", oErr:Description, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      // Should be Ok.
      ? "Ok from Expose Method:", Eval( oParent:ExposePrivateOfParent )
   CATCH oErr
      ? "OOPS No Violation!", oErr:Description, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      // Should be Ok.
      oChild:PublicOfChild := "Public is Ok."
   CATCH oErr
      ? "OOPS No Violation!", oErr:Description, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      ? oChild:PrivateOfParent
      ? "OOPS PrivateOfParent", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      ? oChild:ProtectedOfParent
      ? "OOPS", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      oChild:ProtectedReadOnlyOfParent := "Can NOT assign a READONLY or PROTECTED outside of [DERIVED] Class!"
      ? "OOPS", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      TestByRef( @oChild:ReadOnlyOfParent )
      ? "OOPS", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      ? oCloseChild:PrivateOfParent
      ? "OOPS", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      ? oCloseChild:ProtectedOfParent
      ? "OOPS", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      oCloseChild:ProtectedReadOnlyOfParent := "Can NOT assign a READONLY or PROTECTED outside of [DERIVED] Class!"
      ? "OOPS", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      TestByRef( @oCloseChild:ReadOnlyOfParent )
      ? "OOPS", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   oStranger:TestScope( oChild )

   TRY
      // Should be Ok.
      ? "OK to read:", oChild:ReadOnlyOfParent
   CATCH oErr
      ? "OOPS No Violation!", oErr:Description, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   oChild:Property := "Can assign into a property."

   TRY
      ? oChild:FProperty
      ? "OOPS", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

RETURN

PROCEDURE TestByRef( xByRef )

   xByRef := "New Value"

RETURN

CLASS TStranger

   METHOD TestScope( oObject )

ENDCLASS

METHOD TestScope( oObject ) CLASS TStranger

   LOCAL oErr

   TRY
      ? oObject:ProtectedOfChild
      ? "OOPS", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

RETURN NIL