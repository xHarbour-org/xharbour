/*
   This file is to be used as a secondary linked module along with tstscope.prg and scope1.prg.
 */

#include "hbclass.ch"
#include "classex.ch"

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
      ? "OOPS PrivateOfParent", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      ::ProtectedReadOnlyOfParent := "Can NOT assign PROTECTED READONLY of Parent in Derived Class!"
      ? "OOPS ProtectedReadOnlyOfParent", ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   CATCH oErr
      ? "Caught:", oErr:Description, oErr:Operation, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

RETURN Self

