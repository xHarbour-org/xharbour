/*
   This file is to be used as a secondary linked module along with tstscope.prg and scope2.prg.
 */

#include "hbclass.ch"
#include "classex.ch"

CLASS TParent

   PROPERTY Property READ FProperty WRITE FProperty

   DATA ReadOnlyOfParent READONLY

   PROTECTED:
      DATA ProtectedOfParent
      DATA ProtectedReadOnlyOfParent READONLY

   PRIVATE:
      DATA PrivateOfParent INIT "PrivateOfParent"

   PUBLIC:
   METHOD ChangeReadOnly(x) INLINE ::ReadOnlyOfParent := x

   DATA ExposePrivateOfParent INIT {|| HB_QSelf():PrivateOfParent }

ENDCLASS

CLASS TCloseChild FROM TParent

   DATA ProtectedOfChild PROTECTED

   DATA PublicOfChild

   METHOD Create() CONSTRUCTOR

ENDCLASS

METHOD Create() CLASS TCloseChild

   LOCAL oErr

   ::ProtectedOfParent := "Can assign PROTECTED of Parent in a DERIVED Class - Can read anywhere."
   ::ReadOnlyOfParent := "Can assign READONLY of Parent in a DERIVED Class if NOT PROTECTED - Can read anywhere."
   ::FProperty := "Can assign property from derived class, otherwise hidden."

   TRY
      ? "OK in same Module:", ::PrivateOfParent
   CATCH oErr
      ? "OOPS Same Module!", oErr:Description, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      ::ProtectedReadOnlyOfParent := "Can NOT assign PROTECTED READONLY of Parent in Derived Class!"
   CATCH oErr
      ? "OOPS Same Module!", oErr:Description, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

RETURN Self