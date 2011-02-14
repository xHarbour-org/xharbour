/* $Id$ */

/*
   This file is to be used as a secondary linked module along with tstscope.prg and scope2.prg.
 */

#include "hbclass.ch"
#include "classex.ch"

#ifndef __XHARBOUR__
#xcommand TRY  => BEGIN SEQUENCE WITH {|oErr| Break( oErr )}
#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#xcommand PRIVATE: => HIDDEN:
#xcommand PUBLIC: => EXPORTED:
#else
#xtranslate MODULE FRIENDLY =>
#endif

CLASS TParent MODULE FRIENDLY

   PROPERTY Property READ FProperty WRITE FProperty

   DATA ReadOnlyOfParent READONLY

   PROTECTED:
      DATA ProtectedOfParent
      DATA ProtectedReadOnlyOfParent READONLY

   PRIVATE:
      DATA PrivateOfParent INIT "PrivateOfParent"
   PUBLIC:
   METHOD ChangeReadOnly(x) INLINE ::ReadOnlyOfParent := x

   METHOD ExposePrivateOfParent INLINE Self:PrivateOfParent

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
