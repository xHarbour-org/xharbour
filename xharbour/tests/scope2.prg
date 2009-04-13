/* $Id$ */

/*
   This file is to be used as a secondary linked module along with tstscope.prg and scope1.prg.
 */

#include "hbclass.ch"
#include "classex.ch"

#ifndef __XHARBOUR__
#xcommand TRY  => BEGIN SEQUENCE WITH {|oErr| Break( oErr )}
#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

CLASS TChild FROM TParent

   DATA ProtectedOfChild PROTECTED

   DATA PublicOfChild

   METHOD Create() CONSTRUCTOR

ENDCLASS

METHOD Create() CLASS TChild

   LOCAL oErr, o2

   ::ProtectedOfParent := "Can assign inherited PROTECTED of Parent in a DERIVED Class - Can read anywhere."
   ::ReadOnlyOfParent := "Can assign inherited READONLY of Parent in a DERIVED Class if NOT PROTECTED - Can read anywhere."
   ::FProperty := "Can assign inherited property from derived class, otherwise hidden."
   
   o2 := TParent()
   TRY
      o2:ProtectedOfParent := "Can assign PROTECTED of Parent in a DERIVED Class - Can read anywhere."
   CATCH oErr
      ? "OOPS No Violation!", oErr:Description, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      o2:ReadOnlyOfParent := "Can assign READONLY of Parent in a DERIVED Class if NOT PROTECTED - Can read anywhere."
   CATCH oErr
      ? "OOPS No Violation!", oErr:Description, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END

   TRY
      o2:FProperty := "Can assign property from derived class, otherwise hidden."
   CATCH oErr
      ? "OOPS No Violation!", oErr:Description, ProcName() + '[' + Str( ProcLine(), 3 ) + ']'
   END
   
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

