/*
 * $Id: 2class.prg,v 1.2 2002/10/16 22:53:44 fsgiudice Exp $
 */

//#include "windows.ch"
//#include "wingdi.ch"
//#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
//#include "what32.ch"
//#Include "toolbar.ch"
//#Include "winlview.ch"
//#include "wintypes.ch"
//#include "cstruct.ch"

CLASS SecondClass FROM FirstClass
   METHOD Modify()
   METHOD Print()
ENDCLASS

METHOD Modify()
   ::data_published := 51
   //::data_protected := 52
   //::data_hidden    := 53
   ::data_exported  := 54
   //::data_RO        := 55
RETURN Self

METHOD Print()
   ? "ClassName      = ", ::ClassName      ; view ::ClassName
   ? "data_published = ", ::data_published ; view ::data_published
   ? "data_protected = ", ::data_protected ; view ::data_protected
   ? "data_hidden    = ", ::data_hidden    ; view ::data_hidden
   ? "data_exported  = ", ::data_exported  ; view ::data_exported
   ? "data_RO        = ", ::data_RO        ; VIEW ::data_RO
RETURN Self

