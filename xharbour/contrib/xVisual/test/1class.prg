/*
 * $Id: 1class.prg,v 1.1 2002/10/16 22:49:30 fsgiudice Exp $
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

CLASS FirstClass
   DATA data_published PUBLISHED
   DATA data_protected PROTECTED
   DATA data_hidden    HIDDEN
   DATA data_exported
   DATA data_RO

   METHOD New()    CONSTRUCTOR
   METHOD Modify()
   METHOD Print()
ENDCLASS

METHOD New()
   ::data_published := "1A"
   ::data_protected := "1B"
   ::data_hidden    := "1C"
   ::data_exported  := "1D"
   ::data_RO        := "1E"
RETURN Self

METHOD Modify()
   ::data_published := 1
   ::data_protected := 2
   ::data_hidden    := 3
   ::data_exported  := 4
   ::data_RO        := 5
RETURN Self

METHOD Print()
   ? "data_published = ", ::data_published ; view ::data_published
   ? "data_protected = ", ::data_protected ; view ::data_protected
   ? "data_hidden    = ", ::data_hidden    ; view ::data_hidden
   ? "data_exported  = ", ::data_exported  ; view ::data_exported
   ? "data_RO        = ", ::data_RO        ; VIEW ::data_RO
RETURN Self

