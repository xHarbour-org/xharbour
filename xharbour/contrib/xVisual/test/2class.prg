/*
 * $Id: 2class.prg,v 1.1 2002/10/16 22:49:30 fsgiudice Exp $
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
ENDCLASS

METHOD Modify()
   ::data_published := 51
   ::data_protected := 52
   ::data_hidden    := 53
   ::data_exported  := 54
   ::data_RO        := 55
RETURN Self

