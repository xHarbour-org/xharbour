/*
 * $Id: test.prg,v 1.2 2002/10/16 22:53:44 fsgiudice Exp $
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

#define CRLF CHR(13)+CHR(10)

PROCEDURE Main()

  LOCAL FC := FirstClass():New()
  LOCAL SC := SecondClass():New()

  clear screen

  ? "Original Data from FirstClass"
  FC:Print()

  ? CRLF

  ? "Modified Data internally from FirstClass"
  FC:Modify()
  FC:Print()

  ? CRLF
wait
  ? "Modified FirstClass Data externally"

  FC:data_published := "AA"
  FC:data_protected := "BB"
  FC:data_hidden    := "CC"
  FC:data_exported  := "DD"
  FC:data_RO        := "EE"
  FC:Print()
  ? CRLF

  ? "Original Data from SecondClass"
  SC:Print()
  ? CRLF

wait
  ? "Modified Data internally from SecondClass"
  SC:Modify()
  SC:Print()

  ? CRLF

  ? "Modified SecondClass Data externally"

  SC:data_published := "AA"
  SC:data_protected := "BB"
  SC:data_hidden    := "CC"
  SC:data_exported  := "DD"
  SC:data_RO        := "EE"

  SC:Print()
  ? CRLF

RETURN 