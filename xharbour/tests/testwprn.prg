#include 'set.ch'
function main
local x
local aPrinter
  CLS
  ?? 'Test program for WINDOWS printing'
  ?  '---------------------------------'
  ? SET(_SET_DEVICE)
  aPrinter:=GetPrinters()
  IF EMPTY(aPrinter)
   ? '----- No Printers installed'
  ELSE
    set printer to (GetDefaultPrinter())
    ? SET(_SET_PRINTER)
    ? SET(_SET_PRINTFILE)
    set console off
    set printer on
    ? 'Default Printer'
    ?'----------------'
    ? GetDefaultPrinter()
    ?
    ? 'Printers Available'
    ?'-------------------'
    FOR x:= 1 TO LEN(aPrinter)
      ? aPrinter[x]
    NEXT x
    aPrinter:= GetPrinters(.T.)
    ? 'Printers and Ports'
    ?'-------------------'
    FOR x:= 1 TO LEN(aPrinter)
      ? aPrinter[x,1]+' on '+aPrinter[x,2]
    NEXT x
    EJECT
    set printer off
    set console on
    set printer to
  ENDIF
  ? SET(_SET_PRINTER)
  ? SET(_SET_DEVICE)
  WAIT
return nil
