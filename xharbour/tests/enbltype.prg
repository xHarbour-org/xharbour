#include "hbclass.ch"

PROCEDURE Main()

   LOCAL aVar := { "One", 2, "Three" }

   CLS

   ENABLE TYPE CLASS NUMERIC, ARRAY

   ? aVar:AsString
   ? aVar[2]:AsString

RETURN

