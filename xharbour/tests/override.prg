#include "hbclass.ch"

PROCEDURE Main()

   LOCAL cVar := "Hello", GetList := {}

   CLS

   OVERRIDE METHOD Display IN CLASS GET WITH MyGetDisplay
   OVERRIDE METHOD AsString IN CLASS ARRAY WITH MyArrayString

   @ 10,10 GET cVar

   ? GetList:AsString()

RETURN

STATIC FUNCTION MyGetDisplay

   LOCAl Self := HB_QSelf()

   Alert( ::VarGet() )

RETURN Self

STATIC FUNCTION MyArrayString()

   LOCAl Self := HB_QSelf()

RETURN "{ ... }"
