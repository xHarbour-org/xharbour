#include "hbclass.ch"

PROCEDURE Main()

   LOCAL cVar := "Hello", GetList := {}

   CLS

   EXTEND CLASS GET WITH MESSAGE AsString METHOD GetAsString

   EXTEND CLASS NUMERIC WITH METHOD Plus

   ? 3:AsString + " test"

   ? 7:Plus( "33" )

   @ 10,10 GET cVar
   ? GetList[1]:AsString()

RETURN

STATIC FUNCTION Plus( xArgument )

   LOCAl Self := HB_QSelf()

RETURN Self + Val( xArgument )

STATIC FUNCTION GetAsString

   LOCAl Self := HB_QSelf()

RETURN ValToPrgExp( Self )
