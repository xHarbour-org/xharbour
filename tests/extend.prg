#include "hbclass.ch"

PROCEDURE Main()

   LOCAL cVar := "Hello", GetList := {}

   CLS

   EXTEND CLASS GET WITH MESSAGE AsString METHOD GetAsString

   EXTEND CLASS NUMERIC WITH MESSAGE Plus( xArgument ) INLINE Self + Val( xArgument )

   ? 3:AsString + " test"

   ? 7:Plus( "33" )

   @ 10,10 GET cVar
   ? GetList[1]:AsString()

RETURN

STATIC FUNCTION GetAsString

   LOCAl Self := HB_QSelf()

RETURN ValToPrgExp( Self )
