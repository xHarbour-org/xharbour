#include "hbclass.ch"

PROCEDURE Main()

   EXTEND NUMERIC WITH METHOD MyNumMethod

   EXTEND NUMERIC WITH MESSAGE PLUS METHOD MyNumPlus

   3:MyNumMethod()

   ? 7:Plus( "33" )

RETURN

STATIC FUNCTION MyNumMethod

   LOCAl Self := HB_QSelf()

RETURN Alert( Str( Self ) )

STATIC FUNCTION MyNumPlus( xArgument )

   LOCAl Self := HB_QSelf()

RETURN Self + Val( xArgument )
