#include "hbclass.ch"

PROCEDURE Main()

   EXTEND NUMERIC WITH METHOD MyNumMethod

   EXTEND NUMERIC WITH MESSAGE Plus( xArgument ) INLINE Self + Val( xArgument )

   3:MyNumMethod()

   ? 7:Plus( "33" )

   WAIT

RETURN

STATIC FUNCTION MyNumMethod

   LOCAl Self := HB_QSelf()

RETURN Alert( Str( Self ) )
