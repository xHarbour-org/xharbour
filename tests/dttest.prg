/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Carlos Bacco
 * www - http://harbour-project.org
 *
 */

#include "simpleio.ch"

PROCEDURE Main()

   ? DATETIME( 1974 )

   ? " VALTYPE( DATETIME() ) =>", VALTYPE( DATETIME() )
   ? " YEAR(    DATETIME() ) =>", YEAR(    DATETIME() )
   ? " MONTH(   DATETIME() ) =>", MONTH(   DATETIME() )
   ? " DAY(     DATETIME() ) =>", DAY(     DATETIME() )
   ?

   ? " VALTYPE( DATETIME( 1974, 5, 31 ) ) =>", VALTYPE( DATETIME( 1974, 5, 31 ) )
   ? " YEAR(    DATETIME( 1974, 5, 31 ) ) =>", YEAR(    DATETIME( 1974, 5, 31 ) )
   ? " MONTH(   DATETIME( 1974, 5, 31 ) ) =>", MONTH(   DATETIME( 1974, 5, 31 ) )
   ? " DAY(     DATETIME( 1974, 5, 31 ) ) =>", DAY(     DATETIME( 1974, 5, 31 ) )
   ? " DTOC(    DATETIME( 1974, 5, 31 ) ) =>", DTOC(    DATETIME( 1974, 5, 31 ) )
   ?

   ? " VALTYPE( DATETIME( 1974, 31, 5, NIL, NIL, NIL ) ) =>", VALTYPE( DATETIME( 1974, 31, 5, NIL, NIL, NIL ) )
   ?

   ? " VALTYPE( DATETIME( 2001, 10, 13, 18, 42, 16  ) ) =>", VALTYPE( DATETIME( 2001, 10, 13, 18, 42, 16  ) )
   ?

   ? " VALTYPE( DATETIME( NIL, NIL, NIL, 10, 36, 05  ) )     =>", VALTYPE( DATETIME( NIL, NIL, NIL, 10, 36, 05  ) )
   ? "          DATETIME( NIL, NIL, NIL, 10, 36, 05  )       =>",          DATETIME( NIL, NIL, NIL, 10, 36, 05  )
   ?

   ? " VALTYPE( DATETIME( NIL, NIL, NIL, 10, 36, 05, 176 ) ) =>", VALTYPE( DATETIME( NIL, NIL, NIL, 10, 36, 05, 176  ) )
   ? "          DATETIME( NIL, NIL, NIL, 10, 36, 05, 176 )   =>",          DATETIME( NIL, NIL, NIL, 10, 36, 05, 176  )

   ? " VALTYPE( DATETIME( 0, 0, 0, 10, 36, 05, 176 ) ) =>", VALTYPE( DATETIME( 0, 0, 0, 10, 36, 05, 176  ) )
   ? "          DATETIME( 0, 0, 0, 10, 36, 05, 176 )   =>",          DATETIME( 0, 0, 0, 10, 36, 05, 176  )

   RETURN
