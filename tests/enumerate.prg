#include "hbclass.ch"

PROCEDURE Main()

   LOCAL cStr := "*H*e*l*l*o", c

   ASSOCIATE CLASS StringEnum WITH TYPE CHARACTER

   FOR EACH c IN cStr
      ? c
   NEXT

RETURN

CLASS StringEnum
   METHOD Enumerate( nEnumOp, nIndex ) OPERATOR "FOR EACH"
ENDCLASS

METHOD Enumerate( nEnumOp, nIndex ) CLASS StringEnum

   SWITCH nEnumOp
      CASE FOREACH_BEGIN
         EXIT

      CASE FOREACH_ENUMERATE
         nIndex *= 2

         IF nIndex > Len( Self )
            BREAK
         ENDIF

         RETURN Self[nIndex]

      CASE FOREACH_END
         EXIT
   END

RETURN Self

