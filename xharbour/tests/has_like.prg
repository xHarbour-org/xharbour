PROCEDURE Main()

   LOCAL Number := HB_RegexComp( "[0-9].?[0-9]*" )
   LOCAL Identifier := HB_RegexComp( "[_a-zA-Z][_a-zA-Z0-9]*" )
   LOCAL acExp := { "3.14 * R ^ 2", "Total" }, cExp

   CLS

   FOR EACH cExp IN acExp
      ? "Scanning:", "'" + cExp + "'"

      IF cExp HAS Number
         ? "   ", "Found a Number."
      ENDIF

      IF cExp LIKE Identifier
         ? "   ", '<' + cExp + '>', "is a valid Identifier."
      ENDIF

      ?
   NEXT
Inkey( 0 )
RETURN

