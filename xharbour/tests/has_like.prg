PROCEDURE Main()

   LOCAL Identifier := HB_RegexComp( "[_a-zA-Z][_a-zA-Z0-9]*" )
   LOCAL acExp := { "Amount + Tax", "Total" }

   CLS

   IF acExp[1] HAS Identifier
      ? "Found an Identifier in:", acExp[1]
   ENDIF

   IF acExp[2] LIKE Identifier
      ? acExp[2], "is a valid Identifier."
   ENDIF

RETURN

