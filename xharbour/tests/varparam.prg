PROCEDURE Main( ... )

   LOCAL x

   FOR EACH x IN HB_aParams()
       ? x
   NEXT

   ? Test( 9, 8, 7, 6, 5, 4, 3, 2, 1 )

RETURN

FUNCTION Test( ... )

   LOCAL x, aParams := HB_aParams()

   FOR EACH x IN aParams
       ? x
   NEXT

RETURN aParams[ -1 ]
