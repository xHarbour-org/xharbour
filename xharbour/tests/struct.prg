
#include "hbstruct.ch"

procedure main()

   local oStru := nil

   STRUCTURE oStru

      MEMBER c1, c2, c3, c4 AS STRING

      MEMBER c5             AS STRING INIT "XHarbour"

      MEMBER n1, n2, n3, n4 AS NUMERIC

      MEMBER b1             AS CODEBLOCK INIT { || QOut( "Hello!!!" ) }

      MEMBER a1, a2, a3     AS ARRAY

      MEMBER d1             AS DATE INIT Date()

   ENDSTRUCTURE

   ? ValToPrg( oStru:c1 )
   ? ValToPrg( oStru:c2 )
   ? ValToPrg( oStru:c3 )
   ? ValToPrg( oStru:c4 )
   ? ValToPrg( oStru:c5 )
   ? ValToPrg( oStru:b1 )
   ? ValToPrg( oStru:d1 )

   Eval( oStru:b1 )
   ?

return


