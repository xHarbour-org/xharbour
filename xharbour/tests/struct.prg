
#include "hbstruct.ch"

procedure main()

   local oStru := nil

   STRUCTURE oStru

      MEMBER c1, c2, c3, c4 AS STRING

      MEMBER c5             AS STRING INIT "XHarbour"

      MEMBER n1, n2, n3, n4 AS NUMERIC

      MEMBER b1             AS CODEBLOCK

      MEMBER a1, a2, a3     AS ARRAY

      MEMBER d1             AS DATE INIT Date()

   ENDSTRUCTURE

   ? oStru:c1
   ? oStru:c2
   ? oStru:c3
   ? oStru:c4
   ? oStru:c5
   ?
   ? oStru:d1

   Eval( oStru:b1 )

return


