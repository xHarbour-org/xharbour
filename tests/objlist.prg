#include "hbclass.ch"

Procedure Main()

   LOCAL o := TTest()
   LOCAL aVars := __objGetValueList( o, NIL, HB_OO_CLSTP_PROTECTED + HB_OO_CLSTP_HIDDEN )
   LOCAL aVar

   FOR EACH aVar IN aVars
      ? aVar[1], aVar[2]
   NEXT

Return

CLASS TTest
   Protected:
      VAR Prot1
      VAR Prot2

   Hidden:
      VAR Hid1

   Exported:
      VAR Var1
      VAR Var2
ENDCLASS
