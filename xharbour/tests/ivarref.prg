#include "hbclass.ch"

PROCEDURE Main()

   LOCAL o := TMyClass()

   Test( @( o:Var ) )
   Test( @( o:ClassVar ) )
   Test( @( o:SharedClassVar ) )

   ? o:Var
   ? o:ClassVar
   ? o:SharedClassVar
   ?

   o:Var := ProcLine()
   o:ClassVar := ProcLine()
   o:SharedClassVar := ProcLine()

   ? o:Var
   ? o:ClassVar
   ? o:SharedClassVar

RETURN

PROCEDURE Test( xByRef )
  static nId := '1'

  xByRef := "Changed in " + ProcName() + '[' + nId++ + ']'

RETURN

CLASS TMyClass
   CLASS VAR ClassVar
   CLASS VAR SharedClassVar SHARED
   VAR       Var
ENDCLASS
