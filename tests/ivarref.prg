#include "hbclass.ch"

PROCEDURE Main()

   LOCAL o := TMyClass(), e, NonObject, aVar, xByRef

   Test( @o:Var )
   Test( @o:ClassVar )
   Test( @o:SharedClassVar )

   TRY
      Test( @o:NonInstance )
   CATCH e
      ? "Caught:", e:Description
   END

   TRY
      Test( @NonObject:SharedClassVar )
   CATCH e
      ? "Caught:", e:Description
   END

   ? o:Var
   ? o:ClassVar
   ? o:SharedClassVar
   ?

   aVar := { @o:Var, @o:ClassVar, @o:SharedClassVar }

   FOR EACH xByRef IN aVar
      xByRef := "Changed in " + ProcName() + '[' + Str( HB_EnumIndex(), 1 ) + ']'
   NEXT

   ? o:Var
   ? o:ClassVar
   ? o:SharedClassVar
   ?

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
