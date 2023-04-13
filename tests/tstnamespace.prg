/*
  NOTE: You must compile extnamespace.prg FIRST, then namespace.prg finally
        compile tstnamespace.prg and link it as main module with both
        namespace and extnamespace!
 */

USING NAMESPACE MyNamespace

DYNAMIC HrbNamespace.HrbProc

PROCEDURE Main()

   LOCAL h

   // Because we have USING NAMESPACE MyNamespace we don't need explicit qualifier!
   Main()
   ? "Agin..."
   // Or explicit.
   MyNamespace.Main()
   ?
   // Or implied for code section
   WITH NAMESPACE MyNamespace2
      ProcOf2()

      WITH NAMESPACE MyNamespace
         ? Str( 10 )
      END

      // Back at MyNamespace2
      ? FunOf2()
      ? Str( 100 )
   END

   // Namespace2 is a RUNTIME NS, so we can call it even in Macros.
   // Macro namespace calls must always be fully qualified as macro has no knowledge of WITH NAMESPACE, etc.
   ? &( "MyNamespace2.Str( 1000 )" )

   MyOptional.SomeOptional()
   // Because this namespace is OPTIONAL we can also call it withOUT qualifier!
   SomeOptional()

   __Run( "harbour namespace -gh -n -w -i../include" )
   h := __hrbLoad( "namespace.hrb" )
   HrbNamespace.HrbProc()
   __hrbUnLoad( h )

RETURN

RUNTIME NAMESPACE DynNamespace

   PROCEDURE SomeDyn()
      ? ProcName()
   RETURN

END
