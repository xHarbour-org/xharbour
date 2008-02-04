/*
  NOTE: You must compile extnamespace.prg FIRST, then namespace.prg finally
        compile tstnamespace.prg and link it as main module with both
        namespace and extnamespace!
 */

USING NAMESPACE MyNamespace

PROCEDURE Main()

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

   MyOptional.SomeOptional()
   // Because this namespace is OPTIONAL we can also call it withOUT qualifier!
   SomeOptional()

RETURN
