// NOTE: You must compile extnamespace.prg FIRST, then link with both namespace and extnamespace!

USING NAMESPACE MyNamespace
USING NAMESPACE MyNamespace2

PROCEDURE Main()

   // Because we have USING NAMESPACE MyNamespace we don't need explicit qualifier!
   Main()
   ? "Agin..."
   // Or explicit.
   MyNamespace.Main()
   ?

   // Because we have USING NAMESPACE MyNamespace2 we don't need explicit qualifier!
   FunOf2()

   MyOptional.SomeOptional()
   // Because this namespace is OPTIONAL we can also call it withOUT qualifier!
   SomeOptional()

RETURN
