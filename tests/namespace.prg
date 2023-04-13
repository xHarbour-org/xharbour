/*
   $Id$
*/

#ifdef __HRB__
   // Callback into PRG host
   DYNAMIC DynNamespace.SomeDyn
#endif

NAMESPACE MyNameSpace

   PROCEDURE Main()

     ? ProcName()

     // No qualifier needed for SAME level memebers.
     ? Str(1)
     // Explicit qualifier to the Global namespace
     ? Global.Str(1)
     // Or for code section
     WITH NAMESPACE Global
        ? Str(2)
        ? Str(3)
     END

     // No qualifier needed for SAME level memebers.
     StaticProc()
     // Or explicit.
     MyNamespace.StaticProc() // Can only be called in same compilation unit!!!

     // Sub level entities must be called explicitly!
     MyNamespace.Sub.SomeSub()

     // Call to member defined EXTERNALly by means of EXTERNAL NAMESPACE below!
     // WARNING 1: You can NOT use USING for Namespaces defined in SAME source because you'll create a cyclic dependancy!!!
     // WARNING 2: You MUST compile the EXTERNAL module FIRST!!!
     #ifdef __HRB__
        MyNameSpace.SubExtern.Ext1()

        // STATIC members of EXTERNAL namespaces can not be accessed because they belong to a different compilation unit!
        #ifdef SHOW_COMPILE_ERR
           MyNameSpace.SubExtern.SubSubExtern.SubExtStatic()
        #endif
     #endif

     MyOptional.SomeOptional()

     #ifdef __HRB__
        // Defined as C function in extnamespace.prg
        ? ExtMember2()
     #endif

   RETURN

   FUNCTION Str(x)
   RETURN "!" + AllTrim( Global.Str(x) ) + "!"

   PROCEDURE SomeProc()
      ? ProcName()
   RETURN

   // STATIC members can not be called from OUTSIDE this compilation unit!
   STATIC PROCEDURE StaticProc()
      ? ProcName()
   RETURN

   NAMESPACE Sub

      PROCEDURE SomeSub()
         ? ProcName()

          // No qualifier needed for OUTER level memebers.
          SomeProc()
          // Or explicit.
          MyNamespace.SomeProc()

      RETURN

   END

   #ifdef __HRB__
      // Complete sub level[s] definition at external compilation unit!
      //NOTE: More than 1 namespace can use the *same* EXTERNAL NAMESPACE unit!
      // WARNING: You MUST compile the extern module FIRST!!!
      EXTERNAL NAMESPACE SubExtern
   #endif


   #ifdef __HRB__
      EXTERNAL NAMESPACE MEMBER ExtMember
      EXTERNAL NAMESPACE MEMBER ExtMember2
   #endif

END

RUNTIME NAMESPACE MyNameSpace2

   PROCEDURE ProcOf2()
      ? ProcName()
   RETURN

   FUNCTION FunOf2()
   RETURN ProcName()


   FUNCTION Str(x)
   RETURN "**" + AllTrim( Global.Str(x) ) + "**"

   STATIC PROCEDURE StatOf2()
      ? ProcName()
   RETURN
END

// Members of OPTIONAL NAMESPACE can be called WITH or withOUT the namespace qualifier!
OPTIONAL NAMESPACE MyOptional
  PROCEDURE SomeOptional()
     ? ProcName()
  RETURN
END

#ifdef __HRB__
   RUNTIME NAMESPACE HrbNamespace

      PROCEDURE HrbProc()
         ? ProcName()

         // Callback into PRG host
         DynNamespace.SomeDyn()
      RETURN

   END
#endif
