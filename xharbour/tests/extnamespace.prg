// Here we implement a namespace branch for which ever NAMESPACE will want to use it by means of the EXTRANAL NAMESPACE request.
IMPLEMENTS NAMESPACE SubExtern

   PROCEDURE Ext1()
      ? ProcName()

     // No qualifier needed for SAME level memebers.
      Ext2()

      // Calls to sub level must be explicit!
      MyNameSpace.SubExtern.SubSubExtern.SubExt()

   RETURN

   PROCEDURE Ext2()
      ? ProcName()
   RETURN

   NAMESPACE SubSubExtern

      PROCEDURE SubExt()
         ? ProcName()

         // STATICs members can only be accesed in same compilation unit!
         SubExtStatic()
      RETURN

      STATIC PROCEDURE SubExtStatic()
         ? ProcName()
      RETURN

   END

END

PROCEDURE ExtMember() IMPLEMENTS NAMESPACE MyNamespace
   ? ProcName()
RETURN

#pragma BEGINDUMP
HB_FUNC_EXTERNAL_NAMESPACE( NSID_MYNAMESPACE, EXTMEMBER2 )
{
   hb_retc( "Namespace Function in C" );
}
#pragma ENDDUMP
