#define HB_CDP_SUPPORT_OFF
#include "hbextern.ch"

DYNAMIC Main

// Dynamic Namespace support
DYNAMIC HrbNamespace.HrbProc

STATIC ahDlls := {}

PROCEDURE TestDyn()
   LOCAL nRow := Row(), nCol := Col()

   Main( "test.dbf" )
   SetPos( nRow, nCol )

   // From Namespace.hrb
   HrbNameSpace.HrbProc()

RETURN

INIT PROCEDURE LoadDlls()
   __Run( "harbour db_brows -gh -n -w -i../include" )
   aAdd( ahDlls, __hrbLoad( "db_brows.hrb" ) )

   __Run( "harbour namespace -gh -n -w -i../include" )
   aAdd( ahDlls, __hrbLoad( "namespace.hrb" ) )
RETURN

EXIT PROCEDURE UnloadDlls()
   LOCAL hDll

   FOR EACH hDll IN ahDlls
      __hrbUnload( hDll )
   NEXT
RETURN

// Will be called from HrbNamespace.HrbProc()
RUNTIME NAMESPACE DynNamespace

   PROCEDURE SomeDyn()
      ? ProcName()
   RETURN

END
