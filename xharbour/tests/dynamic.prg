#include "hbextern.ch"

DYNAMIC Main

STATIC ahDlls := {}

PROCEDURE TestDyn()
   LOCAL nRow := Row(), nCol := Col()

   Main( "test.dbf" )

   SetPos( nRow, nCol )
RETURN

INIT PROCEDURE LoadDlls()
   __Run( "harbour.exe db_brows -gh -n -w -i..\include" )
   aAdd( ahDlls, __hrbLoad( "db_brows.hrb" ) )
RETURN

EXIT PROCEDURE UnloadDlls()
   LOCAL hDll

   FOR EACH hDll IN ahDlls
      __hrbUnload( hDll )
   NEXT
RETURN
