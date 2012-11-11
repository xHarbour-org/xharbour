/*
 * $Id$
 */

#include "hbxdiff.ch"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL pMMFOld, pMMFNew
   LOCAL cFileCtx

   pMMFOld := xdl_init_mmfile( XDLT_STD_BLKSIZE )
   pMMFNew := xdl_init_mmfile( XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC )

   cFileCtx := MemoRead( __FILE__ )

   xdl_write_mmfile( pMMFOld, @cFileCtx )
   xdl_write_mmfile( pMMFNew, cFileCtx + hb_osnewline() + Space( 3 ) + "RETURN NIL" + hb_osnewline() )

   xdl_diff( pMMFOld, pMMFNew, 0, 3, { | x, y | Diff( x, y ) } )
   xdl_diff( pMMFOld, pMMFNew, 0, 3, @Diff() )

   RETURN

FUNCTION Diff( ... )

   LOCAL e, params := hb_aParams()

   FOR e := 1 to PCount()
      IF params[ e ] != NIL
         ? params[ e ]
      ENDIF
   NEXT

   RETURN 0

