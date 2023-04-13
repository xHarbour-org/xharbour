// #include "set.ch" - Preset in pptable.c

PROCEDURE MAIN()

   LOCAL nID

   CLS

   ? "DEFAULT IDLEREPEAT =", SET( _SET_IDLEREPEAT )
   ?
   ? "Idle Block should be displayed multiple times until key or 2 seconds elapsed!"
   ? "Press any key to begin..."
   ?
   Inkey(0)

   HB_IDLEADD( {|| QOut( "Idle Block" ) } )
   Inkey( 2 )

   SET( _SET_IDLEREPEAT, .F. )

   nID := HB_IDLEADD( {|| QOut( "Idle Block2" ) } )

   CLS
   ? "Idle Block & Block-2 should display ONCE! while waitning for key or 2 seconds elapsed!"
   ?
   Inkey( 2 )

   HB_IDLEDEL( nID)

   ?
   ? "Idle Block but NO Block-2 should display ONCE! while waitning for key or 2 seconds elapsed!"
   ?
   Inkey( 2 )
   ?

RETURN
