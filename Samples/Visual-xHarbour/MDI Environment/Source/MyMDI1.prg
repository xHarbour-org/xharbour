GLOBAL EXTERNAL AppCaption

GLOBAL EXTERNAL MDI1

#include "vxh.ch"
#include "MyMDI1.xfm"

#translate xCRLF => CHR(13) + CHR(10)
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD MyMDI1_OnShowWindow( Sender ) CLASS MyMDI1
   Sender:Caption := "xHarbour.com Training Center -1- | " + AppCaption
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyMDI1_OnClose( Sender ) CLASS MyMDI1
   MDI1 := NIL
RETURN Self