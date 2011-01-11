GLOBAL EXTERNAL AppCaption

GLOBAL EXTERNAL MDI2

#include "vxh.ch"
#include "MyMDI2.xfm"

#translate xCRLF => CHR(13) + CHR(10)
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD MyMDI2_OnShowWindow( Sender ) CLASS MyMDI2
   Sender:Caption := "xHarbour.com Training Center -2- | " + AppCaption
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyMDI2_OnClose( Sender ) CLASS MyMDI2
   MDI2 := NIL
RETURN Self