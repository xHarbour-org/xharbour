#include "vxh.ch"
#include "Form2.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form2_OnDestroy( Sender ) CLASS Form2
   ::Application:MainForm:oProgress:=NIL
RETURN Self