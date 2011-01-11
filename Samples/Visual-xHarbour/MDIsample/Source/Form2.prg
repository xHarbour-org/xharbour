#include "vxh.ch"
#include "Form2.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form2_OnLoad( Sender ) CLASS Form2
   ::Label1:Caption := ::Params[1]
   ::Label2:Caption := ::Params[2]
   ::Maximize()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS Form2
   ::Close()
RETURN Self