#include "vxh.ch"
#include "Form2.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form2_OnLoad( Sender ) CLASS Form2
   ::WebBrowser1:Url := ::Application:MainForm:cFileOut
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form2_OnCreate( Sender ) CLASS Form2
   ::Application:MainForm:oTest := Sender
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form2_OnDestroy( Sender ) CLASS Form2
   ::Application:MainForm:oTest := NIL   
RETURN Self