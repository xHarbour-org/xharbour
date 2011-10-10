#include "vxh.ch"
#include "FormProggy.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS FormProggy
   ::Application:MainForm:lCanceled := .T.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormProggy_OnCreate( Sender ) CLASS FormProggy
   with object ::Application:MainForm
      :MyPanel:Enabled := .F.
      :oProggy := Sender   
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormProggy_OnDestroy( Sender ) CLASS FormProggy
   with object ::Application:MainForm
      :oProggy := NIL   
      :MyPanel:Enabled := .T.
   end
RETURN Self