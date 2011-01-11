#include "vxh.ch"
#include "FormXmlData.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD FormXmlData_OnLoad( Sender ) CLASS FormXmlData
   local i
   with object ::Application:MainForm
      for i:=1 to :SampleCols
         ::DataGrid1:Children[i]:Caption := :SampleCaps[i]
      next
   end
RETURN Self