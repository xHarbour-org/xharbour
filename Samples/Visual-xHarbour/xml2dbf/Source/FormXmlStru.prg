#include "vxh.ch"
#include "FormXmlStru.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD FormXmlStru_OnCreate( Sender ) CLASS FormXmlStru
  
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormXmlStru_OnLoad( Sender ) CLASS FormXmlStru
   ::Application:MainForm:MtXml:GoTop()
   ::GridXml:Update()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridXml_OnClick( Sender ) CLASS FormXmlStru
   with object ::Application:MainForm
      :MtDbf:Fields:TAG := :MtXml:Fields:TAG
      :MtDbf:Fields:ATTR := :MtXml:Fields:ATTR
      :MtDbf:Fields:SELECTION := :MtXml:Fields:SELECTION
      :GridDbf:Update()
   end
   ::Close()
RETURN Self