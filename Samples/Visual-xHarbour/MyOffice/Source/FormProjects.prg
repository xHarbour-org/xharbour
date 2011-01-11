GLOBAL EXTERNAL hModel

#include "vxh.ch"
#include "FormProjects.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD PictureBox1_OnLButtonUp( Sender ) CLASS FormProjects
     FormPrices( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormProjects_OnLoad( Sender ) CLASS FormProjects
   local i, nModels:=len( hModel )
   if nModels < 1
      ::MessageBox( "No models available", "", MB_ICONEXCLAMATION )
      ::Close()
      return Self
   endif
   
   ::Edit1:Caption:=""
   ::DateTimePicker1:Date:=Date() + 7
   for i:=1 to nModels
      ::ComboBox1:AddItem( hGetKeyAt( hModel, i ) )
   next
   ::ComboBox1:SetCurSel( 1 )
   
   ::Edit1:SetFocus()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonAdd_OnClick( Sender ) CLASS FormProjects
   local cID:=alltrim( ::Edit1:Caption )
   if empty(cID) .OR. len(cID)>10
      ::MessageBox( "ID empty or too long", "", MB_ICONEXCLAMATION )
      ::Edit1:SetFocus()
      return Self
   endif
   
   ::DataTable1:Append()
   with object ::DataTable1:Fields
      :ID:=cID
      :TIMELINE:=::DateTimePicker1:Date
      :MODEL:=::ComboBox1:GetSelString()
   end
   
   ::Close()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormProjects
   ::Close()   
RETURN Self