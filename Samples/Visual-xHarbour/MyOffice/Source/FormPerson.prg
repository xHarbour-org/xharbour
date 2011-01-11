#include "vxh.ch"
#include "FormPerson.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormPerson_OnLoad( Sender ) CLASS FormPerson
   local oTable:=::Application:MainForm:oEmployees:DataTable1
   with object oTable:Fields
      ::MaskEdit1:Caption:=padr( :NAME, 20 )
      ::MaskEdit2:Caption:=:BIRTHDAY
      ::MaskEdit3:Caption:=:SALARY
   end   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonSave_OnClick( Sender ) CLASS FormPerson
   with object ::Application:MainForm:oEmployees
      with object :DataTable1
         :RecLock()
         with object :Fields
            :NAME:=alltrim( ::MaskEdit1:Caption )
            :BIRTHDAY:=::MaskEdit2:Caption
            :SALARY:=::MaskEdit3:Caption
         end
         :UnLock()
      end
      :DataGrid1:Update()
   end
   
   ::Close()   
RETURN Self