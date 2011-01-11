#include "vxh.ch"
#include "FormEmployees.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD ButtonAppend_OnClick( Sender ) CLASS FormEmployees
   with object ::DataTable1
      :GoBottom()
      :Append()
   end
   ::DataGrid1:Update()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonUpdate_OnClick( Sender ) CLASS FormEmployees
   FormPerson( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDelete_OnClick( Sender ) CLASS FormEmployees
   with object ::DataTable1
      if :RecCount() < 1
         return Self
      endif
      :RecLock()
      :Delete()
      :UnLock()
   end
   ::DataGrid1:Update()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormEmployees_OnLoad( Sender ) CLASS FormEmployees
   ::Application:MainForm:oEmployees:=Sender   
RETURN Self