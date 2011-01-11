#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   ::DataTable1:GoTop()
   ::DataGrid1:Update()
   ::DataTable2:OrdSetFocus( "K_ID" )
   ::MyRefresh()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyRefresh( Sender ) CLASS Form1
   local cVal:=::DataTable1:Fields:ORDERID
   with object ::DataTable2
      :SetTopScope( cVal )
      :SetBottomScope( cVal )
      :GoTop()
   end
   ::DataGrid2:Update()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD DataGrid1_OnClick( Sender ) CLASS Form1
   ::MyRefresh()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS Form1
   ::DataTable2:KillScope()
   ::DataGrid2:Update()
RETURN Self