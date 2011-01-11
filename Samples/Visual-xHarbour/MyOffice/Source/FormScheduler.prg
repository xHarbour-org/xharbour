#include "vxh.ch"
#include "FormScheduler.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormScheduler_OnLoad( Sender ) CLASS FormScheduler
   ::ListBox1:AddString( "" )
   with object ::DataTable2
      :GoTop()
      do while !:Eof()
         ::ListBox1:AddString( :Fields:NAME )
         :Skip()
      enddo
   end
   ::ListBox1:SetCurSel( 1 )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ListBox1_OnLButtonUp( Sender ) CLASS FormScheduler
   with object ::DataTable1
      :RecLock()
      :Fields:WORKER:=Sender:GetString( Sender:GetCurSel() )
      :UnLock()
   end
   ::DataGrid1:Update()   
RETURN Self