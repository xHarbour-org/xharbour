GLOBAL EXTERNAL oIni, hModel

#include "vxh.ch"
#include "FormPrices.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormPrices_OnLoad( Sender ) CLASS FormPrices
   local i, n:=len( hModel ), a
   if n < 1
      return Self
   endif
   ::MemoryTable1:Table:=array(0)
   
   for i:=1 to n
      a:=hGetPairAt( hModel, i )
      ::MemoryTable1:Append()
      with object ::MemoryTable1:Fields
         :A:=padr( a[1], 10 )
         :B:=a[2]
         :C:=a[2]*1.25
      end
   next
   ::MemoryTable1:GoTop()
   ::DataGrid1:Update()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn1_OnSave( Sender ) CLASS FormPrices
   local cVal:=alltrim( Sender:Caption )
   ::MemoryTable1:Fields:A:=padr( cVal, 10 )
   ::DataGrid1:UpdateRow()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn2_OnSave( Sender ) CLASS FormPrices
  local nVal:=Sender:Caption
  with object ::MemoryTable1:Fields
     :B:=nVal
     :C:=nVal*1.25
  end
  ::DataGrid1:UpdateRow()      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonAppend_OnClick( Sender ) CLASS FormPrices
   with object ::MemoryTable1
      :GoBottom()
      :Append()
      with object :Fields
         :A:=space(10)
         :B:=0
         :C:=0
      end
   end
   ::DataGrid1:Update()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonApply_OnClick( Sender ) CLASS FormPrices
   local nError:=0, cKey
   
   hModel:={ => }
   oIni:DelSection( "models" )
   
   with object ::MemoryTable1
      :GoTop()
      do while !:Eof()
         cKey:=alltrim( :Fields:A )
         if empty(cKey) .OR. hGetPos( hModel, cKey ) > 0
            nError:=1
            exit
         endif
         hModel[cKey]:=:Fields:B
         oIni:Write( "models", cKey, ltrim(str(:Fields:B)) )
         :Skip()
      enddo
   end
   
   if nError > 0
      alert( "Model name empty or not unique" )
      return Self
   endif
   
   ::Close()   
RETURN Self