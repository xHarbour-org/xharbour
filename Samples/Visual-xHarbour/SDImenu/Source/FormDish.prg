GLOBAL EXTERNAL oApp, oDish, oSauce

#include "vxh.ch"
#include "FormDish.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormDish_OnLoad( Sender ) CLASS FormDish
   ::MyFillSauces()
   ::EditName:Caption:=""
   ::EditPrice:Caption:="0"
   with object ::gdish
      :DataSource:=oApp:tdish
      :AutoAddColumns()
      oApp:tdish:GoTop()
      :Update()
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormDish_OnDestroy( Sender ) CLASS FormDish
   ::gdish:DataSource:=NIL
   oDish:=NIL
   oApp:SetFocus()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyFillSauces() CLASS FormDish
   oApp:tsauce:GoTop()
   with object ::BoxSauce
      :ResetContent()
      do while !oApp:tsauce:Eof()
         :AddItem( oApp:tsauce:Fields:NAME )
         oApp:tsauce:Skip()
      enddo
   end   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonAdd_OnClick( Sender ) CLASS FormDish
   local cName:=alltrim( ::EditName:Caption )
   local nPrice:=val( alltrim( ::EditPrice:Caption ) )
   local cSauce:=if( ::BoxSauce:GetCurSel()>0, ::BoxSauce:GetSelString(), "" )
   if empty( cName ) .or. len(cName) > 30
      ::MessageBox( "Name empty or too long", "", MB_ICONEXCLAMATION )
      return Self
   endif
   if nPrice<1.0 .or. nPrice>300.0
      ::MessageBox( "Price should be between $1.00 and $300.00", "", MB_ICONEXCLAMATION )
      return Self
   endif
   with object oApp:tdish
      :Append()
      :RecLock()
      :Fields:NAME:=cName
      :Fields:SAUCE:=cSauce
      :Fields:PRICE:=nPrice
      :UnLock()
   end
   ::gdish:Update()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDel_OnClick( Sender ) CLASS FormDish
   if oApp:tdish:Reccount() < 1
      return Self
   endif
   with object oApp:tdish
      :RecLock()
      :Delete()
      :UnLock()
   end
   ::gdish:Update()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonUpdate_OnClick( Sender ) CLASS FormDish
   if oSauce == NIL
      oSauce:=FormSauce(NIL)
   endif
   oSauce:SetFocus()
RETURN Self