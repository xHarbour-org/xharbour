GLOBAL EXTERNAL oApp, oDish, oSauce

#include "vxh.ch"
#include "FormSauce.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD FormSauce_OnLoad( Sender ) CLASS FormSauce
   with object ::gsauce
      :DataSource:=oApp:tsauce
      :AutoAddColumns()
      oApp:tsauce:GoTop()
      :Update()
   end
   ::EditName:Caption:=""
   ::EditPrice:Caption:="0"
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormSauce_OnDestroy( Sender ) CLASS FormSauce
   if oDish <> NIL
      oDish:MyFillSauces()
   endif
   ::gsauce:DataSource:=NIL
   oSauce:=NIL
   oApp:SetFocus()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonAdd_OnClick( Sender ) CLASS FormSauce
   local cName:=alltrim( ::EditName:Caption )
   local nPrice:=val( alltrim( ::EditPrice:Caption ) )
   if empty( cName ) .or. len(cName) > 30
      ::MessageBox( "Name empty or too long", "", MB_ICONEXCLAMATION )
      return Self
   endif
   if nPrice<1.0 .or. nPrice>50.0
      ::MessageBox( "Price should be between $1.00 and $50.00", "", MB_ICONEXCLAMATION )
      return Self
   endif
   with object oApp:tsauce
      :Append()
      :RecLock()
      :Fields:NAME:=cName
      :Fields:PRICE:=nPrice
      :UnLock()
   end
   ::gsauce:Update()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDel_OnClick( Sender ) CLASS FormSauce
   if oApp:tsauce:Reccount() < 1
      return Self
   endif
   with object oApp:tsauce
      :RecLock()
      :Delete()
      :UnLock()
   end
   ::gsauce:Update()      
RETURN Self