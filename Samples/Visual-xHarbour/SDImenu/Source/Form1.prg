GLOBAL oApp:=NIL, oDish:=NIL, oSauce:=NIL

#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   local lOK:=.f.
   oApp:=Sender
   try
      with object ::tdish
         :Alias:="dish"
         :Path:=".\tables"
         :FileName:="dishes.dbf"
         :Open()
      end
      with object ::tsauce
         :Alias:="sauce"
         :Path:=".\tables"
         :FileName:="sauces.dbf"
         :Open()
      end
      lOK:=.t.
   catch
      ::MessageBox( "Datatable open error", "Error", MB_ICONHAND )
   end
   if !lOK
      ::Close()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnSysCommand( Sender ) CLASS Form1
   if ::wParam == SC_CLOSE
      if ::MessageBox( "Do you want to exit the application?", "", MB_ICONQUESTION|MB_YESNO ) == IDYES
         ::MyCleanUp()
      else
         return 0
      endif
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Dish_OnClick( Sender ) CLASS Form1
   if oDish == NIL
      oDish:=FormDish(NIL)
   endif
   oDish:SetFocus()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Sauce_OnClick( Sender ) CLASS Form1
   if oSauce == NIL
      oSauce:=FormSauce(NIL)
   endif
   oSauce:SetFocus()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyCleanUp() CLASS Form1
   try
      ::tdish:Close()
      ::tsauce:Close()
   catch
      ::MessageBox( "Error while closing datatables", "Error", MB_ICONHAND )
   end
RETURN Self