GLOBAL EXTERNAL oModeless, nCounter

#include "vxh.ch"
#include "MyModeless.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD MyModeless_OnCreate( Sender ) CLASS MyModeless
   oModeless:=Sender
   alert("create modeless form")   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModeless_OnLoad( Sender ) CLASS MyModeless
   alert("load modeless form")
   nCounter++
   Sender:StatusBarPanel1:Caption:="Counter:"
   Sender:StatusBarPanel2:Caption:=str(nCounter)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModeless_OnShowWindow( Sender ) CLASS MyModeless
   alert("show modeless form")
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModeless_OnHideWindow( Sender ) CLASS MyModeless
   alert("hide modeless form")
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModeless_OnClose( Sender ) CLASS MyModeless
   alert("close modeless form")
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModeless_OnSysCommand( Sender ) CLASS MyModeless
   local nResponse
   switch ::wParam
   case SC_CLOSE
      nResponse=::MessageBox( "Do you really want to close the modeless form", "MyModeless", MB_ICONQUESTION|MB_YESNO )
      if nResponse <> IDYES
         return 1
      endif
      exit
   case SC_MINIMIZE
      alert("minimize modeless form")
      exit
   case SC_MAXIMIZE
      alert("maximize modeless form")
      exit
   case SC_RESTORE
      alert("restore modeless form")
      exit
   default
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModeless_OnDestroy( Sender ) CLASS MyModeless
   oModeless:=NIL
   ::Application:MainForm:SetFocus()
   alert("destroy modeless form")
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonUpdate_OnClick( Sender ) CLASS MyModeless
   nCounter++
   with object ::Application:MainForm
      :StatusBarPanel2:Caption:=str(nCounter)
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonApply_OnClick( Sender ) CLASS MyModeless
   alert("APPLY button clicked")
   ::Close()   
RETURN Self