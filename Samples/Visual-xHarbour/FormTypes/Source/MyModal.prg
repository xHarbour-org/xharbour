GLOBAL EXTERNAL nCounter

#include "vxh.ch"
#include "MyModal.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD MyModal_OnInitDialog( Sender ) CLASS MyModal
   alert("create modal form")   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModal_OnLoad( Sender ) CLASS MyModal
   alert("load modal form")      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModal_OnShowWindow( Sender ) CLASS MyModal
   alert("show modal form")   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModal_OnCancel( Sender ) CLASS MyModal
   alert("OnCancel modal form")      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModal_OnClose( Sender ) CLASS MyModal
   alert("OnClose modal form")      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModal_OnDestroy( Sender ) CLASS MyModal
   alert("destroy modal form")      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyModal_OnSysCommand( Sender ) CLASS MyModal
   local nResponse
   switch ::wParam
   case SC_CLOSE
      nResponse=::MessageBox( "Do you really want to close the modal form", "MyModal", MB_ICONQUESTION|MB_YESNO )
      if nResponse <> IDYES
         return 1
      endif
      exit
   case SC_MINIMIZE
      alert("minimize modal form")
      exit
   case SC_MAXIMIZE
      alert("maximize modal form")
      exit
   case SC_RESTORE
      alert("restore modal form")
      exit
   default
   end   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonApply_OnClick( Sender ) CLASS MyModal
   alert("APPLY button clicked")
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonUpdateApp_OnClick( Sender ) CLASS MyModal
   nCounter++
   with object ::Application:MainForm
      :StatusBarPanel2:Caption:=str(nCounter)
   end
RETURN Self