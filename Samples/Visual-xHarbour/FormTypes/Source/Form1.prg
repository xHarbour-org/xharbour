GLOBAL oModeless:=NIL, nCounter:=0

#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnCreate( Sender ) CLASS Form1
   alert("create application form")
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   alert("application OnLoad")
   Sender:StatusBarPanel1:Caption:="Counter:"
   Sender:StatusBarPanel2:Caption:=str(nCounter)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnClose( Sender ) CLASS Form1
   alert("application OnClose")      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnSysCommand( Sender ) CLASS Form1
   local nResponse
   if ::wParam == SC_CLOSE
      nResponse=::MessageBox( "Do you really want to close the application form", "Application", MB_ICONQUESTION|MB_YESNO )
      if nResponse <> IDYES
         return 0
      endif
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnDestroy( Sender ) CLASS Form1
   alert( "destroy application form")  
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CreateModeless_OnClick( Sender ) CLASS Form1
   if oModeless <> NIL
      ::MessageBox( "A modeless form already exists", "", MB_ICONEXCLAMATION )
      return Self
   endif
   MyModeless( NIL )  
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ShowModeless_OnClick( Sender ) CLASS Form1
   if oModeless == NIL
      ::MessageBox( "The modeless form hasn't been created yet", "", MB_ICONEXCLAMATION )
      return Self
   endif
   oModeless:Show()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD HideModeless_OnClick( Sender ) CLASS Form1
   if oModeless == NIL
      ::MessageBox( "The modeless form hasn't been created yet", "", MB_ICONEXCLAMATION )
      return Self
   endif
   oModeless:Hide()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MinimizeModeless_OnClick( Sender ) CLASS Form1
   if oModeless == NIL
      ::MessageBox( "The modeless form hasn't been created yet", "", MB_ICONEXCLAMATION )
      return Self
   endif
   oModeless:Minimize()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RestoreModeless_OnClick( Sender ) CLASS Form1
   if oModeless == NIL
      ::MessageBox( "The modeless form hasn't been created yet", "", MB_ICONEXCLAMATION )
      return Self
   endif
   oModeless:Restore()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CloseModeless_OnClick( Sender ) CLASS Form1
   if oModeless == NIL
      ::MessageBox( "The modeless form hasn't been created yet", "", MB_ICONEXCLAMATION )
      return Self
   endif
   oModeless:Close()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CountModeless_OnClick( Sender ) CLASS Form1
   if oModeless == NIL
      ::MessageBox( "The modeless form hasn't been created yet", "", MB_ICONEXCLAMATION )
      return Self
   endif
   nCounter++
   with object oModeless
      :StatusBarPanel1:Caption:="Counter:"
      :StatusBarPanel2:Caption:=str(nCounter)
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CreateModal_OnClick( Sender ) CLASS Form1
   MyModal( ::this )   
RETURN Self