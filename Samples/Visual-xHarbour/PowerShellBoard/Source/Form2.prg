#include "vxh.ch"
#include "Form2.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD PSTimer_OnTimeOut( Sender ) CLASS Form2
   if ::IsWindowVisible()
      if ::nPos < 1
         ::nPos++
         ::Proggy:StepIt()
         ::PSProcess()
      else
         if ::nPos%5 == 0
            if file( ::cFile )
               ::PSTimer:Stop()
               ::Close()
            endif
         endif
         ::nPos++
         ::Proggy:StepIt()
      endif
   else
      return Self
   endif
   
   if ::nPos > 99
      ::PSTimer:Stop()
      ::MessageBox( "Query time-out, please check the PowerShell process", "PowerShell Board", MB_OK|MB_ICONEXCLAMATION )
      ::Application:Exit()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD PSProcess() CLASS Form2
   local cCommand := ::Params[1]
   ShellExecute( NIL, "Open", 'PowerShell', cCommand, , SW_HIDE )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form2_OnLoad( Sender ) CLASS Form2
   ::nPos:=0
   ::cFile := ::Params[2]
   if File( ::cFile )
      DeleteFile( ::cFile )
   endif
   ::PSTimer:Start()
RETURN Self