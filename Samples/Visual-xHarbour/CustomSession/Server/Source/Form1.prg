#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   local cApp := "C:\test\CustomSession\Client\console.exe"
   local cUrl1 := "http://www.sourceforge.net"
   local cLocal1 := "C:\test\CustomSession\loco1.txt"
   local cUrl2 := "http://www.xharbour.org"
   local cLocal2 := "C:\test\CustomSession\loco2.txt"
   local cExe := "C:\test\crc\tessssst.exe"
   local lGo := .t.   

   ::lManage := .f.
// the following line normally should be decommented, first time experience "as is"
//   Sender:ShowMode:=SW_HIDE
   ::oSession := CustomSession( .t., .t., ::MyWebBrowser, ::MyNotifyIcon )
   if ::oSession == NIL
      ::Application:Exit()
   endif
   if !::oSession:StartServer()
      alert( ::oSession:EM )
      ::Application:Exit()
   endif
   
   if !::oSession:InsertJob( "CONS", "C" ,cApp, NIL, "00:00:00", NIL, "Y", "E" )
      alert( ::oSession:EM )
      lGo := .f.
   endif

   if !::oSession:InsertJob( "DOWNXH", "S" ,cUrl2, cLocal2, "00:00:00", "00:00:40" )
      alert( ::oSession:EM )
      lGo := .f.
   endif
   if !::oSession:InsertJob( "DOWNSU", "S" ,cUrl1, cLocal1, "00:00:20" )
      alert( ::oSession:EM )
      lGo := .f.
   endif
   
// this is an example for using different job setup methods
   if !::oSession:InsertJob( "TST", "C" , cExe )  // minimal options
      alert( ::oSession:EM )
      lGo := .f.
   endif 
// altering schedule times   
   ::oSession:SetJobSchedule( "TST", "00:00:10", "00:00:20" )
// setting parameters ( command line parameters for executables; GET parameters for scripts )
// a prepared string is also accepted instead of hash ( in case of scripts what should follow the "?" symbol in the URL )
   ::oSession:SetJobQuery( "TST", { "a"=>'"bla bla"', "b"=>'"12 34 56"' } )
// altering server behaviour in case of error   
   ::oSession:SetJobErrMan( "TST", "R", 2, .t. )

   if !lGo
      ::oSession:StopServer()
      alert( "Server stopped due to wrong joblist" )
      ::Application:Exit()
   endif
   
   ::MyTimer:Start()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyOnTimeOut( Sender ) CLASS Form1
   if !::oSession:Server()
      Sender:Stop()
      alert( ::oSession:EM )
      ::Close()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnClose( Sender ) CLASS Form1
   ::MyTimer:Stop()
   if ::oSession <> NIL
      ::oSession:StopServer()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyWebBrowser_DocumentComplete( Sender, pDisp, URL ) CLASS Form1
   if ::oSession == NIL
      return Self
   endif
   ::oSession:SaveDoc()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyWebBrowser_NavigateError( Sender, pDisp, URL, Frame, StatusCode, Cancel ) CLASS Form1
   if ::oSession <> NIL
      ::oSession:SetScriptError(.t.)
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuManage_OnClick( Sender ) CLASS Form1
   if !::lManage
      FormManage(NIL)
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuStop_OnClick( Sender ) CLASS Form1
   if ::MessageBox( "Do you really want to stop the Sample Task Scheduler?", "Task Scheduler", MB_ICONQUESTION|MB_YESNO ) == IDYES
      ::Close()
   endif   
RETURN Self