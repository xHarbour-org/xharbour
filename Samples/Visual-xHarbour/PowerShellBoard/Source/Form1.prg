#include "vxh.ch"
#include "Form1.xfm"

#include "winreg.ch"
#include "hbdll.ch"
IMPORT WideCharToMultiByte( p1, p2, p3, p4, p5, p6, p7, p8 ) FROM Kernel32.dll EXPORTED AS WideCharToMultiByte

//---------------------------------------- End of system code ----------------------------------------//

// PowerShell is Microsoft's utility software designed to perform tasks
// in the contexts of the local system, Active Directory, or Windows Azure

// PowerShell is integrated in Windows 7, and is offered by Microsoft
// as a free download for Vista and XP systems as well

// Designing one's own PowerShell-Board application supports users
// in performing complex routine tasks in simple and safe manner

// PowerShell can also be used to employ .NET classes, or to run 64 bit executables

//----------------------------------------------------------------------------------------------------//
METHOD ButtonProcs_OnClick( Sender ) CLASS Form1
   local cOutFile := ".\test.txt"
   local cCommand := ""
// generating a PowerShell command, the output is a text file ( Unicode )   
   cCommand += '"get-process * | format-table processname'
   if ::CheckId:Checked()
      cCommand += ', Id'
   endif
   if ::CheckHandle:Checked()
      cCommand += ', Handle'
   endif
   if ::CheckStart:Checked()
      cCommand += ', starttime'
   endif
   if ::CheckExit:Checked()
      cCommand += ', ExitTime'
   endif
   if ::CheckUser:Checked()
      cCommand += ', UserProcessorTime'
   endif
   cCommand += ' -auto | out-file '+ cOutFile +'"'
// passing the command to be run
   Form2(::this, { cCommand, cOutFile })
// reading and formatting the output      
   ClassicList( ::This, { cOutFile } )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonServ_OnClick( Sender ) CLASS Form1
   local ScrFile := ::Application:Path + "\test.ps1"   
   local CsvFile := ::Application:Path + "\test.csv"
   local cCommand := '-executionpolicy bypass -file "'+ ScrFile +'"'
   local cWhat:="",  cScript:=""
// generating a simple PowerShell script, the output is a CSV file
   if ::RadioRunning:GetState() == BST_CHECKED
      cWhat := "running"
   elseif ::RadioStopped:GetState() == BST_CHECKED
      cWhat := "stopped"
   endif
   cScript += '$prefilt = Get-Service *'
   if !empty(cWhat)
      cScript += ' | where {$_.status -eq "'+ cWhat +'" }'
   endif
   cScript += chr(13)+chr(10)
   cScript += '$service = $prefilt | select-object DisplayName, ServiceName, Status, CanStop, CanPauseAndContinue, DependentServices' +chr(13)+chr(10)
   cScript += '$service | export-CSV "'+ CsvFile +'"' +chr(13)+chr(10)
// writing the script to the harddisk   
   if !memowrit( ScrFile, cScript, .F. )
      ::MessageBox( "MemoWrit error", "PowerShell Board", MB_OK|MB_ICONHAND )
      return Self
   endif
// passing the script running command
   Form2(::this, { cCommand, CsvFile })
// reading and formatting the output      
   GridList( ::This, { CsvFile, ::CheckDeps:Checked() } )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnCreate( Sender ) CLASS Form1
   local cPath := "software\microsoft\powershell\1\shellids\microsoft.powershell"
   local cKey := "path"
   if Os_IsWin7()   // PowerShell is already part of the OS
      return Self
   endif
   if ( GetRegistry( HKEY_LOCAL_MACHINE, cPath, cKey ) == NIL )
      ::MessageBox( "Sorry, PowerShell 2.0 is not installed on the local machine!", "PowerShell Board", MB_OK|MB_ICONHAND )
      ::Application:Exit()
   endif
RETURN Self