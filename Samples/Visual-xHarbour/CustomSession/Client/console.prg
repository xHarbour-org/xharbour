GLOBAL hTaskIdle:=NIL, oSession := NIL
GLOBAL MyJob:="DOWNXH", MyValue:="*"

function main()
   local i, j, cFile:= hb_argv(1)
   if hb_argc()<1 .or. valtype(cFile)<>"C"
      wait "Wrong session file parameter"
      return NIL
   endif

   if !MyStartBackground( cFile )  
      return NIL
   endif
   
   for i:=1 to 100
      if lastkey() == 27
         exit
      endif
      for j:= 1 to 10
         if lastkey() == 27
            exit
         endif
         ? j
         inkey( 1.0 )
      next
      ShowData()
   next

   MyStopBackground()
   
   wait "...ok..."
return NIL


// dummy function - needed only when a console program does not link ShellExecute,
// and it connects as a CustomSession CLIENT to a session file
function ShellExecute()
return 100



// displays current information
function ShowData()
   local cValue, aTimes
   if oSession == NIL
      return NIL
   endif
   aTimes := oSession:GetTimes()
   ? "Server time-stamp: " + aTimes[1]
   ? "Client time-stamp: " + aTimes[2]
   cValue := oSession:GetNotice( MyJob )   
   if cValue == NIL
      ? "----------------- no job " + MyJob
   else
      if cValue <> MyValue
         MyValue := cValue
         ? "------------------ job " + MyJob + " time info: " + MyValue
      endif
   endif
   
return NIL



function MyStartBackground( cFile )

   oSession := CustomSession():new( .f. )
   if oSession == NIL
      return NIL
   endif
   if !oSession:Connect( cFile )
      wait "cannot connect"
      wait oSession:EM
      oSession := NIL
      return .f.
   endif
   
   hb_idlesleepmsec( 50 )
   hTaskIdle := hb_idleadd( { || MyCheckBackground() } )
   if valtype( hTaskIdle ) == "P"
      return .t.
   endif
   
   wait "error when starting idle process"
   oSession:Disconnect()
   oSession := NIL
   
return .f.



function MyCheckBackground()
   local lOk := oSession:Client()
   if !lOk
      ? oSession:EM
      MyStopBackGround()
   endif
return .T.



function MyStopBackground()
   
   if hTaskIdle <> NIL
      hb_idledel( hTaskIdle )
      hTaskIdle := NIL
   endif 

   if oSession == NIL
      return .t.
   endif
   if !oSession:Disconnect()
      wait "Session - failed to disconnect"
      wait oSession:EM
   endif
   oSession := NIL
   
return .T.