#include "vxh.ch"
#include "FormManage.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD ButtonApply_OnClick( Sender ) CLASS FormManage
   local i
   ::mt:GoTop()
   for i:=1 to len( ::aOld )
      if ::aOld[i, 2] <> ::mt:Fields:STARTED  // job status has been modified
         if !::Application:MainForm:oSession:SetJobStatus( ::dg:DataSource:Fields:JOB, ::dg:DataSource:Fields:STARTED )
            alert( ::Application:MainForm:oSession:EM )
         endif
      endif
      if ::aOld[i, 3] <> ::mt:Fields:NOTIFY   // "notify" option has been modified
         if !::Application:MainForm:oSession:SetJobNotify( ::dg:DataSource:Fields:JOB, ::dg:DataSource:Fields:NOTIFY )
            alert( ::Application:MainForm:oSession:EM )
         endif
      endif
      ::mt:Skip()
   next
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormManage
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormManage_OnLoad( Sender ) CLASS FormManage
   local i
   with object ::Application:MainForm
      :MyTimer:Stop()  // stop looping, so don't mess job setups
      :lManage := .t.
      ::aOld := :oSession:GetJobs()  // keep a copy of the current setups
   end
   for i:= 1 to len( ::aOld )
      with object ::mt
         :Append()
         :Fields:JOB := ::aOld[i, 1]
         :Fields:STARTED := ::aOld[i, 2]
         :Fields:NOTIFY := ::aOld[i, 3]
      end
   next   
   ::mt:GoTop()
   ::dg:Update()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD dg_OnClick( Sender ) CLASS FormManage
   local nCol:=Sender:ColPos
   if nCol < 2
      return Self
   endif
   with object ::dg:DataSource
      if nCol == 2
         :Fields:STARTED := !:Fields:STARTED
      else
         :Fields:NOTIFY := !:Fields:NOTIFY
      endif
   end
   ::dg:Update()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormManage_OnDestroy( Sender ) CLASS FormManage
   with object ::Application:MainForm
      :MyTimer:Start()  // restarting the engine
      :lManage := .f.
   end
RETURN Self