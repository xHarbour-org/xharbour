GLOBAL EXTERNAL oApp, oChild, oErr, oTable
GLOBAL EXTERNAL lExchange, lStatistics
GLOBAL EXTERNAL cIniPath, cExchangeName
GLOBAL EXTERNAL cDriver, hDriver

#include "vxh.ch"
#include "FormExchange.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormExchange_OnLoad( Sender ) CLASS FormExchange
   ::Caption:=if(lExchange, "Copy To...", "Append From...")
   ::LabelName:Caption:=if(lExchange, "Destination File", "Source File")
   ::EditName:Caption:=""
   ::EditFOR:Caption:=""
   ::PBar:Visible:=.F.
   ::PBar:SetRange({1,50})
   ::PBar:SetStep(1)
   cExchangeName:=""
   myFillBox(Sender:BoxVIA, cDriver, @hDriver)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonSelect_OnClick( Sender ) CLASS FormExchange
// read destionation file (copy to) or source file (append from)   
   local npos
   local cRDD:=right(hDriver[cDriver],3)
   local cExt:=if(cRDD<>"ADT", "DBF", "ADT")

   if lExchange                    // copy to...
      with object oApp:SaveFile
         :DefaultExt:=cExt
         :Filter:="DBase Tables (*.DBF)|*.DBF|Advantage Tables (*.ADT)|*.ADT|All files ( *.* )|*.*"
         :InitialDirectory:=cIniPath
         :Title:="Select location and name for file to copy to"
         :FileName:=""
         :Show()
         cExchangeName:=:FileName
      end
   else                             // append from... data
      with object oApp:OpenFile
      :DefaultExt:=cExt
      :Filter:="DBase Tables (*.DBF)|*.DBF|Advantage Tables (*.ADT)|*.ADT|All files ( *.* )|*.*"
      :InitialDirectory:=cIniPath
      :Title:="Select file to append data from"
      :FileName:=""
      :Show()   
      cExchangeName:=:FileName
      end
   endif

   if empty(cExchangeName)
      return Self
   endif

   npos:=RAt("\", cExchangeName)
   cIniPath:=if(npos>0, Left(cExchangeName, npos-1), "")
   ::EditName:Caption:=cExchangeName

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioVIA_OnClick( Sender ) CLASS FormExchange
   ::BoxVIA:Enabled:=.T.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioBlank_OnClick( Sender ) CLASS FormExchange
   ::BoxVIA:Enabled:=.f.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioTab_OnClick( Sender ) CLASS FormExchange
   ::BoxVIA:Enabled:=.f.   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioPipe_OnClick( Sender ) CLASS FormExchange
   ::BoxVIA:Enabled:=.f.   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioSDF_OnClick( Sender ) CLASS FormExchange
   ::BoxVIA:Enabled:=.f.   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonOK_OnClick( Sender ) CLASS FormExchange 
   
   local lFOR, lVIA, lBlank, lTab, lPipe, lSDF, lOK
   local cFOR:=alltrim(::EditFOR:Caption), cType, cOpened
   local cVIA:=HGetValueAt( hDriver, ::BoxVIA:GetCurSel())
   local nSeconds, nADSErr
// check values found on the form   
   if empty(alltrim(cExchangeName))
      ::MessageBox(if(lExchange, "Please select a valid filename to copy to", "Please select the file to append from"),;
                  "Warning", MB_ICONEXCLAMATION)
      ::Enable()
      return Self
   endif
   
   cOpened:=oTable:Path+if(empty(oTable:Path), "", "\")+oTable:FileName
   if upper(alltrim(cOpened)) == upper(alltrim(cExchangeName))
      ::MessageBox("Please select a file which is different from the current file",;
                  "Warning", MB_ICONEXCLAMATION)
      ::Enable()
      return Self
   endif
   
   lFOR:=if(::CheckFOR:Checked(), .t., .f.)
   if lFOR
      if empty(cFOR)
         ::MessageBox( "Please type in a FOR condition or uncheck its activation option", "Warning", MB_ICONEXCLAMATION)
         return Self
      endif
      if len(cFOR) > 255
         ::MessageBox( "The current FOR condition has "+ltrim(str(len(cFOR)))+" characters - the maximum is 255", "Condition too long", MB_ICONEXCLAMATION)
         return Self
      endif
      lOK:=.f.
      Try
         cType:=valtype(&cFOR)
         lOK:=.t.
      catch oErr
         ::MessageBox( "Please type in a valid expression", "Warning", MB_ICONEXCLAMATION)
      end
      if !lOK
         return Self
      endif
      if cType <> "L"
         ::MessageBox( "Please type in a valid expression which evaluates to logical type", "Warning", MB_ICONEXCLAMATION)
         return Self
      endif
   endif

   ::Disable()
// proceed   
   lVIA:=if( ::RadioVIA:GetState()==BST_CHECKED, .t., .f.)
   lBlank:=if( ::RadioBlank:GetState()==BST_CHECKED, .t., .f.)
   lTab:=if( ::RadioTab:GetState()==BST_CHECKED, .t., .f.)
   lPipe:=if( ::RadioPipe:GetState()==BST_CHECKED, .t., .f.)
   lSDF:=if( ::RadioSDF:GetState()==BST_CHECKED, .t., .f.)

   ::PBar:Visible:=.t.
   ::Timer1:Start()
   lOK:=.f.
   
   nSeconds:=Seconds()

   Try

      if lExchange .and. lFOR   // copy to... with FOR
         if lSDF
            copy to (cExchangeName) for &cFOR SDF
         elseif lBlank
            copy to (cExchangeName) for &cFOR delimited with BLANK
         elseif lTab 
            copy to (cExchangeName) for &cFOR delimited with TAB
         elseif lPipe
            copy to (cExchangeName) for &cFOR delimited with PIPE
         else  //  via
            copy to (cExchangeName) for &cFOR VIA (cVIA)
         endif
      endif
   
      if !lExchange .and. lFOR   // append from... with FOR
         if lSDF
            append from (cExchangeName) for &cFOR SDF
         elseif lBlank
            append from (cExchangeName) for &cFOR delimited with BLANK
         elseif lTab 
            append from (cExchangeName) for &cFOR delimited with TAB
         elseif lPipe
            append from (cExchangeName) for &cFOR delimited with PIPE
         else  //  via
            append from (cExchangeName) for &cFOR VIA (cVIA)
         endif
      endif
   
      if lExchange .and. !lFOR
         if lSDF
            copy to (cExchangeName) SDF
         elseif lBlank
            copy to (cExchangeName) delimited with BLANK
         elseif lTab 
            copy to (cExchangeName) delimited with TAB
         elseif lPipe
            copy to (cExchangeName) delimited with PIPE
         else  //  via
            copy to (cExchangeName) VIA (cVIA)
         endif
      endif
      
      if !lExchange .and. !lFOR
         if lSDF
            append from (cExchangeName) SDF
         elseif lBlank
            append from (cExchangeName) delimited with BLANK
         elseif lTab 
            append from (cExchangeName) delimited with TAB
         elseif lPipe
            append from (cExchangeName) delimited with PIPE
         else  //  via
            append from (cExchangeName) VIA (cVIA)
         endif
      endif

      if cVIA == "ADT"
         nADSErr:=AdsGetLastError()
         if nADSErr > 0
            ::MessageBox( "Operation terminated with ADS error code "+ltrim(str(nADSErr)), "", MB_ICONEXCLAMATION)
         endif
      endif
      
      lOK:=.t.
      
   catch oErr
      myError(oApp, oErr)      
   end
   
   nSeconds=mySeconds( nSeconds, Seconds())
   
   ::Timer1:Stop()
   ::PBar:Visible:=.f.
   
   if !lExchange
      oChild:Grid:Update()
   endif
   if lStatistics
      ::MessageBox(if(lExchange, "Copy To...", "Append From...")+" has been terminated in "+ltrim(str(nSeconds))+" seconds",;
                   "", MB_ICONINFORMATION)
   endif

   ::Enable() 
   
   ::Close()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormExchange
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Timer1_OnTimeOut( Sender ) CLASS FormExchange
   ::PBar:StepIt()
   Sender:Start()
RETURN Self