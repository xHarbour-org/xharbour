GLOBAL EXTERNAL oApp, oChild, oErr, oTable
GLOBAL EXTERNAL cTableName, cIndexName, cIniPath
GLOBAL EXTERNAL lOpenTable, lOpenIndex, lShowd, lMarker, lMarkerSec
GLOBAL EXTERNAL lConvertOEM, lShadowRow, lShowGrid, lShowHeaders, lShared
GLOBAL EXTERNAL cFontFace, cFontSize, cDriver
GLOBAL EXTERNAL cBackColor, cForeColor, cHighlightColor, cHighlightTextColor
GLOBAL EXTERNAL cMarkerCondition, cMarkerConditionSec
GLOBAL EXTERNAL cMarkerColor, cMarkerColorSec, cMarkerTextColor, cMarkerTextColorSec
GLOBAL EXTERNAL nMarkerColor, nMarkerTextColor, nMarkerColorSec, nMarkerTextColorSec
GLOBAL EXTERNAL nCursorCalc, nStructural, nAlias, aIndex
GLOBAL EXTERNAL hFontFace, hFontSize, hBack, hFore
GLOBAL EXTERNAL hHighlight, hHighlightText, hDriver

#include "vxh.ch"
#include "ChildGrid.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD ChildGrid_OnLoad( Sender ) CLASS ChildGrid  
   local i, npos, nl, lOK
   local cRDD:=right(hDriver[cDriver],3)
   local cExt:=if(left(hDriver[cDriver],3)=="DBF", "DBF", "ADT")
   local cFilter:=if(cExt=="DBF", "DBase Tables (*.DBF)|*.DBF|", "Advantage Tables (*.ADT)|*.ADT|")+;
                     "All files ( *.* )|*.*"
   oChild:=Sender
   Sender:MDIMaximize()                     
   oTable:=if( cRDD=="ADT", Sender:ADSFree, Sender:Table)                                       
// get table name
   with object oApp:OpenFile
      :DefaultExt:=cExt
      :Filter:=cFilter
      :InitialDirectory:=cIniPath
      :Title:="Open Table"
      :FileName:=""
      :Show()
   end
   if empty(oApp:OpenFile:FileName)
      Sender:Close()
      return Self
   endif

   oApp:Cursor:=nCursorCalc
// set table properties   
   cTableName:=oApp:OpenFile:FileName
   oTable:Driver:=hDriver[cDriver]
   nl:=len(oApp:OpenFile:FileName)
   npos:=RAt("\",oApp:OpenFile:FileName)
   if npos > 0
      oTable:Path:=Left(oApp:OpenFile:FileName, npos-1)
      oTable:FileName:=Right(oApp:OpenFile:FileName, nl-npos)
   else
      oTable:Path:=""
      oTable:FileName:=oApp:OpenFile:FileName
   endif
   cIniPath:=oTable:Path
   oTable:Shared:=lShared
   oTable:SetAlias("c"+ltrim(str(++nAlias,6)))

// generate structural index name   
   nl:=RAt(".", oTable:FileName)
   cIndexName:=oTable:Path+"\"+if( nl>0, left(oTable:FileName, nl), oTable:FileName)
   if cRDD=="NTX"
      cIndexName:=cIndexName+cRDD
   elseif cRDD=="ADT"
      cIndexName:=cIndexName+"ADI"
   else
      cIndexName:=cIndexName+"CDX"
   endif

// open table
   lOK:=.f.
   try
      oTable:Open()
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   if !lOK
      Sender:Close()
      return Self
   endif
   
// test table empty
   if len(oTable:Structure) < 1
       ::MessageBox("The selected Table doesn't contain any columns","Warning",MB_ICONEXCLAMATION)
       lOK:=.f.
       try
          oTable:Close()
          lOK:=.t.
       catch oErr
          myError(oApp, oErr)
       end
       if !lOK
          oApp:Enable()
          ::Application:Exit()
       endif
       Sender:Close()
       return Self
   endif

   Sender:BackColor:=NIL
   try
      with object Sender:Grid
         :Visible:=.t.
         :DataSource:=oTable
         :AutoAddColumns()
         :Font:FaceName:=hFontFace[cFontFace]
         :Font:PointSize:=hFontSize[cFontSize]
         :ConvertOEM:=lConvertOEM
         :ShadowRow:=lShadowRow
         :ShowGrid:=lShowGrid
         :ShowHeaders:=lShowHeaders
         :ForeColor:=hFore[cForeColor]
         :BackColor:=hBack[cBackColor]
         :HighlightColor:=hHighlight[cHighlightColor]
         :HighlightTextColor:=hHighlightText[cHighlightTextColor]
      end
      Sender:GridColumn1:EventHandler["OnQueryImageIndex"]:="myImageIndex" 
      lOpenTable:=.t.
   catch oErr
      myError(oApp, oErr)
   end   
   if !lOpenTable
      Sender:Close()
      return Self
   endif   
// set up index stuff
   aIndex:={{"", "", "", "no", "natural order", "", ""}}
   oApp:BoxIndex:AddString(aIndex[1,5])
// open structural index   
   if lOpenTable
      if cRDD<>"FPT" .and. file(cIndexName)
         myIndexOpen()
      endif
      nStructural:=if( cRDD=="NTX", 1, len(aIndex))
      oTable:GoTop()
      Sender:Grid:Update()   
   endif      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myImageIndex( Sender ) CLASS ChildGrid
// managing small icons which represent present or deleted free table rows   
   if lShowd
      if ::Grid:DataSource:Deleted()
         return 2
      else
         return 1
      endif
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myMarkerBack( Sender ) CLASS ChildGrid
// sets marker backcolor   
   if lMarker .and. &cMarkerCondition
      return nMarkerColor
   elseif lMarkerSec .and. &cMarkerConditionSec
      return nMarkerColorSec
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myMarkerFore( Sender ) CLASS ChildGrid
// sets marker forecolor (text color)
   if lMarker .and. &cMarkerCondition
      return nMarkerTextColor
   elseif lMarkerSec .and. &cMarkerConditionSec
      return nMarkerTextColorSec
   endif      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ChildGrid_OnClose( Sender ) CLASS ChildGrid
   if lOpenTable
      try
         ::Grid:DataSource:Close()
         //oTable:Close()
         lOpenTable:=.f.
      catch oErr
         myError(oErr)
      end
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Grid_OnCreate( Sender ) CLASS ChildGrid
   Sender:Visible:=.f.
RETURN Self