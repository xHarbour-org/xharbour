GLOBAL EXTERNAL oApp, oErr, oTable, oVTable
GLOBAL EXTERNAL nCursorCalc, nCursorNormal
GLOBAL EXTERNAL cTableName

GLOBAL EXTERNAL lShowd, lView, lCols, lTop, lBottom, lMarker, lMarkerSec
GLOBAL EXTERNAL cTopCondition, cBottomCondition, cViewIndex
GLOBAL EXTERNAL cMarkerCondition, cMarkerConditionSec, cTempPath
GLOBAL EXTERNAL aIndex, aCols, aView, aMarker
GLOBAL EXTERNAL lShadowRow, lShowGrid, lShowHeaders, lConvertOEM
GLOBAL EXTERNAL cDriver, cDateFormat, cCodePage, cFontFace, cFontSize
GLOBAL EXTERNAL cBackColor, cForeColor, cHighlightColor, cHighlightTextColor
GLOBAL EXTERNAL nLight, nDark, nTemp, nAlias
GLOBAL EXTERNAL nMarkerColor, nMarkerTextColor, nMarkerColorSec, nMarkerTextColorSec
GLOBAL EXTERNAL hDriver, hDate, hCode
GLOBAL EXTERNAL hFontFace, hFontSize, hBack, hFore
GLOBAL EXTERNAL hHighlight, hHighlightText

#include "vxh.ch"
#include "FormCurrentView.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormCurrentView_OnLoad( Sender ) CLASS FormCurrentView
   local i, npos, nl, lOK, cVal, cIndexKey, cIndexFilter
   local cRDD:=right(hDriver[cDriver],3)
// set up VIEW datatable info
   ::Cursor:=nCursorCalc
   ::Disable()
   oVTable:=if(cRDD=="ADT", Sender:VAdsFree, Sender:VTable)
   oVTable:Driver:=hDriver[cDriver]
   nl:=len(cTableName)
   npos:=RAt("\", cTableName)
   if npos > 0
      oVTable:Path:=Left(cTableName, npos-1)
      oVTable:FileName:=Right(cTableName, nl-npos)
   else
      oVTable:Path:=""
      oVTable:FileName:=cTableName
   endif
   oVTable:SetAlias("c"+ltrim(str(++nAlias,6)))

// open VIEW table
   lOK:=.f.
   try
      oVTable:Open()      
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   if !lOK
      ::Enable()
      ::Cursor:=nCursorNormal
      ::Close()
      return Self
   endif

// if ordering or filtering are active, creates temporary index
   if lTop .or. lBottom
      
      nTemp++
      cViewIndex:=myTempFileName( @nTemp, cTempPath, cRDD)
      cIndexKey:=if(lTop, cTopCondition, "RecNo()")
      cIndexFilter:=if(lBottom, cBottomCondition, "")
      lOK:=.f.
      try
         oVTable:CreateOrder( cViewIndex, "temp", cIndexKey, cIndexFilter)
         lOK:=.t.
      catch oErr
         myError(oApp, oErr)
      end
      
      if !lOK  // temporary index file creation failure
         try
            ::VGrid:DataSource:Close()
            //oVTable:Close()
            lOK:=.t.
         catch oErr
            myError(oApp, oErr)
         end   
         if !lOK  // close fails
            ::Enable()
            ::Application:Exit()
            return Self
         endif
         if file(cViewIndex)
            DeleteFile(cViewIndex)
         endif
         ::Enable()
         ::Cursor:=nCursorNormal
         ::Close()                  
      endif
      
   endif

// link to grid and manage columns
   lOK:=.f.
   try
      ::VGrid:DataSource:=oVTable
      ::VGrid:AutoAddColumns()
      if lCols       // column selection is active
         for i:=len(aCols) to 2 step -1
            if !aView[i]
               ::VGrid:DeleteColumn(i)
            endif
         next
      endif
      ::VGrid:Font:FaceName:=hFontFace[cFontFace]
      ::VGrid:Font:PointSize:=hFontSize[cFontSize]
      ::VGrid:ConvertOEM:=lConvertOEM
      ::VGrid:ShadowRow:=lShadowRow
      ::VGrid:ShowGrid:=lShowGrid
      ::VGrid:ShowHeaders:=lShowHeaders
      ::VGrid:ForeColor:=hFore[cForeColor]
      ::VGrid:BackColor:=hBack[cBackColor]
      ::VGrid:HighlightColor:=hHighlight[cHighlightColor]
      ::VGrid:HighlightTextColor:=hHighlightText[cHighlightTextColor]
//      ::GridColumn1:ImageIndex:=0
      ::GridColumn1:EventHandler["OnQueryImageIndex"]:="myImgIndex"
      if lMarker .or. lMarkerSec  // when a marker is active
         for i:=1 to len(aCols)
            cVal:="GridColumn"+ltrim(str(i))           
            if ( aMarker[i] .and. !lCols ) .or. ( aMarker[i] .and. lCols .and. aView[i] )
               ::&cVal:EventHandler["OnQueryBackColor"]:="myMarkBack"
               ::&cVal:EventHandler["OnQueryForeColor"]:="myMarkFore"
            endif
         next
      endif      
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end   
   
   if lOK
      
      oVTable:Gotop()
      ::VGrid:Update()
      ::Enable()
      ::Cursor:=nCursorNormal
      
   else             // something went wrong after table open
      
      try
         ::VGrid:DataSource:Close()
         //oVTable:Close()
         lOK:=.t.
      catch oErr
         myError(oApp, oErr)
      end   
      if !lOK  // close fails
         ::Enable()
         ::Application:Exit()
         return Self
      endif
      if file(cViewIndex)
         DeleteFile(cViewIndex)
      endif
      ::Enable()
      ::Cursor:=nCursorNormal
      ::Close()
      
   endif
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormCurrentView_OnClose( Sender ) CLASS FormCurrentView
   local lOK:=.f.
   ::Cursor:=nCursorCalc
   ::Disable()   
   try
      ::VGrid:DataSource:Close()
      //oVTable:Close()
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end   
   if !lOK  // close fails
      ::Enable()
      ::Application:Exit()
      return Self
   endif
   if file(cViewIndex)
      DeleteFile(cViewIndex)
   endif
   ::Enable()
   ::Cursor:=nCursorNormal
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myMarkBack( Sender ) CLASS FormCurrentView
// marker backcolor   
   if lMarker .and. &cMarkerCondition
      return nMarkerColor
   elseif lMarkerSec .and. &cMarkerConditionSec
      return nMarkerColorSec
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myMarkFore( Sender ) CLASS FormCurrentView
// marker forecolor (text)   
   if lMarker .and. &cMarkerCondition
      return nMarkerTextColor
   elseif lMarkerSec .and. &cMarkerConditionSec
      return nMarkerTextColorSec
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myImgIndex( Sender ) CLASS FormCurrentView
// manage small icons marking present/deleted rows   
   if lShowd
      if ::VGrid:DataSource:Deleted()
         return 2
      else
         return 1
      endif
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormCurrentView_OnDestroy( Sender ) CLASS FormCurrentView
   select(oTable:Alias)
RETURN Self