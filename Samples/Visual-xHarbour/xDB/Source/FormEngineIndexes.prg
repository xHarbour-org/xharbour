GLOBAL EXTERNAL oApp, oErr, oTable
GLOBAL EXTERNAL nCursorCalc, nCursorNormal
GLOBAL EXTERNAL aCursor, aStatistics
GLOBAL EXTERNAL nMaxCursor
GLOBAL EXTERNAL lStatistics, lOpenTable
GLOBAL EXTERNAL cEngine, cSchema, cSchemaTable, hConnection, oSQL
GLOBAL EXTERNAL cTempPath
GLOBAL EXTERNAL nAlias, nTemp
GLOBAL EXTERNAL nLight, nDark, nMarkerColor, nMarkerColorSec

#include "vxh.ch"
#include "FormEngineIndexes.xfm"
//---------------------------------------- End of system code ----------------------------------------//



function StruBarChart(oG, nPositionStru, aPositionStru, aTextStru, aColorStru, nColBack, nColText, lMarg)
// draws a variable number of horizontal bars with desired values, labels, colors
   local i:=0, j:=0, nVal:=0, nMax:=0, nSum:=0
   local nTop:=0, nLeft:=0, nRight:=0, nBottom:=0
   local nVert:=oG:Height, nHoriz:=oG:Width, nPos:=nPositionStru
   local nMarg:=10, nTextW:=140, nTextDesc:=80, nTextProc:=60, nTextH:=20
   local nStep:=0
   local nBarW:=0, nBarH:=0
   local hBrushBack:=CreateSolidBrush(nColBack)
   local hFont:=GetStockObject(DEFAULT_GUI_FONT)
   local hBrushCur
   oG:Drawing:SelectObject(hFont)
   
   nMax:=nHoriz-3*nMarg-nTextW
   nStep:=(nVert-2*nMarg)/nPos
   nBarH:=Max(nTextH, nStep*3/4)
   nVal:=0
   nSum:=0
   for i:=1 to nPos
      nSum:=nSum+aPositionStru[i]
      if aPositionStru[i] > nVal
         nVal:=aPositionStru[i]
      endif
   next
   
   oG:Drawing:FillRect({0,0,nHoriz,nVert},hBrushBack)
   
   for i:=1 to nPos
      if aPositionStru[i] < 0.01
         loop
      endif
      nBarW:=aPositionStru[i]*nMax/nVal
      hBrushCur:=CreateSolidBrush(aColorStru[i])
      nLeft:=nTextW+2*nMarg+1
      nRight:=nLeft+nBarW-1
      nTop:=nMarg+(i-1)*nStep+1
      nBottom:=nTop+nBarH-1
      if lMarg
         oG:Drawing:Rectangle(nLeft, nTop, nRight, nBottom, nColText, hBrushCur)
      else
         oG:Drawing:FillRect({nLeft, nTop, nRight, nBottom}, hBrushCur)
      endif
   next

   oG:Drawing:SetBkMode(TRANSPARENT)
   oG:Drawing:SetTextColor(nColText)
   
   for i:=1 to nPos
      nLeft:=nMarg+1
      nRight:=nLeft+nTextDesc-1
      nTop:=nMarg+(i-1)*nStep+1
      nBottom:=nTop+nTextH-1
      oG:Drawing:DrawText(aTextStru[i], {nLeft, nTop, nRight, nBottom},  DT_LEFT)
      nLeft:=nMarg+nTextDesc
      nRight:=nLeft+nTextProc
      oG:Drawing:DrawText(str(aPositionStru[i],7,2)+" %", {nLeft, nTop, nRight, nBottom},  DT_RIGHT)
   next

return NIL



//----------------------------------------------------------------------------------------------------//
METHOD FormEngineIndexes_OnLoad( Sender ) CLASS FormEngineIndexes
// query and list schema tables in a combobox   
   local i, cQuery
   
   if cEngine == "MYSQL"
      cQuery:="select left(column_name,32) "+;
              "from information_schema.columns "+;
              "where table_schema='"+cSchema+"' and table_name='"+cSchemaTable+"' "+;
              "order by column_name"
   elseif cEngine == "FB"
      cQuery:="select RDB$FIELD_NAME from RDB$RELATION_FIELDS "+;
              "where RDB$RELATION_NAME='"+cSchemaTable+"' "+;
              "order by RDB$FIELD_NAME"
   elseif cEngine == "MS"
      cQuery:="select left(column_name, 32) "+;
              "from information_schema.columns "+;
              "where table_name='"+cSchemaTable+"' "+;
              "order by column_name"
   endif
   oApp:Cursor:=nCursorCalc
   #ifdef XDB_SQLRDD
      ASize( aCursor, 0)   
      oSQL:Exec( cQuery, .f., .t., @aCursor, , ,nMaxCursor )
   #endif
   oApp:Cursor:=nCursorNormal
   for i:=1 to len(aCursor)
      ::BoxColumn:AddString(aCursor[i,1])
   next
   
   ::LabelTable:Caption:="Current Table: "+cSchemaTable
   ::PBar:SetRange({1, 50})
   ::PBar:SetStep(1)
   ::LabelChart:Visible:=.f.
   ::ChartBox:Visible:=.F.
   ::PBar:Visible:=.f.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCalculate_OnClick( Sender ) CLASS FormEngineIndexes
   local nSel:=::BoxColumn:GetCurSel(), cColumn, cQuery, cQueryTotal
   local cFile, cAlias, lOK, aGrand:={{}}
   local nVal, nTot, nRows, nGrand
   local n1:=0, n2:=0, n3:=0, n4:=0, p1, p2, p3, p4
   field nCounter
   
   if nSel < 1
      ::MessageBox( "Please choose a column from the list", "No column selected", MB_ICONEXCLAMATION)
      return Self
   endif
   cColumn:=alltrim(::BoxColumn:GetSelString())
// query total number of rows;
// receives a cursor with a row for each key group (duplicate or not)
// the row contains the number of keys in a group
   ::Disable()
   cFile:=myTempFileName( @nTemp, cTempPath, "dbf")
   cAlias:="c"+ltrim(str(++nAlias, 6))
   cQueryTotal:="select cast(count(*) as char(10)) as ngrand "+;
           "from "+cSchemaTable                
   cQuery:="select cast(count("+cColumn+") as char(10)) as nCounter "+;
           "from "+cSchemaTable+" "+;
           "where "+cColumn+" is not null "+;
           "group by "+cColumn+" "+;
           "order by "+cColumn
   lOK:=.f.
   ::Timer1:Start()
   ::PBar:Visible:=.t.
   try                        // query
      #ifdef XDB_SQLRDD
         oSQL:Exec( cQueryTotal, .t., .t., @aGrand, , , 1)
         oSQL:Exec( cQuery, .T., .t., , cFile, cAlias, , .t.)
      #endif      
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   if !lOK
      ::PBar:Visible:=.f.
      ::Timer1:Stop()
      ::Enable()
      return Self
   endif
// check totals received   
   nGrand:=if( len(aGrand)>0, val(alltrim(aGrand[1,1])), -1)
   select(cAlias)
   if Reccount()<1 .or. nGrand<1
      DBCloseArea()
      if nGrand < 1
         ::MessageBox( "The current table is empty", "", MB_ICONINFORMATION )
      else
         if Reccount()<1
            ::MessageBox( "The selected column contains only null values", "", MB_ICONINFORMATION )
         endif
      endif
      ::PBar:Visible:=.f.
      ::Timer1:Stop()
      ::Enable()
      return Self
   endif
// calculate statistics on cursor table   
   nTot:=0
   nRows:=0
   DBEval({|| nTot:=nTot+val(nCounter), nRows:=nRows+1})
   if nTot<1 .or. nRows<1
      ::MessageBox("A problem has been encountered while processing intermediate results", "", MB_ICONINFORMATION)
      DBCloseArea()
      deletefile(cFile)
      ::PBar:Visible:=.f.
      ::Timer1:Stop()
      ::Enable()
      return Self
   endif
   DBEval({|| nVal:=val(nCounter)*100.00/nTot, ;
                    n1:=n1+if(nVal<1.00,1,0), ;
                    n2:=n2+if(nVal<2.00,1,0), ;
                    n3:=n3+if(nVal<3.00,1,0), ;
                    n4:=n4+if(nVal>=3.00,1,0)})
   DBCloseArea()
   deletefile(cFile)
   
   n3:=n3-n2
   n2:=n2-n1
   p1:=round(n1*100.00/nRows, 2)
   p2:=round(n2*100.00/nRows, 2)
   p3:=round(n3*100.00/nRows, 2)
   p4:=round(n4*100.00/nRows, 2)
// display values   
   ::LabelGrand:Caption:=ltrim(str(nGrand,10))
   ::LabelNull:Caption:=ltrim(str(nGrand-nTot,10))
   ::LabelNullPr:Caption:=ltrim(str((nGrand-nTot)*100.0/nGrand ,6,2))
   ::LabelTotal:Caption:=ltrim(str(nTot,10))
   ::LabelDistinct:Caption:=ltrim(str(nRows,10))
   aStatistics[1]:=p1
   aStatistics[2]:=p2
   aStatistics[3]:=p3
   aStatistics[4]:=p4
   ::LabelChart:Visible:=.t.
   ::ChartBox:Visible:=.t.   
   ::PBar:Visible:=.f.
   ::Timer1:Stop()   
   ::Enable()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonClose_OnClick( Sender ) CLASS FormEngineIndexes
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxColumn_OnCBNSelEndOk( Sender ) CLASS FormEngineIndexes
// refresh edit zones   
   ::LabelTotal:Caption:=""
   ::LabelDistinct:Caption:=""
   ::LabelChart:Visible:=.f.
   ::ChartBox:Visible:=.f.
   ::LabelGrand:Caption:=""
   ::LabelNull:Caption:=""
   ::LabelNullPr:Caption:=""
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormEngineIndexes_OnDestroy( Sender ) CLASS FormEngineIndexes
   if lOpenTable
      select(oTable:Alias)
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Timer1_OnTimeOut( Sender ) CLASS FormEngineIndexes
   ::PBar:StepIt()
   Sender:Start()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ChartBox_OnPaint( Sender ) CLASS FormEngineIndexes
   StruBarChart( ::ChartBox, 4, aStatistics, { "Less then 1 %", "1 % - 2 %", "2 % - 3 %", "More then 3 %"}, ;
                 { nMarkerColorSec, nMarkerColorSec, nMarkerColor, nMarkerColor }, ;
                 nLight, nDark, .t.)
   return 0   
RETURN Self