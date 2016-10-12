GLOBAL oIni:=NIL, oApp:=NIL, oChild:=NIL, oErr:=Nil, oCursor:=NIL
GLOBAL oTable:=NIL, oVtable:=NIL
GLOBAL oStr:=NIL, oF:=NIL

GLOBAL nCursorCalc, nCursorNormal

GLOBAL aCursor:={{}}, aHelper:={{}}
GLOBAL cEngine:="NO", cSchema:="", cSchemaTable:=""
GLOBAL hConnection:=NIL, oSQL:=NIL
GLOBAL cDefaultEngine:="MYSQL", cCursorExport:="", lKeepDeleted:=.f.
GLOBAL aConString:={ "", "", "", "", "", "", "", "", "", "" }, aTemp:={ "", "", "", "" }
GLOBAL nConStringMax:=0, nConStringSel:=0
GLOBAL cQuerySource:=""

GLOBAL cTableName:="", cIndexName:="", cIniPath:="", cNewTable:=""
GLOBAL cExchangeName:="", cStruName:="", cTempPath:="", cTempFileName:=""
GLOBAL nCalcInit1:=0, nCalcInit2:=0, nCalcInit3:=0
GLOBAL nCalcNext:=1, cCalcExpr:="", cCalcFor:="", cCalcWhile:=""
GLOBAL nCountNext:=1, cCountFor:="", cCountWhile:=""

GLOBAL lExchange:=.t., lStruCreate:=.f., lOpenStru:=.f., lAppend:=.f.
GLOBAL lShowd:=.f., lOpenTable:=.f., lCount:=.f.
GLOBAL lOpenIndex:=.f., lOrderActive:=.f.
GLOBAL lView:=.f., lCols:=.f., lTop:=.f., lBottom:=.f.
GLOBAL lMarker:=.f., lMarkerSec:=.f.
GLOBAL cMarkerCondition:="", cMarkerConditionSec:=""
GLOBAL cTopCondition:="", cBottomCondition:="", cViewIndex:=""
GLOBAL aIndex:=NIL, aCols:={}, aView:={}, aMarker:={}, aStatistics:={0, 0, 0, 0}
GLOBAL aStru:={{}}, aF:={{}}

GLOBAL nShowdImage:=9, nHidedImage:=10
GLOBAL nAlias:=1000, nEpoch:=1940, nTemp:=1, nStructural:=1

GLOBAL lShared:=.t., lReadOnly:=.t., lSoftSeek:=.t., lStatistics:=.t., lCentury:=.t.
GLOBAL lShadowRow:=.t., lShowGrid:=.t., lShowHeaders:=.t., lConvertOEM:=.f.

GLOBAL nLight, nDark
GLOBAL cDriver, cDateFormat, cCodePage, cFontFace, cFontSize
GLOBAL cMaxCursor, cMaxCursorExt, nMaxCursor, nMaxCursorExt
GLOBAL cBackColor, cForeColor, cHighlightColor, cHighlightTextColor
GLOBAL cMarkerColor, cMarkerColorSec, cMarkerTextColor, cMarkerTextColorSec
GLOBAL nMarkerColor, nMarkerTextColor, nMarkerColorSec, nMarkerTextColorSec
GLOBAL nTestBack, nTestFore, nTestBackSec, nTestForeSec
GLOBAL hDriver, hDate, hCode, hMaxCursor, hMaxCursorExt
GLOBAL hFontFace, hFontSize, hBack, hFore
GLOBAL hHighlight, hHighlightText
GLOBAL hMarker, hMarkerText, hMarkerSec, hMarkerTextSec

#include "vxh.ch"
#include "Form1.xfm"
#include "ads.ch"
#include "dbinfo.ch"
//---------------------------------------- End of system code ----------------------------------------//



function myCheckName(cNameString, nLenNameString)
   local i, cS:=alltrim(cNameString), lOK:=.T., c
   if len(cS)<1 .or. len(cS)>nLenNameString
      return .f.
   endif
   if !IsAlpha(substr(cS, 1, 1))
      return .f.
   endif
   for i:=2 to len(cS)
      c:=substr(cS,i,1)
      if c == "_"
         loop
      endif
      if !IsAlNum(c)
         lOK:=.f.
         exit
      endif
   next
return lOK



function mySeconds(nTimeOld, nTimeNew)
   local nTimeElapsed:=nTimeNew-nTimeOld+if( nTimeNew<nTimeOld, 86400, 0)
return (nTimeElapsed)


function myGetColor(oBox, hBox)
// oBox - combobox, contains color names
// hBox - hash, contains color name & color code peers
   local nColor:=-1
   local cKey:=HGetKeyAt( hBox, oBox:GetCurSel())
   if len(cKey)==6.and.cKey=="custom"
      oApp:ColorPick:Show()      
      if oApp:ColorPick:Color <> NIL
         nColor:=oApp:ColorPick:Color
      endif
   else
      nColor:=hBox[cKey]
   endif
return nColor

function myIndexOpen()
// opens structural or additional .ntx, .cdx, .adi indexes 
// adds index info in table "aIndex" - contains all opened indexes

   local nOld:=len(aIndex), nl, npos
   local i, nKeys:=0, nAdsKeys:=0
   local cRDD:=right(hDriver[cDriver],3), cBag
   local cBagName, cKeyType, cKeyName, cUnique, cKeyExpr, cKeyForExpr, cWholeBagName
   local lOK:=.f., lCompIndex:=.f.

   nl:=len(cIndexName)
   npos:=RAt("\", cIndexName)
   cBag:=if( npos>0, Right(cIndexName, nl-npos), cIndexName)

// retains number of already opened indexes
   if cRDD=="ADT" .and. lOpenIndex
      nAdsKeys:=DBOrderInfo(DBOI_ORDERCOUNT)
   endif
   
   try
      oTable:SetIndex(cIndexName)
      lOK:=.t.
      lOpenIndex:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   
   if lOK .and. cRDD=="NTX"
      lCompIndex:=DBOrderInfo(DBOI_ISMULTITAG, cIndexName) // if the index is an order bag
      nKeys:=if(lCompIndex, DBOrderInfo(DBOI_ORDERCOUNT, cIndexName), 1)  // number of keys in the index
      if !lCompIndex   // simple .ntx
         cBagName:=cBag
         cKeyType:=DBOrderInfo(DBOI_KEYTYPE,, nOld)
         cKeyName:=""
         cUnique:=if(DBOrderInfo(DBOI_UNIQUE,, nOld),"yes", "no")
         cKeyExpr:=DBOrderInfo(DBOI_EXPRESSION,, nOld)
         cKeyForExpr:=DBOrderInfo(DBOI_CONDITION,, nOld)
         cWholeBagName:=cIndexName
         AADD(aIndex, {cBagName, cKeyType, cKeyName, cUnique, cKeyExpr, cKeyForExpr, cWholeBagName})
      endif
   elseif lOK .and. cRDD=="CDX"      
      lCompIndex:=.t.
      nKeys:=DBOrderInfo(DBOI_ORDERCOUNT, cIndexName)         
   elseif lOK .and. cRDD=="ADT"
      lCompIndex:=.t.
      nKeys:=DBOrderInfo(DBOI_ORDERCOUNT)-nAdsKeys      
   endif
   
   if lCompIndex  // extracting individual index info from orderbags
      for i:=nOld to nOld+nKeys-1
         cBagName:=cBag
         cKeyType:=DBOrderInfo(DBOI_KEYTYPE,, i)
         cKeyName:=DBOrderInfo(DBOI_NAME,, i)
         cUnique:=if(DBOrderInfo(DBOI_UNIQUE,, i), "yes", "no")
         cKeyExpr:=DBOrderInfo(DBOI_EXPRESSION,, i)
         cKeyForExpr:=DBOrderInfo(DBOI_CONDITION,, i)
         cWholeBagName:=cIndexName
         AADD(aIndex, {cBagName, cKeyType, cKeyName, cUnique, cKeyExpr, cKeyForExpr, cWholeBagName})
      next
   endif

   for i:=nOld+1 to nOld+nKeys   // refresh index combobox on the app form
      oApp:BoxIndex:AddString(aIndex[i,5])
   next
   
return NIL   



function myError(oWin, oError)  // general error message dialog
   oWin:messagebox(oError:SubSystem+" "+;
                str(oError:SubCode,7)+" "+;
                oError:Operation+" "+;
                oError:Description, ;
                "Error", MB_ICONHAND)   
return NIL



function myFillBox(oBox, cValue, hBox)
// oBox - combobox to fill with a list of strings
// cValue - string to select after filling
// hBox - hash with keys which are going to be listed in the combo
   local pos, i
   pos:=if( HHasKey(hBox,cValue), HGetPos(hBox,cValue), 1)
   for i:=1 to len(hBox)
      oBox:AddString(HGetKeyAt(hBox,i))
   next
   oBox:SetCurSel(pos)      
return NIL



function myTempFileName(nTempNr, cTemporaryPath, cTempExt)
// temporary file name generation - starts with a random number generated at .exe startup   
   local cTempFile
   do while .t.
      nTempNr++
      cTempFile:=cTemporaryPath+if(empty(cTemporaryPath), "", "\")+"T"+strzero(nTempNr,6)+"."+cTempExt
      if !file(cTempFile)
         exit
      endif
   enddo
return (cTempFile)



function myMemoCheck(cMemoContent, nSampleLength)
// for analyzing memo fields   
   local i, nc, lOK:=.t., nMax:=if( pcount()>1, nSampleLength, len(cMemoContent))
   for i:=1 to nMax
      nc:=asc(subs( cMemoContent, i, 1))
      if nc<30 .or. nc>175
         lOK:=.f.
         exit
      endif
   next
return lOK



//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnCreate( Sender ) CLASS Form1
   local nWidth, nHeight
// new task instance
   if ::Application:Running == .T.
         ::Application:Exit()
   endif
// screen resolution
   nWidth:=GetSystemMetrics( SM_CXSCREEN )
   nHeight:=GetSystemMetrics( SM_CYSCREEN )
   if nWidth<800 .or. nHeight<600
      MessageBox(,"xDB needs screen resolution  800 x 600  or higher !","",MB_ICONHAND|MB_OK)
      ::Application:Exit()
   endif
// mouse cursor shapes   
   nCursorCalc:=IDC_WAIT
   nCursorNormal:=IDC_ARROW
// set up starting number for temporary file name generation
   HB_RandomSeed(Seconds())
   nTemp:=HB_RandomInt(100000, 800000)
// path for temporary files
   cTempPath:=GetEnv("TEMP")
   if empty(cTempPath)
      cTempPath:=left(HB_ARGV(0), RAT( "\", HB_ARGV(0)))
   endif
// managing application windows's initial position
   Sender:aStartPos:={ 0, 0, 800, 600 }
   Sender:aInitPos:={ 0, 0, 800, 600 }
// .ini file instance   
   oIni:=IniFile( ::Application:Path + "\xDB.ini")
// restore application window position and size
   with object Sender
      :MyStartPos( Sender )
      :Left:=:aStartPos[1]
      :Top:=:aStartPos[2]
      :Width:=:aStartPos[3]
      :Height:=:aStartPos[4]
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   local i, nColor
   oApp:=Sender
   nLight:=RGB( 255, 255, 255)
   nDark:=RGB( 0, 0, 0)
// set up application window   
   ::Holder:Visible:=.f.
   ::PanelBar:BackColor:=nLight
// set Advantage Local Server as default for free Advantage tables
   AdsSetServerType(ADS_LOCAL_SERVER)
// number of connection strings for an engine to retain in the .ini file
   nConStringMax:=len(aConString)
   cIniPath:=CurDrive()+":"+DirName()
// creating hash tables for comboboxes used in settings  
   hDriver:={"DBase III"=>"DBFNTX", ;
             "DBase IV"=>"DBFCDX", ;
             "Visual Foxpro"=>"DBFFPT", ;
             "Free ADS table"=>"ADT"}
   hDate:={"USA"=>"USA", "BRITISH"=>"BRITISH", "JAPAN"=>"JAPAN"}
   hCode:={"English"=>"EN", "German"=>"DEWIN"}
   hMaxCursor:={"1000"=>1000, "2000"=>2000, "3000"=>3000, "4000"=>4000, "5000"=>5000}
   hMaxCursorExt:={"  5000"=>5000, " 10000"=>10000, ;
                   " 50000"=>50000, "100000"=>100000 }
   hFontFace:={"Tahoma"=>"Tahoma", ;
               "Arial"=>"Arial", ;
               "Verdana"=>"Verdana", ;
               "Courier New"=>"Courier New", ;
               "Georgia"=>"Georgia", ;
               "Lucida Console"=>"Lucida Console", ;
               "Microsoft Sans Serif"=>"Microsoft Sans Serif"}
   hFontSize:={"Small"=>8, "Medium"=>10, "Large"=>12}
   hBack:={" default"=>NIL, "Blue"=>RGB( 200, 255, 255), ;
           "Black"=>RGB(0,0,0), "White"=>RGB(255,255,255), ;
           "Green"=>RGB( 215, 235, 215), "Yellow"=>RGB( 255, 255, 200), ;
           "custom"=>RGB(255, 255, 255) }
   hFore:={" default"=>NIL, "Black"=>RGB(0,0,0), "White"=>RGB(255,255,255),;
           "Blue"=>RGB( 0, 96, 160),;
             "Green"=>RGB( 0, 128, 128), "Brown"=>RGB( 128, 64, 64),;
             "Red"=>RGB( 192, 0, 0), "custom"=>RGB(0, 0, 0) }
   hHighlight:={" default"=>NIL, "Green"=>RGB( 90, 180, 150), "Grey"=>RGB(128, 128, 128), ;
                 "Black"=>RGB(0,0,0), "White"=>RGB(255,255,255),;
                "Blue"=>RGB( 60, 170, 220), "Brown"=>RGB(160, 96, 32), "custom"=>RGB(128, 128, 128) }
   hHighlightText:={" default"=>NIL, "Black"=>RGB(0,0,0), "White"=>RGB(255,255,255), ;
                    "custom"=>RGB(255, 255, 255) }
   hMarker:={"Blue"=>RGB( 60, 170, 220), " Red (default)"=>RGB( 196, 64, 64),;
             "Yellow"=>RGB( 224, 128, 64), "custom"=>RGB( 196, 64, 64) }
   hMarkerText:={ " White (default)"=>RGB( 255, 255, 255), "Black"=>RGB( 0, 0, 0),;
                  "custom"=>RGB(255, 255, 255) }
   hMarkerSec:={" Blue (default)"=>RGB( 60, 170, 220), "Red"=>RGB( 196, 64, 64),;
             "Yellow"=>RGB( 224, 128, 64), "custom"=>RGB( 60, 170, 220) }
   hMarkerTextSec:={ " White (default)"=>RGB( 255, 255, 255), "Black"=>RGB( 0, 0, 0),;
                  "custom"=>RGB(255, 255, 255) }
// setting default values for hash data
   cDriver:=HGetKeyAt(hDriver,1)
   cDateFormat:=HGetKeyAt(hDate,3)
   cCodePage:=HGetKeyAt(hCode,1)
   cMaxCursor:=HGetKeyAt(hMaxCursor,1)
   cMaxCursorExt:=HGetKeyAt(hMaxCursorExt,1)
   cFontFace:=HGetKeyAt(hFontFace,6)
   cFontSize:=HGetKeyAt(hFontSize,3)
   cBackColor:=HGetKeyAt(hBack,1)
   cForeColor:=HGetKeyAt(hFore,1)
   cHighlightColor:=HGetKeyAt(hHighlight,1)
   cHighlightTextColor:=HGetKeyAt(hHighlightText,1)
   cMarkerColor:=HGetKeyAt(hMarker,1)
   cMarkerTextColor:=HGetKeyAt(hMarkerText,1)
   cMarkerColorSec:=HGetKeyAt(hMarkerSec,1)
   cMarkerTextColorSec:=HGetKeyAt(hMarkerTextSec,1)
// initial write to the .ini file
   if file(oIni:Name) == .f.
       ::myWriteSettings(Self)
   endif     
// read in last settings from the .ini file      
   cDriver:=oIni:ReadString("Data", "Driver", HGetKeyAt(hDriver,1))
   cDriver:=if( HHasKey(hDriver,cDriver), cDriver, HGetKeyAt(hDriver,1))
   lShared:=oIni:ReadLogical("Data", "Shared", "Yes")
   lReadOnly:=oIni:ReadLogical("Data", "ReadOnly", "Yes")
   lSoftSeek:=oIni:ReadLogical("Data", "SoftSeek", "Yes")
   lStatistics:=oIni:ReadLogical("Data", "Statistics", "Yes")
   cDateFormat:=oIni:ReadString("Data", "DateFormat", HGetKeyAt(hDate,3))
   cDateFormat:=if( HHasKey(hDate,cDateFormat), cDateFormat, HGetKeyAt(hDate,3))
   nEpoch:=oIni:ReadNumber("Data", "Epoch", 1940)
   nEpoch:=if( nEpoch<100.or.nEpoch>2900, 1940, nEpoch)
   lCentury:=oIni:ReadLogical("Data", "Century", "Yes")
   cCodePage:=oIni:ReadString("Data", "CodePage", HGetKeyAt(hCode,1))
   cCodePage:=if( HHasKey(hCode,cCodePage), cCodePage, HGetKeyAt(hCode,1))
   lConvertOEM:=oIni:ReadLogical("Data", "ConvertOEM", "No")
   cDefaultEngine:=oIni:ReadString("Data", "DefaultEngine", "NO") // SQL engine
   lKeepDeleted:=oIni:ReadLogical("Data", "KeepDeleted", "No")  // used at connecting to a SQL engine

// maximum number of memorytable rows - limit for SQLRDD query results
   cMaxCursor:=oIni:ReadString("Data", "MaxCursor", HGetKeyAt(hMaxCursor,1))
   cMaxCursor:=if( HHasKey(hMaxCursor,cMaxCursor), cMaxCursor, HGetKeyAt(hMaxCursor,1))   
// maximum number of .dbf rows - limit for SQLRDD query results
   cMaxCursorExt:=oIni:ReadString("Data", "MaxCursorExt", HGetKeyAt(hMaxCursorExt,1))
   cMaxCursorExt:=if( HHasKey(hMaxCursorExt,cMaxCursorExt), cMaxCursorExt, HGetKeyAt(hMaxCursorExt,1))
// grid appearance   
   cFontFace:=oIni:ReadString("Appearance", "FontFace", HGetKeyAt(hFontFace,6))
   cFontFace:=if( HHasKey(hFontFace,cFontFace), cFontFace, HGetKeyAt(hFontFace,6))
   cFontSize:=oIni:ReadString("Appearance", "FontSize", HGetKeyAt(hFontSize,3))
   cFontSize:=if( HHasKey(hFontSize,cFontSize), cFontSize, HGetKeyAt(hFontSize,3))
   lShadowRow:=oIni:ReadLogical("Appearance", "ShadowRow", "Yes")
   lShowGrid:=oIni:ReadLogical("Appearance", "ShowGrid", "Yes")
   lShowHeaders:=oIni:ReadLogical("Appearance", "ShowHeaders", "Yes")   
   cBackColor:=oIni:ReadString("Appearance", "BackColor", HGetKeyAt(hBack,1))
   cBackColor:=if( HHasKey(hBack,cBackColor), cBackColor, HGetKeyAt(hBack,1))
   hBack["custom"]:=oIni:ReadNumber("Appearance", "BackColorCustom", RGB(255, 255, 255))   
   cForeColor:=oIni:ReadString("Appearance", "ForeColor", HGetKeyAt(hFore,1))
   cForeColor:=if( HHasKey(hFore,cForeColor), cForeColor, HGetKeyAt(hFore,1))
   hFore["custom"]:=oIni:ReadNumber("Appearance", "ForeColorCustom", RGB(0, 0, 0))
   cHighlightColor:=oIni:ReadString("Appearance", "HighlightColor", HGetKeyAt(hHighlight,1))
   cHighlightColor:=if( HHasKey(hHighlight,cHighlightColor), cHighlightColor, HGetKeyAt(hHighlight,1))
   hHighlight["custom"]:=oIni:ReadNumber("Appearance", "HighlightColorCustom", RGB(192, 192, 192))
   cHighlightTextColor:=oIni:ReadString("Appearance", "HighlightTextColor", HGetKeyAt(hHighlightText,1))
   cHighlightTextColor:=if( HHasKey(hHighlightText,cHighlightTextColor), cHighlightTextColor, HGetKeyAt(hHighlightText,1))
   hHighlightText["custom"]:=oIni:ReadNumber("Appearance", "HighlightTextColorCustom", RGB(255, 255, 255))
// first marker colors   
   cMarkerColor:=oIni:ReadString("Appearance", "MarkerColor", HGetKeyAt(hMarker,1))
   cMarkerColor:=if( HHasKey(hMarker,cMarkerColor), cMarkerColor, HGetKeyAt(hMarker,1))
   hMarker["custom"]:=oIni:ReadNumber("Appearance", "MarkerColorCustom", RGB(128, 128, 128))   
   cMarkerTextColor:=oIni:ReadString("Appearance", "MarkerTextColor", HGetKeyAt(hMarkerText,1))
   cMarkerTextColor:=if( HHasKey(hMarkerText,cMarkerTextColor), cMarkerTextColor, HGetKeyAt(hMarkerText,1))
   hMarkerText["custom"]:=oIni:ReadNumber("Appearance", "MarkerTextColorCustom", RGB(255, 255, 255))
// second marker colors   
   cMarkerColorSec:=oIni:ReadString("Appearance", "MarkerColorSec", HGetKeyAt(hMarkerSec,1))
   cMarkerColorSec:=if( HHasKey(hMarkerSec,cMarkerColorSec), cMarkerColorSec, HGetKeyAt(hMarkerSec,1))
   hMarkerSec["custom"]:=oIni:ReadNumber("Appearance", "MarkerColorSecCustom", RGB(128, 128, 128))   
   cMarkerTextColorSec:=oIni:ReadString("Appearance", "MarkerTextColorSec", HGetKeyAt(hMarkerTextSec,1))
   cMarkerTextColorSec:=if( HHasKey(hMarkerTextSec,cMarkerTextColorSec), cMarkerTextColorSec, HGetKeyAt(hMarkerTextSec,1))
   hMarkerTextSec["custom"]:=oIni:ReadNumber("Appearance", "MarkerTextColorSecCustom", RGB(255, 255, 255))

// applying them
   ::myApplySettings(Self)  

// setting the SET DELETED contextmenu and coolmenu items
   if lShowd
      ::MenuShowd:Check()
      ::ContextShowd:Check()
      ::MenuHided:Uncheck()
      ::ContextHided:Uncheck()
      set deleted off
   else
      ::MenuShowd:Uncheck()
      ::ContextShowd:Uncheck()
      ::MenuHided:Check()
      ::ContextHided:Check()
      set deleted on
   endif
// enabling only menus and toolbuttons permitted at program startup
   ::ContextConnect:Enabled:=.t.
   ::ContextInfo:Enabled:=.f.
   ::ContextDisconnect:Enabled:=.f.
   ::ToolSettings:Enabled:=.t.
   #ifdef XDB_SQLRDD
      ::ToolEngine:Enabled:=.t.
   #endif
   ::ToolNew:Enabled:=.t.
   ::ToolOpen:Enabled:=.t.
   ::ToolShowd:ImageIndex:=if(lShowd, nShowdImage, nHidedImage)   
   ::MenuNewTable:Enabled:=.t.
   ::MenuOpenTable:Enabled:=.t.
   ::MenuSettings:Enabled:=.t.

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuNewTable_OnClick( Sender ) CLASS Form1
   FormCreateTable( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuOpenTable_OnClick( Sender ) CLASS Form1
   local i, nl
   
   ChildGrid( ::this )
   if !lOpenTable
      ::Cursor:=nCursorNormal
      return Self
   endif
// setting the index box
   ::BoxIndex:SetCurSel(1)
// set up view and marker stuff; 
// aF is for Table FIELDS update
   nl:=len(oTable:Structure)
   oF:=oTable:Fields
   ASize(aCols, nl)
   ASize(aView, nl)
   ASize(aMarker, nl)
   ASize(aF, nl)
   for i:=1 to nl
      aCols[i]:=oTable:Structure[i,1]
      aView[i]:=.f.
      aMarker[i]:=.f.
      aF[i]:={ oTable:Structure[i,1], oTable:Structure[i,2], ;
               oTable:Structure[i,3], oTable:Structure[i,4], "" }
   next

// set up enabled menus and toolbuttons enabled in the new context
   ::MenuOpenTable:Enabled:=.f.
   ::MenuCloseTable:Enabled:=.t.
   ::MenuExchange:Enabled:=.t.
   ::MenuView:Enabled:=.t.
   ::MenuProperties:Enabled=.t.
   ::MenuSettings:Enabled:=.f.
   ::MenuSaveStru:Enabled:=.t.
   ::MenuSaveExStru:Enabled:=.t.
   ::MenuAppend:Enabled:=if( lReadOnly, .f., .t.)
   ::MenuUpdate:Enabled:=if( lReadOnly, .f., .t.)
   ::MenuDelete:Enabled:=if( lReadOnly, .f., .t.)
   ::MenuRecall:Enabled:=if( lReadOnly, .f., .t.)
   ::MenuDBEval:Enabled:=.t.
   ::MenuCount:Enabled:=.t.
   ::ButtonAppend:Enabled:=if( lReadOnly, .f., .t.)
   ::ButtonUpdate:Enabled:=if( lReadOnly, .f., .t.)
   ::ButtonDelete:Enabled:=if( lReadOnly, .f., .t.)
   ::ButtonRecall:Enabled:=if( lReadOnly, .f., .t.)
   ::MenuMarker:Enabled=.t.
   if !lReadOnly .and. hDriver[cDriver] <> "DBFFPT"
      ::MenuIndex:Enabled=.t.
      ::ContextIndex:Enabled:=.t.
   endif
   ::MenuImport:Enabled:=if( lReadOnly, .f., .t.)
   ::ContextAppendFrom:Enabled:=if( lReadOnly, .f., .t.)
   if hDriver[cDriver] <> "DBFFPT"
      ::MenuOpenIndex:Enabled=.t.
      ::ToolOpeni:Enabled:=.t.
   endif
   ::ToolSettings:Enabled:=.f.
   ::ToolOpen:Enabled:=.f.
   ::ToolClose:Enabled:=.t.
   ::ToolProperties:Enabled:=.t.
   ::ToolView:Enabled:=.t.
   ::ToolMarker:Enabled:=.t.
   ::ButtonSeek:Enabled:=if(hDriver[cDriver] <> "DBFFPT", .t., .f.)
   ::PanelBar:BackColor:=NIL
   ::Holder:Visible:=.t.
   ::Cursor:=nCursorNormal
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuCloseTable_OnClick( Sender ) CLASS Form1
   local i, cName, lOK:=.t.
   ::Cursor:=nCursorCalc   
// closing the table and initializing table specific variables
   try
      oChild:Close()
      lOpenIndex:=.f.
      lOrderActive:=.f.
      lMarker:=.f.
      lMarkerSec:=.f.
      lCols:=.f.
      lTop:=.f.
      lBottom:=.f.
      lView:=.f.
      ::BoxIndex:ResetContent()
      ::EditSeek:Caption:=""
      ::EditSkip:Caption:=""
      cMarkerCondition:=""
      cMarkerConditionSec:=""
      cTopCondition:=""
      cBottomCondition:=""
      aIndex:=NIL
      ASize(aCols, 0)
      ASize(aView, 0)
      ASize(aMarker, 0)
      ASize(aF, 0)
      oF:=NIL
   catch oErr
      lOK:=.f.
      myError(oApp, oErr)
   end
   
   if lOpenTable==.t. .or. lOK==.f. // close failure or other problem
      ::Application:Exit()
      return Self
   endif

// set up menus and toolbuttons in the new context
   ::Holder:Visible:=.f.
   ::PanelBar:BackColor:=nLight
   ::MenuOpenTable:Enabled:=.t.
   ::MenuCloseTable:Enabled:=.f.
   ::MenuExchange:Enabled:=.f.
   ::MenuView:Enabled:=.f.
   ::MenuProperties:Enabled=.f.
   ::MenuSettings:Enabled:=.t.
   ::MenuSaveStru:Enabled:=.f.
   ::MenuSaveExStru:Enabled:=.f.
   ::MenuAppend:Enabled=.f.
   ::MenuUpdate:Enabled=.f.
   ::MenuDelete:Enabled=.f.
   ::MenuRecall:Enabled=.f.
   ::MenuDBEval:Enabled=.f.
   ::MenuCount:Enabled:=.f.
   ::MenuOpenIndex:Enabled=.f.
   ::MenuIndex:Enabled:=.f.
   ::MenuMarker:Enabled=.f.   
   ::ToolSettings:Enabled:=.t.
   ::ToolOpen:Enabled:=.t.
   ::ToolOpeni:Enabled:=.f.
   ::ToolClose:Enabled:=.f.
   ::ToolProperties:Enabled:=.f.
   ::ToolView:Enabled:=.f.
   ::ToolMarker:Enabled:=.f. 
   ::Cursor:=nCursorNormal  
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuView_OnClick( Sender ) CLASS Form1
   if !lShared
      ::MessageBox( "VIEW needs to open free tables with shared access mode", "Current table opened with exclusive access mode", MB_ICONEXCLAMATION)
      return Self
   endif
   FormView( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuProperties_OnClick( Sender ) CLASS Form1
   FormProperties( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuSettings_OnClick( Sender ) CLASS Form1
   FormSettings( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuAppend_OnClick( Sender ) CLASS Form1
   lAppend:=.t.
   FormUpdateRecord( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuUpdate_OnClick( Sender ) CLASS Form1
   if oTable:Deleted()
      ::MessageBox( "It's not permitted to update deleted rows", "Warning", MB_ICONEXCLAMATION)
      return Self
   endif
   FormUpdateRecord( ::this )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuDelete_OnClick( Sender ) CLASS Form1
// delete free table's current row   
   if oTable:Deleted()
      return Self
   endif
   ::Cursor:=nCursorCalc
   ::Disable()
   try
      if lShared
         oTable:RecLock()
      endif
      oTable:Delete()
      if lShared
         oTable:UnLock()
      endif
   catch oErr
      myError(oApp, oErr)
   end
   oChild:Grid:Update()
   ::Enable()
   ::Cursor:=nCursorNormal
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuRecall_OnClick( Sender ) CLASS Form1
// recall free table's current row   
   if !oTable:Deleted()
      return Self
   endif   
   ::Cursor:=nCursorCalc
   ::Disable()
   try
      if lShared
         oTable:RecLock()
      endif
      oTable:Recall()
      if lShared
         oTable:UnLock()
      endif
   catch oErr
      myError(oApp, oErr)
   end
   oChild:Grid:Update()
   ::Enable()
   ::Cursor:=nCursorNormal
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuHided_OnClick( Sender ) CLASS Form1
// apply "set deleted on"
   ::Disable()     
   ::MenuHided:Check()
   ::ContextHided:Check()
   ::MenuShowd:Uncheck()
   ::ContextShowd:Uncheck()
   ::ToolShowd:ImageIndex:=nHidedImage
   lShowd:=.f.
   set deleted on
   if lOpenTable
      ::Disable()
      oChild:Grid:Update()
      ::Enable()
   endif      
   ::Enable()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuShowd_OnClick( Sender ) CLASS Form1
// apply "set deleted off"
   ::Disable()
   ::MenuHided:Uncheck()
   ::ContextHided:Uncheck()
   ::MenuShowd:Check()
   ::ContextShowd:Check()
   ::ToolShowd:ImageIndex:=nShowdImage
   lShowd:=.t.
   set deleted off
   if lOpenTable
      ::Disable()
      oChild:Grid:Update()
      ::Enable()
   endif      
   ::Enable()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuOpenIndex_OnClick( Sender ) CLASS Form1
// open additional indexes for the current free table   
   local i, cRDD:=right(hDriver[cDriver],3), cExt, lOK:=.t.
   if cRDD=="NTX"
      cEXT:=cRDD
   elseif cRDD=="ADT"
      cEXT:="ADI"
   else
      cEXT:="CDX"
   endif
   ::OpenFile:DefaultExt:=cEXT
   if cExt == "NTX"
      ::OPenFile:Filter:="DBase III (*.NTX)|*.NTX|All files ( *.* )|*.*"
   elseif cEXT == "CDX"
      ::OPenFile:Filter:="DBase IV (*.CDX)|*.CDX|All files ( *.* )|*.*"
   else
      ::OPenFile:Filter:="Advantage native (*.ADI)|*.ADI|All files ( *.* )|*.*"
   endif
   
// getting the name of the index to open   
   ::OpenFile:InitialDirectory:=cIniPath
   ::OPenFile:Title:="Open Index"
   ::OpenFile:FileName:=""
   ::OpenFile:Show()
   if empty(::OpenFile:FileName)
      return Self
   endif
   for i:=2 to len(aIndex)
      if upper(::OpenFile:FileName) == upper(aIndex[i,7])
         lOK:=.f.
      endif
   next
   if !lOK
      ::MessageBox("The selected index file is already opened","Warning",MB_ICONEXCLAMATION)
      return Self
   endif
   
   ::Disable()   
   
   cIndexName:=::OpenFile:FileName
   myIndexOpen()
   oChild:Grid:Update()
   
   ::Enable()   
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuMarker_OnClick( Sender ) CLASS Form1   
   FormMarker( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuVisit_OnClick( Sender ) CLASS Form1
   ShellExecute(NIL, "Open", "http://www.xHarbour.com",,, SW_SHOW)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuBuy_OnClick( Sender ) CLASS Form1
   ShellExecute(NIL, "Open", "http://www.xHarbour.com/Order/",,, SW_SHOW)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuAbout_OnClick( Sender ) CLASS Form1
   ::MessageBox("This Visual xHarbour sample project was created by xHarbour.com Inc."+InetCRLF()+;
                "The user is free to change the source code of this sample project to his/her desire.",;
                "xDB "+::Application:Version+" | About", MB_ICONINFORMATION)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolNew_OnClick( Sender ) CLASS Form1
   ::MenuNewTable_OnClick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolOpen_OnClick( Sender ) CLASS Form1
   ::MenuOpenTable_OnClick(Self)         
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolOpeni_OnClick( Sender ) CLASS Form1
   ::MenuOpenIndex_OnClick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolClose_OnClick( Sender ) CLASS Form1
   ::MenuCloseTable_OnClick(Self)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolProperties_OnClick( Sender ) CLASS Form1
   ::MenuProperties_OnClick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolView_OnClick( Sender ) CLASS Form1
   ::MenuView_OnClick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolMarker_OnClick( Sender ) CLASS Form1
   ::MenuMarker_OnClick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextHided_OnClick( Sender ) CLASS Form1
   ::MenuHided_OnClick(Self)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextShowd_OnClick( Sender ) CLASS Form1
   ::MenuShowd_OnClick(Self)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonUp_OnClick( Sender ) CLASS Form1
// "go top" - current free table
   ::Cursor:=nCursorCalc
   ::Disable()   
   oTable:GoTop()
   oChild:Grid:Update()
   ::Enable()
   ::Cursor:=nCursorNormal
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDown_OnClick( Sender ) CLASS Form1
// "go bottom" - current free table   
   ::Cursor:=nCursorCalc
   ::Disable()   
   oTable:GoBottom()
   oChild:Grid:Update()
   ::Enable()
   ::Cursor:=nCursorNormal
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnClose( Sender ) CLASS Form1
// when closing the application window
   local nResponse:=-1
   if lOpenTable
      nResponse:=::MessageBox("Are you sure you want to close the Table and to exit the application ?","",MB_YESNO|MB_ICONQUESTION)
      if nResponse == 7
         return 1
      endif
      ::Cursor:=nCursorCalc
      try
         oChild:Grid:DataSource:Close()
         oTable:Close()
      catch oErr
         myError(oApp, oErr)
      end
   endif
   ::myEndPos( Sender )
   ::Cursor:=nCursorNormal
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myApplySettings( Sender ) CLASS Form1
// after reading the .ini file and after modifying general settings
// general settings aren't dependent on the currently opened table
   ::Disable()
   
   set softseek (lSoftSeek)
   set epoch to nEpoch
   set century (lCentury)
   if upper(cDateFormat) == "BRITISH"
      set date BRITISH
   elseif upper(cDateFormat) == "JAPAN"
      set date JAPAN
   else
      set date USA
   endif
   HB_SetCodePage(cCodePage)
   
   nMarkerColor:=HGet( hMarker, cMarkerColor)
   nMarkerTextColor:=HGet( hMarkerText, cMarkerTextColor)
   nMarkerColorSec:=HGet( hMarkerSec, cMarkerColorSec)
   nMarkerTextColorSec:=HGet( hMarkerTextSec, cMarkerTextColorSec)
   
   nMaxCursor:=HGet( hMaxCursor, cMaxCursor)
   nMaxCursorExt:=HGet( hMaxCursorExt, cMaxCursorExt)
   
   ::Enable()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myWriteSettings( Sender ) CLASS Form1
// writing in the .ini file - when creating or modifying settings
// SQL connection string settings are managed separately in FormConnect

   ::Disable()
   
   with object oIni
      
      :WriteString("Data", "Driver", cDriver)
      :WriteLogical("Data", "Shared", lShared)
      :WriteLogical("Data", "ReadOnly", lReadOnly)
      :WriteLogical("Data", "SoftSeek", lSoftSeek)

      :WriteLogical("Data", "Statistics", lStatistics)
      :WriteString("Data", "DateFormat", cDateFormat)
      :WriteNumber("Data", "Epoch", nEpoch)
      :WriteLogical("Data", "Century", lCentury)
      :WriteString("Data", "CodePage", cCodePage)
      :WriteLogical("Data", "ConvertOEM", lConvertOEM)      
      :WriteString("Data", "MaxCursor", cMaxCursor)
      :WriteString("Data", "MaxCursorExt", cMaxCursorExt)
      :WriteString("Data", "DefaultEngine", cDefaultEngine)
      :WriteLogical("Data", "KeepDeleted", lKeepDeleted)
   
      :WriteString("Appearance", "FontFace", cFontFace)
      :WriteString("Appearance", "FontSize", cFontSize)
      :WriteLogical("Appearance", "ShadowRow", lShadowRow)
      :WriteLogical("Appearance", "ShowGrid", lShowGrid)
      :WriteLogical("Appearance", "ShowHeaders", lShowHeaders)
      :WriteString("Appearance", "BackColor", cBackColor)
      :WriteString("Appearance", "ForeColor", cForeColor) 
      :WriteString("Appearance", "HighlightColor", cHighlightColor)
      :WriteString("Appearance", "HighlightTextColor", cHighlightTextColor)
      :WriteString("Appearance", "MarkerColor", cMarkerColor)   
      :WriteString("Appearance", "MarkerTextColor", cMarkerTextColor)   
      :WriteString("Appearance", "MarkerColorSec", cMarkerColorSec)   
      :WriteString("Appearance", "MarkerTextColorSec", cMarkerTextColorSec)   
   
      :WriteNumber("Appearance", "BackColorCustom", HGet( hBack, "custom"))
      :WriteNumber("Appearance", "ForeColorCustom", HGet( hFore, "custom"))
      :WriteNumber("Appearance", "HighlightColorCustom", HGet( hHighlight, "custom"))
      :WriteNumber("Appearance", "HighlightTextColorCustom", HGet( hHighlightText, "custom"))
      :WriteNumber("Appearance", "MarkerColorCustom", HGet( hMarker, "custom"))   
      :WriteNumber("Appearance", "MarkerTextColorCustom", HGet( hMarkerText, "custom"))   
      :WriteNumber("Appearance", "MarkerColorSecCustom", HGet( hMarkerSec, "custom"))   
      :WriteNumber("Appearance", "MarkerTextColorSecCustom", HGet( hMarkerTextSec, "custom"))   
   
      :WriteString("MySQL", "Engine", "MYSQL")
      :WriteString("FireBird", "Engine", "FB")

   end
   
   ::Enable()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonSeek_OnClick( Sender ) CLASS Form1
   local cExpr:=alltrim(::EditSeek:Caption), cType, lOK:=.f.
   local nIndex:=::BoxIndex:GetCurSel()
// checking the SEEK expression   
   if nIndex < 2
      ::MessageBox("Please select an active index order before using the SEEK functionality","Warning", MB_ICONEXCLAMATION)
      return Self
   endif
   if len(cExpr) > 255
      ::MessageBox("The current SEEK expression length "+ltrim(str(len(cExpr)))+" exceeds 255 characters","Expression too long", MB_ICONEXCLAMATION)
      return Self
   endif
   try
      cType:=valtype(&(cExpr))
      lOK:=.t.
   catch oErr
      ::MessageBox("Please type in a valid key expression in the editing zone","Evaluation error", MB_ICONEXCLAMATION)
      ::EditSeek:Caption:=""      
   end
   if !lOK
      return Self
   endif  
   if aIndex[nIndex,2] <> cType
      ::MessageBox("The expression typed in the editing zone has to be evaluated to type * "+aIndex[nIndex ,2]+"' *","Warning", MB_ICONEXCLAMATION)
      return Self
   endif
   
   ::Disable()
// executing properly SEEK   
   lOK:=.f.
   try
      oTable:Seek(&(cExpr), lSoftSeek)
      lOK:=.t.      
   catch oErr
      ::MessageBox("Please type in a valid key expression in the editing zone","Seek error", MB_ICONEXCLAMATION)
      ::EditSeek:Caption:=""      
   end   
   if lOK
      oChild:Grid:Update()
   endif
   
   ::Enable()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonSkip_OnClick( Sender ) CLASS Form1
   local cExpr:=alltrim(::EditSkip:Caption), cType:="", lOK:=.f., nPos
// checking the SKIP expression   
   if len(cExpr) > 255
      ::MessageBox("The current SKIP expression length "+ltrim(str(len(cExpr)))+" exceeds 255 characters","Expression too long", MB_ICONEXCLAMATION)
      return Self
   endif
   try
      cType:=valtype(&(cExpr))
      lOK:=.t.
   catch oErr
      ::MessageBox("Please type in a valid numeric expression in the editing zone","Evaluation error", MB_ICONEXCLAMATION)
      ::EditSkip:Caption:=""
   end
   if !lOK
      return Self
   endif
   if cType<>"N"
      ::MessageBox("Please type in a valid numeric expression in the editing zone","Evaluation error", MB_ICONEXCLAMATION)
      ::EditSkip:Caption:=""
      return Self
   endif
   
   ::Disable()
// executing properly SKIP   
   nPos:=&(cExpr)
   oTable:Skip(nPos)
   oChild:Grid:Update()
   
   ::Enable()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonAppend_OnClick( Sender ) CLASS Form1
   ::MenuAppend_Onclick(Self)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonUpdate_OnClick( Sender ) CLASS Form1
   ::MenuUpdate_Onclick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDelete_OnClick( Sender ) CLASS Form1
   ::MenuDelete_Onclick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonRecall_OnClick( Sender ) CLASS Form1
   ::MenuRecall_Onclick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxIndex_OnCBNSelEndOk( Sender ) CLASS Form1
// changing the active index and displaying order of the current free table
   local nInd:=Sender:GetCurSel()
   if len(aIndex)==1 .or. (nInd==1.and.lOrderActive==.f.)
      return Self
   endif   
   ::Disable()
   if nInd==1 .and. lOrderActive==.t.
      oTable:SetOrder(0)
      lOrderActive:=.f.
   else
      oTable:SetOrder(nInd-1)
      lOrderActive:=.t.
   endif
   oChild:Grid:Update()
   ::Enable()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuExit_OnClick( Sender ) CLASS Form1
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuIndex_OnClick( Sender ) CLASS Form1
   FormAddIndex( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuExport_OnClick( Sender ) CLASS Form1
   lExchange:=.t.
   FormExchange( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuImport_OnClick( Sender ) CLASS Form1
   lExchange:=.f.
   FormExchange( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuSaveStru_OnClick( Sender ) CLASS Form1
// creates an empty table with the current free table's structure    
   local npos, lOK:=.f.
   local cRDD:=right(hDriver[cDriver],3)
   local cFilter:=if( cRDD=="ADT", "Advantage tables (*.ADT)|*.ADT", ;
                      "DBase tables (*.DBF)|*.DBF")+;
                      "|All files ( *.* )|*.*"
   local aTableStruct:=DBStruct()
// get name of the new, empty file
   with object oApp:SaveFile
      :DefaultExt:=if( cRDD=="ADT", "ADT", "DBF")
      :Filter:=cFilter
      :InitialDirectory:=cIniPath
      :Title:="Select location and name for the new empty table"
      :FileName:=""
      :Show()
      cStruName:=:FileName   
   end
   if empty(cStruName)
      return Self
   endif
// create it
   ::Disable()
   npos:=RAt("\", cStruName)
   cIniPath:=if( npos>0, Left(cStruName, npos-1), "")
   try
      DBCreate( cStruName, aTableStruct, hDriver[cDriver], NIL)
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end   
   ::Enable()
   
   if lOK
      ::MessageBox("The new empty table has been created successfully",;
                   "", MB_ICONINFORMATION)
   endif
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuSaveExStru_OnClick( Sender ) CLASS Form1
// save current free table's extended structure ( in .dbf file )
   local npos, lOK:=.f., lSuccess:=.f.
   local cRDD:=right(hDriver[cDriver],3)
// get the new .dbf  file name
   with object oApp:SaveFile
      :DefaultExt:="DBF"
      :Filter:="DBase Tables (*.DBF)|*.DBF|All files ( *.* )|*.*"
      :InitialDirectory:=cIniPath
      :Title:="Select location and name for the new extended structure file"
      :FileName:=""
      :Show()
      cStruName:=:FileName   
   end
   if empty(cStruName)
      return Self
   endif
// create it
   ::Disable()
   npos:=RAt("\", cStruName)
   cIniPath:=if( npos>0, Left(cStruName, npos-1), "")
   try
      lSuccess:=DBCopyExtStruct(cStruName)
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end   
   ::Enable()
   
   if lOK .and. lSuccess
      ::MessageBox("The new structure file  has been created successfully",;
                   "", MB_ICONINFORMATION)
   elseif lOK
      ::MessageBox("The new structure file hasn't been created due to a problem",;
                   "Warning", MB_ICONEXCLAMATION)      
   endif
      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuCreateExStru_OnClick( Sender ) CLASS Form1
   lStruCreate:=.t.
   FormStructure( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuUpdateExStru_OnClick( Sender ) CLASS Form1
   lStruCreate:=.f.
   FormStructure( ::this )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextCopyTo_OnClick( Sender ) CLASS Form1
   ::MenuExport_OnClick(Self)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextAppendFrom_OnClick( Sender ) CLASS Form1
   ::MenuImport_OnClick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextCopyFile_OnClick( Sender ) CLASS Form1
   ::MenuSaveStru_OnClick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextCopyStru_OnClick( Sender ) CLASS Form1
   ::MenuSaveExStru_OnClick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextShowStru_OnClick( Sender ) CLASS Form1
   FormShowStructure( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextIndex_OnClick( Sender ) CLASS Form1
   FormAddIndex( ::this )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolSettings_OnClick( Sender ) CLASS Form1
   ::MenuSettings_OnClick(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDBEval_OnClick( Sender ) CLASS Form1
   FormDBEval( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextConnect_OnClick( Sender ) CLASS Form1
// connect to MySQL or FireBird server using their native interface via SQLRDD
   FormConnect( ::this )
   if cEngine == "NO"
      return Self
   endif
// set up menus according to the new context   
   ::ToolImport:Enabled:=.t.
   ::ContextConnect:Enabled:=.f.
   ::ContextInfo:Enabled:=.t.
   ::ContextDisconnect:Enabled:=.t.
   ::ToolQuery:Enabled:=.t.
   ::ToolImport:Enabled:=.t.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextInfo_OnClick( Sender ) CLASS Form1
   FormEngineInfo( ::this )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextDisconnect_OnClick( Sender ) CLASS Form1   
// disconnect MySQL or FireBird   
   local lOK:=.f.
   if cEngine == "NO"
      return Self
   endif
   ::Cursor:=nCursorCalc
   try
      #ifdef XDB_SQLRDD
         try
            SR_EndConnection(hConnection)
         catch oErr
            myError( oApp, oErr)
         end
      #endif
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   ::Cursor:=nCursorNormal
   if !lOK        // disconnect fails
      ::Application:Exit()
   endif   
   cEngine:="NO"
// set up menus according to the new context   
   ::ContextConnect:Enabled:=.t.
   ::ContextInfo:Enabled:=.f.
   ::ContextDisconnect:Enabled:=.f.
   ::ToolImport:Enabled:=.f.
   ::ToolQuery:Enabled:=.f.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuDBEval_OnClick( Sender ) CLASS Form1
   FormDBEval( ::this )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuCount_OnClick( Sender ) CLASS Form1
   lCount:=.t.
   FormDBEval( ::this )
   lCount:=.f.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCount_OnClick( Sender ) CLASS Form1
   lCount:=.t.
   FormDBEval( ::this )
   lCount:=.f.   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolQuery_OnClick( Sender ) CLASS Form1
   FormInstantQuery( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolImport_OnClick( Sender ) CLASS Form1
   if lOpenTable .and. !lShared
      ::MessageBox( "IMPORT needs to open free tables with shared access mode", "Current table opened with exclusive access mode", MB_ICONEXCLAMATION)
      return Self
   endif
   FormImportInEngine( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myStartPos( Sender ) CLASS Form1
   local i, nPos, nVal, cVal, aVal
   local aPos:=AClone( ::aInitPos )
   ::aStartPos[3]:=GetSystemMetrics( SM_CXSCREEN )
   ::aStartPos[4]:=GetSystemMetrics( SM_CYSCREEN )
   cVal=oIni:Read( "Appearance", "AppMetrics", "" )
   aVal:=HB_ATokens( cVal, ";" )
   nPos:=min( 4, len(aVal) )
   for i:=1 to nPos
      nVal:=val( aVal[i] )
      aPos[i]:=if( nVal<0, 0, nVal )
   next
   if aPos[3] < ::aInitPos[3]
      aPos[3]:=::aInitPos[3]
   endif
   if aPos[4] < ::aInitPos[4]
      aPos[4]:=::aInitPos[4]
   endif
   if ( aPos[1]+aPos[3] <= ::aStartPos[3] ) .and. ;
      ( aPos[2]+aPos[4] <= ::aStartPos[4] )
      for i:=1 to 4
         ::aStartPos[i]:=aPos[i]
      next
   else
      for i:=1 to 4
         ::aStartPos[i]:=::aInitPos[i]
      next
   endif      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myEndPos( Sender ) CLASS Form1
   local cString:=ltrim(str(Sender:Left)) + ";" + ;
                  ltrim(str(Sender:Top)) + ";" + ;
                  ltrim(str(Sender:Width)) + ";" + ;
                  ltrim(str(Sender:Height)) + ";"
   oIni:Write( "Appearance", "AppMetrics", cString )      
RETURN Self