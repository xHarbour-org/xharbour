GLOBAL EXTERNAL oIni, oApp, oErr
GLOBAL EXTERNAL lOpenTable, lStatistics
GLOBAL EXTERNAL lShowd, lShared, lReadOnly, lSoftSeek, lCentury
GLOBAL EXTERNAL lShadowRow, lShowGrid, lShowHeaders, lConvertOEM

GLOBAL EXTERNAL nLight, nDark, nEpoch
GLOBAL EXTERNAL cDriver, cDateFormat, cCodePage, cFontFace, cFontSize
GLOBAL EXTERNAL cMaxCursor, cMaxCursorExt, nMaxCursor, nMaxCursorExt
GLOBAL EXTERNAL cBackColor, cForeColor, cHighlightColor, cHighlightTextColor
GLOBAL EXTERNAL cMarkerColor, cMarkerColorSec, cMarkerTextColor, cMarkerTextColorSec
GLOBAL EXTERNAL nMarkerColor, nMarkerTextColor, nMarkerColorSec, nMarkerTextColorSec
GLOBAL EXTERNAL nTestBack, nTestFore, nTestBackSec, nTestForeSec
GLOBAL EXTERNAL hDriver, hDate, hCode, hMaxCursor, hMaxCursorExt
GLOBAL EXTERNAL hFontFace, hFontSize, hBack, hFore
GLOBAL EXTERNAL hHighlight, hHighlightText
GLOBAL EXTERNAL hMarker, hMarkerText, hMarkerSec, hMarkerTextSec

#include "vxh.ch"
#include "FormSettings.xfm"
//---------------------------------------- End of system code ----------------------------------------//


//----------------------------------------------------------------------------------------------------//
METHOD FormSettings_OnLoad( Sender ) CLASS FormSettings
// initializing form controls   
   myFillBox(::BoxDriver, cDriver, @hDriver)
   myFillBox(::BoxDate, cDateFormat, @hDate)   
   myFillBox(::BoxCode, cCodePage, @hCode)   
   myFillBox(::BoxMemo, cMaxCursor, @hMaxCursor)
   myFillBox(::BoxFile, cMaxCursorExt, @hMaxCursorExt)
   myFillBox(::BoxFontFace, cFontFace, @hFontFace)
   myFillBox(::BoxFontSize, cFontSize, @hFontSize)   
   myFillBox(::BoxBack, cBackColor, @hBack)   
   myFillBox(::BoxFore, cForeColor, @hFore)   
   myFillBox(::BoxHighLight, cHighlightColor, @hHighLight)   
   myFillBox(::BoxMarker, cMarkerColor, @hMarker)   
   myFillBox(::BoxMarkerSec, cMarkerColorSec, @hMarkerSec)   
   myFillBox(::BoxHighLightTextColor, cHighlightTextColor, @hHighLightText)   
   myFillBox(::BoxMarkerTextColor, cMarkerTextColor, @hMarkerText)   
   myFillBox(::BoxMarkerTextColorSec, cMarkerTextColorSec, @hMarkerTextSec)   

   ::CheckShared:SetState(if( lShared, BST_CHECKED, BST_UNCHECKED))
   ::CheckReadOnly:SetState(if( lReadOnly, BST_CHECKED, BST_UNCHECKED))
   ::CheckSoftSeek:SetState(if( lSoftSeek, BST_CHECKED, BST_UNCHECKED))
   ::CheckStatistics:SetState(if( lStatistics, BST_CHECKED, BST_UNCHECKED))
   ::CheckCentury:SetState(if( lCentury, BST_CHECKED, BST_UNCHECKED))
   ::CheckConvertOEM:SetState(if( lConvertOEM, BST_CHECKED, BST_UNCHECKED))
   ::CheckShadowRow:SetState(if( lShadowRow, BST_CHECKED, BST_UNCHECKED))
   ::CheckShowGrid:SetState(if( lShowGrid, BST_CHECKED, BST_UNCHECKED))
   ::CheckShowHeaders:SetState(if( lShowHeaders, BST_CHECKED, BST_UNCHECKED))   
   ::EditEpoch:Caption:=strzero(nEpoch,4)
// marker color codes - for better performance kept as global binary values
   nTestBack:=nMarkerColor
   nTestFore:=nMarkerTextColor
   nTestBackSec:=nMarkerColorSec
   nTestForeSec:=nMarkerTextColorSec
// set up sample grid with colors   
   with object ::GridTest
      :Font:FaceName:=hFontFace[cFontFace]
      :Font:PointSize:=hFontSize[cFontSize]
      :BackColor:=hBack[cBackColor]
      :ForeColor:=hFore[cForeColor]
      :HighlightColor:=hHighlight[cHighlightColor]
      :HighlightTextColor:=hHighlightText[cHighlightTextColor]
      :Update()
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonOK_OnClick( Sender ) CLASS FormSettings
// save new values in memory
   cDriver:=::BoxDriver:GetSelString()
   lShared:=if(::CheckShared:Checked, .t., .f.)
   lReadOnly:=if(::CheckReadOnly:Checked, .t., .f.)
   lSoftSeek:=if(::CheckSoftSeek:Checked, .t., .f.)
   lStatistics:=if(::CheckStatistics:Checked, .t., .f.)
   cDateFormat:=::BoxDate:GetSelString()
   cMaxCursor:=::BoxMemo:GetSelString()
   cMaxCursorExt:=::BoxFile:GetSelString()
   nEpoch:=val(alltrim(::EditEpoch:Caption))
   lCentury:=if(::CheckCentury:Checked, .t., .f.)
   cCodePage:=::BoxCode:GetSelString()
   lConvertOEM:=if(::CheckConvertOEM:Checked, .t., .f.)
   cFontFace:=::BoxFontFace:GetSelString()
   cFontSize:=::BoxFontSize:GetSelString()
   lShadowRow:=if(::CheckShadowRow:Checked, .t., .f.)
   lShowGrid:=if(::CheckShowGrid:Checked, .t., .f.)
   lShowHeaders:=if(::CheckShowHeaders:Checked, .t., .f.)
// grid properties   
   cBackColor:=::BoxBack:GetSelString()
   if cBackColor == "custom"
      hBack["custom"]:=::GridTest:BackColor
   endif
   cForeColor:=::BoxFore:GetSelString()
   if cForeColor == "custom"
      hFore["custom"]:=::GridTest:ForeColor
   endif   
   cHighlightColor:=::BoxHighlight:GetSelString()
   if cHighlightColor == "custom"
      hHighlight["custom"]:=::GridTest:HighlightColor
   endif
   cHighlightTextColor:=::BoxHighlightTextColor:GetSelString()
   if cHighlightTextColor == "custom"
      hHighlightText["custom"]:=::GridTest:HighlightTextColor
   endif
// marker colors   
   cMarkerColor:=::BoxMarker:GetSelString()
   if cMarkerColor == "custom"
      hMarker["custom"]:=nTestBack
   endif
   cMarkerTextColor:=::BoxMarkerTextColor:GetSelString()
   if cMarkerTextColor == "custom"
      hMarkerText["custom"]:=nTestFore
   endif   
   cMarkerColorSec:=::BoxMarkerSec:GetSelString()
   if cMarkerColorSec == "custom"
      hMarkerSec["custom"]:=nTestBackSec
   endif
   cMarkerTextColorSec:=::BoxMarkerTextColorSec:GetSelString()
   if cMarkerTextColorSec == "custom"
      hMarkerTextSec["custom"]:=nTestForeSec
   endif   
// update the .ini file
   oApp:myWriteSettings(Self)
// apply the new settings
   oApp:myApplySettings(Self) 
   ::Close()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxBack_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local nColor:=myGetColor( Sender, @hBack)
   if nColor <> -1
      ::GridTest:BackColor:=nColor
      ::GridTest:Update()
      ::GridTest:SetFocus()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxFore_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local nColor:=myGetColor( Sender, @hFore)
   if nColor <> -1
      ::GridTest:ForeColor:=nColor
      ::GridTest:Update()
      ::GridTest:SetFocus()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxHighlight_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local nColor:=myGetColor( Sender, @hHighlight)
   if nColor <> -1
      ::GridTest:HighlightColor:=nColor
      ::GridTest:Update()
      ::GridTest:SetFocus()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxMarker_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local nColor:=myGetColor( Sender, @hMarker)
   if nColor <> -1
      nTestBack:=nColor
      ::GridTest:Update()
      ::GridTest:SetFocus()
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD EditEpoch_OnKillFocus( Sender ) CLASS FormSettings
   local n:=val(alltrim(Sender:Caption))
   if n<100 .or. n>2900
      ::MessageBox("Please type in a value between 100 and 2900","Epoch value not supported",MB_ICONEXCLAMATION)
      Sender:Caption:="1940"
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormSettings
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxMarkerTextColor_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local nColor:=myGetColor( Sender, @hMarkerText)
   if nColor <> -1
      nTestFore:=nColor
      ::GridTest:Update()
      ::GridTest:SetFocus()
   endif      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxHighlightTextColor_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local nColor:=myGetColor( Sender, @hHighlightText)
   if nColor <> -1
      ::GridTest:HighlightTextColor:=nColor
      ::GridTest:Update()
      ::GridTest:SetFocus()
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumnb_OnQueryBackColor( Sender ) CLASS FormSettings
   return nTestBack
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumnb_OnQueryForeColor( Sender ) CLASS FormSettings
   return nTestFore
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxFontFace_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local cFont:=HGetKeyAt( hFontFace, Sender:GetCurSel())
   ::GridTest:Font:FaceName:=cFont
   ::GridTest:Update()
   ::GridTest:SetFocus()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxFontSize_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local cP:=HGetKeyAt( hFontSize, Sender:GetCurSel())
   ::GridTest:Font:PointSize:=hFontSize[cP]
   ::GridTest:Update()   
   ::GridTest:SetFocus()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumnc_OnQueryBackColor( Sender ) CLASS FormSettings
   return nTestBackSec   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumnc_OnQueryForeColor( Sender ) CLASS FormSettings
   return nTestForeSec   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxMarkerSec_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local nColor:=myGetColor( Sender, @hMarkerSec)
   if nColor <> -1
      nTestBackSec:=nColor
      ::GridTest:Update()
      ::GridTest:SetFocus()
   endif      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxMarkerTextColorSec_OnCBNSelEndOk( Sender ) CLASS FormSettings
   local nColor:=myGetColor( Sender, @hMarkerTextSec)
   if nColor <> -1
      nTestForeSec:=nColor
      ::GridTest:Update()
      ::GridTest:SetFocus()
   endif         
RETURN Self