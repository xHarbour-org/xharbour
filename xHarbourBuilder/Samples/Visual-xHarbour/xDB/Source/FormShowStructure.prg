GLOBAL EXTERNAL oApp, oErr, oTable

GLOBAL EXTERNAL cFontFace, cFontSize
GLOBAL EXTERNAL cBackColor, cForeColor, cHighlightColor, cHighlightTextColor
GLOBAL EXTERNAL hFontFace, hFontSize, hBack, hFore, hHighlight, hHighlightText

#include "vxh.ch"
#include "FormShowStructure.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormShowStructure_OnLoad( Sender ) CLASS FormShowStructure
   ::MTable:Table:=oTable:Structure
   ::MTable:GoTop()
   with object ::Grid
      :Font:FaceName:=HFontFace[cFontFace]
      :Font:PointSize:=hFontSize[cFontSize]
      :BackColor:=hBack[cBackColor]
      :ForeColor:=hFore[cForeColor]
      :HighlightColor:=hHighlight[cHighlightColor]      
      :HighlightTextColor:=hHighlightText[cHighlightTextColor]      
      :Update()
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonClose_OnClick( Sender ) CLASS FormShowStructure
   ::Close()
RETURN Self