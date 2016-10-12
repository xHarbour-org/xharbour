GLOBAL EXTERNAL oApp, oErr, oCursor
GLOBAL EXTERNAL nCursorCalc, nCursorNormal
GLOBAL EXTERNAL cFontFace, cFontSize, cBackColor, cForeColor
GLOBAL EXTERNAL cHighlightColor, cHighlightTextColor, cTempFileName
GLOBAL EXTERNAL hFontFace, hFontSize, hBack, hFore
GLOBAL EXTERNAL hHighlight, hHighlightText
GLOBAL EXTERNAL aTemp

#include "vxh.ch"
#include "FormCursor.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormCursor_OnLoad( Sender ) CLASS FormCursor
   local lQueryOpen:=.f.
   oCursor:=Sender
   cTempFileName:=""
   ::dt:Path:=aTemp[1]      // open temp file with cursor data
   ::dt:FileName:=aTemp[2]
   ::dt:Alias:=aTemp[3]
   try
      ::dt:Open()
      lQueryOpen:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   if !lQueryOpen            // failure
      if file(aTemp[4])
         try
            deletefile(aTemp[4])
         catch oErr
            myError( oApp, oErr)
         end
      endif
   endif

   with object ::grid           // display cursor data in grid
      :DataSource:=::dt
      :AutoAddColumns()
      :Font:FaceName:=HFontFace[cFontFace]
      :Font:PointSize:=hFontSize[cFontSize]
      :BackColor:=hBack[cBackColor]
      :ForeColor:=hFore[cForeColor]
      :HighlightColor:=hHighlight[cHighlightColor]      
      :HighlightTextColor:=hHighlightText[cHighlightTextColor]      
      :Gotop()
      :Update()   
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormCursor_OnDestroy( Sender ) CLASS FormCursor  
   if file(cTempFileName)         // delete temp data
      try
         deletefile(cTempFileName)
      catch oErr
         myError(oApp, oErr)
      end
   endif
   cTempFileName:=""
   oCursor:=NIL
   aTemp:={ "", "", "", "" }
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD dt_OnClose( Sender ) CLASS FormCursor
   cTempFileName:=::dt:Path+if( empty(::dt:Path), "", "\")+::dt:FileName   
RETURN Self