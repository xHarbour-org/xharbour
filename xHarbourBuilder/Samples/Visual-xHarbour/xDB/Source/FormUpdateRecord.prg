GLOBAL EXTERNAL oApp, oChild, oErr, oTable
GLOBAL EXTERNAL oF, aF
GLOBAL EXTERNAL nCursorCalc, nCursorNormal
GLOBAL EXTERNAL lAppend, lShared, lCentury
GLOBAL EXTERNAL lShadowRow, lShowGrid, lShowHeaders, lConvertOEM

GLOBAL EXTERNAL cDriver, cFontFace, cFontSize
GLOBAL EXTERNAL cBackColor, cForeColor
GLOBAL EXTERNAL cHighlightColor, cHighlightTextColor
GLOBAL EXTERNAL hDriver, hFontFace, hFontSize, hBack, hFore
GLOBAL EXTERNAL hHighlight, hHighlightText


#include "vxh.ch"
#include "FormUpdateRecord.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD GridColumn5_OnSave( Sender ) CLASS FormUpdateRecord
   
   local i:=::MTable:RecNo()
   local c:=alltrim(Sender:Caption)
   local nPic, cPic, cVal
   
   if aF[i,2]=="N"   //  numeric fields
      
      if aF[i,4] > 0         // numeric value with decimals
         nPic:=aF[i,3]-aF[i,4]-1
         cPic:=if( nPic>0, replicate("#", nPic), "")+"."+;
               if( nPic>0, replicate("#", aF[i,4]) ,"")
      else                   // integer
         cPic:=replicate("9", aF[i,3])
      endif
      cVal:=transform( val(c), cPic)
      if "*" $ cVal
         return 0
      else
         aF[i,5]:=cVal
      endif

   elseif aF[i,2]=="D"       // date value
      
      if lCentury.and.len(c)<>10 .or. !lCentury.and.len(c)<>8
         return 0
      endif
      aF[i,5]:=dtoc(ctod(c))
      
   elseif aF[i,2]=="L"       // boolean value
      
      c:=upper(c)
      if len(c)<>1 .or. !(c$"TF")
         return 0
      endif
      aF[i,5]:=c
      
   elseif aF[i,2] == "B"      // double
      
      aF[i,5]:=str( val(c), 20, 5 )
      
   elseif aF[i,2] == "I"     // long integer
      
      aF[i,5]:=str( val(c), 12 )
      
   elseif aF[i,2]=="C".and.aF[i,3]<=255.and.aF[i,4]==0   // strings
      
      aF[i,5]:=padr( c, aF[i,3])
      
   endif   
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormUpdateRecord_OnLoad( Sender ) CLASS FormUpdateRecord
   
   local i
      
   oF:=oTable:Fields

   if !lAppend     
// normally here is memorized for example TIMESTAMP_OLD,
// the current record's "last update" timestamp
   endif

   for i:=1 to len(aF)
      
      if lAppend    // initializing with default values
         if aF[i,2]=="N"
            aF[i,5]:=str( 0, aF[i,3], aF[i,4] )
         elseif aF[i,2]=="D"
            aF[i,5]:=dtoc(ctod("//"))
         elseif aF[i,2]=="L"
            aF[i,5]:="F"
         elseif aF[i,2]=="B"
            aF[i,5]:=str( 0, 20, 5 )
         elseif aF[i,2]=="I"
            aF[i,5]:=str( 0, 12 )
         elseif aF[i,2]=="C".and.aF[i,3]<=255.and.aF[i,4]==0
            aF[i,5]:=space(aF[i,3])
         else
            aF[i,5]:=""
         endif
      endif
            
      if !lAppend   // getting old field values
         if aF[i,2]=="N"
            aF[i,5]:=str( oF:FieldGet(i), aF[i,3], aF[i,4] )
         elseif aF[i,2]=="B"
            aF[i,5]:=str( oF:FieldGet(i), 20, 5)
         elseif aF[i,2]=="I"
            aF[i,5]:=str( oF:FieldGet(i), 12)
         elseif aF[i,2]=="D"
            aF[i,5]:=dtoc(oF:FieldGet(i))
         elseif aF[i,2]=="L"
            aF[i,5]:=if( oF:FieldGet(i), "T", "F")
         elseif aF[i,2]=="C" .and. aF[i,3]<=255.and.aF[i,4]==0
               aF[i,5]:=padr( oF:FieldGet(i), aF[i,3])
         endif
      endif
      
   next
// set up Grid with field structure and current row values
   ::Caption:=if( lAppend, "Append Record", "Update Record")
   ::MTable:Table:=aF
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
METHOD FormUpdateRecord_OnClose( Sender ) CLASS FormUpdateRecord
   lAppend:=.f.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonOK_OnClick( Sender ) CLASS FormUpdateRecord
   
   local i, lOK
   
   ::Cursor:=nCursorCalc
   ::Disable()
   if lAppend       // append confirmed
      lOK:=.f.
      try
         oTable:Append()
         lOK:=.t.
      catch oErr
         myError(oApp, oErr)
      end
      if !lOK         // append fails
         lAppend:=.f.
         ::Enable()
         ::Cursor:=nCursorNormal
         ::Close()
         return Self
      endif
   endif

   if !lAppend .and. lShared  // update with optimistic locking scheme
// normally here is compared TIMESTAMP_OLD
// with the current record's "last update" timestamp;   
// if the current value is more recent than TIMESTAMP_OLD, 
// then the UPDATE operation is discarded or it's performed
// other action depending the specific business logic    
      lOK:=.f.
      try
         oTable:RecLock()
         lOK:=.t.
      catch oErr
         myError(oApp, oErr)
      end
      if !lOK           // locking fails
         ::Enable()
         ::Cursor:=nCursorNormal
         ::Close()
         return Self
      endif
   endif

   lOK:=.f.
   try
      for i:=1 to len(aF)        // writing back new field values
         if aF[i,2] $ "NIB"
            oF:FieldPut(i, val(aF[i,5]))
         elseif aF[i,2]=="D"
            oF:FieldPut(i, ctod(aF[i,5]))
         elseif aF[i,2]=="L"
            oF:FieldPut(i, if( upper(aF[i,5])=="T", .t., .f.))
         elseif aF[i,2]=="C".and.aF[i,3]<=255.and.aF[i,4]==0
            oF:FieldPut(i, aF[i,5])
         endif
      next
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   if !lOK                        // update (write) fails
      ::Enable()
      ::nCursor:=nCursorNormal
      ::Close()
      return Self
   endif  
   
   try   
// in real-world applications commit is made on document level
// or depending on the specific business logic
      oTable:Commit()
   catch oErr
      myError(oApp, oErr)
   end
   
   if !lAppend .and. lShared       // unlock
      try
         oTable:UnLock()
      catch oErr
         myError(oApp, oErr)
      end
   endif

   oChild:Grid:Update()      // update grid on application form
   lAppend:=.f.   
   ::Enable()
   ::Cursor:=nCursorNormal
   ::Close()   
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormUpdateRecord
   lAppend:=.f.
   ::Close()
RETURN Self