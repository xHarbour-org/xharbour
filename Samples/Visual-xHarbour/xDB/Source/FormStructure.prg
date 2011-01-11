GLOBAL EXTERNAL oApp, oErr, oStr, oTable
GLOBAL EXTERNAL lStruCreate, lOpenTable, lOpenStru
GLOBAL EXTERNAL cStruName, cTempPath, cIniPath
GLOBAL EXTERNAL cDriver, hDriver
GLOBAL EXTERNAL nAlias, nTemp
GLOBAL EXTERNAL aStru

GLOBAL EXTERNAL cFontFace, cFontSize
GLOBAL EXTERNAL cBackColor, cForeColor
GLOBAL EXTERNAL cHighlightColor, cHighlightTextColor
GLOBAL EXTERNAL hFontFace, hFontSize, hBack, hFore, hHighlight, hHighlightText

#include "vxh.ch"
#include "FormStructure.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD FormStructure_OnLoad( Sender ) CLASS FormStructure
   
   local npos, nl
   local cRDD:=right(hDriver[cDriver],3)
   local i, j, lOK, cNew
   local aS:={ {"FIELD_NAME","C",10,0},;
               {"FIELD_TYPE","C",1,0},;
               {"FIELD_LEN","N",3,0},;
               {"FIELD_DEC","N",3,0} }

   field field_name, field_type, field_len, field_dec

   lOpenStru:=.f.
   Sender:Caption:=if(lStruCreate, "Create", "Update")+" Extended File Structure"

   if lStruCreate   //  new structure, create temporary file to keep it

      nTemp++
      cStruName:=myTempFileName( @nTemp, cTempPath, "dbf")
      lOK:=.f.
      try
         DBCreate(cStruName, aS, "DBFNTX", NIL)
         lOK:=.t.
      catch oErr
         myError(oApp,oERR)
      end
      if !lOK
         ::Close()
         return Self
      endif

   else      // structure already exists
      
      with object oApp:OpenFile      
         :DefaultExt:="DBF"
         :Filter:="DBase Tables (*.DBF)|*.DBF|All files ( *.* )|*.*"
         :InitialDirectory:=cIniPath
         :Title:="Select extended structure file"
         :FileName:=""
         :Show()   
         cStruName:=:FileName      
      end
      
      if empty(cStruName)
         ::Close()
         return Self
      else
         npos:=RAt("\", cStruName)
         cIniPath:=if(nPos>0, Left(cStruName, npos-1), "")
      endif
      
   endif      
   
   lOK:=.f.
   try                                  // open it
      cNew:="S"+ltrim(str(++nAlias))
      DBUseArea(.t., "DBFNTX", cStruName, cNew, .f., .f.)
      lOpenStru:=.t.
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end   
   if !lOK
      ::Close()
      return Self
   endif
   select(cNew)
   lOK:=.t.          // check if it's an extended structure table
   if fcount() < 4
      lOK:=.f.
   endif
   for i:=1 to 4
      if upper(FieldName(i))<>aS[i,1] .or. ;
         upper(FieldType(i))<>aS[i,2] .or. ;
         FieldLen(i)<aS[i,3] .or. ;
         FieldDec(i)<aS[i,4]
         lOK:=.f.
      endif
   next
   if !lOK
      oApp:MessageBox("The selected file is not an extended structure file", "Warning", MB_ICONEXCLAMATION)
      DBCloseArea()
      lOpenStru:=.f.
      ::Close()
      return Self
   endif
   pack             // read in field structure
   go top
   ASize(aStru, reccount())
   i:=0
   for i:=1 to reccount()
      goto i
      aStru[i]:={padr(field_name,10), field_type, field_len, field_dec}
   next

   ::STable:Table:=aStru        // display the structure in Grid
   ::STable:GoTop()
   with object ::Grid
      :Font:FaceName:=HFontFace[cFontFace]
      :Font:PointSize:=hFontSize[cFontSize]
      :BackColor:=hBack[cBackColor]
      :ForeColor:=hFore[cForeColor]
      :HighlightColor:=hHighlight[cHighlightColor]      
      :HighlightTextColor:=hHighlightText[cHighlightTextColor]      
      :Update()
      oStr:=:DataSource:Fields
   end
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonAdd_OnClick( Sender ) CLASS FormStructure
   local nMax:=::STable:Reccount(), i, j, lOK, cc, cName
// add new field   
   ::Disable()
   
   j:=0
   do while .t.  // find fieldname first field name NEWnn which doesn't exist
      j++
      cName:="NEW"+ltrim(str(j))
      lOK:=.t.
      for i:=1 to nMax
         cc:=upper(alltrim(aStru[i,1]))
         if len(cc)==len(cName) .and. cc==cName
            lOK:=.f.
         endif
      next
      if lOK
         exit
      endif
   enddo
   ::STable:Append()    // complete with default values
   oStr:NAME:=padr(cName,10)
   oStr:TYPE:="C"
   oStr:LENGTH:=10
   oStr:DECIMALS:=0
   ::Grid:Update()
   
   ::Enable()   
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDelete_OnClick( Sender ) CLASS FormStructure
   local nl:=::STable:Reccount()
   if nl < 1
      return Self
   endif
// delete field in current row   
   ::Disable()
   
   ::STable:Delete()
   ::Grid:Update()
   
   ::Enable()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonSave_OnClick( Sender ) CLASS FormStructure
   local i, npos, cNewFile, lOK

   ::Disable()

   if !lStruCreate   // erase old structure on harddisk
      zap
   endif
   for i:=1 to len(aStru)    // write new structure to harddisk
      DBAppend()
      replace field_name with rtrim(aStru[i,1]), ;
              field_type with aStru[i,2], ;
              field_len  with aStru[i,3], ;
              field_dec  with aStru[i,4]
   next
   DBCloseArea()           // close structure file
   lOpenStru:=.f.   
   
   ::Enable()
   
   if lStruCreate    // if new structure, choose permanent file name to save it

      with object oApp:SaveFile
         :DefaultExt:="DBF"
         :Filter:="DBase Tables (*.DBF)|*.DBF|All files ( *.* )|*.*"
         :InitialDirectory:=cIniPath
         :Title:="Create extended structure file"
         :FileName:=""
         :Show()
         cNewFile:=:FileName
      end
      if empty(cNewFile)
         ::MessageBox(cStruName, "The new extended structure file was created at:", MB_ICONINFORMATION)
      else
         ::Disable()
         lOK:=.f.
         try          // do the file copy from temporary to permanent
            copy file (cStruName) to (cNewFile)
            lOK:=.t.
            delete file (cStruName)
            cStruName:=cNewFile
         catch oErr
            myError(oApp, oErr)
         end
         if !lOK
            ::MessageBox(cStruName, "The new extended structure file was created at:", MB_ICONINFORMATION)         
         endif         
         npos:=RAt("\", cStruName)
         cIniPath:=if(nPos>0, Left(cStruName, npos-1), "")   
         ::Enable()
      endif     

   endif
   
   ::Close()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormStructure
   local nRes
   if lOpenStru
      nRes:=::MessageBox("Are you sure you want to abandon the operation","",MB_ICONQUESTION|MB_YESNO)
      if nRes == 7
         return Self
      else
         ::Disable()
         DBCloseArea()
         lOpenStru:=.f.
         ::Enable()
      endif
   endif
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormStructure_OnClose( Sender ) CLASS FormStructure
   local nRes
   if lOpenStru
      nRes:=::MessageBox("Are you sure you want to abandon the operation","",MB_ICONQUESTION|MB_YESNO)
      if nRes == 7
         return 0
      else
         ::Disable()
         DBCloseArea()
         lOpenStru:=.f.
      endif
   endif
   aSize(aStru, 0)
   lStruCreate:=.f.
   ::Enable()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn1_OnSave( Sender ) CLASS FormStructure
// field name   
   local cn:=upper(alltrim(Sender:Caption))
   local i, cc, np:=::STable:Recno(), lOK:=.t.
   if empty(cn)
      ::MessageBox("Field name cannot be empty","Warning",MB_ICONEXCLAMATION)
      return 0
   endif
   go top
   for i:=1 to ::STable:Reccount()
      if i == np
         loop
      endif
      cc:=upper(alltrim(aStru[i,1]))
      if len(cc)==len(cn) .and. cc==cn
         lOK:=.f.
      endif
   next
   if !lOK
      ::MessageBox("Field name must be unique","Warning",MB_ICONEXCLAMATION)
      return 0
   endif   
   oStr:NAME:=padr(cn,10)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn2_OnSave( Sender ) CLASS FormStructure
// field type   
   local cType:=alltrim(Sender:Caption)
   local nl:=oStr:Length, nd:=oStr:Decimals
   local cRDD:=right(hDriver[cDriver],3)
   if empty(cType).or.!cType$"BCDILMN"
      ::MessageBox("Field type should be one of the followings: B, C, D, I, L, M, N","Warning",MB_ICONEXCLAMATION)
      return 0
   endif
   if cType=="D" .or. cType=="B"
      oStr:Length:=8
      oStr:Decimals:=0
   elseif cType=="I"
      oStr:Length:=4
      oStr:Decimals:=0
   elseif cType=="L"
      oStr:Length:=1
      oStr:Decimals:=0
   elseif cType=="M"
      oStr:Length:=if(cRDD=="ADT", 9, 10)
      oStr:Decimals:=0
   elseif cType=="N"
      oStr:Length:=if( nl>20.or.nl<1, 1, nl)
      oStr:Decimals=if( nd>18, 0, nd)
   else
      oStr:Decimals:=0
   endif
   oStr:TYPE:=cType
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn3_OnSave( Sender ) CLASS FormStructure
// field length   
   local nl:=if(valtype(Sender:Caption)=="N", Sender:Caption, val(alltrim(Sender:Caption)))
   local ct:=oStr:TYPE
   local cRDD:=right(hDriver[cDriver],3)
   if nl < 0
      return 0
   endif
   if ct == "C"
      if nl<1 .or. nl>255
         return 0
      endif
   elseif ct $ "BD"
      if nl <> 8
         return 0
      endif
   elseif ct == "I"
      if nl <> 4
         return 0
      endif
   elseif ct == "L"
      if nl <> 1
         return 0
      endif
   elseif ct == "M"
      if cRDD == "ADT"
         if nl<>9
            return 0
         endif
      else
         if nl<>10
            return 0
         endif
      endif
   elseif ct == "N"
      if nl<1 .or. nl>20
         return 0
      endif
   endif
   oStr:LENGTH:=nl
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn4_OnSave( Sender ) CLASS FormStructure
// field decimals   
   local nl:=if(valtype(Sender:Caption)=="N", Sender:Caption, val(alltrim(Sender:Caption)))
   local np:=oStr:LENGTH
   local ct:=oStr:TYPE
   local cRDD:=right(hDriver[cDriver],3)
   if nl < 0
      return 0
   endif
   if ct == "C"
      if nl<1 .or. nl>255
         return 0
      endif
   elseif ct $ "BDIL"
      if nl <> 0
         return 0
      endif
   elseif ct == "M"
      if cRDD == "ADT"
         if nl<>0
            return 0
         endif
      else
         if nl<>0
            return 0
         endif
      endif
   elseif ct == "N"
      if nl<1 .or. nl>18 .or. nl>np-2
         return 0
      endif
   endif
   oStr:DECIMALS:=nl
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormStructure_OnDestroy( Sender ) CLASS FormStructure
   if lOpenTable
      select(oTable:Alias)
   endif   
RETURN Self