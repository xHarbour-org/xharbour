GLOBAL EXTERNAL oApp, oErr, oTable
GLOBAL EXTERNAL lOpenTable, lStruCreate
GLOBAL EXTERNAL cIniPath, cStruName, cNewTable
GLOBAL EXTERNAL cDriver, hDriver
GLOBAL EXTERNAL nAlias
GLOBAL EXTERNAL aStru

#include "vxh.ch"
#include "FormCreateTable.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD ButtonStru_OnClick( Sender ) CLASS FormCreateTable
// choose extended structure file   
   local npos
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
      return Self
   else
      npos:=RAt("\", cStruName)
      cIniPath:=if(nPos>0, Left(cStruName, npos-1), "")
      ::EditStru:Caption:=cStruName
   endif         
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonNew_OnClick( Sender ) CLASS FormCreateTable
// select new table name   
   local npos
   local cRDD:=right(hDriver[cDriver],3)
   local cFilter:=if(cRDD=="ADT", "Advantage Tables (*.ADT)|*.ADT", ;
                      "DBase Tables (*.DBF)|*.DBF")+;
                      "|All files ( *.* )|*.*"
   with object oApp:SaveFile                      
      :DefaultExt:=if(cRDD=="ADT", "ADT", "DBF")
      :Filter:=cFilter
      :InitialDirectory:=cIniPath
      :Title:="New table location and name"
      :FileName:=""
      :Show()
      cNewTable:=:FileName
   end
   if empty(cNewTable)
      return Self
   else
      npos:=RAt("\", cNewTable)
      cIniPath:=if(nPos>0, Left(cNewTable, npos-1), "")   
      ::EditNew:Caption:=cNewTable
   endif     
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCreate_OnClick( Sender ) CLASS FormCreateTable  
   
   local npos, nl
   local i, j, lOK, cNew
   local aS:={ {"FIELD_NAME","C",10,0},;
               {"FIELD_TYPE","C",1,0},;
               {"FIELD_LEN","N",3,0},;
               {"FIELD_DEC","N",3,0} }

   field field_name, field_type, field_len, field_dec      
// chech found values
   if empty(alltrim(cNewTable)) .or. empty(alltrim(cStruName))
      ::MessageBox("Please select both files", "Warning", MB_ICONEXCLAMATION)
      return Self
   endif   
   if len(cNewTable)==len(cStruName) .and. upper(cNewTable)==upper(cStruName)
      ::MessageBox("Please select different files", "Warning", MB_ICONEXCLAMATION)
      return Self
   endif      

   ::Disable()

   lOK:=.f.          // open extended structure file
   try
      cNew:="S"+ltrim(str(++nAlias))
      DBUseArea(.t., "DBFNTX", cStruName, cNew, .f., .f.)
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end   
   if !lOK
      ::Enable()
      return Self
   endif
   
   select(cNew)        // check it if it's really a structure file
   lOK:=.t.
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
      ::Enable()
      return Self
   endif
   
   pack                 // read in the field structure
   go top
   ASize(aStru, reccount())
   i:=0
   for i:=1 to reccount()
      goto i
      aStru[i]:={padr(field_name,10), field_type, field_len, field_dec}
   next
   DBCloseArea()
   
   lOK:=.f.            // create new empty table
   try
      DBCreate(cNewTable, aStru, hDriver[cDriver], NIL)
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   if !lOK
      ::Enable()
      return Self
   endif
   
   ::MessageBox("The new table has been created successfully", "", MB_ICONINFORMATION)

   ::Enable()   
   ::Close()

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormCreateTable
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormCreateTable_OnLoad( Sender ) CLASS FormCreateTable
   ::EditStru:Caption:=cStruName
   cNewTable:=""
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonNewStru_OnClick( Sender ) CLASS FormCreateTable
   lStruCreate:=.t.     // structure file is going to be created here
   FormStructure( ::this )
   if !empty(cStruName)
      ::EditStru:Caption:=cStruName
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormCreateTable_OnDestroy( Sender ) CLASS FormCreateTable
   if lOpenTable
      select(oTable:Alias)
   endif   
RETURN Self