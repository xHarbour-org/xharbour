GLOBAL EXTERNAL oApp, oErr, oTable
GLOBAL EXTERNAL nCursorCalc, nCursorNormal
GLOBAL EXTERNAL aIndex, aCursor
GLOBAL EXTERNAL nMaxCursor, nMaxCursorExt
GLOBAL EXTERNAL cEngine, cSchema, cSchemaTable, hConnection, oSQL

GLOBAL EXTERNAL cTableName, cIniPath, cExchangeName
GLOBAL EXTERNAL cTempPath
GLOBAL EXTERNAL lOpenTable, lShared, lStatistics
GLOBAL EXTERNAL nAlias, nTemp
GLOBAL EXTERNAL cDriver, cDateFormat, cCodePage
GLOBAL EXTERNAL hDriver, hDate, hCode


#include "vxh.ch"
#include "FormImportInEngine.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormImportInEngine_OnLoad( Sender ) CLASS FormImportInEngine
// query and load in combobox list with tables in schema   
   local i, cQuery
   
   if cEngine == "MYSQL"
      cQuery:="select left(table_name,32) "+;
               "from information_schema.tables "+;
               "where table_schema='"+cSchema+"' and table_name not like 'sr_mgmnt%' "+;
               "order by table_name"
   elseif cEngine == "FB"
      cQuery:="select RDB$RELATION_NAME "+;
               "from RDB$RELATIONS "+;
               "where RDB$VIEW_BLR IS NULL and RDB$SYSTEM_FLAG=0 and RDB$RELATION_NAME not like 'SR_MGMNT%' "+;
               "order by RDB$RELATION_NAME"
   elseif cEngine == "MS"
      cQuery:="select left(name, 32) "+;
              "from sysobjects "+;
              "where (type='U' or type='V') and name not like 'SR_MGMNT%' "+;
              "order by name"
   endif

   oApp:Cursor:=nCursorCalc
   #ifdef XDB_SQLRDD
      ASize( aCursor, 0)   
      oSQL:Exec( cQuery, .f., .t., @aCursor, , ,nMaxCursor )
   #endif
   oApp:Cursor:=nCursorNormal

   for i:=1 to len(aCursor)
      ::BoxTable:AddString(aCursor[i,1])
   next
   if len(aCursor) > 0
      ::BoxTable:SetCurSel(1)
   endif
   if lOpenTable
      ::EditFree:Caption:=cTableName
      ::EditFree:Enabled:=.f.
      ::ButtonSelect:Enabled:=.f.
   else
      ::ToolFree:Enabled:=.f.
      ::CheckIndex:Visible:=.f.
   endif
   ::PBar:SetRange({1, 50})
   ::PBar:SetStep(1)
   ::PBar:Visible:=.f.
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolFree_OnClick( Sender ) CLASS FormImportInEngine
   FormProperties( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolSchema_OnClick( Sender ) CLASS FormImportInEngine
   FormEngineInfo( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolIndex_OnClick( Sender ) CLASS FormImportInEngine
   local n:=::BoxTable:GetCurSel()
   if !::RadioAppend:GetState() == BST_CHECKED
      ::MessageBox( "Please select option 'Use an existent table' and choose a table from list", "", MB_ICONEXCLAMATION)
      return Self
   endif
   if n < 1
      ::MessageBox( "Currently there is no table selected", "", MB_ICONEXCLAMATION)
      return Self
   endif
   cSchemaTable:=::BoxTable:GetSelString()
   FormEngineIndexes( ::this )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonSelect_OnClick( Sender ) CLASS FormImportInEngine   
// get table name to import   
   local npos
   local cRDD:=right(hDriver[cDriver],3)
   local cExt:=if(cRDD<>"ADT", "DBF", "ADT")
   with object oApp:OpenFile
      :DefaultExt:=cExt
      :Filter:="DBase Tables (*.DBF)|*.DBF|Advantage Tables (*.ADT)|*.ADT|All files ( *.* )|*.*"
      :InitialDirectory:=cIniPath
      :Title:="Select file to import data from"
      :FileName:=""
      :Show()   
      cExchangeName:=:FileName
   end
   if empty(cExchangeName)
      return Self
   endif
   npos:=RAt("\", cExchangeName)
   cIniPath:=if(npos>0, Left(cExchangeName, npos-1), "")
   ::EditFree:Caption:=cExchangeName   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonOK_OnClick( Sender ) CLASS FormImportInEngine
   
   local cFile:=alltrim(::EditFree:Caption), nSel:=::BoxTable:GetCurSel()
   local i, j, nPos, nSeconds, cTable, cPos, cAlias, lOK, lIndexes
   local nLen:=0, cVIA:=hDriver[cDriver]
   local ci, cj, aStruct
   
   if empty(cFile)   // free table name not specified
      ::MessageBox( "Please select a valid free table", "Empty source table name", MB_ICONEXCLAMATION)
      return Self
   endif
   
   if ::RadioAppend:GetState()==BST_CHECKED .and. ::CheckIndex:Checked()
      ::MessageBox( "Index reproducing option is valid only when creating a new table", "", MB_ICONEXCLAMATION)
      return Self
   endif  

   lIndexes:=.f.    // are there indexes to rebuild ?
   if lOpenTable .and. ::CheckIndex:Checked() .and. len(aIndex) > 0
      nLen:=len(aIndex)
      nPos:=0
      for i:=2 to nLen       // index name must not be empty
         if empty(alltrim(aIndex[i,3]))  // index tag's name
            nPos:=i
            exit
         endif
      next
      if nPos > 0
         ::MessageBox( "Index tag names have to be completed and unique", "Empty index tag name(s)", MB_ICONEXCLAMATION)
         return Self
      endif
      for i:=2 to nLen-1     // index name must not be duplicate
         for j:=3 to nLen
            if i == j
               loop
            endif
            ci:=upper(alltrim(aIndex[i,3]))
            cj:=upper(alltrim(aIndex[j,3]))
            if len(ci)==len(cj) .and. ci==cj
               nPos:=i
               exit
            endif
         next
      next
      if nPos > 0
         ::MessageBox( "Index tag names have to be completed and unique", "Duplicate Index tag name(s)", MB_ICONEXCLAMATION)
         return Self
      endif
      lIndexes:=.t. 
   endif
   
   if ::RadioCreate:GetState() == BST_CHECKED  // import in new table
      cTable:=alltrim(::EditTable:Caption)
      if !myCheckName(cTable, 31)
         ::MessageBox( "Please complete the table Name field with a valid name - the maximum length is 31 characters", "Table name not valid", MB_ICONEXCLAMATION)
         return Self
      endif
      nPos:=0
      for i:=1 to ::BoxTable:GetCount()  // table name must not exist
         cPos:=alltrim(::BoxTable:GetString(i))
         if len(cPos)==len(cTable) .and. upper(cPos)==upper(cTable)
            nPos:=i
            exit
         endif
      next
      if nPos > 0
         ::MessageBox( "The specified table already exists in the schema", "", MB_ICONEXCLAMATION)
         return Self
      endif
   elseif ::RadioAppend:GetState() == BST_CHECKED
      if nSel < 1
         ::MessageBox( "There is no schema table selected", "", MB_ICONEXCLAMATION)
         return Self
      endif
      cTable:=::BoxTable:GetSelString()
   endif
   
   ::Disable()   
                            // start import   
   ::Timer1:Start()
   ::PBar:Visible:=.t.
   if ::RadioCreate:GetState() == BST_CHECKED  // new table
      lOK:=.f.
      cAlias:="c"+ltrim(str(++nAlias,6))
      try              // read in table structure and create it
         #ifdef XDB_SQLRDD
            DBUseArea(.t., cVIA, cFile, cAlias, .t., .t.)
            select(cAlias)
            aStruct:=DBStruct()   
            DBCreate( upper(cTable), aStruct, "SQLRDD", NIL)
         #endif
         lOK:=.t.
      catch oErr
         myError(oApp, oErr)
      end
      if !lOK          // failure
         ::PBar:Visible:=.f.
         ::Timer1:Stop()
         ::Enable()
         return Self
      endif
   endif
   
   lOK:=.f.
   cAlias:="c"+ltrim(str(++nAlias,6))
   nSeconds:=Seconds()
   try                  // append from free table
      #ifdef XDB_SQLRDD
         DBUseArea(.t., "SQLRDD", upper(cTable), cAlias, .t., .f.)
         select(cAlias)
         append from (cFile) via (cVIA)
         DBCommit()
         ::BoxTable:AddString(upper(cTable))
         ::BoxTable:SetCurSel(::BoxTable:GetCount())     
         nSeconds:=mySeconds( nSeconds, Seconds())
         if lStatistics
            ::PBar:Visible:=.f.
            ::Timer1:Stop()
            ::MessageBox("Free table import terminated in "+ltrim(str(nSeconds))+" seconds", "", MB_ICONINFORMATION)
         endif
         if lIndexes       // indexing
            ::PBar:Visible:=.t.
            ::Timer1:Start()
            nSeconds:=Seconds()
            for i:=2 to nLen
               OrdCreate(, aIndex[i,3], aIndex[i,5] )
            next
            DBCommit()
            nSeconds:=mySeconds( nSeconds, Seconds())
            if lStatistics
               ::PBar:Visible:=.f.
               ::Timer1:Stop()
               ::MessageBox("Index reproducing terminated in "+ltrim(str(nSeconds))+" seconds", "", MB_ICONINFORMATION)
            endif
         endif
         DBCloseArea() 
      #endif
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   
   if !lOK .or. !lStatistics
      ::PBar:Visible:=.f.
      ::Timer1:Stop()
   endif
   if lOK
      ::EditTable:Caption:=""
   endif
   
   ::Enable()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormImportInEngine
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioCreate_OnClick( Sender ) CLASS FormImportInEngine
   ::EditTable:Enabled:=.t.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioAppend_OnClick( Sender ) CLASS FormImportInEngine
   ::EditTable:Enabled:=.F.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormImportInEngine_OnDestroy( Sender ) CLASS FormImportInEngine
   if lOpenTable
      select(oTable:Alias)
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Timer1_OnTimeOut( Sender ) CLASS FormImportInEngine
   ::PBar:StepIt()
   Sender:Start()
RETURN Self