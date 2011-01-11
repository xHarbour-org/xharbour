GLOBAL EXTERNAL oApp, oTable, oErr, oCursor
GLOBAL EXTERNAL cSchema
GLOBAL EXTERNAL nMaxCursor, nMaxCursorExt, nAlias, nTemp
GLOBAL EXTERNAL cTempPath, cQuerySource, cIniPath, cCursorExport
GLOBAL EXTERNAL lOpenTable, lStatistics
GLOBAL EXTERNAL aCursor, aHelper, aTemp
GLOBAL EXTERNAL nCursorCalc, nCursorNormal
GLOBAL EXTERNAL cEngine, hConnection, oSQL

#include "vxh.ch"
#include "FormInstantQuery.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormInstantQuery_OnLoad( Sender ) CLASS FormInstantQuery   
// query and display metadata: table and column names and datatypes   
   local cQuery, lOK
   
   ::PBar:Visible:=.f.
   ::PBar:SetRange({1, 50})
   ::PBar:SetStep()
   
   ::EditQuery:Enabled:=.f.
   ::EditQuery:Caption:=cQuerySource
   
   if cEngine == "MYSQL"
      cQuery:="select left(table_name,32) as table_name, "+;
                     "left(column_name,32) as field_name, "+;
                     "left(column_type,32) as field_type "+;
              "from information_schema.columns "+;
              "where table_schema='"+cSchema+"' and table_name not like 'sr_mgmnt%' "+;
              "order by table_name"
   elseif cEngine == "FB"
      cQuery:="SELECT r.RDB$RELATION_NAME AS table_name, "+;
                     "r.RDB$FIELD_NAME AS field_name, "+;
                           "CASE f.RDB$FIELD_TYPE "+;
                                 "WHEN 261 THEN 'BLOB' "+;
                                 "WHEN 14 THEN 'CHAR' "+;
                                 "WHEN 40 THEN 'CSTRING' "+;
                                 "WHEN 11 THEN 'D_FLOAT' "+;
                                 "WHEN 27 THEN 'DOUBLE' "+;
                                 "WHEN 10 THEN 'FLOAT' "+;
                                 "WHEN 16 THEN 'INT64' "+;
                                 "WHEN 8 THEN 'INTEGER' "+;
                                 "WHEN 9 THEN 'QUAD' "+;
                                 "WHEN 7 THEN 'SMALLINT' "+;
                                 "WHEN 12 THEN 'DATE' "+;
                                 "WHEN 13 THEN 'TIME' "+;
                                 "WHEN 35 THEN 'TIMESTAMP' "+;
                                 "WHEN 37 THEN 'VARCHAR' "+;
                                 "ELSE 'UNKNOWN' "+;
                           "END AS field_type "+;
              "FROM RDB$RELATION_FIELDS r "+;
                     "LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME "+;
              "WHERE f.RDB$SYSTEM_FLAG=0 and r.RDB$RELATION_NAME not like 'SR_MGMNT%' "+;
              "ORDER BY r.RDB$RELATION_NAME"
   elseif cEngine=="MS"
      cQuery:="select left(s.table_name, 32), "+;
                     "left(s.column_name, 32), "+;
                     "left(s.data_type, 32) "+;
              "from information_schema.columns s, sysobjects o "+;
              "where object_id(s.table_name)=o.id and o.type='U' and s.table_name not like 'SR_MGMNT%' "+;
              "order by s.table_name, s.column_name"
   endif
      
   oApp:Cursor:=nCursorCalc
   ASize( aCursor, 0)   
   lOK:=.f.
   try
      #ifdef XDB_SQLRDD
         oSQL:Exec( cQuery, .T., .t., @aCursor, , ,nMaxCursor )
      #endif
      lOK:=.t.
   catch oErr
      myError( oApp, oErr)
   end
   oApp:Cursor:=nCursorNormal
   if !lOK
      return Self
   endif
   ::Helper:DataSource:Table:=aCursor
   ::Helper:GoTop()
   ::Helper:Update()
   ::Enable()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormInstantQuery_OnClose( Sender ) CLASS FormInstantQuery
   cQuerySource:=alltrim(::EditQuery:Caption)
   ::myCloseTempFile(Self)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolEdit_OnClick( Sender ) CLASS FormInstantQuery
   local cTempFile, lOK
   ::myCloseTempFile(Self)
   ::EditQuery:Enabled:=.t.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolSave_OnClick( Sender ) CLASS FormInstantQuery
   local cFile, lRet, nPos
   local cQuery:=alltrim(::EditQuery:Caption)
   if empty(cQuery)
      ::MessageBox( "Query source code is empty", "Warning", MB_ICONEXCLAMATION)
      return Self
   endif
// read filename
   with object oApp:SaveFile
      :DefaultExt:="txt"
      :Filter:="Text files (*.txt)|*.txt|All files ( *.* )|*.*"
      :InitialDirectory:=cIniPath
      :Title:="Save query source code as file..."
      :FileName:=""
      :Show()
      cFile:=:FileName
   end
   if empty(cFile)
      return Self
   else
      nPos:=RAt("\", cFile)
      cIniPath:=if(nPos>0, Left( cFile, nPos-1), "")   
   endif
                          // save query source text file
   ::Disable()
   try
      lRet:=MemoWrit( cFile, cQuery)
      if !lRet
         ::MessageBox( "Text file write error", "", MB_ICONEXCLAMATION)
      endif
   catch oErr
      myError( oApp, oErr)
   end
   ::Enable()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolRun_OnClick( Sender ) CLASS FormInstantQuery
   
   local cQuery, cTempFile, cTempName, cTempAlias, lOK, lFetch, nSeconds
   
   cQuery:=alltrim(::EditQuery:Caption)
   if empty(cQuery)
      ::MessageBox( "Query source code is empty", "Warning", MB_ICONEXCLAMATION)
      return Self
   endif

   ::Disable()
   ::PBar:Visible:=.t.
   ::EditQuery:Enabled:=.f.   
   ::myCloseTempFile(Self)   // clean up previous temporary file - if any
   ::Timer1:Start()  
   lFetch:=if( upper(left(cQuery,6))=="SELECT", .t., .f.) // "select" brings back a cursor with data, other SQL commands not   
   if lFetch .and. ( " INTO " $ upper(cQuery) )
      lFetch:=.f.  // select ... into ... doesn't bring back a cursor
   endif
// cursor is retained in a dBase III file, because here
// the cursor's total number of rows cannot be estimated
// in case of cursors which for sure contain a reduced number of rows
// a memorytable is faster - this depends on the user's free RAM
   if lFetch       // prepare temporary file for cursor data
      cTempName:=myTempFileName( @nTemp, "", "dbf")
      cTempFile:=cTempPath+if( empty(cTempPath), "", "\")+cTempName
      cTempAlias:="c"+ltrim(str(++nAlias,6))
      aTemp[1]:=cTempPath
      aTemp[2]:=cTempName
      aTemp[3]:=cTempAlias
      aTemp[4]:=cTempFile
      if file(cTempFile)
         try
            deletefile(cTempFile)
         catch oErr
            myError( oApp, oErr)
         end
      endif
   endif
   
   lOK:=.f.
   nSeconds:=Seconds()
   try                       // execute SQL command
      if lFetch
         #ifdef XDB_SQLRDD
            oSQL:Exec( cQuery, .T., .t., , cTempFile, cTempAlias, nMaxCursorExt)
         #endif
         select(cTempAlias)
         DBCloseArea()
      else
         #ifdef XDB_SQLRDD
            oSQL:Exec( cQuery, .t., .f.)
         #endif
      endif
      lOK:=.t.
   catch oErr
      ::MessageBox(oErr:SubSystem+str(oErr:SubCode,7)+" "+oErr:Operation+" "+oErr:Description,;
                   "SQL query problem",;
                   MB_ICONHAND)
   end   
   nSeconds:=mySeconds(nSeconds, Seconds())
   ::Timer1:Stop()
   
   if !lOK                             // failure
      if file(cTempFile)
         try
            deletefile(cTempFile)
         catch oErr
            myError( oApp, oErr)
         end
      endif
      ::PBar:Visible:=.f.
      ::Enable()
      return Self
   endif
      
   ::PBar:Visible:=.f.
   if !lFetch                     // no result data
      if lStatistics
         ::MessageBox( "The SQL command execution has been terminated in "+ltrim(str(nSeconds))+" seconds", "", MB_ICONINFORMATION)
      endif
      ::Enable()
      return Self
   endif
   
   FormCursor( ::this )   
   if lStatistics
      ::MessageBox( "The SQL command execution has been terminated in "+ltrim(str(nSeconds))+" seconds", "", MB_ICONINFORMATION)
   endif
      
   ::Enable()
      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolLoad_OnClick( Sender ) CLASS FormInstantQuery
   
   local cFile, cQuery, lOK, nPos
   ::EditQuery:Enabled:=.f.
// get file name      
   with object oApp:OpenFile
      :DefaultExt:="txt"
      :Filter:="Text files (*.txt|*.txt|All files ( *.* )|*.*"
      :InitialDirectory:=cIniPath
      :Title:="Select query source file to restore"
      :FileName:=""
      :Show()   
      cFile:=:FileName      
   end
   if empty(cFile)
      return Self
   else
      nPos:=RAt("\", cFile)
      cIniPath:=if(nPos>0, Left(cFile, nPos-1), "")
   endif
   if FileSize(cFile) < 5
      ::MessageBox( "The specified file is empty or too small", "", MB_ICONEXCLAMATION)
      return Self
   endif
// restore 
   ::Disable()
   lOK:=.f.         // read query source from text file
   try
      cQuery:=MemoRead(cFile)
      lOK:=.t.
   catch oErr
      myError( oApp, oErr)
   end
   if !lOK
      ::Enable()
      return Self
   endif
   cQuery:=StrTran( cQuery, chr(26))
   ::EditQuery:Caption:=cQuery
   ::Enable()
// close the temp file shown in grid
   ::myCloseTempFile(Self)
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myCloseTempFile( Sender ) CLASS FormInstantQuery
   if oCursor == NIL
      return Self
   Endif   
   ::Disable()
   oCursor:Close()
   ::Enable()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Timer1_OnTimeOut( Sender ) CLASS FormInstantQuery
   ::PBar:StepIt()
   Sender:Start()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextDB3_OnClick( Sender ) CLASS FormInstantQuery
   cCursorExport:="DBFNTX"
   ::myCursorExport(Self)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextDB4_OnClick( Sender ) CLASS FormInstantQuery
   cCursorExport:="DBFCDX"
   ::myCursorExport(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ContextADT_OnClick( Sender ) CLASS FormInstantQuery
   cCursorExport:="ADT"
   ::myCursorExport(Self)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myCursorExport( Sender ) CLASS FormInstantQuery
   local cFile, nPos, nSeconds, nADSErr, lOK
   local cExt:=if( left(cCursorExport,3)=="DBF", "DBF", "ADT")
   local cVIA:=cCursorExport
   if oCursor == NIL
      ::MessageBox( "There is no query result file opened", "Warning", MB_ICONEXCLAMATION)
      return Self
   endif
// read filename
   with object oApp:SaveFile
      :DefaultExt:=cExt
      :Filter:=if ( cExt=="DBF", "DBase files (*.dbf)|*.dbf|", "Advantage files (*.adt)|*.adt|" )+"All files ( *.* )|*.*"
      :InitialDirectory:=cIniPath
      :Title:="Save query result cursor as file..."
      :FileName:=""
      :Show()
      cFile:=:FileName
   end
   if empty(cFile)
      return Self
   else
      nPos:=RAt("\", cFile)
      cIniPath:=if(nPos>0, Left( cFile, nPos-1), "")   
   endif
// save query result
   ::Disable()
   lOK:=.f.
   nSeconds:=Seconds()
   ::PBar:Visible:=.t.
   ::Timer1:Start()
   try                       // save cursor data in permanent table
      select(oCursor:dt:Alias)           
      copy to (cFile) via (cVIA)      
      if cVIA == "ADT"
         nADSErr:=AdsGetLastError()
         if nADSErr > 0
            ::MessageBox( "Operation terminated with ADS error "+ltrim(str(nADSErr)), "", MB_ICONEXCLAMATION)
         endif
      endif
      lOK:=.t.
   catch oErr
      myError( oApp, oErr)
   end
   ::Timer1:Stop()
   nSeconds:=mySeconds( nSeconds, Seconds())
   ::PBar:Visible:=.f.
   if lStatistics
      ::MessageBox( "Query result saving terminated in "+ltrim(str(nSeconds))+" seconds", "", MB_ICONINFORMATION)
   endif
   ::Enable()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormInstantQuery_OnDestroy( Sender ) CLASS FormInstantQuery
   if lOpenTable
      select(oTable:Alias)
   endif   
RETURN Self