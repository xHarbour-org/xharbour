GLOBAL EXTERNAL oApp, oErr, oSQL
GLOBAL EXTERNAL cEngine, nMaxCursor, aCursor
GLOBAL EXTERNAL cSchema
GLOBAL EXTERNAL nCursorCalc, nCursorNormal
GLOBAL EXTERNAL cFontFace, cFontSize, cBackColor, cForeColor
GLOBAL EXTERNAL cHighlightColor, cHighlightTextColor
GLOBAL EXTERNAL hFontFace, hFontSize, hBack, hFore
GLOBAL EXTERNAL hHighlight, hHighlightText

#include "vxh.ch"
#include "FormEngineInfo.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD FormEngineInfo_OnLoad( Sender ) CLASS FormEngineInfo
  ::Grid:Visible:=.f. 
  ::Grid:DataSource:=if( cEngine=="MYSQL", ::mtmy, ::mtfb)
  ::Grid:AutoAddColumns()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myInitGrid( Sender ) CLASS FormEngineInfo
// clean up grid and display the last query result cursor   
   ::Grid:DataSource:Table:=aCursor
   ::Grid:Font:FaceName:=HFontFace[cFontFace]
   ::Grid:Font:PointSize:=hFontSize[cFontSize]
   ::Grid:BackColor:=hBack[cBackColor]
   ::Grid:ForeColor:=hFore[cForeColor]
   ::Grid:HighlightColor:=hHighlight[cHighlightColor]      
   ::Grid:HighlightTextColor:=hHighlightText[cHighlightTextColor]      
   ::Grid:Gotop()
   ::Grid:Update()
   ::Grid:Visible:=.t.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolButtonTables_OnClick( Sender ) CLASS FormEngineInfo
// displays metadata about schema tables
   local cQuery
   
   if cEngine == "MYSQL"
      cQuery:="select left(table_name,32), "+;
                     "left(table_type,32), "+;
                     "convert(table_rows, char(32)), "+;
                     "convert(avg_row_length*table_rows, char(32)), "+;
                     "convert(create_time, char(32)), "+;
                     "convert(update_time, char(32)), "+;
                     "left(table_collation,32) "+;
               "from information_schema.tables "+;
               "where table_schema='"+cSchema+"' and table_name not like 'sr_mgmnt%' "+;
               "order by table_name"
   elseif cEngine == "FB"
      cQuery:="select RDB$RELATION_NAME, "+;
                     "cast(RDB$FIELD_ID as CHAR(32)), "+;
                     "RDB$OWNER_NAME, "+;
                     "RDB$SECURITY_CLASS "+;
               "from RDB$RELATIONS "+;
               "where RDB$VIEW_BLR IS NULL and RDB$SYSTEM_FLAG=0 and RDB$RELATION_NAME not like 'SR_MGMNT%' "+;
               "order by RDB$RELATION_NAME"
   elseif cEngine == "MS"
      cQuery:="select left(name, 32), "+;
                     "crdate, "+;
                     "type_txt = case type when 'U' then 'Table' else 'View' end, "+;
                     "ltrim(str(schema_ver,12)) "+;
              "from sysobjects "+;
              "where (type='U' or type='V') and name not like 'SR_MGMNT%' "+;
              "order by name"
   endif

   ::Cursor:=nCursorCalc
   ::Disable()
   ::Grid:Visible:=.f.
   ::Caption:="Schema Informations"
   #ifdef XDB_SQLRDD
      ASize( aCursor, 0)
      oSQL:Exec( cQuery, .f., .t., @aCursor, , ,nMaxCursor )
   #endif
   if len(aCursor) < 1
      ::MessageBox("There are no tables in the current schema","",MB_OK)
      ::Enable()
      ::Cursor:=nCursorNormal
      return Self
   endif
   
   if cEngine == "MYSQL"     // update headers
      ::GridColumn1:Caption:="Table name"
      ::GridColumn2:Caption:="Table type"
      ::GridColumn3:Caption:="Rows"
      ::GridColumn4:Caption:="Table Length"
      ::GridColumn5:Caption:="Create time"
      ::GridColumn6:Caption:="Update time"
      ::GridColumn7:Caption:="Table collation"
   elseif cEngine == "FB"
      ::GridColumn1:Caption:="Table name"
      ::GridColumn2:Caption:="Field ID"
      ::GridColumn3:Caption:="Owner name"
      ::GridColumn4:Caption:="Security class"
   elseif cEngine == "MS"
      ::GridColumn1:Caption:="Table name"
      ::GridColumn2:Caption:="Table creation date"
      ::GridColumn3:Caption:="Table type"
      ::GridColumn4:Caption:="Schema version"
   endif

   ::Caption:="Schema Informations - Tables"
   ::myInitGrid(Self)   // refresh grid content   
   ::Enable()
   ::Cursor:=nCursorNormal

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolButtonColumns_OnClick( Sender ) CLASS FormEngineInfo
// displays metadata about schema columns
   local cQuery
   local nw:=hFontSize[cFontSize]
   
   if cEngine == "MYSQL"
      cQuery:="select left(table_name,32), "+;
                     "left(column_name,32), "+;
                     "left(data_type,32), "+;
                     "left(column_type,32), "+;
                     "left(column_key,32), "+;
                     "left(character_set_name,32), "+;
                     "left(collation_name,32) "+;
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
                           "END AS field_type, "+;
                        "coll.RDB$COLLATION_NAME AS field_collation "+;
              "FROM RDB$RELATION_FIELDS r "+;
                     "LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME "+;
                     "LEFT JOIN RDB$COLLATIONS coll ON r.RDB$COLLATION_ID = coll.RDB$COLLATION_ID "+;
                               "AND f.RDB$CHARACTER_SET_ID = coll.RDB$CHARACTER_SET_ID "+;
              "WHERE f.RDB$SYSTEM_FLAG=0 and r.RDB$RELATION_NAME not like 'SR_MGMNT%' "+;
              "ORDER BY r.RDB$RELATION_NAME"
   elseif cEngine == "MS"
      cQuery:="select left(s.table_name, 32), "+;
                     "left(s.column_name, 32), "+;
                     "left(s.data_type, 32), "+;
                     "left(s.collation_name, 32) "+;
              "from information_schema.columns s, sysobjects o "+;
              "where object_id(s.table_name)=o.id and o.type='U' and s.table_name not like 'SR_MGMNT%' "+;
              "order by s.table_name, s.column_name"
   endif

   ::Cursor:=nCursorCalc
   ::Disable()   
   ::Grid:Visible:=.f. 
   ::Caption:="Schema Informations"
   #ifdef XDB_SQLRDD
      ASize( aCursor, 0)   
      oSQL:Exec( cQuery, .f., .t., @aCursor, , ,nMaxCursor )
   #endif
   if len(aCursor) < 1
      ::MessageBox("There are no columns in the current schema","",MB_OK)
      ::Enable()
      ::Cursor:=nCursorNormal
      return Self
   endif
   
   if cEngine == "MYSQL"      // update headers
      ::GridColumn1:Caption:="Table name"
      ::GridColumn2:Caption:="Column name"
      ::GridColumn3:Caption:="Data type"
      ::GridColumn4:Caption:="Column type"
      ::GridColumn5:Caption:="Column key"
      ::GridColumn6:Caption:="Character set name"
      ::GridColumn7:Caption:="Collation name"
   elseif cEngine == "FB"
      ::GridColumn1:Caption:="Table name"
      ::GridColumn2:Caption:="Column name"
      ::GridColumn3:Caption:="Data type"
      ::GridColumn4:Caption:="Column collation"
   elseif cEngine == "MS"
      ::GridColumn1:Caption:="Table name"
      ::GridColumn2:Caption:="Column name"
      ::GridColumn3:Caption:="Data type"
      ::GridColumn4:Caption:="Collation name"
   endif
   
   ::Caption:="Schema Informations - Columns"   
   ::myInitGrid(Self)     // refresh Grid content   
   ::Enable()
   ::Cursor:=nCursorNormal

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolButtonIndexes_OnClick( Sender ) CLASS FormEngineInfo
// displays metadata about index statistics   
   local cQuery
   local nw:=hFontSize[cFontSize]
   
   if cEngine == "MYSQL"
      cQuery:="select left(table_name,32), "+;
                     "left(column_name,32), "+;
                     "left(index_name,32), "+;
                     "convert(non_unique,char(32)), "+;
                     "convert(seq_in_index,char(32)), "+;
                     "left(collation,32), "+;
                     "convert(cardinality,char(32)) "+;
               "from information_schema.statistics "+;
               "where table_schema='"+cSchema+"' and table_name not like 'sr_mgmnt%' "+;
               "order by table_name"
   elseif cEngine == "FB"
      cQuery:="select RDB$RELATION_NAME, "+;
                     "RDB$INDEX_NAME, "+;
                     "cast(RDB$UNIQUE_FLAG as char(32)), "+;
                     "cast(RDB$STATISTICS as char(32)) "+;
               "from RDB$INDICES "+;
               "where RDB$SYSTEM_FLAG IS NULL and RDB$RELATION_NAME NOT LIKE 'SR_MGMNT%' "+;
               "order by RDB$RELATION_NAME"
   elseif cEngine == "MS"
      cQuery:="select left(o.name, 32), "+;
                     "left(i.name, 32), "+;
                     "ltrim(str(i.rows,12)), "+;
                     "ltrim(str(i.keycnt,12)) "+;
              "from sysobjects o, sysindexes i "+;
              "where i.id=o.id and o.type='U' "+;
              "and o.name not like 'SR_MGMNT%' "+;
              "order by o.name, i.name"
   endif

   ::Cursor:=nCursorCalc
   ::Disable()   
   ::Grid:Visible:=.f. 
   ::Caption:="Schema Informations"
   #ifdef XDB_SQLRDD
      aSize( aCursor, 0)
      oSQL:Exec(cQuery, .f., .t., @aCursor, , ,nMaxCursor)
   #endif
   if len(aCursor) < 1
      ::MessageBox("There are no indexes in the current schema","",MB_OK)
      ::Enable()
      ::Cursor:=nCursorNormal
      return Self
   endif
   
   if cEngine == "MYSQL"      // update headers
      ::GridColumn1:Caption:="Table name"
      ::GridColumn2:Caption:="Column name"
      ::GridColumn3:Caption:="Index name"
      ::GridColumn4:Caption:="Non_unique"
      ::GridColumn5:Caption:="Seq_in_index"
      ::GridColumn6:Caption:="Collation"
      ::GridColumn7:Caption:="Cardinality"
   elseif cEngine == "FB"
      ::GridColumn1:Caption:="Table name"
      ::GridColumn2:Caption:="Index name"
      ::GridColumn3:Caption:="Unique flag"
      ::GridColumn4:Caption:="Statistics"
   elseif cEngine == "MS"
      ::GridColumn1:Caption:="Table name"
      ::GridColumn2:Caption:="Index name"
      ::GridColumn3:Caption:="Number of rows"
      ::GridColumn4:Caption:="Key count"
   endif
   
   ::Caption:="Schema Informations - Indexes"
   ::myInitGrid(Self)          // refresh Grid content   
   ::Enable()
   ::Cursor:=nCursorNormal

RETURN Self