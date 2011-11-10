#include "vxh.ch"
#include "GridList.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD GridList_OnLoad( Sender ) CLASS GridList
   local lDeps := ::Params[2]
   local i, nRows, nCols, aText
   
   if !lDeps
      ::DataGrid1:DeleteColumn(6)
   endif
   
   aText := fparse( ::Params[1] )
   nRows := len(aText)
   if nRows > 2
      for i:=3 to nRows
         nCols := len( aText[i] )
         ::MemTab:Append()
         with object ::MemTab:Fields
            :A := aText[i, 1]
            :B := aText[i, 2]
            :C := aText[i, 3]
            :D := aText[i, 4]
            :E := aText[i, 5]
            if lDeps .and. nCols>5
               :F := aText[i, 6]
            endif
         end
      next
      ::MemTab:GoTop()
      ::DataGrid1:Update()
   else
      ::MessageBox( "The query result CSV file is empty or not available", "PowerShell Board", MB_OK|MB_ICONEXCLAMATION )
   endif
   
RETURN Self