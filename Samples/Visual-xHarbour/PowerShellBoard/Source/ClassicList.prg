#include "vxh.ch"
#include "ClassicList.xfm"

#define CP_OEMCP 1
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD ClassicList_OnLoad( Sender ) CLASS ClassicList
   local cFile := ::Params[1]
   ::FillData( cFile )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FillData( cFile ) CLASS ClassicList
   local cWide, cAsc, nOut, nRes
   local i, nList, aList, cList
   
   ::MyListBox:ResetContent()
   cWide := memoread(cFile)
   nOut := len(cWide)/2
   cAsc := space( nOut )
   nRes := WideCharToMultiByte( CP_OEMCP, NIL, @cWide, Int(nOut), @cAsc, Int(nOut), NIL, NIL )
   
   aList := hb_regexall( "(?im).+", cAsc )
   nList:= len(aList)
   for i:=2 to nList
      cList := aList[i][1]
      if !empty( alltrim( cList ) )
         ::MyListBox:AddString( cList )
      endif
   next
RETURN Self