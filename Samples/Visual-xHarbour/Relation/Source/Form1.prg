#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   
// general syntax:  SetRelation( oTableInto, Key, lAdditive )
// where Key might be a string containing the key expression, or a codeblock

    ::DataTable1:OrdSetFocus( "K_ID" )
    with object ::DataTable2
       :OrdSetFocus( "K_STATE" )
       :SetRelation( ::DataTable1, "ID" )
       :GoTop()
    end
    ::DataGrid1:Update()

RETURN Self