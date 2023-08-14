#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   
// general syntax:  SetRelation( oTableInto, Key, lAdditive )
// where Key might be a string containing the key expression, or a codeblock

   LOCAL cPath
   
   cPath := Left( GetModuleFileName(), Rat("\" ,GetModuleFileName() )-1 )
         
   WITH OBJECT ::DataTable1
      :Filename := cPath+"\People.dbf"
      :Open()
   END
   
   WITH OBJECT ::DataTable2
      :Filename := cPath+"\Peopleex.dbf"
      :Open()
   END

    ::DataTable1:OrdSetFocus( "K_ID" )
    with object ::DataTable2
       :OrdSetFocus( "K_STATE" )
       :SetRelation( ::DataTable1, "ID" )
       :GoTop()
    end
    ::DataGrid1:Update()

RETURN Self