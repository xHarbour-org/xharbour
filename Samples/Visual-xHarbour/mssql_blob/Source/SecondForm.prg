GLOBAL EXTERNAL oSQL, aGrid, bGrid

#include "vxh.ch"
#include "SecondForm.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD SecondForm_OnLoad( Sender ) CLASS SecondForm
   ::MyDataGrid:DataSource:Table:=aGrid
   ::MyDataGrid:Update()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD SecondForm_OnDestroy( Sender ) CLASS SecondForm
   aGrid:=ASize( aGrid, 0 )
   bGrid:=ASize( bGrid, 0 )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS SecondForm
// display picture
   local cFile:=::Application:Path+"\man.jpg"
   local cCode:=::MyDataGrid:DataSource:Fields:CODE
   local nMaxL:=1, oError:=NIL
   local phrase:="select mval1 from test_b where code='"+cCode+"' and mval1 is not NULL"
   
   ::Disable()
   try
      oSQL:Exec(phrase, .t., .t., bGrid,,, nMaxL )
   catch oError
      ::Enable()
      ::MessageBox( oError:SubSystem+str(oError:SubCode,7)+" "+oError:Operation+" "+oError:Description, "Query image" )
      return Self
   end
   ::Enable()
      
   if len( bGrid ) < 1
      ::MessageBox( "No image loaded", "" )
      return Self
   endif
     
   if MemoWrit( cFile, bGrid[1,1], .f. ) == .f.
      ::MessageBox( "Problem writing temp file "+cFile, "" )
   else
      if FileSize( cFile ) >= 10
         try
            ::MyPictureBox:ImageName:=cFile
         catch oError
            ::MessageBox( oError:SubSystem+str(oError:SubCode,7)+" "+oError:Operation+" "+oError:Description, "Displaying image" )
         end
      else
         ::MessageBox( "Image too small to be displayed","" )
      endif
   endif
   
RETURN Self