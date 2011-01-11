#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD ToolStripButton1_OnClick( Sender ) CLASS Form1
   local cIndex:=::Application:Path + "\people.cdx"
   if !file( cIndex )
      ::MessageBox( "Please click 'Maintenance'", "Index not found" )
      return Self
   endif   
   Form2( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolStripButton2_OnClick( Sender ) CLASS Form1
   local cIndex:=::Application:Path + "\people.cdx"
   ::Disable()  // ignores user interaction with the application
// In real-life environments permanent indexes are maintained
// only when the table(s) are opened for exclusive usage.
// This code sequence serves solely for demonstrating the usage
// of Reindex(), and the simplest form of CreateOrder() method.
   if file( cIndex )
      if !::DataTable1:Shared
         ::DataTable1:Reindex()
      endif
   else
      with object ::DataTable1
         :CreateOrder( cIndex, "K_ID", "id" )
         :CreateOrder( cIndex, "K_NAME", "name" )
      end
   endif
   ::Enable()
   ::MessageBox( "End of maintenance", "" )
RETURN Self