#include "vxh.ch"
#include "Form2.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD Form2_OnLoad( Sender ) CLASS Form2
// initializing the three user variables declared for Form1
   ::MyTable:=::Application:MainForm:DataTable1
   ::MyTmpIndex:=::Application:Path + "\tmpindex.cdx"
   ::MyTmpTag:="tmpindex"

// we are going to use the two index tags contained by people.cdx,
// and additionally a temporary index of payments, which is created
// when the user opens Form2, and deleted when the user closes Form2.
   if file( ::MyTmpIndex )
      if deletefile( ::MyTmpIndex ) > 0
         ::MessageBox( "File "+::MyTmpIndex+" couldn't be deleted", "error" )
         ::Close()
         return Self
      endif
   endif

// creating the temporary index,
// setting the initially active index tag
   with object ::MyTable
      :CreateIndex( ::MyTmpIndex, "payment" )
      :OrdSetFocus( "K_NAME" )
      :GoTop()
   end
// Note: the CreateIndex(...) method creates a single tag
// in each physical file, and the file name is used as tag name
   
   ::DataGrid1:Update()
   ::Helper()     // demo only
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn1_OnHeaderClick( Sender ) CLASS Form2
// setting permanent index tag K_ID as active index   
   ::MyTable:OrdSetFocus( "K_ID" )
   ::DataGrid1:Update()
   ::Helper()     // demo only
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn2_OnHeaderClick( Sender ) CLASS Form2
// setting permanent index tag K_NAME as active index   
   ::MyTable:OrdSetFocus( "K_NAME" )
   ::DataGrid1:Update()      
   ::Helper()     // demo only
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn3_OnHeaderClick( Sender ) CLASS Form2
// setting the temporary index tag as active index   
   ::MyTable:OrdSetFocus( ::MyTmpTag )
   ::DataGrid1:Update()   
   ::Helper()     // demo only
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form2_OnDestroy( Sender ) CLASS Form2
// OnDestroy is always fired, regardless of how Form2 is being closed.
// OrdListClear() closes all indexes, except the structural one.
   ( ::MyTable:Alias )->( OrdListClear() )
   if DeleteFile( ::MyTmpIndex ) > 0
      ::MessageBox( "File "+::MyTmpIndex+" couldn't be deleted", "warning" )
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Helper() CLASS Form2
   local nPos:=::MyTable:IndexOrd()
   ::LabelNr:Caption:=str( nPos )
   ::LabelName:Caption:=::MyTable:OrdName( nPos )
RETURN Self