#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn1_OnHeaderClick( Sender ) CLASS Form1
// when the user clicks the ID column header   
// activate index tag with name K_ID   
   ::MyTable:OrdSetFocus( "K_ID" )
   ::MyGrid:Update()
   ::HelperMethod()  // demo only
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn2_OnHeaderClick( Sender ) CLASS Form1
// when the user clicks the NAME column header   
// activate index tag with name K_NAME   
   ::MyTable:OrdSetFocus( "K_NAME" )   
   ::MyGrid:Update()
   ::HelperMethod()   // demo only
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
// Just to demonstrate how to open an index file...
// In case of using more not structural index files,
// there will be a SetIndex(...) method calls for each of them
// When :SetAutopen is turned on - by default -
// the table's structural index is opened automatically.
   if !::Application:SetAutopen
      ::MyTable:SetIndex( ::Application:Path+"\people.cdx" )
   endif
// set the initially active index tag
   ::MyTable:OrdSetFocus( "K_NAME" )
   ::MyTable:GoTop()
   ::MyGrid:Update()
// when MyTable is updated, all the opened indexes
// are automatically updated, no need of special care
   ::HelperMethod()  // demo only
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonNoOrd_OnClick( Sender ) CLASS Form1
// ::MyTable:SetOrder( N ) sets the N'th opened index tag,
// use N=0 to set the physical record order   
   ::MyTable:SetOrder( 0 )
   ::MyGrid:Update()
   ::HelperMethod()    // demo only
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD HelperMethod() CLASS Form1   
   local nActive:=::MyTable:IndexOrd() // opened index tag number
   ::LabelNr:Caption:=str(nActive)
   ::LabelName:Caption:=::MyTable:OrdName( nActive ) // tag name
RETURN Self