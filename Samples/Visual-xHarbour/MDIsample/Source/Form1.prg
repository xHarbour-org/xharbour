#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS Form1
   local nPos := ascan( ::MDIClient:Children, ::oOne )
   if nPos > 0
      ::oOne:Restore()
   else
      ::oOne := Form2( ::this, { "One", "First" } )   
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Button2_OnClick( Sender ) CLASS Form1
   local nPos := ascan( ::MDIClient:Children, ::oTwo )
   if nPos > 0
      ::oTwo:Restore()
   else
      ::oTwo := Form2( ::this, { "Two", "Second" } )   
   endif
RETURN Self