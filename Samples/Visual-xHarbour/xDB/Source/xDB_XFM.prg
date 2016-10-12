#include "vxh.ch"
//---------------------------------------- End of system code ----------------------------------------//

//------------------------------------------------------------------------------------------------------------------------------------

CLASS __xDB INHERIT Application
   // Components declaration
   METHOD Init() CONSTRUCTOR

   // Event declaration
ENDCLASS

METHOD Init( oParent, aParameters ) CLASS __xDB
   ::Super:Init( oParent, aParameters )


   // Populate Components
   // Properties declaration
   ::Resources            := {  }

   ::Create()

   // Populate Children
RETURN Self

