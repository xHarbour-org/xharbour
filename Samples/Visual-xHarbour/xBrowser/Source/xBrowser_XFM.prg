#include "vxh.ch"
//---------------------------------------- End of system code ----------------------------------------//

//------------------------------------------------------------------------------------------------------------------------------------

CLASS __xBrowser INHERIT Application
   // Components declaration
   METHOD Init() CONSTRUCTOR

   // Event declaration
ENDCLASS

METHOD Init( oParent, aParameters ) CLASS __xBrowser
   ::Super:Init( oParent, aParameters )


   // Populate Components
   // Properties declaration
   ::Version              := "1.1.0.0"
   ::Resources            := {  }

   ::Create()

   // Populate Children
RETURN Self

