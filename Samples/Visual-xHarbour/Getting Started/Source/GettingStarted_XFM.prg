#include "vxh.ch"
//---------------------------------------- End of system code ----------------------------------------//

//------------------------------------------------------------------------------------------------------------------------------------

CLASS __GettingStarted INHERIT Application
   // Components declaration
   METHOD Init() CONSTRUCTOR

   // Event declaration
ENDCLASS

METHOD Init( oParent, aParameters ) CLASS __GettingStarted
   ::Super:Init( oParent, aParameters )


   // Populate Components
   // Properties declaration
   ::Version              := "2.1.0.0"
   ::Resources            := {  }

   ::Create()

   // Populate Children
RETURN Self

