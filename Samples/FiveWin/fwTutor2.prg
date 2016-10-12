// Our first Window in FiveWin !!!

#include "FiveWin.ch"

//----------------------------------------------------------------------------//

function Main()

   local oWnd

   DEFINE WINDOW oWnd FROM 3, 6 TO 20, 70 ;
      TITLE "Welcome to " + FWDESCRIPTION COLOR "W/B"

   @ 2, 2 SAY "Hello world!"

   ACTIVATE WINDOW oWnd ;
      ON RIGHT CLICK MsgInfo( "Right Click" ) ;
      VALID MsgYesNo( "Are you sure?", "Do you want to exit?" )

return nil

//----------------------------------------------------------------------------//