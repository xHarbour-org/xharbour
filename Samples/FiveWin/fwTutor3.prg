// Our first Window, now with Control events examples and with a message bar

#include "FiveWin.ch"

//----------------------------------------------------------------------------//

function Main()

   local oWnd, oBrush

   DEFINE BRUSH oBrush FILENAME "..\bitmaps\Back1.bmp"

   DEFINE WINDOW oWnd TITLE "FiveWin" ;
      FROM 3, 6 TO 20, 70 ;
      BRUSH oBrush

   SET MESSAGE OF oWnd TO FWVERSION + " " + FWCOPYRIGHT // Strings defined
                                                        // inside FiveWin.ch
   ACTIVATE WINDOW oWnd ;
      ON LEFT CLICK MsgInfo( "Left Click", "Event!" ) ;
      ON RESIZE MsgAlert( "ReSize", "Event!" ) ;
      ON PAINT  oWnd:Say( 2, 2, "Hello World!" )

return nil

//----------------------------------------------------------------------------//