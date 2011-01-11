// Typical Welcome message, from Windows!

// FWVERSION, FWCOPYRIGHT and FWDESCRIPTION are just
// some defines placed at FiveWin.ch to support three versions:
// Clipper, Xbase++ and [x]Harbour!

#include "FiveWin.ch"

//----------------------------------------------------------------------------//

function Main()

   MsgInfo( FWVERSION + Chr( 13 ) + FWCOPYRIGHT, FWDESCRIPTION )

return nil

//----------------------------------------------------------------------------//