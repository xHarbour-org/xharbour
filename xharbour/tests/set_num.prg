//
// $Id: set_num.prg,v 1.2 1999/10/16 03:28:56 dholm Exp $
//

// Testing SET

#include "set.ch"

function Main()
local n, cNewLine := HB_OSNewLine()

   for n := 1 to _SET_COUNT
      outstd (cNewLine)
      outstd (set (n))
   next

return nil
