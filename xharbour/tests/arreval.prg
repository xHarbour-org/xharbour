//
// $Id: arreval.prg,v 1.1 1999/10/04 18:46:26 vszel Exp $
//

function Main()

   local a := { 100, 200, 300 }

   aEval(a, {|nValue, nIndex| QOut(nValue, nIndex) })

return nil

