//
// $Id: procline.prg,v 1.1 1999/10/04 18:46:30 vszel Exp $
//


FUNCTION Main()

? "hello 1", ProcLine(), "Expected: ", 8

? "hello 2", ProcLine(), "Expected: ", 10

? "hello 3", ProcLine(), "Expected: ", 12

RETURN NIL
