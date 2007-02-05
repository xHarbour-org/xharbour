/*

To run the test:

1 - Create a .hrb file: ..\bin\harbour.exe tstloop.prg -gh -n

2 - Build tsthrb.exe  : bldtest hrbtest

3 - Run the test      : hrbtest tstloop.hrb

*/

#include "hbextern.ch"

PROCEDURE Main( cFile, cPar1, cPar2, cPar3, cPar4, cPar5, cPar6, cPar7, cPar8, cPar9 )

   LOCAL pHrb

   IF File( cFile )
      pHrb  := __hrbLoad( cFile )
      __hrbDo( pHrb, cPar1, cPar2, cPar3, cPar4, cPar5, cPar6, cPar7, cPar8, cPar9 )
      __hrbUnload( pHrb )
   ELSE
      ? "No .hrb file supplied" 
   ENDIF
   
RETURN