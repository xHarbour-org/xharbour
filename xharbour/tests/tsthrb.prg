
/*

To run the test:

1 - Create a .hrb file: ..\bin\harbour.exe tstloop.prg -gh -n

2 - Build tsthrb.exe  : bldtest hrbtest

3 - Run the test      : hrbtest tstloop.hrb

*/


function main( cFile, cPar1, cPar2, cPar3, cPar4, cPar5, cPar6, cPar7, cPar8, cPar9 )

   local cBody, pHrb

   if file( cFile )
      cBody := memoread( cFile )

      pHrb  := __hrbLoad( cBody )
      __hrbDo( pHrb, cPar1, cPar2, cPar3, cPar4, cPar5, cPar6, cPar7, cPar8, cPar9 )
      __hrbUnload( pHrb )
   else
      ? "No .hrb file supplied" 
   endif

return
