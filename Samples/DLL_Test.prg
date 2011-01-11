/*
 Note: build using DllTest.exe.xbp or make sure to link 
 against DllOne.lib and DllTwo.lib import libraries
 and import library of the DLL R/T.
*/

PROCEDURE _AppMain

   LOCAL oDllOne := DllOne( "My Text ONE!" )
   LOCAL oDllTwo := DllTwo( "My Text TWO!" )

   PUBLIC g_cExitMessage := "Exit from DllOne using PUBLIC!"

   ? oDllOne:cText
   ? oDllTwo:cText

   ? TestInterDllOne( {|| DllTwo( "From Dll ONE!" )} ):cText
   ? TestInterDllTwo( {|| DllOne( "From Dll TWO!" )} ):cText

   WAIT

RETURN
