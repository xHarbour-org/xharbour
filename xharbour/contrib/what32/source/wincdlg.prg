
// WHAT32
// Common Dialog interface
#Define RCF_DIALOG     0      // used internally (user custom dialog class - advanced option)
#Include "commdlg.ch"
#Include "windows.ch"
/*
pragma(4)
#Include "ctruct.ch"
#Include "WinStruc.ch"
*/

// Under development !!!

*-----------------------------------------------------------------------------*

// FindText()
// don't forget to call RegisterWindowMessage(FINDMESSAGESTRING) before calling
// SYNTAX: FinText(<hWnd>,[<hInst>],<nFlags>,<cFindWhat>,[<bAction>]) -> hDlg
// isDialogMessage() will detect this dialog automatically, if called in auto mode

Function FindText( hWnd, hInst, nFlags, cFindWhat, bAction)

   LOCAL nIndex
   LOCAL n
   LOCAL aDialog := _Get_aDialog()
   LOCAL aWindow := _Get_aWindow()
   LOCAL hDlg

   // register the dialog

   If  ( nIndex := aScan( aDialog, { | x | x[ 1 ] == NIL } ) ) == 0
      aAdd( aDialog, { 0, bAction, 1 } )
      nIndex := Len( aDialog )
   Else
      aDialog[ nIndex ] := { 0, bAction, 1 }  // 0 means waiting...
   EndIf                                      // 1 means modal

   // we need to add it here too, to QUIT on the last window !!!
   // note type 0

   If ( n := aScan( aWindow, { | x | x[ 1 ] == NIL } ) ) == 0
       aAdd( aWindow, { 0, RCF_DIALOG, { } } )
       n := Len( aWindow )
   Else
      aWindow[ n ] := { 0, RCF_DIALOG, { } }  // window 0 means waiting ...
   EndIf

   // create the dialog
   hDlg := _FindText( hWnd, hInst, nFlags, cFindWhat) //, _GetDlgProc( ) )

   // if failed to create
   If hDlg == 0
      aDialog[ nIndex ] := { NIL , NIL, NIL }
      aWindow[ n ] := { NIL , NIL , { } }
      __KillWindow( )
   EndIf

   Return( hDlg )

*-----------------------------------------------------------------------------*

// FindText()
// don't forget to call RegisterWindowMessage(FINDMESSAGESTRING) before calling
// SYNTAX: ReplaceText(<hWnd>,[<hInst>],<nFlags>,<cFindWhat>,<cReplaceWith>,[<bAction>]) -> hDlg
// isDialogMessage() will detect this dialog automatically, if called in auto mode

Function ReplaceText( hWnd, hInst, nFlags, cFindWhat, cReplaceWith, bAction)

   LOCAL n
   LOCAL nIndex
   LOCAL aDialog := _Get_aDialog()
   LOCAL aWindow := _Get_aWindow()
   LOCAL hDlg

   // register the dialog

   If  ( nIndex := aScan( aDialog, { | x | x[ 1 ] == NIL } ) ) == 0
      aAdd( aDialog, { 0, bAction, 1 } )
      nIndex := Len( aDialog )
   Else
      aDialog[ nIndex ] := { 0, bAction, 1 }  // 0 means waiting...
   EndIf                                      // 1 means modal

   // we need to add it here too, to QUIT on the last window !!!
   // note type 0

   If ( n := aScan( aWindow, { | x | x[ 1 ] == NIL } ) ) == 0
       aAdd( aWindow, { 0, RCF_DIALOG, { } } )
       n := Len( aWindow )
   Else
      aWindow[ n ] := { 0, RCF_DIALOG, { } }  // window 0 means waiting ...
   EndIf

   // create the dialog
   hDlg := _ReplaceText( hWnd, hInst, nFlags, cFindWhat,cReplaceWith ) //, _GetDlgProc( ) )

   // if failed to create
   If hDlg == 0
      aDialog[ nIndex ] := { NIL , NIL, NIL }
      aWindow[ n ] := { NIL , NIL , { } }
      __KillWindow( )
   EndIf

   Return( hDlg )


*-----------------------------------------------------------------------------*

/*

GetOpenFileName( hWnd, @cPath, cTitle, aFilter, nFlags, cInitDir, cDefExt, nIndex)

hWnd:     Handle to parent window
cPath:    (optional) if OFN_ALLOWMULTISELECT the path is stored
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { {'Data Bases','*.dbf'},{'Clipper','*.prg'} }
nFlags:   OFN_* values default to OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. 'DBF'
nIndex:   Index position of types
 
Returns:  If OFN_ALLOWMULTISELECT
              Array of files selected
          else
              FileName.
          endif
 

*/

FUNCTION GetOpenFileName( hWnd, cPath, cTitle, aFilter, nFlags, cIniDir, cDefExt, nIndex )
local aFiles,cRet,cFile,n,x,c:=''
IF aFilter==nil
   aFilter:={}
END
IF ValType( aFilter ) == "A"
   FOR n:=1 TO LEN(aFilter)
       c+=aFilter[n][1]+chr(0)+aFilter[n][2]+chr(0)
   NEXT
ENDIF
if AND(nFlags,OFN_ALLOWMULTISELECT ) > 0
   cFile:=space(32000)
  ELSE
   cFile:=padr(trim(cPath),255,chr(0))
END
cRet:=_GetOpenFileName(hWnd, @cFile, cTitle, c, nFlags, cIniDir, cDefExt, @nIndex)
if AND(nFlags,OFN_ALLOWMULTISELECT ) > 0
   n:=AT(CHR(0)+CHR(0),cFile)
   cFile:=LEFT(cFile,n)
   aFiles:={}
   IF n==0 // no double chr(0) user must have pressed cancel
      RETURN(aFiles)
   END
   x:=AT(CHR(0),cFile) // fist null
   cPath:=LEFT(cFile,x)
   cFile:=STRTRAN(cFile,cPath)
   IF !EMPTY(cFile) // user selected more than 1 file
      c:=''
      FOR n:=1 TO LEN(cFile)
          IF SUBSTR(cFile,n,1)==CHR(0)
             AADD(aFiles,c)
             c:=''
             LOOP
          END
          c+=SUBSTR(cFile,n,1)
      NEXT
     ELSE
      cFile:=cPath
      x:=RAT('\',cFile)
      cPath:=LEFT(cFile,x-1)
      aFiles:={STRTRAN(STRTRAN(cFile,cPath),'\')}
   END
   Return(aFiles)
else
  cRet:=left(cRet,at(chr(0),cRet)-1)
end
Return(cRet)



*-----------------------------------------------------------------------------*

/*
GetSaveFileName( hWnd, cFile, cTitle, aFilter, nFlags, cInitDir, cDefExt, nIndex)
 
hWnd:     Handle to parent window
cFile:    (optional) Default FileName
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { {'Data Bases','*.dbf'},{'Clipper','*.prg'} }
nFlags:   OFN_* values default to OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. 'DBF'
nIndex:   Index position of types
 
Returns:  FileName.
*/


FUNCTION GetSaveFileName(hWnd, cFile, cTitle, aFilter, nFlags, cIniDir, cDefExt, nIndex )
local n,c:=''
IF aFilter==nil
   aFilter:={}
END
FOR n:=1 TO LEN(aFilter)
    c+=aFilter[n][1]+chr(0)+aFilter[n][2]+chr(0)
NEXT
cFile:=_GetSaveFileName(hWnd, cFile, cTitle, c, nFlags, cIniDir, cDefExt, @nIndex )
Return(cFile)









