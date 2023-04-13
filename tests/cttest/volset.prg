/*
 * xHarbour CT Volume() function test
 * Author: Eduardo Fernandes <modalsist@yahoo.com.br>
 */

function main()

Local cDrive,cPrevVol,cVol

cSetSafety(.f.) // for tests only. if true the volume does not overwrite.

cls

cDrive := "A:\"
cVol   := space(11)

@10,05 say "Enter a drive letter to change volume : " get cDrive
@12,05 say "Enter the new volume name of drive    : " get cVol  
read

cPrevVol := GetVolInfo( cDrive )

if volume(cDrive+rtrim(cVol))
 ? "The previous volume "+rtrim(cPrevVol)+ " of drive "+cDrive+" was changed to "+getvolinfo(cDrive)
else
 ? "Error"
endif

return nil

