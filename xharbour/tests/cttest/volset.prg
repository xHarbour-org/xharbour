/*
 * xHarbour CT Volume() function test
 * Author: Eduardo Fernandes <modalsist@yahoo.com.br>
 */

function main()

Local cDrive,cVol

cSetSafety(.f.) // for tests only. if true the volume does not overwrite.

cls

cDrive := "a"
cVol   := space(11)

@10,05 say "Enter a drive letter to change volume : " get cDrive
@12,05 say "Enter the new volume name of drive    : " get cVol  
read

// the trailing backslash is required.
cDrive += ":\"

? volume(cDrive+ltrim(cVol))

return nil


