/*
 * xHarbour CT GetVolInfo() function test
 * Author: Eduardo Fernandes <modalsist@yahoo.com.br>
 */

function main()

Local cDrive

cls

cDrive := "c"

@10,5 say "Enter a drive letter to query volume : " get cDrive pict "!"
read

// note that trainling backslash is required.

@12,5 say "The volume of drive <"+upper(cDrive)+":> is "+GetVolInfo( cDrive+":\" )
 
return nil


