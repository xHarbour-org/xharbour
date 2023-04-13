/*
 * xHarbour CT VolSerial() function test
 * Author: Eduardo Fernandes <modalsist@yahoo.com.br>
 */

function main()

Local cDrive,nSerial,cHex

cls

cDrive := "C:\"

@10,5 say "Enter a drive letter to query serial number : " get cDrive
read

// note that the trailing backslash is required.

nSerial := VolSerial( cDrive )

if nSerial != -1
  cHex := NumToHex( nSerial )
  ? Stuff(if(!empty(cHex),cHex,"00000000"),5,0,"-")
else
  alert("Drive "+Subs(cDrive,1,2)+" does not available.")
endif

? int(nSerial)

return nil
