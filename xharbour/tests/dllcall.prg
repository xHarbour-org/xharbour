#include "hbdll.ch"

#define VOLUME_BUFFER_SIZE 256

PROCEDURE Main()

  LOCAL cVolumeName := Space(VOLUME_BUFFER_SIZE)
  LOCAL nSerial     := Int( 0 )

  GetVolumeInformation( "C:\", @cVolumeName, VOLUME_BUFFER_SIZE, @nSerial, 0, 0, 0, 0 )
  ? "Volume:", Trim( cVolumeName ), "Serial:", NumToHex( nSerial, 8 )

   ShellExecute( 0, 'Open', 'http://www.xharbour.org/', 0 )

RETURN

// FoxPro style Decleration
DECLARE Integer GetVolumeInformation IN Kernel32.dll ;
        String lpRootPathName, String @ lpVolumeNameBuffer, ;
        Integer nVolumeNameSize, Integer @ lpVolumeSerialNumber, ;
        Integer @ lpMaximumComponentLength, Integer @ lpFileSystemFlags, ;
        String @ lpFileSystemNameBuffer , Integer nFileSystemNameSize

// FWH style decleration
DLL32 FUNCTION ShellExecute( hWnd AS LONG, cOperation AS LPSTR,;
                             cFile AS LPSTR, cParams AS LPSTR,;
                             cDir AS LPSTR, nShow AS LONG ) AS LONG PASCAL ;
                             FROM "ShellExecuteA" LIB "shell32.dll"

