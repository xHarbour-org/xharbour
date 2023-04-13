#include "hbdll.ch"

// Native style
IMPORT HWND GetActiveWindow(VOID) FROM User32.dll
IMPORT int MessageBox( HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType ) FROM User32.dll //EXPORTED AS MessageBoxA

// FoxPro style Decleration
DECLARE Integer GetVolumeInformation IN Kernel32.dll ;
        String lpRootPathName, String @ lpVolumeNameBuffer, ;
        Integer nVolumeNameSize, Integer @ lpVolumeSerialNumber, ;
        Integer @ lpMaximumComponentLength, Integer @ lpFileSystemFlags, ;
        String @ lpFileSystemNameBuffer , Integer nFileSystemNameSize

// FWH style decleration
DLL FUNCTION ShellExecute( hWnd AS LONG, cOperation AS LPSTR,;
                             cFile AS LPSTR, cParams AS LPSTR,;
                             cDir AS LPSTR, nShow AS LONG ) AS LONG PASCAL ;
                             FROM "ShellExecuteA" LIB "shell32.dll"

#define VOLUME_BUFFER_SIZE 256

PROCEDURE Main()

  LOCAL cVolumeName := Space(VOLUME_BUFFER_SIZE)
  LOCAL nSerial     := 0
  LOCAL hParent := GetActiveWindow()

  //? hParent

  MessageBox( hParent, "Welcome to the fun world of xHarbour", "xHarbour Dll access to WinAPI", 0 )

  GetVolumeInformation( "C:\", @cVolumeName, VOLUME_BUFFER_SIZE, @nSerial, 0, 0, 0, 0 )
  ? "Volume:", Trim( cVolumeName ), "Serial:", NumToHex( nSerial, 8 )

  ShellExecute( 0, "Open", "http://www.xharbour.org/", 0 )

RETURN
