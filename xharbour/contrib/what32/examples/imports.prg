
#include "c:\what32\include\import.ch"
#Include "c:\what32\include\WinTypes.ch"


STATIC USER32
STATIC GDI32
STATIC COMCTL32
STATIC KERNEL32
STATIC WINSPOOL
STATIC WINMM

INIT PROCEDURE LoadLibs()
   USER32   := LoadLibrary("USER32")
   GDI32    := LoadLibrary("GDI32")
   COMCTL32 := LoadLibrary("COMCTL32")
   KERNEL32 := LoadLibrary("Kernel32")
   WINSPOOL := LoadLibrary("WINSPOOL.DRV")
   WINMM    := LoadLibrary("WINMM")
RETURN

EXIT PROCEDURE FreeLibs
   FreeLibrary(USER32)
   FreeLibrary(GDI32)
   FreeLibrary(COMCTL32)
   FreeLibrary(KERNEL32)
   FreeLibrary(WINSPOOL)
   FreeLibrary(WINMM)
RETURN

// MCI

#define MCIERROR    DWORD
#define MCIDEVICEID UINT

IMPORT WINMM FUNCTION BOOL        mciExecute(LPCSTR cCmd ) // obsolete
IMPORT WINMM FUNCTION BOOL        mciSendStringA(LPCSTR lpstrCommand, LPSTR lpstrReturnString, UINT uReturnLength, HWND hwndCallback) AS mciSendString
IMPORT WINMM FUNCTION BOOL        sndPlaySoundA(LPCSTR pszSound, UINT fuSound) AS sndPlaySound
IMPORT WINMM FUNCTION MCIERROR    mciSendCommandA(MCIDEVICEID mciId, UINT uMsg, DWORD dwParam1, DWORD dwParam2) AS mciSendCommand
IMPORT WINMM FUNCTION MCIDEVICEID mciGetDeviceIDA(LPCSTR pszDevice) AS mciGetDeviceID
IMPORT WINMM FUNCTION MCIDEVICEID mciGetDeviceIDFromElementIDA(DWORD dwElementID, LPCSTR lpstrType ) AS mciGetDeviceIDFromElementID
IMPORT WINMM FUNCTION BOOL        mciGetErrorStringA(MCIERROR mcierr, LPSTR pszText, UINT cchText) AS mciGetErrorString

// SPOOLER

IMPORT WINSPOOL FUNCTION BOOL ClosePrinter( HANDLE hPrinter )
IMPORT WINSPOOL FUNCTION BOOL EndDocPrinter(HANDLE hPrinter)
IMPORT WINSPOOL FUNCTION BOOL EndPagePrinter(HANDLE hPrinter)




// etc.

