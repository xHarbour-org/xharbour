
//What32
//AJ Wos

/*
This is the example code of creating "import" functions from a DLL
see import.ch for details of implementation, and also PPO of this file.
Using this technique, you load a DLL library on the application startup
and resolve the function address on the first call to each function.

On subsequent calls of that function, the function addresses will have 
been resolved, and will be very fast, just as if they were wrapped in 
the C code. 

The DLL library will be released on on application exit.

You can create separate sources (like this one) for each included DLL library.

The syntax have been puposefully arranged so, that you can grab the function
definitions almost directly from the DLL header file, and use it with minimal 
changes only.

Make sure that all parameter and return value types are resolved before use.
(see wintypes.ch)

*/


#Include "wintypes.ch"
#include "import.ch"

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

IMPORT WINMM    FUNCTION BOOL sndPlaySoundA(LPCSTR pszSound, UINT fuSound)
IMPORT WINSPOOL FUNCTION BOOL ClosePrinter( HANDLE hPrinter )
IMPORT WINSPOOL FUNCTION BOOL EndDocPrinter(HANDLE hPrinter)
IMPORT WINSPOOL FUNCTION BOOL EndPagePrinter(HANDLE hPrinter)
IMPORT KERNEL32 FUNCTION VOID ExitProcess( UINT uExitCode )

DLL  FUNCTION BOOL sndPlaySoundA(LPCSTR pszSound, UINT fuSound) as BOOL  LIB  WINMM
// etc.


