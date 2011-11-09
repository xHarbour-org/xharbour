/*
 * $Id$
 */
/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com

  Authors: Ron Pinkas <Ron@xHarbour.com>
           Augusto Infante <August@Winbuilt.com>

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/

// RON (702) 914 2731

#pragma comment( lib, "comctl32.lib" )
#pragma comment( lib, "comdlg32.lib" )
#pragma comment( lib, "gdi32.lib" )
#pragma comment( lib, "shell32.lib" )
#pragma comment( lib, "winmm.lib" )
#pragma comment( lib, "lz32.lib" )
#pragma comment( lib, "Netapi32.lib" )
#pragma comment( lib, "Msimg32.lib" )
#pragma comment( lib, "mapi32.lib" )
#pragma comment( lib, "wininet.lib" )
#pragma comment( lib, "shlwapi.lib" )
#pragma comment( lib, "wincore.lib" )
#pragma comment( lib, "version.lib")
#pragma comment( lib, "Rpcrt4.lib")


#define _WIN32_WINNT 0x0500

#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

#include "item.api"
#include "hbdefs.h"
#include "hbvmpub.h"
#include "hbinit.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbvm.h"
#include "hbapierr.h"
#include "hbpcode.h"
#include "hbstack.h"

// Not a mistake MUST precede windows.h because it then #includes winsock.h
#ifndef __XCC__
   #include <winsock2.h>
#endif

#include <windows.h>
#include <commdlg.h>
#include <shlobj.h>
#include <shellapi.h>
#include <lm.h>
#include <winreg.h>
#include <mapi.h>
#include <oledlg.h>
#include <wininet.h>
#include <psapi.h>
#include <shlwapi.h>
#include <Rpc.h>
#include <ole2.h>
#include <wincrypt.h>

typedef struct _DWM_BLURBEHIND {
    DWORD dwFlags;
    BOOL fEnable;
    HRGN hRgnBlur;
    BOOL fTransitionOnMaximized;
} DWM_BLURBEHIND, *PDWM_BLURBEHIND;

typedef struct _MARGINS {
    int cxLeftWidth;
    int cxRightWidth;
    int cyTopHeight;
    int cyBottomHeight;
} MARGINS, *PMARGINS;

static PHB_DYNS pHB_CSTRUCTURE = NULL, pPOINTER, pVALUE, pDEVALUE;
static HINSTANCE hCryptUI = NULL, hCrypt32 = NULL, hUser32 = NULL, hShell32 = NULL, hGdi32 = NULL, hNetAPI32 = NULL, hWinmm = NULL, hAdvApi32 = NULL, hHtmlHelp = NULL, hMAPI = NULL, hOleDlg = NULL, hAviCap = NULL, hWininet = NULL, hPSAPI = NULL, hshlwapi = NULL, hKernel32 = NULL, hdwmapi = NULL;

static CRITICAL_SECTION s_cs; /* This is the critical section object -- once initialized, it cannot be moved in memory */

#define WINAPI_DEBUG_BUFFER_LEN 8192
static char s_sBuffer[ WINAPI_DEBUG_BUFFER_LEN ];

HB_EXTERN_BEGIN

extern HB_EXPORT LPSTR hb_oleWideToAnsi( const BSTR wString );
extern HB_EXPORT LPWSTR hb_oleAnsiToWide( const char *cString );
extern HB_EXPORT BSTR hb_oleAnsiToSysString( const LPSTR cString );
extern HB_EXPORT PHB_ITEM hb_hashGetValueAt( PHB_ITEM pHash, ULONG ulPos );

HB_EXTERN_END

#if defined( __XCC__ )
typedef struct {
    LPITEMIDLIST pidl;
    BOOL fRecursive;
} SHChangeNotifyEntry;

typedef struct {
   BOOL bRoot;
   LPITEMIDLIST lpi;
   LPSHELLFOLDER lpsfParent;
   LPITEMIDLIST lpifq;
   IShellFolder *pRoot;
}LPTVITEMDATA;

typedef struct _PERFORMANCE_INFORMATION {
   DWORD cb;
   ULONG CommitTotal;
   ULONG CommitLimit;
   ULONG CommitPeak;
   ULONG PhysicalTotal;
   ULONG PhysicalAvailable;
   ULONG SystemCache;
   ULONG KernelTotal;
   ULONG KernelPaged;
   ULONG KernelNonpaged;
   ULONG PageSize;
   DWORD HandleCount;
   DWORD ProcessCount;
   DWORD ThreadCount;
} PERFORMANCE_INFORMATION,  *PPERFORMANCE_INFORMATION;
#endif

#ifndef NO_PRG_CODE
   #define __PRG_SOURCE__ __FILE__
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER HB_PCODE_VER

   HB_FUNC_INIT( _INITSYMBOLS_ );
   HB_FUNC_EXIT( _CLEANUP_ );

   HB_FUNC( ALERT );
   HB_FUNC( MESSAGEBOX );
   HB_FUNC( GETMODULEFILENAME );

   HB_FUNC_EXTERN( VALTYPE );
   HB_FUNC_EXTERN( STRTRAN );
   HB_FUNC_EXTERN( AEVAL );

   HB_INIT_SYMBOLS_BEGIN( winapi__InitSymbols )
   { "_INITSYMBOLS_$",  {HB_FS_INIT}, {HB_INIT_FUNCNAME( _INITSYMBOLS_ )}, &ModuleFakeDyn },
   { "ALERT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ALERT )}, NULL },
   { "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
   { "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
   { "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
   { "MESSAGEBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MESSAGEBOX )}, NULL },
   { "GETMODULEFILENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETMODULEFILENAME )}, NULL },
   { "_CLEANUP_$",  {HB_FS_EXIT}, {HB_EXIT_FUNCNAME( _CLEANUP_ )}, &ModuleFakeDyn }
   HB_INIT_SYMBOLS_END( winapi__InitSymbols )

   #if defined(HB_PRAGMA_STARTUP)
      #pragma startup winapi__InitSymbols
   #elif defined(_MSC_VER)
      #if _MSC_VER >= 1010
         #pragma data_seg( ".CRT$XIY" )
         #pragma comment( linker, "/Merge:.CRT=.data" )
      #else
         #pragma data_seg( "XIY" )
      #endif
      static HB_$INITSYM hb_vm_auto_winapi__InitSymbols = winapi__InitSymbols;
      #pragma data_seg()
   #endif
#endif

#define CRYPTUI_SELECT_ISSUEDTO_COLUMN  0x000000001
#define CRYPTUI_SELECT_ISSUEDBY_COLUMN  0x000000002
#define CRYPTUI_SELECT_INTENDEDUSE_COLUMN  0x000000004
#define CRYPTUI_SELECT_FRIENDLYNAME_COLUMN  0x000000008
#define CRYPTUI_SELECT_LOCATION_COLUMN  0x000000010
#define CRYPTUI_SELECT_EXPIRATION_COLUMN  0x000000020

static BOOL (WINAPI *pQueryServiceStatusEx)(SC_HANDLE,SC_STATUS_TYPE,LPBYTE,DWORD,LPDWORD)                     = NULL;
static BOOL (WINAPI *pQueryServiceStatus)(SC_HANDLE,LPSERVICE_STATUS)                                          = NULL;
static BOOL (WINAPI *pQueryServiceConfig)(SC_HANDLE,LPQUERY_SERVICE_CONFIG,DWORD,LPDWORD)                      = NULL;
static BOOL (WINAPI *pQueryServiceConfig2)(SC_HANDLE,DWORD,LPBYTE,DWORD,LPDWORD)                               = NULL;
static BOOL (WINAPI *pChangeServiceConfig2)(SC_HANDLE,DWORD,LPVOID)                                            = NULL;

static HCERTSTORE (WINAPI *pCertOpenSystemStore)(HCRYPTPROV,LPCSTR)                                            = NULL;

static PCCERT_CONTEXT (WINAPI *pCryptUIDlgSelectCertificateFromStore)(HCERTSTORE,HWND,LPCWSTR,LPCWSTR,DWORD,DWORD,LPVOID) = NULL;
static BOOL (WINAPI* pCertFreeCertificateContext)(PCCERT_CONTEXT)                                              = NULL;
static BOOL (WINAPI* pCertCloseStore)(HCERTSTORE,DWORD)                                                        = NULL;
static DWORD (WINAPI* pCertNameToStr)(DWORD,PCERT_NAME_BLOB,DWORD,LPTSTR,DWORD)                                = NULL;
static DWORD (WINAPI* pCertGetNameString)(PCCERT_CONTEXT,DWORD,DWORD,LPVOID,LPTSTR,DWORD)                      = NULL;

static BOOL (WINAPI *pEndTask)(HWND,BOOL,BOOL)                                                                 = NULL;
static BOOL (WINAPI *pIsHungAppWindow)(HWND)                                                                   = NULL;
static HWND (WINAPI *pGetAncestor)(HWND,UINT)                                                                  = NULL;
static BOOL (WINAPI *pTrackMouseEvent)(LPTRACKMOUSEEVENT)                                                      = NULL;
static BOOL (WINAPI *pGetScrollBarInfo)(HWND,LONG,PSCROLLBARINFO)                                              = NULL;
static HWND (WINAPI *pRealChildWindowFromPoint)(HWND,POINT)                                                    = NULL;
static VOID (WINAPI *pSwitchToThisWindow)(HWND,BOOL)                                                           = NULL;
static BOOL (WINAPI *pSetLayeredWindowAttributes)(HWND,COLORREF,BYTE,DWORD)                                    = NULL;
static BOOL (WINAPI *psndPlaySound)(LPCSTR,UINT)                                                               = NULL;

static ULONG (WINAPI *pSHChangeNotifyRegister)(HWND,int,LONG,UINT,int,SHChangeNotifyEntry*)                    = NULL;
static BOOL  (WINAPI *pSHChangeNotifyDeregister)(ULONG)                                                        = NULL;

static HRESULT (WINAPI *pSHGetFolderPath)(HWND,int,HANDLE,DWORD,LPTSTR)                                        = NULL;
static HRESULT (WINAPI *pSHGetFolderPathAndSubDir)(HWND,int,HANDLE,DWORD,LPCTSTR,LPTSTR)                       = NULL;
static HRESULT (WINAPI *pSHILCreateFromPath)(LPCWSTR, ITEMIDLIST*, DWORD*)                                     = NULL;

static COLORREF (WINAPI *pGetDCBrushColor)(HDC)                                                                = NULL;
static COLORREF (WINAPI *pSetDCBrushColor)(HDC,COLORREF)                                                       = NULL;
static COLORREF (WINAPI *pGetDCPenColor)(HDC)                                                                  = NULL;

static NET_API_STATUS (WINAPI *pNetGroupGetUsers)(LPCWSTR,LPCWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD_PTR) = NULL;
static NET_API_STATUS (WINAPI *pNetGroupEnum)(LPCWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD_PTR)             = NULL;
static NET_API_STATUS (WINAPI *pNetUserGetLocalGroups)(LPCWSTR,LPCWSTR,DWORD,DWORD,PBYTE*,DWORD,PDWORD,PDWORD) = NULL;
static NET_API_STATUS (WINAPI *pNetApiBufferFree)(PVOID)                                                       = NULL;

static HWND (WINAPI *pHtmlHelp)(HWND,LPCSTR,UINT,DWORD_PTR)                                                    = NULL;

static ULONG (WINAPI *pMAPISendMail)(LHANDLE,ULONG,lpMapiMessage,FLAGS,ULONG)                                  = NULL;
static ULONG (WINAPI *pMAPISendDocuments)(ULONG,LPTSTR,LPTSTR,LPTSTR,ULONG)                                    = NULL;
static UINT  (WINAPI *pOleUIInsertObject)(LPOLEUIINSERTOBJECT)                                                 = NULL;

static HWND  (WINAPI *pcapCreateCaptureWindow)(LPCTSTR,DWORD,int,int,int,int,HWND,int)                         = NULL;

static INTERNET_STATUS_CALLBACK (WINAPI *pInternetSetStatusCallback)(HINTERNET,INTERNET_STATUS_CALLBACK)       = NULL;
static BOOL (WINAPI *pGetPerformanceInfo)(PPERFORMANCE_INFORMATION,DWORD)                                      = NULL;
static HRESULT (WINAPI *pUrlGetPart)(LPCTSTR,LPTSTR,LPDWORD,DWORD,DWORD)                                       = NULL;
static BOOL (WINAPI *pGlobalMemoryStatusEx)(LPMEMORYSTATUSEX)                                                  = NULL;

static LANGID (WINAPI *pGetUserDefaultUILanguage)(VOID)                                                        = NULL;
static LANGID (WINAPI *pGetSystemDefaultUILanguage)(VOID)                                                      = NULL;

static HRESULT (WINAPI *pDwmEnableBlurBehindWindow)(HWND,const DWM_BLURBEHIND*)                                = NULL;
static HRESULT (WINAPI *pDwmExtendFrameIntoClientArea)(HWND,const MARGINS*)                                    = NULL;
static HRESULT (WINAPI *pDwmGetColorizationColor)(DWORD*,BOOL*)                                                = NULL;
static HRESULT (WINAPI *pDwmIsCompositionEnabled)(BOOL*)                                                       = NULL;


#ifdef TRACE_ARRAYPOINTER
   static unsigned int s_uiArrayPointers = 0;
#endif

static void winapi_Exit( void * cargo )
{
   ( cargo );

   /* Release system object when all finished -- usually at the end of the cleanup code */
   DeleteCriticalSection( &s_cs );
}

static void winapi_Init( void * cargo )
{
   ( cargo );

   pHB_CSTRUCTURE = hb_dynsymFind( "HB_CSTRUCTURE" );

   pPOINTER       = hb_dynsymGetCase( "POINTER" );
   pVALUE         = hb_dynsymGetCase( "VALUE" );
   pDEVALUE       = hb_dynsymGetCase( "DEVALUE" );

   /* Initialize the critical section -- This must be done before locking */
   InitializeCriticalSection( &s_cs );

   hb_vmAtExit( winapi_Exit, NULL );
}

HB_CALL_ON_STARTUP_BEGIN( _winapi_init_ )
   hb_vmAtInit( winapi_Init, NULL );
HB_CALL_ON_STARTUP_END( _winapi_init_ )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup _winapi_init_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM winapi_init = _winapi_init_;
   #pragma data_seg()
#endif

//----------------------------------------------------------------------------------------------------------------------------

#ifdef WINAPI_DEMO

 static int iFirst = 0;

 static void hbtRetnl( LONG nRet )
 {
    if( iFirst == 0 )
    {
       iFirst = 1;

       #ifdef VXH
        MessageBox( GetActiveWindow(), (LPCSTR) "This application was build with the demo version of Visual xHarbour. \n \n Copyright (c) 2003-2010 xHarbour.com Inc. \n http://www.xHarbour.com"
                                     , (LPCSTR) "Visual xHarbour Demo", MB_OK | MB_ICONINFORMATION );
       #else
        MessageBox( GetActiveWindow(), (LPCSTR) "This application was build with the demo version of xHarbour Builder. \n \n Copyright (c) 2003-2010 xHarbour.com Inc. \n http://www.xHarbour.com"
                                     , (LPCSTR) "xHarbour Builder Demo", MB_OK | MB_ICONINFORMATION );
       #endif
    }

    hb_retnl( nRet );
 }

 #undef hb_retnl
 #define hb_retnl( lNumber ) hbtRetnl( lNumber )

#endif


//----------------------------------------------------------------------------------------------------------------------------
void OutputDebugValues( const char *sFormat, ... )
{
    /* Enter the critical section -- other threads are locked out */
    EnterCriticalSection( &s_cs );
    {
       va_list ap;

       s_sBuffer[0] = '\0';

       va_start( ap, sFormat );

       #ifdef __XCC__
          vsnprintf( s_sBuffer, WINAPI_DEBUG_BUFFER_LEN, sFormat, ap );
       #else
          _vsnprintf( s_sBuffer, WINAPI_DEBUG_BUFFER_LEN, sFormat, ap );
       #endif

       va_end( ap );

       OutputDebugString( s_sBuffer );
    }
    /* Leave the critical section -- other threads can now EnterCriticalSection() */
    LeaveCriticalSection( &s_cs );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC_INIT( _INITSYMBOLS_ )
{
   //TraceLog( NULL, "_INITSYMBOLS_" );
   if( hCryptUI == NULL )
   {
      hCryptUI = LoadLibrary( "CryptUI.dll" );
   }

   if( hCrypt32 == NULL )
   {
      hCrypt32 = LoadLibrary( "Crypt32.dll" );
   }

   if( hUser32 == NULL )
   {
      hUser32 = LoadLibrary( "User32.dll" );
   }

   if( hShell32 == NULL )
   {
      hShell32 = LoadLibrary( "Shell32.dll" );
   }

   if( hNetAPI32 == NULL )
   {
      hNetAPI32 = LoadLibrary( "NetAPI32.dll" );
   }

   if( hGdi32 == NULL )
   {
      hGdi32 = LoadLibrary( "Gdi32.dll" );
   }

   if( hWinmm == NULL )
   {
      hWinmm = LoadLibrary( "Winmm.dll" );
   }

   if( hAdvApi32 == NULL )
   {
      hAdvApi32 = LoadLibrary( "AdvApi32.dll" );
   }

   if( hHtmlHelp == NULL )
   {
      hHtmlHelp = LoadLibrary( "hhctrl.ocx" );
   }

   if( hMAPI == NULL )
   {
      hMAPI = LoadLibrary( "mapi32.dll" );
   }

   if( hOleDlg == NULL )
   {
      hOleDlg = LoadLibrary( "oledlg.dll" );
   }

   if( hAviCap == NULL )
   {
      hAviCap = LoadLibrary( "avicap32.dll" );
   }

   if( hWininet == NULL )
   {
      hWininet = LoadLibrary( "Wininet.dll" );
   }

   if( hPSAPI == NULL )
   {
      hPSAPI = LoadLibrary( "PSAPI.dll" );
   }
   if( hshlwapi == NULL )
   {
      hshlwapi = LoadLibrary( "shlwapi.dll" );
   }

   if( hKernel32 == NULL )
   {
      hKernel32 = LoadLibrary( "Kernel32.dll" );
   }

   if( hdwmapi == NULL )
   {
      hdwmapi = LoadLibrary( "dwmapi.dll" );
   }

   if( hCryptUI )
   {
      pCryptUIDlgSelectCertificateFromStore = (PCCERT_CONTEXT (WINAPI *)(HCERTSTORE,HWND,LPCWSTR,LPCWSTR,DWORD,DWORD,LPVOID)) GetProcAddress( hCryptUI, "CryptUIDlgSelectCertificateFromStore" );
   }

   if( hCrypt32 )
   {
      pCertOpenSystemStore        = (HCERTSTORE (WINAPI *)(HCRYPTPROV,LPCSTR))                         GetProcAddress( hCrypt32, "CertOpenSystemStoreA" );
      pCertFreeCertificateContext = (BOOL (WINAPI *)(PCCERT_CONTEXT))                                  GetProcAddress( hCrypt32, "CertFreeCertificateContext" );
      pCertCloseStore             = (BOOL (WINAPI *)(HCERTSTORE,DWORD))                                GetProcAddress( hCrypt32, "CertCloseStore" );
      pCertNameToStr              = (DWORD (WINAPI *)(DWORD,PCERT_NAME_BLOB,DWORD,LPTSTR,DWORD))       GetProcAddress( hCrypt32, "CertNameToStrA" );
      pCertGetNameString          = (DWORD (WINAPI *)(PCCERT_CONTEXT,DWORD,DWORD,LPVOID,LPTSTR,DWORD)) GetProcAddress( hCrypt32, "CertGetNameStringA" );
   }

   if( hUser32 )
   {
      pIsHungAppWindow            = (BOOL (WINAPI *)(HWND))                      GetProcAddress( hUser32, "IsHungAppWindow" );
      pEndTask                    = (BOOL (WINAPI *)(HWND,BOOL,BOOL))            GetProcAddress( hUser32, "EndTask" );
      pGetAncestor                = (HWND (WINAPI *)(HWND,UINT))                 GetProcAddress( hUser32, "GetAncestor" );
      pTrackMouseEvent            = (BOOL (WINAPI *)(LPTRACKMOUSEEVENT))         GetProcAddress( hUser32, "TrackMouseEvent" );
      pGetScrollBarInfo           = (BOOL (WINAPI *)(HWND,LONG, PSCROLLBARINFO)) GetProcAddress( hUser32, "GetScrollBarInfo" );
      pRealChildWindowFromPoint   = (HWND (WINAPI *)(HWND,POINT))                GetProcAddress( hUser32, "RealChildWindowFromPoint" );
      pSwitchToThisWindow         = (VOID (WINAPI *)(HWND,BOOL))                 GetProcAddress( hUser32, "SwitchToThisWindow" );
      pSetLayeredWindowAttributes = (BOOL (WINAPI *)(HWND,COLORREF,BYTE,DWORD))  GetProcAddress( hUser32, "SetLayeredWindowAttributes");
   }

   if( hShell32 )
   {
      pSHChangeNotifyRegister   = (ULONG   (WINAPI *)(HWND,int,LONG,UINT,int,SHChangeNotifyEntry*)) GetProcAddress( hShell32, "SHChangeNotifyRegister" );
      pSHChangeNotifyDeregister = (BOOL    (WINAPI *)(ULONG))                                       GetProcAddress( hShell32, "SHChangeNotifyDeregister" );

      pSHGetFolderPath          = (HRESULT (WINAPI *)(HWND,int,HANDLE,DWORD,LPTSTR))                GetProcAddress( hShell32, "SHGetFolderPathA" );
      pSHGetFolderPathAndSubDir = (HRESULT (WINAPI *)(HWND,int,HANDLE,DWORD,LPCTSTR,LPTSTR))        GetProcAddress( hShell32, "SHGetFolderPathAndSubDirA" );
      pSHILCreateFromPath       = (HRESULT (WINAPI *)(LPCWSTR, ITEMIDLIST*, DWORD*))                GetProcAddress( hShell32, "SHILCreateFromPathA" );
   }

   if( hNetAPI32 )
   {
      pNetGroupGetUsers      = (NET_API_STATUS (WINAPI *)(LPCWSTR,LPCWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD_PTR)) GetProcAddress( hNetAPI32, "NetGroupGetUsers" );
      pNetGroupEnum          = (NET_API_STATUS (WINAPI *)(LPCWSTR,DWORD,PBYTE*,DWORD,PDWORD,PDWORD,PDWORD_PTR))         GetProcAddress( hNetAPI32, "NetGroupEnum" );
      pNetUserGetLocalGroups = (NET_API_STATUS (WINAPI *)(LPCWSTR,LPCWSTR,DWORD,DWORD,PBYTE*,DWORD,PDWORD,PDWORD))      GetProcAddress( hNetAPI32, "NetUserGetLocalGroups" );
      pNetApiBufferFree      = (NET_API_STATUS (WINAPI *)(PVOID))                                                       GetProcAddress( hNetAPI32, "NetApiBufferFree" );
   }

   if( hGdi32 )
   {
      pGetDCBrushColor = (COLORREF (WINAPI *)(HDC)) GetProcAddress( hGdi32, "GetDCBrushColor" );
      pSetDCBrushColor = (COLORREF (WINAPI *)(HDC,COLORREF)) GetProcAddress( hGdi32, "SetDCBrushColor" );
      pGetDCPenColor   = (COLORREF (WINAPI *)(HDC)) GetProcAddress( hGdi32, "GetDCPenColor" );
   }

   if( hWinmm )
   {
      psndPlaySound = (BOOL (WINAPI *)(LPCSTR,UINT)) GetProcAddress( hWinmm, "sndPlaySoundA" );
   }

   if( hAdvApi32 )
   {
      pQueryServiceStatusEx  = (BOOL (WINAPI *)(SC_HANDLE,SC_STATUS_TYPE,LPBYTE,DWORD,LPDWORD)) GetProcAddress( hAdvApi32, "QueryServiceStatusEx" );
      pQueryServiceStatus    = (BOOL (WINAPI *)(SC_HANDLE,LPSERVICE_STATUS)) GetProcAddress( hAdvApi32, "QueryServiceStatus" );
      pQueryServiceConfig    = (BOOL (WINAPI *)(SC_HANDLE,LPQUERY_SERVICE_CONFIG,DWORD,LPDWORD)) GetProcAddress( hAdvApi32, "QueryServiceConfig" );
      pQueryServiceConfig2   = (BOOL (WINAPI *)(SC_HANDLE,DWORD,LPBYTE,DWORD,LPDWORD)) GetProcAddress( hAdvApi32, "QueryServiceConfig2" );
      pChangeServiceConfig2  = (BOOL (WINAPI *)(SC_HANDLE,DWORD,LPVOID)) GetProcAddress( hAdvApi32, "ChangeServiceConfig2" );
   }

   if( hHtmlHelp )
   {
      pHtmlHelp = (HWND (WINAPI *)(HWND,LPCSTR,UINT,DWORD_PTR)) GetProcAddress( hHtmlHelp, "HtmlHelpA" );
   }

   if( hMAPI )
   {
      pMAPISendMail = (ULONG (WINAPI *)(LHANDLE,ULONG,lpMapiMessage,FLAGS,ULONG)) GetProcAddress( hMAPI, "MAPISendMail" );
      pMAPISendDocuments = (ULONG (WINAPI *)(ULONG,LPTSTR,LPTSTR,LPTSTR,ULONG)) GetProcAddress( hMAPI, "MAPISendDocuments" );
   }

   if( hOleDlg )
   {
      pOleUIInsertObject = (UINT (WINAPI *)(LPOLEUIINSERTOBJECT)) GetProcAddress( hOleDlg, "OleUIInsertObjectA" );
   }

   if( hAviCap )
   {
      pcapCreateCaptureWindow = (HWND (WINAPI *)(LPCTSTR,DWORD,int,int,int,int,HWND,int)) GetProcAddress( hAviCap, "capCreateCaptureWindowA" );
   }

   if( hWininet )
   {
      pInternetSetStatusCallback = (INTERNET_STATUS_CALLBACK (WINAPI *)(HINTERNET,INTERNET_STATUS_CALLBACK)) GetProcAddress( hWininet, "InternetSetStatusCallback" );
   }
   if( hPSAPI )
   {
      pGetPerformanceInfo = (BOOL (WINAPI *)(PPERFORMANCE_INFORMATION,DWORD)) GetProcAddress( hPSAPI, "GetPerformanceInfo");
   }
   if( hshlwapi )
   {
      pUrlGetPart = (HRESULT (WINAPI *)(LPCTSTR,LPTSTR,LPDWORD,DWORD,DWORD)) GetProcAddress( hshlwapi, "UrlGetPartA");
   }
   if( hKernel32 )
   {
      pGlobalMemoryStatusEx = (BOOL (WINAPI *)(LPMEMORYSTATUSEX)) GetProcAddress( hKernel32, "GlobalMemoryStatusEx");
      pGetUserDefaultUILanguage = (LANGID (WINAPI *)(VOID)) GetProcAddress( hKernel32, "GetUserDefaultUILanguage");
      pGetSystemDefaultUILanguage = (LANGID (WINAPI *)(VOID)) GetProcAddress( hKernel32, "GetSystemDefaultUILanguage");
   }
   if( hdwmapi )
   {
      pDwmEnableBlurBehindWindow = (HRESULT (WINAPI *)(HWND,const DWM_BLURBEHIND*)) GetProcAddress( hdwmapi, "DwmEnableBlurBehindWindow");
      pDwmExtendFrameIntoClientArea = (HRESULT (WINAPI *)(HWND,const MARGINS*))     GetProcAddress( hdwmapi, "DwmExtendFrameIntoClientArea");
      pDwmGetColorizationColor = (HRESULT (WINAPI *)(DWORD*,BOOL*))                 GetProcAddress( hdwmapi, "DwmGetColorizationColor");
      pDwmIsCompositionEnabled = (HRESULT (WINAPI *)(BOOL*))                        GetProcAddress( hdwmapi, "DwmIsCompositionEnabled");
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC_EXIT( _CLEANUP_ )
{
   //TraceLog( NULL, "_CLEANUP_" );

   if( hCryptUI )
   {
      FreeLibrary( hCryptUI );
   }

   if( hCrypt32 )
   {
      FreeLibrary( hCrypt32 );
   }

   if( hUser32 )
   {
      FreeLibrary( hUser32 );
   }

   if( hShell32 )
   {
      FreeLibrary( hShell32 );
   }

   if( hNetAPI32 )
   {
      FreeLibrary( hNetAPI32 );
   }

   if( hGdi32 )
   {
      FreeLibrary( hGdi32 );
   }

   if( hWinmm )
   {
      FreeLibrary( hWinmm );
   }

   if( hAdvApi32 )
   {
      FreeLibrary( hAdvApi32 );
   }

   if( hHtmlHelp )
   {
      FreeLibrary( hHtmlHelp );
   }
   if( hMAPI )
   {
      FreeLibrary( hMAPI );
   }
   if( hOleDlg )
   {
      FreeLibrary( hOleDlg );
   }
   if( hAviCap )
   {
      FreeLibrary( hAviCap );
   }
   if( hWininet )
   {
      FreeLibrary( hWininet );
   }
   if( hPSAPI )
   {
      FreeLibrary( hPSAPI );
   }
   if( hshlwapi )
   {
      FreeLibrary( hshlwapi );
   }
   if( hKernel32 )
   {
      FreeLibrary( hKernel32 );
   }
   if( hdwmapi )
   {
      FreeLibrary( hdwmapi );
   }

}

//-------------------------------------------------------------------------------------------------
#ifndef VXH
   //#define NO_PRG_CODE
   #ifdef NO_PRG_CODE
      HB_FUNC( ALERT )
      {
         char szPath[MAX_PATH];

         if( ! GetModuleFileName( NULL, szPath, MAX_PATH ) )
         {
            strcpy( szPath, "xHarbour Builder/WinAPI" );
         }

         MessageBox( 0, hb_parcx(1), szPath, 0 );

         hb_retni( 1 );
      }
   #else
      HB_FUNC( ALERT )
      {
         static const BYTE pcode[] =
         {
          HB_P_FRAME, 1, 1,   /* locals, params */
      /* 00003 */ HB_P_BASELINE, 12, 0,   /* 12 */
          HB_P_LOCALNEARSETSTR, 2, 1, 0,  /* CMESSAGE 1*/
          0,
      /* 00011 */ HB_P_LINEOFFSET, 2, /* 14 */
          HB_P_PUSHSYMNEAR, 2,    /* VALTYPE */
          HB_P_PUSHNIL,
          HB_P_PUSHLOCALNEAR, 1,  /* XMESSAGE */
          HB_P_FUNCTIONSHORT, 1,
          HB_P_PUSHSTRSHORT, 2,   /* 2 */
          'C', 0,
          HB_P_EXACTLYEQUAL,
          HB_P_JUMPFALSENEAR, 24, /* 24 (abs: 00049) */
      /* 00027 */ HB_P_LINEOFFSET, 3, /* 15 */
          HB_P_PUSHSYMNEAR, 3,    /* STRTRAN */
          HB_P_PUSHNIL,
          HB_P_PUSHLOCALNEAR, 1,  /* XMESSAGE */
          HB_P_PUSHSTRSHORT, 2,   /* 2 */
          ';', 0,
          HB_P_PUSHSTRSHORT, 3,   /* 3 */
          13, 10, 0,
          HB_P_FUNCTIONSHORT, 3,
          HB_P_POPLOCALNEAR, 2,   /* CMESSAGE */
          HB_P_JUMPNEAR, 39,  /* 39 (abs: 00086) */
      /* 00049 */ HB_P_LINEOFFSET, 5, /* 17 */
          HB_P_PUSHSYMNEAR, 4,    /* AEVAL */
          HB_P_PUSHNIL,
          HB_P_PUSHLOCALNEAR, 1,  /* XMESSAGE */
          HB_P_PUSHBLOCK, 28, 0,  /* 28 */
          1, 0,   /* number of local parameters (1) */
          1, 0,   /* number of local variables (1) */
          2, 0,   /* CMESSAGE */
          HB_P_PUSHLOCALNEAR, 255,    /* localvar1 */
          HB_P_PUSHLOCALNEAR, 1,  /* codeblockvar1 */
          HB_P_PUSHSTRSHORT, 2,   /* 2 */
          13, 0,
          HB_P_PLUS,
          HB_P_PUSHSTRSHORT, 2,   /* 2 */
          10, 0,
          HB_P_PLUS,
          HB_P_PLUS,
          HB_P_DUPLICATE,
          HB_P_POPLOCALNEAR, 255, /* localvar1 */
          HB_P_ENDBLOCK,
          HB_P_DOSHORT, 2,
      /* 00086 */ HB_P_LINEOFFSET, 8, /* 20 */
          HB_P_PUSHSYMNEAR, 5,    /* MESSAGEBOX */
          HB_P_PUSHNIL,
          HB_P_ZERO,
          HB_P_PUSHLOCALNEAR, 2,  /* CMESSAGE */
          HB_P_PUSHSYMNEAR, 6,    /* GETMODULEFILENAME */
          HB_P_PUSHNIL,
          HB_P_FUNCTIONSHORT, 0,
          HB_P_PUSHLONG, 0, 0, 4, 0,  /* 262144 */
          HB_P_FUNCTIONSHORT, 4,
          HB_P_RETVALUE,
          HB_P_ENDPROC
      /* 00108 */
         };

         hb_vmExecute( pcode, symbols );
      }
   #endif
#endif

//-------------------------------------------------------------------------------------------------
HB_FUNC( GLOBALSTRING )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );

   if( pString )
   {
      HGLOBAL hglbCopy = GlobalAlloc( GMEM_MOVEABLE, pString->item.asString.length + 1 );
      LPTSTR  lptstrCopy;

      if( hglbCopy )
      {
         // Lock the handle and copy the text to the buffer.
         lptstrCopy = (LPTSTR) GlobalLock( hglbCopy );
         memcpy( lptstrCopy, pString->item.asString.value, pString->item.asString.length );
         lptstrCopy[ pString->item.asString.length ] = '\0';
         GlobalUnlock( hglbCopy );

         hb_retnl( (long) hglbCopy );
         return;
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 6001, NULL, "GlobalString", 1, hb_paramError( 1 ) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( FREEGLOBALSTRING )
{
   GlobalFree( (HGLOBAL) hb_parnl(1) );
}

//-------------------------------------------------------------------------------------------------
static PHB_ITEM hb_pureparam( int iParam, LONG iMask )
{
   HB_THREAD_STUB

   if( pHB_CSTRUCTURE == NULL )
   {
       HB_INIT_FUNCNAME( _INITSYMBOLS_ )();
   }

   if( ( iParam >= 0 && iParam <= hb_pcount() ) )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam  );

      if( ( HB_TYPE ) iMask == HB_IT_ANY || pItem->type & ( HB_TYPE ) iMask )
      {
         return pItem;
      }
      else
      {
         if( (HB_TYPE) iMask == HB_IT_NUMERIC && HB_IS_NUMERIC( pItem ) )
         {
            return pItem;
         }
      }
   }

   return NULL;
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( HB_POINTER2STRING )
{
   PHB_ITEM pPointer = hb_pureparam( 1, HB_IT_ANY );
   PHB_ITEM pLen     = hb_pureparam( 2, HB_IT_NUMERIC );

   if( HB_IS_POINTER( pPointer ) && pLen )
   {
      hb_retclenStatic( (char *) hb_itemGetPtr( pPointer ), (ULONG) hb_itemGetNL( pLen ) );
   }
   else if( HB_IS_INTEGER( pPointer ) && pLen )
   {
      hb_retclenStatic( (char *) hb_itemGetNI( pPointer ), (ULONG) hb_itemGetNL( pLen ) );
   }
   else if( HB_IS_LONG( pPointer ) && pLen )
   {
      hb_retclenStatic( (char *) hb_itemGetNL( pPointer ), (ULONG) hb_itemGetNL( pLen ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 6001, NULL, "HB_Pointer2String", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ARRAYPOINTER )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      PHB_ITEM pCopy = hb_itemNew( pArray );

      #ifdef TRACE_ARRAYPOINTER
         s_uiArrayPointers++;
         TraceLog( NULL, "Copy: %p class: %s\n", (void *) pCopy, hb_objGetClsName( pCopy ) );
      #endif
      hb_retnl( (long) (void *) pCopy );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 6001, NULL, "ArrayPointer", 1, hb_paramError( 1 ) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( STRINGPOINTER )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );

   if( pString )
   {
      hb_retnl( (long) pString->item.asString.value );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 6001, NULL, "StringPointer", 1, hb_paramError( 1 ) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( LONGFROMPOINTER )
{
   PHB_ITEM pPointer = hb_param( 1, HB_IT_ANY );
   void *pVoid;

   if( HB_IS_POINTER( pPointer ) )
   {
      pVoid = pPointer->item.asPointer.value;
   }
   else if( HB_IS_INTEGER( pPointer ) )
   {
      pVoid = (void *) pPointer->item.asInteger.value;
   }
   else if( HB_IS_LONG( pPointer ) )
   {
      pVoid = (void *) (long) pPointer->item.asLong.value;
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 6001, NULL, "LongFromPointer", 1, hb_paramError( 1 ) );
   }

   hb_retnl( (long) * (long *) pVoid );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ARRAYFROMPOINTER )
{
   PHB_ITEM pPointer = hb_param( 1, HB_IT_ANY );
   void *pVoid;

   if( HB_IS_POINTER( pPointer ) )
   {
      pVoid = pPointer->item.asPointer.value;
   }
   else if( HB_IS_INTEGER( pPointer ) )
   {
      pVoid = (void *) pPointer->item.asInteger.value;
   }
   else if( HB_IS_LONG( pPointer ) )
   {
      pVoid = (void *) (long) pPointer->item.asLong.value;
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 6001, NULL, "ArrayFromPointer", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   if( hb_parl( 2 ) )
   {
      #ifdef TRACE_ARRAYPOINTER
         s_uiArrayPointers--;
      #endif

      hb_itemReturnForward((PHB_ITEM) pVoid );
      hb_itemRelease( (PHB_ITEM) pVoid );
   }
   else
   {
      //OutputDebugValues( "Return copy: %p Type: %i\n", pVoid, ( (PHB_ITEM) pVoid )->type );
      hb_itemReturn((PHB_ITEM) pVoid );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( RELEASEARRAYPOINTER )
{
   PHB_ITEM pPointer = hb_param( 1, HB_IT_ANY );
   void *pVoid;

   if( HB_IS_POINTER( pPointer ) )
   {
      pVoid = pPointer->item.asPointer.value;
   }
   else if( HB_IS_INTEGER( pPointer ) )
   {
      pVoid = (void *) pPointer->item.asInteger.value;
   }
   else if( HB_IS_LONG( pPointer ) )
   {
      pVoid = (void *) (long) pPointer->item.asLong.value;
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 6001, NULL, "ReleaseArrayPointer", 1, hb_paramError( 1 ) );
      return;
   }

   #ifdef TRACE_ARRAYPOINTER
      TraceLog( NULL, "(%i) Release: %p class: %s\n", --s_uiArrayPointers, (void *) pVoid, hb_objGetClsName( (PHB_ITEM) pVoid ) );
   #endif

   hb_itemRelease( (PHB_ITEM) pVoid );
}

//-------------------------------------------------------------------------------------------------
// BEGIN API Wrappers.
//-------------------------------------------------------------------------------------------------
HB_FUNC( REGISTERCLASS )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( (LONG) RegisterClass( (CONST WNDCLASS*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "REGISTERCLASS", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( UNREGISTERCLASS )
{
   hb_retl( UnregisterClass( (LPCTSTR) hb_parc(1), (HINSTANCE) hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( REGISTERCLASSEX )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( (LONG) RegisterClassEx( (CONST WNDCLASSEX*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "REGISTERCLASSEX", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEWINDOW )
{
   hb_retnl( (LONG) CreateWindow( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2), (DWORD) hb_parnl(2),
                                   hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7),
                                  (HWND) hb_parnl(8), (HMENU) hb_parnl(9), (HINSTANCE) hb_parnl(10), (LPVOID) hb_parc(11) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEWINDOWEX )
{
   PHB_ITEM pStructure = hb_pureparam( 12, HB_IT_ANY );
   void *lpParam;

   if( pStructure  )
   {
      if( HB_IS_OBJECT( pStructure ) )
      {
         if( strncmp( hb_objGetClsName( pStructure ), "C Structure", 11 ) == 0 )
         {
            hb_vmPushSymbol( pVALUE->pSymbol );
            hb_itemPushForward( pStructure );
            hb_vmSend(0);

            lpParam = (void *) hb_stackReturnItem()->item.asString.value;
         }
         else
         {
            lpParam = (void *) hb_itemNew( pStructure );
         }
      }
      else if( HB_IS_POINTER( pStructure ) )
      {
         lpParam = pStructure->item.asPointer.value;
      }
      else
      {
         lpParam = (void *) hb_itemGetNL( pStructure );
      }
   }
   else
   {
      lpParam = NULL;
   }

   hb_retnl( (LONG) CreateWindowEx( (DWORD) hb_parnl(1), (LPCTSTR) hb_parc(2), (LPCTSTR) hb_parc(3), (DWORD) hb_parnl(4),
                                    hb_parni(5), hb_parni(6), hb_parni(7), hb_parni(8),
                                    (HWND) hb_parnl(9), (HMENU) hb_parnl(10), (HINSTANCE) hb_parnl(11), lpParam ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SHOWWINDOW )
{
   hb_retl( ShowWindow( (HWND) hb_parnl(1), hb_parni( 2 ) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETMESSAGE )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      BOOL bResult;
      MSG *pMsg = (MSG *) hb_xgrab( sizeof( MSG ) );

      bResult = GetMessage( pMsg, (HWND) hb_parnl(2), (UINT) hb_parnl(3), (UINT) hb_parnl(4) );

      if( bResult && bResult != -1 )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "MSG", 3 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pmsg
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pMsg, sizeof( MSG ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pMsg );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETMESSAGE", 4, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ISDIALOGMESSAGE )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( IsDialogMessage( (HWND) hb_parnl(1), (MSG *) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ISDIALOGMESSAGE", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( TRANSLATEMESSAGE )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( TranslateMessage( (const MSG*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "TRANSLATEMESSAGE", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DISPATCHMESSAGE )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( DispatchMessage( (const MSG*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DISPATCHMESSAGE", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DEFWINDOWPROC )
{
   hb_retnl( (LONG) DefWindowProc( (HWND) hb_parnl(1), (UINT) hb_parnl(2), (WPARAM) hb_parnl(3), (LPARAM) hb_parnl(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DEFWINDOWPROCPOINTER )
{
   hb_retnl( (LONG) DefWindowProc );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DEFDLGPROCPOINTER )
{
   hb_retnl( (LONG) DefDlgProc );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETMODULEHANDLE )
{
   hb_retnl( (LONG) GetModuleHandle( hb_parc(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( POSTQUITMESSAGE )
{
   #ifdef WINAPI_DEMO

     #ifdef VXH
      MessageBox( GetActiveWindow(), (LPCSTR) "This application was build with the demo version of Visual xHarbour. \n \n Copyright (c) 2003-2010 xHarbour.com Inc. \n http://www.xHarbour.com"
                                   , (LPCSTR) "Visual xHarbour Demo", MB_OK | MB_ICONINFORMATION );
     #else
      MessageBox( GetActiveWindow(), (LPCSTR) "This application was build with the demo version of xHarbour Builder. \n \n Copyright (c) 2003-2010 xHarbour.com Inc. \n http://www.xHarbour.com"
                                   , (LPCSTR) "xHarbour Builder Demo", MB_OK | MB_ICONINFORMATION );
     #endif

   #endif

   PostQuitMessage( hb_parni(1) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETWINDOWTEXT )
{
   SetWindowText( (HWND) hb_parnl(1), (LPCTSTR) hb_parc(2) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETFOCUS )
{
   hb_retnl( (long) SetFocus( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETCARETPOS )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      POINT *pPoint = (POINT *) hb_xgrab( sizeof( POINT ) );

      if( GetCaretPos( pPoint ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "POINT", 5 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pmsg
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pPoint, sizeof( POINT ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pPoint );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETCARETPOS", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETCARETPOS )
{
   hb_retl( SetCaretPos( hb_parnl(1), hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETTEXTEXTENTPOINT32 )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pStructure )
   {
      SIZE *pSize = (SIZE *) hb_xgrab( sizeof( SIZE ) );
      PHB_ITEM pString = hb_param( 2, HB_IT_STRING );

      if( pString && GetTextExtentPoint32( (HDC) hb_parnl(1), pString->item.asString.value, pString->item.asString.length, pSize ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "SIZE", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pSize
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pSize, sizeof( SIZE ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pSize );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETTEXTEXTENTPOINT32", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETDC )
{
   hb_retnl( (LONG) GetDC( ISNIL(1)?NULL:(HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( RELEASEDC )
{
   hb_retni( ReleaseDC( ISNIL(1)?NULL:(HWND) hb_parnl(1), (HDC) hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DESTROYWINDOW )
{
   hb_retl( DestroyWindow( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETTEXTMETRICS )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      TEXTMETRIC *pTextMetric = (TEXTMETRIC *) hb_xgrab( sizeof( TEXTMETRIC ) );

      if( GetTextMetrics( (HDC) hb_parnl(1), pTextMetric ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "TEXTMETRIC", 10 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pTextMetric
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pTextMetric, sizeof( TEXTMETRIC ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pTextMetric );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETTEXTMETRICS", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SENDMESSAGE )
{
   PHB_ITEM pLPARAM = hb_pureparam( 4, HB_IT_ANY );
   BOOL bByRef;
   LPARAM lParam;

   if( pLPARAM )
   {
      if( HB_IS_BYREF( pLPARAM ) )
      {
         bByRef = TRUE;
         pLPARAM = hb_itemUnRef( pLPARAM );
      }
      else
      {
         bByRef = FALSE;
      }

      if( HB_IS_OBJECT( pLPARAM ) )
      {
         if( strncmp( hb_objGetClsName( pLPARAM ), "C Structure", 11 ) == 0 )
         {
            hb_vmPushSymbol( pVALUE->pSymbol );
            hb_vmPush( pLPARAM );
            hb_vmSend(0);

            // lParam := @Structure:InternalBuffer
            lParam = (LPARAM) hb_parc(-1);
         }
         else
         {
            lParam = (LPARAM) ( pLPARAM->item.asArray.value );
         }
      }
      else if( HB_IS_STRING( pLPARAM ) )
      {
         lParam = (LPARAM) hb_itemGetCPtr( pLPARAM );
      }
      else if( HB_IS_NUMERIC( pLPARAM ) )
      {
         lParam = hb_itemGetNL( pLPARAM );
      }
      else if( HB_IS_LOGICAL( pLPARAM ) )
      {
         lParam = (LPARAM) hb_itemGetL( pLPARAM );
      }
      else if( HB_IS_NIL( pLPARAM ) )
      {
         lParam = 0;
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 6001, NULL, "SENDMESSAGE", 4, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4) );
      }
   }
   else
   {
      bByRef = FALSE;
      lParam = (LPARAM) 0;
   }

   hb_retnl( SendMessage( (HWND) hb_parnl(1), (UINT) hb_parnl(2), (WPARAM) hb_parnl(3), lParam ) );

   if( bByRef )
   {
      if( HB_IS_OBJECT( pLPARAM ) )
      {
         if( strncmp( hb_objGetClsName( pLPARAM ), "C Structure", 11 ) == 0 )
         {
            //::InternalBuffer := pLPARAM
            // Already used a Reference to InternalBuffer

            //::DeValue()
            hb_vmPushSymbol( pDEVALUE->pSymbol );
            hb_vmPush( pLPARAM );
            hb_vmSend(0);
         }
      }
      else if( HB_IS_STRING( pLPARAM ) )
      {
         hb_storc( (char *) lParam, 4 );
      }
      else if( HB_IS_NUMERIC( pLPARAM ) )
      {
         hb_stornl( 4, lParam );
      }
      else if( HB_IS_NIL( pLPARAM ) )
      {
         hb_stornl( 4, lParam );
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 6001, NULL, "SENDMESSAGE", 4, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4) );
      }
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( POSTMESSAGE )
{
   hb_retnl( PostMessage( (HWND) hb_parnl(1), (UINT) hb_parnl(2), (WPARAM) hb_parnl(3), (LPARAM) hb_parnl(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( TEXTOUT )
{
    hb_retl( TextOut( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), (LPCTSTR) hb_parc(4), hb_parni(5) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( BEGINPAINT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      PAINTSTRUCT *pPaint = (PAINTSTRUCT *) hb_xgrab( sizeof( PAINTSTRUCT ) );
      HDC hDC = BeginPaint( (HWND) hb_parnl(1), pPaint );

      if( hDC )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "PAINTSTRUCT", 11 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pPaint
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pPaint, sizeof( PAINTSTRUCT ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retnl( (LONG) hDC );
         return;
      }
      else
      {
         hb_xfree( (void *) pPaint );
         hb_retnl(0);
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "BEGINPAINT", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ENDPAINT )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( (LONG) EndPaint( (HWND) hb_parnl(1), (CONST PAINTSTRUCT *) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ENDPAINT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETTEXTCOLOR )
{
   hb_retnl( (LONG) SetTextColor( (HDC) hb_parnl(1), (COLORREF) hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SELECTOBJECT )
{
   hb_retnl( (LONG) SelectObject( (HDC) hb_parnl(1), (HGDIOBJ) hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEFONT )
{
   hb_retnl( (LONG) CreateFont( hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), (DWORD) hb_parnl(6),
                                (DWORD) hb_parnl(7), (DWORD) hb_parnl(8), (DWORD) hb_parnl( 9 ), (DWORD) hb_parnl(10),
                                (DWORD) hb_parnl(11), (DWORD) hb_parnl(12), (DWORD) hb_parnl(13), (LPCTSTR) hb_parc(14) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( INVALIDATERECT )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure == NULL || HB_IS_NIL( pStructure ) )
   {
      hb_retl( InvalidateRect( (HWND) hb_parnl(1), NULL, ISLOG(3) ? hb_parl( 3 ) : TRUE ) );
   }
   else if(  HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( InvalidateRect( (HWND) hb_parnl(1), (CONST RECT*) hb_parc(-1), ISLOG(3) ? hb_parl( 3 ) : TRUE ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "INVALIDATERECT", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( FILLRECT )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( (BOOL) FillRect( (HDC) hb_parnl(1), (CONST RECT *) hb_parc(-1), (HBRUSH) hb_parnl(3) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "FILLRECT", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CALLWINDOWPROC )
{
   hb_retnl( (LONG) CallWindowProc( (WNDPROC) hb_parnl(1), (HWND) hb_parnl(2), (UINT) hb_parnl(3), (WPARAM) hb_parnl(4), (LPARAM) hb_parnl(5) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETWINDOWLONG )
{
   PHB_ITEM pValue = hb_param( 3, HB_IT_ANY );
   long lValue;

   if( HB_IS_NUMERIC( pValue ) )
   {
      lValue = hb_itemGetNL( pValue );
   }
   else if( HB_IS_POINTER( pValue ) )
   {
      lValue = (long) hb_itemGetPtr( pValue );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETWINDOWLONG", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      return;
   }

   hb_retnl( SetWindowLong( (HWND) hb_parnl(1), hb_parni(2), lValue ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETWINDOWLONG )
{
   hb_retnl( GetWindowLong( (HWND) hb_parnl(1), hb_parni(2) ));
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETCLASSINFO )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pStructure )
   {
      PHB_ITEM pClass = hb_param( 2, HB_IT_ANY );
      WNDCLASS *pWndClass;
      LPCSTR lpClassName;

      if( HB_IS_STRING( pClass ) )
      {
         lpClassName = pClass->item.asString.value;
      }
      else if( HB_IS_INTEGER( pClass ) )
      {
         lpClassName = (LPCSTR) pClass->item.asInteger.value;
      }
      else if( HB_IS_LONG( pClass ) )
      {
         lpClassName = (LPCSTR) (long) pClass->item.asLong.value;
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 6001, NULL, "GETCLASSINFO", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      }

      pWndClass = (WNDCLASS *) hb_xgrab( sizeof( WNDCLASS ) );

      if( GetClassInfo( (HINSTANCE) hb_parnl(1), lpClassName, pWndClass ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "WNDCLASS", 8 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pWndClass
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pWndClass, sizeof( WNDCLASS ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pWndClass );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETCLASSINFO", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETSYSCOLORBRUSH )
{
   hb_retnl( (LONG) GetSysColorBrush( hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETCLIENTRECT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      RECT *pRect = (RECT *) hb_xgrab( sizeof( RECT ) );

      if( GetClientRect( (HWND) hb_parnl(1), pRect ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "RECT", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pRect
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pRect, sizeof( RECT ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pRect );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETCLIENTRECT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( LOADCURSOR )
{
   hb_retnl( (LONG) LoadCursor( (HINSTANCE) hb_parnl(1), hb_parinfo(2) == HB_IT_STRING ? hb_parc(2): MAKEINTRESOURCE( hb_parnl(2) ) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( LOADCURSORFROMFILE )
{
   hb_retnl( (LONG) LoadCursorFromFile( (LPCTSTR) hb_parc(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( COPYICON )
{
   hb_retnl( (LONG) CopyIcon( (HICON) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETCURSOR )
{
   hb_retnl( (LONG) SetCursor( (HCURSOR) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETCURSOR )
{
   hb_retnl( (LONG) GetCursor() );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( OUTPUTDEBUGSTRING )
{
   OutputDebugString( hb_parc(1) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DEFDLGPROC )
{
  hb_retnl( DefDlgProc( (HWND) hb_parnl(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETNEXTDLGTABITEM )
{
   hb_retnl( (LONG) GetNextDlgTabItem( (HWND) hb_parnl(1), (HWND) hb_parnl(2), hb_parl( 3 ) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETCLASSLONG )
{
   hb_retnl( (LONG) SetClassLong( (HWND) hb_parnl(1), hb_parni( 2 ), hb_parnl(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETACTIVEWINDOW )
{
   hb_retnl( (LONG) GetActiveWindow() );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETFOCUS )
{
   hb_retnl( (LONG) GetFocus(  ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEMUTEX )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( (ULONG) CreateMutex( (LPSECURITY_ATTRIBUTES) hb_parc(-1), hb_parl( 2 ), (LPCTSTR) hb_parc(3) ) );
   }
   else
   {
      hb_retnl( (ULONG) CreateMutex( NULL, hb_parl( 2 ), (LPCTSTR) hb_parc(3) ) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( RELEASEMUTEX )
{
  hb_retnl( (LONG) ReleaseMutex( (HANDLE) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( OPENMUTEX )
{
  hb_retnl( (LONG) OpenMutex( hb_parnl(1), hb_parl(2), (LPCTSTR) hb_parc(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETMODULEFILENAME )
{
   char szBuffer[ MAX_PATH + 1 ] = {0} ;

   GetModuleFileName( ISNIL(1) ? GetModuleHandle( NULL ) : (HMODULE) hb_parnl(1), szBuffer ,MAX_PATH );

   hb_retc( szBuffer );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETLASTERROR )
{
  hb_retnl( ( LONG ) GetLastError() );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETSYSCOLOR )
{
  hb_retnl( GetSysColor( hb_parni(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETTEMPPATH )
{
   char szBuffer[ MAX_PATH + 1 ] = {0} ;

   GetTempPath( MAX_PATH, szBuffer );

   hb_retc( szBuffer );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETWINDOWSDIRECTORY )
{
   char *szBuffer = (char *) hb_xgrab( MAX_PATH + 1 );
   szBuffer[0] = '\0';
   GetWindowsDirectory( szBuffer, MAX_PATH );
   hb_retcAdopt( szBuffer );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEACCELERATORTABLE )
{
   PHB_ITEM pArray = hb_pureparam( 1, HB_IT_ARRAY );

   if( pArray )
   {
      ACCEL *pAccelerators;
      INT iCount = pArray->item.asArray.value->ulLen;
      INT i ;
      PHB_ITEM pSubArray;

      pAccelerators = (ACCEL *) hb_xgrab( iCount * sizeof( ACCEL ) );

      for( i = 0; i < iCount; i++ )
      {
         pSubArray = hb_arrayGetItemPtr( pArray, i + 1 );

         pAccelerators[i].fVirt = (BYTE) hb_arrayGetNI( pSubArray, 1 );
         pAccelerators[i].key   = (WORD) hb_arrayGetNI( pSubArray, 2 );
         pAccelerators[i].cmd   = (WORD) hb_arrayGetNI( pSubArray, 3 );
      }

      hb_retnl( (LONG) CreateAcceleratorTable( pAccelerators, iCount ) );
      hb_xfree( (void *) pAccelerators );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "CREATEACCELERATORTABLE", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DESTROYACCELERATORTABLE )
{
   hb_retl( DestroyAcceleratorTable( (HACCEL) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( LOADICON )
{
   hb_retnl( (LONG) LoadIcon(  ( ISNIL(1) ? NULL : (HINSTANCE) hb_parnl(1) ) ,
             ( hb_parinfo(2) == HB_IT_STRING ? hb_parc(2) : MAKEINTRESOURCE( (WORD) hb_parni(2) ) ) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( INITCOMMONCONTROLS )
{
   InitCommonControls();
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( INITCOMMONCONTROLSEX )
{
  INITCOMMONCONTROLSEX icc ;

  icc.dwSize = sizeof( INITCOMMONCONTROLSEX );
  icc.dwICC = hb_parnl(1);

  hb_retl( InitCommonControlsEx( &icc ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETVERSIONEX )
{
   BOOL bRet;
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   // NOTE: Expecting *only* OSVERSIONINFOEX!!!
   if( pStructure )
   {
      OSVERSIONINFOEX *pOsvi = (OSVERSIONINFOEX *) hb_xgrab( sizeof( OSVERSIONINFOEX ) );

      pOsvi->dwOSVersionInfoSize = sizeof( OSVERSIONINFOEX );
      if( ( bRet = GetVersionEx( (OSVERSIONINFO *) pOsvi ) ) == 0 );
      {
         pOsvi->dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
         bRet = GetVersionEx( (OSVERSIONINFO *) pOsvi );
      }
      if( bRet )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "OSVERSIONINFOEX", 15 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pOsvi
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pOsvi, sizeof( OSVERSIONINFOEX ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pOsvi );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETVERSIONEX", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETANCESTOR )
{
   if( pGetAncestor )
   {
      hb_retnl( (LONG) pGetAncestor( (HWND) hb_parnl(1), (UINT) hb_parnl(2) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6999, NULL, "GETANCESTOR", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( TRANSLATEACCELERATOR )
{
   PHB_ITEM pStructure = hb_pureparam( 3, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retni( TranslateAccelerator( (HWND) hb_parnl(1), (HACCEL) hb_parnl(2), (CONST LPMSG) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "TRANSLATEACCELERATOR", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( TRANSLATEMDISYSACCEL )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retni( TranslateMDISysAccel( (HWND) hb_parnl(1), (MSG *) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "TRANSLATEMDISYSACCEL", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( PEEKMESSAGE )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      MSG *pMsg = (MSG *) hb_xgrab( sizeof( MSG ) );

      if( PeekMessage( pMsg, (HWND) hb_parnl(2), (UINT) hb_parnl(3), (UINT) hb_parnl(4), (UINT) hb_parnl(5) ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "MSG", 3 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pmsg
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pMsg, sizeof( MSG ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pMsg );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "PEEKMESSAGE", 5, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4), hb_paramError(5) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DELETEOBJECT )
{
   hb_retl( DeleteObject( (HGDIOBJ) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATESOLIDBRUSH )
{
   hb_retnl( (LONG) CreateSolidBrush( (COLORREF) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETRVALUE )
{
   hb_retni( (INT) GetRValue( (DWORD) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETGVALUE )
{
   hb_retni( (INT) GetGValue( (DWORD) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETBVALUE )
{
   hb_retni( (INT) GetBValue( (DWORD) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ISWINDOWVISIBLE )
{
   hb_retl( IsWindowVisible( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ISWINDOW )
{
   hb_retl( IsWindow( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETWINDOWTEXT )
{
   PHB_ITEM pPointer = hb_pureparam( 2, HB_IT_ANY );
   int iLen;
   char *cText;

   if( pPointer )
   {
      iLen = hb_parnl(3);

      if( HB_IS_BYREF( pPointer ) )
      {
         pPointer = hb_itemUnRef( pPointer );

         if( iLen == 0 )
         {
            if( HB_IS_STRING( pPointer ) )
            {
               iLen = pPointer->item.asString.length;
            }
            else
            {
               hb_retnl(0);
               return;
            }
         }

         cText = (char *) hb_xgrab( iLen + 1 );
      }
      else if( HB_IS_POINTER( pPointer ) && iLen )
      {
         cText = (char *) pPointer->item.asPointer.value;
      }
      else
      {
         hb_retnl(0);
         return;
      }
   }
   else
   {
      hb_retnl(0);
      return;
   }

   iLen = GetWindowText( (HWND) hb_parnl(1), (LPSTR) cText, iLen + 1 );

   if( iLen )
   {
      // Will only affect @sString argument - pPointer is directly manipulated by GetWindowText().
      hb_storclenAdopt( cText, iLen, 2 );
   }

   hb_retnl( iLen );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETWINDOWTEXTLENGTH )
{
   hb_retni( GetWindowTextLength( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ENABLEWINDOW )
{
   EnableWindow( (HWND) hb_parnl(1), hb_parl(2) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( LOCKWINDOWUPDATE )
{
   hb_retl( LockWindowUpdate( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( FINDWINDOWEX )
{
   hb_retnl( (LONG) FindWindowEx( (HWND) hb_parnl(1), (HWND) hb_parnl(2), (LPCSTR) hb_parc(3), (LPCSTR) hb_parc(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETSCROLLINFO )
{
   PHB_ITEM pStructure = hb_pureparam( 3, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retni( SetScrollInfo( (HWND) hb_parnl(1), hb_parni(2), (CONST LPSCROLLINFO) hb_parc(-1), hb_parl(4) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETSCROLLINFO", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( TRACKMOUSEEVENT )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      if( pTrackMouseEvent )
      {
         hb_retl( _TrackMouseEvent( (CONST LPTRACKMOUSEEVENT) hb_parc(-1) ) );
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 6999, NULL, "TRACKMOUSEEVENT", 1, hb_paramError(1) );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "TRACKMOUSEEVENT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETSCROLLBARINFO )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pGetScrollBarInfo == NULL )
   {
      hb_errRT_BASE( EG_ARG, 6999, NULL, "SCROLLBARINFO", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      return;
   }

   if( pStructure )
   {
      SCROLLBARINFO *pSbi = (SCROLLBARINFO *) hb_xgrab( sizeof( SCROLLBARINFO ) );

      pSbi->cbSize = sizeof( SCROLLBARINFO );

      if( pGetScrollBarInfo( (HWND) hb_parnl(1), hb_parnl(2), pSbi ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "SCROLLBARINFO" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "SCROLLBARINFO", 13 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pSbi
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pSbi, sizeof( SCROLLBARINFO ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pSbi );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SCROLLBARINFO", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

HB_FUNC( SETWINDOWPLACEMENT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( SetWindowPlacement( (HWND) hb_parnl(1), (WINDOWPLACEMENT *) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETWINDOWPLACEMENT", 2, hb_paramError(1), hb_paramError(2) );
   }
}

HB_FUNC( GETWINDOWPLACEMENT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      WINDOWPLACEMENT *pWndpl = (WINDOWPLACEMENT *) hb_xgrab( sizeof( WINDOWPLACEMENT ) );

      pWndpl->length = sizeof( WINDOWPLACEMENT );

      if( GetWindowPlacement( (HWND) hb_parnl(1), pWndpl ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "WINDOWPLACEMENT", 15 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pWndpl
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pWndpl, sizeof( WINDOWPLACEMENT ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pWndpl );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETWINDOWPLACEMENT", 2, hb_paramError(1), hb_paramError(2) );
   }
}


HB_FUNC( GETWINDOWINFO )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      WINDOWINFO *pWndInfo = (WINDOWINFO *) hb_xgrab( sizeof( WINDOWINFO ) );

      pWndInfo->cbSize = sizeof( WINDOWINFO );

      if( GetWindowInfo( (HWND) hb_parnl(1), pWndInfo ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "WINDOWINFO", 10 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pWndInfo, sizeof( WINDOWINFO ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pWndInfo );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETWINDOWINFO", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETDESKTOPWINDOW )
{
   hb_retnl( (LONG) GetDesktopWindow(  ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DEFFRAMEPROC )
{
  hb_retnl( DefFrameProc( (HWND) hb_parnl(1), (HWND) hb_parnl(2), hb_parnl(3), hb_parnl(4), hb_parnl(5)));
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DEFMDICHILDPROC )
{
  hb_retnl( DefMDIChildProc( (HWND) hb_parnl(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETSCROLLINFO )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pStructure )
   {
      SCROLLINFO *pSi = (SCROLLINFO *) hb_xgrab( sizeof( SCROLLINFO ) );
      pSi->cbSize = sizeof( SCROLLINFO );
      pSi->fMask = SIF_TRACKPOS;

      if( GetScrollInfo( (HWND) hb_parnl(1), hb_parni(2), pSi ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "SCROLLINFO", 10 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pSi
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pSi, sizeof( SCROLLINFO ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pSi );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETSCROLLINFO", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETICONINFO )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      ICONINFO *pIi = (ICONINFO *) hb_xgrab( sizeof( ICONINFO ) );

      if( GetIconInfo( (HICON) hb_parnl(1), pIi ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "ICONINFO", 8 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pRect
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pIi, sizeof( ICONINFO ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pIi );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETICONINFO", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETMENUITEMINFO )
{
   PHB_ITEM pStructure = hb_param( 4, HB_IT_BYREF );

   if( pStructure )
   {
      LPMENUITEMINFO pMii = (LPMENUITEMINFO) hb_xgrab( sizeof( MENUITEMINFO ) );
      if( GetMenuItemInfo( (HMENU) hb_parnl(1), (UINT) hb_parni( 2 ), hb_parl( 3 ), pMii ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "MENUITEMINFO", 12 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pRect
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pMii, sizeof( MENUITEMINFO ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pMii );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETMENUITEMINFO", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETMENUSTRING )
{
   char cText[ MAX_PATH + 1 ] = {0};

   GetMenuString( (HMENU) hb_parnl(1), (UINT) hb_parnl(2),(LPSTR) cText, MAX_PATH, (UINT) hb_parnl(3) );

   hb_retc( cText );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETMENUITEMID )
{
   hb_retni( GetMenuItemID( (HMENU) hb_parnl(1), hb_parni( 2 ) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETMENUITEMCOUNT )
{
   hb_retni( GetMenuItemCount( (HMENU) hb_parnl(1) ));
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETSYSTEMMENU )
{
   hb_retnl( (LONG) GetSystemMenu( (HWND) hb_parnl(1), hb_parl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEPOPUPMENU )
{
   hb_retnl( (LONG) CreatePopupMenu() );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( LOWORD )
{
   hb_retni( (int) (short) LOWORD( hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( HIWORD )
{
   hb_retni( (int) (short) HIWORD( hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( MAKELONG )
{
   hb_retnl( ((LONG) (((WORD) (hb_parni(1))) | ((DWORD) ((WORD) (hb_parni(2)))) << 16)) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SHOWCARET )
{
   hb_retl( ShowCaret( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( HIDECARET )
{
   hb_retl( HideCaret( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATECARET )
{
   hb_retl( CreateCaret( (HWND) hb_parnl(1), (HBITMAP) hb_parnl(2), hb_parni(3), hb_parni(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DESTROYCARET )
{
   hb_retl( DestroyCaret() );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETCAPTURE )
{
   hb_retnl( (LONG) SetCapture( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( RELEASECAPTURE )
{
   hb_retl( ReleaseCapture() );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETOPENCLIPBOARDWINDOW )
{
   hb_retnl( (LONG) GetOpenClipboardWindow() );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( OPENCLIPBOARD )
{
   hb_retl( OpenClipboard( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( EMPTYCLIPBOARD )
{
   hb_retl( EmptyClipboard() );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SCROLLWINDOW )
{
   PHB_ITEM pStructure = hb_pureparam( 4, HB_IT_ANY );
   PHB_ITEM pStructure2 = hb_pureparam( 5, HB_IT_ANY );
   RECT *lpRect;
   RECT *lpClipRect ;

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);
      lpRect = (RECT *) hb_parc(-1);
   }
   else
   {
      lpRect = NULL;
   }

   if( pStructure2 && HB_IS_OBJECT( pStructure2 ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure2 );
      hb_vmSend(0);
      lpClipRect = (RECT *) hb_parc(-1);
   }
   else
   {
      lpClipRect = NULL;
   }

   hb_retl( ScrollWindow( (HWND) hb_parnl(1), hb_parni(2), hb_parni(3), (CONST RECT *) lpRect, (CONST RECT *) lpClipRect ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETACTIVEWINDOW )
{
   hb_retnl( (LONG) SetActiveWindow( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( BRINGWINDOWTOTOP )
{
   hb_retl( BringWindowToTop( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATECOMPATIBLEDC )
{
   hb_retnl( (LONG) CreateCompatibleDC( (HDC) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( WINDOWFROMDC )
{
   hb_retnl( (LONG) WindowFromDC( (HDC) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETWINDOWDC )
{
   hb_retnl( (LONG) GetWindowDC( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEDC )
{
   DEVMODE *lpInitData ;
   PHB_ITEM pStructure = hb_pureparam( 4, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);
      lpInitData = ( CONST LPDEVMODE ) hb_parc(-1);
   }

   hb_retnl( (ULONG) CreateDC( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2), NULL, ISNIL(4) ? NULL : lpInitData ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( RESETDC )
{
   DEVMODE *lpInitData ;
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);
      lpInitData = ( CONST LPDEVMODE ) hb_parc(-1);

      hb_retnl( (LONG) ResetDCA( (HDC) hb_parnl(1), ISNIL( 2 )? NULL : lpInitData ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "RESETDC", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETDCORGEX )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      POINT *pPoint;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      // pPoint := @Structure:InternalBuffer
      pPoint = (POINT *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      if( GetDCOrgEx( (HDC) hb_parnl(1), pPoint ) )
      {
         //::InternalBuffer := pPoint
         // Already used a Reference to InternalBuffer

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retnl( TRUE );
      }
      else
      {
         hb_retnl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETDCORGEX", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( OFFSETRECT )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      RECT *pRect ;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      // pRect := @Structure:InternalBuffer
      pRect = (RECT *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      if( OffsetRect( pRect, hb_parni( 2 ), hb_parni( 3 ) ) )
      {
         //::InternalBuffer := pRect
         // Already used a Reference to InternalBuffer

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retnl( TRUE );
      }
      else
      {
         hb_retnl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "OFFSETRECT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SCREENTOCLIENT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      POINT *pPoint;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      // pPoint := @Structure:InternalBuffer
      pPoint = (POINT *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      if( ScreenToClient( (HWND) hb_parnl(1), pPoint ) > 0 )
      {
         //::InternalBuffer := pPoint
         // Already used a Reference to InternalBuffer

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retnl( TRUE );
      }
      else
      {
         hb_retnl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SCREENTOCLIENT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CLIENTTOSCREEN )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      POINT *pPoint;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      // pPoint := @Structure:InternalBuffer
      pPoint = (POINT *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      if( ClientToScreen( (HWND) hb_parnl(1), pPoint ) > 0 )
      {
         //::InternalBuffer := pPoint
         // Already used a Reference to InternalBuffer

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retnl( TRUE );
      }
      else
      {
         hb_retnl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "CLIENTTOSCREEN", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETWINDOWPOS )
{
   hb_retl( SetWindowPos( (HWND) hb_parnl(1), (HWND) hb_parnl(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), (UINT) hb_parnl(7) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( BEGINDEFERWINDOWPOS )
{
   hb_retnl( (LONG) BeginDeferWindowPos( hb_parni( 1 ) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DEFERWINDOWPOS )
{
   hb_retnl( (LONG) DeferWindowPos( (HDWP) hb_parnl(1), (HWND) hb_parnl(2), (HWND) hb_parnl(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7), (UINT) hb_parnl(8) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ENDDEFERWINDOWPOS )
{
   hb_retl( EndDeferWindowPos( (HDWP) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SENDDLGITEMMESSAGE )
{
   char *cText;
   PHB_ITEM pText = hb_pureparam( 5, HB_IT_STRING );

   if( pText )
   {
      cText = (char*) hb_xgrab( pText->item.asString.length + 1 );
      hb_xmemcpy( cText, pText->item.asString.value, pText->item.asString.length + 1 );
   }
   else
   {
      cText = NULL;
   }

   hb_retnl( (LONG) SendDlgItemMessage( (HWND) hb_parnl(1), hb_parni(2), (UINT) hb_parnl(3), (WPARAM) hb_parnl(4), (cText ? (LPARAM) cText : (LPARAM) hb_parnl(5)) ) );

   if( pText )
   {
      hb_storclenAdopt( cText, pText->item.asString.length, 5 );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( REDRAWWINDOW )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure == NULL || HB_IS_NIL( pStructure ) )
   {
      hb_retl( RedrawWindow( (HWND) hb_parnl(1), NULL, (HRGN) hb_parnl(3), (UINT) hb_parnl(4) ) );
   }
   else if(  HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( RedrawWindow( (HWND) hb_parnl(1), (CONST RECT*) hb_parc(-1), (HRGN) hb_parnl(3), (UINT) hb_parnl(4) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "REDRAWWINDOW", 4, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ISWINDOWENABLED )
{
   hb_retl( IsWindowEnabled( (HWND) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( VALIDATERECT )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure == NULL || HB_IS_NIL( pStructure ) )
   {
      hb_retl( ValidateRect( (HWND) hb_parnl(1), NULL ) );
   }
   else if(  HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( ValidateRect( (HWND) hb_parnl(1), (CONST RECT*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "VALIDATERECT", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( MAPWINDOWPOINTS )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   // Note: has to be a POINT or a RECT structure.
   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      POINT *pPoints;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      // pPoint := @Structure:InternalBuffer
      pPoints = (POINT *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      hb_retni( MapWindowPoints( (HWND) hb_parnl(1), (HWND) hb_parnl(2), (CONST LPPOINT) pPoints, (UINT) hb_parnl(4) ) );

      if( hb_parnl(4 ) <= 2 )
      {
         //::InternalBuffer := pPoints
         // Already used a Reference to InternalBuffer

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "MAPWINDOWPOINTS", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DELETEDC )
{
   hb_retl( DeleteDC( (HDC) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( DELETEMENU )
{
  hb_retl( DeleteMenu( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), (UINT) hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETMENUITEMINFO )
{
   PHB_ITEM pStructure = hb_pureparam( 4, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( SetMenuItemInfo( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), hb_parl(3), (CONST MENUITEMINFOA*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETMENUITEMINFO", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( INSERTMENUITEM )
{
   PHB_ITEM pStructure = hb_pureparam( 4, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( InsertMenuItem( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), hb_parl(3), (CONST MENUITEMINFOA*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "INSERTMENUITEM", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( ISMENU )
{
   hb_retl( IsMenu((HMENU) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETMENUDEFAULTITEM )
{
   hb_retnl( SetMenuDefaultItem( (HMENU) hb_parnl(1), hb_parnl(2), hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETMENU )
{
   hb_retl( SetMenu( (HWND) hb_parnl(1), (HMENU) hb_parnl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETMENU )
{
   hb_retnl( (LONG) GetMenu( (HWND) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( CREATEMENU )
{
   hb_retnl( (LONG) CreateMenu() );
}

//-----------------------------------------------------------------------------
HB_FUNC( APPENDMENU )
{
   hb_retl( AppendMenu( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), (UINT_PTR) hb_parnl(3), (LPCTSTR) hb_parc(4) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( INSERTMENU )
{
   hb_retl( InsertMenu( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), (UINT) hb_parnl(3), (UINT) hb_parnl(4), (LPCTSTR) hb_parc(5) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( MODIFYMENU )
{
   hb_retl( ModifyMenu( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), (UINT) hb_parnl(3), (UINT) hb_parnl(4), (LPCTSTR) hb_parc(5) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( TRACKPOPUPMENU )
{
   hb_retnl( TrackPopupMenu( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), hb_parni(3), hb_parni(4), hb_parni(5), (HWND) hb_parnl(6), NULL ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETFOREGROUNDWINDOW )
{
   hb_retl( SetForegroundWindow( (HWND) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETFOREGROUNDWINDOW )
{
   hb_retnl( (LONG) GetForegroundWindow() );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETCLASSLONG )
{
   hb_retnl( (LONG) GetClassLong( (HWND) hb_parnl(1), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( CALLNEXTHOOKEX )
{
    CallNextHookEx( (HHOOK) hb_parnl(1), hb_parni(2), (WPARAM) hb_parnl(3), (LPARAM) hb_parnl(4) );
}

//-----------------------------------------------------------------------------
HB_FUNC( MENUITEMFROMPOINT )
{
   PHB_ITEM pStructure = hb_pureparam( 3, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( MenuItemFromPoint( (HWND) hb_parnl(1), (HMENU) hb_parnl(2), *(POINT*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "MENUITEMFROMPOINT", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( GETCLASSNAME )
{
   char *cText= (char*) hb_xgrab( MAX_PATH+1 );

   GetClassName( (HWND) hb_parnl(1), (LPSTR) cText, MAX_PATH );

   hb_retcAdopt( cText);
}

//-----------------------------------------------------------------------------
HB_FUNC( DESTROYMENU )
{
  hb_retl( DestroyMenu( (HMENU) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETCURRENTTHREADID )
{
   hb_retnl( (DWORD) GetCurrentThreadId() );
}

//-----------------------------------------------------------------------------
HB_FUNC( POSTTHREADMESSAGE )
{
   hb_retl( PostThreadMessage( (DWORD) hb_parnl(1), (UINT) hb_parnl(2), (WPARAM) hb_parnl(3), (LPARAM) hb_parnl(4) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( POLYGON )
{
   PHB_ITEM pArray = hb_pureparam( 2, HB_IT_ARRAY );

   if( pArray )
   {
      POINT *pPoints;
      int iCount = pArray->item.asArray.value->ulLen;
      int i;

      pPoints  = (POINT *) hb_xgrab( iCount * sizeof( POINT ) );

      for ( i = 0; i < iCount; i++ )
      {
         hb_vmPushSymbol( pVALUE->pSymbol );
         hb_vmPush( hb_itemArrayGet( pArray, i + 1 ) );
         hb_vmSend(0);

         pPoints[ i ] = *( (POINT *) hb_parc(-1) );
      }

      hb_retl( Polygon( (HDC) hb_parnl(1), pPoints, iCount ) );

      hb_xfree( (void *) pPoints );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "POLYGON", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//-----------------------------------------------------------------------------
//HB_FUNC( UNHOOKWINDOWSHOOKEX )
//{
//   UnhookWindowsHookEx( (HHOOK) hb_parnl(1) );
//}

//-----------------------------------------------------------------------------
HB_FUNC( MOVEWINDOW )
{
   hb_retl( MoveWindow( (HWND) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), (ISNIL(6) ? TRUE : hb_parl(6) ) ));
}

//-----------------------------------------------------------------------------
HB_FUNC( UPDATEWINDOW )
{
   hb_retl( UpdateWindow( (HWND) hb_parnl(1)) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETTIMER )
{
   hb_retni( SetTimer( (HWND) hb_parnl(1), (UINT) hb_parnl(2), (UINT) hb_parnl(3), ISNIL(4) ? NULL : (TIMERPROC) hb_parnl(4) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( KILLTIMER )
{
   hb_retl( KillTimer( (HWND) hb_parnl(1), (UINT) hb_parnl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( LOADIMAGE )
{
   hb_retnl( (LONG) LoadImage( ISNIL(1) ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl(1),
                               (hb_parinfo(2) == HB_IT_STRING ? hb_parc(2) : MAKEINTRESOURCE( (WORD) hb_parni(2))),
                               (UINT) hb_parnl(3), hb_parni(4), hb_parni(5), (UINT) hb_parnl(6) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( LOADBITMAP )
{
   hb_retnl( (LONG) LoadBitmap( ISNIL(1) ? GetModuleHandle( NULL ): (HINSTANCE) hb_parnl(1),
                                hb_parinfo(2)==HB_IT_STRING ? (LPCTSTR) hb_parc(2) : MAKEINTRESOURCE( (WORD) hb_parni(2) ) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( CREATEPATTERNBRUSH )
{
   hb_retnl( (LONG) CreatePatternBrush( (HBITMAP) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( CREATEMDIWINDOW )
{
   hb_retnl( (LONG) CreateMDIWindow( (LPCSTR) hb_parc(1), (LPCSTR) hb_parc(2), (DWORD) hb_parnd(3),
                                hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7),
                                (HWND) hb_parnl(8), (HINSTANCE) hb_parnl(9), (LPARAM) hb_parnl(10) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETWINDOWRECT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      RECT *pRect = (RECT *) hb_xgrab( sizeof( RECT ) );

      if( GetWindowRect( (HWND) hb_parnl(1), pRect ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            //HB_CStructure( "MSG" )
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "RECT", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         //::InternalBuffer := pRect
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pRect, sizeof( RECT ) );

         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pRect );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETWINDOWRECT", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( DRAWTEXT )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_OBJECT );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      RECT *pRect;
      LPCTSTR cText = hb_parc(2);

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      // pRect := @Structure:InternalBuffer
      pRect = (RECT *)( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      hb_retni( DrawText( (HDC) hb_parnl(1), (LPCTSTR) cText, strlen(cText), pRect, ISNIL(4) ? DT_LEFT : (UINT) hb_parnl(4) ) );

      //::InternalBuffer := pRect
      // Already used a Reference to InternalBuffer

      if( ISBYREF(3) )
      {
         //::DeValue()
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DRAWTEXT", 4, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( SETBKMODE )
{
   hb_retni( SetBkMode( (HDC) hb_parnl(1), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETSYSTEMMETRICS )
{
   hb_retni( GetSystemMetrics( hb_parni(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETKEYSTATE )
{
   hb_retni( GetKeyState( hb_parni(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( CHECKBIT )
{
   hb_retl( hb_parnl(1) & ( 1 << ( hb_parni(2) - 1 ) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETDLGITEM )
{
   hb_retnl( (LONG) GetDlgItem( (HWND) hb_parnl(1), hb_parni(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETPARENT )
{
   hb_retnl( (LONG) SetParent( (HWND) hb_parnl(1), (HWND) hb_parnl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETWINDOWRGN )
{
   hb_retni( SetWindowRgn( (HWND) hb_parnl(1), (HRGN) hb_parnl(2), hb_parl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETPROFILESTRING )
{
   DWORD nSize = 1024 ;
   LPTSTR bBuffer = (LPTSTR) hb_xgrab( nSize );
   DWORD dwLen ;
   const char * lpSection = hb_parc(1);
   const char * lpEntry   = hb_parc(2);
   const char * lpDefault = hb_parc(3);

   while( TRUE )
   {
      dwLen = GetProfileString( lpSection , lpEntry ,lpDefault , bBuffer, nSize );

      if( ( ( ( lpSection == NULL ) || ( lpEntry == NULL ) ) && ( dwLen == nSize - 2 ) ) || ( ( lpSection && lpEntry ) && ( dwLen == nSize - 1 ) ) )
      {
         nSize *= 2;
         bBuffer = (LPSTR) hb_xrealloc( (void *) bBuffer, nSize );
      }
      else
      {
         break ;
      }
   }

   if( dwLen )
   {
      hb_retclen( ( char * ) bBuffer, dwLen );
   }
   else
   {
      hb_retc( lpDefault );
   }

   hb_xfree( bBuffer );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETPRIVATEPROFILESTRING )
{
   DWORD nSize = 1024 ;
   LPTSTR bBuffer = (LPTSTR) hb_xgrab( nSize );
   DWORD dwLen ;
   const char * lpSection  = hb_parc(1);
   const char * lpEntry    = hb_parc(2);
   const char * lpDefault  = hb_parc(3);
   const char * lpFileName = hb_parc(4);

   while ( TRUE )
   {
      dwLen = GetPrivateProfileString( lpSection , lpEntry ,lpDefault , bBuffer, nSize , lpFileName);

      if ( ( ( ( lpSection == NULL ) || ( lpEntry == NULL ) ) && ( dwLen == nSize - 2 ) ) || ( ( lpSection && lpEntry ) && ( dwLen == nSize - 1 ) ) )
      {
        nSize *= 2 ;
        bBuffer = (LPTSTR) hb_xrealloc( (void *) bBuffer, nSize );
      }
      else
      {
        break ;
      }
   }

   if( dwLen )
   {
      hb_retclen( ( char * ) bBuffer, dwLen );
   }
   else
   {
      hb_retc( lpDefault );
   }

   hb_xfree( bBuffer );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( WRITEPROFILESECTION )
{
   hb_retl( WriteProfileSection( (LPCSTR) hb_parc(1), (LPCSTR) hb_parc(2) ) ) ;
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( WRITEPROFILESTRING )
{
   hb_retl( WriteProfileString( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2), (LPCTSTR) hb_parc(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( WRITEPRIVATEPROFILESECTION )
{
   hb_retl( WritePrivateProfileSection( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2), (LPCTSTR) hb_parc(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETPRIVATEPROFILEINT )
{
   hb_retni( GetPrivateProfileInt( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2), hb_parni(3), (LPCTSTR) hb_parc(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( WRITEPRIVATEPROFILESTRING )
{
   hb_retl( WritePrivateProfileString( (LPCTSTR) hb_parc(1), (LPCSTR) hb_parc(2), (LPCSTR) hb_parc(3), (LPCSTR) hb_parc(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETPROFILEINT )
{
   hb_retni( GetProfileInt( (LPCSTR) hb_parc(1), (LPCSTR) hb_parc(2), hb_parni(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SYSTEMPARAMETERSINFO )
{
   BOOL bRet;
   int iLen = hb_parni(2);
   PHB_ITEM pParam = hb_param( 3, HB_IT_BYREF );

   if( pParam && HB_IS_OBJECT( pParam ) )
   {
      //void *pBuffer;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pParam );
      hb_vmSend(0);

      //pBuffer = (void *) ( pParam->item.asArray.value->pItems + pParam->item.asArray.value->ulLen - 1 )->item.asString.value;
      bRet = SystemParametersInfo( (UINT) hb_parnl(1), iLen, (void *) hb_parc(-1), (UINT) hb_parnl(4) );

      if( bRet )
      {
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pParam );
         hb_vmSend(0);
      }
   }
   else if( ISBYREF(3) )
   {
      long nVal;

      bRet = SystemParametersInfo( (UINT) hb_parni(1), (UINT) hb_parni(2), &nVal, (UINT) hb_parni(4) );
      hb_stornl( nVal, 3 );
   }
   else
   {
      bRet = SystemParametersInfo( (UINT) hb_parni(1), (UINT) hb_parni(2), (PVOID) hb_parnl(3), (UINT) hb_parni(4) );
   }

   hb_retl( bRet );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETOBJECT )
{
   HGDIOBJ hObj = (HGDIOBJ) hb_parnl(1);
   int iBuffer = hb_parni( 2 );
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pStructure && strncmp( hb_objGetClsName( pStructure ), "C Structure", 11 ) == 0 )
   {
      LPVOID pBuffer = (LPVOID) hb_xgrab( iBuffer );
      int iResult = GetObject( hObj, iBuffer, pBuffer );

      hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pBuffer, iBuffer );

      hb_vmPushSymbol( pDEVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      hb_retni( iResult );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETOBJECT", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}


//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEFONTINDIRECT )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( (LONG) CreateFontIndirect( (CONST LPLOGFONT) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "CREATEFONTINDIRECT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CHOOSEFONT )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );
   LPCHOOSEFONT pCf;

   if( pStructure )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      pCf = (LPCHOOSEFONT) hb_parc(-1);

      if( ChooseFont( pCf ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "CHOOSEFONT", 10 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();

            hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pCf, sizeof( CHOOSEFONT ) );
         }

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "CHOOSEFONT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETSTOCKOBJECT )
{
   hb_retnl( (LONG) GetStockObject( hb_parni(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETCURSORPOS )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      POINT *pPoint = (POINT *) hb_xgrab( sizeof( POINT ) );

      if( GetCursorPos( pPoint ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "POINT", 5 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pPoint, sizeof( POINT ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pPoint );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETCURSORPOS", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATERECTRGN )
{
   hb_retnl( (LONG) CreateRectRgn( hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETDCEX )
{
   hb_retnl( (LONG) GetDCEx( (HWND) hb_parnl(1), (HRGN) hb_parnl(2), (DWORD) hb_parnl(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( PTINRECT )
{
   PHB_ITEM pStructure  = hb_pureparam( 1, HB_IT_ANY );
   PHB_ITEM pStructure2 = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && pStructure2 && HB_IS_OBJECT( pStructure ) && HB_IS_OBJECT( pStructure2 ) )
   {
      RECT *pRect;
      POINT *pPoint;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);
      pRect =  (RECT *) hb_parc(-1);

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure2 );
      hb_vmSend(0);
      pPoint = (POINT *) hb_parc(-1);

      hb_retl( PtInRect( (CONST RECT*) pRect, *pPoint ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "PTINRECT", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( RECTANGLE )
{
   hb_retl( Rectangle( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( LINETO )
{
   hb_retl( LineTo( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEPEN )
{
   hb_retnl( (LONG) CreatePen( hb_parni(1), hb_parni(2), (COLORREF) hb_parnl(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( MOVETOEX )
{
   PHB_ITEM pStructure  = hb_pureparam( 4, HB_IT_ANY );

   if( pStructure == NULL || HB_IS_NIL( pStructure ) )
   {
      hb_retnl( MoveToEx( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), NULL ) );
   }
   else if(  HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( MoveToEx( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), (CONST LPPOINT) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "MOVETOEX", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETBKCOLOR )
{
   hb_retnl( (ULONG) SetBkColor( (HDC) hb_parnl(1), (COLORREF) hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETBKCOLOR )
{
   hb_retnl( (ULONG) GetBkColor( (HDC) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETTEXTCOLOR )
{
   hb_retnl( (ULONG) GetTextColor( (HDC) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETTEXTALIGN )
{
   hb_retni( SetTextAlign( (HDC) hb_parnl(1), (UINT) hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
/*
HB_FUNC( POLYLINE )
{
   PHB_ITEM pArray = hb_pureparam( 2, HB_IT_ARRAY );

   if( pArray )
   {
      POINT *pPoints;
      int iCount = pArray->item.asArray.value->ulLen;
      int i;

      pPoints  = (POINT *) hb_xgrab( iCount * sizeof( POINT ) );

      for ( i = 0; i < iCount; i++ )
      {
         hb_vmPushSymbol( pVALUE->pSymbol );
         hb_vmPush( hb_itemArrayGet( pArray, i + 1 ) );
         hb_vmSend(0);

         pPoints[ i ] = *( (POINT *) hb_parc(-1) );
      }

      hb_retl( Polyline( (HDC) hb_parnl(1), pPoints, iCount ) );
      hb_xfree( (void *) pPoints );
   }
   else
   {
      hb_retl(0);
   }
}
*/

/*
   must use this version due to:

   Unrecoverable error 9027:
   Premature Array/Object Release detected 012C92E8

   Called from LEN(0)
   Called from C Structure POINT:VALUE(526)
   Called from POLYLINE(0)
   Called from DATAGRID:DISPLAYDATA(1160)

   "... I think it might me releasing my "STATIC" structure ????, see Ron"
   useage:

   static pt

   DEFAULT pt TO (struct POINT)

   pt:x := nRight-1
   pt:y := nBottom

   pt2:x := nRight-1
   pt2:y := nTop

   PolyLine( hDC, { pt, pt2 } )

   The following function works fine !
*/

HB_FUNC( POLYLINE )
{
   POINT * Point ;
   int iCount ;
   int i ;
   PHB_ITEM aParam ;
   PHB_ITEM pStructure;

   if (ISARRAY( 2 ) )
   {
       iCount = (int) hb_parinfa( 2, 0 );
       Point  = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          pStructure = hb_itemArrayGet( aParam, i+1 );

          hb_vmPushSymbol( pVALUE->pSymbol );
          hb_itemPushForward( pStructure );
          hb_vmSend(0);

          *(Point+i) = *(POINT*) hb_parc(-1);
       }

       hb_retl( Polyline( (HDC) hb_parnl(1), Point, iCount ) );
       hb_xfree(Point);
   }
   else
   {
      hb_retl(0);
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( EXTTEXTOUT )
{
   PHB_ITEM pStructure  = hb_pureparam( 5, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      const char * cText = hb_parc(6);
      PINT pDx;
      UINT i;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      if( ISARRAY(7) )
      {
          UINT iCount = hb_parinfa( 7, 0 );

          pDx = (PINT) hb_xgrab( iCount * sizeof( INT ) );

          for( i = 0; i < iCount; i++ )
          {
             pDx[ i ] = hb_parni( 7, i + 1 );
          }
      }

      hb_retl( ExtTextOut( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), (UINT) hb_parnl(4), (CONST RECT*) hb_parc(-1),
                         (LPCSTR) cText, (UINT) strlen(cText), ISARRAY(7) ? pDx : NULL ) );

      if( ISARRAY(7) )
      {
         hb_xfree( (void *) pDx );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "EXTTEXTOUT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DRAWFOCUSRECT )
{
   PHB_ITEM pStructure  = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( (BOOL) DrawFocusRect( (HDC) hb_parnl(1), (CONST RECT*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DRAWFOCUSRECT", 1, hb_paramError(1), hb_paramError(2) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( FRAMERECT )
{
   PHB_ITEM pStructure  = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( (BOOL) FrameRect( (HDC) hb_parnl(1), (CONST RECT*) hb_parc(-1), (HBRUSH) hb_parnl(3) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "FRAMERECT", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETDEVICECAPS )
{
   hb_retni( GetDeviceCaps( (HDC) hb_parnl(1), hb_parni(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETCLIPBOX )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );
   int iRet;
   if( pStructure )
   {
      RECT *pRect = (RECT *) hb_xgrab( sizeof( RECT ) );

      iRet = GetClipBox( (HDC) hb_parnl(1), pRect );
      if( iRet  != ERROR )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "RECT", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pRect, sizeof( RECT ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
         hb_retni( iRet );
      }
      else
      {
         hb_xfree( (void *) pRect );
         hb_retni( iRet );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETCLIPBOX", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DRAWFRAMECONTROL )
{
   PHB_ITEM pStructure  = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( DrawFrameControl( (HDC) hb_parnl(1), (RECT*) hb_parc(-1), (UINT) hb_parnl(3), (UINT) hb_parnl(4) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DRAWFRAMECONTROL", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( DRAWEDGE )
{
   PHB_ITEM pStructure  = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retni( DrawEdge( (HDC) hb_parnl(1), (RECT*) hb_parc(-1), (UINT) hb_parnl(3), (UINT) hb_parnl(4) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DRAWEDGE", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( TRANSMITCOMMCHAR )
{
   hb_retl( TransmitCommChar( ( HANDLE ) hb_parnl(1),  hb_parc(2)[0] ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETCAPTURE )
{
   hb_retnl( (LONG) GetCapture() );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( DRAWSTATE )
{
   hb_retl( DrawState( (HDC) hb_parnl(1), (HBRUSH) hb_parnl(2), NULL, (LPARAM) hb_parnl(3), (WPARAM) hb_parnl(4), hb_parni(5), hb_parni(6), hb_parni(7), hb_parni(8), (UINT) hb_parni(9) ) );
}

//------------------------------------------------------------------------------------------------

HB_FUNC( DESTROYICON )
{
   hb_retl( DestroyIcon( (HICON) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------

HB_FUNC( DRAWICONEX )
{
   hb_retl( DrawIconEx( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), (HICON) hb_parnl(4), hb_parni(5), hb_parni(6), (UINT) hb_parni(7), (HBRUSH) hb_parnl(8), (UINT) hb_parni(9) ) );
}

//------------------------------------------------------------------------------------------------

HB_FUNC( GETSUBMENU )
{
   hb_retnl( (LONG) GetSubMenu( (HMENU) hb_parnl(1), hb_parni(2) ) );
}

//------------------------------------------------------------------------------------------------
#include <Lmcons.h>

HB_FUNC( GETUSERNAME )
{
   DWORD nSize = UNLEN + 1;
   char szUser[ UNLEN + 1 ];

   if ( GetUserName( szUser, &nSize ) )
   {
      hb_retclen( szUser, nSize-1 );
   }
   else
   {
      hb_retc("");
   }
}
//------------------------------------------------------------------------------------------------

HB_FUNC( GLOBALMEMORYSTATUSEX )
{
   PHB_ITEM pStructure  = hb_param( 1, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      if( pGlobalMemoryStatusEx )
      {
         MEMORYSTATUSEX *pMem;

         hb_vmPushSymbol( pVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         // pMem := @Structure:InternalBuffer
         pMem = (MEMORYSTATUSEX *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;
         pMem->dwLength = sizeof(pMem);

         pGlobalMemoryStatusEx( (MEMORYSTATUSEX*) pMem );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GLOBALMEMORYSTATUSEX", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GLOBALMEMORYSTATUS )
{
   PHB_ITEM pStructure  = hb_param( 1, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      MEMORYSTATUS *pMem;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      pMem = (MEMORYSTATUS *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      GlobalMemoryStatus( (MEMORYSTATUS*) pMem );

      hb_vmPushSymbol( pDEVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GLOBALMEMORYSTATUS", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( FORMATMESSAGE )
{
   LPVOID Buffer;
   DWORD nRet;

   //TODO: add support for array of arguments.

   nRet =  FormatMessage( ISNIL(1) ? FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS : (DWORD) hb_parnl(1),
                          ISNIL(2) ? NULL : hb_parc(2),
                          ISNIL(3) ? GetLastError() : (DWORD) hb_parnl(3),
                          ISNIL(4) ? (DWORD )NULL : (DWORD) hb_parnl(4),
                          (LPTSTR) &Buffer,
                          ISNIL(6) ? (DWORD) NULL : (DWORD) hb_parnl(6),
                          NULL );

   if ( nRet > 0 )
   {
      hb_retc( (char *) Buffer );
      LocalFree( Buffer );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( INFLATERECT )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      RECT *pRect ;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      pRect = (RECT *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      if( InflateRect( pRect, hb_parni( 2 ), hb_parni( 3 ) ) )
      {
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retnl( TRUE );
      }
      else
      {
         hb_retnl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "INFLATERECT", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( ISCHILD )
{
   hb_retl( (BOOL) IsChild( (HWND) hb_parnl(1), (HWND) hb_parnl(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETCOMPUTERNAME )
{
   char cText[ MAX_COMPUTERNAME_LENGTH + 1 ];
   DWORD nSize = MAX_COMPUTERNAME_LENGTH + 1;

   if( GetComputerName( (LPSTR) &cText, &nSize ) )
   {
      hb_retclen( cText, nSize );
   }
   else
   {
      hb_retc("");
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETPARENT )
{
   hb_retnl( (LONG) GetParent( (HWND) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETDLGCTRLID )
{
   hb_retni( GetDlgCtrlID( (HWND) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( MESSAGEBEEP )
{
   hb_retl( MessageBeep( ISNIL(1) ? 0xFFFFFFFF : hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SETCLIPBOARDDATA )
{
   SetClipboardData( (UINT) hb_parni(1), (HANDLE) hb_parnl(2) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( ISCLIPBOARDFORMATAVAILABLE )
{
   hb_retl( IsClipboardFormatAvailable( hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGISTERCLIPBOARDFORMAT )
{
   hb_retni( RegisterClipboardFormat( (LPCSTR) hb_parc(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETCLIPBOARDDATA )
{
   HANDLE hClipMem;
   LPSTR  lpClip;

   hClipMem = GetClipboardData( (UINT) hb_parni(1) );

   if( hClipMem )
   {
      lpClip = (LPSTR)  GlobalLock(hClipMem);

      hb_retc( lpClip );

      GlobalUnlock( hClipMem );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CLOSECLIPBOARD )
{
   hb_retl( CloseClipboard() );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SETPROP )
{
   hb_retl( SetProp( (HWND) hb_parnl(1), (LPCSTR) hb_parc(2), (HANDLE) hb_parnl(3) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REMOVEPROP )
{
   hb_retnl( (LONG) RemoveProp( (HWND) hb_parnl(1), (LPCSTR) hb_parc(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETPROP )
{
   hb_retnl( (LONG) GetProp( (HWND) hb_parnl(1), (LPCSTR) hb_parc(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( ENDDIALOG )
{
  EndDialog( (HWND) hb_parnl(1) , hb_parni(2) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( LOADRESOURCE )
{
   hb_retnl( (LONG) LoadResource( (HMODULE) hb_parnl(1), (HRSRC) hb_parnl(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( LOCKRESOURCE )
{
   hb_retc( (char*) LockResource( (HGLOBAL) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( UNLOCKRESOURCE )
{
   hb_retnl( (LONG) UnlockResource( (HGLOBAL) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( FREERESOURCE )
{
   hb_retnl( (LONG) FreeResource( (HGLOBAL) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( FINDRESOURCE )
{
   hb_retnl( (LONG) FindResource( (HMODULE) hb_parnl(1), (LPCSTR) hb_parc(2), (LPCSTR) hb_parnl(3) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( FINDRESOURCEEX )
{
   hb_retnl( (LONG) FindResourceEx( (HMODULE) hb_parnl(1), (LPCSTR) hb_parc(2), (LPCSTR) hb_parnl(3), (WORD) hb_parni(4) ) );
}

//------------------------------------------------------------------------------------------------
// Temporary function SYSNONCLIENTMETRICS to fill NONCLIENTMETRICS structure, this function should
// be replaced by SYSTEMPARAMETERSINFO
//------------------------------------------------------------------------------------------------
HB_FUNC( SYSNONCLIENTMETRICS )
{
   PHB_ITEM pStructure  = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      NONCLIENTMETRICS *pNcm = (NONCLIENTMETRICS *) hb_xgrab( sizeof( NONCLIENTMETRICS ) );
      pNcm->cbSize = sizeof( NONCLIENTMETRICS );

      if( SystemParametersInfo( SPI_GETNONCLIENTMETRICS, pNcm->cbSize, pNcm, 0 ) != 0 )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "NONCLIENTMETRICS", 16 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pNcm, sizeof( NONCLIENTMETRICS ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retc( pNcm->lfMessageFont.lfFaceName );
         return;
      }
      else
      {
         hb_xfree( (void *) pNcm );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SYSNONCLIENTMETRICS", 2, hb_paramError(1), hb_paramError(2) );
   }
}

HB_FUNC( DEFWINDOWPROCADDRESS )
{
   hb_retnl( (long) DefWindowProc );
}

HB_FUNC( DEFFRAMEPROCADDRESS )
{
   hb_retnl( (long) DefFrameProc );
}

HB_FUNC( DEFMDICHILDPROCADDRESS )
{
   hb_retnl( (long) DefMDIChildProc );
}

HB_FUNC( GETASYNCKEYSTATE )
{
   hb_retni( GetAsyncKeyState( hb_parni(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SCROLLWINDOWEX )
{
   PHB_ITEM pStructure = hb_pureparam( 4, HB_IT_ANY );
   PHB_ITEM pStructure2 = hb_pureparam( 5, HB_IT_ANY );
   PHB_ITEM pStructure3 = hb_pureparam( 7, HB_IT_BYREF );

   RECT *lpRect;
   RECT *lpClipRect ;
   LPRECT prcUpdate;

   int iRet;

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);
      lpRect = (RECT *) hb_parc(-1);
   }
   else
   {
      lpRect = NULL;
   }

   if( pStructure2 && HB_IS_OBJECT( pStructure2 ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure2 );
      hb_vmSend(0);
      lpClipRect = (RECT *) hb_parc(-1);
   }
   else
   {
      lpClipRect = NULL;
   }

   if( pStructure3 )
   {
      prcUpdate = (LPRECT) hb_xgrab( sizeof( RECT ) );
   }
   else
   {
      prcUpdate = NULL;
   }

   iRet = ScrollWindowEx( (HWND) hb_parnl(1), hb_parni(2), hb_parni(3), (CONST RECT *) lpRect, (CONST RECT *) lpClipRect, (HRGN) hb_parnl(6), prcUpdate, hb_parni(8) );

   if( iRet == ERROR )
   {
      if( prcUpdate )
      {
         hb_xfree( (void *) prcUpdate );
      }
   }
   else
   {
      if( prcUpdate )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure3 ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "RECT", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure3 = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure3->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) prcUpdate, sizeof( RECT ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure3 );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure3 );
         }
      }
   }

   hb_retni( iRet );
}

//-----------------------------------------------------------------------------
HB_FUNC( DRAWICON )
{
   hb_retl( DrawIcon( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), (HICON) hb_parnl(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETOPENFILENAME )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      OPENFILENAME *pOfn;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      pOfn = (OPENFILENAME *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      if( GetOpenFileName( pOfn ) )
      {
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETOPENFILENAME", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETSAVEFILENAME )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      OPENFILENAME *pOfn;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      pOfn = (OPENFILENAME *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      if( GetSaveFileName( pOfn ) )
      {
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETSAVEFILENAME", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( WINEXEC )
{
   hb_retni( WinExec( (LPCSTR) hb_parc(1), (UINT) hb_parni( 2 ) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SHELLEXECUTE )
{
   hb_retnl( (LONG) ShellExecute( (HWND) hb_parnl(1), (LPCSTR) hb_parc(2), (LPCSTR) hb_parc(3), (LPCSTR) hb_parc(4), (LPCSTR) hb_parc(5), hb_parni(6) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETLOGICALDRIVESTRINGS )
{
   hb_retnl( (LONG) GetLogicalDriveStrings( (DWORD) hb_parnl(1), (LPSTR) hb_parc(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( COPYFILE )
{
   hb_retl( CopyFile( (LPCSTR) hb_parc(1), (LPCSTR) hb_parc(2), (BOOL) hb_parl(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( WRITEFILE )
{
   DWORD pWritten;
   hb_retl( WriteFile( (HANDLE) hb_parnl(1), (LPBYTE) hb_parc(2), (DWORD) hb_parni(3), &pWritten, NULL ) );
   hb_stornl( pWritten, 4 );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( READFILE )
{
   PHB_ITEM pStructure = hb_pureparam( 5, HB_IT_ANY );
   char * cBuffer = (char *) hb_xgrab( hb_parnl(3) );
   DWORD nRead = 0;
   BOOL  bRet;
   OVERLAPPED *pOl;

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);
      pOl = (OVERLAPPED *) hb_parc(-1);
   }
   else
   {
      pOl = NULL;
   }

   bRet = ReadFile( (HANDLE) hb_parnl(1), cBuffer, (DWORD) hb_parnl(3), &nRead, pOl );

   if( bRet )
   {
      hb_storclen( (char *) cBuffer, nRead, 2 );
   }
   hb_stornl( nRead, 4 );
   hb_xfree( cBuffer );
   hb_retl( bRet );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CANCELIO )
{
   hb_retl( CancelIo( (HANDLE) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( FLUSHFILEBUFFERS )
{
   hb_retl( FlushFileBuffers( (HANDLE) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( COPYFILEEX )
{
   BOOL bCancel = FALSE;
   hb_retl( CopyFileEx( (LPCSTR) hb_parc(1), (LPCSTR) hb_parc(2), (LPPROGRESS_ROUTINE) hb_parnl(3), NULL, (LPBOOL) bCancel, hb_parnl(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETROP2 )
{
   hb_retni( SetROP2( (HDC) hb_parnl(1), hb_parni( 2 ) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEBITMAP )
{
   hb_retnl( (LONG) CreateBitmap( hb_parni(1), hb_parni(2), (UINT) hb_parni(3), (UINT) hb_parni(4), hb_parc(5) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( BITBLT )
{
   hb_retl( BitBlt( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), (HDC) hb_parnl(6), hb_parni(7), hb_parni(8), (DWORD) hb_parnl(9) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( MASKBLT )
{
   hb_retl( MaskBlt( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), (HDC) hb_parnl(6), hb_parni(7), hb_parni(8), (HBITMAP) hb_parnl(9), hb_parni(10), hb_parni(11), (DWORD) hb_parnl(12) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( TRANSPARENTBLT )
{
   hb_retl( TransparentBlt( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), (HDC) hb_parnl(6), hb_parni(7), hb_parni(8), hb_parni(9), hb_parni(10), (UINT) hb_parni(11) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( PATBLT )
{
   hb_retl( PatBlt( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), (DWORD) hb_parnl(6) ) );
}


//-------------------------------------------------------------------------------------------------
HB_FUNC( CHECKRADIOBUTTON )
{
    hb_retl( CheckRadioButton( (HWND) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4) ) );
}

HB_FUNC( GETPROCADDRESS )
{
   hb_retnl( (LONG) GetProcAddress( (HMODULE) hb_parnl(1), (LPCSTR) hb_parc(2) ) );
}

#if 0 // xHarbour DllCall.c
//-------------------------------------------------------------------------------------------------
HB_FUNC( LOADLIBRARY )
{
   hb_retnl( (LONG) LoadLibraryA( (LPCSTR) hb_parc(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( FREELIBRARY )
{
   hb_retl( FreeLibrary( (HMODULE) hb_parnl(1) ) );
}
#endif

//-------------------------------------------------------------------------------------------------
HB_FUNC( LOADLIBRARYEX )
{
   hb_retnl( (LONG) LoadLibraryExA( (LPCSTR) hb_parc(1), (HANDLE) hb_parnl(2), (DWORD) hb_parnl(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( WINDOWFROMPOINT )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      hb_retnl( (LONG) WindowFromPoint( *(POINT*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "WINDOWFROMPOINT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CHILDWINDOWFROMPOINT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      hb_retnl( (LONG) ChildWindowFromPoint( (HWND) hb_parnl(1), *(POINT*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ChildWindowFromPoint", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( REALCHILDWINDOWFROMPOINT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_OBJECT );

   if( pRealChildWindowFromPoint == NULL )
   {
      hb_errRT_BASE( EG_ARG, 6999, NULL, "RealChildWindowFromPoint", 1, hb_paramError(1) );
      return;
   }

   if( pStructure )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      hb_retnl( (LONG) pRealChildWindowFromPoint( (HWND) hb_parnl(1), *(POINT*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "RealChildWindowFromPoint", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CHILDWINDOWFROMPOINTEX )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      hb_retnl( (LONG) ChildWindowFromPointEx( (HWND) hb_parnl(1), *(POINT*) hb_parc(-1), hb_parnl(3) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ChildWindowFromPointEx", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( INVERTRECT )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( (BOOL) InvertRect( (HDC) hb_parnl(1), (CONST RECT *) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "INVERTRECT", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETDLGITEMINT )
{
   hb_retl( SetDlgItemInt( (HWND) hb_parnl(1), hb_parni(2), (UINT) hb_parni(3), hb_parl(4) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( SETDLGITEMTEXT )
{
    SetDlgItemText( (HWND) hb_parnl(1), hb_parni(2), (LPCTSTR) hb_parc(3) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETDLGITEMTEXT )
{
   char *cText;
   UINT uiLen = 0;
   BOOL bByRef;

   PHB_ITEM pBuffer = hb_pureparam( 3, HB_IT_ANY );

   if( pBuffer && HB_IS_BYREF( pBuffer ) )
   {
      bByRef = TRUE;
   }
   else
   {
      bByRef = FALSE;
   }

   if( uiLen == 0 )
   {
      uiLen = SendMessage( GetDlgItem( (HWND) hb_parnl(1), hb_parni(2) ), WM_GETTEXTLENGTH, 0, 0 ) + 1 ;
   }

   cText = (char *) hb_xgrab( uiLen );

   uiLen = GetDlgItemText( (HWND) hb_parnl(1), hb_parni(2), (LPTSTR) cText, uiLen );

   if( bByRef )
   {
      hb_storclenAdopt( cText, uiLen, 3 );
      hb_retnl( (long) uiLen );
   }
   else
   {
      hb_retclenAdopt( cText, uiLen );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( DPTOLP )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      POINT *pPoint;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      pPoint = (POINT *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      if( DPtoLP( (HDC) hb_parnl(1), pPoint, hb_parni(3) ) )
      {
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retnl( TRUE );
      }
      else
      {
         hb_retnl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DPtoLP", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( LPTODP )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      POINT *pPoint;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      pPoint = (POINT *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;

      if( LPtoDP( (HDC) hb_parnl(1), pPoint, hb_parni(3) ) )
      {
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retnl( TRUE );
      }
      else
      {
         hb_retnl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "LPTODP", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( GETMESSAGEPOS )
{
   hb_retnl( (LONG) GetMessagePos() );
}

//-----------------------------------------------------------------------------
HB_FUNC( SHAPPBARMESSAGE )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      APPBARDATA *pData = (APPBARDATA *) hb_xgrab( sizeof( APPBARDATA ) );

      UINT_PTR pRet = SHAppBarMessage( (DWORD) hb_parnl(1), pData );

      PHB_ITEM pByRef;

      if( HB_IS_OBJECT( pStructure ) )
      {
         pByRef = NULL;
      }
      else
      {
         hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
         hb_vmPushNil();
         hb_itemPushStaticString( "APPBARDATA", 10 );
         hb_vmDo(1);

         pByRef = pStructure;
         pStructure = hb_stackReturnItem();
      }

      hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pData, sizeof( APPBARDATA ) );

      hb_vmPushSymbol( pDEVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      if( pByRef )
      {
         hb_itemForwardValue( pByRef, pStructure );
      }

      hb_retnl( (long) pRet );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SHAppBarMessage", 1, hb_paramError(1), hb_paramError(2) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( CREATECOMPATIBLEBITMAP )
{
   hb_retnl( (LONG) CreateCompatibleBitmap( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( ENABLEMENUITEM )
{
   hb_retl( EnableMenuItem( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), (UINT) hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( LOADMENU )
{
   hb_retnl( (long) LoadMenu( (HINSTANCE) hb_parnl(1), (LPCTSTR) hb_parc(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( CHECKDLGBUTTON )
{
   hb_retl( CheckDlgButton( (HWND) hb_parnl(1), hb_parni( 2 ), (UINT) hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( ISDLGBUTTONCHECKED )
{
   hb_retnl( IsDlgButtonChecked( (HWND) hb_parnl(1), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SHILCREATEFROMPATH )
{
   LONG lRet, bRet = FALSE;
   ITEMIDLIST *ppidl = (ITEMIDLIST *) hb_xgrab( sizeof( ITEMIDLIST ) );
   DWORD rgflnOut;

   rgflnOut = (DWORD) hb_parnl(3);

   if( pSHILCreateFromPath )
   {
      lRet = (LONG) pSHILCreateFromPath( (LPCWSTR) hb_parc(1), ppidl, &rgflnOut );
   }
   else
   {
      lRet = E_NOTIMPL;
   }

   if( SUCCEEDED( lRet ) )
   {
      hb_stornl( (LONG) ppidl, 2 );
      hb_stornl( (LONG) rgflnOut, 3 );
      bRet = TRUE;
   }
   else
   {
      hb_stornl( 0, 2 );
   }

   hb_retl( bRet );
}

//-----------------------------------------------------------------------------
HB_FUNC( SHGETFOLDERPATH )
{
   PHB_ITEM pPath = hb_pureparam( 5, HB_IT_BYREF );
   char pszPath[ MAX_PATH + 1 ];
   LONG lRet;

   if( pPath == NULL )
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SHGetFolderPath", 5, hb_paramError(1), hb_paramError(2),  hb_paramError(3),  hb_paramError(4), hb_paramError(5) );
      return;
   }

   if( pSHGetFolderPath )
   {
      lRet = (LONG) pSHGetFolderPath( (HWND) hb_parnl(1), hb_parni(2), (HANDLE) hb_parnl(3), (DWORD) hb_parnl(4), pszPath );
   }
   else
   {
      lRet = E_NOTIMPL;
   }

   if( SUCCEEDED( lRet ) )
   {
      hb_storc( (char *) pszPath, 5 );
   }
   else
   {
      hb_storc( NULL, 5 );
   }

   hb_retnl( lRet );
}

//-----------------------------------------------------------------------------
HB_FUNC( SHGETFOLDERPATHANDSUBDIR )
{
   PHB_ITEM pSubDir = hb_pureparam( 5, HB_IT_BYREF );
   PHB_ITEM pPath   = hb_pureparam( 6, HB_IT_BYREF );
   char pszSubDir[ MAX_PATH + 1 ];
   char pszPath[ MAX_PATH + 1 ];
   LONG lRet;

   if( pPath == NULL || pSubDir == NULL )
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SHGETFOLDERPATHANDSUBDIR", 5, hb_paramError(1), hb_paramError(2),  hb_paramError(3),  hb_paramError(4), hb_paramError(5) );
      return;
   }

   if( pSHGetFolderPathAndSubDir )
   {
      lRet = (LONG) pSHGetFolderPathAndSubDir( (HWND) hb_parnl(1), hb_parni(2), (HANDLE) hb_parnl(3), (DWORD) hb_parnl(4), pszSubDir, pszPath );
   }
   else
   {
      lRet = E_NOTIMPL;
   }

   if( SUCCEEDED( lRet ) )
   {
      hb_storc( (char *) pszSubDir, 5 );
      hb_storc( (char *) pszPath, 6 );
   }
   else
   {
      hb_storc( NULL, 5 );
      hb_storc( NULL, 6 );
   }

   hb_retnl( lRet );
}

//-----------------------------------------------------------------------------
HB_FUNC( REGISTERWINDOWMESSAGE )
{
   hb_retni( RegisterWindowMessage( (LPCSTR) hb_parc(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( ISICONIC )
{
   hb_retl( IsIconic( (HWND) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SENDMESSAGECALLBACK )
{
   SENDASYNCPROC lpResultCallBack = NULL;
   ULONG_PTR dwData = 0;

   hb_retl( SendMessageCallback( (HWND) hb_parnl(1), (UINT) hb_parni(2), (WPARAM) hb_parnl(3), (LPARAM) hb_parnl(4), lpResultCallBack, dwData ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( CHECKMENUITEM )
{
   hb_retnl( (LONG) CheckMenuItem( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), (UINT) hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETMENUITEMBITMAPS )
{
    hb_retl( SetMenuItemBitmaps( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), (UINT) hb_parnl(3), (HBITMAP) hb_parnl(4), (HBITMAP) hb_parnl(5) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( ENUMCHILDWINDOWS )
{
    hb_retl( EnumChildWindows( (HWND) hb_parnl(1), (WNDENUMPROC) hb_parnl(2), (LPARAM) hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETWINDOW )
{
    hb_retnl( (LONG) GetWindow( (HWND) hb_parnl(1), (UINT) hb_parnl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETDIALOGBASEUNITS )
{
    hb_retnl( GetDialogBaseUnits() );
}

//-----------------------------------------------------------------------------
HB_FUNC( OLEINITIALIZE )
{
    hb_retnl( OleInitialize( NULL ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( OLEUNINITIALIZE )
{
    OleUninitialize();
}

//-----------------------------------------------------------------------------
HB_FUNC( COINITIALIZE )
{
   hb_retnl( CoInitialize( NULL ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( KEYBD_EVENT )
{
   keybd_event( (BYTE) hb_parni(1), 0, (DWORD) hb_parnl(2), hb_parnl(3) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETDLGITEMINT )
{
   BOOL lTranslated = FALSE;
   BOOL lSigned     = ( ISNIL(4) ? TRUE : hb_parl(4) );
   hb_retni( GetDlgItemInt( (HWND) hb_parnl(1), hb_parni(2), &lTranslated, lSigned ) );
   if( ISBYREF(3) )
   {
      hb_storl( lTranslated, 3 );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( SETCURSORPOS )
{
   hb_retl( SetCursorPos( hb_parni(1), hb_parni(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETTICKCOUNT )
{
   hb_retnl( (LONG) GetTickCount() );
}

//-----------------------------------------------------------------------------
HB_FUNC( MAPDIALOGRECT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );
   if( pStructure )
   {
      RECT *pRect = (RECT *) hb_xgrab( sizeof( RECT ) );
      if( MapDialogRect( (HWND) hb_parnl(1), pRect ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "RECT", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pRect, sizeof( RECT ) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pRect );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "MAPDIALOGRECT", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( SLEEP )
{
   Sleep( (DWORD) hb_parnl(1) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETMENUINFO )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( SetMenuInfo( (HMENU) hb_parnl(1), (CONST LPCMENUINFO) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETMENUINFO", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( GETMENUINFO )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );
   if( pStructure )
   {
      MENUINFO* pMi = (MENUINFO*) hb_xgrab( sizeof( MENUINFO ) );

      if( GetMenuInfo( (HMENU) hb_parnl(1), pMi ) )
      {
         PHB_ITEM pByRef;
         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "MENUINFO", 8 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pMi, sizeof( MENUINFO ) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pMi );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETMENUINFO", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( FINDWINDOW )
{
   hb_retnl( (ULONG) FindWindow( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( ENUMDISPLAYMONITORS )
{
   hb_retl( EnumDisplayMonitors( (HDC) hb_parnl(1), (LPCRECT) hb_parc(2), (MONITORENUMPROC) hb_parnl(3), (LPARAM) hb_parnl(4) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( MONITORFROMWINDOW )
{
   hb_retnl( (long) MonitorFromWindow( (HWND) hb_parnl(1), (DWORD) hb_parnl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( MONITORFROMPOINT )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( (long) MonitorFromPoint( *(POINT*) hb_parc(-1), (DWORD) hb_parnl(2) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "MONITORFROMPOINT", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( MONITORFROMRECT )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      RECT *pRect;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);
      pRect =  (RECT *) hb_parc(-1);

      hb_retnl( (long) MonitorFromRect( (CONST RECT*) pRect, (DWORD) hb_parnl(2) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "MONITORFROMRECT", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( GETMONITORINFO )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );
   if( pStructure )
   {
      MONITORINFOEX* pMi = (MONITORINFOEX*) hb_xgrab( sizeof( MONITORINFOEX ) );
      pMi->cbSize = sizeof( MONITORINFOEX );

      if( GetMonitorInfo( (HMONITOR) hb_parnl(1), (LPMONITORINFO) pMi ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "MONITORINFOEX", 13 );
            hb_vmDo(1);
            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char*) pMi, sizeof( MONITORINFOEX ) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pMi );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ENUMDISPLAYDEVICES", 1, hb_paramError(1) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( CREATEDIBITMAP )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );
   PHB_ITEM pStructure2 = hb_pureparam( 5, HB_IT_ANY );
   BITMAPINFOHEADER *bmih;
   BITMAPINFO *bmi;

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);
      bmih = (BITMAPINFOHEADER *) hb_parc(-1);
   }

   if( pStructure2 && HB_IS_OBJECT( pStructure2 ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure2 );
      hb_vmSend(0);
      bmi = (BITMAPINFO *) hb_parc(-1);
   }

   hb_retnl( (long) CreateDIBitmap( (HDC) hb_parnl(1), (CONST BITMAPINFOHEADER *) bmih, (DWORD) hb_parnl(3), (VOID *) hb_parc(4), (CONST BITMAPINFO *) bmi, (UINT) hb_parni(6) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETWINDOWLONGPTR )
{
   hb_retnl( (LONG) GetWindowLongPtr( (HWND) hb_parnl(1), hb_parni(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SETWINDOWLONGPTR )
{
   hb_retnl( (LONG) SetWindowLongPtr( (HWND) hb_parnl(1), hb_parni(2), (LONG_PTR) hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( INITIALIZEFLATSB )
{
   hb_retl( InitializeFlatSB( (HWND) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( FLATSB_ENABLESCROLLBAR )
{
   hb_retl( FlatSB_EnableScrollBar( (HWND) hb_parnl(1), (int) hb_parni(2), (UINT) hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( FLATSB_SHOWSCROLLBAR )
{
   hb_retl( FlatSB_ShowScrollBar( (HWND) hb_parnl(1), (int) hb_parni(2), hb_parl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SWITCHTOTHISWINDOW )
{
   if( pSwitchToThisWindow == NULL )
   {
      hb_errRT_BASE( EG_ARG, 6999, NULL, "SwitchToThisWindow", 1, hb_paramError(1) );
      return;
   }
   pSwitchToThisWindow( (HWND) hb_parnl(1), hb_parl(2) );
}

//-----------------------------------------------------------------------------
HB_FUNC( OPENFILE )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );
   if( pStructure )
   {
      LPOFSTRUCT pOf = (LPOFSTRUCT) hb_xgrab( sizeof(OFSTRUCT) );
      if( OpenFile( (LPCSTR) hb_parc(1), pOf, (UINT) hb_parnl(1) ) )
      {
         PHB_ITEM pByRef;
         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "OFSTRUCT", 8 );
            hb_vmDo(1);
            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char*) pOf, sizeof( OFSTRUCT ) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);
         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pOf );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "OPENFILE", 1, hb_paramError(1) );
   }
}


//-----------------------------------------------------------------------------
HB_FUNC( CLIPCURSOR )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure == NULL || HB_IS_NIL( pStructure ) )
   {
      hb_retl( ClipCursor( NULL ) );
   }
   else if(  HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( ClipCursor( (CONST RECT*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "CLIPCURSOR", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( ENUMPROPS )
{
   hb_retni( EnumProps( (HWND) hb_parnl(1), (PROPENUMPROC) hb_parnl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( REMOVEMENU )
{
   hb_retl( RemoveMenu( (HMENU) hb_parnl(1), (UINT) hb_parnl(2), (UINT) hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETMENUSTATE )
{
   hb_retnl( GetMenuState( (HMENU) hb_parnl(1), (UINT) hb_parni(2), (UINT) hb_parni(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( NETGROUPGETUSERS )
{
   PHB_ITEM pArray = hb_param( 4, HB_IT_ARRAY );

   if( pArray && pNetGroupGetUsers && pNetApiBufferFree )
   {
      PBYTE bufptr = NULL;
      LPCWSTR servername = hb_oleAnsiToWide( (LPSTR) hb_parc(1) );
      LPCWSTR groupname = hb_oleAnsiToWide( (LPSTR) hb_parc(2) );
      DWORD entriesread = 0, totalentries = 0;
      NET_API_STATUS Result;

      Result = pNetGroupGetUsers( servername, groupname, (DWORD) 0, &bufptr, MAX_PREFERRED_LENGTH, &entriesread, &totalentries, (PDWORD_PTR) 0 );

      if( servername )
      {
         hb_xfree( (void *) servername );
      }

      if( groupname )
      {
         hb_xfree( (void *) groupname );
      }

      if( Result == NERR_Success )
      {
         char *sName;
         DWORD i;
         HB_ITEM_NEW( User );

         hb_arraySize( pArray, entriesread );

         for( i = 0; i < entriesread; i++ )
         {
            sName = hb_oleWideToAnsi( ((LPWSTR *) bufptr)[i] );

            hb_itemPutCPtr( &User, sName, strlen( sName ) );

            hb_arraySetForward( pArray, i + 1, &User );
         }

         hb_stornl( (long) entriesread, 6 );
         hb_stornl( (long) totalentries, 7 );
      }

      if( bufptr )
      {
         pNetApiBufferFree( (LPVOID) bufptr );
      }

      hb_retnl( (long) Result );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "NETGROUPGETUSERS", 7, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4), hb_paramError(5), hb_paramError(6), hb_paramError(7) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( NETGROUPENUM )
{
   PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

   if( pArray && pNetGroupEnum && pNetApiBufferFree )
   {
      PBYTE bufptr = NULL;
      LPCWSTR servername = hb_oleAnsiToWide( (LPSTR) hb_parc(1) );
      DWORD entriesread = 0, totalentries = 0;
      NET_API_STATUS Result;

      Result = pNetGroupEnum( servername, (DWORD) 0, &bufptr, MAX_PREFERRED_LENGTH, &entriesread, &totalentries, (PDWORD_PTR) 0 );

      if( servername )
      {
         hb_xfree( (void *) servername );
      }

      if( Result == NERR_Success )
      {
         char *sName;
         DWORD i;
         HB_ITEM_NEW( User );

         hb_arraySize( pArray, entriesread );

         for( i = 0; i < entriesread; i++ )
         {
            sName = hb_oleWideToAnsi( ((LPWSTR *) ( bufptr ) )[i] );

            hb_itemPutCPtr( &User, sName, strlen( sName ) );

            hb_arraySetForward( pArray, i + 1, &User );
         }

         hb_stornl( (long) entriesread, 5 );
         hb_stornl( (long) totalentries, 6 );
      }

      if( bufptr )
      {
         pNetApiBufferFree( (LPVOID) bufptr );
      }

      hb_retnl( (long) Result );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "NETGROUPENUM", 6, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4), hb_paramError(5), hb_paramError(6) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( NETUSERGETLOCALGROUPS )
{
   PHB_ITEM pArray = hb_param( 5, HB_IT_ARRAY );

   if( pArray && pNetUserGetLocalGroups && pNetApiBufferFree )
   {
      PBYTE bufptr = NULL;
      LPCWSTR servername = hb_oleAnsiToWide( (LPSTR) hb_parc(1) );
      LPCWSTR username = hb_oleAnsiToWide( (LPSTR) hb_parc(2) );
      DWORD entriesread = 0, totalentries = 0;
      NET_API_STATUS Result;

      Result = pNetUserGetLocalGroups( servername, username, (DWORD) 0 , hb_parnl(4), &bufptr, MAX_PREFERRED_LENGTH, &entriesread, &totalentries );

      if( servername )
      {
         hb_xfree( (void *) servername );
      }

      if( username )
      {
         hb_xfree( (void *) username );
      }

      if( Result == NERR_Success )
      {
         char *sGroup;
         DWORD i;
         HB_ITEM_NEW( Group );

         hb_arraySize( pArray, entriesread );

         for( i = 0; i < entriesread; i++ )
         {
            sGroup = hb_oleWideToAnsi( ((LPWSTR *) bufptr)[i] );

            hb_itemPutCPtr( &Group, sGroup, strlen( sGroup ) );

            hb_arraySetForward( pArray, i + 1, &Group );
         }

         hb_stornl( (long) entriesread, 7 );
         hb_stornl( (long) totalentries, 8 );
      }

      if( bufptr )
      {
         pNetApiBufferFree( (LPVOID) bufptr );
      }

      hb_retnl( (long) Result );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "NETUSERGETLOCALGROUPS", 8, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4), hb_paramError(5), hb_paramError(6), hb_paramError(7), hb_paramError(8) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( BEGINUPDATERESOURCE )
{
   hb_retnl( (long) BeginUpdateResource( (LPCTSTR) hb_parc(1), (BOOL) hb_parl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( UPDATERESOURCE )
{
   hb_retl( (BOOL) UpdateResource( (HANDLE) hb_parnl(1), (LPCTSTR) hb_parc(2), (LPCTSTR) hb_parc(3), (WORD) hb_parnl(4), (LPVOID) hb_parc(5), (DWORD) hb_parnl(6) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( ENDUPDATERESOURCE )
{
   hb_retnl( (long) EndUpdateResource( (HANDLE) hb_parnl(1), (BOOL) hb_parl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SIZEOFRESOURCE )
{
   hb_retnl( (long) SizeofResource( (HMODULE) hb_parnl(1), (HRSRC) hb_parnl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( REMOVEFONTRESOURCE )
{
   hb_retni( RemoveFontResource( hb_parc(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( ADDFONTRESOURCE )
{
   hb_retni( AddFontResource( hb_parc(1) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETTEMPFILENAME )
{
   char *cText = (char*) hb_xgrab( MAX_PATH + 1 );
   hb_retni( GetTempFileName( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2), (UINT) hb_parni(3), (LPTSTR) cText ) );
   if( ISBYREF( 4 ) )
   {
      hb_storclenAdopt( cText, strlen(cText), 4 );
   }
   else
   {
      hb_retclenAdopt( cText, strlen(cText) );
   }
}

//-----------------------------------------------------------------------------
HB_FUNC( MOVEFILE )
{
   hb_retl( MoveFile( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DLLREGISTERSERVER )
{
   HRESULT hRes;
   HMODULE hLib;
   HRESULT (WINAPI *pProc)( void );
   hLib = LoadLibrary( hb_parc(1) );

   if( hLib )
   {
      pProc = (HRESULT (WINAPI *)(void)) GetProcAddress( hLib, "DllRegisterServer" );
      hRes = pProc();
      FreeLibrary(hLib);
   }
   hb_retl( hRes == S_OK );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ANIMATEWINDOW )
{
   hb_retl( AnimateWindow( (HWND) hb_parnl(1), (DWORD) hb_parnl(2), (DWORD) hb_parnl(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETMAPMODE )
{
   hb_retni( SetMapMode( (HDC) hb_parnl(1), hb_parni( 2 ) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( IMAGELISTGETIMAGEINFO )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pStructure )
   {
      IMAGEINFO *pii = (IMAGEINFO*) hb_xgrab( sizeof( IMAGEINFO ) );
      if( ImageList_GetImageInfo( (HIMAGELIST) hb_parnl(1), hb_parni(2), pii ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "IMAGEINFO", 9 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pii, sizeof( IMAGEINFO ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pii );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "IMAGELISTGETIMAGEINFO", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETMAPMODE )
{
   hb_retni( GetMapMode( (HDC) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SETLAYEREDWINDOWATTRIBUTES )
{
   if( pSetLayeredWindowAttributes )
   {
      hb_retl( pSetLayeredWindowAttributes( (HWND) hb_parnl(1), (COLORREF) hb_parnl(2), (BYTE) hb_parni(3), (DWORD) hb_parni(4) ) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( DIALOGBOX )
{
   hb_retni( DialogBox( (HINSTANCE) hb_parnl(1), hb_parc(2), (HWND) hb_parnl(3), (DLGPROC) hb_parnl(4) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( DIALOGBOXINDIRECT )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retni( DialogBoxIndirect( (HINSTANCE) hb_parnl(1), (LPCDLGTEMPLATE) hb_parc(-1), (HWND) hb_parnl(3), (DLGPROC) hb_parnl(4) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DIALOGBOXINDIRECT", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CREATEDIALOG )
{
   hb_retnl( (LONG) CreateDialog( (HINSTANCE) hb_parnl(1), hb_parinfo(2) == HB_IT_STRING ? hb_parc(2): MAKEINTRESOURCE( hb_parnl(2) ), (HWND) hb_parnl(3), (DLGPROC) hb_parnl(4) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CREATEDIALOGINDIRECT )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( (LONG) CreateDialogIndirect( (HINSTANCE) hb_parnl(1), (LPCDLGTEMPLATE) hb_parc(-1), (HWND) hb_parnl(3), (DLGPROC) hb_parnl(4) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "CREATEDIALOGINDIRECT", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGOPENKEYEX )
{
   LONG lRet;
   HKEY hKey ;

   if( ( lRet = RegOpenKeyEx( (HKEY) hb_parnl(1), (LPCTSTR) hb_parc(2), (DWORD) hb_parnl(3), (REGSAM) hb_parnl(4), &hKey ) ) == ERROR_SUCCESS )
   {
      hb_stornl( (LONG) hKey, 5 );
   }

   hb_retnl( lRet );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGCLOSEKEY )
{
   hb_retnl( RegCloseKey( (HKEY) hb_parnl(1) ) );
}

HB_FUNC( REGENUMKEY )
{
   HKEY hKey = (HKEY) hb_parnl(1);
   WCHAR wName[256];
   LONG lRet;

   lRet = RegEnumKeyW( hKey, (DWORD) hb_parnl(2), wName, 256 );

   if( lRet == ERROR_SUCCESS )
   {
      hb_storclenAdopt( hb_oleWideToAnsi( wName ), wcslen( wName ), 3 );
   }

   hb_retnl( lRet );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( PROGIDFROMCLASSID )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      LPOLESTR wProgID = NULL;
      HRESULT lRet;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      lRet = ProgIDFromCLSID( (REFIID) hb_parc(-1), &wProgID );

      if( lRet == S_OK )
      {
         hb_storclenAdopt( hb_oleWideToAnsi( wProgID ), wcslen( wProgID ), 2 );

         CoTaskMemFree( (void *) wProgID );
      }

      hb_retnl( (LONG) lRet );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "PROGIDFROMCLASSID", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CLASSIDFROMPROGID )
{
   BSTR wProgID;
   CLSID clsid;
   HRESULT lRet;

   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      wProgID = hb_oleAnsiToWide( hb_parc(1) );

      if( ( lRet = CLSIDFromProgID( wProgID, &clsid ) ) == S_OK )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "GUID", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCL( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) &clsid, sizeof( GUID ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
      }

      if( wProgID )
      {
         hb_xfree( wProgID );
      }

      hb_retnl( (LONG) lRet );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "CLASSIDFROMPROGID", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CLASSIDFROMSTRING )
{
   HRESULT lRet;
   CLSID clsid;
   BSTR wClassID;

   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      wClassID = hb_oleAnsiToWide( hb_parc(1 ) );

      if( ( lRet = CLSIDFromString( wClassID, &clsid ) ) == S_OK );
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "GUID", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCL( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) &clsid, sizeof( GUID ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
      }

      if( wClassID )
      {
         hb_xfree( wClassID );
      }

      hb_retnl( (LONG) lRet );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "CLASSIDFROMSTRING", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( STRINGFROMCLASSID )
{
   LPOLESTR wClassID;
   HRESULT lRet;
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      lRet = StringFromCLSID( (REFCLSID) hb_parnl(-1), &wClassID );

      if( lRet == S_OK )
      {
         hb_storclenAdopt( hb_oleWideToAnsi( wClassID ), wcslen( wClassID ), 2 );
         CoTaskMemFree( (void *) wClassID );
      }

      hb_retnl( (LONG) lRet );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "STRINGFROMCLASSID", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( MAKEINTRESOURCE )
{
   hb_retclenStatic( MAKEINTRESOURCE( hb_parnl(1) ), 2 );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETSHORTPATHNAME )
{
   if( hb_param( 1, HB_IT_STRING ) && hb_param( 2, HB_IT_BYREF ) )
   {
      char szBuffer[ MAX_PATH + 1 ] = {0} ;

      if( GetShortPathName( hb_parc(1), szBuffer, MAX_PATH ) )
      {
        hb_storc( szBuffer, 2 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETSHORTPATHNAME", 2, hb_paramError(1), hb_paramError(2) );
   }

}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETPROFILESECTION )
{

   DWORD  nSize   = 4096;
   LPTSTR bBuffer = (LPTSTR) hb_xgrab( nSize );
   DWORD  dwLen;
   const char * lpSection  = ISNIL(1) ? NULL : hb_parc(1 );

   while ( TRUE )
   {
      dwLen = GetProfileSection( lpSection, bBuffer, nSize );

      if ( ( ( ( lpSection == NULL ) ) && ( nSize - dwLen == 2 ) ) || ( ( lpSection ) && ( nSize - dwLen == 1 ) ) )
      {
         hb_xfree( bBuffer );
         nSize *= 2;
         bBuffer = (LPTSTR) hb_xgrab( nSize );
      }
      else
      {
         break;
      }
   }

   if( dwLen )
   {
      hb_retclen( (char *) bBuffer, dwLen );
   }
   else
   {
      hb_xfree( bBuffer );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETPRIVATEPROFILESECTION )
{

   DWORD  nSize   = 4096;
   LPTSTR bBuffer = (LPTSTR) hb_xgrab( nSize );
   DWORD  dwLen;
   const char * lpSection  = ISNIL(1) ? NULL : hb_parc(1 );
   const char * lpFileName = hb_parc(2);

   while ( TRUE )
   {
      dwLen = GetPrivateProfileSection( lpSection, bBuffer, nSize, lpFileName );

      if ( ( ( ( lpSection == NULL ) ) && ( nSize - dwLen == 2 ) ) || ( ( lpSection ) && ( nSize - dwLen == 1 ) ) )
      {
         hb_xfree( bBuffer );
         nSize *= 2;
         bBuffer = (LPTSTR) hb_xgrab( nSize );
      }
      else
      {
         break;
      }
   }

   if( dwLen )
   {
      hb_retclen( (char *) bBuffer, dwLen );
   }
   else
   {
      hb_xfree( bBuffer );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SHELL_NOTIFYICON )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( Shell_NotifyIcon( (DWORD) hb_parnl(1), (NOTIFYICONDATA*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SHELL_NOTIFYICON", 1, hb_paramError(1), hb_paramError(2) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( MESSAGEBOX )
{
   hb_retni( MessageBox( ( HWND ) hb_parnl(1), hb_parc(2), hb_parc(3), hb_parni( 4 ) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( OEMTOCHAR )
{
   char *buffer;

   if ( hb_parclen(1) > 0 )
   {
       buffer = (char *) hb_xgrab( hb_parclen(1) );
       OemToCharBuff( hb_parc(1), buffer, hb_parclen(1) );
       hb_retclenAdopt( buffer, hb_parclen(1) );
   }
   else
   {
       hb_retc("");
   }
}
//------------------------------------------------------------------------------------------------

HB_FUNC( WINAPIVER )
{
   hb_retc( __DATE__ "->" __TIME__ );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( HILITEMENUITEM )
{
   hb_retl( HiliteMenuItem( (HWND) hb_parnl(1), (HMENU) hb_parnl(2), (UINT) hb_parnl(3), (UINT) hb_parnl(4) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( ENDMENU )
{
   hb_retl( EndMenu() );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( UNREALIZEOBJECT )
{
   hb_retl( UnrealizeObject( (HGDIOBJ) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SETVIEWPORTEXTEX )
{
   PHB_ITEM pStructure = hb_param( 4, HB_IT_BYREF );

   if( pStructure )
   {
      SIZE *pSize = (SIZE *) hb_xgrab( sizeof( SIZE ) );

      if ( SetViewportExtEx( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), pSize ) )
      {
         PHB_ITEM pByRef;
         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "SIZE", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pSize, sizeof( SIZE ) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pSize );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETVIEWPORTEXTEX", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SETBRUSHORGEX )
{

   PHB_ITEM pStructure = hb_param( 4, HB_IT_BYREF );

   if( pStructure )
   {
      POINT *pPoint = (POINT *) hb_xgrab( sizeof( POINT ) );

      if ( SetBrushOrgEx( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), pPoint) )
      {
         PHB_ITEM pByRef;
         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "POINT", 5 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }
         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pPoint, sizeof( POINT ) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pPoint );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETBRUSHORGEX", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGISTERHOTKEY )
{
   hb_retl( RegisterHotKey( (HWND) hb_parnl(1), hb_parni(2), (UINT) hb_parnl(3), (UINT) hb_parnl(4) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETDCBRUSHCOLOR )
{
   if( pGetDCBrushColor )
   {
      hb_retnl( (LONG) pGetDCBrushColor( (HDC) hb_parnl(1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6999, NULL, "GETDCBRUSHCOLOR", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGQUERYVALUEEX )
{
   LONG lError;
   DWORD lpType=hb_parnl(4);

   DWORD lpcbData = 0;
   lError = RegQueryValueEx( (HKEY) hb_parnl(1), (LPTSTR) hb_parc(2), NULL, &lpType, NULL, &lpcbData );

   if ( lError == ERROR_SUCCESS && lpcbData )
   {
      BYTE *lpData;

      lpData = (BYTE *) hb_xgrab( ( int ) ( lpcbData + 1 ) );
      lError= RegQueryValueEx( (HKEY) hb_parnl(1), (LPTSTR) hb_parc(2), NULL, &lpType, (BYTE*) lpData, &lpcbData );

      if ( lError == ERROR_SUCCESS )
      {
         hb_storclenAdopt( (char *) lpData, strlen( (const char *) lpData ), 5 );
      }
      else
      {
         hb_xfree( (void *) lpData );
      }
   }

   hb_retnl( lError );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGENUMKEYEX )
{
   FILETIME ft;
   long bErr;
   TCHAR Buffer[255];
   DWORD dwBuffSize = 255;
   TCHAR Class[255];
   DWORD dwClass = 255;

   bErr = RegEnumKeyEx( (HKEY) hb_parnl(1), hb_parnl(2), Buffer, &dwBuffSize, NULL, Class, &dwClass, &ft );

   if ( bErr != ERROR_SUCCESS )
   {
      hb_retnl(-1);
   }
   else
   {
      hb_storc( Buffer, 3 );
      hb_stornl( ( long ) dwBuffSize, 4 );
      hb_storc( Class, 6 );
      hb_stornl( ( long ) dwClass, 7 );
      hb_retnl(1);
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC ( REGENUMVALUE )
{
   long bErr;
   TCHAR pwzName[256];
   ULONG cchName = 256;
   DWORD fType;
   BYTE  *lpData = NULL;
   DWORD cbData = 1;

   bErr = RegEnumValue( (HKEY) hb_parnl(1), hb_parnl(2), pwzName, &cchName, NULL, &fType, lpData, &cbData);

   if ( bErr == ERROR_SUCCESS )
   {
      if( cbData )
      {
         lpData = (BYTE *) hb_xgrab( cbData );
      }
      else
      {
         lpData = (BYTE *) "";
      }

      bErr = RegEnumValue( (HKEY) hb_parnl(1), hb_parnl(2), pwzName, &cchName, NULL, &fType, lpData, &cbData );

      if ( bErr == ERROR_SUCCESS )
      {
         hb_storclen( pwzName, cchName, 3 );
         hb_stornl( ( long ) fType, 4 );
         hb_storclenAdopt( (char *) lpData, strlen( (const char *) lpData ), 5 );
      }
   }

   hb_retnl( bErr );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGSETVALUEEX )
{
   if( (DWORD) hb_parnl(4) == REG_SZ )
   {
      hb_retnl( (long) RegSetValueEx( (HKEY) hb_parnl(1), hb_parc(2), 0, hb_parnl(4), ( BYTE * const ) hb_parc(5), ( strlen( hb_parc(5) ) + 1 ) )  );
   }
   else
   {
      PBYTE dwData = (PBYTE) hb_parnl(5);
      hb_retnl( (long) RegSetValueEx( (HKEY) hb_parnl(1), hb_parc(2), 0, REG_DWORD, (PBYTE) &dwData, sizeof(PDWORD) ) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGCREATEKEY )
{
   HKEY hKey;
   LONG nErr;

   nErr = RegCreateKey( (HKEY) hb_parnl(1), hb_parc(2), &hKey );
   if ( nErr == ERROR_SUCCESS )
   {
      hb_stornl( PtrToLong(hKey), 3 );
   }
   hb_retnl( nErr );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGCREATEKEYEX )
{
   HKEY hkResult;
   DWORD dwDisposition;
   LONG nErr;

   PHB_ITEM pStructure = hb_pureparam( 7, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      nErr = RegCreateKeyEx( (HKEY) hb_parnl(1), (LPCSTR) hb_parc(2), (DWORD) 0, (LPSTR) hb_parc(4), (DWORD) hb_parnl(5), (DWORD) hb_parnl(6), (LPSECURITY_ATTRIBUTES) hb_parc(-1), &hkResult, &dwDisposition );

      if ( nErr == ERROR_SUCCESS )
      {
         hb_stornl( (LONG) hkResult, 8 );
         hb_stornl( (LONG) dwDisposition, 9 );
      }
      hb_retnl( nErr );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "REGCREATEKEYEX", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGDELETEKEY )
{
   hb_retnl( RegDeleteKey( (HKEY) hb_parnl(1), ( LPCTSTR ) hb_parc(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC ( REGDELETEVALUE )
{
   hb_retnl( RegDeleteValue( (HKEY) hb_parnl(1), hb_parc(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( ENUMFONTFAMILIES )
{
   PHB_ITEM pValue = hb_param( 3, HB_IT_ANY );
   long lValue;

   if( HB_IS_NUMERIC( pValue ) )
   {
      lValue = hb_itemGetNL( pValue );
      hb_retni( EnumFontFamilies( (HDC) hb_parnl(1), (LPCSTR) hb_parc(2), (FONTENUMPROC) lValue, (LPARAM) hb_parnl(4) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ENUMFONTFAMILIES", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      return;
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( ENUMFONTS )
{
   PHB_ITEM pValue = hb_param( 3, HB_IT_ANY );
   long lValue;

   if( HB_IS_NUMERIC( pValue ) )
   {
      lValue = hb_itemGetNL( pValue );
      hb_retni( EnumFonts( (HDC) hb_parnl(1), (LPCTSTR) hb_parc(2), (FONTENUMPROC) lValue, (LPARAM) hb_parnl(4) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ENUMFONTS", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      return;
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SNDPLAYSOUND )
{
   if( psndPlaySound )
   {
      hb_retl( (BOOL) psndPlaySound( (LPCSTR) hb_parc(1), (UINT) hb_parnl(2) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6999, NULL, "SNDPLAYSOUND", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GRADIENTFILL )
{
   PHB_ITEM pStructure;
   PHB_ITEM pArray1 = hb_pureparam( 2, HB_IT_ARRAY );
   PHB_ITEM pArray2 = hb_pureparam( 4, HB_IT_ARRAY );

   if( pArray1 && pArray2 )
   {
      TRIVERTEX *pVertex;
      GRADIENT_RECT *pRect;
      GRADIENT_TRIANGLE *pTria;

      ULONG dwMode = (ULONG) hb_parnl(6);
      int iCount1 = pArray1->item.asArray.value->ulLen;
      int iCount2 = pArray2->item.asArray.value->ulLen;

      int i;

      pVertex  = (TRIVERTEX *) hb_xgrab( iCount1 * sizeof( TRIVERTEX ) );

      if( dwMode == GRADIENT_FILL_TRIANGLE )
      {
         pTria = (GRADIENT_TRIANGLE *) hb_xgrab( iCount2 * sizeof( GRADIENT_TRIANGLE ) );
      }
      else
      {
         pRect = (GRADIENT_RECT *) hb_xgrab( iCount2 * sizeof( GRADIENT_RECT ) );
      }

      for ( i = 0; i < iCount1; i++ )
      {
         pStructure = hb_itemArrayGet( pArray1, i+1 );

         hb_vmPushSymbol( pVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         *(pVertex+i) = *( (TRIVERTEX *) hb_parc(-1) );
      }

      for ( i = 0; i < iCount2; i++ )
      {
         pStructure = hb_itemArrayGet( pArray2, i+1 );

         hb_vmPushSymbol( pVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( dwMode == GRADIENT_FILL_TRIANGLE )
         {
            *(pTria+i) = *( (GRADIENT_TRIANGLE *) hb_parc(-1) );
         }
         else
         {
            *(pRect+i) = *( (GRADIENT_RECT *) hb_parc(-1) );
         }
      }

      hb_retl( GradientFill( (HDC) hb_parnl(1), pVertex, (ULONG) iCount1, dwMode == GRADIENT_FILL_TRIANGLE ? (PVOID) pTria : (PVOID) pRect, (ULONG) iCount2, dwMode ) );

      hb_xfree( (void *) pVertex );

      if( dwMode == GRADIENT_FILL_TRIANGLE )
      {
          hb_xfree( (void *) pTria );
      }
      else
      {
          hb_xfree( (void *) pRect );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GRADIENTFILL", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( FILLGRADIENT )
{
   HDC hdc             = (HDC) hb_parnl(1);
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );
   COLORREF from       = (COLORREF) hb_parnl(3);
   COLORREF to         = (COLORREF) hb_parnl(4);
   BOOL leftright      = hb_parl(5);
   RECT *rect;

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      int fromrgb[3],drgb[3],rgb[3];
      int size,start;
      int left,top,width,height;
      HBRUSH hbrush;
      int z1;
      int z2;
      int i;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      rect = (RECT *) hb_parc(-1);

      // get R,G,B values
      fromrgb[0] = GetRValue( from );
      fromrgb[1] = GetGValue( from );
      fromrgb[2] = GetBValue( from );

      drgb[0] = GetRValue( to ) - fromrgb[0];
      drgb[1] = GetGValue( to ) - fromrgb[1];
      drgb[2] = GetBValue( to ) - fromrgb[2];

      // draw from top to bottom or left to right
      if( leftright )
      {
         start  = rect->left;
         size   = rect->right - rect->left;
         top    = rect->top;
         height = rect->bottom - rect->top;
       }
       else
       {
         start  = rect->top;
         size   = rect->bottom - rect->top;
         left   = rect->left;
         width  = rect->right - rect->left;
      }

      for( i = 0; i < 256; i++ )
      {
          int j;

          for( j = 0; j < 3; j++ )
          {
              rgb[j]=fromrgb[j] + MulDiv(i,drgb[j],255);
          }

          hbrush = (HBRUSH) SelectObject( hdc, CreateSolidBrush( RGB( rgb[0], rgb[1], rgb[2] ) ) );
          z1 = MulDiv( i,   size, 256 );
          z2 = MulDiv( i+1, size, 256 );

          if( leftright )
          {
             left  = start+z1; // from left
             width = z2-z1;
          }
          else
          {
             top    = start+z1; // from top
             height = z2-z1;
          }

          PatBlt( hdc, left, top, width, height, PATCOPY );
          DeleteObject( SelectObject( hdc, hbrush ) );
      }
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SAVEDC )
{
   hb_retni( SaveDC( (HDC) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( RESTOREDC )
{
   hb_retl( RestoreDC( (HDC) hb_parnl(1), hb_parni(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC ( OPENPROCESSTOKEN )
{
   PHANDLE pTokenHandle = NULL;
   BOOL bRet;

   bRet = OpenProcessToken( (HANDLE) hb_parnl(1), (DWORD) hb_parnl(2), pTokenHandle );
//   if( bRet )
//   {
      hb_stornl( (LONG) pTokenHandle, 3 );
//   }
   hb_retl( bRet );
}

//------------------------------------------------------------------------------------------------
HB_FUNC ( LOOKUPPRIVILEGEVALUE )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );
   if( pStructure )
   {
      PLUID pLuid = (LUID*) hb_xgrab( sizeof(LUID) );

      if( LookupPrivilegeValue( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2), pLuid ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "LUID", 4 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pLuid, sizeof(LUID) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pLuid );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "LOOKUPPRIVILEGEVALUE", 1, hb_paramError(1) );
   }
}


//------------------------------------------------------------------------------------------------
HB_FUNC( ADJUSTTOKENPRIVILEGES )
{
   PHB_ITEM pStructure  = hb_pureparam( 3, HB_IT_ANY );
   PHB_ITEM pStructure2 = hb_pureparam( 5, HB_IT_ANY );
   PTOKEN_PRIVILEGES pNewState;
   PTOKEN_PRIVILEGES pPreviousState;
   DWORD ReturnLength = 0;

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);
      pNewState = (PTOKEN_PRIVILEGES) hb_parc(-1);


      if( pStructure2 && HB_IS_OBJECT( pStructure2 ) )
      {
         pPreviousState = (TOKEN_PRIVILEGES*) hb_xgrab( sizeof(TOKEN_PRIVILEGES) );
      }
      else
      {
         pPreviousState = NULL;
      }

      if( AdjustTokenPrivileges( (HANDLE) hb_parnl(1), (BOOL) hb_parl(2), (TOKEN_PRIVILEGES*) pNewState, (DWORD) hb_parnl(4), (TOKEN_PRIVILEGES*) pPreviousState, &ReturnLength ) )
      {
         if( pPreviousState )
         {
            PHB_ITEM pByRef;

            if( HB_IS_OBJECT( pStructure2 ) )
            {
               pByRef = NULL;
            }
            else
            {
               hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
               hb_vmPushNil();
               hb_itemPushStaticString( "TOKEN_PRIVILEGES", 16 );
               hb_vmDo(1);

               pByRef = pStructure2;
               pStructure2 = hb_stackReturnItem();
            }

            hb_itemPutCRaw( pStructure2->item.asArray.value->pItems + pStructure2->item.asArray.value->ulLen - 1, (char *) pPreviousState, sizeof(TOKEN_PRIVILEGES) );
            hb_vmPushSymbol( pDEVALUE->pSymbol );
            hb_vmPush( pStructure2 );
            hb_vmSend(0);

            if( pByRef )
            {
               hb_itemForwardValue( pByRef, pStructure2 );
            }
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
      if( pPreviousState )
      {
         hb_xfree( (void *) pPreviousState );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ADJUSTTOKENPRIVILEGES", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( EXITWINDOWSEX )
{
   HANDLE hToken;
   TOKEN_PRIVILEGES tkp;
   OpenProcessToken( GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken );
   LookupPrivilegeValue( NULL, SE_SHUTDOWN_NAME, &tkp.Privileges[0].Luid );

   tkp.PrivilegeCount = 1;
   tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

   AdjustTokenPrivileges( hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES) NULL, 0 );

   hb_retl( ExitWindowsEx( (UINT) hb_parni( 1 ), (DWORD) hb_parnl(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC ( GETCURRENTPROCESS )
{
   hb_retnl( (LONG) GetCurrentProcess() );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( INITIATESYSTEMSHUTDOWN )
{
   HANDLE hToken;
   TOKEN_PRIVILEGES tkp;
   OpenProcessToken( GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken );
   LookupPrivilegeValue( NULL, SE_SHUTDOWN_NAME, &tkp.Privileges[0].Luid );

   tkp.PrivilegeCount = 1;
   tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

   AdjustTokenPrivileges( hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES) NULL, 0 );

   hb_retl( InitiateSystemShutdown( (LPTSTR) hb_parc(1), (LPTSTR) hb_parc(2), (DWORD) hb_parnl(3), (BOOL) hb_parl(4), (BOOL) hb_parl(5) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETSYSTEMDIRECTORY )
{
   char szBuffer[ MAX_PATH+1 ] = {0} ;
   GetSystemDirectory( szBuffer, MAX_PATH );
   hb_retc( szBuffer );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( EXTRACTICON )
{
   hb_retnl( (long) ExtractIcon( (HINSTANCE) hb_parnl(1), (LPCTSTR) hb_parc(2), (UINT) hb_parni(3) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( EXTRACTASSOCIATEDICON )
{
   WORD lpIcon = (WORD) hb_parnl(3);
   hb_retnl( (long) ExtractAssociatedIcon( (HINSTANCE) hb_parnl(1), (LPTSTR) hb_parc(2), &lpIcon ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( OPENSCMANAGER )
{
   hb_retnl( (long) OpenSCManager( (LPCTSTR) hb_parc(1), (LPCTSTR) hb_parc(2), (DWORD) hb_parnl(3) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( OPENSERVICE )
{
   hb_retnl( (long) OpenService( (SC_HANDLE) hb_parnl(1), (LPCTSTR) hb_parc(2), (DWORD) hb_parnl(3) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CLOSESERVICEHANDLE )
{
   hb_retl( CloseServiceHandle( (SC_HANDLE) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
DWORD StopService( SC_HANDLE hSCM, SC_HANDLE hService, BOOL fStopDependencies, DWORD dwTimeout )
{
   SERVICE_STATUS_PROCESS ssp;
   DWORD dwStartTime = GetTickCount();
   DWORD dwBytesNeeded;
   SERVICE_STATUS ss;

   // Make sure the service is not already stopped
   if ( !pQueryServiceStatusEx( hService, SC_STATUS_PROCESS_INFO, (LPBYTE)&ssp, sizeof(SERVICE_STATUS_PROCESS), &dwBytesNeeded ) )
      return GetLastError();

   if ( ssp.dwCurrentState == SERVICE_STOPPED )
      return ERROR_SUCCESS;

   // If a stop is pending, just wait for it
   while ( ssp.dwCurrentState == SERVICE_STOP_PENDING )
   {
      Sleep( ssp.dwWaitHint );
      if ( !pQueryServiceStatusEx( hService, SC_STATUS_PROCESS_INFO, (LPBYTE)&ssp, sizeof(SERVICE_STATUS_PROCESS), &dwBytesNeeded ) )
         return GetLastError();

      if ( ssp.dwCurrentState == SERVICE_STOPPED )
         return ERROR_SUCCESS;

      if ( GetTickCount() - dwStartTime > dwTimeout )
         return ERROR_TIMEOUT;
   }

   // If the service is running, dependencies must be stopped first
   if ( fStopDependencies )
   {
      DWORD i;
      DWORD dwBytesNeeded;
      DWORD dwCount;

      LPENUM_SERVICE_STATUS   lpDependencies = NULL;
      ENUM_SERVICE_STATUS     ess;
      SC_HANDLE               hDepService;

      // Pass a zero-length buffer to get the required buffer size
      if ( EnumDependentServices( hService, SERVICE_ACTIVE, lpDependencies, 0, &dwBytesNeeded, &dwCount ) )
      {
         // If the Enum call succeeds, then there are no dependent
         // services so do nothing
      }
      else
      {
         if ( GetLastError() != ERROR_MORE_DATA )
            return GetLastError(); // Unexpected error

         // Allocate a buffer for the dependencies
         lpDependencies = (LPENUM_SERVICE_STATUS) HeapAlloc( GetProcessHeap(), HEAP_ZERO_MEMORY, dwBytesNeeded );

         if ( !lpDependencies )
            return GetLastError();

         __try {
            // Enumerate the dependencies
            if ( !EnumDependentServices( hService, SERVICE_ACTIVE, lpDependencies, dwBytesNeeded, &dwBytesNeeded, &dwCount ) )
               return GetLastError();

            for ( i = 0; i < dwCount; i++ )
            {
               ess = *(lpDependencies + i);

               // Open the service
               hDepService = OpenService( hSCM, ess.lpServiceName, SERVICE_STOP | SERVICE_QUERY_STATUS );
               if ( !hDepService )
                  return GetLastError();

               __try {
                   // Send a stop code
                  if ( !ControlService( hDepService,
                           SERVICE_CONTROL_STOP,
                           &ss ) )
                     return GetLastError();

                  // Wait for the service to stop
                  while ( ss.dwCurrentState != SERVICE_STOPPED )
                  {
                     Sleep( ss.dwWaitHint );
                     if ( !pQueryServiceStatusEx( hDepService, SC_STATUS_PROCESS_INFO, (LPBYTE)&ssp,  sizeof(SERVICE_STATUS_PROCESS), &dwBytesNeeded ) )
                        return GetLastError();

                     if ( ss.dwCurrentState == SERVICE_STOPPED )
                        break;

                     if ( GetTickCount() - dwStartTime > dwTimeout )
                        return ERROR_TIMEOUT;
                  }
               }
               __finally
               {
                  // Always release the service handle
                  CloseServiceHandle( hDepService );
               }
            }
         }
         __finally
         {
            // Always free the enumeration buffer
            HeapFree( GetProcessHeap(), 0, lpDependencies );
         }
      }
   }

   // Send a stop code to the main service
   if ( !ControlService( hService, SERVICE_CONTROL_STOP, &ss ) )
      return GetLastError();

   // Wait for the service to stop
   while ( ss.dwCurrentState != SERVICE_STOPPED )
   {
      Sleep( ss.dwWaitHint );
      if ( !pQueryServiceStatusEx( hService, SC_STATUS_PROCESS_INFO, (LPBYTE)&ssp, sizeof(SERVICE_STATUS_PROCESS), &dwBytesNeeded ) )
         return GetLastError();

      if ( ss.dwCurrentState == SERVICE_STOPPED )
         break;

      if ( GetTickCount() - dwStartTime > dwTimeout )
         return ERROR_TIMEOUT;
   }

   // Return success
   return ERROR_SUCCESS;
}

//------------------------------------------------------------------------------------------------
HB_FUNC( STARTSERVICE )
{
   LPCTSTR* lpServiceArgVectors = NULL;
   hb_retnl( (long) StartService( (SC_HANDLE) hb_parnl(1), (DWORD) hb_parnl(2), lpServiceArgVectors ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( STOPSERVICE )
{
   hb_retnl( (long) StopService( (SC_HANDLE) hb_parnl(1), (SC_HANDLE) hb_parnl(2), hb_parnl(3), (DWORD) hb_parnl(4) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CREATESERVICE )
{
   hb_retnl( (long) CreateService( (SC_HANDLE) hb_parnl(1),
                                   (LPCTSTR) hb_parc(2),
                                   (LPCTSTR) hb_parc(3),
                                   (DWORD) hb_parnl(4),
                                   (DWORD) hb_parnl(5),
                                   (DWORD) hb_parnl(6),
                                   (DWORD) hb_parnl(7),
                                   (LPCTSTR) hb_parc(8),
                                   (LPCTSTR) hb_parc(9),
                                   (LPDWORD) hb_parnl(10),
                                   (LPCTSTR) hb_parc(11),
                                   (LPCTSTR) hb_parc(12),
                                   (LPCTSTR) hb_parc(13) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( DELETESERVICE )
{
   hb_retnl( DeleteService( (SC_HANDLE) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REGISTERSERVICECTRLHANDLER )
{
   hb_retnl( (LONG) RegisterServiceCtrlHandler( (LPCTSTR) hb_parc(1), (LPHANDLER_FUNCTION) hb_parnl(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SETSERVICESTATUS )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( SetServiceStatus( (SERVICE_STATUS_HANDLE) hb_parnl(1), (LPSERVICE_STATUS) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETSERVICESTATUS", 1, hb_paramError(1) );
   }
}


//------------------------------------------------------------------------------------------------
HB_FUNC( SETEVENT )
{
   hb_retl( SetEvent( (HANDLE) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CREATEEVENT )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( (long) CreateEvent( (LPSECURITY_ATTRIBUTES) hb_parc(-1), hb_parl(2), hb_parl(3), (LPTSTR) hb_parc(4) ) );
   }
   else
   {
      hb_retnl( (long) CreateEvent( NULL, hb_parl(2), hb_parl(3), (LPTSTR) hb_parc(4) ) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( WSACREATEEVENT )
{
   hb_retnl( (long) WSACreateEvent() );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( WSAWAITFORMULTIPLEEVENTS )
{
   PHB_ITEM pArray = hb_pureparam( 1, HB_IT_ARRAY );
   int iCount = pArray->item.asArray.value->ulLen;
   WSAEVENT* lphEvents = (WSAEVENT*) hb_xgrab( iCount * sizeof( WSAEVENT ) );

   hb_retnl( (long) WSAWaitForMultipleEvents( WSA_MAXIMUM_WAIT_EVENTS, (const WSAEVENT*) lphEvents, hb_parl(2), (DWORD) hb_parnl(3), hb_parl(4) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( WAITFORSINGLEOBJECT )
{
   hb_retnl( (long) WaitForSingleObject( (HANDLE) hb_parnl(1), (DWORD) hb_parnl(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( BEEP )
{
   hb_retl( Beep( (DWORD) hb_parnl(1), (DWORD) hb_parnl(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CLOSEHANDLE )
{
   hb_retl( CloseHandle( (HANDLE) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------

HB_FUNC( RUNSERVICE )
{
   SERVICE_TABLE_ENTRY serviceTable = {0};

   serviceTable.lpServiceName = (LPTSTR) hb_parc(1);
   serviceTable.lpServiceProc = (LPSERVICE_MAIN_FUNCTION) hb_parnl(2);

   hb_retl( StartServiceCtrlDispatcher( &serviceTable ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( ENDTASK )
{
   if( pEndTask )
   {
      hb_retl( pEndTask( (HWND) hb_parnl(1), hb_parl(2), hb_parl(3) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6999, NULL, "ENDTASK", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SENDMESSAGETIMEOUT )
{
   DWORD dwResult;

   if( SUCCEEDED( SendMessageTimeout( (HWND) hb_parnl(1), (UINT) hb_parni(1), (WPARAM) hb_parnl(2), (LPARAM) hb_parnl(3), (UINT) hb_parni(4), (UINT) hb_parni(5), &dwResult ) ) )
   {
      hb_retnl( dwResult );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( ISHUNGAPPWINDOW )
{
   if( pIsHungAppWindow )
   {
      hb_retl( pIsHungAppWindow( (HWND) hb_parnl(1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6999, NULL, "ISHUNGAPPWINDOW", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( DRAWCAPTION )
{
   PHB_ITEM pStructure = hb_pureparam( 3, HB_IT_ANY );

   if(  HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( DrawCaption( (HWND) hb_parnl(1), (HDC) hb_parnl(2), (CONST RECT*) hb_parc(-1), (UINT) hb_parnl(3) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DRAWCAPTION", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( PRINTDLG )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      PRINTDLG *pPd;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      pPd = (PRINTDLG *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;
      pPd->lStructSize = sizeof( PRINTDLG );

      if( PrintDlg( pPd ) )
      {
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "PRINTDLG", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( PAGESETUPDLG )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );
   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      HGLOBAL hDevMode;
      PAGESETUPDLG *pPd;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      pPd = (PAGESETUPDLG *) ( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1 )->item.asString.value;
      pPd->lStructSize = sizeof( PAGESETUPDLG );
      
      if( pPd->Flags & PSD_RETURNDEFAULT )
      {
         pPd->hDevMode = NULL;
         pPd->hDevNames = NULL;
      }
      else
      {
         hDevMode = GlobalAlloc(GHND, sizeof(DEVMODE));
         if ( hDevMode ) {
            DEVMODE *pDevMode = (DEVMODE *) GlobalLock(hDevMode);
            if ( pDevMode ) {
               pDevMode->dmSize = sizeof(DEVMODE);
               pDevMode->dmFields = DM_ORIENTATION | DM_PAPERSIZE | DM_PAPERWIDTH | DM_PAPERLENGTH;
               pDevMode->dmOrientation = hb_parnl(2); //DMORIENT_LANDSCAPE;
               pDevMode->dmPaperSize = hb_parnl(3);   //DMPAPER_LETTER;
               pDevMode->dmPaperWidth = hb_parnl(4);
               pDevMode->dmPaperLength = hb_parnl(5);
            }
            GlobalUnlock(hDevMode);
            pPd->hDevMode = hDevMode;
         }
      }

      if( PageSetupDlg( pPd ) )
      {
         DEVMODE *pDevMode;
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         pDevMode = (DEVMODE*) GlobalLock( pPd->hDevMode );
         hb_stornl( (ULONG) pDevMode->dmOrientation, 2 );
         hb_stornl( (ULONG) pDevMode->dmPaperSize, 3 );
         hb_stornl( (ULONG) pDevMode->dmPaperWidth, 4 );
         hb_stornl( (ULONG) pDevMode->dmPaperLength, 5 );
         GlobalUnlock( pPd->hDevMode );
         
         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "PAGESETUPDLG", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( COMMDLGEXTENDEDERROR )
{
   hb_retnl( (long) CommDlgExtendedError() );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GLOBALLOCK )
{
   hb_retnl( (LONG) GlobalLock( (HGLOBAL) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GLOBALUNLOCK )
{
   hb_retl( GlobalUnlock( (HGLOBAL) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GLOBALFREE )
{
   hb_retnl( (LONG) GlobalFree( (HGLOBAL) hb_parnl(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( ENUMRESOURCETYPES )
{
   hb_retl( EnumResourceTypes( (HMODULE) hb_parnl(1), (ENUMRESTYPEPROC) hb_parnl(2), (LONG_PTR) hb_parnl(3) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( COPYIMAGE )
{
   hb_retnl( (long) CopyImage( (HANDLE) hb_parnl(1), (UINT) hb_parni(2), hb_parni(3), hb_parni(4), (UINT) hb_parni(5) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( MAPISENDDOCUMENTS )
{
   if( pMAPISendDocuments )
   {
      hb_retl( pMAPISendDocuments( (ULONG) hb_parnl(1), (LPTSTR) hb_parc(2), (LPTSTR) hb_parc(3), (LPTSTR) hb_parc(4), (ULONG) hb_parnl(5) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "MAPISENDMAIL", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( MAPISENDMAIL )
{
   PHB_ITEM pStructure = hb_pureparam( 3, HB_IT_ANY );

   if(  pMAPISendMail && HB_IS_OBJECT( pStructure ) )
   {
      lpMapiMessage pMsg;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      pMsg = (lpMapiMessage) hb_parc(-1);

      if( !ISNIL(6) )
      {
         PHB_ITEM pArray1 = hb_pureparam( 6, HB_IT_ARRAY );
         int iCount1 = pArray1->item.asArray.value->ulLen;
         int i;

         lpMapiFileDesc pFiles;
         pFiles = (lpMapiFileDesc) hb_xgrab( iCount1 * sizeof( MapiFileDesc ) );

         for ( i = 0; i < iCount1; i++ )
         {
            hb_vmPushSymbol( pVALUE->pSymbol );
            hb_vmPush( hb_itemArrayGet( pArray1, i + 1 ) );
            hb_vmSend(0);

            pFiles[i] = *( (lpMapiFileDesc) hb_parc(-1) );
         }
         pMsg->lpFiles = pFiles;
      }

      hb_retnl( (long) pMAPISendMail( (LHANDLE) hb_parnl(1), (ULONG) hb_parnl(2), pMsg, (FLAGS) hb_parnl(4), (ULONG) hb_parnl(5) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "MAPISENDMAIL", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( HTMLHELP )
{
   if( pHtmlHelp )
   {
      hb_retnl( (long) pHtmlHelp( (HWND) hb_parnl(1), (LPCSTR) hb_parc(2), (UINT) hb_parnl(3), (DWORD_PTR) hb_parnl(4) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6999, NULL, "HTMLHELP", 1, hb_paramError(1) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETLOCALTIME )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      LPSYSTEMTIME pst = (LPSYSTEMTIME) hb_xgrab( sizeof( SYSTEMTIME ) );
      PHB_ITEM pByRef;

      GetLocalTime( pst );

      if( HB_IS_OBJECT( pStructure ) )
      {
         pByRef = NULL;
      }
      else
      {
         hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
         hb_vmPushNil();
         hb_itemPushStaticString( "SYSTEMTIME", 10 );
         hb_vmDo(1);

         pByRef = pStructure;
         pStructure = hb_stackReturnItem();
      }

      hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pst, sizeof(SYSTEMTIME) );
      hb_vmPushSymbol( pDEVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      if( pByRef )
      {
         hb_itemForwardValue( pByRef, pStructure );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETLOCALTIME", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETSYSTEMTIME )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      LPSYSTEMTIME pst = (LPSYSTEMTIME) hb_xgrab( sizeof( SYSTEMTIME ) );
      PHB_ITEM pByRef;

      GetSystemTime( pst );

      if( HB_IS_OBJECT( pStructure ) )
      {
         pByRef = NULL;
      }
      else
      {
         hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
         hb_vmPushNil();
         hb_itemPushStaticString( "SYSTEMTIME", 10 );
         hb_vmDo(1);

         pByRef = pStructure;
         pStructure = hb_stackReturnItem();
      }

      hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pst, sizeof(SYSTEMTIME) );
      hb_vmPushSymbol( pDEVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      if( pByRef )
      {
         hb_itemForwardValue( pByRef, pStructure );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETSYSTEMTIME", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}


//------------------------------------------------------------------------------------------------
BOOL EnablePrivilege(LPCTSTR PrivilegeName)
{
   LUID Privilege;
   TOKEN_PRIVILEGES TokenPrivileges;
   HANDLE hToken;

   if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken)) return FALSE;
   if (!LookupPrivilegeValue(NULL, PrivilegeName, &Privilege)) goto abort;
   TokenPrivileges.PrivilegeCount = 1;
   TokenPrivileges.Privileges[0].Luid = Privilege;
   TokenPrivileges.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
   if (!AdjustTokenPrivileges(hToken, FALSE, &TokenPrivileges, sizeof(TokenPrivileges), NULL, NULL)) goto abort;

   CloseHandle(hToken);
   return TRUE;

abort:
   CloseHandle(hToken);
   return FALSE;
}

BOOL DisablePrivilege(LPCTSTR PrivilegeName)
{
   LUID Privilege;
   TOKEN_PRIVILEGES TokenPrivileges;
   HANDLE hToken;

   if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken)) return FALSE;
   if (!LookupPrivilegeValue(NULL, PrivilegeName, &Privilege)) goto abort;
   TokenPrivileges.PrivilegeCount = 1;
   TokenPrivileges.Privileges[0].Luid = Privilege;
   TokenPrivileges.Privileges[0].Attributes = 0;
   if (!AdjustTokenPrivileges(hToken, FALSE, &TokenPrivileges, sizeof(TokenPrivileges), NULL, NULL)) goto abort;
   CloseHandle(hToken);
   return TRUE;
abort:
   CloseHandle(hToken);
   return FALSE;
}


//------------------------------------------------------------------------------------------------
HB_FUNC( SETSYSTEMTIME )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      if( EnablePrivilege( SE_SYSTEMTIME_NAME ) )
      {
         hb_vmPushSymbol( pVALUE->pSymbol );
         hb_itemPushForward( pStructure );
         hb_vmSend(0);

         hb_retl( SetSystemTime( (CONST LPSYSTEMTIME) hb_parc(-1) ) );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETSYSTEMTIME", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CREATEFILE )
{
   PHB_ITEM pStructure = hb_pureparam( 4, HB_IT_ANY );
   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retnl( (long) CreateFile( (LPCTSTR) hb_parc(1), (DWORD) hb_parnl(2), (DWORD) hb_parnl(3), (LPSECURITY_ATTRIBUTES) hb_parc(-1), (DWORD) hb_parnl(5), (DWORD) hb_parnl(6), (HANDLE) hb_parnl(7) ) );
   }
   else
   {
      hb_retnl( (long) CreateFile( (LPCTSTR) hb_parc(1), (DWORD) hb_parnl(2), (DWORD) hb_parnl(3), NULL, (DWORD) hb_parnl(5), (DWORD) hb_parnl(6), (HANDLE) hb_parnl(7) ) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SETCOMMSTATE )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );
   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( SetCommState( (HANDLE) hb_parnl(1), (LPDCB) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETCOMMSTATE", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SETCOMMTIMEOUTS )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );
   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( SetCommTimeouts( (HANDLE) hb_parnl(1), (LPCOMMTIMEOUTS) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SETCOMMTIMEOUTS", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETCOMMSTATE )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      LPDCB pDcb = (LPDCB) hb_xgrab( sizeof( DCB ) );
      pDcb->DCBlength = sizeof(DCB);

      if( GetCommState( (HANDLE) hb_parnl(1), pDcb ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "DCB", 3 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pDcb, sizeof(DCB) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pDcb );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETCOMMSTATE", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( PURGECOMM )
{
   hb_retl( PurgeComm( (HANDLE) hb_parnl(1), hb_parnl(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( WAITCOMMEVENT )
{
   DWORD dMask ;
   PHB_ITEM pStructure = hb_pureparam( 3, HB_IT_ANY );
   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( WaitCommEvent( (HANDLE) hb_parnl(1), &dMask, (LPOVERLAPPED) hb_parc(-1) ) );
   }
   else
   {
      hb_retl( WaitCommEvent( (HANDLE) hb_parnl(1), &dMask, NULL ) );
   }
   hb_stornl( (ULONG) dMask, 2 );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( MAPVIRTUALKEY )
{
   hb_retni( (UINT) MapVirtualKey( (UINT) hb_parni(1), (UINT) hb_parni(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETDEFAULTPRINTER )
{
   SetDefaultPrinter( (LPCTSTR) hb_parc(1) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( OLEUIINSERTOBJECT )
{
   BOOL bRet = FALSE;
   OLEUIINSERTOBJECT pOUI;
   char szFile[256];
   szFile[0] = '\0';

   ZeroMemory( &pOUI, sizeof( pOUI ) );

   pOUI.cbStruct  = sizeof(OLEUIINSERTOBJECT);
   pOUI.hWndOwner = (HWND) hb_parnl(1);
   pOUI.dwFlags   = (DWORD) hb_parnl(2);
   pOUI.lpszFile  = szFile;
   pOUI.cchFile   = sizeof(szFile);

   if( pOleUIInsertObject )
   {
      if( pOleUIInsertObject( &pOUI ) == OLEUI_OK )
      {
         bRet = TRUE;
      }
   }
   hb_retl( bRet );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( FINDFIRSTFILE )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      HANDLE hRes;
      LPWIN32_FIND_DATA lpFindFileData = (LPWIN32_FIND_DATA) hb_xgrab( sizeof( WIN32_FIND_DATA ) );

      hRes = FindFirstFile( (LPCTSTR) hb_parc(1), lpFindFileData );

      if( hRes != INVALID_HANDLE_VALUE )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "WIN32_FIND_DATA", 15 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) lpFindFileData, sizeof( WIN32_FIND_DATA ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
      }
      else
      {
         hb_xfree( (void *) lpFindFileData );
      }
      hb_retnl( (long) hRes );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "FINDFIRSTFILE", 4, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETPIXEL )
{
   hb_retnl( (long) GetPixel( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETPIXEL )
{
   hb_retnl( (long) SetPixel( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), (COLORREF) hb_parnl(4) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( UPDATECOLORS )
{
   hb_retl( UpdateColors( (HDC) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SELECTPALETTE )
{
   hb_retnl( (long) SelectPalette( (HDC) hb_parnl(1), (HPALETTE) hb_parnl(2), hb_parl(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEHALFTONEPALETTE )
{
   hb_retnl( (long) CreateHalftonePalette( (HDC) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( REALIZEPALETTE )
{
   hb_retnl( (long) RealizePalette( (HDC) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( RESIZEPALETTE )
{
   hb_retl( ResizePalette( (HPALETTE) hb_parnl(1), (UINT) hb_parni(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETNEARESTCOLOR )
{
   hb_retnl( (long) GetNearestColor( (HDC) hb_parnl(1), (COLORREF) hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETNEARESTPALETTEINDEX )
{
   hb_retnl( (long) GetNearestPaletteIndex( (HPALETTE) hb_parnl(1), (COLORREF) hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ANIMATEPALETTE )
{
   PHB_ITEM pStructure = hb_pureparam( 4, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( AnimatePalette( (HPALETTE) hb_parnl(1), (UINT) hb_parni(2), (UINT) hb_parni(3), (CONST PALETTEENTRY*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ANIMATEPALETTE", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEPALETTE )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( (long) CreatePalette( (CONST LOGPALETTE*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "CREATEPALETTE", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETDCPENCOLOR )
{
   if( pGetDCPenColor )
   {
      hb_retnl( (long) pGetDCPenColor( (HDC) hb_parnl(1) ) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETENVIRONMENTVARIABLE )
{
   hb_retl( SetEnvironmentVariableA( (LPCSTR) hb_parc(1), (LPCSTR) hb_parc(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SHGETFILEINFO )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pStructure )
   {
      SHFILEINFO *psfi = (SHFILEINFO*) hb_xgrab( sizeof( SHFILEINFO ) );
      PHB_ITEM pByRef;

      SHGetFileInfo( hb_parc(1), (DWORD) hb_parnl(2), psfi, (UINT) sizeof(SHFILEINFO), (UINT) hb_parnl(4) );

      if( HB_IS_OBJECT( pStructure ) )
      {
         pByRef = NULL;
      }
      else
      {
         hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
         hb_vmPushNil();
         hb_itemPushStaticString( "SHFILEINFO", 10 );
         hb_vmDo(1);

         pByRef = pStructure;
         pStructure = hb_stackReturnItem();
      }

      hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) psfi, sizeof(SHFILEINFO) );
      hb_vmPushSymbol( pDEVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      if( pByRef )
      {
         hb_itemForwardValue( pByRef, pStructure );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SHGETFILEINFO", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
#define SHCNRF_InterruptLevel     0x0001
#define SHCNRF_ShellLevel         0x0002
#define SHCNRF_RecursiveInterrupt 0x1000
#define SHCNRF_NewDelivery        0x8000

//SHCreateShellFolderViewEX

BOOL GetIdListFromPath ( const char *cPath, LPITEMIDLIST *lpItemIdList )
{
   WCHAR wszPath[ MAX_PATH + 1 ];
   LPSHELLFOLDER pShellFolder = NULL;
   HRESULT       hr;
   ULONG         chUsed;

   MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cPath, -1, wszPath, MAX_PATH );

   if( SHGetDesktopFolder( &pShellFolder ) != NOERROR )
   {
      return FALSE;
   }

   hr = pShellFolder->lpVtbl->ParseDisplayName ( pShellFolder, NULL, NULL, wszPath, &chUsed, lpItemIdList, NULL );

   if( FAILED( hr ) )
   {
      pShellFolder->lpVtbl->Release( pShellFolder );
      *lpItemIdList = NULL;
      return FALSE;
   }

   pShellFolder->lpVtbl->Release( pShellFolder );

   return TRUE;
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( REGISTERNOTIFYFOLDERID )
{
   const LONG fEvents = SHCNE_CREATE | SHCNE_DELETE |
                        SHCNE_MKDIR | SHCNE_RMDIR | SHCNE_RENAMEFOLDER |
                        SHCNE_RENAMEITEM | SHCNE_UPDATEITEM | SHCNE_ALLEVENTS |
                        SHCNE_ATTRIBUTES | SHCNE_DISKEVENTS;
   SHChangeNotifyEntry sEntry;

   sEntry.fRecursive = TRUE;
   sEntry.pidl = (LPITEMIDLIST) hb_parnl(2);

   if( pSHChangeNotifyRegister )
   {
      hb_retnl( (long) pSHChangeNotifyRegister( (HWND) hb_parnl(1), SHCNRF_ShellLevel | SHCNRF_InterruptLevel, fEvents, (UINT) hb_parnl(3), 1, &sEntry) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( REGISTERNOTIFY )
{
   LPITEMIDLIST pidl = { 0 };
   SHChangeNotifyEntry sEntry;

   if( GetIdListFromPath( hb_parc(2), &pidl ) )
   {
      //LPMALLOC pMalloc;
      sEntry.fRecursive = TRUE;
      sEntry.pidl = pidl;

      //SHGetMalloc(&pMalloc);

      if( pSHChangeNotifyRegister )
      {
         hb_retnl( (long) pSHChangeNotifyRegister( (HWND) hb_parnl(1), SHCNRF_ShellLevel | SHCNRF_InterruptLevel | SHCNRF_NewDelivery, SHCNE_ALLEVENTS, (UINT) hb_parnl(3), 1, &sEntry) );
      }

      //pMalloc->lpVtbl->Release( pMalloc );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( UNREGISTERNOTIFY )
{
   if( pSHChangeNotifyDeregister )
      pSHChangeNotifyDeregister( (ULONG) hb_parnl(1) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DRAGACCEPTFILES )
{
   DragAcceptFiles( (HWND) hb_parnl(1), hb_parl(2) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DRAGFINISH )
{
   DragFinish( (HDROP) hb_parnl(1) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DRAGQUERYPOINT )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      BOOL bResult;
      POINT *pPoint = (POINT *) hb_xgrab( sizeof( POINT ) );
      PHB_ITEM pByRef;

      bResult = DragQueryPoint( (HDROP) hb_parnl(1), pPoint );

      if( HB_IS_OBJECT( pStructure ) )
      {
         pByRef = NULL;
      }
      else
      {
         hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
         hb_vmPushNil();
         hb_itemPushStaticString( "POINT", 5 );
         hb_vmDo(1);

         pByRef = pStructure;
         pStructure = hb_stackReturnItem();
      }

      hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pPoint, sizeof(POINT) );
      hb_vmPushSymbol( pDEVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      if( pByRef )
      {
         hb_itemForwardValue( pByRef, pStructure );
      }

      hb_retl( bResult );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DRAGQUERYPOINT", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DRAGQUERYFILE )
{
   PHB_ITEM pFileName = hb_pureparam( 3, HB_IT_ANY );
   PHB_ITEM pLen = hb_param( 4, HB_IT_NUMERIC );

   char *lpszFile = NULL;
   UINT cch = 0;

   if( pFileName == NULL || HB_IS_NIL( pFileName ) )
   {
      lpszFile = NULL;
   }
   else if( HB_IS_BYREF( pFileName ) )
   {
      pFileName = hb_itemUnRef( pFileName );

      if( pLen )
      {
         cch = (UINT) hb_itemGetNL( pLen );
      }
      else if( HB_IS_STRING( pFileName ) )
      {
         cch = (UINT) pFileName->item.asString.length;
      }
      else
      {
         cch = 0;
      }

      if( cch )
      {
         lpszFile = (char *) hb_xgrab( cch + 1 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "DragQueryFile", 4, hb_paramError(1), hb_paramError(2),  hb_paramError(3),  hb_paramError(4) );
      return;
   }

   cch = DragQueryFile( (HDROP) hb_parnl(1), (UINT) hb_parnl(2), (LPTSTR) lpszFile, cch );

   if( lpszFile && cch )
   {
      hb_storclenAdopt( lpszFile, cch, 3 );
   }

   hb_retnl( (LONG) cch );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CAPCREATECAPTUREWINDOW )
{
   if( pcapCreateCaptureWindow )
   {
      hb_retnl( (long) pcapCreateCaptureWindow( (LPCTSTR) hb_parc(1), (DWORD) hb_parnl(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), (HWND) hb_parnl(7), hb_parni(8) ) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FILETIMETOSYSTEMTIME )
{
   PHB_ITEM pFileTime = hb_pureparam( 1, HB_IT_ANY );
   PHB_ITEM pSystemTime = hb_param( 2, HB_IT_BYREF );

   if( pSystemTime )
   {
      FILETIME *pFt;
      LPSYSTEMTIME pSt;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pFileTime );
      hb_vmSend(0);
      pFt = (FILETIME*) hb_parc(-1);

      pSt = (LPSYSTEMTIME) hb_xgrab( sizeof( SYSTEMTIME ) );

      if( FileTimeToSystemTime( pFt, pSt ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pSystemTime ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "SYSTEMTIME", 10 );
            hb_vmDo(1);

            pByRef = pSystemTime;
            pSystemTime = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pSystemTime->item.asArray.value->pItems + pSystemTime->item.asArray.value->ulLen - 1, (char *) pSt, sizeof(SYSTEMTIME) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pSystemTime );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pSystemTime );
         }
         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
      if( pSt )
      {
         hb_xfree( (void *) pSt );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "FILETIMETOSYSTEMTIME", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }

}

//-------------------------------------------------------------------------------------------------
HB_FUNC( INTERNETGETLASTRESPONSEINFO )
{
   PHB_ITEM pText = hb_pureparam( 2, HB_IT_BYREF );

   if( pText )
   {
      DWORD nError = 0;
      char cText[ 256 ];
      DWORD nLen = sizeof( cText );

      if( InternetGetLastResponseInfo( &nError, (LPTSTR) &cText, &nLen ) )
      {
         hb_stornl( nError, 1 );
         hb_storclen( cText, nLen, 2 );
      }
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( INTERNETDIAL )
{
   DWORD nRet = 0;
   hb_retnl( InternetDial( (HWND) hb_parnl(1), (LPTSTR) hb_parc(2), (DWORD) hb_parnl(3), &nRet, 0 ) );
   if ISBYREF( 4 )
   {
      hb_stornl( (ULONG) nRet, 4 );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( INTERNETGETCONNECTEDSTATE )
{
   LPDWORD lpdwFlags = (LPDWORD) 0;
   hb_retl( InternetGetConnectedState( lpdwFlags, 0 ) );
   if ISBYREF( 1 )
   {
      hb_stornl( (ULONG) lpdwFlags, 1 );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( INTERNETOPEN )
{
   hb_retnl( (ULONG) InternetOpen( (LPCTSTR) hb_parc(1), (DWORD) hb_parnl(2), (LPCTSTR) hb_parc(3), (LPCTSTR) hb_parc(4), (DWORD) hb_parnl(5) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( INTERNETCONNECT )
{
   DWORD_PTR dwContext = 2;
   HINTERNET hRet = InternetConnect( (HINTERNET) hb_parnl(1), (LPCTSTR) hb_parc(2), (INTERNET_PORT) hb_parni(3),
                                      (LPCTSTR) hb_parc(4), (LPCTSTR) hb_parc(5), (DWORD) hb_parnl(6),
                                      (DWORD) hb_parnl(7), dwContext );
   if( hRet != NULL )
   {
      hb_retnl( (ULONG) hRet );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPOPENFILE )
{
   HINTERNET hFtp         = (HINTERNET) hb_parnl(1);
   LPCTSTR   lpszFileName = hb_parc(2);
   DWORD     dwAccess     = (DWORD) hb_parni(3);
   DWORD     dwFlags      = (DWORD) hb_parni(4);
   DWORD_PTR dwContext    = (DWORD_PTR) hb_parnl(5);
   hb_retnl( (ULONG) FtpOpenFile( hFtp, lpszFileName, dwAccess, dwFlags, dwContext ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( INTERNETWRITEFILE )
{
   HINTERNET hFile                    = (HINTERNET) hb_parnl(1);
   LPCVOID   lpBuffer                 = hb_parc(2);
   DWORD     dwNumberOfBytesToWrite   = (DWORD) hb_parnl(3);
   LPDWORD   lpdwNumberOfBytesWritten = (LPDWORD) 0;

   hb_retl( InternetWriteFile( hFile, lpBuffer, dwNumberOfBytesToWrite, lpdwNumberOfBytesWritten ) );

   if ISBYREF( 4 )
   {
      hb_stornl( ( ULONG ) lpdwNumberOfBytesWritten, 4 );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( INTERNETREADFILE )
{
   HINTERNET hFile = (HINTERNET) hb_parnl(1);
   LPCVOID lpBuffer = hb_parc(2);
   DWORD dwNumberOfBytesToRead = (DWORD) hb_parnl(3);
   LPDWORD lpdwNumberOfBytesRead = (LPDWORD) 0;
   BOOL bRet ;

   bRet = InternetReadFile( hFile, &lpBuffer, dwNumberOfBytesToRead, lpdwNumberOfBytesRead );

   hb_retl( bRet );

   if ( bRet )
   {
      if ISBYREF( 4 )
      {
         hb_stornl( (ULONG) lpdwNumberOfBytesRead, 4 );
      }
      hb_storclen( (char *) lpBuffer, (ULONG) lpdwNumberOfBytesRead, 2 );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPCOMMAND )
{
   HINTERNET hInternet       = (HINTERNET *) hb_parnl(1);
   BOOL      fExpectResponse = hb_parl(2);
   DWORD     dwFlags         = (DWORD) hb_parnl(3);
   LPCTSTR   lpszCommand     = (LPCTSTR) hb_parc(4);
   DWORD_PTR dwContext       = (DWORD_PTR) hb_parnl(5);
   HINTERNET hFtpCommand     = 0;
   BOOL      bRet ;

   bRet = FtpCommand( hInternet, fExpectResponse, dwFlags, lpszCommand, dwContext, &hFtpCommand );

   hb_retl( bRet );

   if ( bRet )
   {
      hb_stornl( (ULONG) hFtpCommand, 6 );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPFINDFIRSTFILE )
{
   HINTERNET hResult;
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pStructure )
   {
      LPWIN32_FIND_DATA lpFindFileData = (LPWIN32_FIND_DATA) hb_xgrab( sizeof( WIN32_FIND_DATA ) );

      hResult = FtpFindFirstFile( (HINTERNET) hb_parnl(1), (LPCTSTR) hb_parc(2), lpFindFileData, (DWORD) hb_parnl(4), (DWORD_PTR) hb_parnl(5) );
      if( hResult != INVALID_HANDLE_VALUE )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "WIN32_FIND_DATA", 15 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) lpFindFileData, sizeof(WIN32_FIND_DATA) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
      }
      else
      {
         hb_xfree( (void *) lpFindFileData );
      }
      hb_retnl( (long) hResult );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "FTPFINDFIRSTFILE", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }

}
/*
//-------------------------------------------------------------------------------------------------
HB_FUNC ( INTERNETFINDNEXTFILE )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );
   BOOL bRes;
   if( pStructure )
   {
      LPWIN32_FIND_DATA lpFindFileData = (LPWIN32_FIND_DATA) hb_xgrab( sizeof( WIN32_FIND_DATA ) );

      bRes = InternetFindNextFile( (HINTERNET) hb_parnl(1), (LPVOID) lpFindFileData );

      if( bRes )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "WIN32_FIND_DATA", 15 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) lpFindFileData, sizeof(WIN32_FIND_DATA) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

      }
      else
      {
         hb_xfree( (void *) lpFindFileData );
      }
      hb_retl( bRes );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "INTERNETFINDNEXTFILE", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }

}
*/
//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPGETFILE )
{
   hb_retl( FtpGetFile( (HINTERNET) hb_parnl(1), (LPCTSTR) hb_parc(2), (LPCTSTR) hb_parc(3), hb_parl(4),
                        (DWORD) hb_parnl(5), (DWORD) hb_parnl(6), (DWORD_PTR) hb_parnl(7) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPPUTFILE )
{
   hb_retl( FtpPutFile( (HINTERNET) hb_parnl(1), (LPCTSTR) hb_parc(2), (LPCTSTR) hb_parc(3),
                        (DWORD) hb_parnl(4), (DWORD_PTR) hb_parnl(5) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPCREATEDIRECTORY )
{
   hb_retl( FtpCreateDirectoryA( (HINTERNET) hb_parnl(1), (LPCTSTR) hb_parc(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPREMOVEDIRECTORY )
{
   hb_retl( FtpRemoveDirectoryA( (HINTERNET) hb_parnl(1), (LPCTSTR) hb_parc(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPDELETEFILE )
{
   hb_retl( FtpDeleteFile( (HINTERNET) hb_parnl(1), (LPCTSTR) hb_parc(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPRENAMEFILE )
{
   hb_retl( FtpRenameFileA( (HINTERNET) hb_parnl(1), (LPCTSTR) hb_parc(2), (LPCTSTR) hb_parc(3) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPGETCURRENTDIRECTORY )
{
   LPTSTR lpszCurrentDirectory = (LPTSTR) hb_xgrab( MAX_PATH );
   DWORD  dwCurrentDirectory   = MAX_PATH;
   BOOL   bRet ;

   bRet = FtpGetCurrentDirectory( (HINTERNET) hb_parnl(1), lpszCurrentDirectory, &dwCurrentDirectory );
   hb_retl( bRet );

   if ( bRet )
   {
      if ( ISBYREF( 2 ) )
      {
         hb_storclen( (char *) lpszCurrentDirectory, (ULONG) dwCurrentDirectory, 2 );
      }
   }
   hb_xfree( lpszCurrentDirectory );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( FTPSETCURRENTDIRECTORY )
{
   hb_retl( FtpSetCurrentDirectoryA( (HINTERNET) hb_parnl(1), (LPTSTR) hb_parc(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( INTERNETCLOSEHANDLE )
{
   hb_retl( InternetCloseHandle( (HINTERNET) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC ( INTERNETATTEMPTCONNECT )
{
   hb_retnl( (ULONG) InternetAttemptConnect( 0 ) );
}

//void CALLBACK InternetStatusCallback( HINTERNET hInternet, DWORD dwContext, DWORD dwInternetStatus, LPVOID lpvStatusInformation, DWORD dwStatusInformationLength )
//{
//}

//-------------------------------------------------------------------------------------------------
//HB_FUNC ( INTERNETSETSTATUSCALLBACK )
//{
//   hb_retnl( (long) InternetSetStatusCallback( (HINTERNET) hb_parnl(1), (INTERNET_STATUS_CALLBACK) hb_parnl(2) ) );
//}

static PHB_DYNS pDynSym;

VOID CALLBACK InternetStatusCallback( HINTERNET hInternet, DWORD dwContext, DWORD dwInternetStatus, LPVOID lpvStatusInformation, DWORD dwStatusInformationLength)
{
   if ( pDynSym )
   {
       hb_vmPushSymbol( (HB_SYMB *) pDynSym );
       hb_vmPushNil();
       hb_vmPushLong( (long) hInternet );
       hb_vmPushLong( (long) dwContext );
       hb_vmPushLong( (long) dwInternetStatus );

       if( dwInternetStatus == INTERNET_STATUS_RESPONSE_RECEIVED )
       {
          hb_vmPushLong( *((LPDWORD) lpvStatusInformation) );
       }
       else
       {
          hb_vmPushLong( (LONG) lpvStatusInformation );
       }

       hb_vmPushLong( (long) dwStatusInformationLength );
       hb_vmDo( 5 );
   }
}


HB_FUNC ( INTERNETSETSTATUSCALLBACK )
{
   pDynSym = (PHB_DYNS) hb_parptr(2);
   hb_retnl( (long) InternetSetStatusCallback( (HINTERNET) hb_parnl(1), (INTERNET_STATUS_CALLBACK) InternetStatusCallback ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETPERFORMANCEINFO )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      BOOL bResult;
      PERFORMANCE_INFORMATION *pInfo = (PERFORMANCE_INFORMATION *) hb_xgrab( sizeof( PERFORMANCE_INFORMATION ) );
      PHB_ITEM pByRef;

      bResult = pGetPerformanceInfo( pInfo, sizeof( PERFORMANCE_INFORMATION ) );

      if( HB_IS_OBJECT( pStructure ) )
      {
         pByRef = NULL;
      }
      else
      {
         hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
         hb_vmPushNil();
         hb_itemPushStaticString( "PERFORMANCE_INFORMATION", 23 );
         hb_vmDo(1);

         pByRef = pStructure;
         pStructure = hb_stackReturnItem();
      }

      hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pInfo, sizeof(PERFORMANCE_INFORMATION) );
      hb_vmPushSymbol( pDEVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      if( pByRef )
      {
         hb_itemForwardValue( pByRef, pStructure );
      }

      hb_retl( bResult );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETPERFORMANCEINFO", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETVERSIONINFO )
{
   char *csRet;
   HANDLE hLib = LoadLibrary( hb_parc(1) );
   HRSRC hResource = FindResource( (HMODULE) hLib, MAKEINTRESOURCE(VS_VERSION_INFO), RT_VERSION );

   if (hResource != NULL)
   {
      HGLOBAL hGlobal = LoadResource( (HMODULE) hLib, hResource );

      if( hGlobal != NULL)
      {
         LPVOID versionInfo  = LockResource( hGlobal );

         if( versionInfo != NULL )
         {
            DWORD vLen,langD;
            BOOL retVal;
            LPVOID retbuf = NULL;
            char fileEntry[256];

            sprintf( fileEntry,"\\VarFileInfo\\Translation" );
            retVal = VerQueryValue( versionInfo, fileEntry, &retbuf, (UINT *)&vLen );

            if( retVal && vLen==4 )
            {
               memcpy( &langD, retbuf, 4 );

               sprintf( fileEntry, "\\StringFileInfo\\%02X%02X%02X%02X\\%s",
                        (langD & 0xff00 ) >> 8, langD & 0xff, (langD & 0xff000000 ) >> 24,
                        (langD & 0xff0000 ) >> 16, hb_parc(2) );
            }
            else
            {
               sprintf( fileEntry, "\\StringFileInfo\\%04X04B0\\%s", GetUserDefaultLangID(), hb_parc(2) );
            }

            if( VerQueryValue( versionInfo, fileEntry, &retbuf, (UINT *)&vLen ) )
            {
               csRet = (char*)retbuf;
            }
         }

         UnlockResource( hGlobal );
         FreeResource( hGlobal );
      }
   }

   hb_retc( csRet );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETFILEINFORMATIONBYHANDLE )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      BOOL bResult;
      BY_HANDLE_FILE_INFORMATION *pInfo = (BY_HANDLE_FILE_INFORMATION *) hb_xgrab( sizeof( BY_HANDLE_FILE_INFORMATION ) );
      PHB_ITEM pByRef;

      bResult = GetFileInformationByHandle( (HANDLE) hb_parnl(1), pInfo );

      if( HB_IS_OBJECT( pStructure ) )
      {
         pByRef = NULL;
      }
      else
      {
         hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
         hb_vmPushNil();
         hb_itemPushStaticString( "BY_HANDLE_FILE_INFORMATION", 26 );
         hb_vmDo(1);

         pByRef = pStructure;
         pStructure = hb_stackReturnItem();
      }

      hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pInfo, sizeof(BY_HANDLE_FILE_INFORMATION) );
      hb_vmPushSymbol( pDEVALUE->pSymbol );
      hb_vmPush( pStructure );
      hb_vmSend(0);

      if( pByRef )
      {
         hb_itemForwardValue( pByRef, pStructure );
      }
      hb_retl( bResult );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETFILEINFORMATIONBYHANDLE", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( FILETIMETODOSDATETIME )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      WORD date = 0;
      WORD time = 0;

      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( FileTimeToDosDateTime( (CONST FILETIME*) hb_parc(-1), &date, &time) );

      hb_storclen( (char *) &date, 2, 2 );
      hb_storclen( (char *) &time, 3, 2 );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "FILETIMETODOSDATETIME", 1, hb_paramError(1) );
   }

}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CLEARCOMMERROR )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pStructure )
   {
      DWORD lpErrors;
      COMSTAT *pcs = (COMSTAT *) hb_xgrab( sizeof( COMSTAT ) );

      if( ClearCommError( (HANDLE) hb_parnl(1), &lpErrors, pcs ) )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "COMSTAT", 7 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pcs, sizeof( COMSTAT ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pcs );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETMESSAGE", 4, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETCOMMMODEMSTATUS )
{
   DWORD dwStatus;
   hb_retl( GetCommModemStatus( (HANDLE) hb_parnl(1), &dwStatus ) );
   hb_stornl( (ULONG) dwStatus, 2 );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETCOMMPROPERTIES )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      BOOL bResult;
      COMMPROP *pcp = (COMMPROP *) hb_xgrab( sizeof( COMMPROP ) );

      bResult = GetCommProperties( (HANDLE) hb_parnl(1), pcp );
      if( bResult )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "COMMPROP", 8 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pcp, sizeof(COMMPROP) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
      }
      hb_retl( bResult );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETCOMMPROPERTIES", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ESCAPECOMMFUNCTION )
{
   hb_retl( EscapeCommFunction( (HANDLE) hb_parnl(1), hb_parnl(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETKEYNAMETEXT )
{
   int iSize = hb_parni(3);
   char * cBuffer = (char *) hb_xgrab( iSize );
   int iRead;

   iRead = GetKeyNameText( hb_parnl(1), (LPTSTR) cBuffer, iSize );

   if( iRead && ISBYREF(2) )
   {
      hb_storclenAdopt( cBuffer, strlen(cBuffer), 2 );
   }
   else
   {
      hb_xfree( (void *) cBuffer );
   }
   hb_retl( iRead );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( VKKEYSCAN )
{
   hb_retnl( (long) VkKeyScan( hb_parcx(1)[0] ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( OEMKEYSCAN )
{
   hb_retnl( (long) OemKeyScan( (WORD) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( TOASCII )
{
   BYTE lpKeyState[256];
   WORD wChar;

   memset( lpKeyState, 0, 256 );

   // TODO: Aug - what does it suppose to do, since you do not pass KeyState, and don't get translated char?
   hb_retl( ToAscii( (UINT) hb_parni(1), (UINT) hb_parnl(2), lpKeyState, &wChar, (UINT) hb_parni(5) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( ALPHABLEND )
{
   PHB_ITEM pStructure = hb_pureparam( 11, HB_IT_ANY );
   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( AlphaBlend( (HDC) hb_parnl(1),
                                 hb_parni(2),
                                 hb_parni(3),
                                 hb_parni(4),
                                 hb_parni(5),
                           (HDC) hb_parnl(6),
                                 hb_parni(7),
                                 hb_parni(8),
                                 hb_parni(9),
                                 hb_parni(10),
          *(BLENDFUNCTION*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "ALPHABLEND", 1, hb_paramError(1) );
   }
}


//-------------------------------------------------------------------------------------------------
HB_FUNC( GETHOSTBYNAME )
{
   HOSTENT *he ;
   he = gethostbyname( hb_parcx( 1 ) ) ;
   hb_retclen( ( char *)he, sizeof(HOSTENT) ) ;
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( URLGETPART )
{
   LPCTSTR pszURL = (LPCTSTR) hb_parc(1);
   TCHAR szHost[1024];
   DWORD cbHostBuf;

   szHost[0] = '\0';
   cbHostBuf = sizeof(szHost)/sizeof(TCHAR);

   if( SUCCEEDED( UrlGetPart( pszURL, szHost, &cbHostBuf, hb_parni(2), 0 ) ) )
   {
      hb_retc( (char*) szHost);
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETQUEUESTATUS )
{
   hb_retnl( (long) GetQueueStatus( (UINT) hb_parnl(1) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( WAITMESSAGE )
{
   hb_retl( WaitMessage() );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DWMENABLEBLURBEHINDWINDOW )
{
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pDwmEnableBlurBehindWindow )
   {
      if( pStructure && HB_IS_OBJECT( pStructure ) )
      {
         hb_vmPushSymbol( pVALUE->pSymbol );
         hb_itemPushForward( pStructure );
         hb_vmSend(0);

         hb_retnl( (LONG) pDwmEnableBlurBehindWindow( (HWND) hb_parnl(1), (const DWM_BLURBEHIND*) hb_parc(-1) ) );
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 6001, NULL, "DWMENABLEBLURBEHINDWINDOW", 1, hb_paramError(1) );
      }
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DWMEXTENDFRAMEINTOCLIENTAREA )
{
   BOOL bRet = FALSE;
   PHB_ITEM pStructure = hb_pureparam( 2, HB_IT_ANY );

   if( pDwmExtendFrameIntoClientArea )
   {
      if( pStructure && HB_IS_OBJECT( pStructure ) )
      {
         HRESULT lRet;

         hb_vmPushSymbol( pVALUE->pSymbol );
         hb_itemPushForward( pStructure );
         hb_vmSend(0);

         lRet = pDwmExtendFrameIntoClientArea( (HWND) hb_parnl(1), (const MARGINS*) hb_parc(-1) );
         if( lRet == S_OK )
         {
            bRet = TRUE;
         }
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 6001, NULL, "DWMEXTENDFRAMEINTOCLIENTAREA", 1, hb_paramError(1) );
      }
   }
   hb_retl( bRet );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DWMGETCOLORIZATIONCOLOR )
{
   BOOL bRet = FALSE;
   if( pDwmGetColorizationColor )
   {
      HRESULT lRet;
      DWORD pcrColorization;
      BOOL  pfOpaqueBlend;

      lRet = pDwmGetColorizationColor( &pcrColorization, &pfOpaqueBlend );

      if( lRet == S_OK )
      {
         hb_stornl( (long) pcrColorization, 1 );
         hb_storl( pfOpaqueBlend, 2 );
         bRet = TRUE;
      }
   }
   hb_retl( bRet );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( DWMISCOMPOSITIONENABLED )
{
   BOOL bRet = FALSE;
   if( pDwmIsCompositionEnabled )
   {
      HRESULT lRet;
      BOOL pfEnabled;
      lRet = pDwmIsCompositionEnabled( &pfEnabled );

      if( lRet == S_OK )
      {
         bRet = pfEnabled;
      }
   }
   hb_retl( bRet );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( INVALIDATERGN )
{
   hb_retl( InvalidateRgn( (HWND) hb_parnl(1), (HRGN) hb_parnl(2), hb_parl(3) ) );
}

//-------------------------------------------------------------------------------------------------
/*
HB_FUNC( SHFILEOPERATION )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );
   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( SHFileOperation( *(SHFILEOPSTRUCT*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SHFILEOPERATION", 1, hb_paramError(1) );
   }
}
*/

HB_FUNC( SHFILEOPERATION )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      int iResult;
      SHFILEOPSTRUCT *sfo = (SHFILEOPSTRUCT *) hb_xgrab( sizeof( SHFILEOPSTRUCT ) );

      iResult = SHFileOperation( sfo );
      if( iResult )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "SHFILEOPSTRUCT", 14 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) sfo, sizeof(SHFILEOPSTRUCT) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
      }
      hb_retni( iResult );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SHFILEOPERATION", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CREATEUUID )
{
   unsigned char *StringUuid;
   UUID __RPC_FAR Uuid;
   UuidCreate( &Uuid );
   UuidToString( &Uuid, &StringUuid );
   hb_retc( (const char *) StringUuid );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( QUERYSERVICESTATUS )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      DWORD dRes;
      SERVICE_STATUS *ss = (SERVICE_STATUS *) hb_xgrab( sizeof( SERVICE_STATUS ) );
      
      dRes = pQueryServiceStatus( (SC_HANDLE) hb_parnl(1), ss );

      if( dRes )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "SERVICE_STATUS", 14 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) ss, sizeof(SERVICE_STATUS) );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
      }
      hb_retni( dRes );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "QUERYSERVICESTATUS", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( QUERYSERVICECONFIG )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );

   if( pStructure )
   {
      BOOL bRes;
      DWORD dwBytesNeeded = 0;

      QUERY_SERVICE_CONFIG *qsc = (QUERY_SERVICE_CONFIG *) hb_xgrab( sizeof(QUERY_SERVICE_CONFIG) );

      bRes = pQueryServiceConfig( (SC_HANDLE) hb_parnl(1), qsc, dwBytesNeeded, &dwBytesNeeded );
      if( !bRes )
      {
         DWORD retVal = GetLastError();
         if( retVal == ERROR_INSUFFICIENT_BUFFER )
         {
            DWORD dwBytes = sizeof(QUERY_SERVICE_CONFIG) + dwBytesNeeded;
            hb_xfree( (void *) qsc );
            qsc = (QUERY_SERVICE_CONFIG *) hb_xgrab( dwBytes );
            bRes = pQueryServiceConfig( (SC_HANDLE) hb_parnl(1), qsc, dwBytes, &dwBytesNeeded );
            dwBytesNeeded = dwBytes;
         }
      }

      if( bRes )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "QUERY_SERVICE_CONFIG", 20 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) qsc, dwBytesNeeded );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
      }
      hb_retl( bRes );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "QUERYSERVICECONFIG", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( QUERYSERVICECONFIG2 )
{
   PHB_ITEM pStructure = hb_param( 3, HB_IT_BYREF );

   if( pStructure )
   {
      BOOL bRes;
      DWORD dwInfoLevel = (DWORD) hb_parni(2);
      DWORD dwBytesNeeded = 0;
      LPBYTE qsc;

      pQueryServiceConfig2( (SC_HANDLE) hb_parnl(1), dwInfoLevel, NULL, 0, &dwBytesNeeded );
      qsc = (LPBYTE) hb_xgrab( dwBytesNeeded );

      bRes = pQueryServiceConfig2( (SC_HANDLE) hb_parnl(1), dwInfoLevel, qsc, dwBytesNeeded, &dwBytesNeeded );

      if( bRes )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( pStructure->item.asString.value, strlen( pStructure->item.asString.value ) );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) qsc, dwBytesNeeded );
         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }
      }
      hb_retl( bRes );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "QUERYSERVICECONFIG2", 1, hb_paramError(3) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( CHANGESERVICEDESCRIPTION )
{
   SERVICE_DESCRIPTION sd;
   sd.lpDescription = (LPTSTR) hb_parc(2);
   pChangeServiceConfig2( (SC_HANDLE) hb_parnl(1), SERVICE_CONFIG_DESCRIPTION, (LPVOID) &sd );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SHELLEXECUTEEX )
{
   PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

   if( pStructure )
   {
      BOOL bResult;
      SHELLEXECUTEINFO *pSei = (SHELLEXECUTEINFO *) hb_xgrab( sizeof( SHELLEXECUTEINFO ) );

      bResult = ShellExecuteEx( pSei );

      if( bResult && bResult != -1 )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "SHELLEXECUTEINFO", 3 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pSei, sizeof( SHELLEXECUTEINFO ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pSei );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "SHELLEXECUTEEX", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETSTRETCHBLTMODE )
{
   hb_retni( SetStretchBltMode( (HDC) hb_parnl(1), hb_parni(2) ) );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( STRETCHDIBITS )
{
   PHB_ITEM pStructure = hb_pureparam( 11, HB_IT_ANY );

   if( pStructure == NULL || HB_IS_NIL( pStructure ) )
   {
      hb_retl( StretchDIBits( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7), hb_parni(8), hb_parni(9), (VOID *) hb_parc(10),
                               NULL, (UINT) hb_parni(12), (DWORD) hb_parnl(13) ) );
   }
   else if(  HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( StretchDIBits( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7), hb_parni(8), hb_parni(9), (VOID *) hb_parc(10),
                               (const BITMAPINFO*) hb_parc(-1), (UINT) hb_parni(12), (DWORD) hb_parnl(13) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "STRETCHDIBITS", 11, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4), hb_paramError(5), hb_paramError(6), hb_paramError(7), hb_paramError(8), hb_paramError(9), hb_paramError(10), hb_paramError(11) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( OLEGETICONOFCLASS )
{
   CLSID clsid;
   BSTR lpszLabel;
   BSTR wClassID;

   wClassID = hb_oleAnsiToWide( hb_parc(1) );
   CLSIDFromString( wClassID, &clsid );

   lpszLabel = hb_oleAnsiToWide( hb_parc(2) );

   hb_retnl( (long) OleGetIconOfClass( &clsid, lpszLabel, hb_parl(3) ) );

   hb_xfree( wClassID );
   hb_xfree( lpszLabel );
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( IMAGELISTDRAWINDIRECT  )
{
   PHB_ITEM pStructure = hb_pureparam( 1, HB_IT_ANY );

   if( pStructure && HB_IS_OBJECT( pStructure ) )
   {
      hb_vmPushSymbol( pVALUE->pSymbol );
      hb_itemPushForward( pStructure );
      hb_vmSend(0);

      hb_retl( ImageList_DrawIndirect( (IMAGELISTDRAWPARAMS*) hb_parc(-1) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "IMAGELISTDRAWINDIRECT", 1, hb_paramError(1) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SETDCBRUSHCOLOR )
{
   hb_retnl( (long) pSetDCBrushColor( (HDC) hb_parnl(1), (COLORREF) hb_parnl(2) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( SHOWSCROLLBAR )
{
   hb_retl( ShowScrollBar( (HWND) hb_parnl(1), (int) hb_parni(2), hb_parl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( ENABLESCROLLBAR )
{
   hb_retl( EnableScrollBar( (HWND) hb_parnl(1), (int) hb_parni(2), (UINT) hb_parnl(3) ) );
}

//-----------------------------------------------------------------------------
HB_FUNC( GETUSERDEFAULTUILANGUAGE )
{
   hb_retnl( (long) pGetUserDefaultUILanguage() );
}

HB_FUNC( GETSYSTEMDEFAULTUILANGUAGE )
{
   hb_retnl( (long) pGetSystemDefaultUILanguage() );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( XGRADIENTFILL )
{
   PHB_ITEM pArray1 = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pArray2 = hb_param( 4, HB_IT_ARRAY );

   if( pArray1 && pArray2 )
   {
      //PHB_ITEM pItem;
      TRIVERTEX *pVertex;
      GRADIENT_RECT *pRect;
      GRADIENT_TRIANGLE *pTria;

      ULONG dwMode = (ULONG) hb_parnl(6);
      int iCount1 = pArray1->item.asArray.value->ulLen;
      int iCount2 = pArray2->item.asArray.value->ulLen;

      int i;

      pVertex  = (TRIVERTEX *) hb_xgrab( iCount1 * sizeof( TRIVERTEX ) );

      if( dwMode == GRADIENT_FILL_TRIANGLE )
      {
         pTria = (GRADIENT_TRIANGLE *) hb_xgrab( iCount2 * sizeof( GRADIENT_TRIANGLE ) );
      }
      else
      {
         pRect = (GRADIENT_RECT *) hb_xgrab( iCount2 * sizeof( GRADIENT_RECT ) );
      }

      for ( i = 0; i < iCount1; i++ )
      {
         PHB_ITEM pItem = hb_itemArrayGet( pArray1, i+1 );
         pVertex[i].x     = hb_arrayGetNI( pItem, 1 );
         pVertex[i].y     = hb_arrayGetNI( pItem, 2 );
         pVertex[i].Red   = hb_arrayGetNI( pItem, 3 );
         pVertex[i].Green = hb_arrayGetNI( pItem, 4 );
         pVertex[i].Blue  = hb_arrayGetNI( pItem, 5 );
         hb_itemRelease( pItem );
      }

      for ( i = 0; i < iCount2; i++ )
      {
         if( dwMode == GRADIENT_FILL_TRIANGLE )
         {
            PHB_ITEM pItem = (PHB_ITEM) hb_itemArrayGet( pArray2, i + 1 );
            pTria[i].Vertex1 = hb_arrayGetNI( pItem, 1 );
            pTria[i].Vertex2 = hb_arrayGetNI( pItem, 2 );
            pTria[i].Vertex3 = hb_arrayGetNI( pItem, 3 );
            hb_itemRelease( pItem );
         }
         else
         {
            PHB_ITEM pItem = (PHB_ITEM) hb_itemArrayGet( pArray2, i + 1 );
            pRect[i].UpperLeft  = hb_arrayGetNI( pItem, 1 );
            pRect[i].LowerRight = hb_arrayGetNI( pItem, 2 );
            hb_itemRelease( pItem );
         }
      }

      hb_retl( GradientFill( (HDC) hb_parnl(1), pVertex, (ULONG) iCount1, dwMode == GRADIENT_FILL_TRIANGLE ? (PVOID) pTria : (PVOID) pRect, (ULONG) iCount2, dwMode ) );

      hb_xfree( (void *) pVertex );

      if( dwMode == GRADIENT_FILL_TRIANGLE )
      {
          hb_xfree( (void *) pTria );
      }
      else
      {
          hb_xfree( (void *) pRect );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GRADIENTFILL", 2, hb_paramError(1), hb_paramError(2) );
   }
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( GETCURRENTDIRECTORY )
{
   char szBuffer[ MAX_PATH+1 ] = {0} ;
   GetCurrentDirectory( MAX_PATH, szBuffer );
   hb_retc( szBuffer );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( SETCURRENTDIRECTORY )
{
   hb_retl( SetCurrentDirectory( (LPCTSTR) hb_parc(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( REMOVEDIRECTORY )
{
   hb_retl( RemoveDirectory( (LPCTSTR) hb_parc(1) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( MAKEWPARAM )
{
   hb_retnl( (long) MAKEWPARAM( (WORD) hb_parnl(1), (WORD) hb_parnl(2) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( UNICODETOSTRING )
{
   int iLen = WideCharToMultiByte( CP_ACP, 0, (LPCWSTR) hb_parc(1), -1, NULL, 0, NULL, NULL );
   if ( iLen )
   {
      char *cString = (char *) hb_xgrab( iLen );
      WideCharToMultiByte( CP_ACP, 0, (LPCWSTR) hb_parc(1), -1, cString, iLen, NULL, NULL );
      hb_retclenAdopt( cString, iLen );
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( EXTFLOODFILL )
{
   hb_retl( ExtFloodFill( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), (COLORREF) hb_parnl(4), (UINT) hb_parnl(5) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( FLOODFILL )
{
   hb_retl( FloodFill( (HDC) hb_parnl(1), hb_parni(2), hb_parni(3), (COLORREF) hb_parnl(4) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( CREATEPOLYGONRGN )
{
   POINT * pPoint ;
   int iCount ;
   int i ;
   PHB_ITEM aParam ;
   PHB_ITEM pStructure;

   if (ISARRAY( 1 ) )
   {
       iCount = (int) hb_parinfa( 1, 0 );
       pPoint  = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(1,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          pStructure = hb_itemArrayGet( aParam, i+1 );

          hb_vmPushSymbol( pVALUE->pSymbol );
          hb_itemPushForward( pStructure );
          hb_vmSend(0);

          *(pPoint+i) = *(POINT*) hb_parc(-1);
       }

       hb_retnl( (LONG) CreatePolygonRgn( pPoint, iCount, hb_parni(2) ) );
       hb_xfree(pPoint);
   }
}

//------------------------------------------------------------------------------------------------
HB_FUNC( FILLRGN )
{
   hb_retl( FillRgn( (HDC) hb_parnl(1), (HRGN) hb_parnl(2), (HBRUSH) hb_parnl(3) ) );
}

//------------------------------------------------------------------------------------------------
HB_FUNC( GETCOMBOBOXINFO )
{
   PHB_ITEM pStructure = hb_param( 2, HB_IT_BYREF );
   if( pStructure )
   {
      BOOL bResult;
      COMBOBOXINFO *pcbi = (COMBOBOXINFO *) hb_xgrab( sizeof( COMBOBOXINFO ) );

      bResult = GetComboBoxInfo( (HWND) hb_parnl(1), pcbi );
      if( bResult && bResult != -1 )
      {
         PHB_ITEM pByRef;

         if( HB_IS_OBJECT( pStructure ) )
         {
            pByRef = NULL;
         }
         else
         {
            hb_vmPushSymbol( pHB_CSTRUCTURE->pSymbol );
            hb_vmPushNil();
            hb_itemPushStaticString( "COMBOBOXINFO", 12 );
            hb_vmDo(1);

            pByRef = pStructure;
            pStructure = hb_stackReturnItem();
         }

         hb_itemPutCRaw( pStructure->item.asArray.value->pItems + pStructure->item.asArray.value->ulLen - 1, (char *) pcbi, sizeof( COMBOBOXINFO ) );

         hb_vmPushSymbol( pDEVALUE->pSymbol );
         hb_vmPush( pStructure );
         hb_vmSend(0);

         if( pByRef )
         {
            hb_itemForwardValue( pByRef, pStructure );
         }

         hb_retl( TRUE );
      }
      else
      {
         hb_xfree( (void *) pcbi );
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 6001, NULL, "GETCOMBOBOXINFO", 4, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4) );
   }

}

//-------------------------------------------------------------------------------------------------
HB_FUNC( SHOWCURSOR )
{
   hb_retl( ShowCursor( hb_parl( 1 ) ) );
}

//------------------------------------------------------------------------------------------------
//HB_FUNC( CRYPTUIDLGSELECTCERTIFICATEFROMSTORE )
//{
//   if( pCryptUIDlgSelectCertificateFromStore )
//   {
//      void *pvReserved;
//      HCERTSTORE hcert;
//      LPCWSTR pwszTitle = hb_oleAnsiToWide( (LPSTR) hb_parc(3) );
//      LPCWSTR pwszDisplayString = hb_oleAnsiToWide( (LPSTR) hb_parc(4) );
//      hb_retl( pCryptUIDlgSelectCertificateFromStore( (HCERTSTORE) hb_parnl(1), (HWND) hb_parnl(2), pwszTitle, pwszDisplayString, (DWORD) hb_parnl(5), (DWORD) hb_parnl(6), pvReserved ) );
//   }
//   else
//   {
//      hb_errRT_BASE( EG_ARG, 6999, NULL, "CryptUIDlgSelectCertificateFromStore", 1, hb_paramError(1) );
//   }
//}

HB_FUNC( CERTIFICATEDIALOG )
{
   HCERTSTORE       hCertStore = NULL;        
   PCCERT_CONTEXT   pCertContext = NULL;      
   
   if( pCertOpenSystemStore )
   {
      TCHAR * pszStoreName = TEXT("MY");
      if( hCertStore = pCertOpenSystemStore( NULL, pszStoreName ))
      {
         if( pCertContext = pCryptUIDlgSelectCertificateFromStore( hCertStore, (HWND) hb_parnl(1), NULL, NULL, CRYPTUI_SELECT_LOCATION_COLUMN, 0, NULL ) )
         {
            if( pCertNameToStr )
            {
               DWORD dwSize = 0;
               //CERT_NAME_BLOB NameBlob = pCertContext->pCertInfo->Subject;
               //dwSize = pCertNameToStr( pCertContext->dwCertEncodingType, &NameBlob, CERT_SIMPLE_NAME_STR, NULL, 0);
               dwSize = pCertGetNameString(pCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE,0, NULL, NULL, 0 );
               if( dwSize > 1 )
               {
                  char *cBuffer = (char *) hb_xgrab( (int)dwSize + 1 );
                  //pCertNameToStr( pCertContext->dwCertEncodingType, &NameBlob, CERT_SIMPLE_NAME_STR, cBuffer, dwSize );
                  pCertGetNameString(pCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE,0, NULL, cBuffer, dwSize );
                  hb_retcAdopt( cBuffer );
               }
               pCertFreeCertificateContext(pCertContext);
            }
         }
         pCertCloseStore(hCertStore,0);
      }
   }
}

