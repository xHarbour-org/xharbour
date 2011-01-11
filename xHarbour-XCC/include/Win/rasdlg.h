#ifndef _RASDLG_H
#define _RASDLG_H

/* Remote Access Common Dialog API definitions */

#include <pshpack4.h>

#ifdef __cplusplus
extern "C" {
#endif

#include <ras.h>

typedef VOID (WINAPI* RASPBDLGFUNCW)(ULONG_PTR,DWORD,LPWSTR,LPVOID);
typedef VOID (WINAPI* RASPBDLGFUNCA)(ULONG_PTR,DWORD,LPSTR,LPVOID);

#define RASPBDEVENT_AddEntry  1
#define RASPBDEVENT_EditEntry  2
#define RASPBDEVENT_RemoveEntry 3
#define RASPBDEVENT_DialEntry  4
#define RASPBDEVENT_EditGlobals  5
#define RASPBDEVENT_NoUser  6
#define RASPBDEVENT_NoUserEdit  7

#define RASNOUSER_SmartCard  0x00000001

#define RASNOUSERW struct tagRASNOUSERW
RASNOUSERW {
    DWORD dwSize;
    DWORD dwFlags;
    DWORD dwTimeoutMs;
    WCHAR szUserName[UNLEN+1];
    WCHAR szPassword[PWLEN+1];
    WCHAR szDomain[DNLEN+1];
};

#define RASNOUSERA struct tagRASNOUSERA
RASNOUSERA {
    DWORD dwSize;
    DWORD dwFlags;
    DWORD dwTimeoutMs;
    CHAR szUserName[UNLEN+1];
    CHAR szPassword[PWLEN+1];
    CHAR szDomain[DNLEN+1];
};

#ifdef UNICODE
#define RASNOUSER  RASNOUSERW
#else
#define RASNOUSER  RASNOUSERA
#endif

#define LPRASNOUSERW  RASNOUSERW*
#define LPRASNOUSERA  RASNOUSERA*
#define LPRASNOUSER  RASNOUSER*

#define RASPBDFLAG_PositionDlg  0x00000001
#define RASPBDFLAG_ForceCloseOnDial  0x00000002
#define RASPBDFLAG_NoUser  0x00000010
#define RASPBDFLAG_UpdateDefaults  0x80000000

#define RASPBDLGW struct tagRASPBDLGW
RASPBDLGW {
    DWORD dwSize;
    HWND hwndOwner;
    DWORD dwFlags;
    LONG xDlg;
    LONG yDlg;
    ULONG_PTR dwCallbackId;
    RASPBDLGFUNCW pCallback;
    DWORD dwError;
    ULONG_PTR reserved;
    ULONG_PTR reserved2;
};

#define RASPBDLGA struct tagRASPBDLGA
RASPBDLGA {
    DWORD dwSize;
    HWND hwndOwner;
    DWORD dwFlags;
    LONG xDlg;
    LONG yDlg;
    ULONG_PTR dwCallbackId;
    RASPBDLGFUNCA pCallback;
    DWORD dwError;
    ULONG_PTR reserved;
    ULONG_PTR reserved2;
};

#ifdef UNICODE
#define RASPBDLG  RASPBDLGW
#define RASPBDLGFUNC  RASPBDLGFUNCW
#else
#define RASPBDLG  RASPBDLGA
#define RASPBDLGFUNC  RASPBDLGFUNCA
#endif

#define LPRASPBDLGW  RASPBDLGW*
#define LPRASPBDLGA  RASPBDLGA*
#define LPRASPBDLG  RASPBDLG*

#define RASEDFLAG_PositionDlg  0x00000001
#define RASEDFLAG_NewEntry  0x00000002
#define RASEDFLAG_CloneEntry  0x00000004
#define RASEDFLAG_NoRename  0x00000008
#define RASEDFLAG_ShellOwned  0x40000000
#define RASEDFLAG_NewPhoneEntry  0x00000010
#define RASEDFLAG_NewTunnelEntry  0x00000020
#define RASEDFLAG_NewDirectEntry  0x00000040

#define RASENTRYDLGW struct tagRASENTRYDLGW
RASENTRYDLGW {
    DWORD dwSize;
    HWND hwndOwner;
    DWORD dwFlags;
    LONG xDlg;
    LONG yDlg;
    WCHAR szEntry[RAS_MaxEntryName+1];
    DWORD dwError;
    ULONG_PTR reserved;
    ULONG_PTR reserved2;
};

#define RASENTRYDLGA struct tagRASENTRYDLGA
RASENTRYDLGA {
    DWORD dwSize;
    HWND hwndOwner;
    DWORD dwFlags;
    LONG xDlg;
    LONG yDlg;
    CHAR szEntry[RAS_MaxEntryName+1];
    DWORD dwError;
    ULONG_PTR reserved;
    ULONG_PTR reserved2;
};

#ifdef UNICODE
#define RASENTRYDLG  RASENTRYDLGW
#else
#define RASENTRYDLG  RASENTRYDLGA
#endif

#define LPRASENTRYDLGW  RASENTRYDLGW*
#define LPRASENTRYDLGA  RASENTRYDLGA*
#define LPRASENTRYDLG  RASENTRYDLG*

#define RASDDFLAG_PositionDlg  0x00000001
#define RASDDFLAG_NoPrompt  0x00000002
#define RASDDFLAG_LinkFailure  0x80000000

#define RASDIALDLG struct tagRASDIALDLG
RASDIALDLG {
    DWORD dwSize;
    HWND hwndOwner;
    DWORD dwFlags;
    LONG xDlg;
    LONG yDlg;
    DWORD dwSubEntry;
    DWORD dwError;
    ULONG_PTR reserved;
    ULONG_PTR reserved2;
};

#define LPRASDIALDLG RASDIALDLG*

#define RASMDPAGE_Status  0
#define RASMDPAGE_Summary  1
#define RASMDPAGE_Preferences 2

#define RASMDFLAG_PositionDlg  0x00000001
#define RASMDFLAG_UpdateDefaults  0x80000000

#define RASMONITORDLG struct tagRASMONITORDLG
RASMONITORDLG {
    DWORD dwSize;
    HWND hwndOwner;
    DWORD dwFlags;
    DWORD dwStartPage;
    LONG xDlg;
    LONG yDlg;
    DWORD dwError;
    ULONG_PTR reserved;
    ULONG_PTR reserved2;
};

#define LPRASMONITORDLG RASMONITORDLG*

#if (WINVER >= 0x500)
typedef BOOL (WINAPI *RasCustomDialDlgFn)(HINSTANCE,DWORD,LPTSTR,LPTSTR,LPTSTR,LPRASDIALDLG,PVOID);
typedef BOOL (WINAPI *RasCustomEntryDlgFn)(HINSTANCE,LPTSTR,LPTSTR,LPRASENTRYDLG,DWORD);
typedef DWORD (WINAPI *PFNRASGETBUFFER)(PBYTE*,PDWORD);
typedef DWORD (WINAPI *PFNRASFREEBUFFER)(PBYTE);
typedef DWORD (WINAPI *PFNRASSENDBUFFER)(HANDLE,PBYTE,DWORD);
typedef DWORD (WINAPI *PFNRASRECEIVEBUFFER)(HANDLE,PBYTE,PDWORD,DWORD,HANDLE);
typedef DWORD (WINAPI *PFNRASRETRIEVEBUFFER)(HANDLE,PBYTE,PDWORD);
typedef  DWORD (WINAPI *RasCustomScriptExecuteFn)(HANDLE,LPCWSTR,LPCWSTR,PFNRASGETBUFFER,PFNRASFREEBUFFER,PFNRASSENDBUFFER,PFNRASRECEIVEBUFFER,PFNRASRETRIEVEBUFFER,HWND,RASDIALPARAMS*,PVOID);
#endif /* WINVER >= 0x500 */

BOOL APIENTRY RasPhonebookDlgA(LPSTR,LPSTR,LPRASPBDLGA);
BOOL APIENTRY RasPhonebookDlgW(LPWSTR,LPWSTR,LPRASPBDLGW);
BOOL APIENTRY RasEntryDlgA(LPSTR,LPSTR,LPRASENTRYDLGA);
BOOL APIENTRY RasEntryDlgW(LPWSTR,LPWSTR,LPRASENTRYDLGW);
BOOL APIENTRY RasDialDlgA(LPSTR,LPSTR,LPSTR,LPRASDIALDLG);
BOOL APIENTRY RasDialDlgW(LPWSTR,LPWSTR,LPWSTR,LPRASDIALDLG);
BOOL APIENTRY RasMonitorDlgA(LPSTR,LPRASMONITORDLG);
BOOL APIENTRY RasMonitorDlgW(LPWSTR,LPRASMONITORDLG);

#ifdef UNICODE
#define RasPhonebookDlg  RasPhonebookDlgW
#define RasEntryDlg  RasEntryDlgW
#define RasDialDlg  RasDialDlgW
#define RasMonitorDlg  RasMonitorDlgW
#else
#define RasPhonebookDlg  RasPhonebookDlgA
#define RasEntryDlg  RasEntryDlgA
#define RasDialDlg  RasDialDlgA
#define RasMonitorDlg  RasMonitorDlgA
#endif

#ifdef __cplusplus
}
#endif

#include <poppack.h>

#endif /* _RASDLG_H */
