#ifndef _TLHELP32_H
#define _TLHELP32_H

/* Windows Tool Help definitions */

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_MODULE_NAME32  255

#define TH32CS_SNAPHEAPLIST 0x00000001
#define TH32CS_SNAPPROCESS  0x00000002
#define TH32CS_SNAPTHREAD  0x00000004
#define TH32CS_SNAPMODULE  0x00000008
#define TH32CS_SNAPALL  (TH32CS_SNAPHEAPLIST|TH32CS_SNAPPROCESS|TH32CS_SNAPTHREAD|TH32CS_SNAPMODULE)
#define TH32CS_INHERIT  0x80000000

#define HF32_DEFAULT  1
#define HF32_SHARED  2

#define LF32_FIXED  0x00000001
#define LF32_FREE  0x00000002
#define LF32_MOVEABLE 0x00000004
#define LF32_DECOMMIT 0x00000008
#define LF32_BIGBLOCK 0x00000010

typedef struct tagHEAPLIST32 {
    SIZE_T dwSize;
    DWORD th32ProcessID;
    ULONG_PTR th32HeapID;
    DWORD dwFlags;
} HEAPLIST32, *PHEAPLIST32, *LPHEAPLIST32;

typedef struct tagHEAPENTRY32 {
    SIZE_T dwSize;
    HANDLE hHandle;
    ULONG_PTR dwAddress;
    SIZE_T dwBlockSize;
    DWORD dwFlags;
    DWORD dwLockCount;
    DWORD dwResvd;
    DWORD th32ProcessID;
    ULONG_PTR th32HeapID;
} HEAPENTRY32, *PHEAPENTRY32, *LPHEAPENTRY32;

#ifndef _WINCE
typedef struct tagPROCESSENTRY32W {
    DWORD dwSize;
    DWORD cntUsage;
    DWORD th32ProcessID;
    ULONG_PTR th32DefaultHeapID;
    DWORD th32ModuleID;
    DWORD cntThreads;
    DWORD th32ParentProcessID;
    LONG pcPriClassBase;
    DWORD dwFlags;
    WCHAR szExeFile[MAX_PATH];
} PROCESSENTRY32W, *PPROCESSENTRY32W, *LPPROCESSENTRY32W;
#endif /* _WINCE */

typedef struct tagPROCESSENTRY32 {
    DWORD dwSize;
    DWORD cntUsage;
    DWORD th32ProcessID;
    ULONG_PTR th32DefaultHeapID;
    DWORD th32ModuleID;
    DWORD cntThreads;
    DWORD th32ParentProcessID;
    LONG pcPriClassBase;
    DWORD dwFlags;
#ifdef _WINCE
    TCHAR szExeFile[MAX_PATH];
    DWORD th32MemoryBase;
    DWORD th32AccessKey;
#else
    CHAR szExeFile[MAX_PATH];
#endif /* _WINCE */
} PROCESSENTRY32, *PPROCESSENTRY32, *LPPROCESSENTRY32;

typedef struct tagTHREADENTRY32 {
    DWORD dwSize;
    DWORD cntUsage;
    DWORD th32ThreadID;
    DWORD th32OwnerProcessID;
    LONG tpBasePri;
    LONG tpDeltaPri;
    DWORD dwFlags;
#ifdef _WINCE
    DWORD th32AccessKey;
    DWORD th32CurrentProcessID;
#endif
} THREADENTRY32, *PTHREADENTRY32, *LPTHREADENTRY32;

#ifndef _WINCE
typedef struct tagMODULEENTRY32W {
    DWORD dwSize;
    DWORD th32ModuleID;
    DWORD th32ProcessID;
    DWORD GlblcntUsage;
    DWORD ProccntUsage;
    BYTE *modBaseAddr;
    DWORD modBaseSize;
    HMODULE hModule;
    WCHAR szModule[MAX_MODULE_NAME32+1];
    WCHAR szExePath[MAX_PATH];
} MODULEENTRY32W, *PMODULEENTRY32W, *LPMODULEENTRY32W;
#endif /* _WINCE */

typedef struct tagMODULEENTRY32 {
    DWORD dwSize;
    DWORD th32ModuleID;
    DWORD th32ProcessID;
    DWORD GlblcntUsage;
    DWORD ProccntUsage;
    BYTE *modBaseAddr;
    DWORD modBaseSize;
    HMODULE hModule;
#ifdef _WINCE
    TCHAR szModule[MAX_PATH];
    TCHAR szExePath[MAX_PATH];
    DWORD dwFlags;
#else
    char szModule[MAX_MODULE_NAME32+1];
    char szExePath[MAX_PATH];
#endif
} MODULEENTRY32, *PMODULEENTRY32, *LPMODULEENTRY32;

HANDLE WINAPI CreateToolhelp32Snapshot(DWORD,DWORD);
BOOL WINAPI Heap32ListFirst(HANDLE,LPHEAPLIST32);
BOOL WINAPI Heap32ListNext(HANDLE,LPHEAPLIST32);
BOOL WINAPI Heap32First(LPHEAPENTRY32,DWORD,ULONG_PTR);
BOOL WINAPI Heap32Next(LPHEAPENTRY32);
BOOL WINAPI Toolhelp32ReadProcessMemory(DWORD,LPCVOID,LPVOID,DWORD,LPDWORD);
BOOL WINAPI Process32First(HANDLE,LPPROCESSENTRY32);
BOOL WINAPI Process32Next(HANDLE,LPPROCESSENTRY32);
BOOL WINAPI Thread32First(HANDLE,LPTHREADENTRY32);
BOOL WINAPI Thread32Next(HANDLE,LPTHREADENTRY32);
BOOL WINAPI Module32First(HANDLE,LPMODULEENTRY32);
BOOL WINAPI Module32Next(HANDLE,LPMODULEENTRY32);

#ifdef _WINCE
BOOL WINAPI CloseToolhelp32Snapshot(HANDLE);
#else /* _WINCE */
BOOL WINAPI Process32FirstW(HANDLE,LPPROCESSENTRY32W);
BOOL WINAPI Process32NextW(HANDLE,LPPROCESSENTRY32W);
BOOL WINAPI Module32FirstW(HANDLE,LPMODULEENTRY32W);
BOOL WINAPI Module32NextW(HANDLE,LPMODULEENTRY32W);

#ifdef UNICODE
#define Process32First Process32FirstW
#define Process32Next Process32NextW
#define PROCESSENTRY32 PROCESSENTRY32W
#define PPROCESSENTRY32 PPROCESSENTRY32W
#define LPPROCESSENTRY32 LPPROCESSENTRY32W
#define Module32First Module32FirstW
#define Module32Next Module32NextW
#define MODULEENTRY32 MODULEENTRY32W
#define PMODULEENTRY32 PMODULEENTRY32W
#define LPMODULEENTRY32 LPMODULEENTRY32W
#endif /* UNICODE */
#endif /* _WINCE */

#ifdef __cplusplus
}
#endif

#endif /* _TLHELP32_H */
