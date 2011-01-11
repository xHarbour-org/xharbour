#ifndef _PSAPI_H
#define _PSAPI_H

/* Windows PSAPI.DLL definitions */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _MODULEINFO {
    LPVOID lpBaseOfDll;
    DWORD SizeOfImage;
    LPVOID EntryPoint;
} MODULEINFO, *LPMODULEINFO;

typedef struct _PSAPI_WS_WATCH_INFORMATION {
    LPVOID FaultingPc;
    LPVOID FaultingVa;
} PSAPI_WS_WATCH_INFORMATION, *PPSAPI_WS_WATCH_INFORMATION;

typedef struct _PROCESS_MEMORY_COUNTERS {
    DWORD cb;
    DWORD PageFaultCount;
    SIZE_T PeakWorkingSetSize;
    SIZE_T WorkingSetSize;
    SIZE_T QuotaPeakPagedPoolUsage;
    SIZE_T QuotaPagedPoolUsage;
    SIZE_T QuotaPeakNonPagedPoolUsage;
    SIZE_T QuotaNonPagedPoolUsage;
    SIZE_T PagefileUsage;
    SIZE_T PeakPagefileUsage;
} PROCESS_MEMORY_COUNTERS;
typedef PROCESS_MEMORY_COUNTERS *PPROCESS_MEMORY_COUNTERS;

#if _WIN32_WINNT >= 0x0501
typedef struct _PROCESS_MEMORY_COUNTERS_EX {
    DWORD cb;
    DWORD PageFaultCount;
    SIZE_T PeakWorkingSetSize;
    SIZE_T WorkingSetSize;
    SIZE_T QuotaPeakPagedPoolUsage;
    SIZE_T QuotaPagedPoolUsage;
    SIZE_T QuotaPeakNonPagedPoolUsage;
    SIZE_T QuotaNonPagedPoolUsage;
    SIZE_T PagefileUsage;
    SIZE_T PeakPagefileUsage;
    SIZE_T PrivateUsage;
} PROCESS_MEMORY_COUNTERS_EX;
typedef PROCESS_MEMORY_COUNTERS_EX *PPROCESS_MEMORY_COUNTERS_EX;
#endif

typedef struct _PERFORMANCE_INFORMATION {
    DWORD cb;
    SIZE_T CommitTotal;
    SIZE_T CommitLimit;
    SIZE_T CommitPeak;
    SIZE_T PhysicalTotal;
    SIZE_T PhysicalAvailable;
    SIZE_T SystemCache;
    SIZE_T KernelTotal;
    SIZE_T KernelPaged;
    SIZE_T KernelNonpaged;
    SIZE_T PageSize;
    DWORD HandleCount;
    DWORD ProcessCount;
    DWORD ThreadCount;
} PERFORMANCE_INFORMATION, *PPERFORMANCE_INFORMATION, PERFORMACE_INFORMATION, *PPERFORMACE_INFORMATION;

typedef struct _ENUM_PAGE_FILE_INFORMATION {
    DWORD cb;
    DWORD Reserved;
    SIZE_T TotalSize;
    SIZE_T TotalInUse;
    SIZE_T PeakUsage;
} ENUM_PAGE_FILE_INFORMATION, *PENUM_PAGE_FILE_INFORMATION;

typedef BOOL (*PENUM_PAGE_FILE_CALLBACKW)(LPVOID,PENUM_PAGE_FILE_INFORMATION,LPCWSTR);
typedef BOOL (*PENUM_PAGE_FILE_CALLBACKA)(LPVOID,PENUM_PAGE_FILE_INFORMATION,LPCSTR);

BOOL WINAPI EnumProcesses(DWORD*,DWORD,DWORD*);
BOOL WINAPI EnumProcessModules(HANDLE,HMODULE*,DWORD,LPDWORD);
DWORD WINAPI GetModuleBaseNameA(HANDLE,HMODULE,LPSTR,DWORD);
DWORD WINAPI GetModuleBaseNameW(HANDLE,HMODULE,LPWSTR,DWORD);
DWORD WINAPI GetModuleFileNameExA(HANDLE,HMODULE,LPSTR,DWORD);
DWORD WINAPI GetModuleFileNameExW(HANDLE,HMODULE,LPWSTR,DWORD);
BOOL WINAPI GetModuleInformation(HANDLE,HMODULE,LPMODULEINFO,DWORD);
BOOL WINAPI EmptyWorkingSet(HANDLE);
BOOL WINAPI QueryWorkingSet(HANDLE,PVOID,DWORD);
BOOL WINAPI InitializeProcessForWsWatch(HANDLE);
BOOL WINAPI GetWsChanges(HANDLE,PPSAPI_WS_WATCH_INFORMATION,DWORD);
DWORD WINAPI GetMappedFileNameW(HANDLE,LPVOID,LPWSTR,DWORD);
DWORD WINAPI GetMappedFileNameA(HANDLE,LPVOID,LPSTR,DWORD);
BOOL WINAPI EnumDeviceDrivers(LPVOID*,DWORD,LPDWORD);
DWORD WINAPI GetDeviceDriverBaseNameA(LPVOID,LPSTR,DWORD);
DWORD WINAPI GetDeviceDriverBaseNameW(LPVOID,LPWSTR,DWORD);
DWORD WINAPI GetDeviceDriverFileNameA(LPVOID,LPSTR,DWORD);
DWORD WINAPI GetDeviceDriverFileNameW(LPVOID,LPWSTR,DWORD);
BOOL WINAPI GetProcessMemoryInfo(HANDLE,PPROCESS_MEMORY_COUNTERS,DWORD);
BOOL WINAPI GetPerformanceInfo(PPERFORMACE_INFORMATION,DWORD);
BOOL WINAPI EnumPageFilesW(PENUM_PAGE_FILE_CALLBACKW,LPVOID);
BOOL WINAPI EnumPageFilesA(PENUM_PAGE_FILE_CALLBACKA,LPVOID);
DWORD WINAPI GetProcessImageFileNameA(HANDLE,LPSTR,DWORD);
DWORD WINAPI GetProcessImageFileNameW(HANDLE,LPWSTR,DWORD);

#ifdef UNICODE
#define GetModuleBaseName GetModuleBaseNameW
#define GetModuleFileNameEx GetModuleFileNameExW
#define GetMappedFileName GetMappedFileNameW
#define GetDeviceDriverBaseName GetDeviceDriverBaseNameW
#define GetDeviceDriverFileName GetDeviceDriverFileNameW
#define PENUM_PAGE_FILE_CALLBACK PENUM_PAGE_FILE_CALLBACKW
#define EnumPageFiles EnumPageFilesW
#define GetProcessImageFileName  GetProcessImageFileNameW
#else
#define GetModuleBaseName GetModuleBaseNameA
#define GetModuleFileNameEx GetModuleFileNameExA
#define GetMappedFileName GetMappedFileNameA
#define GetDeviceDriverBaseName GetDeviceDriverBaseNameA
#define GetDeviceDriverFileName GetDeviceDriverFileNameA
#define PENUM_PAGE_FILE_CALLBACK PENUM_PAGE_FILE_CALLBACKA
#define EnumPageFiles EnumPageFilesA
#define GetProcessImageFileName  GetProcessImageFileNameA
#endif

#ifdef __cplusplus
}
#endif

#endif /* _PSAPI_H */
