#ifndef _RESAPI_DEFS_H
#define _RESAPI_DEFS_H

/* Windows Clusters resources definitions */

#include <windows.h>
#include <winsvc.h>
#include <clusapi.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2198)  /* Nameless field is not standard */
#endif

#define STARTUP_ROUTINE "Startup"

#define CLRES_V1_FUNCTION_SIZE  sizeof(CLRES_V1_FUNCTIONS)
#define CLRES_VERSION_V1_00  0x100

#define CLRES_V1_FUNCTION_TABLE(_Name,_Version,_Prefix,_Arbitrate,_Release,_ResControl,_ResTypeControl)  \
    CLRES_FUNCTION_TABLE _Name = { \
    CLRES_V1_FUNCTION_SIZE, \
    _Version, \
    _Prefix##Open, \
    _Prefix##Close, \
    _Prefix##Online, \
    _Prefix##Offline, \
    _Prefix##Terminate, \
    _Prefix##LooksAlive, \
    _Prefix##IsAlive, \
    _Arbitrate, \
    _Release, \
    _ResControl, \
    _ResTypeControl }

#endif /* _RESAPI_DEFS_H */

#ifndef _RESAPI_H
#define _RESAPI_H

#define ResUtilInitializeResourceStatus(_resource_status_)  ZeroMemory(_resource_status_,sizeof(RESOURCE_STATUS))

#ifndef FIELD_OFFSET
#define FIELD_OFFSET(type, field)  ((LONG)&(((type *)0)->field))
#endif

#define RESUTIL_PROPITEM_READ_ONLY  0x00000001
#define RESUTIL_PROPITEM_REQUIRED  0x00000002
#define RESUTIL_PROPITEM_SIGNED  0x00000004

typedef PVOID RESID;
typedef HANDLE RESOURCE_HANDLE;

typedef struct RESOURCE_STATUS {
    CLUSTER_RESOURCE_STATE ResourceState;
    DWORD CheckPoint;
    DWORD WaitHint;
    HANDLE EventHandle;
} RESOURCE_STATUS, *PRESOURCE_STATUS;

typedef DWORD (__stdcall *PSET_RESOURCE_STATUS_ROUTINE)(RESOURCE_HANDLE,PRESOURCE_STATUS);
typedef VOID (__stdcall *PQUORUM_RESOURCE_LOST)(RESOURCE_HANDLE);

typedef enum LOG_LEVEL {
    LOG_INFORMATION,
    LOG_WARNING,
    LOG_ERROR,
    LOG_SEVERE
} LOG_LEVEL, *PLOG_LEVEL;

typedef VOID (__cdecl *PLOG_EVENT_ROUTINE)(RESOURCE_HANDLE,LOG_LEVEL,LPCWSTR,...);
typedef RESID (__stdcall *POPEN_ROUTINE)(LPCWSTR,HKEY,RESOURCE_HANDLE);
typedef VOID (__stdcall *PCLOSE_ROUTINE)(RESID);
typedef DWORD (__stdcall *PONLINE_ROUTINE)(RESID,LPHANDLE);
typedef DWORD (__stdcall *POFFLINE_ROUTINE)(RESID);
typedef VOID (__stdcall *PTERMINATE_ROUTINE)(RESID);
typedef BOOL (__stdcall *PIS_ALIVE_ROUTINE)(RESID);
typedef BOOL (__stdcall *PLOOKS_ALIVE_ROUTINE)(RESID);
typedef DWORD (__stdcall *PARBITRATE_ROUTINE)(RESID,PQUORUM_RESOURCE_LOST);
typedef DWORD (__stdcall *PRELEASE_ROUTINE)(RESID);
typedef DWORD (__stdcall *PRESOURCE_CONTROL_ROUTINE)(RESID,DWORD,PVOID,DWORD,PVOID,DWORD,LPDWORD);
typedef DWORD (__stdcall *PRESOURCE_TYPE_CONTROL_ROUTINE)(LPCWSTR,DWORD,PVOID,DWORD,PVOID,DWORD,LPDWORD);

typedef enum _RESOURCE_EXIT_STATE {
    ResourceExitStateContinue,
    ResourceExitStateTerminate,
    ResourceExitStateMax
} RESOURCE_EXIT_STATE;

typedef struct CLRES_V1_FUNCTIONS {
    POPEN_ROUTINE Open;
    PCLOSE_ROUTINE Close;
    PONLINE_ROUTINE Online;
    POFFLINE_ROUTINE Offline;
    PTERMINATE_ROUTINE Terminate;
    PLOOKS_ALIVE_ROUTINE LooksAlive;
    PIS_ALIVE_ROUTINE IsAlive;
    PARBITRATE_ROUTINE Arbitrate;
    PRELEASE_ROUTINE Release;
    PRESOURCE_CONTROL_ROUTINE ResourceControl;
    PRESOURCE_TYPE_CONTROL_ROUTINE ResourceTypeControl;
} CLRES_V1_FUNCTIONS, *PCLRES_V1_FUNCTIONS;

typedef struct CLRES_FUNCTION_TABLE {
    DWORD TableSize;
    DWORD Version;
    union {
        CLRES_V1_FUNCTIONS V1Functions;
    };
} CLRES_FUNCTION_TABLE, *PCLRES_FUNCTION_TABLE;

typedef struct RESUTIL_PROPERTY_ITEM {
    LPWSTR Name;
    LPWSTR KeyName;
    DWORD Format;
    union {
        DWORD_PTR DefaultPtr;
        DWORD Default;
        LPVOID lpDefault;
    };
    DWORD Minimum;
    DWORD Maximum;
    DWORD Flags;
    DWORD Offset;
} RESUTIL_PROPERTY_ITEM, *PRESUTIL_PROPERTY_ITEM;

typedef DWORD (__stdcall *PSTARTUP_ROUTINE)(LPCWSTR,DWORD,DWORD,PSET_RESOURCE_STATUS_ROUTINE,PLOG_EVENT_ROUTINE,PCLRES_FUNCTION_TABLE*);

typedef enum RESOURCE_MONITOR_STATE {
    RmonInitializing,
    RmonIdle,
    RmonStartingResource,
    RmonInitializingResource,
    RmonOnlineResource,
    RmonOfflineResource,
    RmonShutdownResource,
    RmonDeletingResource,
    RmonIsAlivePoll,
    RmonLooksAlivePoll,
    RmonArbitrateResource,
    RmonReleaseResource,
    RmonResourceControl,
    RmonResourceTypeControl
} RESOURCE_MONITOR_STATE;

typedef struct MONITOR_STATE {
    LARGE_INTEGER LastUpdate;
    RESOURCE_MONITOR_STATE State;
    HANDLE ActiveResource;
    BOOL ResmonStop;
} MONITOR_STATE, *PMONITOR_STATE;

typedef struct CLUS_WORKER {
    HANDLE hThread;
    BOOL Terminate;
} CLUS_WORKER, *PCLUS_WORKER;

typedef DWORD(WINAPI *PWORKER_START_ROUTINE)(PCLUS_WORKER,LPVOID);
typedef DWORD (*LPRESOURCE_CALLBACK)(HRESOURCE,HRESOURCE,PVOID);

DWORD WINAPI ResUtilStartResourceService(LPCWSTR,LPSC_HANDLE);
DWORD WINAPI ResUtilVerifyResourceService(LPCWSTR);
DWORD WINAPI ResUtilStopResourceService(LPCWSTR);
DWORD WINAPI ResUtilVerifyService(SC_HANDLE);
DWORD WINAPI ResUtilStopService(SC_HANDLE);
DWORD WINAPI ResUtilCreateDirectoryTree(LPCWSTR);
BOOL WINAPI ResUtilIsPathValid(LPCWSTR);
DWORD WINAPI ResUtilEnumProperties(const PRESUTIL_PROPERTY_ITEM,LPWSTR,DWORD,LPDWORD,LPDWORD);
DWORD WINAPI ResUtilEnumPrivateProperties(HKEY,LPWSTR,DWORD,LPDWORD,LPDWORD);
DWORD WINAPI ResUtilGetProperties(HKEY,const PRESUTIL_PROPERTY_ITEM,PVOID,DWORD,LPDWORD,LPDWORD);
DWORD WINAPI ResUtilGetAllProperties(HKEY,const PRESUTIL_PROPERTY_ITEM,PVOID,DWORD,LPDWORD,LPDWORD);
DWORD WINAPI ResUtilGetPrivateProperties(HKEY,PVOID,DWORD,LPDWORD,LPDWORD);
DWORD WINAPI ResUtilGetPropertySize(HKEY,const PRESUTIL_PROPERTY_ITEM,LPDWORD,LPDWORD);
DWORD WINAPI ResUtilGetProperty(HKEY,const PRESUTIL_PROPERTY_ITEM,PVOID*,LPDWORD);
DWORD WINAPI ResUtilVerifyPropertyTable(const PRESUTIL_PROPERTY_ITEM,PVOID,BOOL,const PVOID,DWORD,LPBYTE);
DWORD WINAPI ResUtilSetPropertyTable(HKEY,const PRESUTIL_PROPERTY_ITEM,PVOID,BOOL,const PVOID,DWORD,LPBYTE);
DWORD WINAPI ResUtilSetPropertyTableEx(HKEY,const PRESUTIL_PROPERTY_ITEM,PVOID,BOOL,const PVOID,DWORD,BOOL,LPBYTE);
DWORD WINAPI ResUtilSetPropertyParameterBlock(HKEY,const PRESUTIL_PROPERTY_ITEM,PVOID,const LPBYTE,const PVOID,DWORD,LPBYTE);
DWORD WINAPI ResUtilSetPropertyParameterBlockEx(HKEY,const PRESUTIL_PROPERTY_ITEM,PVOID,const LPBYTE,const PVOID,DWORD,BOOL,LPBYTE);
DWORD WINAPI ResUtilSetUnknownProperties(HKEY,const PRESUTIL_PROPERTY_ITEM,const PVOID,DWORD);
DWORD WINAPI ResUtilGetPropertiesToParameterBlock(HKEY,const PRESUTIL_PROPERTY_ITEM,LPBYTE,BOOL,LPWSTR*);
DWORD WINAPI ResUtilPropertyListFromParameterBlock(const PRESUTIL_PROPERTY_ITEM,PVOID,LPDWORD,const LPBYTE,LPDWORD,LPDWORD);
DWORD WINAPI ResUtilDupParameterBlock(LPBYTE,const LPBYTE,const PRESUTIL_PROPERTY_ITEM);
void WINAPI ResUtilFreeParameterBlock(LPBYTE,const LPBYTE,const PRESUTIL_PROPERTY_ITEM);
DWORD WINAPI ResUtilAddUnknownProperties(HKEY,const PRESUTIL_PROPERTY_ITEM,PVOID,DWORD,LPDWORD,LPDWORD);
DWORD WINAPI ResUtilSetPrivatePropertyList(HKEY,const PVOID,DWORD);
DWORD WINAPI ResUtilVerifyPrivatePropertyList(const PVOID,DWORD);
PWSTR WINAPI ResUtilDupString(LPCWSTR);
DWORD WINAPI ResUtilGetBinaryValue(HKEY,LPCWSTR,LPBYTE*,DWORD*);
LPWSTR WINAPI ResUtilGetSzValue(HKEY,LPCWSTR);
LPWSTR WINAPI ResUtilGetExpandSzValue(HKEY,LPCWSTR,BOOL);
__inline DWORD WINAPI ResUtilGetMultiSzValue(HKEY hkeyClusterKey,LPCWSTR pszValueName,LPWSTR * ppszOutValue,LPDWORD pcbOutValueSize) { return ResUtilGetBinaryValue(hkeyClusterKey,pszValueName,(LPBYTE *)ppszOutValue,pcbOutValueSize); }
DWORD WINAPI ResUtilGetDwordValue(HKEY,LPCWSTR,LPDWORD,DWORD);
DWORD WINAPI ResUtilSetBinaryValue(HKEY,LPCWSTR,const LPBYTE,DWORD,LPBYTE*,LPDWORD);
DWORD WINAPI ResUtilSetSzValue(HKEY,LPCWSTR,LPCWSTR,LPWSTR*);
DWORD WINAPI ResUtilSetExpandSzValue(HKEY,LPCWSTR,LPCWSTR,LPWSTR*);
DWORD WINAPI ResUtilSetMultiSzValue(HKEY,LPCWSTR,LPCWSTR,DWORD,LPWSTR*,LPDWORD);
DWORD WINAPI ResUtilSetDwordValue(HKEY,LPCWSTR,DWORD,LPDWORD);
DWORD WINAPI ResUtilGetBinaryProperty(LPBYTE*,LPDWORD,const PCLUSPROP_BINARY,const LPBYTE,DWORD,LPBYTE*,LPDWORD);
DWORD WINAPI ResUtilGetSzProperty(LPWSTR*,const PCLUSPROP_SZ,LPCWSTR,LPBYTE*,LPDWORD);
DWORD WINAPI ResUtilGetMultiSzProperty(LPWSTR*,LPDWORD,const PCLUSPROP_SZ,LPCWSTR,DWORD,LPBYTE*,LPDWORD);
DWORD WINAPI ResUtilGetDwordProperty(LPDWORD,const PCLUSPROP_DWORD,DWORD,DWORD,DWORD,LPBYTE*,LPDWORD);
LPVOID WINAPI ResUtilGetEnvironmentWithNetName(HRESOURCE);
DWORD WINAPI ResUtilFreeEnvironment(LPVOID);
LPWSTR WINAPI ResUtilExpandEnvironmentStrings(LPCWSTR);
DWORD WINAPI ResUtilSetResourceServiceEnvironment(LPCWSTR,HRESOURCE,PLOG_EVENT_ROUTINE,RESOURCE_HANDLE);
DWORD WINAPI ResUtilSetResourceServiceStartParameters(LPCWSTR,SC_HANDLE,LPSC_HANDLE,PLOG_EVENT_ROUTINE,RESOURCE_HANDLE);
DWORD WINAPI ResUtilFindSzProperty(const PVOID pPropertyList,DWORD,LPCWSTR,LPWSTR*);
DWORD WINAPI ResUtilFindExpandSzProperty(const PVOID,DWORD,LPCWSTR,LPWSTR*);
DWORD WINAPI ResUtilFindExpandedSzProperty(const PVOID,DWORD,LPCWSTR,LPWSTR*);
DWORD WINAPI ResUtilFindDwordProperty(const PVOID,DWORD,LPCWSTR,LPDWORD);
DWORD WINAPI ResUtilFindBinaryProperty(const PVOID,DWORD,LPCWSTR,LPBYTE*,LPDWORD);
DWORD WINAPI ResUtilFindMultiSzProperty(const PVOID,DWORD,LPCWSTR,LPWSTR*,LPDWORD);
DWORD WINAPI ResUtilFindLongProperty(const PVOID,DWORD,LPCWSTR,LPLONG);
DWORD WINAPI ClusWorkerCreate(PCLUS_WORKER,PWORKER_START_ROUTINE,PVOID);
BOOL WINAPI ClusWorkerCheckTerminate(PCLUS_WORKER);
VOID WINAPI ClusWorkerTerminate(PCLUS_WORKER);
BOOL WINAPI ResUtilResourcesEqual(HRESOURCE,HRESOURCE);
BOOL WINAPI ResUtilResourceTypesEqual(LPCWSTR,HRESOURCE);
BOOL WINAPI ResUtilIsResourceClassEqual(PCLUS_RESOURCE_CLASS_INFO,HRESOURCE);
DWORD WINAPI ResUtilEnumResources(HRESOURCE,LPCWSTR,LPRESOURCE_CALLBACK,PVOID);
HRESOURCE WINAPI ResUtilGetResourceDependency(HANDLE,LPCWSTR);
HRESOURCE WINAPI ResUtilGetResourceDependencyByName(HCLUSTER,HANDLE,LPCWSTR,BOOL);
HRESOURCE WINAPI ResUtilGetResourceDependencyByClass(HCLUSTER,HANDLE,PCLUS_RESOURCE_CLASS_INFO,BOOL);
HRESOURCE WINAPI ResUtilGetResourceNameDependency(LPCWSTR,LPCWSTR);
DWORD WINAPI ResUtilGetResourceDependentIPAddressProps(HRESOURCE,LPWSTR,DWORD*,LPWSTR,DWORD*,LPWSTR,DWORD*);
DWORD WINAPI ResUtilFindDependentDiskResourceDriveLetter(HCLUSTER,HRESOURCE,LPWSTR,DWORD*);

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
}
#endif

#endif /* _RESAPI_H */
