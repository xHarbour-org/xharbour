#ifndef _CEUTIL_H
#define _CEUTIL_H

/* Service helper definitions (Windows CE) */

#ifdef __cplusplus
extern "C" {
#endif

typedef HKEY HCESVC;
typedef PHKEY PHCESVC;
typedef DWORD DEVICEID;

#define DEVICE_GUEST  (DEVICEID)-1
#define DEVICE_INVALID  (DEVICEID)0

#define SVC_FLAG_GUEST  0x0001
#define SVC_FLAG_CURRENT_PROFILE  0x0002
#define SVC_FLAG_PROFILE  0x0004
#define SVC_FLAG_ALL_PROFILES  0x0008
#define SVC_FLAG_COMMON  0x0010
#define SVC_FLAG_ALL  0x001F

enum {
    CESVC_ROOT_COMMON=0,
    CESVC_ROOT_MACHINE = CESVC_ROOT_COMMON,
    CESVC_ROOT_USER,
    CESVC_DEVICES,
    CESVC_DEVICEX,
    CESVC_DEVICE,
    CESVC_DEVICE_SELECTED,
    CESVC_SERVICES,
    CESVC_SERVICES_USER = CESVC_SERVICES,
    CESVC_SERVICES_COMMON,
    CESVC_SYNC,
    CESVC_SYNC_COMMON,
    CESVC_FILTERS,
    CESVC_SPECIAL_DEFAULTS,
    CESVC_CUSTOM_MENUS,
    CESVC_SYNCX
};

typedef struct {
    DWORD cbSize;
    DWORD Flags;
    DWORD ProfileId;
    BOOL Enabled;
} SVCINFO_GENERIC;

typedef struct {
    DWORD cbSize;
    DWORD Flags;
    DWORD ProfileId;
    BOOL Enabled;
    LPTSTR DisplayName;
    LPTSTR ProgId;
} SVCINFO_SYNC;

HRESULT __stdcall CeSvcAdd(LPTSTR,LPTSTR,LPVOID);
HRESULT __stdcall CeSvcRemove(LPTSTR,LPTSTR,DWORD);
HRESULT __stdcall CeSvcQueryInfo(LPTSTR,LPTSTR,LPVOID,DWORD);

HRESULT __stdcall CeSvcOpen(UINT,LPTSTR,BOOL,PHCESVC);
HRESULT __stdcall CeSvcOpenEx(HCESVC,LPTSTR,BOOL,PHCESVC);
HRESULT __stdcall CeSvcClose(HCESVC);
HRESULT __stdcall CeSvcDelete(HCESVC);

HRESULT __stdcall CeSvcGetString(HCESVC,LPCTSTR,LPTSTR,DWORD);
HRESULT __stdcall CeSvcSetString(HCESVC,LPCTSTR,LPCTSTR);
HRESULT __stdcall CeSvcGetDword(HCESVC,LPCTSTR,LPDWORD);
HRESULT __stdcall CeSvcSetDword(HCESVC,LPCTSTR,DWORD);
HRESULT __stdcall CeSvcGetBinary(HCESVC,LPCTSTR,LPBYTE,LPDWORD);
HRESULT __stdcall CeSvcSetBinary(HCESVC,LPCTSTR,LPBYTE,DWORD);
HRESULT __stdcall CeSvcDeleteVal(HCESVC,LPCTSTR);

DEVICEID __stdcall CeGetDeviceId(void);
DEVICEID __stdcall CeGetSelectedDeviceId(void);

HRESULT __stdcall CeSvcEnumProfiles(PHCESVC,DWORD,PDWORD);

#ifdef __cplusplus
}
#endif

#endif /* _CEUTIL_H */
