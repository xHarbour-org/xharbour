#ifndef _DPLOBBY_H
#define _DPLOBBY_H

/* DirectPlayLobby definitions */

#include "dplay.h"

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2198)  /* Nameless field is not standard */
#endif

DEFINE_GUID(IID_IDirectPlayLobby,0xaf465c71,0x9588,0x11cf,0xa0,0x20,0x0,0xaa,0x0,0x61,0x57,0xac);
DEFINE_GUID(IID_IDirectPlayLobbyA,0x26c66a70,0xb367,0x11cf,0xa0,0x24,0x0,0xaa,0x0,0x61,0x57,0xac);
DEFINE_GUID(IID_IDirectPlayLobby2,0x194c220,0xa303,0x11d0,0x9c,0x4f,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(IID_IDirectPlayLobby2A,0x1bb4af80,0xa303,0x11d0,0x9c,0x4f,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(IID_IDirectPlayLobby3,0x2db72490,0x652c,0x11d1,0xa7,0xa8,0x0,0x0,0xf8,0x3,0xab,0xfc);
DEFINE_GUID(IID_IDirectPlayLobby3A,0x2db72491,0x652c,0x11d1,0xa7,0xa8,0x0,0x0,0xf8,0x3,0xab,0xfc);
DEFINE_GUID(CLSID_DirectPlayLobby,0x2fe8f810,0xb2a5,0x11d0,0xa7,0x87,0x0,0x0,0xf8,0x3,0xab,0xfc);

typedef struct IDirectPlayLobby *LPDIRECTPLAYLOBBY;
typedef struct IDirectPlayLobby *LPDIRECTPLAYLOBBYA;
typedef struct IDirectPlayLobby IDirectPlayLobbyA;
typedef struct IDirectPlayLobby2 *LPDIRECTPLAYLOBBY2;
typedef struct IDirectPlayLobby2 *LPDIRECTPLAYLOBBY2A;
typedef struct IDirectPlayLobby2 IDirectPlayLobby2A;
typedef struct IDirectPlayLobby3 *LPDIRECTPLAYLOBBY3;
typedef struct IDirectPlayLobby3 *LPDIRECTPLAYLOBBY3A;
typedef struct IDirectPlayLobby3 IDirectPlayLobby3A;

typedef struct DPLAPPINFO {
    DWORD dwSize;
    GUID guidApplication;
    union {
        LPSTR lpszAppNameA;
        LPWSTR lpszAppName;
    };
} DPLAPPINFO, *LPDPLAPPINFO;
typedef const DPLAPPINFO *LPCDPLAPPINFO;

typedef struct DPCOMPOUNDADDRESSELEMENT {
    GUID guidDataType;
    DWORD dwDataSize;
    LPVOID lpData;
} DPCOMPOUNDADDRESSELEMENT, *LPDPCOMPOUNDADDRESSELEMENT;
typedef const DPCOMPOUNDADDRESSELEMENT *LPCDPCOMPOUNDADDRESSELEMENT;

typedef struct DPAPPLICATIONDESC {
    DWORD dwSize;
    DWORD dwFlags;
    union {
        LPSTR lpszApplicationNameA;
        LPWSTR lpszApplicationName;
    };
    GUID guidApplication;
    union {
        LPSTR lpszFilenameA;
        LPWSTR lpszFilename;
    };
    union {
        LPSTR lpszCommandLineA;
        LPWSTR lpszCommandLine;
    };
    union {
        LPSTR lpszPathA;
        LPWSTR lpszPath;
    };
    union {
        LPSTR lpszCurrentDirectoryA;
        LPWSTR lpszCurrentDirectory;
    };
    LPSTR lpszDescriptionA;
    LPWSTR lpszDescriptionW;
} DPAPPLICATIONDESC, *LPDPAPPLICATIONDESC;

typedef struct DPAPPLICATIONDESC2 {
    DWORD dwSize;
    DWORD dwFlags;
    union {
        LPSTR lpszApplicationNameA;
        LPWSTR lpszApplicationName;
    };
    GUID guidApplication;
    union {
        LPSTR lpszFilenameA;
        LPWSTR lpszFilename;
    };
    union {
        LPSTR lpszCommandLineA;
        LPWSTR lpszCommandLine;
    };
    union {
        LPSTR lpszPathA;
        LPWSTR lpszPath;
    };
    union {
        LPSTR lpszCurrentDirectoryA;
        LPWSTR lpszCurrentDirectory;
    };
    LPSTR lpszDescriptionA;
    LPWSTR lpszDescriptionW;
    union {
        LPSTR lpszAppLauncherNameA;
        LPWSTR lpszAppLauncherName;
    };
} DPAPPLICATIONDESC2, *LPDPAPPLICATIONDESC2;

typedef BOOL (PASCAL *LPDPENUMADDRESSCALLBACK)(REFGUID,DWORD,LPCVOID,LPVOID);
typedef BOOL (PASCAL *LPDPLENUMADDRESSTYPESCALLBACK)(REFGUID,LPVOID,DWORD);
typedef BOOL (PASCAL * LPDPLENUMLOCALAPPLICATIONSCALLBACK)(LPCDPLAPPINFO,LPVOID,DWORD);

#ifdef UNICODE
#define DirectPlayLobbyCreate  DirectPlayLobbyCreateW
#else
#define DirectPlayLobbyCreate  DirectPlayLobbyCreateA
#endif /* UNICODE */

extern HRESULT WINAPI DirectPlayLobbyCreateW(LPGUID,LPDIRECTPLAYLOBBY*,IUnknown*,LPVOID,DWORD);
extern HRESULT WINAPI DirectPlayLobbyCreateA(LPGUID,LPDIRECTPLAYLOBBYA*,IUnknown*,LPVOID,DWORD);

#undef INTERFACE
#define INTERFACE IDirectPlayLobby
DECLARE_INTERFACE_(IDirectPlayLobby,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Connect)(THIS_ DWORD,LPDIRECTPLAY2*,IUnknown*) PURE;
    STDMETHOD(CreateAddress)(THIS_ REFGUID,REFGUID,LPCVOID,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(EnumAddress)(THIS_ LPDPENUMADDRESSCALLBACK,LPCVOID,DWORD,LPVOID) PURE;
    STDMETHOD(EnumAddressTypes)(THIS_ LPDPLENUMADDRESSTYPESCALLBACK,REFGUID,LPVOID,DWORD) PURE;
    STDMETHOD(EnumLocalApplications)(THIS_ LPDPLENUMLOCALAPPLICATIONSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(GetConnectionSettings)(THIS_ DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(ReceiveLobbyMessage)(THIS_ DWORD,DWORD,LPDWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(RunApplication)(THIS_ DWORD,LPDWORD,LPDPLCONNECTION,HANDLE) PURE;
    STDMETHOD(SendLobbyMessage)(THIS_ DWORD,DWORD,LPVOID,DWORD) PURE;
    STDMETHOD(SetConnectionSettings)(THIS_ DWORD,DWORD,LPDPLCONNECTION) PURE;
    STDMETHOD(SetLobbyMessageEvent)(THIS_ DWORD,DWORD,HANDLE) PURE;

};

#undef INTERFACE
#define INTERFACE IDirectPlayLobby2
DECLARE_INTERFACE_(IDirectPlayLobby2,IDirectPlayLobby)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Connect)(THIS_ DWORD,LPDIRECTPLAY2*,IUnknown*) PURE;
    STDMETHOD(CreateAddress)(THIS_ REFGUID,REFGUID,LPCVOID,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(EnumAddress)(THIS_ LPDPENUMADDRESSCALLBACK,LPCVOID,DWORD,LPVOID) PURE;
    STDMETHOD(EnumAddressTypes)(THIS_ LPDPLENUMADDRESSTYPESCALLBACK,REFGUID,LPVOID,DWORD) PURE;
    STDMETHOD(EnumLocalApplications)(THIS_ LPDPLENUMLOCALAPPLICATIONSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(GetConnectionSettings)(THIS_ DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(ReceiveLobbyMessage)(THIS_ DWORD,DWORD,LPDWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(RunApplication)(THIS_ DWORD,LPDWORD,LPDPLCONNECTION,HANDLE) PURE;
    STDMETHOD(SendLobbyMessage)(THIS_ DWORD,DWORD,LPVOID,DWORD) PURE;
    STDMETHOD(SetConnectionSettings)(THIS_ DWORD,DWORD,LPDPLCONNECTION) PURE;
    STDMETHOD(SetLobbyMessageEvent)(THIS_ DWORD,DWORD,HANDLE) PURE;
    STDMETHOD(CreateCompoundAddress)(THIS_ LPCDPCOMPOUNDADDRESSELEMENT,DWORD,LPVOID,LPDWORD) PURE;
};

#undef INTERFACE
#define INTERFACE IDirectPlayLobby3
DECLARE_INTERFACE_(IDirectPlayLobby3,IDirectPlayLobby)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Connect)(THIS_ DWORD,LPDIRECTPLAY2*,IUnknown*) PURE;
    STDMETHOD(CreateAddress)(THIS_ REFGUID,REFGUID,LPCVOID,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(EnumAddress)(THIS_ LPDPENUMADDRESSCALLBACK,LPCVOID,DWORD,LPVOID) PURE;
    STDMETHOD(EnumAddressTypes)(THIS_ LPDPLENUMADDRESSTYPESCALLBACK,REFGUID,LPVOID,DWORD) PURE;
    STDMETHOD(EnumLocalApplications)(THIS_ LPDPLENUMLOCALAPPLICATIONSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(GetConnectionSettings)(THIS_ DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(ReceiveLobbyMessage)(THIS_ DWORD,DWORD,LPDWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(RunApplication)(THIS_ DWORD,LPDWORD,LPDPLCONNECTION,HANDLE) PURE;
    STDMETHOD(SendLobbyMessage)(THIS_ DWORD,DWORD,LPVOID,DWORD) PURE;
    STDMETHOD(SetConnectionSettings)(THIS_ DWORD,DWORD,LPDPLCONNECTION) PURE;
    STDMETHOD(SetLobbyMessageEvent)(THIS_ DWORD,DWORD,HANDLE) PURE;
    STDMETHOD(CreateCompoundAddress)(THIS_ LPCDPCOMPOUNDADDRESSELEMENT,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(ConnectEx)(THIS_ DWORD,REFIID,LPVOID *,IUnknown *) PURE;
    STDMETHOD(RegisterApplication)(THIS_ DWORD,LPVOID) PURE;
    STDMETHOD(UnregisterApplication)(THIS_ DWORD,REFGUID) PURE;
    STDMETHOD(WaitForConnectionSettings)(THIS_ DWORD) PURE;
};

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirectPlayLobby_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectPlayLobby_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirectPlayLobby_Release(p)  (p)->lpVtbl->Release(p)
#define IDirectPlayLobby_Connect(p,a,b,c)  (p)->lpVtbl->Connect(p,a,b,c)
#define IDirectPlayLobby_ConnectEx(p,a,b,c,d)  (p)->lpVtbl->ConnectEx(p,a,b,c,d)
#define IDirectPlayLobby_CreateAddress(p,a,b,c,d,e,f)  (p)->lpVtbl->CreateAddress(p,a,b,c,d,e,f)
#define IDirectPlayLobby_CreateCompoundAddress(p,a,b,c,d)  (p)->lpVtbl->CreateCompoundAddress(p,a,b,c,d)
#define IDirectPlayLobby_EnumAddress(p,a,b,c,d)  (p)->lpVtbl->EnumAddress(p,a,b,c,d)
#define IDirectPlayLobby_EnumAddressTypes(p,a,b,c,d)  (p)->lpVtbl->EnumAddressTypes(p,a,b,c,d)
#define IDirectPlayLobby_EnumLocalApplications(p,a,b,c)  (p)->lpVtbl->EnumLocalApplications(p,a,b,c)
#define IDirectPlayLobby_GetConnectionSettings(p,a,b,c)  (p)->lpVtbl->GetConnectionSettings(p,a,b,c)
#define IDirectPlayLobby_ReceiveLobbyMessage(p,a,b,c,d,e)  (p)->lpVtbl->ReceiveLobbyMessage(p,a,b,c,d,e)
#define IDirectPlayLobby_RegisterApplication(p,a,b)  (p)->lpVtbl->RegisterApplication(p,a,b)
#define IDirectPlayLobby_RunApplication(p,a,b,c,d)  (p)->lpVtbl->RunApplication(p,a,b,c,d)
#define IDirectPlayLobby_SendLobbyMessage(p,a,b,c,d)  (p)->lpVtbl->SendLobbyMessage(p,a,b,c,d)
#define IDirectPlayLobby_SetConnectionSettings(p,a,b,c)  (p)->lpVtbl->SetConnectionSettings(p,a,b,c)
#define IDirectPlayLobby_SetLobbyMessageEvent(p,a,b,c)  (p)->lpVtbl->SetLobbyMessageEvent(p,a,b,c)
#define IDirectPlayLobby_UnregisterApplication(p,a,b)  (p)->lpVtbl->UnregisterApplication(p,a,b)
#define IDirectPlayLobby_WaitForConnectionSettings(p,a)  (p)->lpVtbl->WaitForConnectionSettings(p,a)
#else /* C++ */
#define IDirectPlayLobby_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirectPlayLobby_AddRef(p)  (p)->AddRef()
#define IDirectPlayLobby_Release(p)  (p)->Release()
#define IDirectPlayLobby_Connect(p,a,b,c)  (p)->Connect(a,b,c)
#define IDirectPlayLobby_ConnectEx(p,a,b,c,d)  (p)->ConnectEx(a,b,c,d)
#define IDirectPlayLobby_CreateAddress(p,a,b,c,d,e,f)  (p)->CreateAddress(a,b,c,d,e,f)
#define IDirectPlayLobby_CreateCompoundAddress(p,a,b,c,d)  (p)->CreateCompoundAddress(a,b,c,d)
#define IDirectPlayLobby_EnumAddress(p,a,b,c,d)  (p)->EnumAddress(a,b,c,d)
#define IDirectPlayLobby_EnumAddressTypes(p,a,b,c,d)  (p)->EnumAddressTypes(a,b,c,d)
#define IDirectPlayLobby_EnumLocalApplications(p,a,b,c)  (p)->EnumLocalApplications(a,b,c)
#define IDirectPlayLobby_GetConnectionSettings(p,a,b,c)  (p)->GetConnectionSettings(a,b,c)
#define IDirectPlayLobby_ReceiveLobbyMessage(p,a,b,c,d,e)  (p)->ReceiveLobbyMessage(a,b,c,d,e)
#define IDirectPlayLobby_RegisterApplication(p,a,b)  (p)->RegisterApplication(a,b)
#define IDirectPlayLobby_RunApplication(p,a,b,c,d)  (p)->RunApplication(a,b,c,d)
#define IDirectPlayLobby_SendLobbyMessage(p,a,b,c,d)  (p)->SendLobbyMessage(a,b,c,d)
#define IDirectPlayLobby_SetConnectionSettings(p,a,b,c)  (p)->SetConnectionSettings(a,b,c)
#define IDirectPlayLobby_SetLobbyMessageEvent(p,a,b,c)  (p)->SetLobbyMessageEvent(a,b,c)
#define IDirectPlayLobby_UnregisterApplication(p,a,b)  (p)->UnregisterApplication(a,b)
#define IDirectPlayLobby_WaitForConnectionSettings(p,a)  (p)->WaitForConnectionSettings(a)
#endif /* C++ */

#define DPLWAIT_CANCEL  0x00000001

#define DPLMSG_SYSTEM  0x00000001
#define DPLMSG_STANDARD  0x00000002

#define DPLAPP_NOENUM  0x80000000
#define DPLAPP_AUTOVOICE  0x00000001 
#define DPLAPP_SELFVOICE  0x00000002 

typedef struct _DPLMSG_GENERIC {
    DWORD dwType;
} DPLMSG_GENERIC, *LPDPLMSG_GENERIC;

typedef struct _DPLMSG_SYSTEMMESSAGE {
    DWORD dwType;
    GUID guidInstance;
} DPLMSG_SYSTEMMESSAGE, *LPDPLMSG_SYSTEMMESSAGE;

typedef struct _DPLMSG_SETPROPERTY {
    DWORD dwType;
    DWORD dwRequestID;
    GUID guidPlayer;
    GUID guidPropertyTag;
    DWORD dwDataSize;
    DWORD dwPropertyData[1];
} DPLMSG_SETPROPERTY, *LPDPLMSG_SETPROPERTY;

#define DPL_NOCONFIRMATION  0

typedef struct _DPLMSG_SETPROPERTYRESPONSE {
    DWORD dwType;
    DWORD dwRequestID;
    GUID guidPlayer;
    GUID guidPropertyTag;
    HRESULT hr;
} DPLMSG_SETPROPERTYRESPONSE, *LPDPLMSG_SETPROPERTYRESPONSE;

typedef struct _DPLMSG_GETPROPERTY {
    DWORD dwType;
    DWORD dwRequestID;
    GUID guidPlayer;
    GUID guidPropertyTag;
} DPLMSG_GETPROPERTY, *LPDPLMSG_GETPROPERTY;

typedef struct _DPLMSG_GETPROPERTYRESPONSE {
    DWORD dwType;
    DWORD dwRequestID;
    GUID guidPlayer;
    GUID guidPropertyTag;
    HRESULT hr;
    DWORD dwDataSize;
    DWORD dwPropertyData[1];
} DPLMSG_GETPROPERTYRESPONSE, *LPDPLMSG_GETPROPERTYRESPONSE;

typedef struct _DPLMSG_NEWSESSIONHOST {
    DWORD dwType;
    GUID guidInstance;
} DPLMSG_NEWSESSIONHOST, *LPDPLMSG_NEWSESSIONHOST;

#define DPLSYS_CONNECTIONSETTINGSREAD  0x00000001
#define DPLSYS_DPLAYCONNECTFAILED  0x00000002
#define DPLSYS_DPLAYCONNECTSUCCEEDED  0x00000003
#define DPLSYS_APPTERMINATED  0x00000004
#define DPLSYS_SETPROPERTY  0x00000005
#define DPLSYS_SETPROPERTYRESPONSE  0x00000006
#define DPLSYS_GETPROPERTY  0x00000007
#define DPLSYS_GETPROPERTYRESPONSE  0x00000008
#define DPLSYS_NEWSESSIONHOST  0x00000009
#define DPLSYS_NEWCONNECTIONSETTINGS  0x0000000A
#define DPLSYS_LOBBYCLIENTRELEASE  0x0000000B

DEFINE_GUID(DPLPROPERTY_MessagesSupported,0x762ccda1,0xd916,0x11d0,0xba,0x39,0x0,0xc0,0x4f,0xd7,0xed,0x67);
DEFINE_GUID(DPLPROPERTY_LobbyGuid,0xf56920a0,0xd218,0x11d0,0xba,0x39,0x0,0xc0,0x4f,0xd7,0xed,0x67);
DEFINE_GUID(DPLPROPERTY_PlayerGuid,0xb4319322,0xd20d,0x11d0,0xba,0x39,0x0,0xc0,0x4f,0xd7,0xed,0x67);

typedef struct _DPLDATA_PLAYERGUID {
    GUID guidPlayer;
    DWORD dwPlayerFlags;
} DPLDATA_PLAYERGUID, *LPDPLDATA_PLAYERGUID;

DEFINE_GUID(DPLPROPERTY_PlayerScore,0x48784000,0xd219,0x11d0,0xba,0x39,0x0,0xc0,0x4f,0xd7,0xed,0x67);

typedef struct _DPLDATA_PLAYERSCORE {
    DWORD dwScoreCount;
    LONG Score[1];
} DPLDATA_PLAYERSCORE, *LPDPLDATA_PLAYERSCORE;

typedef struct _DPADDRESS {
    GUID guidDataType;
    DWORD dwDataSize;
} DPADDRESS, *LPDPADDRESS;

DEFINE_GUID(DPAID_TotalSize,0x1318f560,0x912c,0x11d0,0x9d,0xaa,0x0,0xa0,0xc9,0xa,0x43,0xcb);
DEFINE_GUID(DPAID_ServiceProvider,0x7d916c0,0xe0af,0x11cf,0x9c,0x4e,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(DPAID_LobbyProvider,0x59b95640,0x9667,0x11d0,0xa7,0x7d,0x0,0x0,0xf8,0x3,0xab,0xfc);
DEFINE_GUID(DPAID_Phone,0x78ec89a0,0xe0af,0x11cf,0x9c,0x4e,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(DPAID_PhoneW,0xba5a7a70,0x9dbf,0x11d0,0x9c,0xc1,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(DPAID_Modem,0xf6dcc200,0xa2fe,0x11d0,0x9c,0x4f,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(DPAID_ModemW,0x1fd92e0,0xa2ff,0x11d0,0x9c,0x4f,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(DPAID_INet,0xc4a54da0,0xe0af,0x11cf,0x9c,0x4e,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(DPAID_INetW,0xe63232a0,0x9dbf,0x11d0,0x9c,0xc1,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(DPAID_INetPort,0xe4524541,0x8ea5,0x11d1,0x8a,0x96,0x0,0x60,0x97,0xb0,0x14,0x11);

#define DPCPA_NOFLOW  0
#define DPCPA_XONXOFFFLOW  1
#define DPCPA_RTSFLOW  2
#define DPCPA_DTRFLOW  3
#define DPCPA_RTSDTRFLOW  4

typedef struct _DPCOMPORTADDRESS {
    DWORD dwComPort;
    DWORD dwBaudRate;
    DWORD dwStopBits;
    DWORD dwParity;
    DWORD dwFlowControl;
} DPCOMPORTADDRESS, *LPDPCOMPORTADDRESS;

DEFINE_GUID(DPAID_ComPort,0xf2f0ce00,0xe0af,0x11cf,0x9c,0x4e,0x0,0xa0,0xc9,0x5,0x42,0x5e);

#define DPLAD_SYSTEM  DPLMSG_SYSTEM

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
};
#endif

#endif /* _DPLOBBY_H */

