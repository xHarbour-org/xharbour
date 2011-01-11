#ifndef _DPLOBBY_H
#define _DPLOBBY_H

/* DirectPlay8 Lobby definitions */

#include <ole2.h>

#ifdef __cplusplus
extern "C" {
#endif

DEFINE_GUID(CLSID_DirectPlay8LobbiedApplication,0x667955ad, 0x6b3b, 0x43ca, 0xb9, 0x49, 0xbc, 0x69, 0xb5, 0xba, 0xff, 0x7f);
DEFINE_GUID(CLSID_DirectPlay8LobbyClient, 0x3b2b6775, 0x70b6, 0x45af, 0x8d, 0xea, 0xa2, 0x9, 0xc6, 0x95, 0x59, 0xf3);
DEFINE_GUID(IID_IDirectPlay8LobbiedApplication,0x819074a3, 0x16c, 0x11d3, 0xae, 0x14, 0x0, 0x60, 0x97, 0xb0, 0x14, 0x11);
DEFINE_GUID(IID_IDirectPlay8LobbyClient,0x819074a2, 0x16c, 0x11d3, 0xae, 0x14, 0x0, 0x60, 0x97, 0xb0, 0x14, 0x11);

typedef struct IDirectPlay8LobbiedApplication *PDIRECTPLAY8LOBBIEDAPPLICATION;
typedef struct IDirectPlay8LobbyClient *PDIRECTPLAY8LOBBYCLIENT;

#define DPL_MSGID_LOBBY  0x8000
#define DPL_MSGID_RECEIVE  (0x0001|DPL_MSGID_LOBBY)
#define DPL_MSGID_CONNECT  (0x0002|DPL_MSGID_LOBBY)
#define DPL_MSGID_DISCONNECT  (0x0003|DPL_MSGID_LOBBY)
#define DPL_MSGID_SESSION_STATUS  (0x0004|DPL_MSGID_LOBBY)
#define DPL_MSGID_CONNECTION_SETTINGS  (0x0005|DPL_MSGID_LOBBY)

#define DPLHANDLE_ALLCONNECTIONS  0xFFFFFFFF

#define DPLSESSION_CONNECTED  0x0001
#define DPLSESSION_COULDNOTCONNECT  0x0002
#define DPLSESSION_DISCONNECTED  0x0003
#define DPLSESSION_TERMINATED  0x0004
#define DPLSESSION_HOSTMIGRATED  0x0005
#define DPLSESSION_HOSTMIGRATEDHERE  0x0006

#define DPLAVAILABLE_ALLOWMULTIPLECONNECT  0x0001

#define DPLCONNECT_LAUNCHNEW  0x0001
#define DPLCONNECT_LAUNCHNOTFOUND  0x0002

#define DPLCONNECTSETTINGS_HOST  0x0001

#define DPLINITIALIZE_DISABLEPARAMVAL  0x0001

typedef struct _DPL_APPLICATION_INFO {
    GUID guidApplication;
    PWSTR pwszApplicationName;
    DWORD dwNumRunning;
    DWORD dwNumWaiting;
    DWORD dwFlags;
} DPL_APPLICATION_INFO,  *PDPL_APPLICATION_INFO;

typedef struct _DPL_CONNECTION_SETTINGS {
    DWORD dwSize;
    DWORD dwFlags;
    DPN_APPLICATION_DESC dpnAppDesc;
    IDirectPlay8Address *pdp8HostAddress;
    IDirectPlay8Address **ppdp8DeviceAddresses;
    DWORD cNumDeviceAddresses;
    PWSTR pwszPlayerName;
} DPL_CONNECTION_SETTINGS, *PDPL_CONNECTION_SETTINGS;

typedef struct _DPL_CONNECT_INFO {
    DWORD dwSize;
    DWORD dwFlags;
    GUID guidApplication;
    PDPL_CONNECTION_SETTINGS pdplConnectionSettings;
    PVOID pvLobbyConnectData;
    DWORD dwLobbyConnectDataSize;
} DPL_CONNECT_INFO,  *PDPL_CONNECT_INFO;

typedef struct  _DPL_PROGRAM_DESC {
    DWORD dwSize;
    DWORD dwFlags;
    GUID guidApplication;
    PWSTR pwszApplicationName;
    PWSTR pwszCommandLine;
    PWSTR pwszCurrentDirectory;
    PWSTR pwszDescription;
    PWSTR pwszExecutableFilename;
    PWSTR pwszExecutablePath;
    PWSTR pwszLauncherFilename;
    PWSTR pwszLauncherPath;
} DPL_PROGRAM_DESC, *PDPL_PROGRAM_DESC;

typedef struct _DPL_MESSAGE_CONNECT {
    DWORD dwSize;
    DPNHANDLE hConnectId;
    PDPL_CONNECTION_SETTINGS pdplConnectionSettings;
    PVOID pvLobbyConnectData;
    DWORD dwLobbyConnectDataSize;
    PVOID pvConnectionContext;
} DPL_MESSAGE_CONNECT, *PDPL_MESSAGE_CONNECT;

typedef struct _DPL_MESSAGE_CONNECTION_SETTINGS {
    DWORD dwSize;
    DPNHANDLE hSender;
    PDPL_CONNECTION_SETTINGS pdplConnectionSettings;
    PVOID pvConnectionContext;
} DPL_MESSAGE_CONNECTION_SETTINGS, *PDPL_MESSAGE_CONNECTION_SETTINGS;

typedef struct _DPL_MESSAGE_DISCONNECT {
    DWORD dwSize;
    DPNHANDLE hDisconnectId;
    HRESULT hrReason;
    PVOID pvConnectionContext;
} DPL_MESSAGE_DISCONNECT, *PDPL_MESSAGE_DISCONNECT;

typedef struct _DPL_MESSAGE_RECEIVE {
    DWORD dwSize;
    DPNHANDLE hSender;
    BYTE *pBuffer;
    DWORD dwBufferSize;
    PVOID pvConnectionContext;
} DPL_MESSAGE_RECEIVE, *PDPL_MESSAGE_RECEIVE;

typedef struct _DPL_MESSAGE_SESSION_STATUS {
    DWORD dwSize;
    DPNHANDLE hSender;
    DWORD dwStatus;
    PVOID pvConnectionContext;
} DPL_MESSAGE_SESSION_STATUS, *PDPL_MESSAGE_SESSION_STATUS;

#undef INTERFACE
#define INTERFACE IDirectPlay8LobbyClient
DECLARE_INTERFACE_(IDirectPlay8LobbyClient,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Initialize)(THIS_ const PVOID,const PFNDPNMESSAGEHANDLER,const DWORD) PURE;
    STDMETHOD(EnumLocalPrograms)(THIS_ GUID *const,BYTE *const,DWORD *const,DWORD *const, const DWORD) PURE;
    STDMETHOD(ConnectApplication)(THIS_ DPL_CONNECT_INFO *const,const PVOID,DPNHANDLE *const,const DWORD,const DWORD) PURE;
    STDMETHOD(Send)(THIS_ const DPNHANDLE,BYTE *const,const DWORD,const DWORD) PURE;
    STDMETHOD(ReleaseApplication)(THIS_ const DPNHANDLE,const DWORD) PURE;
    STDMETHOD(Close)(THIS_ const DWORD) PURE;
    STDMETHOD(GetConnectionSettings)(THIS_ const DPNHANDLE,DPL_CONNECTION_SETTINGS * const,DWORD*,const DWORD) PURE;
    STDMETHOD(SetConnectionSettings)(THIS_ const DPNHANDLE,const DPL_CONNECTION_SETTINGS * const,const DWORD) PURE;
};

#undef INTERFACE
#define INTERFACE IDirectPlay8LobbiedApplication
DECLARE_INTERFACE_(IDirectPlay8LobbiedApplication,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Initialize)(THIS_ const PVOID,const PFNDPNMESSAGEHANDLER,DPNHANDLE * const,const DWORD) PURE;
    STDMETHOD(RegisterProgram)(THIS_ PDPL_PROGRAM_DESC,const DWORD) PURE;
    STDMETHOD(UnRegisterProgram)(THIS_ GUID*,const DWORD) PURE;
    STDMETHOD(Send)(THIS_ const DPNHANDLE,BYTE *const,const DWORD,const DWORD) PURE;
    STDMETHOD(SetAppAvailable)(THIS_ const BOOL,const DWORD) PURE;
    STDMETHOD(UpdateStatus)(THIS_ const DPNHANDLE,const DWORD,const DWORD) PURE;
    STDMETHOD(Close)(THIS_ const DWORD) PURE;
    STDMETHOD(GetConnectionSettings)(THIS_ const DPNHANDLE,DPL_CONNECTION_SETTINGS * const,DWORD*,const DWORD) PURE;
    STDMETHOD(SetConnectionSettings)(THIS_ const DPNHANDLE,const DPL_CONNECTION_SETTINGS * const,const DWORD) PURE;
};

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirectPlay8LobbyClient_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectPlay8LobbyClient_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirectPlay8LobbyClient_Release(p)  (p)->lpVtbl->Release(p)
#define IDirectPlay8LobbyClient_Initialize(p,a,b,c)  (p)->lpVtbl->Initialize(p,a,b,c)
#define IDirectPlay8LobbyClient_EnumLocalPrograms(p,a,b,c,d,e)  (p)->lpVtbl->EnumLocalPrograms(p,a,b,c,d,e)
#define IDirectPlay8LobbyClient_ConnectApplication(p,a,b,c,d,e)  (p)->lpVtbl->ConnectApplication(p,a,b,c,d,e)
#define IDirectPlay8LobbyClient_Send(p,a,b,c,d)  (p)->lpVtbl->Send(p,a,b,c,d)
#define IDirectPlay8LobbyClient_ReleaseApplication(p,a,b)  (p)->lpVtbl->ReleaseApplication(p,a,b)
#define IDirectPlay8LobbyClient_Close(p,a)  (p)->lpVtbl->Close(p,a)
#define IDirectPlay8LobbyClient_GetConnectionSettings(p,a,b,c,d)  (p)->lpVtbl->GetConnectionSettings(p,a,b,c,d)
#define IDirectPlay8LobbyClient_SetConnectionSettings(p,a,b,c)  (p)->lpVtbl->SetConnectionSettings(p,a,b,c)
#define IDirectPlay8LobbiedApplication_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectPlay8LobbiedApplication_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirectPlay8LobbiedApplication_Release(p)  (p)->lpVtbl->Release(p)
#define IDirectPlay8LobbiedApplication_Initialize(p,a,b,c,d)  (p)->lpVtbl->Initialize(p,a,b,c,d)
#define IDirectPlay8LobbiedApplication_RegisterProgram(p,a,b)  (p)->lpVtbl->RegisterProgram(p,a,b)
#define IDirectPlay8LobbiedApplication_UnRegisterProgram(p,a,b)  (p)->lpVtbl->UnRegisterProgram(p,a,b)
#define IDirectPlay8LobbiedApplication_Send(p,a,b,c,d)  (p)->lpVtbl->Send(p,a,b,c,d)
#define IDirectPlay8LobbiedApplication_SetAppAvailable(p,a,b)  (p)->lpVtbl->SetAppAvailable(p,a,b)
#define IDirectPlay8LobbiedApplication_UpdateStatus(p,a,b,c)  (p)->lpVtbl->UpdateStatus(p,a,b,c)
#define IDirectPlay8LobbiedApplication_Close(p,a)  (p)->lpVtbl->Close(p,a)
#define IDirectPlay8LobbiedApplication_GetConnectionSettings(p,a,b,c,d)  (p)->lpVtbl->GetConnectionSettings(p,a,b,c,d)
#define IDirectPlay8LobbiedApplication_SetConnectionSettings(p,a,b,c)  (p)->lpVtbl->SetConnectionSettings(p,a,b,c)
#else /* C++ */
#define IDirectPlay8LobbyClient_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirectPlay8LobbyClient_AddRef(p)  (p)->AddRef()
#define IDirectPlay8LobbyClient_Release(p)  (p)->Release()
#define IDirectPlay8LobbyClient_Initialize(p,a,b,c)  (p)->Initialize(a,b,c)
#define IDirectPlay8LobbyClient_EnumLocalPrograms(p,a,b,c,d,e)  (p)->EnumLocalPrograms(a,b,c,d,e)
#define IDirectPlay8LobbyClient_ConnectApplication(p,a,b,c,d,e)  (p)->ConnectApplication(a,b,c,d,e)
#define IDirectPlay8LobbyClient_Send(p,a,b,c,d)  (p)->Send(a,b,c,d)
#define IDirectPlay8LobbyClient_ReleaseApplication(p,a,b)  (p)->ReleaseApplication(a,b)
#define IDirectPlay8LobbyClient_Close(p,a)  (p)->Close(a)
#define IDirectPlay8LobbyClient_GetConnectionSettings(p,a,b,c,d)  (p)->GetConnectionSettings(a,b,c,d)
#define IDirectPlay8LobbyClient_SetConnectionSettings(p,a,b,c)  (p)->SetConnectionSettings(a,b,c)
#define IDirectPlay8LobbiedApplication_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirectPlay8LobbiedApplication_AddRef(p)  (p)->AddRef()
#define IDirectPlay8LobbiedApplication_Release(p)  (p)->Release()
#define IDirectPlay8LobbiedApplication_Initialize(p,a,b,c,d)  (p)->Initialize(a,b,c,d)
#define IDirectPlay8LobbiedApplication_RegisterProgram(p,a,b)  (p)->RegisterProgram(a,b)
#define IDirectPlay8LobbiedApplication_UnRegisterProgram(p,a,b)  (p)->UnRegisterProgram(a,b)
#define IDirectPlay8LobbiedApplication_Send(p,a,b,c,d)  (p)->Send(a,b,c,d)
#define IDirectPlay8LobbiedApplication_SetAppAvailable(p,a,b)  (p)->SetAppAvailable(a,b)
#define IDirectPlay8LobbiedApplication_UpdateStatus(p,a,b,c)  (p)->UpdateStatus(a,b,c)
#define IDirectPlay8LobbiedApplication_Close(p,a)  (p)->Close(a)
#define IDirectPlay8LobbiedApplication_GetConnectionSettings(p,a,b,c,d)  (p)->GetConnectionSettings(a,b,c,d)
#define IDirectPlay8LobbiedApplication_SetConnectionSettings(p,a,b,c)  (p)->SetConnectionSettings(a,b,c)
#endif /* C++ */

#ifdef __cplusplus
}
#endif

#endif /* _DPLOBBY_H */
