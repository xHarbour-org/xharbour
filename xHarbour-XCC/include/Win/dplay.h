#ifndef _DPLAY_H
#define _DPLAY_H

/* DirectPlay definitions */

#include <ole2.h>

#ifndef _WIN64
#define DWORD_PTR DWORD
#endif

typedef LPVOID (*LPRGLPVOID)[];
typedef LPRGLPVOID PRGPVOID, LPRGPVOID, PRGLPVOID, PAPVOID, LPAPVOID, PALPVOID, LPALPVOID;

#define VOL volatile
typedef VOID *VOL LPVOIDV;

#define _FACDP  0x877
#define MAKE_DPHRESULT(code)  MAKE_HRESULT(1,_FACDP,code)

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2198)  /* Nameless field is not standard */
#endif

DEFINE_GUID(IID_IDirectPlay2,0x2b74f7c0,0x9154,0x11cf,0xa9,0xcd,0x0,0xaa,0x0,0x68,0x86,0xe3);
DEFINE_GUID(IID_IDirectPlay2A,0x9d460580,0xa822,0x11cf,0x96,0xc,0x0,0x80,0xc7,0x53,0x4e,0x82);
DEFINE_GUID(IID_IDirectPlay3,0x133efe40,0x32dc,0x11d0,0x9c,0xfb,0x0,0xa0,0xc9,0xa,0x43,0xcb);
DEFINE_GUID(IID_IDirectPlay3A,0x133efe41,0x32dc,0x11d0,0x9c,0xfb,0x0,0xa0,0xc9,0xa,0x43,0xcb);
DEFINE_GUID(IID_IDirectPlay4,0xab1c530,0x4745,0x11d1,0xa7,0xa1,0x0,0x0,0xf8,0x3,0xab,0xfc);
DEFINE_GUID(IID_IDirectPlay4A,0xab1c531,0x4745,0x11d1,0xa7,0xa1,0x0,0x0,0xf8,0x3,0xab,0xfc);
DEFINE_GUID(IID_IDirectPlay,0x5454e9a0,0xdb65,0x11ce,0x92,0x1c,0x00,0xaa,0x00,0x6c,0x49,0x72);
DEFINE_GUID(CLSID_DirectPlay,0xd1eb6d20,0x8923,0x11d0,0x9d,0x97,0x0,0xa0,0xc9,0xa,0x43,0xcb);
DEFINE_GUID(DPSPGUID_IPX,0x685bc400,0x9d2c,0x11cf,0xa9,0xcd,0x0,0xaa,0x0,0x68,0x86,0xe3);
DEFINE_GUID(DPSPGUID_TCPIP,0x36E95EE0,0x8577,0x11cf,0x96,0xc,0x0,0x80,0xc7,0x53,0x4e,0x82);
DEFINE_GUID(DPSPGUID_SERIAL,0xf1d6860,0x88d9,0x11cf,0x9c,0x4e,0x0,0xa0,0xc9,0x5,0x42,0x5e);
DEFINE_GUID(DPSPGUID_MODEM,0x44eaa760,0xcb68,0x11cf,0x9c,0x4e,0x0,0xa0,0xc9,0x5,0x42,0x5e);

#ifndef IDIRECTPLAY2_OR_GREATER
typedef struct IDirectPlay *LPDIRECTPLAY;
#else
typedef struct IUnknown *LPDIRECTPLAY;
#endif

typedef struct IDirectPlay2 *LPDIRECTPLAY2;
typedef struct IDirectPlay2 *LPDIRECTPLAY2A;
typedef struct IDirectPlay2 IDirectPlay2A;
typedef struct IDirectPlay3 *LPDIRECTPLAY3;
typedef struct IDirectPlay3 *LPDIRECTPLAY3A;
typedef struct IDirectPlay3 IDirectPlay3A;
typedef struct IDirectPlay4 *LPDIRECTPLAY4;
typedef struct IDirectPlay4 *LPDIRECTPLAY4A;
typedef struct IDirectPlay4 IDirectPlay4A;

typedef DWORD DPID, *LPDPID;

#define DPID_SYSMSG  0
#define DPID_ALLPLAYERS  0
#define DPID_RESERVEDRANGE  100

#define DPID_UNKNOWN  0xFFFFFFFF

typedef struct {
    DWORD dwSize;
    DWORD dwFlags;
    DWORD dwMaxBufferSize;
    DWORD dwMaxQueueSize;
    DWORD dwMaxPlayers;
    DWORD dwHundredBaud;
    DWORD dwLatency;
    DWORD dwMaxLocalPlayers;
    DWORD dwHeaderLength;
    DWORD dwTimeout;
} DPCAPS, *LPDPCAPS;

#define DPCAPS_ISHOST  0x00000002
#define DPCAPS_GROUPOPTIMIZED  0x00000008
#define DPCAPS_KEEPALIVEOPTIMIZED  0x00000010
#define DPCAPS_GUARANTEEDOPTIMIZED  0x00000020
#define DPCAPS_GUARANTEEDSUPPORTED  0x00000040
#define DPCAPS_SIGNINGSUPPORTED  0x00000080
#define DPCAPS_ENCRYPTIONSUPPORTED  0x00000100
#define DPPLAYERCAPS_LOCAL  0x00000800
#define DPCAPS_ASYNCCANCELSUPPORTED  0x00001000
#define DPCAPS_ASYNCCANCELALLSUPPORTED  0x00002000
#define DPCAPS_SENDTIMEOUTSUPPORTED  0x00004000
#define DPCAPS_SENDPRIORITYSUPPORTED  0x00008000
#define DPCAPS_ASYNCSUPPORTED  0x00010000

typedef struct {
    DWORD dwSize;
    DWORD dwFlags;
    GUID guidInstance;
    GUID guidApplication;
    DWORD dwMaxPlayers;
    DWORD dwCurrentPlayers;
    union {
        LPWSTR lpszSessionName;
        LPSTR lpszSessionNameA;
    };
    union {
        LPWSTR lpszPassword;
        LPSTR lpszPasswordA;
    };
    DWORD_PTR dwReserved1;
    DWORD_PTR dwReserved2;
    DWORD_PTR dwUser1;
    DWORD_PTR dwUser2;
    DWORD_PTR dwUser3;
    DWORD_PTR dwUser4;
} DPSESSIONDESC2, *LPDPSESSIONDESC2;
typedef DPSESSIONDESC2 * VOL LPDPSESSIONDESC2_V;

typedef const DPSESSIONDESC2 *LPCDPSESSIONDESC2;
 
#define DPSESSION_NEWPLAYERSDISABLED  0x00000001 
#define DPSESSION_MIGRATEHOST  0x00000004
#define DPSESSION_NOMESSAGEID  0x00000008
#define DPSESSION_JOINDISABLED  0x00000020
#define DPSESSION_KEEPALIVE  0x00000040
#define DPSESSION_NODATAMESSAGES  0x00000080
#define DPSESSION_SECURESERVER  0x00000100
#define DPSESSION_PRIVATE  0x00000200
#define DPSESSION_PASSWORDREQUIRED  0x00000400
#define DPSESSION_MULTICASTSERVER  0x00000800
#define DPSESSION_CLIENTSERVER  0x00001000
#define DPSESSION_DIRECTPLAYPROTOCOL  0x00002000
#define DPSESSION_NOPRESERVEORDER  0x00004000
#define DPSESSION_OPTIMIZELATENCY  0x00008000
#define DPSESSION_ALLOWVOICERETRO  0x00010000
#define DPSESSION_NOSESSIONDESCMESSAGES  0x00020000
 
typedef struct {
    DWORD dwSize;
    DWORD dwFlags;
    union {
        LPWSTR lpszShortName;
        LPSTR lpszShortNameA;
    };
    union {
        LPWSTR lpszLongName;
        LPSTR lpszLongNameA;
    };
} DPNAME, *LPDPNAME;
typedef const DPNAME *LPCDPNAME;

typedef struct {
    DWORD dwSize;
    DWORD dwFlags;
    union {
        LPWSTR lpszUsername;
        LPSTR lpszUsernameA;
    };    
    union {
        LPWSTR lpszPassword;
        LPSTR lpszPasswordA;
    };    
    union {
        LPWSTR lpszDomain;
        LPSTR lpszDomainA;
    };    
} DPCREDENTIALS, *LPDPCREDENTIALS;
typedef const DPCREDENTIALS *LPCDPCREDENTIALS;

typedef struct {
    DWORD dwSize;
    DWORD dwFlags;
    union {
        LPWSTR lpszSSPIProvider;
        LPSTR lpszSSPIProviderA;
    };
    union {
        LPWSTR lpszCAPIProvider;
        LPSTR lpszCAPIProviderA;
    };
    DWORD dwCAPIProviderType;
    DWORD dwEncryptionAlgorithm;
} DPSECURITYDESC, *LPDPSECURITYDESC;
typedef const DPSECURITYDESC *LPCDPSECURITYDESC;

typedef struct {
    DWORD dwSize;
    DWORD dwFlags;
    union {
        LPWSTR lpszAccountID;
        LPSTR lpszAccountIDA;
    };
} DPACCOUNTDESC, *LPDPACCOUNTDESC;
typedef const DPACCOUNTDESC *LPCDPACCOUNTDESC;

typedef const GUID *LPCGUID;

typedef struct {
    DWORD dwSize;
    DWORD dwFlags;
    LPDPSESSIONDESC2 lpSessionDesc;
    LPDPNAME lpPlayerName;
    GUID guidSP;
    LPVOID lpAddress;
    DWORD dwAddressSize;
} DPLCONNECTION, *LPDPLCONNECTION;
typedef const DPLCONNECTION *LPCDPLCONNECTION;

typedef struct {
    DWORD dwSize;
    DWORD dwFlags;
    union {
        LPWSTR lpszMessage;
        LPSTR lpszMessageA;
    };    
} DPCHAT, *LPDPCHAT;

typedef struct {
    UINT len;
    PUCHAR pData;
} SGBUFFER, *PSGBUFFER, *LPSGBUFFER;

#define DPESC_TIMEDOUT  0x00000001

typedef BOOL (PASCAL *LPDPENUMSESSIONSCALLBACK2)(LPCDPSESSIONDESC2,LPDWORD,DWORD,LPVOID);
typedef BOOL (PASCAL *LPDPENUMPLAYERSCALLBACK2)(DPID,DWORD,LPCDPNAME,DWORD,LPVOID);
typedef BOOL (PASCAL *LPDPENUMDPCALLBACK)(LPGUID,LPWSTR,DWORD,DWORD,LPVOID);
typedef BOOL (PASCAL *LPDPENUMDPCALLBACKA)(LPGUID,LPSTR,DWORD,DWORD,LPVOID);
typedef BOOL (PASCAL *LPDPENUMCONNECTIONSCALLBACK)(LPCGUID,LPVOID,DWORD,LPCDPNAME,DWORD,LPVOID);

#ifdef UNICODE
#define DirectPlayEnumerate  DirectPlayEnumerateW
#else
#define DirectPlayEnumerate  DirectPlayEnumerateA
#endif /* UNICODE */

extern HRESULT WINAPI DirectPlayEnumerateA(LPDPENUMDPCALLBACKA,LPVOID);
extern HRESULT WINAPI DirectPlayEnumerateW(LPDPENUMDPCALLBACK,LPVOID);
extern HRESULT WINAPI DirectPlayCreate(LPGUID,LPDIRECTPLAY*,IUnknown*);

#undef INTERFACE
#define INTERFACE IDirectPlay2
DECLARE_INTERFACE_(IDirectPlay2,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS)  PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(AddPlayerToGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(Close)(THIS) PURE;
    STDMETHOD(CreateGroup)(THIS_ LPDPID,LPDPNAME,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(CreatePlayer)(THIS_ LPDPID,LPDPNAME,HANDLE,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(DeletePlayerFromGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(DestroyGroup)(THIS_ DPID) PURE;
    STDMETHOD(DestroyPlayer)(THIS_ DPID) PURE;
    STDMETHOD(EnumGroupPlayers)(THIS_ DPID,LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(EnumGroups)(THIS_ LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(EnumPlayers)(THIS_ LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(EnumSessions)(THIS_ LPDPSESSIONDESC2,DWORD,LPDPENUMSESSIONSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(GetCaps)(THIS_ LPDPCAPS,DWORD) PURE;
    STDMETHOD(GetGroupData)(THIS_ DPID,LPVOID,LPDWORD,DWORD) PURE;
    STDMETHOD(GetGroupName)(THIS_ DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetMessageCount)(THIS_ DPID,LPDWORD) PURE;
    STDMETHOD(GetPlayerAddress)(THIS_ DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetPlayerCaps)(THIS_ DPID,LPDPCAPS,DWORD) PURE;
    STDMETHOD(GetPlayerData)(THIS_ DPID,LPVOID,LPDWORD,DWORD) PURE;
    STDMETHOD(GetPlayerName)(THIS_ DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetSessionDesc)(THIS_ LPVOID,LPDWORD) PURE;
    STDMETHOD(Initialize)(THIS_ LPGUID) PURE;
    STDMETHOD(Open)(THIS_ LPDPSESSIONDESC2,DWORD) PURE;
    STDMETHOD(Receive)(THIS_ LPDPID,LPDPID,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(Send)(THIS_ DPID,DPID,DWORD,LPVOID,DWORD) PURE;
    STDMETHOD(SetGroupData)(THIS_ DPID,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(SetGroupName)(THIS_ DPID,LPDPNAME,DWORD) PURE;
    STDMETHOD(SetPlayerData)(THIS_ DPID,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(SetPlayerName)(THIS_ DPID,LPDPNAME,DWORD) PURE;
    STDMETHOD(SetSessionDesc)(THIS_ LPDPSESSIONDESC2,DWORD) PURE;
};

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirectPlay2_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectPlay2_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirectPlay2_Release(p)  (p)->lpVtbl->Release(p)
#define IDirectPlay2_AddPlayerToGroup(p,a,b)  (p)->lpVtbl->AddPlayerToGroup(p,a,b)
#define IDirectPlay2_Close(p)  (p)->lpVtbl->Close(p)
#define IDirectPlay2_CreateGroup(p,a,b,c,d,e)  (p)->lpVtbl->CreateGroup(p,a,b,c,d,e)
#define IDirectPlay2_CreatePlayer(p,a,b,c,d,e,f)  (p)->lpVtbl->CreatePlayer(p,a,b,c,d,e,f)
#define IDirectPlay2_DeletePlayerFromGroup(p,a,b)  (p)->lpVtbl->DeletePlayerFromGroup(p,a,b)
#define IDirectPlay2_DestroyGroup(p,a)  (p)->lpVtbl->DestroyGroup(p,a)
#define IDirectPlay2_DestroyPlayer(p,a)  (p)->lpVtbl->DestroyPlayer(p,a)
#define IDirectPlay2_EnumGroupPlayers(p,a,b,c,d,e)  (p)->lpVtbl->EnumGroupPlayers(p,a,b,c,d,e)
#define IDirectPlay2_EnumGroups(p,a,b,c,d)  (p)->lpVtbl->EnumGroups(p,a,b,c,d)
#define IDirectPlay2_EnumPlayers(p,a,b,c,d)  (p)->lpVtbl->EnumPlayers(p,a,b,c,d)
#define IDirectPlay2_EnumSessions(p,a,b,c,d,e)  (p)->lpVtbl->EnumSessions(p,a,b,c,d,e)
#define IDirectPlay2_GetCaps(p,a,b)  (p)->lpVtbl->GetCaps(p,a,b)
#define IDirectPlay2_GetMessageCount(p,a,b)  (p)->lpVtbl->GetMessageCount(p,a,b)
#define IDirectPlay2_GetGroupData(p,a,b,c,d)  (p)->lpVtbl->GetGroupData(p,a,b,c,d)
#define IDirectPlay2_GetGroupName(p,a,b,c)  (p)->lpVtbl->GetGroupName(p,a,b,c)
#define IDirectPlay2_GetPlayerAddress(p,a,b,c)  (p)->lpVtbl->GetPlayerAddress(p,a,b,c)
#define IDirectPlay2_GetPlayerCaps(p,a,b,c)  (p)->lpVtbl->GetPlayerCaps(p,a,b,c)
#define IDirectPlay2_GetPlayerData(p,a,b,c,d)  (p)->lpVtbl->GetPlayerData(p,a,b,c,d)
#define IDirectPlay2_GetPlayerName(p,a,b,c)  (p)->lpVtbl->GetPlayerName(p,a,b,c)
#define IDirectPlay2_GetSessionDesc(p,a,b)  (p)->lpVtbl->GetSessionDesc(p,a,b)
#define IDirectPlay2_Initialize(p,a)  (p)->lpVtbl->Initialize(p,a)
#define IDirectPlay2_Open(p,a,b)  (p)->lpVtbl->Open(p,a,b)
#define IDirectPlay2_Receive(p,a,b,c,d,e)  (p)->lpVtbl->Receive(p,a,b,c,d,e)
#define IDirectPlay2_Send(p,a,b,c,d,e)  (p)->lpVtbl->Send(p,a,b,c,d,e)
#define IDirectPlay2_SetGroupData(p,a,b,c,d)  (p)->lpVtbl->SetGroupData(p,a,b,c,d)
#define IDirectPlay2_SetGroupName(p,a,b,c)  (p)->lpVtbl->SetGroupName(p,a,b,c)
#define IDirectPlay2_SetPlayerData(p,a,b,c,d)  (p)->lpVtbl->SetPlayerData(p,a,b,c,d)
#define IDirectPlay2_SetPlayerName(p,a,b,c)  (p)->lpVtbl->SetPlayerName(p,a,b,c)
#define IDirectPlay2_SetSessionDesc(p,a,b)  (p)->lpVtbl->SetSessionDesc(p,a,b)
#else /* C++ */
#define IDirectPlay2_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirectPlay2_AddRef(p)  (p)->AddRef()
#define IDirectPlay2_Release(p)  (p)->Release()
#define IDirectPlay2_AddPlayerToGroup(p,a,b)  (p)->AddPlayerToGroup(a,b)
#define IDirectPlay2_Close(p)  (p)->Close()
#define IDirectPlay2_CreateGroup(p,a,b,c,d,e)  (p)->CreateGroup(a,b,c,d,e)
#define IDirectPlay2_CreatePlayer(p,a,b,c,d,e,f)  (p)->CreatePlayer(a,b,c,d,e,f)
#define IDirectPlay2_DeletePlayerFromGroup(p,a,b)  (p)->DeletePlayerFromGroup(a,b)
#define IDirectPlay2_DestroyGroup(p,a)  (p)->DestroyGroup(a)
#define IDirectPlay2_DestroyPlayer(p,a)  (p)->DestroyPlayer(a)
#define IDirectPlay2_EnumGroupPlayers(p,a,b,c,d,e)  (p)->EnumGroupPlayers(a,b,c,d,e)
#define IDirectPlay2_EnumGroups(p,a,b,c,d)  (p)->EnumGroups(a,b,c,d)
#define IDirectPlay2_EnumPlayers(p,a,b,c,d)  (p)->EnumPlayers(a,b,c,d)
#define IDirectPlay2_EnumSessions(p,a,b,c,d,e)  (p)->EnumSessions(a,b,c,d,e)
#define IDirectPlay2_GetCaps(p,a,b)  (p)->GetCaps(a,b)
#define IDirectPlay2_GetMessageCount(p,a,b)  (p)->GetMessageCount(a,b)
#define IDirectPlay2_GetGroupData(p,a,b,c,d)  (p)->GetGroupData(a,b,c,d)
#define IDirectPlay2_GetGroupName(p,a,b,c)  (p)->GetGroupName(a,b,c)
#define IDirectPlay2_GetPlayerAddress(p,a,b,c)  (p)->GetPlayerAddress(a,b,c)
#define IDirectPlay2_GetPlayerCaps(p,a,b,c)  (p)->GetPlayerCaps(a,b,c)
#define IDirectPlay2_GetPlayerData(p,a,b,c,d)  (p)->GetPlayerData(a,b,c,d)
#define IDirectPlay2_GetPlayerName(p,a,b,c)  (p)->GetPlayerName(a,b,c)
#define IDirectPlay2_GetSessionDesc(p,a,b)  (p)->GetSessionDesc(a,b)
#define IDirectPlay2_Initialize(p,a)  (p)->Initialize(a)
#define IDirectPlay2_Open(p,a,b)  (p)->Open(a,b)
#define IDirectPlay2_Receive(p,a,b,c,d,e)  (p)->Receive(a,b,c,d,e)
#define IDirectPlay2_Send(p,a,b,c,d,e)  (p)->Send(a,b,c,d,e)
#define IDirectPlay2_SetGroupData(p,a,b,c,d)  (p)->SetGroupData(a,b,c,d)
#define IDirectPlay2_SetGroupName(p,a,b,c)  (p)->SetGroupName(a,b,c)
#define IDirectPlay2_SetPlayerData(p,a,b,c,d)  (p)->SetPlayerData(a,b,c,d)
#define IDirectPlay2_SetPlayerName(p,a,b,c)  (p)->SetPlayerName(a,b,c)
#define IDirectPlay2_SetSessionDesc(p,a,b)  (p)->SetSessionDesc(a,b)
#endif /* C++ */

#undef INTERFACE
#define INTERFACE IDirectPlay3
DECLARE_INTERFACE_(IDirectPlay3,IDirectPlay2)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS)  PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(AddPlayerToGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(Close)(THIS) PURE;
    STDMETHOD(CreateGroup)(THIS_ LPDPID,LPDPNAME,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(CreatePlayer)(THIS_ LPDPID,LPDPNAME,HANDLE,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(DeletePlayerFromGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(DestroyGroup)(THIS_ DPID) PURE;
    STDMETHOD(DestroyPlayer)(THIS_ DPID) PURE;
    STDMETHOD(EnumGroupPlayers)(THIS_ DPID,LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(EnumGroups)(THIS_ LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(EnumPlayers)(THIS_ LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(EnumSessions)(THIS_ LPDPSESSIONDESC2,DWORD,LPDPENUMSESSIONSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(GetCaps)(THIS_ LPDPCAPS,DWORD) PURE;
    STDMETHOD(GetGroupData)(THIS_ DPID,LPVOID,LPDWORD,DWORD) PURE;
    STDMETHOD(GetGroupName)(THIS_ DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetMessageCount)(THIS_ DPID,LPDWORD) PURE;
    STDMETHOD(GetPlayerAddress)(THIS_ DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetPlayerCaps)(THIS_ DPID,LPDPCAPS,DWORD) PURE;
    STDMETHOD(GetPlayerData)(THIS_ DPID,LPVOID,LPDWORD,DWORD) PURE;
    STDMETHOD(GetPlayerName)(THIS_ DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetSessionDesc)(THIS_ LPVOID,LPDWORD) PURE;
    STDMETHOD(Initialize)(THIS_ LPGUID) PURE;
    STDMETHOD(Open)(THIS_ LPDPSESSIONDESC2,DWORD) PURE;
    STDMETHOD(Receive)(THIS_ LPDPID,LPDPID,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(Send)(THIS_ DPID,DPID,DWORD,LPVOID,DWORD) PURE;
    STDMETHOD(SetGroupData)(THIS_ DPID,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(SetGroupName)(THIS_ DPID,LPDPNAME,DWORD) PURE;
    STDMETHOD(SetPlayerData)(THIS_ DPID,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(SetPlayerName)(THIS_ DPID,LPDPNAME,DWORD) PURE;
    STDMETHOD(SetSessionDesc)(THIS_ LPDPSESSIONDESC2,DWORD) PURE;
    STDMETHOD(AddGroupToGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(CreateGroupInGroup)(THIS_ DPID,LPDPID,LPDPNAME,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(DeleteGroupFromGroup)(THIS_ DPID,DPID) PURE;	
    STDMETHOD(EnumConnections)(THIS_ LPCGUID,LPDPENUMCONNECTIONSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(EnumGroupsInGroup)(THIS_ DPID,LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(GetGroupConnectionSettings)(THIS_ DWORD,DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(InitializeConnection)(THIS_ LPVOID,DWORD) PURE;
    STDMETHOD(SecureOpen)(THIS_ LPCDPSESSIONDESC2,DWORD,LPCDPSECURITYDESC,LPCDPCREDENTIALS) PURE;
    STDMETHOD(SendChatMessage)(THIS_ DPID,DPID,DWORD,LPDPCHAT) PURE;
    STDMETHOD(SetGroupConnectionSettings)(THIS_ DWORD,DPID,LPDPLCONNECTION) PURE;
    STDMETHOD(StartSession)(THIS_ DWORD,DPID) PURE;
    STDMETHOD(GetGroupFlags)(THIS_ DPID,LPDWORD) PURE;
    STDMETHOD(GetGroupParent)(THIS_ DPID,LPDPID) PURE;
    STDMETHOD(GetPlayerAccount)(THIS_ DPID,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetPlayerFlags)(THIS_ DPID,LPDWORD) PURE;
};

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirectPlay3_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectPlay3_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirectPlay3_Release(p)  (p)->lpVtbl->Release(p)
#define IDirectPlay3_AddPlayerToGroup(p,a,b)  (p)->lpVtbl->AddPlayerToGroup(p,a,b)
#define IDirectPlay3_Close(p)  (p)->lpVtbl->Close(p)
#define IDirectPlay3_CreateGroup(p,a,b,c,d,e)  (p)->lpVtbl->CreateGroup(p,a,b,c,d,e)
#define IDirectPlay3_CreatePlayer(p,a,b,c,d,e,f)  (p)->lpVtbl->CreatePlayer(p,a,b,c,d,e,f)
#define IDirectPlay3_DeletePlayerFromGroup(p,a,b)  (p)->lpVtbl->DeletePlayerFromGroup(p,a,b)
#define IDirectPlay3_DestroyGroup(p,a)  (p)->lpVtbl->DestroyGroup(p,a)
#define IDirectPlay3_DestroyPlayer(p,a)  (p)->lpVtbl->DestroyPlayer(p,a)
#define IDirectPlay3_EnumGroupPlayers(p,a,b,c,d,e)  (p)->lpVtbl->EnumGroupPlayers(p,a,b,c,d,e)
#define IDirectPlay3_EnumGroups(p,a,b,c,d)  (p)->lpVtbl->EnumGroups(p,a,b,c,d)
#define IDirectPlay3_EnumPlayers(p,a,b,c,d)  (p)->lpVtbl->EnumPlayers(p,a,b,c,d)
#define IDirectPlay3_EnumSessions(p,a,b,c,d,e)  (p)->lpVtbl->EnumSessions(p,a,b,c,d,e)
#define IDirectPlay3_GetCaps(p,a,b)  (p)->lpVtbl->GetCaps(p,a,b)
#define IDirectPlay3_GetMessageCount(p,a,b)  (p)->lpVtbl->GetMessageCount(p,a,b)
#define IDirectPlay3_GetGroupData(p,a,b,c,d)  (p)->lpVtbl->GetGroupData(p,a,b,c,d)
#define IDirectPlay3_GetGroupName(p,a,b,c)  (p)->lpVtbl->GetGroupName(p,a,b,c)
#define IDirectPlay3_GetPlayerAddress(p,a,b,c)  (p)->lpVtbl->GetPlayerAddress(p,a,b,c)
#define IDirectPlay3_GetPlayerCaps(p,a,b,c)  (p)->lpVtbl->GetPlayerCaps(p,a,b,c)
#define IDirectPlay3_GetPlayerData(p,a,b,c,d)  (p)->lpVtbl->GetPlayerData(p,a,b,c,d)
#define IDirectPlay3_GetPlayerName(p,a,b,c)  (p)->lpVtbl->GetPlayerName(p,a,b,c)
#define IDirectPlay3_GetSessionDesc(p,a,b)  (p)->lpVtbl->GetSessionDesc(p,a,b)
#define IDirectPlay3_Initialize(p,a)  (p)->lpVtbl->Initialize(p,a)
#define IDirectPlay3_Open(p,a,b)  (p)->lpVtbl->Open(p,a,b)
#define IDirectPlay3_Receive(p,a,b,c,d,e)  (p)->lpVtbl->Receive(p,a,b,c,d,e)
#define IDirectPlay3_Send(p,a,b,c,d,e)  (p)->lpVtbl->Send(p,a,b,c,d,e)
#define IDirectPlay3_SetGroupData(p,a,b,c,d)  (p)->lpVtbl->SetGroupData(p,a,b,c,d)
#define IDirectPlay3_SetGroupName(p,a,b,c)  (p)->lpVtbl->SetGroupName(p,a,b,c)
#define IDirectPlay3_SetPlayerData(p,a,b,c,d)  (p)->lpVtbl->SetPlayerData(p,a,b,c,d)
#define IDirectPlay3_SetPlayerName(p,a,b,c)  (p)->lpVtbl->SetPlayerName(p,a,b,c)
#define IDirectPlay3_SetSessionDesc(p,a,b)  (p)->lpVtbl->SetSessionDesc(p,a,b)
#define IDirectPlay3_AddGroupToGroup(p,a,b)  (p)->lpVtbl->AddGroupToGroup(p,a,b)
#define IDirectPlay3_CreateGroupInGroup(p,a,b,c,d,e,f)  (p)->lpVtbl->CreateGroupInGroup(p,a,b,c,d,e,f)
#define IDirectPlay3_DeleteGroupFromGroup(p,a,b)  (p)->lpVtbl->DeleteGroupFromGroup(p,a,b)
#define IDirectPlay3_EnumConnections(p,a,b,c,d)  (p)->lpVtbl->EnumConnections(p,a,b,c,d)
#define IDirectPlay3_EnumGroupsInGroup(p,a,b,c,d,e)  (p)->lpVtbl->EnumGroupsInGroup(p,a,b,c,d,e)
#define IDirectPlay3_GetGroupConnectionSettings(p,a,b,c,d)  (p)->lpVtbl->GetGroupConnectionSettings(p,a,b,c,d)
#define IDirectPlay3_InitializeConnection(p,a,b)  (p)->lpVtbl->InitializeConnection(p,a,b)
#define IDirectPlay3_SecureOpen(p,a,b,c,d)  (p)->lpVtbl->SecureOpen(p,a,b,c,d)
#define IDirectPlay3_SendChatMessage(p,a,b,c,d)  (p)->lpVtbl->SendChatMessage(p,a,b,c,d)
#define IDirectPlay3_SetGroupConnectionSettings(p,a,b,c)  (p)->lpVtbl->SetGroupConnectionSettings(p,a,b,c)
#define IDirectPlay3_StartSession(p,a,b)  (p)->lpVtbl->StartSession(p,a,b)
#define IDirectPlay3_GetGroupFlags(p,a,b)  (p)->lpVtbl->GetGroupFlags(p,a,b)
#define IDirectPlay3_GetGroupParent(p,a,b)  (p)->lpVtbl->GetGroupParent(p,a,b)
#define IDirectPlay3_GetPlayerAccount(p,a,b,c,d)  (p)->lpVtbl->GetPlayerAccount(p,a,b,c,d)
#define IDirectPlay3_GetPlayerFlags(p,a,b)  (p)->lpVtbl->GetPlayerFlags(p,a,b)
#else /* C++ */
#define IDirectPlay3_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirectPlay3_AddRef(p)  (p)->AddRef()
#define IDirectPlay3_Release(p)  (p)->Release()
#define IDirectPlay3_AddPlayerToGroup(p,a,b)  (p)->AddPlayerToGroup(a,b)
#define IDirectPlay3_Close(p)  (p)->Close()
#define IDirectPlay3_CreateGroup(p,a,b,c,d,e)  (p)->CreateGroup(a,b,c,d,e)
#define IDirectPlay3_CreatePlayer(p,a,b,c,d,e,f)  (p)->CreatePlayer(a,b,c,d,e,f)
#define IDirectPlay3_DeletePlayerFromGroup(p,a,b)  (p)->DeletePlayerFromGroup(a,b)
#define IDirectPlay3_DestroyGroup(p,a)  (p)->DestroyGroup(a)
#define IDirectPlay3_DestroyPlayer(p,a)  (p)->DestroyPlayer(a)
#define IDirectPlay3_EnumGroupPlayers(p,a,b,c,d,e)  (p)->EnumGroupPlayers(a,b,c,d,e)
#define IDirectPlay3_EnumGroups(p,a,b,c,d)  (p)->EnumGroups(a,b,c,d)
#define IDirectPlay3_EnumPlayers(p,a,b,c,d)  (p)->EnumPlayers(a,b,c,d)
#define IDirectPlay3_EnumSessions(p,a,b,c,d,e)  (p)->EnumSessions(a,b,c,d,e)
#define IDirectPlay3_GetCaps(p,a,b)  (p)->GetCaps(a,b)
#define IDirectPlay3_GetMessageCount(p,a,b)  (p)->GetMessageCount(a,b)
#define IDirectPlay3_GetGroupData(p,a,b,c,d)  (p)->GetGroupData(a,b,c,d)
#define IDirectPlay3_GetGroupName(p,a,b,c)  (p)->GetGroupName(a,b,c)
#define IDirectPlay3_GetPlayerAddress(p,a,b,c)  (p)->GetPlayerAddress(a,b,c)
#define IDirectPlay3_GetPlayerCaps(p,a,b,c)  (p)->GetPlayerCaps(a,b,c)
#define IDirectPlay3_GetPlayerData(p,a,b,c,d)  (p)->GetPlayerData(a,b,c,d)
#define IDirectPlay3_GetPlayerName(p,a,b,c)  (p)->GetPlayerName(a,b,c)
#define IDirectPlay3_GetSessionDesc(p,a,b)  (p)->GetSessionDesc(a,b)
#define IDirectPlay3_Initialize(p,a)  (p)->Initialize(a)
#define IDirectPlay3_Open(p,a,b)  (p)->Open(a,b)
#define IDirectPlay3_Receive(p,a,b,c,d,e)  (p)->Receive(a,b,c,d,e)
#define IDirectPlay3_Send(p,a,b,c,d,e)  (p)->Send(a,b,c,d,e)
#define IDirectPlay3_SetGroupData(p,a,b,c,d)  (p)->SetGroupData(a,b,c,d)
#define IDirectPlay3_SetGroupName(p,a,b,c)  (p)->SetGroupName(a,b,c)
#define IDirectPlay3_SetPlayerData(p,a,b,c,d)  (p)->SetPlayerData(a,b,c,d)
#define IDirectPlay3_SetPlayerName(p,a,b,c)  (p)->SetPlayerName(a,b,c)
#define IDirectPlay3_SetSessionDesc(p,a,b)  (p)->SetSessionDesc(a,b)
#define IDirectPlay3_AddGroupToGroup(p,a,b)  (p)->AddGroupToGroup(a,b)
#define IDirectPlay3_CreateGroupInGroup(p,a,b,c,d,e,f)  (p)->CreateGroupInGroup(a,b,c,d,e,f)
#define IDirectPlay3_DeleteGroupFromGroup(p,a,b)  (p)->DeleteGroupFromGroup(a,b)
#define IDirectPlay3_EnumConnections(p,a,b,c,d)  (p)->EnumConnections(a,b,c,d)
#define IDirectPlay3_EnumGroupsInGroup(p,a,b,c,d,e)  (p)->EnumGroupsInGroup(a,b,c,d,e)
#define IDirectPlay3_GetGroupConnectionSettings(p,a,b,c,d)  (p)->GetGroupConnectionSettings(a,b,c,d)
#define IDirectPlay3_InitializeConnection(p,a,b)  (p)->InitializeConnection(a,b)
#define IDirectPlay3_SecureOpen(p,a,b,c,d)  (p)->SecureOpen(a,b,c,d)
#define IDirectPlay3_SendChatMessage(p,a,b,c,d)  (p)->SendChatMessage(a,b,c,d)
#define IDirectPlay3_SetGroupConnectionSettings(p,a,b,c)  (p)->SetGroupConnectionSettings(a,b,c)
#define IDirectPlay3_StartSession(p,a,b)  (p)->StartSession(a,b)
#define IDirectPlay3_GetGroupFlags(p,a,b)  (p)->GetGroupFlags(a,b)
#define IDirectPlay3_GetGroupParent(p,a,b)  (p)->GetGroupParent(a,b)
#define IDirectPlay3_GetPlayerAccount(p,a,b,c,d)  (p)->GetPlayerAccount(a,b,c,d)
#define IDirectPlay3_GetPlayerFlags(p,a,b)  (p)->GetPlayerFlags(a,b)
#endif /* C++ */

#undef INTERFACE
#define INTERFACE IDirectPlay4
DECLARE_INTERFACE_(IDirectPlay4,IDirectPlay3)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS)  PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(AddPlayerToGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(Close)(THIS) PURE;
    STDMETHOD(CreateGroup)(THIS_ LPDPID,LPDPNAME,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(CreatePlayer)(THIS_ LPDPID,LPDPNAME,HANDLE,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(DeletePlayerFromGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(DestroyGroup)(THIS_ DPID) PURE;
    STDMETHOD(DestroyPlayer)(THIS_ DPID) PURE;
    STDMETHOD(EnumGroupPlayers)(THIS_ DPID,LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(EnumGroups)(THIS_ LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(EnumPlayers)(THIS_ LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(EnumSessions)(THIS_ LPDPSESSIONDESC2,DWORD,LPDPENUMSESSIONSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(GetCaps)(THIS_ LPDPCAPS,DWORD) PURE;
    STDMETHOD(GetGroupData)(THIS_ DPID,LPVOID,LPDWORD,DWORD) PURE;
    STDMETHOD(GetGroupName)(THIS_ DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetMessageCount)(THIS_ DPID,LPDWORD) PURE;
    STDMETHOD(GetPlayerAddress)(THIS_ DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetPlayerCaps)(THIS_ DPID,LPDPCAPS,DWORD) PURE;
    STDMETHOD(GetPlayerData)(THIS_ DPID,LPVOID,LPDWORD,DWORD) PURE;
    STDMETHOD(GetPlayerName)(THIS_ DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetSessionDesc)(THIS_ LPVOID,LPDWORD) PURE;
    STDMETHOD(Initialize)(THIS_ LPGUID) PURE;
    STDMETHOD(Open)(THIS_ LPDPSESSIONDESC2,DWORD) PURE;
    STDMETHOD(Receive)(THIS_ LPDPID,LPDPID,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(Send)(THIS_ DPID,DPID,DWORD,LPVOID,DWORD) PURE;
    STDMETHOD(SetGroupData)(THIS_ DPID,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(SetGroupName)(THIS_ DPID,LPDPNAME,DWORD) PURE;
    STDMETHOD(SetPlayerData)(THIS_ DPID,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(SetPlayerName)(THIS_ DPID,LPDPNAME,DWORD) PURE;
    STDMETHOD(SetSessionDesc)(THIS_ LPDPSESSIONDESC2,DWORD) PURE;
    STDMETHOD(AddGroupToGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(CreateGroupInGroup)(THIS_ DPID,LPDPID,LPDPNAME,LPVOID,DWORD,DWORD) PURE;
    STDMETHOD(DeleteGroupFromGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(EnumConnections)(THIS_ LPCGUID,LPDPENUMCONNECTIONSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(EnumGroupsInGroup)(THIS_ DPID,LPGUID,LPDPENUMPLAYERSCALLBACK2,LPVOID,DWORD) PURE;
    STDMETHOD(GetGroupConnectionSettings)(THIS_ DWORD,DPID,LPVOID,LPDWORD) PURE;
    STDMETHOD(InitializeConnection)(THIS_ LPVOID,DWORD) PURE;
    STDMETHOD(SecureOpen)(THIS_ LPCDPSESSIONDESC2,DWORD,LPCDPSECURITYDESC,LPCDPCREDENTIALS) PURE;
    STDMETHOD(SendChatMessage)(THIS_ DPID,DPID,DWORD,LPDPCHAT) PURE;
    STDMETHOD(SetGroupConnectionSettings)(THIS_ DWORD,DPID,LPDPLCONNECTION) PURE;
    STDMETHOD(StartSession)(THIS_ DWORD,DPID) PURE;
    STDMETHOD(GetGroupFlags)(THIS_ DPID,LPDWORD) PURE;
    STDMETHOD(GetGroupParent)(THIS_ DPID,LPDPID) PURE;
    STDMETHOD(GetPlayerAccount)(THIS_ DPID,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(GetPlayerFlags)(THIS_ DPID,LPDWORD) PURE;
    STDMETHOD(GetGroupOwner)(THIS_ DPID,LPDPID) PURE;
    STDMETHOD(SetGroupOwner)(THIS_ DPID,DPID) PURE;
    STDMETHOD(SendEx)(THIS_ DPID,DPID,DWORD,LPVOID,DWORD,DWORD,DWORD,LPVOID,DWORD_PTR*) PURE;
    STDMETHOD(GetMessageQueue)(THIS_ DPID,DPID,DWORD,LPDWORD,LPDWORD) PURE;
    STDMETHOD(CancelMessage)(THIS_ DWORD,DWORD) PURE;
    STDMETHOD(CancelPriority)(THIS_ DWORD,DWORD,DWORD) PURE;
};

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirectPlayX_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectPlayX_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirectPlayX_Release(p)  (p)->lpVtbl->Release(p)
#define IDirectPlayX_AddPlayerToGroup(p,a,b)  (p)->lpVtbl->AddPlayerToGroup(p,a,b)
#define IDirectPlayX_CancelMessage(p,a,b)  (p)->lpVtbl->CancelMessage(p,a,b)
#define IDirectPlayX_CancelPriority(p,a,b,c)  (p)->lpVtbl->CancelPriority(p,a,b,c)
#define IDirectPlayX_Close(p)  (p)->lpVtbl->Close(p)
#define IDirectPlayX_CreateGroup(p,a,b,c,d,e)  (p)->lpVtbl->CreateGroup(p,a,b,c,d,e)
#define IDirectPlayX_CreatePlayer(p,a,b,c,d,e,f)  (p)->lpVtbl->CreatePlayer(p,a,b,c,d,e,f)
#define IDirectPlayX_DeletePlayerFromGroup(p,a,b)  (p)->lpVtbl->DeletePlayerFromGroup(p,a,b)
#define IDirectPlayX_DestroyGroup(p,a)  (p)->lpVtbl->DestroyGroup(p,a)
#define IDirectPlayX_DestroyPlayer(p,a)  (p)->lpVtbl->DestroyPlayer(p,a)
#define IDirectPlayX_EnumGroupPlayers(p,a,b,c,d,e)  (p)->lpVtbl->EnumGroupPlayers(p,a,b,c,d,e)
#define IDirectPlayX_EnumGroups(p,a,b,c,d)  (p)->lpVtbl->EnumGroups(p,a,b,c,d)
#define IDirectPlayX_EnumPlayers(p,a,b,c,d)  (p)->lpVtbl->EnumPlayers(p,a,b,c,d)
#define IDirectPlayX_EnumSessions(p,a,b,c,d,e)  (p)->lpVtbl->EnumSessions(p,a,b,c,d,e)
#define IDirectPlayX_GetCaps(p,a,b)  (p)->lpVtbl->GetCaps(p,a,b)
#define IDirectPlayX_GetMessageCount(p,a,b)  (p)->lpVtbl->GetMessageCount(p,a,b)
#define IDirectPlayX_GetMessageQueue(p,a,b,c,d,e)  (p)->lpVtbl->GetMessageQueue(p,a,b,c,d,e)
#define IDirectPlayX_GetGroupData(p,a,b,c,d)  (p)->lpVtbl->GetGroupData(p,a,b,c,d)
#define IDirectPlayX_GetGroupName(p,a,b,c)  (p)->lpVtbl->GetGroupName(p,a,b,c)
#define IDirectPlayX_GetPlayerAddress(p,a,b,c)  (p)->lpVtbl->GetPlayerAddress(p,a,b,c)
#define IDirectPlayX_GetPlayerCaps(p,a,b,c)  (p)->lpVtbl->GetPlayerCaps(p,a,b,c)
#define IDirectPlayX_GetPlayerData(p,a,b,c,d)  (p)->lpVtbl->GetPlayerData(p,a,b,c,d)
#define IDirectPlayX_GetPlayerName(p,a,b,c)  (p)->lpVtbl->GetPlayerName(p,a,b,c)
#define IDirectPlayX_GetSessionDesc(p,a,b)  (p)->lpVtbl->GetSessionDesc(p,a,b)
#define IDirectPlayX_Initialize(p,a)  (p)->lpVtbl->Initialize(p,a)
#define IDirectPlayX_Open(p,a,b)  (p)->lpVtbl->Open(p,a,b)
#define IDirectPlayX_Receive(p,a,b,c,d,e)  (p)->lpVtbl->Receive(p,a,b,c,d,e)
#define IDirectPlayX_Send(p,a,b,c,d,e)  (p)->lpVtbl->Send(p,a,b,c,d,e)
#define IDirectPlayX_SendEx(p,a,b,c,d,e,f,g,h,i)  (p)->lpVtbl->SendEx(p,a,b,c,d,e,f,g,h,i)
#define IDirectPlayX_SetGroupData(p,a,b,c,d)  (p)->lpVtbl->SetGroupData(p,a,b,c,d)
#define IDirectPlayX_SetGroupName(p,a,b,c)  (p)->lpVtbl->SetGroupName(p,a,b,c)
#define IDirectPlayX_SetPlayerData(p,a,b,c,d)  (p)->lpVtbl->SetPlayerData(p,a,b,c,d)
#define IDirectPlayX_SetPlayerName(p,a,b,c)  (p)->lpVtbl->SetPlayerName(p,a,b,c)
#define IDirectPlayX_SetSessionDesc(p,a,b)  (p)->lpVtbl->SetSessionDesc(p,a,b)
#define IDirectPlayX_AddGroupToGroup(p,a,b)  (p)->lpVtbl->AddGroupToGroup(p,a,b)
#define IDirectPlayX_CreateGroupInGroup(p,a,b,c,d,e,f)  (p)->lpVtbl->CreateGroupInGroup(p,a,b,c,d,e,f)
#define IDirectPlayX_DeleteGroupFromGroup(p,a,b)  (p)->lpVtbl->DeleteGroupFromGroup(p,a,b)
#define IDirectPlayX_EnumConnections(p,a,b,c,d)  (p)->lpVtbl->EnumConnections(p,a,b,c,d)
#define IDirectPlayX_EnumGroupsInGroup(p,a,b,c,d,e)  (p)->lpVtbl->EnumGroupsInGroup(p,a,b,c,d,e)
#define IDirectPlayX_GetGroupConnectionSettings(p,a,b,c,d)  (p)->lpVtbl->GetGroupConnectionSettings(p,a,b,c,d)
#define IDirectPlayX_InitializeConnection(p,a,b)  (p)->lpVtbl->InitializeConnection(p,a,b)
#define IDirectPlayX_SecureOpen(p,a,b,c,d)  (p)->lpVtbl->SecureOpen(p,a,b,c,d)
#define IDirectPlayX_SendChatMessage(p,a,b,c,d)  (p)->lpVtbl->SendChatMessage(p,a,b,c,d)
#define IDirectPlayX_SetGroupConnectionSettings(p,a,b,c)  (p)->lpVtbl->SetGroupConnectionSettings(p,a,b,c)
#define IDirectPlayX_StartSession(p,a,b)  (p)->lpVtbl->StartSession(p,a,b)
#define IDirectPlayX_GetGroupFlags(p,a,b)  (p)->lpVtbl->GetGroupFlags(p,a,b)
#define IDirectPlayX_GetGroupParent(p,a,b)  (p)->lpVtbl->GetGroupParent(p,a,b)
#define IDirectPlayX_GetPlayerAccount(p,a,b,c,d)  (p)->lpVtbl->GetPlayerAccount(p,a,b,c,d)
#define IDirectPlayX_GetPlayerFlags(p,a,b)  (p)->lpVtbl->GetPlayerFlags(p,a,b)
#define IDirectPlayX_GetGroupOwner(p,a,b)  (p)->lpVtbl->GetGroupOwner(p,a,b)
#define IDirectPlayX_SetGroupOwner(p,a,b)  (p)->lpVtbl->SetGroupOwner(p,a,b)
#else /* C++ */
#define IDirectPlayX_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirectPlayX_AddRef(p)  (p)->AddRef()
#define IDirectPlayX_Release(p)  (p)->Release()
#define IDirectPlayX_AddPlayerToGroup(p,a,b)  (p)->AddPlayerToGroup(a,b)
#define IDirectPlayX_CancelMessage(p,a,b)  (p)->CancelMessage(a,b)
#define IDirectPlayX_CancelPriority(p,a,b,c)  (p)->CancelPriority(a,b,c)
#define IDirectPlayX_Close(p)  (p)->Close()
#define IDirectPlayX_CreateGroup(p,a,b,c,d,e)  (p)->CreateGroup(a,b,c,d,e)
#define IDirectPlayX_CreatePlayer(p,a,b,c,d,e,f)  (p)->CreatePlayer(a,b,c,d,e,f)
#define IDirectPlayX_DeletePlayerFromGroup(p,a,b)  (p)->DeletePlayerFromGroup(a,b)
#define IDirectPlayX_DestroyGroup(p,a)  (p)->DestroyGroup(a)
#define IDirectPlayX_DestroyPlayer(p,a)  (p)->DestroyPlayer(a)
#define IDirectPlayX_EnumGroupPlayers(p,a,b,c,d,e)  (p)->EnumGroupPlayers(a,b,c,d,e)
#define IDirectPlayX_EnumGroups(p,a,b,c,d)  (p)->EnumGroups(a,b,c,d)
#define IDirectPlayX_EnumPlayers(p,a,b,c,d)  (p)->EnumPlayers(a,b,c,d)
#define IDirectPlayX_EnumSessions(p,a,b,c,d,e)  (p)->EnumSessions(a,b,c,d,e)
#define IDirectPlayX_GetCaps(p,a,b)  (p)->GetCaps(a,b)
#define IDirectPlayX_GetMessageCount(p,a,b)  (p)->GetMessageCount(a,b)
#define IDirectPlayX_GetMessageQueue(p,a,b,c,d,e)  (p)->GetMessageQueue(a,b,c,d,e)
#define IDirectPlayX_GetGroupData(p,a,b,c,d)  (p)->GetGroupData(a,b,c,d)
#define IDirectPlayX_GetGroupName(p,a,b,c)  (p)->GetGroupName(a,b,c)
#define IDirectPlayX_GetPlayerAddress(p,a,b,c)  (p)->GetPlayerAddress(a,b,c)
#define IDirectPlayX_GetPlayerCaps(p,a,b,c)  (p)->GetPlayerCaps(a,b,c)
#define IDirectPlayX_GetPlayerData(p,a,b,c,d)  (p)->GetPlayerData(a,b,c,d)
#define IDirectPlayX_GetPlayerName(p,a,b,c)  (p)->GetPlayerName(a,b,c)
#define IDirectPlayX_GetSessionDesc(p,a,b)  (p)->GetSessionDesc(a,b)
#define IDirectPlayX_Initialize(p,a)  (p)->Initialize(a)
#define IDirectPlayX_Open(p,a,b)  (p)->Open(a,b)
#define IDirectPlayX_Receive(p,a,b,c,d,e)  (p)->Receive(a,b,c,d,e)
#define IDirectPlayX_Send(p,a,b,c,d,e)  (p)->Send(a,b,c,d,e)
#define IDirectPlayX_SendEx(p,a,b,c,d,e,f,g,h,i)  (p)->SendEx(a,b,c,d,e,f,g,h,i)
#define IDirectPlayX_SetGroupData(p,a,b,c,d)  (p)->SetGroupData(a,b,c,d)
#define IDirectPlayX_SetGroupName(p,a,b,c)  (p)->SetGroupName(a,b,c)
#define IDirectPlayX_SetPlayerData(p,a,b,c,d)  (p)->SetPlayerData(a,b,c,d)
#define IDirectPlayX_SetPlayerName(p,a,b,c)  (p)->SetPlayerName(a,b,c)
#define IDirectPlayX_SetSessionDesc(p,a,b)  (p)->SetSessionDesc(a,b)
#define IDirectPlayX_AddGroupToGroup(p,a,b)  (p)->AddGroupToGroup(a,b)
#define IDirectPlayX_CreateGroupInGroup(p,a,b,c,d,e,f)  (p)->CreateGroupInGroup(a,b,c,d,e,f)
#define IDirectPlayX_DeleteGroupFromGroup(p,a,b)  (p)->DeleteGroupFromGroup(a,b)
#define IDirectPlayX_EnumConnections(p,a,b,c,d)  (p)->EnumConnections(a,b,c,d)
#define IDirectPlayX_EnumGroupsInGroup(p,a,b,c,d,e)  (p)->EnumGroupsInGroup(a,b,c,d,e)
#define IDirectPlayX_GetGroupConnectionSettings(p,a,b,c,d)  (p)->GetGroupConnectionSettings(a,b,c,d)
#define IDirectPlayX_InitializeConnection(p,a,b)  (p)->InitializeConnection(a,b)
#define IDirectPlayX_SecureOpen(p,a,b,c,d)  (p)->SecureOpen(a,b,c,d)
#define IDirectPlayX_SendChatMessage(p,a,b,c,d)  (p)->SendChatMessage(a,b,c,d)
#define IDirectPlayX_SetGroupConnectionSettings(p,a,b,c)  (p)->SetGroupConnectionSettings(a,b,c)
#define IDirectPlayX_StartSession(p,a,b)  (p)->StartSession(a,b)
#define IDirectPlayX_GetGroupFlags(p,a,b)  (p)->GetGroupFlags(a,b)
#define IDirectPlayX_GetGroupParent(p,a,b)  (p)->GetGroupParent(a,b)
#define IDirectPlayX_GetPlayerAccount(p,a,b,c,d)  (p)->GetPlayerAccount(a,b,c,d)
#define IDirectPlayX_GetPlayerFlags(p,a,b)  (p)->GetPlayerFlags(a,b)
#define IDirectPlayX_GetGroupOwner(p,a,b)  (p)->GetGroupOwner(a,b)
#define IDirectPlayX_SetGroupOwner(p,a,b)  (p)->SetGroupOwner(a,b)
#endif /* C++ */

#define DPCONNECTION_DIRECTPLAY  0x00000001
#define DPCONNECTION_DIRECTPLAYLOBBY  0x00000002

#define DPENUMPLAYERS_ALL  0x00000000
#define DPENUMGROUPS_ALL  DPENUMPLAYERS_ALL
#define DPENUMPLAYERS_LOCAL  0x00000008
#define DPENUMGROUPS_LOCAL  DPENUMPLAYERS_LOCAL
#define DPENUMPLAYERS_REMOTE  0x00000010
#define DPENUMGROUPS_REMOTE  DPENUMPLAYERS_REMOTE
#define DPENUMPLAYERS_GROUP  0x00000020
#define DPENUMPLAYERS_SESSION  0x00000080
#define DPENUMGROUPS_SESSION  DPENUMPLAYERS_SESSION
#define DPENUMPLAYERS_SERVERPLAYER  0x00000100
#define DPENUMPLAYERS_SPECTATOR  0x00000200
#define DPENUMGROUPS_SHORTCUT  0x00000400
#define DPENUMGROUPS_STAGINGAREA  0x00000800
#define DPENUMGROUPS_HIDDEN  0x00001000
#define DPENUMPLAYERS_OWNER  0x00002000

#define DPPLAYER_SERVERPLAYER  DPENUMPLAYERS_SERVERPLAYER
#define DPPLAYER_SPECTATOR  DPENUMPLAYERS_SPECTATOR
#define DPPLAYER_LOCAL  DPENUMPLAYERS_LOCAL
#define DPPLAYER_OWNER  DPENUMPLAYERS_OWNER

#define DPGROUP_STAGINGAREA  DPENUMGROUPS_STAGINGAREA
#define DPGROUP_LOCAL  DPENUMGROUPS_LOCAL
#define DPGROUP_HIDDEN  DPENUMGROUPS_HIDDEN

#define DPENUMSESSIONS_AVAILABLE  0x00000001 
#define DPENUMSESSIONS_ALL  0x00000002
#define DPENUMSESSIONS_ASYNC  0x00000010
#define DPENUMSESSIONS_STOPASYNC  0x00000020
#define DPENUMSESSIONS_PASSWORDREQUIRED  0x00000040
#define DPENUMSESSIONS_RETURNSTATUS  0x00000080

#define DPGETCAPS_GUARANTEED  0x00000001

#define DPGET_REMOTE  0x00000000
#define DPGET_LOCAL  0x00000001

#define DPOPEN_JOIN  0x00000001
#define DPOPEN_CREATE  0x00000002
#define DPOPEN_RETURNSTATUS  DPENUMSESSIONS_RETURNSTATUS

#define DPLCONNECTION_CREATESESSION  DPOPEN_CREATE
#define DPLCONNECTION_JOINSESSION  DPOPEN_JOIN

#define DPRECEIVE_ALL  0x00000001
#define DPRECEIVE_TOPLAYER  0x00000002
#define DPRECEIVE_FROMPLAYER  0x00000004
#define DPRECEIVE_PEEK  0x00000008

#define DPSEND_GUARANTEED  0x00000001
#define DPSEND_HIGHPRIORITY  0x00000002
#define DPSEND_OPENSTREAM  0x00000008
#define DPSEND_CLOSESTREAM  0x00000010
#define DPSEND_SIGNED  0x00000020
#define DPSEND_ENCRYPTED  0x00000040
#define DPSEND_LOBBYSYSTEMMESSAGE  0x00000080
#define DPSEND_ASYNC  0x00000200
#define DPSEND_NOSENDCOMPLETEMSG  0x00000400
#define DPSEND_MAX_PRI  0x0000FFFF
#define DPSEND_MAX_PRIORITY  DPSEND_MAX_PRI

#define DPSET_REMOTE  0x00000000
#define DPSET_LOCAL  0x00000001
#define DPSET_GUARANTEED  0x00000002

#define DPMESSAGEQUEUE_SEND  0x00000001
#define DPMESSAGEQUEUE_RECEIVE  0x00000002
 
#define DPCONNECT_RETURNSTATUS  (DPENUMSESSIONS_RETURNSTATUS)

#define DPSYS_CREATEPLAYERORGROUP  0x0003
#define DPSYS_DESTROYPLAYERORGROUP  0x0005
#define DPSYS_ADDPLAYERTOGROUP  0x0007
#define DPSYS_DELETEPLAYERFROMGROUP  0x0021
#define DPSYS_SESSIONLOST  0x0031
#define DPSYS_HOST  0x0101
#define DPSYS_SETPLAYERORGROUPDATA  0x0102
#define DPSYS_SETPLAYERORGROUPNAME  0x0103
#define DPSYS_SETSESSIONDESC  0x0104
#define DPSYS_ADDGROUPTOGROUP  0x0105
#define DPSYS_DELETEGROUPFROMGROUP  0x0106
#define DPSYS_SECUREMESSAGE  0x0107
#define DPSYS_STARTSESSION  0x0108
#define DPSYS_CHAT  0x0109
#define DPSYS_SETGROUPOWNER  0x010A
#define DPSYS_SENDCOMPLETE  0x010d

#define DPPLAYERTYPE_GROUP  0x00000000
#define DPPLAYERTYPE_PLAYER  0x00000001

typedef struct {
    DWORD dwType;
} DPMSG_GENERIC, *LPDPMSG_GENERIC;

typedef struct {
    DWORD dwType;
    DWORD dwPlayerType;
    DPID dpId;
    DWORD dwCurrentPlayers;
    LPVOID lpData;
    DWORD dwDataSize;
    DPNAME dpnName;
    DPID dpIdParent;
    DWORD dwFlags;
} DPMSG_CREATEPLAYERORGROUP, *LPDPMSG_CREATEPLAYERORGROUP;

typedef struct {
    DWORD dwType;
    DWORD dwPlayerType;
    DPID dpId;
    LPVOID lpLocalData;
    DWORD dwLocalDataSize;
    LPVOID lpRemoteData;
    DWORD dwRemoteDataSize;
    DPNAME dpnName;
    DPID dpIdParent;
    DWORD dwFlags;
} DPMSG_DESTROYPLAYERORGROUP, *LPDPMSG_DESTROYPLAYERORGROUP;

typedef struct {
    DWORD dwType;
    DPID dpIdGroup;
    DPID dpIdPlayer;
} DPMSG_ADDPLAYERTOGROUP, *LPDPMSG_ADDPLAYERTOGROUP;

typedef DPMSG_ADDPLAYERTOGROUP DPMSG_DELETEPLAYERFROMGROUP;
typedef DPMSG_DELETEPLAYERFROMGROUP *LPDPMSG_DELETEPLAYERFROMGROUP;

typedef struct {
    DWORD dwType;
    DPID dpIdParentGroup;
    DPID dpIdGroup;
} DPMSG_ADDGROUPTOGROUP, *LPDPMSG_ADDGROUPTOGROUP;

typedef DPMSG_ADDGROUPTOGROUP DPMSG_DELETEGROUPFROMGROUP;
typedef DPMSG_DELETEGROUPFROMGROUP *LPDPMSG_DELETEGROUPFROMGROUP;

typedef struct {
    DWORD dwType;
    DWORD dwPlayerType;
    DPID dpId;
    LPVOID lpData;
    DWORD dwDataSize;
} DPMSG_SETPLAYERORGROUPDATA, *LPDPMSG_SETPLAYERORGROUPDATA;

typedef struct {
    DWORD dwType;
    DWORD dwPlayerType;
    DPID dpId;
    DPNAME dpnName;
} DPMSG_SETPLAYERORGROUPNAME, *LPDPMSG_SETPLAYERORGROUPNAME;

typedef struct {
    DWORD dwType;
    DPSESSIONDESC2 dpDesc;
} DPMSG_SETSESSIONDESC, *LPDPMSG_SETSESSIONDESC;

typedef DPMSG_GENERIC DPMSG_HOST;
typedef DPMSG_HOST *LPDPMSG_HOST;

typedef DPMSG_GENERIC DPMSG_SESSIONLOST;
typedef DPMSG_SESSIONLOST *LPDPMSG_SESSIONLOST;

typedef struct {
    DWORD dwType;
    DWORD dwFlags;
    DPID dpIdFrom;
    LPVOID lpData;
    DWORD dwDataSize;
} DPMSG_SECUREMESSAGE, *LPDPMSG_SECUREMESSAGE;

typedef struct {
    DWORD dwType;
    LPDPLCONNECTION lpConn;
} DPMSG_STARTSESSION, *LPDPMSG_STARTSESSION;

typedef struct {
    DWORD dwType;
    DWORD dwFlags;
    DPID idFromPlayer;
    DPID idToPlayer;
    DPID idToGroup;
    LPDPCHAT lpChat;
} DPMSG_CHAT, *LPDPMSG_CHAT;

typedef struct {
    DWORD dwType;
    DPID idGroup;
    DPID idNewOwner;
    DPID idOldOwner;
} DPMSG_SETGROUPOWNER, *LPDPMSG_SETGROUPOWNER;

typedef struct {
    DWORD dwType;
    DPID idFrom;
    DPID idTo;
    DWORD dwFlags;
    DWORD dwPriority;
    DWORD dwTimeout;
    LPVOID lpvContext;
    DWORD dwMsgID;
    HRESULT hr;
    DWORD dwSendTime;
} DPMSG_SENDCOMPLETE, *LPDPMSG_SENDCOMPLETE;

#define DP_OK  S_OK
#define DPERR_ALREADYINITIALIZED  MAKE_DPHRESULT(5)
#define DPERR_ACCESSDENIED  MAKE_DPHRESULT(10)
#define DPERR_ACTIVEPLAYERS  MAKE_DPHRESULT(20)
#define DPERR_BUFFERTOOSMALL  MAKE_DPHRESULT(30)
#define DPERR_CANTADDPLAYER  MAKE_DPHRESULT(40)
#define DPERR_CANTCREATEGROUP  MAKE_DPHRESULT(50)
#define DPERR_CANTCREATEPLAYER  MAKE_DPHRESULT(60)
#define DPERR_CANTCREATESESSION  MAKE_DPHRESULT(70)
#define DPERR_CAPSNOTAVAILABLEYET  MAKE_DPHRESULT(80)
#define DPERR_EXCEPTION  MAKE_DPHRESULT(90)
#define DPERR_GENERIC  E_FAIL
#define DPERR_INVALIDFLAGS  MAKE_DPHRESULT(120)
#define DPERR_INVALIDOBJECT  MAKE_DPHRESULT(130)
#define DPERR_INVALIDPARAM  E_INVALIDARG
#define DPERR_INVALIDPARAMS  DPERR_INVALIDPARAM
#define DPERR_INVALIDPLAYER  MAKE_DPHRESULT(150)
#define DPERR_INVALIDGROUP  MAKE_DPHRESULT(155)
#define DPERR_NOCAPS  MAKE_DPHRESULT(160)
#define DPERR_NOCONNECTION  MAKE_DPHRESULT(170)
#define DPERR_NOMEMORY  E_OUTOFMEMORY
#define DPERR_OUTOFMEMORY  DPERR_NOMEMORY
#define DPERR_NOMESSAGES  MAKE_DPHRESULT(190)
#define DPERR_NONAMESERVERFOUND  MAKE_DPHRESULT(200)
#define DPERR_NOPLAYERS  MAKE_DPHRESULT(210)
#define DPERR_NOSESSIONS  MAKE_DPHRESULT(220)
#define DPERR_PENDING  E_PENDING
#define DPERR_SENDTOOBIG  MAKE_DPHRESULT(230)
#define DPERR_TIMEOUT  MAKE_DPHRESULT(240)
#define DPERR_UNAVAILABLE  MAKE_DPHRESULT(250)
#define DPERR_UNSUPPORTED  E_NOTIMPL
#define DPERR_BUSY  MAKE_DPHRESULT(270)
#define DPERR_USERCANCEL  MAKE_DPHRESULT(280) 
#define DPERR_NOINTERFACE  E_NOINTERFACE
#define DPERR_CANNOTCREATESERVER  MAKE_DPHRESULT(290)
#define DPERR_PLAYERLOST  MAKE_DPHRESULT(300)
#define DPERR_SESSIONLOST  MAKE_DPHRESULT(310)
#define DPERR_UNINITIALIZED  MAKE_DPHRESULT(320)
#define DPERR_NONEWPLAYERS  MAKE_DPHRESULT(330)
#define DPERR_INVALIDPASSWORD  MAKE_DPHRESULT(340)
#define DPERR_CONNECTING  MAKE_DPHRESULT(350)
#define DPERR_CONNECTIONLOST  MAKE_DPHRESULT(360)
#define DPERR_UNKNOWNMESSAGE  MAKE_DPHRESULT(370)
#define DPERR_CANCELFAILED  MAKE_DPHRESULT(380)
#define DPERR_INVALIDPRIORITY  MAKE_DPHRESULT(390)
#define DPERR_NOTHANDLED  MAKE_DPHRESULT(400)
#define DPERR_CANCELLED  MAKE_DPHRESULT(410)
#define DPERR_ABORTED  MAKE_DPHRESULT(420)
#define DPERR_BUFFERTOOLARGE  MAKE_DPHRESULT(1000)
#define DPERR_CANTCREATEPROCESS  MAKE_DPHRESULT(1010)
#define DPERR_APPNOTSTARTED  MAKE_DPHRESULT(1020)
#define DPERR_INVALIDINTERFACE  MAKE_DPHRESULT(1030)
#define DPERR_NOSERVICEPROVIDER  MAKE_DPHRESULT(1040)
#define DPERR_UNKNOWNAPPLICATION  MAKE_DPHRESULT(1050)
#define DPERR_NOTLOBBIED  MAKE_DPHRESULT(1070)
#define DPERR_SERVICEPROVIDERLOADED  MAKE_DPHRESULT(1080)
#define DPERR_ALREADYREGISTERED  MAKE_DPHRESULT(1090)
#define DPERR_NOTREGISTERED  MAKE_DPHRESULT(1100)
#define DPERR_AUTHENTICATIONFAILED  MAKE_DPHRESULT(2000)
#define DPERR_CANTLOADSSPI  MAKE_DPHRESULT(2010)
#define DPERR_ENCRYPTIONFAILED  MAKE_DPHRESULT(2020)
#define DPERR_SIGNFAILED  MAKE_DPHRESULT(2030)
#define DPERR_CANTLOADSECURITYPACKAGE  MAKE_DPHRESULT(2040)
#define DPERR_ENCRYPTIONNOTSUPPORTED  MAKE_DPHRESULT(2050)
#define DPERR_CANTLOADCAPI  MAKE_DPHRESULT(2060)
#define DPERR_NOTLOGGEDIN  MAKE_DPHRESULT(2070)
#define DPERR_LOGONDENIED  MAKE_DPHRESULT(2080)

#ifndef IDIRECTPLAY2_OR_GREATER

#define DPOPEN_OPENSESSION  DPOPEN_JOIN
#define DPOPEN_CREATESESSION  DPOPEN_CREATE

#define DPENUMSESSIONS_PREVIOUS  0x00000004
#define DPENUMPLAYERS_PREVIOUS  0x00000004

#define DPSEND_GUARANTEE  DPSEND_GUARANTEED
#define DPSEND_TRYONCE  0x00000004

#define DPCAPS_NAMESERVICE  0x00000001
#define DPCAPS_NAMESERVER  DPCAPS_ISHOST
#define DPCAPS_GUARANTEED  0x00000004

#define DPLONGNAMELEN  52
#define DPSHORTNAMELEN  20
#define DPSESSIONNAMELEN  32
#define DPPASSWORDLEN  16
#define DPUSERRESERVED  16

#define DPSYS_ADDPLAYER  0x0003
#define DPSYS_DELETEPLAYER  0x0005

#define DPSYS_DELETEGROUP  0x0020
#define DPSYS_DELETEPLAYERFROMGRP  0x0021
#define DPSYS_CONNECT  0x484b

typedef struct {
    DWORD dwType;
    DWORD dwPlayerType;
    DPID dpId;
    char szLongName[DPLONGNAMELEN];
    char szShortName[DPSHORTNAMELEN];
    DWORD dwCurrentPlayers;
}   DPMSG_ADDPLAYER;

typedef DPMSG_ADDPLAYER DPMSG_ADDGROUP;

typedef struct {
    DWORD dwType;
    DPID dpIdGroup;
    DPID dpIdPlayer;
} DPMSG_GROUPADD;

typedef DPMSG_GROUPADD DPMSG_GROUPDELETE;

typedef struct {
    DWORD dwType;
    DPID dpId;
} DPMSG_DELETEPLAYER;

typedef BOOL (PASCAL *LPDPENUMPLAYERSCALLBACK)(DPID,LPSTR,LPSTR,DWORD,LPVOID);

typedef struct {
    DWORD dwSize;
    GUID guidSession;
    DWORD_PTR dwSession;
    DWORD dwMaxPlayers;
    DWORD dwCurrentPlayers;
    DWORD dwFlags;
    char szSessionName[DPSESSIONNAMELEN];
    char szUserField[DPUSERRESERVED];
    DWORD_PTR dwReserved1;
    char szPassword[DPPASSWORDLEN];
    DWORD_PTR dwReserved2;
    DWORD_PTR dwUser1;
    DWORD_PTR dwUser2;
    DWORD_PTR dwUser3;
    DWORD_PTR dwUser4;
} DPSESSIONDESC,*LPDPSESSIONDESC;

typedef BOOL (PASCAL *LPDPENUMSESSIONSCALLBACK)(LPDPSESSIONDESC,LPVOID,LPDWORD,DWORD);

#undef INTERFACE
#define INTERFACE IDirectPlay
DECLARE_INTERFACE_(IDirectPlay,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS)  PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(AddPlayerToGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(Close)(THIS) PURE;
    STDMETHOD(CreatePlayer)(THIS_ LPDPID,LPSTR,LPSTR,LPHANDLE) PURE;
    STDMETHOD(CreateGroup)(THIS_ LPDPID,LPSTR,LPSTR) PURE;
    STDMETHOD(DeletePlayerFromGroup)(THIS_ DPID,DPID) PURE;
    STDMETHOD(DestroyPlayer)(THIS_ DPID) PURE;
    STDMETHOD(DestroyGroup)(THIS_ DPID) PURE;
    STDMETHOD(EnableNewPlayers)(THIS_ BOOL) PURE;
    STDMETHOD(EnumGroupPlayers)(THIS_ DPID,LPDPENUMPLAYERSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(EnumGroups)(THIS_ DWORD_PTR,LPDPENUMPLAYERSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(EnumPlayers)(THIS_ DWORD_PTR,LPDPENUMPLAYERSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(EnumSessions)(THIS_ LPDPSESSIONDESC,DWORD,LPDPENUMSESSIONSCALLBACK,LPVOID,DWORD) PURE;
    STDMETHOD(GetCaps)(THIS_ LPDPCAPS) PURE;
    STDMETHOD(GetMessageCount)(THIS_ DPID,LPDWORD) PURE;
    STDMETHOD(GetPlayerCaps)(THIS_ DPID,LPDPCAPS) PURE;
    STDMETHOD(GetPlayerName)(THIS_ DPID,LPSTR,LPDWORD,LPSTR,LPDWORD) PURE;
    STDMETHOD(Initialize)(THIS_ LPGUID) PURE;
    STDMETHOD(Open)(THIS_ LPDPSESSIONDESC) PURE;
    STDMETHOD(Receive)(THIS_ LPDPID,LPDPID,DWORD,LPVOID,LPDWORD) PURE;
    STDMETHOD(SaveSession)(THIS_ LPSTR) PURE;
    STDMETHOD(Send)(THIS_ DPID,DPID,DWORD,LPVOID,DWORD) PURE;
    STDMETHOD(SetPlayerName)(THIS_ DPID,LPSTR,LPSTR) PURE;
};

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirectPlay_AddPlayerToGroup(p,a,b)  (p)->lpVtbl->AddPlayerToGroup(p,a,b)
#define IDirectPlay_Close(p)  (p)->lpVtbl->Close(p)
#define IDirectPlay_CreateGroup(p,a,b,c)  (p)->lpVtbl->CreateGroup(p,a,b,c)
#define IDirectPlay_CreatePlayer(p,a,b,c,d)  (p)->lpVtbl->CreatePlayer(p,a,b,c,d)
#define IDirectPlay_DeletePlayerFromGroup(p,a,b)  (p)->lpVtbl->DeletePlayerFromGroup(p,a,b)
#define IDirectPlay_DestroyGroup(p,a)  (p)->lpVtbl->DestroyGroup(p,a)
#define IDirectPlay_DestroyPlayer(p,a)  (p)->lpVtbl->DestroyPlayer(p,a)
#define IDirectPlay_EnableNewPlayers(p,a)  (p)->lpVtbl->EnableNewPlayers(p,a)
#define IDirectPlay_EnumGroupPlayers(p,a,b,c,d)  (p)->lpVtbl->EnumGroupPlayers(p,a,b,c,d)
#define IDirectPlay_EnumGroups(p,a,b,c,d)  (p)->lpVtbl->EnumGroups(p,a,b,c,d)
#define IDirectPlay_EnumPlayers(p,a,b,c,d)  (p)->lpVtbl->EnumPlayers(p,a,b,c,d)
#define IDirectPlay_EnumSessions(p,a,b,c,d,e)  (p)->lpVtbl->EnumSessions(p,a,b,c,d,e)
#define IDirectPlay_GetCaps(p,a)  (p)->lpVtbl->GetCaps(p,a)
#define IDirectPlay_GetMessageCount(p,a,b)  (p)->lpVtbl->GetMessageCount(p,a,b)
#define IDirectPlay_GetPlayerCaps(p,a,b)  (p)->lpVtbl->GetPlayerCaps(p,a,b)
#define IDirectPlay_GetPlayerName(p,a,b,c,d,e)  (p)->lpVtbl->GetPlayerName(p,a,b,c,d,e)
#define IDirectPlay_Initialize(p,a)  (p)->lpVtbl->Initialize(p,a)
#define IDirectPlay_Open(p,a)  (p)->lpVtbl->Open(p,a)
#define IDirectPlay_Receive(p,a,b,c,d,e)  (p)->lpVtbl->Receive(p,a,b,c,d,e)
#define IDirectPlay_SaveSession(p,a)  (p)->lpVtbl->SaveSession(p,a)
#define IDirectPlay_Send(p,a,b,c,d,e)  (p)->lpVtbl->Send(p,a,b,c,d,e)
#define IDirectPlay_SetPlayerName(p,a,b,c)  (p)->lpVtbl->SetPlayerName(p,a,b,c)
#else /* C++ */
#define IDirectPlay_AddPlayerToGroup(p,a,b)  (p)->AddPlayerToGroup(a,b)
#define IDirectPlay_Close(p)  (p)->Close()
#define IDirectPlay_CreateGroup(p,a,b,c)  (p)->CreateGroup(a,b,c)
#define IDirectPlay_CreatePlayer(p,a,b,c,d)  (p)->CreatePlayer(a,b,c,d)
#define IDirectPlay_DeletePlayerFromGroup(p,a,b)  (p)->DeletePlayerFromGroup(a,b)
#define IDirectPlay_DestroyGroup(p,a)  (p)->DestroyGroup(a)
#define IDirectPlay_DestroyPlayer(p,a)  (p)->DestroyPlayer(a)
#define IDirectPlay_EnableNewPlayers(p,a)  (p)->EnableNewPlayers(a)
#define IDirectPlay_EnumGroupPlayers(p,a,b,c,d)  (p)->EnumGroupPlayers(a,b,c,d)
#define IDirectPlay_EnumGroups(p,a,b,c,d)  (p)->EnumGroups(a,b,c,d)
#define IDirectPlay_EnumPlayers(p,a,b,c,d)  (p)->EnumPlayers(a,b,c,d)
#define IDirectPlay_EnumSessions(p,a,b,c,d,e)  (p)->EnumSessions(a,b,c,d,e)
#define IDirectPlay_GetCaps(p,a)  (p)->GetCaps(a)
#define IDirectPlay_GetMessageCount(p,a,b)  (p)->GetMessageCount(a,b)
#define IDirectPlay_GetPlayerCaps(p,a,b)  (p)->GetPlayerCaps(a,b)
#define IDirectPlay_GetPlayerName(p,a,b,c,d,e)  (p)->GetPlayerName(a,b,c,d,e)
#define IDirectPlay_Initialize(p,a)  (p)->Initialize(a)
#define IDirectPlay_Open(p,a)  (p)->Open(a)
#define IDirectPlay_Receive(p,a,b,c,d,e)  (p)->Receive(a,b,c,d,e)
#define IDirectPlay_SaveSession(p,a)  (p)->SaveSession(a)
#define IDirectPlay_Send(p,a,b,c,d,e)  (p)->Send(a,b,c,d,e)
#define IDirectPlay_SetPlayerName(p,a,b,c)  (p)->SetPlayerName(a,b,c)
#endif /* C++ */

#endif /* IDIRECTPLAY2_OR_GREATER */

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirectPlay_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectPlay_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirectPlay_Release(p)  (p)->lpVtbl->Release(p)
#else
#define IDirectPlay_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirectPlay_AddRef(p)  (p)->AddRef()
#define IDirectPlay_Release(p)  (p)->Release()
#endif /* C++ */

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
};
#endif

#endif /* _DPLAY_H */
