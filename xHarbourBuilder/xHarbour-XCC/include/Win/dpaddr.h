#ifndef _DPADDR_H
#define _DPADDR_H

/* DirectPlayAddress definitions */

#include <ole2.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "dplay8.h"

DEFINE_GUID(CLSID_DirectPlay8Address,0x934a9523,0xa3ca,0x4bc5,0xad,0xa0,0xd6,0xd9,0x5d,0x97,0x94,0x21);
DEFINE_GUID(IID_IDirectPlay8Address,0x83783300,0x4063,0x4c8a,0x9d,0xb3,0x82,0x83,0xa,0x7f,0xeb,0x31);
DEFINE_GUID(IID_IDirectPlay8AddressIP,0xe5a0e990,0x2bad,0x430b,0x87,0xda,0xa1,0x42,0xcf,0x75,0xde,0x58);

typedef struct IDirectPlay8Address *PDIRECTPLAY8ADDRESS, *LPDIRECTPLAY8ADDRESS;
typedef struct IDirectPlay8AddressIP *PDIRECTPLAY8ADDRESSIP, *LPDIRECTPLAY8ADDRESSIP;

typedef struct sockaddr SOCKADDR;

#define DPNA_DATATYPE_STRING  0x00000001
#define DPNA_DATATYPE_DWORD  0x00000002
#define DPNA_DATATYPE_GUID  0x00000003
#define DPNA_DATATYPE_BINARY  0x00000004
#define DPNA_DATATYPE_STRING_ANSI  0x00000005

#define DPNA_DPNSVR_PORT  6073

#define DPNA_INDEX_INVALID  0xFFFFFFFF

#define DPNA_SEPARATOR_KEYVALUE  L'='
#define DPNA_SEPARATOR_USERDATA  L'#'
#define DPNA_SEPARATOR_COMPONENT  L';'
#define DPNA_ESCAPECHAR  L'%'

#define DPNA_HEADER  L"x-directplay:/"

#define DPNA_KEY_APPLICATION_INSTANCE  L"applicationinstance"
#define DPNA_KEY_BAUD  L"baud"
#define DPNA_KEY_DEVICE  L"device"
#define DPNA_KEY_FLOWCONTROL  L"flowcontrol"
#define DPNA_KEY_HOSTNAME  L"hostname"
#define DPNA_KEY_PARITY  L"parity"
#define DPNA_KEY_PHONENUMBER  L"phonenumber"
#define DPNA_KEY_PORT  L"port"
#define DPNA_KEY_PROGRAM  L"program"
#define DPNA_KEY_PROVIDER  L"provider"
#define DPNA_KEY_STOPBITS  L"stopbits"

#define DPNA_BAUD_RATE_9600  9600
#define DPNA_BAUD_RATE_14400  14400
#define DPNA_BAUD_RATE_19200  19200
#define DPNA_BAUD_RATE_38400  38400
#define DPNA_BAUD_RATE_56000  56000
#define DPNA_BAUD_RATE_57600  57600
#define DPNA_BAUD_RATE_115200  115200

#define DPNA_STOP_BITS_ONE  L"1"
#define DPNA_STOP_BITS_ONE_FIVE  L"1.5"
#define DPNA_STOP_BITS_TWO  L"2"

#define DPNA_PARITY_NONE  L"NONE"
#define DPNA_PARITY_EVEN  L"EVEN"
#define DPNA_PARITY_ODD  L"ODD"
#define DPNA_PARITY_MARK  L"MARK"
#define DPNA_PARITY_SPACE  L"SPACE"

#define DPNA_FLOW_CONTROL_NONE  L"NONE"
#define DPNA_FLOW_CONTROL_XONXOFF  L"XONXOFF"
#define DPNA_FLOW_CONTROL_RTS  L"RTS"
#define DPNA_FLOW_CONTROL_DTR  L"DTR"
#define DPNA_FLOW_CONTROL_RTSDTR  L"RTSDTR"

#define DPNA_VALUE_TCPIPPROVIDER  L"IP"
#define DPNA_VALUE_IPXPROVIDER  L"IPX"
#define DPNA_VALUE_MODEMPROVIDER  L"MODEM"
#define DPNA_VALUE_SERIALPROVIDER  L"SERIAL"

#define DPNA_HEADER_A  "x-directplay:/"
#define DPNA_SEPARATOR_KEYVALUE_A  '='
#define DPNA_SEPARATOR_USERDATA_A  '#'
#define DPNA_SEPARATOR_COMPONENT_A  ';'
#define DPNA_ESCAPECHAR_A  '%'

#define DPNA_KEY_APPLICATION_INSTANCE_A  "applicationinstance"
#define DPNA_KEY_BAUD_A  "baud"
#define DPNA_KEY_DEVICE_A  "device"
#define DPNA_KEY_FLOWCONTROL_A  "flowcontrol"
#define DPNA_KEY_HOSTNAME_A  "hostname"
#define DPNA_KEY_PARITY_A  "parity"
#define DPNA_KEY_PHONENUMBER_A  "phonenumber"
#define DPNA_KEY_PORT_A  "port"
#define DPNA_KEY_PROGRAM_A  "program"
#define DPNA_KEY_PROVIDER_A  "provider"
#define DPNA_KEY_STOPBITS_A  "stopbits"

#define DPNA_STOP_BITS_ONE_A  "1"
#define DPNA_STOP_BITS_ONE_FIVE_A  "1.5"
#define DPNA_STOP_BITS_TWO_A  "2"

#define DPNA_PARITY_NONE_A  "NONE"
#define DPNA_PARITY_EVEN_A  "EVEN"
#define DPNA_PARITY_ODD_A  "ODD"
#define DPNA_PARITY_MARK_A  "MARK"
#define DPNA_PARITY_SPACE_A  "SPACE"

#define DPNA_FLOW_CONTROL_NONE_A  "NONE"
#define DPNA_FLOW_CONTROL_XONXOFF_A  "XONXOFF"
#define DPNA_FLOW_CONTROL_RTS_A  "RTS"
#define DPNA_FLOW_CONTROL_DTR_A  "DTR"
#define DPNA_FLOW_CONTROL_RTSDTR_A  "RTSDTR"

#define DPNA_VALUE_TCPIPPROVIDER_A  "IP"
#define DPNA_VALUE_IPXPROVIDER_A  "IPX"
#define DPNA_VALUE_MODEMPROVIDER_A  "MODEM"
#define DPNA_VALUE_SERIALPROVIDER_A  "SERIAL"

#undef INTERFACE
#define INTERFACE IDirectPlay8Address
DECLARE_INTERFACE_(IDirectPlay8Address,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID, LPVOID *) PURE;
    STDMETHOD_(ULONG, AddRef)(THIS) PURE;
    STDMETHOD_(ULONG, Release)(THIS) PURE;
    STDMETHOD(BuildFromURLW)(THIS_ WCHAR*) PURE;
    STDMETHOD(BuildFromURLA)(THIS_ CHAR*) PURE;
    STDMETHOD(Duplicate)(THIS_ PDIRECTPLAY8ADDRESS*) PURE;
    STDMETHOD(SetEqual)(THIS_ PDIRECTPLAY8ADDRESS) PURE;
    STDMETHOD(IsEqual)(THIS_ PDIRECTPLAY8ADDRESS) PURE;
    STDMETHOD(Clear)(THIS) PURE;
    STDMETHOD(GetURLW)(THIS_ WCHAR*,PDWORD) PURE;
    STDMETHOD(GetURLA)(THIS_ CHAR*,PDWORD) PURE;
    STDMETHOD(GetSP)(THIS_ GUID*) PURE;
    STDMETHOD(GetUserData)(THIS_ void*,PDWORD) PURE;
    STDMETHOD(SetSP)(THIS_ const GUID * const) PURE;
    STDMETHOD(SetUserData)(THIS_ const void * const,const DWORD) PURE;
    STDMETHOD(GetNumComponents)(THIS_ PDWORD) PURE;
    STDMETHOD(GetComponentByName)(THIS_ const WCHAR * const,void*,PDWORD,PDWORD) PURE;
    STDMETHOD(GetComponentByIndex)(THIS_ const DWORD,WCHAR*,PDWORD,void*,PDWORD,PDWORD) PURE;
    STDMETHOD(AddComponent)(THIS_ const WCHAR * const,const void * const,const DWORD,const DWORD) PURE;
    STDMETHOD(GetDevice)(THIS_ GUID*) PURE;
    STDMETHOD(SetDevice)(THIS_ const GUID * const)PURE;
    STDMETHOD(BuildFromDPADDRESS)(THIS_ LPVOID,DWORD) PURE;
};

#undef INTERFACE
#define INTERFACE IDirectPlay8AddressIP
DECLARE_INTERFACE_(IDirectPlay8AddressIP,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,PVOID*) PURE;
    STDMETHOD_(ULONG, AddRef)(THIS) PURE;
    STDMETHOD_(ULONG, Release)(THIS) PURE;
    STDMETHOD(BuildFromSockAddr)(THIS_ const SOCKADDR * const) PURE;
    STDMETHOD(BuildAddress)(THIS_ const WCHAR * const,const USHORT) PURE;
    STDMETHOD(BuildLocalAddress)(THIS_ const GUID * const,const USHORT) PURE;
    STDMETHOD(GetSockAddress)(THIS_ SOCKADDR*,PDWORD) PURE;
    STDMETHOD(GetLocalAddress)(THIS_ GUID*,USHORT*) PURE;
    STDMETHOD(GetAddress)(THIS_ WCHAR*,PDWORD,USHORT*) PURE;
};

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirectPlay8Address_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectPlay8Address_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirectPlay8Address_Release(p)  (p)->lpVtbl->Release(p)
#define IDirectPlay8Address_BuildFromURLW(p,a)  (p)->lpVtbl->BuildFromURLW(p,a)
#define IDirectPlay8Address_BuildFromURLA(p,a)  (p)->lpVtbl->BuildFromURLA(p,a)
#define IDirectPlay8Address_Duplicate(p,a)  (p)->lpVtbl->Duplicate(p,a)
#define IDirectPlay8Address_SetEqual(p,a)  (p)->lpVtbl->SetEqual(p,a)
#define IDirectPlay8Address_IsEqual(p,a)  (p)->lpVtbl->IsEqual(p,a)
#define IDirectPlay8Address_Clear(p)  (p)->lpVtbl->Clear(p)
#define IDirectPlay8Address_GetURLW(p,a,b)  (p)->lpVtbl->GetURLW(p,a,b)
#define IDirectPlay8Address_GetURLA(p,a,b)  (p)->lpVtbl->GetURLA(p,a,b)
#define IDirectPlay8Address_GetSP(p,a)  (p)->lpVtbl->GetSP(p,a)
#define IDirectPlay8Address_GetUserData(p,a,b)  (p)->lpVtbl->GetUserData(p,a,b)
#define IDirectPlay8Address_SetSP(p,a)  (p)->lpVtbl->SetSP(p,a)
#define IDirectPlay8Address_SetUserData(p,a,b)  (p)->lpVtbl->SetUserData(p,a,b)
#define IDirectPlay8Address_GetNumComponents(p,a)  (p)->lpVtbl->GetNumComponents(p,a)
#define IDirectPlay8Address_GetComponentByName(p,a,b,c,d)  (p)->lpVtbl->GetComponentByName(p,a,b,c,d)
#define IDirectPlay8Address_GetComponentByIndex(p,a,b,c,d,e,f)  (p)->lpVtbl->GetComponentByIndex(p,a,b,c,d,e,f)
#define IDirectPlay8Address_AddComponent(p,a,b,c,d)  (p)->lpVtbl->AddComponent(p,a,b,c,d)
#define IDirectPlay8Address_SetDevice(p,a)  (p)->lpVtbl->SetDevice(p,a)
#define IDirectPlay8Address_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirectPlay8Address_BuildFromDirectPlay4Address(p,a,b)  (p)->lpVtbl->BuildFromDirectPlay4Address(p,a,b)

#define IDirectPlay8AddressIP_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirectPlay8AddressIP_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirectPlay8AddressIP_Release(p)  (p)->lpVtbl->Release(p)
#define IDirectPlay8AddressIP_BuildFromSockAddr(p,a)  (p)->lpVtbl->BuildFromSockAddr(p,a)
#define IDirectPlay8AddressIP_BuildAddress(p,a,b)  (p)->lpVtbl->BuildAddress(p,a,b)
#define IDirectPlay8AddressIP_BuildLocalAddress(p,a,b)  (p)->lpVtbl->BuildLocalAddress(p,a,b)
#define IDirectPlay8AddressIP_GetSockAddress(p,a,b)  (p)->lpVtbl->GetSockAddress(p,a,b)
#define IDirectPlay8AddressIP_GetLocalAddress(p,a,b)  (p)->lpVtbl->GetLocalAddress(p,a,b)
#define IDirectPlay8AddressIP_GetAddress(p,a,b,c)  (p)->lpVtbl->GetAddress(p,a,b,c)
#else /* C++ */
#define IDirectPlay8Address_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirectPlay8Address_AddRef(p)  (p)->AddRef()
#define IDirectPlay8Address_Release(p)  (p)->Release()
#define IDirectPlay8Address_BuildFromURLW(p,a)  (p)->BuildFromURLW(a)
#define IDirectPlay8Address_BuildFromURLA(p,a)  (p)->BuildFromURLA(a)
#define IDirectPlay8Address_Duplicate(p,a)  (p)->Duplicate(a)
#define IDirectPlay8Address_SetEqual(p,a)  (p)->SetEqual(a)
#define IDirectPlay8Address_IsEqual(p,a)  (p)->IsEqual(a)
#define IDirectPlay8Address_Clear(p)  (p)->Clear()
#define IDirectPlay8Address_GetURLW(p,a,b)  (p)->GetURLW(a,b)
#define IDirectPlay8Address_GetURLA(p,a,b)  (p)->GetURLA(a,b)
#define IDirectPlay8Address_GetSP(p,a)  (p)->GetSP(a)
#define IDirectPlay8Address_GetUserData(p,a,b)  (p)->GetUserData(a,b)
#define IDirectPlay8Address_SetSP(p,a)  (p)->SetSP(a)
#define IDirectPlay8Address_SetUserData(p,a,b)  (p)->SetUserData(a,b)
#define IDirectPlay8Address_GetNumComponents(p,a)  (p)->GetNumComponents(a)
#define IDirectPlay8Address_GetComponentByName(p,a,b,c,d)  (p)->GetComponentByName(a,b,c,d)
#define IDirectPlay8Address_GetComponentByIndex(p,a,b,c,d,e,f)  (p)->GetComponentByIndex(a,b,c,d,e,f)
#define IDirectPlay8Address_AddComponent(p,a,b,c,d)  (p)->AddComponent(a,b,c,d)
#define IDirectPlay8Address_SetDevice(p,a)  (p)->SetDevice(a)
#define IDirectPlay8Address_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirectPlay8Address_BuildFromDirectPlay4Address(p,a,b)  (p)->BuildFromDirectPlay4Address(a,b)

#define IDirectPlay8AddressIP_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirectPlay8AddressIP_AddRef(p)  (p)->AddRef()
#define IDirectPlay8AddressIP_Release(p)  (p)->Release()
#define IDirectPlay8AddressIP_BuildFromSockAddr(p,a)  (p)->BuildFromSockAddr(a)
#define IDirectPlay8AddressIP_BuildAddress(p,a,b)  (p)->BuildAddress(a,b)
#define IDirectPlay8AddressIP_BuildLocalAddress(p,a,b)  (p)->BuildLocalAddress(a,b)
#define IDirectPlay8AddressIP_GetSockAddress(p,a,b)  (p)->GetSockAddress(a,b)
#define IDirectPlay8AddressIP_GetLocalAddress(p,a,b)  (p)->GetLocalAddress(a,b)
#define IDirectPlay8AddressIP_GetAddress(p,a,b,c)  (p)->GetAddress(a,b,c)
#endif /* C++ */

#ifdef __cplusplus
}
#endif

#endif /* _DPADDR_H */
