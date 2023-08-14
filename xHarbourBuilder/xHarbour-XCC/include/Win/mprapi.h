#ifndef _MPRAPI_H
#define _MPRAPI_H

/* MPR definitions */

#include <lmcons.h>
#include <ras.h>

#ifdef __cplusplus
extern "C" {
#endif

#define RRAS_SERVICE_NAME  TEXT("RemoteAccess")

#define PID_IPX  0x0000002B
#define PID_IP  0x00000021
#define PID_NBF  0x0000003F
#define PID_ATALK  0x00000029

#define MAX_INTERFACE_NAME_LEN  256
#define MAX_TRANSPORT_NAME_LEN  40
#define MAX_MEDIA_NAME  16
#define MAX_PORT_NAME  16
#define MAX_DEVICE_NAME  128
#define MAX_PHONE_NUMBER_LEN  128
#define MAX_DEVICETYPE_NAME  16

#define MPR_INTERFACE_OUT_OF_RESOURCES  0x00000001
#define MPR_INTERFACE_ADMIN_DISABLED  0x00000002
#define MPR_INTERFACE_CONNECTION_FAILURE  0x00000004
#define MPR_INTERFACE_SERVICE_PAUSED  0x00000008
#define MPR_INTERFACE_DIALOUT_HOURS_RESTRICTION  0x00000010
#define MPR_INTERFACE_NO_MEDIA_SENSE  0x00000020
#define MPR_INTERFACE_NO_DEVICE  0x00000040

typedef enum _ROUTER_INTERFACE_TYPE {
    ROUTER_IF_TYPE_CLIENT,
    ROUTER_IF_TYPE_HOME_ROUTER,
    ROUTER_IF_TYPE_FULL_ROUTER,
    ROUTER_IF_TYPE_DEDICATED,
    ROUTER_IF_TYPE_INTERNAL,
    ROUTER_IF_TYPE_LOOPBACK,
    ROUTER_IF_TYPE_TUNNEL1,
    ROUTER_IF_TYPE_DIALOUT
} ROUTER_INTERFACE_TYPE;

typedef enum _ROUTER_CONNECTION_STATE {
    ROUTER_IF_STATE_UNREACHABLE,
    ROUTER_IF_STATE_DISCONNECTED,
    ROUTER_IF_STATE_CONNECTING,
    ROUTER_IF_STATE_CONNECTED
} ROUTER_CONNECTION_STATE;

typedef struct _MPR_INTERFACE_0 {
    WCHAR wszInterfaceName[MAX_INTERFACE_NAME_LEN+1];
    HANDLE hInterface;
    BOOL fEnabled;
    ROUTER_INTERFACE_TYPE dwIfType;
    ROUTER_CONNECTION_STATE dwConnectionState;
    DWORD fUnReachabilityReasons;
    DWORD dwLastError;
} MPR_INTERFACE_0, *PMPR_INTERFACE_0;

typedef struct _MPR_IPINIP_INTERFACE_0 {
    WCHAR wszFriendlyName[MAX_INTERFACE_NAME_LEN+1];
    GUID Guid;
} MPR_IPINIP_INTERFACE_0, *PMPR_IPINIP_INTERFACE_0;

#if (WINVER >= 0x0500)

#define MPR_MaxDeviceType  RAS_MaxDeviceType
#define MPR_MaxPhoneNumber  RAS_MaxPhoneNumber
#define MPR_MaxIpAddress  RAS_MaxIpAddress
#define MPR_MaxIpxAddress  RAS_MaxIpxAddress

#define MPR_MaxEntryName  RAS_MaxEntryName
#define MPR_MaxDeviceName  RAS_MaxDeviceName
#define MPR_MaxCallbackNumber  RAS_MaxCallbackNumber

#define MPR_MaxAreaCode  RAS_MaxAreaCode
#define MPR_MaxPadType  RAS_MaxPadType
#define MPR_MaxX25Address  RAS_MaxX25Address
#define MPR_MaxFacilities  RAS_MaxFacilities
#define MPR_MaxUserData  RAS_MaxUserData

#define MPRIO_SpecificIpAddr  RASEO_SpecificIpAddr
#define MPRIO_SpecificNameServers  RASEO_SpecificNameServers
#define MPRIO_IpHeaderCompression  RASEO_IpHeaderCompression
#define MPRIO_RemoteDefaultGateway  RASEO_RemoteDefaultGateway
#define MPRIO_DisableLcpExtensions  RASEO_DisableLcpExtensions
#define MPRIO_SwCompression  RASEO_SwCompression
#define MPRIO_RequireEncryptedPw  RASEO_RequireEncryptedPw
#define MPRIO_RequireMsEncryptedPw  RASEO_RequireMsEncryptedPw
#define MPRIO_RequireDataEncryption  RASEO_RequireDataEncryption
#define MPRIO_NetworkLogon  RASEO_NetworkLogon
#define MPRIO_PromoteAlternates  RASEO_PromoteAlternates
#define MPRIO_SecureLocalFiles  RASEO_SecureLocalFiles
#define MPRIO_RequireEAP  RASEO_RequireEAP
#define MPRIO_RequirePAP  RASEO_RequirePAP
#define MPRIO_RequireSPAP  RASEO_RequireSPAP
#define MPRIO_SharedPhoneNumbers  RASEO_SharedPhoneNumbers
#define MPRIO_RequireCHAP  RASEO_RequireCHAP
#define MPRIO_RequireMsCHAP  RASEO_RequireMsCHAP

#define MPRNP_Ipx  RASNP_Ipx
#define MPRNP_Ip  RASNP_Ip

#define MPRDT_Modem  RASDT_Modem
#define MPRDT_Isdn  RASDT_Isdn
#define MPRDT_X25  RASDT_X25
#define MPRDT_Vpn  RASDT_Vpn
#define MPRDT_Pad  RASDT_Pad
#define MPRDT_Generic  RASDT_Generic
#define MPRDT_Serial  RASDT_Serial
#define MPRDT_FrameRelay  RASDT_FrameRelay
#define MPRDT_Atm  RASDT_Atm
#define MPRDT_Sonet  RASDT_Sonet
#define MPRDT_SW56  RASDT_SW56
#define MPRDT_Irda  RASDT_Irda
#define MPRDT_Parallel  RASDT_Parallel

#define MPRET_Phone  RASET_Phone
#define MPRET_Vpn  RASET_Vpn
#define MPRET_Direct  RASET_Direct

#define MPRDM_DialAll  RASEDM_DialAll
#define MPRDM_DialAsNeeded  RASEDM_DialAsNeeded

#define MPRIDS_Disabled  RASIDS_Disabled
#define MPRIDS_UseGlobalValue  RASIDS_UseGlobalValue

#define MPR_ET_None  ET_None
#define MPR_ET_Require  ET_Require
#define MPR_ET_RequireMax  ET_RequireMax
#define MPR_ET_Optional  ET_Optional

#define MPR_VS_Default  VS_Default
#define MPR_VS_PptpOnly  VS_PptpOnly
#define MPR_VS_PptpFirst  VS_PptpFirst
#define MPR_VS_L2tpOnly  VS_L2tpOnly
#define MPR_VS_L2tpFirst  VS_L2tpFirst

typedef struct _MPR_INTERFACE_1 {
    WCHAR wszInterfaceName[MAX_INTERFACE_NAME_LEN+1];
    HANDLE hInterface;
    BOOL fEnabled;
    ROUTER_INTERFACE_TYPE dwIfType;
    ROUTER_CONNECTION_STATE dwConnectionState;
    DWORD fUnReachabilityReasons;
    DWORD dwLastError;
    LPWSTR lpwsDialoutHoursRestriction;
} MPR_INTERFACE_1, *PMPR_INTERFACE_1;

typedef struct _MPR_INTERFACE_2 {
    WCHAR wszInterfaceName[MAX_INTERFACE_NAME_LEN+1];
    HANDLE hInterface;
    BOOL fEnabled;
    ROUTER_INTERFACE_TYPE dwIfType;
    ROUTER_CONNECTION_STATE dwConnectionState;
    DWORD fUnReachabilityReasons;
    DWORD dwLastError;
    DWORD dwfOptions;
    WCHAR szLocalPhoneNumber[ RAS_MaxPhoneNumber + 1 ];
    PWCHAR szAlternates;
    DWORD ipaddr;
    DWORD ipaddrDns;
    DWORD ipaddrDnsAlt;
    DWORD ipaddrWins;
    DWORD ipaddrWinsAlt;
    DWORD dwfNetProtocols;
    WCHAR szDeviceType[MPR_MaxDeviceType+1];
    WCHAR szDeviceName[MPR_MaxDeviceName+1];
    WCHAR szX25PadType[MPR_MaxPadType+1];
    WCHAR szX25Address[MPR_MaxX25Address+1];
    WCHAR szX25Facilities[MPR_MaxFacilities+1];
    WCHAR szX25UserData[MPR_MaxUserData+1];
    DWORD dwChannels;
    DWORD dwSubEntries;
    DWORD dwDialMode;
    DWORD dwDialExtraPercent;
    DWORD dwDialExtraSampleSeconds;
    DWORD dwHangUpExtraPercent;
    DWORD dwHangUpExtraSampleSeconds;
    DWORD dwIdleDisconnectSeconds;
    DWORD dwType;
    DWORD dwEncryptionType;
    DWORD dwCustomAuthKey;
    DWORD dwCustomAuthDataSize;
    LPBYTE lpbCustomAuthData;
    GUID guidId;
    DWORD dwVpnStrategy;
} MPR_INTERFACE_2, *PMPR_INTERFACE_2;

typedef struct _MPR_DEVICE_0 {
    WCHAR szDeviceType[MPR_MaxDeviceType+1];
    WCHAR szDeviceName[MPR_MaxDeviceName+1];
} MPR_DEVICE_0, *PMPR_DEVICE_0;

typedef struct _MPR_DEVICE_1 {
    WCHAR szDeviceType[MPR_MaxDeviceType+1];
    WCHAR szDeviceName[MPR_MaxDeviceName+1];
    WCHAR szLocalPhoneNumber[MPR_MaxPhoneNumber+1];
    PWCHAR szAlternates;
} MPR_DEVICE_1, *PMPR_DEVICE_1;

typedef struct _MPR_CREDENTIALSEX_0 {
    DWORD dwSize;
    LPBYTE lpbCredentialsInfo;
} MPR_CREDENTIALSEX_0, *PMPR_CREDENTIALSEX_0;

#endif /* WINVER >= 0x0500 */

typedef struct _MPR_TRANSPORT_0 {
    DWORD dwTransportId;
    HANDLE hTransport;
    WCHAR wszTransportName[MAX_TRANSPORT_NAME_LEN+1];
} MPR_TRANSPORT_0, *PMPR_TRANSPORT_0;

typedef struct _MPR_IFTRANSPORT_0 {
    DWORD dwTransportId;
    HANDLE hIfTransport;
    WCHAR wszIfTransportName[MAX_TRANSPORT_NAME_LEN+1];
} MPR_IFTRANSPORT_0, *PMPR_IFTRANSPORT_0;

typedef struct _MPR_SERVER_0 {
    BOOL fLanOnlyMode;
    DWORD dwUpTime;
    DWORD dwTotalPorts;
    DWORD dwPortsInUse;
} MPR_SERVER_0, *PMPR_SERVER_0;

typedef enum _RAS_PORT_CONDITION {
    RAS_PORT_NON_OPERATIONAL,
    RAS_PORT_DISCONNECTED,
    RAS_PORT_CALLING_BACK,
    RAS_PORT_LISTENING,
    RAS_PORT_AUTHENTICATING,
    RAS_PORT_AUTHENTICATED,
    RAS_PORT_INITIALIZING
} RAS_PORT_CONDITION;

typedef enum _RAS_HARDWARE_CONDITION {
    RAS_HARDWARE_OPERATIONAL,
    RAS_HARDWARE_FAILURE
} RAS_HARDWARE_CONDITION;

typedef struct _RAS_PORT_0 {
    HANDLE hPort;
    HANDLE hConnection;
    RAS_PORT_CONDITION dwPortCondition;
    DWORD dwTotalNumberOfCalls;
    DWORD dwConnectDuration;
    WCHAR wszPortName[MAX_PORT_NAME+1];
    WCHAR wszMediaName[MAX_MEDIA_NAME+1];
    WCHAR wszDeviceName[MAX_DEVICE_NAME+1];
    WCHAR wszDeviceType[MAX_DEVICETYPE_NAME+1];
} RAS_PORT_0, *PRAS_PORT_0;

typedef struct _RAS_PORT_1 {
    HANDLE hPort;
    HANDLE hConnection;
    RAS_HARDWARE_CONDITION dwHardwareCondition;
    DWORD dwLineSpeed;
    DWORD dwBytesXmited;
    DWORD dwBytesRcved;
    DWORD dwFramesXmited;
    DWORD dwFramesRcved;
    DWORD dwCrcErr;
    DWORD dwTimeoutErr;
    DWORD dwAlignmentErr;
    DWORD dwHardwareOverrunErr;
    DWORD dwFramingErr;
    DWORD dwBufferOverrunErr;
    DWORD dwCompressionRatioIn;
    DWORD dwCompressionRatioOut;
} RAS_PORT_1, *PRAS_PORT_1;

#define IPADDRESSLEN  15
#define IPXADDRESSLEN  22
#define ATADDRESSLEN  32

typedef struct _PPP_NBFCP_INFO {
    DWORD dwError;
    WCHAR wszWksta[NETBIOS_NAME_LEN+1];
} PPP_NBFCP_INFO;

typedef struct _PPP_IPCP_INFO {
    DWORD dwError;
    WCHAR wszAddress[IPADDRESSLEN+1];
    WCHAR wszRemoteAddress[IPADDRESSLEN+1];
} PPP_IPCP_INFO;

#define PPP_IPCP_VJ  0x00000001

typedef struct _PPP_IPCP_INFO2 {
    DWORD dwError;
    WCHAR wszAddress[IPADDRESSLEN+1];
    WCHAR wszRemoteAddress[IPADDRESSLEN+1];
    DWORD dwOptions;
    DWORD dwRemoteOptions;
} PPP_IPCP_INFO2;

typedef struct _PPP_IPXCP_INFO {
    DWORD dwError;
    WCHAR wszAddress[IPXADDRESSLEN+1];
} PPP_IPXCP_INFO;

typedef struct _PPP_ATCP_INFO {
    DWORD dwError;
    WCHAR wszAddress[ATADDRESSLEN+1];
} PPP_ATCP_INFO;

typedef struct _PPP_INFO {
   PPP_NBFCP_INFO nbf;
   PPP_IPCP_INFO ip;
   PPP_IPXCP_INFO ipx;
   PPP_ATCP_INFO at;
} PPP_INFO;

#if (WINVER >= 0x0500)

#define RASCCPCA_MPPC  0x00000006
#define RASCCPCA_STAC  0x00000005

#define PPP_CCP_COMPRESSION  0x00000001
#define PPP_CCP_ENCRYPTION40BITOLD  0x00000010
#define PPP_CCP_ENCRYPTION40BIT  0x00000020
#define PPP_CCP_ENCRYPTION128BIT  0x00000040
#define PPP_CCP_ENCRYPTION56BIT  0x00000080
#define PPP_CCP_HISTORYLESS  0x01000000

#define PPP_LCP_PAP  0xC023
#define PPP_LCP_SPAP  0xC123
#define PPP_LCP_CHAP  0xC223
#define PPP_LCP_EAP  0xC227

#define PPP_LCP_CHAP_MD5  0x05
#define PPP_LCP_CHAP_MS  0x80
#define PPP_LCP_CHAP_MSV2  0x81

#define PPP_LCP_MULTILINK_FRAMING  0x00000001
#define PPP_LCP_PFC  0x00000002
#define PPP_LCP_ACFC  0x00000004
#define PPP_LCP_SSHF  0x00000008
#define PPP_LCP_DES_56  0x00000010
#define PPP_LCP_3_DES  0x00000020

typedef struct _PPP_CCP_INFO {
    DWORD dwError;
    DWORD dwCompressionAlgorithm;
    DWORD dwOptions;
    DWORD dwRemoteCompressionAlgorithm;
    DWORD dwRemoteOptions;
} PPP_CCP_INFO;

typedef struct _PPP_LCP_INFO {
    DWORD dwError;
    DWORD dwAuthenticationProtocol;
    DWORD dwAuthenticationData;
    DWORD dwRemoteAuthenticationProtocol;
    DWORD dwRemoteAuthenticationData;
    DWORD dwTerminateReason;
    DWORD dwRemoteTerminateReason;
    DWORD dwOptions;
    DWORD dwRemoteOptions;
    DWORD dwEapTypeId;
    DWORD dwRemoteEapTypeId;
} PPP_LCP_INFO;

typedef struct _PPP_INFO_2 {
    PPP_NBFCP_INFO nbf;
    PPP_IPCP_INFO2 ip;
    PPP_IPXCP_INFO ipx;
    PPP_ATCP_INFO at;
    PPP_CCP_INFO ccp;
    PPP_LCP_INFO lcp;
} PPP_INFO_2;

#endif /* WINVER >= 0x0500 */

#define RAS_FLAGS_PPP_CONNECTION  0x00000001
#define RAS_FLAGS_MESSENGER_PRESENT  0x00000002
#define RAS_FLAGS_RAS_CONNECTION  0x00000004

typedef struct _RAS_CONNECTION_0 {
    HANDLE hConnection;
    HANDLE hInterface;
    DWORD dwConnectDuration;
    ROUTER_INTERFACE_TYPE dwInterfaceType;
    DWORD dwConnectionFlags;
    WCHAR wszInterfaceName[MAX_INTERFACE_NAME_LEN+1];
    WCHAR wszUserName[UNLEN+1];
    WCHAR wszLogonDomain[DNLEN+1];
    WCHAR wszRemoteComputer[NETBIOS_NAME_LEN+1];
} RAS_CONNECTION_0, *PRAS_CONNECTION_0;

typedef struct _RAS_CONNECTION_1 {
    HANDLE hConnection;
    HANDLE hInterface;
    PPP_INFO PppInfo;
    DWORD dwBytesXmited;
    DWORD dwBytesRcved;
    DWORD dwFramesXmited;
    DWORD dwFramesRcved;
    DWORD dwCrcErr;
    DWORD dwTimeoutErr;
    DWORD dwAlignmentErr;
    DWORD dwHardwareOverrunErr;
    DWORD dwFramingErr;
    DWORD dwBufferOverrunErr;
    DWORD dwCompressionRatioIn;
    DWORD dwCompressionRatioOut;
} RAS_CONNECTION_1, *PRAS_CONNECTION_1;

#if (WINVER >= 0x0500)
typedef struct _RAS_CONNECTION_2 {
    HANDLE hConnection;
    WCHAR wszUserName[UNLEN+1];
    ROUTER_INTERFACE_TYPE dwInterfaceType;
    GUID guid;
    PPP_INFO_2 PppInfo2;
} RAS_CONNECTION_2, *PRAS_CONNECTION_2;
#endif /* WINVER >= 0x0500 */

#define RASPRIV_NoCallback  0x01
#define RASPRIV_AdminSetCallback  0x02
#define RASPRIV_CallerSetCallback  0x04
#define RASPRIV_DialinPrivilege  0x08
#define RASPRIV_CallbackType (RASPRIV_AdminSetCallback|RASPRIV_CallerSetCallback|RASPRIV_NoCallback)

#define RASPRIV2_DialinPolicy  0x1

#ifndef _RAS_USER_0_DEFINED
#define _RAS_USER_0_DEFINED
typedef struct _RAS_USER_0 {
    BYTE bfPrivilege;
    WCHAR wszPhoneNumber[MAX_PHONE_NUMBER_LEN+1];
} RAS_USER_0, *PRAS_USER_0;
#endif

typedef struct _RAS_USER_1 {
    BYTE bfPrivilege;
    WCHAR wszPhoneNumber[MAX_PHONE_NUMBER_LEN+1];
    BYTE bfPrivilege2;
} RAS_USER_1, *PRAS_USER_1;

typedef HANDLE RAS_SERVER_HANDLE;
typedef HANDLE MPR_SERVER_HANDLE;
typedef HANDLE MIB_SERVER_HANDLE;

DWORD APIENTRY MprAdminConnectionEnum(RAS_SERVER_HANDLE,DWORD,LPBYTE*,DWORD,LPDWORD,LPDWORD,LPDWORD);
DWORD APIENTRY MprAdminPortEnum(RAS_SERVER_HANDLE,DWORD,HANDLE,LPBYTE*,DWORD,LPDWORD,LPDWORD,LPDWORD);
DWORD APIENTRY MprAdminConnectionGetInfo(RAS_SERVER_HANDLE,DWORD,HANDLE,LPBYTE*);
DWORD APIENTRY MprAdminPortGetInfo(RAS_SERVER_HANDLE,DWORD,HANDLE,LPBYTE*);
DWORD APIENTRY MprAdminConnectionClearStats(RAS_SERVER_HANDLE,HANDLE);
DWORD APIENTRY MprAdminPortClearStats(RAS_SERVER_HANDLE,HANDLE);
DWORD APIENTRY MprAdminPortReset(RAS_SERVER_HANDLE,HANDLE);
DWORD APIENTRY MprAdminPortDisconnect(RAS_SERVER_HANDLE,HANDLE);
BOOL APIENTRY MprAdminAcceptNewConnection(RAS_CONNECTION_0*,RAS_CONNECTION_1*);
BOOL APIENTRY MprAdminAcceptNewLink(RAS_PORT_0*,RAS_PORT_1*);
VOID APIENTRY MprAdminConnectionHangupNotification(RAS_CONNECTION_0*,RAS_CONNECTION_1*);
VOID APIENTRY MprAdminLinkHangupNotification(RAS_PORT_0*,RAS_PORT_1*);
DWORD APIENTRY MprAdminGetIpAddressForUser(WCHAR*,WCHAR*,DWORD*,BOOL*);
VOID APIENTRY MprAdminReleaseIpAddress(WCHAR*,WCHAR*,DWORD*);
DWORD APIENTRY MprAdminUserGetInfo(const WCHAR*,const WCHAR*,DWORD,LPBYTE);
DWORD APIENTRY MprAdminUserSetInfo(const WCHAR*,const WCHAR*,DWORD,const LPBYTE);
DWORD APIENTRY MprAdminGetPDCServer(const WCHAR*,const WCHAR*,LPWSTR);
BOOL APIENTRY MprAdminIsServiceRunning(LPWSTR);
DWORD APIENTRY MprAdminServerConnect(LPWSTR,MPR_SERVER_HANDLE*);
VOID APIENTRY MprAdminServerDisconnect(MPR_SERVER_HANDLE);
DWORD APIENTRY MprAdminBufferFree(LPVOID);
DWORD APIENTRY MprAdminGetErrorString(DWORD,LPWSTR*);
DWORD APIENTRY MprAdminServerGetInfo(MPR_SERVER_HANDLE,DWORD,LPBYTE*);
DWORD APIENTRY MprAdminTransportSetInfo(MPR_SERVER_HANDLE,DWORD,LPBYTE,DWORD,LPBYTE,DWORD);
DWORD APIENTRY MprAdminTransportGetInfo(MPR_SERVER_HANDLE,DWORD,LPBYTE*,LPDWORD,LPBYTE*,LPDWORD);
DWORD APIENTRY MprAdminInterfaceGetHandle(MPR_SERVER_HANDLE,LPWSTR,HANDLE*,BOOL);
DWORD APIENTRY MprAdminInterfaceCreate(MPR_SERVER_HANDLE,DWORD,LPBYTE,HANDLE*);
DWORD APIENTRY MprAdminInterfaceGetInfo(MPR_SERVER_HANDLE,HANDLE,DWORD,LPBYTE*);
DWORD APIENTRY MprAdminInterfaceSetInfo(MPR_SERVER_HANDLE,HANDLE,DWORD,LPBYTE);
DWORD APIENTRY MprAdminInterfaceDelete(MPR_SERVER_HANDLE,HANDLE);
DWORD APIENTRY MprAdminInterfaceTransportRemove(MPR_SERVER_HANDLE,HANDLE,DWORD);
DWORD APIENTRY MprAdminInterfaceTransportAdd(MPR_SERVER_HANDLE,HANDLE,DWORD,LPBYTE,DWORD);
DWORD APIENTRY MprAdminInterfaceTransportGetInfo(MPR_SERVER_HANDLE,HANDLE,DWORD,LPBYTE*,LPDWORD);
DWORD APIENTRY MprAdminInterfaceTransportSetInfo(MPR_SERVER_HANDLE,HANDLE,DWORD,LPBYTE,DWORD);
DWORD APIENTRY MprAdminInterfaceEnum(MPR_SERVER_HANDLE,DWORD,LPBYTE*,DWORD,LPDWORD,LPDWORD,LPDWORD);
DWORD APIENTRY MprSetupIpInIpInterfaceFriendlyNameEnum(PWCHAR,LPBYTE*,LPDWORD);
DWORD APIENTRY MprSetupIpInIpInterfaceFriendlyNameFree(LPVOID);
DWORD APIENTRY MprSetupIpInIpInterfaceFriendlyNameCreate(PWCHAR,PMPR_IPINIP_INTERFACE_0);
DWORD APIENTRY MprSetupIpInIpInterfaceFriendlyNameDelete(PWCHAR,GUID*);
DWORD APIENTRY MprAdminInterfaceSetCredentials(LPWSTR,LPWSTR,LPWSTR,LPWSTR,LPWSTR);
DWORD APIENTRY MprAdminInterfaceGetCredentials(LPWSTR,LPWSTR,LPWSTR,LPWSTR,LPWSTR);
DWORD APIENTRY MprAdminInterfaceConnect(MPR_SERVER_HANDLE,HANDLE,HANDLE,BOOL);
DWORD APIENTRY MprAdminInterfaceDisconnect(MPR_SERVER_HANDLE,HANDLE);
DWORD APIENTRY MprAdminInterfaceUpdateRoutes(MPR_SERVER_HANDLE,HANDLE,DWORD,HANDLE);
DWORD APIENTRY MprAdminInterfaceQueryUpdateResult(MPR_SERVER_HANDLE,HANDLE,DWORD,LPDWORD);
DWORD APIENTRY MprAdminInterfaceUpdatePhonebookInfo(MPR_SERVER_HANDLE,HANDLE);
DWORD APIENTRY MprAdminMIBServerConnect(LPWSTR,MIB_SERVER_HANDLE*);
VOID APIENTRY MprAdminMIBServerDisconnect(MIB_SERVER_HANDLE);
DWORD APIENTRY MprAdminMIBEntryCreate(MIB_SERVER_HANDLE,DWORD,DWORD,LPVOID,DWORD);
DWORD APIENTRY MprAdminMIBEntryDelete(MIB_SERVER_HANDLE,DWORD,DWORD,LPVOID,DWORD);
DWORD APIENTRY MprAdminMIBEntrySet(MIB_SERVER_HANDLE,DWORD,DWORD,LPVOID,DWORD);
DWORD APIENTRY MprAdminMIBEntryGet(MIB_SERVER_HANDLE,DWORD,DWORD,LPVOID,DWORD,LPVOID*,LPDWORD);
DWORD APIENTRY MprAdminMIBEntryGetFirst(MIB_SERVER_HANDLE,DWORD,DWORD,LPVOID,DWORD,LPVOID*,LPDWORD);
DWORD APIENTRY MprAdminMIBEntryGetNext(MIB_SERVER_HANDLE,DWORD,DWORD,LPVOID,DWORD,LPVOID*,LPDWORD);
DWORD APIENTRY MprAdminMIBGetTrapInfo(MIB_SERVER_HANDLE,DWORD,DWORD,LPVOID,DWORD,LPVOID*,LPDWORD);
DWORD APIENTRY MprAdminMIBSetTrapInfo(DWORD,DWORD,HANDLE,LPVOID,DWORD,LPVOID*,LPDWORD);
DWORD APIENTRY MprAdminMIBBufferFree(LPVOID);
DWORD APIENTRY MprConfigServerInstall(DWORD,PVOID);
DWORD APIENTRY MprConfigServerConnect(LPWSTR,HANDLE*);
VOID APIENTRY MprConfigServerDisconnect(HANDLE);
DWORD APIENTRY MprConfigServerRefresh(HANDLE);
DWORD APIENTRY MprConfigBufferFree(LPVOID);
DWORD APIENTRY MprConfigServerGetInfo(HANDLE,DWORD,LPBYTE*);
DWORD APIENTRY MprConfigServerBackup(HANDLE,LPWSTR);
DWORD APIENTRY MprConfigServerRestore(HANDLE,LPWSTR);
DWORD APIENTRY MprConfigTransportCreate(HANDLE,DWORD,LPWSTR,LPBYTE,DWORD,LPBYTE,DWORD,LPWSTR,HANDLE*);
DWORD APIENTRY MprConfigTransportDelete(HANDLE,HANDLE);
DWORD APIENTRY MprConfigTransportGetHandle(HANDLE,DWORD,HANDLE*);
DWORD APIENTRY MprConfigTransportSetInfo(HANDLE,HANDLE,LPBYTE,DWORD,LPBYTE,DWORD,LPWSTR);
DWORD APIENTRY MprConfigTransportGetInfo(HANDLE,HANDLE,LPBYTE*,LPDWORD,LPBYTE*,LPDWORD,LPWSTR*);
DWORD APIENTRY MprConfigTransportEnum(HANDLE,DWORD,LPBYTE*,DWORD,LPDWORD,LPDWORD,LPDWORD);
DWORD APIENTRY MprConfigInterfaceCreate(HANDLE,DWORD,LPBYTE,HANDLE*);
DWORD APIENTRY MprConfigInterfaceDelete(HANDLE,HANDLE);
DWORD APIENTRY MprConfigInterfaceGetHandle(HANDLE,LPWSTR,HANDLE*);
DWORD APIENTRY MprConfigInterfaceGetInfo(HANDLE,HANDLE,DWORD,LPBYTE*,LPDWORD);
DWORD APIENTRY MprConfigInterfaceSetInfo(HANDLE,HANDLE,DWORD,LPBYTE);
DWORD APIENTRY MprConfigInterfaceEnum(HANDLE,DWORD,LPBYTE*,DWORD,LPDWORD,LPDWORD,LPDWORD);
DWORD APIENTRY MprConfigInterfaceTransportAdd(HANDLE,HANDLE,DWORD,LPWSTR,LPBYTE,DWORD,HANDLE*);
DWORD APIENTRY MprConfigInterfaceTransportRemove(HANDLE,HANDLE,HANDLE);
DWORD APIENTRY MprConfigInterfaceTransportGetHandle(HANDLE,HANDLE,DWORD,HANDLE*);
DWORD APIENTRY MprConfigInterfaceTransportGetInfo(HANDLE,HANDLE,HANDLE,LPBYTE*,LPDWORD);
DWORD APIENTRY MprConfigInterfaceTransportSetInfo(HANDLE,HANDLE,HANDLE,LPBYTE,DWORD);
DWORD APIENTRY MprConfigInterfaceTransportEnum(HANDLE,HANDLE,DWORD,LPBYTE*,DWORD,LPDWORD,LPDWORD,LPDWORD);
DWORD APIENTRY MprConfigGetFriendlyName(HANDLE,PWCHAR,PWCHAR,DWORD);
DWORD APIENTRY MprConfigGetGuidName(HANDLE,PWCHAR,PWCHAR,DWORD);
DWORD APIENTRY MprInfoCreate(DWORD,LPVOID*);
DWORD APIENTRY MprInfoDelete(LPVOID);
DWORD APIENTRY MprInfoRemoveAll(LPVOID,LPVOID*);
DWORD APIENTRY MprInfoDuplicate(LPVOID,LPVOID*);
DWORD APIENTRY MprInfoBlockAdd(LPVOID,DWORD,DWORD,DWORD,LPBYTE,LPVOID*);
DWORD APIENTRY MprInfoBlockRemove(LPVOID,DWORD,LPVOID*);
DWORD APIENTRY MprInfoBlockSet(LPVOID,DWORD,DWORD,DWORD,LPBYTE,LPVOID*);
DWORD APIENTRY MprInfoBlockFind(LPVOID,DWORD,LPDWORD,LPDWORD,LPBYTE*);
DWORD APIENTRY MprInfoBlockQuerySize(LPVOID);
#if (WINVER >= 0x0500)
BOOL APIENTRY MprAdminAcceptNewConnection2(RAS_CONNECTION_0*,RAS_CONNECTION_1*,RAS_CONNECTION_2*);
VOID APIENTRY MprAdminConnectionHangupNotification2(RAS_CONNECTION_0*,RAS_CONNECTION_1*,RAS_CONNECTION_2*);
DWORD APIENTRY MprAdminInitializeDll(VOID);
DWORD APIENTRY MprAdminTerminateDll(VOID);
DWORD APIENTRY MprAdminSendUserMessage(MPR_SERVER_HANDLE,HANDLE,LPWSTR);
DWORD APIENTRY MprAdminTransportCreate(MPR_SERVER_HANDLE,DWORD,LPWSTR,LPBYTE,DWORD,LPBYTE,DWORD,LPWSTR);
DWORD APIENTRY MprAdminDeviceEnum(MPR_SERVER_HANDLE,DWORD,LPBYTE*,LPDWORD);
DWORD APIENTRY MprAdminInterfaceDeviceGetInfo(MPR_SERVER_HANDLE,HANDLE,DWORD,DWORD,LPBYTE*);
DWORD APIENTRY MprAdminInterfaceDeviceSetInfo(MPR_SERVER_HANDLE,HANDLE,DWORD,DWORD,LPBYTE);
DWORD APIENTRY MprAdminInterfaceSetCredentialsEx(MPR_SERVER_HANDLE,HANDLE,DWORD,LPBYTE);
DWORD APIENTRY MprAdminInterfaceGetCredentialsEx(MPR_SERVER_HANDLE,HANDLE,DWORD,LPBYTE*);
DWORD APIENTRY MprAdminRegisterConnectionNotification(MPR_SERVER_HANDLE,HANDLE);
DWORD APIENTRY MprAdminDeregisterConnectionNotification(MPR_SERVER_HANDLE,HANDLE);
#endif /* WINVER >= 0x0500 */

#define MprInfoBlockExists(h,t)  (MprInfoBlockFind((h),(t),NULL,NULL,NULL) == NO_ERROR)

#ifdef __cplusplus
}
#endif

#endif /* _MPRAPI_H */

