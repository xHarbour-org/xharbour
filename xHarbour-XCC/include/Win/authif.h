#ifndef _AUTHIF_H
#define _AUTHIF_H

/* Internet Authentication Service extension definitions */

#define AUTHSRV_PARAMETERS_KEY_W  L"System\\CurrentControlSet\\Services\\AuthSrv\\Parameters"
#define AUTHSRV_EXTENSIONS_VALUE_W  L"ExtensionDLLs"
#define AUTHSRV_AUTHORIZATION_VALUE_W  L"AuthorizationDLLs"

#define RADIUS_EXTENSION_INIT  "RadiusExtensionInit"
#define RADIUS_EXTENSION_TERM  "RadiusExtensionTerm"
#define RADIUS_EXTENSION_PROCESS  "RadiusExtensionProcess"
#define RADIUS_EXTENSION_PROCESS_EX  "RadiusExtensionProcessEx"
#define RADIUS_EXTENSION_FREE_ATTRIBUTES  "RadiusExtensionFreeAttributes"

typedef enum _RADIUS_ATTRIBUTE_TYPE {
    ratMinimum = 0,
    ratUserName = 1,
    ratUserPassword = 2,
    ratCHAPPassword = 3,
    ratNASIPAddress = 4,
    ratNASPort = 5,
    ratServiceType = 6,
    ratFramedProtocol = 7,
    ratFramedIPAddress = 8,
    ratFramedIPNetmask = 9,
    ratFramedRouting = 10,
    ratFilterId = 11,
    ratFramedMTU = 12,
    ratFramedCompression = 13,
    ratLoginIPHost = 14,
    ratLoginService = 15,
    ratLoginPort = 16,
    ratReplyMessage = 18,
    ratCallbackNumber = 19,
    ratCallbackId = 20,
    ratFramedRoute = 22,
    ratFramedIPXNetwork = 23,
    ratState = 24,
    ratClass = 25,
    ratVendorSpecific = 26,
    ratSessionTimeout = 27,
    ratIdleTimeout = 28,
    ratTerminationAction = 29,
    ratCalledStationId = 30,
    ratCallingStationId = 31,
    ratNASIdentifier = 32,
    ratProxyState = 33,
    ratLoginLATService = 34,
    ratLoginLATNode = 35,
    ratLoginLATGroup = 36,
    ratFramedAppleTalkLink = 37,
    ratFramedAppleTalkNetwork = 38,
    ratFramedAppleTalkZone = 39,
    ratAcctStatusType = 40,
    ratAcctDelayTime = 41,
    ratAcctInputOctets = 42,
    ratAcctOutputOctets = 43,
    ratAcctSessionId = 44,
    ratAcctAuthentic = 45,
    ratAcctSessionTime = 46,
    ratAcctInputPackets = 47,
    ratAcctOutputPackets = 48,
    ratAcctTerminationCause = 49,
    ratCHAPChallenge = 60,
    ratNASPortType = 61,
    ratPortLimit = 62,
    ratCode = 262,
    ratIdentifier = 263,
    ratAuthenticator = 264,
    ratSrcIPAddress = 265,
    ratSrcPort = 266,
    ratProvider = 267,
    ratStrippedUserName = 268,
    ratFQUserName = 269,
    ratPolicyName = 270
} RADIUS_ATTRIBUTE_TYPE;

typedef enum _RADIUS_AUTHENTICATION_PROVIDER {
    rapUnknown,
    rapUsersFile,
    rapProxy,
    rapWindowsNT,
    rapMCIS,
    rapODBC
} RADIUS_AUTHENTICATION_PROVIDER;

typedef enum _RADIUS_DATA_TYPE {
    rdtUnknown,
    rdtString,
    rdtAddress,
    rdtInteger,
    rdtTime
} RADIUS_DATA_TYPE;

typedef struct _RADIUS_ATTRIBUTE {
    DWORD dwAttrType;
    RADIUS_DATA_TYPE fDataType;
    DWORD cbDataLength;
    union {
        DWORD dwValue;
        PCSTR lpValue;
    };
} RADIUS_ATTRIBUTE, *PRADIUS_ATTRIBUTE;

typedef enum _RADIUS_ACTION {
    raContinue,
    raReject,
    raAccept
} RADIUS_ACTION, *PRADIUS_ACTION;

typedef DWORD (WINAPI *PRADIUS_EXTENSION_INIT)(VOID);
typedef VOID (WINAPI *PRADIUS_EXTENSION_TERM)(VOID);
typedef DWORD (WINAPI *PRADIUS_EXTENSION_PROCESS)(CONST RADIUS_ATTRIBUTE*,PRADIUS_ACTION);
typedef DWORD (WINAPI *PRADIUS_EXTENSION_PROCESS_EX)(CONST RADIUS_ATTRIBUTE*,PRADIUS_ATTRIBUTE*,PRADIUS_ACTION);
typedef VOID (WINAPI *PRADIUS_EXTENSION_FREE_ATTRIBUTES)(PRADIUS_ATTRIBUTE);

#endif /* _AUTHIF_H */
