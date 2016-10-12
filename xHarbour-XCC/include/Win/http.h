#ifndef _HTTP_H
#define _HTTP_H

/* HTTP API definitions */

#include <winsock2.h>
#include <ws2tcpip.h>

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2198)  /* Nameless field is not standard */
#endif

#undef HTTP_VERSION

#define HTTP_INITIALIZE_SERVER  0x00000001
#define HTTP_INITIALIZE_CONFIG  0x00000002

#define HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY  0x00000001

#define HTTP_SEND_RESPONSE_FLAG_DISCONNECT  0x00000001
#define HTTP_SEND_RESPONSE_FLAG_MORE_DATA  0x00000002

#define HTTP_FLUSH_RESPONSE_FLAG_RECURSIVE  0x00000001

#define HTTP_NULL_ID  (0ULL)
#define HTTP_IS_NULL_ID(pid)  (HTTP_NULL_ID == *(pid))
#define HTTP_SET_NULL_ID(pid)  (*(pid) = HTTP_NULL_ID)

#define HTTP_BYTE_RANGE_TO_EOF  ((ULONGLONG)-1)

#define HTTP_VERSION_UNKNOWN  {0,0}
#define HTTP_VERSION_0_9  {0,9}
#define HTTP_VERSION_1_0  {1,0}
#define HTTP_VERSION_1_1  {1,1}

#define HTTP_SET_VERSION(version,major,minor) \
do { \
    (version).MajorVersion = (major); \
    (version).MinorVersion = (minor); \
} while (0)

#define HTTP_EQUAL_VERSION(version,major,minor)  (((version).MajorVersion == (major)) && ((version).MinorVersion == (minor)))
#define HTTP_GREATER_VERSION(version,major,minor)  ((version).MajorVersion > (major) || ((version).MajorVersion == (major) && (version).MinorVersion > (minor)))
#define HTTP_LESS_VERSION(version,major,minor)  ((version).MajorVersion < (major) || ((version).MajorVersion == (major) && (version).MinorVersion < (minor)))
#define HTTP_NOT_EQUAL_VERSION(version,major,minor)  (!HTTP_EQUAL_VERSION(version,major,minor))
#define HTTP_GREATER_EQUAL_VERSION(version,major,minor)  (!HTTP_LESS_VERSION(version,major,minor))
#define HTTP_LESS_EQUAL_VERSION(version,major,minor)  (!HTTP_GREATER_VERSION(version,major,minor))

#define HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS  0x00000001

#define HTTP_SERVICE_CONFIG_SSL_FLAG_USE_DS_MAPPER  0x00000001
#define HTTP_SERVICE_CONFIG_SSL_FLAG_NEGOTIATE_CLIENT_CERT  0x00000002
#define HTTP_SERVICE_CONFIG_SSL_FLAG_NO_RAW_FILTER  0x00000004

typedef ULONGLONG HTTP_OPAQUE_ID, *PHTTP_OPAQUE_ID;
typedef HTTP_OPAQUE_ID HTTP_REQUEST_ID, *PHTTP_REQUEST_ID;
typedef HTTP_OPAQUE_ID HTTP_CONNECTION_ID, *PHTTP_CONNECTION_ID;
typedef HTTP_OPAQUE_ID HTTP_RAW_CONNECTION_ID, *PHTTP_RAW_CONNECTION_ID;

typedef struct _HTTP_BYTE_RANGE {
    ULARGE_INTEGER StartingOffset;
    ULARGE_INTEGER Length;
} HTTP_BYTE_RANGE, *PHTTP_BYTE_RANGE;

typedef struct _HTTP_VERSION {
    USHORT MajorVersion;
    USHORT MinorVersion;
} HTTP_VERSION, *PHTTP_VERSION;

typedef enum _HTTP_VERB {
    HttpVerbUnparsed,
    HttpVerbUnknown,
    HttpVerbInvalid,
    HttpVerbOPTIONS,
    HttpVerbGET,
    HttpVerbHEAD,
    HttpVerbPOST,
    HttpVerbPUT,
    HttpVerbDELETE,
    HttpVerbTRACE,
    HttpVerbCONNECT,
    HttpVerbTRACK,
    HttpVerbMOVE,
    HttpVerbCOPY,
    HttpVerbPROPFIND,
    HttpVerbPROPPATCH,
    HttpVerbMKCOL,
    HttpVerbLOCK,
    HttpVerbUNLOCK,
    HttpVerbSEARCH,
    HttpVerbMaximum
} HTTP_VERB, *PHTTP_VERB;

typedef enum _HTTP_HEADER_ID {
    HttpHeaderCacheControl = 0,
    HttpHeaderConnection = 1,
    HttpHeaderDate = 2,
    HttpHeaderKeepAlive = 3,
    HttpHeaderPragma = 4,
    HttpHeaderTrailer = 5,
    HttpHeaderTransferEncoding = 6,
    HttpHeaderUpgrade = 7,
    HttpHeaderVia = 8,
    HttpHeaderWarning = 9,
    HttpHeaderAllow = 10,
    HttpHeaderContentLength = 11,
    HttpHeaderContentType = 12,
    HttpHeaderContentEncoding = 13,
    HttpHeaderContentLanguage = 14,
    HttpHeaderContentLocation = 15,
    HttpHeaderContentMd5 = 16,
    HttpHeaderContentRange = 17,
    HttpHeaderExpires = 18,
    HttpHeaderLastModified = 19,
    HttpHeaderAccept = 20,
    HttpHeaderAcceptCharset = 21,
    HttpHeaderAcceptEncoding = 22,
    HttpHeaderAcceptLanguage = 23,
    HttpHeaderAuthorization = 24,
    HttpHeaderCookie = 25,
    HttpHeaderExpect = 26,
    HttpHeaderFrom = 27,
    HttpHeaderHost = 28,
    HttpHeaderIfMatch = 29,
    HttpHeaderIfModifiedSince = 30,
    HttpHeaderIfNoneMatch = 31,
    HttpHeaderIfRange = 32,
    HttpHeaderIfUnmodifiedSince = 33,
    HttpHeaderMaxForwards = 34,
    HttpHeaderProxyAuthorization = 35,
    HttpHeaderReferer = 36,
    HttpHeaderRange = 37,
    HttpHeaderTe = 38,
    HttpHeaderTranslate = 39,
    HttpHeaderUserAgent = 40,
    HttpHeaderRequestMaximum = 41,
    HttpHeaderAcceptRanges = 20,
    HttpHeaderAge = 21,
    HttpHeaderEtag = 22,
    HttpHeaderLocation = 23,
    HttpHeaderProxyAuthenticate = 24,
    HttpHeaderRetryAfter = 25,
    HttpHeaderServer = 26,
    HttpHeaderSetCookie = 27,
    HttpHeaderVary = 28,
    HttpHeaderWwwAuthenticate = 29,
    HttpHeaderResponseMaximum = 30,
    HttpHeaderMaximum = 41
} HTTP_HEADER_ID, *PHTTP_HEADER_ID;

typedef struct _HTTP_KNOWN_HEADER {
    USHORT RawValueLength;
    PCSTR pRawValue;
} HTTP_KNOWN_HEADER, *PHTTP_KNOWN_HEADER;

typedef struct _HTTP_UNKNOWN_HEADER {
    USHORT NameLength;
    USHORT RawValueLength;
    PCSTR pName;
    PCSTR pRawValue;
} HTTP_UNKNOWN_HEADER, *PHTTP_UNKNOWN_HEADER;

typedef enum _HTTP_DATA_CHUNK_TYPE {
    HttpDataChunkFromMemory,
    HttpDataChunkFromFileHandle,
    HttpDataChunkFromFragmentCache,
    HttpDataChunkMaximum
} HTTP_DATA_CHUNK_TYPE, *PHTTP_DATA_CHUNK_TYPE;

typedef struct _HTTP_DATA_CHUNK {
    HTTP_DATA_CHUNK_TYPE DataChunkType;
    union {
        struct {
            PVOID pBuffer;
            ULONG BufferLength;
        } FromMemory;
        struct {
            HTTP_BYTE_RANGE ByteRange;
            HANDLE FileHandle;
        } FromFileHandle;
        struct {
            USHORT FragmentNameLength;
            PCWSTR pFragmentName;
        } FromFragmentCache;
    };
} HTTP_DATA_CHUNK, *PHTTP_DATA_CHUNK;

typedef struct _HTTP_REQUEST_HEADERS {
    USHORT UnknownHeaderCount;
    PHTTP_UNKNOWN_HEADER pUnknownHeaders;
    USHORT TrailerCount;
    PHTTP_UNKNOWN_HEADER pTrailers;
    HTTP_KNOWN_HEADER KnownHeaders[HttpHeaderRequestMaximum];
} HTTP_REQUEST_HEADERS, *PHTTP_REQUEST_HEADERS;

typedef struct _HTTP_RESPONSE_HEADERS {
    USHORT UnknownHeaderCount;
    PHTTP_UNKNOWN_HEADER pUnknownHeaders;
    USHORT TrailerCount;
    PHTTP_UNKNOWN_HEADER pTrailers;
    HTTP_KNOWN_HEADER KnownHeaders[HttpHeaderResponseMaximum];
} HTTP_RESPONSE_HEADERS, *PHTTP_RESPONSE_HEADERS;

typedef struct _HTTP_TRANSPORT_ADDRESS {
    PSOCKADDR pRemoteAddress;
    PSOCKADDR pLocalAddress;
} HTTP_TRANSPORT_ADDRESS, *PHTTP_TRANSPORT_ADDRESS;

typedef struct _HTTP_COOKED_URL {
    USHORT FullUrlLength;
    USHORT HostLength;
    USHORT AbsPathLength;
    USHORT QueryStringLength;
    PCWSTR pFullUrl;
    PCWSTR pHost;
    PCWSTR pAbsPath;
    PCWSTR pQueryString;
} HTTP_COOKED_URL, *PHTTP_COOKED_URL;

typedef ULONGLONG HTTP_URL_CONTEXT;

typedef struct _HTTP_SSL_CLIENT_CERT_INFO {
    ULONG CertFlags;
    ULONG CertEncodedSize;
    PUCHAR pCertEncoded;
    HANDLE Token;
    BOOLEAN CertDeniedByMapper;
} HTTP_SSL_CLIENT_CERT_INFO, *PHTTP_SSL_CLIENT_CERT_INFO;

typedef struct _HTTP_SSL_INFO {
    USHORT ServerCertKeySize;
    USHORT ConnectionKeySize;
    ULONG ServerCertIssuerSize;
    ULONG ServerCertSubjectSize;
    PCSTR pServerCertIssuer;
    PCSTR pServerCertSubject;
    PHTTP_SSL_CLIENT_CERT_INFO pClientCertInfo;
    ULONG SslClientCertNegotiated;
} HTTP_SSL_INFO, *PHTTP_SSL_INFO;

typedef struct _HTTP_REQUEST {
    ULONG Flags;
    HTTP_CONNECTION_ID ConnectionId;
    HTTP_REQUEST_ID RequestId;
    HTTP_URL_CONTEXT UrlContext;
    HTTP_VERSION Version;
    HTTP_VERB Verb;
    USHORT UnknownVerbLength;
    USHORT RawUrlLength;
    PCSTR pUnknownVerb;
    PCSTR pRawUrl;
    HTTP_COOKED_URL CookedUrl;
    HTTP_TRANSPORT_ADDRESS Address;
    HTTP_REQUEST_HEADERS Headers;
    ULONGLONG BytesReceived;
    USHORT EntityChunkCount;
    PHTTP_DATA_CHUNK pEntityChunks;
    HTTP_RAW_CONNECTION_ID RawConnectionId;
    PHTTP_SSL_INFO pSslInfo;
} HTTP_REQUEST, *PHTTP_REQUEST;

typedef struct _HTTP_RESPONSE {
    ULONG Flags;
    HTTP_VERSION Version;
    USHORT StatusCode;
    USHORT ReasonLength;
    PCSTR pReason;
    HTTP_RESPONSE_HEADERS Headers;
    USHORT EntityChunkCount;
    PHTTP_DATA_CHUNK pEntityChunks;
} HTTP_RESPONSE, *PHTTP_RESPONSE;

typedef enum _HTTP_CACHE_POLICY_TYPE {
    HttpCachePolicyNocache,
    HttpCachePolicyUserInvalidates,
    HttpCachePolicyTimeToLive,
    HttpCachePolicyMaximum
} HTTP_CACHE_POLICY_TYPE, *PHTTP_CACHE_POLICY_TYPE;

typedef struct _HTTP_CACHE_POLICY {
    HTTP_CACHE_POLICY_TYPE Policy;
    ULONG SecondsToLive;
} HTTP_CACHE_POLICY, *PHTTP_CACHE_POLICY;

typedef enum _HTTP_SERVICE_CONFIG_ID {
    HttpServiceConfigIPListenList,
    HttpServiceConfigSSLCertInfo,
    HttpServiceConfigUrlAclInfo,
    HttpServiceConfigMax
} HTTP_SERVICE_CONFIG_ID, *PHTTP_SERVICE_CONFIG_ID;

typedef enum _HTTP_SERVICE_CONFIG_QUERY_TYPE {
    HttpServiceConfigQueryExact,
    HttpServiceConfigQueryNext,
    HttpServiceConfigQueryMax
} HTTP_SERVICE_CONFIG_QUERY_TYPE, *PHTTP_SERVICE_CONFIG_QUERY_TYPE;

typedef struct _HTTP_SERVICE_CONFIG_SSL_KEY {
    PSOCKADDR pIpPort;
} HTTP_SERVICE_CONFIG_SSL_KEY, *PHTTP_SERVICE_CONFIG_SSL_KEY;

typedef struct _HTTP_SERVICE_CONFIG_SSL_PARAM {
    ULONG SslHashLength;
    PVOID pSslHash;
    GUID AppId;
    PWSTR pSslCertStoreName;
    DWORD DefaultCertCheckMode;
    DWORD DefaultRevocationFreshnessTime;
    DWORD DefaultRevocationUrlRetrievalTimeout;
    PWSTR pDefaultSslCtlIdentifier;
    PWSTR pDefaultSslCtlStoreName;
    DWORD DefaultFlags;
} HTTP_SERVICE_CONFIG_SSL_PARAM, *PHTTP_SERVICE_CONFIG_SSL_PARAM;

typedef struct _HTTP_SERVICE_CONFIG_SSL_SET {
    HTTP_SERVICE_CONFIG_SSL_KEY KeyDesc;
    HTTP_SERVICE_CONFIG_SSL_PARAM ParamDesc;
} HTTP_SERVICE_CONFIG_SSL_SET, *PHTTP_SERVICE_CONFIG_SSL_SET;

typedef struct _HTTP_SERVICE_CONFIG_SSL_QUERY {
    HTTP_SERVICE_CONFIG_QUERY_TYPE QueryDesc;
    HTTP_SERVICE_CONFIG_SSL_KEY KeyDesc;
    DWORD dwToken;
} HTTP_SERVICE_CONFIG_SSL_QUERY, *PHTTP_SERVICE_CONFIG_SSL_QUERY;

typedef struct _HTTP_SERVICE_CONFIG_IP_LISTEN_PARAM {
    USHORT AddrLength;
    PSOCKADDR pAddress;
} HTTP_SERVICE_CONFIG_IP_LISTEN_PARAM, *PHTTP_SERVICE_CONFIG_IP_LISTEN_PARAM;

typedef struct _HTTP_SERVICE_CONFIG_IP_LISTEN_QUERY {
    ULONG AddrCount;
    SOCKADDR_STORAGE AddrList[ANYSIZE_ARRAY];
} HTTP_SERVICE_CONFIG_IP_LISTEN_QUERY, *PHTTP_SERVICE_CONFIG_IP_LISTEN_QUERY;

typedef struct _HTTP_SERVICE_CONFIG_URLACL_KEY {
    PWSTR pUrlPrefix;
} HTTP_SERVICE_CONFIG_URLACL_KEY, *PHTTP_SERVICE_CONFIG_URLACL_KEY;

typedef struct _HTTP_SERVICE_CONFIG_URLACL_PARAM {
    PWSTR pStringSecurityDescriptor;
} HTTP_SERVICE_CONFIG_URLACL_PARAM, *PHTTP_SERVICE_CONFIG_URLACL_PARAM;

typedef struct _HTTP_SERVICE_CONFIG_URLACL_SET {
    HTTP_SERVICE_CONFIG_URLACL_KEY   KeyDesc;
    HTTP_SERVICE_CONFIG_URLACL_PARAM ParamDesc;
} HTTP_SERVICE_CONFIG_URLACL_SET, *PHTTP_SERVICE_CONFIG_URLACL_SET;

typedef struct _HTTP_SERVICE_CONFIG_URLACL_QUERY {
    HTTP_SERVICE_CONFIG_QUERY_TYPE QueryDesc;
    HTTP_SERVICE_CONFIG_URLACL_KEY KeyDesc;
    DWORD dwToken;
} HTTP_SERVICE_CONFIG_URLACL_QUERY, *PHTTP_SERVICE_CONFIG_URLACL_QUERY;

#if !defined(HTTPAPI_LINKAGE)
#define HTTPAPI_LINKAGE DECLSPEC_IMPORT
#endif

#define HTTPAPI_VERSION_1  {1,0}

typedef struct _HTTPAPI_VERSION {
    USHORT HttpApiMajorVersion;
    USHORT HttpApiMinorVersion;
} HTTPAPI_VERSION, *PHTTPAPI_VERSION;

HTTPAPI_LINKAGE ULONG WINAPI HttpInitialize(HTTPAPI_VERSION,ULONG,PVOID);
HTTPAPI_LINKAGE ULONG WINAPI HttpTerminate(ULONG,PVOID);
HTTPAPI_LINKAGE ULONG WINAPI HttpCreateHttpHandle(PHANDLE,ULONG);
HTTPAPI_LINKAGE ULONG WINAPI HttpReceiveClientCertificate(HANDLE,HTTP_CONNECTION_ID,ULONG,PHTTP_SSL_CLIENT_CERT_INFO,ULONG,PULONG,LPOVERLAPPED);
HTTPAPI_LINKAGE ULONG WINAPI HttpAddUrl(HANDLE,PCWSTR,PVOID);
HTTPAPI_LINKAGE ULONG WINAPI HttpRemoveUrl(HANDLE,PCWSTR);
HTTPAPI_LINKAGE ULONG WINAPI HttpReceiveHttpRequest(HANDLE,HTTP_REQUEST_ID,ULONG,PHTTP_REQUEST,ULONG,PULONG,LPOVERLAPPED);
HTTPAPI_LINKAGE ULONG WINAPI HttpReceiveRequestEntityBody(HANDLE,HTTP_REQUEST_ID,ULONG,PVOID,ULONG,PULONG,LPOVERLAPPED);
HTTPAPI_LINKAGE ULONG WINAPI HttpSendHttpResponse(HANDLE,HTTP_REQUEST_ID,ULONG,PHTTP_RESPONSE,PVOID,PULONG,PVOID,ULONG,LPOVERLAPPED,PVOID);
HTTPAPI_LINKAGE ULONG WINAPI HttpSendResponseEntityBody(HANDLE,HTTP_REQUEST_ID,ULONG,USHORT,PHTTP_DATA_CHUNK,PULONG,PVOID,ULONG,LPOVERLAPPED,PVOID);
HTTPAPI_LINKAGE ULONG WINAPI HttpWaitForDisconnect(HANDLE,HTTP_CONNECTION_ID,LPOVERLAPPED);
HTTPAPI_LINKAGE ULONG WINAPI HttpFlushResponseCache(HANDLE,PCWSTR,ULONG,LPOVERLAPPED);
HTTPAPI_LINKAGE ULONG WINAPI HttpAddFragmentToCache(HANDLE,PCWSTR,PHTTP_DATA_CHUNK,PHTTP_CACHE_POLICY,LPOVERLAPPED);
HTTPAPI_LINKAGE ULONG WINAPI HttpReadFragmentFromCache(HANDLE,PCWSTR,PHTTP_BYTE_RANGE,PVOID,ULONG,PULONG,LPOVERLAPPED);
HTTPAPI_LINKAGE ULONG WINAPI HttpSetServiceConfiguration(HANDLE,HTTP_SERVICE_CONFIG_ID,PVOID,ULONG,LPOVERLAPPED);
HTTPAPI_LINKAGE ULONG WINAPI HttpDeleteServiceConfiguration(HANDLE,HTTP_SERVICE_CONFIG_ID,PVOID,ULONG,LPOVERLAPPED);
HTTPAPI_LINKAGE ULONG WINAPI HttpQueryServiceConfiguration(HANDLE,HTTP_SERVICE_CONFIG_ID,PVOID,ULONG,PVOID,ULONG,PULONG,LPOVERLAPPED);

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
}
#endif

#endif /* _HTTP_H */
