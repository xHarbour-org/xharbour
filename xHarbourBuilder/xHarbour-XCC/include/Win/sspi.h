#ifndef _SSPI_H
#define _SSPI_H

/* Security Support Provider Interface (SSPI) definitions */

#ifdef __cplusplus
extern "C" {
#endif

#ifdef SECURITY_WIN32
#define ISSP_LEVEL  32
#define ISSP_MODE  1
#endif

#ifdef SECURITY_KERNEL
#define ISSP_LEVEL  32

#ifdef ISSP_MODE
#undef ISSP_MODE
#endif
#define ISSP_MODE  0
#endif /* SECURITY_KERNEL */

#ifndef ISSP_LEVEL
#error  You must define one of SECURITY_WIN32, SECURITY_KERNEL
#endif

typedef WCHAR SEC_WCHAR;
typedef CHAR SEC_CHAR;

typedef LONG SECURITY_STATUS;

#define SEC_TEXT TEXT
#define SEC_ENTRY __stdcall

#ifdef UNICODE
typedef SEC_WCHAR *SECURITY_PSTR;
typedef CONST SEC_WCHAR *SECURITY_PCSTR;
#else
typedef SEC_CHAR *SECURITY_PSTR;
typedef CONST SEC_CHAR *SECURITY_PCSTR;
#endif /* UNICODE */

#ifndef __SECHANDLE_DEFINED__
#define __SECHANDLE_DEFINED__
typedef struct _SecHandle {
    ULONG_PTR dwLower;
    ULONG_PTR dwUpper;
} SecHandle, *PSecHandle;
#endif /* __SECHANDLE_DEFINED__ */

#define SecInvalidateHandle(x)  ((PSecHandle) x)->dwLower = (ULONG_PTR) -1 ; ((PSecHandle) x)->dwUpper = (ULONG_PTR) -1;

typedef SecHandle CredHandle;
typedef PSecHandle PCredHandle;

typedef SecHandle CtxtHandle;
typedef PSecHandle PCtxtHandle;

#ifdef WIN32_CHICAGO
typedef unsigned __int64 QWORD;
typedef QWORD SECURITY_INTEGER, *PSECURITY_INTEGER;
#define SEC_SUCCESS(Status) ((Status) >= 0)
#elif defined(_NTDEF_) || defined(_WINNT_)
typedef LARGE_INTEGER _SECURITY_INTEGER, SECURITY_INTEGER, *PSECURITY_INTEGER;
#else /* _NTDEF_ || _WINNT_ */
typedef struct _SECURITY_INTEGER {
    unsigned long LowPart;
    long HighPart;
} SECURITY_INTEGER, *PSECURITY_INTEGER;
#endif /* _NTDEF_ || _WINNT_ */

typedef SECURITY_INTEGER TimeStamp;
typedef SECURITY_INTEGER *PTimeStamp;

#ifndef _NTDEF_
typedef struct _SECURITY_STRING {
    unsigned short Length;
    unsigned short MaximumLength;
    unsigned short *Buffer;
} SECURITY_STRING, *PSECURITY_STRING;
#else /* _NTDEF_ */
typedef UNICODE_STRING SECURITY_STRING, *PSECURITY_STRING;
#endif /* _NTDEF_ */

typedef struct _SecPkgInfoW {
    unsigned long fCapabilities;
    unsigned short wVersion;
    unsigned short wRPCID;
    unsigned long cbMaxToken;
    SEC_WCHAR *Name;
    SEC_WCHAR *Comment;
} SecPkgInfoW, *PSecPkgInfoW;

typedef struct _SecPkgInfoA {
    unsigned long fCapabilities;
    unsigned short wVersion;
    unsigned short wRPCID;
    unsigned long cbMaxToken;
    SEC_CHAR *Name;
    SEC_CHAR *Comment;
} SecPkgInfoA, *PSecPkgInfoA;

#ifdef UNICODE
#define SecPkgInfo SecPkgInfoW
#define PSecPkgInfo PSecPkgInfoW
#else
#define SecPkgInfo SecPkgInfoA
#define PSecPkgInfo PSecPkgInfoA
#endif /* UNICODE */

#define SECPKG_FLAG_INTEGRITY  0x00000001
#define SECPKG_FLAG_PRIVACY  0x00000002
#define SECPKG_FLAG_TOKEN_ONLY  0x00000004
#define SECPKG_FLAG_DATAGRAM  0x00000008
#define SECPKG_FLAG_CONNECTION  0x00000010
#define SECPKG_FLAG_MULTI_REQUIRED  0x00000020
#define SECPKG_FLAG_CLIENT_ONLY  0x00000040
#define SECPKG_FLAG_EXTENDED_ERROR  0x00000080
#define SECPKG_FLAG_IMPERSONATION  0x00000100
#define SECPKG_FLAG_ACCEPT_WIN32_NAME  0x00000200
#define SECPKG_FLAG_STREAM  0x00000400
#define SECPKG_FLAG_NEGOTIABLE  0x00000800
#define SECPKG_FLAG_GSS_COMPATIBLE  0x00001000
#define SECPKG_FLAG_LOGON  0x00002000
#define SECPKG_FLAG_ASCII_BUFFERS  0x00004000
#define SECPKG_FLAG_FRAGMENT  0x00008000
#define SECPKG_FLAG_MUTUAL_AUTH  0x00010000
#define SECPKG_FLAG_DELEGATION  0x00020000

#define SECPKG_ID_NONE  0xFFFF

typedef struct _SecBuffer {
    unsigned long cbBuffer;
    unsigned long BufferType;
    void *pvBuffer;
} SecBuffer, *PSecBuffer;

typedef struct _SecBufferDesc {
    unsigned long ulVersion;
    unsigned long cBuffers;
    PSecBuffer pBuffers;
} SecBufferDesc, *PSecBufferDesc;

#define SECBUFFER_VERSION  0

#define SECBUFFER_EMPTY  0
#define SECBUFFER_DATA  1
#define SECBUFFER_TOKEN  2
#define SECBUFFER_PKG_PARAMS  3
#define SECBUFFER_MISSING  4
#define SECBUFFER_EXTRA  5
#define SECBUFFER_STREAM_TRAILER  6
#define SECBUFFER_STREAM_HEADER  7
#define SECBUFFER_NEGOTIATION_INFO  8
#define SECBUFFER_PADDING  9
#define SECBUFFER_STREAM  10
#define SECBUFFER_MECHLIST  11
#define SECBUFFER_MECHLIST_SIGNATURE 12

#define SECBUFFER_ATTRMASK  0xF0000000
#define SECBUFFER_READONLY  0x80000000
#define SECBUFFER_RESERVED  0x40000000

typedef struct _SEC_NEGOTIATION_INFO {
    unsigned long Size;
    unsigned long NameLength;
    SEC_WCHAR *Name;
    void *Reserved;
} SEC_NEGOTIATION_INFO, *PSEC_NEGOTIATION_INFO;

#define SECURITY_NATIVE_DREP  0x00000010
#define SECURITY_NETWORK_DREP  0x00000000

#define SECPKG_CRED_INBOUND  0x00000001
#define SECPKG_CRED_OUTBOUND  0x00000002
#define SECPKG_CRED_BOTH  0x00000003
#define SECPKG_CRED_DEFAULT  0x00000004
#define SECPKG_CRED_RESERVED  0xF0000000

#define ISC_REQ_DELEGATE  0x00000001
#define ISC_REQ_MUTUAL_AUTH  0x00000002
#define ISC_REQ_REPLAY_DETECT  0x00000004
#define ISC_REQ_SEQUENCE_DETECT  0x00000008
#define ISC_REQ_CONFIDENTIALITY  0x00000010
#define ISC_REQ_USE_SESSION_KEY  0x00000020
#define ISC_REQ_PROMPT_FOR_CREDS  0x00000040
#define ISC_REQ_USE_SUPPLIED_CREDS  0x00000080
#define ISC_REQ_ALLOCATE_MEMORY  0x00000100
#define ISC_REQ_USE_DCE_STYLE  0x00000200
#define ISC_REQ_DATAGRAM  0x00000400
#define ISC_REQ_CONNECTION  0x00000800
#define ISC_REQ_CALL_LEVEL  0x00001000
#define ISC_REQ_FRAGMENT_SUPPLIED  0x00002000
#define ISC_REQ_EXTENDED_ERROR  0x00004000
#define ISC_REQ_STREAM  0x00008000
#define ISC_REQ_INTEGRITY  0x00010000
#define ISC_REQ_IDENTIFY  0x00020000
#define ISC_REQ_NULL_SESSION  0x00040000
#define ISC_REQ_MANUAL_CRED_VALIDATION  0x00080000
#define ISC_REQ_RESERVED1  0x00100000
#define ISC_REQ_FRAGMENT_TO_FIT  0x00200000

#define ISC_RET_DELEGATE  0x00000001
#define ISC_RET_MUTUAL_AUTH  0x00000002
#define ISC_RET_REPLAY_DETECT  0x00000004
#define ISC_RET_SEQUENCE_DETECT  0x00000008
#define ISC_RET_CONFIDENTIALITY  0x00000010
#define ISC_RET_USE_SESSION_KEY  0x00000020
#define ISC_RET_USED_COLLECTED_CREDS  0x00000040
#define ISC_RET_USED_SUPPLIED_CREDS  0x00000080
#define ISC_RET_ALLOCATED_MEMORY  0x00000100
#define ISC_RET_USED_DCE_STYLE  0x00000200
#define ISC_RET_DATAGRAM  0x00000400
#define ISC_RET_CONNECTION  0x00000800
#define ISC_RET_INTERMEDIATE_RETURN  0x00001000
#define ISC_RET_CALL_LEVEL  0x00002000
#define ISC_RET_EXTENDED_ERROR  0x00004000
#define ISC_RET_STREAM  0x00008000
#define ISC_RET_INTEGRITY  0x00010000
#define ISC_RET_IDENTIFY  0x00020000
#define ISC_RET_NULL_SESSION  0x00040000
#define ISC_RET_MANUAL_CRED_VALIDATION  0x00080000
#define ISC_RET_RESERVED1  0x00100000
#define ISC_RET_FRAGMENT_ONLY  0x00200000

#define ASC_REQ_DELEGATE  0x00000001
#define ASC_REQ_MUTUAL_AUTH  0x00000002
#define ASC_REQ_REPLAY_DETECT  0x00000004
#define ASC_REQ_SEQUENCE_DETECT  0x00000008
#define ASC_REQ_CONFIDENTIALITY  0x00000010
#define ASC_REQ_USE_SESSION_KEY  0x00000020
#define ASC_REQ_ALLOCATE_MEMORY  0x00000100
#define ASC_REQ_USE_DCE_STYLE  0x00000200
#define ASC_REQ_DATAGRAM  0x00000400
#define ASC_REQ_CONNECTION  0x00000800
#define ASC_REQ_CALL_LEVEL  0x00001000
#define ASC_REQ_EXTENDED_ERROR  0x00008000
#define ASC_REQ_STREAM  0x00010000
#define ASC_REQ_INTEGRITY  0x00020000
#define ASC_REQ_LICENSING  0x00040000
#define ASC_REQ_IDENTIFY  0x00080000
#define ASC_REQ_ALLOW_NULL_SESSION  0x00100000
#define ASC_REQ_ALLOW_NON_USER_LOGONS  0x00200000
#define ASC_REQ_ALLOW_CONTEXT_REPLAY  0x00400000
#define ASC_REQ_FRAGMENT_TO_FIT  0x00800000
#define ASC_REQ_FRAGMENT_SUPPLIED  0x00002000

#define ASC_RET_DELEGATE  0x00000001
#define ASC_RET_MUTUAL_AUTH  0x00000002
#define ASC_RET_REPLAY_DETECT  0x00000004
#define ASC_RET_SEQUENCE_DETECT  0x00000008
#define ASC_RET_CONFIDENTIALITY  0x00000010
#define ASC_RET_USE_SESSION_KEY  0x00000020
#define ASC_RET_ALLOCATED_MEMORY  0x00000100
#define ASC_RET_USED_DCE_STYLE  0x00000200
#define ASC_RET_DATAGRAM  0x00000400
#define ASC_RET_CONNECTION  0x00000800
#define ASC_RET_CALL_LEVEL  0x00002000
#define ASC_RET_THIRD_LEG_FAILED  0x00004000
#define ASC_RET_EXTENDED_ERROR  0x00008000
#define ASC_RET_STREAM  0x00010000
#define ASC_RET_INTEGRITY  0x00020000
#define ASC_RET_LICENSING  0x00040000
#define ASC_RET_IDENTIFY  0x00080000
#define ASC_RET_NULL_SESSION  0x00100000
#define ASC_RET_ALLOW_NON_USER_LOGONS  0x00200000
#define ASC_RET_ALLOW_CONTEXT_REPLAY  0x00400000
#define ASC_RET_FRAGMENT_ONLY  0x00800000

#define SECPKG_CRED_ATTR_NAMES  1

typedef struct _SecPkgCredentials_NamesW {
    SEC_WCHAR *sUserName;
} SecPkgCredentials_NamesW, *PSecPkgCredentials_NamesW;

typedef struct _SecPkgCredentials_NamesA {
    SEC_CHAR *sUserName;
} SecPkgCredentials_NamesA, *PSecPkgCredentials_NamesA;

#ifdef UNICODE
#define SecPkgCredentials_Names SecPkgCredentials_NamesW
#define PSecPkgCredentials_Names PSecPkgCredentials_NamesW
#else
#define SecPkgCredentials_Names SecPkgCredentials_NamesA
#define PSecPkgCredentials_Names PSecPkgCredentials_NamesA
#endif /* UNICODE */

#define SECPKG_ATTR_SIZES  0
#define SECPKG_ATTR_NAMES  1
#define SECPKG_ATTR_LIFESPAN  2
#define SECPKG_ATTR_DCE_INFO  3
#define SECPKG_ATTR_STREAM_SIZES  4
#define SECPKG_ATTR_KEY_INFO  5
#define SECPKG_ATTR_AUTHORITY  6
#define SECPKG_ATTR_PROTO_INFO  7
#define SECPKG_ATTR_PASSWORD_EXPIRY 8
#define SECPKG_ATTR_SESSION_KEY  9
#define SECPKG_ATTR_PACKAGE_INFO  10
#define SECPKG_ATTR_USER_FLAGS  11
#define SECPKG_ATTR_NEGOTIATION_INFO 12
#define SECPKG_ATTR_NATIVE_NAMES  13
#define SECPKG_ATTR_FLAGS  14

typedef struct _SecPkgContext_Sizes {
    unsigned long cbMaxToken;
    unsigned long cbMaxSignature;
    unsigned long cbBlockSize;
    unsigned long cbSecurityTrailer;
} SecPkgContext_Sizes, *PSecPkgContext_Sizes;

typedef struct _SecPkgContext_StreamSizes {
    unsigned long cbHeader;
    unsigned long cbTrailer;
    unsigned long cbMaximumMessage;
    unsigned long cBuffers;
    unsigned long cbBlockSize;
} SecPkgContext_StreamSizes, *PSecPkgContext_StreamSizes;

typedef struct _SecPkgContext_NamesW {
    SEC_WCHAR *sUserName;
} SecPkgContext_NamesW, *PSecPkgContext_NamesW;

typedef struct _SecPkgContext_NamesA {
    SEC_CHAR *sUserName;
} SecPkgContext_NamesA, *PSecPkgContext_NamesA;

#ifdef UNICODE
#define SecPkgContext_Names SecPkgContext_NamesW
#define PSecPkgContext_Names PSecPkgContext_NamesW
#else
#define SecPkgContext_Names SecPkgContext_NamesA
#define PSecPkgContext_Names PSecPkgContext_NamesA
#endif /* UNICODE */

typedef struct _SecPkgContext_Lifespan {
    TimeStamp tsStart;
    TimeStamp tsExpiry;
} SecPkgContext_Lifespan, *PSecPkgContext_Lifespan;

typedef struct _SecPkgContext_DceInfo {
    unsigned long AuthzSvc;
    void *pPac;
} SecPkgContext_DceInfo, *PSecPkgContext_DceInfo;

typedef struct _SecPkgContext_KeyInfoA {
    SEC_CHAR *sSignatureAlgorithmName;
    SEC_CHAR *sEncryptAlgorithmName;
    unsigned long KeySize;
    unsigned long SignatureAlgorithm;
    unsigned long EncryptAlgorithm;
} SecPkgContext_KeyInfoA, *PSecPkgContext_KeyInfoA;

typedef struct _SecPkgContext_KeyInfoW {
    SEC_WCHAR *sSignatureAlgorithmName;
    SEC_WCHAR *sEncryptAlgorithmName;
    unsigned long KeySize;
    unsigned long SignatureAlgorithm;
    unsigned long EncryptAlgorithm;
} SecPkgContext_KeyInfoW, *PSecPkgContext_KeyInfoW;

#ifdef UNICODE
#define SecPkgContext_KeyInfo  SecPkgContext_KeyInfoW
#define PSecPkgContext_KeyInfo  PSecPkgContext_KeyInfoW
#else
#define SecPkgContext_KeyInfo  SecPkgContext_KeyInfoA
#define PSecPkgContext_KeyInfo  PSecPkgContext_KeyInfoA
#endif

typedef struct _SecPkgContext_AuthorityA {
    SEC_CHAR *sAuthorityName;
} SecPkgContext_AuthorityA, *PSecPkgContext_AuthorityA;

typedef struct _SecPkgContext_AuthorityW {
    SEC_WCHAR *sAuthorityName;
} SecPkgContext_AuthorityW, *PSecPkgContext_AuthorityW;

#ifdef UNICODE
#define SecPkgContext_Authority SecPkgContext_AuthorityW
#define PSecPkgContext_Authority  PSecPkgContext_AuthorityW
#else
#define SecPkgContext_Authority SecPkgContext_AuthorityA
#define PSecPkgContext_Authority  PSecPkgContext_AuthorityA
#endif

typedef struct _SecPkgContext_ProtoInfoA {
    SEC_CHAR *sProtocolName;
    unsigned long majorVersion;
    unsigned long minorVersion;
} SecPkgContext_ProtoInfoA, *PSecPkgContext_ProtoInfoA;

typedef struct _SecPkgContext_ProtoInfoW {
    SEC_WCHAR *sProtocolName;
    unsigned long majorVersion;
    unsigned long minorVersion;
} SecPkgContext_ProtoInfoW, *PSecPkgContext_ProtoInfoW;

#ifdef UNICODE
#define SecPkgContext_ProtoInfo  SecPkgContext_ProtoInfoW
#define PSecPkgContext_ProtoInfo  PSecPkgContext_ProtoInfoW
#else
#define SecPkgContext_ProtoInfo  SecPkgContext_ProtoInfoA
#define PSecPkgContext_ProtoInfo  PSecPkgContext_ProtoInfoA
#endif

typedef struct _SecPkgContext_PasswordExpiry {
    TimeStamp tsPasswordExpires;
} SecPkgContext_PasswordExpiry, *PSecPkgContext_PasswordExpiry;

typedef struct _SecPkgContext_SessionKey {
    unsigned long SessionKeyLength;
    unsigned char *SessionKey;
} SecPkgContext_SessionKey, *PSecPkgContext_SessionKey;

typedef struct _SecPkgContext_PackageInfoW {
    PSecPkgInfoW PackageInfo;
} SecPkgContext_PackageInfoW, *PSecPkgContext_PackageInfoW;

typedef struct _SecPkgContext_PackageInfoA {
    PSecPkgInfoA PackageInfo;
} SecPkgContext_PackageInfoA, *PSecPkgContext_PackageInfoA;

typedef struct _SecPkgContext_UserFlags {
    unsigned long UserFlags;
} SecPkgContext_UserFlags, *PSecPkgContext_UserFlags;

typedef struct _SecPkgContext_Flags {
    unsigned long Flags;
} SecPkgContext_Flags, *PSecPkgContext_Flags;

#ifdef UNICODE
#define SecPkgContext_PackageInfo  SecPkgContext_PackageInfoW
#define PSecPkgContext_PackageInfo  PSecPkgContext_PackageInfoW
#else
#define SecPkgContext_PackageInfo  SecPkgContext_PackageInfoA
#define PSecPkgContext_PackageInfo  PSecPkgContext_PackageInfoA
#endif

typedef struct _SecPkgContext_NegotiationInfoA {
    PSecPkgInfoA PackageInfo;
    unsigned long NegotiationState;
} SecPkgContext_NegotiationInfoA, *PSecPkgContext_NegotiationInfoA;

typedef struct _SecPkgContext_NegotiationInfoW {
    PSecPkgInfoW PackageInfo;
    unsigned long NegotiationState;
} SecPkgContext_NegotiationInfoW, *PSecPkgContext_NegotiationInfoW;

#ifdef UNICODE
#define SecPkgContext_NegotiationInfo  SecPkgContext_NegotiationInfoW
#define PSecPkgContext_NegotiationInfo  PSecPkgContext_NegotiationInfoW
#else
#define SecPkgContext_NegotiationInfo  SecPkgContext_NegotiationInfoA
#define PSecPkgContext_NegotiationInfo  PSecPkgContext_NegotiationInfoA
#endif

#define SECPKG_NEGOTIATION_COMPLETE  0
#define SECPKG_NEGOTIATION_OPTIMISTIC  1
#define SECPKG_NEGOTIATION_IN_PROGRESS  2
#define SECPKG_NEGOTIATION_DIRECT  3

typedef struct _SecPkgContext_NativeNamesW {
    SEC_WCHAR *sClientName;
    SEC_WCHAR *sServerName;
} SecPkgContext_NativeNamesW, *PSecPkgContext_NativeNamesW;

typedef struct _SecPkgContext_NativeNamesA {
    SEC_CHAR *sClientName;
    SEC_CHAR *sServerName;
} SecPkgContext_NativeNamesA, *PSecPkgContext_NativeNamesA;

#ifdef UNICODE
#define SecPkgContext_NativeNames SecPkgContext_NativeNamesW
#define PSecPkgContext_NativeNames PSecPkgContext_NativeNamesW
#else
#define SecPkgContext_NativeNames SecPkgContext_NativeNamesA
#define PSecPkgContext_NativeNames PSecPkgContext_NativeNamesA
#endif /* UNICODE */

typedef void (SEC_ENTRY *SEC_GET_KEY_FN)(void*,void*,unsigned long,void**,SECURITY_STATUS*);

#define SECPKG_CONTEXT_EXPORT_RESET_NEW  0x00000001
#define SECPKG_CONTEXT_EXPORT_DELETE_OLD  0x00000002

SECURITY_STATUS SEC_ENTRY AcquireCredentialsHandleW(
#if ISSP_MODE == 0
    PSECURITY_STRING, PSECURITY_STRING,
#else
    SEC_WCHAR*, SEC_WCHAR*,
#endif
    unsigned long, void*, void*,SEC_GET_KEY_FN,void*,PCredHandle,PTimeStamp);

typedef SECURITY_STATUS(SEC_ENTRY *ACQUIRE_CREDENTIALS_HANDLE_FN_W)(
#if ISSP_MODE == 0
    PSECURITY_STRING, PSECURITY_STRING,
#else
    SEC_WCHAR*, SEC_WCHAR*,
#endif
    unsigned long,void*,void*,SEC_GET_KEY_FN,void*,PCredHandle,PTimeStamp);

SECURITY_STATUS SEC_ENTRY AcquireCredentialsHandleA(SEC_CHAR*,SEC_CHAR*,unsigned long,void*,void*,SEC_GET_KEY_FN,void*,PCredHandle,PTimeStamp);

typedef SECURITY_STATUS (SEC_ENTRY *ACQUIRE_CREDENTIALS_HANDLE_FN_A)(SEC_CHAR*,SEC_CHAR*,unsigned long,void*,void*,SEC_GET_KEY_FN,void*,PCredHandle,PTimeStamp);

#ifdef UNICODE
#define AcquireCredentialsHandle AcquireCredentialsHandleW
#define ACQUIRE_CREDENTIALS_HANDLE_FN ACQUIRE_CREDENTIALS_HANDLE_FN_W
#else
#define AcquireCredentialsHandle AcquireCredentialsHandleA
#define ACQUIRE_CREDENTIALS_HANDLE_FN ACQUIRE_CREDENTIALS_HANDLE_FN_A
#endif /* UNICODE */

SECURITY_STATUS SEC_ENTRY FreeCredentialsHandle(PCredHandle);

typedef SECURITY_STATUS(SEC_ENTRY * FREE_CREDENTIALS_HANDLE_FN)(PCredHandle);

SECURITY_STATUS SEC_ENTRY AddCredentialsW(PCredHandle hCredentials,
#if ISSP_MODE == 0
    PSECURITY_STRING pPrincipal, PSECURITY_STRING pPackage,
#else
    SEC_WCHAR * pszPrincipal, SEC_WCHAR * pszPackage,
#endif
    unsigned long fCredentialUse, void *pAuthData, SEC_GET_KEY_FN pGetKeyFn, void *pvGetKeyArgument, PTimeStamp ptsExpiry);

typedef SECURITY_STATUS(SEC_ENTRY * ADD_CREDENTIALS_FN_W) (PCredHandle,
#if ISSP_MODE == 0
    PSECURITY_STRING, PSECURITY_STRING,
#else
    SEC_WCHAR *, SEC_WCHAR *,
#endif
    unsigned long, void *, SEC_GET_KEY_FN, void *, PTimeStamp);

SECURITY_STATUS SEC_ENTRY AddCredentialsA(PCredHandle hCredentials, SEC_CHAR * pszPrincipal, SEC_CHAR * pszPackage, unsigned long fCredentialUse, void *pAuthData, SEC_GET_KEY_FN pGetKeyFn, void *pvGetKeyArgument, PTimeStamp ptsExpiry);

typedef SECURITY_STATUS(SEC_ENTRY * ADD_CREDENTIALS_FN_A) (PCredHandle, SEC_CHAR *, SEC_CHAR *, unsigned long, void *, SEC_GET_KEY_FN, void *, PTimeStamp);

#ifdef UNICODE
#define AddCredentials  AddCredentialsW
#define ADD_CREDENTIALS_FN  ADD_CREDENTIALS_FN_W
#else
#define AddCredentials  AddCredentialsA
#define ADD_CREDENTIALS_FN ADD_CREDENTIALS_FN_A
#endif

#ifdef WIN32_CHICAGO
SECURITY_STATUS SEC_ENTRY SspiLogonUserW(SEC_WCHAR*,SEC_WCHAR*,SEC_WCHAR*,SEC_WCHAR*);
SECURITY_STATUS SEC_ENTRY SspiLogonUserA(SEC_CHAR*,SEC_CHAR*,SEC_CHAR*,SEC_CHAR*);

typedef SECURITY_STATUS (SEC_ENTRY *SSPI_LOGON_USER_FN_W)(SEC_CHAR*,SEC_CHAR*,SEC_CHAR*,SEC_CHAR*);
typedef SECURITY_STATUS (SEC_ENTRY *SSPI_LOGON_USER_FN_A)(SEC_CHAR*,SEC_CHAR*,SEC_CHAR*,SEC_CHAR*);

#ifdef UNICODE
#define SspiLogonUser SspiLogonUserW
#define SSPI_LOGON_USER_FN SSPI_LOGON_USER_FN_W
#else
#define SspiLogonUser SspiLogonUserA
#define SSPI_LOGON_USER_FN SSPI_LOGON_USER_FN_A
#endif /* UNICODE */
#endif /* WIN32_CHICAGO */

SECURITY_STATUS SEC_ENTRY InitializeSecurityContextW(PCredHandle phCredential, PCtxtHandle phContext,
#if ISSP_MODE == 0
    PSECURITY_STRING pTargetName,
#else
    SEC_WCHAR * pszTargetName,
#endif
    unsigned long fContextReq, unsigned long Reserved1, unsigned long TargetDataRep, PSecBufferDesc pInput, unsigned long Reserved2, PCtxtHandle phNewContext, PSecBufferDesc pOutput, unsigned long *pfContextAttr, PTimeStamp ptsExpiry);

typedef SECURITY_STATUS(SEC_ENTRY * INITIALIZE_SECURITY_CONTEXT_FN_W)(PCredHandle, PCtxtHandle,
#if ISSP_MODE == 0
    PSECURITY_STRING,
#else
    SEC_WCHAR *,
#endif
    unsigned long, unsigned long, unsigned long, PSecBufferDesc, unsigned long, PCtxtHandle, PSecBufferDesc, unsigned long *, PTimeStamp);

SECURITY_STATUS SEC_ENTRY InitializeSecurityContextA(PCredHandle phCredential, PCtxtHandle phContext, SEC_CHAR * pszTargetName, unsigned long fContextReq, unsigned long Reserved1, unsigned long TargetDataRep, PSecBufferDesc pInput, unsigned long Reserved2, PCtxtHandle phNewContext, PSecBufferDesc pOutput, unsigned long *pfContextAttr, PTimeStamp ptsExpiry);

typedef SECURITY_STATUS(SEC_ENTRY * INITIALIZE_SECURITY_CONTEXT_FN_A) (PCredHandle, PCtxtHandle, SEC_CHAR *, unsigned long, unsigned long, unsigned long, PSecBufferDesc, unsigned long, PCtxtHandle, PSecBufferDesc, unsigned long *, PTimeStamp);

#ifdef UNICODE
#define InitializeSecurityContext InitializeSecurityContextW
#define INITIALIZE_SECURITY_CONTEXT_FN INITIALIZE_SECURITY_CONTEXT_FN_W
#else
#define InitializeSecurityContext InitializeSecurityContextA
#define INITIALIZE_SECURITY_CONTEXT_FN INITIALIZE_SECURITY_CONTEXT_FN_A
#endif /* UNICODE */

SECURITY_STATUS SEC_ENTRY AcceptSecurityContext(PCredHandle phCredential, PCtxtHandle phContext, PSecBufferDesc pInput, unsigned long fContextReq, unsigned long TargetDataRep, PCtxtHandle phNewContext, PSecBufferDesc pOutput, unsigned long *pfContextAttr, PTimeStamp ptsExpiry);

typedef SECURITY_STATUS(SEC_ENTRY * ACCEPT_SECURITY_CONTEXT_FN) (PCredHandle, PCtxtHandle, PSecBufferDesc, unsigned long, unsigned long, PCtxtHandle, PSecBufferDesc, unsigned long *, PTimeStamp);

SECURITY_STATUS SEC_ENTRY CompleteAuthToken(PCtxtHandle phContext, PSecBufferDesc pToken);

typedef SECURITY_STATUS(SEC_ENTRY * COMPLETE_AUTH_TOKEN_FN) (PCtxtHandle, PSecBufferDesc);

SECURITY_STATUS SEC_ENTRY ImpersonateSecurityContext(PCtxtHandle phContext);

typedef SECURITY_STATUS(SEC_ENTRY * IMPERSONATE_SECURITY_CONTEXT_FN) (PCtxtHandle);

SECURITY_STATUS SEC_ENTRY RevertSecurityContext(PCtxtHandle phContext);

typedef SECURITY_STATUS(SEC_ENTRY * REVERT_SECURITY_CONTEXT_FN) (PCtxtHandle);

SECURITY_STATUS SEC_ENTRY QuerySecurityContextToken(PCtxtHandle phContext, void **Token);

typedef SECURITY_STATUS(SEC_ENTRY * QUERY_SECURITY_CONTEXT_TOKEN_FN) (PCtxtHandle, void **);

SECURITY_STATUS SEC_ENTRY DeleteSecurityContext(PCtxtHandle phContext);

typedef SECURITY_STATUS(SEC_ENTRY * DELETE_SECURITY_CONTEXT_FN) (PCtxtHandle);

SECURITY_STATUS SEC_ENTRY ApplyControlToken(PCtxtHandle phContext, PSecBufferDesc pInput);

typedef SECURITY_STATUS(SEC_ENTRY * APPLY_CONTROL_TOKEN_FN) (PCtxtHandle, PSecBufferDesc);

SECURITY_STATUS SEC_ENTRY QueryContextAttributesW(PCtxtHandle phContext, unsigned long ulAttribute, void *pBuffer);

typedef SECURITY_STATUS(SEC_ENTRY * QUERY_CONTEXT_ATTRIBUTES_FN_W) (PCtxtHandle, unsigned long, void *);

SECURITY_STATUS SEC_ENTRY QueryContextAttributesA(PCtxtHandle phContext, unsigned long ulAttribute, void *pBuffer);

typedef SECURITY_STATUS(SEC_ENTRY * QUERY_CONTEXT_ATTRIBUTES_FN_A) (PCtxtHandle, unsigned long, void *);

#ifdef UNICODE
#define QueryContextAttributes QueryContextAttributesW
#define QUERY_CONTEXT_ATTRIBUTES_FN QUERY_CONTEXT_ATTRIBUTES_FN_W
#else
#define QueryContextAttributes QueryContextAttributesA
#define QUERY_CONTEXT_ATTRIBUTES_FN QUERY_CONTEXT_ATTRIBUTES_FN_A
#endif /* UNICODE */

SECURITY_STATUS SEC_ENTRY QueryCredentialsAttributesW(PCredHandle phCredential, unsigned long ulAttribute, void *pBuffer);

typedef SECURITY_STATUS(SEC_ENTRY * QUERY_CREDENTIALS_ATTRIBUTES_FN_W) (PCredHandle, unsigned long, void *);

SECURITY_STATUS SEC_ENTRY QueryCredentialsAttributesA(PCredHandle phCredential, unsigned long ulAttribute, void *pBuffer);

typedef SECURITY_STATUS(SEC_ENTRY * QUERY_CREDENTIALS_ATTRIBUTES_FN_A) (PCredHandle, unsigned long, void *);

#ifdef UNICODE
#define QueryCredentialsAttributes QueryCredentialsAttributesW
#define QUERY_CREDENTIALS_ATTRIBUTES_FN QUERY_CREDENTIALS_ATTRIBUTES_FN_W
#else
#define QueryCredentialsAttributes QueryCredentialsAttributesA
#define QUERY_CREDENTIALS_ATTRIBUTES_FN QUERY_CREDENTIALS_ATTRIBUTES_FN_A
#endif /* UNICODE */

SECURITY_STATUS SEC_ENTRY FreeContextBuffer(void *pvContextBuffer);

typedef SECURITY_STATUS(SEC_ENTRY * FREE_CONTEXT_BUFFER_FN) (void *);

SECURITY_STATUS SEC_ENTRY MakeSignature(PCtxtHandle phContext, unsigned long fQOP, PSecBufferDesc pMessage, unsigned long MessageSeqNo);

typedef SECURITY_STATUS(SEC_ENTRY * MAKE_SIGNATURE_FN) (PCtxtHandle, unsigned long, PSecBufferDesc, unsigned long);

SECURITY_STATUS SEC_ENTRY VerifySignature(PCtxtHandle phContext, PSecBufferDesc pMessage, unsigned long MessageSeqNo, unsigned long *pfQOP);

typedef SECURITY_STATUS(SEC_ENTRY * VERIFY_SIGNATURE_FN) (PCtxtHandle, PSecBufferDesc, unsigned long, unsigned long *);

SECURITY_STATUS SEC_ENTRY EncryptMessage(PCtxtHandle phContext, unsigned long fQOP, PSecBufferDesc pMessage, unsigned long MessageSeqNo);

typedef SECURITY_STATUS(SEC_ENTRY * ENCRYPT_MESSAGE_FN) (PCtxtHandle, unsigned long, PSecBufferDesc, unsigned long);

SECURITY_STATUS SEC_ENTRY DecryptMessage(PCtxtHandle phContext, PSecBufferDesc pMessage, unsigned long MessageSeqNo, unsigned long *pfQOP);

typedef SECURITY_STATUS(SEC_ENTRY * DECRYPT_MESSAGE_FN) (PCtxtHandle, PSecBufferDesc, unsigned long, unsigned long *);

SECURITY_STATUS SEC_ENTRY EnumerateSecurityPackagesW(unsigned long *pcPackages, PSecPkgInfoW * ppPackageInfo);

typedef SECURITY_STATUS(SEC_ENTRY * ENUMERATE_SECURITY_PACKAGES_FN_W) (unsigned long *, PSecPkgInfoW *);

SECURITY_STATUS SEC_ENTRY EnumerateSecurityPackagesA(unsigned long *pcPackages, PSecPkgInfoA * ppPackageInfo);

typedef SECURITY_STATUS(SEC_ENTRY * ENUMERATE_SECURITY_PACKAGES_FN_A) (unsigned long *, PSecPkgInfoA *);

#ifdef UNICODE
#define EnumerateSecurityPackages EnumerateSecurityPackagesW
#define ENUMERATE_SECURITY_PACKAGES_FN ENUMERATE_SECURITY_PACKAGES_FN_W
#else
#define EnumerateSecurityPackages EnumerateSecurityPackagesA
#define ENUMERATE_SECURITY_PACKAGES_FN ENUMERATE_SECURITY_PACKAGES_FN_A
#endif /* UNICODE */

SECURITY_STATUS SEC_ENTRY QuerySecurityPackageInfoW(
#if ISSP_MODE == 0
    PSECURITY_STRING pPackageName,
#else
    SEC_WCHAR * pszPackageName,
#endif
    PSecPkgInfoW * ppPackageInfo);

typedef SECURITY_STATUS(SEC_ENTRY * QUERY_SECURITY_PACKAGE_INFO_FN_W)(
#if ISSP_MODE == 0
    PSECURITY_STRING,
#else
    SEC_WCHAR *,
#endif
    PSecPkgInfoW *);

SECURITY_STATUS SEC_ENTRY QuerySecurityPackageInfoA(SEC_CHAR * pszPackageName, PSecPkgInfoA * ppPackageInfo);

typedef SECURITY_STATUS(SEC_ENTRY * QUERY_SECURITY_PACKAGE_INFO_FN_A)(SEC_CHAR *, PSecPkgInfoA *);

#ifdef UNICODE
#define QuerySecurityPackageInfo QuerySecurityPackageInfoW
#define QUERY_SECURITY_PACKAGE_INFO_FN QUERY_SECURITY_PACKAGE_INFO_FN_W
#else
#define QuerySecurityPackageInfo QuerySecurityPackageInfoA
#define QUERY_SECURITY_PACKAGE_INFO_FN QUERY_SECURITY_PACKAGE_INFO_FN_A
#endif /* UNICODE */

typedef enum _SecDelegationType {
    SecFull,
    SecService,
    SecTree,
    SecDirectory,
    SecObject
} SecDelegationType, *PSecDelegationType;

SECURITY_STATUS SEC_ENTRY DelegateSecurityContext(PCtxtHandle phContext,
#if ISSP_MODE == 0
    PSECURITY_STRING pTarget,
#else
    SEC_CHAR * pszTarget,
#endif
    SecDelegationType DelegationType, PTimeStamp pExpiry, PSecBuffer pPackageParameters, PSecBufferDesc pOutput);

SECURITY_STATUS SEC_ENTRY ExportSecurityContext(PCtxtHandle phContext, ULONG fFlags, PSecBuffer pPackedContext, void **pToken);

typedef SECURITY_STATUS(SEC_ENTRY * EXPORT_SECURITY_CONTEXT_FN) (PCtxtHandle, ULONG, PSecBuffer, void **);

SECURITY_STATUS SEC_ENTRY ImportSecurityContextW(
#if ISSP_MODE == 0
    PSECURITY_STRING pszPackage,
#else
    SEC_WCHAR * pszPackage,
#endif
    PSecBuffer pPackedContext, void *Token, PCtxtHandle phContext);

typedef SECURITY_STATUS(SEC_ENTRY * IMPORT_SECURITY_CONTEXT_FN_W)(
#if ISSP_MODE == 0
    PSECURITY_STRING,
#else
    SEC_WCHAR *,
#endif
    PSecBuffer, VOID *, PCtxtHandle);

SECURITY_STATUS SEC_ENTRY ImportSecurityContextA(SEC_CHAR * pszPackage, PSecBuffer pPackedContext, VOID * Token, PCtxtHandle phContext);

typedef SECURITY_STATUS(SEC_ENTRY * IMPORT_SECURITY_CONTEXT_FN_A) (SEC_CHAR *, PSecBuffer, void *, PCtxtHandle);

#ifdef UNICODE
#define ImportSecurityContext ImportSecurityContextW
#define IMPORT_SECURITY_CONTEXT_FN IMPORT_SECURITY_CONTEXT_FN_W
#else
#define ImportSecurityContext ImportSecurityContextA
#define IMPORT_SECURITY_CONTEXT_FN IMPORT_SECURITY_CONTEXT_FN_A
#endif /* UNICODE */

#if ISSP_MODE == 0
NTSTATUS NTAPI SecMakeSPN(PUNICODE_STRING,PUNICODE_STRING,PUNICODE_STRING,USHORT,PUNICODE_STRING,PUNICODE_STRING,PULONG,BOOLEAN);
#endif

#define SECURITY_ENTRYPOINT_ANSIW "InitSecurityInterfaceW"
#define SECURITY_ENTRYPOINT_ANSIA "InitSecurityInterfaceA"
#define SECURITY_ENTRYPOINTW SEC_TEXT("InitSecurityInterfaceW")
#define SECURITY_ENTRYPOINTA SEC_TEXT("InitSecurityInterfaceA")
#define SECURITY_ENTRYPOINT16 "INITSECURITYINTERFACEA"

#ifdef SECURITY_WIN32
#ifdef UNICODE
#define SECURITY_ENTRYPOINT SECURITY_ENTRYPOINTW
#define SECURITY_ENTRYPOINT_ANSI SECURITY_ENTRYPOINT_ANSIW
#else /* UNICODE */
#define SECURITY_ENTRYPOINT SECURITY_ENTRYPOINTA
#define SECURITY_ENTRYPOINT_ANSI SECURITY_ENTRYPOINT_ANSIA
#endif /* UNICODE */
#else /* SECURITY_WIN32 */
/* #define SECURITY_ENTRYPOINT SECURITY_ENTRYPOINT16 */
/* #define SECURITY_ENTRYPOINT_ANSI SECURITY_ENTRYPOINT16 */
#endif /* SECURITY_WIN32 */

#define FreeCredentialHandle FreeCredentialsHandle

typedef struct _SECURITY_FUNCTION_TABLE_W {
    unsigned long dwVersion;
    ENUMERATE_SECURITY_PACKAGES_FN_W EnumerateSecurityPackagesW;
    QUERY_CREDENTIALS_ATTRIBUTES_FN_W QueryCredentialsAttributesW;
    ACQUIRE_CREDENTIALS_HANDLE_FN_W AcquireCredentialsHandleW;
    FREE_CREDENTIALS_HANDLE_FN FreeCredentialsHandle;
#ifndef WIN32_CHICAGO
    void *Reserved2;
#else /* WIN32_CHICAGO */
    SSPI_LOGON_USER_FN SspiLogonUserW;
#endif /* WIN32_CHICAGO */
    INITIALIZE_SECURITY_CONTEXT_FN_W InitializeSecurityContextW;
    ACCEPT_SECURITY_CONTEXT_FN AcceptSecurityContext;
    COMPLETE_AUTH_TOKEN_FN CompleteAuthToken;
    DELETE_SECURITY_CONTEXT_FN DeleteSecurityContext;
    APPLY_CONTROL_TOKEN_FN ApplyControlToken;
    QUERY_CONTEXT_ATTRIBUTES_FN_W QueryContextAttributesW;
    IMPERSONATE_SECURITY_CONTEXT_FN ImpersonateSecurityContext;
    REVERT_SECURITY_CONTEXT_FN RevertSecurityContext;
    MAKE_SIGNATURE_FN MakeSignature;
    VERIFY_SIGNATURE_FN VerifySignature;
    FREE_CONTEXT_BUFFER_FN FreeContextBuffer;
    QUERY_SECURITY_PACKAGE_INFO_FN_W QuerySecurityPackageInfoW;
    void *Reserved3;
    void *Reserved4;
    EXPORT_SECURITY_CONTEXT_FN ExportSecurityContext;
    IMPORT_SECURITY_CONTEXT_FN_W ImportSecurityContextW;
    ADD_CREDENTIALS_FN_W AddCredentialsW;
    void *Reserved8;
    QUERY_SECURITY_CONTEXT_TOKEN_FN QuerySecurityContextToken;
    ENCRYPT_MESSAGE_FN EncryptMessage;
    DECRYPT_MESSAGE_FN DecryptMessage;
} SecurityFunctionTableW, *PSecurityFunctionTableW;

typedef struct _SECURITY_FUNCTION_TABLE_A {
    unsigned long dwVersion;
    ENUMERATE_SECURITY_PACKAGES_FN_A EnumerateSecurityPackagesA;
    QUERY_CREDENTIALS_ATTRIBUTES_FN_A QueryCredentialsAttributesA;
    ACQUIRE_CREDENTIALS_HANDLE_FN_A AcquireCredentialsHandleA;
    FREE_CREDENTIALS_HANDLE_FN FreeCredentialHandle;
#ifndef WIN32_CHICAGO
    void *Reserved2;
#else /* WIN32_CHICAGO */
    SSPI_LOGON_USER_FN SspiLogonUserA;
#endif /* WIN32_CHICAGO */
    INITIALIZE_SECURITY_CONTEXT_FN_A InitializeSecurityContextA;
    ACCEPT_SECURITY_CONTEXT_FN AcceptSecurityContext;
    COMPLETE_AUTH_TOKEN_FN CompleteAuthToken;
    DELETE_SECURITY_CONTEXT_FN DeleteSecurityContext;
    APPLY_CONTROL_TOKEN_FN ApplyControlToken;
    QUERY_CONTEXT_ATTRIBUTES_FN_A QueryContextAttributesA;
    IMPERSONATE_SECURITY_CONTEXT_FN ImpersonateSecurityContext;
    REVERT_SECURITY_CONTEXT_FN RevertSecurityContext;
    MAKE_SIGNATURE_FN MakeSignature;
    VERIFY_SIGNATURE_FN VerifySignature;
    FREE_CONTEXT_BUFFER_FN FreeContextBuffer;
    QUERY_SECURITY_PACKAGE_INFO_FN_A QuerySecurityPackageInfoA;
    void *Reserved3;
    void *Reserved4;
    EXPORT_SECURITY_CONTEXT_FN ExportSecurityContext;
    IMPORT_SECURITY_CONTEXT_FN_A ImportSecurityContextA;
    ADD_CREDENTIALS_FN_A AddCredentialsA;
    void *Reserved8;
    QUERY_SECURITY_CONTEXT_TOKEN_FN QuerySecurityContextToken;
    ENCRYPT_MESSAGE_FN EncryptMessage;
    DECRYPT_MESSAGE_FN DecryptMessage;
} SecurityFunctionTableA, *PSecurityFunctionTableA;

#ifdef UNICODE
#define SecurityFunctionTable SecurityFunctionTableW
#define PSecurityFunctionTable PSecurityFunctionTableW
#else
#define SecurityFunctionTable SecurityFunctionTableA
#define PSecurityFunctionTable PSecurityFunctionTableA
#endif /* UNICODE */

#define SECURITY_

#define SECURITY_SUPPORT_PROVIDER_INTERFACE_VERSION  1

PSecurityFunctionTableA SEC_ENTRY InitSecurityInterfaceA(void);

typedef PSecurityFunctionTableA(SEC_ENTRY * INIT_SECURITY_INTERFACE_A) (void);

PSecurityFunctionTableW SEC_ENTRY InitSecurityInterfaceW(void);

typedef PSecurityFunctionTableW(SEC_ENTRY * INIT_SECURITY_INTERFACE_W) (void);

#ifdef UNICODE
#define InitSecurityInterface InitSecurityInterfaceW
#define INIT_SECURITY_INTERFACE INIT_SECURITY_INTERFACE_W
#else
#define InitSecurityInterface InitSecurityInterfaceA
#define INIT_SECURITY_INTERFACE INIT_SECURITY_INTERFACE_A
#endif /* UNICODE */

#ifdef SECURITY_WIN32
SECURITY_STATUS SEC_ENTRY SaslEnumerateProfilesA(LPSTR * ProfileList, ULONG * ProfileCount);
SECURITY_STATUS SEC_ENTRY SaslEnumerateProfilesW(LPWSTR * ProfileList, ULONG * ProfileCount);

#ifdef UNICODE
#define SaslEnumerateProfiles  SaslEnumerateProfilesW
#else
#define SaslEnumerateProfiles  SaslEnumerateProfilesA
#endif

SECURITY_STATUS SEC_ENTRY SaslGetProfilePackageA(LPSTR ProfileName, PSecPkgInfoA * PackageInfo);
SECURITY_STATUS SEC_ENTRY SaslGetProfilePackageW(LPWSTR ProfileName, PSecPkgInfoW * PackageInfo);

#ifdef UNICODE
#define SaslGetProfilePackage  SaslGetProfilePackageW
#else
#define SaslGetProfilePackage  SaslGetProfilePackageA
#endif

SECURITY_STATUS SEC_ENTRY SaslIdentifyPackageA(PSecBufferDesc pInput, PSecPkgInfoA * PackageInfo);
SECURITY_STATUS SEC_ENTRY SaslIdentifyPackageW(PSecBufferDesc pInput, PSecPkgInfoW * PackageInfo);

#ifdef UNICODE
#define SaslIdentifyPackage SaslIdentifyPackageW
#else
#define SaslIdentifyPackage SaslIdentifyPackageA
#endif

SECURITY_STATUS SEC_ENTRY SaslInitializeSecurityContextW(PCredHandle,PCtxtHandle,LPWSTR,unsigned long,unsigned long, unsigned long,PSecBufferDesc,unsigned long,PCtxtHandle,PSecBufferDesc,unsigned long*,PTimeStamp);
SECURITY_STATUS SEC_ENTRY SaslInitializeSecurityContextA(PCredHandle,PCtxtHandle,LPSTR,unsigned long,unsigned long,unsigned long,PSecBufferDesc,unsigned long,PCtxtHandle,PSecBufferDesc,unsigned long*,PTimeStamp);

#ifdef UNICODE
#define SaslInitializeSecurityContext  SaslInitializeSecurityContextW
#else
#define SaslInitializeSecurityContext  SaslInitializeSecurityContextA
#endif

SECURITY_STATUS SEC_ENTRY SaslAcceptSecurityContext(PCredHandle,PCtxtHandle,PSecBufferDesc,unsigned long,unsigned long,PCtxtHandle,PSecBufferDesc,unsigned long*,PTimeStamp);
#endif /* SECURITY_WIN32 */

#ifdef __cplusplus
}
#endif

#endif /* _SSPI_H */
