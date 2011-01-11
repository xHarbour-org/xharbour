#ifndef _RPCDCE_H
#define _RPCDCE_H

/* Windows DCE RPC runtime API definitions */

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2028)  /* Missing prototype */
#endif

typedef I_RPC_HANDLE RPC_BINDING_HANDLE;
typedef RPC_BINDING_HANDLE handle_t;
#define rpc_binding_handle_t RPC_BINDING_HANDLE

#ifndef GUID_DEFINED
#include <guiddef.h>
#endif

#ifndef UUID_DEFINED
#define UUID_DEFINED
typedef GUID UUID;
#ifndef uuid_t
#define uuid_t UUID
#endif
#endif

typedef struct _RPC_BINDING_VECTOR {
    unsigned long Count;
    RPC_BINDING_HANDLE BindingH[1];
} RPC_BINDING_VECTOR;

#ifndef rpc_binding_vector_t
#define rpc_binding_vector_t RPC_BINDING_VECTOR
#endif

typedef struct _UUID_VECTOR {
    unsigned long Count;
    UUID *Uuid[1];
} UUID_VECTOR;

#ifndef uuid_vector_t
#define uuid_vector_t UUID_VECTOR
#endif

typedef void *RPC_IF_HANDLE;

#ifndef IFID_DEFINED
#define IFID_DEFINED
typedef struct _RPC_IF_ID {
    UUID Uuid;
    unsigned short VersMajor;
    unsigned short VersMinor;
} RPC_IF_ID;
#endif

#define RPC_C_BINDING_INFINITE_TIMEOUT  10
#define RPC_C_BINDING_MIN_TIMEOUT  0
#define RPC_C_BINDING_DEFAULT_TIMEOUT  5
#define RPC_C_BINDING_MAX_TIMEOUT  9

#define RPC_C_CANCEL_INFINITE_TIMEOUT  -1

#define RPC_C_LISTEN_MAX_CALLS_DEFAULT  1234
#define RPC_C_PROTSEQ_MAX_REQS_DEFAULT  10

#define RPC_C_BIND_TO_ALL_NICS  1
#define RPC_C_USE_INTERNET_PORT  0x1
#define RPC_C_USE_INTRANET_PORT  0x2
#define RPC_C_DONT_FAIL  0x4

#define RPC_C_MQ_TEMPORARY  0x0000
#define RPC_C_MQ_PERMANENT  0x0001
#define RPC_C_MQ_CLEAR_ON_OPEN  0x0002
#define RPC_C_MQ_USE_EXISTING_SECURITY  0x0004
#define RPC_C_MQ_AUTHN_LEVEL_NONE  0x0000
#define RPC_C_MQ_AUTHN_LEVEL_PKT_INTEGRITY  0x0008
#define RPC_C_MQ_AUTHN_LEVEL_PKT_PRIVACY  0x0010

#define RPC_C_OPT_MQ_DELIVERY  1
#define RPC_C_OPT_MQ_PRIORITY  2
#define RPC_C_OPT_MQ_JOURNAL  3
#define RPC_C_OPT_MQ_ACKNOWLEDGE  4
#define RPC_C_OPT_MQ_AUTHN_SERVICE  5
#define RPC_C_OPT_MQ_AUTHN_LEVEL  6
#define RPC_C_OPT_MQ_TIME_TO_REACH_QUEUE  7
#define RPC_C_OPT_MQ_TIME_TO_BE_RECEIVED  8
#define RPC_C_OPT_BINDING_NONCAUSAL  9
#define RPC_C_OPT_SECURITY_CALLBACK  10
#define RPC_C_OPT_UNIQUE_BINDING  11
#define RPC_C_OPT_MAX_OPTIONS  12

#define RPC_C_MQ_EXPRESS  0
#define RPC_C_MQ_RECOVERABLE  1

#define RPC_C_MQ_JOURNAL_NONE  0
#define RPC_C_MQ_JOURNAL_DEADLETTER  1
#define RPC_C_MQ_JOURNAL_ALWAYS  2

#define RPC_C_FULL_CERT_CHAIN  0x0001

#define RPC_C_AUTHN_LEVEL_DEFAULT  0
#define RPC_C_AUTHN_LEVEL_NONE  1
#define RPC_C_AUTHN_LEVEL_CONNECT  2
#define RPC_C_AUTHN_LEVEL_CALL  3
#define RPC_C_AUTHN_LEVEL_PKT  4
#define RPC_C_AUTHN_LEVEL_PKT_INTEGRITY  5
#define RPC_C_AUTHN_LEVEL_PKT_PRIVACY  6

#define RPC_C_IMP_LEVEL_DEFAULT  0
#define RPC_C_IMP_LEVEL_ANONYMOUS  1
#define RPC_C_IMP_LEVEL_IDENTIFY  2
#define RPC_C_IMP_LEVEL_IMPERSONATE  3
#define RPC_C_IMP_LEVEL_DELEGATE  4

#define RPC_C_QOS_IDENTITY_STATIC  0
#define RPC_C_QOS_IDENTITY_DYNAMIC  1

#define RPC_C_QOS_CAPABILITIES_DEFAULT  0
#define RPC_C_QOS_CAPABILITIES_MUTUAL_AUTH  1
#define RPC_C_QOS_CAPABILITIES_MAKE_FULLSIC  2
#define RPC_C_QOS_CAPABILITIES_ANY_AUTHORITY  4

#define RPC_C_PROTECT_LEVEL_DEFAULT  (RPC_C_AUTHN_LEVEL_DEFAULT)
#define RPC_C_PROTECT_LEVEL_NONE  (RPC_C_AUTHN_LEVEL_NONE)
#define RPC_C_PROTECT_LEVEL_CONNECT  (RPC_C_AUTHN_LEVEL_CONNECT)
#define RPC_C_PROTECT_LEVEL_CALL  (RPC_C_AUTHN_LEVEL_CALL)
#define RPC_C_PROTECT_LEVEL_PKT  (RPC_C_AUTHN_LEVEL_PKT)
#define RPC_C_PROTECT_LEVEL_PKT_INTEGRITY  (RPC_C_AUTHN_LEVEL_PKT_INTEGRITY)
#define RPC_C_PROTECT_LEVEL_PKT_PRIVACY  (RPC_C_AUTHN_LEVEL_PKT_PRIVACY)

#define RPC_C_AUTHN_NONE  0
#define RPC_C_AUTHN_DCE_PRIVATE  1
#define RPC_C_AUTHN_DCE_PUBLIC  2
#define RPC_C_AUTHN_DEC_PUBLIC  4
#define RPC_C_AUTHN_GSS_NEGOTIATE  9
#define RPC_C_AUTHN_WINNT  10
#define RPC_C_AUTHN_GSS_SCHANNEL  14
#define RPC_C_AUTHN_GSS_KERBEROS  16
#define RPC_C_AUTHN_DPA  17
#define RPC_C_AUTHN_MSN  18
#define RPC_C_AUTHN_MQ  100
#define RPC_C_AUTHN_DEFAULT  0xFFFFFFFFL

#define RPC_C_NO_CREDENTIALS  ((RPC_AUTH_IDENTITY_HANDLE)~0UL)

#define RPC_C_SECURITY_QOS_VERSION  1L

#define RPC_C_STATS_CALLS_IN  0
#define RPC_C_STATS_CALLS_OUT  1
#define RPC_C_STATS_PKTS_IN  2
#define RPC_C_STATS_PKTS_OUT  3

#define RPC_MGR_EPV void

#define SEC_WINNT_AUTH_IDENTITY_ANSI  0x1
#define SEC_WINNT_AUTH_IDENTITY_UNICODE  0x2

#define RPC_C_AUTHZ_NONE  0
#define RPC_C_AUTHZ_NAME  1
#define RPC_C_AUTHZ_DCE  2
#define RPC_C_AUTHZ_DEFAULT  0xFFFFFFFF

#define DCE_C_ERROR_STRING_LEN  256

#define RPC_C_EP_ALL_ELTS  0
#define RPC_C_EP_MATCH_BY_IF  1
#define RPC_C_EP_MATCH_BY_OBJ  2
#define RPC_C_EP_MATCH_BY_BOTH  3

#define RPC_C_VERS_ALL  1
#define RPC_C_VERS_COMPATIBLE  2
#define RPC_C_VERS_EXACT  3
#define RPC_C_VERS_MAJOR_ONLY  4
#define RPC_C_VERS_UPTO  5

#define RPC_C_MGMT_INQ_IF_IDS  0
#define RPC_C_MGMT_INQ_PRINC_NAME  1
#define RPC_C_MGMT_INQ_STATS  2
#define RPC_C_MGMT_IS_SERVER_LISTEN  3
#define RPC_C_MGMT_STOP_SERVER_LISTEN  4

#define RPC_C_PARM_MAX_PACKET_LENGTH  1
#define RPC_C_PARM_BUFFER_LENGTH  2

#define RPC_IF_AUTOLISTEN  0x0001
#define RPC_IF_OLE  0x0002
#define RPC_IF_ALLOW_UNKNOWN_AUTHORITY  0x0004
#define RPC_IF_ALLOW_SECURE_ONLY  0x0008

typedef struct _RPC_PROTSEQ_VECTORA {
    unsigned int Count;
    unsigned char *Protseq[1];
} RPC_PROTSEQ_VECTORA;

typedef struct _RPC_PROTSEQ_VECTORW {
    unsigned int Count;
    unsigned short *Protseq[1];
} RPC_PROTSEQ_VECTORW;

typedef struct _RPC_POLICY {
    unsigned int Length;
    unsigned long EndpointFlags;
    unsigned long NICFlags;
    } RPC_POLICY, *PRPC_POLICY;

typedef void __RPC_USER RPC_OBJECT_INQ_FN(UUID*,UUID*,RPC_STATUS*);
typedef RPC_STATUS RPC_ENTRY RPC_IF_CALLBACK_FN(RPC_IF_HANDLE,void*);
typedef void RPC_ENTRY RPC_SECURITY_CALLBACK_FN(void*);

typedef struct {
    unsigned int Count;
    unsigned long Stats[1];
} RPC_STATS_VECTOR;

typedef struct {
  unsigned long Count;
  RPC_IF_ID *IfId[1];
} RPC_IF_ID_VECTOR;

typedef void *RPC_AUTH_IDENTITY_HANDLE;
typedef void *RPC_AUTHZ_HANDLE;

typedef struct _RPC_SECURITY_QOS {
    unsigned long Version;
    unsigned long Capabilities;
    unsigned long IdentityTracking;
    unsigned long ImpersonationType;
} RPC_SECURITY_QOS, *PRPC_SECURITY_QOS;

typedef struct _SEC_WINNT_AUTH_IDENTITY_W {
  unsigned short *User;
  unsigned long UserLength;
  unsigned short *Domain;
  unsigned long DomainLength;
  unsigned short *Password;
  unsigned long PasswordLength;
  unsigned long Flags;
} SEC_WINNT_AUTH_IDENTITY_W, *PSEC_WINNT_AUTH_IDENTITY_W;

typedef struct _SEC_WINNT_AUTH_IDENTITY_A {
  unsigned char *User;
  unsigned long UserLength;
  unsigned char *Domain;
  unsigned long DomainLength;
  unsigned char *Password;
  unsigned long PasswordLength;
  unsigned long Flags;
} SEC_WINNT_AUTH_IDENTITY_A, *PSEC_WINNT_AUTH_IDENTITY_A;

typedef void (__RPC_USER *RPC_AUTH_KEY_RETRIEVAL_FN)(void*,unsigned short*,unsigned long,void**,RPC_STATUS*);

typedef struct {
    unsigned char *UserName;
    unsigned char *ComputerName;
    unsigned short Privilege;
    unsigned long AuthFlags;
} RPC_CLIENT_INFORMATION1, *PRPC_CLIENT_INFORMATION1;

typedef I_RPC_HANDLE *RPC_EP_INQ_HANDLE;

typedef int (__RPC_API *RPC_MGMT_AUTHORIZATION_FN)(RPC_BINDING_HANDLE,unsigned long,RPC_STATUS*);

RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingCopy(RPC_BINDING_HANDLE,RPC_BINDING_HANDLE*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingFree(RPC_BINDING_HANDLE*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingSetOption(RPC_BINDING_HANDLE,unsigned long,ULONG_PTR);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqOption(RPC_BINDING_HANDLE,unsigned long,ULONG_PTR*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingFromStringBindingA(unsigned char*,RPC_BINDING_HANDLE*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingFromStringBindingW(unsigned short*,RPC_BINDING_HANDLE*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcSsGetContextBinding(void*,RPC_BINDING_HANDLE*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqObject(RPC_BINDING_HANDLE,UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingReset(RPC_BINDING_HANDLE);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingSetObject(RPC_BINDING_HANDLE,UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtInqDefaultProtectLevel(unsigned long,unsigned long*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingToStringBindingA(RPC_BINDING_HANDLE,unsigned char**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingToStringBindingW(RPC_BINDING_HANDLE,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingVectorFree(RPC_BINDING_VECTOR**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcStringBindingComposeA(unsigned char*,unsigned char*,unsigned char*,unsigned char*,unsigned char*,unsigned char**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcStringBindingComposeW(unsigned short*,unsigned short*,unsigned short*,unsigned short*,unsigned short*,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcStringBindingParseA(unsigned char*,unsigned char**,unsigned char**,unsigned char**,unsigned char**,unsigned char**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcStringBindingParseW(unsigned short*,unsigned short**,unsigned short**,unsigned short**,unsigned short**,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcStringFreeA(unsigned char**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcStringFreeW(unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcIfInqId(RPC_IF_HANDLE,RPC_IF_ID*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcNetworkIsProtseqValidA(unsigned char*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcNetworkIsProtseqValidW(unsigned short*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtInqComTimeout(RPC_BINDING_HANDLE,unsigned int*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtSetComTimeout(RPC_BINDING_HANDLE,unsigned int);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtSetCancelTimeout(long);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcNetworkInqProtseqsA(RPC_PROTSEQ_VECTORA**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcNetworkInqProtseqsW(RPC_PROTSEQ_VECTORW**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcObjectInqType(UUID*,UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcObjectSetInqFn(RPC_OBJECT_INQ_FN*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcObjectSetType(UUID*,UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcProtseqVectorFreeA(RPC_PROTSEQ_VECTORA**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcProtseqVectorFreeW(RPC_PROTSEQ_VECTORW**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerInqBindings(RPC_BINDING_VECTOR**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerInqIf(RPC_IF_HANDLE,UUID*,RPC_MGR_EPV**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerListen(unsigned int,unsigned int,unsigned int);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerRegisterIf(RPC_IF_HANDLE,UUID*,RPC_MGR_EPV*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerRegisterIfEx(RPC_IF_HANDLE,UUID*,RPC_MGR_EPV*,unsigned int,unsigned int,RPC_IF_CALLBACK_FN*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerRegisterIf2(RPC_IF_HANDLE,UUID*,RPC_MGR_EPV*,unsigned int,unsigned int,unsigned int,RPC_IF_CALLBACK_FN*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUnregisterIf(RPC_IF_HANDLE,UUID*,unsigned int);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseAllProtseqs(unsigned int,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseAllProtseqsEx(unsigned int,void*,PRPC_POLICY);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseAllProtseqsIf(unsigned int,RPC_IF_HANDLE,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseAllProtseqsIfEx(unsigned int,RPC_IF_HANDLE,void*,PRPC_POLICY);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqA(unsigned char*,unsigned int,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqExA(unsigned char*,unsigned int,void*,PRPC_POLICY);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqW(unsigned short*,unsigned int,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqExW(unsigned short*,unsigned int,void*,PRPC_POLICY);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqEpA(unsigned char*,unsigned int,unsigned char*,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqEpExA(unsigned char*,unsigned int,unsigned char*,void*,PRPC_POLICY);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqEpW(unsigned short*,unsigned int,unsigned short*,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqEpExW(unsigned short*,unsigned int,unsigned short*,void*,PRPC_POLICY);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqIfA(unsigned char*,unsigned int,RPC_IF_HANDLE,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqIfExA(unsigned char*,unsigned int,RPC_IF_HANDLE,void*,PRPC_POLICY);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqIfW(unsigned short*,unsigned int,RPC_IF_HANDLE,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerUseProtseqIfExW(unsigned short*,unsigned int,RPC_IF_HANDLE,void*,PRPC_POLICY);
RPCRTAPI void RPC_ENTRY RpcServerYield();
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtStatsVectorFree(RPC_STATS_VECTOR**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtInqStats(RPC_BINDING_HANDLE,RPC_STATS_VECTOR**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtIsServerListening(RPC_BINDING_HANDLE);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtStopServerListening(RPC_BINDING_HANDLE);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtWaitServerListen(void);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtSetServerStackSize(unsigned long);
RPCRTAPI void RPC_ENTRY RpcSsDontSerializeContext(void);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtEnableIdleCleanup(void);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtInqIfIds(RPC_BINDING_HANDLE,RPC_IF_ID_VECTOR**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcIfIdVectorFree(RPC_IF_ID_VECTOR**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtInqServerPrincNameA(RPC_BINDING_HANDLE,unsigned long,unsigned char**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtInqServerPrincNameW(RPC_BINDING_HANDLE,unsigned long,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerInqDefaultPrincNameA(unsigned long,unsigned char**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerInqDefaultPrincNameW(unsigned long,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcEpResolveBinding(RPC_BINDING_HANDLE,RPC_IF_HANDLE);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcNsBindingInqEntryNameA(RPC_BINDING_HANDLE,unsigned long,unsigned char**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcNsBindingInqEntryNameW(RPC_BINDING_HANDLE,unsigned long,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcImpersonateClient(RPC_BINDING_HANDLE);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcRevertToSelfEx(RPC_BINDING_HANDLE);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcRevertToSelf();
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqAuthClientA(RPC_BINDING_HANDLE,RPC_AUTHZ_HANDLE*,unsigned char**,unsigned long*,unsigned long*,unsigned long*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqAuthClientW(RPC_BINDING_HANDLE,RPC_AUTHZ_HANDLE*,unsigned short**,unsigned long*,unsigned long*,unsigned long*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqAuthClientExA(RPC_BINDING_HANDLE,RPC_AUTHZ_HANDLE*,unsigned char**,unsigned long*,unsigned long*,unsigned long*,unsigned long);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqAuthClientExW(RPC_BINDING_HANDLE,RPC_AUTHZ_HANDLE*,unsigned short**,unsigned long*,unsigned long*,unsigned long*,unsigned long);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqAuthInfoA(RPC_BINDING_HANDLE,unsigned char**,unsigned long*,unsigned long*,RPC_AUTH_IDENTITY_HANDLE*,unsigned long*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqAuthInfoW(RPC_BINDING_HANDLE,unsigned short**,unsigned long*,unsigned long*,RPC_AUTH_IDENTITY_HANDLE*,unsigned long*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingSetAuthInfoA(RPC_BINDING_HANDLE,unsigned char*,unsigned long,unsigned long,RPC_AUTH_IDENTITY_HANDLE,unsigned long);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingSetAuthInfoW(RPC_BINDING_HANDLE,unsigned short*,unsigned long,unsigned long,RPC_AUTH_IDENTITY_HANDLE,unsigned long);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingSetAuthInfoExA(RPC_BINDING_HANDLE,unsigned char*,unsigned long,unsigned long,RPC_AUTH_IDENTITY_HANDLE,unsigned long,RPC_SECURITY_QOS*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingSetAuthInfoExW(RPC_BINDING_HANDLE,unsigned short*,unsigned long,unsigned long,RPC_AUTH_IDENTITY_HANDLE,unsigned long,RPC_SECURITY_QOS*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqAuthInfoExA(RPC_BINDING_HANDLE,unsigned char**,unsigned long*,unsigned long*,RPC_AUTH_IDENTITY_HANDLE*,unsigned long*,unsigned long,RPC_SECURITY_QOS*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingInqAuthInfoExW(RPC_BINDING_HANDLE,unsigned short**,unsigned long*,unsigned long*,RPC_AUTH_IDENTITY_HANDLE*,unsigned long*,unsigned long,RPC_SECURITY_QOS*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerRegisterAuthInfoA(unsigned char*,unsigned long,RPC_AUTH_KEY_RETRIEVAL_FN,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerRegisterAuthInfoW(unsigned short*,unsigned long,RPC_AUTH_KEY_RETRIEVAL_FN,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcBindingServerFromClient(RPC_BINDING_HANDLE,RPC_BINDING_HANDLE*);
RPCRTAPI void RPC_ENTRY RpcRaiseException(RPC_STATUS);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcTestCancel();
RPCRTAPI RPC_STATUS RPC_ENTRY RpcServerTestCancel(RPC_BINDING_HANDLE);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcCancelThread(void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcCancelThreadEx(void*,long);
RPCRTAPI RPC_STATUS RPC_ENTRY UuidCreate(UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY UuidCreateSequential(UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY UuidToStringA(UUID*,unsigned char**);
RPCRTAPI RPC_STATUS RPC_ENTRY UuidFromStringA(unsigned char*,UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY UuidToStringW(UUID*,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY UuidFromStringW(unsigned short*,UUID*);
RPCRTAPI signed int RPC_ENTRY UuidCompare(UUID*,UUID*,RPC_STATUS*);
RPCRTAPI RPC_STATUS RPC_ENTRY UuidCreateNil(UUID*);
RPCRTAPI int RPC_ENTRY UuidEqual(UUID*,UUID*,RPC_STATUS*);
RPCRTAPI unsigned short RPC_ENTRY UuidHash(UUID*,RPC_STATUS*);
RPCRTAPI int RPC_ENTRY UuidIsNil(UUID*,RPC_STATUS*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcEpRegisterNoReplaceA(RPC_IF_HANDLE,RPC_BINDING_VECTOR*,UUID_VECTOR*,unsigned char*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcEpRegisterNoReplaceW(RPC_IF_HANDLE,RPC_BINDING_VECTOR*,UUID_VECTOR*,unsigned short*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcEpRegisterA(RPC_IF_HANDLE,RPC_BINDING_VECTOR*,UUID_VECTOR*,unsigned char*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcEpRegisterW(RPC_IF_HANDLE,RPC_BINDING_VECTOR*,UUID_VECTOR*,unsigned short*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcEpUnregister(RPC_IF_HANDLE,RPC_BINDING_VECTOR*,UUID_VECTOR*);
RPCRTAPI RPC_STATUS RPC_ENTRY DceErrorInqTextA(RPC_STATUS,unsigned char*);
RPCRTAPI RPC_STATUS RPC_ENTRY DceErrorInqTextW(RPC_STATUS,unsigned short*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtEpEltInqBegin(RPC_BINDING_HANDLE,unsigned long,RPC_IF_ID*,unsigned long,UUID*,RPC_EP_INQ_HANDLE*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtEpEltInqDone(RPC_EP_INQ_HANDLE*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtEpEltInqNextA(RPC_EP_INQ_HANDLE,RPC_IF_ID*,RPC_BINDING_HANDLE*,UUID*,unsigned char**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtEpEltInqNextW(RPC_EP_INQ_HANDLE,RPC_IF_ID*,RPC_BINDING_HANDLE*,UUID*,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtEpUnregister(RPC_BINDING_HANDLE,RPC_IF_ID*,RPC_BINDING_HANDLE,UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcMgmtSetAuthorizationFn(RPC_MGMT_AUTHORIZATION_FN);

#ifdef UNICODE
#define SEC_WINNT_AUTH_IDENTITY SEC_WINNT_AUTH_IDENTITY_W
#define PSEC_WINNT_AUTH_IDENTITY PSEC_WINNT_AUTH_IDENTITY_W
#define _SEC_WINNT_AUTH_IDENTITY _SEC_WINNT_AUTH_IDENTITY_W
#define RPC_PROTSEQ_VECTOR RPC_PROTSEQ_VECTORW
#define RpcBindingFromStringBinding RpcBindingFromStringBindingW
#define RpcBindingToStringBinding RpcBindingToStringBindingW
#define RpcStringBindingCompose RpcStringBindingComposeW
#define RpcStringBindingParse RpcStringBindingParseW
#define RpcStringFree RpcStringFreeW
#define RpcNetworkIsProtseqValid RpcNetworkIsProtseqValidW
#define RpcNetworkInqProtseqs RpcNetworkInqProtseqsW
#define RpcProtseqVectorFree RpcProtseqVectorFreeW
#define RpcServerUseProtseq RpcServerUseProtseqW
#define RpcServerUseProtseqEx RpcServerUseProtseqExW
#define RpcServerUseProtseqEp RpcServerUseProtseqEpW
#define RpcServerUseProtseqEpEx RpcServerUseProtseqEpExW
#define RpcServerUseProtseqIf RpcServerUseProtseqIfW
#define RpcServerUseProtseqIfEx RpcServerUseProtseqIfExW
#define RpcMgmtInqServerPrincName RpcMgmtInqServerPrincNameW
#define RpcServerInqDefaultPrincName RpcServerInqDefaultPrincNameW
#define RpcNsBindingInqEntryName RpcNsBindingInqEntryNameW
#define RpcBindingInqAuthClient RpcBindingInqAuthClientW
#define RpcBindingInqAuthClientEx RpcBindingInqAuthClientExW
#define RpcBindingInqAuthInfo RpcBindingInqAuthInfoW
#define RpcBindingSetAuthInfo RpcBindingSetAuthInfoW
#define RpcServerRegisterAuthInfo RpcServerRegisterAuthInfoW
#define RpcBindingInqAuthInfoEx RpcBindingInqAuthInfoExW
#define RpcBindingSetAuthInfoEx RpcBindingSetAuthInfoExW
#define UuidFromString UuidFromStringW
#define UuidToString UuidToStringW
#define RpcEpRegisterNoReplace RpcEpRegisterNoReplaceW
#define RpcEpRegister RpcEpRegisterW
#define DceErrorInqText DceErrorInqTextW
#define RpcMgmtEpEltInqNext RpcMgmtEpEltInqNextW
#else
#define SEC_WINNT_AUTH_IDENTITY SEC_WINNT_AUTH_IDENTITY_A
#define PSEC_WINNT_AUTH_IDENTITY PSEC_WINNT_AUTH_IDENTITY_A
#define _SEC_WINNT_AUTH_IDENTITY _SEC_WINNT_AUTH_IDENTITY_A
#define RPC_PROTSEQ_VECTOR RPC_PROTSEQ_VECTORA
#define RpcBindingFromStringBinding RpcBindingFromStringBindingA
#define RpcBindingToStringBinding RpcBindingToStringBindingA
#define RpcStringBindingCompose RpcStringBindingComposeA
#define RpcStringBindingParse RpcStringBindingParseA
#define RpcStringFree RpcStringFreeA
#define RpcNetworkIsProtseqValid RpcNetworkIsProtseqValidA
#define RpcNetworkInqProtseqs RpcNetworkInqProtseqsA
#define RpcProtseqVectorFree RpcProtseqVectorFreeA
#define RpcServerUseProtseq RpcServerUseProtseqA
#define RpcServerUseProtseqEx RpcServerUseProtseqExA
#define RpcServerUseProtseqEp RpcServerUseProtseqEpA
#define RpcServerUseProtseqEpEx RpcServerUseProtseqEpExA
#define RpcServerUseProtseqIf RpcServerUseProtseqIfA
#define RpcServerUseProtseqIfEx RpcServerUseProtseqIfExA
#define RpcMgmtInqServerPrincName RpcMgmtInqServerPrincNameA
#define RpcServerInqDefaultPrincName RpcServerInqDefaultPrincNameA
#define RpcNsBindingInqEntryName RpcNsBindingInqEntryNameA
#define RpcBindingInqAuthClient RpcBindingInqAuthClientA
#define RpcBindingInqAuthClientEx RpcBindingInqAuthClientExA
#define RpcBindingInqAuthInfo RpcBindingInqAuthInfoA
#define RpcBindingSetAuthInfo RpcBindingSetAuthInfoA
#define RpcServerRegisterAuthInfo RpcServerRegisterAuthInfoA
#define RpcBindingInqAuthInfoEx RpcBindingInqAuthInfoExA
#define RpcBindingSetAuthInfoEx RpcBindingSetAuthInfoExA
#define UuidFromString UuidFromStringA
#define UuidToString UuidToStringA
#define RpcEpRegisterNoReplace RpcEpRegisterNoReplaceA
#define RpcEpRegister RpcEpRegisterA
#define DceErrorInqText DceErrorInqTextA
#define RpcMgmtEpEltInqNext RpcMgmtEpEltInqNextA
#endif /* UNICODE */

#include <rpcdcep.h>

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
}
#endif

#endif /* _RPCDCE_H */

