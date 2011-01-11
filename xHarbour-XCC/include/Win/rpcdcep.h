#ifndef _RPCDCEP_H
#define _RPCDCEP_H

/* Windows Private RPC runtime API definition */

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2028)  /* Missing prototype */
#endif

#define RPC_CONTEXT_HANDLE_DEFAULT_GUARD  ((void*)0xFFFFF00D)
#define RPC_CONTEXT_HANDLE_DEFAULT_FLAGS  0x00000000UL
#define RPC_CONTEXT_HANDLE_FLAGS  0x30000000UL
#define RPC_CONTEXT_HANDLE_SERIALIZE  0x10000000UL
#define RPC_CONTEXT_HANDLE_DONT_SERIALIZE  0x20000000UL

#define RPC_NCA_FLAGS_DEFAULT  0x00000000
#define RPC_NCA_FLAGS_IDEMPOTENT  0x00000001
#define RPC_NCA_FLAGS_BROADCAST  0x00000002
#define RPC_NCA_FLAGS_MAYBE  0x00000004

#define RPC_BUFFER_COMPLETE  0x00001000
#define RPC_BUFFER_PARTIAL  0x00002000
#define RPC_BUFFER_EXTRA  0x00004000
#define RPC_BUFFER_ASYNC  0x00008000
#define RPC_BUFFER_NONOTIFY  0x00010000

#define RPCFLG_MESSAGE  0x01000000UL
#define RPCFLG_AUTO_COMPLETE  0x08000000UL
#define RPCFLG_LOCAL_CALL  0x10000000UL
#define RPCFLG_INPUT_SYNCHRONOUS  0x20000000UL
#define RPCFLG_ASYNCHRONOUS  0x40000000UL
#define RPCFLG_NON_NDR  0x80000000UL

#define RPC_FLAGS_VALID_BIT  0x00008000

#define NT351_INTERFACE_SIZE  0x40
#define RPC_INTERFACE_HAS_PIPES  0x0001

#define TRANSPORT_TYPE_CN  0x01
#define TRANSPORT_TYPE_DG  0x02
#define TRANSPORT_TYPE_LPC  0x04
#define TRANSPORT_TYPE_WMSG  0x08

typedef struct _RPC_VERSION {
    unsigned short MajorVersion;
    unsigned short MinorVersion;
} RPC_VERSION;

typedef struct _RPC_SYNTAX_IDENTIFIER {
    GUID SyntaxGUID;
    RPC_VERSION SyntaxVersion;
} RPC_SYNTAX_IDENTIFIER, * PRPC_SYNTAX_IDENTIFIER;

typedef struct _RPC_MESSAGE {
    RPC_BINDING_HANDLE Handle;
    unsigned long DataRepresentation;
    void *Buffer;
    unsigned int BufferLength;
    unsigned int ProcNum;
    PRPC_SYNTAX_IDENTIFIER TransferSyntax;
    void *RpcInterfaceInformation;
    void *ReservedForRuntime;
    RPC_MGR_EPV *ManagerEpv;
    void *ImportContext;
    unsigned long RpcFlags;
} RPC_MESSAGE, *PRPC_MESSAGE;

typedef RPC_STATUS

RPC_ENTRY RPC_FORWARD_FUNCTION(UUID*,RPC_VERSION*,UUID*,unsigned char*,void**);

enum RPC_ADDRESS_CHANGE_TYPE {
    PROTOCOL_NOT_LOADED = 1,
    PROTOCOL_LOADED,
    PROTOCOL_ADDRESS_CHANGE
};

typedef void RPC_ENTRY RPC_ADDRESS_CHANGE_FN(void*);
typedef void (__RPC_STUB *RPC_DISPATCH_FUNCTION)(PRPC_MESSAGE);

typedef struct {
    unsigned int DispatchTableCount;
    RPC_DISPATCH_FUNCTION *DispatchTable;
    LONG_PTR Reserved;
} RPC_DISPATCH_TABLE, *PRPC_DISPATCH_TABLE;

typedef struct _RPC_PROTSEQ_ENDPOINT {
    unsigned char *RpcProtocolSequence;
    unsigned char *Endpoint;
} RPC_PROTSEQ_ENDPOINT, *PRPC_PROTSEQ_ENDPOINT;

typedef struct _RPC_SERVER_INTERFACE {
    unsigned int Length;
    RPC_SYNTAX_IDENTIFIER InterfaceId;
    RPC_SYNTAX_IDENTIFIER TransferSyntax;
    PRPC_DISPATCH_TABLE DispatchTable;
    unsigned int RpcProtseqEndpointCount;
    PRPC_PROTSEQ_ENDPOINT RpcProtseqEndpoint;
    RPC_MGR_EPV *DefaultManagerEpv;
    void const *InterpreterInfo;
    unsigned int Flags;
} RPC_SERVER_INTERFACE, *PRPC_SERVER_INTERFACE;

typedef struct _RPC_CLIENT_INTERFACE {
    unsigned int Length;
    RPC_SYNTAX_IDENTIFIER InterfaceId;
    RPC_SYNTAX_IDENTIFIER TransferSyntax;
    PRPC_DISPATCH_TABLE DispatchTable;
    unsigned int RpcProtseqEndpointCount;
    PRPC_PROTSEQ_ENDPOINT RpcProtseqEndpoint;
    ULONG_PTR Reserved;
    void const *InterpreterInfo;
    unsigned int Flags;
} RPC_CLIENT_INTERFACE, *PRPC_CLIENT_INTERFACE;

typedef void *I_RPC_MUTEX;

typedef void (__RPC_USER *PRPC_RUNDOWN)(void*);

typedef struct _RPC_TRANSFER_SYNTAX {
    UUID Uuid;
    unsigned short VersMajor;
    unsigned short VersMinor;
} RPC_TRANSFER_SYNTAX;

typedef void (*RPCLT_PDU_FILTER_FUNC)(void*,unsigned int,int);
typedef void (__cdecl *RPC_SETFILTER_FUNC)(RPCLT_PDU_FILTER_FUNC);

#ifndef WINNT
typedef RPC_STATUS (*RPC_BLOCKING_FN)(void*,void*,void*);
#endif

RPC_ADDRESS_CHANGE_FN *RPC_ENTRY I_RpcServerInqAddressChangeFn();
RPC_STATUS RPC_ENTRY I_RpcServerSetAddressChangeFn(RPC_ADDRESS_CHANGE_FN*);

RPCRTAPI void RPC_ENTRY I_RpcRequestMutex(I_RPC_MUTEX*);
RPCRTAPI void RPC_ENTRY I_RpcClearMutex(I_RPC_MUTEX);
RPCRTAPI void RPC_ENTRY I_RpcDeleteMutex(I_RPC_MUTEX);
RPCRTAPI void *RPC_ENTRY I_RpcAllocate(unsigned int);
RPCRTAPI void RPC_ENTRY I_RpcFree(void*);
RPCRTAPI void RPC_ENTRY I_RpcPauseExecution(unsigned long);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcGetExtendedError();
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcGetBuffer(RPC_MESSAGE*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcGetBufferWithObject(RPC_MESSAGE*,UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcSendReceive(RPC_MESSAGE*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcFreeBuffer(RPC_MESSAGE*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcSend(PRPC_MESSAGE);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcReceive(PRPC_MESSAGE,unsigned int);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcFreePipeBuffer(RPC_MESSAGE*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcReallocPipeBuffer(PRPC_MESSAGE,unsigned int);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcMonitorAssociation(RPC_BINDING_HANDLE,PRPC_RUNDOWN,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcStopMonitorAssociation(RPC_BINDING_HANDLE);
RPCRTAPI RPC_BINDING_HANDLE RPC_ENTRY I_RpcGetCurrentCallHandle(void);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcGetAssociationContext(RPC_BINDING_HANDLE,void**);
RPCRTAPI void * RPC_ENTRY I_RpcGetServerContextList(RPC_BINDING_HANDLE);
RPCRTAPI void RPC_ENTRY I_RpcSetServerContextList(RPC_BINDING_HANDLE,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqTransportType(RPC_BINDING_HANDLE,unsigned int*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcNsInterfaceExported(unsigned long,unsigned short*,RPC_SERVER_INTERFACE*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcNsInterfaceUnexported(unsigned long,unsigned short*,RPC_SERVER_INTERFACE*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingToStaticStringBindingW(RPC_BINDING_HANDLE,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqSecurityContext(RPC_BINDING_HANDLE,void**);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqWireIdForSnego(RPC_BINDING_HANDLE,unsigned char*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcIfInqTransferSyntaxes(RPC_IF_HANDLE,RPC_TRANSFER_SYNTAX*,unsigned int,unsigned int*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_UuidCreate(UUID*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingCopy(RPC_BINDING_HANDLE,RPC_BINDING_HANDLE*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingIsClientLocal(RPC_BINDING_HANDLE,unsigned int*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqConnId(RPC_BINDING_HANDLE,void**,int*);
RPCRTAPI void RPC_ENTRY I_RpcSsDontSerializeContext(void);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcLaunchDatagramReceiveThread(void*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerRegisterForwardFunction(RPC_FORWARD_FUNCTION*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcConnectionInqSockBuffSize(unsigned long*,unsigned long*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcConnectionSetSockBuffSize(unsigned long,unsigned long);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerInqTransportType(unsigned int*);
RPCRTAPI long RPC_ENTRY I_RpcMapWin32Status(RPC_STATUS);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcNsBindingSetEntryNameW(RPC_BINDING_HANDLE,unsigned long,unsigned short*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcNsBindingSetEntryNameA(RPC_BINDING_HANDLE,unsigned long,unsigned char*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUseProtseqEp2A(unsigned char*,unsigned char*,unsigned int,unsigned char*,void*,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUseProtseqEp2W(unsigned short*,unsigned short*,unsigned int,unsigned short*,void*,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUseProtseq2W(unsigned short*,unsigned short*,unsigned int,void*,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUseProtseq2A(unsigned char*,unsigned char*,unsigned int,void*,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqDynamicEndpointW(RPC_BINDING_HANDLE,unsigned short**);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqDynamicEndpointA(RPC_BINDING_HANDLE,unsigned char**);
#ifndef WINNT
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerStartListening(void*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerStopListening();
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingSetAsync(RPC_BINDING_HANDLE,RPC_BLOCKING_FN,unsigned long);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcSetThreadParams(int,void*,void*);
RPCRTAPI unsigned int RPC_ENTRY I_RpcWindowProc(void*,unsigned int,unsigned int,unsigned long);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUnregisterEndpointA(unsigned char*,unsigned char*);
RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUnregisterEndpointW(unsigned short*,unsigned short*);
#endif /* WINNT */

#ifdef UNICODE
#define I_RpcNsBindingSetEntryName I_RpcNsBindingSetEntryNameW
#define I_RpcServerUseProtseqEp2 I_RpcServerUseProtseqEp2W
#define I_RpcServerUseProtseq2 I_RpcServerUseProtseq2W
#define I_RpcBindingInqDynamicEndpoint I_RpcBindingInqDynamicEndpointW
#define I_RpcServerUnregisterEndpoint I_RpcServerUnregisterEndpointW  /* WINNT */
#else
#define I_RpcNsBindingSetEntryName I_RpcNsBindingSetEntryNameA
#define I_RpcServerUseProtseqEp2 I_RpcServerUseProtseqEp2A
#define I_RpcServerUseProtseq2 I_RpcServerUseProtseq2A
#define I_RpcBindingInqDynamicEndpoint I_RpcBindingInqDynamicEndpointA
#define I_RpcServerUnregisterEndpoint I_RpcServerUnregisterEndpointA  /* WINNT */
#endif

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
}
#endif

#endif /* _RPCDCEP_H */
