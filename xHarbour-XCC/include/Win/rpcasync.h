#ifndef _RPCASYNC_H
#define _RPCASYNC_H

/* Windows Async RPC runtime API definitions */

#if defined(__RPC_WIN64__)
#include <pshpack8.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define RPC_ASYNC_VERSION_1_0  sizeof(RPC_ASYNC_STATE)

#define RPC_C_NOTIFY_ON_SEND_COMPLETE  0x1
#define RPC_C_INFINITE_TIMEOUT  INFINITE

#define RpcAsyncGetCallHandle(pAsync)  (((PRPC_ASYNC_STATE) pAsync)->RuntimeInfo)

typedef enum _RPC_NOTIFICATION_TYPES {
    RpcNotificationTypeNone,
    RpcNotificationTypeEvent,
    RpcNotificationTypeApc,
    RpcNotificationTypeIoc,
    RpcNotificationTypeHwnd,
    RpcNotificationTypeCallback
} RPC_NOTIFICATION_TYPES;

typedef enum _RPC_ASYNC_EVENT {
    RpcCallComplete,
    RpcSendComplete,
    RpcReceiveComplete
} RPC_ASYNC_EVENT;

struct _RPC_ASYNC_STATE;

typedef void RPC_ENTRY RPCNOTIFICATION_ROUTINE(struct _RPC_ASYNC_STATE*,void*,RPC_ASYNC_EVENT);
typedef RPCNOTIFICATION_ROUTINE *PFN_RPCNOTIFICATION_ROUTINE;

typedef struct _RPC_ASYNC_STATE {
    unsigned int Size;
    unsigned long Signature;
    long Lock;
    unsigned long Flags;
    void *StubInfo;
    void *UserInfo;
    void *RuntimeInfo;
    RPC_ASYNC_EVENT Event;
    RPC_NOTIFICATION_TYPES NotificationType;
    union {
        struct {
            PFN_RPCNOTIFICATION_ROUTINE NotificationRoutine;
            HANDLE hThread;
        } APC;
        struct {
            HANDLE hIOPort;
            DWORD dwNumberOfBytesTransferred;
            DWORD_PTR dwCompletionKey;
            LPOVERLAPPED lpOverlapped;
        } IOC;
        struct {
            HWND hWnd;
            UINT Msg;
        } HWND;
        HANDLE hEvent;
        PFN_RPCNOTIFICATION_ROUTINE NotificationRoutine;
    } u;
    LONG_PTR Reserved[4];
} RPC_ASYNC_STATE, *PRPC_ASYNC_STATE;

RPCRTAPI RPC_STATUS RPC_ENTRY RpcAsyncInitializeHandle(PRPC_ASYNC_STATE,unsigned int);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcAsyncRegisterInfo(PRPC_ASYNC_STATE);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcAsyncGetCallStatus(PRPC_ASYNC_STATE);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcAsyncCompleteCall(PRPC_ASYNC_STATE,void*);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcAsyncAbortCall(PRPC_ASYNC_STATE,unsigned long);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcAsyncCancelCall(PRPC_ASYNC_STATE,BOOL);
RPCRTAPI RPC_STATUS RPC_ENTRY RpcAsyncCleanupThread(DWORD);

RPC_STATUS RPC_ENTRY I_RpcAsyncSetHandle(PRPC_MESSAGE,PRPC_ASYNC_STATE);
RPC_STATUS RPC_ENTRY I_RpcAsyncAbortCall(PRPC_ASYNC_STATE,unsigned long);

#ifdef __cplusplus
}
#endif

#if defined(__RPC_WIN64__)
#include <poppack.h>
#endif

#endif /* _RPCASYNC_H */
