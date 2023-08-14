#ifndef _EVNTRACE_H
#define _EVNTRACE_H

/* Windows NT Event tracing definitions */

#if defined(_WINNT_) || defined(WINNT)

#ifndef WMIAPI
#ifdef _WMI_SOURCE_
#define WMIAPI __stdcall
#else
#define WMIAPI DECLSPEC_IMPORT __stdcall
#endif /* _WMI_SOURCE */
#endif /* WMIAPI */

#include <guiddef.h>

#define MAX_MOF_FIELDS  16

#define EVENT_TRACE_TYPE_INFO  0x00
#define EVENT_TRACE_TYPE_START  0x01
#define EVENT_TRACE_TYPE_END  0x02
#define EVENT_TRACE_TYPE_DC_START  0x03
#define EVENT_TRACE_TYPE_DC_END  0x04
#define EVENT_TRACE_TYPE_EXTENSION  0x05
#define EVENT_TRACE_TYPE_REPLY  0x06
#define EVENT_TRACE_TYPE_DEQUEUE  0x07
#define EVENT_TRACE_TYPE_CHECKPOINT  0x08
#define EVENT_TRACE_TYPE_RESERVED9  0x09

#define EVENT_TRACE_TYPE_LOAD  0x0A

#define EVENT_TRACE_TYPE_IO_READ  0x0A
#define EVENT_TRACE_TYPE_IO_WRITE  0x0B

#define EVENT_TRACE_TYPE_MM_TF  0x0A
#define EVENT_TRACE_TYPE_MM_DZF  0x0B
#define EVENT_TRACE_TYPE_MM_COW  0x0C
#define EVENT_TRACE_TYPE_MM_GPF  0x0D
#define EVENT_TRACE_TYPE_MM_HPF  0x0E

#define EVENT_TRACE_TYPE_SEND  0x0A
#define EVENT_TRACE_TYPE_RECEIVE  0x0B
#define EVENT_TRACE_TYPE_CONNECT  0x0C
#define EVENT_TRACE_TYPE_DISCONNECT  0x0D

#define EVENT_TRACE_TYPE_GUIDMAP  0x0A
#define EVENT_TRACE_TYPE_CONFIG  0x0B
#define EVENT_TRACE_TYPE_SIDINFO  0x0C
#define EVENT_TRACE_TYPE_SECURITY  0x0D

#define EVENT_TRACE_TYPE_REGCREATE  0x0A
#define EVENT_TRACE_TYPE_REGOPEN  0x0B
#define EVENT_TRACE_TYPE_REGDELETE  0x0C
#define EVENT_TRACE_TYPE_REGQUERY  0x0D
#define EVENT_TRACE_TYPE_REGSETVALUE  0x0E
#define EVENT_TRACE_TYPE_REGDELETEVALUE  0x0F
#define EVENT_TRACE_TYPE_REGQUERYVALUE  0x10
#define EVENT_TRACE_TYPE_REGENUMERATEKEY  0x11
#define EVENT_TRACE_TYPE_REGENUMERATEVALUEKEY  0x12
#define EVENT_TRACE_TYPE_REGQUERYMULTIPLEVALUE  0x13
#define EVENT_TRACE_TYPE_REGSETINFORMATION  0x14
#define EVENT_TRACE_TYPE_REGFLUSH  0x15

#define EVENT_TRACE_FLAG_PROCESS  0x00000001
#define EVENT_TRACE_FLAG_THREAD  0x00000002
#define EVENT_TRACE_FLAG_IMAGE_LOAD  0x00000004
#define EVENT_TRACE_FLAG_DISK_IO  0x00000100
#define EVENT_TRACE_FLAG_DISK_FILE_IO  0x00000200
#define EVENT_TRACE_FLAG_MEMORY_PAGE_FAULTS 0x00001000
#define EVENT_TRACE_FLAG_MEMORY_HARD_FAULTS 0x00002000
#define EVENT_TRACE_FLAG_NETWORK_TCPIP  0x00010000
#define EVENT_TRACE_FLAG_REGISTRY  0x00020000
#define EVENT_TRACE_FLAG_EXTENSION  0x80000000
#define EVENT_TRACE_FLAG_FORWARD_WMI  0x40000000
#define EVENT_TRACE_FLAG_ENABLE_RESERVE  0x20000000

#define EVENT_TRACE_FILE_MODE_NONE  0x0000
#define EVENT_TRACE_FILE_MODE_SEQUENTIAL  0x0001
#define EVENT_TRACE_FILE_MODE_CIRCULAR  0x0002

#define EVENT_TRACE_REAL_TIME_MODE  0x0100
#define EVENT_TRACE_DELAY_OPEN_FILE_MODE  0x0200
#define EVENT_TRACE_BUFFERING_MODE  0x0400
#define EVENT_TRACE_PRIVATE_LOGGER_MODE  0x0800
#define EVENT_TRACE_ADD_HEADER_MODE  0x1000

#define EVENT_TRACE_CONTROL_QUERY  0
#define EVENT_TRACE_CONTROL_STOP  1
#define EVENT_TRACE_CONTROL_UPDATE  2

#define DEFINE_TRACE_MOF_FIELD(MOF,ptr,length,type) \
    (MOF)->DataPtr = (ULONG64)ptr; \
    (MOF)->Length = (ULONG)length; \
    (MOF)->DataType = (ULONG)type;

DEFINE_GUID(EventTraceGuid,0x68fdd900,0x4a3e,0x11d1,0x84,0xf4,0x00,0x00,0xf8,0x04,0x64,0xe3);
DEFINE_GUID(SystemTraceControlGuid,0x9e814aad,0x3204,0x11d2,0x9a,0x82,0x00,0x60,0x08,0xa8,0x69,0x39);

typedef ULONG64 TRACEHANDLE, *PTRACEHANDLE;

typedef struct _TRACE_ENABLE_CONTEXT {
    USHORT LoggerId;
    UCHAR Level;
    UCHAR InternalFlag;
    ULONG EnableFlags;
} TRACE_ENABLE_CONTEXT, *PTRACE_ENABLE_CONTEXT;

typedef struct _EVENT_TRACE_HEADER {
    USHORT Size;
    UCHAR HeaderType;
    UCHAR MarkerFlags;
    union {
        ULONG Version;
        struct {
            UCHAR Type;
            UCHAR Level;
            USHORT Version;
        } Class;
    };
    ULONGLONG ThreadId;
    LARGE_INTEGER TimeStamp;
    union {
        GUID Guid;
        ULONGLONG GuidPtr;
    };
    union {
        struct {
            ULONG ClientContext;
            ULONG Flags;
        };
        struct {
            ULONG KernelTime;
            ULONG UserTime;
        };
        ULONG64 ProcessorTime;
    };
} EVENT_TRACE_HEADER, *PEVENT_TRACE_HEADER;

typedef struct _EVENT_INSTANCE_HEADER {
    USHORT Size;
    UCHAR HeaderType;
    UCHAR MarkerFlags;
    union {
        ULONG Version;
        struct {
            UCHAR Type;
            UCHAR Level;
            USHORT Version;
        } Class;
    };
    ULONGLONG ThreadId;
    LARGE_INTEGER TimeStamp;
    ULONGLONG RegHandle;
    ULONG InstanceId;
    ULONG ParentInstanceId;
    union {
        struct {
            ULONG ClientContext;
            ULONG Flags;
        };
        struct {
            ULONG KernelTime;
            ULONG UserTime;
        };
        ULONG64 ProcessorTime;
    };
    ULONGLONG ParentRegHandle;
} EVENT_INSTANCE_HEADER, *PEVENT_INSTANCE_HEADER;

typedef struct _MOF_FIELD {
    ULONG64 DataPtr;
    ULONG Length;
    ULONG DataType;
} MOF_FIELD, *PMOF_FIELD;

#if !defined(_NTDDK_) || defined(_WMIKM_)
typedef struct _TRACE_LOGFILE_HEADER {
    ULONG BufferSize;
    union {
        ULONG Version;
        struct {
            UCHAR MajorVersion;
            UCHAR MinorVersion;
            UCHAR SubVersion;
            UCHAR SubMinorVersion;
        } VersionDetail;
    };
    ULONG ProviderVersion;
    ULONG NumberOfProcessors;
    LARGE_INTEGER EndTime;
    ULONG TimerResolution;
    ULONG MaximumFileSize;
    ULONG LogFileMode;
    ULONG BuffersWritten;
    union {
        GUID LogInstanceGuid;
        struct {
            ULONG StartBuffers;
            ULONG PointerSize;
            ULONG EventsLost;
            ULONG Reserved32;
        };
    };
#if defined(_WMIKM_)
    PWCHAR LoggerName;
    PWCHAR LogFileName;
    RTL_TIME_ZONE_INFORMATION TimeZone;
#else
    LPWSTR LoggerName;
    LPWSTR LogFileName;
    TIME_ZONE_INFORMATION TimeZone;
#endif /* _WMIKM_ */
    LARGE_INTEGER BootTime;
    LARGE_INTEGER PerfFreq;
    LARGE_INTEGER StartTime;
    ULONG ReservedFlags;
    ULONG BuffersLost;
} TRACE_LOGFILE_HEADER, *PTRACE_LOGFILE_HEADER;
#endif /* !_NTDDK_ || _WMIKM_ */

typedef struct EVENT_INSTANCE_INFO {
    HANDLE RegHandle;
    ULONG InstanceId;
} EVENT_INSTANCE_INFO, *PEVENT_INSTANCE_INFO;

#if !defined(_WMIKM_) && !defined(_NTDDK_)
typedef struct _EVENT_TRACE_PROPERTIES {
    WNODE_HEADER Wnode;
    ULONG BufferSize;
    ULONG MinimumBuffers;
    ULONG MaximumBuffers;
    ULONG MaximumFileSize;
    ULONG LogFileMode;
    ULONG FlushTimer;
    ULONG EnableFlags;
    LONG AgeLimit;
    ULONG NumberOfBuffers;
    ULONG FreeBuffers;
    ULONG EventsLost;
    ULONG BuffersWritten;
    ULONG LogBuffersLost;
    ULONG RealTimeBuffersLost;
    HANDLE LoggerThreadId;
    ULONG LogFileNameOffset;
    ULONG LoggerNameOffset;
} EVENT_TRACE_PROPERTIES, *PEVENT_TRACE_PROPERTIES;

typedef struct _TRACE_GUID_REGISTRATION {
    LPCGUID Guid;
    HANDLE RegHandle;
} TRACE_GUID_REGISTRATION, *PTRACE_GUID_REGISTRATION;

typedef struct _EVENT_TRACE {
    EVENT_TRACE_HEADER Header;
    ULONG InstanceId;
    ULONG ParentInstanceId;
    GUID ParentGuid;
    PVOID MofData;
    ULONG MofLength;
    ULONG ClientContext;
} EVENT_TRACE, *PEVENT_TRACE;

typedef struct _EVENT_TRACE_LOGFILEW EVENT_TRACE_LOGFILEW, *PEVENT_TRACE_LOGFILEW;
typedef struct _EVENT_TRACE_LOGFILEA EVENT_TRACE_LOGFILEA, *PEVENT_TRACE_LOGFILEA;
typedef ULONG (WINAPI *PEVENT_TRACE_BUFFER_CALLBACKW)(PEVENT_TRACE_LOGFILEW);
typedef ULONG (WINAPI *PEVENT_TRACE_BUFFER_CALLBACKA)(PEVENT_TRACE_LOGFILEA);
typedef VOID (WINAPI *PEVENT_CALLBACK)(PEVENT_TRACE);
typedef ULONG (WINAPI *WMIDPREQUEST)(WMIDPREQUESTCODE,PVOID,ULONG*,PVOID);

struct _EVENT_TRACE_LOGFILEW {
    LPWSTR LogFileName;
    LPWSTR LoggerName;
    LONGLONG CurrentTime;
    ULONG BuffersRead;
    ULONG LogFileMode;
    EVENT_TRACE CurrentEvent;
    TRACE_LOGFILE_HEADER LogfileHeader;
    PEVENT_TRACE_BUFFER_CALLBACKW BufferCallback;
    ULONG BufferSize;
    ULONG Filled;
    ULONG EventsLost;
    PEVENT_CALLBACK EventCallback;
    ULONG IsKernelTrace;
    PVOID Context;
};

struct _EVENT_TRACE_LOGFILEA {
    LPSTR LogFileName;
    LPSTR LoggerName;
    LONGLONG CurrentTime;
    ULONG BuffersRead;
    ULONG LogFileMode;
    EVENT_TRACE CurrentEvent;
    TRACE_LOGFILE_HEADER LogfileHeader;
    PEVENT_TRACE_BUFFER_CALLBACKA BufferCallback;
    ULONG BufferSize;
    ULONG Filled;
    ULONG EventsLost;
    PEVENT_CALLBACK EventCallback;
    ULONG IsKernelTrace;
    PVOID Context;
};

#ifdef __cplusplus
extern "C" {
#endif

EXTERN_C ULONG WMIAPI StartTraceW(PTRACEHANDLE,LPCWSTR,PEVENT_TRACE_PROPERTIES);
EXTERN_C ULONG WMIAPI StartTraceA(PTRACEHANDLE,LPCSTR,PEVENT_TRACE_PROPERTIES);
EXTERN_C ULONG WMIAPI ControlTraceW(TRACEHANDLE,LPCWSTR,PEVENT_TRACE_PROPERTIES,ULONG);
EXTERN_C ULONG WMIAPI ControlTraceA(TRACEHANDLE,LPCSTR,PEVENT_TRACE_PROPERTIES,ULONG);
EXTERN_C ULONG WMIAPI QueryAllTracesW(PEVENT_TRACE_PROPERTIES*,ULONG,PULONG);
EXTERN_C ULONG WMIAPI QueryAllTracesA(PEVENT_TRACE_PROPERTIES*,ULONG,PULONG);
EXTERN_C ULONG WMIAPI CreateTraceInstanceId(HANDLE,PEVENT_INSTANCE_INFO);
EXTERN_C ULONG WMIAPI EnableTrace(ULONG,ULONG,ULONG,LPCGUID,TRACEHANDLE);
EXTERN_C ULONG WMIAPI TraceEvent(TRACEHANDLE,PEVENT_TRACE_HEADER);
EXTERN_C ULONG WMIAPI TraceEventInstance(TRACEHANDLE,PEVENT_INSTANCE_HEADER,PEVENT_INSTANCE_INFO,PEVENT_INSTANCE_INFO);
EXTERN_C ULONG WMIAPI RegisterTraceGuidsW(WMIDPREQUEST,PVOID,LPCGUID,ULONG,PTRACE_GUID_REGISTRATION,LPCWSTR,LPCWSTR,PTRACEHANDLE);
EXTERN_C ULONG WMIAPI RegisterTraceGuidsA(WMIDPREQUEST,PVOID,LPCGUID,ULONG,PTRACE_GUID_REGISTRATION,LPCSTR,LPCSTR,PTRACEHANDLE);
EXTERN_C ULONG WMIAPI UnregisterTraceGuids(TRACEHANDLE);
EXTERN_C TRACEHANDLE WMIAPI GetTraceLoggerHandle(PVOID);
EXTERN_C UCHAR WMIAPI GetTraceEnableLevel(TRACEHANDLE);
EXTERN_C ULONG WMIAPI GetTraceEnableFlags(TRACEHANDLE);
EXTERN_C TRACEHANDLE WMIAPI OpenTraceA(PEVENT_TRACE_LOGFILEA);
EXTERN_C TRACEHANDLE WMIAPI OpenTraceW(PEVENT_TRACE_LOGFILEW);
EXTERN_C ULONG WMIAPI ProcessTrace(PTRACEHANDLE,ULONG,LPFILETIME,LPFILETIME);
EXTERN_C ULONG WMIAPI CloseTrace(TRACEHANDLE);
EXTERN_C ULONG WMIAPI SetTraceCallback(LPCGUID,PEVENT_CALLBACK);
EXTERN_C ULONG WMIAPI RemoveTraceCallback(LPCGUID);

#if (WINVER >= 0x0501)
EXTERN_C ULONG WMIAPI FlushTraceW(TRACEHANDLE,LPCWSTR,PEVENT_TRACE_PROPERTIES);
EXTERN_C ULONG WMIAPI FlushTraceA(TRACEHANDLE,LPCSTR,PEVENT_TRACE_PROPERTIES);
EXTERN_C ULONG WMIAPI EnumerateTraceGuids(PTRACE_GUID_PROPERTIES*,ULONG,PULONG);
#endif /* WINVER >= 0x0501 */

#ifdef __cplusplus
}
#endif

#ifdef UNICODE
#define PEVENT_TRACE_BUFFER_CALLBACK  PEVENT_TRACE_BUFFER_CALLBACKW
#define EVENT_TRACE_LOGFILE  EVENT_TRACE_LOGFILEW
#define PEVENT_TRACE_LOGFILE  PEVENT_TRACE_LOGFILEW
#define RegisterTraceGuids  RegisterTraceGuidsW
#define StartTrace  StartTraceW
#define ControlTrace  ControlTraceW
#define StopTrace(a,b,c)  ControlTraceW((a),(b),(c),EVENT_TRACE_CONTROL_STOP)
#define QueryTrace(a,b,c)  ControlTraceW((a),(b),(c),EVENT_TRACE_CONTROL_QUERY)
#define UpdateTrace(a,b,c)  ControlTraceW((a),(b),(c),EVENT_TRACE_CONTROL_UPDATE)
#define QueryAllTraces  QueryAllTracesW
#define OpenTrace  OpenTraceW
#if (WINVER >= 0x0501)
#define FlushTrace  FlushTraceW
#endif /* WINVER >= 0x0501 */
#else
#define PEVENT_TRACE_BUFFER_CALLBACK  PEVENT_TRACE_BUFFER_CALLBACKA
#define EVENT_TRACE_LOGFILE  EVENT_TRACE_LOGFILEA
#define PEVENT_TRACE_LOGFILE  PEVENT_TRACE_LOGFILEA
#define RegisterTraceGuids  RegisterTraceGuidsA
#define StartTrace  StartTraceA
#define ControlTrace  ControlTraceA
#define StopTrace(a,b,c)  ControlTraceA((a),(b),(c),EVENT_TRACE_CONTROL_STOP)
#define QueryTrace(a,b,c)  ControlTraceA((a),(b),(c),EVENT_TRACE_CONTROL_QUERY)
#define UpdateTrace(a,b,c)  ControlTraceA((a),(b),(c),EVENT_TRACE_CONTROL_UPDATE)
#define QueryAllTraces  QueryAllTracesA
#define OpenTrace  OpenTraceA
#if (WINVER >= 0x0501)
#define FlushTrace  FlushTraceA
#endif /* WINVER >= 0x0501 */
#endif /* UNICODE */

#endif /* _WMIKM_  && _NTDDK_ */

#endif /* WINNT */
#endif /* _EVNTRACE_ */
