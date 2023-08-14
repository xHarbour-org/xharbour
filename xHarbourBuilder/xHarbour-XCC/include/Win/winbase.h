#ifndef _WINBASE_H
#define _WINBASE_H

/* Windows Base API definitions */

#define WINADVAPI DECLSPEC_IMPORT
#define WINBASEAPI DECLSPEC_IMPORT

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2028)  /* Missing prototype */
#pragma warn(disable:2197)  /* 'type' is not a standard bit-field type */
#pragma warn(disable:2198)  /* Nameless field is not standard */
#endif

#define DefineHandleTable(w)  ((w),TRUE)
#define LimitEmsPages(dw)
#define SetSwapAreaSize(w)  (w)
#define LockSegment(w)  GlobalFix((HANDLE)(w))
#define UnlockSegment(w)  GlobalUnfix((HANDLE)(w))
#define GetCurrentTime()  GetTickCount()
#define Yield()
#define INVALID_HANDLE_VALUE  ((HANDLE)-1)
#define INVALID_FILE_SIZE  ((DWORD)0xFFFFFFFF)
#define INVALID_SET_FILE_POINTER  ((DWORD)-1)
#define FILE_BEGIN  0
#define FILE_CURRENT  1
#define FILE_END  2
#define TIME_ZONE_ID_INVALID  ((DWORD)0xFFFFFFFF)
#define WAIT_FAILED  ((DWORD)0xFFFFFFFF)
#define WAIT_OBJECT_0  ((STATUS_WAIT_0 )+0)
#define WAIT_ABANDONED  ((STATUS_ABANDONED_WAIT_0)+0)
#define WAIT_ABANDONED_0  ((STATUS_ABANDONED_WAIT_0)+0)
#define WAIT_IO_COMPLETION  STATUS_USER_APC
#define STILL_ACTIVE  STATUS_PENDING

#define EXCEPTION_ACCESS_VIOLATION  STATUS_ACCESS_VIOLATION
#define EXCEPTION_DATATYPE_MISALIGNMENT  STATUS_DATATYPE_MISALIGNMENT
#define EXCEPTION_BREAKPOINT  STATUS_BREAKPOINT
#define EXCEPTION_SINGLE_STEP  STATUS_SINGLE_STEP
#define EXCEPTION_ARRAY_BOUNDS_EXCEEDED  STATUS_ARRAY_BOUNDS_EXCEEDED
#define EXCEPTION_FLT_DENORMAL_OPERAND  STATUS_FLOAT_DENORMAL_OPERAND
#define EXCEPTION_FLT_DIVIDE_BY_ZERO  STATUS_FLOAT_DIVIDE_BY_ZERO
#define EXCEPTION_FLT_INEXACT_RESULT  STATUS_FLOAT_INEXACT_RESULT
#define EXCEPTION_FLT_INVALID_OPERATION  STATUS_FLOAT_INVALID_OPERATION
#define EXCEPTION_FLT_OVERFLOW  STATUS_FLOAT_OVERFLOW
#define EXCEPTION_FLT_STACK_CHECK  STATUS_FLOAT_STACK_CHECK
#define EXCEPTION_FLT_UNDERFLOW  STATUS_FLOAT_UNDERFLOW
#define EXCEPTION_INT_DIVIDE_BY_ZERO  STATUS_INTEGER_DIVIDE_BY_ZERO
#define EXCEPTION_INT_OVERFLOW  STATUS_INTEGER_OVERFLOW
#define EXCEPTION_PRIV_INSTRUCTION  STATUS_PRIVILEGED_INSTRUCTION
#define EXCEPTION_IN_PAGE_ERROR  STATUS_IN_PAGE_ERROR
#define EXCEPTION_ILLEGAL_INSTRUCTION  STATUS_ILLEGAL_INSTRUCTION
#define EXCEPTION_NONCONTINUABLE_EXCEPTION  STATUS_NONCONTINUABLE_EXCEPTION
#define EXCEPTION_STACK_OVERFLOW  STATUS_STACK_OVERFLOW
#define EXCEPTION_INVALID_DISPOSITION  STATUS_INVALID_DISPOSITION
#define EXCEPTION_GUARD_PAGE  STATUS_GUARD_PAGE_VIOLATION
#define EXCEPTION_INVALID_HANDLE  STATUS_INVALID_HANDLE
#define CONTROL_C_EXIT  STATUS_CONTROL_C_EXIT

#define MoveMemory  RtlMoveMemory
#define CopyMemory  RtlCopyMemory
#define FillMemory  RtlFillMemory
#define ZeroMemory  RtlZeroMemory

#define FILE_FLAG_WRITE_THROUGH  0x80000000
#define FILE_FLAG_OVERLAPPED  0x40000000
#define FILE_FLAG_NO_BUFFERING  0x20000000
#define FILE_FLAG_RANDOM_ACCESS  0x10000000
#define FILE_FLAG_SEQUENTIAL_SCAN  0x08000000
#define FILE_FLAG_DELETE_ON_CLOSE  0x04000000
#define FILE_FLAG_BACKUP_SEMANTICS  0x02000000
#define FILE_FLAG_POSIX_SEMANTICS  0x01000000
#define FILE_FLAG_OPEN_REPARSE_POINT  0x00200000
#define FILE_FLAG_OPEN_NO_RECALL  0x00100000

#define CREATE_NEW  1
#define CREATE_ALWAYS  2
#define OPEN_EXISTING  3
#define OPEN_ALWAYS  4
#define TRUNCATE_EXISTING  5

#if (_WIN32_WINNT >= 0x0400)
#define PROGRESS_CONTINUE  0
#define PROGRESS_CANCEL  1
#define PROGRESS_STOP  2
#define PROGRESS_QUIET  3
#define CALLBACK_CHUNK_FINISHED  0x00000000
#define CALLBACK_STREAM_SWITCH  0x00000001
#define COPY_FILE_FAIL_IF_EXISTS  0x00000001
#define COPY_FILE_RESTARTABLE  0x00000002
#define COPY_FILE_OPEN_SOURCE_FOR_WRITE  0x00000004
#endif /* _WIN32_WINNT >= 0x0400 */

#if (_WIN32_WINNT >= 0x0500)
#define REPLACEFILE_WRITE_THROUGH  0x00000001
#define REPLACEFILE_IGNORE_MERGE_ERRORS  0x00000002
#endif /* _WIN32_WINNT >= 0x0500 */

#define PIPE_ACCESS_INBOUND  0x00000001
#define PIPE_ACCESS_OUTBOUND  0x00000002
#define PIPE_ACCESS_DUPLEX  0x00000003
#define PIPE_CLIENT_END  0x00000000
#define PIPE_SERVER_END  0x00000001
#define PIPE_WAIT  0x00000000
#define PIPE_NOWAIT  0x00000001
#define PIPE_READMODE_BYTE  0x00000000
#define PIPE_READMODE_MESSAGE  0x00000002
#define PIPE_TYPE_BYTE  0x00000000
#define PIPE_TYPE_MESSAGE  0x00000004
#define PIPE_UNLIMITED_INSTANCES  255
#define SECURITY_ANONYMOUS  (SecurityAnonymous<<16)
#define SECURITY_IDENTIFICATION  (SecurityIdentification<<16)
#define SECURITY_IMPERSONATION  (SecurityImpersonation<<16)
#define SECURITY_DELEGATION  (SecurityDelegation<<16)
#define SECURITY_CONTEXT_TRACKING  0x00040000
#define SECURITY_EFFECTIVE_ONLY  0x00080000
#define SECURITY_SQOS_PRESENT  0x00100000
#define SECURITY_VALID_SQOS_FLAGS  0x001F0000

#define MUTEX_MODIFY_STATE MUTANT_QUERY_STATE
#define MUTEX_ALL_ACCESS MUTANT_ALL_ACCESS

#define SP_SERIALCOMM  ((DWORD)0x00000001)

#define PST_UNSPECIFIED  ((DWORD)0x00000000)
#define PST_RS232  ((DWORD)0x00000001)
#define PST_PARALLELPORT  ((DWORD)0x00000002)
#define PST_RS422  ((DWORD)0x00000003)
#define PST_RS423  ((DWORD)0x00000004)
#define PST_RS449  ((DWORD)0x00000005)
#define PST_MODEM  ((DWORD)0x00000006)
#define PST_FAX  ((DWORD)0x00000021)
#define PST_SCANNER  ((DWORD)0x00000022)
#define PST_NETWORK_BRIDGE  ((DWORD)0x00000100)
#define PST_LAT  ((DWORD)0x00000101)
#define PST_TCPIP_TELNET  ((DWORD)0x00000102)
#define PST_X25  ((DWORD)0x00000103)

#define PCF_DTRDSR  ((DWORD)0x0001)
#define PCF_RTSCTS  ((DWORD)0x0002)
#define PCF_RLSD  ((DWORD)0x0004)
#define PCF_PARITY_CHECK  ((DWORD)0x0008)
#define PCF_XONXOFF  ((DWORD)0x0010)
#define PCF_SETXCHAR  ((DWORD)0x0020)
#define PCF_TOTALTIMEOUTS  ((DWORD)0x0040)
#define PCF_INTTIMEOUTS  ((DWORD)0x0080)
#define PCF_SPECIALCHARS  ((DWORD)0x0100)
#define PCF_16BITMODE  ((DWORD)0x0200)

#define SP_PARITY  ((DWORD)0x0001)
#define SP_BAUD  ((DWORD)0x0002)
#define SP_DATABITS  ((DWORD)0x0004)
#define SP_STOPBITS  ((DWORD)0x0008)
#define SP_HANDSHAKING  ((DWORD)0x0010)
#define SP_PARITY_CHECK  ((DWORD)0x0020)
#define SP_RLSD  ((DWORD)0x0040)

#define BAUD_075  ((DWORD)0x00000001)
#define BAUD_110  ((DWORD)0x00000002)
#define BAUD_134_5  ((DWORD)0x00000004)
#define BAUD_150  ((DWORD)0x00000008)
#define BAUD_300  ((DWORD)0x00000010)
#define BAUD_600  ((DWORD)0x00000020)
#define BAUD_1200  ((DWORD)0x00000040)
#define BAUD_1800  ((DWORD)0x00000080)
#define BAUD_2400  ((DWORD)0x00000100)
#define BAUD_4800  ((DWORD)0x00000200)
#define BAUD_7200  ((DWORD)0x00000400)
#define BAUD_9600  ((DWORD)0x00000800)
#define BAUD_14400  ((DWORD)0x00001000)
#define BAUD_19200  ((DWORD)0x00002000)
#define BAUD_38400  ((DWORD)0x00004000)
#define BAUD_56K  ((DWORD)0x00008000)
#define BAUD_128K  ((DWORD)0x00010000)
#define BAUD_115200  ((DWORD)0x00020000)
#define BAUD_57600  ((DWORD)0x00040000)
#define BAUD_USER  ((DWORD)0x10000000)

#define DATABITS_5  ((WORD)0x0001)
#define DATABITS_6  ((WORD)0x0002)
#define DATABITS_7  ((WORD)0x0004)
#define DATABITS_8  ((WORD)0x0008)
#define DATABITS_16  ((WORD)0x0010)
#define DATABITS_16X  ((WORD)0x0020)

#define STOPBITS_10  ((WORD)0x0001)
#define STOPBITS_15  ((WORD)0x0002)
#define STOPBITS_20  ((WORD)0x0004)
#define PARITY_NONE  ((WORD)0x0100)
#define PARITY_ODD  ((WORD)0x0200)
#define PARITY_EVEN  ((WORD)0x0400)
#define PARITY_MARK  ((WORD)0x0800)
#define PARITY_SPACE  ((WORD)0x1000)

#define COMMPROP_INITIALIZED ((DWORD)0xE73CF52E)

#define DTR_CONTROL_DISABLE  0x00
#define DTR_CONTROL_ENABLE  0x01
#define DTR_CONTROL_HANDSHAKE  0x02

#define RTS_CONTROL_DISABLE  0x00
#define RTS_CONTROL_ENABLE  0x01
#define RTS_CONTROL_HANDSHAKE  0x02
#define RTS_CONTROL_TOGGLE  0x03

#define GMEM_FIXED  0x0000
#define GMEM_MOVEABLE  0x0002
#define GMEM_NOCOMPACT  0x0010
#define GMEM_NODISCARD  0x0020
#define GMEM_ZEROINIT  0x0040
#define GMEM_MODIFY  0x0080
#define GMEM_DISCARDABLE  0x0100
#define GMEM_NOT_BANKED  0x1000
#define GMEM_SHARE  0x2000
#define GMEM_DDESHARE  0x2000
#define GMEM_NOTIFY  0x4000
#define GMEM_LOWER  GMEM_NOT_BANKED
#define GMEM_VALID_FLAGS  0x7F72
#define GMEM_INVALID_HANDLE  0x8000
#define GMEM_DISCARDED 0x4000
#define GMEM_LOCKCOUNT 0x00FF

#define GHND  (GMEM_MOVEABLE|GMEM_ZEROINIT)
#define GPTR  (GMEM_FIXED|GMEM_ZEROINIT)

#define LMEM_FIXED  0x0000
#define LMEM_MOVEABLE  0x0002
#define LMEM_NOCOMPACT  0x0010
#define LMEM_NODISCARD  0x0020
#define LMEM_ZEROINIT  0x0040
#define LMEM_MODIFY  0x0080
#define LMEM_DISCARDABLE  0x0F00
#define LMEM_VALID_FLAGS  0x0F72
#define LMEM_INVALID_HANDLE  0x8000
#define LMEM_DISCARDED  0x4000
#define LMEM_LOCKCOUNT  0x00FF

#define LHND  (LMEM_MOVEABLE|LMEM_ZEROINIT)
#define LPTR  (LMEM_FIXED|LMEM_ZEROINIT)

#define NONZEROLHND  (LMEM_MOVEABLE)
#define NONZEROLPTR  (LMEM_FIXED)

#define FreeModule(hLibModule)  FreeLibrary((hLibModule))
#define MakeProcInstance(lpProc,hInstance)  (lpProc)
#define FreeProcInstance(lpProc)  (lpProc)

#define GlobalLRUNewest(h)  ((HANDLE)(h))
#define GlobalLRUOldest(h)  ((HANDLE)(h))
#define GlobalDiscard(h)  GlobalReAlloc((h),0,GMEM_MOVEABLE)
#define LocalDiscard(h)  LocalReAlloc((h),0,LMEM_MOVEABLE)

#define DEBUG_PROCESS  0x00000001
#define DEBUG_ONLY_THIS_PROCESS  0x00000002
#define CREATE_SUSPENDED  0x00000004
#define DETACHED_PROCESS  0x00000008
#define CREATE_NEW_CONSOLE  0x00000010
#define NORMAL_PRIORITY_CLASS  0x00000020
#define IDLE_PRIORITY_CLASS  0x00000040
#define HIGH_PRIORITY_CLASS  0x00000080
#define REALTIME_PRIORITY_CLASS  0x00000100
#define CREATE_NEW_PROCESS_GROUP  0x00000200
#define CREATE_UNICODE_ENVIRONMENT  0x00000400
#define CREATE_SEPARATE_WOW_VDM  0x00000800
#define CREATE_SHARED_WOW_VDM  0x00001000
#define CREATE_FORCEDOS  0x00002000
#define BELOW_NORMAL_PRIORITY_CLASS  0x00004000
#define ABOVE_NORMAL_PRIORITY_CLASS  0x00008000
#define CREATE_BREAKAWAY_FROM_JOB  0x01000000
#define CREATE_WITH_USERPROFILE  0x02000000
#define CREATE_DEFAULT_ERROR_MODE  0x04000000
#define CREATE_NO_WINDOW  0x08000000
#define PROFILE_USER  0x10000000
#define PROFILE_KERNEL  0x20000000
#define PROFILE_SERVER  0x40000000

#define THREAD_PRIORITY_LOWEST  THREAD_BASE_PRIORITY_MIN
#define THREAD_PRIORITY_BELOW_NORMAL  (THREAD_PRIORITY_LOWEST+1)
#define THREAD_PRIORITY_NORMAL  0
#define THREAD_PRIORITY_HIGHEST  THREAD_BASE_PRIORITY_MAX
#define THREAD_PRIORITY_ABOVE_NORMAL  (THREAD_PRIORITY_HIGHEST-1)
#define THREAD_PRIORITY_ERROR_RETURN  (MAXLONG)
#define THREAD_PRIORITY_TIME_CRITICAL  THREAD_BASE_PRIORITY_LOWRT
#define THREAD_PRIORITY_IDLE  THREAD_BASE_PRIORITY_IDLE

#define EXCEPTION_DEBUG_EVENT  1
#define CREATE_THREAD_DEBUG_EVENT  2
#define CREATE_PROCESS_DEBUG_EVENT  3
#define EXIT_THREAD_DEBUG_EVENT  4
#define EXIT_PROCESS_DEBUG_EVENT  5
#define LOAD_DLL_DEBUG_EVENT  6
#define UNLOAD_DLL_DEBUG_EVENT  7
#define OUTPUT_DEBUG_STRING_EVENT  8
#define RIP_EVENT  9

#define DRIVE_UNKNOWN  0
#define DRIVE_NO_ROOT_DIR  1
#define DRIVE_REMOVABLE  2
#define DRIVE_FIXED  3
#define DRIVE_REMOTE  4
#define DRIVE_CDROM  5
#define DRIVE_RAMDISK  6

#define GetFreeSpace(w)  (0x100000L)

#define FILE_TYPE_UNKNOWN  0x0000
#define FILE_TYPE_DISK  0x0001
#define FILE_TYPE_CHAR  0x0002
#define FILE_TYPE_PIPE  0x0003
#define FILE_TYPE_REMOTE  0x8000

#define STD_INPUT_HANDLE  ((DWORD)-10)
#define STD_OUTPUT_HANDLE  ((DWORD)-11)
#define STD_ERROR_HANDLE  ((DWORD)-12)

#define NOPARITY  0
#define ODDPARITY  1
#define EVENPARITY  2
#define MARKPARITY  3
#define SPACEPARITY  4

#define ONESTOPBIT  0
#define ONE5STOPBITS  1
#define TWOSTOPBITS  2

#define IGNORE  0
#define INFINITE  0xFFFFFFFF

#define CBR_110  110
#define CBR_300  300
#define CBR_600  600
#define CBR_1200  1200
#define CBR_2400  2400
#define CBR_4800  4800
#define CBR_9600  9600
#define CBR_14400  14400
#define CBR_19200  19200
#define CBR_38400  38400
#define CBR_56000  56000
#define CBR_57600  57600
#define CBR_115200  115200
#define CBR_128000  128000
#define CBR_256000  256000

#define CE_RXOVER  0x0001
#define CE_OVERRUN  0x0002
#define CE_RXPARITY  0x0004
#define CE_FRAME  0x0008
#define CE_BREAK  0x0010
#define CE_TXFULL  0x0100
#define CE_PTO  0x0200
#define CE_IOE  0x0400
#define CE_DNS  0x0800
#define CE_OOP  0x1000
#define CE_MODE  0x8000

#define IE_BADID  (-1)
#define IE_OPEN  (-2)
#define IE_NOPEN  (-3)
#define IE_MEMORY  (-4)
#define IE_DEFAULT  (-5)
#define IE_HARDWARE  (-10)
#define IE_BYTESIZE  (-11)
#define IE_BAUDRATE  (-12)

#define EV_RXCHAR  0x0001
#define EV_RXFLAG  0x0002
#define EV_TXEMPTY  0x0004
#define EV_CTS  0x0008
#define EV_DSR  0x0010
#define EV_RLSD  0x0020
#define EV_BREAK  0x0040
#define EV_ERR  0x0080
#define EV_RING  0x0100
#define EV_PERR  0x0200
#define EV_RX80FULL  0x0400
#define EV_EVENT1  0x0800
#define EV_EVENT2  0x1000

#define SETXOFF  1
#define SETXON  2
#define SETRTS  3
#define CLRRTS  4
#define SETDTR  5
#define CLRDTR  6
#define RESETDEV  7
#define SETBREAK  8
#define CLRBREAK  9

#define PURGE_TXABORT  0x0001
#define PURGE_RXABORT  0x0002
#define PURGE_TXCLEAR  0x0004
#define PURGE_RXCLEAR  0x0008

#define LPTx  0x80

#define MS_CTS_ON  ((DWORD)0x0010)
#define MS_DSR_ON  ((DWORD)0x0020)
#define MS_RING_ON  ((DWORD)0x0040)
#define MS_RLSD_ON  ((DWORD)0x0080)

#define S_QUEUEEMPTY  0
#define S_THRESHOLD  1
#define S_ALLTHRESHOLD  2

#define S_NORMAL  0
#define S_LEGATO  1
#define S_STACCATO  2

#define S_PERIOD512  0
#define S_PERIOD1024  1
#define S_PERIOD2048  2
#define S_PERIODVOICE  3
#define S_WHITE512  4
#define S_WHITE1024  5
#define S_WHITE2048  6
#define S_WHITEVOICE  7

#define S_SERDVNA  (-1)
#define S_SEROFM  (-2)
#define S_SERMACT  (-3)
#define S_SERQFUL  (-4)
#define S_SERBDNT  (-5)
#define S_SERDLN  (-6)
#define S_SERDCC  (-7)
#define S_SERDTP  (-8)
#define S_SERDVL  (-9)
#define S_SERDMD  (-10)
#define S_SERDSH  (-11)
#define S_SERDPT  (-12)
#define S_SERDFQ  (-13)
#define S_SERDDR  (-14)
#define S_SERDSR  (-15)
#define S_SERDST  (-16)

#define NMPWAIT_WAIT_FOREVER  0xFFFFFFFF
#define NMPWAIT_NOWAIT  0x00000001
#define NMPWAIT_USE_DEFAULT_WAIT  0x00000000

#define FS_CASE_IS_PRESERVED  FILE_CASE_PRESERVED_NAMES
#define FS_CASE_SENSITIVE  FILE_CASE_SENSITIVE_SEARCH
#define FS_UNICODE_STORED_ON_DISK  FILE_UNICODE_ON_DISK
#define FS_PERSISTENT_ACLS  FILE_PERSISTENT_ACLS
#define FS_VOL_IS_COMPRESSED  FILE_VOLUME_IS_COMPRESSED
#define FS_FILE_COMPRESSION  FILE_FILE_COMPRESSION
#define FS_FILE_ENCRYPTION  FILE_SUPPORTS_ENCRYPTION

#define FILE_MAP_COPY  SECTION_QUERY
#define FILE_MAP_WRITE  SECTION_MAP_WRITE
#define FILE_MAP_READ  SECTION_MAP_READ
#define FILE_MAP_ALL_ACCESS  SECTION_ALL_ACCESS

#define OF_READ  0x00000000
#define OF_WRITE  0x00000001
#define OF_READWRITE  0x00000002
#define OF_SHARE_COMPAT  0x00000000
#define OF_SHARE_EXCLUSIVE  0x00000010
#define OF_SHARE_DENY_WRITE  0x00000020
#define OF_SHARE_DENY_READ  0x00000030
#define OF_SHARE_DENY_NONE  0x00000040
#define OF_PARSE  0x00000100
#define OF_DELETE  0x00000200
#define OF_VERIFY  0x00000400
#define OF_CANCEL  0x00000800
#define OF_CREATE  0x00001000
#define OF_PROMPT  0x00002000
#define OF_EXIST  0x00004000
#define OF_REOPEN  0x00008000

#define UnlockResource(hResData)  ((hResData),0)
#define MAXINTATOM  0xC000
#define MAKEINTATOM(i)  (LPTSTR)((ULONG_PTR)((WORD)(i)))
#define INVALID_ATOM  ((ATOM)0)

#define PROCESS_HEAP_REGION  0x0001
#define PROCESS_HEAP_UNCOMMITTED_RANGE  0x0002
#define PROCESS_HEAP_ENTRY_BUSY  0x0004
#define PROCESS_HEAP_ENTRY_MOVEABLE  0x0010
#define PROCESS_HEAP_ENTRY_DDESHARE  0x0020

#define SCS_32BIT_BINARY  0
#define SCS_DOS_BINARY  1
#define SCS_WOW_BINARY  2
#define SCS_PIF_BINARY  3
#define SCS_POSIX_BINARY  4
#define SCS_OS216_BINARY  5

#define SEM_FAILCRITICALERRORS  0x0001
#define SEM_NOGPFAULTERRORBOX  0x0002
#define SEM_NOALIGNMENTFAULTEXCEPT  0x0004
#define SEM_NOOPENFILEERRORBOX  0x8000

#define LOCKFILE_FAIL_IMMEDIATELY  0x00000001
#define LOCKFILE_EXCLUSIVE_LOCK  0x00000002

#define HANDLE_FLAG_INHERIT  0x00000001
#define HANDLE_FLAG_PROTECT_FROM_CLOSE  0x00000002

#define HINSTANCE_ERROR  32

#define GET_TAPE_MEDIA_INFORMATION  0
#define GET_TAPE_DRIVE_INFORMATION  1

#define SET_TAPE_MEDIA_INFORMATION  0
#define SET_TAPE_DRIVE_INFORMATION  1

#define FORMAT_MESSAGE_ALLOCATE_BUFFER  0x00000100
#define FORMAT_MESSAGE_IGNORE_INSERTS  0x00000200
#define FORMAT_MESSAGE_FROM_STRING  0x00000400
#define FORMAT_MESSAGE_FROM_HMODULE  0x00000800
#define FORMAT_MESSAGE_FROM_SYSTEM  0x00001000
#define FORMAT_MESSAGE_ARGUMENT_ARRAY  0x00002000
#define FORMAT_MESSAGE_MAX_WIDTH_MASK  0x000000FF

#define FILE_ENCRYPTABLE  0
#define FILE_IS_ENCRYPTED  1
#define FILE_SYSTEM_ATTR  2
#define FILE_ROOT_DIR  3
#define FILE_SYSTEM_DIR  4
#define FILE_UNKNOWN  5
#define FILE_SYSTEM_NOT_SUPPORT  6
#define FILE_USER_DISALLOWED  7
#define FILE_READ_ONLY  8
#define FILE_DIR_DISALLOWED  9

#define EFS_USE_RECOVERY_KEYS  (0x1)

#define CREATE_FOR_IMPORT  (1)
#define CREATE_FOR_DIR  (2)

#define TLS_OUT_OF_INDEXES  (DWORD)0xFFFFFFFF

#define BACKUP_INVALID  0x00000000
#define BACKUP_DATA  0x00000001
#define BACKUP_EA_DATA  0x00000002
#define BACKUP_SECURITY_DATA  0x00000003
#define BACKUP_ALTERNATE_DATA  0x00000004
#define BACKUP_LINK  0x00000005
#define BACKUP_PROPERTY_DATA  0x00000006
#define BACKUP_OBJECT_ID  0x00000007
#define BACKUP_REPARSE_DATA  0x00000008
#define BACKUP_SPARSE_BLOCK  0x00000009

#define STREAM_NORMAL_ATTRIBUTE  0x00000000
#define STREAM_MODIFIED_WHEN_READ  0x00000001
#define STREAM_CONTAINS_SECURITY  0x00000002
#define STREAM_CONTAINS_PROPERTIES  0x00000004
#define STREAM_SPARSE_ATTRIBUTE  0x00000008

#define STARTF_USESHOWWINDOW  0x00000001
#define STARTF_USESIZE  0x00000002
#define STARTF_USEPOSITION  0x00000004
#define STARTF_USECOUNTCHARS  0x00000008
#define STARTF_USEFILLATTRIBUTE  0x00000010
#define STARTF_RUNFULLSCREEN  0x00000020
#define STARTF_FORCEONFEEDBACK  0x00000040
#define STARTF_FORCEOFFFEEDBACK  0x00000080
#define STARTF_USESTDHANDLES  0x00000100
#define STARTF_USEHOTKEY  0x00000200

#define SHUTDOWN_NORETRY  0x00000001

#define DONT_RESOLVE_DLL_REFERENCES  0x00000001
#define LOAD_LIBRARY_AS_DATAFILE  0x00000002
#define LOAD_WITH_ALTERED_SEARCH_PATH  0x00000008

#define DDD_RAW_TARGET_PATH  0x00000001
#define DDD_REMOVE_DEFINITION  0x00000002
#define DDD_EXACT_MATCH_ON_REMOVE  0x00000004
#define DDD_NO_BROADCAST_SYSTEM  0x00000008

#define EXPAND_LOCAL_DRIVES

#if (_WIN32_WINNT >= 0x0400)
#define FIND_FIRST_EX_CASE_SENSITIVE  0x00000001
#endif /* _WIN32_WINNT >= 0x0400 */

#define MOVEFILE_REPLACE_EXISTING  0x00000001
#define MOVEFILE_COPY_ALLOWED  0x00000002
#define MOVEFILE_DELAY_UNTIL_REBOOT  0x00000004
#define MOVEFILE_WRITE_THROUGH  0x00000008
#if (_WIN32_WINNT >= 0x0500)
#define MOVEFILE_CREATE_HARDLINK  0x00000010
#define MOVEFILE_FAIL_IF_NOT_TRACKABLE  0x00000020
#endif /* _WIN32_WINNT >= 0x0500 */

#define EVENTLOG_FULL_INFO  0

#define MAX_COMPUTERNAME_LENGTH  15

#define LOGON32_LOGON_INTERACTIVE  2
#define LOGON32_LOGON_NETWORK  3
#define LOGON32_LOGON_BATCH  4
#define LOGON32_LOGON_SERVICE  5
#define LOGON32_LOGON_UNLOCK  7
#if (_WIN32_WINNT >= 0x0500)
#define LOGON32_LOGON_NETWORK_CLEARTEXT  8
#define LOGON32_LOGON_NEW_CREDENTIALS  9
#endif /* _WIN32_WINNT >= 0x0500 */

#define LOGON32_PROVIDER_DEFAULT  0
#define LOGON32_PROVIDER_WINNT35  1
#if (_WIN32_WINNT >= 0x0400)
#define LOGON32_PROVIDER_WINNT40  2
#endif /* _WIN32_WINNT >= 0x0400 */
#if (_WIN32_WINNT >= 0x0500)
#define LOGON32_PROVIDER_WINNT50  3
#endif /* _WIN32_WINNT >= 0x0500 */

#if (_WIN32_WINNT >= 0x0500)
#define LOGON_WITH_PROFILE  0x00000001
#define LOGON_NETCREDENTIALS_ONLY  0x00000002
#endif /* _WIN32_WINNT >= 0x0500 */

#define MAX_MANGLED_SITE  (27)

#if (_WIN32_WINNT >= 0x0400)
#define HW_PROFILE_GUIDLEN  39
#define MAX_PROFILE_LEN  80

#define DOCKINFO_UNDOCKED  (0x1)
#define DOCKINFO_DOCKED  (0x2)
#define DOCKINFO_USER_SUPPLIED  (0x4)
#define DOCKINFO_USER_UNDOCKED  (DOCKINFO_USER_SUPPLIED|DOCKINFO_UNDOCKED)
#define DOCKINFO_USER_DOCKED  (DOCKINFO_USER_SUPPLIED|DOCKINFO_DOCKED)
#endif /* _WIN32_WINNT >= 0x0400 */

#define TC_NORMAL  0
#define TC_HARDERR  1
#define TC_GP_TRAP  2
#define TC_SIGNAL  3

#define AC_LINE_OFFLINE  0x00
#define AC_LINE_ONLINE  0x01
#define AC_LINE_BACKUP_POWER  0x02
#define AC_LINE_UNKNOWN  0xFF

#define BATTERY_FLAG_HIGH  0x01
#define BATTERY_FLAG_LOW  0x02
#define BATTERY_FLAG_CRITICAL  0x04
#define BATTERY_FLAG_CHARGING  0x08
#define BATTERY_FLAG_NO_BATTERY  0x80
#define BATTERY_FLAG_UNKNOWN  0xFF
#define BATTERY_PERCENTAGE_UNKNOWN  0xFF
#define BATTERY_LIFE_UNKNOWN  0xFFFFFFFF

#if (_WIN32_WINNT >= 0x0501)
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_A_A  "GetSystemWow64DirectoryA"
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_A_W  L"GetSystemWow64DirectoryA"
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_A_T  TEXT("GetSystemWow64DirectoryA")
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_W_A  "GetSystemWow64DirectoryW"
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_W_W  L"GetSystemWow64DirectoryW"
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_W_T  TEXT("GetSystemWow64DirectoryW")
#endif /* _WIN32_WINNT >= 0x0501 */

typedef struct _OVERLAPPED {
    ULONG_PTR Internal;
    ULONG_PTR InternalHigh;
    DWORD Offset;
    DWORD OffsetHigh;
    HANDLE hEvent;
} OVERLAPPED, *LPOVERLAPPED;

typedef struct _SECURITY_ATTRIBUTES {
    DWORD nLength;
    LPVOID lpSecurityDescriptor;
    BOOL bInheritHandle;
} SECURITY_ATTRIBUTES, *PSECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;

typedef struct _PROCESS_INFORMATION {
    HANDLE hProcess;
    HANDLE hThread;
    DWORD dwProcessId;
    DWORD dwThreadId;
} PROCESS_INFORMATION, *PPROCESS_INFORMATION, *LPPROCESS_INFORMATION;

typedef struct _FILETIME {
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
} FILETIME, *PFILETIME, *LPFILETIME;

typedef struct _SYSTEMTIME {
    WORD wYear;
    WORD wMonth;
    WORD wDayOfWeek;
    WORD wDay;
    WORD wHour;
    WORD wMinute;
    WORD wSecond;
    WORD wMilliseconds;
} SYSTEMTIME, *PSYSTEMTIME, *LPSYSTEMTIME;

typedef DWORD(WINAPI *PTHREAD_START_ROUTINE)(LPVOID);
typedef PTHREAD_START_ROUTINE LPTHREAD_START_ROUTINE;

#if (_WIN32_WINNT >= 0x0400)
typedef VOID(WINAPI *PFIBER_START_ROUTINE)(LPVOID);
typedef PFIBER_START_ROUTINE LPFIBER_START_ROUTINE;
#endif /* _WIN32_WINNT >= 0x0400 */

#ifndef _WINCE
typedef RTL_CRITICAL_SECTION CRITICAL_SECTION;
typedef PRTL_CRITICAL_SECTION PCRITICAL_SECTION;
typedef PRTL_CRITICAL_SECTION LPCRITICAL_SECTION;
typedef RTL_CRITICAL_SECTION_DEBUG CRITICAL_SECTION_DEBUG;
typedef PRTL_CRITICAL_SECTION_DEBUG PCRITICAL_SECTION_DEBUG;
typedef PRTL_CRITICAL_SECTION_DEBUG LPCRITICAL_SECTION_DEBUG;
#endif /* _WINCE */

#if defined(_X86_) || defined(_IA64_)
typedef PLDT_ENTRY LPLDT_ENTRY;
#else
typedef LPVOID LPLDT_ENTRY;
#endif

typedef struct _COMMPROP {
    WORD wPacketLength;
    WORD wPacketVersion;
    DWORD dwServiceMask;
    DWORD dwReserved1;
    DWORD dwMaxTxQueue;
    DWORD dwMaxRxQueue;
    DWORD dwMaxBaud;
    DWORD dwProvSubType;
    DWORD dwProvCapabilities;
    DWORD dwSettableParams;
    DWORD dwSettableBaud;
    WORD wSettableData;
    WORD wSettableStopParity;
    DWORD dwCurrentTxQueue;
    DWORD dwCurrentRxQueue;
    DWORD dwProvSpec1;
    DWORD dwProvSpec2;
    WCHAR wcProvChar[1];
} COMMPROP, *LPCOMMPROP;

typedef struct _COMSTAT {
    DWORD fCtsHold:1;
    DWORD fDsrHold:1;
    DWORD fRlsdHold:1;
    DWORD fXoffHold:1;
    DWORD fXoffSent:1;
    DWORD fEof:1;
    DWORD fTxim:1;
    DWORD fReserved:25;
    DWORD cbInQue;
    DWORD cbOutQue;
} COMSTAT, *LPCOMSTAT;

typedef struct _DCB {
    DWORD DCBlength;
    DWORD BaudRate;
    DWORD fBinary:1;
    DWORD fParity:1;
    DWORD fOutxCtsFlow:1;
    DWORD fOutxDsrFlow:1;
    DWORD fDtrControl:2;
    DWORD fDsrSensitivity:1;
    DWORD fTXContinueOnXoff:1;
    DWORD fOutX:1;
    DWORD fInX:1;
    DWORD fErrorChar:1;
    DWORD fNull:1;
    DWORD fRtsControl:2;
    DWORD fAbortOnError:1;
    DWORD fDummy2:17;
    WORD wReserved;
    WORD XonLim;
    WORD XoffLim;
    BYTE ByteSize;
    BYTE Parity;
    BYTE StopBits;
    char XonChar;
    char XoffChar;
    char ErrorChar;
    char EofChar;
    char EvtChar;
    WORD wReserved1;
} DCB, *LPDCB;

typedef struct _COMMTIMEOUTS {
    DWORD ReadIntervalTimeout;
    DWORD ReadTotalTimeoutMultiplier;
    DWORD ReadTotalTimeoutConstant;
    DWORD WriteTotalTimeoutMultiplier;
    DWORD WriteTotalTimeoutConstant;
} COMMTIMEOUTS, *LPCOMMTIMEOUTS;

typedef struct _COMMCONFIG {
    DWORD dwSize;
    WORD wVersion;
    WORD wReserved;
    DCB dcb;
    DWORD dwProviderSubType;
    DWORD dwProviderOffset;
    DWORD dwProviderSize;
    WCHAR wcProviderData[1];
} COMMCONFIG, *LPCOMMCONFIG;

typedef struct _SYSTEM_INFO {
    union {
        DWORD dwOemId;
        struct {
            WORD wProcessorArchitecture;
            WORD wReserved;
        };
    };
    DWORD dwPageSize;
    LPVOID lpMinimumApplicationAddress;
    LPVOID lpMaximumApplicationAddress;
    DWORD_PTR dwActiveProcessorMask;
    DWORD dwNumberOfProcessors;
    DWORD dwProcessorType;
    DWORD dwAllocationGranularity;
    WORD wProcessorLevel;
    WORD wProcessorRevision;
} SYSTEM_INFO, *LPSYSTEM_INFO;

typedef struct _MEMORYSTATUS {
    DWORD dwLength;
    DWORD dwMemoryLoad;
    SIZE_T dwTotalPhys;
    SIZE_T dwAvailPhys;
    SIZE_T dwTotalPageFile;
    SIZE_T dwAvailPageFile;
    SIZE_T dwTotalVirtual;
    SIZE_T dwAvailVirtual;
} MEMORYSTATUS, *LPMEMORYSTATUS;

typedef struct _EXCEPTION_DEBUG_INFO {
    EXCEPTION_RECORD ExceptionRecord;
    DWORD dwFirstChance;
} EXCEPTION_DEBUG_INFO, *LPEXCEPTION_DEBUG_INFO;

typedef struct _CREATE_THREAD_DEBUG_INFO {
    HANDLE hThread;
    LPVOID lpThreadLocalBase;
    LPTHREAD_START_ROUTINE lpStartAddress;
} CREATE_THREAD_DEBUG_INFO, *LPCREATE_THREAD_DEBUG_INFO;

typedef struct _CREATE_PROCESS_DEBUG_INFO {
    HANDLE hFile;
    HANDLE hProcess;
    HANDLE hThread;
    LPVOID lpBaseOfImage;
    DWORD dwDebugInfoFileOffset;
    DWORD nDebugInfoSize;
    LPVOID lpThreadLocalBase;
    LPTHREAD_START_ROUTINE lpStartAddress;
    LPVOID lpImageName;
    WORD fUnicode;
} CREATE_PROCESS_DEBUG_INFO, *LPCREATE_PROCESS_DEBUG_INFO;

typedef struct _EXIT_THREAD_DEBUG_INFO {
    DWORD dwExitCode;
} EXIT_THREAD_DEBUG_INFO, *LPEXIT_THREAD_DEBUG_INFO;

typedef struct _EXIT_PROCESS_DEBUG_INFO {
    DWORD dwExitCode;
} EXIT_PROCESS_DEBUG_INFO, *LPEXIT_PROCESS_DEBUG_INFO;

typedef struct _LOAD_DLL_DEBUG_INFO {
    HANDLE hFile;
    LPVOID lpBaseOfDll;
    DWORD dwDebugInfoFileOffset;
    DWORD nDebugInfoSize;
    LPVOID lpImageName;
    WORD fUnicode;
} LOAD_DLL_DEBUG_INFO, *LPLOAD_DLL_DEBUG_INFO;

typedef struct _UNLOAD_DLL_DEBUG_INFO {
    LPVOID lpBaseOfDll;
} UNLOAD_DLL_DEBUG_INFO, *LPUNLOAD_DLL_DEBUG_INFO;

typedef struct _OUTPUT_DEBUG_STRING_INFO {
    LPSTR lpDebugStringData;
    WORD fUnicode;
    WORD nDebugStringLength;
} OUTPUT_DEBUG_STRING_INFO, *LPOUTPUT_DEBUG_STRING_INFO;

typedef struct _RIP_INFO {
    DWORD dwError;
    DWORD dwType;
} RIP_INFO, *LPRIP_INFO;

typedef struct _DEBUG_EVENT {
    DWORD dwDebugEventCode;
    DWORD dwProcessId;
    DWORD dwThreadId;
    union {
        EXCEPTION_DEBUG_INFO Exception;
        CREATE_THREAD_DEBUG_INFO CreateThread;
        CREATE_PROCESS_DEBUG_INFO CreateProcessInfo;
        EXIT_THREAD_DEBUG_INFO ExitThread;
        EXIT_PROCESS_DEBUG_INFO ExitProcess;
        LOAD_DLL_DEBUG_INFO LoadDll;
        UNLOAD_DLL_DEBUG_INFO UnloadDll;
        OUTPUT_DEBUG_STRING_INFO DebugString;
        RIP_INFO RipInfo;
    } u;
} DEBUG_EVENT, *LPDEBUG_EVENT;

typedef PCONTEXT LPCONTEXT;
typedef PEXCEPTION_RECORD LPEXCEPTION_RECORD;
typedef PEXCEPTION_POINTERS LPEXCEPTION_POINTERS;

#define OFS_MAXPATHNAME  128
typedef struct _OFSTRUCT {
        BYTE cBytes;
        BYTE fFixedDisk;
        WORD nErrCode;
        WORD Reserved1;
        WORD Reserved2;
        CHAR szPathName[OFS_MAXPATHNAME];
} OFSTRUCT, *LPOFSTRUCT, *POFSTRUCT;

typedef struct _MEMORYSTATUSEX {
        DWORD dwLength;
        DWORD dwMemoryLoad;
        DWORDLONG ullTotalPhys;
        DWORDLONG ullAvailPhys;
        DWORDLONG ullTotalPageFile;
        DWORDLONG ullAvailPageFile;
        DWORDLONG ullTotalVirtual;
        DWORDLONG ullAvailVirtual;
        DWORDLONG ullAvailExtendedVirtual;
} MEMORYSTATUSEX, *LPMEMORYSTATUSEX;

typedef struct _PROCESS_HEAP_ENTRY {
    PVOID lpData;
    DWORD cbData;
    BYTE cbOverhead;
    BYTE iRegionIndex;
    WORD wFlags;
    union {
        struct {
            HANDLE hMem;
            DWORD dwReserved[3];
        } Block;
        struct {
            DWORD dwCommittedSize;
            DWORD dwUnCommittedSize;
            LPVOID lpFirstBlock;
            LPVOID lpLastBlock;
        } Region;
    };
} PROCESS_HEAP_ENTRY, *LPPROCESS_HEAP_ENTRY, *PPROCESS_HEAP_ENTRY;

typedef LONG(WINAPI *PTOP_LEVEL_EXCEPTION_FILTER)(struct _EXCEPTION_POINTERS*);
typedef PTOP_LEVEL_EXCEPTION_FILTER LPTOP_LEVEL_EXCEPTION_FILTER;

typedef struct _BY_HANDLE_FILE_INFORMATION {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD dwVolumeSerialNumber;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD nNumberOfLinks;
    DWORD nFileIndexHigh;
    DWORD nFileIndexLow;
} BY_HANDLE_FILE_INFORMATION, *PBY_HANDLE_FILE_INFORMATION, *LPBY_HANDLE_FILE_INFORMATION;

typedef struct _TIME_ZONE_INFORMATION {
        LONG Bias;
        WCHAR StandardName[32];
        SYSTEMTIME StandardDate;
        LONG StandardBias;
        WCHAR DaylightName[32];
        SYSTEMTIME DaylightDate;
        LONG DaylightBias;
} TIME_ZONE_INFORMATION, *PTIME_ZONE_INFORMATION, *LPTIME_ZONE_INFORMATION;

typedef DWORD(WINAPI *PFE_EXPORT_FUNC)(PBYTE,PVOID,ULONG);
typedef DWORD(WINAPI *PFE_IMPORT_FUNC)(PBYTE,PVOID,PULONG);

typedef VOID(WINAPI *LPOVERLAPPED_COMPLETION_ROUTINE)(DWORD,DWORD,LPOVERLAPPED);

typedef struct _WIN32_STREAM_ID {
    DWORD dwStreamId;
    DWORD dwStreamAttributes;
    LARGE_INTEGER Size;
    DWORD dwStreamNameSize;
    WCHAR cStreamName[ANYSIZE_ARRAY];
} WIN32_STREAM_ID, *LPWIN32_STREAM_ID;

typedef struct _STARTUPINFOA {
    DWORD cb;
    LPSTR lpReserved;
    LPSTR lpDesktop;
    LPSTR lpTitle;
    DWORD dwX;
    DWORD dwY;
    DWORD dwXSize;
    DWORD dwYSize;
    DWORD dwXCountChars;
    DWORD dwYCountChars;
    DWORD dwFillAttribute;
    DWORD dwFlags;
    WORD wShowWindow;
    WORD cbReserved2;
    LPBYTE lpReserved2;
    HANDLE hStdInput;
    HANDLE hStdOutput;
    HANDLE hStdError;
} STARTUPINFOA, *LPSTARTUPINFOA;

typedef struct _STARTUPINFOW {
    DWORD cb;
    LPWSTR lpReserved;
    LPWSTR lpDesktop;
    LPWSTR lpTitle;
    DWORD dwX;
    DWORD dwY;
    DWORD dwXSize;
    DWORD dwYSize;
    DWORD dwXCountChars;
    DWORD dwYCountChars;
    DWORD dwFillAttribute;
    DWORD dwFlags;
    WORD wShowWindow;
    WORD cbReserved2;
    LPBYTE lpReserved2;
    HANDLE hStdInput;
    HANDLE hStdOutput;
    HANDLE hStdError;
} STARTUPINFOW, *LPSTARTUPINFOW;

typedef struct _WIN32_FIND_DATAA {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    CHAR cFileName[MAX_PATH];
    CHAR cAlternateFileName[14];
} WIN32_FIND_DATAA, *PWIN32_FIND_DATAA, *LPWIN32_FIND_DATAA;

typedef struct _WIN32_FIND_DATAW {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    WCHAR cFileName[MAX_PATH];
    WCHAR cAlternateFileName[14];
} WIN32_FIND_DATAW, *PWIN32_FIND_DATAW, *LPWIN32_FIND_DATAW;

typedef struct _WIN32_FILE_ATTRIBUTE_DATA {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
} WIN32_FILE_ATTRIBUTE_DATA, *LPWIN32_FILE_ATTRIBUTE_DATA;

typedef enum _GET_FILEEX_INFO_LEVELS {
    GetFileExInfoStandard,
    GetFileExMaxInfoLevel
} GET_FILEEX_INFO_LEVELS;

#if (_WIN32_WINNT >= 0x0400)
typedef enum _FINDEX_INFO_LEVELS {
    FindExInfoStandard,
    FindExInfoMaxInfoLevel
} FINDEX_INFO_LEVELS;

typedef enum _FINDEX_SEARCH_OPS {
    FindExSearchNameMatch,
    FindExSearchLimitToDirectories,
    FindExSearchLimitToDevices,
    FindExSearchMaxSearchOp
} FINDEX_SEARCH_OPS;
#endif /* _WIN32_WINNT >= 0x0400 */

#if (_WIN32_WINNT >= 0x0400)
typedef DWORD(WINAPI *LPPROGRESS_ROUTINE)(LARGE_INTEGER,LARGE_INTEGER,LARGE_INTEGER,LARGE_INTEGER,DWORD,DWORD,HANDLE,HANDLE,LPVOID);
#endif /* _WIN32_WINNT >= 0x0400 */

typedef struct _EVENTLOG_FULL_INFORMATION {
    DWORD dwFull;
} EVENTLOG_FULL_INFORMATION, *LPEVENTLOG_FULL_INFORMATION;

#if (_WIN32_WINNT >= 0x0500)
typedef enum _COMPUTER_NAME_FORMAT {
    ComputerNameNetBIOS,
    ComputerNameDnsHostname,
    ComputerNameDnsDomain,
    ComputerNameDnsFullyQualified,
    ComputerNamePhysicalNetBIOS,
    ComputerNamePhysicalDnsHostname,
    ComputerNamePhysicalDnsDomain,
    ComputerNamePhysicalDnsFullyQualified,
    ComputerNameMax
} COMPUTER_NAME_FORMAT;
#endif /* _WIN32_WINNT >= 0x0500 */

#if (_WIN32_WINNT >= 0x0500)
typedef WAITORTIMERCALLBACKFUNC WAITORTIMERCALLBACK;
#endif /* _WIN32_WINNT >= 0x0500 */

#if (_WIN32_WINNT >= 0x0400)
typedef struct tagHW_PROFILE_INFOA {
    DWORD dwDockInfo;
    CHAR szHwProfileGuid[HW_PROFILE_GUIDLEN];
    CHAR szHwProfileName[MAX_PROFILE_LEN];
} HW_PROFILE_INFOA, *LPHW_PROFILE_INFOA;

typedef struct tagHW_PROFILE_INFOW {
    DWORD dwDockInfo;
    WCHAR szHwProfileGuid[HW_PROFILE_GUIDLEN];
    WCHAR szHwProfileName[MAX_PROFILE_LEN];
} HW_PROFILE_INFOW, *LPHW_PROFILE_INFOW;
#endif /* _WIN32_WINNT >= 0x0400 */

typedef struct _SYSTEM_POWER_STATUS {
    BYTE ACLineStatus;
    BYTE BatteryFlag;
    BYTE BatteryLifePercent;
    BYTE Reserved1;
    DWORD BatteryLifeTime;
    DWORD BatteryFullLifeTime;
} SYSTEM_POWER_STATUS, *LPSYSTEM_POWER_STATUS;

#ifdef STRICT
typedef BOOL (CALLBACK *ENUMRESTYPEPROCA)(HMODULE,LPSTR,LONG_PTR);
typedef BOOL (CALLBACK *ENUMRESTYPEPROCW)(HMODULE,LPWSTR,LONG_PTR);
typedef BOOL (CALLBACK *ENUMRESNAMEPROCA)(HMODULE,LPCSTR,LPSTR,LONG_PTR);
typedef BOOL (CALLBACK *ENUMRESNAMEPROCW)(HMODULE,LPCWSTR,LPWSTR,LONG_PTR);
typedef BOOL (CALLBACK *ENUMRESLANGPROCA)(HMODULE,LPCSTR,LPCSTR,WORD,LONG_PTR);
typedef BOOL (CALLBACK *ENUMRESLANGPROCW)(HMODULE,LPCWSTR,LPCWSTR,WORD,LONG_PTR);
#else
typedef FARPROC ENUMRESTYPEPROCA;
typedef FARPROC ENUMRESTYPEPROCW;
typedef FARPROC ENUMRESNAMEPROCA;
typedef FARPROC ENUMRESNAMEPROCW;
typedef FARPROC ENUMRESLANGPROCA;
typedef FARPROC ENUMRESLANGPROCW;
#endif /* STRICT */

#if (_WIN32_WINNT >= 0x0501)
typedef enum _MEMORY_RESOURCE_NOTIFICATION_TYPE {
    LowMemoryResourceNotification,
    HighMemoryResourceNotification
} MEMORY_RESOURCE_NOTIFICATION_TYPE;

typedef UINT (WINAPI* PGET_SYSTEM_WOW64_DIRECTORY_A)(LPSTR,UINT);
typedef UINT (WINAPI* PGET_SYSTEM_WOW64_DIRECTORY_W)(LPWSTR,UINT);

typedef enum {
    WinNullSid = 0,
    WinWorldSid = 1,
    WinLocalSid = 2,
    WinCreatorOwnerSid = 3,
    WinCreatorGroupSid = 4,
    WinCreatorOwnerServerSid = 5,
    WinCreatorGroupServerSid = 6,
    WinNtAuthoritySid = 7,
    WinDialupSid = 8,
    WinNetworkSid = 9,
    WinBatchSid = 10,
    WinInteractiveSid = 11,
    WinServiceSid = 12,
    WinAnonymousSid = 13,
    WinProxySid = 14,
    WinEnterpriseControllersSid = 15,
    WinSelfSid = 16,
    WinAuthenticatedUserSid = 17,
    WinRestrictedCodeSid = 18,
    WinTerminalServerSid = 19,
    WinRemoteLogonIdSid = 20,
    WinLogonIdsSid = 21,
    WinLocalSystemSid = 22,
    WinLocalServiceSid = 23,
    WinNetworkServiceSid = 24,
    WinBuiltinDomainSid = 25,
    WinBuiltinAdministratorsSid = 26,
    WinBuiltinUsersSid = 27,
    WinBuiltinGuestsSid = 28,
    WinBuiltinPowerUsersSid = 29,
    WinBuiltinAccountOperatorsSid = 30,
    WinBuiltinSystemOperatorsSid = 31,
    WinBuiltinPrintOperatorsSid = 32,
    WinBuiltinBackupOperatorsSid = 33,
    WinBuiltinReplicatorSid = 34,
    WinBuiltinPreWindows2000CompatibleAccessSid = 35,
    WinBuiltinRemoteDesktopUsersSid = 36,
    WinBuiltinNetworkConfigurationOperatorsSid = 37,
    WinAccountAdministratorSid = 38,
    WinAccountGuestSid = 39,
    WinAccountKrbtgtSid = 40,
    WinAccountDomainAdminsSid = 41,
    WinAccountDomainUsersSid = 42,
    WinAccountDomainGuestsSid = 43,
    WinAccountComputersSid = 44,
    WinAccountControllersSid = 45,
    WinAccountCertAdminsSid = 46,
    WinAccountSchemaAdminsSid = 47,
    WinAccountEnterpriseAdminsSid = 48,
    WinAccountPolicyAdminsSid = 49,
    WinAccountRasAndIasServersSid = 50,
} WELL_KNOWN_SID_TYPE;
#endif /* _WIN32_WINNT >= 0x0501 */

#if defined(_M_IA64)
#define InterlockedIncrement _InterlockedIncrement
#define InterlockedDecrement _InterlockedDecrement
#define InterlockedExchange _InterlockedExchange
#define InterlockedExchangeAdd _InterlockedExchangeAdd
#define InterlockedCompareExchange _InterlockedCompareExchange
#define InterlockedExchangePointer _InterlockedExchangePointer
#define InterlockedCompareExchangePointer _InterlockedCompareExchangePointer
LONG __cdecl InterlockedIncrement(LPLONG);
LONG __cdecl InterlockedDecrement(LPLONG);
LONG __cdecl InterlockedExchange(LPLONG,LONG);
LONG __cdecl InterlockedExchangeAdd(LPLONG,LONG);
LONG __cdecl InterlockedCompareExchange(PLONG,LONG,LONG);
PVOID __cdecl InterlockedExchangePointer(PVOID*,PVOID);
PVOID __cdecl InterlockedCompareExchangePointer(PVOID*,PVOID,PVOID);
#else /* defined(_M_IA64) */
WINBASEAPI LONG WINAPI InterlockedIncrement(LPLONG);
WINBASEAPI LONG WINAPI InterlockedDecrement(LPLONG);
WINBASEAPI LONG WINAPI InterlockedExchange(LPLONG,LONG);
WINBASEAPI LONG WINAPI InterlockedExchangeAdd(LPLONG,LONG);
WINBASEAPI LONG WINAPI InterlockedCompareExchange(LPLONG,LONG,LONG);
#define InterlockedExchangePointer(Target,Value)  (PVOID)InterlockedExchange((PLONG)(Target),(LONG)(Value))
#define InterlockedCompareExchangePointer(Destination,ExChange,Comperand)  (PVOID)InterlockedCompareExchange((PLONG)(Destination),(LONG)(ExChange),(LONG)(Comperand))
#endif /* defined(_M_IA64) */

#ifdef _WINCE
int WINAPI WinMain(HINSTANCE,HINSTANCE,LPWSTR,int);
#else
int WINAPI WinMain(HINSTANCE,HINSTANCE,LPSTR,int);
#endif /* _WINCE */

WINBASEAPI BOOL WINAPI FreeResource(HGLOBAL);
#ifndef _WINCE
WINBASEAPI LPVOID WINAPI LockResource(HGLOBAL);
#endif /* _WINCE */
WINBASEAPI BOOL WINAPI FreeLibrary(HMODULE);
WINBASEAPI DECLSPEC_NORETURN VOID WINAPI FreeLibraryAndExitThread(HMODULE,DWORD);
WINBASEAPI BOOL WINAPI DisableThreadLibraryCalls(HMODULE);
#ifdef _WINCE
#define GetProcAddress  GetProcAddressW
WINBASEAPI FARPROC WINAPI GetProcAddressW(HMODULE,LPCWSTR);
WINBASEAPI FARPROC WINAPI GetProcAddressA(HMODULE,LPCSTR);
#else
WINBASEAPI FARPROC WINAPI GetProcAddress(HMODULE,LPCSTR);
#endif /* _WINCE */
WINBASEAPI DWORD WINAPI GetVersion(VOID);
WINBASEAPI HGLOBAL WINAPI GlobalAlloc(UINT,SIZE_T);
WINBASEAPI HGLOBAL WINAPI GlobalReAlloc(HGLOBAL,SIZE_T,UINT);
WINBASEAPI SIZE_T WINAPI GlobalSize(HGLOBAL);
WINBASEAPI UINT WINAPI GlobalFlags(HGLOBAL);
WINBASEAPI LPVOID WINAPI GlobalLock(HGLOBAL);
WINBASEAPI HGLOBAL WINAPI GlobalHandle(LPCVOID);
WINBASEAPI BOOL WINAPI GlobalUnlock(HGLOBAL);
WINBASEAPI HGLOBAL WINAPI GlobalFree(HGLOBAL);
WINBASEAPI SIZE_T WINAPI GlobalCompact(DWORD);
WINBASEAPI VOID WINAPI GlobalFix(HGLOBAL);
WINBASEAPI VOID WINAPI GlobalUnfix(HGLOBAL);
WINBASEAPI LPVOID WINAPI GlobalWire(HGLOBAL);
WINBASEAPI BOOL WINAPI GlobalUnWire(HGLOBAL);
WINBASEAPI VOID WINAPI GlobalMemoryStatus(LPMEMORYSTATUS);
WINBASEAPI BOOL WINAPI GlobalMemoryStatusEx(LPMEMORYSTATUSEX);
WINBASEAPI HLOCAL WINAPI LocalAlloc(UINT,SIZE_T);
WINBASEAPI HLOCAL WINAPI LocalReAlloc(HLOCAL,SIZE_T,UINT);
WINBASEAPI LPVOID WINAPI LocalLock(HLOCAL);
WINBASEAPI HLOCAL WINAPI LocalHandle(LPCVOID);
WINBASEAPI BOOL WINAPI LocalUnlock(HLOCAL);
WINBASEAPI SIZE_T WINAPI LocalSize(HLOCAL);
WINBASEAPI UINT WINAPI LocalFlags(HLOCAL);
WINBASEAPI HLOCAL WINAPI LocalFree(HLOCAL);
WINBASEAPI SIZE_T WINAPI LocalShrink(HLOCAL,UINT);
WINBASEAPI SIZE_T WINAPI LocalCompact(UINT);
WINBASEAPI BOOL WINAPI FlushInstructionCache(HANDLE,LPCVOID,DWORD);
WINBASEAPI LPVOID WINAPI VirtualAlloc(LPVOID,SIZE_T,DWORD,DWORD);
WINBASEAPI BOOL WINAPI VirtualFree(LPVOID,SIZE_T,DWORD);
WINBASEAPI BOOL WINAPI VirtualProtect(LPVOID,SIZE_T,DWORD,PDWORD);
WINBASEAPI DWORD WINAPI VirtualQuery(LPCVOID,PMEMORY_BASIC_INFORMATION,DWORD);
WINBASEAPI LPVOID WINAPI VirtualAllocEx(HANDLE,LPVOID,SIZE_T,DWORD,DWORD);
WINBASEAPI UINT WINAPI GetWriteWatch(DWORD,PVOID,SIZE_T,PVOID*,PULONG_PTR,PULONG);
WINBASEAPI UINT WINAPI ResetWriteWatch(LPVOID,SIZE_T);
WINBASEAPI BOOL WINAPI VirtualFreeEx(HANDLE,LPVOID,SIZE_T,DWORD);
WINBASEAPI BOOL WINAPI VirtualProtectEx(HANDLE,LPVOID,SIZE_T,DWORD,PDWORD);
WINBASEAPI DWORD WINAPI VirtualQueryEx(HANDLE,LPCVOID,PMEMORY_BASIC_INFORMATION,DWORD);
WINBASEAPI HANDLE WINAPI HeapCreate(DWORD,SIZE_T,SIZE_T);
WINBASEAPI BOOL WINAPI HeapDestroy(HANDLE);
WINBASEAPI LPVOID WINAPI HeapAlloc(HANDLE,DWORD,SIZE_T);
WINBASEAPI LPVOID WINAPI HeapReAlloc(HANDLE,DWORD,LPVOID,SIZE_T);
WINBASEAPI BOOL WINAPI HeapFree(HANDLE,DWORD,LPVOID);
WINBASEAPI SIZE_T WINAPI HeapSize(HANDLE,DWORD,LPCVOID);
WINBASEAPI BOOL WINAPI HeapValidate(HANDLE,DWORD,LPCVOID);
WINBASEAPI SIZE_T WINAPI HeapCompact(HANDLE,DWORD);
WINBASEAPI HANDLE WINAPI GetProcessHeap(VOID);
WINBASEAPI DWORD WINAPI GetProcessHeaps(DWORD,PHANDLE);
WINBASEAPI BOOL WINAPI HeapLock(HANDLE);
WINBASEAPI BOOL WINAPI HeapUnlock(HANDLE);
WINBASEAPI BOOL WINAPI HeapWalk(HANDLE,LPPROCESS_HEAP_ENTRY);
WINBASEAPI BOOL WINAPI GetBinaryTypeA(LPCSTR,LPDWORD);
WINBASEAPI BOOL WINAPI GetBinaryTypeW(LPCWSTR,LPDWORD);
WINBASEAPI DWORD WINAPI GetShortPathNameA(LPCSTR,LPSTR,DWORD);
WINBASEAPI DWORD WINAPI GetShortPathNameW(LPCWSTR,LPWSTR,DWORD);
WINBASEAPI DWORD WINAPI GetLongPathNameA(LPCSTR,LPSTR,DWORD);
WINBASEAPI DWORD WINAPI GetLongPathNameW(LPCWSTR,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI GetProcessAffinityMask(HANDLE,PDWORD_PTR,PDWORD_PTR);
WINBASEAPI BOOL WINAPI SetProcessAffinityMask(HANDLE,DWORD_PTR);
WINBASEAPI BOOL WINAPI GetProcessTimes(HANDLE,LPFILETIME,LPFILETIME,LPFILETIME,LPFILETIME);
WINBASEAPI BOOL WINAPI GetProcessIoCounters(HANDLE,PIO_COUNTERS);
WINBASEAPI BOOL WINAPI GetProcessWorkingSetSize(HANDLE,PSIZE_T,PSIZE_T);
WINBASEAPI BOOL WINAPI SetProcessWorkingSetSize(HANDLE,SIZE_T,SIZE_T);
WINBASEAPI HANDLE WINAPI OpenProcess(DWORD,BOOL,DWORD);
#ifndef _WINCE
WINBASEAPI HANDLE WINAPI GetCurrentProcess(VOID);
WINBASEAPI DWORD WINAPI GetCurrentProcessId(VOID);
#endif /* _WINCE */
WINBASEAPI DECLSPEC_NORETURN VOID WINAPI ExitProcess(UINT);
WINBASEAPI BOOL WINAPI TerminateProcess(HANDLE,UINT);
WINBASEAPI BOOL WINAPI GetExitCodeProcess(HANDLE,LPDWORD);
WINBASEAPI VOID WINAPI FatalExit(int);
WINBASEAPI LPSTR WINAPI GetEnvironmentStrings(VOID);
WINBASEAPI LPWSTR WINAPI GetEnvironmentStringsW(VOID);
WINBASEAPI BOOL WINAPI FreeEnvironmentStringsA(LPSTR);
WINBASEAPI BOOL WINAPI FreeEnvironmentStringsW(LPWSTR);
WINBASEAPI VOID WINAPI RaiseException(DWORD,DWORD,DWORD,CONST ULONG_PTR*);
WINBASEAPI LONG WINAPI UnhandledExceptionFilter(struct _EXCEPTION_POINTERS*);
WINBASEAPI LPTOP_LEVEL_EXCEPTION_FILTER WINAPI SetUnhandledExceptionFilter(LPTOP_LEVEL_EXCEPTION_FILTER);
#if (_WIN32_WINNT >= 0x0400)
WINBASEAPI LPVOID WINAPI CreateFiber(DWORD,LPFIBER_START_ROUTINE,LPVOID);
WINBASEAPI VOID WINAPI DeleteFiber(LPVOID);
WINBASEAPI LPVOID WINAPI ConvertThreadToFiber(LPVOID);
WINBASEAPI VOID WINAPI SwitchToFiber(LPVOID);
WINBASEAPI BOOL WINAPI SwitchToThread(VOID);
WINBASEAPI DWORD WINAPI SetThreadIdealProcessor(HANDLE,DWORD);
typedef VOID(APIENTRY *PAPCFUNC)(ULONG_PTR);
WINBASEAPI DWORD WINAPI QueueUserAPC(PAPCFUNC,HANDLE,ULONG_PTR);
#endif /* _WIN32_WINNT >= 0x0400 */
WINBASEAPI HANDLE WINAPI CreateThread(LPSECURITY_ATTRIBUTES,DWORD,LPTHREAD_START_ROUTINE,LPVOID,DWORD,LPDWORD);
WINBASEAPI HANDLE WINAPI CreateRemoteThread(HANDLE,LPSECURITY_ATTRIBUTES,DWORD,LPTHREAD_START_ROUTINE,LPVOID,DWORD,LPDWORD);
#ifndef _WINCE
WINBASEAPI HANDLE WINAPI GetCurrentThread(VOID);
WINBASEAPI DWORD WINAPI GetCurrentThreadId(VOID);
#endif /* _WINCE */
WINBASEAPI DWORD_PTR WINAPI SetThreadAffinityMask(HANDLE,DWORD_PTR);
WINBASEAPI BOOL WINAPI SetProcessPriorityBoost(HANDLE,BOOL);
WINBASEAPI BOOL WINAPI GetProcessPriorityBoost(HANDLE,PBOOL);
WINBASEAPI BOOL WINAPI RequestWakeupLatency(LATENCY_TIME);
WINBASEAPI BOOL WINAPI IsSystemResumeAutomatic(VOID);
WINBASEAPI HANDLE WINAPI OpenThread(DWORD,BOOL,DWORD);
WINBASEAPI BOOL WINAPI SetThreadPriority(HANDLE,int);
WINBASEAPI BOOL WINAPI SetThreadPriorityBoost(HANDLE,BOOL);
WINBASEAPI BOOL WINAPI GetThreadPriorityBoost(HANDLE,PBOOL);
WINBASEAPI int WINAPI GetThreadPriority(HANDLE);
WINBASEAPI BOOL WINAPI GetThreadTimes(HANDLE,LPFILETIME,LPFILETIME,LPFILETIME,LPFILETIME);
WINBASEAPI DECLSPEC_NORETURN VOID WINAPI ExitThread(DWORD);
WINBASEAPI BOOL WINAPI TerminateThread(HANDLE,DWORD);
WINBASEAPI BOOL WINAPI GetExitCodeThread(HANDLE,LPDWORD);
WINBASEAPI BOOL WINAPI GetThreadSelectorEntry(HANDLE,DWORD,LPLDT_ENTRY);
WINBASEAPI EXECUTION_STATE WINAPI SetThreadExecutionState(EXECUTION_STATE);
WINBASEAPI DWORD WINAPI GetLastError(VOID);
WINBASEAPI VOID WINAPI SetLastError(DWORD);
WINBASEAPI BOOL WINAPI GetOverlappedResult(HANDLE,LPOVERLAPPED,LPDWORD,BOOL);
WINBASEAPI HANDLE WINAPI CreateIoCompletionPort(HANDLE,HANDLE,ULONG_PTR,DWORD);
WINBASEAPI BOOL WINAPI GetQueuedCompletionStatus(HANDLE,LPDWORD,PULONG_PTR,LPOVERLAPPED*,DWORD);
WINBASEAPI BOOL WINAPI PostQueuedCompletionStatus(HANDLE,DWORD,ULONG_PTR,LPOVERLAPPED);
WINBASEAPI UINT WINAPI SetErrorMode(UINT);
WINBASEAPI BOOL WINAPI ReadProcessMemory(HANDLE,LPCVOID,LPVOID,DWORD,LPDWORD);
WINBASEAPI BOOL WINAPI WriteProcessMemory(HANDLE,LPVOID,LPVOID,DWORD,LPDWORD);
WINBASEAPI BOOL WINAPI GetThreadContext(HANDLE,LPCONTEXT);
WINBASEAPI BOOL WINAPI SetThreadContext(HANDLE,CONST CONTEXT*);
WINBASEAPI DWORD WINAPI SuspendThread(HANDLE);
WINBASEAPI DWORD WINAPI ResumeThread(HANDLE);
#if (_WIN32_WINNT >= 0x0400) || (_WIN32_WINDOWS > 0x0400)
WINBASEAPI BOOL WINAPI IsDebuggerPresent(VOID);
#endif
#ifndef _WINCE
WINBASEAPI VOID WINAPI DebugBreak(VOID);
#endif /* _WINCE */
WINBASEAPI BOOL WINAPI WaitForDebugEvent(LPDEBUG_EVENT,DWORD);
WINBASEAPI BOOL WINAPI ContinueDebugEvent(DWORD,DWORD,DWORD);
WINBASEAPI BOOL WINAPI DebugActiveProcess(DWORD);
WINBASEAPI VOID WINAPI InitializeCriticalSection(LPCRITICAL_SECTION);
WINBASEAPI VOID WINAPI EnterCriticalSection(LPCRITICAL_SECTION);
WINBASEAPI VOID WINAPI LeaveCriticalSection(LPCRITICAL_SECTION);
#if (_WIN32_WINNT >= 0x0403)
WINBASEAPI BOOL WINAPI InitializeCriticalSectionAndSpinCount(LPCRITICAL_SECTION,DWORD);
WINBASEAPI DWORD WINAPI SetCriticalSectionSpinCount(LPCRITICAL_SECTION,DWORD);
#endif /* _WIN32_WINNT >= 0x0403 */
#if (_WIN32_WINNT >= 0x0400)
WINBASEAPI BOOL WINAPI TryEnterCriticalSection(LPCRITICAL_SECTION);
#endif /* _WIN32_WINNT >= 0x0400 */
WINBASEAPI VOID WINAPI DeleteCriticalSection(LPCRITICAL_SECTION);
#ifndef _WINCE
WINBASEAPI BOOL WINAPI SetEvent(HANDLE);
WINBASEAPI BOOL WINAPI ResetEvent(HANDLE);
WINBASEAPI BOOL WINAPI PulseEvent(HANDLE);
#endif /* _WINCE */
WINBASEAPI BOOL WINAPI ReleaseSemaphore(HANDLE,LONG,LPLONG);
WINBASEAPI BOOL WINAPI ReleaseMutex(HANDLE);
WINBASEAPI DWORD WINAPI WaitForSingleObject(HANDLE,DWORD);
WINBASEAPI DWORD WINAPI WaitForMultipleObjects(DWORD,CONST HANDLE*,BOOL,DWORD);
WINBASEAPI VOID WINAPI Sleep(DWORD);
WINBASEAPI HGLOBAL WINAPI LoadResource(HMODULE,HRSRC);
WINBASEAPI DWORD WINAPI SizeofResource(HMODULE,HRSRC);
WINBASEAPI ATOM WINAPI GlobalDeleteAtom(ATOM);
WINBASEAPI BOOL WINAPI InitAtomTable(DWORD);
WINBASEAPI ATOM WINAPI DeleteAtom(ATOM);
WINBASEAPI UINT WINAPI SetHandleCount(UINT);
WINBASEAPI DWORD WINAPI GetLogicalDrives(VOID);
WINBASEAPI BOOL WINAPI LockFile(HANDLE,DWORD,DWORD,DWORD,DWORD);
WINBASEAPI BOOL WINAPI UnlockFile(HANDLE,DWORD,DWORD,DWORD,DWORD);
WINBASEAPI BOOL WINAPI LockFileEx(HANDLE,DWORD,DWORD,DWORD,DWORD,LPOVERLAPPED);
WINBASEAPI BOOL WINAPI UnlockFileEx(HANDLE,DWORD,DWORD,DWORD,LPOVERLAPPED);
WINBASEAPI BOOL WINAPI GetFileInformationByHandle(HANDLE,LPBY_HANDLE_FILE_INFORMATION);
WINBASEAPI DWORD WINAPI GetFileType(HANDLE);
WINBASEAPI DWORD WINAPI GetFileSize(HANDLE,LPDWORD);
WINBASEAPI BOOL WINAPI GetFileSizeEx(HANDLE,PLARGE_INTEGER);
WINBASEAPI HANDLE WINAPI GetStdHandle(DWORD);
WINBASEAPI BOOL WINAPI SetStdHandle(DWORD,HANDLE);
WINBASEAPI BOOL WINAPI WriteFile(HANDLE,LPCVOID,DWORD,LPDWORD,LPOVERLAPPED);
WINBASEAPI BOOL WINAPI ReadFile(HANDLE,LPVOID,DWORD,LPDWORD,LPOVERLAPPED);
WINBASEAPI BOOL WINAPI FlushFileBuffers(HANDLE);
WINBASEAPI BOOL WINAPI DeviceIoControl(HANDLE,DWORD,LPVOID,DWORD,LPVOID,DWORD,LPDWORD,LPOVERLAPPED);
WINBASEAPI BOOL WINAPI RequestDeviceWakeup(HANDLE);
WINBASEAPI BOOL WINAPI CancelDeviceWakeupRequest(HANDLE);
WINBASEAPI BOOL WINAPI GetDevicePowerState(HANDLE,BOOL*);
WINBASEAPI BOOL WINAPI SetMessageWaitingIndicator(HANDLE,ULONG);
WINBASEAPI BOOL WINAPI SetEndOfFile(HANDLE);
WINBASEAPI DWORD WINAPI SetFilePointer(HANDLE,LONG,PLONG,DWORD);
WINBASEAPI BOOL WINAPI SetFilePointerEx(HANDLE,LARGE_INTEGER,PLARGE_INTEGER,DWORD);
WINBASEAPI BOOL WINAPI FindClose(HANDLE);
WINBASEAPI BOOL WINAPI GetFileTime(HANDLE,LPFILETIME,LPFILETIME,LPFILETIME);
WINBASEAPI BOOL WINAPI SetFileTime(HANDLE,CONST FILETIME*,CONST FILETIME*,CONST FILETIME*);
WINBASEAPI BOOL WINAPI CloseHandle(HANDLE);
WINBASEAPI BOOL WINAPI DuplicateHandle(HANDLE,HANDLE,HANDLE,LPHANDLE,DWORD,BOOL,DWORD);
WINBASEAPI BOOL WINAPI GetHandleInformation(HANDLE,LPDWORD);
WINBASEAPI BOOL WINAPI SetHandleInformation(HANDLE,DWORD,DWORD);
WINBASEAPI DWORD WINAPI LoadModule(LPCSTR,LPVOID);
WINBASEAPI UINT WINAPI WinExec(LPCSTR,UINT);
WINBASEAPI BOOL WINAPI ClearCommBreak(HANDLE);
WINBASEAPI BOOL WINAPI ClearCommError(HANDLE,LPDWORD,LPCOMSTAT);
WINBASEAPI BOOL WINAPI SetupComm(HANDLE,DWORD,DWORD);
WINBASEAPI BOOL WINAPI EscapeCommFunction(HANDLE,DWORD);
WINBASEAPI BOOL WINAPI GetCommConfig(HANDLE,LPCOMMCONFIG,LPDWORD);
WINBASEAPI BOOL WINAPI GetCommMask(HANDLE,LPDWORD);
WINBASEAPI BOOL WINAPI GetCommProperties(HANDLE,LPCOMMPROP);
WINBASEAPI BOOL WINAPI GetCommModemStatus(HANDLE,LPDWORD);
WINBASEAPI BOOL WINAPI GetCommState(HANDLE,LPDCB);
WINBASEAPI BOOL WINAPI GetCommTimeouts(HANDLE,LPCOMMTIMEOUTS);
WINBASEAPI BOOL WINAPI PurgeComm(HANDLE,DWORD);
WINBASEAPI BOOL WINAPI SetCommBreak(HANDLE);
WINBASEAPI BOOL WINAPI SetCommConfig(HANDLE,LPCOMMCONFIG,DWORD);
WINBASEAPI BOOL WINAPI SetCommMask(HANDLE,DWORD);
WINBASEAPI BOOL WINAPI SetCommState(HANDLE,LPDCB);
WINBASEAPI BOOL WINAPI SetCommTimeouts(HANDLE,LPCOMMTIMEOUTS);
WINBASEAPI BOOL WINAPI TransmitCommChar(HANDLE,char);
WINBASEAPI BOOL WINAPI WaitCommEvent(HANDLE,LPDWORD,LPOVERLAPPED);
WINBASEAPI DWORD WINAPI SetTapePosition(HANDLE,DWORD,DWORD,DWORD,DWORD,BOOL);
WINBASEAPI DWORD WINAPI GetTapePosition(HANDLE,DWORD,LPDWORD,LPDWORD,LPDWORD);
WINBASEAPI DWORD WINAPI PrepareTape(HANDLE,DWORD,BOOL);
WINBASEAPI DWORD WINAPI EraseTape(HANDLE,DWORD,BOOL);
WINBASEAPI DWORD WINAPI CreateTapePartition(HANDLE,DWORD,DWORD,DWORD);
WINBASEAPI DWORD WINAPI WriteTapemark(HANDLE,DWORD,DWORD,BOOL);
WINBASEAPI DWORD WINAPI GetTapeStatus(HANDLE);
WINBASEAPI DWORD WINAPI GetTapeParameters(HANDLE,DWORD,LPDWORD,LPVOID);
WINBASEAPI DWORD WINAPI SetTapeParameters(HANDLE,DWORD,LPVOID);
WINBASEAPI BOOL WINAPI Beep(DWORD,DWORD);
WINBASEAPI int WINAPI MulDiv(int,int,int);
WINBASEAPI VOID WINAPI GetSystemTime(LPSYSTEMTIME);
WINBASEAPI VOID WINAPI GetSystemTimeAsFileTime(LPFILETIME);
WINBASEAPI BOOL WINAPI SetSystemTime(CONST SYSTEMTIME*);
WINBASEAPI VOID WINAPI GetLocalTime(LPSYSTEMTIME);
WINBASEAPI BOOL WINAPI SetLocalTime(CONST SYSTEMTIME*);
WINBASEAPI VOID WINAPI GetSystemInfo(LPSYSTEM_INFO);
WINBASEAPI BOOL WINAPI IsProcessorFeaturePresent(DWORD);
WINBASEAPI BOOL WINAPI SystemTimeToTzSpecificLocalTime(LPTIME_ZONE_INFORMATION,LPSYSTEMTIME,LPSYSTEMTIME);
WINBASEAPI DWORD WINAPI GetTimeZoneInformation(LPTIME_ZONE_INFORMATION);
WINBASEAPI BOOL WINAPI SetTimeZoneInformation(CONST TIME_ZONE_INFORMATION*);
WINBASEAPI BOOL WINAPI SystemTimeToFileTime(CONST SYSTEMTIME*,LPFILETIME);
WINBASEAPI BOOL WINAPI FileTimeToLocalFileTime(CONST FILETIME*,LPFILETIME);
WINBASEAPI BOOL WINAPI LocalFileTimeToFileTime(CONST FILETIME*,LPFILETIME);
WINBASEAPI BOOL WINAPI FileTimeToSystemTime(CONST FILETIME*,LPSYSTEMTIME);
WINBASEAPI LONG WINAPI CompareFileTime(CONST FILETIME*,CONST FILETIME*);
WINBASEAPI BOOL WINAPI FileTimeToDosDateTime(CONST FILETIME*,LPWORD,LPWORD);
WINBASEAPI BOOL WINAPI DosDateTimeToFileTime(WORD,WORD,LPFILETIME);
WINBASEAPI DWORD WINAPI GetTickCount(VOID);
WINBASEAPI BOOL WINAPI SetSystemTimeAdjustment(DWORD,BOOL);
WINBASEAPI BOOL WINAPI GetSystemTimeAdjustment(PDWORD,PDWORD,PBOOL);
WINBASEAPI DWORD WINAPI FormatMessageA(DWORD,LPCVOID,DWORD,DWORD,LPSTR,DWORD,va_list*);
WINBASEAPI DWORD WINAPI FormatMessageW(DWORD,LPCVOID,DWORD,DWORD,LPWSTR,DWORD,va_list*);
WINBASEAPI BOOL WINAPI CreatePipe(PHANDLE,PHANDLE,LPSECURITY_ATTRIBUTES,DWORD);
WINBASEAPI BOOL WINAPI ConnectNamedPipe(HANDLE,LPOVERLAPPED);
WINBASEAPI BOOL WINAPI DisconnectNamedPipe(HANDLE);
WINBASEAPI BOOL WINAPI SetNamedPipeHandleState(HANDLE,LPDWORD,LPDWORD,LPDWORD);
WINBASEAPI BOOL WINAPI GetNamedPipeInfo(HANDLE,LPDWORD,LPDWORD,LPDWORD,LPDWORD);
WINBASEAPI BOOL WINAPI PeekNamedPipe(HANDLE,LPVOID,DWORD,LPDWORD,LPDWORD,LPDWORD);
WINBASEAPI BOOL WINAPI TransactNamedPipe(HANDLE,LPVOID,DWORD,LPVOID,DWORD,LPDWORD,LPOVERLAPPED);
WINBASEAPI HANDLE WINAPI CreateMailslotA(LPCSTR,DWORD,DWORD,LPSECURITY_ATTRIBUTES);
WINBASEAPI HANDLE WINAPI CreateMailslotW(LPCWSTR,DWORD,DWORD,LPSECURITY_ATTRIBUTES);
WINBASEAPI BOOL WINAPI GetMailslotInfo(HANDLE,LPDWORD,LPDWORD,LPDWORD,LPDWORD);
WINBASEAPI BOOL WINAPI SetMailslotInfo(HANDLE,DWORD);
WINBASEAPI LPVOID WINAPI MapViewOfFile(HANDLE,DWORD,DWORD,DWORD,SIZE_T);
WINBASEAPI BOOL WINAPI FlushViewOfFile(LPCVOID,SIZE_T);
WINBASEAPI BOOL WINAPI UnmapViewOfFile(LPCVOID);
WINADVAPI BOOL WINAPI EncryptFileA(LPCSTR);
WINADVAPI BOOL WINAPI EncryptFileW(LPCWSTR);
WINADVAPI BOOL WINAPI DecryptFileA(LPCSTR,DWORD);
WINADVAPI BOOL WINAPI DecryptFileW(LPCWSTR,DWORD);
WINADVAPI BOOL WINAPI FileEncryptionStatusA(LPCSTR,LPDWORD);
WINADVAPI BOOL WINAPI FileEncryptionStatusW(LPCWSTR,LPDWORD);
WINADVAPI DWORD WINAPI OpenEncryptedFileRawA(LPCSTR,ULONG,PVOID*);
WINADVAPI DWORD WINAPI OpenEncryptedFileRawW(LPCWSTR,ULONG,PVOID*);
WINADVAPI DWORD WINAPI ReadEncryptedFileRaw(PFE_EXPORT_FUNC,PVOID,PVOID);
WINADVAPI DWORD WINAPI WriteEncryptedFileRaw(PFE_IMPORT_FUNC,PVOID,PVOID);
WINADVAPI VOID WINAPI CloseEncryptedFileRaw(PVOID);
WINBASEAPI int WINAPI lstrcmpA(LPCSTR,LPCSTR);
WINBASEAPI int WINAPI lstrcmpW(LPCWSTR,LPCWSTR);
WINBASEAPI int WINAPI lstrcmpiA(LPCSTR,LPCSTR);
WINBASEAPI int WINAPI lstrcmpiW(LPCWSTR,LPCWSTR);
WINBASEAPI LPSTR WINAPI lstrcpynA(LPSTR,LPCSTR,int);
WINBASEAPI LPWSTR WINAPI lstrcpynW(LPWSTR,LPCWSTR,int);
WINBASEAPI LPSTR WINAPI lstrcpyA(LPSTR,LPCSTR);
WINBASEAPI LPWSTR WINAPI lstrcpyW(LPWSTR,LPCWSTR);
WINBASEAPI LPSTR WINAPI lstrcatA(LPSTR,LPCSTR);
WINBASEAPI LPWSTR WINAPI lstrcatW(LPWSTR,LPCWSTR);
WINBASEAPI int WINAPI lstrlenA(LPCSTR);
WINBASEAPI int WINAPI lstrlenW(LPCWSTR);
WINBASEAPI HFILE WINAPI OpenFile(LPCSTR,LPOFSTRUCT,UINT);
WINBASEAPI HFILE WINAPI _lopen(LPCSTR,int);
WINBASEAPI HFILE WINAPI _lcreat(LPCSTR,int);
WINBASEAPI UINT WINAPI _lread(HFILE,LPVOID,UINT);
WINBASEAPI UINT WINAPI _lwrite(HFILE,LPCSTR,UINT);
WINBASEAPI long WINAPI _hread(HFILE,LPVOID,long);
WINBASEAPI long WINAPI _hwrite(HFILE,LPCSTR,long);
WINBASEAPI HFILE WINAPI _lclose(HFILE);
WINBASEAPI LONG WINAPI _llseek(HFILE,LONG,int);
WINADVAPI BOOL WINAPI IsTextUnicode(CONST LPVOID,int,LPINT);
WINBASEAPI DWORD WINAPI TlsAlloc(VOID);
WINBASEAPI LPVOID WINAPI TlsGetValue(DWORD);
WINBASEAPI BOOL WINAPI TlsSetValue(DWORD,LPVOID);
WINBASEAPI BOOL WINAPI TlsFree(DWORD);
WINBASEAPI DWORD WINAPI SleepEx(DWORD,BOOL);
WINBASEAPI DWORD WINAPI WaitForSingleObjectEx(HANDLE,DWORD,BOOL);
WINBASEAPI DWORD WINAPI WaitForMultipleObjectsEx(DWORD,CONST HANDLE*,BOOL,DWORD,BOOL);
#if (_WIN32_WINNT >= 0x0400)
WINBASEAPI DWORD WINAPI SignalObjectAndWait(HANDLE,HANDLE,DWORD,BOOL);
#endif /* _WIN32_WINNT >= 0x0400 */
WINBASEAPI BOOL WINAPI ReadFileEx(HANDLE,LPVOID,DWORD,LPOVERLAPPED,LPOVERLAPPED_COMPLETION_ROUTINE);
WINBASEAPI BOOL WINAPI WriteFileEx(HANDLE,LPCVOID,DWORD,LPOVERLAPPED,LPOVERLAPPED_COMPLETION_ROUTINE);
WINBASEAPI BOOL WINAPI BackupRead(HANDLE,LPBYTE,DWORD,LPDWORD,BOOL,BOOL,LPVOID*);
WINBASEAPI BOOL WINAPI BackupSeek(HANDLE,DWORD,DWORD,LPDWORD,LPDWORD,LPVOID*);
WINBASEAPI BOOL WINAPI BackupWrite(HANDLE,LPBYTE,DWORD,LPDWORD,BOOL,BOOL,LPVOID*);
WINBASEAPI BOOL WINAPI ReadFileScatter(HANDLE,FILE_SEGMENT_ELEMENT [],DWORD,LPDWORD,LPOVERLAPPED);
WINBASEAPI BOOL WINAPI WriteFileGather(HANDLE,FILE_SEGMENT_ELEMENT [],DWORD,LPDWORD,LPOVERLAPPED);
WINBASEAPI HANDLE WINAPI CreateMutexA(LPSECURITY_ATTRIBUTES,BOOL,LPCSTR);
WINBASEAPI HANDLE WINAPI CreateMutexW(LPSECURITY_ATTRIBUTES,BOOL,LPCWSTR);
WINBASEAPI HANDLE WINAPI OpenMutexA(DWORD,BOOL,LPCSTR);
WINBASEAPI HANDLE WINAPI OpenMutexW(DWORD,BOOL,LPCWSTR);
WINBASEAPI HANDLE WINAPI CreateEventA(LPSECURITY_ATTRIBUTES,BOOL,BOOL,LPCSTR);
WINBASEAPI HANDLE WINAPI CreateEventW(LPSECURITY_ATTRIBUTES,BOOL,BOOL,LPCWSTR);
WINBASEAPI HANDLE WINAPI OpenEventA(DWORD,BOOL,LPCSTR);
WINBASEAPI HANDLE WINAPI OpenEventW(DWORD,BOOL,LPCWSTR);
WINBASEAPI HANDLE WINAPI CreateSemaphoreA(LPSECURITY_ATTRIBUTES,LONG,LONG,LPCSTR);
WINBASEAPI HANDLE WINAPI CreateSemaphoreW(LPSECURITY_ATTRIBUTES,LONG,LONG,LPCWSTR);
WINBASEAPI HANDLE WINAPI OpenSemaphoreA(DWORD,BOOL,LPCSTR);
WINBASEAPI HANDLE WINAPI OpenSemaphoreW(DWORD,BOOL,LPCWSTR);
#if (_WIN32_WINNT >= 0x0400) || (_WIN32_WINDOWS > 0x0400)
typedef VOID(APIENTRY * PTIMERAPCROUTINE) (LPVOID,DWORD,DWORD);
WINBASEAPI HANDLE WINAPI CreateWaitableTimerA(LPSECURITY_ATTRIBUTES,BOOL,LPCSTR);
WINBASEAPI HANDLE WINAPI CreateWaitableTimerW(LPSECURITY_ATTRIBUTES,BOOL,LPCWSTR);
WINBASEAPI HANDLE WINAPI OpenWaitableTimerA(DWORD,BOOL,LPCSTR);
WINBASEAPI HANDLE WINAPI OpenWaitableTimerW(DWORD,BOOL,LPCWSTR);
WINBASEAPI BOOL WINAPI SetWaitableTimer(HANDLE,const LARGE_INTEGER*,LONG,PTIMERAPCROUTINE,LPVOID,BOOL);
WINBASEAPI BOOL WINAPI CancelWaitableTimer(HANDLE);
#endif /* (_WIN32_WINNT >= 0x0400) || (_WIN32_WINDOWS > 0x0400) */
WINBASEAPI HANDLE WINAPI CreateFileMappingA(HANDLE,LPSECURITY_ATTRIBUTES,DWORD,DWORD,DWORD,LPCSTR);
WINBASEAPI HANDLE WINAPI CreateFileMappingW(HANDLE,LPSECURITY_ATTRIBUTES,DWORD,DWORD,DWORD,LPCWSTR);
WINBASEAPI HANDLE WINAPI OpenFileMappingA(DWORD,BOOL,LPCSTR);
WINBASEAPI HANDLE WINAPI OpenFileMappingW(DWORD,BOOL,LPCWSTR);
WINBASEAPI DWORD WINAPI GetLogicalDriveStringsA(DWORD,LPSTR);
WINBASEAPI DWORD WINAPI GetLogicalDriveStringsW(DWORD,LPWSTR);
WINBASEAPI HMODULE WINAPI LoadLibraryA(LPCSTR);
WINBASEAPI HMODULE WINAPI LoadLibraryW(LPCWSTR);
WINBASEAPI HMODULE WINAPI LoadLibraryExA(LPCSTR,HANDLE,DWORD);
WINBASEAPI HMODULE WINAPI LoadLibraryExW(LPCWSTR,HANDLE,DWORD);
WINBASEAPI DWORD WINAPI GetModuleFileNameA(HMODULE,LPSTR,DWORD);
WINBASEAPI DWORD WINAPI GetModuleFileNameW(HMODULE,LPWSTR,DWORD);
WINBASEAPI HMODULE WINAPI GetModuleHandleA(LPCSTR);
WINBASEAPI HMODULE WINAPI GetModuleHandleW(LPCWSTR);
WINBASEAPI BOOL WINAPI CreateProcessA(LPCSTR,LPSTR,LPSECURITY_ATTRIBUTES,LPSECURITY_ATTRIBUTES,BOOL,DWORD,LPVOID,LPCSTR,LPSTARTUPINFOA,LPPROCESS_INFORMATION);
WINBASEAPI BOOL WINAPI CreateProcessW(LPCWSTR,LPWSTR,LPSECURITY_ATTRIBUTES,LPSECURITY_ATTRIBUTES,BOOL,DWORD,LPVOID,LPCWSTR,LPSTARTUPINFOW,LPPROCESS_INFORMATION);
WINBASEAPI BOOL WINAPI SetProcessShutdownParameters(DWORD,DWORD);
WINBASEAPI BOOL WINAPI GetProcessShutdownParameters(LPDWORD,LPDWORD);
WINBASEAPI DWORD WINAPI GetProcessVersion(DWORD);
WINBASEAPI VOID WINAPI FatalAppExitA(UINT,LPCSTR);
WINBASEAPI VOID WINAPI FatalAppExitW(UINT,LPCWSTR);
WINBASEAPI VOID WINAPI GetStartupInfoA(LPSTARTUPINFOA);
WINBASEAPI VOID WINAPI GetStartupInfoW(LPSTARTUPINFOW);
WINBASEAPI LPSTR WINAPI GetCommandLineA(VOID);
WINBASEAPI LPWSTR WINAPI GetCommandLineW(VOID);
WINBASEAPI DWORD WINAPI GetEnvironmentVariableA(LPCSTR,LPSTR,DWORD);
WINBASEAPI DWORD WINAPI GetEnvironmentVariableW(LPCWSTR,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI SetEnvironmentVariableA(LPCSTR,LPCSTR);
WINBASEAPI BOOL WINAPI SetEnvironmentVariableW(LPCWSTR,LPCWSTR);
WINBASEAPI DWORD WINAPI ExpandEnvironmentStringsA(LPCSTR,LPSTR,DWORD);
WINBASEAPI DWORD WINAPI ExpandEnvironmentStringsW(LPCWSTR,LPWSTR,DWORD);
WINBASEAPI VOID WINAPI OutputDebugStringA(LPCSTR);
WINBASEAPI VOID WINAPI OutputDebugStringW(LPCWSTR);
WINBASEAPI HRSRC WINAPI FindResourceA(HMODULE,LPCSTR,LPCSTR);
WINBASEAPI HRSRC WINAPI FindResourceW(HMODULE,LPCWSTR,LPCWSTR);
WINBASEAPI HRSRC WINAPI FindResourceExA(HMODULE,LPCSTR,LPCSTR,WORD);
WINBASEAPI HRSRC WINAPI FindResourceExW(HMODULE,LPCWSTR,LPCWSTR,WORD);
WINBASEAPI BOOL WINAPI EnumResourceTypesA(HMODULE,ENUMRESTYPEPROCA,LONG_PTR);
WINBASEAPI BOOL WINAPI EnumResourceTypesW(HMODULE,ENUMRESTYPEPROCW,LONG_PTR);
WINBASEAPI BOOL WINAPI EnumResourceNamesA(HMODULE,LPCSTR,ENUMRESNAMEPROCA,LONG_PTR);
WINBASEAPI BOOL WINAPI EnumResourceNamesW(HMODULE,LPCWSTR,ENUMRESNAMEPROCW,LONG_PTR);
WINBASEAPI BOOL WINAPI EnumResourceLanguagesA(HMODULE,LPCSTR,LPCSTR,ENUMRESLANGPROCA,LONG_PTR);
WINBASEAPI BOOL WINAPI EnumResourceLanguagesW(HMODULE,LPCWSTR,LPCWSTR,ENUMRESLANGPROCW,LONG_PTR);
WINBASEAPI HANDLE WINAPI BeginUpdateResourceA(LPCSTR,BOOL);
WINBASEAPI HANDLE WINAPI BeginUpdateResourceW(LPCWSTR,BOOL);
WINBASEAPI BOOL WINAPI UpdateResourceA(HANDLE,LPCSTR,LPCSTR,WORD,LPVOID,DWORD);
WINBASEAPI BOOL WINAPI UpdateResourceW(HANDLE,LPCWSTR,LPCWSTR,WORD,LPVOID,DWORD);
WINBASEAPI BOOL WINAPI EndUpdateResourceA(HANDLE,BOOL);
WINBASEAPI BOOL WINAPI EndUpdateResourceW(HANDLE,BOOL);
WINBASEAPI ATOM WINAPI GlobalAddAtomA(LPCSTR);
WINBASEAPI ATOM WINAPI GlobalAddAtomW(LPCWSTR);
WINBASEAPI ATOM WINAPI GlobalFindAtomA(LPCSTR);
WINBASEAPI ATOM WINAPI GlobalFindAtomW(LPCWSTR);
WINBASEAPI UINT WINAPI GlobalGetAtomNameA(ATOM,LPSTR,int);
WINBASEAPI UINT WINAPI GlobalGetAtomNameW(ATOM,LPWSTR,int);
WINBASEAPI ATOM WINAPI AddAtomA(LPCSTR);
WINBASEAPI ATOM WINAPI AddAtomW(LPCWSTR);
WINBASEAPI ATOM WINAPI FindAtomA(LPCSTR);
WINBASEAPI ATOM WINAPI FindAtomW(LPCWSTR);
WINBASEAPI UINT WINAPI GetAtomNameA(ATOM,LPSTR,int);
WINBASEAPI UINT WINAPI GetAtomNameW(ATOM,LPWSTR,int);
WINBASEAPI UINT WINAPI GetProfileIntA(LPCSTR,LPCSTR,INT);
WINBASEAPI UINT WINAPI GetProfileIntW(LPCWSTR,LPCWSTR,INT);
WINBASEAPI DWORD WINAPI GetProfileStringA(LPCSTR,LPCSTR,LPCSTR,LPSTR,DWORD);
WINBASEAPI DWORD WINAPI GetProfileStringW(LPCWSTR,LPCWSTR,LPCWSTR,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI WriteProfileStringA(LPCSTR,LPCSTR,LPCSTR);
WINBASEAPI BOOL WINAPI WriteProfileStringW(LPCWSTR,LPCWSTR,LPCWSTR);
WINBASEAPI DWORD WINAPI GetProfileSectionA(LPCSTR,LPSTR,DWORD);
WINBASEAPI DWORD WINAPI GetProfileSectionW(LPCWSTR,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI WriteProfileSectionA(LPCSTR,LPCSTR);
WINBASEAPI BOOL WINAPI WriteProfileSectionW(LPCWSTR,LPCWSTR);
WINBASEAPI UINT WINAPI GetPrivateProfileIntA(LPCSTR,LPCSTR,INT,LPCSTR);
WINBASEAPI UINT WINAPI GetPrivateProfileIntW(LPCWSTR,LPCWSTR,INT,LPCWSTR);
WINBASEAPI DWORD WINAPI GetPrivateProfileStringA(LPCSTR,LPCSTR,LPCSTR,LPSTR,DWORD,LPCSTR);
WINBASEAPI DWORD WINAPI GetPrivateProfileStringW(LPCWSTR,LPCWSTR,LPCWSTR,LPWSTR,DWORD,LPCWSTR);
WINBASEAPI BOOL WINAPI WritePrivateProfileStringA(LPCSTR,LPCSTR,LPCSTR,LPCSTR);
WINBASEAPI BOOL WINAPI WritePrivateProfileStringW(LPCWSTR,LPCWSTR,LPCWSTR,LPCWSTR);
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionA(LPCSTR,LPSTR,DWORD,LPCSTR);
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionW(LPCWSTR,LPWSTR,DWORD,LPCWSTR);
WINBASEAPI BOOL WINAPI WritePrivateProfileSectionA(LPCSTR,LPCSTR,LPCSTR);
WINBASEAPI BOOL WINAPI WritePrivateProfileSectionW(LPCWSTR,LPCWSTR,LPCWSTR);
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionNamesA(LPSTR,DWORD,LPCSTR);
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionNamesW(LPWSTR,DWORD,LPCWSTR);
WINBASEAPI BOOL WINAPI GetPrivateProfileStructA(LPCSTR,LPCSTR,LPVOID,UINT,LPCSTR);
WINBASEAPI BOOL WINAPI GetPrivateProfileStructW(LPCWSTR,LPCWSTR,LPVOID,UINT,LPCWSTR);
WINBASEAPI BOOL WINAPI WritePrivateProfileStructA(LPCSTR,LPCSTR,LPVOID,UINT,LPCSTR);
WINBASEAPI BOOL WINAPI WritePrivateProfileStructW(LPCWSTR,LPCWSTR,LPVOID,UINT,LPCWSTR);
WINBASEAPI UINT WINAPI GetDriveTypeA(LPCSTR);
WINBASEAPI UINT WINAPI GetDriveTypeW(LPCWSTR);
WINBASEAPI UINT WINAPI GetSystemDirectoryA(LPSTR,UINT);
WINBASEAPI UINT WINAPI GetSystemDirectoryW(LPWSTR,UINT);
WINBASEAPI DWORD WINAPI GetTempPathA(DWORD,LPSTR);
WINBASEAPI DWORD WINAPI GetTempPathW(DWORD,LPWSTR);
WINBASEAPI UINT WINAPI GetTempFileNameA(LPCSTR,LPCSTR,UINT,LPSTR);
WINBASEAPI UINT WINAPI GetTempFileNameW(LPCWSTR,LPCWSTR,UINT,LPWSTR);
WINBASEAPI UINT WINAPI GetWindowsDirectoryA(LPSTR,UINT);
WINBASEAPI UINT WINAPI GetWindowsDirectoryW(LPWSTR,UINT);
WINBASEAPI UINT WINAPI GetSystemWindowsDirectoryA(LPSTR,UINT);
WINBASEAPI UINT WINAPI GetSystemWindowsDirectoryW(LPWSTR,UINT);
WINBASEAPI BOOL WINAPI SetCurrentDirectoryA(LPCSTR);
WINBASEAPI BOOL WINAPI SetCurrentDirectoryW(LPCWSTR);
WINBASEAPI DWORD WINAPI GetCurrentDirectoryA(DWORD,LPSTR);
WINBASEAPI DWORD WINAPI GetCurrentDirectoryW(DWORD,LPWSTR);
WINBASEAPI BOOL WINAPI GetDiskFreeSpaceA(LPCSTR,LPDWORD,LPDWORD,LPDWORD,LPDWORD);
WINBASEAPI BOOL WINAPI GetDiskFreeSpaceW(LPCWSTR,LPDWORD,LPDWORD,LPDWORD,LPDWORD);
WINBASEAPI BOOL WINAPI GetDiskFreeSpaceExA(LPCSTR,PULARGE_INTEGER,PULARGE_INTEGER,PULARGE_INTEGER);
WINBASEAPI BOOL WINAPI GetDiskFreeSpaceExW(LPCWSTR,PULARGE_INTEGER,PULARGE_INTEGER,PULARGE_INTEGER);
WINBASEAPI BOOL WINAPI CreateDirectoryA(LPCSTR,LPSECURITY_ATTRIBUTES);
WINBASEAPI BOOL WINAPI CreateDirectoryW(LPCWSTR,LPSECURITY_ATTRIBUTES);
WINBASEAPI BOOL WINAPI CreateDirectoryExA(LPCSTR,LPCSTR,LPSECURITY_ATTRIBUTES);
WINBASEAPI BOOL WINAPI CreateDirectoryExW(LPCWSTR,LPCWSTR,LPSECURITY_ATTRIBUTES);
WINBASEAPI BOOL WINAPI RemoveDirectoryA(LPCSTR);
WINBASEAPI BOOL WINAPI RemoveDirectoryW(LPCWSTR);
WINBASEAPI DWORD WINAPI GetFullPathNameA(LPCSTR,DWORD,LPSTR,LPSTR*);
WINBASEAPI DWORD WINAPI GetFullPathNameW(LPCWSTR,DWORD,LPWSTR,LPWSTR*);
WINBASEAPI BOOL WINAPI DefineDosDeviceA(DWORD,LPCSTR,LPCSTR);
WINBASEAPI BOOL WINAPI DefineDosDeviceW(DWORD,LPCWSTR,LPCWSTR);
WINBASEAPI DWORD WINAPI QueryDosDeviceA(LPCSTR,LPSTR,DWORD);
WINBASEAPI DWORD WINAPI QueryDosDeviceW(LPCWSTR,LPWSTR,DWORD);
WINBASEAPI HANDLE WINAPI CreateFileA(LPCSTR,DWORD,DWORD,LPSECURITY_ATTRIBUTES,DWORD,DWORD,HANDLE);
WINBASEAPI HANDLE WINAPI CreateFileW(LPCWSTR,DWORD,DWORD,LPSECURITY_ATTRIBUTES,DWORD,DWORD,HANDLE);
WINBASEAPI BOOL WINAPI SetFileAttributesA(LPCSTR,DWORD);
WINBASEAPI BOOL WINAPI SetFileAttributesW(LPCWSTR,DWORD);
WINBASEAPI DWORD WINAPI GetFileAttributesA(LPCSTR);
WINBASEAPI DWORD WINAPI GetFileAttributesW(LPCWSTR);
WINBASEAPI BOOL WINAPI GetFileAttributesExA(LPCSTR,GET_FILEEX_INFO_LEVELS,LPVOID);
WINBASEAPI BOOL WINAPI GetFileAttributesExW(LPCWSTR,GET_FILEEX_INFO_LEVELS,LPVOID);
WINBASEAPI DWORD WINAPI GetCompressedFileSizeA(LPCSTR,LPDWORD);
WINBASEAPI DWORD WINAPI GetCompressedFileSizeW(LPCWSTR,LPDWORD);
WINBASEAPI BOOL WINAPI DeleteFileA(LPCSTR);
WINBASEAPI BOOL WINAPI DeleteFileW(LPCWSTR);
#if (_WIN32_WINNT >= 0x0400)
WINBASEAPI HANDLE WINAPI FindFirstFileExA(LPCSTR,FINDEX_INFO_LEVELS,LPVOID,FINDEX_SEARCH_OPS,LPVOID,DWORD);
WINBASEAPI HANDLE WINAPI FindFirstFileExW(LPCWSTR,FINDEX_INFO_LEVELS,LPVOID,FINDEX_SEARCH_OPS,LPVOID,DWORD);
#endif /* _WIN32_WINNT >= 0x0400 */
WINBASEAPI HANDLE WINAPI FindFirstFileA(LPCSTR,LPWIN32_FIND_DATAA);
WINBASEAPI HANDLE WINAPI FindFirstFileW(LPCWSTR,LPWIN32_FIND_DATAW);
WINBASEAPI BOOL WINAPI FindNextFileA(HANDLE,LPWIN32_FIND_DATAA);
WINBASEAPI BOOL WINAPI FindNextFileW(HANDLE,LPWIN32_FIND_DATAW);
WINBASEAPI DWORD WINAPI SearchPathA(LPCSTR,LPCSTR,LPCSTR,DWORD,LPSTR,LPSTR*);
WINBASEAPI DWORD WINAPI SearchPathW(LPCWSTR,LPCWSTR,LPCWSTR,DWORD,LPWSTR,LPWSTR*);
WINBASEAPI BOOL WINAPI CopyFileA(LPCSTR,LPCSTR,BOOL);
WINBASEAPI BOOL WINAPI CopyFileW(LPCWSTR,LPCWSTR,BOOL);
#if (_WIN32_WINNT >= 0x0400)
WINBASEAPI BOOL WINAPI CopyFileExA(LPCSTR,LPCSTR,LPPROGRESS_ROUTINE,LPVOID,LPBOOL,DWORD);
WINBASEAPI BOOL WINAPI CopyFileExW(LPCWSTR,LPCWSTR,LPPROGRESS_ROUTINE,LPVOID,LPBOOL,DWORD);
#endif /* _WIN32_WINNT >= 0x0400 */
WINBASEAPI BOOL WINAPI MoveFileA(LPCSTR,LPCSTR);
WINBASEAPI BOOL WINAPI MoveFileW(LPCWSTR,LPCWSTR);
WINBASEAPI BOOL WINAPI MoveFileExA(LPCSTR,LPCSTR,DWORD);
WINBASEAPI BOOL WINAPI MoveFileExW(LPCWSTR,LPCWSTR,DWORD);
#if (_WIN32_WINNT >= 0x0500)
WINBASEAPI BOOL WINAPI MoveFileWithProgressA(LPCSTR,LPCSTR,LPPROGRESS_ROUTINE,LPVOID,DWORD);
WINBASEAPI BOOL WINAPI MoveFileWithProgressW(LPCWSTR,LPCWSTR,LPPROGRESS_ROUTINE,LPVOID,DWORD);
WINBASEAPI BOOL WINAPI ReplaceFileA(LPCSTR,LPCSTR,LPCSTR,DWORD,LPVOID,LPVOID);
WINBASEAPI BOOL WINAPI ReplaceFileW(LPCWSTR,LPCWSTR,LPCWSTR,DWORD,LPVOID,LPVOID);
WINBASEAPI BOOL WINAPI CreateHardLinkA(LPCSTR,LPCSTR,LPSECURITY_ATTRIBUTES);
WINBASEAPI BOOL WINAPI CreateHardLinkW(LPCWSTR,LPCWSTR,LPSECURITY_ATTRIBUTES);
#endif /* _WIN32_WINNT >= 0x0500 */
WINBASEAPI HANDLE WINAPI CreateNamedPipeA(LPCSTR,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,LPSECURITY_ATTRIBUTES);
WINBASEAPI HANDLE WINAPI CreateNamedPipeW(LPCWSTR,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,LPSECURITY_ATTRIBUTES);
WINBASEAPI BOOL WINAPI GetNamedPipeHandleStateA(HANDLE,LPDWORD,LPDWORD,LPDWORD,LPDWORD,LPSTR,DWORD);
WINBASEAPI BOOL WINAPI GetNamedPipeHandleStateW(HANDLE,LPDWORD,LPDWORD,LPDWORD,LPDWORD,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI CallNamedPipeA(LPCSTR,LPVOID,DWORD,LPVOID,DWORD,LPDWORD,DWORD);
WINBASEAPI BOOL WINAPI CallNamedPipeW(LPCWSTR,LPVOID,DWORD,LPVOID,DWORD,LPDWORD,DWORD);
WINBASEAPI BOOL WINAPI WaitNamedPipeA(LPCSTR,DWORD);
WINBASEAPI BOOL WINAPI WaitNamedPipeW(LPCWSTR,DWORD);
WINBASEAPI BOOL WINAPI SetVolumeLabelA(LPCSTR,LPCSTR);
WINBASEAPI BOOL WINAPI SetVolumeLabelW(LPCWSTR,LPCWSTR);
WINBASEAPI VOID WINAPI SetFileApisToOEM(VOID);
WINBASEAPI VOID WINAPI SetFileApisToANSI(VOID);
WINBASEAPI BOOL WINAPI AreFileApisANSI(VOID);
WINBASEAPI BOOL WINAPI GetVolumeInformationA(LPCSTR,LPSTR,DWORD,LPDWORD,LPDWORD,LPDWORD,LPSTR,DWORD);
WINBASEAPI BOOL WINAPI GetVolumeInformationW(LPCWSTR,LPWSTR,DWORD,LPDWORD,LPDWORD,LPDWORD,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI CancelIo(HANDLE);
WINADVAPI BOOL WINAPI ClearEventLogA(HANDLE,LPCSTR);
WINADVAPI BOOL WINAPI ClearEventLogW(HANDLE,LPCWSTR);
WINADVAPI BOOL WINAPI BackupEventLogA(HANDLE,LPCSTR);
WINADVAPI BOOL WINAPI BackupEventLogW(HANDLE,LPCWSTR);
WINADVAPI BOOL WINAPI CloseEventLog(HANDLE);
WINADVAPI BOOL WINAPI DeregisterEventSource(HANDLE);
WINADVAPI BOOL WINAPI NotifyChangeEventLog(HANDLE,HANDLE);
WINADVAPI BOOL WINAPI GetNumberOfEventLogRecords(HANDLE,PDWORD);
WINADVAPI BOOL WINAPI GetOldestEventLogRecord(HANDLE,PDWORD);
WINADVAPI HANDLE WINAPI OpenEventLogA(LPCSTR,LPCSTR);
WINADVAPI HANDLE WINAPI OpenEventLogW(LPCWSTR,LPCWSTR);
WINADVAPI HANDLE WINAPI RegisterEventSourceA(LPCSTR,LPCSTR);
WINADVAPI HANDLE WINAPI RegisterEventSourceW(LPCWSTR,LPCWSTR);
WINADVAPI HANDLE WINAPI OpenBackupEventLogA(LPCSTR,LPCSTR);
WINADVAPI HANDLE WINAPI OpenBackupEventLogW(LPCWSTR,LPCWSTR);
WINADVAPI BOOL WINAPI ReadEventLogA(HANDLE,DWORD,DWORD,LPVOID,DWORD,DWORD*,DWORD*);
WINADVAPI BOOL WINAPI ReadEventLogW(HANDLE,DWORD,DWORD,LPVOID,DWORD,DWORD*,DWORD*);
WINADVAPI BOOL WINAPI ReportEventA(HANDLE,WORD,WORD,DWORD,PSID,WORD,DWORD,LPCSTR*,LPVOID);
WINADVAPI BOOL WINAPI ReportEventW(HANDLE,WORD,WORD,DWORD,PSID,WORD,DWORD,LPCWSTR*,LPVOID);
WINADVAPI BOOL WINAPI GetEventLogInformation(HANDLE,DWORD,LPVOID,DWORD,LPDWORD);
WINADVAPI BOOL WINAPI DuplicateToken(HANDLE,SECURITY_IMPERSONATION_LEVEL,PHANDLE);
WINADVAPI BOOL WINAPI GetKernelObjectSecurity(HANDLE,SECURITY_INFORMATION,PSECURITY_DESCRIPTOR,DWORD,LPDWORD);
WINADVAPI BOOL WINAPI ImpersonateNamedPipeClient(HANDLE);
WINADVAPI BOOL WINAPI ImpersonateSelf(SECURITY_IMPERSONATION_LEVEL);
WINADVAPI BOOL WINAPI RevertToSelf(VOID);
WINADVAPI BOOL APIENTRY SetThreadToken(PHANDLE,HANDLE);
WINADVAPI BOOL WINAPI AccessCheck(PSECURITY_DESCRIPTOR,HANDLE,DWORD,PGENERIC_MAPPING,PPRIVILEGE_SET,LPDWORD,LPDWORD,LPBOOL);
#if (_WIN32_WINNT >= 0x0500)
WINADVAPI BOOL WINAPI AccessCheckByType(PSECURITY_DESCRIPTOR,PSID,HANDLE,DWORD,POBJECT_TYPE_LIST,DWORD,PGENERIC_MAPPING,PPRIVILEGE_SET,LPDWORD,LPDWORD,LPBOOL);
WINADVAPI BOOL WINAPI AccessCheckByTypeResultList(PSECURITY_DESCRIPTOR,PSID,HANDLE,DWORD,POBJECT_TYPE_LIST,DWORD,PGENERIC_MAPPING,PPRIVILEGE_SET,LPDWORD,LPDWORD,LPDWORD);
#endif /* _WIN32_WINNT >=  0x0500 */
WINADVAPI BOOL WINAPI OpenProcessToken(HANDLE,DWORD,PHANDLE);
WINADVAPI BOOL WINAPI OpenThreadToken(HANDLE,DWORD,BOOL,PHANDLE);
WINADVAPI BOOL WINAPI GetTokenInformation(HANDLE,TOKEN_INFORMATION_CLASS,LPVOID,DWORD,PDWORD);
WINADVAPI BOOL WINAPI SetTokenInformation(HANDLE,TOKEN_INFORMATION_CLASS,LPVOID,DWORD);
WINADVAPI BOOL WINAPI AdjustTokenPrivileges(HANDLE,BOOL,PTOKEN_PRIVILEGES,DWORD,PTOKEN_PRIVILEGES,PDWORD);
WINADVAPI BOOL WINAPI AdjustTokenGroups(HANDLE,BOOL,PTOKEN_GROUPS,DWORD,PTOKEN_GROUPS,PDWORD);
WINADVAPI BOOL WINAPI PrivilegeCheck(HANDLE,PPRIVILEGE_SET,LPBOOL);
WINADVAPI BOOL WINAPI AccessCheckAndAuditAlarmA(LPCSTR,LPVOID,LPSTR,LPSTR,PSECURITY_DESCRIPTOR,DWORD,PGENERIC_MAPPING,BOOL,LPDWORD,LPBOOL,LPBOOL);
WINADVAPI BOOL WINAPI AccessCheckAndAuditAlarmW(LPCWSTR,LPVOID,LPWSTR,LPWSTR,PSECURITY_DESCRIPTOR,DWORD,PGENERIC_MAPPING,BOOL,LPDWORD,LPBOOL,LPBOOL);
#if (_WIN32_WINNT >= 0x0500)
WINADVAPI BOOL WINAPI AccessCheckByTypeAndAuditAlarmA(LPCSTR,LPVOID,LPCSTR,LPCSTR,PSECURITY_DESCRIPTOR,PSID,DWORD,AUDIT_EVENT_TYPE,DWORD,POBJECT_TYPE_LIST,DWORD,PGENERIC_MAPPING,BOOL,LPDWORD,LPBOOL,LPBOOL);
WINADVAPI BOOL WINAPI AccessCheckByTypeAndAuditAlarmW(LPCWSTR,LPVOID,LPCWSTR,LPCWSTR,PSECURITY_DESCRIPTOR,PSID,DWORD,AUDIT_EVENT_TYPE,DWORD,POBJECT_TYPE_LIST,DWORD,PGENERIC_MAPPING,BOOL,LPDWORD,LPBOOL,LPBOOL);
WINADVAPI BOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmA(LPCSTR,LPVOID,LPCSTR,LPCSTR,PSECURITY_DESCRIPTOR,PSID,DWORD,AUDIT_EVENT_TYPE,DWORD,POBJECT_TYPE_LIST,DWORD,PGENERIC_MAPPING,BOOL,LPDWORD,LPDWORD,LPBOOL);
WINADVAPI BOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmW(LPCWSTR,LPVOID,LPCWSTR,LPCWSTR,PSECURITY_DESCRIPTOR,PSID,DWORD,AUDIT_EVENT_TYPE,DWORD,POBJECT_TYPE_LIST,DWORD,PGENERIC_MAPPING,BOOL,LPDWORD,LPDWORD,LPBOOL);
WINADVAPI BOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmByHandleA(LPCSTR,LPVOID,HANDLE,LPCSTR,LPCSTR,PSECURITY_DESCRIPTOR,PSID,DWORD,AUDIT_EVENT_TYPE,DWORD,POBJECT_TYPE_LIST,DWORD,PGENERIC_MAPPING,BOOL,LPDWORD,LPDWORD,LPBOOL);
WINADVAPI BOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmByHandleW(LPCWSTR,LPVOID,HANDLE,LPCWSTR,LPCWSTR,PSECURITY_DESCRIPTOR,PSID,DWORD,AUDIT_EVENT_TYPE,DWORD,POBJECT_TYPE_LIST,DWORD,PGENERIC_MAPPING,BOOL,LPDWORD,LPDWORD,LPBOOL);
#endif /* _WIN32_WINNT >= 0x0500 */
WINADVAPI BOOL WINAPI ObjectOpenAuditAlarmA(LPCSTR,LPVOID,LPSTR,LPSTR,PSECURITY_DESCRIPTOR,HANDLE,DWORD,DWORD,PPRIVILEGE_SET,BOOL,BOOL,LPBOOL);
WINADVAPI BOOL WINAPI ObjectOpenAuditAlarmW(LPCWSTR,LPVOID,LPWSTR,LPWSTR,PSECURITY_DESCRIPTOR,HANDLE,DWORD,DWORD,PPRIVILEGE_SET,BOOL,BOOL,LPBOOL);
WINADVAPI BOOL WINAPI ObjectPrivilegeAuditAlarmA(LPCSTR,LPVOID,HANDLE,DWORD,PPRIVILEGE_SET,BOOL);
WINADVAPI BOOL WINAPI ObjectPrivilegeAuditAlarmW(LPCWSTR,LPVOID,HANDLE,DWORD,PPRIVILEGE_SET,BOOL);
WINADVAPI BOOL WINAPI ObjectCloseAuditAlarmA(LPCSTR,LPVOID,BOOL);
WINADVAPI BOOL WINAPI ObjectCloseAuditAlarmW(LPCWSTR,LPVOID,BOOL);
WINADVAPI BOOL WINAPI ObjectDeleteAuditAlarmA(LPCSTR,LPVOID,BOOL);
WINADVAPI BOOL WINAPI ObjectDeleteAuditAlarmW(LPCWSTR,LPVOID,BOOL);
WINADVAPI BOOL WINAPI PrivilegedServiceAuditAlarmA(LPCSTR,LPCSTR,HANDLE,PPRIVILEGE_SET,BOOL);
WINADVAPI BOOL WINAPI PrivilegedServiceAuditAlarmW(LPCWSTR,LPCWSTR,HANDLE,PPRIVILEGE_SET,BOOL);
WINADVAPI BOOL WINAPI IsValidSid(PSID);
WINADVAPI BOOL WINAPI EqualSid(PSID,PSID);
WINADVAPI BOOL WINAPI EqualPrefixSid(PSID,PSID);
WINADVAPI DWORD WINAPI GetSidLengthRequired(UCHAR);
WINADVAPI BOOL WINAPI AllocateAndInitializeSid(PSID_IDENTIFIER_AUTHORITY,BYTE,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,PSID*);
WINADVAPI PVOID WINAPI FreeSid(PSID);
WINADVAPI BOOL WINAPI InitializeSid(PSID,PSID_IDENTIFIER_AUTHORITY,BYTE);
WINADVAPI PSID_IDENTIFIER_AUTHORITY WINAPI GetSidIdentifierAuthority(PSID);
WINADVAPI PDWORD WINAPI GetSidSubAuthority(PSID,DWORD);
WINADVAPI PUCHAR WINAPI GetSidSubAuthorityCount(PSID);
WINADVAPI DWORD WINAPI GetLengthSid(PSID);
WINADVAPI BOOL WINAPI CopySid(DWORD,PSID,PSID);
WINADVAPI BOOL WINAPI AreAllAccessesGranted(DWORD,DWORD);
WINADVAPI BOOL WINAPI AreAnyAccessesGranted(DWORD,DWORD);
WINADVAPI VOID WINAPI MapGenericMask(PDWORD,PGENERIC_MAPPING);
WINADVAPI BOOL WINAPI IsValidAcl(PACL);
WINADVAPI BOOL WINAPI InitializeAcl(PACL,DWORD,DWORD);
WINADVAPI BOOL WINAPI GetAclInformation(PACL,LPVOID,DWORD,ACL_INFORMATION_CLASS);
WINADVAPI BOOL WINAPI SetAclInformation(PACL,LPVOID,DWORD,ACL_INFORMATION_CLASS);
WINADVAPI BOOL WINAPI AddAce(PACL,DWORD,DWORD,LPVOID,DWORD);
WINADVAPI BOOL WINAPI DeleteAce(PACL,DWORD);
WINADVAPI BOOL WINAPI GetAce(PACL,DWORD,LPVOID*);
WINADVAPI BOOL WINAPI AddAccessAllowedAce(PACL,DWORD,DWORD,PSID);
#if (_WIN32_WINNT >= 0x0500)
WINADVAPI BOOL WINAPI AddAccessAllowedAceEx(PACL,DWORD,DWORD,DWORD,PSID);
WINADVAPI BOOL WINAPI AddAccessDeniedAceEx(PACL,DWORD,DWORD,DWORD,PSID);
WINADVAPI BOOL WINAPI AddAuditAccessAceEx(PACL,DWORD,DWORD,DWORD,PSID,BOOL,BOOL);
WINADVAPI BOOL WINAPI AddAccessAllowedObjectAce(PACL,DWORD,DWORD,DWORD,GUID*,GUID*,PSID);
WINADVAPI BOOL WINAPI AddAccessDeniedObjectAce(PACL,DWORD,DWORD,DWORD,GUID*,GUID*,PSID);
WINADVAPI BOOL WINAPI AddAuditAccessObjectAce(PACL,DWORD,DWORD,DWORD,GUID*,GUID*,PSID,BOOL,BOOL);
WINADVAPI BOOL WINAPI SetSecurityDescriptorControl(PSECURITY_DESCRIPTOR,SECURITY_DESCRIPTOR_CONTROL,SECURITY_DESCRIPTOR_CONTROL);
WINADVAPI BOOL WINAPI ConvertToAutoInheritPrivateObjectSecurity(PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR*,GUID*,BOOLEAN,PGENERIC_MAPPING);
WINADVAPI BOOL WINAPI CreatePrivateObjectSecurityEx(PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR*,GUID*,BOOL,ULONG,HANDLE,PGENERIC_MAPPING);
WINADVAPI BOOL WINAPI SetPrivateObjectSecurityEx(SECURITY_INFORMATION,PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR*,ULONG,PGENERIC_MAPPING,HANDLE);
#endif /* _WIN32_WINNT >= 0x0500 */
WINADVAPI BOOL WINAPI AddAccessDeniedAce(PACL,DWORD,DWORD,PSID);
WINADVAPI BOOL WINAPI AddAuditAccessAce(PACL,DWORD,DWORD,PSID,BOOL,BOOL);
WINADVAPI BOOL WINAPI FindFirstFreeAce(PACL,LPVOID*);
WINADVAPI BOOL WINAPI InitializeSecurityDescriptor(PSECURITY_DESCRIPTOR,DWORD);
WINADVAPI BOOL WINAPI IsValidSecurityDescriptor(PSECURITY_DESCRIPTOR);
WINADVAPI DWORD WINAPI GetSecurityDescriptorLength(PSECURITY_DESCRIPTOR);
WINADVAPI BOOL WINAPI GetSecurityDescriptorControl(PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR_CONTROL,LPDWORD);
WINADVAPI BOOL WINAPI SetSecurityDescriptorDacl(PSECURITY_DESCRIPTOR,BOOL,PACL,BOOL);
WINADVAPI BOOL WINAPI GetSecurityDescriptorDacl(PSECURITY_DESCRIPTOR,LPBOOL,PACL*,LPBOOL);
WINADVAPI BOOL WINAPI SetSecurityDescriptorSacl(PSECURITY_DESCRIPTOR,BOOL,PACL,BOOL);
WINADVAPI BOOL WINAPI GetSecurityDescriptorSacl(PSECURITY_DESCRIPTOR,LPBOOL,PACL*,LPBOOL);
WINADVAPI BOOL WINAPI SetSecurityDescriptorOwner(PSECURITY_DESCRIPTOR,PSID,BOOL);
WINADVAPI BOOL WINAPI GetSecurityDescriptorOwner(PSECURITY_DESCRIPTOR,PSID*,LPBOOL);
WINADVAPI BOOL WINAPI SetSecurityDescriptorGroup(PSECURITY_DESCRIPTOR,PSID,BOOL);
WINADVAPI BOOL WINAPI GetSecurityDescriptorGroup(PSECURITY_DESCRIPTOR,PSID*,LPBOOL);
WINADVAPI DWORD WINAPI SetSecurityDescriptorRMControl(PSECURITY_DESCRIPTOR,PUCHAR);
WINADVAPI DWORD WINAPI GetSecurityDescriptorRMControl(PSECURITY_DESCRIPTOR,PUCHAR);
WINADVAPI BOOL WINAPI CreatePrivateObjectSecurity(PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR*,BOOL,HANDLE,PGENERIC_MAPPING);
WINADVAPI BOOL WINAPI SetPrivateObjectSecurity(SECURITY_INFORMATION,PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR*,PGENERIC_MAPPING,HANDLE);
WINADVAPI BOOL WINAPI GetPrivateObjectSecurity(PSECURITY_DESCRIPTOR,SECURITY_INFORMATION,PSECURITY_DESCRIPTOR,DWORD,PDWORD);
WINADVAPI BOOL WINAPI DestroyPrivateObjectSecurity(PSECURITY_DESCRIPTOR*);
WINADVAPI BOOL WINAPI MakeSelfRelativeSD(PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR,LPDWORD);
WINADVAPI BOOL WINAPI MakeAbsoluteSD(PSECURITY_DESCRIPTOR,PSECURITY_DESCRIPTOR,LPDWORD,PACL,LPDWORD,PACL,LPDWORD,PSID,LPDWORD,PSID,LPDWORD);
WINADVAPI BOOL WINAPI MakeAbsoluteSD2(PSECURITY_DESCRIPTOR,LPDWORD);
WINADVAPI BOOL WINAPI SetFileSecurityA(LPCSTR,SECURITY_INFORMATION,PSECURITY_DESCRIPTOR);
WINADVAPI BOOL WINAPI SetFileSecurityW(LPCWSTR,SECURITY_INFORMATION,PSECURITY_DESCRIPTOR);
WINADVAPI BOOL WINAPI GetFileSecurityA(LPCSTR,SECURITY_INFORMATION,PSECURITY_DESCRIPTOR,DWORD,LPDWORD);
WINADVAPI BOOL WINAPI GetFileSecurityW(LPCWSTR,SECURITY_INFORMATION,PSECURITY_DESCRIPTOR,DWORD,LPDWORD);
WINADVAPI BOOL WINAPI SetKernelObjectSecurity(HANDLE,SECURITY_INFORMATION,PSECURITY_DESCRIPTOR);
WINBASEAPI HANDLE WINAPI FindFirstChangeNotificationA(LPCSTR,BOOL,DWORD);
WINBASEAPI HANDLE WINAPI FindFirstChangeNotificationW(LPCWSTR,BOOL,DWORD);
WINBASEAPI BOOL WINAPI FindNextChangeNotification(HANDLE);
WINBASEAPI BOOL WINAPI FindCloseChangeNotification(HANDLE);
#if (_WIN32_WINNT >= 0x0400)
WINBASEAPI BOOL WINAPI ReadDirectoryChangesW(HANDLE,LPVOID,DWORD,BOOL,DWORD,LPDWORD,LPOVERLAPPED,LPOVERLAPPED_COMPLETION_ROUTINE);
#endif /* _WIN32_WINNT >= 0x0400 */
WINBASEAPI BOOL WINAPI VirtualLock(LPVOID,SIZE_T);
WINBASEAPI BOOL WINAPI VirtualUnlock(LPVOID,SIZE_T);
WINBASEAPI LPVOID WINAPI MapViewOfFileEx(HANDLE,DWORD,DWORD,DWORD,SIZE_T,LPVOID);
WINBASEAPI BOOL WINAPI SetPriorityClass(HANDLE,DWORD);
WINBASEAPI DWORD WINAPI GetPriorityClass(HANDLE);
WINBASEAPI BOOL WINAPI IsBadReadPtr(CONST VOID*,UINT_PTR);
WINBASEAPI BOOL WINAPI IsBadWritePtr(LPVOID,UINT_PTR);
WINBASEAPI BOOL WINAPI IsBadHugeReadPtr(CONST VOID*,UINT_PTR);
WINBASEAPI BOOL WINAPI IsBadHugeWritePtr(LPVOID,UINT_PTR);
WINBASEAPI BOOL WINAPI IsBadCodePtr(FARPROC);
WINBASEAPI BOOL WINAPI IsBadStringPtrA(LPCSTR,UINT_PTR);
WINBASEAPI BOOL WINAPI IsBadStringPtrW(LPCWSTR,UINT_PTR);
WINADVAPI BOOL WINAPI LookupAccountSidA(LPCSTR,PSID,LPSTR,LPDWORD,LPSTR,LPDWORD,PSID_NAME_USE);
WINADVAPI BOOL WINAPI LookupAccountSidW(LPCWSTR,PSID,LPWSTR,LPDWORD,LPWSTR,LPDWORD,PSID_NAME_USE);
WINADVAPI BOOL WINAPI LookupAccountNameA(LPCSTR,LPCSTR,PSID,LPDWORD,LPSTR,LPDWORD,PSID_NAME_USE);
WINADVAPI BOOL WINAPI LookupAccountNameW(LPCWSTR,LPCWSTR,PSID,LPDWORD,LPWSTR,LPDWORD,PSID_NAME_USE);
WINADVAPI BOOL WINAPI LookupPrivilegeValueA(LPCSTR,LPCSTR,PLUID);
WINADVAPI BOOL WINAPI LookupPrivilegeValueW(LPCWSTR,LPCWSTR,PLUID);
WINADVAPI BOOL WINAPI LookupPrivilegeNameA(LPCSTR,PLUID,LPSTR,LPDWORD);
WINADVAPI BOOL WINAPI LookupPrivilegeNameW(LPCWSTR,PLUID,LPWSTR,LPDWORD);
WINADVAPI BOOL WINAPI LookupPrivilegeDisplayNameA(LPCSTR,LPCSTR,LPSTR,LPDWORD,LPDWORD);
WINADVAPI BOOL WINAPI LookupPrivilegeDisplayNameW(LPCWSTR,LPCWSTR,LPWSTR,LPDWORD,LPDWORD);
WINADVAPI BOOL WINAPI AllocateLocallyUniqueId(PLUID);
WINBASEAPI BOOL WINAPI BuildCommDCBA(LPCSTR,LPDCB);
WINBASEAPI BOOL WINAPI BuildCommDCBW(LPCWSTR,LPDCB);
WINBASEAPI BOOL WINAPI BuildCommDCBAndTimeoutsA(LPCSTR,LPDCB,LPCOMMTIMEOUTS);
WINBASEAPI BOOL WINAPI BuildCommDCBAndTimeoutsW(LPCWSTR,LPDCB,LPCOMMTIMEOUTS);
WINBASEAPI BOOL WINAPI CommConfigDialogA(LPCSTR,HWND,LPCOMMCONFIG);
WINBASEAPI BOOL WINAPI CommConfigDialogW(LPCWSTR,HWND,LPCOMMCONFIG);
WINBASEAPI BOOL WINAPI GetDefaultCommConfigA(LPCSTR,LPCOMMCONFIG,LPDWORD);
WINBASEAPI BOOL WINAPI GetDefaultCommConfigW(LPCWSTR,LPCOMMCONFIG,LPDWORD);
WINBASEAPI BOOL WINAPI SetDefaultCommConfigA(LPCSTR,LPCOMMCONFIG,DWORD);
WINBASEAPI BOOL WINAPI SetDefaultCommConfigW(LPCWSTR,LPCOMMCONFIG,DWORD);
WINBASEAPI BOOL WINAPI GetComputerNameA(LPSTR,LPDWORD);
WINBASEAPI BOOL WINAPI GetComputerNameW(LPWSTR,LPDWORD);
WINBASEAPI BOOL WINAPI SetComputerNameA(LPCSTR);
WINBASEAPI BOOL WINAPI SetComputerNameW(LPCWSTR);
#if (_WIN32_WINNT >= 0x0500)
WINBASEAPI BOOL WINAPI GetComputerNameExA(COMPUTER_NAME_FORMAT,LPSTR,LPDWORD);
WINBASEAPI BOOL WINAPI GetComputerNameExW(COMPUTER_NAME_FORMAT,LPWSTR,LPDWORD);
WINBASEAPI BOOL WINAPI SetComputerNameExA(COMPUTER_NAME_FORMAT,LPCSTR);
WINBASEAPI BOOL WINAPI SetComputerNameExW(COMPUTER_NAME_FORMAT,LPCWSTR);
WINBASEAPI BOOL WINAPI DnsHostnameToComputerNameA(LPSTR,LPSTR,LPDWORD);
WINBASEAPI BOOL WINAPI DnsHostnameToComputerNameW(LPWSTR,LPWSTR,LPDWORD);
#endif /* _WIN32_WINNT >= 0x0500 */
WINADVAPI BOOL WINAPI GetUserNameA(LPSTR,LPDWORD);
WINADVAPI BOOL WINAPI GetUserNameW(LPWSTR,LPDWORD);
WINADVAPI BOOL WINAPI LogonUserA(LPSTR,LPSTR,LPSTR,DWORD,DWORD,PHANDLE);
WINADVAPI BOOL WINAPI LogonUserW(LPWSTR,LPWSTR,LPWSTR,DWORD,DWORD,PHANDLE);
WINADVAPI BOOL WINAPI ImpersonateLoggedOnUser(HANDLE);
WINADVAPI BOOL WINAPI CreateProcessAsUserA(HANDLE,LPCSTR,LPSTR,LPSECURITY_ATTRIBUTES,LPSECURITY_ATTRIBUTES,BOOL,DWORD,LPVOID,LPCSTR,LPSTARTUPINFOA,LPPROCESS_INFORMATION);
WINADVAPI BOOL WINAPI CreateProcessAsUserW(HANDLE,LPCWSTR,LPWSTR,LPSECURITY_ATTRIBUTES,LPSECURITY_ATTRIBUTES,BOOL,DWORD,LPVOID,LPCWSTR,LPSTARTUPINFOW,LPPROCESS_INFORMATION);
#if (_WIN32_WINNT >= 0x0500)
WINADVAPI BOOL WINAPI CreateProcessWithLogonW(LPCWSTR,LPCWSTR,LPCWSTR,DWORD,LPCWSTR,LPWSTR,DWORD,LPVOID,LPCWSTR,LPSTARTUPINFOW,LPPROCESS_INFORMATION);
#endif /* _WIN32_WINNT >= 0x0500 */
WINADVAPI BOOL APIENTRY ImpersonateAnonymousToken(HANDLE);
WINADVAPI BOOL WINAPI DuplicateTokenEx(HANDLE,DWORD,LPSECURITY_ATTRIBUTES,SECURITY_IMPERSONATION_LEVEL,TOKEN_TYPE,PHANDLE);
WINADVAPI BOOL APIENTRY CreateRestrictedToken(HANDLE,DWORD,DWORD,PSID_AND_ATTRIBUTES,DWORD,PLUID_AND_ATTRIBUTES,DWORD,PSID_AND_ATTRIBUTES,PHANDLE);
WINADVAPI BOOL WINAPI IsProcessRestricted();
WINADVAPI BOOL WINAPI IsTokenRestricted(HANDLE);
WINADVAPI PSID WINAPI GetSiteSidFromToken(HANDLE);
WINADVAPI PSID WINAPI GetSiteSidFromUrl(LPCWSTR);
WINADVAPI HRESULT WINAPI GetSiteNameFromSid(PSID,LPWSTR*);
WINADVAPI HRESULT WINAPI GetMangledSiteSid(PSID,ULONG,LPWSTR*);
WINADVAPI ULONG WINAPI GetSiteDirectoryA(HANDLE,LPSTR,ULONG);
WINADVAPI ULONG WINAPI GetSiteDirectoryW(HANDLE,LPWSTR,ULONG);
BOOL APIENTRY CheckTokenMembership(HANDLE TokenHandle,PSID SidToCheck,PBOOL IsMember);
#if (_WIN32_WINNT >= 0x0500)
WINBASEAPI BOOL WINAPI RegisterWaitForSingleObject(PHANDLE,HANDLE,WAITORTIMERCALLBACK,PVOID,ULONG,ULONG);
WINBASEAPI HANDLE WINAPI RegisterWaitForSingleObjectEx(HANDLE,WAITORTIMERCALLBACK,PVOID,ULONG,ULONG);
WINBASEAPI BOOL WINAPI UnregisterWait(HANDLE);
WINBASEAPI BOOL WINAPI UnregisterWaitEx(HANDLE,HANDLE);
WINBASEAPI BOOL WINAPI QueueUserWorkItem(LPTHREAD_START_ROUTINE,PVOID,ULONG);
WINBASEAPI BOOL WINAPI BindIoCompletionCallback(HANDLE,LPOVERLAPPED_COMPLETION_ROUTINE,ULONG);
WINBASEAPI HANDLE WINAPI CreateTimerQueue(VOID);
WINBASEAPI BOOL WINAPI CreateTimerQueueTimer(PHANDLE,HANDLE,WAITORTIMERCALLBACK,PVOID,DWORD,DWORD,ULONG);
WINBASEAPI BOOL WINAPI ChangeTimerQueueTimer(HANDLE,HANDLE,ULONG,ULONG);
WINBASEAPI BOOL WINAPI DeleteTimerQueueTimer(HANDLE,HANDLE,HANDLE);
WINBASEAPI BOOL WINAPI DeleteTimerQueueEx(HANDLE,HANDLE);
WINBASEAPI HANDLE WINAPI SetTimerQueueTimer(HANDLE,WAITORTIMERCALLBACK,PVOID,DWORD,DWORD,BOOL);
WINBASEAPI BOOL WINAPI CancelTimerQueueTimer(HANDLE,HANDLE);
WINBASEAPI BOOL WINAPI DeleteTimerQueue(HANDLE);
#endif /* _WIN32_WINNT >= 0x0500 */
#if (_WIN32_WINNT >= 0x0400)
WINADVAPI BOOL WINAPI GetCurrentHwProfileA(LPHW_PROFILE_INFOA);
WINADVAPI BOOL WINAPI GetCurrentHwProfileW(LPHW_PROFILE_INFOW);
#endif /* _WIN32_WINNT >= 0x0400 */
WINBASEAPI BOOL WINAPI QueryPerformanceCounter(LARGE_INTEGER*);
WINBASEAPI BOOL WINAPI QueryPerformanceFrequency(LARGE_INTEGER*);
WINBASEAPI BOOL WINAPI GetVersionExA(LPOSVERSIONINFOA);
WINBASEAPI BOOL WINAPI GetVersionExW(LPOSVERSIONINFOW);
WINBASEAPI BOOL WINAPI VerifyVersionInfoA(LPOSVERSIONINFOEXA,DWORD,DWORDLONG);
WINBASEAPI BOOL WINAPI VerifyVersionInfoW(LPOSVERSIONINFOEXW,DWORD,DWORDLONG);
BOOL WINAPI GetSystemPowerStatus(LPSYSTEM_POWER_STATUS lpSystemPowerStatus);
BOOL WINAPI SetSystemPowerState(BOOL fSuspend,BOOL fForce);
#if (_WIN32_WINNT >= 0x0500)
WINBASEAPI BOOL WINAPI AllocateUserPhysicalPages(HANDLE,PULONG_PTR,PULONG_PTR);
WINBASEAPI BOOL WINAPI FreeUserPhysicalPages(HANDLE,PULONG_PTR,PULONG_PTR);
WINBASEAPI BOOL WINAPI MapUserPhysicalPages(PVOID,ULONG_PTR,PULONG_PTR);
WINBASEAPI BOOL WINAPI MapUserPhysicalPagesScatter(PVOID*,ULONG_PTR,PULONG_PTR);
WINBASEAPI HANDLE WINAPI CreateJobObjectA(LPSECURITY_ATTRIBUTES,LPCSTR);
WINBASEAPI HANDLE WINAPI CreateJobObjectW(LPSECURITY_ATTRIBUTES,LPCWSTR);
WINBASEAPI HANDLE WINAPI OpenJobObjectA(DWORD,BOOL,LPCSTR);
WINBASEAPI HANDLE WINAPI OpenJobObjectW(DWORD,BOOL,LPCWSTR);
WINBASEAPI BOOL WINAPI AssignProcessToJobObject(HANDLE,HANDLE);
WINBASEAPI BOOL WINAPI TerminateJobObject(HANDLE,UINT);
WINBASEAPI BOOL WINAPI QueryInformationJobObject(HANDLE,JOBOBJECTINFOCLASS,LPVOID,DWORD,LPDWORD);
WINBASEAPI BOOL WINAPI SetInformationJobObject(HANDLE,JOBOBJECTINFOCLASS,LPVOID,DWORD);
WINBASEAPI HANDLE WINAPI FindFirstVolumeA(LPSTR,DWORD);
WINBASEAPI HANDLE WINAPI FindFirstVolumeW(LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI FindNextVolumeA(HANDLE,LPSTR,DWORD);
WINBASEAPI BOOL WINAPI FindNextVolumeW(HANDLE,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI FindVolumeClose(HANDLE);
WINBASEAPI HANDLE WINAPI FindFirstVolumeMountPointA(LPCSTR,LPSTR,DWORD);
WINBASEAPI HANDLE WINAPI FindFirstVolumeMountPointW(LPCWSTR,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI FindNextVolumeMountPointA(HANDLE,LPSTR,DWORD);
WINBASEAPI BOOL WINAPI FindNextVolumeMountPointW(HANDLE,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI FindVolumeMountPointClose(HANDLE);
WINBASEAPI BOOL WINAPI SetVolumeMountPointA(LPCSTR,LPCSTR);
WINBASEAPI BOOL WINAPI SetVolumeMountPointW(LPCWSTR,LPCWSTR);
WINBASEAPI BOOL WINAPI DeleteVolumeMountPointA(LPCSTR);
WINBASEAPI BOOL WINAPI DeleteVolumeMountPointW(LPCWSTR);
WINBASEAPI BOOL WINAPI GetVolumeNameForVolumeMountPointA(LPCSTR,LPSTR,DWORD);
WINBASEAPI BOOL WINAPI GetVolumeNameForVolumeMountPointW(LPCWSTR,LPWSTR,DWORD);
WINBASEAPI BOOL WINAPI GetVolumePathNameA(LPCSTR,LPSTR,DWORD);
WINBASEAPI BOOL WINAPI GetVolumePathNameW(LPCWSTR,LPWSTR,DWORD);
#endif /* _WIN32_WINNT >= 0x0500 */
WINBASEAPI BOOL WINAPI ProcessIdToSessionId(DWORD,DWORD*);

#if (_WIN32_WINNT >= 0x0501)
WINBASEAPI VOID WINAPI GetNativeSystemInfo(LPSYSTEM_INFO);
WINBASEAPI HANDLE WINAPI CreateMemoryResourceNotification(MEMORY_RESOURCE_NOTIFICATION_TYPE);
WINBASEAPI BOOL WINAPI QueryMemoryResourceNotification(HANDLE,PBOOL);
WINBASEAPI UINT WINAPI GetSystemWow64DirectoryA(LPSTR,UINT);
WINBASEAPI UINT WINAPI GetSystemWow64DirectoryW(LPWSTR,UINT);
WINBASEAPI DWORD WINAPI WTSGetActiveConsoleSessionId(void);
WINBASEAPI BOOL WINAPI IsWow64Process(HANDLE,PBOOL);
WINADVAPI BOOL WINAPI IsWellKnownSid(PSID,WELL_KNOWN_SID_TYPE);
WINADVAPI BOOL WINAPI CreateWellKnownSid(WELL_KNOWN_SID_TYPE,PSID,PSID,DWORD*);
WINADVAPI BOOL WINAPI EqualDomainSid(PSID,PSID,BOOL*);
WINADVAPI BOOL WINAPI GetWindowsAccountDomainSid(PSID,PSID,DWORD*);
#endif /* _WIN32_WINNT >= 0x0501 */

#ifdef UNICODE
typedef STARTUPINFOW STARTUPINFO;
typedef LPSTARTUPINFOW LPSTARTUPINFO;
typedef WIN32_FIND_DATAW WIN32_FIND_DATA;
typedef PWIN32_FIND_DATAW PWIN32_FIND_DATA;
typedef LPWIN32_FIND_DATAW LPWIN32_FIND_DATA;
#if (_WIN32_WINNT >= 0x0400)
typedef HW_PROFILE_INFOW HW_PROFILE_INFO;
typedef LPHW_PROFILE_INFOW LPHW_PROFILE_INFO;
#endif /* _WIN32_WINNT >= 0x0400 */
#ifdef STRICT
#define ENUMRESTYPEPROC ENUMRESTYPEPROCW
#define ENUMRESNAMEPROC ENUMRESNAMEPROCW
#define ENUMRESLANGPROC ENUMRESLANGPROCW
#else
typedef ENUMRESTYPEPROCW ENUMRESTYPEPROC;
typedef ENUMRESNAMEPROCW ENUMRESNAMEPROC;
typedef ENUMRESLANGPROCW ENUMRESLANGPROC;
#endif /* STRICT */
#if (_WIN32_WINNT >= 0x0501)
#define GetSystemWow64Directory  GetSystemWow64DirectoryW
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_A GET_SYSTEM_WOW64_DIRECTORY_NAME_W_A
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_W GET_SYSTEM_WOW64_DIRECTORY_NAME_W_W
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_T GET_SYSTEM_WOW64_DIRECTORY_NAME_W_T
#endif /* _WIN32_WINNT >= 0x0501 */
#define GetBinaryType GetBinaryTypeW
#define GetShortPathName GetShortPathNameW
#define GetLongPathName GetLongPathNameW
#define GetEnvironmentStrings GetEnvironmentStringsW
#define FreeEnvironmentStrings FreeEnvironmentStringsW
#define FormatMessage FormatMessageW
#define CreateMailslot CreateMailslotW
#define EncryptFile EncryptFileW
#define DecryptFile DecryptFileW
#define FileEncryptionStatus FileEncryptionStatusW
#define OpenEncryptedFileRaw OpenEncryptedFileRawW
#define lstrcmp lstrcmpW
#define lstrcmpi lstrcmpiW
#define lstrcpyn lstrcpynW
#define lstrcpy lstrcpyW
#define lstrcat lstrcatW
#define lstrlen lstrlenW
#define CreateMutex CreateMutexW
#define OpenMutex OpenMutexW
#define CreateEvent CreateEventW
#define OpenEvent OpenEventW
#define CreateSemaphore CreateSemaphoreW
#define OpenSemaphore OpenSemaphoreW
#define CreateWaitableTimer CreateWaitableTimerW
#define OpenWaitableTimer OpenWaitableTimerW
#define CreateFileMapping CreateFileMappingW
#define OpenFileMapping OpenFileMappingW
#define GetLogicalDriveStrings GetLogicalDriveStringsW
#define LoadLibrary LoadLibraryW
#define LoadLibraryEx LoadLibraryExW
#define GetModuleFileName GetModuleFileNameW
#define GetModuleHandle GetModuleHandleW
#define CreateProcess CreateProcessW
#define FatalAppExit FatalAppExitW
#define GetStartupInfo GetStartupInfoW
#define GetCommandLine GetCommandLineW
#define GetEnvironmentVariable GetEnvironmentVariableW
#define SetEnvironmentVariable SetEnvironmentVariableW
#define ExpandEnvironmentStrings ExpandEnvironmentStringsW
#define OutputDebugString OutputDebugStringW
#define FindResource FindResourceW
#define FindResourceEx FindResourceExW
#define EnumResourceTypes EnumResourceTypesW
#define EnumResourceNames EnumResourceNamesW
#define EnumResourceLanguages EnumResourceLanguagesW
#define BeginUpdateResource BeginUpdateResourceW
#define UpdateResource UpdateResourceW
#define EndUpdateResource EndUpdateResourceW
#define GlobalAddAtom GlobalAddAtomW
#define GlobalFindAtom GlobalFindAtomW
#define GlobalGetAtomName GlobalGetAtomNameW
#define AddAtom AddAtomW
#define FindAtom FindAtomW
#define GetAtomName GetAtomNameW
#define GetProfileInt GetProfileIntW
#define GetProfileString GetProfileStringW
#define WriteProfileString WriteProfileStringW
#define GetProfileSection GetProfileSectionW
#define WriteProfileSection WriteProfileSectionW
#define GetPrivateProfileInt GetPrivateProfileIntW
#define GetPrivateProfileString GetPrivateProfileStringW
#define WritePrivateProfileString WritePrivateProfileStringW
#define GetPrivateProfileSection GetPrivateProfileSectionW
#define WritePrivateProfileSection WritePrivateProfileSectionW
#define GetPrivateProfileSectionNames GetPrivateProfileSectionNamesW
#define GetPrivateProfileStruct GetPrivateProfileStructW
#define WritePrivateProfileStruct WritePrivateProfileStructW
#define GetDriveType GetDriveTypeW
#define GetSystemDirectory GetSystemDirectoryW
#define GetTempPath GetTempPathW
#define GetTempFileName GetTempFileNameW
#define GetWindowsDirectory GetWindowsDirectoryW
#define GetSystemWindowsDirectory GetSystemWindowsDirectoryW
#define SetCurrentDirectory SetCurrentDirectoryW
#define GetCurrentDirectory GetCurrentDirectoryW
#define GetDiskFreeSpace GetDiskFreeSpaceW
#define GetDiskFreeSpaceEx GetDiskFreeSpaceExW
#define CreateDirectory CreateDirectoryW
#define CreateDirectoryEx CreateDirectoryExW
#define RemoveDirectory RemoveDirectoryW
#define GetFullPathName GetFullPathNameW
#define DefineDosDevice DefineDosDeviceW
#define QueryDosDevice QueryDosDeviceW
#define CreateFile CreateFileW
#define SetFileAttributes SetFileAttributesW
#define GetFileAttributes GetFileAttributesW
#define GetFileAttributesEx GetFileAttributesExW
#define GetCompressedFileSize GetCompressedFileSizeW
#define DeleteFile DeleteFileW
#define FindFirstFileEx FindFirstFileExW
#define FindFirstFile FindFirstFileW
#define FindNextFile FindNextFileW
#define SearchPath SearchPathW
#define CopyFile CopyFileW
#define CopyFileEx CopyFileExW
#define MoveFile MoveFileW
#define MoveFileEx MoveFileExW
#define MoveFileWithProgress MoveFileWithProgressW
#define ReplaceFile ReplaceFileW
#define CreateHardLink CreateHardLinkW
#define CreateNamedPipe CreateNamedPipeW
#define GetNamedPipeHandleState GetNamedPipeHandleStateW
#define CallNamedPipe CallNamedPipeW
#define WaitNamedPipe WaitNamedPipeW
#define SetVolumeLabel SetVolumeLabelW
#define GetVolumeInformation GetVolumeInformationW
#define ClearEventLog ClearEventLogW
#define BackupEventLog BackupEventLogW
#define OpenEventLog OpenEventLogW
#define RegisterEventSource RegisterEventSourceW
#define OpenBackupEventLog OpenBackupEventLogW
#define ReadEventLog ReadEventLogW
#define ReportEvent ReportEventW
#define AccessCheckAndAuditAlarm AccessCheckAndAuditAlarmW
#define AccessCheckByTypeAndAuditAlarm AccessCheckByTypeAndAuditAlarmW
#define AccessCheckByTypeResultListAndAuditAlarm AccessCheckByTypeResultListAndAuditAlarmW
#define AccessCheckByTypeResultListAndAuditAlarmByHandle AccessCheckByTypeResultListAndAuditAlarmByHandleW
#define ObjectOpenAuditAlarm ObjectOpenAuditAlarmW
#define ObjectPrivilegeAuditAlarm ObjectPrivilegeAuditAlarmW
#define ObjectCloseAuditAlarm ObjectCloseAuditAlarmW
#define ObjectDeleteAuditAlarm ObjectDeleteAuditAlarmW
#define PrivilegedServiceAuditAlarm PrivilegedServiceAuditAlarmW
#define SetFileSecurity SetFileSecurityW
#define GetFileSecurity GetFileSecurityW
#define FindFirstChangeNotification FindFirstChangeNotificationW
#define IsBadStringPtr IsBadStringPtrW
#define LookupAccountSid LookupAccountSidW
#define LookupAccountName LookupAccountNameW
#define LookupPrivilegeValue LookupPrivilegeValueW
#define LookupPrivilegeName LookupPrivilegeNameW
#define LookupPrivilegeDisplayName LookupPrivilegeDisplayNameW
#define BuildCommDCB BuildCommDCBW
#define BuildCommDCBAndTimeouts BuildCommDCBAndTimeoutsW
#define CommConfigDialog CommConfigDialogW
#define GetDefaultCommConfig GetDefaultCommConfigW
#define SetDefaultCommConfig SetDefaultCommConfigW
#define GetComputerName GetComputerNameW
#define SetComputerName SetComputerNameW
#define GetComputerNameEx GetComputerNameExW
#define SetComputerNameEx SetComputerNameExW
#define DnsHostnameToComputerName DnsHostnameToComputerNameW
#define GetUserName GetUserNameW
#define LogonUser LogonUserW
#define CreateProcessAsUser CreateProcessAsUserW
#define GetSiteDirectory GetSiteDirectoryW
#define GetCurrentHwProfile GetCurrentHwProfileW
#define GetVersionEx GetVersionExW
#define VerifyVersionInfo VerifyVersionInfoW
#define CreateJobObject CreateJobObjectW
#define OpenJobObject OpenJobObjectW
#define FindFirstVolume FindFirstVolumeW
#define FindNextVolume FindNextVolumeW
#define FindFirstVolumeMountPoint FindFirstVolumeMountPointW
#define FindNextVolumeMountPoint FindNextVolumeMountPointW
#define SetVolumeMountPoint SetVolumeMountPointW
#define DeleteVolumeMountPoint DeleteVolumeMountPointW
#define GetVolumeNameForVolumeMountPoint GetVolumeNameForVolumeMountPointW
#define GetVolumePathName GetVolumePathNameW
#else /* UNICODE */
typedef STARTUPINFOA STARTUPINFO;
typedef LPSTARTUPINFOA LPSTARTUPINFO;
typedef WIN32_FIND_DATAA WIN32_FIND_DATA;
typedef PWIN32_FIND_DATAA PWIN32_FIND_DATA;
typedef LPWIN32_FIND_DATAA LPWIN32_FIND_DATA;
#if (_WIN32_WINNT >= 0x0400)
typedef HW_PROFILE_INFOA HW_PROFILE_INFO;
typedef LPHW_PROFILE_INFOA LPHW_PROFILE_INFO;
#endif /* _WIN32_WINNT >= 0x0400 */
#ifdef STRICT
#define ENUMRESTYPEPROC ENUMRESTYPEPROCA
#define ENUMRESNAMEPROC ENUMRESNAMEPROCA
#define ENUMRESLANGPROC ENUMRESLANGPROCA
#else
typedef ENUMRESTYPEPROCA ENUMRESTYPEPROC;
typedef ENUMRESNAMEPROCA ENUMRESNAMEPROC;
typedef ENUMRESLANGPROCA ENUMRESLANGPROC;
#endif /* STRICT */
#if (_WIN32_WINNT >= 0x0501)
#define GetSystemWow64Directory  GetSystemWow64DirectoryA
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_A GET_SYSTEM_WOW64_DIRECTORY_NAME_A_A
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_W GET_SYSTEM_WOW64_DIRECTORY_NAME_A_W
#define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_T GET_SYSTEM_WOW64_DIRECTORY_NAME_A_T
#endif /* _WIN32_WINNT >= 0x0501 */
#define GetBinaryType GetBinaryTypeA
#define GetShortPathName GetShortPathNameA
#define GetLongPathName GetLongPathNameA
#define GetEnvironmentStringsA GetEnvironmentStrings
#define FreeEnvironmentStrings FreeEnvironmentStringsA
#define FormatMessage FormatMessageA
#define CreateMailslot CreateMailslotA
#define EncryptFile EncryptFileA
#define DecryptFile DecryptFileA
#define FileEncryptionStatus FileEncryptionStatusA
#define OpenEncryptedFileRaw OpenEncryptedFileRawA
#define lstrcmp lstrcmpA
#define lstrcmpi lstrcmpiA
#define lstrcpyn lstrcpynA
#define lstrcpy lstrcpyA
#define lstrcat lstrcatA
#define lstrlen lstrlenA
#define CreateMutex CreateMutexA
#define OpenMutex OpenMutexA
#define CreateEvent CreateEventA
#define OpenEvent OpenEventA
#define CreateSemaphore CreateSemaphoreA
#define OpenSemaphore OpenSemaphoreA
#define CreateWaitableTimer CreateWaitableTimerA
#define OpenWaitableTimer OpenWaitableTimerA
#define CreateFileMapping CreateFileMappingA
#define OpenFileMapping OpenFileMappingA
#define GetLogicalDriveStrings GetLogicalDriveStringsA
#define LoadLibrary LoadLibraryA
#define LoadLibraryEx LoadLibraryExA
#define GetModuleFileName GetModuleFileNameA
#define GetModuleHandle GetModuleHandleA
#define CreateProcess CreateProcessA
#define FatalAppExit FatalAppExitA
#define GetStartupInfo GetStartupInfoA
#define GetCommandLine GetCommandLineA
#define GetEnvironmentVariable GetEnvironmentVariableA
#define SetEnvironmentVariable SetEnvironmentVariableA
#define ExpandEnvironmentStrings ExpandEnvironmentStringsA
#define OutputDebugString OutputDebugStringA
#define FindResource FindResourceA
#define FindResourceEx FindResourceExA
#define EnumResourceTypes EnumResourceTypesA
#define EnumResourceNames EnumResourceNamesA
#define EnumResourceLanguages EnumResourceLanguagesA
#define BeginUpdateResource BeginUpdateResourceA
#define UpdateResource UpdateResourceA
#define EndUpdateResource EndUpdateResourceA
#define GlobalAddAtom GlobalAddAtomA
#define GlobalFindAtom GlobalFindAtomA
#define GlobalGetAtomName GlobalGetAtomNameA
#define AddAtom AddAtomA
#define FindAtom FindAtomA
#define GetAtomName GetAtomNameA
#define GetProfileInt GetProfileIntA
#define GetProfileString GetProfileStringA
#define WriteProfileString WriteProfileStringA
#define GetProfileSection GetProfileSectionA
#define WriteProfileSection WriteProfileSectionA
#define GetPrivateProfileInt GetPrivateProfileIntA
#define GetPrivateProfileString GetPrivateProfileStringA
#define WritePrivateProfileString WritePrivateProfileStringA
#define GetPrivateProfileSection GetPrivateProfileSectionA
#define WritePrivateProfileSection WritePrivateProfileSectionA
#define GetPrivateProfileSectionNames GetPrivateProfileSectionNamesA
#define GetPrivateProfileStruct GetPrivateProfileStructA
#define WritePrivateProfileStruct WritePrivateProfileStructA
#define GetDriveType GetDriveTypeA
#define GetSystemDirectory GetSystemDirectoryA
#define GetTempPath GetTempPathA
#define GetTempFileName GetTempFileNameA
#define GetWindowsDirectory GetWindowsDirectoryA
#define GetSystemWindowsDirectory GetSystemWindowsDirectoryA
#define SetCurrentDirectory SetCurrentDirectoryA
#define GetCurrentDirectory GetCurrentDirectoryA
#define GetDiskFreeSpace GetDiskFreeSpaceA
#define GetDiskFreeSpaceEx GetDiskFreeSpaceExA
#define CreateDirectory CreateDirectoryA
#define CreateDirectoryEx CreateDirectoryExA
#define RemoveDirectory RemoveDirectoryA
#define GetFullPathName GetFullPathNameA
#define DefineDosDevice DefineDosDeviceA
#define QueryDosDevice QueryDosDeviceA
#define CreateFile CreateFileA
#define SetFileAttributes SetFileAttributesA
#define GetFileAttributes GetFileAttributesA
#define GetFileAttributesEx GetFileAttributesExA
#define GetCompressedFileSize GetCompressedFileSizeA
#define DeleteFile DeleteFileA
#define FindFirstFileEx FindFirstFileExA
#define FindFirstFile FindFirstFileA
#define FindNextFile FindNextFileA
#define SearchPath SearchPathA
#define CopyFile CopyFileA
#define CopyFileEx CopyFileExA
#define MoveFile MoveFileA
#define MoveFileEx MoveFileExA
#define MoveFileWithProgress MoveFileWithProgressA
#define ReplaceFile ReplaceFileA
#define CreateHardLink CreateHardLinkA
#define CreateNamedPipe CreateNamedPipeA
#define GetNamedPipeHandleState GetNamedPipeHandleStateA
#define CallNamedPipe CallNamedPipeA
#define WaitNamedPipe WaitNamedPipeA
#define SetVolumeLabel SetVolumeLabelA
#define GetVolumeInformation GetVolumeInformationA
#define ClearEventLog ClearEventLogA
#define BackupEventLog BackupEventLogA
#define OpenEventLog OpenEventLogA
#define RegisterEventSource RegisterEventSourceA
#define OpenBackupEventLog OpenBackupEventLogA
#define ReadEventLog ReadEventLogA
#define ReportEvent ReportEventA
#define AccessCheckAndAuditAlarm AccessCheckAndAuditAlarmA
#define AccessCheckByTypeAndAuditAlarm AccessCheckByTypeAndAuditAlarmA
#define AccessCheckByTypeResultListAndAuditAlarm AccessCheckByTypeResultListAndAuditAlarmA
#define AccessCheckByTypeResultListAndAuditAlarmByHandle AccessCheckByTypeResultListAndAuditAlarmByHandleA
#define ObjectOpenAuditAlarm ObjectOpenAuditAlarmA
#define ObjectPrivilegeAuditAlarm ObjectPrivilegeAuditAlarmA
#define ObjectCloseAuditAlarm ObjectCloseAuditAlarmA
#define ObjectDeleteAuditAlarm ObjectDeleteAuditAlarmA
#define PrivilegedServiceAuditAlarm PrivilegedServiceAuditAlarmA
#define SetFileSecurity SetFileSecurityA
#define GetFileSecurity GetFileSecurityA
#define FindFirstChangeNotification FindFirstChangeNotificationA
#define IsBadStringPtr IsBadStringPtrA
#define LookupAccountSid LookupAccountSidA
#define LookupAccountName LookupAccountNameA
#define LookupPrivilegeValue LookupPrivilegeValueA
#define LookupPrivilegeName LookupPrivilegeNameA
#define LookupPrivilegeDisplayName LookupPrivilegeDisplayNameA
#define BuildCommDCB BuildCommDCBA
#define BuildCommDCBAndTimeouts BuildCommDCBAndTimeoutsA
#define CommConfigDialog CommConfigDialogA
#define GetDefaultCommConfig GetDefaultCommConfigA
#define SetDefaultCommConfig SetDefaultCommConfigA
#define GetComputerName GetComputerNameA
#define SetComputerName SetComputerNameA
#define GetComputerNameEx GetComputerNameExA
#define SetComputerNameEx SetComputerNameExA
#define DnsHostnameToComputerName DnsHostnameToComputerNameA
#define GetUserName GetUserNameA
#define LogonUser LogonUserA
#define CreateProcessAsUser CreateProcessAsUserA
#define GetSiteDirectory GetSiteDirectoryA
#define GetCurrentHwProfile GetCurrentHwProfileA
#define GetVersionEx GetVersionExA
#define VerifyVersionInfo VerifyVersionInfoA
#define CreateJobObject CreateJobObjectA
#define OpenJobObject OpenJobObjectA
#define FindFirstVolume FindFirstVolumeA
#define FindNextVolume FindNextVolumeA
#define FindFirstVolumeMountPoint FindFirstVolumeMountPointA
#define FindNextVolumeMountPoint FindNextVolumeMountPointA
#define SetVolumeMountPoint SetVolumeMountPointA
#define DeleteVolumeMountPoint DeleteVolumeMountPointA
#define GetVolumeNameForVolumeMountPoint GetVolumeNameForVolumeMountPointA
#define GetVolumePathName GetVolumePathNameA
#endif /* UNICODE */

#define HasOverlappedIoCompleted(lpOverlapped)  ((lpOverlapped)->Internal != STATUS_PENDING)

#ifdef _WINCE
#include <dbgapi.h>
#include <kfuncs.h>
#endif

#include <winerror.h>

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
}
#endif

#endif /* _WINBASE_H */
