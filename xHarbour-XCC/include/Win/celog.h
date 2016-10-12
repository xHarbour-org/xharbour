#ifndef _CELOG_H
#define _CELOG_H

/* CELog event logging definitions (Windows CE) */

#ifdef __cplusplus
extern "C" {
#endif

#define CELOG_VERSION  1

typedef struct  __CEL_HEADER {
    DWORD Length:16;
    DWORD ID:14;
    DWORD Reserved:1;
    DWORD fTimeStamp:1;
} CEL_HEADER, *PCEL_HEADER;

#define CEL_HEADER_TIMESTAMP  0x80000000
#define CEL_HEADER_LENGTH_MASK  0x0000FFFF
#define CEL_HEADER_ID_MASK  0x3FFF0000

#define CELID_CS_ENTER  1
#define CELID_CS_LEAVE  2
#define CELID_EVENT_CREATE  3
#define CELID_EVENT_SET  4
#define CELID_EVENT_RESET  5
#define CELID_EVENT_PULSE  6
#define CELID_EVENT_CLOSE  7
#define CELID_EVENT_DELETE  8
#define CELID_WAIT_MULTI  9
#define CELID_SLEEP  10
#define CELID_SEM_CREATE  15
#define CELID_SEM_RELEASE  16
#define CELID_SEM_CLOSE  17
#define CELID_SEM_DELETE  18
#define CELID_HEAP_CREATE  25
#define CELID_HEAP_ALLOC  26
#define CELID_HEAP_REALLOC  27
#define CELID_HEAP_FREE  28
#define CELID_HEAP_DESTROY  29
#define CELID_VIRTUAL_ALLOC  35
#define CELID_VIRTUAL_COPY  36
#define CELID_VIRTUAL_FREE  37
#define CELID_THREAD_SWITCH  45
#define CELID_THREAD_MIGRATE  46
#define CELID_THREAD_CREATE  47
#define CELID_THREAD_CLOSE  48
#define CELID_THREAD_TERMINATE  49
#define CELID_THREAD_DELETE  50
#define CELID_PROCESS_CREATE  51
#define CELID_PROCESS_CLOSE  52
#define CELID_PROCESS_TERMINATE  53
#define CELID_PROCESS_DELETE  54
#define CELID_MUTEX_CREATE  60
#define CELID_MUTEX_RELEASE  61
#define CELID_MUTEX_CLOSE  62
#define CELID_MUTEX_DELETE  63
#define CELID_RAW_LONG  70
#define CELID_RAW_ULONG  71
#define CELID_RAW_SHORT  72
#define CELID_RAW_USHORT  73
#define CELID_RAW_WCHAR  74
#define CELID_RAW_CHAR  75
#define CELID_RAW_UCHAR  76
#define CELID_RAW_FLOAT  77
#define CELID_RAW_DOUBLE  78
#define CELID_SYSTEM_TLB  80
#define CELID_SYSTEM_PAGE  81
#define CELID_SYSTEM_INVERT  82
#define CELID_THREAD_PRIORITY  83
#define CELID_THREAD_QUANTUM  84
#define CELID_MODULE_LOAD  85
#define CELID_MODULE_FREE  86
#define CELID_INTERRUPTS  87
#define CELID_KCALL_ENTER  88
#define CELID_KCALL_LEAVE  89
#define CELID_FLAGGED  90
#define CELID_MEMTRACK_ALLOC  91
#define CELID_MEMTRACK_REALLOC  92
#define CELID_MEMTRACK_FREE  93
#define CELID_MEMTRACK_HEAPCREATE  94
#define CELID_MEMTRACK_CREATEPROC  95
#define CELID_MEMTRACK_LOADMODULE  96
#define CELID_MEMTRACK_ATTACHP  97
#define CELID_MEMTRACK_ATTACHM  98
#define CELID_MEMTRACK_DETACHP  99
#define CELID_MEMTRACK_HEAPDESTROY  100
#define CELID_MEMTRACK_FREEMODULE  101
#define CELID_MEMTRACK_BASELINE  102
#define CELID_BOOT_TIME  103
#define CELID_GDI  104
#define CELID_RDP  105
#define CELID_PROFILER_START  106
#define CELID_PROFILER_STOP  107
#define CELID_MONTECARLO_HIT  108
#define CELID_OEMPROFILER_HIT  109
#define CELID_DATA_LOSS  150
#define CELID_SYNC_END  0x1FFE
#define CELID_LOG_MARKER  0x1FFF
#define CELID_USER  0x2000
#define CELID_MAX  0x3FFF

#define CELZONE_INTERRUPT  0x00000001
#define CELZONE_RESCHEDULE  0x00000002
#define CELZONE_IDLE  0x00000004
#define CELZONE_TLB  0x00000008
#define CELZONE_DEMANDPAGE  0x00000010
#define CELZONE_THREAD  0x00000020
#define CELZONE_PROCESS  0x00000040
#define CELZONE_PRIORITYINV  0x00000080
#define CELZONE_CRITSECT  0x00000100
#define CELZONE_SYNCH  0x00000200
#define CELZONE_WAITOBJ  0x00000400
#define CELZONE_HEAP  0x00000800
#define CELZONE_VIRTMEM  0x00001000
#define CELZONE_CACHE  0x00002000
#define CELZONE_LOADER  0x00004000
#define CELZONE_MEMTRACKING  0x00008000
#define CELZONE_WINDOW  0x00100000
#define CELZONE_MESSAGE  0x00200000
#define CELZONE_KCALL  0x00400000
#define CELZONE_ALWAYSON  0x40000000
#define CELZONE_MISC  0x80000000

#define BOOT_TIME_LAUNCHING_FS  01
#define BOOT_TIME_FS_INITED  10
#define BOOT_TIME_FS_OBJ_STORE_INITIALIZED  11
#define BOOT_TIME_FS_FILES_INITIALIZED  12
#define BOOT_TIME_FS_REG_INITIALIZED  13
#define BOOT_TIME_FS_DB_INITIALIZED  14
#define BOOT_TIME_FS_LAUNCH  15
#define BOOT_TIME_DEV_ACTIVATE  20
#define BOOT_TIME_DEV_FINISHED  21
#define BOOT_TIME_GWES_FINISHED  30
#define BOOT_TIME_SYSTEM_STARTED  40

#define CEL_GDI_AddFontResource  0
#define CEL_GDI_PatBlt  1
#define CEL_GDI_BitBlt  2
#define CEL_GDI_CombineRgn  3
#define CEL_GDI_CreateBitmap  4
#define CEL_GDI_CreateCompatibleBitmap  5
#define CEL_GDI_CreateCompatibleDC  6
#define CEL_GDI_CreateDIBPatternBrushPt  7
#define CEL_GDI_CreateFontIndirectW  8
#define CEL_GDI_CreateRectRgnIndirect  9
#define CEL_GDI_CreatePenIndirect  10
#define CEL_GDI_CreateSolidBrush  11
#define CEL_GDI_DeleteDC  12
#define CEL_GDI_DeleteObject  13
#define CEL_GDI_DrawEdge  14
#define CEL_GDI_DrawFocusRect  15
#define CEL_GDI_DrawTextW  16
#define CEL_GDI_Ellipse  17
#define CEL_GDI_EnumFontFamiliesW  18
#define CEL_GDI_EnumFontsW  19
#define CEL_GDI_ExcludeClipRect  20
#define CEL_GDI_ExtTextOutW  21
#define CEL_GDI_SetTextAlign  22
#define CEL_GDI_GetTextAlign  23
#define CEL_GDI_FillRect  24
#define CEL_GDI_GetBkColor  25
#define CEL_GDI_GetBkMode  26
#define CEL_GDI_GetClipRgn  27
#define CEL_GDI_GetClipBox  28
#define CEL_GDI_GetCurrentObject  29
#define CEL_GDI_GetDeviceCaps 30
#define CEL_GDI_GetNearestColor  31
#define CEL_GDI_GetObjectW  32
#define CEL_GDI_GetObjectType  33
#define CEL_GDI_GetPixel  34
#define CEL_GDI_GetRegionData  35
#define CEL_GDI_GetSysColorBrush  36
#define CEL_GDI_GetRgnBox  37
#define CEL_GDI_GetStockObject  38
#define CEL_GDI_GetTextColor  39
#define CEL_GDI_GetTextExtentExPointW  40
#define CEL_GDI_GetTextFaceW  41
#define CEL_GDI_GetTextMetricsW  42
#define CEL_GDI_GetCharWidth32  43
#define CEL_GDI_IntersectClipRect  44
#define CEL_GDI_MaskBlt  45
#define CEL_GDI_OffsetRgn  46
#define CEL_GDI_MoveToEx  47
#define CEL_GDI_LineTo  48
#define CEL_GDI_GetCurrentPositionEx  49
#define CEL_GDI_Polygon  50
#define CEL_GDI_Polyline  51
#define CEL_GDI_PtInRegion  52
#define CEL_GDI_Rectangle  53
#define CEL_GDI_RectInRegion  54
#define CEL_GDI_RemoveFontResourceW  55
#define CEL_GDI_RestoreDC  56
#define CEL_GDI_RoundRect  57
#define CEL_GDI_SaveDC  58
#define CEL_GDI_SelectClipRgn  59
#define CEL_GDI_SelectObject  60
#define CEL_GDI_SetBkColor  61
#define CEL_GDI_SetBkMode  62
#define CEL_GDI_SetBrushOrgEx  63
#define CEL_GDI_SetPixel  64
#define CEL_GDI_SetTextColor  65
#define CEL_GDI_StretchBlt  66
#define CEL_GDI_StretchDIBits  67
#define CEL_GDI_CloseEnhMetaFile  68
#define CEL_GDI_CreateEnhMetaFileW  69
#define CEL_GDI_DeleteEnhMetaFile  70
#define CEL_GDI_PlayEnhMetaFile  71
#define CEL_GDI_CreatePalette  72
#define CEL_GDI_SelectPalette  73
#define CEL_GDI_RealizePalette  74
#define CEL_GDI_GetPaletteEntries  75
#define CEL_GDI_SetPaletteEntries  76
#define CEL_GDI_GetSystemPaletteEntries  77
#define CEL_GDI_GetNearestPaletteIndex  78
#define CEL_GDI_GetDIBColorTable  79
#define CEL_GDI_SetDIBColorTable  80
#define CEL_GDI_CreatePen  81
#define CEL_GDI_StartDocW  82
#define CEL_GDI_EndDoc  83
#define CEL_GDI_StartPage  84
#define CEL_GDI_EndPage  85
#define CEL_GDI_AbortDoc  86
#define CEL_GDI_SetAbortProc  87
#define CEL_GDI_CreateDCW  88
#define CEL_GDI_CreateRectRgn  89
#define CEL_GDI_ExtCreateRegion  90
#define CEL_GDI_FillRgn  91
#define CEL_GDI_SetROP2  92
#define CEL_GDI_RectVisible  93
#define CEL_GDI_SetRectRgn  94
#define CEL_GDI_CreatePatternBrush  95
#define CEL_GDI_CreateBitmapFromPointer  96
#define CEL_GDI_SetViewportOrgEx  97
#define CEL_GDI_TransparentImage  98
#define CEL_GDI_TranslateCharsetInfo  99
#define CEL_GDI_ExtEscape  100
#define CEL_GDI_SetDIBitsToDevice  101
#define CEL_GDI_GradientFill  102
#define CEL_GDI_InvertRect  103
#define CEL_GDI_GetCharABCWidths  104

typedef struct __CEL_CRITSEC_ENTER {
    HANDLE hCS;
    HANDLE hOwnerThread;
} CEL_CRITSEC_ENTER, *PCEL_CRITSEC_ENTER;

typedef struct __CEL_CRITSEC_LEAVE {
    HANDLE  hCS;
    HANDLE  hOwnerThread;
} CEL_CRITSEC_LEAVE, *PCEL_CRITSEC_LEAVE;

typedef struct __CEL_EVENT_CREATE {
    HANDLE hEvent;
    DWORD fManual:1;
    DWORD fInitialState:1;
    DWORD dwReserved:30;
    WCHAR szName[0];
} CEL_EVENT_CREATE, *PCEL_EVENT_CREATE;

typedef struct __CEL_EVENT_SET {
    HANDLE hEvent;
} CEL_EVENT_SET, *PCEL_EVENT_SET;

typedef struct __CEL_EVENT_RESET {
    HANDLE hEvent;
} CEL_EVENT_RESET, *PCEL_EVENT_RESET;

typedef struct __CEL_EVENT_PULSE {
    HANDLE hEvent;
} CEL_EVENT_PULSE, *PCEL_EVENT_PULSE;

typedef struct __CEL_EVENT_CLOSE {
    HANDLE hEvent;
} CEL_EVENT_CLOSE, *PCEL_EVENT_CLOSE;

typedef struct __CEL_EVENT_DELETE {
    HANDLE hEvent;
} CEL_EVENT_DELETE, *PCEL_EVENT_DELETE;

typedef struct __CEL_WAIT_MULTI {
    DWORD dwTimeout;
    DWORD fWaitAll:1;
    DWORD dwReserved:31;
    HANDLE hHandles[0];
} CEL_WAIT_MULTI, *PCEL_WAIT_MULTI;

typedef struct __CEL_SLEEP {
    DWORD dwTimeout;
} CEL_SLEEP, *PCEL_SLEEP;

typedef struct __CEL_SEM_CREATE {
    HANDLE hSem;
    DWORD dwInitCount;
    DWORD dwMaxCount;
    WCHAR szName[0];
} CEL_SEM_CREATE, *PCEL_SEM_CREATE;

typedef struct __CEL_SEM_RELEASE {
    HANDLE hSem;
    DWORD dwReleaseCount;
    DWORD dwPreviousCount;
} CEL_SEM_RELEASE, *PCEL_SEM_RELEASE;

typedef struct __CEL_SEM_CLOSE {
    HANDLE hSem;
} CEL_SEM_CLOSE, *PCEL_SEM_CLOSE;

typedef struct __CEL_SEM_DELETE {
    HANDLE hSem;
} CEL_SEM_DELETE, *PCEL_SEM_DELETE;

typedef struct __CEL_HEAP_CREATE {
    DWORD dwOptions;
    DWORD dwInitSize;
    DWORD dwMaxSize;
    HANDLE hHeap;
} CEL_HEAP_CREATE, *PCEL_HEAP_CREATE;

typedef struct __CEL_HEAP_ALLOC {
    HANDLE hHeap;
    DWORD dwFlags;
    DWORD dwBytes;
    DWORD lpMem;
    BYTE bReserved[0];
} CEL_HEAP_ALLOC, *PCEL_HEAP_ALLOC;

typedef struct __CEL_HEAP_REALLOC {
    HANDLE hHeap;
    DWORD dwFlags;
    DWORD dwBytes;
    DWORD lpMemOld;
    DWORD lpMem;
} CEL_HEAP_REALLOC, *PCEL_HEAP_REALLOC;

typedef struct __CEL_HEAP_FREE {
    HANDLE hHeap;
    DWORD dwFlags;
    DWORD lpMem;
    BYTE bReserved[0];
} CEL_HEAP_FREE, *PCEL_HEAP_FREE;

typedef struct __CEL_HEAP_DESTROY {
    HANDLE hHeap;
} CEL_HEAP_DESTROY, *PCEL_HEAP_DESTROY;

typedef struct __CEL_VIRTUAL_ALLOC {
    DWORD dwResult;
    DWORD dwAddress;
    DWORD dwSize;
    DWORD dwType;
    DWORD dwProtect;
    BYTE bReserved[0];
} CEL_VIRTUAL_ALLOC, *PCEL_VIRTUAL_ALLOC;

typedef struct __CEL_VIRTUAL_COPY {
    DWORD dwDest;
    DWORD dwSource;
    DWORD dwSize;
    DWORD dwProtect;
} CEL_VIRTUAL_COPY, *PCEL_VIRTUAL_COPY;

typedef struct __CEL_VIRTUAL_FREE {
    DWORD dwAddress;
    DWORD dwSize;
    DWORD dwType;
    BYTE bReserved[0];
} CEL_VIRTUAL_FREE, *PCEL_VIRTUAL_FREE;

typedef struct __CEL_THREAD_SWITCH {
    HANDLE hThread;
} CEL_THREAD_SWITCH, *PCEL_THREAD_SWITCH;

typedef struct __CEL_THREAD_MIGRATE {
    HANDLE hProcess;
    BOOL bProcessEntry;
} CEL_THREAD_MIGRATE, *PCEL_THREAD_MIGRATE;

typedef struct __CEL_THREAD_CREATE {
    HANDLE hThread;
    HANDLE hProcess;
    HANDLE hModule;
    WCHAR szName[0];
} CEL_THREAD_CREATE, *PCEL_THREAD_CREATE;

typedef struct __CEL_THREAD_CLOSE {
    HANDLE hThread;
} CEL_THREAD_CLOSE, *PCEL_THREAD_CLOSE;

typedef struct __CEL_THREAD_TERMINATE {
    HANDLE hThread;
} CEL_THREAD_TERMINATE, *PCEL_THREAD_TERMINATE;

typedef struct __CEL_THREAD_DELETE {
    HANDLE hThread;
} CEL_THREAD_DELETE, *PCEL_THREAD_DELETE;

typedef struct __CEL_PROCESS_CREATE {
    HANDLE hProcess;
    WCHAR szName[0];
} CEL_PROCESS_CREATE, *PCEL_PROCESS_CREATE;

typedef struct __CEL_PROCESS_CLOSE {
    HANDLE hProcess;
} CEL_PROCESS_CLOSE, *PCEL_PROCESS_CLOSE;

typedef struct __CEL_PROCESS_TERMINATE {
    HANDLE hProcess;
} CEL_PROCESS_TERMINATE, *PCEL_PROCESS_TERMINATE;

typedef struct __CEL_PROCESS_DELETE {
    HANDLE hProcess;
} CEL_PROCESS_DELETE, *PCEL_PROCESS_DELETE;

typedef struct __CEL_MUTEX_CREATE {
    HANDLE hMutex;
    WCHAR szName[0];
} CEL_MUTEX_CREATE, *PCEL_MUTEX_CREATE;

typedef struct __CEL_MUTEX_RELEASE {
    HANDLE hMutex;
} CEL_MUTEX_RELEASE, *PCEL_MUTEX_RELEASE;

typedef struct __CEL_MUTEX_CLOSE {
    HANDLE hMutex;
} CEL_MUTEX_CLOSE, *PCEL_MUTEX_CLOSE;

typedef struct __CEL_MUTEX_DELETE {
    HANDLE hMutex;
} CEL_MUTEX_DELETE, *PCEL_MUTEX_DELETE;

typedef struct __CEL_SYSTEM_TLB {
    DWORD dwCount;
} CEL_SYSTEM_TLB, *PCEL_SYSTEM_TLB;

typedef struct __CEL_SYSTEM_PAGE {
    DWORD dwAddress;
    DWORD fReadWrite:1;
    DWORD dwReserved:31;
} CEL_SYSTEM_PAGE, *PCEL_SYSTEM_PAGE;

typedef struct __CEL_SYSTEM_INVERT {
    HANDLE hThread;
    int nPriority;
} CEL_SYSTEM_INVERT, *PCEL_SYSTEM_INVERT;

typedef struct __CEL_THREAD_PRIORITY {
    HANDLE hThread;
    int nPriority;
} CEL_THREAD_PRIORITY, *PCEL_THREAD_PRIORITY;

typedef struct __CEL_THREAD_QUANTUM {
    HANDLE hThread;
    DWORD dwQuantum;
} CEL_THREAD_QUANTUM, *PCEL_THREAD_QUANTUM;

typedef struct __CEL_MODULE_LOAD {
    HANDLE hProcess;
    HANDLE hModule;
    WCHAR szName[0];
} CEL_MODULE_LOAD, *PCEL_MODULE_LOAD;

typedef struct __CEL_MODULE_FREE {
    HANDLE hProcess;
    HANDLE hModule;
} CEL_MODULE_FREE, *PCEL_MODULE_FREE;

typedef struct __CEL_INT_DATA {
    DWORD dwTimeStamp;
    WORD wSysIntr;
    WORD wNestingLevel;
} CEL_INT_DATA, *PCEL_INT_DATA;

typedef struct __CEL_INTERRUPTS {
    DWORD dwDiscarded;
    CEL_INT_DATA IntData[0];
} CEL_INTERRUPTS, *PCEL_INTERRUPTS;

typedef struct __CEL_MemTrackAlloc {
    HANDLE hHeap;
    DWORD dwPID;
    DWORD dwCallerPID;
    DWORD dwTID;
    DWORD dwFlags;
    DWORD dwBytes;
    DWORD lpMem;
    DWORD adwStackTrace[0];
} CEL_MEMTRACK_ALLOC, *PCEL_MEMTRACK_ALLOC;

typedef struct __CEL_MemTrackReAlloc {
    HANDLE hHeap;
    DWORD dwPID;
    DWORD dwCallerPID;
    DWORD dwTID;
    DWORD dwFlags;
    DWORD dwBytes;
    DWORD lpMem;
    DWORD lpMemOld;
    DWORD adwStackTrace[0];
} CEL_MEMTRACK_REALLOC, *PCEL_MEMTRACK_REALLOC;

typedef struct __CEL_MemTrackFree {
    HANDLE hHeap;
    DWORD dwPID;
    DWORD dwCallerPID;
    DWORD dwTID;
    DWORD dwFlags;
    DWORD lpMem;
    DWORD adwStackTrace[0];
} CEL_MEMTRACK_FREE, *PCEL_MEMTRACK_FREE;

typedef struct  __CEL_MemTrackHeapCreate {
    DWORD dwPID;
    DWORD dwTID;
    DWORD dwOptions;
    DWORD dwInitSize;
    DWORD dwMaxSize;
    HANDLE hHeap;
} CEL_MEMTRACK_HEAPCREATE, *PCEL_MEMTRACK_HEAPCREATE;

typedef struct  __CEL_MEMTRACK_CREATEPROC {
    HANDLE hProcess;
    DWORD dwVmBase;
    WCHAR szName[0];
} CEL_MEMTRACK_CREATEPROC, *PCEL_MEMTRACK_CREATEPROC;

typedef struct __CEL_MEMTRACK_LOADMODULE {
    HANDLE hProcess;
    HANDLE hModule;
    DWORD dwBase;
    WCHAR szName[0];
} CEL_MEMTRACK_LOADMODULE, *PCEL_MEMTRACK_LOADMODULE;

typedef struct  __CEL_MEMTRACK_ATTACHP {
    HANDLE hProcess;
    DWORD dwVmBase;
    WCHAR szName[0];
} CEL_MEMTRACK_ATTACHP, *PCEL_MEMTRACK_ATTACHP;

typedef struct __CEL_MEMTRACK_ATTACHM {
    HANDLE hProcess;
    HANDLE hModule;
    DWORD dwBase;
    WCHAR szName[0];
} CEL_MEMTRACK_ATTACHM, *PCEL_MEMTRACK_ATTACHM;

typedef struct __CEL_MEMTRACK_DETACHP {
    HANDLE hProcess;
} CEL_MEMTRACK_DETACHP, *PCEL_MEMTRACK_DETACHP;

typedef struct __CEL_MemTrackHeapDestroy {
    DWORD dwPID;
    DWORD dwTID;
    HANDLE hHeap;
} CEL_MEMTRACK_HEAPDESTROY, *PCEL_MEMTRACK_HEAPDESTROY;

typedef struct __CEL_MEMTRACK_FREEMODULE {
    HANDLE hProcess;
    HANDLE hModule;
} CEL_MEMTRACK_FREEMODULE, *PCEL_MEMTRACK_FREEMODULE;

typedef struct __CEL_MEMTRACK_BASELINE {
    DWORD dwReserved;
} CEL_MEMTRACK_BASELINE, *PCEL_MEMTRACK_BASELINE;

typedef struct _CEL_BOOT_TIME {
    DWORD dwAction;
    WCHAR szName[0];
} CEL_BOOT_TIME, *PCEL_BOOT_TIME;

typedef struct _CEL_GDI_INFO {
    DWORD dwGDIOp;
    DWORD dwTimeSpent;
    DWORD dwContext;
    DWORD dwContext2;
    DWORD dwContext3;
    DWORD dwContext4;
} CEL_GDI_INFO, *PCEL_GDI_INFO;

typedef struct _CEL_RDP_INFO {
    BYTE bOrderType;
    BYTE bOrder;
    DWORD dwTimeSpent;
} CEL_RDP_INFO, *PCEL_RDP_INFO;

typedef ProfilerControl _CEL_PROFILER_START, CEL_PROFILER_START, *PCEL_PROFILER_START;

typedef struct _CEL_MONTECARLO_HIT {
    DWORD dwReturnAddr;
} CEL_MONTECARLO_HIT, *PCEL_MONTECARLO_HIT;

typedef struct _CEL_OEMPROFILER_HIT {
    DWORD dwReturnAddr;
    DWORD dwBufSize;
    BYTE bData[0];
} CEL_OEMPROFILER_HIT, *PCEL_OEMPROFILER_HIT;

typedef struct __CEL_DATA_LOSS {
    DWORD dwBytes;
} CEL_DATA_LOSS, *PCEL_DATA_LOSS;

typedef struct __CEL_LOG_MARKER {
   DWORD dwFrequency;
   DWORD dwDefaultQuantum;
} CEL_LOG_MARKER, *PCEL_LOG_MARKER;

typedef struct __CEL_BUFFER {
    DWORD dwMaskProcess;
    DWORD dwMaskUser;
    DWORD dwMaskCE;
    PDWORD pWrite;
    DWORD dwBytesLeft;
    DWORD dwSize;
    DWORD pBuffer[0];
} CEL_BUFFER, *PCEL_BUFFER;

#ifndef CeLogData
void CeLogData(BOOL,WORD,PVOID,WORD,DWORD,DWORD);
#endif /* CeLogData */

#ifndef CeLogSetZones
void CeLogSetZones(DWORD,DWORD,DWORD);
#endif /* CeLogSetZones */

#ifndef CeLogReSync
BOOL CeLogReSync(void);
#endif /* CeLogReSync */

#ifdef SHIP_BUILD
#define CELOGDATA(Time,ID,Data,Len,Zone1,Zone2)  ((void)0)
#else
#define CELOGDATA(Time,ID,Data,Len,Zone1,Zone2)  CeLogData(Time,ID,Data,Len,Zone1,Zone2) 
#endif /* SHIP_BUILD */

extern DWORD dwCeLogLargeBuf;
extern DWORD dwCeLogSmallBuf;
extern DWORD dwCeLogFlushTimeout;
extern DWORD dwCeLogLargeBufFlush;
extern int nCeLogThreadPrio;

#ifdef __cplusplus
}
#endif

#endif /* _CELOG_H */
