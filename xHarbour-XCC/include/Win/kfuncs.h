#ifndef _KFUNCS_H
#define _KFUNCS_H

/* Kernel functions (Windows CE) */

#define NUM_SYS_HANDLES  32

#define SYS_HANDLE_BASE  64
#define SH_WIN32  0
#define SH_CURTHREAD  1
#define SH_CURPROC  2

#define SH_LAST_NOTIFY  16
#define SH_GDI  16
#define SH_WMGR  17
#define SH_WNET  18
#define SH_COMM  19
#define SH_FILESYS_APIS  20
#define SH_SHELL  21
#define SH_DEVMGR_APIS  22
#define SH_TAPI  23
#define SH_PATCHER  24
#define SH_LASTRESERVED  24

#if defined(ARM)
#define PUserKData  ((LPBYTE)0xFFFFC800)
#else
#define PUserKData  ((LPBYTE)0x00005800)
#endif

#define SYSHANDLE_OFFSET  0x004

#define EVENT_PULSE  1
#define EVENT_RESET  2
#define EVENT_SET  3

#define TLS_FUNCALLOC  0
#define TLS_FUNCFREE  1

#define VERIFY_READ_FLAG  0
#define VERIFY_EXECUTE_FLAG  0
#define VERIFY_WRITE_FLAG  1
#define VERIFY_KERNEL_OK  2

#if defined(_M_ARM)
#elif defined (_M_IX86)
inline void DebugBreak(void) { __asm int 3 }
#else
extern void DebugBreak(void);
#endif

#ifndef EventModify
BOOL WINAPI EventModify(HANDLE,DWORD);
#endif

#ifndef TlsCall
DWORD WINAPI TlsCall(DWORD, DWORD);
#endif

#ifndef CeGetCurrentTrust
DWORD CeGetCurrentTrust(void);
#endif

#ifndef CeGetCallerTrust
DWORD CeGetCallerTrust(void);
#endif

inline BOOL PulseEvent(HANDLE h) { return EventModify(h,EVENT_PULSE); }
inline BOOL ResetEvent(HANDLE h) { return EventModify(h,EVENT_RESET); }
inline BOOL SetEvent(HANDLE h) { return EventModify(h,EVENT_SET); }

inline HANDLE GetCurrentThread(void) { return ((HANDLE)(SH_CURTHREAD+SYS_HANDLE_BASE)); }
inline HANDLE GetCurrentProcess(void) { return ((HANDLE)(SH_CURPROC+SYS_HANDLE_BASE)); }
inline DWORD GetCurrentThreadId(void) { return ((DWORD)(((HANDLE *)(PUserKData+SYSHANDLE_OFFSET))[SH_CURTHREAD])); }
inline DWORD GetCurrentProcessId(void) { return ((DWORD)(((HANDLE *)(PUserKData+SYSHANDLE_OFFSET))[SH_CURPROC])); }

inline DWORD WINAPI TlsAlloc(void) { return TlsCall(TLS_FUNCALLOC,0); }
inline BOOL WINAPI TlsFree(DWORD dwTlsIndex) { return TlsCall(TLS_FUNCFREE,dwTlsIndex); }

inline LPVOID LockResource(HGLOBAL hResData) {  return (LPVOID)hResData; }

#endif /* _KFUNCS_H */
