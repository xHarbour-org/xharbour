#ifndef _DBGAPI_H
#define _DBGAPI_H

/* Debug Message and Zone APIs (Windows CE) */

#ifdef __cplusplus
extern "C" {
#endif

void WINAPIV NKDbgPrintfW(LPCWSTR, ...);
void WINAPI WriteDebugLED(WORD,DWORD);

typedef struct _DBGPARAM {
    WCHAR lpszName[32];
    WCHAR rglpszZones[16][32];
    ULONG ulZoneMask;
} DBGPARAM, *LPDBGPARAM;

#define DEBUGZONE(n)  (dpCurSettings.ulZoneMask&(0x00000001<<n))

#ifdef SHIP_BUILD

#define ERRORMSG(cond,printf_exp) ((void)0)
#define RETAILMSG(cond,printf_exp) ((void)0)
#define DEBUGMSG(cond,printf_exp) ((void)0)
#define DEBUGLED(cond,parms) ((void)0)
#define DBGCHK(module,exp) ((void)0)
#define DEBUGCHK(exp) ((void)0)
#define DEBUGREGISTER(hMod) ((void)0)

#else /* SHIP_BUILD */

#ifdef DEBUG
#define DEBUGMSG(cond,printf_exp)  ((void)((cond)?(NKDbgPrintfW printf_exp),1:0))
#define DBGCHK(module,exp)  ((void)((exp)?1:(NKDbgPrintfW(TEXT("%s: DEBUGCHK failed in file %s at line %d \r\n"), \
    (LPWSTR)module, TEXT(__FILE__) ,__LINE__ ),DebugBreak(),0)))
#define DEBUGLED(cond,parms)  ((void)((cond)?(WriteDebugLED parms),1:0))
#define DEBUGCHK(exp)  DBGCHK(dpCurSettings.lpszName, exp)
extern DBGPARAM dpCurSettings;
BOOL RegisterDbgZones(HMODULE,LPDBGPARAM);
#define DEBUGREGISTER(hMod)  RegisterDbgZones(hMod, &dpCurSettings)
#else /* DEBUG */
#define DEBUGMSG(cond,printf_exp)  ((void)0)
#define DEBUGLED(cond,parms)  ((void)0)
#define DBGCHK(module,exp)  ((void)0)
#define DEBUGCHK(exp)  ((void)0)
#define DEBUGREGISTER(hMod)  ((void)0)
#endif /* DEBUG */

#define RETAILMSG(cond,printf_exp)  ((cond)?(NKDbgPrintfW printf_exp),1:0)
#define ERRORMSG(cond,printf_exp)  ((cond)?(NKDbgPrintfW(TEXT("ERROR: %s line %d: "),TEXT(__FILE__),__LINE__), NKDbgPrintfW printf_exp),1:0)

#endif /* SHIP_BUILD */

#define RETAILLED(cond,parms)  ((void)((cond)?(WriteDebugLED parms),1:0))

#define ASSERTMSG(msg,exp)  (DEBUGMSG(!exp,(msg)),DBGCHK(TEXT("Unknown"),exp))
#define ASSERT(exp)  DBGCHK(TEXT("Unknown"), exp)
#define ASSERT_IMPLIES(cond,exp)  ASSERT(!(cond)||(exp))
#ifdef DEBUG
#define VERIFY(exp)  ASSERT(exp)
#else
#define VERIFY(exp)  ((void)(exp))
#endif

#ifdef __cplusplus
}
#endif

#endif /* _DBGAPI_H */
