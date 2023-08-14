#ifndef _STRSAFE_H
#define _STRSAFE_H

/* Safe string API definitions */

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <wchar.h>

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2073)  /* Overflow in converting constant expression */
#endif

#ifndef _HRESULT_DEFINED
#define _HRESULT_DEFINED
typedef long HRESULT;
#endif 

#ifndef SUCCEEDED
#define SUCCEEDED(hr)  ((HRESULT)(hr) >= 0)
#endif

#ifndef FAILED
#define FAILED(hr)  ((HRESULT)(hr) < 0)
#endif

#ifndef S_OK
#define S_OK  ((HRESULT)0x00000000L)
#endif

#ifdef __cplusplus
#define _STRSAFE_EXTERN_C  extern "C"
#else
#define _STRSAFE_EXTERN_C  extern
#endif

#if defined(STRSAFE_LIB)
#define STRSAFEAPI  _STRSAFE_EXTERN_C HRESULT __stdcall
#define STRSAFEAPIV  _STRSAFE_EXTERN_C HRESULT __cdecl
#pragma comment(lib, "strsafe.lib")
#elif defined(STRSAFE_LIB_IMPL)
#define STRSAFEAPI  _STRSAFE_EXTERN_C HRESULT __stdcall
#define STRSAFEAPIV  _STRSAFE_EXTERN_C HRESULT __cdecl
#else
#define STRSAFEAPI  __inline HRESULT __stdcall
#define STRSAFEAPIV  __inline HRESULT __cdecl
#define STRSAFE_INLINE
#endif

#define STRSAFE_INLINE_API  __inline HRESULT __stdcall
#define STRSAFE_INLINE_APIV  __inline HRESULT __cdecl

#if defined(STRSAFE_NO_CB_FUNCTIONS) && defined(STRSAFE_NO_CCH_FUNCTIONS)
#error cannot specify both STRSAFE_NO_CB_FUNCTIONS and STRSAFE_NO_CCH_FUNCTIONS !!
#endif

/*
#ifdef STRSAFE_LIB_IMPL
#define STRSAFE_INLINE
#endif
*/

#define STRSAFE_MAX_CCH  2147483647

#define STRSAFE_IGNORE_NULLS  0x00000100
#define STRSAFE_FILL_BEHIND_NULL  0x00000200
#define STRSAFE_FILL_ON_FAILURE  0x00000400
#define STRSAFE_NULL_ON_FAILURE  0x00000800
#define STRSAFE_NO_TRUNCATION  0x00001000

#define STRSAFE_VALID_FLAGS  (0x000000FF|STRSAFE_IGNORE_NULLS|STRSAFE_FILL_BEHIND_NULL|STRSAFE_FILL_ON_FAILURE|STRSAFE_NULL_ON_FAILURE|STRSAFE_NO_TRUNCATION)

#define STRSAFE_FILL_BYTE(x)     ((unsigned long)((x & 0x000000FF)|STRSAFE_FILL_BEHIND_NULL))
#define STRSAFE_FAILURE_BYTE(x)  ((unsigned long)((x & 0x000000FF)|STRSAFE_FILL_ON_FAILURE))

#define STRSAFE_GET_FILL_PATTERN(dwFlags)  ((int)(dwFlags & 0x000000FF))

#define STRSAFE_E_INSUFFICIENT_BUFFER  ((HRESULT)0x8007007AL)
#define STRSAFE_E_INVALID_PARAMETER    ((HRESULT)0x80070057L)
#define STRSAFE_E_END_OF_FILE          ((HRESULT)0x80070026L)

#if defined(STRSAFE_INLINE) || defined(STRSAFE_LIB_IMPL)
STRSAFEAPI StringCopyWorkerA(char*,size_t,const char*);
STRSAFEAPI StringCopyWorkerW(wchar_t*,size_t,const wchar_t*);
STRSAFEAPI StringCopyExWorkerA(char*,size_t,size_t,const char*,char**,size_t*,unsigned long);
STRSAFEAPI StringCopyExWorkerW(wchar_t*,size_t,size_t,const wchar_t*,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCopyNWorkerA(char*,size_t,const char*,size_t);
STRSAFEAPI StringCopyNWorkerW(wchar_t*,size_t,const wchar_t*,size_t);
STRSAFEAPI StringCopyNExWorkerA(char*,size_t,size_t,const char*,size_t,char**,size_t*,unsigned long);
STRSAFEAPI StringCopyNExWorkerW(wchar_t*,size_t,size_t,const wchar_t*,size_t,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCatWorkerA(char*,size_t,const char*);
STRSAFEAPI StringCatWorkerW(wchar_t*,size_t,const wchar_t*);
STRSAFEAPI StringCatExWorkerA(char*,size_t,size_t,const char*,char**,size_t*,unsigned long);
STRSAFEAPI StringCatExWorkerW(wchar_t*,size_t,size_t,const wchar_t*,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCatNWorkerA(char*,size_t,const char*,size_t);
STRSAFEAPI StringCatNWorkerW(wchar_t*,size_t,const wchar_t*,size_t);
STRSAFEAPI StringCatNExWorkerA(char*,size_t,size_t,const char*,size_t,char**,size_t*,unsigned long);
STRSAFEAPI StringCatNExWorkerW(wchar_t*,size_t,size_t,const wchar_t*,size_t,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringVPrintfWorkerA(char*,size_t,const char*,va_list);
STRSAFEAPI StringVPrintfWorkerW(wchar_t*,size_t,const wchar_t*,va_list);
STRSAFEAPI StringVPrintfExWorkerA(char*,size_t,size_t,char**,size_t*,unsigned long,const char*,va_list);
STRSAFEAPI StringVPrintfExWorkerW(wchar_t*,size_t,size_t,wchar_t**,size_t*,unsigned long,const wchar_t*,va_list);
STRSAFEAPI StringLengthWorkerA(const char*,size_t,size_t*);
STRSAFEAPI StringLengthWorkerW(const wchar_t*,size_t,size_t*);
#endif /* STRSAFE_INLINE */

#ifndef STRSAFE_LIB_IMPL
STRSAFE_INLINE_API StringGetsExWorkerA(char*,size_t,size_t,char**,size_t*,unsigned long);
STRSAFE_INLINE_API StringGetsExWorkerW(wchar_t*,size_t,size_t,wchar_t**,size_t*,unsigned long);
#endif /* STRSAFE_LIB_IMPL */

#ifndef STRSAFE_NO_CCH_FUNCTIONS
STRSAFEAPI StringCchCopyA(char*,size_t,const char*);
STRSAFEAPI StringCchCopyW(wchar_t*,size_t,const wchar_t*);
STRSAFEAPI StringCchCopyExA(char*,size_t,const char*,char**,size_t*,unsigned long);
STRSAFEAPI StringCchCopyExW(wchar_t*,size_t,const wchar_t*,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCchCopyNA(char*,size_t,const char*, size_t);
STRSAFEAPI StringCchCopyNW(wchar_t*,size_t,const wchar_t*,size_t);
STRSAFEAPI StringCchCopyNExA(char*,size_t,const char*,size_t,char**,size_t*,unsigned long);
STRSAFEAPI StringCchCopyNExW(wchar_t*,size_t,const wchar_t*,size_t,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCchCatA(char*,size_t,const char*);
STRSAFEAPI StringCchCatW(wchar_t*,size_t,const wchar_t*);
STRSAFEAPI StringCchCatExA(char*,size_t,const char*,char**,size_t*,unsigned long);
STRSAFEAPI StringCchCatExW(wchar_t*,size_t,const wchar_t*,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCchCatNA(char*,size_t,const char*,size_t);
STRSAFEAPI StringCchCatNW(wchar_t*,size_t,const wchar_t*,size_t);
STRSAFEAPI StringCchCatNExA(char*,size_t,const char*,size_t,char**,size_t*,unsigned long);
STRSAFEAPI StringCchCatNExW(wchar_t*,size_t,const wchar_t*,size_t,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCchVPrintfA(char*,size_t,const char*,va_list);
STRSAFEAPI StringCchVPrintfW(wchar_t*,size_t,const wchar_t*,va_list);
STRSAFEAPIV StringCchPrintfA(char*,size_t,const char*, ...);
STRSAFEAPIV StringCchPrintfW(wchar_t*,size_t,const wchar_t*, ...);
STRSAFEAPIV StringCchPrintfExA(char*,size_t,char**,size_t*,unsigned long,const char*, ...);
STRSAFEAPIV StringCchPrintfExW(wchar_t*,size_t,wchar_t**,size_t*,unsigned long,const wchar_t*, ...);
STRSAFEAPI StringCchVPrintfExA(char*,size_t,char**,size_t*,unsigned long,const char*,va_list);
STRSAFEAPI StringCchVPrintfExW(wchar_t*,size_t,wchar_t**,size_t*,unsigned long,const wchar_t*,va_list);
STRSAFEAPI StringCchLengthA(const char*,size_t,size_t*);
STRSAFEAPI StringCchLengthW(const wchar_t*,size_t,size_t*);

#ifndef STRSAFE_LIB_IMPL
STRSAFE_INLINE_API StringCchGetsA(char*, size_t);
STRSAFE_INLINE_API StringCchGetsW(wchar_t*,size_t);
STRSAFE_INLINE_API StringCchGetsExA(char*,size_t, char**,size_t*,unsigned long);
STRSAFE_INLINE_API StringCchGetsExW(wchar_t*,size_t,wchar_t**,size_t*,unsigned long);
#endif /* STRSAFE_LIB_IMPL */

#ifdef UNICODE
#define StringCchCopy  StringCchCopyW
#define StringCchCopyEx  StringCchCopyExW
#define StringCchCopyN  StringCchCopyNW
#define StringCchCopyNEx  StringCchCopyNExW
#define StringCchCat  StringCchCatW
#define StringCchCatEx  StringCchCatExW
#define StringCchCatN  StringCchCatNW
#define StringCchCatNEx  StringCchCatNExW
#define StringCchVPrintf  StringCchVPrintfW
#define StringCchPrintf  StringCchPrintfW
#define StringCchPrintfEx  StringCchPrintfExW
#define StringCchVPrintfEx  StringCchVPrintfExW
#define StringCchGets  StringCchGetsW
#define StringCchGetsEx  StringCchGetsExW
#define StringCchLength  StringCchLengthW
#else
#define StringCchCopy  StringCchCopyA
#define StringCchCopyEx  StringCchCopyExA
#define StringCchCopyN  StringCchCopyNA
#define StringCchCopyNEx  StringCchCopyNExA
#define StringCchCat  StringCchCatA
#define StringCchCatEx  StringCchCatExA
#define StringCchCatN  StringCchCatNA
#define StringCchCatNEx  StringCchCatNExA
#define StringCchVPrintf  StringCchVPrintfA
#define StringCchPrintf  StringCchPrintfA
#define StringCchPrintfEx  StringCchPrintfExA
#define StringCchVPrintfEx  StringCchVPrintfExA
#define StringCchGets  StringCchGetsA
#define StringCchGetsEx  StringCchGetsExA
#define StringCchLength  StringCchLengthA
#endif /* UNICODE */

#endif /* STRSAFE_NO_CCH_FUNCTIONS */

#ifndef STRSAFE_NO_CB_FUNCTIONS
STRSAFEAPI StringCbCopyA(char*,size_t,const char*);
STRSAFEAPI StringCbCopyW(wchar_t*,size_t,const wchar_t*);
STRSAFEAPI StringCbCopyExA(char*,size_t,const char*,char**,size_t*,unsigned long);
STRSAFEAPI StringCbCopyExW(wchar_t*,size_t,const wchar_t*,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCbCopyNA(char*,size_t,const char*,size_t);
STRSAFEAPI StringCbCopyNW(wchar_t*,size_t,const wchar_t*,size_t);
STRSAFEAPI StringCbCopyNExA(char*,size_t,const char*,size_t,char**,size_t*,unsigned long);
STRSAFEAPI StringCbCopyNExW(wchar_t*,size_t,const wchar_t*,size_t,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCbCatA(char*,size_t,const char*);
STRSAFEAPI StringCbCatW(wchar_t*,size_t,const wchar_t*);
STRSAFEAPI StringCbCatExA(char*,size_t,const char*,char**,size_t*,unsigned long);
STRSAFEAPI StringCbCatExW(wchar_t*,size_t,const wchar_t*,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCbCatNA(char*,size_t,const char*,size_t);
STRSAFEAPI StringCbCatNW(wchar_t*,size_t,const wchar_t*,size_t);
STRSAFEAPI StringCbCatNExA(char*,size_t,const char*,size_t,char**,size_t*,unsigned long);
STRSAFEAPI StringCbCatNExW(wchar_t*,size_t,const wchar_t*,size_t,wchar_t**,size_t*,unsigned long);
STRSAFEAPI StringCbVPrintfA(char*,size_t,const char*,va_list);
STRSAFEAPI StringCbVPrintfW(wchar_t*,size_t,const wchar_t*,va_list);
STRSAFEAPIV StringCbPrintfA(char*,size_t,const char*, ...);
STRSAFEAPIV StringCbPrintfW(wchar_t*,size_t,const wchar_t*, ...);
STRSAFEAPIV StringCbPrintfExA(char*,size_t,char**,size_t*,unsigned long,const char*, ...);
STRSAFEAPIV StringCbPrintfExW(wchar_t*,size_t,wchar_t**,size_t*,unsigned long,const wchar_t*, ...);
STRSAFEAPI StringCbVPrintfExA(char*,size_t,char**,size_t*,unsigned long,const char*,va_list);
STRSAFEAPI StringCbVPrintfExW(wchar_t*,size_t,wchar_t**,size_t*,unsigned long,const wchar_t*,va_list);
STRSAFEAPI StringCbLengthA(const char*,size_t,size_t*);
STRSAFEAPI StringCbLengthW(const wchar_t*,size_t,size_t*);

#ifndef STRSAFE_LIB_IMPL
STRSAFE_INLINE_API StringCbGetsA(char*,size_t);
STRSAFE_INLINE_API StringCbGetsW(wchar_t*,size_t);
STRSAFE_INLINE_API StringCbGetsExA(char*,size_t, char**,size_t*,unsigned long);
STRSAFE_INLINE_API StringCbGetsExW(wchar_t*,size_t,wchar_t**,size_t*,unsigned long);
#endif /* STRSAFE_LIB_IMPL */

#ifdef UNICODE
#define StringCbCopy  StringCbCopyW
#define StringCbCopyEx  StringCbCopyExW
#define StringCbCopyN  StringCbCopyNW
#define StringCbCopyNEx  StringCbCopyNExW
#define StringCbCat  StringCbCatW
#define StringCbCatEx  StringCbCatExW
#define StringCbCatN  StringCbCatNW
#define StringCbCatNEx  StringCbCatNExW
#define StringCbVPrintf  StringCbVPrintfW
#define StringCbPrintf  StringCbPrintfW
#define StringCbPrintfEx  StringCbPrintfExW
#define StringCbVPrintfEx  StringCbVPrintfExW
#define StringCbGets  StringCbGetsW
#define StringCbGetsEx  StringCbGetsExW
#define StringCbLength  StringCbLengthW
#else
#define StringCbCopy  StringCbCopyA
#define StringCbCopyEx  StringCbCopyExA
#define StringCbCopyN  StringCbCopyNA
#define StringCbCopyNEx  StringCbCopyNExA
#define StringCbCat  StringCbCatA
#define StringCbCatEx  StringCbCatExA
#define StringCbCatN  StringCbCatNA
#define StringCbCatNEx  StringCbCatNExA
#define StringCbVPrintf  StringCbVPrintfA
#define StringCbPrintf  StringCbPrintfA
#define StringCbPrintfEx  StringCbPrintfExA
#define StringCbVPrintfEx  StringCbVPrintfExA
#define StringCbGets  StringCbGetsA
#define StringCbGetsEx  StringCbGetsExA
#define StringCbLength  StringCbLengthA
#endif /* UNICODE */

#endif /* STRSAFE_NO_CB_FUNCTIONS */


#ifndef STRSAFE_NO_CCH_FUNCTIONS
#ifdef STRSAFE_INLINE
STRSAFEAPI StringCchCopyA(char *pszDest, size_t cchDest, const char *pszSrc)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyWorkerA(pszDest, cchDest, pszSrc);
}

STRSAFEAPI StringCchCopyW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyWorkerW(pszDest, cchDest, pszSrc);
}

STRSAFEAPI StringCchCopyExA(char *pszDest, size_t cchDest, const char *pszSrc, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return  STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyExWorkerA(pszDest, cchDest, cchDest * sizeof(char), pszSrc, ppszDestEnd, pcchRemaining, dwFlags);
}

STRSAFEAPI StringCchCopyExW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyExWorkerW(pszDest, cchDest, cchDest * sizeof(wchar_t), pszSrc, ppszDestEnd, pcchRemaining, dwFlags);
}

STRSAFEAPI StringCchCopyNA(char *pszDest, size_t cchDest, const char *pszSrc, size_t cchSrc)
{
    if (cchDest > STRSAFE_MAX_CCH || cchSrc > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyNWorkerA(pszDest, cchDest, pszSrc, cchSrc);
}

STRSAFEAPI StringCchCopyNW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc, size_t cchSrc)
{
    if (cchDest > STRSAFE_MAX_CCH || cchSrc > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyNWorkerW(pszDest, cchDest, pszSrc, cchSrc);
}

STRSAFEAPI StringCchCopyNExA(char *pszDest, size_t cchDest, const char *pszSrc, size_t cchSrc, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH || cchSrc > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyNExWorkerA(pszDest, cchDest, cchDest * sizeof(char), pszSrc, cchSrc, ppszDestEnd, pcchRemaining, dwFlags);
}

STRSAFEAPI StringCchCopyNExW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc, size_t cchSrc, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH || cchSrc > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyNExWorkerW(pszDest, cchDest, cchDest * sizeof(wchar_t), pszSrc, cchSrc, ppszDestEnd, pcchRemaining, dwFlags);
}

STRSAFEAPI StringCchCatA(char *pszDest, size_t cchDest, const char *pszSrc)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatWorkerA(pszDest, cchDest, pszSrc);
}

STRSAFEAPI StringCchCatW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatWorkerW(pszDest, cchDest, pszSrc);
}

STRSAFEAPI StringCchCatExA(char *pszDest, size_t cchDest, const char *pszSrc, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatExWorkerA(pszDest, cchDest, cchDest * sizeof(char), pszSrc, ppszDestEnd, pcchRemaining, dwFlags);
}

STRSAFEAPI StringCchCatExW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatExWorkerW(pszDest, cchDest, cchDest * sizeof(wchar_t), pszSrc, ppszDestEnd, pcchRemaining, dwFlags);
}

STRSAFEAPI StringCchCatNA(char *pszDest, size_t cchDest, const char *pszSrc, size_t cchMaxAppend)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatNWorkerA(pszDest, cchDest, pszSrc, cchMaxAppend);
}

STRSAFEAPI StringCchCatNW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc, size_t cchMaxAppend)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatNWorkerW(pszDest, cchDest, pszSrc, cchMaxAppend);
}

STRSAFEAPI StringCchCatNExA(char *pszDest, size_t cchDest, const char *pszSrc, size_t cchMaxAppend, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatNExWorkerA(pszDest, cchDest, cchDest * sizeof(char), pszSrc, cchMaxAppend, ppszDestEnd, pcchRemaining, dwFlags);
}

STRSAFEAPI StringCchCatNExW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc, size_t cchMaxAppend, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatNExWorkerW(pszDest, cchDest, cchDest * sizeof(wchar_t), pszSrc, cchMaxAppend, ppszDestEnd, pcchRemaining, dwFlags);
}

STRSAFEAPI StringCchVPrintfA(char *pszDest, size_t cchDest, const char *pszFormat, va_list argList)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringVPrintfWorkerA(pszDest, cchDest, pszFormat, argList);
}

STRSAFEAPI StringCchVPrintfW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszFormat, va_list argList)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringVPrintfWorkerW(pszDest, cchDest, pszFormat, argList);
}

STRSAFEAPIV StringCchPrintfA(char *pszDest, size_t cchDest, const char *pszFormat, ...)
{
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
    {
        va_list argList;

        va_start(argList, pszFormat);
        hr = StringVPrintfWorkerA(pszDest, cchDest, pszFormat, argList);
        va_end(argList);
    }

    return hr;
}

STRSAFEAPIV StringCchPrintfW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszFormat, ...)
{
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
    {
        va_list argList;

        va_start(argList, pszFormat);
        hr = StringVPrintfWorkerW(pszDest, cchDest, pszFormat, argList);
        va_end(argList);
    }

    return hr;
}

STRSAFEAPIV StringCchPrintfExA(char *pszDest, size_t cchDest, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags, const char *pszFormat, ...)
{
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
    {
        va_list argList;

        va_start(argList, pszFormat);
        hr = StringVPrintfExWorkerA(pszDest, cchDest, cchDest * sizeof(char), ppszDestEnd, pcchRemaining, dwFlags, pszFormat, argList);
        va_end(argList);
    }

    return hr;
}

STRSAFEAPIV StringCchPrintfExW(wchar_t *pszDest, size_t cchDest, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags, const wchar_t *pszFormat, ...)
{
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
    {
        va_list argList;

        va_start(argList, pszFormat);
        hr = StringVPrintfExWorkerW(pszDest, cchDest, cchDest * sizeof(wchar_t), ppszDestEnd, pcchRemaining, dwFlags, pszFormat, argList);
        va_end(argList);
    }

    return hr;
}

STRSAFEAPI StringCchVPrintfExA(char *pszDest, size_t cchDest, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags, const char *pszFormat, va_list argList)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringVPrintfExWorkerA(pszDest, cchDest, cchDest * sizeof(char), ppszDestEnd, pcchRemaining, dwFlags, pszFormat, argList);
}

STRSAFEAPI StringCchVPrintfExW(wchar_t *pszDest, size_t cchDest, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags, const wchar_t *pszFormat, va_list argList)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringVPrintfExWorkerW(pszDest, cchDest, cchDest * sizeof(wchar_t), ppszDestEnd, pcchRemaining, dwFlags, pszFormat, argList);
}

STRSAFEAPI StringCchLengthA(const char *psz, size_t cchMax, size_t *pcch)
{
    if (psz == NULL || cchMax > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringLengthWorkerA(psz, cchMax, pcch);
}

STRSAFEAPI StringCchLengthW(const wchar_t *psz, size_t cchMax, size_t *pcch)
{
    if (psz == NULL || cchMax > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringLengthWorkerW(psz, cchMax, pcch);
}
#endif /* STRSAFE_INLINE */

#ifndef STRSAFE_LIB_IMPL
STRSAFE_INLINE_API StringCchGetsA(char *pszDest, size_t cchDest)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringGetsExWorkerA(pszDest, cchDest, cchDest * sizeof(char), NULL, NULL, 0);
}

STRSAFE_INLINE_API StringCchGetsW(wchar_t *pszDest, size_t cchDest)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringGetsExWorkerW(pszDest, cchDest, cchDest * sizeof(wchar_t), NULL, NULL, 0);
}

STRSAFE_INLINE_API StringCchGetsExA(char *pszDest, size_t cchDest, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringGetsExWorkerA(pszDest, cchDest, cchDest * sizeof(char), ppszDestEnd, pcchRemaining, dwFlags);
}

STRSAFE_INLINE_API StringCchGetsExW(wchar_t *pszDest, size_t cchDest, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringGetsExWorkerW(pszDest, cchDest, cchDest * sizeof(wchar_t), ppszDestEnd, pcchRemaining, dwFlags);
}
#endif /* STRSAFE_LIB_IMPL */

#endif /* STRSAFE_NO_CCH_FUNCTIONS */


#ifndef STRSAFE_NO_CB_FUNCTIONS
#ifdef STRSAFE_INLINE
STRSAFEAPI StringCbCopyA(char *pszDest, size_t cbDest, const char *pszSrc)
{
    size_t cchDest = cbDest / sizeof(char);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyWorkerA(pszDest, cchDest, pszSrc);
}

STRSAFEAPI StringCbCopyW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszSrc)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyWorkerW(pszDest, cchDest, pszSrc);
}

STRSAFEAPI StringCbCopyExA(char *pszDest, size_t cbDest, const char *pszSrc, char **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    size_t cchRemaining = 0;
    size_t cchDest = cbDest / sizeof(char);
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringCopyExWorkerA(pszDest, cchDest, cbDest, pszSrc, ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(char)) + (cbDest % sizeof(char));
    }

    return hr;
}

STRSAFEAPI StringCbCopyExW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszSrc, wchar_t **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    size_t cchRemaining = 0;
    size_t cchDest = cbDest / sizeof(wchar_t);
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringCopyExWorkerW(pszDest, cchDest, cbDest, pszSrc, ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t));
    }

    return hr;
}

STRSAFEAPI StringCbCopyNA(char *pszDest, size_t cbDest, const char *pszSrc, size_t cbSrc)
{
    size_t cchDest = cbDest / sizeof(char);
    size_t cchSrc = cbSrc / sizeof(char);

    if (cchDest > STRSAFE_MAX_CCH || cchSrc > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyNWorkerA(pszDest, cchDest, pszSrc, cchSrc);
}

STRSAFEAPI StringCbCopyNW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszSrc, size_t cbSrc)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    size_t cchSrc = cbSrc / sizeof(wchar_t);

    if (cchDest > STRSAFE_MAX_CCH || cchSrc > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCopyNWorkerW(pszDest, cchDest, pszSrc, cchSrc);
}

STRSAFEAPI StringCbCopyNExA(char *pszDest, size_t cbDest, const char *pszSrc, size_t cbSrc, char **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    size_t cchDest = cbDest / sizeof(char);
    size_t cchSrc = cbSrc / sizeof(char);
    size_t cchRemaining = 0;
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH || cchSrc > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringCopyNExWorkerA(pszDest, cchDest, cbDest, pszSrc, cchSrc, ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(char)) + (cbDest % sizeof(char));
    }

    return hr;
}

STRSAFEAPI StringCbCopyNExW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszSrc, size_t cbSrc, wchar_t **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    size_t cchSrc = cbSrc / sizeof(wchar_t);
    size_t cchRemaining = 0;
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH || cchSrc > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringCopyNExWorkerW(pszDest, cchDest, cbDest, pszSrc, cchSrc, ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t));
    }

    return hr;
}

STRSAFEAPI StringCbCatA(char *pszDest, size_t cbDest, const char *pszSrc)
{
    size_t cchDest = cbDest / sizeof(char);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatWorkerA(pszDest, cchDest, pszSrc);
}

STRSAFEAPI StringCbCatW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszSrc)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatWorkerW(pszDest, cchDest, pszSrc);
}

STRSAFEAPI StringCbCatExA(char *pszDest, size_t cbDest, const char *pszSrc, char **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    size_t cchDest = cbDest / sizeof(char);
    size_t cchRemaining = 0;
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringCatExWorkerA(pszDest, cchDest, cbDest, pszSrc, ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(char)) + (cbDest % sizeof(char));
    }

    return hr;
}

STRSAFEAPI StringCbCatExW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszSrc, wchar_t **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    size_t cchRemaining = 0;
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringCatExWorkerW(pszDest, cchDest, cbDest, pszSrc, ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t));
    }

    return hr;
}

STRSAFEAPI StringCbCatNA(char *pszDest, size_t cbDest, const char *pszSrc, size_t cbMaxAppend)
{
    size_t cchDest = cbDest / sizeof(char);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatNWorkerA(pszDest, cchDest, pszSrc, cbMaxAppend / sizeof(char));
}

STRSAFEAPI StringCbCatNW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszSrc, size_t cbMaxAppend)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringCatNWorkerW(pszDest, cchDest, pszSrc, cbMaxAppend / sizeof(wchar_t));
}

STRSAFEAPI StringCbCatNExA(char *pszDest, size_t cbDest, const char *pszSrc, size_t cbMaxAppend, char **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    size_t cchDest = cbDest / sizeof(char);
    size_t cchRemaining = 0;
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringCatNExWorkerA(pszDest, cchDest, cbDest, pszSrc, cbMaxAppend / sizeof(char), ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(char)) + (cbDest % sizeof(char));
    }

    return hr;
}

STRSAFEAPI StringCbCatNExW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszSrc, size_t cbMaxAppend, wchar_t **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    HRESULT hr;
    size_t cchDest = cbDest / sizeof(wchar_t);
    size_t cchRemaining = 0;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringCatNExWorkerW(pszDest, cchDest, cbDest, pszSrc, cbMaxAppend / sizeof(wchar_t), ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t));
    }

    return hr;
}

STRSAFEAPI StringCbVPrintfA(char *pszDest, size_t cbDest, const char *pszFormat, va_list argList)
{
    size_t cchDest = cbDest / sizeof(char);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringVPrintfWorkerA(pszDest, cchDest, pszFormat, argList);
}

STRSAFEAPI StringCbVPrintfW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszFormat, va_list argList)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringVPrintfWorkerW(pszDest, cchDest, pszFormat, argList);
}

STRSAFEAPIV StringCbPrintfA(char *pszDest, size_t cbDest, const char *pszFormat, ...)
{
    size_t cchDest = cbDest / sizeof(char);
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
    {
        va_list argList;

        va_start(argList, pszFormat);
        hr = StringVPrintfWorkerA(pszDest, cchDest, pszFormat, argList);
        va_end(argList);
    }

    return hr;
}

STRSAFEAPIV StringCbPrintfW(wchar_t *pszDest, size_t cbDest, const wchar_t *pszFormat, ...)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
    {
        va_list argList;

        va_start(argList, pszFormat);
        hr = StringVPrintfWorkerW(pszDest, cchDest, pszFormat, argList);
        va_end(argList);
    }

    return hr;
}

STRSAFEAPIV StringCbPrintfExA(char *pszDest, size_t cbDest, char **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags, const char *pszFormat, ...)
{
    HRESULT hr;
    size_t cchDest = cbDest / sizeof(char);
    size_t cchRemaining = 0;

    if (cchDest > STRSAFE_MAX_CCH)
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        va_list argList;

        va_start(argList, pszFormat);
        hr = StringVPrintfExWorkerA(pszDest, cchDest, cbDest, ppszDestEnd, &cchRemaining, dwFlags, pszFormat, argList);
        va_end(argList);
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(char)) + (cbDest % sizeof(char));
    }

    return hr;
}

STRSAFEAPIV StringCbPrintfExW(wchar_t *pszDest, size_t cbDest, wchar_t **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags, const wchar_t *pszFormat, ...)
{
    HRESULT hr;
    size_t cchDest;
    size_t cchRemaining = 0;

    cchDest = cbDest / sizeof(wchar_t);

    if (cchDest > STRSAFE_MAX_CCH)
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        va_list argList;

        va_start(argList, pszFormat);

        hr = StringVPrintfExWorkerW(pszDest, cchDest, cbDest, ppszDestEnd, &cchRemaining, dwFlags, pszFormat, argList);

        va_end(argList);
    }

    if (SUCCEEDED(hr) || (hr == STRSAFE_E_INSUFFICIENT_BUFFER))
    {
        if (pcbRemaining)
        {
            
            *pcbRemaining = (cchRemaining * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t));
        }
    }

    return hr;
}

STRSAFEAPI StringCbVPrintfExA(char *pszDest, size_t cbDest, char **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags, const char *pszFormat, va_list argList)
{
    size_t cchDest = cbDest / sizeof(char);
    size_t cchRemaining = 0;
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringVPrintfExWorkerA(pszDest, cchDest, cbDest, ppszDestEnd, &cchRemaining, dwFlags, pszFormat, argList);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(char)) + (cbDest % sizeof(char));
    }

    return hr;
}

STRSAFEAPI StringCbVPrintfExW(wchar_t *pszDest, size_t cbDest, wchar_t **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags, const wchar_t *pszFormat, va_list argList)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    size_t cchRemaining = 0;
    HRESULT hr;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringVPrintfExWorkerW(pszDest, cchDest, cbDest, ppszDestEnd, &cchRemaining, dwFlags, pszFormat, argList);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t));
    }

    return hr;
}

STRSAFEAPI StringCbLengthA(const char *psz, size_t cbMax, size_t *pcb)
{
    size_t cchMax = cbMax / sizeof(char);
    size_t cch = 0;
    HRESULT hr;

    if (psz == NULL || cchMax > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringLengthWorkerA(psz, cchMax, &cch);

    if (SUCCEEDED(hr) && pcb)
        *pcb = cch * sizeof(char);

    return hr;
}

STRSAFEAPI StringCbLengthW(const wchar_t *psz, size_t cbMax, size_t *pcb)
{
    size_t cchMax = cbMax / sizeof(wchar_t);
    size_t cch = 0;
    HRESULT hr;

    if (psz == NULL || cchMax > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringLengthWorkerW(psz, cchMax, &cch);

    if (SUCCEEDED(hr) && pcb)
        *pcb = cch * sizeof(wchar_t);

    return hr;
}
#endif /* STRSAFE_INLINE */

#ifndef STRSAFE_LIB_IMPL
STRSAFE_INLINE_API StringCbGetsA(char *pszDest, size_t cbDest)
{
    size_t cchDest = cbDest / sizeof(char);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringGetsExWorkerA(pszDest, cchDest, cbDest, NULL, NULL, 0);
}

STRSAFE_INLINE_API StringCbGetsW(wchar_t *pszDest, size_t cbDest)
{
    size_t cchDest = cbDest / sizeof(wchar_t);
    if (cchDest > STRSAFE_MAX_CCH)
        return STRSAFE_E_INVALID_PARAMETER;
    else
        return StringGetsExWorkerW(pszDest, cchDest, cbDest, NULL, NULL, 0);
}

STRSAFE_INLINE_API StringCbGetsExA(char *pszDest, size_t cbDest, char **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    HRESULT hr;
    size_t cchDest = cbDest / sizeof(char);
    size_t cchRemaining = 0;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringGetsExWorkerA(pszDest, cchDest, cbDest, ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER || hr == STRSAFE_E_END_OF_FILE)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(char)) + (cbDest % sizeof(char));
    }

    return hr;
}

STRSAFE_INLINE_API StringCbGetsExW(wchar_t *pszDest, size_t cbDest, wchar_t **ppszDestEnd, size_t *pcbRemaining, unsigned long dwFlags)
{
    HRESULT hr;
    size_t cchDest = cbDest / sizeof(wchar_t);
    size_t cchRemaining = 0;

    if (cchDest > STRSAFE_MAX_CCH)
        hr = STRSAFE_E_INVALID_PARAMETER;
    else
        hr = StringGetsExWorkerW(pszDest, cchDest, cbDest, ppszDestEnd, &cchRemaining, dwFlags);

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER || hr == STRSAFE_E_END_OF_FILE)
    {
        if (pcbRemaining)
            *pcbRemaining = (cchRemaining * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t));
    }

    return hr;
}
#endif /* STRSAFE_LIB_IMPL */

#endif /* STRSAFE_NO_CB_FUNCTIONS */


#ifdef STRSAFE_INLINE
STRSAFEAPI StringCopyWorkerA(char *pszDest, size_t cchDest, const char *pszSrc)
{
    HRESULT hr = S_OK;

    if (cchDest == 0)
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        while (cchDest && *pszSrc != '\0')
        {
            *pszDest++ = *pszSrc++;
            cchDest--;
        }

        if (cchDest == 0)
        {
            pszDest--;
            hr = STRSAFE_E_INSUFFICIENT_BUFFER;
        }

        *pszDest= '\0';
    }

    return hr;
}

STRSAFEAPI StringCopyWorkerW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc)
{
    HRESULT hr = S_OK;

    if (cchDest == 0)
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        while (cchDest && *pszSrc != L'\0')
        {
            *pszDest++ = *pszSrc++;
            cchDest--;
        }

        if (cchDest == 0)
        {
            pszDest--;
            hr = STRSAFE_E_INSUFFICIENT_BUFFER;
        }

        *pszDest= L'\0';
    }

    return hr;
}

STRSAFEAPI StringCopyExWorkerA(char *pszDest, size_t cchDest, size_t cbDest, const char *pszSrc, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    char *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest != 0 || cbDest != 0)
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }

            if (pszSrc == NULL)
                pszSrc = "";
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                pszDestEnd = pszDest;
                cchRemaining = 0;

                if (*pszSrc != '\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                pszDestEnd = pszDest;
                cchRemaining = cchDest;

                while (cchRemaining && (*pszSrc != '\0'))
                {
                    *pszDestEnd++= *pszSrc++;
                    cchRemaining--;
                }

                if (cchRemaining > 0)
                {
                    if (dwFlags & STRSAFE_FILL_BEHIND_NULL)
                    {
                        memset(pszDestEnd + 1, STRSAFE_GET_FILL_PATTERN(dwFlags), ((cchRemaining - 1) * sizeof(char)) + (cbDest % sizeof(char)));
                    }
                }
                else
                {
                    
                    pszDestEnd--;
                    cchRemaining++;

                    hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }

                *pszDestEnd = '\0';
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = '\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE | STRSAFE_NO_TRUNCATION))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = '\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || (hr == STRSAFE_E_INSUFFICIENT_BUFFER))
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringCopyExWorkerW(wchar_t *pszDest, size_t cchDest, size_t cbDest, const wchar_t *pszSrc, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    wchar_t *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest != 0 || cbDest != 0)
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }

            if (pszSrc == NULL)
                pszSrc = L"";
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                pszDestEnd = pszDest;
                cchRemaining = 0;

                if (*pszSrc != L'\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                pszDestEnd = pszDest;
                cchRemaining = cchDest;

                while (cchRemaining && (*pszSrc != L'\0'))
                {
                    *pszDestEnd++= *pszSrc++;
                    cchRemaining--;
                }

                if (cchRemaining > 0)
                {
                    if (dwFlags & STRSAFE_FILL_BEHIND_NULL)
                    {
                        memset(pszDestEnd + 1, STRSAFE_GET_FILL_PATTERN(dwFlags), ((cchRemaining - 1) * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t)));
                    }
                }
                else
                {
                    pszDestEnd--;
                    cchRemaining++;
                    hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }

                *pszDestEnd = L'\0';
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = L'\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE | STRSAFE_NO_TRUNCATION))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = L'\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringCopyNWorkerA(char *pszDest, size_t cchDest, const char *pszSrc, size_t cchSrc)
{
    HRESULT hr = S_OK;

    if (cchDest == 0)
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        while (cchDest && cchSrc && *pszSrc != '\0')
        {
            *pszDest++ = *pszSrc++;
            cchDest--;
            cchSrc--;
        }

        if (cchDest == 0)
        {
            pszDest--;
            hr = STRSAFE_E_INSUFFICIENT_BUFFER;
        }

        *pszDest= '\0';
    }

    return hr;
}

STRSAFEAPI StringCopyNWorkerW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc, size_t cchSrc)
{
    HRESULT hr = S_OK;

    if (cchDest == 0)
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        while (cchDest && cchSrc && *pszSrc != L'\0')
        {
            *pszDest++ = *pszSrc++;
            cchDest--;
            cchSrc--;
        }

        if (cchDest == 0)
        {
            pszDest--;
            hr = STRSAFE_E_INSUFFICIENT_BUFFER;
        }

        *pszDest= L'\0';
    }

    return hr;
}

STRSAFEAPI StringCopyNExWorkerA(char *pszDest, size_t cchDest, size_t cbDest, const char *pszSrc, size_t cchSrc, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    char *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest != 0 || cbDest != 0)
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }

            if (pszSrc == NULL)
                pszSrc = "";
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                pszDestEnd = pszDest;
                cchRemaining = 0;

                if (*pszSrc != '\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                pszDestEnd = pszDest;
                cchRemaining = cchDest;

                while (cchRemaining && cchSrc && *pszSrc != '\0')
                {
                    *pszDestEnd++= *pszSrc++;
                    cchRemaining--;
                    cchSrc--;
                }

                if (cchRemaining > 0)
                {
                    if (dwFlags & STRSAFE_FILL_BEHIND_NULL)
                    {
                        memset(pszDestEnd + 1, STRSAFE_GET_FILL_PATTERN(dwFlags), ((cchRemaining - 1) * sizeof(char)) + (cbDest % sizeof(char)));
                    }
                }
                else
                {
                    
                    pszDestEnd--;
                    cchRemaining++;

                    hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }

                *pszDestEnd = '\0';
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = '\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE | STRSAFE_NO_TRUNCATION))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = '\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringCopyNExWorkerW(wchar_t *pszDest, size_t cchDest, size_t cbDest, const wchar_t *pszSrc, size_t cchSrc, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    wchar_t *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest != 0 || cbDest != 0)
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }

            if (pszSrc == NULL)
                pszSrc = L"";
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                pszDestEnd = pszDest;
                cchRemaining = 0;

                if (*pszSrc != L'\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                pszDestEnd = pszDest;
                cchRemaining = cchDest;

                while (cchRemaining && cchSrc && (*pszSrc != L'\0'))
                {
                    *pszDestEnd++= *pszSrc++;
                    cchRemaining--;
                    cchSrc--;
                }

                if (cchRemaining > 0)
                {
                    if (dwFlags & STRSAFE_FILL_BEHIND_NULL)
                    {
                        memset(pszDestEnd + 1, STRSAFE_GET_FILL_PATTERN(dwFlags), ((cchRemaining - 1) * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t)));
                    }
                }
                else
                {
                    
                    pszDestEnd--;
                    cchRemaining++;
                    hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }

                *pszDestEnd = L'\0';
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = L'\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE | STRSAFE_NO_TRUNCATION))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = L'\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringCatWorkerA(char *pszDest, size_t cchDest, const char *pszSrc)
{
   size_t cchDestCurrent = 0;
   HRESULT hr;

   hr = StringLengthWorkerA(pszDest, cchDest, &cchDestCurrent);

   if (SUCCEEDED(hr))
       hr = StringCopyWorkerA(pszDest + cchDestCurrent, cchDest - cchDestCurrent, pszSrc);

   return hr;
}

STRSAFEAPI StringCatWorkerW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc)
{
   size_t cchDestCurrent = 0;
   HRESULT hr;

   hr = StringLengthWorkerW(pszDest, cchDest, &cchDestCurrent);

   if (SUCCEEDED(hr))
       hr = StringCopyWorkerW(pszDest + cchDestCurrent, cchDest - cchDestCurrent, pszSrc);

   return hr;
}

STRSAFEAPI StringCatExWorkerA(char *pszDest, size_t cchDest, size_t cbDest, const char *pszSrc, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    char *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        size_t cchDestCurrent;

        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest == 0 && cbDest == 0)
                    cchDestCurrent = 0;
                else
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }
            else
            {
                hr = StringLengthWorkerA(pszDest, cchDest, &cchDestCurrent);
                if (SUCCEEDED(hr))
                {
                    pszDestEnd = pszDest + cchDestCurrent;
                    cchRemaining = cchDest - cchDestCurrent;
                }
            }

            if (pszSrc == NULL)
                pszSrc = "";
        }
        else
        {
            hr = StringLengthWorkerA(pszDest, cchDest, &cchDestCurrent);
            if (SUCCEEDED(hr))
            {
                pszDestEnd = pszDest + cchDestCurrent;
                cchRemaining = cchDest - cchDestCurrent;
            }
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                if (*pszSrc != '\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                hr = StringCopyExWorkerA(pszDestEnd, cchRemaining,
                    (cchRemaining * sizeof(char)) + (cbDest % sizeof(char)),
                    pszSrc, &pszDestEnd, &cchRemaining,
                    dwFlags & (~(STRSAFE_FILL_ON_FAILURE | STRSAFE_NULL_ON_FAILURE)));
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = '\0';
                }
            }

            if (dwFlags & STRSAFE_NULL_ON_FAILURE)
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = '\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringCatExWorkerW(wchar_t *pszDest, size_t cchDest, size_t cbDest, const wchar_t *pszSrc, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    wchar_t *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        size_t cchDestCurrent;

        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest == 0 && cbDest == 0)
                    cchDestCurrent = 0;
                else
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }
            else
            {
                hr = StringLengthWorkerW(pszDest, cchDest, &cchDestCurrent);
                if (SUCCEEDED(hr))
                {
                    pszDestEnd = pszDest + cchDestCurrent;
                    cchRemaining = cchDest - cchDestCurrent;
                }
            }

            if (pszSrc == NULL)
                pszSrc = L"";
        }
        else
        {
            hr = StringLengthWorkerW(pszDest, cchDest, &cchDestCurrent);
            if (SUCCEEDED(hr))
            {
                pszDestEnd = pszDest + cchDestCurrent;
                cchRemaining = cchDest - cchDestCurrent;
            }
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                if (*pszSrc != L'\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                hr = StringCopyExWorkerW(pszDestEnd, cchRemaining,
                    (cchRemaining * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t)),
                    pszSrc, &pszDestEnd, &cchRemaining,
                    dwFlags & (~(STRSAFE_FILL_ON_FAILURE | STRSAFE_NULL_ON_FAILURE)));
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = L'\0';
                }
            }

            if (dwFlags & STRSAFE_NULL_ON_FAILURE)
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = L'\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringCatNWorkerA(char *pszDest, size_t cchDest, const char *pszSrc, size_t cchMaxAppend)
{
    size_t cchDestCurrent = 0;
    HRESULT hr;

    hr = StringLengthWorkerA(pszDest, cchDest, &cchDestCurrent);

    if (SUCCEEDED(hr))
        hr = StringCopyNWorkerA(pszDest + cchDestCurrent, cchDest - cchDestCurrent, pszSrc, cchMaxAppend);

    return hr;
}

STRSAFEAPI StringCatNWorkerW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszSrc, size_t cchMaxAppend)
{
    size_t cchDestCurrent = 0;
    HRESULT hr;

    hr = StringLengthWorkerW(pszDest, cchDest, &cchDestCurrent);

    if (SUCCEEDED(hr))
        hr = StringCopyNWorkerW(pszDest + cchDestCurrent, cchDest - cchDestCurrent, pszSrc, cchMaxAppend);

    return hr;
}

STRSAFEAPI StringCatNExWorkerA(char *pszDest, size_t cchDest, size_t cbDest, const char *pszSrc, size_t cchMaxAppend, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    char *pszDestEnd = pszDest;
    size_t cchRemaining = 0;
    size_t cchDestCurrent = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if ((cchDest == 0) && (cbDest == 0))
                    cchDestCurrent = 0;
                else
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }
            else
            {
                hr = StringLengthWorkerA(pszDest, cchDest, &cchDestCurrent);
                if (SUCCEEDED(hr))
                {
                    pszDestEnd = pszDest + cchDestCurrent;
                    cchRemaining = cchDest - cchDestCurrent;
                }
            }

            if (pszSrc == NULL)
                pszSrc = "";
        }
        else
        {
            hr = StringLengthWorkerA(pszDest, cchDest, &cchDestCurrent);
            if (SUCCEEDED(hr))
            {
                pszDestEnd = pszDest + cchDestCurrent;
                cchRemaining = cchDest - cchDestCurrent;
            }
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                if (*pszSrc != '\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                hr = StringCopyNExWorkerA(pszDestEnd, cchRemaining,
                    (cchRemaining * sizeof(char)) + (cbDest % sizeof(char)),
                    pszSrc, cchMaxAppend, &pszDestEnd, &cchRemaining,
                    dwFlags & (~(STRSAFE_FILL_ON_FAILURE | STRSAFE_NULL_ON_FAILURE)));
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = '\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = '\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringCatNExWorkerW(wchar_t *pszDest, size_t cchDest, size_t cbDest, const wchar_t *pszSrc, size_t cchMaxAppend, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    wchar_t *pszDestEnd = pszDest;
    size_t cchRemaining = 0;
    size_t cchDestCurrent = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest == 0 && cbDest == 0)
                    cchDestCurrent = 0;
                else
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }
            else
            {
                hr = StringLengthWorkerW(pszDest, cchDest, &cchDestCurrent);
                if (SUCCEEDED(hr))
                {
                    pszDestEnd = pszDest + cchDestCurrent;
                    cchRemaining = cchDest - cchDestCurrent;
                }
            }

            if (pszSrc == NULL)
                pszSrc = L"";
        }
        else
        {
            hr = StringLengthWorkerW(pszDest, cchDest, &cchDestCurrent);
            if (SUCCEEDED(hr))
            {
                pszDestEnd = pszDest + cchDestCurrent;
                cchRemaining = cchDest - cchDestCurrent;
            }
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                if (*pszSrc != L'\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                hr = StringCopyNExWorkerW(pszDestEnd, cchRemaining,
                    (cchRemaining * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t)),
                    pszSrc, cchMaxAppend, &pszDestEnd, &cchRemaining,
                    dwFlags & (~(STRSAFE_FILL_ON_FAILURE | STRSAFE_NULL_ON_FAILURE)));
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = L'\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = L'\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringVPrintfWorkerA(char *pszDest, size_t cchDest, const char *pszFormat, va_list argList)
{
    HRESULT hr = S_OK;

    if (cchDest == 0)
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        size_t cchMax = cchDest - 1;
        int iRet = vsnprintf(pszDest, cchMax, pszFormat, argList);
        if (iRet < 0 || ((size_t)iRet) > cchMax)
        {
            pszDest += cchMax;
            *pszDest = '\0';

            hr = STRSAFE_E_INSUFFICIENT_BUFFER;
        }
        else if (((size_t)iRet) == cchMax)
        {
            pszDest += cchMax;
            *pszDest = '\0';
        }
    }

    return hr;
}

STRSAFEAPI StringVPrintfWorkerW(wchar_t *pszDest, size_t cchDest, const wchar_t *pszFormat, va_list argList)
{
    HRESULT hr = S_OK;

    if (cchDest == 0)
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        size_t cchMax = cchDest - 1;
        int iRet = _vsnwprintf(pszDest, cchMax, pszFormat, argList);
        if (iRet < 0 || ((size_t)iRet) > cchMax)
        {
            pszDest += cchMax;
            *pszDest = L'\0';

            hr = STRSAFE_E_INSUFFICIENT_BUFFER;
        }
        else if (((size_t)iRet) == cchMax)
        {
            pszDest += cchMax;
            *pszDest = L'\0';
        }
    }

    return hr;
}

STRSAFEAPI StringVPrintfExWorkerA(char *pszDest, size_t cchDest, size_t cbDest, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags, const char *pszFormat, va_list argList)
{
    HRESULT hr = S_OK;
    char *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest != 0 || cbDest != 0)
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }

            if (pszFormat == NULL)
                pszFormat = "";
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                pszDestEnd = pszDest;
                cchRemaining = 0;

                if (*pszFormat != '\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                size_t cchMax = cchDest - 1;
                int iRet = vsnprintf(pszDest, cchMax, pszFormat, argList);
                if (iRet < 0 || ((size_t)iRet) > cchMax)
                {
                    pszDestEnd = pszDest + cchMax;
                    cchRemaining = 1;
                    *pszDestEnd = '\0';

                    hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
                else if (((size_t)iRet) == cchMax)
                {
                    pszDestEnd = pszDest + cchMax;
                    cchRemaining = 1;
                    *pszDestEnd = '\0';
                }
                else if (((size_t)iRet) < cchMax)
                {
                    pszDestEnd = pszDest + iRet;
                    cchRemaining = cchDest - iRet;

                    if (dwFlags & STRSAFE_FILL_BEHIND_NULL)
                    {
                        memset(pszDestEnd + 1, STRSAFE_GET_FILL_PATTERN(dwFlags), ((cchRemaining - 1) * sizeof(char)) + (cbDest % sizeof(char)));
                    }
                }
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = '\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE | STRSAFE_NO_TRUNCATION))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = '\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringVPrintfExWorkerW(wchar_t *pszDest, size_t cchDest, size_t cbDest, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags, const wchar_t *pszFormat, va_list argList)
{
    HRESULT hr = S_OK;
    wchar_t *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest != 0 || cbDest != 0)
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }

            if (pszFormat == NULL)
                pszFormat = L"";
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest == 0)
            {
                pszDestEnd = pszDest;
                cchRemaining = 0;

                if (*pszFormat != L'\0')
                {
                    if (pszDest == NULL)
                        hr = STRSAFE_E_INVALID_PARAMETER;
                    else
                        hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
            }
            else
            {
                size_t cchMax = cchDest - 1;
                int iRet = _vsnwprintf(pszDest, cchMax, pszFormat, argList);
                if (iRet < 0 || ((size_t)iRet) > cchMax)
                {
                    pszDestEnd = pszDest + cchMax;
                    cchRemaining = 1;
                    *pszDestEnd = L'\0';

                    hr = STRSAFE_E_INSUFFICIENT_BUFFER;
                }
                else if (((size_t)iRet) == cchMax)
                {
                    pszDestEnd = pszDest + cchMax;
                    cchRemaining = 1;
                    *pszDestEnd = L'\0';
                }
                else if (((size_t)iRet) < cchMax)
                {
                    pszDestEnd = pszDest + iRet;
                    cchRemaining = cchDest - iRet;

                    if (dwFlags & STRSAFE_FILL_BEHIND_NULL)
                    {
                        memset(pszDestEnd + 1, STRSAFE_GET_FILL_PATTERN(dwFlags), ((cchRemaining - 1) * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t)));
                    }
                }
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = L'\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE | STRSAFE_NO_TRUNCATION))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = L'\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || (hr == STRSAFE_E_INSUFFICIENT_BUFFER))
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFEAPI StringLengthWorkerA(const char *psz, size_t cchMax, size_t *pcch)
{
    HRESULT hr = S_OK;
    size_t cchMaxPrev = cchMax;

    while (cchMax && *psz != '\0')
    {
        psz++;
        cchMax--;
    }

    if (cchMax == 0)
        hr = STRSAFE_E_INVALID_PARAMETER;

    if (SUCCEEDED(hr) && pcch)
        *pcch = cchMaxPrev - cchMax;

    return hr;
}

STRSAFEAPI StringLengthWorkerW(const wchar_t *psz, size_t cchMax, size_t *pcch)
{
    HRESULT hr = S_OK;
    size_t cchMaxPrev = cchMax;

    while (cchMax && (*psz != L'\0'))
    {
        psz++;
        cchMax--;
    }

    if (cchMax == 0)
        hr = STRSAFE_E_INVALID_PARAMETER;

    if (SUCCEEDED(hr) && pcch)
        *pcch = cchMaxPrev - cchMax;

    return hr;
}
#endif /* STRSAFE_INLINE */


#ifndef STRSAFE_LIB_IMPL
STRSAFE_INLINE_API StringGetsExWorkerA(char *pszDest, size_t cchDest, size_t cbDest, char **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    char *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest != 0 || cbDest != 0)
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest <= 1)
            {
                pszDestEnd = pszDest;
                cchRemaining = cchDest;

                if (cchDest == 1)
                    *pszDestEnd = '\0';

                hr = STRSAFE_E_INSUFFICIENT_BUFFER;
            }
            else
            {
                char ch;

                pszDestEnd = pszDest;
                cchRemaining = cchDest;

                while (cchRemaining > 1 && (ch = (char)getc(stdin)) != '\n')
                {
                    if (ch == EOF)
                    {
                        if (pszDestEnd == pszDest)
                            hr = STRSAFE_E_END_OF_FILE;
                        break;
                    }

                    *pszDestEnd = ch;

                    pszDestEnd++;
                    cchRemaining--;
                }

                if (cchRemaining > 0)
                {
                    if (dwFlags & STRSAFE_FILL_BEHIND_NULL)
                    {
                        memset(pszDestEnd + 1, STRSAFE_GET_FILL_PATTERN(dwFlags), ((cchRemaining - 1) * sizeof(char)) + (cbDest % sizeof(char)));
                    }
                }

                *pszDestEnd = '\0';
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = '\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE | STRSAFE_NO_TRUNCATION))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = '\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER || hr == STRSAFE_E_END_OF_FILE)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

STRSAFE_INLINE_API StringGetsExWorkerW(wchar_t *pszDest, size_t cchDest, size_t cbDest, wchar_t **ppszDestEnd, size_t *pcchRemaining, unsigned long dwFlags)
{
    HRESULT hr = S_OK;
    wchar_t *pszDestEnd = pszDest;
    size_t cchRemaining = 0;

    if (dwFlags & (~STRSAFE_VALID_FLAGS))
    {
        hr = STRSAFE_E_INVALID_PARAMETER;
    }
    else
    {
        if (dwFlags & STRSAFE_IGNORE_NULLS)
        {
            if (pszDest == NULL)
            {
                if (cchDest != 0 || cbDest != 0)
                    hr = STRSAFE_E_INVALID_PARAMETER;
            }
        }

        if (SUCCEEDED(hr))
        {
            if (cchDest <= 1)
            {
                pszDestEnd = pszDest;
                cchRemaining = cchDest;

                if (cchDest == 1)
                    *pszDestEnd = L'\0';

                hr = STRSAFE_E_INSUFFICIENT_BUFFER;
            }
            else
            {
                wchar_t ch;

                pszDestEnd = pszDest;
                cchRemaining = cchDest;

                while ((cchRemaining > 1) && (ch = (wchar_t)getwc(stdin)) != L'\n')
                {
                    if (ch == EOF)
                    {
                        if (pszDestEnd == pszDest)
                            hr = STRSAFE_E_END_OF_FILE;
                        break;
                    }

                    *pszDestEnd = ch;

                    pszDestEnd++;
                    cchRemaining--;
                }

                if (cchRemaining > 0)
                {
                    
                    if (dwFlags & STRSAFE_FILL_BEHIND_NULL)
                    {
                        memset(pszDestEnd + 1, STRSAFE_GET_FILL_PATTERN(dwFlags), ((cchRemaining - 1) * sizeof(wchar_t)) + (cbDest % sizeof(wchar_t)));
                    }
                }

                *pszDestEnd = L'\0';
            }
        }
    }

    if (FAILED(hr))
    {
        if (pszDest)
        {
            if (dwFlags & STRSAFE_FILL_ON_FAILURE)
            {
                memset(pszDest, STRSAFE_GET_FILL_PATTERN(dwFlags), cbDest);

                if (STRSAFE_GET_FILL_PATTERN(dwFlags) == 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                }
                else if (cchDest > 0)
                {
                    pszDestEnd = pszDest + cchDest - 1;
                    cchRemaining = 1;
                    *pszDestEnd = L'\0';
                }
            }

            if (dwFlags & (STRSAFE_NULL_ON_FAILURE | STRSAFE_NO_TRUNCATION))
            {
                if (cchDest > 0)
                {
                    pszDestEnd = pszDest;
                    cchRemaining = cchDest;
                    *pszDestEnd = L'\0';
                }
            }
        }
    }

    if (SUCCEEDED(hr) || hr == STRSAFE_E_INSUFFICIENT_BUFFER || hr == STRSAFE_E_END_OF_FILE)
    {
        if (ppszDestEnd)
            *ppszDestEnd = pszDestEnd;

        if (pcchRemaining)
            *pcchRemaining = cchRemaining;
    }

    return hr;
}

#define StringCopyWorkerA  StringCopyWorkerA_instead_use_StringCchCopyA_or_StringCchCopyExA;
#define StringCopyWorkerW  StringCopyWorkerW_instead_use_StringCchCopyW_or_StringCchCopyExW;
#define StringCopyExWorkerA  StringCopyExWorkerA_instead_use_StringCchCopyA_or_StringCchCopyExA;
#define StringCopyExWorkerW  StringCopyExWorkerW_instead_use_StringCchCopyW_or_StringCchCopyExW;
#define StringCatWorkerA  StringCatWorkerA_instead_use_StringCchCatA_or_StringCchCatExA;
#define StringCatWorkerW  StringCatWorkerW_instead_use_StringCchCatW_or_StringCchCatExW;
#define StringCatExWorkerA  StringCatExWorkerA_instead_use_StringCchCatA_or_StringCchCatExA;
#define StringCatExWorkerW  StringCatExWorkerW_instead_use_StringCchCatW_or_StringCchCatExW;
#define StringCatNWorkerA  StringCatNWorkerA_instead_use_StringCchCatNA_or_StrincCbCatNA;
#define StringCatNWorkerW  StringCatNWorkerW_instead_use_StringCchCatNW_or_StringCbCatNW;
#define StringCatNExWorkerA  StringCatNExWorkerA_instead_use_StringCchCatNExA_or_StringCbCatNExA;
#define StringCatNExWorkerW  StringCatNExWorkerW_instead_use_StringCchCatNExW_or_StringCbCatNExW;
#define StringVPrintfWorkerA  StringVPrintfWorkerA_instead_use_StringCchVPrintfA_or_StringCchVPrintfExA;
#define StringVPrintfWorkerW  StringVPrintfWorkerW_instead_use_StringCchVPrintfW_or_StringCchVPrintfExW;
#define StringVPrintfExWorkerA  StringVPrintfExWorkerA_instead_use_StringCchVPrintfA_or_StringCchVPrintfExA;
#define StringVPrintfExWorkerW  StringVPrintfExWorkerW_instead_use_StringCchVPrintfW_or_StringCchVPrintfExW;
#define StringLengthWorkerA  StringLengthWorkerA_instead_use_StringCchLengthA_or_StringCbLengthA;
#define StringLengthWorkerW  StringLengthWorkerW_instead_use_StringCchLengthW_or_StringCbLengthW;

#ifndef STRSAFE_NO_DEPRECATE

#undef strcpy
#define strcpy  strcpy_instead_use_StringCbCopyA_or_StringCchCopyA;

#undef wcscpy
#define wcscpy  wcscpy_instead_use_StringCbCopyW_or_StringCchCopyW;

#undef strcat
#define strcat  strcat_instead_use_StringCbCatA_or_StringCchCatA;

#undef wcscat
#define wcscat  wcscat_instead_use_StringCbCatW_or_StringCchCatW;

#undef sprintf
#define sprintf  sprintf_instead_use_StringCbPrintfA_or_StringCchPrintfA;

#undef swprintf
#define swprintf  swprintf_instead_use_StringCbPrintfW_or_StringCchPrintfW;

#undef vsprintf
#define vsprintf  vsprintf_instead_use_StringCbVPrintfA_or_StringCchVPrintfA;

#undef vswprintf
#define vswprintf  vswprintf_instead_use_StringCbVPrintfW_or_StringCchVPrintfW;

#undef snprintf
#define snprintf  snprintf_instead_use_StringCbPrintfA_or_StringCchPrintfA;

#undef swprintf
#define swprintf  swprintf_instead_use_StringCbPrintfW_or_StringCchPrintfW;

#undef vsnprintf
#define vsnprintf  vsnprintf_instead_use_StringCbVPrintfA_or_StringCchVPrintfA;

#undef _vsnwprintf
#define _vsnwprintf  _vsnwprintf_instead_use_StringCbVPrintfW_or_StringCchVPrintfW;

#undef strcpyA
#define strcpyA  strcpyA_instead_use_StringCbCopyA_or_StringCchCopyA;

#undef strcpyW
#define strcpyW  strcpyW_instead_use_StringCbCopyW_or_StringCchCopyW;

#undef lstrcpy
#define lstrcpy  lstrcpy_instead_use_StringCbCopy_or_StringCchCopy;

#undef lstrcpyA
#define lstrcpyA  lstrcpyA_instead_use_StringCbCopyA_or_StringCchCopyA;

#undef lstrcpyW
#define lstrcpyW  lstrcpyW_instead_use_StringCbCopyW_or_StringCchCopyW;

#undef StrCpy
#define StrCpy  StrCpy_instead_use_StringCbCopy_or_StringCchCopy;

#undef StrCpyA
#define StrCpyA  StrCpyA_instead_use_StringCbCopyA_or_StringCchCopyA;

#undef StrCpyW
#define StrCpyW  StrCpyW_instead_use_StringCbCopyW_or_StringCchCopyW;

#undef _tcscpy
#define _tcscpy  _tcscpy_instead_use_StringCbCopy_or_StringCchCopy;

#undef _ftcscpy
#define _ftcscpy  _ftcscpy_instead_use_StringCbCopy_or_StringCchCopy;

#undef lstrcat
#define lstrcat  lstrcat_instead_use_StringCbCat_or_StringCchCat;

#undef lstrcatA
#define lstrcatA  lstrcatA_instead_use_StringCbCatA_or_StringCchCatA;

#undef lstrcatW
#define lstrcatW  lstrcatW_instead_use_StringCbCatW_or_StringCchCatW;

#undef StrCat
#define StrCat  StrCat_instead_use_StringCbCat_or_StringCchCat;

#undef StrCatA
#define StrCatA  StrCatA_instead_use_StringCbCatA_or_StringCchCatA;

#undef StrCatW
#define StrCatW  StrCatW_instead_use_StringCbCatW_or_StringCchCatW;

#undef StrNCat
#define StrNCat  StrNCat_instead_use_StringCbCatN_or_StringCchCatN;

#undef StrNCatA
#define StrNCatA  StrNCatA_instead_use_StringCbCatNA_or_StringCchCatNA;

#undef StrNCatW
#define StrNCatW  StrNCatW_instead_use_StringCbCatNW_or_StringCchCatNW;

#undef StrCatN
#define StrCatN  StrCatN_instead_use_StringCbCatN_or_StringCchCatN;

#undef StrCatNA
#define StrCatNA  StrCatNA_instead_use_StringCbCatNA_or_StringCchCatNA;

#undef StrCatNW
#define StrCatNW  StrCatNW_instead_use_StringCbCatNW_or_StringCchCatNW;

#undef _tcscat
#define _tcscat  _tcscat_instead_use_StringCbCat_or_StringCchCat;

#undef _ftcscat
#define _ftcscat  _ftcscat_instead_use_StringCbCat_or_StringCchCat;

#undef wsprintf
#define wsprintf  wsprintf_instead_use_StringCbPrintf_or_StringCchPrintf;

#undef wsprintfA
#define wsprintfA  wsprintfA_instead_use_StringCbPrintfA_or_StringCchPrintfA;

#undef wsprintfW
#define wsprintfW  wsprintfW_instead_use_StringCbPrintfW_or_StringCchPrintfW;

#undef wvsprintf
#define wvsprintf  wvsprintf_instead_use_StringCbVPrintf_or_StringCchVPrintf;

#undef wvsprintfA
#define wvsprintfA  wvsprintfA_instead_use_StringCbVPrintfA_or_StringCchVPrintfA;

#undef wvsprintfW
#define wvsprintfW  wvsprintfW_instead_use_StringCbVPrintfW_or_StringCchVPrintfW;

#undef _vstprintf
#define _vstprintf  _vstprintf_instead_use_StringCbVPrintf_or_StringCchVPrintf;

#undef _vsntprintf
#define _vsntprintf  _vsntprintf_instead_use_StringCbVPrintf_or_StringCchVPrintf;

#undef _stprintf
#define _stprintf  _stprintf_instead_use_StringCbPrintf_or_StringCchPrintf;

#undef _sntprintf
#define _sntprintf  _sntprintf_instead_use_StringCbPrintf_or_StringCchPrintf;

#undef _getts
#define _getts  _getts_instead_use_StringCbGets_or_StringCchGets;

#undef gets
#define gets  _gets_instead_use_StringCbGetsA_or_StringCchGetsA;

#undef _getws
#define _getws  _getws_instead_use_StringCbGetsW_or_StringCchGetsW;

#endif /* STRSAFE_NO_DEPRECATE */
#endif /* STRSAFE_LIB_IMPL */

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#endif /* _STRSAFE_H */
