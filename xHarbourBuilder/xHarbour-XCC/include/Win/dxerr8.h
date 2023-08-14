#ifndef _DXERR8_H
#define _DXERR8_H

/* DirectX Error Library definitions */

#ifdef __cplusplus
extern "C" {
#endif

#if defined(DEBUG) || defined(_DEBUG)
#define DXTRACE_MSG(str)  DXTrace(__FILE__,(DWORD)__LINE__,0,str,FALSE)
#define DXTRACE_ERR(str,hr)  DXTrace(__FILE__,(DWORD)__LINE__,hr,str,TRUE)
#define DXTRACE_ERR_NOMSGBOX(str,hr)  DXTrace(__FILE__,(DWORD)__LINE__,hr,str,FALSE)
#else
#define DXTRACE_MSG(str)  (0L)
#define DXTRACE_ERR(str,hr)  (hr)
#define DXTRACE_ERR_NOMSGBOX(str,hr)  (hr)
#endif

const char* WINAPI DXGetErrorString8A(HRESULT);
const WCHAR* WINAPI DXGetErrorString8W(HRESULT);

const char* WINAPI DXGetErrorDescription8A(HRESULT);
const WCHAR* WINAPI DXGetErrorDescription8W(HRESULT);

HRESULT WINAPI DXTraceA(char*,DWORD,HRESULT,char*,BOOL);
HRESULT WINAPI DXTraceW(char*,DWORD,HRESULT,WCHAR*,BOOL);

#ifdef UNICODE
#define DXGetErrorString8 DXGetErrorString8W
#define DXGetErrorDescription8 DXGetErrorDescription8W
#define DXTrace DXTraceW
#else
#define DXGetErrorString8 DXGetErrorString8A
#define DXGetErrorDescription8 DXGetErrorDescription8A
#define DXTrace DXTraceA
#endif

#ifdef __cplusplus
}
#endif

#endif /* _DXERR8_H */

