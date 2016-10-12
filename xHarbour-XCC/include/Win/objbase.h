#include <rpc.h>
#include <rpcndr.h>

#ifndef _OBJBASE_H
#define _OBJBASE_H

/* Windows Component Object Model defintions */

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2109)  /* More than x external identifiers */
#pragma warn(disable:2028)  /* Missing prototype */
#endif

#include <pshpack8.h>

#define WINOLEAPI EXTERN_C DECLSPEC_IMPORT HRESULT STDAPICALLTYPE
#define WINOLEAPI_(type) EXTERN_C DECLSPEC_IMPORT type STDAPICALLTYPE

#if defined(__cplusplus) && !defined(CINTERFACE)

#define interface struct
#define STDMETHOD(method)  virtual HRESULT STDMETHODCALLTYPE method
#define STDMETHOD_(type,method)  virtual type STDMETHODCALLTYPE method
#define STDMETHODV(method)  virtual HRESULT STDMETHODVCALLTYPE method
#define STDMETHODV_(type,method)  virtual type STDMETHODVCALLTYPE method
#define PURE  = 0
#define THIS_
#define THIS  void
#define DECLARE_INTERFACE(iface)  interface iface
#define DECLARE_INTERFACE_(iface,baseiface)  interface iface : public baseiface

#ifndef BEGIN_INTERFACE
#define BEGIN_INTERFACE
#define END_INTERFACE
#endif

#else /* __cplusplus && !CINTERFACE */

#define interface struct
#define STDMETHOD(method)  HRESULT (STDMETHODCALLTYPE *method)
#define STDMETHOD_(type,method)  type (STDMETHODCALLTYPE *method)
#define STDMETHODV(method)  HRESULT (STDMETHODVCALLTYPE *method)
#define STDMETHODV_(type,method)  type (STDMETHODVCALLTYPE *method)
#define PURE
#define THIS_  INTERFACE *This,
#define THIS  INTERFACE *This

#ifdef CONST_VTABLE
#undef CONST_VTBL
#define CONST_VTBL const
#define DECLARE_INTERFACE(iface)  typedef interface iface { const struct iface##Vtbl *lpVtbl; } iface; typedef const struct iface##Vtbl iface##Vtbl; const struct iface##Vtbl
#else
#undef CONST_VTBL
#define CONST_VTBL
#define DECLARE_INTERFACE(iface)  typedef interface iface { struct iface##Vtbl *lpVtbl; } iface; typedef struct iface##Vtbl iface##Vtbl; struct iface##Vtbl
#endif
#define DECLARE_INTERFACE_(iface,baseiface)  DECLARE_INTERFACE(iface)

#ifndef BEGIN_INTERFACE
#define BEGIN_INTERFACE
#define END_INTERFACE
#endif

#endif /* __cplusplus && !CINTERFACE */

#ifndef FARSTRUCT
#define FARSTRUCT
#endif

#ifndef HUGEP
#define HUGEP
#endif

#include <stdlib.h>

#define LISet32(li,v)   ((li).HighPart = (v) < 0 ? -1 : 0, (li).LowPart = (v))
#define ULISet32(li,v)  ((li).HighPart = 0,(li).LowPart = (v))

#define CLSCTX_INPROC  (CLSCTX_INPROC_SERVER|CLSCTX_INPROC_HANDLER)
#if (_WIN32_WINNT >= 0x0400) || defined(_WIN32_DCOM)
#define CLSCTX_ALL  (CLSCTX_INPROC_SERVER|CLSCTX_INPROC_HANDLER|CLSCTX_LOCAL_SERVER|CLSCTX_REMOTE_SERVER)
#define CLSCTX_SERVER  (CLSCTX_INPROC_SERVER|CLSCTX_LOCAL_SERVER|CLSCTX_REMOTE_SERVER)
#else
#define CLSCTX_ALL  (CLSCTX_INPROC_SERVER|CLSCTX_INPROC_HANDLER|CLSCTX_LOCAL_SERVER)
#define CLSCTX_SERVER  (CLSCTX_INPROC_SERVER|CLSCTX_LOCAL_SERVER)
#endif

#define MARSHALINTERFACE_MIN  500

#define CWCSTORAGENAME  32

#define STGM_DIRECT  0x00000000L
#define STGM_TRANSACTED  0x00010000L
#define STGM_SIMPLE  0x08000000L
#define STGM_READ  0x00000000L
#define STGM_WRITE  0x00000001L
#define STGM_READWRITE  0x00000002L
#define STGM_SHARE_DENY_NONE  0x00000040L
#define STGM_SHARE_DENY_READ  0x00000030L
#define STGM_SHARE_DENY_WRITE  0x00000020L
#define STGM_SHARE_EXCLUSIVE  0x00000010L
#define STGM_PRIORITY  0x00040000L
#define STGM_DELETEONRELEASE  0x04000000L
#define STGM_NOSCRATCH  0x00100000L
#define STGM_CREATE  0x00001000L
#define STGM_CONVERT  0x00020000L
#define STGM_FAILIFTHERE  0x00000000L
#define STGM_NOSNAPSHOT  0x00200000L
#if (_WIN32_WINNT >= 0x0500)
#define STGM_DIRECT_SWMR  0x00400000L
#endif

#define ASYNC_MODE_COMPATIBILITY  0x00000001L
#define ASYNC_MODE_DEFAULT  0x00000000L

#define STGTY_REPEAT  0x00000100L
#define STG_TOEND  0xFFFFFFFFL

#define STG_LAYOUT_SEQUENTIAL  0x00000000L
#define STG_LAYOUT_INTERLEAVED  0x00000001L

#define STGFMT_STORAGE  0
#define STGFMT_NATIVE  1
#define STGFMT_FILE  3
#define STGFMT_ANY  4
#define STGFMT_DOCFILE  5

#define STGFMT_DOCUMENT  0

#if (_WIN32_WINNT >= 0x0400) || defined(_WIN32_DCOM)
#define COM_RIGHTS_EXECUTE  1
#define COM_RIGHTS_SAFE_FOR_SCRIPTING  2
#endif

typedef enum tagREGCLS {
    REGCLS_SINGLEUSE = 0,
    REGCLS_MULTIPLEUSE = 1,
    REGCLS_MULTI_SEPARATE = 2,
    REGCLS_SUSPENDED = 4,
    REGCLS_SURROGATE = 8
} REGCLS;

typedef interface IRpcStubBuffer IRpcStubBuffer;
typedef interface IRpcChannelBuffer IRpcChannelBuffer;

#include <wtypes.h>
#include <unknwn.h>
#include <objidl.h>
#include <guiddef.h>
#ifndef INITGUID
#include <cguid.h>
#endif

typedef enum tagCOINIT {
    COINIT_APARTMENTTHREADED=0x2,
#if (_WIN32_WINNT >= 0x0400) || defined(_WIN32_DCOM)
    COINIT_MULTITHREADED=0x0,
    COINIT_DISABLE_OLE1DDE=0x4,
    COINIT_SPEED_OVER_MEMORY=0x8,
#endif
} COINIT;

typedef enum tagSTDMSHLFLAGS {
    SMEXF_SERVER=0x01,
    SMEXF_HANDLER=0x02
} STDMSHLFLAGS;

#define STGOPTIONS_VERSION  1

typedef struct tagSTGOPTIONS {
    USHORT usVersion;
    USHORT reserved;
    ULONG ulSectorSize;
} STGOPTIONS;

#if (_WIN32_WINNT >= 0x0400) || defined(_WIN32_DCOM)
typedef enum tagCOWAIT_FLAGS {
    COWAIT_WAITALL=1,
    COWAIT_ALERTABLE=2
} COWAIT_FLAGS;
#endif /* (_WIN32_WINNT >= 0x0400) || defined(_WIN32_DCOM) */

typedef HRESULT (STDAPICALLTYPE *LPFNGETCLASSOBJECT)(REFCLSID,REFIID,LPVOID*);
typedef HRESULT (STDAPICALLTYPE *LPFNCANUNLOADNOW)(void);

WINOLEAPI_(DWORD) CoBuildVersion(VOID);
WINOLEAPI CoInitialize(LPVOID);
WINOLEAPI_(void) CoUninitialize(void);
WINOLEAPI CoGetMalloc(DWORD,LPMALLOC*);
WINOLEAPI_(DWORD) CoGetCurrentProcess(void);
WINOLEAPI CoRegisterMallocSpy(LPMALLOCSPY);
WINOLEAPI CoRevokeMallocSpy(void);
WINOLEAPI CoCreateStandardMalloc(DWORD,IMalloc**);
WINOLEAPI CoGetObjectContext(REFIID,LPVOID*);
WINOLEAPI CoGetClassObject(REFCLSID,DWORD,LPVOID,REFIID,LPVOID*);
WINOLEAPI CoRegisterClassObject(REFCLSID,LPUNKNOWN,DWORD,DWORD,LPDWORD);
WINOLEAPI CoRevokeClassObject(DWORD);
WINOLEAPI CoResumeClassObjects(void);
WINOLEAPI CoSuspendClassObjects(void);
WINOLEAPI_(ULONG) CoAddRefServerProcess(void);
WINOLEAPI_(ULONG) CoReleaseServerProcess(void);
WINOLEAPI CoGetPSClsid(REFIID,CLSID*);
WINOLEAPI CoRegisterPSClsid(REFIID,REFCLSID);
WINOLEAPI  CoRegisterSurrogate(LPSURROGATE);
WINOLEAPI CoGetMarshalSizeMax(ULONG*,REFIID,LPUNKNOWN,DWORD,LPVOID,DWORD);
WINOLEAPI CoMarshalInterface(LPSTREAM,REFIID,LPUNKNOWN,DWORD,LPVOID,DWORD);
WINOLEAPI CoUnmarshalInterface(LPSTREAM,REFIID,LPVOID*);
WINOLEAPI CoMarshalHresult(LPSTREAM,HRESULT);
WINOLEAPI CoUnmarshalHresult(LPSTREAM,HRESULT*);
WINOLEAPI CoReleaseMarshalData(LPSTREAM);
WINOLEAPI CoDisconnectObject(LPUNKNOWN,DWORD);
WINOLEAPI CoLockObjectExternal(LPUNKNOWN,BOOL,BOOL);
WINOLEAPI CoGetStandardMarshal(REFIID,LPUNKNOWN,DWORD,LPVOID,DWORD,LPMARSHAL*);
WINOLEAPI CoGetStdMarshalEx(LPUNKNOWN,DWORD,LPUNKNOWN*);
WINOLEAPI_(BOOL) CoIsHandlerConnected(LPUNKNOWN);
WINOLEAPI CoMarshalInterThreadInterfaceInStream(REFIID,LPUNKNOWN,LPSTREAM*);
WINOLEAPI CoGetInterfaceAndReleaseStream(LPSTREAM,REFIID,LPVOID*);
WINOLEAPI CoCreateFreeThreadedMarshaler(LPUNKNOWN,LPUNKNOWN*);
WINOLEAPI_(HINSTANCE) CoLoadLibrary(LPOLESTR,BOOL);
WINOLEAPI_(void) CoFreeLibrary(HINSTANCE);
WINOLEAPI_(void) CoFreeAllLibraries(void);
WINOLEAPI_(void) CoFreeUnusedLibraries(void);
WINOLEAPI CoCreateInstance(REFCLSID,LPUNKNOWN,DWORD,REFIID,LPVOID*);
WINOLEAPI StringFromCLSID(REFCLSID,LPOLESTR*);
WINOLEAPI CLSIDFromString(LPOLESTR,LPCLSID);
WINOLEAPI StringFromIID(REFIID,LPOLESTR*);
WINOLEAPI IIDFromString(LPOLESTR,LPIID);
WINOLEAPI_(BOOL) CoIsOle1Class(REFCLSID);
WINOLEAPI ProgIDFromCLSID (REFCLSID,LPOLESTR*);
WINOLEAPI CLSIDFromProgID (LPCOLESTR,LPCLSID);
WINOLEAPI CLSIDFromProgIDEx (LPCOLESTR,LPCLSID);
WINOLEAPI_(int) StringFromGUID2(REFGUID,LPOLESTR,int);
WINOLEAPI CoCreateGuid(GUID*);
WINOLEAPI_(BOOL) CoFileTimeToDosDateTime(FILETIME*,LPWORD,LPWORD);
WINOLEAPI_(BOOL) CoDosDateTimeToFileTime(WORD,WORD,FILETIME*);
WINOLEAPI CoFileTimeNow(FILETIME*);
WINOLEAPI CoRegisterMessageFilter(LPMESSAGEFILTER,LPMESSAGEFILTER*);
WINOLEAPI CoGetTreatAsClass(REFCLSID clsidOld, LPCLSID pClsidNew);
WINOLEAPI CoTreatAsClass(REFCLSID clsidOld, REFCLSID clsidNew);
WINOLEAPI_(LPVOID) CoTaskMemAlloc(SIZE_T);
WINOLEAPI_(LPVOID) CoTaskMemRealloc(LPVOID,SIZE_T);
WINOLEAPI_(void) CoTaskMemFree(LPVOID);
WINOLEAPI CreateDataAdviseHolder(LPDATAADVISEHOLDER*);
WINOLEAPI CreateDataCache(LPUNKNOWN,REFCLSID,REFIID,LPVOID*);
WINOLEAPI StgCreateDocfile(const OLECHAR*,DWORD,DWORD,IStorage**);
WINOLEAPI StgCreateDocfileOnILockBytes(ILockBytes*,DWORD,DWORD,IStorage**);
WINOLEAPI StgOpenStorage(const OLECHAR*,IStorage*,DWORD,SNB,DWORD,IStorage**);
WINOLEAPI StgOpenStorageOnILockBytes(ILockBytes*,IStorage*,DWORD,SNB,DWORD,IStorage**);
WINOLEAPI StgIsStorageFile(const OLECHAR*);
WINOLEAPI StgIsStorageILockBytes(ILockBytes*);
WINOLEAPI StgSetTimes(OLECHAR const*,FILETIME const*,FILETIME const*,FILETIME const*);
WINOLEAPI StgOpenAsyncDocfileOnIFillLockBytes(IFillLockBytes*,DWORD,DWORD,IStorage**);
WINOLEAPI StgGetIFillLockBytesOnILockBytes(ILockBytes*,IFillLockBytes**);
WINOLEAPI StgGetIFillLockBytesOnFile(OLECHAR const*,IFillLockBytes**);
WINOLEAPI StgOpenLayoutDocfile(OLECHAR const *,DWORD,DWORD,IStorage**);
WINOLEAPI StgCreateStorageEx(const WCHAR*,DWORD,DWORD,DWORD,STGOPTIONS*,void*,REFIID,void**);
WINOLEAPI StgOpenStorageEx(const WCHAR*,DWORD,DWORD,DWORD,STGOPTIONS*,void*,REFIID,void**);
WINOLEAPI BindMoniker(LPMONIKER,DWORD,REFIID,LPVOID*);
WINOLEAPI CoInstall(IBindCtx*,DWORD,uCLSSPEC*,QUERYCONTEXT*,LPWSTR);
WINOLEAPI CoGetObject(LPCWSTR,BIND_OPTS*,REFIID,void**);
WINOLEAPI MkParseDisplayName(LPBC,LPCOLESTR,ULONG*,LPMONIKER*);
WINOLEAPI MonikerRelativePathTo(LPMONIKER,LPMONIKER,LPMONIKER*,BOOL);
WINOLEAPI MonikerCommonPrefixWith(LPMONIKER,LPMONIKER,LPMONIKER*);
WINOLEAPI CreateBindCtx(DWORD,LPBC*);
WINOLEAPI CreateGenericComposite(LPMONIKER,LPMONIKER,LPMONIKER*);
WINOLEAPI GetClassFile (LPCOLESTR,CLSID*);
WINOLEAPI CreateClassMoniker(REFCLSID,LPMONIKER*);
WINOLEAPI CreateFileMoniker(LPCOLESTR,LPMONIKER*);
WINOLEAPI CreateItemMoniker(LPCOLESTR,LPCOLESTR,LPMONIKER*);
WINOLEAPI CreateAntiMoniker(LPMONIKER*);
WINOLEAPI CreatePointerMoniker(LPUNKNOWN,LPMONIKER*);
WINOLEAPI CreateObjrefMoniker(LPUNKNOWN,LPMONIKER*);
WINOLEAPI GetRunningObjectTable(DWORD,LPRUNNINGOBJECTTABLE*);

#if (_WIN32_WINNT >= 0x0400) || defined(_WIN32_DCOM)
WINOLEAPI CoInitializeEx(LPVOID,DWORD);
WINOLEAPI CoInitializeSecurity(PSECURITY_DESCRIPTOR,LONG,SOLE_AUTHENTICATION_SERVICE*,void*,DWORD,DWORD,void*,DWORD,void*);
WINOLEAPI CoGetCallContext(REFIID,void**);
WINOLEAPI CoQueryProxyBlanket(IUnknown*,DWORD*,DWORD*,OLECHAR**,DWORD*,DWORD*,RPC_AUTH_IDENTITY_HANDLE*,DWORD*);
WINOLEAPI CoSetProxyBlanket(IUnknown*,DWORD,DWORD,OLECHAR*,DWORD,DWORD,RPC_AUTH_IDENTITY_HANDLE,DWORD);
WINOLEAPI CoCopyProxy(IUnknown*,IUnknown**);
WINOLEAPI CoQueryClientBlanket(DWORD*,DWORD*,OLECHAR**,DWORD*,DWORD*,RPC_AUTHZ_HANDLE*,DWORD*);
WINOLEAPI CoImpersonateClient();
WINOLEAPI CoRevertToSelf();
WINOLEAPI CoQueryAuthenticationServices(DWORD*,SOLE_AUTHENTICATION_SERVICE**);
WINOLEAPI CoSwitchCallContext(IUnknown*,IUnknown**);
WINOLEAPI CoGetInstanceFromFile(COSERVERINFO*,CLSID*,IUnknown*,DWORD,DWORD,OLECHAR*,DWORD,MULTI_QI*);
WINOLEAPI CoGetInstanceFromIStorage(COSERVERINFO*,CLSID*,IUnknown*,DWORD,struct IStorage*,DWORD,MULTI_QI*);
WINOLEAPI CoCreateInstanceEx(REFCLSID,IUnknown*,DWORD,COSERVERINFO*,DWORD,MULTI_QI*);
WINOLEAPI CoRegisterChannelHook(REFGUID,IChannelHook*);
WINOLEAPI CoWaitForMultipleHandles (DWORD,DWORD,ULONG,LPHANDLE,LPDWORD);
#endif /* (_WIN32_WINNT >= 0x0400) || defined(_WIN32_DCOM) */

#if (_WIN32_WINNT >= 0x0500) || defined(_WIN32_DCOM)
WINOLEAPI CoGetCancelObject(DWORD,REFIID,void**);
WINOLEAPI CoSetCancelObject(IUnknown*);
WINOLEAPI CoCancelCall(DWORD,ULONG);
WINOLEAPI CoTestCancel();
WINOLEAPI CoEnableCallCancellation(LPVOID);
WINOLEAPI CoDisableCallCancellation(LPVOID);
WINOLEAPI CoAllowSetForegroundWindow(IUnknown*,LPVOID);
#endif /* (_WIN32_WINNT >= 0x0500) || defined(_WIN32_DCOM) */

#if (_WIN32_WINNT >= 0x0501)
WINOLEAPI_(void) CoFreeUnusedLibrariesEx(DWORD,DWORD);
#endif /* (_WIN32_WINNT >= 0x0501) */

STDAPI DllGetClassObject(REFCLSID,REFIID,LPVOID*);
STDAPI DllCanUnloadNow(void);

#include <urlmon.h>
#include <propidl.h>

WINOLEAPI CreateStdProgressIndicator(HWND,LPCOLESTR,IBindStatusCallback*,IBindStatusCallback**);

#include <poppack.h>

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#endif /* _OBJBASE_H */

