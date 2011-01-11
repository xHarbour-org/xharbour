#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 475
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H */

#ifndef _OBJIDL_H
#define _OBJIDL_H

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2109)  /* More than x external identifiers */
#pragma warn(disable:2198)  /* Nameless field is not standard */
#endif

#ifndef __IMarshal_FWD_DEFINED__
#define __IMarshal_FWD_DEFINED__
typedef interface IMarshal IMarshal;
#endif

#ifndef __IMarshal2_FWD_DEFINED__
#define __IMarshal2_FWD_DEFINED__
typedef interface IMarshal2 IMarshal2;
#endif

#ifndef __IMalloc_FWD_DEFINED__
#define __IMalloc_FWD_DEFINED__
typedef interface IMalloc IMalloc;
#endif

#ifndef __IMallocSpy_FWD_DEFINED__
#define __IMallocSpy_FWD_DEFINED__
typedef interface IMallocSpy IMallocSpy;
#endif

#ifndef __IStdMarshalInfo_FWD_DEFINED__
#define __IStdMarshalInfo_FWD_DEFINED__
typedef interface IStdMarshalInfo IStdMarshalInfo;
#endif

#ifndef __IExternalConnection_FWD_DEFINED__
#define __IExternalConnection_FWD_DEFINED__
typedef interface IExternalConnection IExternalConnection;
#endif

#ifndef __IMultiQI_FWD_DEFINED__
#define __IMultiQI_FWD_DEFINED__
typedef interface IMultiQI IMultiQI;
#endif

#ifndef __AsyncIMultiQI_FWD_DEFINED__
#define __AsyncIMultiQI_FWD_DEFINED__
typedef interface AsyncIMultiQI AsyncIMultiQI;
#endif

#ifndef __IInternalUnknown_FWD_DEFINED__
#define __IInternalUnknown_FWD_DEFINED__
typedef interface IInternalUnknown IInternalUnknown;
#endif

#ifndef __IEnumUnknown_FWD_DEFINED__
#define __IEnumUnknown_FWD_DEFINED__
typedef interface IEnumUnknown IEnumUnknown;
#endif

#ifndef __IBindCtx_FWD_DEFINED__
#define __IBindCtx_FWD_DEFINED__
typedef interface IBindCtx IBindCtx;
#endif

#ifndef __IEnumMoniker_FWD_DEFINED__
#define __IEnumMoniker_FWD_DEFINED__
typedef interface IEnumMoniker IEnumMoniker;
#endif

#ifndef __IRunnableObject_FWD_DEFINED__
#define __IRunnableObject_FWD_DEFINED__
typedef interface IRunnableObject IRunnableObject;
#endif

#ifndef __IRunningObjectTable_FWD_DEFINED__
#define __IRunningObjectTable_FWD_DEFINED__
typedef interface IRunningObjectTable IRunningObjectTable;
#endif

#ifndef __IPersist_FWD_DEFINED__
#define __IPersist_FWD_DEFINED__
typedef interface IPersist IPersist;
#endif

#ifndef __IPersistStream_FWD_DEFINED__
#define __IPersistStream_FWD_DEFINED__
typedef interface IPersistStream IPersistStream;
#endif

#ifndef __IMoniker_FWD_DEFINED__
#define __IMoniker_FWD_DEFINED__
typedef interface IMoniker IMoniker;
#endif

#ifndef __IROTData_FWD_DEFINED__
#define __IROTData_FWD_DEFINED__
typedef interface IROTData IROTData;
#endif

#ifndef __IEnumString_FWD_DEFINED__
#define __IEnumString_FWD_DEFINED__
typedef interface IEnumString IEnumString;
#endif

#ifndef __ISequentialStream_FWD_DEFINED__
#define __ISequentialStream_FWD_DEFINED__
typedef interface ISequentialStream ISequentialStream;
#endif

#ifndef __IStream_FWD_DEFINED__
#define __IStream_FWD_DEFINED__
typedef interface IStream IStream;
#endif

#ifndef __IEnumSTATSTG_FWD_DEFINED__
#define __IEnumSTATSTG_FWD_DEFINED__
typedef interface IEnumSTATSTG IEnumSTATSTG;
#endif

#ifndef __IStorage_FWD_DEFINED__
#define __IStorage_FWD_DEFINED__
typedef interface IStorage IStorage;
#endif

#ifndef __IPersistFile_FWD_DEFINED__
#define __IPersistFile_FWD_DEFINED__
typedef interface IPersistFile IPersistFile;
#endif

#ifndef __IPersistStorage_FWD_DEFINED__
#define __IPersistStorage_FWD_DEFINED__
typedef interface IPersistStorage IPersistStorage;
#endif

#ifndef __ILockBytes_FWD_DEFINED__
#define __ILockBytes_FWD_DEFINED__
typedef interface ILockBytes ILockBytes;
#endif

#ifndef __IEnumFORMATETC_FWD_DEFINED__
#define __IEnumFORMATETC_FWD_DEFINED__
typedef interface IEnumFORMATETC IEnumFORMATETC;
#endif

#ifndef __IEnumSTATDATA_FWD_DEFINED__
#define __IEnumSTATDATA_FWD_DEFINED__
typedef interface IEnumSTATDATA IEnumSTATDATA;
#endif

#ifndef __IRootStorage_FWD_DEFINED__
#define __IRootStorage_FWD_DEFINED__
typedef interface IRootStorage IRootStorage;
#endif

#ifndef __IAdviseSink_FWD_DEFINED__
#define __IAdviseSink_FWD_DEFINED__
typedef interface IAdviseSink IAdviseSink;
#endif

#ifndef __AsyncIAdviseSink_FWD_DEFINED__
#define __AsyncIAdviseSink_FWD_DEFINED__
typedef interface AsyncIAdviseSink AsyncIAdviseSink;
#endif

#ifndef __IAdviseSink2_FWD_DEFINED__
#define __IAdviseSink2_FWD_DEFINED__
typedef interface IAdviseSink2 IAdviseSink2;
#endif

#ifndef __AsyncIAdviseSink2_FWD_DEFINED__
#define __AsyncIAdviseSink2_FWD_DEFINED__
typedef interface AsyncIAdviseSink2 AsyncIAdviseSink2;
#endif

#ifndef __IDataObject_FWD_DEFINED__
#define __IDataObject_FWD_DEFINED__
typedef interface IDataObject IDataObject;
#endif

#ifndef __IDataAdviseHolder_FWD_DEFINED__
#define __IDataAdviseHolder_FWD_DEFINED__
typedef interface IDataAdviseHolder IDataAdviseHolder;
#endif

#ifndef __IMessageFilter_FWD_DEFINED__
#define __IMessageFilter_FWD_DEFINED__
typedef interface IMessageFilter IMessageFilter;
#endif

#ifndef __IRpcChannelBuffer_FWD_DEFINED__
#define __IRpcChannelBuffer_FWD_DEFINED__
typedef interface IRpcChannelBuffer IRpcChannelBuffer;
#endif

#ifndef __IRpcChannelBuffer2_FWD_DEFINED__
#define __IRpcChannelBuffer2_FWD_DEFINED__
typedef interface IRpcChannelBuffer2 IRpcChannelBuffer2;
#endif

#ifndef __IAsyncRpcChannelBuffer_FWD_DEFINED__
#define __IAsyncRpcChannelBuffer_FWD_DEFINED__
typedef interface IAsyncRpcChannelBuffer IAsyncRpcChannelBuffer;
#endif

#ifndef __IRpcChannelBuffer3_FWD_DEFINED__
#define __IRpcChannelBuffer3_FWD_DEFINED__
typedef interface IRpcChannelBuffer3 IRpcChannelBuffer3;
#endif

#ifndef __IRpcProxyBuffer_FWD_DEFINED__
#define __IRpcProxyBuffer_FWD_DEFINED__
typedef interface IRpcProxyBuffer IRpcProxyBuffer;
#endif

#ifndef __IRpcStubBuffer_FWD_DEFINED__
#define __IRpcStubBuffer_FWD_DEFINED__
typedef interface IRpcStubBuffer IRpcStubBuffer;
#endif

#ifndef __IPSFactoryBuffer_FWD_DEFINED__
#define __IPSFactoryBuffer_FWD_DEFINED__
typedef interface IPSFactoryBuffer IPSFactoryBuffer;
#endif

#ifndef __IChannelHook_FWD_DEFINED__
#define __IChannelHook_FWD_DEFINED__
typedef interface IChannelHook IChannelHook;
#endif

#ifndef __IClientSecurity_FWD_DEFINED__
#define __IClientSecurity_FWD_DEFINED__
typedef interface IClientSecurity IClientSecurity;
#endif

#ifndef __IServerSecurity_FWD_DEFINED__
#define __IServerSecurity_FWD_DEFINED__
typedef interface IServerSecurity IServerSecurity;
#endif

#ifndef __IClassActivator_FWD_DEFINED__
#define __IClassActivator_FWD_DEFINED__
typedef interface IClassActivator IClassActivator;
#endif

#ifndef __IRpcOptions_FWD_DEFINED__
#define __IRpcOptions_FWD_DEFINED__
typedef interface IRpcOptions IRpcOptions;
#endif

#ifndef __IFillLockBytes_FWD_DEFINED__
#define __IFillLockBytes_FWD_DEFINED__
typedef interface IFillLockBytes IFillLockBytes;
#endif

#ifndef __IProgressNotify_FWD_DEFINED__
#define __IProgressNotify_FWD_DEFINED__
typedef interface IProgressNotify IProgressNotify;
#endif

#ifndef __ILayoutStorage_FWD_DEFINED__
#define __ILayoutStorage_FWD_DEFINED__
typedef interface ILayoutStorage ILayoutStorage;
#endif

#ifndef __IBlockingLock_FWD_DEFINED__
#define __IBlockingLock_FWD_DEFINED__
typedef interface IBlockingLock IBlockingLock;
#endif

#ifndef __ITimeAndNoticeControl_FWD_DEFINED__
#define __ITimeAndNoticeControl_FWD_DEFINED__
typedef interface ITimeAndNoticeControl ITimeAndNoticeControl;
#endif

#ifndef __IOplockStorage_FWD_DEFINED__
#define __IOplockStorage_FWD_DEFINED__
typedef interface IOplockStorage IOplockStorage;
#endif

#ifndef __ISurrogate_FWD_DEFINED__
#define __ISurrogate_FWD_DEFINED__
typedef interface ISurrogate ISurrogate;
#endif

#ifndef __IGlobalInterfaceTable_FWD_DEFINED__
#define __IGlobalInterfaceTable_FWD_DEFINED__
typedef interface IGlobalInterfaceTable IGlobalInterfaceTable;
#endif

#ifndef __IDirectWriterLock_FWD_DEFINED__
#define __IDirectWriterLock_FWD_DEFINED__
typedef interface IDirectWriterLock IDirectWriterLock;
#endif

#ifndef __ISynchronize_FWD_DEFINED__
#define __ISynchronize_FWD_DEFINED__
typedef interface ISynchronize ISynchronize;
#endif

#ifndef __ISynchronizeHandle_FWD_DEFINED__
#define __ISynchronizeHandle_FWD_DEFINED__
typedef interface ISynchronizeHandle ISynchronizeHandle;
#endif

#ifndef __ISynchronizeEvent_FWD_DEFINED__
#define __ISynchronizeEvent_FWD_DEFINED__
typedef interface ISynchronizeEvent ISynchronizeEvent;
#endif

#ifndef __ISynchronizeContainer_FWD_DEFINED__
#define __ISynchronizeContainer_FWD_DEFINED__
typedef interface ISynchronizeContainer ISynchronizeContainer;
#endif

#ifndef __ISynchronizeMutex_FWD_DEFINED__
#define __ISynchronizeMutex_FWD_DEFINED__
typedef interface ISynchronizeMutex ISynchronizeMutex;
#endif

#ifndef __ICancelMethodCalls_FWD_DEFINED__
#define __ICancelMethodCalls_FWD_DEFINED__
typedef interface ICancelMethodCalls ICancelMethodCalls;
#endif

#ifndef __IAsyncManager_FWD_DEFINED__
#define __IAsyncManager_FWD_DEFINED__
typedef interface IAsyncManager IAsyncManager;
#endif

#ifndef __ICallFactory_FWD_DEFINED__
#define __ICallFactory_FWD_DEFINED__
typedef interface ICallFactory ICallFactory;
#endif

#ifndef __IRpcHelper_FWD_DEFINED__
#define __IRpcHelper_FWD_DEFINED__
typedef interface IRpcHelper IRpcHelper;
#endif

#ifndef __IReleaseMarshalBuffers_FWD_DEFINED__
#define __IReleaseMarshalBuffers_FWD_DEFINED__
typedef interface IReleaseMarshalBuffers IReleaseMarshalBuffers;
#endif

#ifndef __IWaitMultiple_FWD_DEFINED__
#define __IWaitMultiple_FWD_DEFINED__
typedef interface IWaitMultiple IWaitMultiple;
#endif

#ifndef __IUrlMon_FWD_DEFINED__
#define __IUrlMon_FWD_DEFINED__
typedef interface IUrlMon IUrlMon;
#endif

#ifndef __IForegroundTransfer_FWD_DEFINED__
#define __IForegroundTransfer_FWD_DEFINED__
typedef interface IForegroundTransfer IForegroundTransfer;
#endif

#ifndef __IPipeByte_FWD_DEFINED__
#define __IPipeByte_FWD_DEFINED__
typedef interface IPipeByte IPipeByte;
#endif

#ifndef __AsyncIPipeByte_FWD_DEFINED__
#define __AsyncIPipeByte_FWD_DEFINED__
typedef interface AsyncIPipeByte AsyncIPipeByte;
#endif

#ifndef __IPipeLong_FWD_DEFINED__
#define __IPipeLong_FWD_DEFINED__
typedef interface IPipeLong IPipeLong;
#endif

#ifndef __AsyncIPipeLong_FWD_DEFINED__
#define __AsyncIPipeLong_FWD_DEFINED__
typedef interface AsyncIPipeLong AsyncIPipeLong;
#endif

#ifndef __IPipeDouble_FWD_DEFINED__
#define __IPipeDouble_FWD_DEFINED__
typedef interface IPipeDouble IPipeDouble;
#endif

#ifndef __AsyncIPipeDouble_FWD_DEFINED__
#define __AsyncIPipeDouble_FWD_DEFINED__
typedef interface AsyncIPipeDouble AsyncIPipeDouble;
#endif

#ifndef __IThumbnailExtractor_FWD_DEFINED__
#define __IThumbnailExtractor_FWD_DEFINED__
typedef interface IThumbnailExtractor IThumbnailExtractor;
#endif

#ifndef __IDummyHICONIncluder_FWD_DEFINED__
#define __IDummyHICONIncluder_FWD_DEFINED__
typedef interface IDummyHICONIncluder IDummyHICONIncluder;
#endif

#include "unknwn.h"

#ifdef __cplusplus
extern "C" {
#endif

    void *__RPC_USER MIDL_user_allocate(size_t);
    void __RPC_USER MIDL_user_free(void *);

    typedef struct _COSERVERINFO {
        DWORD dwReserved1;
        LPWSTR pwszName;
        COAUTHINFO *pAuthInfo;
        DWORD dwReserved2;
    } COSERVERINFO;

    extern RPC_IF_HANDLE __MIDL_itf_objidl_0000_v0_0_c_ifspec;
    extern RPC_IF_HANDLE __MIDL_itf_objidl_0000_v0_0_s_ifspec;

#ifndef __IMarshal_INTERFACE_DEFINED__
#define __IMarshal_INTERFACE_DEFINED__

    typedef IMarshal *LPMARSHAL;

    EXTERN_C const IID IID_IMarshal;

#if defined(__cplusplus) && !defined(CINTERFACE)
     MIDL_INTERFACE("00000003-0000-0000-C000-000000000046") IMarshal:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetUnmarshalClass(REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags, CLSID * pCid) = 0;
        virtual HRESULT STDMETHODCALLTYPE GetMarshalSizeMax(REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags, DWORD * pSize) = 0;
        virtual HRESULT STDMETHODCALLTYPE MarshalInterface(IStream * pStm, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags) = 0;
        virtual HRESULT STDMETHODCALLTYPE UnmarshalInterface(IStream * pStm, REFIID riid, void **ppv) = 0;
        virtual HRESULT STDMETHODCALLTYPE ReleaseMarshalData(IStream * pStm) = 0;
        virtual HRESULT STDMETHODCALLTYPE DisconnectObject(DWORD dwReserved) = 0;
    };
#else
    typedef struct IMarshalVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IMarshal * This, REFIID riid, void **ppvObject);
        ULONG(STDMETHODCALLTYPE * AddRef) (IMarshal * This);
        ULONG(STDMETHODCALLTYPE * Release) (IMarshal * This);
        HRESULT(STDMETHODCALLTYPE * GetUnmarshalClass) (IMarshal * This, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags, CLSID * pCid);
        HRESULT(STDMETHODCALLTYPE * GetMarshalSizeMax) (IMarshal * This, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags, DWORD * pSize);
        HRESULT(STDMETHODCALLTYPE * MarshalInterface) (IMarshal * This, IStream * pStm, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags);
        HRESULT(STDMETHODCALLTYPE * UnmarshalInterface) (IMarshal * This, IStream * pStm, REFIID riid, void **ppv);
        HRESULT(STDMETHODCALLTYPE * ReleaseMarshalData) (IMarshal * This, IStream * pStm);
        HRESULT(STDMETHODCALLTYPE * DisconnectObject) (IMarshal * This, DWORD dwReserved);
    END_INTERFACE} IMarshalVtbl;

    interface IMarshal {
        CONST_VTBL struct IMarshalVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IMarshal_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IMarshal_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IMarshal_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IMarshal_GetUnmarshalClass(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pCid)  \
    (This)->lpVtbl -> GetUnmarshalClass(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pCid)

#define IMarshal_GetMarshalSizeMax(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pSize)  \
    (This)->lpVtbl -> GetMarshalSizeMax(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pSize)

#define IMarshal_MarshalInterface(This,pStm,riid,pv,dwDestContext,pvDestContext,mshlflags)  \
    (This)->lpVtbl -> MarshalInterface(This,pStm,riid,pv,dwDestContext,pvDestContext,mshlflags)

#define IMarshal_UnmarshalInterface(This,pStm,riid,ppv) \
    (This)->lpVtbl -> UnmarshalInterface(This,pStm,riid,ppv)

#define IMarshal_ReleaseMarshalData(This,pStm)  \
    (This)->lpVtbl -> ReleaseMarshalData(This,pStm)

#define IMarshal_DisconnectObject(This,dwReserved)  \
    (This)->lpVtbl -> DisconnectObject(This,dwReserved)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IMarshal_GetUnmarshalClass_Proxy(IMarshal * This, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags, CLSID * pCid);

    void __RPC_STUB IMarshal_GetUnmarshalClass_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMarshal_GetMarshalSizeMax_Proxy(IMarshal * This, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags, DWORD * pSize);

    void __RPC_STUB IMarshal_GetMarshalSizeMax_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMarshal_MarshalInterface_Proxy(IMarshal * This, IStream * pStm, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags);

    void __RPC_STUB IMarshal_MarshalInterface_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMarshal_UnmarshalInterface_Proxy(IMarshal * This, IStream * pStm, REFIID riid, void **ppv);

    void __RPC_STUB IMarshal_UnmarshalInterface_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMarshal_ReleaseMarshalData_Proxy(IMarshal * This, IStream * pStm);

    void __RPC_STUB IMarshal_ReleaseMarshalData_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMarshal_DisconnectObject_Proxy(IMarshal * This, DWORD dwReserved);

    void __RPC_STUB IMarshal_DisconnectObject_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IMarshal_INTERFACE_DEFINED__ */

#ifndef __IMarshal2_INTERFACE_DEFINED__
#define __IMarshal2_INTERFACE_DEFINED__

    typedef IMarshal2 *LPMARSHAL2;

    EXTERN_C const IID IID_IMarshal2;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("000001cf-0000-0000-C000-000000000046")
     IMarshal2:public IMarshal {
      public:
    };

#else

    typedef struct IMarshal2Vtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IMarshal2 * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IMarshal2 * This);

         ULONG(STDMETHODCALLTYPE * Release) (IMarshal2 * This);

         HRESULT(STDMETHODCALLTYPE * GetUnmarshalClass) (IMarshal2 * This, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags, CLSID * pCid);

         HRESULT(STDMETHODCALLTYPE * GetMarshalSizeMax) (IMarshal2 * This, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags, DWORD * pSize);

         HRESULT(STDMETHODCALLTYPE * MarshalInterface) (IMarshal2 * This, IStream * pStm, REFIID riid, void *pv, DWORD dwDestContext, void *pvDestContext, DWORD mshlflags);

         HRESULT(STDMETHODCALLTYPE * UnmarshalInterface) (IMarshal2 * This, IStream * pStm, REFIID riid, void **ppv);

         HRESULT(STDMETHODCALLTYPE * ReleaseMarshalData) (IMarshal2 * This, IStream * pStm);

         HRESULT(STDMETHODCALLTYPE * DisconnectObject) (IMarshal2 * This, DWORD dwReserved);

     END_INTERFACE} IMarshal2Vtbl;

    interface IMarshal2 {
        CONST_VTBL struct IMarshal2Vtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IMarshal2_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IMarshal2_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IMarshal2_Release(This) \
    (This)->lpVtbl -> Release(This)

#define IMarshal2_GetUnmarshalClass(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pCid)  \
    (This)->lpVtbl -> GetUnmarshalClass(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pCid)

#define IMarshal2_GetMarshalSizeMax(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pSize)  \
    (This)->lpVtbl -> GetMarshalSizeMax(This,riid,pv,dwDestContext,pvDestContext,mshlflags,pSize)

#define IMarshal2_MarshalInterface(This,pStm,riid,pv,dwDestContext,pvDestContext,mshlflags)  \
    (This)->lpVtbl -> MarshalInterface(This,pStm,riid,pv,dwDestContext,pvDestContext,mshlflags)

#define IMarshal2_UnmarshalInterface(This,pStm,riid,ppv)  \
    (This)->lpVtbl -> UnmarshalInterface(This,pStm,riid,ppv)

#define IMarshal2_ReleaseMarshalData(This,pStm) \
    (This)->lpVtbl -> ReleaseMarshalData(This,pStm)

#define IMarshal2_DisconnectObject(This,dwReserved)  \
    (This)->lpVtbl -> DisconnectObject(This,dwReserved)

#endif /* COBJMACROS */

#endif

#endif /* __IMarshal2_INTERFACE_DEFINED__ */

#ifndef __IMalloc_INTERFACE_DEFINED__
#define __IMalloc_INTERFACE_DEFINED__

    typedef IMalloc *LPMALLOC;

    EXTERN_C const IID IID_IMalloc;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000002-0000-0000-C000-000000000046")
     IMalloc:public IUnknown {
      public:
        virtual void *STDMETHODCALLTYPE Alloc(SIZE_T cb) = 0;

        virtual void *STDMETHODCALLTYPE Realloc(void *pv, SIZE_T cb) = 0;

        virtual void STDMETHODCALLTYPE Free(void *pv) = 0;

        virtual SIZE_T STDMETHODCALLTYPE GetSize(void *pv) = 0;

        virtual int STDMETHODCALLTYPE DidAlloc(void *pv) = 0;

        virtual void STDMETHODCALLTYPE HeapMinimize(void) = 0;

    };

#else

    typedef struct IMallocVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IMalloc * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IMalloc * This);

         ULONG(STDMETHODCALLTYPE * Release) (IMalloc * This);

        void *(STDMETHODCALLTYPE * Alloc) (IMalloc * This, SIZE_T cb);

        void *(STDMETHODCALLTYPE * Realloc) (IMalloc * This, void *pv, SIZE_T cb);

        void (STDMETHODCALLTYPE * Free) (IMalloc * This, void *pv);

         SIZE_T(STDMETHODCALLTYPE * GetSize) (IMalloc * This, void *pv);

        int (STDMETHODCALLTYPE * DidAlloc) (IMalloc * This, void *pv);

        void (STDMETHODCALLTYPE * HeapMinimize) (IMalloc * This);

     END_INTERFACE} IMallocVtbl;

    interface IMalloc {
        CONST_VTBL struct IMallocVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IMalloc_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IMalloc_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IMalloc_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IMalloc_Alloc(This,cb)  \
    (This)->lpVtbl -> Alloc(This,cb)

#define IMalloc_Realloc(This,pv,cb)  \
    (This)->lpVtbl -> Realloc(This,pv,cb)

#define IMalloc_Free(This,pv)  \
    (This)->lpVtbl -> Free(This,pv)

#define IMalloc_GetSize(This,pv)  \
    (This)->lpVtbl -> GetSize(This,pv)

#define IMalloc_DidAlloc(This,pv)  \
    (This)->lpVtbl -> DidAlloc(This,pv)

#define IMalloc_HeapMinimize(This)  \
    (This)->lpVtbl -> HeapMinimize(This)

#endif /* COBJMACROS */

#endif

    void *STDMETHODCALLTYPE IMalloc_Alloc_Proxy(IMalloc * This, SIZE_T cb);

    void __RPC_STUB IMalloc_Alloc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void *STDMETHODCALLTYPE IMalloc_Realloc_Proxy(IMalloc * This, void *pv, SIZE_T cb);

    void __RPC_STUB IMalloc_Realloc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IMalloc_Free_Proxy(IMalloc * This, void *pv);

    void __RPC_STUB IMalloc_Free_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    SIZE_T STDMETHODCALLTYPE IMalloc_GetSize_Proxy(IMalloc * This, void *pv);

    void __RPC_STUB IMalloc_GetSize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    int STDMETHODCALLTYPE IMalloc_DidAlloc_Proxy(IMalloc * This, void *pv);

    void __RPC_STUB IMalloc_DidAlloc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IMalloc_HeapMinimize_Proxy(IMalloc * This);

    void __RPC_STUB IMalloc_HeapMinimize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IMalloc_INTERFACE_DEFINED__ */

#ifndef __IMallocSpy_INTERFACE_DEFINED__
#define __IMallocSpy_INTERFACE_DEFINED__

    typedef IMallocSpy *LPMALLOCSPY;

    EXTERN_C const IID IID_IMallocSpy;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000001d-0000-0000-C000-000000000046")
     IMallocSpy:public IUnknown {
      public:
        virtual SIZE_T STDMETHODCALLTYPE PreAlloc(SIZE_T cbRequest) = 0;

        virtual void *STDMETHODCALLTYPE PostAlloc(void *pActual) = 0;

        virtual void *STDMETHODCALLTYPE PreFree(void *pRequest, BOOL fSpyed) = 0;

        virtual void STDMETHODCALLTYPE PostFree(BOOL fSpyed) = 0;

        virtual SIZE_T STDMETHODCALLTYPE PreRealloc(void *pRequest, SIZE_T cbRequest, void **ppNewRequest, BOOL fSpyed) = 0;

        virtual void *STDMETHODCALLTYPE PostRealloc(void *pActual, BOOL fSpyed) = 0;

        virtual void *STDMETHODCALLTYPE PreGetSize(void *pRequest, BOOL fSpyed) = 0;

        virtual SIZE_T STDMETHODCALLTYPE PostGetSize(SIZE_T cbActual, BOOL fSpyed) = 0;

        virtual void *STDMETHODCALLTYPE PreDidAlloc(void *pRequest, BOOL fSpyed) = 0;

        virtual int STDMETHODCALLTYPE PostDidAlloc(void *pRequest, BOOL fSpyed, int fActual) = 0;

        virtual void STDMETHODCALLTYPE PreHeapMinimize(void) = 0;

        virtual void STDMETHODCALLTYPE PostHeapMinimize(void) = 0;

    };

#else

    typedef struct IMallocSpyVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IMallocSpy * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IMallocSpy * This);

         ULONG(STDMETHODCALLTYPE * Release) (IMallocSpy * This);

         SIZE_T(STDMETHODCALLTYPE * PreAlloc) (IMallocSpy * This, SIZE_T cbRequest);

        void *(STDMETHODCALLTYPE * PostAlloc) (IMallocSpy * This, void *pActual);

        void *(STDMETHODCALLTYPE * PreFree) (IMallocSpy * This, void *pRequest, BOOL fSpyed);

        void (STDMETHODCALLTYPE * PostFree) (IMallocSpy * This, BOOL fSpyed);

         SIZE_T(STDMETHODCALLTYPE * PreRealloc) (IMallocSpy * This, void *pRequest, SIZE_T cbRequest, void **ppNewRequest, BOOL fSpyed);

        void *(STDMETHODCALLTYPE * PostRealloc) (IMallocSpy * This, void *pActual, BOOL fSpyed);

        void *(STDMETHODCALLTYPE * PreGetSize) (IMallocSpy * This, void *pRequest, BOOL fSpyed);

         SIZE_T(STDMETHODCALLTYPE * PostGetSize) (IMallocSpy * This, SIZE_T cbActual, BOOL fSpyed);

        void *(STDMETHODCALLTYPE * PreDidAlloc) (IMallocSpy * This, void *pRequest, BOOL fSpyed);

        int (STDMETHODCALLTYPE * PostDidAlloc) (IMallocSpy * This, void *pRequest, BOOL fSpyed, int fActual);

        void (STDMETHODCALLTYPE * PreHeapMinimize) (IMallocSpy * This);

        void (STDMETHODCALLTYPE * PostHeapMinimize) (IMallocSpy * This);

     END_INTERFACE} IMallocSpyVtbl;

    interface IMallocSpy {
        CONST_VTBL struct IMallocSpyVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IMallocSpy_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IMallocSpy_AddRef(This) \
    (This)->lpVtbl -> AddRef(This)

#define IMallocSpy_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IMallocSpy_PreAlloc(This,cbRequest)  \
    (This)->lpVtbl -> PreAlloc(This,cbRequest)

#define IMallocSpy_PostAlloc(This,pActual)  \
    (This)->lpVtbl -> PostAlloc(This,pActual)

#define IMallocSpy_PreFree(This,pRequest,fSpyed)  \
    (This)->lpVtbl -> PreFree(This,pRequest,fSpyed)

#define IMallocSpy_PostFree(This,fSpyed)  \
    (This)->lpVtbl -> PostFree(This,fSpyed)

#define IMallocSpy_PreRealloc(This,pRequest,cbRequest,ppNewRequest,fSpyed)  \
    (This)->lpVtbl -> PreRealloc(This,pRequest,cbRequest,ppNewRequest,fSpyed)

#define IMallocSpy_PostRealloc(This,pActual,fSpyed)  \
    (This)->lpVtbl -> PostRealloc(This,pActual,fSpyed)

#define IMallocSpy_PreGetSize(This,pRequest,fSpyed)  \
    (This)->lpVtbl -> PreGetSize(This,pRequest,fSpyed)

#define IMallocSpy_PostGetSize(This,cbActual,fSpyed)  \
    (This)->lpVtbl -> PostGetSize(This,cbActual,fSpyed)

#define IMallocSpy_PreDidAlloc(This,pRequest,fSpyed)  \
    (This)->lpVtbl -> PreDidAlloc(This,pRequest,fSpyed)

#define IMallocSpy_PostDidAlloc(This,pRequest,fSpyed,fActual)  \
    (This)->lpVtbl -> PostDidAlloc(This,pRequest,fSpyed,fActual)

#define IMallocSpy_PreHeapMinimize(This)  \
    (This)->lpVtbl -> PreHeapMinimize(This)

#define IMallocSpy_PostHeapMinimize(This)  \
    (This)->lpVtbl -> PostHeapMinimize(This)

#endif /* COBJMACROS */

#endif

    SIZE_T STDMETHODCALLTYPE IMallocSpy_PreAlloc_Proxy(IMallocSpy * This, SIZE_T cbRequest);

    void __RPC_STUB IMallocSpy_PreAlloc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void *STDMETHODCALLTYPE IMallocSpy_PostAlloc_Proxy(IMallocSpy * This, void *pActual);

    void __RPC_STUB IMallocSpy_PostAlloc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void *STDMETHODCALLTYPE IMallocSpy_PreFree_Proxy(IMallocSpy * This, void *pRequest, BOOL fSpyed);

    void __RPC_STUB IMallocSpy_PreFree_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IMallocSpy_PostFree_Proxy(IMallocSpy * This, BOOL fSpyed);

    void __RPC_STUB IMallocSpy_PostFree_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    SIZE_T STDMETHODCALLTYPE IMallocSpy_PreRealloc_Proxy(IMallocSpy * This, void *pRequest, SIZE_T cbRequest, void **ppNewRequest, BOOL fSpyed);

    void __RPC_STUB IMallocSpy_PreRealloc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void *STDMETHODCALLTYPE IMallocSpy_PostRealloc_Proxy(IMallocSpy * This, void *pActual, BOOL fSpyed);

    void __RPC_STUB IMallocSpy_PostRealloc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void *STDMETHODCALLTYPE IMallocSpy_PreGetSize_Proxy(IMallocSpy * This, void *pRequest, BOOL fSpyed);

    void __RPC_STUB IMallocSpy_PreGetSize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    SIZE_T STDMETHODCALLTYPE IMallocSpy_PostGetSize_Proxy(IMallocSpy * This, SIZE_T cbActual, BOOL fSpyed);

    void __RPC_STUB IMallocSpy_PostGetSize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void *STDMETHODCALLTYPE IMallocSpy_PreDidAlloc_Proxy(IMallocSpy * This, void *pRequest, BOOL fSpyed);

    void __RPC_STUB IMallocSpy_PreDidAlloc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    int STDMETHODCALLTYPE IMallocSpy_PostDidAlloc_Proxy(IMallocSpy * This, void *pRequest, BOOL fSpyed, int fActual);

    void __RPC_STUB IMallocSpy_PostDidAlloc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IMallocSpy_PreHeapMinimize_Proxy(IMallocSpy * This);

    void __RPC_STUB IMallocSpy_PreHeapMinimize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IMallocSpy_PostHeapMinimize_Proxy(IMallocSpy * This);

    void __RPC_STUB IMallocSpy_PostHeapMinimize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IMallocSpy_INTERFACE_DEFINED__ */

#ifndef __IStdMarshalInfo_INTERFACE_DEFINED__
#define __IStdMarshalInfo_INTERFACE_DEFINED__

    typedef IStdMarshalInfo *LPSTDMARSHALINFO;

    EXTERN_C const IID IID_IStdMarshalInfo;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000018-0000-0000-C000-000000000046")
     IStdMarshalInfo:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetClassForHandler(DWORD dwDestContext, void *pvDestContext, CLSID * pClsid) = 0;

    };

#else

    typedef struct IStdMarshalInfoVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IStdMarshalInfo * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IStdMarshalInfo * This);

         ULONG(STDMETHODCALLTYPE * Release) (IStdMarshalInfo * This);

         HRESULT(STDMETHODCALLTYPE * GetClassForHandler) (IStdMarshalInfo * This, DWORD dwDestContext, void *pvDestContext, CLSID * pClsid);

     END_INTERFACE} IStdMarshalInfoVtbl;

    interface IStdMarshalInfo {
        CONST_VTBL struct IStdMarshalInfoVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IStdMarshalInfo_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IStdMarshalInfo_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IStdMarshalInfo_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IStdMarshalInfo_GetClassForHandler(This,dwDestContext,pvDestContext,pClsid)  \
    (This)->lpVtbl -> GetClassForHandler(This,dwDestContext,pvDestContext,pClsid)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IStdMarshalInfo_GetClassForHandler_Proxy(IStdMarshalInfo * This, DWORD dwDestContext, void *pvDestContext, CLSID * pClsid);

    void __RPC_STUB IStdMarshalInfo_GetClassForHandler_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IStdMarshalInfo_INTERFACE_DEFINED__ */

#ifndef __IExternalConnection_INTERFACE_DEFINED__
#define __IExternalConnection_INTERFACE_DEFINED__

    typedef IExternalConnection *LPEXTERNALCONNECTION;

    typedef
        enum tagEXTCONN { EXTCONN_STRONG = 0x1,
        EXTCONN_WEAK = 0x2,
        EXTCONN_CALLABLE = 0x4
    } EXTCONN;

    EXTERN_C const IID IID_IExternalConnection;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000019-0000-0000-C000-000000000046")
     IExternalConnection:public IUnknown {
      public:
        virtual DWORD STDMETHODCALLTYPE AddConnection(DWORD extconn, DWORD reserved) = 0;

        virtual DWORD STDMETHODCALLTYPE ReleaseConnection(DWORD extconn, DWORD reserved, BOOL fLastReleaseCloses) = 0;

    };

#else

    typedef struct IExternalConnectionVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IExternalConnection * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IExternalConnection * This);

         ULONG(STDMETHODCALLTYPE * Release) (IExternalConnection * This);

         DWORD(STDMETHODCALLTYPE * AddConnection) (IExternalConnection * This, DWORD extconn, DWORD reserved);

         DWORD(STDMETHODCALLTYPE * ReleaseConnection) (IExternalConnection * This, DWORD extconn, DWORD reserved, BOOL fLastReleaseCloses);

     END_INTERFACE} IExternalConnectionVtbl;

    interface IExternalConnection {
        CONST_VTBL struct IExternalConnectionVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IExternalConnection_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IExternalConnection_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IExternalConnection_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IExternalConnection_AddConnection(This,extconn,reserved)  \
    (This)->lpVtbl -> AddConnection(This,extconn,reserved)

#define IExternalConnection_ReleaseConnection(This,extconn,reserved,fLastReleaseCloses) \
    (This)->lpVtbl -> ReleaseConnection(This,extconn,reserved,fLastReleaseCloses)

#endif /* COBJMACROS */

#endif

    DWORD STDMETHODCALLTYPE IExternalConnection_AddConnection_Proxy(IExternalConnection * This, DWORD extconn, DWORD reserved);

    void __RPC_STUB IExternalConnection_AddConnection_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    DWORD STDMETHODCALLTYPE IExternalConnection_ReleaseConnection_Proxy(IExternalConnection * This, DWORD extconn, DWORD reserved, BOOL fLastReleaseCloses);

    void __RPC_STUB IExternalConnection_ReleaseConnection_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IExternalConnection_INTERFACE_DEFINED__ */

    typedef IMultiQI *LPMULTIQI;

    typedef struct tagMULTI_QI {
        const IID *pIID;
        IUnknown *pItf;
        HRESULT hr;
    } MULTI_QI;

    extern RPC_IF_HANDLE __MIDL_itf_objidl_0015_v0_0_c_ifspec;
    extern RPC_IF_HANDLE __MIDL_itf_objidl_0015_v0_0_s_ifspec;

#ifndef __IMultiQI_INTERFACE_DEFINED__
#define __IMultiQI_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IMultiQI;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000020-0000-0000-C000-000000000046")
     IMultiQI:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE QueryMultipleInterfaces(ULONG cMQIs, MULTI_QI * pMQIs) = 0;

    };

#else

    typedef struct IMultiQIVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IMultiQI * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IMultiQI * This);

         ULONG(STDMETHODCALLTYPE * Release) (IMultiQI * This);

         HRESULT(STDMETHODCALLTYPE * QueryMultipleInterfaces) (IMultiQI * This, ULONG cMQIs, MULTI_QI * pMQIs);

     END_INTERFACE} IMultiQIVtbl;

    interface IMultiQI {
        CONST_VTBL struct IMultiQIVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IMultiQI_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IMultiQI_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IMultiQI_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IMultiQI_QueryMultipleInterfaces(This,cMQIs,pMQIs)  \
    (This)->lpVtbl -> QueryMultipleInterfaces(This,cMQIs,pMQIs)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IMultiQI_QueryMultipleInterfaces_Proxy(IMultiQI * This, ULONG cMQIs, MULTI_QI * pMQIs);

    void __RPC_STUB IMultiQI_QueryMultipleInterfaces_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IMultiQI_INTERFACE_DEFINED__ */

#ifndef __AsyncIMultiQI_INTERFACE_DEFINED__
#define __AsyncIMultiQI_INTERFACE_DEFINED__

    EXTERN_C const IID IID_AsyncIMultiQI;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("000e0020-0000-0000-C000-000000000046")
     AsyncIMultiQI:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Begin_QueryMultipleInterfaces(ULONG cMQIs, MULTI_QI * pMQIs) = 0;

        virtual HRESULT STDMETHODCALLTYPE Finish_QueryMultipleInterfaces(MULTI_QI * pMQIs) = 0;

    };

#else

    typedef struct AsyncIMultiQIVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (AsyncIMultiQI * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (AsyncIMultiQI * This);

         ULONG(STDMETHODCALLTYPE * Release) (AsyncIMultiQI * This);

         HRESULT(STDMETHODCALLTYPE * Begin_QueryMultipleInterfaces) (AsyncIMultiQI * This, ULONG cMQIs, MULTI_QI * pMQIs);

         HRESULT(STDMETHODCALLTYPE * Finish_QueryMultipleInterfaces) (AsyncIMultiQI * This, MULTI_QI * pMQIs);

     END_INTERFACE} AsyncIMultiQIVtbl;

    interface AsyncIMultiQI {
        CONST_VTBL struct AsyncIMultiQIVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define AsyncIMultiQI_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define AsyncIMultiQI_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define AsyncIMultiQI_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define AsyncIMultiQI_Begin_QueryMultipleInterfaces(This,cMQIs,pMQIs)  \
    (This)->lpVtbl -> Begin_QueryMultipleInterfaces(This,cMQIs,pMQIs)

#define AsyncIMultiQI_Finish_QueryMultipleInterfaces(This,pMQIs)  \
    (This)->lpVtbl -> Finish_QueryMultipleInterfaces(This,pMQIs)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE AsyncIMultiQI_Begin_QueryMultipleInterfaces_Proxy(AsyncIMultiQI * This, ULONG cMQIs, MULTI_QI * pMQIs);

    void __RPC_STUB AsyncIMultiQI_Begin_QueryMultipleInterfaces_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIMultiQI_Finish_QueryMultipleInterfaces_Proxy(AsyncIMultiQI * This, MULTI_QI * pMQIs);

    void __RPC_STUB AsyncIMultiQI_Finish_QueryMultipleInterfaces_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __AsyncIMultiQI_INTERFACE_DEFINED__ */

#ifndef __IInternalUnknown_INTERFACE_DEFINED__
#define __IInternalUnknown_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IInternalUnknown;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000021-0000-0000-C000-000000000046")
     IInternalUnknown:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE QueryInternalInterface(REFIID riid, void **ppv) = 0;

    };

#else

    typedef struct IInternalUnknownVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IInternalUnknown * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IInternalUnknown * This);

         ULONG(STDMETHODCALLTYPE * Release) (IInternalUnknown * This);

         HRESULT(STDMETHODCALLTYPE * QueryInternalInterface) (IInternalUnknown * This, REFIID riid, void **ppv);

     END_INTERFACE} IInternalUnknownVtbl;

    interface IInternalUnknown {
        CONST_VTBL struct IInternalUnknownVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IInternalUnknown_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IInternalUnknown_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IInternalUnknown_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IInternalUnknown_QueryInternalInterface(This,riid,ppv)  \
    (This)->lpVtbl -> QueryInternalInterface(This,riid,ppv)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IInternalUnknown_QueryInternalInterface_Proxy(IInternalUnknown * This, REFIID riid, void **ppv);

    void __RPC_STUB IInternalUnknown_QueryInternalInterface_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IInternalUnknown_INTERFACE_DEFINED__ */

#ifndef __IEnumUnknown_INTERFACE_DEFINED__
#define __IEnumUnknown_INTERFACE_DEFINED__

    typedef IEnumUnknown *LPENUMUNKNOWN;

    EXTERN_C const IID IID_IEnumUnknown;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000100-0000-0000-C000-000000000046")
     IEnumUnknown:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Next(ULONG celt, IUnknown * *rgelt, ULONG * pceltFetched) = 0;

        virtual HRESULT STDMETHODCALLTYPE Skip(ULONG celt) = 0;

        virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Clone(IEnumUnknown * *ppenum) = 0;

    };

#else

    typedef struct IEnumUnknownVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IEnumUnknown * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IEnumUnknown * This);

         ULONG(STDMETHODCALLTYPE * Release) (IEnumUnknown * This);

         HRESULT(STDMETHODCALLTYPE * Next) (IEnumUnknown * This, ULONG celt, IUnknown * *rgelt, ULONG * pceltFetched);

         HRESULT(STDMETHODCALLTYPE * Skip) (IEnumUnknown * This, ULONG celt);

         HRESULT(STDMETHODCALLTYPE * Reset) (IEnumUnknown * This);

         HRESULT(STDMETHODCALLTYPE * Clone) (IEnumUnknown * This, IEnumUnknown * *ppenum);

     END_INTERFACE} IEnumUnknownVtbl;

    interface IEnumUnknown {
        CONST_VTBL struct IEnumUnknownVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IEnumUnknown_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IEnumUnknown_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IEnumUnknown_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IEnumUnknown_Next(This,celt,rgelt,pceltFetched) \
    (This)->lpVtbl -> Next(This,celt,rgelt,pceltFetched)

#define IEnumUnknown_Skip(This,celt)  \
    (This)->lpVtbl -> Skip(This,celt)

#define IEnumUnknown_Reset(This)  \
    (This)->lpVtbl -> Reset(This)

#define IEnumUnknown_Clone(This,ppenum) \
    (This)->lpVtbl -> Clone(This,ppenum)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IEnumUnknown_RemoteNext_Proxy(IEnumUnknown * This, ULONG celt, IUnknown * *rgelt, ULONG * pceltFetched);

    void __RPC_STUB IEnumUnknown_RemoteNext_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumUnknown_Skip_Proxy(IEnumUnknown * This, ULONG celt);

    void __RPC_STUB IEnumUnknown_Skip_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumUnknown_Reset_Proxy(IEnumUnknown * This);

    void __RPC_STUB IEnumUnknown_Reset_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumUnknown_Clone_Proxy(IEnumUnknown * This, IEnumUnknown * *ppenum);

    void __RPC_STUB IEnumUnknown_Clone_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IEnumUnknown_INTERFACE_DEFINED__ */

#ifndef __IBindCtx_INTERFACE_DEFINED__
#define __IBindCtx_INTERFACE_DEFINED__

    typedef IBindCtx *LPBC;

    typedef IBindCtx *LPBINDCTX;

    typedef struct tagBIND_OPTS {
        DWORD cbStruct;
        DWORD grfFlags;
        DWORD grfMode;
        DWORD dwTickCountDeadline;
    } BIND_OPTS;

    typedef struct tagBIND_OPTS *LPBIND_OPTS;

#if defined(__cplusplus)
    typedef struct tagBIND_OPTS2:tagBIND_OPTS {
        DWORD dwTrackFlags;
        DWORD dwClassContext;
        LCID locale;
        COSERVERINFO *pServerInfo;
    } BIND_OPTS2, *LPBIND_OPTS2;
#else
    typedef struct tagBIND_OPTS2 {
        DWORD cbStruct;
        DWORD grfFlags;
        DWORD grfMode;
        DWORD dwTickCountDeadline;
        DWORD dwTrackFlags;
        DWORD dwClassContext;
        LCID locale;
        COSERVERINFO *pServerInfo;
    } BIND_OPTS2;

    typedef struct tagBIND_OPTS2 *LPBIND_OPTS2;

#endif
    typedef
        enum tagBIND_FLAGS { BIND_MAYBOTHERUSER = 1,
        BIND_JUSTTESTEXISTENCE = 2
    } BIND_FLAGS;

    EXTERN_C const IID IID_IBindCtx;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000000e-0000-0000-C000-000000000046")
     IBindCtx:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE RegisterObjectBound(IUnknown * punk) = 0;

        virtual HRESULT STDMETHODCALLTYPE RevokeObjectBound(IUnknown * punk) = 0;

        virtual HRESULT STDMETHODCALLTYPE ReleaseBoundObjects(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetBindOptions(BIND_OPTS * pbindopts) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetBindOptions(BIND_OPTS * pbindopts) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetRunningObjectTable(IRunningObjectTable * *pprot) = 0;

        virtual HRESULT STDMETHODCALLTYPE RegisterObjectParam(LPOLESTR pszKey, IUnknown * punk) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetObjectParam(LPOLESTR pszKey, IUnknown * *ppunk) = 0;

        virtual HRESULT STDMETHODCALLTYPE EnumObjectParam(IEnumString * *ppenum) = 0;

        virtual HRESULT STDMETHODCALLTYPE RevokeObjectParam(LPOLESTR pszKey) = 0;

    };

#else

    typedef struct IBindCtxVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IBindCtx * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IBindCtx * This);

         ULONG(STDMETHODCALLTYPE * Release) (IBindCtx * This);

         HRESULT(STDMETHODCALLTYPE * RegisterObjectBound) (IBindCtx * This, IUnknown * punk);

         HRESULT(STDMETHODCALLTYPE * RevokeObjectBound) (IBindCtx * This, IUnknown * punk);

         HRESULT(STDMETHODCALLTYPE * ReleaseBoundObjects) (IBindCtx * This);

         HRESULT(STDMETHODCALLTYPE * SetBindOptions) (IBindCtx * This, BIND_OPTS * pbindopts);

         HRESULT(STDMETHODCALLTYPE * GetBindOptions) (IBindCtx * This, BIND_OPTS * pbindopts);

         HRESULT(STDMETHODCALLTYPE * GetRunningObjectTable) (IBindCtx * This, IRunningObjectTable * *pprot);

         HRESULT(STDMETHODCALLTYPE * RegisterObjectParam) (IBindCtx * This, LPOLESTR pszKey, IUnknown * punk);

         HRESULT(STDMETHODCALLTYPE * GetObjectParam) (IBindCtx * This, LPOLESTR pszKey, IUnknown * *ppunk);

         HRESULT(STDMETHODCALLTYPE * EnumObjectParam) (IBindCtx * This, IEnumString * *ppenum);

         HRESULT(STDMETHODCALLTYPE * RevokeObjectParam) (IBindCtx * This, LPOLESTR pszKey);

     END_INTERFACE} IBindCtxVtbl;

    interface IBindCtx {
        CONST_VTBL struct IBindCtxVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IBindCtx_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IBindCtx_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IBindCtx_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IBindCtx_RegisterObjectBound(This,punk) \
    (This)->lpVtbl -> RegisterObjectBound(This,punk)

#define IBindCtx_RevokeObjectBound(This,punk)  \
    (This)->lpVtbl -> RevokeObjectBound(This,punk)

#define IBindCtx_ReleaseBoundObjects(This)  \
    (This)->lpVtbl -> ReleaseBoundObjects(This)

#define IBindCtx_SetBindOptions(This,pbindopts) \
    (This)->lpVtbl -> SetBindOptions(This,pbindopts)

#define IBindCtx_GetBindOptions(This,pbindopts) \
    (This)->lpVtbl -> GetBindOptions(This,pbindopts)

#define IBindCtx_GetRunningObjectTable(This,pprot)  \
    (This)->lpVtbl -> GetRunningObjectTable(This,pprot)

#define IBindCtx_RegisterObjectParam(This,pszKey,punk)  \
    (This)->lpVtbl -> RegisterObjectParam(This,pszKey,punk)

#define IBindCtx_GetObjectParam(This,pszKey,ppunk)  \
    (This)->lpVtbl -> GetObjectParam(This,pszKey,ppunk)

#define IBindCtx_EnumObjectParam(This,ppenum)  \
    (This)->lpVtbl -> EnumObjectParam(This,ppenum)

#define IBindCtx_RevokeObjectParam(This,pszKey) \
    (This)->lpVtbl -> RevokeObjectParam(This,pszKey)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IBindCtx_RegisterObjectBound_Proxy(IBindCtx * This, IUnknown * punk);

    void __RPC_STUB IBindCtx_RegisterObjectBound_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBindCtx_RevokeObjectBound_Proxy(IBindCtx * This, IUnknown * punk);

    void __RPC_STUB IBindCtx_RevokeObjectBound_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBindCtx_ReleaseBoundObjects_Proxy(IBindCtx * This);

    void __RPC_STUB IBindCtx_ReleaseBoundObjects_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBindCtx_RemoteSetBindOptions_Proxy(IBindCtx * This, BIND_OPTS2 * pbindopts);

    void __RPC_STUB IBindCtx_RemoteSetBindOptions_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBindCtx_RemoteGetBindOptions_Proxy(IBindCtx * This, BIND_OPTS2 * pbindopts);

    void __RPC_STUB IBindCtx_RemoteGetBindOptions_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBindCtx_GetRunningObjectTable_Proxy(IBindCtx * This, IRunningObjectTable * *pprot);

    void __RPC_STUB IBindCtx_GetRunningObjectTable_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBindCtx_RegisterObjectParam_Proxy(IBindCtx * This, LPOLESTR pszKey, IUnknown * punk);

    void __RPC_STUB IBindCtx_RegisterObjectParam_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBindCtx_GetObjectParam_Proxy(IBindCtx * This, LPOLESTR pszKey, IUnknown * *ppunk);

    void __RPC_STUB IBindCtx_GetObjectParam_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBindCtx_EnumObjectParam_Proxy(IBindCtx * This, IEnumString * *ppenum);

    void __RPC_STUB IBindCtx_EnumObjectParam_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBindCtx_RevokeObjectParam_Proxy(IBindCtx * This, LPOLESTR pszKey);

    void __RPC_STUB IBindCtx_RevokeObjectParam_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IBindCtx_INTERFACE_DEFINED__ */

#ifndef __IEnumMoniker_INTERFACE_DEFINED__
#define __IEnumMoniker_INTERFACE_DEFINED__

    typedef IEnumMoniker *LPENUMMONIKER;

    EXTERN_C const IID IID_IEnumMoniker;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000102-0000-0000-C000-000000000046")
     IEnumMoniker:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Next(ULONG celt, IMoniker * *rgelt, ULONG * pceltFetched) = 0;

        virtual HRESULT STDMETHODCALLTYPE Skip(ULONG celt) = 0;

        virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Clone(IEnumMoniker * *ppenum) = 0;

    };

#else

    typedef struct IEnumMonikerVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IEnumMoniker * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IEnumMoniker * This);

         ULONG(STDMETHODCALLTYPE * Release) (IEnumMoniker * This);

         HRESULT(STDMETHODCALLTYPE * Next) (IEnumMoniker * This, ULONG celt, IMoniker * *rgelt, ULONG * pceltFetched);

         HRESULT(STDMETHODCALLTYPE * Skip) (IEnumMoniker * This, ULONG celt);

         HRESULT(STDMETHODCALLTYPE * Reset) (IEnumMoniker * This);

         HRESULT(STDMETHODCALLTYPE * Clone) (IEnumMoniker * This, IEnumMoniker * *ppenum);

     END_INTERFACE} IEnumMonikerVtbl;

    interface IEnumMoniker {
        CONST_VTBL struct IEnumMonikerVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IEnumMoniker_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IEnumMoniker_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IEnumMoniker_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IEnumMoniker_Next(This,celt,rgelt,pceltFetched) \
    (This)->lpVtbl -> Next(This,celt,rgelt,pceltFetched)

#define IEnumMoniker_Skip(This,celt)  \
    (This)->lpVtbl -> Skip(This,celt)

#define IEnumMoniker_Reset(This)  \
    (This)->lpVtbl -> Reset(This)

#define IEnumMoniker_Clone(This,ppenum) \
    (This)->lpVtbl -> Clone(This,ppenum)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IEnumMoniker_RemoteNext_Proxy(IEnumMoniker * This, ULONG celt, IMoniker * *rgelt, ULONG * pceltFetched);

    void __RPC_STUB IEnumMoniker_RemoteNext_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumMoniker_Skip_Proxy(IEnumMoniker * This, ULONG celt);

    void __RPC_STUB IEnumMoniker_Skip_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumMoniker_Reset_Proxy(IEnumMoniker * This);

    void __RPC_STUB IEnumMoniker_Reset_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumMoniker_Clone_Proxy(IEnumMoniker * This, IEnumMoniker * *ppenum);

    void __RPC_STUB IEnumMoniker_Clone_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IEnumMoniker_INTERFACE_DEFINED__ */

#ifndef __IRunnableObject_INTERFACE_DEFINED__
#define __IRunnableObject_INTERFACE_DEFINED__

    typedef IRunnableObject *LPRUNNABLEOBJECT;

    EXTERN_C const IID IID_IRunnableObject;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000126-0000-0000-C000-000000000046")
     IRunnableObject:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetRunningClass(LPCLSID lpClsid) = 0;

        virtual HRESULT STDMETHODCALLTYPE Run(LPBINDCTX pbc) = 0;

        virtual BOOL STDMETHODCALLTYPE IsRunning(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE LockRunning(BOOL fLock, BOOL fLastUnlockCloses) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetContainedObject(BOOL fContained) = 0;

    };

#else

    typedef struct IRunnableObjectVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRunnableObject * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRunnableObject * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRunnableObject * This);

         HRESULT(STDMETHODCALLTYPE * GetRunningClass) (IRunnableObject * This, LPCLSID lpClsid);

         HRESULT(STDMETHODCALLTYPE * Run) (IRunnableObject * This, LPBINDCTX pbc);

         BOOL(STDMETHODCALLTYPE * IsRunning) (IRunnableObject * This);

         HRESULT(STDMETHODCALLTYPE * LockRunning) (IRunnableObject * This, BOOL fLock, BOOL fLastUnlockCloses);

         HRESULT(STDMETHODCALLTYPE * SetContainedObject) (IRunnableObject * This, BOOL fContained);

     END_INTERFACE} IRunnableObjectVtbl;

    interface IRunnableObject {
        CONST_VTBL struct IRunnableObjectVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRunnableObject_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRunnableObject_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IRunnableObject_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IRunnableObject_GetRunningClass(This,lpClsid)  \
    (This)->lpVtbl -> GetRunningClass(This,lpClsid)

#define IRunnableObject_Run(This,pbc)  \
    (This)->lpVtbl -> Run(This,pbc)

#define IRunnableObject_IsRunning(This) \
    (This)->lpVtbl -> IsRunning(This)

#define IRunnableObject_LockRunning(This,fLock,fLastUnlockCloses)  \
    (This)->lpVtbl -> LockRunning(This,fLock,fLastUnlockCloses)

#define IRunnableObject_SetContainedObject(This,fContained)  \
    (This)->lpVtbl -> SetContainedObject(This,fContained)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRunnableObject_GetRunningClass_Proxy(IRunnableObject * This, LPCLSID lpClsid);

    void __RPC_STUB IRunnableObject_GetRunningClass_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunnableObject_Run_Proxy(IRunnableObject * This, LPBINDCTX pbc);

    void __RPC_STUB IRunnableObject_Run_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunnableObject_RemoteIsRunning_Proxy(IRunnableObject * This);

    void __RPC_STUB IRunnableObject_RemoteIsRunning_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunnableObject_LockRunning_Proxy(IRunnableObject * This, BOOL fLock, BOOL fLastUnlockCloses);

    void __RPC_STUB IRunnableObject_LockRunning_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunnableObject_SetContainedObject_Proxy(IRunnableObject * This, BOOL fContained);

    void __RPC_STUB IRunnableObject_SetContainedObject_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRunnableObject_INTERFACE_DEFINED__ */

#ifndef __IRunningObjectTable_INTERFACE_DEFINED__
#define __IRunningObjectTable_INTERFACE_DEFINED__

    typedef IRunningObjectTable *LPRUNNINGOBJECTTABLE;

    EXTERN_C const IID IID_IRunningObjectTable;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000010-0000-0000-C000-000000000046")
     IRunningObjectTable:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Register(DWORD grfFlags, IUnknown * punkObject, IMoniker * pmkObjectName, DWORD * pdwRegister) = 0;

        virtual HRESULT STDMETHODCALLTYPE Revoke(DWORD dwRegister) = 0;

        virtual HRESULT STDMETHODCALLTYPE IsRunning(IMoniker * pmkObjectName) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetObject(IMoniker * pmkObjectName, IUnknown * *ppunkObject) = 0;

        virtual HRESULT STDMETHODCALLTYPE NoteChangeTime(DWORD dwRegister, FILETIME * pfiletime) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetTimeOfLastChange(IMoniker * pmkObjectName, FILETIME * pfiletime) = 0;

        virtual HRESULT STDMETHODCALLTYPE EnumRunning(IEnumMoniker * *ppenumMoniker) = 0;

    };

#else

    typedef struct IRunningObjectTableVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRunningObjectTable * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRunningObjectTable * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRunningObjectTable * This);

         HRESULT(STDMETHODCALLTYPE * Register) (IRunningObjectTable * This, DWORD grfFlags, IUnknown * punkObject, IMoniker * pmkObjectName, DWORD * pdwRegister);

         HRESULT(STDMETHODCALLTYPE * Revoke) (IRunningObjectTable * This, DWORD dwRegister);

         HRESULT(STDMETHODCALLTYPE * IsRunning) (IRunningObjectTable * This, IMoniker * pmkObjectName);

         HRESULT(STDMETHODCALLTYPE * GetObject) (IRunningObjectTable * This, IMoniker * pmkObjectName, IUnknown * *ppunkObject);

         HRESULT(STDMETHODCALLTYPE * NoteChangeTime) (IRunningObjectTable * This, DWORD dwRegister, FILETIME * pfiletime);

         HRESULT(STDMETHODCALLTYPE * GetTimeOfLastChange) (IRunningObjectTable * This, IMoniker * pmkObjectName, FILETIME * pfiletime);

         HRESULT(STDMETHODCALLTYPE * EnumRunning) (IRunningObjectTable * This, IEnumMoniker * *ppenumMoniker);

     END_INTERFACE} IRunningObjectTableVtbl;

    interface IRunningObjectTable {
        CONST_VTBL struct IRunningObjectTableVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRunningObjectTable_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRunningObjectTable_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IRunningObjectTable_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IRunningObjectTable_Register(This,grfFlags,punkObject,pmkObjectName,pdwRegister)  \
    (This)->lpVtbl -> Register(This,grfFlags,punkObject,pmkObjectName,pdwRegister)

#define IRunningObjectTable_Revoke(This,dwRegister)  \
    (This)->lpVtbl -> Revoke(This,dwRegister)

#define IRunningObjectTable_IsRunning(This,pmkObjectName)  \
    (This)->lpVtbl -> IsRunning(This,pmkObjectName)

#define IRunningObjectTable_GetObject(This,pmkObjectName,ppunkObject)  \
    (This)->lpVtbl -> GetObject(This,pmkObjectName,ppunkObject)

#define IRunningObjectTable_NoteChangeTime(This,dwRegister,pfiletime)  \
    (This)->lpVtbl -> NoteChangeTime(This,dwRegister,pfiletime)

#define IRunningObjectTable_GetTimeOfLastChange(This,pmkObjectName,pfiletime)  \
    (This)->lpVtbl -> GetTimeOfLastChange(This,pmkObjectName,pfiletime)

#define IRunningObjectTable_EnumRunning(This,ppenumMoniker)  \
    (This)->lpVtbl -> EnumRunning(This,ppenumMoniker)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRunningObjectTable_Register_Proxy(IRunningObjectTable * This, DWORD grfFlags, IUnknown * punkObject, IMoniker * pmkObjectName, DWORD * pdwRegister);

    void __RPC_STUB IRunningObjectTable_Register_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunningObjectTable_Revoke_Proxy(IRunningObjectTable * This, DWORD dwRegister);

    void __RPC_STUB IRunningObjectTable_Revoke_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunningObjectTable_IsRunning_Proxy(IRunningObjectTable * This, IMoniker * pmkObjectName);

    void __RPC_STUB IRunningObjectTable_IsRunning_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunningObjectTable_GetObject_Proxy(IRunningObjectTable * This, IMoniker * pmkObjectName, IUnknown * *ppunkObject);

    void __RPC_STUB IRunningObjectTable_GetObject_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunningObjectTable_NoteChangeTime_Proxy(IRunningObjectTable * This, DWORD dwRegister, FILETIME * pfiletime);

    void __RPC_STUB IRunningObjectTable_NoteChangeTime_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunningObjectTable_GetTimeOfLastChange_Proxy(IRunningObjectTable * This, IMoniker * pmkObjectName, FILETIME * pfiletime);

    void __RPC_STUB IRunningObjectTable_GetTimeOfLastChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRunningObjectTable_EnumRunning_Proxy(IRunningObjectTable * This, IEnumMoniker * *ppenumMoniker);

    void __RPC_STUB IRunningObjectTable_EnumRunning_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRunningObjectTable_INTERFACE_DEFINED__ */

#ifndef __IPersist_INTERFACE_DEFINED__
#define __IPersist_INTERFACE_DEFINED__

    typedef IPersist *LPPERSIST;

    EXTERN_C const IID IID_IPersist;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000010c-0000-0000-C000-000000000046")
     IPersist:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetClassID(CLSID * pClassID) = 0;

    };

#else

    typedef struct IPersistVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IPersist * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IPersist * This);

         ULONG(STDMETHODCALLTYPE * Release) (IPersist * This);

         HRESULT(STDMETHODCALLTYPE * GetClassID) (IPersist * This, CLSID * pClassID);

     END_INTERFACE} IPersistVtbl;

    interface IPersist {
        CONST_VTBL struct IPersistVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IPersist_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IPersist_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IPersist_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IPersist_GetClassID(This,pClassID)  \
    (This)->lpVtbl -> GetClassID(This,pClassID)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IPersist_GetClassID_Proxy(IPersist * This, CLSID * pClassID);

    void __RPC_STUB IPersist_GetClassID_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IPersist_INTERFACE_DEFINED__ */

#ifndef __IPersistStream_INTERFACE_DEFINED__
#define __IPersistStream_INTERFACE_DEFINED__

    typedef IPersistStream *LPPERSISTSTREAM;

    EXTERN_C const IID IID_IPersistStream;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000109-0000-0000-C000-000000000046")
     IPersistStream:public IPersist {
      public:
        virtual HRESULT STDMETHODCALLTYPE IsDirty(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Load(IStream * pStm) = 0;

        virtual HRESULT STDMETHODCALLTYPE Save(IStream * pStm, BOOL fClearDirty) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetSizeMax(ULARGE_INTEGER * pcbSize) = 0;

    };

#else

    typedef struct IPersistStreamVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IPersistStream * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IPersistStream * This);

         ULONG(STDMETHODCALLTYPE * Release) (IPersistStream * This);

         HRESULT(STDMETHODCALLTYPE * GetClassID) (IPersistStream * This, CLSID * pClassID);

         HRESULT(STDMETHODCALLTYPE * IsDirty) (IPersistStream * This);

         HRESULT(STDMETHODCALLTYPE * Load) (IPersistStream * This, IStream * pStm);

         HRESULT(STDMETHODCALLTYPE * Save) (IPersistStream * This, IStream * pStm, BOOL fClearDirty);

         HRESULT(STDMETHODCALLTYPE * GetSizeMax) (IPersistStream * This, ULARGE_INTEGER * pcbSize);

     END_INTERFACE} IPersistStreamVtbl;

    interface IPersistStream {
        CONST_VTBL struct IPersistStreamVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IPersistStream_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IPersistStream_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IPersistStream_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IPersistStream_GetClassID(This,pClassID)  \
    (This)->lpVtbl -> GetClassID(This,pClassID)

#define IPersistStream_IsDirty(This)  \
    (This)->lpVtbl -> IsDirty(This)

#define IPersistStream_Load(This,pStm)  \
    (This)->lpVtbl -> Load(This,pStm)

#define IPersistStream_Save(This,pStm,fClearDirty)  \
    (This)->lpVtbl -> Save(This,pStm,fClearDirty)

#define IPersistStream_GetSizeMax(This,pcbSize) \
    (This)->lpVtbl -> GetSizeMax(This,pcbSize)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IPersistStream_IsDirty_Proxy(IPersistStream * This);

    void __RPC_STUB IPersistStream_IsDirty_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistStream_Load_Proxy(IPersistStream * This, IStream * pStm);

    void __RPC_STUB IPersistStream_Load_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistStream_Save_Proxy(IPersistStream * This, IStream * pStm, BOOL fClearDirty);

    void __RPC_STUB IPersistStream_Save_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistStream_GetSizeMax_Proxy(IPersistStream * This, ULARGE_INTEGER * pcbSize);

    void __RPC_STUB IPersistStream_GetSizeMax_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IPersistStream_INTERFACE_DEFINED__ */

#ifndef __IMoniker_INTERFACE_DEFINED__
#define __IMoniker_INTERFACE_DEFINED__

    typedef IMoniker *LPMONIKER;

    typedef
        enum tagMKSYS { MKSYS_NONE = 0,
        MKSYS_GENERICCOMPOSITE = 1,
        MKSYS_FILEMONIKER = 2,
        MKSYS_ANTIMONIKER = 3,
        MKSYS_ITEMMONIKER = 4,
        MKSYS_POINTERMONIKER = 5,
        MKSYS_CLASSMONIKER = 7,
        MKSYS_OBJREFMONIKER = 8,
        MKSYS_SESSIONMONIKER = 9
    } MKSYS;

    typedef
        enum tagMKREDUCE { MKRREDUCE_ONE = 3 << 16,
        MKRREDUCE_TOUSER = 2 << 16,
        MKRREDUCE_THROUGHUSER = 1 << 16,
        MKRREDUCE_ALL = 0
    } MKRREDUCE;

    EXTERN_C const IID IID_IMoniker;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000000f-0000-0000-C000-000000000046")
     IMoniker:public IPersistStream {
      public:
        virtual HRESULT STDMETHODCALLTYPE BindToObject(IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riidResult, void **ppvResult) = 0;

        virtual HRESULT STDMETHODCALLTYPE BindToStorage(IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riid, void **ppvObj) = 0;

        virtual HRESULT STDMETHODCALLTYPE Reduce(IBindCtx * pbc, DWORD dwReduceHowFar, IMoniker * *ppmkToLeft, IMoniker * *ppmkReduced) = 0;

        virtual HRESULT STDMETHODCALLTYPE ComposeWith(IMoniker * pmkRight, BOOL fOnlyIfNotGeneric, IMoniker * *ppmkComposite) = 0;

        virtual HRESULT STDMETHODCALLTYPE Enum(BOOL fForward, IEnumMoniker * *ppenumMoniker) = 0;

        virtual HRESULT STDMETHODCALLTYPE IsEqual(IMoniker * pmkOtherMoniker) = 0;

        virtual HRESULT STDMETHODCALLTYPE Hash(DWORD * pdwHash) = 0;

        virtual HRESULT STDMETHODCALLTYPE IsRunning(IBindCtx * pbc, IMoniker * pmkToLeft, IMoniker * pmkNewlyRunning) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetTimeOfLastChange(IBindCtx * pbc, IMoniker * pmkToLeft, FILETIME * pFileTime) = 0;

        virtual HRESULT STDMETHODCALLTYPE Inverse(IMoniker * *ppmk) = 0;

        virtual HRESULT STDMETHODCALLTYPE CommonPrefixWith(IMoniker * pmkOther, IMoniker * *ppmkPrefix) = 0;

        virtual HRESULT STDMETHODCALLTYPE RelativePathTo(IMoniker * pmkOther, IMoniker * *ppmkRelPath) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetDisplayName(IBindCtx * pbc, IMoniker * pmkToLeft, LPOLESTR * ppszDisplayName) = 0;

        virtual HRESULT STDMETHODCALLTYPE ParseDisplayName(IBindCtx * pbc, IMoniker * pmkToLeft, LPOLESTR pszDisplayName, ULONG * pchEaten, IMoniker * *ppmkOut) = 0;

        virtual HRESULT STDMETHODCALLTYPE IsSystemMoniker(DWORD * pdwMksys) = 0;

    };

#else

    typedef struct IMonikerVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IMoniker * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IMoniker * This);

         ULONG(STDMETHODCALLTYPE * Release) (IMoniker * This);

         HRESULT(STDMETHODCALLTYPE * GetClassID) (IMoniker * This, CLSID * pClassID);

         HRESULT(STDMETHODCALLTYPE * IsDirty) (IMoniker * This);

         HRESULT(STDMETHODCALLTYPE * Load) (IMoniker * This, IStream * pStm);

         HRESULT(STDMETHODCALLTYPE * Save) (IMoniker * This, IStream * pStm, BOOL fClearDirty);

         HRESULT(STDMETHODCALLTYPE * GetSizeMax) (IMoniker * This, ULARGE_INTEGER * pcbSize);

         HRESULT(STDMETHODCALLTYPE * BindToObject) (IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riidResult, void **ppvResult);

         HRESULT(STDMETHODCALLTYPE * BindToStorage) (IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riid, void **ppvObj);

         HRESULT(STDMETHODCALLTYPE * Reduce) (IMoniker * This, IBindCtx * pbc, DWORD dwReduceHowFar, IMoniker * *ppmkToLeft, IMoniker * *ppmkReduced);

         HRESULT(STDMETHODCALLTYPE * ComposeWith) (IMoniker * This, IMoniker * pmkRight, BOOL fOnlyIfNotGeneric, IMoniker * *ppmkComposite);

         HRESULT(STDMETHODCALLTYPE * Enum) (IMoniker * This, BOOL fForward, IEnumMoniker * *ppenumMoniker);

         HRESULT(STDMETHODCALLTYPE * IsEqual) (IMoniker * This, IMoniker * pmkOtherMoniker);

         HRESULT(STDMETHODCALLTYPE * Hash) (IMoniker * This, DWORD * pdwHash);

         HRESULT(STDMETHODCALLTYPE * IsRunning) (IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, IMoniker * pmkNewlyRunning);

         HRESULT(STDMETHODCALLTYPE * GetTimeOfLastChange) (IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, FILETIME * pFileTime);

         HRESULT(STDMETHODCALLTYPE * Inverse) (IMoniker * This, IMoniker * *ppmk);

         HRESULT(STDMETHODCALLTYPE * CommonPrefixWith) (IMoniker * This, IMoniker * pmkOther, IMoniker * *ppmkPrefix);

         HRESULT(STDMETHODCALLTYPE * RelativePathTo) (IMoniker * This, IMoniker * pmkOther, IMoniker * *ppmkRelPath);

         HRESULT(STDMETHODCALLTYPE * GetDisplayName) (IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, LPOLESTR * ppszDisplayName);

         HRESULT(STDMETHODCALLTYPE * ParseDisplayName) (IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, LPOLESTR pszDisplayName, ULONG * pchEaten, IMoniker * *ppmkOut);

         HRESULT(STDMETHODCALLTYPE * IsSystemMoniker) (IMoniker * This, DWORD * pdwMksys);

     END_INTERFACE} IMonikerVtbl;

    interface IMoniker {
        CONST_VTBL struct IMonikerVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IMoniker_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IMoniker_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IMoniker_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IMoniker_GetClassID(This,pClassID)  \
    (This)->lpVtbl -> GetClassID(This,pClassID)

#define IMoniker_IsDirty(This)  \
    (This)->lpVtbl -> IsDirty(This)

#define IMoniker_Load(This,pStm)  \
    (This)->lpVtbl -> Load(This,pStm)

#define IMoniker_Save(This,pStm,fClearDirty)  \
    (This)->lpVtbl -> Save(This,pStm,fClearDirty)

#define IMoniker_GetSizeMax(This,pcbSize)  \
    (This)->lpVtbl -> GetSizeMax(This,pcbSize)

#define IMoniker_BindToObject(This,pbc,pmkToLeft,riidResult,ppvResult)  \
    (This)->lpVtbl -> BindToObject(This,pbc,pmkToLeft,riidResult,ppvResult)

#define IMoniker_BindToStorage(This,pbc,pmkToLeft,riid,ppvObj)  \
    (This)->lpVtbl -> BindToStorage(This,pbc,pmkToLeft,riid,ppvObj)

#define IMoniker_Reduce(This,pbc,dwReduceHowFar,ppmkToLeft,ppmkReduced) \
    (This)->lpVtbl -> Reduce(This,pbc,dwReduceHowFar,ppmkToLeft,ppmkReduced)

#define IMoniker_ComposeWith(This,pmkRight,fOnlyIfNotGeneric,ppmkComposite)  \
    (This)->lpVtbl -> ComposeWith(This,pmkRight,fOnlyIfNotGeneric,ppmkComposite)

#define IMoniker_Enum(This,fForward,ppenumMoniker)  \
    (This)->lpVtbl -> Enum(This,fForward,ppenumMoniker)

#define IMoniker_IsEqual(This,pmkOtherMoniker)  \
    (This)->lpVtbl -> IsEqual(This,pmkOtherMoniker)

#define IMoniker_Hash(This,pdwHash)  \
    (This)->lpVtbl -> Hash(This,pdwHash)

#define IMoniker_IsRunning(This,pbc,pmkToLeft,pmkNewlyRunning)  \
    (This)->lpVtbl -> IsRunning(This,pbc,pmkToLeft,pmkNewlyRunning)

#define IMoniker_GetTimeOfLastChange(This,pbc,pmkToLeft,pFileTime)  \
    (This)->lpVtbl -> GetTimeOfLastChange(This,pbc,pmkToLeft,pFileTime)

#define IMoniker_Inverse(This,ppmk)  \
    (This)->lpVtbl -> Inverse(This,ppmk)

#define IMoniker_CommonPrefixWith(This,pmkOther,ppmkPrefix)  \
    (This)->lpVtbl -> CommonPrefixWith(This,pmkOther,ppmkPrefix)

#define IMoniker_RelativePathTo(This,pmkOther,ppmkRelPath)  \
    (This)->lpVtbl -> RelativePathTo(This,pmkOther,ppmkRelPath)

#define IMoniker_GetDisplayName(This,pbc,pmkToLeft,ppszDisplayName)  \
    (This)->lpVtbl -> GetDisplayName(This,pbc,pmkToLeft,ppszDisplayName)

#define IMoniker_ParseDisplayName(This,pbc,pmkToLeft,pszDisplayName,pchEaten,ppmkOut)  \
    (This)->lpVtbl -> ParseDisplayName(This,pbc,pmkToLeft,pszDisplayName,pchEaten,ppmkOut)

#define IMoniker_IsSystemMoniker(This,pdwMksys) \
    (This)->lpVtbl -> IsSystemMoniker(This,pdwMksys)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IMoniker_RemoteBindToObject_Proxy(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riidResult, IUnknown * *ppvResult);

    void __RPC_STUB IMoniker_RemoteBindToObject_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_RemoteBindToStorage_Proxy(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riid, IUnknown * *ppvObj);

    void __RPC_STUB IMoniker_RemoteBindToStorage_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_Reduce_Proxy(IMoniker * This, IBindCtx * pbc, DWORD dwReduceHowFar, IMoniker * *ppmkToLeft, IMoniker * *ppmkReduced);

    void __RPC_STUB IMoniker_Reduce_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_ComposeWith_Proxy(IMoniker * This, IMoniker * pmkRight, BOOL fOnlyIfNotGeneric, IMoniker * *ppmkComposite);

    void __RPC_STUB IMoniker_ComposeWith_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_Enum_Proxy(IMoniker * This, BOOL fForward, IEnumMoniker * *ppenumMoniker);

    void __RPC_STUB IMoniker_Enum_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_IsEqual_Proxy(IMoniker * This, IMoniker * pmkOtherMoniker);

    void __RPC_STUB IMoniker_IsEqual_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_Hash_Proxy(IMoniker * This, DWORD * pdwHash);

    void __RPC_STUB IMoniker_Hash_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_IsRunning_Proxy(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, IMoniker * pmkNewlyRunning);

    void __RPC_STUB IMoniker_IsRunning_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_GetTimeOfLastChange_Proxy(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, FILETIME * pFileTime);

    void __RPC_STUB IMoniker_GetTimeOfLastChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_Inverse_Proxy(IMoniker * This, IMoniker * *ppmk);

    void __RPC_STUB IMoniker_Inverse_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_CommonPrefixWith_Proxy(IMoniker * This, IMoniker * pmkOther, IMoniker * *ppmkPrefix);

    void __RPC_STUB IMoniker_CommonPrefixWith_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_RelativePathTo_Proxy(IMoniker * This, IMoniker * pmkOther, IMoniker * *ppmkRelPath);

    void __RPC_STUB IMoniker_RelativePathTo_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_GetDisplayName_Proxy(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, LPOLESTR * ppszDisplayName);

    void __RPC_STUB IMoniker_GetDisplayName_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_ParseDisplayName_Proxy(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, LPOLESTR pszDisplayName, ULONG * pchEaten, IMoniker * *ppmkOut);

    void __RPC_STUB IMoniker_ParseDisplayName_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IMoniker_IsSystemMoniker_Proxy(IMoniker * This, DWORD * pdwMksys);

    void __RPC_STUB IMoniker_IsSystemMoniker_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IMoniker_INTERFACE_DEFINED__ */

#ifndef __IROTData_INTERFACE_DEFINED__
#define __IROTData_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IROTData;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("f29f6bc0-5021-11ce-aa15-00006901293f")
     IROTData:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetComparisonData(byte * pbData, ULONG cbMax, ULONG * pcbData) = 0;

    };

#else

    typedef struct IROTDataVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IROTData * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IROTData * This);

         ULONG(STDMETHODCALLTYPE * Release) (IROTData * This);

         HRESULT(STDMETHODCALLTYPE * GetComparisonData) (IROTData * This, byte * pbData, ULONG cbMax, ULONG * pcbData);

     END_INTERFACE} IROTDataVtbl;

    interface IROTData {
        CONST_VTBL struct IROTDataVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IROTData_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IROTData_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IROTData_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IROTData_GetComparisonData(This,pbData,cbMax,pcbData)  \
    (This)->lpVtbl -> GetComparisonData(This,pbData,cbMax,pcbData)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IROTData_GetComparisonData_Proxy(IROTData * This, byte * pbData, ULONG cbMax, ULONG * pcbData);

    void __RPC_STUB IROTData_GetComparisonData_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IROTData_INTERFACE_DEFINED__ */

#ifndef __IEnumString_INTERFACE_DEFINED__
#define __IEnumString_INTERFACE_DEFINED__

    typedef IEnumString *LPENUMSTRING;

    EXTERN_C const IID IID_IEnumString;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000101-0000-0000-C000-000000000046")
     IEnumString:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Next(ULONG celt, LPOLESTR * rgelt, ULONG * pceltFetched) = 0;

        virtual HRESULT STDMETHODCALLTYPE Skip(ULONG celt) = 0;

        virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Clone(IEnumString * *ppenum) = 0;

    };

#else

    typedef struct IEnumStringVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IEnumString * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IEnumString * This);

         ULONG(STDMETHODCALLTYPE * Release) (IEnumString * This);

         HRESULT(STDMETHODCALLTYPE * Next) (IEnumString * This, ULONG celt, LPOLESTR * rgelt, ULONG * pceltFetched);

         HRESULT(STDMETHODCALLTYPE * Skip) (IEnumString * This, ULONG celt);

         HRESULT(STDMETHODCALLTYPE * Reset) (IEnumString * This);

         HRESULT(STDMETHODCALLTYPE * Clone) (IEnumString * This, IEnumString * *ppenum);

     END_INTERFACE} IEnumStringVtbl;

    interface IEnumString {
        CONST_VTBL struct IEnumStringVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IEnumString_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IEnumString_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IEnumString_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IEnumString_Next(This,celt,rgelt,pceltFetched)  \
    (This)->lpVtbl -> Next(This,celt,rgelt,pceltFetched)

#define IEnumString_Skip(This,celt)  \
    (This)->lpVtbl -> Skip(This,celt)

#define IEnumString_Reset(This) \
    (This)->lpVtbl -> Reset(This)

#define IEnumString_Clone(This,ppenum)  \
    (This)->lpVtbl -> Clone(This,ppenum)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IEnumString_RemoteNext_Proxy(IEnumString * This, ULONG celt, LPOLESTR * rgelt, ULONG * pceltFetched);

    void __RPC_STUB IEnumString_RemoteNext_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumString_Skip_Proxy(IEnumString * This, ULONG celt);

    void __RPC_STUB IEnumString_Skip_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumString_Reset_Proxy(IEnumString * This);

    void __RPC_STUB IEnumString_Reset_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumString_Clone_Proxy(IEnumString * This, IEnumString * *ppenum);

    void __RPC_STUB IEnumString_Clone_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IEnumString_INTERFACE_DEFINED__ */

#ifndef __ISequentialStream_INTERFACE_DEFINED__
#define __ISequentialStream_INTERFACE_DEFINED__

    EXTERN_C const IID IID_ISequentialStream;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0c733a30-2a1c-11ce-ade5-00aa0044773d")
     ISequentialStream:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Read(void *pv, ULONG cb, ULONG * pcbRead) = 0;

        virtual HRESULT STDMETHODCALLTYPE Write(const void *pv, ULONG cb, ULONG * pcbWritten) = 0;

    };

#else

    typedef struct ISequentialStreamVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ISequentialStream * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ISequentialStream * This);

         ULONG(STDMETHODCALLTYPE * Release) (ISequentialStream * This);

         HRESULT(STDMETHODCALLTYPE * Read) (ISequentialStream * This, void *pv, ULONG cb, ULONG * pcbRead);

         HRESULT(STDMETHODCALLTYPE * Write) (ISequentialStream * This, const void *pv, ULONG cb, ULONG * pcbWritten);

     END_INTERFACE} ISequentialStreamVtbl;

    interface ISequentialStream {
        CONST_VTBL struct ISequentialStreamVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ISequentialStream_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ISequentialStream_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define ISequentialStream_Release(This) \
    (This)->lpVtbl -> Release(This)

#define ISequentialStream_Read(This,pv,cb,pcbRead)  \
    (This)->lpVtbl -> Read(This,pv,cb,pcbRead)

#define ISequentialStream_Write(This,pv,cb,pcbWritten)  \
    (This)->lpVtbl -> Write(This,pv,cb,pcbWritten)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ISequentialStream_RemoteRead_Proxy(ISequentialStream * This, byte * pv, ULONG cb, ULONG * pcbRead);

    void __RPC_STUB ISequentialStream_RemoteRead_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ISequentialStream_RemoteWrite_Proxy(ISequentialStream * This, const byte * pv, ULONG cb, ULONG * pcbWritten);

    void __RPC_STUB ISequentialStream_RemoteWrite_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ISequentialStream_INTERFACE_DEFINED__ */

#ifndef __IStream_INTERFACE_DEFINED__
#define __IStream_INTERFACE_DEFINED__

    typedef IStream *LPSTREAM;

    typedef struct tagSTATSTG {
        LPOLESTR pwcsName;
        DWORD type;
        ULARGE_INTEGER cbSize;
        FILETIME mtime;
        FILETIME ctime;
        FILETIME atime;
        DWORD grfMode;
        DWORD grfLocksSupported;
        CLSID clsid;
        DWORD grfStateBits;
        DWORD reserved;
    } STATSTG;
    typedef
        enum tagSTGTY { STGTY_STORAGE = 1,
        STGTY_STREAM = 2,
        STGTY_LOCKBYTES = 3,
        STGTY_PROPERTY = 4
    } STGTY;

    typedef
        enum tagSTREAM_SEEK { STREAM_SEEK_SET = 0,
        STREAM_SEEK_CUR = 1,
        STREAM_SEEK_END = 2
    } STREAM_SEEK;

    typedef
        enum tagLOCKTYPE { LOCK_WRITE = 1,
        LOCK_EXCLUSIVE = 2,
        LOCK_ONLYONCE = 4
    } LOCKTYPE;

    EXTERN_C const IID IID_IStream;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000000c-0000-0000-C000-000000000046")
     IStream:public ISequentialStream {
      public:
        virtual HRESULT STDMETHODCALLTYPE Seek(LARGE_INTEGER dlibMove, DWORD dwOrigin, ULARGE_INTEGER * plibNewPosition) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetSize(ULARGE_INTEGER libNewSize) = 0;

        virtual HRESULT STDMETHODCALLTYPE CopyTo(IStream * pstm, ULARGE_INTEGER cb, ULARGE_INTEGER * pcbRead, ULARGE_INTEGER * pcbWritten) = 0;

        virtual HRESULT STDMETHODCALLTYPE Commit(DWORD grfCommitFlags) = 0;

        virtual HRESULT STDMETHODCALLTYPE Revert(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE LockRegion(ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType) = 0;

        virtual HRESULT STDMETHODCALLTYPE UnlockRegion(ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType) = 0;

        virtual HRESULT STDMETHODCALLTYPE Stat(STATSTG * pstatstg, DWORD grfStatFlag) = 0;

        virtual HRESULT STDMETHODCALLTYPE Clone(IStream * *ppstm) = 0;

    };

#else

    typedef struct IStreamVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IStream * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IStream * This);

         ULONG(STDMETHODCALLTYPE * Release) (IStream * This);

         HRESULT(STDMETHODCALLTYPE * Read) (IStream * This, void *pv, ULONG cb, ULONG * pcbRead);

         HRESULT(STDMETHODCALLTYPE * Write) (IStream * This, const void *pv, ULONG cb, ULONG * pcbWritten);

         HRESULT(STDMETHODCALLTYPE * Seek) (IStream * This, LARGE_INTEGER dlibMove, DWORD dwOrigin, ULARGE_INTEGER * plibNewPosition);

         HRESULT(STDMETHODCALLTYPE * SetSize) (IStream * This, ULARGE_INTEGER libNewSize);

         HRESULT(STDMETHODCALLTYPE * CopyTo) (IStream * This, IStream * pstm, ULARGE_INTEGER cb, ULARGE_INTEGER * pcbRead, ULARGE_INTEGER * pcbWritten);

         HRESULT(STDMETHODCALLTYPE * Commit) (IStream * This, DWORD grfCommitFlags);

         HRESULT(STDMETHODCALLTYPE * Revert) (IStream * This);

         HRESULT(STDMETHODCALLTYPE * LockRegion) (IStream * This, ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType);

         HRESULT(STDMETHODCALLTYPE * UnlockRegion) (IStream * This, ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType);

         HRESULT(STDMETHODCALLTYPE * Stat) (IStream * This, STATSTG * pstatstg, DWORD grfStatFlag);

         HRESULT(STDMETHODCALLTYPE * Clone) (IStream * This, IStream * *ppstm);

     END_INTERFACE} IStreamVtbl;

    interface IStream {
        CONST_VTBL struct IStreamVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IStream_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IStream_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IStream_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IStream_Read(This,pv,cb,pcbRead)  \
    (This)->lpVtbl -> Read(This,pv,cb,pcbRead)

#define IStream_Write(This,pv,cb,pcbWritten)  \
    (This)->lpVtbl -> Write(This,pv,cb,pcbWritten)

#define IStream_Seek(This,dlibMove,dwOrigin,plibNewPosition)  \
    (This)->lpVtbl -> Seek(This,dlibMove,dwOrigin,plibNewPosition)

#define IStream_SetSize(This,libNewSize)  \
    (This)->lpVtbl -> SetSize(This,libNewSize)

#define IStream_CopyTo(This,pstm,cb,pcbRead,pcbWritten) \
    (This)->lpVtbl -> CopyTo(This,pstm,cb,pcbRead,pcbWritten)

#define IStream_Commit(This,grfCommitFlags)  \
    (This)->lpVtbl -> Commit(This,grfCommitFlags)

#define IStream_Revert(This)  \
    (This)->lpVtbl -> Revert(This)

#define IStream_LockRegion(This,libOffset,cb,dwLockType)  \
    (This)->lpVtbl -> LockRegion(This,libOffset,cb,dwLockType)

#define IStream_UnlockRegion(This,libOffset,cb,dwLockType)  \
    (This)->lpVtbl -> UnlockRegion(This,libOffset,cb,dwLockType)

#define IStream_Stat(This,pstatstg,grfStatFlag) \
    (This)->lpVtbl -> Stat(This,pstatstg,grfStatFlag)

#define IStream_Clone(This,ppstm)  \
    (This)->lpVtbl -> Clone(This,ppstm)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IStream_RemoteSeek_Proxy(IStream * This, LARGE_INTEGER dlibMove, DWORD dwOrigin, ULARGE_INTEGER * plibNewPosition);

    void __RPC_STUB IStream_RemoteSeek_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStream_SetSize_Proxy(IStream * This, ULARGE_INTEGER libNewSize);

    void __RPC_STUB IStream_SetSize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStream_RemoteCopyTo_Proxy(IStream * This, IStream * pstm, ULARGE_INTEGER cb, ULARGE_INTEGER * pcbRead, ULARGE_INTEGER * pcbWritten);

    void __RPC_STUB IStream_RemoteCopyTo_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStream_Commit_Proxy(IStream * This, DWORD grfCommitFlags);

    void __RPC_STUB IStream_Commit_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStream_Revert_Proxy(IStream * This);

    void __RPC_STUB IStream_Revert_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStream_LockRegion_Proxy(IStream * This, ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType);

    void __RPC_STUB IStream_LockRegion_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStream_UnlockRegion_Proxy(IStream * This, ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType);

    void __RPC_STUB IStream_UnlockRegion_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStream_Stat_Proxy(IStream * This, STATSTG * pstatstg, DWORD grfStatFlag);

    void __RPC_STUB IStream_Stat_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStream_Clone_Proxy(IStream * This, IStream * *ppstm);

    void __RPC_STUB IStream_Clone_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IStream_INTERFACE_DEFINED__ */

#ifndef __IEnumSTATSTG_INTERFACE_DEFINED__
#define __IEnumSTATSTG_INTERFACE_DEFINED__

    typedef IEnumSTATSTG *LPENUMSTATSTG;

    EXTERN_C const IID IID_IEnumSTATSTG;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000000d-0000-0000-C000-000000000046")
     IEnumSTATSTG:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Next(ULONG celt, STATSTG * rgelt, ULONG * pceltFetched) = 0;

        virtual HRESULT STDMETHODCALLTYPE Skip(ULONG celt) = 0;

        virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Clone(IEnumSTATSTG * *ppenum) = 0;

    };

#else

    typedef struct IEnumSTATSTGVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IEnumSTATSTG * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IEnumSTATSTG * This);

         ULONG(STDMETHODCALLTYPE * Release) (IEnumSTATSTG * This);

         HRESULT(STDMETHODCALLTYPE * Next) (IEnumSTATSTG * This, ULONG celt, STATSTG * rgelt, ULONG * pceltFetched);

         HRESULT(STDMETHODCALLTYPE * Skip) (IEnumSTATSTG * This, ULONG celt);

         HRESULT(STDMETHODCALLTYPE * Reset) (IEnumSTATSTG * This);

         HRESULT(STDMETHODCALLTYPE * Clone) (IEnumSTATSTG * This, IEnumSTATSTG * *ppenum);

     END_INTERFACE} IEnumSTATSTGVtbl;

    interface IEnumSTATSTG {
        CONST_VTBL struct IEnumSTATSTGVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IEnumSTATSTG_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IEnumSTATSTG_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IEnumSTATSTG_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IEnumSTATSTG_Next(This,celt,rgelt,pceltFetched) \
    (This)->lpVtbl -> Next(This,celt,rgelt,pceltFetched)

#define IEnumSTATSTG_Skip(This,celt)  \
    (This)->lpVtbl -> Skip(This,celt)

#define IEnumSTATSTG_Reset(This)  \
    (This)->lpVtbl -> Reset(This)

#define IEnumSTATSTG_Clone(This,ppenum) \
    (This)->lpVtbl -> Clone(This,ppenum)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IEnumSTATSTG_RemoteNext_Proxy(IEnumSTATSTG * This, ULONG celt, STATSTG * rgelt, ULONG * pceltFetched);

    void __RPC_STUB IEnumSTATSTG_RemoteNext_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumSTATSTG_Skip_Proxy(IEnumSTATSTG * This, ULONG celt);

    void __RPC_STUB IEnumSTATSTG_Skip_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumSTATSTG_Reset_Proxy(IEnumSTATSTG * This);

    void __RPC_STUB IEnumSTATSTG_Reset_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumSTATSTG_Clone_Proxy(IEnumSTATSTG * This, IEnumSTATSTG * *ppenum);

    void __RPC_STUB IEnumSTATSTG_Clone_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IEnumSTATSTG_INTERFACE_DEFINED__ */

#ifndef __IStorage_INTERFACE_DEFINED__
#define __IStorage_INTERFACE_DEFINED__

    typedef IStorage *LPSTORAGE;

    typedef struct tagRemSNB {
        unsigned long ulCntStr;
        unsigned long ulCntChar;
        OLECHAR rgString[1];
    } RemSNB;

    typedef RemSNB *wireSNB;

    typedef OLECHAR **SNB;

    EXTERN_C const IID IID_IStorage;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000000b-0000-0000-C000-000000000046")
     IStorage:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE CreateStream(const OLECHAR * pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStream * *ppstm) = 0;

        virtual HRESULT STDMETHODCALLTYPE OpenStream(const OLECHAR * pwcsName, void *reserved1, DWORD grfMode, DWORD reserved2, IStream * *ppstm) = 0;

        virtual HRESULT STDMETHODCALLTYPE CreateStorage(const OLECHAR * pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStorage * *ppstg) = 0;

        virtual HRESULT STDMETHODCALLTYPE OpenStorage(const OLECHAR * pwcsName, IStorage * pstgPriority, DWORD grfMode, SNB snbExclude, DWORD reserved, IStorage * *ppstg) = 0;

        virtual HRESULT STDMETHODCALLTYPE CopyTo(DWORD ciidExclude, const IID * rgiidExclude, SNB snbExclude, IStorage * pstgDest) = 0;

        virtual HRESULT STDMETHODCALLTYPE MoveElementTo(const OLECHAR * pwcsName, IStorage * pstgDest, const OLECHAR * pwcsNewName, DWORD grfFlags) = 0;

        virtual HRESULT STDMETHODCALLTYPE Commit(DWORD grfCommitFlags) = 0;

        virtual HRESULT STDMETHODCALLTYPE Revert(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE EnumElements(DWORD reserved1, void *reserved2, DWORD reserved3, IEnumSTATSTG * *ppenum) = 0;

        virtual HRESULT STDMETHODCALLTYPE DestroyElement(const OLECHAR * pwcsName) = 0;

        virtual HRESULT STDMETHODCALLTYPE RenameElement(const OLECHAR * pwcsOldName, const OLECHAR * pwcsNewName) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetElementTimes(const OLECHAR * pwcsName, const FILETIME * pctime, const FILETIME * patime, const FILETIME * pmtime) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetClass(REFCLSID clsid) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetStateBits(DWORD grfStateBits, DWORD grfMask) = 0;

        virtual HRESULT STDMETHODCALLTYPE Stat(STATSTG * pstatstg, DWORD grfStatFlag) = 0;

    };

#else

    typedef struct IStorageVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IStorage * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IStorage * This);

         ULONG(STDMETHODCALLTYPE * Release) (IStorage * This);

         HRESULT(STDMETHODCALLTYPE * CreateStream) (IStorage * This, const OLECHAR * pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStream * *ppstm);

         HRESULT(STDMETHODCALLTYPE * OpenStream) (IStorage * This, const OLECHAR * pwcsName, void *reserved1, DWORD grfMode, DWORD reserved2, IStream * *ppstm);

         HRESULT(STDMETHODCALLTYPE * CreateStorage) (IStorage * This, const OLECHAR * pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStorage * *ppstg);

         HRESULT(STDMETHODCALLTYPE * OpenStorage) (IStorage * This, const OLECHAR * pwcsName, IStorage * pstgPriority, DWORD grfMode, SNB snbExclude, DWORD reserved, IStorage * *ppstg);

         HRESULT(STDMETHODCALLTYPE * CopyTo) (IStorage * This, DWORD ciidExclude, const IID * rgiidExclude, SNB snbExclude, IStorage * pstgDest);

         HRESULT(STDMETHODCALLTYPE * MoveElementTo) (IStorage * This, const OLECHAR * pwcsName, IStorage * pstgDest, const OLECHAR * pwcsNewName, DWORD grfFlags);

         HRESULT(STDMETHODCALLTYPE * Commit) (IStorage * This, DWORD grfCommitFlags);

         HRESULT(STDMETHODCALLTYPE * Revert) (IStorage * This);

         HRESULT(STDMETHODCALLTYPE * EnumElements) (IStorage * This, DWORD reserved1, void *reserved2, DWORD reserved3, IEnumSTATSTG * *ppenum);

         HRESULT(STDMETHODCALLTYPE * DestroyElement) (IStorage * This, const OLECHAR * pwcsName);

         HRESULT(STDMETHODCALLTYPE * RenameElement) (IStorage * This, const OLECHAR * pwcsOldName, const OLECHAR * pwcsNewName);

         HRESULT(STDMETHODCALLTYPE * SetElementTimes) (IStorage * This, const OLECHAR * pwcsName, const FILETIME * pctime, const FILETIME * patime, const FILETIME * pmtime);

         HRESULT(STDMETHODCALLTYPE * SetClass) (IStorage * This, REFCLSID clsid);

         HRESULT(STDMETHODCALLTYPE * SetStateBits) (IStorage * This, DWORD grfStateBits, DWORD grfMask);

         HRESULT(STDMETHODCALLTYPE * Stat) (IStorage * This, STATSTG * pstatstg, DWORD grfStatFlag);

     END_INTERFACE} IStorageVtbl;

    interface IStorage {
        CONST_VTBL struct IStorageVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IStorage_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IStorage_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IStorage_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IStorage_CreateStream(This,pwcsName,grfMode,reserved1,reserved2,ppstm)  \
    (This)->lpVtbl -> CreateStream(This,pwcsName,grfMode,reserved1,reserved2,ppstm)

#define IStorage_OpenStream(This,pwcsName,reserved1,grfMode,reserved2,ppstm)  \
    (This)->lpVtbl -> OpenStream(This,pwcsName,reserved1,grfMode,reserved2,ppstm)

#define IStorage_CreateStorage(This,pwcsName,grfMode,reserved1,reserved2,ppstg) \
    (This)->lpVtbl -> CreateStorage(This,pwcsName,grfMode,reserved1,reserved2,ppstg)

#define IStorage_OpenStorage(This,pwcsName,pstgPriority,grfMode,snbExclude,reserved,ppstg)  \
    (This)->lpVtbl -> OpenStorage(This,pwcsName,pstgPriority,grfMode,snbExclude,reserved,ppstg)

#define IStorage_CopyTo(This,ciidExclude,rgiidExclude,snbExclude,pstgDest)  \
    (This)->lpVtbl -> CopyTo(This,ciidExclude,rgiidExclude,snbExclude,pstgDest)

#define IStorage_MoveElementTo(This,pwcsName,pstgDest,pwcsNewName,grfFlags)  \
    (This)->lpVtbl -> MoveElementTo(This,pwcsName,pstgDest,pwcsNewName,grfFlags)

#define IStorage_Commit(This,grfCommitFlags)  \
    (This)->lpVtbl -> Commit(This,grfCommitFlags)

#define IStorage_Revert(This)  \
    (This)->lpVtbl -> Revert(This)

#define IStorage_EnumElements(This,reserved1,reserved2,reserved3,ppenum)  \
    (This)->lpVtbl -> EnumElements(This,reserved1,reserved2,reserved3,ppenum)

#define IStorage_DestroyElement(This,pwcsName)  \
    (This)->lpVtbl -> DestroyElement(This,pwcsName)

#define IStorage_RenameElement(This,pwcsOldName,pwcsNewName)  \
    (This)->lpVtbl -> RenameElement(This,pwcsOldName,pwcsNewName)

#define IStorage_SetElementTimes(This,pwcsName,pctime,patime,pmtime)  \
    (This)->lpVtbl -> SetElementTimes(This,pwcsName,pctime,patime,pmtime)

#define IStorage_SetClass(This,clsid)  \
    (This)->lpVtbl -> SetClass(This,clsid)

#define IStorage_SetStateBits(This,grfStateBits,grfMask)  \
    (This)->lpVtbl -> SetStateBits(This,grfStateBits,grfMask)

#define IStorage_Stat(This,pstatstg,grfStatFlag)  \
    (This)->lpVtbl -> Stat(This,pstatstg,grfStatFlag)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IStorage_CreateStream_Proxy(IStorage * This, const OLECHAR * pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStream * *ppstm);

    void __RPC_STUB IStorage_CreateStream_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_RemoteOpenStream_Proxy(IStorage * This, const OLECHAR * pwcsName, unsigned long cbReserved1, byte * reserved1, DWORD grfMode, DWORD reserved2, IStream * *ppstm);

    void __RPC_STUB IStorage_RemoteOpenStream_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_CreateStorage_Proxy(IStorage * This, const OLECHAR * pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStorage * *ppstg);

    void __RPC_STUB IStorage_CreateStorage_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_OpenStorage_Proxy(IStorage * This, const OLECHAR * pwcsName, IStorage * pstgPriority, DWORD grfMode, SNB snbExclude, DWORD reserved, IStorage * *ppstg);

    void __RPC_STUB IStorage_OpenStorage_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_CopyTo_Proxy(IStorage * This, DWORD ciidExclude, const IID * rgiidExclude, SNB snbExclude, IStorage * pstgDest);

    void __RPC_STUB IStorage_CopyTo_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_MoveElementTo_Proxy(IStorage * This, const OLECHAR * pwcsName, IStorage * pstgDest, const OLECHAR * pwcsNewName, DWORD grfFlags);

    void __RPC_STUB IStorage_MoveElementTo_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_Commit_Proxy(IStorage * This, DWORD grfCommitFlags);

    void __RPC_STUB IStorage_Commit_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_Revert_Proxy(IStorage * This);

    void __RPC_STUB IStorage_Revert_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_RemoteEnumElements_Proxy(IStorage * This, DWORD reserved1, unsigned long cbReserved2, byte * reserved2, DWORD reserved3, IEnumSTATSTG * *ppenum);

    void __RPC_STUB IStorage_RemoteEnumElements_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_DestroyElement_Proxy(IStorage * This, const OLECHAR * pwcsName);

    void __RPC_STUB IStorage_DestroyElement_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_RenameElement_Proxy(IStorage * This, const OLECHAR * pwcsOldName, const OLECHAR * pwcsNewName);

    void __RPC_STUB IStorage_RenameElement_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_SetElementTimes_Proxy(IStorage * This, const OLECHAR * pwcsName, const FILETIME * pctime, const FILETIME * patime, const FILETIME * pmtime);

    void __RPC_STUB IStorage_SetElementTimes_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_SetClass_Proxy(IStorage * This, REFCLSID clsid);

    void __RPC_STUB IStorage_SetClass_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_SetStateBits_Proxy(IStorage * This, DWORD grfStateBits, DWORD grfMask);

    void __RPC_STUB IStorage_SetStateBits_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IStorage_Stat_Proxy(IStorage * This, STATSTG * pstatstg, DWORD grfStatFlag);

    void __RPC_STUB IStorage_Stat_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IStorage_INTERFACE_DEFINED__ */

#ifndef __IPersistFile_INTERFACE_DEFINED__
#define __IPersistFile_INTERFACE_DEFINED__

    typedef IPersistFile *LPPERSISTFILE;

    EXTERN_C const IID IID_IPersistFile;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000010b-0000-0000-C000-000000000046")
     IPersistFile:public IPersist {
      public:
        virtual HRESULT STDMETHODCALLTYPE IsDirty(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Load(LPCOLESTR pszFileName, DWORD dwMode) = 0;

        virtual HRESULT STDMETHODCALLTYPE Save(LPCOLESTR pszFileName, BOOL fRemember) = 0;

        virtual HRESULT STDMETHODCALLTYPE SaveCompleted(LPCOLESTR pszFileName) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetCurFile(LPOLESTR * ppszFileName) = 0;

    };

#else

    typedef struct IPersistFileVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IPersistFile * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IPersistFile * This);

         ULONG(STDMETHODCALLTYPE * Release) (IPersistFile * This);

         HRESULT(STDMETHODCALLTYPE * GetClassID) (IPersistFile * This, CLSID * pClassID);

         HRESULT(STDMETHODCALLTYPE * IsDirty) (IPersistFile * This);

         HRESULT(STDMETHODCALLTYPE * Load) (IPersistFile * This, LPCOLESTR pszFileName, DWORD dwMode);

         HRESULT(STDMETHODCALLTYPE * Save) (IPersistFile * This, LPCOLESTR pszFileName, BOOL fRemember);

         HRESULT(STDMETHODCALLTYPE * SaveCompleted) (IPersistFile * This, LPCOLESTR pszFileName);

         HRESULT(STDMETHODCALLTYPE * GetCurFile) (IPersistFile * This, LPOLESTR * ppszFileName);

     END_INTERFACE} IPersistFileVtbl;

    interface IPersistFile {
        CONST_VTBL struct IPersistFileVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IPersistFile_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IPersistFile_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IPersistFile_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IPersistFile_GetClassID(This,pClassID)  \
    (This)->lpVtbl -> GetClassID(This,pClassID)

#define IPersistFile_IsDirty(This)  \
    (This)->lpVtbl -> IsDirty(This)

#define IPersistFile_Load(This,pszFileName,dwMode)  \
    (This)->lpVtbl -> Load(This,pszFileName,dwMode)

#define IPersistFile_Save(This,pszFileName,fRemember)  \
    (This)->lpVtbl -> Save(This,pszFileName,fRemember)

#define IPersistFile_SaveCompleted(This,pszFileName)  \
    (This)->lpVtbl -> SaveCompleted(This,pszFileName)

#define IPersistFile_GetCurFile(This,ppszFileName)  \
    (This)->lpVtbl -> GetCurFile(This,ppszFileName)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IPersistFile_IsDirty_Proxy(IPersistFile * This);

    void __RPC_STUB IPersistFile_IsDirty_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistFile_Load_Proxy(IPersistFile * This, LPCOLESTR pszFileName, DWORD dwMode);

    void __RPC_STUB IPersistFile_Load_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistFile_Save_Proxy(IPersistFile * This, LPCOLESTR pszFileName, BOOL fRemember);

    void __RPC_STUB IPersistFile_Save_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistFile_SaveCompleted_Proxy(IPersistFile * This, LPCOLESTR pszFileName);

    void __RPC_STUB IPersistFile_SaveCompleted_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistFile_GetCurFile_Proxy(IPersistFile * This, LPOLESTR * ppszFileName);

    void __RPC_STUB IPersistFile_GetCurFile_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IPersistFile_INTERFACE_DEFINED__ */

#ifndef __IPersistStorage_INTERFACE_DEFINED__
#define __IPersistStorage_INTERFACE_DEFINED__

    typedef IPersistStorage *LPPERSISTSTORAGE;

    EXTERN_C const IID IID_IPersistStorage;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000010a-0000-0000-C000-000000000046")
     IPersistStorage:public IPersist {
      public:
        virtual HRESULT STDMETHODCALLTYPE IsDirty(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE InitNew(IStorage * pStg) = 0;

        virtual HRESULT STDMETHODCALLTYPE Load(IStorage * pStg) = 0;

        virtual HRESULT STDMETHODCALLTYPE Save(IStorage * pStgSave, BOOL fSameAsLoad) = 0;

        virtual HRESULT STDMETHODCALLTYPE SaveCompleted(IStorage * pStgNew) = 0;

        virtual HRESULT STDMETHODCALLTYPE HandsOffStorage(void) = 0;

    };

#else

    typedef struct IPersistStorageVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IPersistStorage * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IPersistStorage * This);

         ULONG(STDMETHODCALLTYPE * Release) (IPersistStorage * This);

         HRESULT(STDMETHODCALLTYPE * GetClassID) (IPersistStorage * This, CLSID * pClassID);

         HRESULT(STDMETHODCALLTYPE * IsDirty) (IPersistStorage * This);

         HRESULT(STDMETHODCALLTYPE * InitNew) (IPersistStorage * This, IStorage * pStg);

         HRESULT(STDMETHODCALLTYPE * Load) (IPersistStorage * This, IStorage * pStg);

         HRESULT(STDMETHODCALLTYPE * Save) (IPersistStorage * This, IStorage * pStgSave, BOOL fSameAsLoad);

         HRESULT(STDMETHODCALLTYPE * SaveCompleted) (IPersistStorage * This, IStorage * pStgNew);

         HRESULT(STDMETHODCALLTYPE * HandsOffStorage) (IPersistStorage * This);

     END_INTERFACE} IPersistStorageVtbl;

    interface IPersistStorage {
        CONST_VTBL struct IPersistStorageVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IPersistStorage_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IPersistStorage_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IPersistStorage_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IPersistStorage_GetClassID(This,pClassID)  \
    (This)->lpVtbl -> GetClassID(This,pClassID)

#define IPersistStorage_IsDirty(This)  \
    (This)->lpVtbl -> IsDirty(This)

#define IPersistStorage_InitNew(This,pStg)  \
    (This)->lpVtbl -> InitNew(This,pStg)

#define IPersistStorage_Load(This,pStg) \
    (This)->lpVtbl -> Load(This,pStg)

#define IPersistStorage_Save(This,pStgSave,fSameAsLoad) \
    (This)->lpVtbl -> Save(This,pStgSave,fSameAsLoad)

#define IPersistStorage_SaveCompleted(This,pStgNew)  \
    (This)->lpVtbl -> SaveCompleted(This,pStgNew)

#define IPersistStorage_HandsOffStorage(This)  \
    (This)->lpVtbl -> HandsOffStorage(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IPersistStorage_IsDirty_Proxy(IPersistStorage * This);

    void __RPC_STUB IPersistStorage_IsDirty_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistStorage_InitNew_Proxy(IPersistStorage * This, IStorage * pStg);

    void __RPC_STUB IPersistStorage_InitNew_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistStorage_Load_Proxy(IPersistStorage * This, IStorage * pStg);

    void __RPC_STUB IPersistStorage_Load_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistStorage_Save_Proxy(IPersistStorage * This, IStorage * pStgSave, BOOL fSameAsLoad);

    void __RPC_STUB IPersistStorage_Save_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistStorage_SaveCompleted_Proxy(IPersistStorage * This, IStorage * pStgNew);

    void __RPC_STUB IPersistStorage_SaveCompleted_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPersistStorage_HandsOffStorage_Proxy(IPersistStorage * This);

    void __RPC_STUB IPersistStorage_HandsOffStorage_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IPersistStorage_INTERFACE_DEFINED__ */

#ifndef __ILockBytes_INTERFACE_DEFINED__
#define __ILockBytes_INTERFACE_DEFINED__

    typedef ILockBytes *LPLOCKBYTES;

    EXTERN_C const IID IID_ILockBytes;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000000a-0000-0000-C000-000000000046")
     ILockBytes:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE ReadAt(ULARGE_INTEGER ulOffset, void *pv, ULONG cb, ULONG * pcbRead) = 0;

        virtual HRESULT STDMETHODCALLTYPE WriteAt(ULARGE_INTEGER ulOffset, const void *pv, ULONG cb, ULONG * pcbWritten) = 0;

        virtual HRESULT STDMETHODCALLTYPE Flush(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetSize(ULARGE_INTEGER cb) = 0;

        virtual HRESULT STDMETHODCALLTYPE LockRegion(ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType) = 0;

        virtual HRESULT STDMETHODCALLTYPE UnlockRegion(ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType) = 0;

        virtual HRESULT STDMETHODCALLTYPE Stat(STATSTG * pstatstg, DWORD grfStatFlag) = 0;

    };

#else

    typedef struct ILockBytesVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ILockBytes * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ILockBytes * This);

         ULONG(STDMETHODCALLTYPE * Release) (ILockBytes * This);

         HRESULT(STDMETHODCALLTYPE * ReadAt) (ILockBytes * This, ULARGE_INTEGER ulOffset, void *pv, ULONG cb, ULONG * pcbRead);

         HRESULT(STDMETHODCALLTYPE * WriteAt) (ILockBytes * This, ULARGE_INTEGER ulOffset, const void *pv, ULONG cb, ULONG * pcbWritten);

         HRESULT(STDMETHODCALLTYPE * Flush) (ILockBytes * This);

         HRESULT(STDMETHODCALLTYPE * SetSize) (ILockBytes * This, ULARGE_INTEGER cb);

         HRESULT(STDMETHODCALLTYPE * LockRegion) (ILockBytes * This, ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType);

         HRESULT(STDMETHODCALLTYPE * UnlockRegion) (ILockBytes * This, ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType);

         HRESULT(STDMETHODCALLTYPE * Stat) (ILockBytes * This, STATSTG * pstatstg, DWORD grfStatFlag);

     END_INTERFACE} ILockBytesVtbl;

    interface ILockBytes {
        CONST_VTBL struct ILockBytesVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ILockBytes_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ILockBytes_AddRef(This) \
    (This)->lpVtbl -> AddRef(This)

#define ILockBytes_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define ILockBytes_ReadAt(This,ulOffset,pv,cb,pcbRead)  \
    (This)->lpVtbl -> ReadAt(This,ulOffset,pv,cb,pcbRead)

#define ILockBytes_WriteAt(This,ulOffset,pv,cb,pcbWritten)  \
    (This)->lpVtbl -> WriteAt(This,ulOffset,pv,cb,pcbWritten)

#define ILockBytes_Flush(This)  \
    (This)->lpVtbl -> Flush(This)

#define ILockBytes_SetSize(This,cb)  \
    (This)->lpVtbl -> SetSize(This,cb)

#define ILockBytes_LockRegion(This,libOffset,cb,dwLockType)  \
    (This)->lpVtbl -> LockRegion(This,libOffset,cb,dwLockType)

#define ILockBytes_UnlockRegion(This,libOffset,cb,dwLockType)  \
    (This)->lpVtbl -> UnlockRegion(This,libOffset,cb,dwLockType)

#define ILockBytes_Stat(This,pstatstg,grfStatFlag)  \
    (This)->lpVtbl -> Stat(This,pstatstg,grfStatFlag)

#endif /* COBJMACROS */

#endif

    HRESULT __stdcall ILockBytes_RemoteReadAt_Proxy(ILockBytes * This, ULARGE_INTEGER ulOffset, byte * pv, ULONG cb, ULONG * pcbRead);

    void __RPC_STUB ILockBytes_RemoteReadAt_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ILockBytes_RemoteWriteAt_Proxy(ILockBytes * This, ULARGE_INTEGER ulOffset, const byte * pv, ULONG cb, ULONG * pcbWritten);

    void __RPC_STUB ILockBytes_RemoteWriteAt_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ILockBytes_Flush_Proxy(ILockBytes * This);

    void __RPC_STUB ILockBytes_Flush_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ILockBytes_SetSize_Proxy(ILockBytes * This, ULARGE_INTEGER cb);

    void __RPC_STUB ILockBytes_SetSize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ILockBytes_LockRegion_Proxy(ILockBytes * This, ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType);

    void __RPC_STUB ILockBytes_LockRegion_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ILockBytes_UnlockRegion_Proxy(ILockBytes * This, ULARGE_INTEGER libOffset, ULARGE_INTEGER cb, DWORD dwLockType);

    void __RPC_STUB ILockBytes_UnlockRegion_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ILockBytes_Stat_Proxy(ILockBytes * This, STATSTG * pstatstg, DWORD grfStatFlag);

    void __RPC_STUB ILockBytes_Stat_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ILockBytes_INTERFACE_DEFINED__ */

#ifndef __IEnumFORMATETC_INTERFACE_DEFINED__
#define __IEnumFORMATETC_INTERFACE_DEFINED__

    typedef IEnumFORMATETC *LPENUMFORMATETC;

    typedef struct tagDVTARGETDEVICE {
        DWORD tdSize;
        WORD tdDriverNameOffset;
        WORD tdDeviceNameOffset;
        WORD tdPortNameOffset;
        WORD tdExtDevmodeOffset;
        BYTE tdData[1];
    } DVTARGETDEVICE;

    typedef CLIPFORMAT *LPCLIPFORMAT;

    typedef struct tagFORMATETC {
        CLIPFORMAT cfFormat;
        DVTARGETDEVICE *ptd;
        DWORD dwAspect;
        LONG lindex;
        DWORD tymed;
    } FORMATETC;

    typedef struct tagFORMATETC *LPFORMATETC;

    EXTERN_C const IID IID_IEnumFORMATETC;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000103-0000-0000-C000-000000000046")
     IEnumFORMATETC:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Next(ULONG celt, FORMATETC * rgelt, ULONG * pceltFetched) = 0;

        virtual HRESULT STDMETHODCALLTYPE Skip(ULONG celt) = 0;

        virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Clone(IEnumFORMATETC * *ppenum) = 0;

    };

#else

    typedef struct IEnumFORMATETCVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IEnumFORMATETC * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IEnumFORMATETC * This);

         ULONG(STDMETHODCALLTYPE * Release) (IEnumFORMATETC * This);

         HRESULT(STDMETHODCALLTYPE * Next) (IEnumFORMATETC * This, ULONG celt, FORMATETC * rgelt, ULONG * pceltFetched);

         HRESULT(STDMETHODCALLTYPE * Skip) (IEnumFORMATETC * This, ULONG celt);

         HRESULT(STDMETHODCALLTYPE * Reset) (IEnumFORMATETC * This);

         HRESULT(STDMETHODCALLTYPE * Clone) (IEnumFORMATETC * This, IEnumFORMATETC * *ppenum);

     END_INTERFACE} IEnumFORMATETCVtbl;

    interface IEnumFORMATETC {
        CONST_VTBL struct IEnumFORMATETCVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IEnumFORMATETC_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IEnumFORMATETC_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IEnumFORMATETC_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IEnumFORMATETC_Next(This,celt,rgelt,pceltFetched)  \
    (This)->lpVtbl -> Next(This,celt,rgelt,pceltFetched)

#define IEnumFORMATETC_Skip(This,celt)  \
    (This)->lpVtbl -> Skip(This,celt)

#define IEnumFORMATETC_Reset(This)  \
    (This)->lpVtbl -> Reset(This)

#define IEnumFORMATETC_Clone(This,ppenum)  \
    (This)->lpVtbl -> Clone(This,ppenum)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IEnumFORMATETC_RemoteNext_Proxy(IEnumFORMATETC * This, ULONG celt, FORMATETC * rgelt, ULONG * pceltFetched);

    void __RPC_STUB IEnumFORMATETC_RemoteNext_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumFORMATETC_Skip_Proxy(IEnumFORMATETC * This, ULONG celt);

    void __RPC_STUB IEnumFORMATETC_Skip_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumFORMATETC_Reset_Proxy(IEnumFORMATETC * This);

    void __RPC_STUB IEnumFORMATETC_Reset_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumFORMATETC_Clone_Proxy(IEnumFORMATETC * This, IEnumFORMATETC * *ppenum);

    void __RPC_STUB IEnumFORMATETC_Clone_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IEnumFORMATETC_INTERFACE_DEFINED__ */

#ifndef __IEnumSTATDATA_INTERFACE_DEFINED__
#define __IEnumSTATDATA_INTERFACE_DEFINED__

    typedef IEnumSTATDATA *LPENUMSTATDATA;

    typedef
        enum tagADVF { ADVF_NODATA = 1,
        ADVF_PRIMEFIRST = 2,
        ADVF_ONLYONCE = 4,
        ADVF_DATAONSTOP = 64,
        ADVFCACHE_NOHANDLER = 8,
        ADVFCACHE_FORCEBUILTIN = 16,
        ADVFCACHE_ONSAVE = 32
    } ADVF;

    typedef struct tagSTATDATA {
        FORMATETC formatetc;
        DWORD advf;
        IAdviseSink *pAdvSink;
        DWORD dwConnection;
    } STATDATA;

    typedef STATDATA *LPSTATDATA;

    EXTERN_C const IID IID_IEnumSTATDATA;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000105-0000-0000-C000-000000000046")
     IEnumSTATDATA:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Next(ULONG celt, STATDATA * rgelt, ULONG * pceltFetched) = 0;

        virtual HRESULT STDMETHODCALLTYPE Skip(ULONG celt) = 0;

        virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Clone(IEnumSTATDATA * *ppenum) = 0;

    };

#else

    typedef struct IEnumSTATDATAVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IEnumSTATDATA * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IEnumSTATDATA * This);

         ULONG(STDMETHODCALLTYPE * Release) (IEnumSTATDATA * This);

         HRESULT(STDMETHODCALLTYPE * Next) (IEnumSTATDATA * This, ULONG celt, STATDATA * rgelt, ULONG * pceltFetched);

         HRESULT(STDMETHODCALLTYPE * Skip) (IEnumSTATDATA * This, ULONG celt);

         HRESULT(STDMETHODCALLTYPE * Reset) (IEnumSTATDATA * This);

         HRESULT(STDMETHODCALLTYPE * Clone) (IEnumSTATDATA * This, IEnumSTATDATA * *ppenum);

     END_INTERFACE} IEnumSTATDATAVtbl;

    interface IEnumSTATDATA {
        CONST_VTBL struct IEnumSTATDATAVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IEnumSTATDATA_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IEnumSTATDATA_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IEnumSTATDATA_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IEnumSTATDATA_Next(This,celt,rgelt,pceltFetched)  \
    (This)->lpVtbl -> Next(This,celt,rgelt,pceltFetched)

#define IEnumSTATDATA_Skip(This,celt)  \
    (This)->lpVtbl -> Skip(This,celt)

#define IEnumSTATDATA_Reset(This)  \
    (This)->lpVtbl -> Reset(This)

#define IEnumSTATDATA_Clone(This,ppenum)  \
    (This)->lpVtbl -> Clone(This,ppenum)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IEnumSTATDATA_RemoteNext_Proxy(IEnumSTATDATA * This, ULONG celt, STATDATA * rgelt, ULONG * pceltFetched);

    void __RPC_STUB IEnumSTATDATA_RemoteNext_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumSTATDATA_Skip_Proxy(IEnumSTATDATA * This, ULONG celt);

    void __RPC_STUB IEnumSTATDATA_Skip_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumSTATDATA_Reset_Proxy(IEnumSTATDATA * This);

    void __RPC_STUB IEnumSTATDATA_Reset_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IEnumSTATDATA_Clone_Proxy(IEnumSTATDATA * This, IEnumSTATDATA * *ppenum);

    void __RPC_STUB IEnumSTATDATA_Clone_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IEnumSTATDATA_INTERFACE_DEFINED__ */

#ifndef __IRootStorage_INTERFACE_DEFINED__
#define __IRootStorage_INTERFACE_DEFINED__

    typedef IRootStorage *LPROOTSTORAGE;

    EXTERN_C const IID IID_IRootStorage;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000012-0000-0000-C000-000000000046")
     IRootStorage:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE SwitchToFile(LPOLESTR pszFile) = 0;

    };

#else

    typedef struct IRootStorageVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRootStorage * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRootStorage * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRootStorage * This);

         HRESULT(STDMETHODCALLTYPE * SwitchToFile) (IRootStorage * This, LPOLESTR pszFile);

     END_INTERFACE} IRootStorageVtbl;

    interface IRootStorage {
        CONST_VTBL struct IRootStorageVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRootStorage_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRootStorage_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IRootStorage_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IRootStorage_SwitchToFile(This,pszFile) \
    (This)->lpVtbl -> SwitchToFile(This,pszFile)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRootStorage_SwitchToFile_Proxy(IRootStorage * This, LPOLESTR pszFile);

    void __RPC_STUB IRootStorage_SwitchToFile_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRootStorage_INTERFACE_DEFINED__ */

#ifndef __IAdviseSink_INTERFACE_DEFINED__
#define __IAdviseSink_INTERFACE_DEFINED__

    typedef IAdviseSink *LPADVISESINK;

    typedef
        enum tagTYMED { TYMED_HGLOBAL = 1,
        TYMED_FILE = 2,
        TYMED_ISTREAM = 4,
        TYMED_ISTORAGE = 8,
        TYMED_GDI = 16,
        TYMED_MFPICT = 32,
        TYMED_ENHMF = 64,
        TYMED_NULL = 0
    } TYMED;

    typedef struct tagRemSTGMEDIUM {
        DWORD tymed;
        DWORD dwHandleType;
        unsigned long pData;
        unsigned long pUnkForRelease;
        unsigned long cbData;
        byte data[1];
    } RemSTGMEDIUM;

#ifdef NONAMELESSUNION
    typedef struct tagSTGMEDIUM {
        DWORD tymed;
        union {
            HBITMAP hBitmap;
            HMETAFILEPICT hMetaFilePict;
            HENHMETAFILE hEnhMetaFile;
            HGLOBAL hGlobal;
            LPOLESTR lpszFileName;
            IStream *pstm;
            IStorage *pstg;
        } u;
        IUnknown *pUnkForRelease;
    } uSTGMEDIUM;
#else
    typedef struct tagSTGMEDIUM {
        DWORD tymed;
        union {
            HBITMAP hBitmap;
            HMETAFILEPICT hMetaFilePict;
            HENHMETAFILE hEnhMetaFile;
            HGLOBAL hGlobal;
            LPOLESTR lpszFileName;
            IStream *pstm;
            IStorage *pstg;
        };
        IUnknown *pUnkForRelease;
    } uSTGMEDIUM;

#endif /* !NONAMELESSUNION */
    typedef struct _GDI_OBJECT {
        DWORD ObjectType;
        union __MIDL_IAdviseSink_0002 {
            wireHBITMAP hBitmap;
            wireHPALETTE hPalette;
            wireHGLOBAL hGeneric;
        } u;
    } GDI_OBJECT;

    typedef struct _userSTGMEDIUM {
        struct _STGMEDIUM_UNION {
            DWORD tymed;
            union __MIDL_IAdviseSink_0003 {

                wireHMETAFILEPICT hMetaFilePict;
                wireHENHMETAFILE hHEnhMetaFile;
                GDI_OBJECT *hGdiHandle;
                wireHGLOBAL hGlobal;
                LPOLESTR lpszFileName;
                BYTE_BLOB *pstm;
                BYTE_BLOB *pstg;
            } u;
        };
        IUnknown *pUnkForRelease;
    } userSTGMEDIUM;

    typedef userSTGMEDIUM *wireSTGMEDIUM;

    typedef uSTGMEDIUM STGMEDIUM;

    typedef userSTGMEDIUM *wireASYNC_STGMEDIUM;

    typedef STGMEDIUM ASYNC_STGMEDIUM;

    typedef STGMEDIUM *LPSTGMEDIUM;

    typedef struct _userFLAG_STGMEDIUM {
        long ContextFlags;
        long fPassOwnership;
        userSTGMEDIUM Stgmed;
    } userFLAG_STGMEDIUM;

    typedef userFLAG_STGMEDIUM *wireFLAG_STGMEDIUM;

    typedef struct _FLAG_STGMEDIUM {
        long ContextFlags;
        long fPassOwnership;
        STGMEDIUM Stgmed;
    } FLAG_STGMEDIUM;

    EXTERN_C const IID IID_IAdviseSink;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000010f-0000-0000-C000-000000000046")
     IAdviseSink:public IUnknown {
      public:
        virtual void STDMETHODCALLTYPE OnDataChange(FORMATETC * pFormatetc, STGMEDIUM * pStgmed) = 0;

        virtual void STDMETHODCALLTYPE OnViewChange(DWORD dwAspect, LONG lindex) = 0;

        virtual void STDMETHODCALLTYPE OnRename(IMoniker * pmk) = 0;

        virtual void STDMETHODCALLTYPE OnSave(void) = 0;

        virtual void STDMETHODCALLTYPE OnClose(void) = 0;

    };

#else

    typedef struct IAdviseSinkVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IAdviseSink * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IAdviseSink * This);

         ULONG(STDMETHODCALLTYPE * Release) (IAdviseSink * This);

        void (STDMETHODCALLTYPE * OnDataChange) (IAdviseSink * This, FORMATETC * pFormatetc, STGMEDIUM * pStgmed);

        void (STDMETHODCALLTYPE * OnViewChange) (IAdviseSink * This, DWORD dwAspect, LONG lindex);

        void (STDMETHODCALLTYPE * OnRename) (IAdviseSink * This, IMoniker * pmk);

        void (STDMETHODCALLTYPE * OnSave) (IAdviseSink * This);

        void (STDMETHODCALLTYPE * OnClose) (IAdviseSink * This);

     END_INTERFACE} IAdviseSinkVtbl;

    interface IAdviseSink {
        CONST_VTBL struct IAdviseSinkVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IAdviseSink_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAdviseSink_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IAdviseSink_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IAdviseSink_OnDataChange(This,pFormatetc,pStgmed)  \
    (This)->lpVtbl -> OnDataChange(This,pFormatetc,pStgmed)

#define IAdviseSink_OnViewChange(This,dwAspect,lindex)  \
    (This)->lpVtbl -> OnViewChange(This,dwAspect,lindex)

#define IAdviseSink_OnRename(This,pmk)  \
    (This)->lpVtbl -> OnRename(This,pmk)

#define IAdviseSink_OnSave(This)  \
    (This)->lpVtbl -> OnSave(This)

#define IAdviseSink_OnClose(This)  \
    (This)->lpVtbl -> OnClose(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IAdviseSink_RemoteOnDataChange_Proxy(IAdviseSink * This, FORMATETC * pFormatetc, ASYNC_STGMEDIUM * pStgmed);

    void __RPC_STUB IAdviseSink_RemoteOnDataChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IAdviseSink_RemoteOnViewChange_Proxy(IAdviseSink * This, DWORD dwAspect, LONG lindex);

    void __RPC_STUB IAdviseSink_RemoteOnViewChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IAdviseSink_RemoteOnRename_Proxy(IAdviseSink * This, IMoniker * pmk);

    void __RPC_STUB IAdviseSink_RemoteOnRename_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IAdviseSink_RemoteOnSave_Proxy(IAdviseSink * This);

    void __RPC_STUB IAdviseSink_RemoteOnSave_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IAdviseSink_RemoteOnClose_Proxy(IAdviseSink * This);

    void __RPC_STUB IAdviseSink_RemoteOnClose_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IAdviseSink_INTERFACE_DEFINED__ */

#ifndef __AsyncIAdviseSink_INTERFACE_DEFINED__
#define __AsyncIAdviseSink_INTERFACE_DEFINED__

    EXTERN_C const IID IID_AsyncIAdviseSink;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000150-0000-0000-C000-000000000046")
     AsyncIAdviseSink:public IUnknown {
      public:
        virtual void STDMETHODCALLTYPE Begin_OnDataChange(FORMATETC * pFormatetc, STGMEDIUM * pStgmed) = 0;

        virtual void STDMETHODCALLTYPE Finish_OnDataChange(void) = 0;

        virtual void STDMETHODCALLTYPE Begin_OnViewChange(DWORD dwAspect, LONG lindex) = 0;

        virtual void STDMETHODCALLTYPE Finish_OnViewChange(void) = 0;

        virtual void STDMETHODCALLTYPE Begin_OnRename(IMoniker * pmk) = 0;

        virtual void STDMETHODCALLTYPE Finish_OnRename(void) = 0;

        virtual void STDMETHODCALLTYPE Begin_OnSave(void) = 0;

        virtual void STDMETHODCALLTYPE Finish_OnSave(void) = 0;

        virtual void STDMETHODCALLTYPE Begin_OnClose(void) = 0;

        virtual void STDMETHODCALLTYPE Finish_OnClose(void) = 0;

    };

#else

    typedef struct AsyncIAdviseSinkVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (AsyncIAdviseSink * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (AsyncIAdviseSink * This);

         ULONG(STDMETHODCALLTYPE * Release) (AsyncIAdviseSink * This);

        void (STDMETHODCALLTYPE * Begin_OnDataChange) (AsyncIAdviseSink * This, FORMATETC * pFormatetc, STGMEDIUM * pStgmed);

        void (STDMETHODCALLTYPE * Finish_OnDataChange) (AsyncIAdviseSink * This);

        void (STDMETHODCALLTYPE * Begin_OnViewChange) (AsyncIAdviseSink * This, DWORD dwAspect, LONG lindex);

        void (STDMETHODCALLTYPE * Finish_OnViewChange) (AsyncIAdviseSink * This);

        void (STDMETHODCALLTYPE * Begin_OnRename) (AsyncIAdviseSink * This, IMoniker * pmk);

        void (STDMETHODCALLTYPE * Finish_OnRename) (AsyncIAdviseSink * This);

        void (STDMETHODCALLTYPE * Begin_OnSave) (AsyncIAdviseSink * This);

        void (STDMETHODCALLTYPE * Finish_OnSave) (AsyncIAdviseSink * This);

        void (STDMETHODCALLTYPE * Begin_OnClose) (AsyncIAdviseSink * This);

        void (STDMETHODCALLTYPE * Finish_OnClose) (AsyncIAdviseSink * This);

     END_INTERFACE} AsyncIAdviseSinkVtbl;

    interface AsyncIAdviseSink {
        CONST_VTBL struct AsyncIAdviseSinkVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define AsyncIAdviseSink_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define AsyncIAdviseSink_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define AsyncIAdviseSink_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define AsyncIAdviseSink_Begin_OnDataChange(This,pFormatetc,pStgmed)  \
    (This)->lpVtbl -> Begin_OnDataChange(This,pFormatetc,pStgmed)

#define AsyncIAdviseSink_Finish_OnDataChange(This)  \
    (This)->lpVtbl -> Finish_OnDataChange(This)

#define AsyncIAdviseSink_Begin_OnViewChange(This,dwAspect,lindex)  \
    (This)->lpVtbl -> Begin_OnViewChange(This,dwAspect,lindex)

#define AsyncIAdviseSink_Finish_OnViewChange(This)  \
    (This)->lpVtbl -> Finish_OnViewChange(This)

#define AsyncIAdviseSink_Begin_OnRename(This,pmk)  \
    (This)->lpVtbl -> Begin_OnRename(This,pmk)

#define AsyncIAdviseSink_Finish_OnRename(This)  \
    (This)->lpVtbl -> Finish_OnRename(This)

#define AsyncIAdviseSink_Begin_OnSave(This)  \
    (This)->lpVtbl -> Begin_OnSave(This)

#define AsyncIAdviseSink_Finish_OnSave(This)  \
    (This)->lpVtbl -> Finish_OnSave(This)

#define AsyncIAdviseSink_Begin_OnClose(This)  \
    (This)->lpVtbl -> Begin_OnClose(This)

#define AsyncIAdviseSink_Finish_OnClose(This)  \
    (This)->lpVtbl -> Finish_OnClose(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_RemoteOnDataChange_Proxy(AsyncIAdviseSink * This, FORMATETC * pFormatetc, ASYNC_STGMEDIUM * pStgmed);

    void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnDataChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_RemoteOnDataChange_Proxy(AsyncIAdviseSink * This);

    void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnDataChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_RemoteOnViewChange_Proxy(AsyncIAdviseSink * This, DWORD dwAspect, LONG lindex);

    void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnViewChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_RemoteOnViewChange_Proxy(AsyncIAdviseSink * This);

    void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnViewChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_RemoteOnRename_Proxy(AsyncIAdviseSink * This, IMoniker * pmk);

    void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnRename_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_RemoteOnRename_Proxy(AsyncIAdviseSink * This);

    void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnRename_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_RemoteOnSave_Proxy(AsyncIAdviseSink * This);

    void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnSave_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_RemoteOnSave_Proxy(AsyncIAdviseSink * This);

    void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnSave_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_RemoteOnClose_Proxy(AsyncIAdviseSink * This);

    void __RPC_STUB AsyncIAdviseSink_Begin_RemoteOnClose_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_RemoteOnClose_Proxy(AsyncIAdviseSink * This);

    void __RPC_STUB AsyncIAdviseSink_Finish_RemoteOnClose_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __AsyncIAdviseSink_INTERFACE_DEFINED__ */

#ifndef __IAdviseSink2_INTERFACE_DEFINED__
#define __IAdviseSink2_INTERFACE_DEFINED__

    typedef IAdviseSink2 *LPADVISESINK2;

    EXTERN_C const IID IID_IAdviseSink2;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000125-0000-0000-C000-000000000046")
     IAdviseSink2:public IAdviseSink {
      public:
        virtual void STDMETHODCALLTYPE OnLinkSrcChange(IMoniker * pmk) = 0;

    };

#else

    typedef struct IAdviseSink2Vtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IAdviseSink2 * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IAdviseSink2 * This);

         ULONG(STDMETHODCALLTYPE * Release) (IAdviseSink2 * This);

        void (STDMETHODCALLTYPE * OnDataChange) (IAdviseSink2 * This, FORMATETC * pFormatetc, STGMEDIUM * pStgmed);

        void (STDMETHODCALLTYPE * OnViewChange) (IAdviseSink2 * This, DWORD dwAspect, LONG lindex);

        void (STDMETHODCALLTYPE * OnRename) (IAdviseSink2 * This, IMoniker * pmk);

        void (STDMETHODCALLTYPE * OnSave) (IAdviseSink2 * This);

        void (STDMETHODCALLTYPE * OnClose) (IAdviseSink2 * This);

        void (STDMETHODCALLTYPE * OnLinkSrcChange) (IAdviseSink2 * This, IMoniker * pmk);

     END_INTERFACE} IAdviseSink2Vtbl;

    interface IAdviseSink2 {
        CONST_VTBL struct IAdviseSink2Vtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IAdviseSink2_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAdviseSink2_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IAdviseSink2_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IAdviseSink2_OnDataChange(This,pFormatetc,pStgmed)  \
    (This)->lpVtbl -> OnDataChange(This,pFormatetc,pStgmed)

#define IAdviseSink2_OnViewChange(This,dwAspect,lindex) \
    (This)->lpVtbl -> OnViewChange(This,dwAspect,lindex)

#define IAdviseSink2_OnRename(This,pmk) \
    (This)->lpVtbl -> OnRename(This,pmk)

#define IAdviseSink2_OnSave(This)  \
    (This)->lpVtbl -> OnSave(This)

#define IAdviseSink2_OnClose(This)  \
    (This)->lpVtbl -> OnClose(This)

#define IAdviseSink2_OnLinkSrcChange(This,pmk)  \
    (This)->lpVtbl -> OnLinkSrcChange(This,pmk)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IAdviseSink2_RemoteOnLinkSrcChange_Proxy(IAdviseSink2 * This, IMoniker * pmk);

    void __RPC_STUB IAdviseSink2_RemoteOnLinkSrcChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IAdviseSink2_INTERFACE_DEFINED__ */

#ifndef __AsyncIAdviseSink2_INTERFACE_DEFINED__
#define __AsyncIAdviseSink2_INTERFACE_DEFINED__

    EXTERN_C const IID IID_AsyncIAdviseSink2;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000151-0000-0000-C000-000000000046")
     AsyncIAdviseSink2:public AsyncIAdviseSink {
      public:
        virtual void STDMETHODCALLTYPE Begin_OnLinkSrcChange(IMoniker * pmk) = 0;

        virtual void STDMETHODCALLTYPE Finish_OnLinkSrcChange(void) = 0;

    };

#else

    typedef struct AsyncIAdviseSink2Vtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (AsyncIAdviseSink2 * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (AsyncIAdviseSink2 * This);

         ULONG(STDMETHODCALLTYPE * Release) (AsyncIAdviseSink2 * This);

        void (STDMETHODCALLTYPE * Begin_OnDataChange) (AsyncIAdviseSink2 * This, FORMATETC * pFormatetc, STGMEDIUM * pStgmed);

        void (STDMETHODCALLTYPE * Finish_OnDataChange) (AsyncIAdviseSink2 * This);

        void (STDMETHODCALLTYPE * Begin_OnViewChange) (AsyncIAdviseSink2 * This, DWORD dwAspect, LONG lindex);

        void (STDMETHODCALLTYPE * Finish_OnViewChange) (AsyncIAdviseSink2 * This);

        void (STDMETHODCALLTYPE * Begin_OnRename) (AsyncIAdviseSink2 * This, IMoniker * pmk);

        void (STDMETHODCALLTYPE * Finish_OnRename) (AsyncIAdviseSink2 * This);

        void (STDMETHODCALLTYPE * Begin_OnSave) (AsyncIAdviseSink2 * This);

        void (STDMETHODCALLTYPE * Finish_OnSave) (AsyncIAdviseSink2 * This);

        void (STDMETHODCALLTYPE * Begin_OnClose) (AsyncIAdviseSink2 * This);

        void (STDMETHODCALLTYPE * Finish_OnClose) (AsyncIAdviseSink2 * This);

        void (STDMETHODCALLTYPE * Begin_OnLinkSrcChange) (AsyncIAdviseSink2 * This, IMoniker * pmk);

        void (STDMETHODCALLTYPE * Finish_OnLinkSrcChange) (AsyncIAdviseSink2 * This);

     END_INTERFACE} AsyncIAdviseSink2Vtbl;

    interface AsyncIAdviseSink2 {
        CONST_VTBL struct AsyncIAdviseSink2Vtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define AsyncIAdviseSink2_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define AsyncIAdviseSink2_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define AsyncIAdviseSink2_Release(This) \
    (This)->lpVtbl -> Release(This)

#define AsyncIAdviseSink2_Begin_OnDataChange(This,pFormatetc,pStgmed)  \
    (This)->lpVtbl -> Begin_OnDataChange(This,pFormatetc,pStgmed)

#define AsyncIAdviseSink2_Finish_OnDataChange(This)  \
    (This)->lpVtbl -> Finish_OnDataChange(This)

#define AsyncIAdviseSink2_Begin_OnViewChange(This,dwAspect,lindex)  \
    (This)->lpVtbl -> Begin_OnViewChange(This,dwAspect,lindex)

#define AsyncIAdviseSink2_Finish_OnViewChange(This)  \
    (This)->lpVtbl -> Finish_OnViewChange(This)

#define AsyncIAdviseSink2_Begin_OnRename(This,pmk)  \
    (This)->lpVtbl -> Begin_OnRename(This,pmk)

#define AsyncIAdviseSink2_Finish_OnRename(This) \
    (This)->lpVtbl -> Finish_OnRename(This)

#define AsyncIAdviseSink2_Begin_OnSave(This)  \
    (This)->lpVtbl -> Begin_OnSave(This)

#define AsyncIAdviseSink2_Finish_OnSave(This)  \
    (This)->lpVtbl -> Finish_OnSave(This)

#define AsyncIAdviseSink2_Begin_OnClose(This)  \
    (This)->lpVtbl -> Begin_OnClose(This)

#define AsyncIAdviseSink2_Finish_OnClose(This)  \
    (This)->lpVtbl -> Finish_OnClose(This)

#define AsyncIAdviseSink2_Begin_OnLinkSrcChange(This,pmk)  \
    (This)->lpVtbl -> Begin_OnLinkSrcChange(This,pmk)

#define AsyncIAdviseSink2_Finish_OnLinkSrcChange(This)  \
    (This)->lpVtbl -> Finish_OnLinkSrcChange(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink2_Begin_RemoteOnLinkSrcChange_Proxy(AsyncIAdviseSink2 * This, IMoniker * pmk);

    void __RPC_STUB AsyncIAdviseSink2_Begin_RemoteOnLinkSrcChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink2_Finish_RemoteOnLinkSrcChange_Proxy(AsyncIAdviseSink2 * This);

    void __RPC_STUB AsyncIAdviseSink2_Finish_RemoteOnLinkSrcChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __AsyncIAdviseSink2_INTERFACE_DEFINED__ */

#ifndef __IDataObject_INTERFACE_DEFINED__
#define __IDataObject_INTERFACE_DEFINED__

    typedef IDataObject *LPDATAOBJECT;

    typedef
        enum tagDATADIR { DATADIR_GET = 1,
        DATADIR_SET = 2
    } DATADIR;

    EXTERN_C const IID IID_IDataObject;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000010e-0000-0000-C000-000000000046")
     IDataObject:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetData(FORMATETC * pformatetcIn, STGMEDIUM * pmedium) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetDataHere(FORMATETC * pformatetc, STGMEDIUM * pmedium) = 0;

        virtual HRESULT STDMETHODCALLTYPE QueryGetData(FORMATETC * pformatetc) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetCanonicalFormatEtc(FORMATETC * pformatectIn, FORMATETC * pformatetcOut) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetData(FORMATETC * pformatetc, STGMEDIUM * pmedium, BOOL fRelease) = 0;

        virtual HRESULT STDMETHODCALLTYPE EnumFormatEtc(DWORD dwDirection, IEnumFORMATETC * *ppenumFormatEtc) = 0;

        virtual HRESULT STDMETHODCALLTYPE DAdvise(FORMATETC * pformatetc, DWORD advf, IAdviseSink * pAdvSink, DWORD * pdwConnection) = 0;

        virtual HRESULT STDMETHODCALLTYPE DUnadvise(DWORD dwConnection) = 0;

        virtual HRESULT STDMETHODCALLTYPE EnumDAdvise(IEnumSTATDATA * *ppenumAdvise) = 0;

    };

#else

    typedef struct IDataObjectVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IDataObject * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IDataObject * This);

         ULONG(STDMETHODCALLTYPE * Release) (IDataObject * This);

         HRESULT(STDMETHODCALLTYPE * GetData) (IDataObject * This, FORMATETC * pformatetcIn, STGMEDIUM * pmedium);

         HRESULT(STDMETHODCALLTYPE * GetDataHere) (IDataObject * This, FORMATETC * pformatetc, STGMEDIUM * pmedium);

         HRESULT(STDMETHODCALLTYPE * QueryGetData) (IDataObject * This, FORMATETC * pformatetc);

         HRESULT(STDMETHODCALLTYPE * GetCanonicalFormatEtc) (IDataObject * This, FORMATETC * pformatectIn, FORMATETC * pformatetcOut);

         HRESULT(STDMETHODCALLTYPE * SetData) (IDataObject * This, FORMATETC * pformatetc, STGMEDIUM * pmedium, BOOL fRelease);

         HRESULT(STDMETHODCALLTYPE * EnumFormatEtc) (IDataObject * This, DWORD dwDirection, IEnumFORMATETC * *ppenumFormatEtc);

         HRESULT(STDMETHODCALLTYPE * DAdvise) (IDataObject * This, FORMATETC * pformatetc, DWORD advf, IAdviseSink * pAdvSink, DWORD * pdwConnection);

         HRESULT(STDMETHODCALLTYPE * DUnadvise) (IDataObject * This, DWORD dwConnection);

         HRESULT(STDMETHODCALLTYPE * EnumDAdvise) (IDataObject * This, IEnumSTATDATA * *ppenumAdvise);

     END_INTERFACE} IDataObjectVtbl;

    interface IDataObject {
        CONST_VTBL struct IDataObjectVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IDataObject_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IDataObject_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IDataObject_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IDataObject_GetData(This,pformatetcIn,pmedium)  \
    (This)->lpVtbl -> GetData(This,pformatetcIn,pmedium)

#define IDataObject_GetDataHere(This,pformatetc,pmedium)  \
    (This)->lpVtbl -> GetDataHere(This,pformatetc,pmedium)

#define IDataObject_QueryGetData(This,pformatetc)  \
    (This)->lpVtbl -> QueryGetData(This,pformatetc)

#define IDataObject_GetCanonicalFormatEtc(This,pformatectIn,pformatetcOut)  \
    (This)->lpVtbl -> GetCanonicalFormatEtc(This,pformatectIn,pformatetcOut)

#define IDataObject_SetData(This,pformatetc,pmedium,fRelease)  \
    (This)->lpVtbl -> SetData(This,pformatetc,pmedium,fRelease)

#define IDataObject_EnumFormatEtc(This,dwDirection,ppenumFormatEtc)  \
    (This)->lpVtbl -> EnumFormatEtc(This,dwDirection,ppenumFormatEtc)

#define IDataObject_DAdvise(This,pformatetc,advf,pAdvSink,pdwConnection)  \
    (This)->lpVtbl -> DAdvise(This,pformatetc,advf,pAdvSink,pdwConnection)

#define IDataObject_DUnadvise(This,dwConnection)  \
    (This)->lpVtbl -> DUnadvise(This,dwConnection)

#define IDataObject_EnumDAdvise(This,ppenumAdvise)  \
    (This)->lpVtbl -> EnumDAdvise(This,ppenumAdvise)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IDataObject_RemoteGetData_Proxy(IDataObject * This, FORMATETC * pformatetcIn, STGMEDIUM * pRemoteMedium);

    void __RPC_STUB IDataObject_RemoteGetData_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataObject_RemoteGetDataHere_Proxy(IDataObject * This, FORMATETC * pformatetc, STGMEDIUM * pRemoteMedium);

    void __RPC_STUB IDataObject_RemoteGetDataHere_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataObject_QueryGetData_Proxy(IDataObject * This, FORMATETC * pformatetc);

    void __RPC_STUB IDataObject_QueryGetData_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataObject_GetCanonicalFormatEtc_Proxy(IDataObject * This, FORMATETC * pformatectIn, FORMATETC * pformatetcOut);

    void __RPC_STUB IDataObject_GetCanonicalFormatEtc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataObject_RemoteSetData_Proxy(IDataObject * This, FORMATETC * pformatetc, FLAG_STGMEDIUM * pmedium, BOOL fRelease);

    void __RPC_STUB IDataObject_RemoteSetData_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataObject_EnumFormatEtc_Proxy(IDataObject * This, DWORD dwDirection, IEnumFORMATETC * *ppenumFormatEtc);

    void __RPC_STUB IDataObject_EnumFormatEtc_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataObject_DAdvise_Proxy(IDataObject * This, FORMATETC * pformatetc, DWORD advf, IAdviseSink * pAdvSink, DWORD * pdwConnection);

    void __RPC_STUB IDataObject_DAdvise_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataObject_DUnadvise_Proxy(IDataObject * This, DWORD dwConnection);

    void __RPC_STUB IDataObject_DUnadvise_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataObject_EnumDAdvise_Proxy(IDataObject * This, IEnumSTATDATA * *ppenumAdvise);

    void __RPC_STUB IDataObject_EnumDAdvise_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IDataObject_INTERFACE_DEFINED__ */

#ifndef __IDataAdviseHolder_INTERFACE_DEFINED__
#define __IDataAdviseHolder_INTERFACE_DEFINED__

    typedef IDataAdviseHolder *LPDATAADVISEHOLDER;

    EXTERN_C const IID IID_IDataAdviseHolder;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000110-0000-0000-C000-000000000046")
     IDataAdviseHolder:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Advise(IDataObject * pDataObject, FORMATETC * pFetc, DWORD advf, IAdviseSink * pAdvise, DWORD * pdwConnection) = 0;

        virtual HRESULT STDMETHODCALLTYPE Unadvise(DWORD dwConnection) = 0;

        virtual HRESULT STDMETHODCALLTYPE EnumAdvise(IEnumSTATDATA * *ppenumAdvise) = 0;

        virtual HRESULT STDMETHODCALLTYPE SendOnDataChange(IDataObject * pDataObject, DWORD dwReserved, DWORD advf) = 0;

    };

#else

    typedef struct IDataAdviseHolderVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IDataAdviseHolder * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IDataAdviseHolder * This);

         ULONG(STDMETHODCALLTYPE * Release) (IDataAdviseHolder * This);

         HRESULT(STDMETHODCALLTYPE * Advise) (IDataAdviseHolder * This, IDataObject * pDataObject, FORMATETC * pFetc, DWORD advf, IAdviseSink * pAdvise, DWORD * pdwConnection);

         HRESULT(STDMETHODCALLTYPE * Unadvise) (IDataAdviseHolder * This, DWORD dwConnection);

         HRESULT(STDMETHODCALLTYPE * EnumAdvise) (IDataAdviseHolder * This, IEnumSTATDATA * *ppenumAdvise);

         HRESULT(STDMETHODCALLTYPE * SendOnDataChange) (IDataAdviseHolder * This, IDataObject * pDataObject, DWORD dwReserved, DWORD advf);

     END_INTERFACE} IDataAdviseHolderVtbl;

    interface IDataAdviseHolder {
        CONST_VTBL struct IDataAdviseHolderVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IDataAdviseHolder_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IDataAdviseHolder_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IDataAdviseHolder_Release(This) \
    (This)->lpVtbl -> Release(This)

#define IDataAdviseHolder_Advise(This,pDataObject,pFetc,advf,pAdvise,pdwConnection)  \
    (This)->lpVtbl -> Advise(This,pDataObject,pFetc,advf,pAdvise,pdwConnection)

#define IDataAdviseHolder_Unadvise(This,dwConnection)  \
    (This)->lpVtbl -> Unadvise(This,dwConnection)

#define IDataAdviseHolder_EnumAdvise(This,ppenumAdvise) \
    (This)->lpVtbl -> EnumAdvise(This,ppenumAdvise)

#define IDataAdviseHolder_SendOnDataChange(This,pDataObject,dwReserved,advf)  \
    (This)->lpVtbl -> SendOnDataChange(This,pDataObject,dwReserved,advf)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IDataAdviseHolder_Advise_Proxy(IDataAdviseHolder * This, IDataObject * pDataObject, FORMATETC * pFetc, DWORD advf, IAdviseSink * pAdvise, DWORD * pdwConnection);

    void __RPC_STUB IDataAdviseHolder_Advise_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataAdviseHolder_Unadvise_Proxy(IDataAdviseHolder * This, DWORD dwConnection);

    void __RPC_STUB IDataAdviseHolder_Unadvise_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataAdviseHolder_EnumAdvise_Proxy(IDataAdviseHolder * This, IEnumSTATDATA * *ppenumAdvise);

    void __RPC_STUB IDataAdviseHolder_EnumAdvise_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDataAdviseHolder_SendOnDataChange_Proxy(IDataAdviseHolder * This, IDataObject * pDataObject, DWORD dwReserved, DWORD advf);

    void __RPC_STUB IDataAdviseHolder_SendOnDataChange_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IDataAdviseHolder_INTERFACE_DEFINED__ */

#ifndef __IMessageFilter_INTERFACE_DEFINED__
#define __IMessageFilter_INTERFACE_DEFINED__

    typedef IMessageFilter *LPMESSAGEFILTER;

    typedef
        enum tagCALLTYPE { CALLTYPE_TOPLEVEL = 1,
        CALLTYPE_NESTED = 2,
        CALLTYPE_ASYNC = 3,
        CALLTYPE_TOPLEVEL_CALLPENDING = 4,
        CALLTYPE_ASYNC_CALLPENDING = 5
    } CALLTYPE;

    typedef
        enum tagSERVERCALL { SERVERCALL_ISHANDLED = 0,
        SERVERCALL_REJECTED = 1,
        SERVERCALL_RETRYLATER = 2
    } SERVERCALL;

    typedef
        enum tagPENDINGTYPE { PENDINGTYPE_TOPLEVEL = 1,
        PENDINGTYPE_NESTED = 2
    } PENDINGTYPE;

    typedef
        enum tagPENDINGMSG { PENDINGMSG_CANCELCALL = 0,
        PENDINGMSG_WAITNOPROCESS = 1,
        PENDINGMSG_WAITDEFPROCESS = 2
    } PENDINGMSG;

    typedef struct tagINTERFACEINFO {
        IUnknown *pUnk;
        IID iid;
        WORD wMethod;
    } INTERFACEINFO;

    typedef struct tagINTERFACEINFO *LPINTERFACEINFO;

    EXTERN_C const IID IID_IMessageFilter;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000016-0000-0000-C000-000000000046")
     IMessageFilter:public IUnknown {
      public:
        virtual DWORD STDMETHODCALLTYPE HandleInComingCall(DWORD dwCallType, HTASK htaskCaller, DWORD dwTickCount, LPINTERFACEINFO lpInterfaceInfo) = 0;

        virtual DWORD STDMETHODCALLTYPE RetryRejectedCall(HTASK htaskCallee, DWORD dwTickCount, DWORD dwRejectType) = 0;

        virtual DWORD STDMETHODCALLTYPE MessagePending(HTASK htaskCallee, DWORD dwTickCount, DWORD dwPendingType) = 0;

    };

#else

    typedef struct IMessageFilterVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IMessageFilter * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IMessageFilter * This);

         ULONG(STDMETHODCALLTYPE * Release) (IMessageFilter * This);

         DWORD(STDMETHODCALLTYPE * HandleInComingCall) (IMessageFilter * This, DWORD dwCallType, HTASK htaskCaller, DWORD dwTickCount, LPINTERFACEINFO lpInterfaceInfo);

         DWORD(STDMETHODCALLTYPE * RetryRejectedCall) (IMessageFilter * This, HTASK htaskCallee, DWORD dwTickCount, DWORD dwRejectType);

         DWORD(STDMETHODCALLTYPE * MessagePending) (IMessageFilter * This, HTASK htaskCallee, DWORD dwTickCount, DWORD dwPendingType);

     END_INTERFACE} IMessageFilterVtbl;

    interface IMessageFilter {
        CONST_VTBL struct IMessageFilterVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IMessageFilter_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IMessageFilter_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IMessageFilter_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IMessageFilter_HandleInComingCall(This,dwCallType,htaskCaller,dwTickCount,lpInterfaceInfo)  \
    (This)->lpVtbl -> HandleInComingCall(This,dwCallType,htaskCaller,dwTickCount,lpInterfaceInfo)

#define IMessageFilter_RetryRejectedCall(This,htaskCallee,dwTickCount,dwRejectType)  \
    (This)->lpVtbl -> RetryRejectedCall(This,htaskCallee,dwTickCount,dwRejectType)

#define IMessageFilter_MessagePending(This,htaskCallee,dwTickCount,dwPendingType)  \
    (This)->lpVtbl -> MessagePending(This,htaskCallee,dwTickCount,dwPendingType)

#endif /* COBJMACROS */

#endif

    DWORD STDMETHODCALLTYPE IMessageFilter_HandleInComingCall_Proxy(IMessageFilter * This, DWORD dwCallType, HTASK htaskCaller, DWORD dwTickCount, LPINTERFACEINFO lpInterfaceInfo);

    void __RPC_STUB IMessageFilter_HandleInComingCall_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    DWORD STDMETHODCALLTYPE IMessageFilter_RetryRejectedCall_Proxy(IMessageFilter * This, HTASK htaskCallee, DWORD dwTickCount, DWORD dwRejectType);

    void __RPC_STUB IMessageFilter_RetryRejectedCall_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    DWORD STDMETHODCALLTYPE IMessageFilter_MessagePending_Proxy(IMessageFilter * This, HTASK htaskCallee, DWORD dwTickCount, DWORD dwPendingType);

    void __RPC_STUB IMessageFilter_MessagePending_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IMessageFilter_INTERFACE_DEFINED__ */

#ifndef __IRpcChannelBuffer_INTERFACE_DEFINED__
#define __IRpcChannelBuffer_INTERFACE_DEFINED__

    typedef unsigned long RPCOLEDATAREP;

    typedef struct tagRPCOLEMESSAGE {
        void *reserved1;
        RPCOLEDATAREP dataRepresentation;
        void *Buffer;
        ULONG cbBuffer;
        ULONG iMethod;
        void *reserved2[5];
        ULONG rpcFlags;
    } RPCOLEMESSAGE;

    typedef RPCOLEMESSAGE *PRPCOLEMESSAGE;

    EXTERN_C const IID IID_IRpcChannelBuffer;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("D5F56B60-593B-101A-B569-08002B2DBF7A")
     IRpcChannelBuffer:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetBuffer(RPCOLEMESSAGE * pMessage, REFIID riid) = 0;

        virtual HRESULT STDMETHODCALLTYPE SendReceive(RPCOLEMESSAGE * pMessage, ULONG * pStatus) = 0;

        virtual HRESULT STDMETHODCALLTYPE FreeBuffer(RPCOLEMESSAGE * pMessage) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetDestCtx(DWORD * pdwDestContext, void **ppvDestContext) = 0;

        virtual HRESULT STDMETHODCALLTYPE IsConnected(void) = 0;

    };

#else

    typedef struct IRpcChannelBufferVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRpcChannelBuffer * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRpcChannelBuffer * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRpcChannelBuffer * This);

         HRESULT(STDMETHODCALLTYPE * GetBuffer) (IRpcChannelBuffer * This, RPCOLEMESSAGE * pMessage, REFIID riid);

         HRESULT(STDMETHODCALLTYPE * SendReceive) (IRpcChannelBuffer * This, RPCOLEMESSAGE * pMessage, ULONG * pStatus);

         HRESULT(STDMETHODCALLTYPE * FreeBuffer) (IRpcChannelBuffer * This, RPCOLEMESSAGE * pMessage);

         HRESULT(STDMETHODCALLTYPE * GetDestCtx) (IRpcChannelBuffer * This, DWORD * pdwDestContext, void **ppvDestContext);

         HRESULT(STDMETHODCALLTYPE * IsConnected) (IRpcChannelBuffer * This);

     END_INTERFACE} IRpcChannelBufferVtbl;

    interface IRpcChannelBuffer {
        CONST_VTBL struct IRpcChannelBufferVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRpcChannelBuffer_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRpcChannelBuffer_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IRpcChannelBuffer_Release(This) \
    (This)->lpVtbl -> Release(This)

#define IRpcChannelBuffer_GetBuffer(This,pMessage,riid) \
    (This)->lpVtbl -> GetBuffer(This,pMessage,riid)

#define IRpcChannelBuffer_SendReceive(This,pMessage,pStatus)  \
    (This)->lpVtbl -> SendReceive(This,pMessage,pStatus)

#define IRpcChannelBuffer_FreeBuffer(This,pMessage)  \
    (This)->lpVtbl -> FreeBuffer(This,pMessage)

#define IRpcChannelBuffer_GetDestCtx(This,pdwDestContext,ppvDestContext)  \
    (This)->lpVtbl -> GetDestCtx(This,pdwDestContext,ppvDestContext)

#define IRpcChannelBuffer_IsConnected(This)  \
    (This)->lpVtbl -> IsConnected(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer_GetBuffer_Proxy(IRpcChannelBuffer * This, RPCOLEMESSAGE * pMessage, REFIID riid);

    void __RPC_STUB IRpcChannelBuffer_GetBuffer_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer_SendReceive_Proxy(IRpcChannelBuffer * This, RPCOLEMESSAGE * pMessage, ULONG * pStatus);

    void __RPC_STUB IRpcChannelBuffer_SendReceive_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer_FreeBuffer_Proxy(IRpcChannelBuffer * This, RPCOLEMESSAGE * pMessage);

    void __RPC_STUB IRpcChannelBuffer_FreeBuffer_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer_GetDestCtx_Proxy(IRpcChannelBuffer * This, DWORD * pdwDestContext, void **ppvDestContext);

    void __RPC_STUB IRpcChannelBuffer_GetDestCtx_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer_IsConnected_Proxy(IRpcChannelBuffer * This);

    void __RPC_STUB IRpcChannelBuffer_IsConnected_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRpcChannelBuffer_INTERFACE_DEFINED__ */

#ifndef __IRpcChannelBuffer2_INTERFACE_DEFINED__
#define __IRpcChannelBuffer2_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IRpcChannelBuffer2;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("594f31d0-7f19-11d0-b194-00a0c90dc8bf")
     IRpcChannelBuffer2:public IRpcChannelBuffer {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetProtocolVersion(DWORD * pdwVersion) = 0;

    };

#else

    typedef struct IRpcChannelBuffer2Vtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRpcChannelBuffer2 * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRpcChannelBuffer2 * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRpcChannelBuffer2 * This);

         HRESULT(STDMETHODCALLTYPE * GetBuffer) (IRpcChannelBuffer2 * This, RPCOLEMESSAGE * pMessage, REFIID riid);

         HRESULT(STDMETHODCALLTYPE * SendReceive) (IRpcChannelBuffer2 * This, RPCOLEMESSAGE * pMessage, ULONG * pStatus);

         HRESULT(STDMETHODCALLTYPE * FreeBuffer) (IRpcChannelBuffer2 * This, RPCOLEMESSAGE * pMessage);

         HRESULT(STDMETHODCALLTYPE * GetDestCtx) (IRpcChannelBuffer2 * This, DWORD * pdwDestContext, void **ppvDestContext);

         HRESULT(STDMETHODCALLTYPE * IsConnected) (IRpcChannelBuffer2 * This);

         HRESULT(STDMETHODCALLTYPE * GetProtocolVersion) (IRpcChannelBuffer2 * This, DWORD * pdwVersion);

     END_INTERFACE} IRpcChannelBuffer2Vtbl;

    interface IRpcChannelBuffer2 {
        CONST_VTBL struct IRpcChannelBuffer2Vtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRpcChannelBuffer2_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRpcChannelBuffer2_AddRef(This) \
    (This)->lpVtbl -> AddRef(This)

#define IRpcChannelBuffer2_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IRpcChannelBuffer2_GetBuffer(This,pMessage,riid)  \
    (This)->lpVtbl -> GetBuffer(This,pMessage,riid)

#define IRpcChannelBuffer2_SendReceive(This,pMessage,pStatus)  \
    (This)->lpVtbl -> SendReceive(This,pMessage,pStatus)

#define IRpcChannelBuffer2_FreeBuffer(This,pMessage)  \
    (This)->lpVtbl -> FreeBuffer(This,pMessage)

#define IRpcChannelBuffer2_GetDestCtx(This,pdwDestContext,ppvDestContext)  \
    (This)->lpVtbl -> GetDestCtx(This,pdwDestContext,ppvDestContext)

#define IRpcChannelBuffer2_IsConnected(This)  \
    (This)->lpVtbl -> IsConnected(This)

#define IRpcChannelBuffer2_GetProtocolVersion(This,pdwVersion)  \
    (This)->lpVtbl -> GetProtocolVersion(This,pdwVersion)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer2_GetProtocolVersion_Proxy(IRpcChannelBuffer2 * This, DWORD * pdwVersion);

    void __RPC_STUB IRpcChannelBuffer2_GetProtocolVersion_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRpcChannelBuffer2_INTERFACE_DEFINED__ */

#ifndef __IAsyncRpcChannelBuffer_INTERFACE_DEFINED__
#define __IAsyncRpcChannelBuffer_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IAsyncRpcChannelBuffer;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("a5029fb6-3c34-11d1-9c99-00c04fb998aa")
     IAsyncRpcChannelBuffer:public IRpcChannelBuffer2 {
      public:
        virtual HRESULT STDMETHODCALLTYPE Send(RPCOLEMESSAGE * pMsg, ISynchronize * pSync, ULONG * pulStatus) = 0;

        virtual HRESULT STDMETHODCALLTYPE Receive(RPCOLEMESSAGE * pMsg, ULONG * pulStatus) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetDestCtxEx(RPCOLEMESSAGE * pMsg, DWORD * pdwDestContext, void **ppvDestContext) = 0;

    };

#else

    typedef struct IAsyncRpcChannelBufferVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IAsyncRpcChannelBuffer * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IAsyncRpcChannelBuffer * This);

         ULONG(STDMETHODCALLTYPE * Release) (IAsyncRpcChannelBuffer * This);

         HRESULT(STDMETHODCALLTYPE * GetBuffer) (IAsyncRpcChannelBuffer * This, RPCOLEMESSAGE * pMessage, REFIID riid);

         HRESULT(STDMETHODCALLTYPE * SendReceive) (IAsyncRpcChannelBuffer * This, RPCOLEMESSAGE * pMessage, ULONG * pStatus);

         HRESULT(STDMETHODCALLTYPE * FreeBuffer) (IAsyncRpcChannelBuffer * This, RPCOLEMESSAGE * pMessage);

         HRESULT(STDMETHODCALLTYPE * GetDestCtx) (IAsyncRpcChannelBuffer * This, DWORD * pdwDestContext, void **ppvDestContext);

         HRESULT(STDMETHODCALLTYPE * IsConnected) (IAsyncRpcChannelBuffer * This);

         HRESULT(STDMETHODCALLTYPE * GetProtocolVersion) (IAsyncRpcChannelBuffer * This, DWORD * pdwVersion);

         HRESULT(STDMETHODCALLTYPE * Send) (IAsyncRpcChannelBuffer * This, RPCOLEMESSAGE * pMsg, ISynchronize * pSync, ULONG * pulStatus);

         HRESULT(STDMETHODCALLTYPE * Receive) (IAsyncRpcChannelBuffer * This, RPCOLEMESSAGE * pMsg, ULONG * pulStatus);

         HRESULT(STDMETHODCALLTYPE * GetDestCtxEx) (IAsyncRpcChannelBuffer * This, RPCOLEMESSAGE * pMsg, DWORD * pdwDestContext, void **ppvDestContext);

     END_INTERFACE} IAsyncRpcChannelBufferVtbl;

    interface IAsyncRpcChannelBuffer {
        CONST_VTBL struct IAsyncRpcChannelBufferVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IAsyncRpcChannelBuffer_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAsyncRpcChannelBuffer_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IAsyncRpcChannelBuffer_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IAsyncRpcChannelBuffer_GetBuffer(This,pMessage,riid)  \
    (This)->lpVtbl -> GetBuffer(This,pMessage,riid)

#define IAsyncRpcChannelBuffer_SendReceive(This,pMessage,pStatus)  \
    (This)->lpVtbl -> SendReceive(This,pMessage,pStatus)

#define IAsyncRpcChannelBuffer_FreeBuffer(This,pMessage)  \
    (This)->lpVtbl -> FreeBuffer(This,pMessage)

#define IAsyncRpcChannelBuffer_GetDestCtx(This,pdwDestContext,ppvDestContext)  \
    (This)->lpVtbl -> GetDestCtx(This,pdwDestContext,ppvDestContext)

#define IAsyncRpcChannelBuffer_IsConnected(This)  \
    (This)->lpVtbl -> IsConnected(This)

#define IAsyncRpcChannelBuffer_GetProtocolVersion(This,pdwVersion)  \
    (This)->lpVtbl -> GetProtocolVersion(This,pdwVersion)

#define IAsyncRpcChannelBuffer_Send(This,pMsg,pSync,pulStatus)  \
    (This)->lpVtbl -> Send(This,pMsg,pSync,pulStatus)

#define IAsyncRpcChannelBuffer_Receive(This,pMsg,pulStatus)  \
    (This)->lpVtbl -> Receive(This,pMsg,pulStatus)

#define IAsyncRpcChannelBuffer_GetDestCtxEx(This,pMsg,pdwDestContext,ppvDestContext)  \
    (This)->lpVtbl -> GetDestCtxEx(This,pMsg,pdwDestContext,ppvDestContext)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IAsyncRpcChannelBuffer_Send_Proxy(IAsyncRpcChannelBuffer * This, RPCOLEMESSAGE * pMsg, ISynchronize * pSync, ULONG * pulStatus);

    void __RPC_STUB IAsyncRpcChannelBuffer_Send_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IAsyncRpcChannelBuffer_Receive_Proxy(IAsyncRpcChannelBuffer * This, RPCOLEMESSAGE * pMsg, ULONG * pulStatus);

    void __RPC_STUB IAsyncRpcChannelBuffer_Receive_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IAsyncRpcChannelBuffer_GetDestCtxEx_Proxy(IAsyncRpcChannelBuffer * This, RPCOLEMESSAGE * pMsg, DWORD * pdwDestContext, void **ppvDestContext);

    void __RPC_STUB IAsyncRpcChannelBuffer_GetDestCtxEx_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IAsyncRpcChannelBuffer_INTERFACE_DEFINED__ */

#ifndef __IRpcChannelBuffer3_INTERFACE_DEFINED__
#define __IRpcChannelBuffer3_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IRpcChannelBuffer3;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("25B15600-0115-11d0-BF0D-00AA00B8DFD2")
     IRpcChannelBuffer3:public IRpcChannelBuffer2 {
      public:
        virtual HRESULT STDMETHODCALLTYPE Send(RPCOLEMESSAGE * pMsg, ULONG * pulStatus) = 0;

        virtual HRESULT STDMETHODCALLTYPE Receive(RPCOLEMESSAGE * pMsg, ULONG ulSize, ULONG * pulStatus) = 0;

        virtual HRESULT STDMETHODCALLTYPE Cancel(RPCOLEMESSAGE * pMsg) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetCallContext(RPCOLEMESSAGE * pMsg, REFIID riid, void **pInterface) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetDestCtxEx(RPCOLEMESSAGE * pMsg, DWORD * pdwDestContext, void **ppvDestContext) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetState(RPCOLEMESSAGE * pMsg, DWORD * pState) = 0;

        virtual HRESULT STDMETHODCALLTYPE RegisterAsync(RPCOLEMESSAGE * pMsg, IAsyncManager * pAsyncMgr) = 0;

    };

#else

    typedef struct IRpcChannelBuffer3Vtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRpcChannelBuffer3 * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRpcChannelBuffer3 * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRpcChannelBuffer3 * This);

         HRESULT(STDMETHODCALLTYPE * GetBuffer) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMessage, REFIID riid);

         HRESULT(STDMETHODCALLTYPE * SendReceive) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMessage, ULONG * pStatus);

         HRESULT(STDMETHODCALLTYPE * FreeBuffer) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMessage);

         HRESULT(STDMETHODCALLTYPE * GetDestCtx) (IRpcChannelBuffer3 * This, DWORD * pdwDestContext, void **ppvDestContext);

         HRESULT(STDMETHODCALLTYPE * IsConnected) (IRpcChannelBuffer3 * This);

         HRESULT(STDMETHODCALLTYPE * GetProtocolVersion) (IRpcChannelBuffer3 * This, DWORD * pdwVersion);

         HRESULT(STDMETHODCALLTYPE * Send) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, ULONG * pulStatus);

         HRESULT(STDMETHODCALLTYPE * Receive) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, ULONG ulSize, ULONG * pulStatus);

         HRESULT(STDMETHODCALLTYPE * Cancel) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg);

         HRESULT(STDMETHODCALLTYPE * GetCallContext) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, REFIID riid, void **pInterface);

         HRESULT(STDMETHODCALLTYPE * GetDestCtxEx) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, DWORD * pdwDestContext, void **ppvDestContext);

         HRESULT(STDMETHODCALLTYPE * GetState) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, DWORD * pState);

         HRESULT(STDMETHODCALLTYPE * RegisterAsync) (IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, IAsyncManager * pAsyncMgr);

     END_INTERFACE} IRpcChannelBuffer3Vtbl;

    interface IRpcChannelBuffer3 {
        CONST_VTBL struct IRpcChannelBuffer3Vtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRpcChannelBuffer3_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRpcChannelBuffer3_AddRef(This) \
    (This)->lpVtbl -> AddRef(This)

#define IRpcChannelBuffer3_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IRpcChannelBuffer3_GetBuffer(This,pMessage,riid)  \
    (This)->lpVtbl -> GetBuffer(This,pMessage,riid)

#define IRpcChannelBuffer3_SendReceive(This,pMessage,pStatus)  \
    (This)->lpVtbl -> SendReceive(This,pMessage,pStatus)

#define IRpcChannelBuffer3_FreeBuffer(This,pMessage)  \
    (This)->lpVtbl -> FreeBuffer(This,pMessage)

#define IRpcChannelBuffer3_GetDestCtx(This,pdwDestContext,ppvDestContext)  \
    (This)->lpVtbl -> GetDestCtx(This,pdwDestContext,ppvDestContext)

#define IRpcChannelBuffer3_IsConnected(This)  \
    (This)->lpVtbl -> IsConnected(This)

#define IRpcChannelBuffer3_GetProtocolVersion(This,pdwVersion)  \
    (This)->lpVtbl -> GetProtocolVersion(This,pdwVersion)

#define IRpcChannelBuffer3_Send(This,pMsg,pulStatus)  \
    (This)->lpVtbl -> Send(This,pMsg,pulStatus)

#define IRpcChannelBuffer3_Receive(This,pMsg,ulSize,pulStatus)  \
    (This)->lpVtbl -> Receive(This,pMsg,ulSize,pulStatus)

#define IRpcChannelBuffer3_Cancel(This,pMsg)  \
    (This)->lpVtbl -> Cancel(This,pMsg)

#define IRpcChannelBuffer3_GetCallContext(This,pMsg,riid,pInterface)  \
    (This)->lpVtbl -> GetCallContext(This,pMsg,riid,pInterface)

#define IRpcChannelBuffer3_GetDestCtxEx(This,pMsg,pdwDestContext,ppvDestContext)  \
    (This)->lpVtbl -> GetDestCtxEx(This,pMsg,pdwDestContext,ppvDestContext)

#define IRpcChannelBuffer3_GetState(This,pMsg,pState)  \
    (This)->lpVtbl -> GetState(This,pMsg,pState)

#define IRpcChannelBuffer3_RegisterAsync(This,pMsg,pAsyncMgr)  \
    (This)->lpVtbl -> RegisterAsync(This,pMsg,pAsyncMgr)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer3_Send_Proxy(IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, ULONG * pulStatus);

    void __RPC_STUB IRpcChannelBuffer3_Send_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer3_Receive_Proxy(IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, ULONG ulSize, ULONG * pulStatus);

    void __RPC_STUB IRpcChannelBuffer3_Receive_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer3_Cancel_Proxy(IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg);

    void __RPC_STUB IRpcChannelBuffer3_Cancel_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer3_GetCallContext_Proxy(IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, REFIID riid, void **pInterface);

    void __RPC_STUB IRpcChannelBuffer3_GetCallContext_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer3_GetDestCtxEx_Proxy(IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, DWORD * pdwDestContext, void **ppvDestContext);

    void __RPC_STUB IRpcChannelBuffer3_GetDestCtxEx_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer3_GetState_Proxy(IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, DWORD * pState);

    void __RPC_STUB IRpcChannelBuffer3_GetState_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcChannelBuffer3_RegisterAsync_Proxy(IRpcChannelBuffer3 * This, RPCOLEMESSAGE * pMsg, IAsyncManager * pAsyncMgr);

    void __RPC_STUB IRpcChannelBuffer3_RegisterAsync_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRpcChannelBuffer3_INTERFACE_DEFINED__ */

#ifndef __IRpcProxyBuffer_INTERFACE_DEFINED__
#define __IRpcProxyBuffer_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IRpcProxyBuffer;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("D5F56A34-593B-101A-B569-08002B2DBF7A")
     IRpcProxyBuffer:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Connect(IRpcChannelBuffer * pRpcChannelBuffer) = 0;

        virtual void STDMETHODCALLTYPE Disconnect(void) = 0;

    };

#else

    typedef struct IRpcProxyBufferVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRpcProxyBuffer * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRpcProxyBuffer * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRpcProxyBuffer * This);

         HRESULT(STDMETHODCALLTYPE * Connect) (IRpcProxyBuffer * This, IRpcChannelBuffer * pRpcChannelBuffer);

        void (STDMETHODCALLTYPE * Disconnect) (IRpcProxyBuffer * This);

     END_INTERFACE} IRpcProxyBufferVtbl;

    interface IRpcProxyBuffer {
        CONST_VTBL struct IRpcProxyBufferVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRpcProxyBuffer_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRpcProxyBuffer_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IRpcProxyBuffer_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IRpcProxyBuffer_Connect(This,pRpcChannelBuffer) \
    (This)->lpVtbl -> Connect(This,pRpcChannelBuffer)

#define IRpcProxyBuffer_Disconnect(This)  \
    (This)->lpVtbl -> Disconnect(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRpcProxyBuffer_Connect_Proxy(IRpcProxyBuffer * This, IRpcChannelBuffer * pRpcChannelBuffer);

    void __RPC_STUB IRpcProxyBuffer_Connect_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IRpcProxyBuffer_Disconnect_Proxy(IRpcProxyBuffer * This);

    void __RPC_STUB IRpcProxyBuffer_Disconnect_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRpcProxyBuffer_INTERFACE_DEFINED__ */

#ifndef __IRpcStubBuffer_INTERFACE_DEFINED__
#define __IRpcStubBuffer_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IRpcStubBuffer;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("D5F56AFC-593B-101A-B569-08002B2DBF7A")
     IRpcStubBuffer:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Connect(IUnknown * pUnkServer) = 0;

        virtual void STDMETHODCALLTYPE Disconnect(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Invoke(RPCOLEMESSAGE * _prpcmsg, IRpcChannelBuffer * _pRpcChannelBuffer) = 0;

        virtual IRpcStubBuffer *STDMETHODCALLTYPE IsIIDSupported(REFIID riid) = 0;

        virtual ULONG STDMETHODCALLTYPE CountRefs(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE DebugServerQueryInterface(void **ppv) = 0;

        virtual void STDMETHODCALLTYPE DebugServerRelease(void *pv) = 0;

    };

#else

    typedef struct IRpcStubBufferVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRpcStubBuffer * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRpcStubBuffer * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRpcStubBuffer * This);

         HRESULT(STDMETHODCALLTYPE * Connect) (IRpcStubBuffer * This, IUnknown * pUnkServer);

        void (STDMETHODCALLTYPE * Disconnect) (IRpcStubBuffer * This);

         HRESULT(STDMETHODCALLTYPE * Invoke) (IRpcStubBuffer * This, RPCOLEMESSAGE * _prpcmsg, IRpcChannelBuffer * _pRpcChannelBuffer);

        IRpcStubBuffer *(STDMETHODCALLTYPE * IsIIDSupported) (IRpcStubBuffer * This, REFIID riid);

         ULONG(STDMETHODCALLTYPE * CountRefs) (IRpcStubBuffer * This);

         HRESULT(STDMETHODCALLTYPE * DebugServerQueryInterface) (IRpcStubBuffer * This, void **ppv);

        void (STDMETHODCALLTYPE * DebugServerRelease) (IRpcStubBuffer * This, void *pv);

     END_INTERFACE} IRpcStubBufferVtbl;

    interface IRpcStubBuffer {
        CONST_VTBL struct IRpcStubBufferVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRpcStubBuffer_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRpcStubBuffer_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IRpcStubBuffer_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IRpcStubBuffer_Connect(This,pUnkServer) \
    (This)->lpVtbl -> Connect(This,pUnkServer)

#define IRpcStubBuffer_Disconnect(This) \
    (This)->lpVtbl -> Disconnect(This)

#define IRpcStubBuffer_Invoke(This,_prpcmsg,_pRpcChannelBuffer) \
    (This)->lpVtbl -> Invoke(This,_prpcmsg,_pRpcChannelBuffer)

#define IRpcStubBuffer_IsIIDSupported(This,riid)  \
    (This)->lpVtbl -> IsIIDSupported(This,riid)

#define IRpcStubBuffer_CountRefs(This)  \
    (This)->lpVtbl -> CountRefs(This)

#define IRpcStubBuffer_DebugServerQueryInterface(This,ppv)  \
    (This)->lpVtbl -> DebugServerQueryInterface(This,ppv)

#define IRpcStubBuffer_DebugServerRelease(This,pv)  \
    (This)->lpVtbl -> DebugServerRelease(This,pv)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRpcStubBuffer_Connect_Proxy(IRpcStubBuffer * This, IUnknown * pUnkServer);

    void __RPC_STUB IRpcStubBuffer_Connect_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IRpcStubBuffer_Disconnect_Proxy(IRpcStubBuffer * This);

    void __RPC_STUB IRpcStubBuffer_Disconnect_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcStubBuffer_Invoke_Proxy(IRpcStubBuffer * This, RPCOLEMESSAGE * _prpcmsg, IRpcChannelBuffer * _pRpcChannelBuffer);

    void __RPC_STUB IRpcStubBuffer_Invoke_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    IRpcStubBuffer *STDMETHODCALLTYPE IRpcStubBuffer_IsIIDSupported_Proxy(IRpcStubBuffer * This, REFIID riid);

    void __RPC_STUB IRpcStubBuffer_IsIIDSupported_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    ULONG STDMETHODCALLTYPE IRpcStubBuffer_CountRefs_Proxy(IRpcStubBuffer * This);

    void __RPC_STUB IRpcStubBuffer_CountRefs_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcStubBuffer_DebugServerQueryInterface_Proxy(IRpcStubBuffer * This, void **ppv);

    void __RPC_STUB IRpcStubBuffer_DebugServerQueryInterface_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IRpcStubBuffer_DebugServerRelease_Proxy(IRpcStubBuffer * This, void *pv);

    void __RPC_STUB IRpcStubBuffer_DebugServerRelease_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRpcStubBuffer_INTERFACE_DEFINED__ */

#ifndef __IPSFactoryBuffer_INTERFACE_DEFINED__
#define __IPSFactoryBuffer_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IPSFactoryBuffer;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("D5F569D0-593B-101A-B569-08002B2DBF7A")
     IPSFactoryBuffer:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE CreateProxy(IUnknown * pUnkOuter, REFIID riid, IRpcProxyBuffer * *ppProxy, void **ppv) = 0;

        virtual HRESULT STDMETHODCALLTYPE CreateStub(REFIID riid, IUnknown * pUnkServer, IRpcStubBuffer * *ppStub) = 0;

    };

#else

    typedef struct IPSFactoryBufferVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IPSFactoryBuffer * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IPSFactoryBuffer * This);

         ULONG(STDMETHODCALLTYPE * Release) (IPSFactoryBuffer * This);

         HRESULT(STDMETHODCALLTYPE * CreateProxy) (IPSFactoryBuffer * This, IUnknown * pUnkOuter, REFIID riid, IRpcProxyBuffer * *ppProxy, void **ppv);

         HRESULT(STDMETHODCALLTYPE * CreateStub) (IPSFactoryBuffer * This, REFIID riid, IUnknown * pUnkServer, IRpcStubBuffer * *ppStub);

     END_INTERFACE} IPSFactoryBufferVtbl;

    interface IPSFactoryBuffer {
        CONST_VTBL struct IPSFactoryBufferVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IPSFactoryBuffer_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IPSFactoryBuffer_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IPSFactoryBuffer_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IPSFactoryBuffer_CreateProxy(This,pUnkOuter,riid,ppProxy,ppv)  \
    (This)->lpVtbl -> CreateProxy(This,pUnkOuter,riid,ppProxy,ppv)

#define IPSFactoryBuffer_CreateStub(This,riid,pUnkServer,ppStub)  \
    (This)->lpVtbl -> CreateStub(This,riid,pUnkServer,ppStub)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IPSFactoryBuffer_CreateProxy_Proxy(IPSFactoryBuffer * This, IUnknown * pUnkOuter, REFIID riid, IRpcProxyBuffer * *ppProxy, void **ppv);

    void __RPC_STUB IPSFactoryBuffer_CreateProxy_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPSFactoryBuffer_CreateStub_Proxy(IPSFactoryBuffer * This, REFIID riid, IUnknown * pUnkServer, IRpcStubBuffer * *ppStub);

    void __RPC_STUB IPSFactoryBuffer_CreateStub_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IPSFactoryBuffer_INTERFACE_DEFINED__ */

#if  (_WIN32_WINNT >= 0x0400 ) || defined(_WIN32_DCOM)
    typedef struct SChannelHookCallInfo {
        IID iid;
        DWORD cbSize;
        GUID uCausality;
        DWORD dwServerPid;
        DWORD iMethod;
        void *pObject;
    } SChannelHookCallInfo;

    extern RPC_IF_HANDLE __MIDL_itf_objidl_0049_v0_0_c_ifspec;
    extern RPC_IF_HANDLE __MIDL_itf_objidl_0049_v0_0_s_ifspec;

#ifndef __IChannelHook_INTERFACE_DEFINED__
#define __IChannelHook_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IChannelHook;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("1008c4a0-7613-11cf-9af1-0020af6e72f4")
     IChannelHook:public IUnknown {
      public:
        virtual void STDMETHODCALLTYPE ClientGetSize(REFGUID uExtent, REFIID riid, ULONG * pDataSize) = 0;

        virtual void STDMETHODCALLTYPE ClientFillBuffer(REFGUID uExtent, REFIID riid, ULONG * pDataSize, void *pDataBuffer) = 0;

        virtual void STDMETHODCALLTYPE ClientNotify(REFGUID uExtent, REFIID riid, ULONG cbDataSize, void *pDataBuffer, DWORD lDataRep, HRESULT hrFault) = 0;

        virtual void STDMETHODCALLTYPE ServerNotify(REFGUID uExtent, REFIID riid, ULONG cbDataSize, void *pDataBuffer, DWORD lDataRep) = 0;

        virtual void STDMETHODCALLTYPE ServerGetSize(REFGUID uExtent, REFIID riid, HRESULT hrFault, ULONG * pDataSize) = 0;

        virtual void STDMETHODCALLTYPE ServerFillBuffer(REFGUID uExtent, REFIID riid, ULONG * pDataSize, void *pDataBuffer, HRESULT hrFault) = 0;

    };

#else

    typedef struct IChannelHookVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IChannelHook * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IChannelHook * This);

         ULONG(STDMETHODCALLTYPE * Release) (IChannelHook * This);

        void (STDMETHODCALLTYPE * ClientGetSize) (IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG * pDataSize);

        void (STDMETHODCALLTYPE * ClientFillBuffer) (IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG * pDataSize, void *pDataBuffer);

        void (STDMETHODCALLTYPE * ClientNotify) (IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG cbDataSize, void *pDataBuffer, DWORD lDataRep, HRESULT hrFault);

        void (STDMETHODCALLTYPE * ServerNotify) (IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG cbDataSize, void *pDataBuffer, DWORD lDataRep);

        void (STDMETHODCALLTYPE * ServerGetSize) (IChannelHook * This, REFGUID uExtent, REFIID riid, HRESULT hrFault, ULONG * pDataSize);

        void (STDMETHODCALLTYPE * ServerFillBuffer) (IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG * pDataSize, void *pDataBuffer, HRESULT hrFault);

     END_INTERFACE} IChannelHookVtbl;

    interface IChannelHook {
        CONST_VTBL struct IChannelHookVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IChannelHook_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IChannelHook_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IChannelHook_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IChannelHook_ClientGetSize(This,uExtent,riid,pDataSize) \
    (This)->lpVtbl -> ClientGetSize(This,uExtent,riid,pDataSize)

#define IChannelHook_ClientFillBuffer(This,uExtent,riid,pDataSize,pDataBuffer)  \
    (This)->lpVtbl -> ClientFillBuffer(This,uExtent,riid,pDataSize,pDataBuffer)

#define IChannelHook_ClientNotify(This,uExtent,riid,cbDataSize,pDataBuffer,lDataRep,hrFault)  \
    (This)->lpVtbl -> ClientNotify(This,uExtent,riid,cbDataSize,pDataBuffer,lDataRep,hrFault)

#define IChannelHook_ServerNotify(This,uExtent,riid,cbDataSize,pDataBuffer,lDataRep)  \
    (This)->lpVtbl -> ServerNotify(This,uExtent,riid,cbDataSize,pDataBuffer,lDataRep)

#define IChannelHook_ServerGetSize(This,uExtent,riid,hrFault,pDataSize) \
    (This)->lpVtbl -> ServerGetSize(This,uExtent,riid,hrFault,pDataSize)

#define IChannelHook_ServerFillBuffer(This,uExtent,riid,pDataSize,pDataBuffer,hrFault)  \
    (This)->lpVtbl -> ServerFillBuffer(This,uExtent,riid,pDataSize,pDataBuffer,hrFault)

#endif /* COBJMACROS */

#endif

    void STDMETHODCALLTYPE IChannelHook_ClientGetSize_Proxy(IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG * pDataSize);

    void __RPC_STUB IChannelHook_ClientGetSize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IChannelHook_ClientFillBuffer_Proxy(IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG * pDataSize, void *pDataBuffer);

    void __RPC_STUB IChannelHook_ClientFillBuffer_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IChannelHook_ClientNotify_Proxy(IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG cbDataSize, void *pDataBuffer, DWORD lDataRep, HRESULT hrFault);

    void __RPC_STUB IChannelHook_ClientNotify_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IChannelHook_ServerNotify_Proxy(IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG cbDataSize, void *pDataBuffer, DWORD lDataRep);

    void __RPC_STUB IChannelHook_ServerNotify_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IChannelHook_ServerGetSize_Proxy(IChannelHook * This, REFGUID uExtent, REFIID riid, HRESULT hrFault, ULONG * pDataSize);

    void __RPC_STUB IChannelHook_ServerGetSize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    void STDMETHODCALLTYPE IChannelHook_ServerFillBuffer_Proxy(IChannelHook * This, REFGUID uExtent, REFIID riid, ULONG * pDataSize, void *pDataBuffer, HRESULT hrFault);

    void __RPC_STUB IChannelHook_ServerFillBuffer_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IChannelHook_INTERFACE_DEFINED__ */

#endif

    extern const FMTID FMTID_SummaryInformation;
    extern const FMTID FMTID_DocSummaryInformation;
    extern const FMTID FMTID_UserDefinedProperties;
    extern const FMTID FMTID_DiscardableInformation;
    extern const FMTID FMTID_ImageSummaryInformation;
    extern const FMTID FMTID_AudioSummaryInformation;
    extern const FMTID FMTID_VideoSummaryInformation;
    extern const FMTID FMTID_MediaFileSummaryInformation;

#if  (_WIN32_WINNT >= 0x0400 ) || defined(_WIN32_DCOM)

    extern RPC_IF_HANDLE __MIDL_itf_objidl_0050_v0_0_c_ifspec;
    extern RPC_IF_HANDLE __MIDL_itf_objidl_0050_v0_0_s_ifspec;

#ifndef __IClientSecurity_INTERFACE_DEFINED__
#define __IClientSecurity_INTERFACE_DEFINED__

    typedef struct tagSOLE_AUTHENTICATION_SERVICE {
        DWORD dwAuthnSvc;
        DWORD dwAuthzSvc;
        OLECHAR *pPrincipalName;
        HRESULT hr;
    } SOLE_AUTHENTICATION_SERVICE;

    typedef SOLE_AUTHENTICATION_SERVICE *PSOLE_AUTHENTICATION_SERVICE;

    typedef
        enum tagEOLE_AUTHENTICATION_CAPABILITIES { EOAC_NONE = 0,
        EOAC_MUTUAL_AUTH = 0x1,
        EOAC_STATIC_CLOAKING = 0x20,
        EOAC_DYNAMIC_CLOAKING = 0x40,
        EOAC_ANY_AUTHORITY = 0x80,
        EOAC_MAKE_FULLSIC = 0x100,
        EOAC_DEFAULT = 0x800,
        EOAC_SECURE_REFS = 0x2,
        EOAC_ACCESS_CONTROL = 0x4,
        EOAC_APPID = 0x8,
        EOAC_DYNAMIC = 0x10,
        EOAC_REQUIRE_FULLSIC = 0x200,
        EOAC_AUTO_IMPERSONATE = 0x400,
        EOAC_NO_CUSTOM_MARSHAL = 0x2000,
        EOAC_DISABLE_AAA = 0x1000
    } EOLE_AUTHENTICATION_CAPABILITIES;

#define COLE_DEFAULT_PRINCIPAL  ( ( OLECHAR  * )-1 )

#define COLE_DEFAULT_AUTHINFO  ( ( void  * )-1 )

    typedef struct tagSOLE_AUTHENTICATION_INFO {
        DWORD dwAuthnSvc;
        DWORD dwAuthzSvc;
        void *pAuthInfo;
    } SOLE_AUTHENTICATION_INFO;

    typedef struct tagSOLE_AUTHENTICATION_INFO *PSOLE_AUTHENTICATION_INFO;

    typedef struct tagSOLE_AUTHENTICATION_LIST {
        DWORD cAuthInfo;
        SOLE_AUTHENTICATION_INFO *aAuthInfo;
    } SOLE_AUTHENTICATION_LIST;

    typedef struct tagSOLE_AUTHENTICATION_LIST *PSOLE_AUTHENTICATION_LIST;

    EXTERN_C const IID IID_IClientSecurity;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000013D-0000-0000-C000-000000000046")
     IClientSecurity:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE QueryBlanket(IUnknown * pProxy, DWORD * pAuthnSvc, DWORD * pAuthzSvc, OLECHAR * *pServerPrincName, DWORD * pAuthnLevel, DWORD * pImpLevel, void **pAuthInfo, DWORD * pCapabilites) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetBlanket(IUnknown * pProxy, DWORD dwAuthnSvc, DWORD dwAuthzSvc, OLECHAR * pServerPrincName, DWORD dwAuthnLevel, DWORD dwImpLevel, void *pAuthInfo, DWORD dwCapabilities) = 0;

        virtual HRESULT STDMETHODCALLTYPE CopyProxy(IUnknown * pProxy, IUnknown * *ppCopy) = 0;

    };

#else

    typedef struct IClientSecurityVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IClientSecurity * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IClientSecurity * This);

         ULONG(STDMETHODCALLTYPE * Release) (IClientSecurity * This);

         HRESULT(STDMETHODCALLTYPE * QueryBlanket) (IClientSecurity * This, IUnknown * pProxy, DWORD * pAuthnSvc, DWORD * pAuthzSvc, OLECHAR * *pServerPrincName, DWORD * pAuthnLevel, DWORD * pImpLevel, void **pAuthInfo, DWORD * pCapabilites);

         HRESULT(STDMETHODCALLTYPE * SetBlanket) (IClientSecurity * This, IUnknown * pProxy, DWORD dwAuthnSvc, DWORD dwAuthzSvc, OLECHAR * pServerPrincName, DWORD dwAuthnLevel, DWORD dwImpLevel, void *pAuthInfo, DWORD dwCapabilities);

         HRESULT(STDMETHODCALLTYPE * CopyProxy) (IClientSecurity * This, IUnknown * pProxy, IUnknown * *ppCopy);

     END_INTERFACE} IClientSecurityVtbl;

    interface IClientSecurity {
        CONST_VTBL struct IClientSecurityVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IClientSecurity_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IClientSecurity_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IClientSecurity_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IClientSecurity_QueryBlanket(This,pProxy,pAuthnSvc,pAuthzSvc,pServerPrincName,pAuthnLevel,pImpLevel,pAuthInfo,pCapabilites)  \
    (This)->lpVtbl -> QueryBlanket(This,pProxy,pAuthnSvc,pAuthzSvc,pServerPrincName,pAuthnLevel,pImpLevel,pAuthInfo,pCapabilites)

#define IClientSecurity_SetBlanket(This,pProxy,dwAuthnSvc,dwAuthzSvc,pServerPrincName,dwAuthnLevel,dwImpLevel,pAuthInfo,dwCapabilities) \
    (This)->lpVtbl -> SetBlanket(This,pProxy,dwAuthnSvc,dwAuthzSvc,pServerPrincName,dwAuthnLevel,dwImpLevel,pAuthInfo,dwCapabilities)

#define IClientSecurity_CopyProxy(This,pProxy,ppCopy)  \
    (This)->lpVtbl -> CopyProxy(This,pProxy,ppCopy)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IClientSecurity_QueryBlanket_Proxy(IClientSecurity * This, IUnknown * pProxy, DWORD * pAuthnSvc, DWORD * pAuthzSvc, OLECHAR * *pServerPrincName, DWORD * pAuthnLevel, DWORD * pImpLevel, void **pAuthInfo, DWORD * pCapabilites);

    void __RPC_STUB IClientSecurity_QueryBlanket_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IClientSecurity_SetBlanket_Proxy(IClientSecurity * This, IUnknown * pProxy, DWORD dwAuthnSvc, DWORD dwAuthzSvc, OLECHAR * pServerPrincName, DWORD dwAuthnLevel, DWORD dwImpLevel, void *pAuthInfo, DWORD dwCapabilities);

    void __RPC_STUB IClientSecurity_SetBlanket_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IClientSecurity_CopyProxy_Proxy(IClientSecurity * This, IUnknown * pProxy, IUnknown * *ppCopy);

    void __RPC_STUB IClientSecurity_CopyProxy_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IClientSecurity_INTERFACE_DEFINED__ */

#ifndef __IServerSecurity_INTERFACE_DEFINED__
#define __IServerSecurity_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IServerSecurity;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000013E-0000-0000-C000-000000000046")
     IServerSecurity:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE QueryBlanket(DWORD * pAuthnSvc, DWORD * pAuthzSvc, OLECHAR * *pServerPrincName, DWORD * pAuthnLevel, DWORD * pImpLevel, void **pPrivs, DWORD * pCapabilities) = 0;

        virtual HRESULT STDMETHODCALLTYPE ImpersonateClient(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE RevertToSelf(void) = 0;

        virtual BOOL STDMETHODCALLTYPE IsImpersonating(void) = 0;

    };

#else

    typedef struct IServerSecurityVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IServerSecurity * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IServerSecurity * This);

         ULONG(STDMETHODCALLTYPE * Release) (IServerSecurity * This);

         HRESULT(STDMETHODCALLTYPE * QueryBlanket) (IServerSecurity * This, DWORD * pAuthnSvc, DWORD * pAuthzSvc, OLECHAR * *pServerPrincName, DWORD * pAuthnLevel, DWORD * pImpLevel, void **pPrivs, DWORD * pCapabilities);

         HRESULT(STDMETHODCALLTYPE * ImpersonateClient) (IServerSecurity * This);

         HRESULT(STDMETHODCALLTYPE * RevertToSelf) (IServerSecurity * This);

         BOOL(STDMETHODCALLTYPE * IsImpersonating) (IServerSecurity * This);

     END_INTERFACE} IServerSecurityVtbl;

    interface IServerSecurity {
        CONST_VTBL struct IServerSecurityVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IServerSecurity_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IServerSecurity_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IServerSecurity_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IServerSecurity_QueryBlanket(This,pAuthnSvc,pAuthzSvc,pServerPrincName,pAuthnLevel,pImpLevel,pPrivs,pCapabilities)  \
    (This)->lpVtbl -> QueryBlanket(This,pAuthnSvc,pAuthzSvc,pServerPrincName,pAuthnLevel,pImpLevel,pPrivs,pCapabilities)

#define IServerSecurity_ImpersonateClient(This) \
    (This)->lpVtbl -> ImpersonateClient(This)

#define IServerSecurity_RevertToSelf(This)  \
    (This)->lpVtbl -> RevertToSelf(This)

#define IServerSecurity_IsImpersonating(This)  \
    (This)->lpVtbl -> IsImpersonating(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IServerSecurity_QueryBlanket_Proxy(IServerSecurity * This, DWORD * pAuthnSvc, DWORD * pAuthzSvc, OLECHAR * *pServerPrincName, DWORD * pAuthnLevel, DWORD * pImpLevel, void **pPrivs, DWORD * pCapabilities);

    void __RPC_STUB IServerSecurity_QueryBlanket_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IServerSecurity_ImpersonateClient_Proxy(IServerSecurity * This);

    void __RPC_STUB IServerSecurity_ImpersonateClient_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IServerSecurity_RevertToSelf_Proxy(IServerSecurity * This);

    void __RPC_STUB IServerSecurity_RevertToSelf_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    BOOL STDMETHODCALLTYPE IServerSecurity_IsImpersonating_Proxy(IServerSecurity * This);

    void __RPC_STUB IServerSecurity_IsImpersonating_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IServerSecurity_INTERFACE_DEFINED__ */

#ifndef __IClassActivator_INTERFACE_DEFINED__
#define __IClassActivator_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IClassActivator;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000140-0000-0000-C000-000000000046")
     IClassActivator:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetClassObject(REFCLSID rclsid, DWORD dwClassContext, LCID locale, REFIID riid, void **ppv) = 0;

    };

#else

    typedef struct IClassActivatorVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IClassActivator * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IClassActivator * This);

         ULONG(STDMETHODCALLTYPE * Release) (IClassActivator * This);

         HRESULT(STDMETHODCALLTYPE * GetClassObject) (IClassActivator * This, REFCLSID rclsid, DWORD dwClassContext, LCID locale, REFIID riid, void **ppv);

     END_INTERFACE} IClassActivatorVtbl;

    interface IClassActivator {
        CONST_VTBL struct IClassActivatorVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IClassActivator_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IClassActivator_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IClassActivator_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IClassActivator_GetClassObject(This,rclsid,dwClassContext,locale,riid,ppv)  \
    (This)->lpVtbl -> GetClassObject(This,rclsid,dwClassContext,locale,riid,ppv)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IClassActivator_GetClassObject_Proxy(IClassActivator * This, REFCLSID rclsid, DWORD dwClassContext, LCID locale, REFIID riid, void **ppv);

    void __RPC_STUB IClassActivator_GetClassObject_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IClassActivator_INTERFACE_DEFINED__ */

#ifndef __IRpcOptions_INTERFACE_DEFINED__
#define __IRpcOptions_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IRpcOptions;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000144-0000-0000-C000-000000000046")
     IRpcOptions:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Set(IUnknown * pPrx, DWORD dwProperty, ULONG_PTR dwValue) = 0;

        virtual HRESULT STDMETHODCALLTYPE Query(IUnknown * pPrx, DWORD dwProperty, ULONG_PTR * pdwValue) = 0;

    };

#else

    typedef struct IRpcOptionsVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRpcOptions * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRpcOptions * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRpcOptions * This);

         HRESULT(STDMETHODCALLTYPE * Set) (IRpcOptions * This, IUnknown * pPrx, DWORD dwProperty, ULONG_PTR dwValue);

         HRESULT(STDMETHODCALLTYPE * Query) (IRpcOptions * This, IUnknown * pPrx, DWORD dwProperty, ULONG_PTR * pdwValue);

     END_INTERFACE} IRpcOptionsVtbl;

    interface IRpcOptions {
        CONST_VTBL struct IRpcOptionsVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRpcOptions_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRpcOptions_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IRpcOptions_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IRpcOptions_Set(This,pPrx,dwProperty,dwValue)  \
    (This)->lpVtbl -> Set(This,pPrx,dwProperty,dwValue)

#define IRpcOptions_Query(This,pPrx,dwProperty,pdwValue)  \
    (This)->lpVtbl -> Query(This,pPrx,dwProperty,pdwValue)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRpcOptions_Set_Proxy(IRpcOptions * This, IUnknown * pPrx, DWORD dwProperty, ULONG_PTR dwValue);

    void __RPC_STUB IRpcOptions_Set_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcOptions_Query_Proxy(IRpcOptions * This, IUnknown * pPrx, DWORD dwProperty, ULONG_PTR * pdwValue);

    void __RPC_STUB IRpcOptions_Query_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRpcOptions_INTERFACE_DEFINED__ */

    enum __MIDL___MIDL_itf_objidl_0054_0001 { COMBND_RPCTIMEOUT = 0x1
    };
#endif

    extern RPC_IF_HANDLE __MIDL_itf_objidl_0054_v0_0_c_ifspec;
    extern RPC_IF_HANDLE __MIDL_itf_objidl_0054_v0_0_s_ifspec;

#ifndef __IFillLockBytes_INTERFACE_DEFINED__
#define __IFillLockBytes_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IFillLockBytes;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("99caf010-415e-11cf-8814-00aa00b569f5")
     IFillLockBytes:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE FillAppend(const void *pv, ULONG cb, ULONG * pcbWritten) = 0;

        virtual HRESULT STDMETHODCALLTYPE FillAt(ULARGE_INTEGER ulOffset, const void *pv, ULONG cb, ULONG * pcbWritten) = 0;

        virtual HRESULT STDMETHODCALLTYPE SetFillSize(ULARGE_INTEGER ulSize) = 0;

        virtual HRESULT STDMETHODCALLTYPE Terminate(BOOL bCanceled) = 0;

    };

#else

    typedef struct IFillLockBytesVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IFillLockBytes * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IFillLockBytes * This);

         ULONG(STDMETHODCALLTYPE * Release) (IFillLockBytes * This);

         HRESULT(STDMETHODCALLTYPE * FillAppend) (IFillLockBytes * This, const void *pv, ULONG cb, ULONG * pcbWritten);

         HRESULT(STDMETHODCALLTYPE * FillAt) (IFillLockBytes * This, ULARGE_INTEGER ulOffset, const void *pv, ULONG cb, ULONG * pcbWritten);

         HRESULT(STDMETHODCALLTYPE * SetFillSize) (IFillLockBytes * This, ULARGE_INTEGER ulSize);

         HRESULT(STDMETHODCALLTYPE * Terminate) (IFillLockBytes * This, BOOL bCanceled);

     END_INTERFACE} IFillLockBytesVtbl;

    interface IFillLockBytes {
        CONST_VTBL struct IFillLockBytesVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IFillLockBytes_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IFillLockBytes_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IFillLockBytes_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IFillLockBytes_FillAppend(This,pv,cb,pcbWritten)  \
    (This)->lpVtbl -> FillAppend(This,pv,cb,pcbWritten)

#define IFillLockBytes_FillAt(This,ulOffset,pv,cb,pcbWritten)  \
    (This)->lpVtbl -> FillAt(This,ulOffset,pv,cb,pcbWritten)

#define IFillLockBytes_SetFillSize(This,ulSize) \
    (This)->lpVtbl -> SetFillSize(This,ulSize)

#define IFillLockBytes_Terminate(This,bCanceled)  \
    (This)->lpVtbl -> Terminate(This,bCanceled)

#endif /* COBJMACROS */

#endif

    HRESULT __stdcall IFillLockBytes_RemoteFillAppend_Proxy(IFillLockBytes * This, const byte * pv, ULONG cb, ULONG * pcbWritten);

    void __RPC_STUB IFillLockBytes_RemoteFillAppend_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT __stdcall IFillLockBytes_RemoteFillAt_Proxy(IFillLockBytes * This, ULARGE_INTEGER ulOffset, const byte * pv, ULONG cb, ULONG * pcbWritten);

    void __RPC_STUB IFillLockBytes_RemoteFillAt_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IFillLockBytes_SetFillSize_Proxy(IFillLockBytes * This, ULARGE_INTEGER ulSize);

    void __RPC_STUB IFillLockBytes_SetFillSize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IFillLockBytes_Terminate_Proxy(IFillLockBytes * This, BOOL bCanceled);

    void __RPC_STUB IFillLockBytes_Terminate_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IFillLockBytes_INTERFACE_DEFINED__ */

#ifndef __IProgressNotify_INTERFACE_DEFINED__
#define __IProgressNotify_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IProgressNotify;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("a9d758a0-4617-11cf-95fc-00aa00680db4")
     IProgressNotify:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE OnProgress(DWORD dwProgressCurrent, DWORD dwProgressMaximum, BOOL fAccurate, BOOL fOwner) = 0;

    };

#else

    typedef struct IProgressNotifyVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IProgressNotify * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IProgressNotify * This);

         ULONG(STDMETHODCALLTYPE * Release) (IProgressNotify * This);

         HRESULT(STDMETHODCALLTYPE * OnProgress) (IProgressNotify * This, DWORD dwProgressCurrent, DWORD dwProgressMaximum, BOOL fAccurate, BOOL fOwner);

     END_INTERFACE} IProgressNotifyVtbl;

    interface IProgressNotify {
        CONST_VTBL struct IProgressNotifyVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IProgressNotify_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IProgressNotify_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IProgressNotify_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IProgressNotify_OnProgress(This,dwProgressCurrent,dwProgressMaximum,fAccurate,fOwner)  \
    (This)->lpVtbl -> OnProgress(This,dwProgressCurrent,dwProgressMaximum,fAccurate,fOwner)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IProgressNotify_OnProgress_Proxy(IProgressNotify * This, DWORD dwProgressCurrent, DWORD dwProgressMaximum, BOOL fAccurate, BOOL fOwner);

    void __RPC_STUB IProgressNotify_OnProgress_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IProgressNotify_INTERFACE_DEFINED__ */

#ifndef __ILayoutStorage_INTERFACE_DEFINED__
#define __ILayoutStorage_INTERFACE_DEFINED__

    typedef struct tagStorageLayout {
        DWORD LayoutType;
        OLECHAR *pwcsElementName;
        LARGE_INTEGER cOffset;
        LARGE_INTEGER cBytes;
    } StorageLayout;

    EXTERN_C const IID IID_ILayoutStorage;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0e6d4d90-6738-11cf-9608-00aa00680db4")
     ILayoutStorage:public IUnknown {
      public:
        virtual HRESULT __stdcall LayoutScript(StorageLayout * pStorageLayout, DWORD nEntries, DWORD glfInterleavedFlag) = 0;

        virtual HRESULT __stdcall BeginMonitor(void) = 0;

        virtual HRESULT __stdcall EndMonitor(void) = 0;

        virtual HRESULT __stdcall ReLayoutDocfile(OLECHAR * pwcsNewDfName) = 0;

        virtual HRESULT __stdcall ReLayoutDocfileOnILockBytes(ILockBytes * pILockBytes) = 0;

    };

#else

    typedef struct ILayoutStorageVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ILayoutStorage * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ILayoutStorage * This);

         ULONG(STDMETHODCALLTYPE * Release) (ILayoutStorage * This);

         HRESULT(__stdcall * LayoutScript) (ILayoutStorage * This, StorageLayout * pStorageLayout, DWORD nEntries, DWORD glfInterleavedFlag);

         HRESULT(__stdcall * BeginMonitor) (ILayoutStorage * This);

         HRESULT(__stdcall * EndMonitor) (ILayoutStorage * This);

         HRESULT(__stdcall * ReLayoutDocfile) (ILayoutStorage * This, OLECHAR * pwcsNewDfName);

         HRESULT(__stdcall * ReLayoutDocfileOnILockBytes) (ILayoutStorage * This, ILockBytes * pILockBytes);

     END_INTERFACE} ILayoutStorageVtbl;

    interface ILayoutStorage {
        CONST_VTBL struct ILayoutStorageVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ILayoutStorage_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ILayoutStorage_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define ILayoutStorage_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define ILayoutStorage_LayoutScript(This,pStorageLayout,nEntries,glfInterleavedFlag)  \
    (This)->lpVtbl -> LayoutScript(This,pStorageLayout,nEntries,glfInterleavedFlag)

#define ILayoutStorage_BeginMonitor(This)  \
    (This)->lpVtbl -> BeginMonitor(This)

#define ILayoutStorage_EndMonitor(This) \
    (This)->lpVtbl -> EndMonitor(This)

#define ILayoutStorage_ReLayoutDocfile(This,pwcsNewDfName)  \
    (This)->lpVtbl -> ReLayoutDocfile(This,pwcsNewDfName)

#define ILayoutStorage_ReLayoutDocfileOnILockBytes(This,pILockBytes)  \
    (This)->lpVtbl -> ReLayoutDocfileOnILockBytes(This,pILockBytes)

#endif /* COBJMACROS */

#endif

    HRESULT __stdcall ILayoutStorage_LayoutScript_Proxy(ILayoutStorage * This, StorageLayout * pStorageLayout, DWORD nEntries, DWORD glfInterleavedFlag);

    void __RPC_STUB ILayoutStorage_LayoutScript_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT __stdcall ILayoutStorage_BeginMonitor_Proxy(ILayoutStorage * This);

    void __RPC_STUB ILayoutStorage_BeginMonitor_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT __stdcall ILayoutStorage_EndMonitor_Proxy(ILayoutStorage * This);

    void __RPC_STUB ILayoutStorage_EndMonitor_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT __stdcall ILayoutStorage_ReLayoutDocfile_Proxy(ILayoutStorage * This, OLECHAR * pwcsNewDfName);

    void __RPC_STUB ILayoutStorage_ReLayoutDocfile_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT __stdcall ILayoutStorage_ReLayoutDocfileOnILockBytes_Proxy(ILayoutStorage * This, ILockBytes * pILockBytes);

    void __RPC_STUB ILayoutStorage_ReLayoutDocfileOnILockBytes_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ILayoutStorage_INTERFACE_DEFINED__ */

#ifndef __IBlockingLock_INTERFACE_DEFINED__
#define __IBlockingLock_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IBlockingLock;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("30f3d47a-6447-11d1-8e3c-00c04fb9386d")
     IBlockingLock:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Lock(DWORD dwTimeout) = 0;

        virtual HRESULT STDMETHODCALLTYPE Unlock(void) = 0;

    };

#else

    typedef struct IBlockingLockVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IBlockingLock * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IBlockingLock * This);

         ULONG(STDMETHODCALLTYPE * Release) (IBlockingLock * This);

         HRESULT(STDMETHODCALLTYPE * Lock) (IBlockingLock * This, DWORD dwTimeout);

         HRESULT(STDMETHODCALLTYPE * Unlock) (IBlockingLock * This);

     END_INTERFACE} IBlockingLockVtbl;

    interface IBlockingLock {
        CONST_VTBL struct IBlockingLockVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IBlockingLock_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IBlockingLock_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IBlockingLock_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IBlockingLock_Lock(This,dwTimeout)  \
    (This)->lpVtbl -> Lock(This,dwTimeout)

#define IBlockingLock_Unlock(This)  \
    (This)->lpVtbl -> Unlock(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IBlockingLock_Lock_Proxy(IBlockingLock * This, DWORD dwTimeout);

    void __RPC_STUB IBlockingLock_Lock_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IBlockingLock_Unlock_Proxy(IBlockingLock * This);

    void __RPC_STUB IBlockingLock_Unlock_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IBlockingLock_INTERFACE_DEFINED__ */

#ifndef __ITimeAndNoticeControl_INTERFACE_DEFINED__
#define __ITimeAndNoticeControl_INTERFACE_DEFINED__

    EXTERN_C const IID IID_ITimeAndNoticeControl;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("bc0bf6ae-8878-11d1-83e9-00c04fc2c6d4")
     ITimeAndNoticeControl:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE SuppressChanges(DWORD res1, DWORD res2) = 0;

    };

#else

    typedef struct ITimeAndNoticeControlVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ITimeAndNoticeControl * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ITimeAndNoticeControl * This);

         ULONG(STDMETHODCALLTYPE * Release) (ITimeAndNoticeControl * This);

         HRESULT(STDMETHODCALLTYPE * SuppressChanges) (ITimeAndNoticeControl * This, DWORD res1, DWORD res2);

     END_INTERFACE} ITimeAndNoticeControlVtbl;

    interface ITimeAndNoticeControl {
        CONST_VTBL struct ITimeAndNoticeControlVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ITimeAndNoticeControl_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ITimeAndNoticeControl_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define ITimeAndNoticeControl_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define ITimeAndNoticeControl_SuppressChanges(This,res1,res2)  \
    (This)->lpVtbl -> SuppressChanges(This,res1,res2)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ITimeAndNoticeControl_SuppressChanges_Proxy(ITimeAndNoticeControl * This, DWORD res1, DWORD res2);

    void __RPC_STUB ITimeAndNoticeControl_SuppressChanges_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ITimeAndNoticeControl_INTERFACE_DEFINED__ */

#ifndef __IOplockStorage_INTERFACE_DEFINED__
#define __IOplockStorage_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IOplockStorage;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("8d19c834-8879-11d1-83e9-00c04fc2c6d4")
     IOplockStorage:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE CreateStorageEx(LPCWSTR pwcsName, DWORD grfMode, DWORD stgfmt, DWORD grfAttrs, REFIID riid, void **ppstgOpen) = 0;

        virtual HRESULT STDMETHODCALLTYPE OpenStorageEx(LPCWSTR pwcsName, DWORD grfMode, DWORD stgfmt, DWORD grfAttrs, REFIID riid, void **ppstgOpen) = 0;

    };

#else

    typedef struct IOplockStorageVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IOplockStorage * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IOplockStorage * This);

         ULONG(STDMETHODCALLTYPE * Release) (IOplockStorage * This);

         HRESULT(STDMETHODCALLTYPE * CreateStorageEx) (IOplockStorage * This, LPCWSTR pwcsName, DWORD grfMode, DWORD stgfmt, DWORD grfAttrs, REFIID riid, void **ppstgOpen);

         HRESULT(STDMETHODCALLTYPE * OpenStorageEx) (IOplockStorage * This, LPCWSTR pwcsName, DWORD grfMode, DWORD stgfmt, DWORD grfAttrs, REFIID riid, void **ppstgOpen);

     END_INTERFACE} IOplockStorageVtbl;

    interface IOplockStorage {
        CONST_VTBL struct IOplockStorageVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IOplockStorage_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IOplockStorage_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IOplockStorage_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IOplockStorage_CreateStorageEx(This,pwcsName,grfMode,stgfmt,grfAttrs,riid,ppstgOpen)  \
    (This)->lpVtbl -> CreateStorageEx(This,pwcsName,grfMode,stgfmt,grfAttrs,riid,ppstgOpen)

#define IOplockStorage_OpenStorageEx(This,pwcsName,grfMode,stgfmt,grfAttrs,riid,ppstgOpen)  \
    (This)->lpVtbl -> OpenStorageEx(This,pwcsName,grfMode,stgfmt,grfAttrs,riid,ppstgOpen)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IOplockStorage_CreateStorageEx_Proxy(IOplockStorage * This, LPCWSTR pwcsName, DWORD grfMode, DWORD stgfmt, DWORD grfAttrs, REFIID riid, void **ppstgOpen);

    void __RPC_STUB IOplockStorage_CreateStorageEx_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IOplockStorage_OpenStorageEx_Proxy(IOplockStorage * This, LPCWSTR pwcsName, DWORD grfMode, DWORD stgfmt, DWORD grfAttrs, REFIID riid, void **ppstgOpen);

    void __RPC_STUB IOplockStorage_OpenStorageEx_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IOplockStorage_INTERFACE_DEFINED__ */

#ifndef __ISurrogate_INTERFACE_DEFINED__
#define __ISurrogate_INTERFACE_DEFINED__

    typedef ISurrogate *LPSURROGATE;

    EXTERN_C const IID IID_ISurrogate;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000022-0000-0000-C000-000000000046")
     ISurrogate:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE LoadDllServer(REFCLSID Clsid) = 0;

        virtual HRESULT STDMETHODCALLTYPE FreeSurrogate(void) = 0;

    };

#else

    typedef struct ISurrogateVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ISurrogate * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ISurrogate * This);

         ULONG(STDMETHODCALLTYPE * Release) (ISurrogate * This);

         HRESULT(STDMETHODCALLTYPE * LoadDllServer) (ISurrogate * This, REFCLSID Clsid);

         HRESULT(STDMETHODCALLTYPE * FreeSurrogate) (ISurrogate * This);

     END_INTERFACE} ISurrogateVtbl;

    interface ISurrogate {
        CONST_VTBL struct ISurrogateVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ISurrogate_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ISurrogate_AddRef(This) \
    (This)->lpVtbl -> AddRef(This)

#define ISurrogate_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define ISurrogate_LoadDllServer(This,Clsid)  \
    (This)->lpVtbl -> LoadDllServer(This,Clsid)

#define ISurrogate_FreeSurrogate(This)  \
    (This)->lpVtbl -> FreeSurrogate(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ISurrogate_LoadDllServer_Proxy(ISurrogate * This, REFCLSID Clsid);

    void __RPC_STUB ISurrogate_LoadDllServer_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ISurrogate_FreeSurrogate_Proxy(ISurrogate * This);

    void __RPC_STUB ISurrogate_FreeSurrogate_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ISurrogate_INTERFACE_DEFINED__ */

#ifndef __IGlobalInterfaceTable_INTERFACE_DEFINED__
#define __IGlobalInterfaceTable_INTERFACE_DEFINED__

    typedef IGlobalInterfaceTable *LPGLOBALINTERFACETABLE;

    EXTERN_C const IID IID_IGlobalInterfaceTable;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000146-0000-0000-C000-000000000046")
     IGlobalInterfaceTable:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE RegisterInterfaceInGlobal(IUnknown * pUnk, REFIID riid, DWORD * pdwCookie) = 0;

        virtual HRESULT STDMETHODCALLTYPE RevokeInterfaceFromGlobal(DWORD dwCookie) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetInterfaceFromGlobal(DWORD dwCookie, REFIID riid, void **ppv) = 0;

    };

#else

    typedef struct IGlobalInterfaceTableVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IGlobalInterfaceTable * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IGlobalInterfaceTable * This);

         ULONG(STDMETHODCALLTYPE * Release) (IGlobalInterfaceTable * This);

         HRESULT(STDMETHODCALLTYPE * RegisterInterfaceInGlobal) (IGlobalInterfaceTable * This, IUnknown * pUnk, REFIID riid, DWORD * pdwCookie);

         HRESULT(STDMETHODCALLTYPE * RevokeInterfaceFromGlobal) (IGlobalInterfaceTable * This, DWORD dwCookie);

         HRESULT(STDMETHODCALLTYPE * GetInterfaceFromGlobal) (IGlobalInterfaceTable * This, DWORD dwCookie, REFIID riid, void **ppv);

     END_INTERFACE} IGlobalInterfaceTableVtbl;

    interface IGlobalInterfaceTable {
        CONST_VTBL struct IGlobalInterfaceTableVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IGlobalInterfaceTable_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IGlobalInterfaceTable_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IGlobalInterfaceTable_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IGlobalInterfaceTable_RegisterInterfaceInGlobal(This,pUnk,riid,pdwCookie)  \
    (This)->lpVtbl -> RegisterInterfaceInGlobal(This,pUnk,riid,pdwCookie)

#define IGlobalInterfaceTable_RevokeInterfaceFromGlobal(This,dwCookie)  \
    (This)->lpVtbl -> RevokeInterfaceFromGlobal(This,dwCookie)

#define IGlobalInterfaceTable_GetInterfaceFromGlobal(This,dwCookie,riid,ppv)  \
    (This)->lpVtbl -> GetInterfaceFromGlobal(This,dwCookie,riid,ppv)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IGlobalInterfaceTable_RegisterInterfaceInGlobal_Proxy(IGlobalInterfaceTable * This, IUnknown * pUnk, REFIID riid, DWORD * pdwCookie);

    void __RPC_STUB IGlobalInterfaceTable_RegisterInterfaceInGlobal_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IGlobalInterfaceTable_RevokeInterfaceFromGlobal_Proxy(IGlobalInterfaceTable * This, DWORD dwCookie);

    void __RPC_STUB IGlobalInterfaceTable_RevokeInterfaceFromGlobal_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IGlobalInterfaceTable_GetInterfaceFromGlobal_Proxy(IGlobalInterfaceTable * This, DWORD dwCookie, REFIID riid, void **ppv);

    void __RPC_STUB IGlobalInterfaceTable_GetInterfaceFromGlobal_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IGlobalInterfaceTable_INTERFACE_DEFINED__ */

#ifndef __IDirectWriterLock_INTERFACE_DEFINED__
#define __IDirectWriterLock_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IDirectWriterLock;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0e6d4d92-6738-11cf-9608-00aa00680db4")
     IDirectWriterLock:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE WaitForWriteAccess(DWORD dwTimeout) = 0;

        virtual HRESULT STDMETHODCALLTYPE ReleaseWriteAccess(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE HaveWriteAccess(void) = 0;

    };

#else

    typedef struct IDirectWriterLockVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IDirectWriterLock * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IDirectWriterLock * This);

         ULONG(STDMETHODCALLTYPE * Release) (IDirectWriterLock * This);

         HRESULT(STDMETHODCALLTYPE * WaitForWriteAccess) (IDirectWriterLock * This, DWORD dwTimeout);

         HRESULT(STDMETHODCALLTYPE * ReleaseWriteAccess) (IDirectWriterLock * This);

         HRESULT(STDMETHODCALLTYPE * HaveWriteAccess) (IDirectWriterLock * This);

     END_INTERFACE} IDirectWriterLockVtbl;

    interface IDirectWriterLock {
        CONST_VTBL struct IDirectWriterLockVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IDirectWriterLock_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IDirectWriterLock_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IDirectWriterLock_Release(This) \
    (This)->lpVtbl -> Release(This)

#define IDirectWriterLock_WaitForWriteAccess(This,dwTimeout)  \
    (This)->lpVtbl -> WaitForWriteAccess(This,dwTimeout)

#define IDirectWriterLock_ReleaseWriteAccess(This)  \
    (This)->lpVtbl -> ReleaseWriteAccess(This)

#define IDirectWriterLock_HaveWriteAccess(This) \
    (This)->lpVtbl -> HaveWriteAccess(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IDirectWriterLock_WaitForWriteAccess_Proxy(IDirectWriterLock * This, DWORD dwTimeout);

    void __RPC_STUB IDirectWriterLock_WaitForWriteAccess_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDirectWriterLock_ReleaseWriteAccess_Proxy(IDirectWriterLock * This);

    void __RPC_STUB IDirectWriterLock_ReleaseWriteAccess_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IDirectWriterLock_HaveWriteAccess_Proxy(IDirectWriterLock * This);

    void __RPC_STUB IDirectWriterLock_HaveWriteAccess_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IDirectWriterLock_INTERFACE_DEFINED__ */

#ifndef __ISynchronize_INTERFACE_DEFINED__
#define __ISynchronize_INTERFACE_DEFINED__

    EXTERN_C const IID IID_ISynchronize;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000030-0000-0000-C000-000000000046")
     ISynchronize:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Wait(DWORD dwFlags, DWORD dwMilliseconds) = 0;

        virtual HRESULT STDMETHODCALLTYPE Signal(void) = 0;

        virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;

    };

#else

    typedef struct ISynchronizeVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ISynchronize * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ISynchronize * This);

         ULONG(STDMETHODCALLTYPE * Release) (ISynchronize * This);

         HRESULT(STDMETHODCALLTYPE * Wait) (ISynchronize * This, DWORD dwFlags, DWORD dwMilliseconds);

         HRESULT(STDMETHODCALLTYPE * Signal) (ISynchronize * This);

         HRESULT(STDMETHODCALLTYPE * Reset) (ISynchronize * This);

     END_INTERFACE} ISynchronizeVtbl;

    interface ISynchronize {
        CONST_VTBL struct ISynchronizeVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ISynchronize_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ISynchronize_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define ISynchronize_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define ISynchronize_Wait(This,dwFlags,dwMilliseconds)  \
    (This)->lpVtbl -> Wait(This,dwFlags,dwMilliseconds)

#define ISynchronize_Signal(This)  \
    (This)->lpVtbl -> Signal(This)

#define ISynchronize_Reset(This)  \
    (This)->lpVtbl -> Reset(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ISynchronize_Wait_Proxy(ISynchronize * This, DWORD dwFlags, DWORD dwMilliseconds);

    void __RPC_STUB ISynchronize_Wait_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ISynchronize_Signal_Proxy(ISynchronize * This);

    void __RPC_STUB ISynchronize_Signal_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ISynchronize_Reset_Proxy(ISynchronize * This);

    void __RPC_STUB ISynchronize_Reset_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ISynchronize_INTERFACE_DEFINED__ */

#ifndef __ISynchronizeHandle_INTERFACE_DEFINED__
#define __ISynchronizeHandle_INTERFACE_DEFINED__

    EXTERN_C const IID IID_ISynchronizeHandle;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000031-0000-0000-C000-000000000046")
     ISynchronizeHandle:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetHandle(HANDLE * ph) = 0;

    };

#else

    typedef struct ISynchronizeHandleVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ISynchronizeHandle * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ISynchronizeHandle * This);

         ULONG(STDMETHODCALLTYPE * Release) (ISynchronizeHandle * This);

         HRESULT(STDMETHODCALLTYPE * GetHandle) (ISynchronizeHandle * This, HANDLE * ph);

     END_INTERFACE} ISynchronizeHandleVtbl;

    interface ISynchronizeHandle {
        CONST_VTBL struct ISynchronizeHandleVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ISynchronizeHandle_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ISynchronizeHandle_AddRef(This) \
    (This)->lpVtbl -> AddRef(This)

#define ISynchronizeHandle_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define ISynchronizeHandle_GetHandle(This,ph)  \
    (This)->lpVtbl -> GetHandle(This,ph)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ISynchronizeHandle_GetHandle_Proxy(ISynchronizeHandle * This, HANDLE * ph);

    void __RPC_STUB ISynchronizeHandle_GetHandle_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ISynchronizeHandle_INTERFACE_DEFINED__ */

#ifndef __ISynchronizeEvent_INTERFACE_DEFINED__
#define __ISynchronizeEvent_INTERFACE_DEFINED__

    EXTERN_C const IID IID_ISynchronizeEvent;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000032-0000-0000-C000-000000000046")
     ISynchronizeEvent:public ISynchronizeHandle {
      public:
        virtual HRESULT STDMETHODCALLTYPE SetEventHandle(HANDLE * ph) = 0;

    };

#else

    typedef struct ISynchronizeEventVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ISynchronizeEvent * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ISynchronizeEvent * This);

         ULONG(STDMETHODCALLTYPE * Release) (ISynchronizeEvent * This);

         HRESULT(STDMETHODCALLTYPE * GetHandle) (ISynchronizeEvent * This, HANDLE * ph);

         HRESULT(STDMETHODCALLTYPE * SetEventHandle) (ISynchronizeEvent * This, HANDLE * ph);

     END_INTERFACE} ISynchronizeEventVtbl;

    interface ISynchronizeEvent {
        CONST_VTBL struct ISynchronizeEventVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ISynchronizeEvent_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ISynchronizeEvent_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define ISynchronizeEvent_Release(This) \
    (This)->lpVtbl -> Release(This)

#define ISynchronizeEvent_GetHandle(This,ph)  \
    (This)->lpVtbl -> GetHandle(This,ph)

#define ISynchronizeEvent_SetEventHandle(This,ph)  \
    (This)->lpVtbl -> SetEventHandle(This,ph)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ISynchronizeEvent_SetEventHandle_Proxy(ISynchronizeEvent * This, HANDLE * ph);

    void __RPC_STUB ISynchronizeEvent_SetEventHandle_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ISynchronizeEvent_INTERFACE_DEFINED__ */

#ifndef __ISynchronizeContainer_INTERFACE_DEFINED__
#define __ISynchronizeContainer_INTERFACE_DEFINED__

    EXTERN_C const IID IID_ISynchronizeContainer;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000033-0000-0000-C000-000000000046")
     ISynchronizeContainer:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE AddSynchronize(ISynchronize * pSync) = 0;

        virtual HRESULT STDMETHODCALLTYPE WaitMultiple(DWORD dwFlags, DWORD dwTimeOut, ISynchronize * *ppSync) = 0;

    };

#else

    typedef struct ISynchronizeContainerVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ISynchronizeContainer * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ISynchronizeContainer * This);

         ULONG(STDMETHODCALLTYPE * Release) (ISynchronizeContainer * This);

         HRESULT(STDMETHODCALLTYPE * AddSynchronize) (ISynchronizeContainer * This, ISynchronize * pSync);

         HRESULT(STDMETHODCALLTYPE * WaitMultiple) (ISynchronizeContainer * This, DWORD dwFlags, DWORD dwTimeOut, ISynchronize * *ppSync);

     END_INTERFACE} ISynchronizeContainerVtbl;

    interface ISynchronizeContainer {
        CONST_VTBL struct ISynchronizeContainerVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ISynchronizeContainer_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ISynchronizeContainer_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define ISynchronizeContainer_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define ISynchronizeContainer_AddSynchronize(This,pSync)  \
    (This)->lpVtbl -> AddSynchronize(This,pSync)

#define ISynchronizeContainer_WaitMultiple(This,dwFlags,dwTimeOut,ppSync)  \
    (This)->lpVtbl -> WaitMultiple(This,dwFlags,dwTimeOut,ppSync)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ISynchronizeContainer_AddSynchronize_Proxy(ISynchronizeContainer * This, ISynchronize * pSync);

    void __RPC_STUB ISynchronizeContainer_AddSynchronize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ISynchronizeContainer_WaitMultiple_Proxy(ISynchronizeContainer * This, DWORD dwFlags, DWORD dwTimeOut, ISynchronize * *ppSync);

    void __RPC_STUB ISynchronizeContainer_WaitMultiple_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ISynchronizeContainer_INTERFACE_DEFINED__ */

#ifndef __ISynchronizeMutex_INTERFACE_DEFINED__
#define __ISynchronizeMutex_INTERFACE_DEFINED__

    EXTERN_C const IID IID_ISynchronizeMutex;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000025-0000-0000-C000-000000000046")
     ISynchronizeMutex:public ISynchronize {
      public:
        virtual HRESULT STDMETHODCALLTYPE ReleaseMutex(void) = 0;

    };

#else

    typedef struct ISynchronizeMutexVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ISynchronizeMutex * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ISynchronizeMutex * This);

         ULONG(STDMETHODCALLTYPE * Release) (ISynchronizeMutex * This);

         HRESULT(STDMETHODCALLTYPE * Wait) (ISynchronizeMutex * This, DWORD dwFlags, DWORD dwMilliseconds);

         HRESULT(STDMETHODCALLTYPE * Signal) (ISynchronizeMutex * This);

         HRESULT(STDMETHODCALLTYPE * Reset) (ISynchronizeMutex * This);

         HRESULT(STDMETHODCALLTYPE * ReleaseMutex) (ISynchronizeMutex * This);

     END_INTERFACE} ISynchronizeMutexVtbl;

    interface ISynchronizeMutex {
        CONST_VTBL struct ISynchronizeMutexVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ISynchronizeMutex_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ISynchronizeMutex_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define ISynchronizeMutex_Release(This) \
    (This)->lpVtbl -> Release(This)

#define ISynchronizeMutex_Wait(This,dwFlags,dwMilliseconds)  \
    (This)->lpVtbl -> Wait(This,dwFlags,dwMilliseconds)

#define ISynchronizeMutex_Signal(This)  \
    (This)->lpVtbl -> Signal(This)

#define ISynchronizeMutex_Reset(This)  \
    (This)->lpVtbl -> Reset(This)

#define ISynchronizeMutex_ReleaseMutex(This)  \
    (This)->lpVtbl -> ReleaseMutex(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ISynchronizeMutex_ReleaseMutex_Proxy(ISynchronizeMutex * This);

    void __RPC_STUB ISynchronizeMutex_ReleaseMutex_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ISynchronizeMutex_INTERFACE_DEFINED__ */

#ifndef __ICancelMethodCalls_INTERFACE_DEFINED__
#define __ICancelMethodCalls_INTERFACE_DEFINED__

    typedef ICancelMethodCalls *LPCANCELMETHODCALLS;

    EXTERN_C const IID IID_ICancelMethodCalls;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000029-0000-0000-C000-000000000046")
     ICancelMethodCalls:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Cancel(ULONG ulSeconds) = 0;

        virtual HRESULT STDMETHODCALLTYPE TestCancel(void) = 0;

    };

#else

    typedef struct ICancelMethodCallsVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ICancelMethodCalls * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ICancelMethodCalls * This);

         ULONG(STDMETHODCALLTYPE * Release) (ICancelMethodCalls * This);

         HRESULT(STDMETHODCALLTYPE * Cancel) (ICancelMethodCalls * This, ULONG ulSeconds);

         HRESULT(STDMETHODCALLTYPE * TestCancel) (ICancelMethodCalls * This);

     END_INTERFACE} ICancelMethodCallsVtbl;

    interface ICancelMethodCalls {
        CONST_VTBL struct ICancelMethodCallsVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ICancelMethodCalls_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ICancelMethodCalls_AddRef(This) \
    (This)->lpVtbl -> AddRef(This)

#define ICancelMethodCalls_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define ICancelMethodCalls_Cancel(This,ulSeconds)  \
    (This)->lpVtbl -> Cancel(This,ulSeconds)

#define ICancelMethodCalls_TestCancel(This)  \
    (This)->lpVtbl -> TestCancel(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ICancelMethodCalls_Cancel_Proxy(ICancelMethodCalls * This, ULONG ulSeconds);

    void __RPC_STUB ICancelMethodCalls_Cancel_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE ICancelMethodCalls_TestCancel_Proxy(ICancelMethodCalls * This);

    void __RPC_STUB ICancelMethodCalls_TestCancel_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ICancelMethodCalls_INTERFACE_DEFINED__ */

#ifndef __IAsyncManager_INTERFACE_DEFINED__
#define __IAsyncManager_INTERFACE_DEFINED__

    typedef
        enum tagDCOM_CALL_STATE { DCOM_NONE = 0,
        DCOM_CALL_COMPLETE = 0x1,
        DCOM_CALL_CANCELED = 0x2
    } DCOM_CALL_STATE;

    EXTERN_C const IID IID_IAsyncManager;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000002A-0000-0000-C000-000000000046")
     IAsyncManager:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE CompleteCall(HRESULT Result) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetCallContext(REFIID riid, void **pInterface) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetState(ULONG * pulStateFlags) = 0;

    };

#else

    typedef struct IAsyncManagerVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IAsyncManager * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IAsyncManager * This);

         ULONG(STDMETHODCALLTYPE * Release) (IAsyncManager * This);

         HRESULT(STDMETHODCALLTYPE * CompleteCall) (IAsyncManager * This, HRESULT Result);

         HRESULT(STDMETHODCALLTYPE * GetCallContext) (IAsyncManager * This, REFIID riid, void **pInterface);

         HRESULT(STDMETHODCALLTYPE * GetState) (IAsyncManager * This, ULONG * pulStateFlags);

     END_INTERFACE} IAsyncManagerVtbl;

    interface IAsyncManager {
        CONST_VTBL struct IAsyncManagerVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IAsyncManager_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IAsyncManager_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IAsyncManager_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IAsyncManager_CompleteCall(This,Result) \
    (This)->lpVtbl -> CompleteCall(This,Result)

#define IAsyncManager_GetCallContext(This,riid,pInterface)  \
    (This)->lpVtbl -> GetCallContext(This,riid,pInterface)

#define IAsyncManager_GetState(This,pulStateFlags)  \
    (This)->lpVtbl -> GetState(This,pulStateFlags)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IAsyncManager_CompleteCall_Proxy(IAsyncManager * This, HRESULT Result);

    void __RPC_STUB IAsyncManager_CompleteCall_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IAsyncManager_GetCallContext_Proxy(IAsyncManager * This, REFIID riid, void **pInterface);

    void __RPC_STUB IAsyncManager_GetCallContext_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IAsyncManager_GetState_Proxy(IAsyncManager * This, ULONG * pulStateFlags);

    void __RPC_STUB IAsyncManager_GetState_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IAsyncManager_INTERFACE_DEFINED__ */

#ifndef __ICallFactory_INTERFACE_DEFINED__
#define __ICallFactory_INTERFACE_DEFINED__

    EXTERN_C const IID IID_ICallFactory;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("1c733a30-2a1c-11ce-ade5-00aa0044773d")
     ICallFactory:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE CreateCall(REFIID riid, IUnknown * pCtrlUnk, REFIID riid2, IUnknown * *ppv) = 0;

    };

#else

    typedef struct ICallFactoryVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (ICallFactory * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (ICallFactory * This);

         ULONG(STDMETHODCALLTYPE * Release) (ICallFactory * This);

         HRESULT(STDMETHODCALLTYPE * CreateCall) (ICallFactory * This, REFIID riid, IUnknown * pCtrlUnk, REFIID riid2, IUnknown * *ppv);

     END_INTERFACE} ICallFactoryVtbl;

    interface ICallFactory {
        CONST_VTBL struct ICallFactoryVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define ICallFactory_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define ICallFactory_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define ICallFactory_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define ICallFactory_CreateCall(This,riid,pCtrlUnk,riid2,ppv)  \
    (This)->lpVtbl -> CreateCall(This,riid,pCtrlUnk,riid2,ppv)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE ICallFactory_CreateCall_Proxy(ICallFactory * This, REFIID riid, IUnknown * pCtrlUnk, REFIID riid2, IUnknown * *ppv);

    void __RPC_STUB ICallFactory_CreateCall_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __ICallFactory_INTERFACE_DEFINED__ */

#ifndef __IRpcHelper_INTERFACE_DEFINED__
#define __IRpcHelper_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IRpcHelper;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000149-0000-0000-C000-000000000046")
     IRpcHelper:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE GetDCOMProtocolVersion(DWORD * pComVersion) = 0;

        virtual HRESULT STDMETHODCALLTYPE GetIIDFromOBJREF(void *pObjRef, IID * *piid) = 0;

    };

#else

    typedef struct IRpcHelperVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IRpcHelper * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IRpcHelper * This);

         ULONG(STDMETHODCALLTYPE * Release) (IRpcHelper * This);

         HRESULT(STDMETHODCALLTYPE * GetDCOMProtocolVersion) (IRpcHelper * This, DWORD * pComVersion);

         HRESULT(STDMETHODCALLTYPE * GetIIDFromOBJREF) (IRpcHelper * This, void *pObjRef, IID * *piid);

     END_INTERFACE} IRpcHelperVtbl;

    interface IRpcHelper {
        CONST_VTBL struct IRpcHelperVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IRpcHelper_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IRpcHelper_AddRef(This) \
    (This)->lpVtbl -> AddRef(This)

#define IRpcHelper_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IRpcHelper_GetDCOMProtocolVersion(This,pComVersion)  \
    (This)->lpVtbl -> GetDCOMProtocolVersion(This,pComVersion)

#define IRpcHelper_GetIIDFromOBJREF(This,pObjRef,piid)  \
    (This)->lpVtbl -> GetIIDFromOBJREF(This,pObjRef,piid)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IRpcHelper_GetDCOMProtocolVersion_Proxy(IRpcHelper * This, DWORD * pComVersion);

    void __RPC_STUB IRpcHelper_GetDCOMProtocolVersion_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IRpcHelper_GetIIDFromOBJREF_Proxy(IRpcHelper * This, void *pObjRef, IID * *piid);

    void __RPC_STUB IRpcHelper_GetIIDFromOBJREF_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IRpcHelper_INTERFACE_DEFINED__ */

#ifndef __IReleaseMarshalBuffers_INTERFACE_DEFINED__
#define __IReleaseMarshalBuffers_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IReleaseMarshalBuffers;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("eb0cb9e8-7996-11d2-872e-0000f8080859")
     IReleaseMarshalBuffers:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE ReleaseMarshalBuffer(RPCOLEMESSAGE * pMsg, DWORD dwFlags, IUnknown * pChnl) = 0;

    };

#else

    typedef struct IReleaseMarshalBuffersVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IReleaseMarshalBuffers * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IReleaseMarshalBuffers * This);

         ULONG(STDMETHODCALLTYPE * Release) (IReleaseMarshalBuffers * This);

         HRESULT(STDMETHODCALLTYPE * ReleaseMarshalBuffer) (IReleaseMarshalBuffers * This, RPCOLEMESSAGE * pMsg, DWORD dwFlags, IUnknown * pChnl);

     END_INTERFACE} IReleaseMarshalBuffersVtbl;

    interface IReleaseMarshalBuffers {
        CONST_VTBL struct IReleaseMarshalBuffersVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IReleaseMarshalBuffers_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IReleaseMarshalBuffers_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IReleaseMarshalBuffers_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IReleaseMarshalBuffers_ReleaseMarshalBuffer(This,pMsg,dwFlags,pChnl)  \
    (This)->lpVtbl -> ReleaseMarshalBuffer(This,pMsg,dwFlags,pChnl)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IReleaseMarshalBuffers_ReleaseMarshalBuffer_Proxy(IReleaseMarshalBuffers * This, RPCOLEMESSAGE * pMsg, DWORD dwFlags, IUnknown * pChnl);

    void __RPC_STUB IReleaseMarshalBuffers_ReleaseMarshalBuffer_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IReleaseMarshalBuffers_INTERFACE_DEFINED__ */

#ifndef __IWaitMultiple_INTERFACE_DEFINED__
#define __IWaitMultiple_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IWaitMultiple;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("0000002B-0000-0000-C000-000000000046")
     IWaitMultiple:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE WaitMultiple(DWORD timeout, ISynchronize * *pSync) = 0;

        virtual HRESULT STDMETHODCALLTYPE AddSynchronize(ISynchronize * pSync) = 0;

    };

#else

    typedef struct IWaitMultipleVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IWaitMultiple * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IWaitMultiple * This);

         ULONG(STDMETHODCALLTYPE * Release) (IWaitMultiple * This);

         HRESULT(STDMETHODCALLTYPE * WaitMultiple) (IWaitMultiple * This, DWORD timeout, ISynchronize * *pSync);

         HRESULT(STDMETHODCALLTYPE * AddSynchronize) (IWaitMultiple * This, ISynchronize * pSync);

     END_INTERFACE} IWaitMultipleVtbl;

    interface IWaitMultiple {
        CONST_VTBL struct IWaitMultipleVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IWaitMultiple_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IWaitMultiple_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IWaitMultiple_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IWaitMultiple_WaitMultiple(This,timeout,pSync)  \
    (This)->lpVtbl -> WaitMultiple(This,timeout,pSync)

#define IWaitMultiple_AddSynchronize(This,pSync)  \
    (This)->lpVtbl -> AddSynchronize(This,pSync)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IWaitMultiple_WaitMultiple_Proxy(IWaitMultiple * This, DWORD timeout, ISynchronize * *pSync);

    void __RPC_STUB IWaitMultiple_WaitMultiple_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IWaitMultiple_AddSynchronize_Proxy(IWaitMultiple * This, ISynchronize * pSync);

    void __RPC_STUB IWaitMultiple_AddSynchronize_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IWaitMultiple_INTERFACE_DEFINED__ */

#ifndef __IUrlMon_INTERFACE_DEFINED__
#define __IUrlMon_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IUrlMon;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000026-0000-0000-C000-000000000046")
     IUrlMon:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE AsyncGetClassBits(REFCLSID rclsid, LPCWSTR pszTYPE, LPCWSTR pszExt, DWORD dwFileVersionMS, DWORD dwFileVersionLS, LPCWSTR pszCodeBase, IBindCtx * pbc, DWORD dwClassContext, REFIID riid, DWORD flags) = 0;

    };

#else

    typedef struct IUrlMonVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IUrlMon * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IUrlMon * This);

         ULONG(STDMETHODCALLTYPE * Release) (IUrlMon * This);

         HRESULT(STDMETHODCALLTYPE * AsyncGetClassBits) (IUrlMon * This, REFCLSID rclsid, LPCWSTR pszTYPE, LPCWSTR pszExt, DWORD dwFileVersionMS, DWORD dwFileVersionLS, LPCWSTR pszCodeBase, IBindCtx * pbc, DWORD dwClassContext, REFIID riid, DWORD flags);

     END_INTERFACE} IUrlMonVtbl;

    interface IUrlMon {
        CONST_VTBL struct IUrlMonVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IUrlMon_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IUrlMon_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IUrlMon_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IUrlMon_AsyncGetClassBits(This,rclsid,pszTYPE,pszExt,dwFileVersionMS,dwFileVersionLS,pszCodeBase,pbc,dwClassContext,riid,flags) \
    (This)->lpVtbl -> AsyncGetClassBits(This,rclsid,pszTYPE,pszExt,dwFileVersionMS,dwFileVersionLS,pszCodeBase,pbc,dwClassContext,riid,flags)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IUrlMon_AsyncGetClassBits_Proxy(IUrlMon * This, REFCLSID rclsid, LPCWSTR pszTYPE, LPCWSTR pszExt, DWORD dwFileVersionMS, DWORD dwFileVersionLS, LPCWSTR pszCodeBase, IBindCtx * pbc, DWORD dwClassContext, REFIID riid, DWORD flags);

    void __RPC_STUB IUrlMon_AsyncGetClassBits_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IUrlMon_INTERFACE_DEFINED__ */

#ifndef __IForegroundTransfer_INTERFACE_DEFINED__
#define __IForegroundTransfer_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IForegroundTransfer;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("00000145-0000-0000-C000-000000000046")
     IForegroundTransfer:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE AllowForegroundTransfer(void *lpvReserved) = 0;

    };

#else

    typedef struct IForegroundTransferVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IForegroundTransfer * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IForegroundTransfer * This);

         ULONG(STDMETHODCALLTYPE * Release) (IForegroundTransfer * This);

         HRESULT(STDMETHODCALLTYPE * AllowForegroundTransfer) (IForegroundTransfer * This, void *lpvReserved);

     END_INTERFACE} IForegroundTransferVtbl;

    interface IForegroundTransfer {
        CONST_VTBL struct IForegroundTransferVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IForegroundTransfer_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IForegroundTransfer_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IForegroundTransfer_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IForegroundTransfer_AllowForegroundTransfer(This,lpvReserved)  \
    (This)->lpVtbl -> AllowForegroundTransfer(This,lpvReserved)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IForegroundTransfer_AllowForegroundTransfer_Proxy(IForegroundTransfer * This, void *lpvReserved);

    void __RPC_STUB IForegroundTransfer_AllowForegroundTransfer_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IForegroundTransfer_INTERFACE_DEFINED__ */

#ifndef __IPipeByte_INTERFACE_DEFINED__
#define __IPipeByte_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IPipeByte;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("DB2F3ACA-2F86-11d1-8E04-00C04FB9989A")
     IPipeByte:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Pull(BYTE * buf, ULONG cRequest, ULONG * pcReturned) = 0;

        virtual HRESULT STDMETHODCALLTYPE Push(BYTE * buf, ULONG cSent) = 0;

    };

#else

    typedef struct IPipeByteVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IPipeByte * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IPipeByte * This);

         ULONG(STDMETHODCALLTYPE * Release) (IPipeByte * This);

         HRESULT(STDMETHODCALLTYPE * Pull) (IPipeByte * This, BYTE * buf, ULONG cRequest, ULONG * pcReturned);

         HRESULT(STDMETHODCALLTYPE * Push) (IPipeByte * This, BYTE * buf, ULONG cSent);

     END_INTERFACE} IPipeByteVtbl;

    interface IPipeByte {
        CONST_VTBL struct IPipeByteVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IPipeByte_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IPipeByte_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IPipeByte_Release(This) \
    (This)->lpVtbl -> Release(This)

#define IPipeByte_Pull(This,buf,cRequest,pcReturned)  \
    (This)->lpVtbl -> Pull(This,buf,cRequest,pcReturned)

#define IPipeByte_Push(This,buf,cSent)  \
    (This)->lpVtbl -> Push(This,buf,cSent)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IPipeByte_Pull_Proxy(IPipeByte * This, BYTE * buf, ULONG cRequest, ULONG * pcReturned);

    void __RPC_STUB IPipeByte_Pull_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPipeByte_Push_Proxy(IPipeByte * This, BYTE * buf, ULONG cSent);

    void __RPC_STUB IPipeByte_Push_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IPipeByte_INTERFACE_DEFINED__ */

#ifndef __AsyncIPipeByte_INTERFACE_DEFINED__
#define __AsyncIPipeByte_INTERFACE_DEFINED__

    EXTERN_C const IID IID_AsyncIPipeByte;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("DB2F3ACB-2F86-11d1-8E04-00C04FB9989A")
     AsyncIPipeByte:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Begin_Pull(ULONG cRequest) = 0;

        virtual HRESULT STDMETHODCALLTYPE Finish_Pull(BYTE * buf, ULONG * pcReturned) = 0;

        virtual HRESULT STDMETHODCALLTYPE Begin_Push(BYTE * buf, ULONG cSent) = 0;

        virtual HRESULT STDMETHODCALLTYPE Finish_Push(void) = 0;

    };

#else

    typedef struct AsyncIPipeByteVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (AsyncIPipeByte * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (AsyncIPipeByte * This);

         ULONG(STDMETHODCALLTYPE * Release) (AsyncIPipeByte * This);

         HRESULT(STDMETHODCALLTYPE * Begin_Pull) (AsyncIPipeByte * This, ULONG cRequest);

         HRESULT(STDMETHODCALLTYPE * Finish_Pull) (AsyncIPipeByte * This, BYTE * buf, ULONG * pcReturned);

         HRESULT(STDMETHODCALLTYPE * Begin_Push) (AsyncIPipeByte * This, BYTE * buf, ULONG cSent);

         HRESULT(STDMETHODCALLTYPE * Finish_Push) (AsyncIPipeByte * This);

     END_INTERFACE} AsyncIPipeByteVtbl;

    interface AsyncIPipeByte {
        CONST_VTBL struct AsyncIPipeByteVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define AsyncIPipeByte_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define AsyncIPipeByte_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define AsyncIPipeByte_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define AsyncIPipeByte_Begin_Pull(This,cRequest)  \
    (This)->lpVtbl -> Begin_Pull(This,cRequest)

#define AsyncIPipeByte_Finish_Pull(This,buf,pcReturned) \
    (This)->lpVtbl -> Finish_Pull(This,buf,pcReturned)

#define AsyncIPipeByte_Begin_Push(This,buf,cSent)  \
    (This)->lpVtbl -> Begin_Push(This,buf,cSent)

#define AsyncIPipeByte_Finish_Push(This)  \
    (This)->lpVtbl -> Finish_Push(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE AsyncIPipeByte_Begin_Pull_Proxy(AsyncIPipeByte * This, ULONG cRequest);

    void __RPC_STUB AsyncIPipeByte_Begin_Pull_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIPipeByte_Finish_Pull_Proxy(AsyncIPipeByte * This, BYTE * buf, ULONG * pcReturned);

    void __RPC_STUB AsyncIPipeByte_Finish_Pull_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIPipeByte_Begin_Push_Proxy(AsyncIPipeByte * This, BYTE * buf, ULONG cSent);

    void __RPC_STUB AsyncIPipeByte_Begin_Push_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIPipeByte_Finish_Push_Proxy(AsyncIPipeByte * This);

    void __RPC_STUB AsyncIPipeByte_Finish_Push_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __AsyncIPipeByte_INTERFACE_DEFINED__ */

#ifndef __IPipeLong_INTERFACE_DEFINED__
#define __IPipeLong_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IPipeLong;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("DB2F3ACC-2F86-11d1-8E04-00C04FB9989A")
     IPipeLong:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Pull(LONG * buf, ULONG cRequest, ULONG * pcReturned) = 0;

        virtual HRESULT STDMETHODCALLTYPE Push(LONG * buf, ULONG cSent) = 0;

    };

#else

    typedef struct IPipeLongVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IPipeLong * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IPipeLong * This);

         ULONG(STDMETHODCALLTYPE * Release) (IPipeLong * This);

         HRESULT(STDMETHODCALLTYPE * Pull) (IPipeLong * This, LONG * buf, ULONG cRequest, ULONG * pcReturned);

         HRESULT(STDMETHODCALLTYPE * Push) (IPipeLong * This, LONG * buf, ULONG cSent);

     END_INTERFACE} IPipeLongVtbl;

    interface IPipeLong {
        CONST_VTBL struct IPipeLongVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IPipeLong_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IPipeLong_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IPipeLong_Release(This) \
    (This)->lpVtbl -> Release(This)

#define IPipeLong_Pull(This,buf,cRequest,pcReturned)  \
    (This)->lpVtbl -> Pull(This,buf,cRequest,pcReturned)

#define IPipeLong_Push(This,buf,cSent)  \
    (This)->lpVtbl -> Push(This,buf,cSent)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IPipeLong_Pull_Proxy(IPipeLong * This, LONG * buf, ULONG cRequest, ULONG * pcReturned);

    void __RPC_STUB IPipeLong_Pull_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPipeLong_Push_Proxy(IPipeLong * This, LONG * buf, ULONG cSent);

    void __RPC_STUB IPipeLong_Push_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IPipeLong_INTERFACE_DEFINED__ */

#ifndef __AsyncIPipeLong_INTERFACE_DEFINED__
#define __AsyncIPipeLong_INTERFACE_DEFINED__

    EXTERN_C const IID IID_AsyncIPipeLong;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("DB2F3ACD-2F86-11d1-8E04-00C04FB9989A")
     AsyncIPipeLong:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Begin_Pull(ULONG cRequest) = 0;

        virtual HRESULT STDMETHODCALLTYPE Finish_Pull(LONG * buf, ULONG * pcReturned) = 0;

        virtual HRESULT STDMETHODCALLTYPE Begin_Push(LONG * buf, ULONG cSent) = 0;

        virtual HRESULT STDMETHODCALLTYPE Finish_Push(void) = 0;

    };

#else

    typedef struct AsyncIPipeLongVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (AsyncIPipeLong * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (AsyncIPipeLong * This);

         ULONG(STDMETHODCALLTYPE * Release) (AsyncIPipeLong * This);

         HRESULT(STDMETHODCALLTYPE * Begin_Pull) (AsyncIPipeLong * This, ULONG cRequest);

         HRESULT(STDMETHODCALLTYPE * Finish_Pull) (AsyncIPipeLong * This, LONG * buf, ULONG * pcReturned);

         HRESULT(STDMETHODCALLTYPE * Begin_Push) (AsyncIPipeLong * This, LONG * buf, ULONG cSent);

         HRESULT(STDMETHODCALLTYPE * Finish_Push) (AsyncIPipeLong * This);

     END_INTERFACE} AsyncIPipeLongVtbl;

    interface AsyncIPipeLong {
        CONST_VTBL struct AsyncIPipeLongVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define AsyncIPipeLong_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define AsyncIPipeLong_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define AsyncIPipeLong_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define AsyncIPipeLong_Begin_Pull(This,cRequest)  \
    (This)->lpVtbl -> Begin_Pull(This,cRequest)

#define AsyncIPipeLong_Finish_Pull(This,buf,pcReturned) \
    (This)->lpVtbl -> Finish_Pull(This,buf,pcReturned)

#define AsyncIPipeLong_Begin_Push(This,buf,cSent)  \
    (This)->lpVtbl -> Begin_Push(This,buf,cSent)

#define AsyncIPipeLong_Finish_Push(This)  \
    (This)->lpVtbl -> Finish_Push(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE AsyncIPipeLong_Begin_Pull_Proxy(AsyncIPipeLong * This, ULONG cRequest);

    void __RPC_STUB AsyncIPipeLong_Begin_Pull_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIPipeLong_Finish_Pull_Proxy(AsyncIPipeLong * This, LONG * buf, ULONG * pcReturned);

    void __RPC_STUB AsyncIPipeLong_Finish_Pull_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIPipeLong_Begin_Push_Proxy(AsyncIPipeLong * This, LONG * buf, ULONG cSent);

    void __RPC_STUB AsyncIPipeLong_Begin_Push_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIPipeLong_Finish_Push_Proxy(AsyncIPipeLong * This);

    void __RPC_STUB AsyncIPipeLong_Finish_Push_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __AsyncIPipeLong_INTERFACE_DEFINED__ */

#ifndef __IPipeDouble_INTERFACE_DEFINED__
#define __IPipeDouble_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IPipeDouble;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("DB2F3ACE-2F86-11d1-8E04-00C04FB9989A")
     IPipeDouble:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Pull(DOUBLE * buf, ULONG cRequest, ULONG * pcReturned) = 0;

        virtual HRESULT STDMETHODCALLTYPE Push(DOUBLE * buf, ULONG cSent) = 0;

    };

#else

    typedef struct IPipeDoubleVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IPipeDouble * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IPipeDouble * This);

         ULONG(STDMETHODCALLTYPE * Release) (IPipeDouble * This);

         HRESULT(STDMETHODCALLTYPE * Pull) (IPipeDouble * This, DOUBLE * buf, ULONG cRequest, ULONG * pcReturned);

         HRESULT(STDMETHODCALLTYPE * Push) (IPipeDouble * This, DOUBLE * buf, ULONG cSent);

     END_INTERFACE} IPipeDoubleVtbl;

    interface IPipeDouble {
        CONST_VTBL struct IPipeDoubleVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IPipeDouble_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IPipeDouble_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IPipeDouble_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IPipeDouble_Pull(This,buf,cRequest,pcReturned)  \
    (This)->lpVtbl -> Pull(This,buf,cRequest,pcReturned)

#define IPipeDouble_Push(This,buf,cSent)  \
    (This)->lpVtbl -> Push(This,buf,cSent)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IPipeDouble_Pull_Proxy(IPipeDouble * This, DOUBLE * buf, ULONG cRequest, ULONG * pcReturned);

    void __RPC_STUB IPipeDouble_Pull_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IPipeDouble_Push_Proxy(IPipeDouble * This, DOUBLE * buf, ULONG cSent);

    void __RPC_STUB IPipeDouble_Push_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IPipeDouble_INTERFACE_DEFINED__ */

#ifndef __AsyncIPipeDouble_INTERFACE_DEFINED__
#define __AsyncIPipeDouble_INTERFACE_DEFINED__

    EXTERN_C const IID IID_AsyncIPipeDouble;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("DB2F3ACF-2F86-11d1-8E04-00C04FB9989A")
     AsyncIPipeDouble:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Begin_Pull(ULONG cRequest) = 0;

        virtual HRESULT STDMETHODCALLTYPE Finish_Pull(DOUBLE * buf, ULONG * pcReturned) = 0;

        virtual HRESULT STDMETHODCALLTYPE Begin_Push(DOUBLE * buf, ULONG cSent) = 0;

        virtual HRESULT STDMETHODCALLTYPE Finish_Push(void) = 0;

    };

#else

    typedef struct AsyncIPipeDoubleVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (AsyncIPipeDouble * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (AsyncIPipeDouble * This);

         ULONG(STDMETHODCALLTYPE * Release) (AsyncIPipeDouble * This);

         HRESULT(STDMETHODCALLTYPE * Begin_Pull) (AsyncIPipeDouble * This, ULONG cRequest);

         HRESULT(STDMETHODCALLTYPE * Finish_Pull) (AsyncIPipeDouble * This, DOUBLE * buf, ULONG * pcReturned);

         HRESULT(STDMETHODCALLTYPE * Begin_Push) (AsyncIPipeDouble * This, DOUBLE * buf, ULONG cSent);

         HRESULT(STDMETHODCALLTYPE * Finish_Push) (AsyncIPipeDouble * This);

     END_INTERFACE} AsyncIPipeDoubleVtbl;

    interface AsyncIPipeDouble {
        CONST_VTBL struct AsyncIPipeDoubleVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define AsyncIPipeDouble_QueryInterface(This,riid,ppvObject)  \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define AsyncIPipeDouble_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define AsyncIPipeDouble_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define AsyncIPipeDouble_Begin_Pull(This,cRequest)  \
    (This)->lpVtbl -> Begin_Pull(This,cRequest)

#define AsyncIPipeDouble_Finish_Pull(This,buf,pcReturned)  \
    (This)->lpVtbl -> Finish_Pull(This,buf,pcReturned)

#define AsyncIPipeDouble_Begin_Push(This,buf,cSent)  \
    (This)->lpVtbl -> Begin_Push(This,buf,cSent)

#define AsyncIPipeDouble_Finish_Push(This)  \
    (This)->lpVtbl -> Finish_Push(This)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE AsyncIPipeDouble_Begin_Pull_Proxy(AsyncIPipeDouble * This, ULONG cRequest);

    void __RPC_STUB AsyncIPipeDouble_Begin_Pull_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIPipeDouble_Finish_Pull_Proxy(AsyncIPipeDouble * This, DOUBLE * buf, ULONG * pcReturned);

    void __RPC_STUB AsyncIPipeDouble_Finish_Pull_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIPipeDouble_Begin_Push_Proxy(AsyncIPipeDouble * This, DOUBLE * buf, ULONG cSent);

    void __RPC_STUB AsyncIPipeDouble_Begin_Push_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE AsyncIPipeDouble_Finish_Push_Proxy(AsyncIPipeDouble * This);

    void __RPC_STUB AsyncIPipeDouble_Finish_Push_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __AsyncIPipeDouble_INTERFACE_DEFINED__ */

#ifndef __IThumbnailExtractor_INTERFACE_DEFINED__
#define __IThumbnailExtractor_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IThumbnailExtractor;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("969dc708-5c76-11d1-8d86-0000f804b057")
     IThumbnailExtractor:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE ExtractThumbnail(IStorage * pStg, ULONG ulLength, ULONG ulHeight, ULONG * pulOutputLength, ULONG * pulOutputHeight, HBITMAP * phOutputBitmap) = 0;

        virtual HRESULT STDMETHODCALLTYPE OnFileUpdated(IStorage * pStg) = 0;

    };

#else

    typedef struct IThumbnailExtractorVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IThumbnailExtractor * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IThumbnailExtractor * This);

         ULONG(STDMETHODCALLTYPE * Release) (IThumbnailExtractor * This);

         HRESULT(STDMETHODCALLTYPE * ExtractThumbnail) (IThumbnailExtractor * This, IStorage * pStg, ULONG ulLength, ULONG ulHeight, ULONG * pulOutputLength, ULONG * pulOutputHeight, HBITMAP * phOutputBitmap);

         HRESULT(STDMETHODCALLTYPE * OnFileUpdated) (IThumbnailExtractor * This, IStorage * pStg);

     END_INTERFACE} IThumbnailExtractorVtbl;

    interface IThumbnailExtractor {
        CONST_VTBL struct IThumbnailExtractorVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IThumbnailExtractor_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IThumbnailExtractor_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IThumbnailExtractor_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IThumbnailExtractor_ExtractThumbnail(This,pStg,ulLength,ulHeight,pulOutputLength,pulOutputHeight,phOutputBitmap)  \
    (This)->lpVtbl -> ExtractThumbnail(This,pStg,ulLength,ulHeight,pulOutputLength,pulOutputHeight,phOutputBitmap)

#define IThumbnailExtractor_OnFileUpdated(This,pStg)  \
    (This)->lpVtbl -> OnFileUpdated(This,pStg)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IThumbnailExtractor_ExtractThumbnail_Proxy(IThumbnailExtractor * This, IStorage * pStg, ULONG ulLength, ULONG ulHeight, ULONG * pulOutputLength, ULONG * pulOutputHeight, HBITMAP * phOutputBitmap);

    void __RPC_STUB IThumbnailExtractor_ExtractThumbnail_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

    HRESULT STDMETHODCALLTYPE IThumbnailExtractor_OnFileUpdated_Proxy(IThumbnailExtractor * This, IStorage * pStg);

    void __RPC_STUB IThumbnailExtractor_OnFileUpdated_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IThumbnailExtractor_INTERFACE_DEFINED__ */

#ifndef __IDummyHICONIncluder_INTERFACE_DEFINED__
#define __IDummyHICONIncluder_INTERFACE_DEFINED__

    EXTERN_C const IID IID_IDummyHICONIncluder;

#if defined(__cplusplus) && !defined(CINTERFACE)

     MIDL_INTERFACE("947990de-cc28-11d2-a0f7-00805f858fb1")
     IDummyHICONIncluder:public IUnknown {
      public:
        virtual HRESULT STDMETHODCALLTYPE Dummy(HICON h1, HDC h2) = 0;

    };

#else

    typedef struct IDummyHICONIncluderVtbl {
        BEGIN_INTERFACE HRESULT(STDMETHODCALLTYPE * QueryInterface) (IDummyHICONIncluder * This, REFIID riid, void **ppvObject);

         ULONG(STDMETHODCALLTYPE * AddRef) (IDummyHICONIncluder * This);

         ULONG(STDMETHODCALLTYPE * Release) (IDummyHICONIncluder * This);

         HRESULT(STDMETHODCALLTYPE * Dummy) (IDummyHICONIncluder * This, HICON h1, HDC h2);

     END_INTERFACE} IDummyHICONIncluderVtbl;

    interface IDummyHICONIncluder {
        CONST_VTBL struct IDummyHICONIncluderVtbl *lpVtbl;
    };

#ifdef COBJMACROS

#define IDummyHICONIncluder_QueryInterface(This,riid,ppvObject) \
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IDummyHICONIncluder_AddRef(This)  \
    (This)->lpVtbl -> AddRef(This)

#define IDummyHICONIncluder_Release(This)  \
    (This)->lpVtbl -> Release(This)

#define IDummyHICONIncluder_Dummy(This,h1,h2)  \
    (This)->lpVtbl -> Dummy(This,h1,h2)

#endif /* COBJMACROS */

#endif

    HRESULT STDMETHODCALLTYPE IDummyHICONIncluder_Dummy_Proxy(IDummyHICONIncluder * This, HICON h1, HDC h2);

    void __RPC_STUB IDummyHICONIncluder_Dummy_Stub(IRpcStubBuffer * This, IRpcChannelBuffer * _pRpcChannelBuffer, PRPC_MESSAGE _pRpcMessage, DWORD * _pdwStubPhase);

#endif /* __IDummyHICONIncluder_INTERFACE_DEFINED__ */

    extern RPC_IF_HANDLE __MIDL_itf_objidl_0081_v0_0_c_ifspec;
    extern RPC_IF_HANDLE __MIDL_itf_objidl_0081_v0_0_s_ifspec;

    unsigned long __RPC_USER ASYNC_STGMEDIUM_UserSize(unsigned long *, unsigned long, ASYNC_STGMEDIUM *);
    unsigned char *__RPC_USER ASYNC_STGMEDIUM_UserMarshal(unsigned long *, unsigned char *, ASYNC_STGMEDIUM *);
    unsigned char *__RPC_USER ASYNC_STGMEDIUM_UserUnmarshal(unsigned long *, unsigned char *, ASYNC_STGMEDIUM *);
    void __RPC_USER ASYNC_STGMEDIUM_UserFree(unsigned long *, ASYNC_STGMEDIUM *);

    unsigned long __RPC_USER CLIPFORMAT_UserSize(unsigned long *, unsigned long, CLIPFORMAT *);
    unsigned char *__RPC_USER CLIPFORMAT_UserMarshal(unsigned long *, unsigned char *, CLIPFORMAT *);
    unsigned char *__RPC_USER CLIPFORMAT_UserUnmarshal(unsigned long *, unsigned char *, CLIPFORMAT *);
    void __RPC_USER CLIPFORMAT_UserFree(unsigned long *, CLIPFORMAT *);

    unsigned long __RPC_USER FLAG_STGMEDIUM_UserSize(unsigned long *, unsigned long, FLAG_STGMEDIUM *);
    unsigned char *__RPC_USER FLAG_STGMEDIUM_UserMarshal(unsigned long *, unsigned char *, FLAG_STGMEDIUM *);
    unsigned char *__RPC_USER FLAG_STGMEDIUM_UserUnmarshal(unsigned long *, unsigned char *, FLAG_STGMEDIUM *);
    void __RPC_USER FLAG_STGMEDIUM_UserFree(unsigned long *, FLAG_STGMEDIUM *);

    unsigned long __RPC_USER HBITMAP_UserSize(unsigned long *, unsigned long, HBITMAP *);
    unsigned char *__RPC_USER HBITMAP_UserMarshal(unsigned long *, unsigned char *, HBITMAP *);
    unsigned char *__RPC_USER HBITMAP_UserUnmarshal(unsigned long *, unsigned char *, HBITMAP *);
    void __RPC_USER HBITMAP_UserFree(unsigned long *, HBITMAP *);

    unsigned long __RPC_USER HDC_UserSize(unsigned long *, unsigned long, HDC *);
    unsigned char *__RPC_USER HDC_UserMarshal(unsigned long *, unsigned char *, HDC *);
    unsigned char *__RPC_USER HDC_UserUnmarshal(unsigned long *, unsigned char *, HDC *);
    void __RPC_USER HDC_UserFree(unsigned long *, HDC *);

    unsigned long __RPC_USER HICON_UserSize(unsigned long *, unsigned long, HICON *);
    unsigned char *__RPC_USER HICON_UserMarshal(unsigned long *, unsigned char *, HICON *);
    unsigned char *__RPC_USER HICON_UserUnmarshal(unsigned long *, unsigned char *, HICON *);
    void __RPC_USER HICON_UserFree(unsigned long *, HICON *);

    unsigned long __RPC_USER SNB_UserSize(unsigned long *, unsigned long, SNB *);
    unsigned char *__RPC_USER SNB_UserMarshal(unsigned long *, unsigned char *, SNB *);
    unsigned char *__RPC_USER SNB_UserUnmarshal(unsigned long *, unsigned char *, SNB *);
    void __RPC_USER SNB_UserFree(unsigned long *, SNB *);

    unsigned long __RPC_USER STGMEDIUM_UserSize(unsigned long *, unsigned long, STGMEDIUM *);
    unsigned char *__RPC_USER STGMEDIUM_UserMarshal(unsigned long *, unsigned char *, STGMEDIUM *);
    unsigned char *__RPC_USER STGMEDIUM_UserUnmarshal(unsigned long *, unsigned char *, STGMEDIUM *);
    void __RPC_USER STGMEDIUM_UserFree(unsigned long *, STGMEDIUM *);

    HRESULT STDMETHODCALLTYPE IEnumUnknown_Next_Proxy(IEnumUnknown * This, ULONG celt, IUnknown * *rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE IEnumUnknown_Next_Stub(IEnumUnknown * This, ULONG celt, IUnknown * *rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE IBindCtx_SetBindOptions_Proxy(IBindCtx * This, BIND_OPTS * pbindopts);

    HRESULT STDMETHODCALLTYPE IBindCtx_SetBindOptions_Stub(IBindCtx * This, BIND_OPTS2 * pbindopts);

    HRESULT STDMETHODCALLTYPE IBindCtx_GetBindOptions_Proxy(IBindCtx * This, BIND_OPTS * pbindopts);

    HRESULT STDMETHODCALLTYPE IBindCtx_GetBindOptions_Stub(IBindCtx * This, BIND_OPTS2 * pbindopts);

    HRESULT STDMETHODCALLTYPE IEnumMoniker_Next_Proxy(IEnumMoniker * This, ULONG celt, IMoniker * *rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE IEnumMoniker_Next_Stub(IEnumMoniker * This, ULONG celt, IMoniker * *rgelt, ULONG * pceltFetched);

    BOOL STDMETHODCALLTYPE IRunnableObject_IsRunning_Proxy(IRunnableObject * This);

    HRESULT STDMETHODCALLTYPE IRunnableObject_IsRunning_Stub(IRunnableObject * This);

    HRESULT STDMETHODCALLTYPE IMoniker_BindToObject_Proxy(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riidResult, void **ppvResult);

    HRESULT STDMETHODCALLTYPE IMoniker_BindToObject_Stub(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riidResult, IUnknown * *ppvResult);

    HRESULT STDMETHODCALLTYPE IMoniker_BindToStorage_Proxy(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riid, void **ppvObj);

    HRESULT STDMETHODCALLTYPE IMoniker_BindToStorage_Stub(IMoniker * This, IBindCtx * pbc, IMoniker * pmkToLeft, REFIID riid, IUnknown * *ppvObj);

    HRESULT STDMETHODCALLTYPE IEnumString_Next_Proxy(IEnumString * This, ULONG celt, LPOLESTR * rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE IEnumString_Next_Stub(IEnumString * This, ULONG celt, LPOLESTR * rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE ISequentialStream_Read_Proxy(ISequentialStream * This, void *pv, ULONG cb, ULONG * pcbRead);

    HRESULT STDMETHODCALLTYPE ISequentialStream_Read_Stub(ISequentialStream * This, byte * pv, ULONG cb, ULONG * pcbRead);

    HRESULT STDMETHODCALLTYPE ISequentialStream_Write_Proxy(ISequentialStream * This, const void *pv, ULONG cb, ULONG * pcbWritten);

    HRESULT STDMETHODCALLTYPE ISequentialStream_Write_Stub(ISequentialStream * This, const byte * pv, ULONG cb, ULONG * pcbWritten);

    HRESULT STDMETHODCALLTYPE IStream_Seek_Proxy(IStream * This, LARGE_INTEGER dlibMove, DWORD dwOrigin, ULARGE_INTEGER * plibNewPosition);

    HRESULT STDMETHODCALLTYPE IStream_Seek_Stub(IStream * This, LARGE_INTEGER dlibMove, DWORD dwOrigin, ULARGE_INTEGER * plibNewPosition);

    HRESULT STDMETHODCALLTYPE IStream_CopyTo_Proxy(IStream * This, IStream * pstm, ULARGE_INTEGER cb, ULARGE_INTEGER * pcbRead, ULARGE_INTEGER * pcbWritten);

    HRESULT STDMETHODCALLTYPE IStream_CopyTo_Stub(IStream * This, IStream * pstm, ULARGE_INTEGER cb, ULARGE_INTEGER * pcbRead, ULARGE_INTEGER * pcbWritten);

    HRESULT STDMETHODCALLTYPE IEnumSTATSTG_Next_Proxy(IEnumSTATSTG * This, ULONG celt, STATSTG * rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE IEnumSTATSTG_Next_Stub(IEnumSTATSTG * This, ULONG celt, STATSTG * rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE IStorage_OpenStream_Proxy(IStorage * This, const OLECHAR * pwcsName, void *reserved1, DWORD grfMode, DWORD reserved2, IStream * *ppstm);

    HRESULT STDMETHODCALLTYPE IStorage_OpenStream_Stub(IStorage * This, const OLECHAR * pwcsName, unsigned long cbReserved1, byte * reserved1, DWORD grfMode, DWORD reserved2, IStream * *ppstm);

    HRESULT STDMETHODCALLTYPE IStorage_EnumElements_Proxy(IStorage * This, DWORD reserved1, void *reserved2, DWORD reserved3, IEnumSTATSTG * *ppenum);

    HRESULT STDMETHODCALLTYPE IStorage_EnumElements_Stub(IStorage * This, DWORD reserved1, unsigned long cbReserved2, byte * reserved2, DWORD reserved3, IEnumSTATSTG * *ppenum);

    HRESULT STDMETHODCALLTYPE ILockBytes_ReadAt_Proxy(ILockBytes * This, ULARGE_INTEGER ulOffset, void *pv, ULONG cb, ULONG * pcbRead);

    HRESULT __stdcall ILockBytes_ReadAt_Stub(ILockBytes * This, ULARGE_INTEGER ulOffset, byte * pv, ULONG cb, ULONG * pcbRead);

    HRESULT STDMETHODCALLTYPE ILockBytes_WriteAt_Proxy(ILockBytes * This, ULARGE_INTEGER ulOffset, const void *pv, ULONG cb, ULONG * pcbWritten);

    HRESULT STDMETHODCALLTYPE ILockBytes_WriteAt_Stub(ILockBytes * This, ULARGE_INTEGER ulOffset, const byte * pv, ULONG cb, ULONG * pcbWritten);

    HRESULT STDMETHODCALLTYPE IEnumFORMATETC_Next_Proxy(IEnumFORMATETC * This, ULONG celt, FORMATETC * rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE IEnumFORMATETC_Next_Stub(IEnumFORMATETC * This, ULONG celt, FORMATETC * rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE IEnumSTATDATA_Next_Proxy(IEnumSTATDATA * This, ULONG celt, STATDATA * rgelt, ULONG * pceltFetched);

    HRESULT STDMETHODCALLTYPE IEnumSTATDATA_Next_Stub(IEnumSTATDATA * This, ULONG celt, STATDATA * rgelt, ULONG * pceltFetched);

    void STDMETHODCALLTYPE IAdviseSink_OnDataChange_Proxy(IAdviseSink * This, FORMATETC * pFormatetc, STGMEDIUM * pStgmed);

    HRESULT STDMETHODCALLTYPE IAdviseSink_OnDataChange_Stub(IAdviseSink * This, FORMATETC * pFormatetc, ASYNC_STGMEDIUM * pStgmed);

    void STDMETHODCALLTYPE IAdviseSink_OnViewChange_Proxy(IAdviseSink * This, DWORD dwAspect, LONG lindex);

    HRESULT STDMETHODCALLTYPE IAdviseSink_OnViewChange_Stub(IAdviseSink * This, DWORD dwAspect, LONG lindex);

    void STDMETHODCALLTYPE IAdviseSink_OnRename_Proxy(IAdviseSink * This, IMoniker * pmk);

    HRESULT STDMETHODCALLTYPE IAdviseSink_OnRename_Stub(IAdviseSink * This, IMoniker * pmk);

    void STDMETHODCALLTYPE IAdviseSink_OnSave_Proxy(IAdviseSink * This);

    HRESULT STDMETHODCALLTYPE IAdviseSink_OnSave_Stub(IAdviseSink * This);

    void STDMETHODCALLTYPE IAdviseSink_OnClose_Proxy(IAdviseSink * This);

    HRESULT STDMETHODCALLTYPE IAdviseSink_OnClose_Stub(IAdviseSink * This);

    void STDMETHODCALLTYPE IAdviseSink2_OnLinkSrcChange_Proxy(IAdviseSink2 * This, IMoniker * pmk);

    HRESULT STDMETHODCALLTYPE IAdviseSink2_OnLinkSrcChange_Stub(IAdviseSink2 * This, IMoniker * pmk);

    HRESULT STDMETHODCALLTYPE IDataObject_GetData_Proxy(IDataObject * This, FORMATETC * pformatetcIn, STGMEDIUM * pmedium);

    HRESULT STDMETHODCALLTYPE IDataObject_GetData_Stub(IDataObject * This, FORMATETC * pformatetcIn, STGMEDIUM * pRemoteMedium);

    HRESULT STDMETHODCALLTYPE IDataObject_GetDataHere_Proxy(IDataObject * This, FORMATETC * pformatetc, STGMEDIUM * pmedium);

    HRESULT STDMETHODCALLTYPE IDataObject_GetDataHere_Stub(IDataObject * This, FORMATETC * pformatetc, STGMEDIUM * pRemoteMedium);

    HRESULT STDMETHODCALLTYPE IDataObject_SetData_Proxy(IDataObject * This, FORMATETC * pformatetc, STGMEDIUM * pmedium, BOOL fRelease);

    HRESULT STDMETHODCALLTYPE IDataObject_SetData_Stub(IDataObject * This, FORMATETC * pformatetc, FLAG_STGMEDIUM * pmedium, BOOL fRelease);

    HRESULT STDMETHODCALLTYPE IFillLockBytes_FillAppend_Proxy(IFillLockBytes * This, const void *pv, ULONG cb, ULONG * pcbWritten);

    HRESULT __stdcall IFillLockBytes_FillAppend_Stub(IFillLockBytes * This, const byte * pv, ULONG cb, ULONG * pcbWritten);

    HRESULT STDMETHODCALLTYPE IFillLockBytes_FillAt_Proxy(IFillLockBytes * This, ULARGE_INTEGER ulOffset, const void *pv, ULONG cb, ULONG * pcbWritten);

    HRESULT __stdcall IFillLockBytes_FillAt_Stub(IFillLockBytes * This, ULARGE_INTEGER ulOffset, const byte * pv, ULONG cb, ULONG * pcbWritten);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnDataChange_Proxy(AsyncIAdviseSink * This, FORMATETC * pFormatetc, STGMEDIUM * pStgmed);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnDataChange_Stub(AsyncIAdviseSink * This, FORMATETC * pFormatetc, ASYNC_STGMEDIUM * pStgmed);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnDataChange_Proxy(AsyncIAdviseSink * This);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnDataChange_Stub(AsyncIAdviseSink * This);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnViewChange_Proxy(AsyncIAdviseSink * This, DWORD dwAspect, LONG lindex);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnViewChange_Stub(AsyncIAdviseSink * This, DWORD dwAspect, LONG lindex);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnViewChange_Proxy(AsyncIAdviseSink * This);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnViewChange_Stub(AsyncIAdviseSink * This);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnRename_Proxy(AsyncIAdviseSink * This, IMoniker * pmk);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnRename_Stub(AsyncIAdviseSink * This, IMoniker * pmk);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnRename_Proxy(AsyncIAdviseSink * This);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnRename_Stub(AsyncIAdviseSink * This);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnSave_Proxy(AsyncIAdviseSink * This);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnSave_Stub(AsyncIAdviseSink * This);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnSave_Proxy(AsyncIAdviseSink * This);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnSave_Stub(AsyncIAdviseSink * This);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnClose_Proxy(AsyncIAdviseSink * This);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Begin_OnClose_Stub(AsyncIAdviseSink * This);

    void STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnClose_Proxy(AsyncIAdviseSink * This);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink_Finish_OnClose_Stub(AsyncIAdviseSink * This);

    void STDMETHODCALLTYPE AsyncIAdviseSink2_Begin_OnLinkSrcChange_Proxy(AsyncIAdviseSink2 * This, IMoniker * pmk);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink2_Begin_OnLinkSrcChange_Stub(AsyncIAdviseSink2 * This, IMoniker * pmk);

    void STDMETHODCALLTYPE AsyncIAdviseSink2_Finish_OnLinkSrcChange_Proxy(AsyncIAdviseSink2 * This);

    HRESULT STDMETHODCALLTYPE AsyncIAdviseSink2_Finish_OnLinkSrcChange_Stub(AsyncIAdviseSink2 * This);

#ifdef __cplusplus
}
#endif

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#endif /* _OBJIDL_H */
