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
#endif /* COM_NO_WINDOWS_H */

#ifndef _PROPIDL_H
#define _PROPIDL_H

#ifndef __IPropertyStorage_FWD_DEFINED__
#define __IPropertyStorage_FWD_DEFINED__
typedef interface IPropertyStorage IPropertyStorage;
#endif

#ifndef __IPropertySetStorage_FWD_DEFINED__
#define __IPropertySetStorage_FWD_DEFINED__
typedef interface IPropertySetStorage IPropertySetStorage;
#endif

#ifndef __IEnumSTATPROPSTG_FWD_DEFINED__
#define __IEnumSTATPROPSTG_FWD_DEFINED__
typedef interface IEnumSTATPROPSTG IEnumSTATPROPSTG;
#endif

#ifndef __IEnumSTATPROPSETSTG_FWD_DEFINED__
#define __IEnumSTATPROPSETSTG_FWD_DEFINED__
typedef interface IEnumSTATPROPSETSTG IEnumSTATPROPSETSTG;
#endif

#include "objidl.h"
#include "oaidl.h"

#ifdef __cplusplus
extern "C" {
#endif

#if __POCC__ >= 290
#pragma warn(push)
#pragma warn(disable:2198)  /* Nameless field is not standard */
#endif

void * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free(void*);

typedef struct tagVersionedStream {
    GUID guidVersion;
    IStream  *pStream;
} VERSIONEDSTREAM, *LPVERSIONEDSTREAM;

#define PROPSETFLAG_DEFAULT  0
#define PROPSETFLAG_NONSIMPLE  1
#define PROPSETFLAG_ANSI  2
#define PROPSETFLAG_UNBUFFERED  4
#define PROPSETFLAG_CASE_SENSITIVE  8

#define PROPSET_BEHAVIOR_CASE_SENSITIVE  1

typedef struct tagPROPVARIANT PROPVARIANT;

typedef struct tagCAC {
    ULONG cElems;
    CHAR *pElems;
} CAC;

typedef struct tagCAUB {
    ULONG cElems;
    UCHAR *pElems;
} CAUB;

typedef struct tagCAI {
    ULONG cElems;
    SHORT *pElems;
} CAI;

typedef struct tagCAUI {
    ULONG cElems;
    USHORT *pElems;
} CAUI;

typedef struct tagCAL {
    ULONG cElems;
    LONG *pElems;
} CAL;

typedef struct tagCAUL {
    ULONG cElems;
    ULONG *pElems;
} CAUL;

typedef struct tagCAFLT {
    ULONG cElems;
    FLOAT *pElems;
} CAFLT;

typedef struct tagCADBL {
    ULONG cElems;
    DOUBLE *pElems;
} CADBL;

typedef struct tagCACY {
    ULONG cElems;
    CY *pElems;
} CACY;

typedef struct tagCADATE {
    ULONG cElems;
    DATE *pElems;
} CADATE;

typedef struct tagCABSTR {
    ULONG cElems;
    BSTR *pElems;
} CABSTR;

typedef struct tagCABSTRBLOB {
    ULONG cElems;
    BSTRBLOB *pElems;
} CABSTRBLOB;

typedef struct tagCABOOL {
    ULONG cElems;
    VARIANT_BOOL *pElems;
} CABOOL;

typedef struct tagCASCODE {
    ULONG cElems;
    SCODE *pElems;
} CASCODE;

typedef struct tagCAPROPVARIANT {
    ULONG cElems;
    PROPVARIANT *pElems;
} CAPROPVARIANT;

typedef struct tagCAH {
    ULONG cElems;
    LARGE_INTEGER *pElems;
} CAH;

typedef struct tagCAUH {
    ULONG cElems;
    ULARGE_INTEGER *pElems;
} CAUH;

typedef struct tagCALPSTR {
    ULONG cElems;
    LPSTR *pElems;
} CALPSTR;

typedef struct tagCALPWSTR {
    ULONG cElems;
    LPWSTR *pElems;
} CALPWSTR;

typedef struct tagCAFILETIME {
    ULONG cElems;
    FILETIME *pElems;
} CAFILETIME;

typedef struct tagCACLIPDATA {
    ULONG cElems;
    CLIPDATA *pElems;
} CACLIPDATA;

typedef struct tagCACLSID {
    ULONG cElems;
    CLSID *pElems;
} CACLSID;

typedef WORD PROPVAR_PAD1;
typedef WORD PROPVAR_PAD2;
typedef WORD PROPVAR_PAD3;
#define tag_inner_PROPVARIANT

struct tagPROPVARIANT {
    union {
        struct tag_inner_PROPVARIANT {
            VARTYPE vt;
            PROPVAR_PAD1 wReserved1;
            PROPVAR_PAD2 wReserved2;
            PROPVAR_PAD3 wReserved3;
            union {
                CHAR cVal;
                UCHAR bVal;
                SHORT iVal;
                USHORT uiVal;
                LONG lVal;
                ULONG ulVal;
                INT intVal;
                UINT uintVal;
                LARGE_INTEGER hVal;
                ULARGE_INTEGER uhVal;
                FLOAT fltVal;
                DOUBLE dblVal;
                VARIANT_BOOL boolVal;
                _VARIANT_BOOL bool;
                SCODE scode;
                CY cyVal;
                DATE date;
                FILETIME filetime;
                CLSID *puuid;
                CLIPDATA *pclipdata;
                BSTR bstrVal;
                BSTRBLOB bstrblobVal;
                BLOB blob;
                LPSTR pszVal;
                LPWSTR pwszVal;
                IUnknown *punkVal;
                IDispatch *pdispVal;
                IStream *pStream;
                IStorage *pStorage;
                LPVERSIONEDSTREAM pVersionedStream;
                LPSAFEARRAY parray;
                CAC cac;
                CAUB caub;
                CAI cai;
                CAUI caui;
                CAL cal;
                CAUL caul;
                CAH cah;
                CAUH cauh;
                CAFLT caflt;
                CADBL cadbl;
                CABOOL cabool;
                CASCODE cascode;
                CACY cacy;
                CADATE cadate;
                CAFILETIME cafiletime;
                CACLSID cauuid;
                CACLIPDATA caclipdata;
                CABSTR cabstr;
                CABSTRBLOB cabstrblob;
                CALPSTR calpstr;
                CALPWSTR calpwstr;
                CAPROPVARIANT capropvar;
                CHAR *pcVal;
                UCHAR *pbVal;
                SHORT *piVal;
                USHORT *puiVal;
                LONG *plVal;
                ULONG *pulVal;
                INT *pintVal;
                UINT *puintVal;
                FLOAT *pfltVal;
                DOUBLE *pdblVal;
                VARIANT_BOOL *pboolVal;
                DECIMAL *pdecVal;
                SCODE *pscode;
                CY *pcyVal;
                DATE *pdate;
                BSTR *pbstrVal;
                IUnknown **ppunkVal;
                IDispatch **ppdispVal;
                LPSAFEARRAY *pparray;
                PROPVARIANT *pvarVal;
            };
        };
        DECIMAL decVal;
    };
};
typedef struct tagPROPVARIANT *LPPROPVARIANT;

#define PID_DICTIONARY  0
#define PID_CODEPAGE  0x1
#define PID_FIRST_USABLE  0x2
#define PID_FIRST_NAME_DEFAULT  0xfff
#define PID_LOCALE  0x80000000
#define PID_MODIFY_TIME  0x80000001
#define PID_SECURITY  0x80000002
#define PID_BEHAVIOR  0x80000003
#define PID_ILLEGAL  0xffffffff
#define PID_MIN_READONLY  0x80000000
#define PID_MAX_READONLY  0xbfffffff

#define PIDDI_THUMBNAIL  0x00000002L

#define PIDSI_TITLE  0x00000002L
#define PIDSI_SUBJECT  0x00000003L
#define PIDSI_AUTHOR  0x00000004L
#define PIDSI_KEYWORDS  0x00000005L
#define PIDSI_COMMENTS  0x00000006L
#define PIDSI_TEMPLATE  0x00000007L
#define PIDSI_LASTAUTHOR  0x00000008L
#define PIDSI_REVNUMBER  0x00000009L
#define PIDSI_EDITTIME  0x0000000aL
#define PIDSI_LASTPRINTED  0x0000000bL
#define PIDSI_CREATE_DTM  0x0000000cL
#define PIDSI_LASTSAVE_DTM  0x0000000dL
#define PIDSI_PAGECOUNT  0x0000000eL
#define PIDSI_WORDCOUNT  0x0000000fL
#define PIDSI_CHARCOUNT  0x00000010L
#define PIDSI_THUMBNAIL  0x00000011L
#define PIDSI_APPNAME  0x00000012L
#define PIDSI_DOC_SECURITY  0x00000013L

#define PIDDSI_CATEGORY  0x00000002
#define PIDDSI_PRESFORMAT  0x00000003
#define PIDDSI_BYTECOUNT  0x00000004
#define PIDDSI_LINECOUNT  0x00000005
#define PIDDSI_PARCOUNT  0x00000006
#define PIDDSI_SLIDECOUNT  0x00000007
#define PIDDSI_NOTECOUNT  0x00000008
#define PIDDSI_HIDDENCOUNT  0x00000009
#define PIDDSI_MMCLIPCOUNT  0x0000000A
#define PIDDSI_SCALE  0x0000000B
#define PIDDSI_HEADINGPAIR  0x0000000C
#define PIDDSI_DOCPARTS  0x0000000D
#define PIDDSI_MANAGER  0x0000000E
#define PIDDSI_COMPANY  0x0000000F
#define PIDDSI_LINKSDIRTY  0x00000010

#define PIDMSI_EDITOR  0x00000002L
#define PIDMSI_SUPPLIER  0x00000003L
#define PIDMSI_SOURCE  0x00000004L
#define PIDMSI_SEQUENCE_NO  0x00000005L
#define PIDMSI_PROJECT  0x00000006L
#define PIDMSI_STATUS  0x00000007L
#define PIDMSI_OWNER  0x00000008L
#define PIDMSI_RATING  0x00000009L
#define PIDMSI_PRODUCTION  0x0000000AL
#define PIDMSI_COPYRIGHT  0x0000000BL

enum PIDMSI_STATUS_VALUE {
    PIDMSI_STATUS_NORMAL = 0,
    PIDMSI_STATUS_NEW = PIDMSI_STATUS_NORMAL + 1,
    PIDMSI_STATUS_PRELIM = PIDMSI_STATUS_NEW + 1,
    PIDMSI_STATUS_DRAFT = PIDMSI_STATUS_PRELIM + 1,
    PIDMSI_STATUS_INPROGRESS = PIDMSI_STATUS_DRAFT + 1,
    PIDMSI_STATUS_EDIT = PIDMSI_STATUS_INPROGRESS + 1,
    PIDMSI_STATUS_REVIEW = PIDMSI_STATUS_EDIT + 1,
    PIDMSI_STATUS_PROOF = PIDMSI_STATUS_REVIEW + 1,
    PIDMSI_STATUS_FINAL = PIDMSI_STATUS_PROOF + 1,
    PIDMSI_STATUS_OTHER = 0x7fff
};

#define PRSPEC_INVALID  0xffffffff
#define PRSPEC_LPWSTR  0
#define PRSPEC_PROPID  1

typedef struct tagPROPSPEC {
    ULONG ulKind;
    union {
        PROPID propid;
        LPOLESTR lpwstr;
    };
} PROPSPEC;

typedef struct tagSTATPROPSTG {
    LPOLESTR lpwstrName;
    PROPID propid;
    VARTYPE vt;
} STATPROPSTG;

#define PROPSETHDR_OSVER_KIND(dwOSVer)  HIWORD((dwOSVer))
#define PROPSETHDR_OSVER_MAJOR(dwOSVer)  LOBYTE(LOWORD((dwOSVer)))
#define PROPSETHDR_OSVER_MINOR(dwOSVer)  HIBYTE(LOWORD((dwOSVer)))
#define PROPSETHDR_OSVERSION_UNKNOWN  0xFFFFFFFF

typedef struct tagSTATPROPSETSTG {
    FMTID fmtid;
    CLSID clsid;
    DWORD grfFlags;
    FILETIME mtime;
    FILETIME ctime;
    FILETIME atime;
    DWORD dwOSVersion;
} STATPROPSETSTG;

extern RPC_IF_HANDLE __MIDL_itf_propidl_0000_v0_0_c_ifspec;
extern RPC_IF_HANDLE __MIDL_itf_propidl_0000_v0_0_s_ifspec;

#ifndef __IPropertyStorage_INTERFACE_DEFINED__
#define __IPropertyStorage_INTERFACE_DEFINED__
EXTERN_C const IID IID_IPropertyStorage;

#if defined(__cplusplus) && !defined(CINTERFACE)
MIDL_INTERFACE("00000138-0000-0000-C000-000000000046") IPropertyStorage : public IUnknown
{
    public:
    virtual HRESULT STDMETHODCALLTYPE ReadMultiple(ULONG,const PROPSPEC [],PROPVARIANT []) = 0;
    virtual HRESULT STDMETHODCALLTYPE WriteMultiple(ULONG,const PROPSPEC [],const PROPVARIANT [],PROPID) = 0;
    virtual HRESULT STDMETHODCALLTYPE DeleteMultiple(ULONG,const PROPSPEC []) = 0;
    virtual HRESULT STDMETHODCALLTYPE ReadPropertyNames(ULONG,const PROPID [],LPOLESTR []) = 0;
    virtual HRESULT STDMETHODCALLTYPE WritePropertyNames(ULONG,const PROPID [],const LPOLESTR []) = 0;
    virtual HRESULT STDMETHODCALLTYPE DeletePropertyNames(ULONG,const PROPID []) = 0;
    virtual HRESULT STDMETHODCALLTYPE Commit(DWORD) = 0;
    virtual HRESULT STDMETHODCALLTYPE Revert(void) = 0;
    virtual HRESULT STDMETHODCALLTYPE Enum(IEnumSTATPROPSTG**) = 0;
    virtual HRESULT STDMETHODCALLTYPE SetTimes(const FILETIME*,const FILETIME*,const FILETIME*) = 0;
    virtual HRESULT STDMETHODCALLTYPE SetClass(REFCLSID) = 0;
    virtual HRESULT STDMETHODCALLTYPE Stat(STATPROPSETSTG*) = 0;
};
#else
typedef struct IPropertyStorageVtbl
{
    BEGIN_INTERFACE
    HRESULT (STDMETHODCALLTYPE  *QueryInterface)(IPropertyStorage*,REFIID,void**);
    ULONG (STDMETHODCALLTYPE *AddRef)(IPropertyStorage*);
    ULONG (STDMETHODCALLTYPE *Release)(IPropertyStorage*);
    HRESULT (STDMETHODCALLTYPE *ReadMultiple)(IPropertyStorage*,ULONG,const PROPSPEC [],PROPVARIANT []);
    HRESULT (STDMETHODCALLTYPE *WriteMultiple)(IPropertyStorage*,ULONG,const PROPSPEC [],const PROPVARIANT [],PROPID);
    HRESULT (STDMETHODCALLTYPE *DeleteMultiple)(IPropertyStorage*,ULONG,const PROPSPEC []);
    HRESULT (STDMETHODCALLTYPE *ReadPropertyNames)(IPropertyStorage*,ULONG,const PROPID [],LPOLESTR []);
    HRESULT (STDMETHODCALLTYPE *WritePropertyNames)(IPropertyStorage*,ULONG,const PROPID [],const LPOLESTR []);
    HRESULT (STDMETHODCALLTYPE *DeletePropertyNames)(IPropertyStorage*,ULONG,const PROPID []);
    HRESULT (STDMETHODCALLTYPE *Commit)(IPropertyStorage*,DWORD);
    HRESULT (STDMETHODCALLTYPE *Revert)(IPropertyStorage*);
    HRESULT (STDMETHODCALLTYPE *Enum)(IPropertyStorage*,IEnumSTATPROPSTG**);
    HRESULT (STDMETHODCALLTYPE *SetTimes)(IPropertyStorage*,const FILETIME*,const FILETIME*,const FILETIME*);
    HRESULT (STDMETHODCALLTYPE *SetClass)(IPropertyStorage*,REFCLSID);
    HRESULT (STDMETHODCALLTYPE *Stat)(IPropertyStorage*,STATPROPSETSTG*);
    END_INTERFACE
} IPropertyStorageVtbl;

interface IPropertyStorage {
    CONST_VTBL struct IPropertyStorageVtbl *lpVtbl;
};

#ifdef COBJMACROS
#define IPropertyStorage_QueryInterface(This,riid,ppvObject)  (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPropertyStorage_AddRef(This)  (This)->lpVtbl->AddRef(This)
#define IPropertyStorage_Release(This)  (This)->lpVtbl->Release(This)
#define IPropertyStorage_ReadMultiple(This,cpspec,rgpspec,rgpropvar)  (This)->lpVtbl->ReadMultiple(This,cpspec,rgpspec,rgpropvar)
#define IPropertyStorage_WriteMultiple(This,cpspec,rgpspec,rgpropvar,propidNameFirst)  (This)->lpVtbl->WriteMultiple(This,cpspec,rgpspec,rgpropvar,propidNameFirst)
#define IPropertyStorage_DeleteMultiple(This,cpspec,rgpspec)  (This)->lpVtbl->DeleteMultiple(This,cpspec,rgpspec)
#define IPropertyStorage_ReadPropertyNames(This,cpropid,rgpropid,rglpwstrName)  (This)->lpVtbl->ReadPropertyNames(This,cpropid,rgpropid,rglpwstrName)
#define IPropertyStorage_WritePropertyNames(This,cpropid,rgpropid,rglpwstrName)  (This)->lpVtbl->WritePropertyNames(This,cpropid,rgpropid,rglpwstrName)
#define IPropertyStorage_DeletePropertyNames(This,cpropid,rgpropid)  (This)->lpVtbl->DeletePropertyNames(This,cpropid,rgpropid)
#define IPropertyStorage_Commit(This,grfCommitFlags)  (This)->lpVtbl->Commit(This,grfCommitFlags)
#define IPropertyStorage_Revert(This)  (This)->lpVtbl->Revert(This)
#define IPropertyStorage_Enum(This,ppenum)  (This)->lpVtbl->Enum(This,ppenum)
#define IPropertyStorage_SetTimes(This,pctime,patime,pmtime)  (This)->lpVtbl->SetTimes(This,pctime,patime,pmtime)
#define IPropertyStorage_SetClass(This,clsid)  (This)->lpVtbl->SetClass(This,clsid)
#define IPropertyStorage_Stat(This,pstatpsstg)  (This)->lpVtbl->Stat(This,pstatpsstg)
#endif /* COBJMACROS */
#endif /* __cplusplus && !CINTERFACE */

HRESULT STDMETHODCALLTYPE IPropertyStorage_ReadMultiple_Proxy(IPropertyStorage*,ULONG,const PROPSPEC [],PROPVARIANT []);
void __RPC_STUB IPropertyStorage_ReadMultiple_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_WriteMultiple_Proxy(IPropertyStorage*,ULONG,const PROPSPEC [],const PROPVARIANT [],PROPID);
void __RPC_STUB IPropertyStorage_WriteMultiple_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_DeleteMultiple_Proxy(IPropertyStorage*,ULONG,const PROPSPEC []);
void __RPC_STUB IPropertyStorage_DeleteMultiple_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_ReadPropertyNames_Proxy(IPropertyStorage*,ULONG,const PROPID [],LPOLESTR []);
void __RPC_STUB IPropertyStorage_ReadPropertyNames_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_WritePropertyNames_Proxy(IPropertyStorage*,ULONG,const PROPID [],const LPOLESTR []);
void __RPC_STUB IPropertyStorage_WritePropertyNames_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_DeletePropertyNames_Proxy(IPropertyStorage*,ULONG,const PROPID []);
void __RPC_STUB IPropertyStorage_DeletePropertyNames_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_Commit_Proxy(IPropertyStorage*,DWORD);
void __RPC_STUB IPropertyStorage_Commit_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_Revert_Proxy(IPropertyStorage*);
void __RPC_STUB IPropertyStorage_Revert_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_Enum_Proxy(IPropertyStorage*,IEnumSTATPROPSTG**);
void __RPC_STUB IPropertyStorage_Enum_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_SetTimes_Proxy(IPropertyStorage*,const FILETIME*,const FILETIME*,const FILETIME*);
void __RPC_STUB IPropertyStorage_SetTimes_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_SetClass_Proxy(IPropertyStorage*,REFCLSID);
void __RPC_STUB IPropertyStorage_SetClass_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertyStorage_Stat_Proxy(IPropertyStorage*,STATPROPSETSTG*);
void __RPC_STUB IPropertyStorage_Stat_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);

#endif /* __IPropertyStorage_INTERFACE_DEFINED__ */


#ifndef __IPropertySetStorage_INTERFACE_DEFINED__
#define __IPropertySetStorage_INTERFACE_DEFINED__

typedef IPropertySetStorage *LPPROPERTYSETSTORAGE;

EXTERN_C const IID IID_IPropertySetStorage;

#if defined(__cplusplus) && !defined(CINTERFACE)
MIDL_INTERFACE("0000013A-0000-0000-C000-000000000046") IPropertySetStorage : public IUnknown
{
    public:
    virtual HRESULT STDMETHODCALLTYPE Create(REFFMTID,const CLSID*,DWORD,DWORD,IPropertyStorage**) = 0;
    virtual HRESULT STDMETHODCALLTYPE Open(REFFMTID,DWORD,IPropertyStorage**) = 0;
    virtual HRESULT STDMETHODCALLTYPE Delete(REFFMTID) = 0;
    virtual HRESULT STDMETHODCALLTYPE Enum(IEnumSTATPROPSETSTG**) = 0;
};
#else
typedef struct IPropertySetStorageVtbl
{
    BEGIN_INTERFACE
    HRESULT (STDMETHODCALLTYPE *QueryInterface)(IPropertySetStorage*,REFIID,void**);
    ULONG (STDMETHODCALLTYPE *AddRef)(IPropertySetStorage*);
    ULONG (STDMETHODCALLTYPE *Release)(IPropertySetStorage*);
    HRESULT (STDMETHODCALLTYPE *Create)(IPropertySetStorage*,REFFMTID,const CLSID*,DWORD,DWORD,IPropertyStorage**);
    HRESULT (STDMETHODCALLTYPE *Open)(IPropertySetStorage*,REFFMTID,DWORD,IPropertyStorage**);
    HRESULT (STDMETHODCALLTYPE *Delete)(IPropertySetStorage*,REFFMTID);
    HRESULT (STDMETHODCALLTYPE *Enum)(IPropertySetStorage*,IEnumSTATPROPSETSTG**);
    END_INTERFACE
} IPropertySetStorageVtbl;

interface IPropertySetStorage {
    CONST_VTBL struct IPropertySetStorageVtbl *lpVtbl;
};

#ifdef COBJMACROS
#define IPropertySetStorage_QueryInterface(This,riid,ppvObject)  (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IPropertySetStorage_AddRef(This)  (This)->lpVtbl->AddRef(This)
#define IPropertySetStorage_Release(This)  (This)->lpVtbl->Release(This)
#define IPropertySetStorage_Create(This,rfmtid,pclsid,grfFlags,grfMode,ppprstg)  (This)->lpVtbl->Create(This,rfmtid,pclsid,grfFlags,grfMode,ppprstg)
#define IPropertySetStorage_Open(This,rfmtid,grfMode,ppprstg)  (This)->lpVtbl->Open(This,rfmtid,grfMode,ppprstg)
#define IPropertySetStorage_Delete(This,rfmtid)  (This)->lpVtbl->Delete(This,rfmtid)
#define IPropertySetStorage_Enum(This,ppenum)  (This)->lpVtbl->Enum(This,ppenum)
#endif /* COBJMACROS */
#endif /* __cplusplus && !CINTERFACE */

HRESULT STDMETHODCALLTYPE IPropertySetStorage_Create_Proxy(IPropertySetStorage*,REFFMTID,const CLSID*,DWORD,DWORD,IPropertyStorage**);
void __RPC_STUB IPropertySetStorage_Create_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertySetStorage_Open_Proxy(IPropertySetStorage*,REFFMTID,DWORD,IPropertyStorage**);
void __RPC_STUB IPropertySetStorage_Open_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertySetStorage_Delete_Proxy(IPropertySetStorage*,REFFMTID);
void __RPC_STUB IPropertySetStorage_Delete_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IPropertySetStorage_Enum_Proxy(IPropertySetStorage*,IEnumSTATPROPSETSTG**);
void __RPC_STUB IPropertySetStorage_Enum_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);

#endif /* __IPropertySetStorage_INTERFACE_DEFINED__ */


#ifndef __IEnumSTATPROPSTG_INTERFACE_DEFINED__
#define __IEnumSTATPROPSTG_INTERFACE_DEFINED__

typedef IEnumSTATPROPSTG *LPENUMSTATPROPSTG;

EXTERN_C const IID IID_IEnumSTATPROPSTG;

#if defined(__cplusplus) && !defined(CINTERFACE)
MIDL_INTERFACE("00000139-0000-0000-C000-000000000046") IEnumSTATPROPSTG : public IUnknown
{
    public:
    virtual HRESULT STDMETHODCALLTYPE Next(ULONG,STATPROPSTG*,ULONG*) = 0;
    virtual HRESULT STDMETHODCALLTYPE Skip(ULONG) = 0;
    virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;
    virtual HRESULT STDMETHODCALLTYPE Clone(IEnumSTATPROPSTG**) = 0;
};
#else
typedef struct IEnumSTATPROPSTGVtbl
{
    BEGIN_INTERFACE
    HRESULT (STDMETHODCALLTYPE *QueryInterface)(IEnumSTATPROPSTG*,REFIID,void**);
    ULONG (STDMETHODCALLTYPE *AddRef)(IEnumSTATPROPSTG*);
    ULONG (STDMETHODCALLTYPE *Release)(IEnumSTATPROPSTG*);
    HRESULT (STDMETHODCALLTYPE *Next)(IEnumSTATPROPSTG*,ULONG,STATPROPSTG*,ULONG*);
    HRESULT (STDMETHODCALLTYPE *Skip)(IEnumSTATPROPSTG*,ULONG);
    HRESULT (STDMETHODCALLTYPE *Reset)(IEnumSTATPROPSTG*);
    HRESULT (STDMETHODCALLTYPE *Clone)(IEnumSTATPROPSTG*,IEnumSTATPROPSTG**);
    END_INTERFACE
} IEnumSTATPROPSTGVtbl;

interface IEnumSTATPROPSTG {
    CONST_VTBL struct IEnumSTATPROPSTGVtbl *lpVtbl;
};

#ifdef COBJMACROS
#define IEnumSTATPROPSTG_QueryInterface(This,riid,ppvObject)  (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumSTATPROPSTG_AddRef(This)  (This)->lpVtbl->AddRef(This)
#define IEnumSTATPROPSTG_Release(This)  (This)->lpVtbl->Release(This)
#define IEnumSTATPROPSTG_Next(This,celt,rgelt,pceltFetched)  (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumSTATPROPSTG_Skip(This,celt)  (This)->lpVtbl->Skip(This,celt)
#define IEnumSTATPROPSTG_Reset(This)  (This)->lpVtbl->Reset(This)
#define IEnumSTATPROPSTG_Clone(This,ppenum)  (This)->lpVtbl->Clone(This,ppenum)
#endif /* COBJMACROS */
#endif /* __cplusplus && !CINTERFACE */


HRESULT STDMETHODCALLTYPE IEnumSTATPROPSTG_RemoteNext_Proxy(IEnumSTATPROPSTG*,ULONG,STATPROPSTG*,ULONG*);
void __RPC_STUB IEnumSTATPROPSTG_RemoteNext_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IEnumSTATPROPSTG_Skip_Proxy(IEnumSTATPROPSTG*,ULONG);
void __RPC_STUB IEnumSTATPROPSTG_Skip_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IEnumSTATPROPSTG_Reset_Proxy(IEnumSTATPROPSTG*);
void __RPC_STUB IEnumSTATPROPSTG_Reset_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IEnumSTATPROPSTG_Clone_Proxy(IEnumSTATPROPSTG*,IEnumSTATPROPSTG**);
void __RPC_STUB IEnumSTATPROPSTG_Clone_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);

#endif /* __IEnumSTATPROPSTG_INTERFACE_DEFINED__ */


#ifndef __IEnumSTATPROPSETSTG_INTERFACE_DEFINED__
#define __IEnumSTATPROPSETSTG_INTERFACE_DEFINED__

typedef IEnumSTATPROPSETSTG *LPENUMSTATPROPSETSTG;

EXTERN_C const IID IID_IEnumSTATPROPSETSTG;

#if defined(__cplusplus) && !defined(CINTERFACE)
MIDL_INTERFACE("0000013B-0000-0000-C000-000000000046") IEnumSTATPROPSETSTG : public IUnknown
{
    public:
    virtual HRESULT STDMETHODCALLTYPE Next(ULONG,STATPROPSETSTG*,ULONG*) = 0;
    virtual HRESULT STDMETHODCALLTYPE Skip(ULONG) = 0;
    virtual HRESULT STDMETHODCALLTYPE Reset(void) = 0;
    virtual HRESULT STDMETHODCALLTYPE Clone(IEnumSTATPROPSETSTG**) = 0;
};
#else
typedef struct IEnumSTATPROPSETSTGVtbl
{
    BEGIN_INTERFACE
    HRESULT (STDMETHODCALLTYPE *QueryInterface)(IEnumSTATPROPSETSTG*,REFIID,void**);
    ULONG (STDMETHODCALLTYPE *AddRef)(IEnumSTATPROPSETSTG*);
    ULONG (STDMETHODCALLTYPE *Release)(IEnumSTATPROPSETSTG*);
    HRESULT (STDMETHODCALLTYPE *Next)(IEnumSTATPROPSETSTG*,ULONG,STATPROPSETSTG*,ULONG*);
    HRESULT (STDMETHODCALLTYPE *Skip)(IEnumSTATPROPSETSTG*,ULONG);
    HRESULT (STDMETHODCALLTYPE *Reset)(IEnumSTATPROPSETSTG*);
    HRESULT (STDMETHODCALLTYPE *Clone)(IEnumSTATPROPSETSTG*,IEnumSTATPROPSETSTG**);
    END_INTERFACE
} IEnumSTATPROPSETSTGVtbl;

interface IEnumSTATPROPSETSTG {
    CONST_VTBL struct IEnumSTATPROPSETSTGVtbl *lpVtbl;
};

#ifdef COBJMACROS
#define IEnumSTATPROPSETSTG_QueryInterface(This,riid,ppvObject)  (This)->lpVtbl->QueryInterface(This,riid,ppvObject)
#define IEnumSTATPROPSETSTG_AddRef(This)  (This)->lpVtbl->AddRef(This)
#define IEnumSTATPROPSETSTG_Release(This)  (This)->lpVtbl->Release(This)
#define IEnumSTATPROPSETSTG_Next(This,celt,rgelt,pceltFetched)  (This)->lpVtbl->Next(This,celt,rgelt,pceltFetched)
#define IEnumSTATPROPSETSTG_Skip(This,celt)  (This)->lpVtbl->Skip(This,celt)
#define IEnumSTATPROPSETSTG_Reset(This)  (This)->lpVtbl->Reset(This)
#define IEnumSTATPROPSETSTG_Clone(This,ppenum)  (This)->lpVtbl->Clone(This,ppenum)
#endif /* COBJMACROS */
#endif /* __cplusplus && !CINTERFACE */

HRESULT STDMETHODCALLTYPE IEnumSTATPROPSETSTG_RemoteNext_Proxy(IEnumSTATPROPSETSTG*,ULONG,STATPROPSETSTG*,ULONG*);
void __RPC_STUB IEnumSTATPROPSETSTG_RemoteNext_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IEnumSTATPROPSETSTG_Skip_Proxy(IEnumSTATPROPSETSTG*,ULONG);
void __RPC_STUB IEnumSTATPROPSETSTG_Skip_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IEnumSTATPROPSETSTG_Reset_Proxy(IEnumSTATPROPSETSTG*);
void __RPC_STUB IEnumSTATPROPSETSTG_Reset_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);
HRESULT STDMETHODCALLTYPE IEnumSTATPROPSETSTG_Clone_Proxy(IEnumSTATPROPSETSTG*,IEnumSTATPROPSETSTG**);
void __RPC_STUB IEnumSTATPROPSETSTG_Clone_Stub(IRpcStubBuffer*,IRpcChannelBuffer*,PRPC_MESSAGE,DWORD*);

#endif /* __IEnumSTATPROPSETSTG_INTERFACE_DEFINED__ */

typedef IPropertyStorage *LPPROPERTYSTORAGE;

#define CCH_MAX_PROPSTG_NAME  31

WINOLEAPI PropVariantCopy(PROPVARIANT*,const PROPVARIANT*);
WINOLEAPI PropVariantClear(PROPVARIANT*);
WINOLEAPI FreePropVariantArray(ULONG,PROPVARIANT*);

#define _PROPVARIANTINIT_DEFINED_
#ifdef __cplusplus
inline void PropVariantInit(PROPVARIANT *pvar) { memset(pvar,0,sizeof(PROPVARIANT)); }
#else
#define PropVariantInit(pvar)  memset((pvar),0,sizeof(PROPVARIANT))
#endif

#ifndef _STGCREATEPROPSTG_DEFINED_
WINOLEAPI StgCreatePropStg(IUnknown*,REFFMTID,const CLSID*,DWORD,DWORD,IPropertyStorage**);
WINOLEAPI StgOpenPropStg(IUnknown*,REFFMTID,DWORD,DWORD,IPropertyStorage**);
WINOLEAPI StgCreatePropSetStg(IStorage*,DWORD,IPropertySetStorage**);
WINOLEAPI FmtIdToPropStgName(const FMTID*,LPOLESTR);
WINOLEAPI PropStgNameToFmtId(const LPOLESTR,FMTID*);
#endif

extern RPC_IF_HANDLE __MIDL_itf_propidl_0109_v0_0_c_ifspec;
extern RPC_IF_HANDLE __MIDL_itf_propidl_0109_v0_0_s_ifspec;

unsigned long __RPC_USER BSTR_UserSize(unsigned long*,unsigned long,BSTR*);
unsigned char* __RPC_USER BSTR_UserMarshal(unsigned long*,unsigned char*,BSTR*);
unsigned char* __RPC_USER BSTR_UserUnmarshal(unsigned long*,unsigned char*,BSTR*);
void __RPC_USER BSTR_UserFree(unsigned long*,BSTR*);
unsigned long __RPC_USER LPSAFEARRAY_UserSize(unsigned long*,unsigned long,LPSAFEARRAY*);
unsigned char* __RPC_USER LPSAFEARRAY_UserMarshal(unsigned long*,unsigned char*,LPSAFEARRAY*);
unsigned char* __RPC_USER LPSAFEARRAY_UserUnmarshal(unsigned long*,unsigned char*,LPSAFEARRAY*);
void __RPC_USER LPSAFEARRAY_UserFree(unsigned long*,LPSAFEARRAY*);

HRESULT STDMETHODCALLTYPE IEnumSTATPROPSTG_Next_Proxy(IEnumSTATPROPSTG*,ULONG,STATPROPSTG*,ULONG*);
HRESULT STDMETHODCALLTYPE IEnumSTATPROPSTG_Next_Stub(IEnumSTATPROPSTG*,ULONG,STATPROPSTG*,ULONG*);
HRESULT STDMETHODCALLTYPE IEnumSTATPROPSETSTG_Next_Proxy(IEnumSTATPROPSETSTG*,ULONG,STATPROPSETSTG*,ULONG*);
HRESULT STDMETHODCALLTYPE IEnumSTATPROPSETSTG_Next_Stub(IEnumSTATPROPSETSTG*,ULONG,STATPROPSETSTG*,ULONG*);

#if __POCC__ >= 290
#pragma warn(pop)
#endif

#ifdef __cplusplus
}
#endif

#endif /* _PROPIDL_H */
