#ifdef __cplusplus
extern "C" {
#endif

/* DCCMAN module (Desktop Only) OLE interface definitions (Windows CE) */

DEFINE_GUID(IID_IDccManSink,0xa7b88840,0xa812,0x11cf,0x80,0x11,0x0,0xa0,0xc9,0xa,0x8f,0x78);
DEFINE_GUID(IID_IDccMan,0xa7b88841,0xa812,0x11cf,0x80,0x11,0x0,0xa0,0xc9,0xa,0x8f,0x78);
DEFINE_GUID(CLSID_DccMan,0x499c0c20,0xa766,0x11cf,0x80,0x11,0x0,0xa0,0xc9,0xa,0x8f,0x78);

#ifndef _DCCOLE_H
#define _DCCOLE_H

#undef INTERFACE
#define INTERFACE IDccManSink
DECLARE_INTERFACE_ (IDccManSink,IUnknown)
{
    STDMETHOD(OnLogIpAddr)(THIS_ DWORD) PURE;
    STDMETHOD(OnLogTerminated)(THIS) PURE;
    STDMETHOD(OnLogActive)(THIS) PURE;
    STDMETHOD(OnLogInactive)(THIS) PURE;
    STDMETHOD(OnLogAnswered)(THIS) PURE;
    STDMETHOD(OnLogListen)(THIS) PURE;
    STDMETHOD(OnLogDisconnection)(THIS) PURE;
    STDMETHOD(OnLogError)(THIS) PURE;
};
typedef IDccManSink *LPDCCMANSINK;

#undef INTERFACE
#define INTERFACE IDccMan
DECLARE_INTERFACE_ (IDccMan,IUnknown)
{
    STDMETHOD(Advise)(THIS_ IDccManSink*,DWORD*) PURE;
    STDMETHOD(Unadvise)(THIS_ DWORD) PURE;
    STDMETHOD(ShowCommSettings)(THIS) PURE;
    STDMETHOD(AutoconnectEnable)(THIS) PURE;
    STDMETHOD(AutoconnectDisable)(THIS) PURE;
    STDMETHOD(ConnectNow)(THIS) PURE;
    STDMETHOD(DisconnectNow)(THIS) PURE;
    STDMETHOD(SetIconDataTransferring)(THIS) PURE;
    STDMETHOD(SetIconNoDataTransferring)(THIS) PURE;
    STDMETHOD(SetIconError)(THIS) PURE;
};
typedef IDccMan *LPDCCMAN;

#endif /* _DCCOLE_H */

#ifdef __cplusplus
}
#endif

