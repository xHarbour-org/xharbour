#ifndef _DMPLUGIN_H
#define _DMPLUGIN_H

/* DirectMusic performance layer plugin definitions */

#include <windows.h>

#define COM_NO_WINDOWS_H
#include <objbase.h>
#include <mmsystem.h>
#include <dmusici.h>

#include <pshpack8.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DMUS_REGSTR_PATH_TOOLS  "Software\\Microsoft\\DirectMusic\\Tools"

#define DMUS_TRACK_PARAMF_CLOCK  0x01

DEFINE_GUID(CLSID_DirectMusicTempoTrack,0xd2ac2885,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicSeqTrack,0xd2ac2886,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicSysExTrack,0xd2ac2887,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicTimeSigTrack,0xd2ac2888,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicChordTrack,0xd2ac288b,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicCommandTrack,0xd2ac288c,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicStyleTrack,0xd2ac288d,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicMotifTrack,0xd2ac288e,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicSignPostTrack,0xf17e8672,0xc3b4,0x11d1,0x87,0xb,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicBandTrack,0xd2ac2894,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicChordMapTrack,0xd2ac2896,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicMuteTrack,0xd2ac2898,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(CLSID_DirectMusicScriptTrack,0x4108fa85,0x3586,0x11d3,0x8b,0xd7,0x0,0x60,0x8,0x93,0xb1,0xb6);
DEFINE_GUID(CLSID_DirectMusicMarkerTrack,0x55a8fd00,0x4288,0x11d3,0x9b,0xd1,0x8a,0xd,0x61,0xc8,0x88,0x35);
DEFINE_GUID(CLSID_DirectMusicSegmentTriggerTrack,0xbae4d665,0x4ea1,0x11d3,0x8b,0xda,0x0,0x60,0x8,0x93,0xb1,0xb6);
DEFINE_GUID(CLSID_DirectMusicLyricsTrack,0x995c1cf5,0x54ff,0x11d3,0x8b,0xda,0x0,0x60,0x8,0x93,0xb1,0xb6);
DEFINE_GUID(CLSID_DirectMusicParamControlTrack,0x4be0537b,0x5c19,0x11d3,0x8b,0xdc,0x0,0x60,0x8,0x93,0xb1,0xb6);
DEFINE_GUID(CLSID_DirectMusicMelodyFormulationTrack,0xb0684266,0xb57f,0x11d2,0x97,0xf9,0x0,0xc0,0x4f,0xa3,0x6e,0x58);
DEFINE_GUID(CLSID_DirectMusicWaveTrack,0xeed36461,0x9ea5,0x11d3,0x9b,0xd1,0x0,0x80,0xc7,0x15,0xa,0x74);
DEFINE_GUID(IID_IDirectMusicTrack,0xf96029a1,0x4282,0x11d2,0x87,0x17,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(IID_IDirectMusicTool,0xd2ac28ba,0xb39b,0x11d1,0x87,0x4,0x0,0x60,0x8,0x93,0xb1,0xbd);
DEFINE_GUID(IID_IDirectMusicTool8,0xe674303,0x3b05,0x11d3,0x9b,0xd1,0xf9,0xe7,0xf0,0xa0,0x15,0x36);
DEFINE_GUID(IID_IDirectMusicTrack8,0xe674304,0x3b05,0x11d3,0x9b,0xd1,0xf9,0xe7,0xf0,0xa0,0x15,0x36);

interface IDirectMusicTrack;
interface IDirectMusicTool;
interface IDirectMusicTool8;
interface IDirectMusicTrack8;
interface IDirectMusicPerformance;
interface IDirectMusicPerformance8;
interface IDirectMusicSegment;
interface IDirectMusicSegment8;
interface IDirectMusicSegmentState;
interface IDirectMusicSegmentState8;
interface IDirectMusicGraph;

#ifndef __cplusplus
typedef interface IDirectMusicTrack IDirectMusicTrack;
typedef interface IDirectMusicTool IDirectMusicTool;
typedef interface IDirectMusicTool8 IDirectMusicTool8;
typedef interface IDirectMusicTrack8 IDirectMusicTrack8;
typedef interface IDirectMusicPerformance IDirectMusicPerformance;
typedef interface IDirectMusicPerformance8 IDirectMusicPerformance8;
typedef interface IDirectMusicSegment IDirectMusicSegment;
typedef interface IDirectMusicSegment8 IDirectMusicSegment8;
typedef interface IDirectMusicSegmentState IDirectMusicSegmentState;
typedef interface IDirectMusicSegmentState8 IDirectMusicSegmentState8;
typedef interface IDirectMusicGraph IDirectMusicGraph;
#endif /* __cplusplus */

typedef struct _DMUS_PMSG DMUS_PMSG;
typedef long MUSIC_TIME;

typedef enum enumDMUS_TRACKF_FLAGS {
    DMUS_TRACKF_SEEK = 1,
    DMUS_TRACKF_LOOP = 2,
    DMUS_TRACKF_START = 4,
    DMUS_TRACKF_FLUSH = 8,
    DMUS_TRACKF_DIRTY = 0x10,
    DMUS_TRACKF_NOTIFY_OFF = 0x20,
    DMUS_TRACKF_PLAY_OFF = 0x40,
    DMUS_TRACKF_LOOPEND = 0x80,
    DMUS_TRACKF_STOP = 0x100,
    DMUS_TRACKF_RECOMPOSE = 0x200,
    DMUS_TRACKF_CLOCK = 0x400,
} DMUS_TRACKF_FLAGS;

#undef INTERFACE
#define INTERFACE IDirectMusicTool
DECLARE_INTERFACE_(IDirectMusicTool,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Init)(THIS_ IDirectMusicGraph*) PURE;
    STDMETHOD(GetMsgDeliveryType)(THIS_ DWORD*) PURE;
    STDMETHOD(GetMediaTypeArraySize)(THIS_ DWORD*) PURE;
    STDMETHOD(GetMediaTypes)(THIS_ DWORD**,DWORD) PURE;
    STDMETHOD(ProcessPMsg)(THIS_ IDirectMusicPerformance*,DMUS_PMSG*) PURE;
    STDMETHOD(Flush)(THIS_ IDirectMusicPerformance*,DMUS_PMSG*,REFERENCE_TIME) PURE;
};

#undef INTERFACE
#define INTERFACE IDirectMusicTool8
DECLARE_INTERFACE_(IDirectMusicTool8,IDirectMusicTool)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Init)(THIS_ IDirectMusicGraph*) PURE;
    STDMETHOD(GetMsgDeliveryType)(THIS_ DWORD*) PURE;
    STDMETHOD(GetMediaTypeArraySize)(THIS_ DWORD*) PURE;
    STDMETHOD(GetMediaTypes)(THIS_ DWORD**,DWORD) PURE;
    STDMETHOD(ProcessPMsg)(THIS_ IDirectMusicPerformance*,DMUS_PMSG*) PURE;
    STDMETHOD(Flush)(THIS_ IDirectMusicPerformance*,DMUS_PMSG*,REFERENCE_TIME) PURE;
    STDMETHOD(Clone)(THIS_ IDirectMusicTool**) PURE;
};

#undef INTERFACE
#define INTERFACE IDirectMusicTrack
DECLARE_INTERFACE_(IDirectMusicTrack,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Init)(THIS_ IDirectMusicSegment*) PURE;
    STDMETHOD(InitPlay)(THIS_ IDirectMusicSegmentState*,IDirectMusicPerformance*,void**,DWORD,DWORD) PURE;
    STDMETHOD(EndPlay)(THIS_ void*) PURE;
    STDMETHOD(Play)(THIS_ void*,MUSIC_TIME,MUSIC_TIME,MUSIC_TIME,DWORD,IDirectMusicPerformance*,IDirectMusicSegmentState*,DWORD) PURE;
    STDMETHOD(GetParam)(THIS_ REFGUID,MUSIC_TIME,MUSIC_TIME*,void*) PURE;
    STDMETHOD(SetParam)(THIS_ REFGUID,MUSIC_TIME,void*) PURE;
    STDMETHOD(IsParamSupported)(THIS_ REFGUID) PURE;
    STDMETHOD(AddNotificationType)(THIS_ REFGUID) PURE;
    STDMETHOD(RemoveNotificationType)(THIS_ REFGUID) PURE;
    STDMETHOD(Clone)(THIS_ MUSIC_TIME,MUSIC_TIME,IDirectMusicTrack**) PURE;
};

#undef INTERFACE
#define INTERFACE IDirectMusicTrack8
DECLARE_INTERFACE_(IDirectMusicTrack8,IDirectMusicTrack)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Init)(THIS_ IDirectMusicSegment*) PURE;
    STDMETHOD(InitPlay)(THIS_ IDirectMusicSegmentState*,IDirectMusicPerformance*,void**,DWORD,DWORD) PURE;
    STDMETHOD(EndPlay)(THIS_ void*) PURE;
    STDMETHOD(Play)(THIS_ void*,MUSIC_TIME,MUSIC_TIME,MUSIC_TIME,DWORD,IDirectMusicPerformance*,IDirectMusicSegmentState*,DWORD) PURE;
    STDMETHOD(GetParam)(THIS_ REFGUID,MUSIC_TIME,MUSIC_TIME*,void*) PURE;
    STDMETHOD(SetParam)(THIS_ REFGUID,MUSIC_TIME,void*) PURE;
    STDMETHOD(IsParamSupported)(THIS_ REFGUID) PURE;
    STDMETHOD(AddNotificationType)(THIS_ REFGUID) PURE;
    STDMETHOD(RemoveNotificationType)(THIS_ REFGUID) PURE;
    STDMETHOD(Clone)(THIS_ MUSIC_TIME,MUSIC_TIME,IDirectMusicTrack**) PURE;
    STDMETHOD(PlayEx)(THIS_ void*,REFERENCE_TIME,REFERENCE_TIME,REFERENCE_TIME,DWORD,IDirectMusicPerformance*,IDirectMusicSegmentState*,DWORD) PURE;
    STDMETHOD(GetParamEx)(THIS_ REFGUID,REFERENCE_TIME,REFERENCE_TIME*,void*,void*,DWORD) PURE;
    STDMETHOD(SetParamEx)(THIS_ REFGUID,REFERENCE_TIME,void*,void*,DWORD) PURE;
    STDMETHOD(Compose)(THIS_ IUnknown*,DWORD,IDirectMusicTrack**) PURE;
    STDMETHOD(Join)(THIS_ IDirectMusicTrack*,MUSIC_TIME,IUnknown*,DWORD,IDirectMusicTrack**) PURE;
};

#ifdef __cplusplus
};
#endif

#include <poppack.h>

#endif /* _DMPLUGIN_H */
