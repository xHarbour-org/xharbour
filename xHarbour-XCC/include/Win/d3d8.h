#ifndef _D3D8_H
#define _D3D8_H

/* Direct3D include file */

#ifndef DIRECT3D_VERSION
#define DIRECT3D_VERSION  0x0800
#endif

#if (DIRECT3D_VERSION >= 0x0800)

#define D3D_SDK_VERSION  120

#include <stdlib.h>

#define COM_NO_WINDOWS_H
#include <objbase.h>
#include <windows.h>

#if !defined(HMONITOR_DECLARED) && (WINVER < 0x0500)
#define HMONITOR_DECLARED
DECLARE_HANDLE(HMONITOR);
#endif

#define D3DAPI WINAPI

#define D3DSPD_IUNKNOWN  0x00000001L

#define D3DCURRENT_DISPLAY_MODE  0x00EFFFFFL

#define D3DCREATE_FPU_PRESERVE  0x00000002L
#define D3DCREATE_MULTITHREADED  0x00000004L
#define D3DCREATE_PUREDEVICE  0x00000010L
#define D3DCREATE_SOFTWARE_VERTEXPROCESSING  0x00000020L
#define D3DCREATE_HARDWARE_VERTEXPROCESSING  0x00000040L
#define D3DCREATE_MIXED_VERTEXPROCESSING  0x00000080L

#define D3DADAPTER_DEFAULT  0

#define D3DENUM_NO_WHQL_LEVEL  0x00000002L

#define D3DPRESENT_BACK_BUFFERS_MAX  3L

#define D3DSGR_NO_CALIBRATION  0x00000000L
#define D3DSGR_CALIBRATE  0x00000001L

#define D3DCURSOR_IMMEDIATE_UPDATE  0x00000001L

#define _FACD3D  0x876
#define MAKE_D3DHRESULT(code)  MAKE_HRESULT(1,_FACD3D,code)

#define D3D_OK  S_OK

#define D3DERR_WRONGTEXTUREFORMAT  MAKE_D3DHRESULT(2072)
#define D3DERR_UNSUPPORTEDCOLOROPERATION  MAKE_D3DHRESULT(2073)
#define D3DERR_UNSUPPORTEDCOLORARG  MAKE_D3DHRESULT(2074)
#define D3DERR_UNSUPPORTEDALPHAOPERATION  MAKE_D3DHRESULT(2075)
#define D3DERR_UNSUPPORTEDALPHAARG  MAKE_D3DHRESULT(2076)
#define D3DERR_TOOMANYOPERATIONS  MAKE_D3DHRESULT(2077)
#define D3DERR_CONFLICTINGTEXTUREFILTER  MAKE_D3DHRESULT(2078)
#define D3DERR_UNSUPPORTEDFACTORVALUE  MAKE_D3DHRESULT(2079)
#define D3DERR_CONFLICTINGRENDERSTATE  MAKE_D3DHRESULT(2081)
#define D3DERR_UNSUPPORTEDTEXTUREFILTER  MAKE_D3DHRESULT(2082)
#define D3DERR_CONFLICTINGTEXTUREPALETTE  MAKE_D3DHRESULT(2086)
#define D3DERR_DRIVERINTERNALERROR  MAKE_D3DHRESULT(2087)
#define D3DERR_NOTFOUND  MAKE_D3DHRESULT(2150)
#define D3DERR_MOREDATA  MAKE_D3DHRESULT(2151)
#define D3DERR_DEVICELOST  MAKE_D3DHRESULT(2152)
#define D3DERR_DEVICENOTRESET  MAKE_D3DHRESULT(2153)
#define D3DERR_NOTAVAILABLE  MAKE_D3DHRESULT(2154)
#define D3DERR_OUTOFVIDEOMEMORY  MAKE_D3DHRESULT(380)
#define D3DERR_INVALIDDEVICE  MAKE_D3DHRESULT(2155)
#define D3DERR_INVALIDCALL  MAKE_D3DHRESULT(2156)
#define D3DERR_DRIVERINVALIDCALL  MAKE_D3DHRESULT(2157)

#if defined(_WIN32) && !defined(_NO_COM)
DEFINE_GUID(IID_IDirect3D8,0x1dd9e8da,0x1c77,0x4d40,0xb0,0xcf,0x98,0xfe,0xfd,0xff,0x95,0x12);
DEFINE_GUID(IID_IDirect3DDevice8,0x7385e5df,0x8fe8,0x41d5,0x86,0xb6,0xd7,0xb4,0x85,0x47,0xb6,0xcf);
DEFINE_GUID(IID_IDirect3DResource8,0x1b36bb7b,0x9b7,0x410a,0xb4,0x45,0x7d,0x14,0x30,0xd7,0xb3,0x3f);
DEFINE_GUID(IID_IDirect3DBaseTexture8,0xb4211cfa,0x51b9,0x4a9f,0xab,0x78,0xdb,0x99,0xb2,0xbb,0x67,0x8e);
DEFINE_GUID(IID_IDirect3DTexture8,0xe4cdd575,0x2866,0x4f01,0xb1,0x2e,0x7e,0xec,0xe1,0xec,0x93,0x58);
DEFINE_GUID(IID_IDirect3DCubeTexture8,0x3ee5b968,0x2aca,0x4c34,0x8b,0xb5,0x7e,0x0c,0x3d,0x19,0xb7,0x50);
DEFINE_GUID(IID_IDirect3DVolumeTexture8,0x4b8aaafa,0x140f,0x42ba,0x91,0x31,0x59,0x7e,0xaf,0xaa,0x2e,0xad);
DEFINE_GUID(IID_IDirect3DVertexBuffer8,0x8aeeeac7,0x05f9,0x44d4,0xb5,0x91,0x00,0x0b,0x0d,0xf1,0xcb,0x95);
DEFINE_GUID(IID_IDirect3DIndexBuffer8,0x0e689c9a,0x053d,0x44a0,0x9d,0x92,0xdb,0x0e,0x3d,0x75,0x0f,0x86);
DEFINE_GUID(IID_IDirect3DSurface8,0xb96eebca,0xb326,0x4ea5,0x88,0x2f,0x2f,0xf5,0xba,0xe0,0x21,0xdd);
DEFINE_GUID(IID_IDirect3DVolume8,0xbd7349f5,0x14f1,0x42e4,0x9c,0x79,0x97,0x23,0x80,0xdb,0x40,0xc0);
DEFINE_GUID(IID_IDirect3DSwapChain8,0x928c088b,0x76b9,0x4c6b,0xa5,0x36,0xa5,0x90,0x85,0x38,0x76,0xcd);
#endif

#ifdef __cplusplus
interface IDirect3D8;
interface IDirect3DDevice8;
interface IDirect3DResource8;
interface IDirect3DBaseTexture8;
interface IDirect3DTexture8;
interface IDirect3DVolumeTexture8;
interface IDirect3DCubeTexture8;
interface IDirect3DVertexBuffer8;
interface IDirect3DIndexBuffer8;
interface IDirect3DSurface8;
interface IDirect3DVolume8;
interface IDirect3DSwapChain8;
#endif

typedef interface IDirect3D8  IDirect3D8;
typedef interface IDirect3DDevice8  IDirect3DDevice8;
typedef interface IDirect3DResource8  IDirect3DResource8;
typedef interface IDirect3DBaseTexture8  IDirect3DBaseTexture8;
typedef interface IDirect3DTexture8  IDirect3DTexture8;
typedef interface IDirect3DVolumeTexture8  IDirect3DVolumeTexture8;
typedef interface IDirect3DCubeTexture8  IDirect3DCubeTexture8;
typedef interface IDirect3DVertexBuffer8  IDirect3DVertexBuffer8;
typedef interface IDirect3DIndexBuffer8  IDirect3DIndexBuffer8;
typedef interface IDirect3DSurface8  IDirect3DSurface8;
typedef interface IDirect3DVolume8  IDirect3DVolume8;
typedef interface IDirect3DSwapChain8  IDirect3DSwapChain8;

#include "d3d8types.h"
#include "d3d8caps.h"

#ifdef __cplusplus
extern "C" {
#endif

IDirect3D8 * WINAPI Direct3DCreate8(UINT);

#undef INTERFACE
#define INTERFACE IDirect3D8
DECLARE_INTERFACE_(IDirect3D8,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(RegisterSoftwareDevice)(THIS_ void*) PURE;
    STDMETHOD_(UINT,GetAdapterCount)(THIS) PURE;
    STDMETHOD(GetAdapterIdentifier)(THIS_ UINT,DWORD,D3DADAPTER_IDENTIFIER8*) PURE;
    STDMETHOD_(UINT,GetAdapterModeCount)(THIS_ UINT) PURE;
    STDMETHOD(EnumAdapterModes)(THIS_ UINT,UINT,D3DDISPLAYMODE*) PURE;
    STDMETHOD(GetAdapterDisplayMode)(THIS_ UINT,D3DDISPLAYMODE*) PURE;
    STDMETHOD(CheckDeviceType)(THIS_ UINT,D3DDEVTYPE,D3DFORMAT,D3DFORMAT,BOOL) PURE;
    STDMETHOD(CheckDeviceFormat)(THIS_ UINT,D3DDEVTYPE,D3DFORMAT,DWORD,D3DRESOURCETYPE,D3DFORMAT) PURE;
    STDMETHOD(CheckDeviceMultiSampleType)(THIS_ UINT,D3DDEVTYPE,D3DFORMAT,BOOL,D3DMULTISAMPLE_TYPE) PURE;
    STDMETHOD(CheckDepthStencilMatch)(THIS_ UINT,D3DDEVTYPE,D3DFORMAT,D3DFORMAT,D3DFORMAT) PURE;
    STDMETHOD(GetDeviceCaps)(THIS_ UINT,D3DDEVTYPE,D3DCAPS8*) PURE;
    STDMETHOD_(HMONITOR,GetAdapterMonitor)(THIS_ UINT) PURE;
    STDMETHOD(CreateDevice)(THIS_ UINT,D3DDEVTYPE,HWND,DWORD,D3DPRESENT_PARAMETERS*,IDirect3DDevice8**) PURE;
};
typedef struct IDirect3D8 *LPDIRECT3D8, *PDIRECT3D8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3D8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3D8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3D8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3D8_RegisterSoftwareDevice(p,a)  (p)->lpVtbl->RegisterSoftwareDevice(p,a)
#define IDirect3D8_GetAdapterCount(p)  (p)->lpVtbl->GetAdapterCount(p)
#define IDirect3D8_GetAdapterIdentifier(p,a,b,c)  (p)->lpVtbl->GetAdapterIdentifier(p,a,b,c)
#define IDirect3D8_GetAdapterModeCount(p,a)  (p)->lpVtbl->GetAdapterModeCount(p,a)
#define IDirect3D8_EnumAdapterModes(p,a,b,c)  (p)->lpVtbl->EnumAdapterModes(p,a,b,c)
#define IDirect3D8_GetAdapterDisplayMode(p,a,b)  (p)->lpVtbl->GetAdapterDisplayMode(p,a,b)
#define IDirect3D8_CheckDeviceType(p,a,b,c,d,e)  (p)->lpVtbl->CheckDeviceType(p,a,b,c,d,e)
#define IDirect3D8_CheckDeviceFormat(p,a,b,c,d,e,f)  (p)->lpVtbl->CheckDeviceFormat(p,a,b,c,d,e,f)
#define IDirect3D8_CheckDeviceMultiSampleType(p,a,b,c,d,e)  (p)->lpVtbl->CheckDeviceMultiSampleType(p,a,b,c,d,e)
#define IDirect3D8_CheckDepthStencilMatch(p,a,b,c,d,e)  (p)->lpVtbl->CheckDepthStencilMatch(p,a,b,c,d,e)
#define IDirect3D8_GetDeviceCaps(p,a,b,c)  (p)->lpVtbl->GetDeviceCaps(p,a,b,c)
#define IDirect3D8_GetAdapterMonitor(p,a)  (p)->lpVtbl->GetAdapterMonitor(p,a)
#define IDirect3D8_CreateDevice(p,a,b,c,d,e,f)  (p)->lpVtbl->CreateDevice(p,a,b,c,d,e,f)
#else
#define IDirect3D8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3D8_AddRef(p)  (p)->AddRef()
#define IDirect3D8_Release(p)  (p)->Release()
#define IDirect3D8_RegisterSoftwareDevice(p,a)  (p)->RegisterSoftwareDevice(a)
#define IDirect3D8_GetAdapterCount(p)  (p)->GetAdapterCount()
#define IDirect3D8_GetAdapterIdentifier(p,a,b,c)  (p)->GetAdapterIdentifier(a,b,c)
#define IDirect3D8_GetAdapterModeCount(p,a)  (p)->GetAdapterModeCount(a)
#define IDirect3D8_EnumAdapterModes(p,a,b,c)  (p)->EnumAdapterModes(a,b,c)
#define IDirect3D8_GetAdapterDisplayMode(p,a,b)  (p)->GetAdapterDisplayMode(a,b)
#define IDirect3D8_CheckDeviceType(p,a,b,c,d,e)  (p)->CheckDeviceType(a,b,c,d,e)
#define IDirect3D8_CheckDeviceFormat(p,a,b,c,d,e,f)  (p)->CheckDeviceFormat(a,b,c,d,e,f)
#define IDirect3D8_CheckDeviceMultiSampleType(p,a,b,c,d,e)  (p)->CheckDeviceMultiSampleType(a,b,c,d,e)
#define IDirect3D8_CheckDepthStencilMatch(p,a,b,c,d,e)  (p)->CheckDepthStencilMatch(a,b,c,d,e)
#define IDirect3D8_GetDeviceCaps(p,a,b,c)  (p)->GetDeviceCaps(a,b,c)
#define IDirect3D8_GetAdapterMonitor(p,a)  (p)->GetAdapterMonitor(a)
#define IDirect3D8_CreateDevice(p,a,b,c,d,e,f)  (p)->CreateDevice(a,b,c,d,e,f)
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DDevice8
DECLARE_INTERFACE_(IDirect3DDevice8,IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(TestCooperativeLevel)(THIS) PURE;
    STDMETHOD_(UINT,GetAvailableTextureMem)(THIS) PURE;
    STDMETHOD(ResourceManagerDiscardBytes)(THIS_ DWORD) PURE;
    STDMETHOD(GetDirect3D)(THIS_ IDirect3D8**) PURE;
    STDMETHOD(GetDeviceCaps)(THIS_ D3DCAPS8*) PURE;
    STDMETHOD(GetDisplayMode)(THIS_ D3DDISPLAYMODE*) PURE;
    STDMETHOD(GetCreationParameters)(THIS_ D3DDEVICE_CREATION_PARAMETERS*) PURE;
    STDMETHOD(SetCursorProperties)(THIS_ UINT,UINT,IDirect3DSurface8*) PURE;
    STDMETHOD_(void,SetCursorPosition)(THIS_ UINT,UINT,DWORD) PURE;
    STDMETHOD_(BOOL,ShowCursor)(THIS_ BOOL) PURE;
    STDMETHOD(CreateAdditionalSwapChain)(THIS_ D3DPRESENT_PARAMETERS*,IDirect3DSwapChain8**) PURE;
    STDMETHOD(Reset)(THIS_ D3DPRESENT_PARAMETERS*) PURE;
    STDMETHOD(Present)(THIS_ CONST RECT*,CONST RECT*,HWND,CONST RGNDATA*) PURE;
    STDMETHOD(GetBackBuffer)(THIS_ UINT,D3DBACKBUFFER_TYPE,IDirect3DSurface8**) PURE;
    STDMETHOD(GetRasterStatus)(THIS_ D3DRASTER_STATUS*) PURE;
    STDMETHOD_(void,SetGammaRamp)(THIS_ DWORD,CONST D3DGAMMARAMP*) PURE;
    STDMETHOD_(void,GetGammaRamp)(THIS_ D3DGAMMARAMP*) PURE;
    STDMETHOD(CreateTexture)(THIS_ UINT,UINT,UINT,DWORD,D3DFORMAT,D3DPOOL,IDirect3DTexture8**) PURE;
    STDMETHOD(CreateVolumeTexture)(THIS_ UINT,UINT,UINT,UINT,DWORD,D3DFORMAT,D3DPOOL,IDirect3DVolumeTexture8**) PURE;
    STDMETHOD(CreateCubeTexture)(THIS_ UINT,UINT,DWORD,D3DFORMAT,D3DPOOL,IDirect3DCubeTexture8**) PURE;
    STDMETHOD(CreateVertexBuffer)(THIS_ UINT,DWORD,DWORD,D3DPOOL,IDirect3DVertexBuffer8**) PURE;
    STDMETHOD(CreateIndexBuffer)(THIS_ UINT,DWORD,D3DFORMAT,D3DPOOL,IDirect3DIndexBuffer8**) PURE;
    STDMETHOD(CreateRenderTarget)(THIS_ UINT,UINT,D3DFORMAT,D3DMULTISAMPLE_TYPE,BOOL,IDirect3DSurface8**) PURE;
    STDMETHOD(CreateDepthStencilSurface)(THIS_ UINT,UINT,D3DFORMAT,D3DMULTISAMPLE_TYPE,IDirect3DSurface8**) PURE;
    STDMETHOD(CreateImageSurface)(THIS_ UINT,UINT,D3DFORMAT,IDirect3DSurface8**) PURE;
    STDMETHOD(CopyRects)(THIS_ IDirect3DSurface8*,CONST RECT*,UINT,IDirect3DSurface8*,CONST POINT*) PURE;
    STDMETHOD(UpdateTexture)(THIS_ IDirect3DBaseTexture8*,IDirect3DBaseTexture8*) PURE;
    STDMETHOD(GetFrontBuffer)(THIS_ IDirect3DSurface8*) PURE;
    STDMETHOD(SetRenderTarget)(THIS_ IDirect3DSurface8*,IDirect3DSurface8*) PURE;
    STDMETHOD(GetRenderTarget)(THIS_ IDirect3DSurface8**) PURE;
    STDMETHOD(GetDepthStencilSurface)(THIS_ IDirect3DSurface8**) PURE;
    STDMETHOD(BeginScene)(THIS) PURE;
    STDMETHOD(EndScene)(THIS) PURE;
    STDMETHOD(Clear)(THIS_ DWORD,CONST D3DRECT*,DWORD,D3DCOLOR,float,DWORD) PURE;
    STDMETHOD(SetTransform)(THIS_ D3DTRANSFORMSTATETYPE,CONST D3DMATRIX*) PURE;
    STDMETHOD(GetTransform)(THIS_ D3DTRANSFORMSTATETYPE,D3DMATRIX*) PURE;
    STDMETHOD(MultiplyTransform)(THIS_ D3DTRANSFORMSTATETYPE,CONST D3DMATRIX*) PURE;
    STDMETHOD(SetViewport)(THIS_ CONST D3DVIEWPORT8*) PURE;
    STDMETHOD(GetViewport)(THIS_ D3DVIEWPORT8*) PURE;
    STDMETHOD(SetMaterial)(THIS_ CONST D3DMATERIAL8*) PURE;
    STDMETHOD(GetMaterial)(THIS_ D3DMATERIAL8*) PURE;
    STDMETHOD(SetLight)(THIS_ DWORD Index,CONST D3DLIGHT8*) PURE;
    STDMETHOD(GetLight)(THIS_ DWORD Index,D3DLIGHT8*) PURE;
    STDMETHOD(LightEnable)(THIS_ DWORD Index,BOOL) PURE;
    STDMETHOD(GetLightEnable)(THIS_ DWORD,BOOL*) PURE;
    STDMETHOD(SetClipPlane)(THIS_ DWORD,CONST float*) PURE;
    STDMETHOD(GetClipPlane)(THIS_ DWORD,float*) PURE;
    STDMETHOD(SetRenderState)(THIS_ D3DRENDERSTATETYPE,DWORD) PURE;
    STDMETHOD(GetRenderState)(THIS_ D3DRENDERSTATETYPE,DWORD*) PURE;
    STDMETHOD(BeginStateBlock)(THIS) PURE;
    STDMETHOD(EndStateBlock)(THIS_ DWORD*) PURE;
    STDMETHOD(ApplyStateBlock)(THIS_ DWORD) PURE;
    STDMETHOD(CaptureStateBlock)(THIS_ DWORD) PURE;
    STDMETHOD(DeleteStateBlock)(THIS_ DWORD) PURE;
    STDMETHOD(CreateStateBlock)(THIS_ D3DSTATEBLOCKTYPE,DWORD*) PURE;
    STDMETHOD(SetClipStatus)(THIS_ CONST D3DCLIPSTATUS8*) PURE;
    STDMETHOD(GetClipStatus)(THIS_ D3DCLIPSTATUS8*) PURE;
    STDMETHOD(GetTexture)(THIS_ DWORD,IDirect3DBaseTexture8**) PURE;
    STDMETHOD(SetTexture)(THIS_ DWORD,IDirect3DBaseTexture8*) PURE;
    STDMETHOD(GetTextureStageState)(THIS_ DWORD,D3DTEXTURESTAGESTATETYPE,DWORD*) PURE;
    STDMETHOD(SetTextureStageState)(THIS_ DWORD,D3DTEXTURESTAGESTATETYPE,DWORD) PURE;
    STDMETHOD(ValidateDevice)(THIS_ DWORD*) PURE;
    STDMETHOD(GetInfo)(THIS_ DWORD,void*,DWORD) PURE;
    STDMETHOD(SetPaletteEntries)(THIS_ UINT,CONST PALETTEENTRY*) PURE;
    STDMETHOD(GetPaletteEntries)(THIS_ UINT,PALETTEENTRY*) PURE;
    STDMETHOD(SetCurrentTexturePalette)(THIS_ UINT) PURE;
    STDMETHOD(GetCurrentTexturePalette)(THIS_ UINT*) PURE;
    STDMETHOD(DrawPrimitive)(THIS_ D3DPRIMITIVETYPE,UINT,UINT) PURE;
    STDMETHOD(DrawIndexedPrimitive)(THIS_ D3DPRIMITIVETYPE,UINT,UINT,UINT,UINT) PURE;
    STDMETHOD(DrawPrimitiveUP)(THIS_ D3DPRIMITIVETYPE,UINT,CONST void*,UINT) PURE;
    STDMETHOD(DrawIndexedPrimitiveUP)(THIS_ D3DPRIMITIVETYPE,UINT,UINT,UINT,CONST void*,D3DFORMAT,CONST void*,UINT) PURE;
    STDMETHOD(ProcessVertices)(THIS_ UINT,UINT,UINT,IDirect3DVertexBuffer8*,DWORD) PURE;
    STDMETHOD(CreateVertexShader)(THIS_ CONST DWORD*,CONST DWORD*,DWORD*,DWORD) PURE;
    STDMETHOD(SetVertexShader)(THIS_ DWORD) PURE;
    STDMETHOD(GetVertexShader)(THIS_ DWORD*) PURE;
    STDMETHOD(DeleteVertexShader)(THIS_ DWORD) PURE;
    STDMETHOD(SetVertexShaderConstant)(THIS_ DWORD,CONST void*,DWORD) PURE;
    STDMETHOD(GetVertexShaderConstant)(THIS_ DWORD,void*,DWORD) PURE;
    STDMETHOD(GetVertexShaderDeclaration)(THIS_ DWORD,void*,DWORD*) PURE;
    STDMETHOD(GetVertexShaderFunction)(THIS_ DWORD,void*,DWORD*) PURE;
    STDMETHOD(SetStreamSource)(THIS_ UINT,IDirect3DVertexBuffer8*,UINT) PURE;
    STDMETHOD(GetStreamSource)(THIS_ UINT,IDirect3DVertexBuffer8**,UINT*) PURE;
    STDMETHOD(SetIndices)(THIS_ IDirect3DIndexBuffer8*,UINT) PURE;
    STDMETHOD(GetIndices)(THIS_ IDirect3DIndexBuffer8**,UINT*) PURE;
    STDMETHOD(CreatePixelShader)(THIS_ CONST DWORD*,DWORD*) PURE;
    STDMETHOD(SetPixelShader)(THIS_ DWORD) PURE;
    STDMETHOD(GetPixelShader)(THIS_ DWORD*) PURE;
    STDMETHOD(DeletePixelShader)(THIS_ DWORD) PURE;
    STDMETHOD(SetPixelShaderConstant)(THIS_ DWORD,CONST void*,DWORD) PURE;
    STDMETHOD(GetPixelShaderConstant)(THIS_ DWORD,void*,DWORD) PURE;
    STDMETHOD(GetPixelShaderFunction)(THIS_ DWORD,void*,DWORD*) PURE;
    STDMETHOD(DrawRectPatch)(THIS_ UINT,CONST float*,CONST D3DRECTPATCH_INFO*) PURE;
    STDMETHOD(DrawTriPatch)(THIS_ UINT,CONST float*,CONST D3DTRIPATCH_INFO*) PURE;
    STDMETHOD(DeletePatch)(THIS_ UINT) PURE;
};
typedef struct IDirect3DDevice8 *LPDIRECT3DDEVICE8, *PDIRECT3DDEVICE8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DDevice8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DDevice8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DDevice8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DDevice8_TestCooperativeLevel(p)  (p)->lpVtbl->TestCooperativeLevel(p)
#define IDirect3DDevice8_GetAvailableTextureMem(p)  (p)->lpVtbl->GetAvailableTextureMem(p)
#define IDirect3DDevice8_ResourceManagerDiscardBytes(p,a)  (p)->lpVtbl->ResourceManagerDiscardBytes(p,a)
#define IDirect3DDevice8_GetDirect3D(p,a)  (p)->lpVtbl->GetDirect3D(p,a)
#define IDirect3DDevice8_GetDeviceCaps(p,a)  (p)->lpVtbl->GetDeviceCaps(p,a)
#define IDirect3DDevice8_GetDisplayMode(p,a)  (p)->lpVtbl->GetDisplayMode(p,a)
#define IDirect3DDevice8_GetCreationParameters(p,a)  (p)->lpVtbl->GetCreationParameters(p,a)
#define IDirect3DDevice8_SetCursorProperties(p,a,b,c)  (p)->lpVtbl->SetCursorProperties(p,a,b,c)
#define IDirect3DDevice8_SetCursorPosition(p,a,b,c)  (p)->lpVtbl->SetCursorPosition(p,a,b,c)
#define IDirect3DDevice8_ShowCursor(p,a)  (p)->lpVtbl->ShowCursor(p,a)
#define IDirect3DDevice8_CreateAdditionalSwapChain(p,a,b)  (p)->lpVtbl->CreateAdditionalSwapChain(p,a,b)
#define IDirect3DDevice8_Reset(p,a)  (p)->lpVtbl->Reset(p,a)
#define IDirect3DDevice8_Present(p,a,b,c,d)  (p)->lpVtbl->Present(p,a,b,c,d)
#define IDirect3DDevice8_GetBackBuffer(p,a,b,c)  (p)->lpVtbl->GetBackBuffer(p,a,b,c)
#define IDirect3DDevice8_GetRasterStatus(p,a)  (p)->lpVtbl->GetRasterStatus(p,a)
#define IDirect3DDevice8_SetGammaRamp(p,a,b)  (p)->lpVtbl->SetGammaRamp(p,a,b)
#define IDirect3DDevice8_GetGammaRamp(p,a)  (p)->lpVtbl->GetGammaRamp(p,a)
#define IDirect3DDevice8_CreateTexture(p,a,b,c,d,e,f,g)  (p)->lpVtbl->CreateTexture(p,a,b,c,d,e,f,g)
#define IDirect3DDevice8_CreateVolumeTexture(p,a,b,c,d,e,f,g,h)  (p)->lpVtbl->CreateVolumeTexture(p,a,b,c,d,e,f,g,h)
#define IDirect3DDevice8_CreateCubeTexture(p,a,b,c,d,e,f)  (p)->lpVtbl->CreateCubeTexture(p,a,b,c,d,e,f)
#define IDirect3DDevice8_CreateVertexBuffer(p,a,b,c,d,e)  (p)->lpVtbl->CreateVertexBuffer(p,a,b,c,d,e)
#define IDirect3DDevice8_CreateIndexBuffer(p,a,b,c,d,e)  (p)->lpVtbl->CreateIndexBuffer(p,a,b,c,d,e)
#define IDirect3DDevice8_CreateRenderTarget(p,a,b,c,d,e,f)  (p)->lpVtbl->CreateRenderTarget(p,a,b,c,d,e,f)
#define IDirect3DDevice8_CreateDepthStencilSurface(p,a,b,c,d,e)  (p)->lpVtbl->CreateDepthStencilSurface(p,a,b,c,d,e)
#define IDirect3DDevice8_CreateImageSurface(p,a,b,c,d)  (p)->lpVtbl->CreateImageSurface(p,a,b,c,d)
#define IDirect3DDevice8_CopyRects(p,a,b,c,d,e)  (p)->lpVtbl->CopyRects(p,a,b,c,d,e)
#define IDirect3DDevice8_UpdateTexture(p,a,b)  (p)->lpVtbl->UpdateTexture(p,a,b)
#define IDirect3DDevice8_GetFrontBuffer(p,a)  (p)->lpVtbl->GetFrontBuffer(p,a)
#define IDirect3DDevice8_SetRenderTarget(p,a,b)  (p)->lpVtbl->SetRenderTarget(p,a,b)
#define IDirect3DDevice8_GetRenderTarget(p,a)  (p)->lpVtbl->GetRenderTarget(p,a)
#define IDirect3DDevice8_GetDepthStencilSurface(p,a)  (p)->lpVtbl->GetDepthStencilSurface(p,a)
#define IDirect3DDevice8_BeginScene(p)  (p)->lpVtbl->BeginScene(p)
#define IDirect3DDevice8_EndScene(p)  (p)->lpVtbl->EndScene(p)
#define IDirect3DDevice8_Clear(p,a,b,c,d,e,f)  (p)->lpVtbl->Clear(p,a,b,c,d,e,f)
#define IDirect3DDevice8_SetTransform(p,a,b)  (p)->lpVtbl->SetTransform(p,a,b)
#define IDirect3DDevice8_GetTransform(p,a,b)  (p)->lpVtbl->GetTransform(p,a,b)
#define IDirect3DDevice8_MultiplyTransform(p,a,b)  (p)->lpVtbl->MultiplyTransform(p,a,b)
#define IDirect3DDevice8_SetViewport(p,a)  (p)->lpVtbl->SetViewport(p,a)
#define IDirect3DDevice8_GetViewport(p,a)  (p)->lpVtbl->GetViewport(p,a)
#define IDirect3DDevice8_SetMaterial(p,a)  (p)->lpVtbl->SetMaterial(p,a)
#define IDirect3DDevice8_GetMaterial(p,a)  (p)->lpVtbl->GetMaterial(p,a)
#define IDirect3DDevice8_SetLight(p,a,b)  (p)->lpVtbl->SetLight(p,a,b)
#define IDirect3DDevice8_GetLight(p,a,b)  (p)->lpVtbl->GetLight(p,a,b)
#define IDirect3DDevice8_LightEnable(p,a,b)  (p)->lpVtbl->LightEnable(p,a,b)
#define IDirect3DDevice8_GetLightEnable(p,a,b)  (p)->lpVtbl->GetLightEnable(p,a,b)
#define IDirect3DDevice8_SetClipPlane(p,a,b)  (p)->lpVtbl->SetClipPlane(p,a,b)
#define IDirect3DDevice8_GetClipPlane(p,a,b)  (p)->lpVtbl->GetClipPlane(p,a,b)
#define IDirect3DDevice8_SetRenderState(p,a,b)  (p)->lpVtbl->SetRenderState(p,a,b)
#define IDirect3DDevice8_GetRenderState(p,a,b)  (p)->lpVtbl->GetRenderState(p,a,b)
#define IDirect3DDevice8_BeginStateBlock(p)  (p)->lpVtbl->BeginStateBlock(p)
#define IDirect3DDevice8_EndStateBlock(p,a)  (p)->lpVtbl->EndStateBlock(p,a)
#define IDirect3DDevice8_ApplyStateBlock(p,a)  (p)->lpVtbl->ApplyStateBlock(p,a)
#define IDirect3DDevice8_CaptureStateBlock(p,a)  (p)->lpVtbl->CaptureStateBlock(p,a)
#define IDirect3DDevice8_DeleteStateBlock(p,a)  (p)->lpVtbl->DeleteStateBlock(p,a)
#define IDirect3DDevice8_CreateStateBlock(p,a,b)  (p)->lpVtbl->CreateStateBlock(p,a,b)
#define IDirect3DDevice8_SetClipStatus(p,a)  (p)->lpVtbl->SetClipStatus(p,a)
#define IDirect3DDevice8_GetClipStatus(p,a)  (p)->lpVtbl->GetClipStatus(p,a)
#define IDirect3DDevice8_GetTexture(p,a,b)  (p)->lpVtbl->GetTexture(p,a,b)
#define IDirect3DDevice8_SetTexture(p,a,b)  (p)->lpVtbl->SetTexture(p,a,b)
#define IDirect3DDevice8_GetTextureStageState(p,a,b,c)  (p)->lpVtbl->GetTextureStageState(p,a,b,c)
#define IDirect3DDevice8_SetTextureStageState(p,a,b,c)  (p)->lpVtbl->SetTextureStageState(p,a,b,c)
#define IDirect3DDevice8_ValidateDevice(p,a)  (p)->lpVtbl->ValidateDevice(p,a)
#define IDirect3DDevice8_GetInfo(p,a,b,c)  (p)->lpVtbl->GetInfo(p,a,b,c)
#define IDirect3DDevice8_SetPaletteEntries(p,a,b)  (p)->lpVtbl->SetPaletteEntries(p,a,b)
#define IDirect3DDevice8_GetPaletteEntries(p,a,b)  (p)->lpVtbl->GetPaletteEntries(p,a,b)
#define IDirect3DDevice8_SetCurrentTexturePalette(p,a)  (p)->lpVtbl->SetCurrentTexturePalette(p,a)
#define IDirect3DDevice8_GetCurrentTexturePalette(p,a)  (p)->lpVtbl->GetCurrentTexturePalette(p,a)
#define IDirect3DDevice8_DrawPrimitive(p,a,b,c)  (p)->lpVtbl->DrawPrimitive(p,a,b,c)
#define IDirect3DDevice8_DrawIndexedPrimitive(p,a,b,c,d,e)  (p)->lpVtbl->DrawIndexedPrimitive(p,a,b,c,d,e)
#define IDirect3DDevice8_DrawPrimitiveUP(p,a,b,c,d)  (p)->lpVtbl->DrawPrimitiveUP(p,a,b,c,d)
#define IDirect3DDevice8_DrawIndexedPrimitiveUP(p,a,b,c,d,e,f,g,h)  (p)->lpVtbl->DrawIndexedPrimitiveUP(p,a,b,c,d,e,f,g,h)
#define IDirect3DDevice8_ProcessVertices(p,a,b,c,d,e)  (p)->lpVtbl->ProcessVertices(p,a,b,c,d,e)
#define IDirect3DDevice8_CreateVertexShader(p,a,b,c,d)  (p)->lpVtbl->CreateVertexShader(p,a,b,c,d)
#define IDirect3DDevice8_SetVertexShader(p,a)  (p)->lpVtbl->SetVertexShader(p,a)
#define IDirect3DDevice8_GetVertexShader(p,a)  (p)->lpVtbl->GetVertexShader(p,a)
#define IDirect3DDevice8_DeleteVertexShader(p,a)  (p)->lpVtbl->DeleteVertexShader(p,a)
#define IDirect3DDevice8_SetVertexShaderConstant(p,a,b,c)  (p)->lpVtbl->SetVertexShaderConstant(p,a,b,c)
#define IDirect3DDevice8_GetVertexShaderConstant(p,a,b,c)  (p)->lpVtbl->GetVertexShaderConstant(p,a,b,c)
#define IDirect3DDevice8_GetVertexShaderDeclaration(p,a,b,c)  (p)->lpVtbl->GetVertexShaderDeclaration(p,a,b,c)
#define IDirect3DDevice8_GetVertexShaderFunction(p,a,b,c)  (p)->lpVtbl->GetVertexShaderFunction(p,a,b,c)
#define IDirect3DDevice8_SetStreamSource(p,a,b,c)  (p)->lpVtbl->SetStreamSource(p,a,b,c)
#define IDirect3DDevice8_GetStreamSource(p,a,b,c)  (p)->lpVtbl->GetStreamSource(p,a,b,c)
#define IDirect3DDevice8_SetIndices(p,a,b)  (p)->lpVtbl->SetIndices(p,a,b)
#define IDirect3DDevice8_GetIndices(p,a,b)  (p)->lpVtbl->GetIndices(p,a,b)
#define IDirect3DDevice8_CreatePixelShader(p,a,b)  (p)->lpVtbl->CreatePixelShader(p,a,b)
#define IDirect3DDevice8_SetPixelShader(p,a)  (p)->lpVtbl->SetPixelShader(p,a)
#define IDirect3DDevice8_GetPixelShader(p,a)  (p)->lpVtbl->GetPixelShader(p,a)
#define IDirect3DDevice8_DeletePixelShader(p,a)  (p)->lpVtbl->DeletePixelShader(p,a)
#define IDirect3DDevice8_SetPixelShaderConstant(p,a,b,c)  (p)->lpVtbl->SetPixelShaderConstant(p,a,b,c)
#define IDirect3DDevice8_GetPixelShaderConstant(p,a,b,c)  (p)->lpVtbl->GetPixelShaderConstant(p,a,b,c)
#define IDirect3DDevice8_GetPixelShaderFunction(p,a,b,c)  (p)->lpVtbl->GetPixelShaderFunction(p,a,b,c)
#define IDirect3DDevice8_DrawRectPatch(p,a,b,c)  (p)->lpVtbl->DrawRectPatch(p,a,b,c)
#define IDirect3DDevice8_DrawTriPatch(p,a,b,c)  (p)->lpVtbl->DrawTriPatch(p,a,b,c)
#define IDirect3DDevice8_DeletePatch(p,a)  (p)->lpVtbl->DeletePatch(p,a)
#else
#define IDirect3DDevice8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DDevice8_AddRef(p)  (p)->AddRef()
#define IDirect3DDevice8_Release(p)  (p)->Release()
#define IDirect3DDevice8_TestCooperativeLevel(p)  (p)->TestCooperativeLevel()
#define IDirect3DDevice8_GetAvailableTextureMem(p)  (p)->GetAvailableTextureMem()
#define IDirect3DDevice8_ResourceManagerDiscardBytes(p,a)  (p)->ResourceManagerDiscardBytes(a)
#define IDirect3DDevice8_GetDirect3D(p,a)  (p)->GetDirect3D(a)
#define IDirect3DDevice8_GetDeviceCaps(p,a)  (p)->GetDeviceCaps(a)
#define IDirect3DDevice8_GetDisplayMode(p,a)  (p)->GetDisplayMode(a)
#define IDirect3DDevice8_GetCreationParameters(p,a)  (p)->GetCreationParameters(a)
#define IDirect3DDevice8_SetCursorProperties(p,a,b,c)  (p)->SetCursorProperties(a,b,c)
#define IDirect3DDevice8_SetCursorPosition(p,a,b,c)  (p)->SetCursorPosition(a,b,c)
#define IDirect3DDevice8_ShowCursor(p,a)  (p)->ShowCursor(a)
#define IDirect3DDevice8_CreateAdditionalSwapChain(p,a,b)  (p)->CreateAdditionalSwapChain(a,b)
#define IDirect3DDevice8_Reset(p,a)  (p)->Reset(a)
#define IDirect3DDevice8_Present(p,a,b,c,d)  (p)->Present(a,b,c,d)
#define IDirect3DDevice8_GetBackBuffer(p,a,b,c)  (p)->GetBackBuffer(a,b,c)
#define IDirect3DDevice8_GetRasterStatus(p,a)  (p)->GetRasterStatus(a)
#define IDirect3DDevice8_SetGammaRamp(p,a,b)  (p)->SetGammaRamp(a,b)
#define IDirect3DDevice8_GetGammaRamp(p,a)  (p)->GetGammaRamp(a)
#define IDirect3DDevice8_CreateTexture(p,a,b,c,d,e,f,g)  (p)->CreateTexture(a,b,c,d,e,f,g)
#define IDirect3DDevice8_CreateVolumeTexture(p,a,b,c,d,e,f,g,h)  (p)->CreateVolumeTexture(a,b,c,d,e,f,g,h)
#define IDirect3DDevice8_CreateCubeTexture(p,a,b,c,d,e,f)  (p)->CreateCubeTexture(a,b,c,d,e,f)
#define IDirect3DDevice8_CreateVertexBuffer(p,a,b,c,d,e)  (p)->CreateVertexBuffer(a,b,c,d,e)
#define IDirect3DDevice8_CreateIndexBuffer(p,a,b,c,d,e)  (p)->CreateIndexBuffer(a,b,c,d,e)
#define IDirect3DDevice8_CreateRenderTarget(p,a,b,c,d,e,f)  (p)->CreateRenderTarget(a,b,c,d,e,f)
#define IDirect3DDevice8_CreateDepthStencilSurface(p,a,b,c,d,e)  (p)->CreateDepthStencilSurface(a,b,c,d,e)
#define IDirect3DDevice8_CreateImageSurface(p,a,b,c,d)  (p)->CreateImageSurface(a,b,c,d)
#define IDirect3DDevice8_CopyRects(p,a,b,c,d,e)  (p)->CopyRects(a,b,c,d,e)
#define IDirect3DDevice8_UpdateTexture(p,a,b)  (p)->UpdateTexture(a,b)
#define IDirect3DDevice8_GetFrontBuffer(p,a)  (p)->GetFrontBuffer(a)
#define IDirect3DDevice8_SetRenderTarget(p,a,b)  (p)->SetRenderTarget(a,b)
#define IDirect3DDevice8_GetRenderTarget(p,a)  (p)->GetRenderTarget(a)
#define IDirect3DDevice8_GetDepthStencilSurface(p,a)  (p)->GetDepthStencilSurface(a)
#define IDirect3DDevice8_BeginScene(p)  (p)->BeginScene()
#define IDirect3DDevice8_EndScene(p)  (p)->EndScene()
#define IDirect3DDevice8_Clear(p,a,b,c,d,e,f)  (p)->Clear(a,b,c,d,e,f)
#define IDirect3DDevice8_SetTransform(p,a,b)  (p)->SetTransform(a,b)
#define IDirect3DDevice8_GetTransform(p,a,b)  (p)->GetTransform(a,b)
#define IDirect3DDevice8_MultiplyTransform(p,a,b)  (p)->MultiplyTransform(a,b)
#define IDirect3DDevice8_SetViewport(p,a)  (p)->SetViewport(a)
#define IDirect3DDevice8_GetViewport(p,a)  (p)->GetViewport(a)
#define IDirect3DDevice8_SetMaterial(p,a)  (p)->SetMaterial(a)
#define IDirect3DDevice8_GetMaterial(p,a)  (p)->GetMaterial(a)
#define IDirect3DDevice8_SetLight(p,a,b)  (p)->SetLight(a,b)
#define IDirect3DDevice8_GetLight(p,a,b)  (p)->GetLight(a,b)
#define IDirect3DDevice8_LightEnable(p,a,b)  (p)->LightEnable(a,b)
#define IDirect3DDevice8_GetLightEnable(p,a,b)  (p)->GetLightEnable(a,b)
#define IDirect3DDevice8_SetClipPlane(p,a,b)  (p)->SetClipPlane(a,b)
#define IDirect3DDevice8_GetClipPlane(p,a,b)  (p)->GetClipPlane(a,b)
#define IDirect3DDevice8_SetRenderState(p,a,b)  (p)->SetRenderState(a,b)
#define IDirect3DDevice8_GetRenderState(p,a,b)  (p)->GetRenderState(a,b)
#define IDirect3DDevice8_BeginStateBlock(p)  (p)->BeginStateBlock()
#define IDirect3DDevice8_EndStateBlock(p,a)  (p)->EndStateBlock(a)
#define IDirect3DDevice8_ApplyStateBlock(p,a)  (p)->ApplyStateBlock(a)
#define IDirect3DDevice8_CaptureStateBlock(p,a)  (p)->CaptureStateBlock(a)
#define IDirect3DDevice8_DeleteStateBlock(p,a)  (p)->DeleteStateBlock(a)
#define IDirect3DDevice8_CreateStateBlock(p,a,b)  (p)->CreateStateBlock(a,b)
#define IDirect3DDevice8_SetClipStatus(p,a)  (p)->SetClipStatus(a)
#define IDirect3DDevice8_GetClipStatus(p,a)  (p)->GetClipStatus(a)
#define IDirect3DDevice8_GetTexture(p,a,b)  (p)->GetTexture(a,b)
#define IDirect3DDevice8_SetTexture(p,a,b)  (p)->SetTexture(a,b)
#define IDirect3DDevice8_GetTextureStageState(p,a,b,c)  (p)->GetTextureStageState(a,b,c)
#define IDirect3DDevice8_SetTextureStageState(p,a,b,c)  (p)->SetTextureStageState(a,b,c)
#define IDirect3DDevice8_ValidateDevice(p,a)  (p)->ValidateDevice(a)
#define IDirect3DDevice8_GetInfo(p,a,b,c)  (p)->GetInfo(a,b,c)
#define IDirect3DDevice8_SetPaletteEntries(p,a,b)  (p)->SetPaletteEntries(a,b)
#define IDirect3DDevice8_GetPaletteEntries(p,a,b)  (p)->GetPaletteEntries(a,b)
#define IDirect3DDevice8_SetCurrentTexturePalette(p,a)  (p)->SetCurrentTexturePalette(a)
#define IDirect3DDevice8_GetCurrentTexturePalette(p,a)  (p)->GetCurrentTexturePalette(a)
#define IDirect3DDevice8_DrawPrimitive(p,a,b,c)  (p)->DrawPrimitive(a,b,c)
#define IDirect3DDevice8_DrawIndexedPrimitive(p,a,b,c,d,e)  (p)->DrawIndexedPrimitive(a,b,c,d,e)
#define IDirect3DDevice8_DrawPrimitiveUP(p,a,b,c,d)  (p)->DrawPrimitiveUP(a,b,c,d)
#define IDirect3DDevice8_DrawIndexedPrimitiveUP(p,a,b,c,d,e,f,g,h)  (p)->DrawIndexedPrimitiveUP(a,b,c,d,e,f,g,h)
#define IDirect3DDevice8_ProcessVertices(p,a,b,c,d,e)  (p)->ProcessVertices(a,b,c,d,e)
#define IDirect3DDevice8_CreateVertexShader(p,a,b,c,d)  (p)->CreateVertexShader(a,b,c,d)
#define IDirect3DDevice8_SetVertexShader(p,a)  (p)->SetVertexShader(a)
#define IDirect3DDevice8_GetVertexShader(p,a)  (p)->GetVertexShader(a)
#define IDirect3DDevice8_DeleteVertexShader(p,a)  (p)->DeleteVertexShader(a)
#define IDirect3DDevice8_SetVertexShaderConstant(p,a,b,c)  (p)->SetVertexShaderConstant(a,b,c)
#define IDirect3DDevice8_GetVertexShaderConstant(p,a,b,c)  (p)->GetVertexShaderConstant(a,b,c)
#define IDirect3DDevice8_GetVertexShaderDeclaration(p,a,b,c)  (p)->GetVertexShaderDeclaration(a,b,c)
#define IDirect3DDevice8_GetVertexShaderFunction(p,a,b,c)  (p)->GetVertexShaderFunction(a,b,c)
#define IDirect3DDevice8_SetStreamSource(p,a,b,c)  (p)->SetStreamSource(a,b,c)
#define IDirect3DDevice8_GetStreamSource(p,a,b,c)  (p)->GetStreamSource(a,b,c)
#define IDirect3DDevice8_SetIndices(p,a,b)  (p)->SetIndices(a,b)
#define IDirect3DDevice8_GetIndices(p,a,b)  (p)->GetIndices(a,b)
#define IDirect3DDevice8_CreatePixelShader(p,a,b)  (p)->CreatePixelShader(a,b)
#define IDirect3DDevice8_SetPixelShader(p,a)  (p)->SetPixelShader(a)
#define IDirect3DDevice8_GetPixelShader(p,a)  (p)->GetPixelShader(a)
#define IDirect3DDevice8_DeletePixelShader(p,a)  (p)->DeletePixelShader(a)
#define IDirect3DDevice8_SetPixelShaderConstant(p,a,b,c)  (p)->SetPixelShaderConstant(a,b,c)
#define IDirect3DDevice8_GetPixelShaderConstant(p,a,b,c)  (p)->GetPixelShaderConstant(a,b,c)
#define IDirect3DDevice8_GetPixelShaderFunction(p,a,b,c)  (p)->GetPixelShaderFunction(a,b,c)
#define IDirect3DDevice8_DrawRectPatch(p,a,b,c)  (p)->DrawRectPatch(a,b,c)
#define IDirect3DDevice8_DrawTriPatch(p,a,b,c)  (p)->DrawTriPatch(a,b,c)
#define IDirect3DDevice8_DeletePatch(p,a)  (p)->DeletePatch(a)
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DSwapChain8
DECLARE_INTERFACE_(IDirect3DSwapChain8, IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Present)(THIS_ CONST RECT*,CONST RECT*,HWND,CONST RGNDATA*) PURE;
    STDMETHOD(GetBackBuffer)(THIS_ UINT,D3DBACKBUFFER_TYPE,IDirect3DSurface8**) PURE;
};

typedef struct IDirect3DSwapChain8 *LPDIRECT3DSWAPCHAIN8, *PDIRECT3DSWAPCHAIN8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DSwapChain8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DSwapChain8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DSwapChain8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DSwapChain8_Present(p,a,b,c,d)  (p)->lpVtbl->Present(p,a,b,c,d)
#define IDirect3DSwapChain8_GetBackBuffer(p,a,b,c)  (p)->lpVtbl->GetBackBuffer(p,a,b,c)
#else
#define IDirect3DSwapChain8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DSwapChain8_AddRef(p)  (p)->AddRef()
#define IDirect3DSwapChain8_Release(p)  (p)->Release()
#define IDirect3DSwapChain8_Present(p,a,b,c,d)  (p)->Present(a,b,c,d)
#define IDirect3DSwapChain8_GetBackBuffer(p,a,b,c)  (p)->GetBackBuffer(a,b,c)
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DResource8
DECLARE_INTERFACE_(IDirect3DResource8, IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetDevice)(THIS_ IDirect3DDevice8**) PURE;
    STDMETHOD(SetPrivateData)(THIS_ REFGUID,CONST void*,DWORD,DWORD) PURE;
    STDMETHOD(GetPrivateData)(THIS_ REFGUID,void*,DWORD*) PURE;
    STDMETHOD(FreePrivateData)(THIS_ REFGUID) PURE;
    STDMETHOD_(DWORD,SetPriority)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetPriority)(THIS) PURE;
    STDMETHOD_(void,PreLoad)(THIS) PURE;
    STDMETHOD_(D3DRESOURCETYPE,GetType)(THIS) PURE;
};

typedef struct IDirect3DResource8 *LPDIRECT3DRESOURCE8, *PDIRECT3DRESOURCE8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DResource8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DResource8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DResource8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DResource8_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirect3DResource8_SetPrivateData(p,a,b,c,d)  (p)->lpVtbl->SetPrivateData(p,a,b,c,d)
#define IDirect3DResource8_GetPrivateData(p,a,b,c)  (p)->lpVtbl->GetPrivateData(p,a,b,c)
#define IDirect3DResource8_FreePrivateData(p,a)  (p)->lpVtbl->FreePrivateData(p,a)
#define IDirect3DResource8_SetPriority(p,a)  (p)->lpVtbl->SetPriority(p,a)
#define IDirect3DResource8_GetPriority(p)  (p)->lpVtbl->GetPriority(p)
#define IDirect3DResource8_PreLoad(p)  (p)->lpVtbl->PreLoad(p)
#define IDirect3DResource8_GetType(p)  (p)->lpVtbl->GetType(p)
#else
#define IDirect3DResource8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DResource8_AddRef(p)  (p)->AddRef()
#define IDirect3DResource8_Release(p)  (p)->Release()
#define IDirect3DResource8_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirect3DResource8_SetPrivateData(p,a,b,c,d)  (p)->SetPrivateData(a,b,c,d)
#define IDirect3DResource8_GetPrivateData(p,a,b,c)  (p)->GetPrivateData(a,b,c)
#define IDirect3DResource8_FreePrivateData(p,a)  (p)->FreePrivateData(a)
#define IDirect3DResource8_SetPriority(p,a)  (p)->SetPriority(a)
#define IDirect3DResource8_GetPriority(p)  (p)->GetPriority()
#define IDirect3DResource8_PreLoad(p)  (p)->PreLoad()
#define IDirect3DResource8_GetType(p)  (p)->GetType()
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DBaseTexture8
DECLARE_INTERFACE_(IDirect3DBaseTexture8, IDirect3DResource8)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetDevice)(THIS_ IDirect3DDevice8**) PURE;
    STDMETHOD(SetPrivateData)(THIS_ REFGUID,CONST void*,DWORD,DWORD) PURE;
    STDMETHOD(GetPrivateData)(THIS_ REFGUID,void*,DWORD*) PURE;
    STDMETHOD(FreePrivateData)(THIS_ REFGUID) PURE;
    STDMETHOD_(DWORD,SetPriority)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetPriority)(THIS) PURE;
    STDMETHOD_(void,PreLoad)(THIS) PURE;
    STDMETHOD_(D3DRESOURCETYPE, GetType)(THIS) PURE;
    STDMETHOD_(DWORD,SetLOD)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetLOD)(THIS) PURE;
    STDMETHOD_(DWORD,GetLevelCount)(THIS) PURE;
};

typedef struct IDirect3DBaseTexture8 *LPDIRECT3DBASETEXTURE8, *PDIRECT3DBASETEXTURE8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DBaseTexture8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DBaseTexture8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DBaseTexture8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DBaseTexture8_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirect3DBaseTexture8_SetPrivateData(p,a,b,c,d)  (p)->lpVtbl->SetPrivateData(p,a,b,c,d)
#define IDirect3DBaseTexture8_GetPrivateData(p,a,b,c)  (p)->lpVtbl->GetPrivateData(p,a,b,c)
#define IDirect3DBaseTexture8_FreePrivateData(p,a)  (p)->lpVtbl->FreePrivateData(p,a)
#define IDirect3DBaseTexture8_SetPriority(p,a)  (p)->lpVtbl->SetPriority(p,a)
#define IDirect3DBaseTexture8_GetPriority(p)  (p)->lpVtbl->GetPriority(p)
#define IDirect3DBaseTexture8_PreLoad(p)  (p)->lpVtbl->PreLoad(p)
#define IDirect3DBaseTexture8_GetType(p)  (p)->lpVtbl->GetType(p)
#define IDirect3DBaseTexture8_SetLOD(p,a)  (p)->lpVtbl->SetLOD(p,a)
#define IDirect3DBaseTexture8_GetLOD(p)  (p)->lpVtbl->GetLOD(p)
#define IDirect3DBaseTexture8_GetLevelCount(p)  (p)->lpVtbl->GetLevelCount(p)
#else
#define IDirect3DBaseTexture8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DBaseTexture8_AddRef(p)  (p)->AddRef()
#define IDirect3DBaseTexture8_Release(p)  (p)->Release()
#define IDirect3DBaseTexture8_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirect3DBaseTexture8_SetPrivateData(p,a,b,c,d)  (p)->SetPrivateData(a,b,c,d)
#define IDirect3DBaseTexture8_GetPrivateData(p,a,b,c)  (p)->GetPrivateData(a,b,c)
#define IDirect3DBaseTexture8_FreePrivateData(p,a)  (p)->FreePrivateData(a)
#define IDirect3DBaseTexture8_SetPriority(p,a)  (p)->SetPriority(a)
#define IDirect3DBaseTexture8_GetPriority(p)  (p)->GetPriority()
#define IDirect3DBaseTexture8_PreLoad(p)  (p)->PreLoad()
#define IDirect3DBaseTexture8_GetType(p)  (p)->GetType()
#define IDirect3DBaseTexture8_SetLOD(p,a)  (p)->SetLOD(a)
#define IDirect3DBaseTexture8_GetLOD(p)  (p)->GetLOD()
#define IDirect3DBaseTexture8_GetLevelCount(p)  (p)->GetLevelCount()
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DTexture8
DECLARE_INTERFACE_(IDirect3DTexture8, IDirect3DBaseTexture8)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetDevice)(THIS_ IDirect3DDevice8**) PURE;
    STDMETHOD(SetPrivateData)(THIS_ REFGUID,CONST void*,DWORD,DWORD) PURE;
    STDMETHOD(GetPrivateData)(THIS_ REFGUID,void*,DWORD*) PURE;
    STDMETHOD(FreePrivateData)(THIS_ REFGUID) PURE;
    STDMETHOD_(DWORD,SetPriority)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetPriority)(THIS) PURE;
    STDMETHOD_(void,PreLoad)(THIS) PURE;
    STDMETHOD_(D3DRESOURCETYPE, GetType)(THIS) PURE;
    STDMETHOD_(DWORD,SetLOD)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetLOD)(THIS) PURE;
    STDMETHOD_(DWORD,GetLevelCount)(THIS) PURE;
    STDMETHOD(GetLevelDesc)(THIS_ UINT,D3DSURFACE_DESC*) PURE;
    STDMETHOD(GetSurfaceLevel)(THIS_ UINT,IDirect3DSurface8**) PURE;
    STDMETHOD(LockRect)(THIS_ UINT,D3DLOCKED_RECT*,CONST RECT*,DWORD) PURE;
    STDMETHOD(UnlockRect)(THIS_ UINT) PURE;
    STDMETHOD(AddDirtyRect)(THIS_ CONST RECT*) PURE;
};

typedef struct IDirect3DTexture8 *LPDIRECT3DTEXTURE8, *PDIRECT3DTEXTURE8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DTexture8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DTexture8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DTexture8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DTexture8_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirect3DTexture8_SetPrivateData(p,a,b,c,d)  (p)->lpVtbl->SetPrivateData(p,a,b,c,d)
#define IDirect3DTexture8_GetPrivateData(p,a,b,c)  (p)->lpVtbl->GetPrivateData(p,a,b,c)
#define IDirect3DTexture8_FreePrivateData(p,a)  (p)->lpVtbl->FreePrivateData(p,a)
#define IDirect3DTexture8_SetPriority(p,a)  (p)->lpVtbl->SetPriority(p,a)
#define IDirect3DTexture8_GetPriority(p)  (p)->lpVtbl->GetPriority(p)
#define IDirect3DTexture8_PreLoad(p)  (p)->lpVtbl->PreLoad(p)
#define IDirect3DTexture8_GetType(p)  (p)->lpVtbl->GetType(p)
#define IDirect3DTexture8_SetLOD(p,a)  (p)->lpVtbl->SetLOD(p,a)
#define IDirect3DTexture8_GetLOD(p)  (p)->lpVtbl->GetLOD(p)
#define IDirect3DTexture8_GetLevelCount(p)  (p)->lpVtbl->GetLevelCount(p)
#define IDirect3DTexture8_GetLevelDesc(p,a,b)  (p)->lpVtbl->GetLevelDesc(p,a,b)
#define IDirect3DTexture8_GetSurfaceLevel(p,a,b)  (p)->lpVtbl->GetSurfaceLevel(p,a,b)
#define IDirect3DTexture8_LockRect(p,a,b,c,d)  (p)->lpVtbl->LockRect(p,a,b,c,d)
#define IDirect3DTexture8_UnlockRect(p,a)  (p)->lpVtbl->UnlockRect(p,a)
#define IDirect3DTexture8_AddDirtyRect(p,a)  (p)->lpVtbl->AddDirtyRect(p,a)
#else
#define IDirect3DTexture8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DTexture8_AddRef(p)  (p)->AddRef()
#define IDirect3DTexture8_Release(p)  (p)->Release()
#define IDirect3DTexture8_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirect3DTexture8_SetPrivateData(p,a,b,c,d)  (p)->SetPrivateData(a,b,c,d)
#define IDirect3DTexture8_GetPrivateData(p,a,b,c)  (p)->GetPrivateData(a,b,c)
#define IDirect3DTexture8_FreePrivateData(p,a)  (p)->FreePrivateData(a)
#define IDirect3DTexture8_SetPriority(p,a)  (p)->SetPriority(a)
#define IDirect3DTexture8_GetPriority(p)  (p)->GetPriority()
#define IDirect3DTexture8_PreLoad(p)  (p)->PreLoad()
#define IDirect3DTexture8_GetType(p)  (p)->GetType()
#define IDirect3DTexture8_SetLOD(p,a)  (p)->SetLOD(a)
#define IDirect3DTexture8_GetLOD(p)  (p)->GetLOD()
#define IDirect3DTexture8_GetLevelCount(p)  (p)->GetLevelCount()
#define IDirect3DTexture8_GetLevelDesc(p,a,b)  (p)->GetLevelDesc(a,b)
#define IDirect3DTexture8_GetSurfaceLevel(p,a,b)  (p)->GetSurfaceLevel(a,b)
#define IDirect3DTexture8_LockRect(p,a,b,c,d)  (p)->LockRect(a,b,c,d)
#define IDirect3DTexture8_UnlockRect(p,a)  (p)->UnlockRect(a)
#define IDirect3DTexture8_AddDirtyRect(p,a)  (p)->AddDirtyRect(a)
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DVolumeTexture8
DECLARE_INTERFACE_(IDirect3DVolumeTexture8, IDirect3DBaseTexture8)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetDevice)(THIS_ IDirect3DDevice8**) PURE;
    STDMETHOD(SetPrivateData)(THIS_ REFGUID,CONST void*,DWORD,DWORD) PURE;
    STDMETHOD(GetPrivateData)(THIS_ REFGUID,void*,DWORD*) PURE;
    STDMETHOD(FreePrivateData)(THIS_ REFGUID) PURE;
    STDMETHOD_(DWORD,SetPriority)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetPriority)(THIS) PURE;
    STDMETHOD_(void,PreLoad)(THIS) PURE;
    STDMETHOD_(D3DRESOURCETYPE, GetType)(THIS) PURE;
    STDMETHOD_(DWORD,SetLOD)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetLOD)(THIS) PURE;
    STDMETHOD_(DWORD,GetLevelCount)(THIS) PURE;
    STDMETHOD(GetLevelDesc)(THIS_ UINT,D3DVOLUME_DESC*) PURE;
    STDMETHOD(GetVolumeLevel)(THIS_ UINT,IDirect3DVolume8**) PURE;
    STDMETHOD(LockBox)(THIS_ UINT,D3DLOCKED_BOX*,CONST D3DBOX*,DWORD) PURE;
    STDMETHOD(UnlockBox)(THIS_ UINT) PURE;
    STDMETHOD(AddDirtyBox)(THIS_ CONST D3DBOX*) PURE;
};

typedef struct IDirect3DVolumeTexture8 *LPDIRECT3DVOLUMETEXTURE8, *PDIRECT3DVOLUMETEXTURE8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DVolumeTexture8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DVolumeTexture8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DVolumeTexture8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DVolumeTexture8_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirect3DVolumeTexture8_SetPrivateData(p,a,b,c,d)  (p)->lpVtbl->SetPrivateData(p,a,b,c,d)
#define IDirect3DVolumeTexture8_GetPrivateData(p,a,b,c)  (p)->lpVtbl->GetPrivateData(p,a,b,c)
#define IDirect3DVolumeTexture8_FreePrivateData(p,a)  (p)->lpVtbl->FreePrivateData(p,a)
#define IDirect3DVolumeTexture8_SetPriority(p,a)  (p)->lpVtbl->SetPriority(p,a)
#define IDirect3DVolumeTexture8_GetPriority(p)  (p)->lpVtbl->GetPriority(p)
#define IDirect3DVolumeTexture8_PreLoad(p)  (p)->lpVtbl->PreLoad(p)
#define IDirect3DVolumeTexture8_GetType(p)  (p)->lpVtbl->GetType(p)
#define IDirect3DVolumeTexture8_SetLOD(p,a)  (p)->lpVtbl->SetLOD(p,a)
#define IDirect3DVolumeTexture8_GetLOD(p)  (p)->lpVtbl->GetLOD(p)
#define IDirect3DVolumeTexture8_GetLevelCount(p)  (p)->lpVtbl->GetLevelCount(p)
#define IDirect3DVolumeTexture8_GetLevelDesc(p,a,b)  (p)->lpVtbl->GetLevelDesc(p,a,b)
#define IDirect3DVolumeTexture8_GetVolumeLevel(p,a,b)  (p)->lpVtbl->GetVolumeLevel(p,a,b)
#define IDirect3DVolumeTexture8_LockBox(p,a,b,c,d)  (p)->lpVtbl->LockBox(p,a,b,c,d)
#define IDirect3DVolumeTexture8_UnlockBox(p,a)  (p)->lpVtbl->UnlockBox(p,a)
#define IDirect3DVolumeTexture8_AddDirtyBox(p,a)  (p)->lpVtbl->AddDirtyBox(p,a)
#else
#define IDirect3DVolumeTexture8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DVolumeTexture8_AddRef(p)  (p)->AddRef()
#define IDirect3DVolumeTexture8_Release(p)  (p)->Release()
#define IDirect3DVolumeTexture8_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirect3DVolumeTexture8_SetPrivateData(p,a,b,c,d)  (p)->SetPrivateData(a,b,c,d)
#define IDirect3DVolumeTexture8_GetPrivateData(p,a,b,c)  (p)->GetPrivateData(a,b,c)
#define IDirect3DVolumeTexture8_FreePrivateData(p,a)  (p)->FreePrivateData(a)
#define IDirect3DVolumeTexture8_SetPriority(p,a)  (p)->SetPriority(a)
#define IDirect3DVolumeTexture8_GetPriority(p)  (p)->GetPriority()
#define IDirect3DVolumeTexture8_PreLoad(p)  (p)->PreLoad()
#define IDirect3DVolumeTexture8_GetType(p)  (p)->GetType()
#define IDirect3DVolumeTexture8_SetLOD(p,a)  (p)->SetLOD(a)
#define IDirect3DVolumeTexture8_GetLOD(p)  (p)->GetLOD()
#define IDirect3DVolumeTexture8_GetLevelCount(p)  (p)->GetLevelCount()
#define IDirect3DVolumeTexture8_GetLevelDesc(p,a,b)  (p)->GetLevelDesc(a,b)
#define IDirect3DVolumeTexture8_GetVolumeLevel(p,a,b)  (p)->GetVolumeLevel(a,b)
#define IDirect3DVolumeTexture8_LockBox(p,a,b,c,d)  (p)->LockBox(a,b,c,d)
#define IDirect3DVolumeTexture8_UnlockBox(p,a)  (p)->UnlockBox(a)
#define IDirect3DVolumeTexture8_AddDirtyBox(p,a)  (p)->AddDirtyBox(a)
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DCubeTexture8
DECLARE_INTERFACE_(IDirect3DCubeTexture8, IDirect3DBaseTexture8)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetDevice)(THIS_ IDirect3DDevice8**) PURE;
    STDMETHOD(SetPrivateData)(THIS_ REFGUID,CONST void*,DWORD,DWORD) PURE;
    STDMETHOD(GetPrivateData)(THIS_ REFGUID,void*,DWORD*) PURE;
    STDMETHOD(FreePrivateData)(THIS_ REFGUID) PURE;
    STDMETHOD_(DWORD,SetPriority)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetPriority)(THIS) PURE;
    STDMETHOD_(void,PreLoad)(THIS) PURE;
    STDMETHOD_(D3DRESOURCETYPE, GetType)(THIS) PURE;
    STDMETHOD_(DWORD,SetLOD)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetLOD)(THIS) PURE;
    STDMETHOD_(DWORD,GetLevelCount)(THIS) PURE;
    STDMETHOD(GetLevelDesc)(THIS_ UINT,D3DSURFACE_DESC*) PURE;
    STDMETHOD(GetCubeMapSurface)(THIS_ D3DCUBEMAP_FACES,UINT,IDirect3DSurface8**) PURE;
    STDMETHOD(LockRect)(THIS_ D3DCUBEMAP_FACES,UINT,D3DLOCKED_RECT*,CONST RECT*,DWORD) PURE;
    STDMETHOD(UnlockRect)(THIS_ D3DCUBEMAP_FACES,UINT) PURE;
    STDMETHOD(AddDirtyRect)(THIS_ D3DCUBEMAP_FACES,CONST RECT*) PURE;
};

typedef struct IDirect3DCubeTexture8 *LPDIRECT3DCUBETEXTURE8, *PDIRECT3DCUBETEXTURE8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DCubeTexture8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DCubeTexture8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DCubeTexture8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DCubeTexture8_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirect3DCubeTexture8_SetPrivateData(p,a,b,c,d)  (p)->lpVtbl->SetPrivateData(p,a,b,c,d)
#define IDirect3DCubeTexture8_GetPrivateData(p,a,b,c)  (p)->lpVtbl->GetPrivateData(p,a,b,c)
#define IDirect3DCubeTexture8_FreePrivateData(p,a)  (p)->lpVtbl->FreePrivateData(p,a)
#define IDirect3DCubeTexture8_SetPriority(p,a)  (p)->lpVtbl->SetPriority(p,a)
#define IDirect3DCubeTexture8_GetPriority(p)  (p)->lpVtbl->GetPriority(p)
#define IDirect3DCubeTexture8_PreLoad(p)  (p)->lpVtbl->PreLoad(p)
#define IDirect3DCubeTexture8_GetType(p)  (p)->lpVtbl->GetType(p)
#define IDirect3DCubeTexture8_SetLOD(p,a)  (p)->lpVtbl->SetLOD(p,a)
#define IDirect3DCubeTexture8_GetLOD(p)  (p)->lpVtbl->GetLOD(p)
#define IDirect3DCubeTexture8_GetLevelCount(p)  (p)->lpVtbl->GetLevelCount(p)
#define IDirect3DCubeTexture8_GetLevelDesc(p,a,b)  (p)->lpVtbl->GetLevelDesc(p,a,b)
#define IDirect3DCubeTexture8_GetCubeMapSurface(p,a,b,c)  (p)->lpVtbl->GetCubeMapSurface(p,a,b,c)
#define IDirect3DCubeTexture8_LockRect(p,a,b,c,d,e)  (p)->lpVtbl->LockRect(p,a,b,c,d,e)
#define IDirect3DCubeTexture8_UnlockRect(p,a,b)  (p)->lpVtbl->UnlockRect(p,a,b)
#define IDirect3DCubeTexture8_AddDirtyRect(p,a,b)  (p)->lpVtbl->AddDirtyRect(p,a,b)
#else
#define IDirect3DCubeTexture8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DCubeTexture8_AddRef(p)  (p)->AddRef()
#define IDirect3DCubeTexture8_Release(p)  (p)->Release()
#define IDirect3DCubeTexture8_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirect3DCubeTexture8_SetPrivateData(p,a,b,c,d)  (p)->SetPrivateData(a,b,c,d)
#define IDirect3DCubeTexture8_GetPrivateData(p,a,b,c)  (p)->GetPrivateData(a,b,c)
#define IDirect3DCubeTexture8_FreePrivateData(p,a)  (p)->FreePrivateData(a)
#define IDirect3DCubeTexture8_SetPriority(p,a)  (p)->SetPriority(a)
#define IDirect3DCubeTexture8_GetPriority(p)  (p)->GetPriority()
#define IDirect3DCubeTexture8_PreLoad(p)  (p)->PreLoad()
#define IDirect3DCubeTexture8_GetType(p)  (p)->GetType()
#define IDirect3DCubeTexture8_SetLOD(p,a)  (p)->SetLOD(a)
#define IDirect3DCubeTexture8_GetLOD(p)  (p)->GetLOD()
#define IDirect3DCubeTexture8_GetLevelCount(p)  (p)->GetLevelCount()
#define IDirect3DCubeTexture8_GetLevelDesc(p,a,b)  (p)->GetLevelDesc(a,b)
#define IDirect3DCubeTexture8_GetCubeMapSurface(p,a,b,c)  (p)->GetCubeMapSurface(a,b,c)
#define IDirect3DCubeTexture8_LockRect(p,a,b,c,d,e)  (p)->LockRect(a,b,c,d,e)
#define IDirect3DCubeTexture8_UnlockRect(p,a,b)  (p)->UnlockRect(a,b)
#define IDirect3DCubeTexture8_AddDirtyRect(p,a,b)  (p)->AddDirtyRect(a,b)
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DVertexBuffer8
DECLARE_INTERFACE_(IDirect3DVertexBuffer8, IDirect3DResource8)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetDevice)(THIS_ IDirect3DDevice8**) PURE;
    STDMETHOD(SetPrivateData)(THIS_ REFGUID,CONST void*,DWORD,DWORD) PURE;
    STDMETHOD(GetPrivateData)(THIS_ REFGUID,void*,DWORD*) PURE;
    STDMETHOD(FreePrivateData)(THIS_ REFGUID) PURE;
    STDMETHOD_(DWORD,SetPriority)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetPriority)(THIS) PURE;
    STDMETHOD_(void,PreLoad)(THIS) PURE;
    STDMETHOD_(D3DRESOURCETYPE,GetType)(THIS) PURE;
    STDMETHOD(Lock)(THIS_ UINT,UINT,BYTE**,DWORD) PURE;
    STDMETHOD(Unlock)(THIS) PURE;
    STDMETHOD(GetDesc)(THIS_ D3DVERTEXBUFFER_DESC*) PURE;
};

typedef struct IDirect3DVertexBuffer8 *LPDIRECT3DVERTEXBUFFER8, *PDIRECT3DVERTEXBUFFER8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DVertexBuffer8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DVertexBuffer8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DVertexBuffer8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DVertexBuffer8_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirect3DVertexBuffer8_SetPrivateData(p,a,b,c,d)  (p)->lpVtbl->SetPrivateData(p,a,b,c,d)
#define IDirect3DVertexBuffer8_GetPrivateData(p,a,b,c)  (p)->lpVtbl->GetPrivateData(p,a,b,c)
#define IDirect3DVertexBuffer8_FreePrivateData(p,a)  (p)->lpVtbl->FreePrivateData(p,a)
#define IDirect3DVertexBuffer8_SetPriority(p,a)  (p)->lpVtbl->SetPriority(p,a)
#define IDirect3DVertexBuffer8_GetPriority(p)  (p)->lpVtbl->GetPriority(p)
#define IDirect3DVertexBuffer8_PreLoad(p)  (p)->lpVtbl->PreLoad(p)
#define IDirect3DVertexBuffer8_GetType(p)  (p)->lpVtbl->GetType(p)
#define IDirect3DVertexBuffer8_Lock(p,a,b,c,d)  (p)->lpVtbl->Lock(p,a,b,c,d)
#define IDirect3DVertexBuffer8_Unlock(p)  (p)->lpVtbl->Unlock(p)
#define IDirect3DVertexBuffer8_GetDesc(p,a)  (p)->lpVtbl->GetDesc(p,a)
#else
#define IDirect3DVertexBuffer8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DVertexBuffer8_AddRef(p)  (p)->AddRef()
#define IDirect3DVertexBuffer8_Release(p)  (p)->Release()
#define IDirect3DVertexBuffer8_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirect3DVertexBuffer8_SetPrivateData(p,a,b,c,d)  (p)->SetPrivateData(a,b,c,d)
#define IDirect3DVertexBuffer8_GetPrivateData(p,a,b,c)  (p)->GetPrivateData(a,b,c)
#define IDirect3DVertexBuffer8_FreePrivateData(p,a)  (p)->FreePrivateData(a)
#define IDirect3DVertexBuffer8_SetPriority(p,a)  (p)->SetPriority(a)
#define IDirect3DVertexBuffer8_GetPriority(p)  (p)->GetPriority()
#define IDirect3DVertexBuffer8_PreLoad(p)  (p)->PreLoad()
#define IDirect3DVertexBuffer8_GetType(p)  (p)->GetType()
#define IDirect3DVertexBuffer8_Lock(p,a,b,c,d)  (p)->Lock(a,b,c,d)
#define IDirect3DVertexBuffer8_Unlock(p)  (p)->Unlock()
#define IDirect3DVertexBuffer8_GetDesc(p,a)  (p)->GetDesc(a)
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DIndexBuffer8
DECLARE_INTERFACE_(IDirect3DIndexBuffer8, IDirect3DResource8)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetDevice)(THIS_ IDirect3DDevice8**) PURE;
    STDMETHOD(SetPrivateData)(THIS_ REFGUID,CONST void*,DWORD,DWORD) PURE;
    STDMETHOD(GetPrivateData)(THIS_ REFGUID,void*,DWORD*) PURE;
    STDMETHOD(FreePrivateData)(THIS_ REFGUID) PURE;
    STDMETHOD_(DWORD,SetPriority)(THIS_ DWORD) PURE;
    STDMETHOD_(DWORD,GetPriority)(THIS) PURE;
    STDMETHOD_(void,PreLoad)(THIS) PURE;
    STDMETHOD_(D3DRESOURCETYPE,GetType)(THIS) PURE;
    STDMETHOD(Lock)(THIS_ UINT,UINT,BYTE**,DWORD) PURE;
    STDMETHOD(Unlock)(THIS) PURE;
    STDMETHOD(GetDesc)(THIS_ D3DINDEXBUFFER_DESC*) PURE;
};

typedef struct IDirect3DIndexBuffer8 *LPDIRECT3DINDEXBUFFER8, *PDIRECT3DINDEXBUFFER8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DIndexBuffer8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DIndexBuffer8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DIndexBuffer8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DIndexBuffer8_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirect3DIndexBuffer8_SetPrivateData(p,a,b,c,d)  (p)->lpVtbl->SetPrivateData(p,a,b,c,d)
#define IDirect3DIndexBuffer8_GetPrivateData(p,a,b,c)  (p)->lpVtbl->GetPrivateData(p,a,b,c)
#define IDirect3DIndexBuffer8_FreePrivateData(p,a)  (p)->lpVtbl->FreePrivateData(p,a)
#define IDirect3DIndexBuffer8_SetPriority(p,a)  (p)->lpVtbl->SetPriority(p,a)
#define IDirect3DIndexBuffer8_GetPriority(p)  (p)->lpVtbl->GetPriority(p)
#define IDirect3DIndexBuffer8_PreLoad(p)  (p)->lpVtbl->PreLoad(p)
#define IDirect3DIndexBuffer8_GetType(p)  (p)->lpVtbl->GetType(p)
#define IDirect3DIndexBuffer8_Lock(p,a,b,c,d)  (p)->lpVtbl->Lock(p,a,b,c,d)
#define IDirect3DIndexBuffer8_Unlock(p)  (p)->lpVtbl->Unlock(p)
#define IDirect3DIndexBuffer8_GetDesc(p,a)  (p)->lpVtbl->GetDesc(p,a)
#else
#define IDirect3DIndexBuffer8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DIndexBuffer8_AddRef(p)  (p)->AddRef()
#define IDirect3DIndexBuffer8_Release(p)  (p)->Release()
#define IDirect3DIndexBuffer8_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirect3DIndexBuffer8_SetPrivateData(p,a,b,c,d)  (p)->SetPrivateData(a,b,c,d)
#define IDirect3DIndexBuffer8_GetPrivateData(p,a,b,c)  (p)->GetPrivateData(a,b,c)
#define IDirect3DIndexBuffer8_FreePrivateData(p,a)  (p)->FreePrivateData(a)
#define IDirect3DIndexBuffer8_SetPriority(p,a)  (p)->SetPriority(a)
#define IDirect3DIndexBuffer8_GetPriority(p)  (p)->GetPriority()
#define IDirect3DIndexBuffer8_PreLoad(p)  (p)->PreLoad()
#define IDirect3DIndexBuffer8_GetType(p)  (p)->GetType()
#define IDirect3DIndexBuffer8_Lock(p,a,b,c,d)  (p)->Lock(a,b,c,d)
#define IDirect3DIndexBuffer8_Unlock(p)  (p)->Unlock()
#define IDirect3DIndexBuffer8_GetDesc(p,a)  (p)->GetDesc(a)
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DSurface8
DECLARE_INTERFACE_(IDirect3DSurface8, IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetDevice)(THIS_ IDirect3DDevice8**) PURE;
    STDMETHOD(SetPrivateData)(THIS_ REFGUID,CONST void*,DWORD,DWORD) PURE;
    STDMETHOD(GetPrivateData)(THIS_ REFGUID,void*,DWORD*) PURE;
    STDMETHOD(FreePrivateData)(THIS_ REFGUID) PURE;
    STDMETHOD(GetContainer)(THIS_ REFIID,void**) PURE;
    STDMETHOD(GetDesc)(THIS_ D3DSURFACE_DESC*) PURE;
    STDMETHOD(LockRect)(THIS_ D3DLOCKED_RECT*,CONST RECT*,DWORD) PURE;
    STDMETHOD(UnlockRect)(THIS) PURE;
};

typedef struct IDirect3DSurface8 *LPDIRECT3DSURFACE8, *PDIRECT3DSURFACE8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DSurface8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DSurface8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DSurface8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DSurface8_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirect3DSurface8_SetPrivateData(p,a,b,c,d)  (p)->lpVtbl->SetPrivateData(p,a,b,c,d)
#define IDirect3DSurface8_GetPrivateData(p,a,b,c)  (p)->lpVtbl->GetPrivateData(p,a,b,c)
#define IDirect3DSurface8_FreePrivateData(p,a)  (p)->lpVtbl->FreePrivateData(p,a)
#define IDirect3DSurface8_GetContainer(p,a,b)  (p)->lpVtbl->GetContainer(p,a,b)
#define IDirect3DSurface8_GetDesc(p,a)  (p)->lpVtbl->GetDesc(p,a)
#define IDirect3DSurface8_LockRect(p,a,b,c)  (p)->lpVtbl->LockRect(p,a,b,c)
#define IDirect3DSurface8_UnlockRect(p)  (p)->lpVtbl->UnlockRect(p)
#else
#define IDirect3DSurface8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DSurface8_AddRef(p)  (p)->AddRef()
#define IDirect3DSurface8_Release(p)  (p)->Release()
#define IDirect3DSurface8_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirect3DSurface8_SetPrivateData(p,a,b,c,d)  (p)->SetPrivateData(a,b,c,d)
#define IDirect3DSurface8_GetPrivateData(p,a,b,c)  (p)->GetPrivateData(a,b,c)
#define IDirect3DSurface8_FreePrivateData(p,a)  (p)->FreePrivateData(a)
#define IDirect3DSurface8_GetContainer(p,a,b)  (p)->GetContainer(a,b)
#define IDirect3DSurface8_GetDesc(p,a)  (p)->GetDesc(a)
#define IDirect3DSurface8_LockRect(p,a,b,c)  (p)->LockRect(a,b,c)
#define IDirect3DSurface8_UnlockRect(p)  (p)->UnlockRect()
#endif /* __cplusplus || CINTERFACE */

#undef INTERFACE
#define INTERFACE IDirect3DVolume8
DECLARE_INTERFACE_(IDirect3DVolume8, IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,void**) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetDevice)(THIS_ IDirect3DDevice8**) PURE;
    STDMETHOD(SetPrivateData)(THIS_ REFGUID,CONST void*,DWORD,DWORD) PURE;
    STDMETHOD(GetPrivateData)(THIS_ REFGUID,void*,DWORD*) PURE;
    STDMETHOD(FreePrivateData)(THIS_ REFGUID) PURE;
    STDMETHOD(GetContainer)(THIS_ REFIID,void**) PURE;
    STDMETHOD(GetDesc)(THIS_ D3DVOLUME_DESC*) PURE;
    STDMETHOD(LockBox)(THIS_ D3DLOCKED_BOX*,CONST D3DBOX*,DWORD) PURE;
    STDMETHOD(UnlockBox)(THIS) PURE;
};

typedef struct IDirect3DVolume8 *LPDIRECT3DVOLUME8, *PDIRECT3DVOLUME8;

#if !defined(__cplusplus) || defined(CINTERFACE)
#define IDirect3DVolume8_QueryInterface(p,a,b)  (p)->lpVtbl->QueryInterface(p,a,b)
#define IDirect3DVolume8_AddRef(p)  (p)->lpVtbl->AddRef(p)
#define IDirect3DVolume8_Release(p)  (p)->lpVtbl->Release(p)
#define IDirect3DVolume8_GetDevice(p,a)  (p)->lpVtbl->GetDevice(p,a)
#define IDirect3DVolume8_SetPrivateData(p,a,b,c,d)  (p)->lpVtbl->SetPrivateData(p,a,b,c,d)
#define IDirect3DVolume8_GetPrivateData(p,a,b,c)  (p)->lpVtbl->GetPrivateData(p,a,b,c)
#define IDirect3DVolume8_FreePrivateData(p,a)  (p)->lpVtbl->FreePrivateData(p,a)
#define IDirect3DVolume8_GetContainer(p,a,b)  (p)->lpVtbl->GetContainer(p,a,b)
#define IDirect3DVolume8_GetDesc(p,a)  (p)->lpVtbl->GetDesc(p,a)
#define IDirect3DVolume8_LockBox(p,a,b,c)  (p)->lpVtbl->LockBox(p,a,b,c)
#define IDirect3DVolume8_UnlockBox(p)  (p)->lpVtbl->UnlockBox(p)
#else
#define IDirect3DVolume8_QueryInterface(p,a,b)  (p)->QueryInterface(a,b)
#define IDirect3DVolume8_AddRef(p)  (p)->AddRef()
#define IDirect3DVolume8_Release(p)  (p)->Release()
#define IDirect3DVolume8_GetDevice(p,a)  (p)->GetDevice(a)
#define IDirect3DVolume8_SetPrivateData(p,a,b,c,d)  (p)->SetPrivateData(a,b,c,d)
#define IDirect3DVolume8_GetPrivateData(p,a,b,c)  (p)->GetPrivateData(a,b,c)
#define IDirect3DVolume8_FreePrivateData(p,a)  (p)->FreePrivateData(a)
#define IDirect3DVolume8_GetContainer(p,a,b)  (p)->GetContainer(a,b)
#define IDirect3DVolume8_GetDesc(p,a)  (p)->GetDesc(a)
#define IDirect3DVolume8_LockBox(p,a,b,c)  (p)->LockBox(a,b,c)
#define IDirect3DVolume8_UnlockBox(p)  (p)->UnlockBox()
#endif /* __cplusplus || CINTERFACE */

#ifdef __cplusplus
};
#endif

#endif /* (DIRECT3D_VERSION >= 0x0800) */
#endif /* _D3D8_H */

