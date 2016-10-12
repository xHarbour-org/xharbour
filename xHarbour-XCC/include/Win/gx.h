#ifndef _GX_H
#define _GX_H

/* GAPI definitions (Pocket PC) */

#ifndef GXDLL_API
#define GXDLL_API  __declspec(dllimport)
#endif /* GXDLL_API */

#define GX_FULLSCREEN  0x01
#define GX_NORMALKEYS  0x02
#define GX_LANDSCAPEKEYS  0x03

#ifndef kfLandscape
#define kfLandscape  0x8
#define kfPalette  0x10
#define kfDirect  0x20
#define kfDirect555  0x40
#define kfDirect565  0x80
#define kfDirect888  0x100
#define kfDirect444  0x200
#define kfDirectInverted  0x400
#endif

typedef struct GXDisplayProperties {
    DWORD cxWidth;
    DWORD cyHeight;
    long cbxPitch;
    long cbyPitch;
    long cBPP;
    DWORD ffFormat;
} GXDisplayProperties;

typedef struct GXKeyList {
    short vkUp;
    POINT ptUp;
    short vkDown;
    POINT ptDown;
    short vkLeft;
    POINT ptLeft;
    short vkRight;
    POINT ptRight;
    short vkA;
    POINT ptA;
    short vkB;
    POINT ptB;
    short vkC;
    POINT ptC;
    short vkStart;
    POINT ptStart;
} GXKeyList;

GXDLL_API int GXOpenDisplay(HWND,DWORD);
GXDLL_API int GXCloseDisplay(void);
GXDLL_API void * GXBeginDraw(void);
GXDLL_API int GXEndDraw(void);
GXDLL_API int GXOpenInput(void);
GXDLL_API int GXCloseInput(void);
GXDLL_API GXDisplayProperties GXGetDisplayProperties(void);
GXDLL_API GXKeyList GXGetDefaultKeys(int);
GXDLL_API int GXSuspend(void);
GXDLL_API int GXResume(void);
GXDLL_API int GXSetViewport(DWORD,DWORD,DWORD,DWORD);
GXDLL_API BOOL GXIsDisplayDRAMBuffer(void);

#endif /* _GX_H */
