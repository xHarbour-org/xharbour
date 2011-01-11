#ifndef _RICHINK_H
#define _RICHINK_H

/* Windows CE RichInk Edit Control (Pocket PC) */

#include <windows.h>
#include <commctrl.h>

#ifdef __cplusplus
extern "C" {
#endif

#define WC_RICHINKA  "RichInk"
#define WC_RICHINKW  L"RichInk"

#ifdef UNICODE
#define WC_RICHINK  WC_RICHINKW
#else
#define WC_RICHINK  WC_RICHINKA
#endif

#define SF_TEXT  0x0001
#define SF_RTF  0x0002

#define SF_UNICODE  0x0010
#define SF_UTEXT  (SF_TEXT|SF_UNICODE)

#define SFF_PWI  0x0800
#define SF_PWI  (SF_RTF|SFF_PWI|0x010000)

#ifndef EM_CANPASTE
#define EM_CANPASTE  (WM_USER+50)
#endif
#define EM_STREAMIN  (WM_USER+73)
#define EM_STREAMOUT  (WM_USER+74)
#define EM_SETPAGESTYLE  (WM_USER+287)
#define EM_GETPAGESTYLE  (WM_USER+323)
#define EM_SETWRAPMODE  (WM_USER+319)
#define EM_GETWRAPMODE  (WM_USER+227)
#define EM_SETVIEW  (WM_USER+284)
#define EM_GETVIEW  (WM_USER+254)
#define EM_SETINKLAYER  (WM_USER+288)
#define EM_SETZOOMPERCENT  (WM_USER+290)
#define EM_GETZOOMPERCENT  (WM_USER+289)
#define EM_SETPENMODE  (WM_USER+329)
#define EM_GETPENMODE  (WM_USER+328)
#define EM_UNDOEVENT  (WM_USER+234)
#define EM_REDOEVENT  (WM_USER+235)
#define EM_CANREDO  (WM_USER+246)
#define EM_CLEARALL  (WM_USER+331)
#define EM_SETVIEWATTRIBUTES  (WM_USER+332)
#define EM_INSERTLINKS  (WM_USER+333)

#define VIEWATTRIBUTE_ZOOM  0x01
#define VIEWATTRIBUTE_PAGESTYLE  0x02
#define VIEWATTRIBUTE_INKLAYER  0x04
#define VIEWATTRIBUTE_VIEW  0x08

#define RI_WRAPTOPAGE  0
#define RI_WRAPTOWINDOW  1

#define VT_TYPINGVIEW  0
#define VT_WRITINGVIEW  2
#define VT_DRAWINGVIEW  3

#define VL_SMARTINK  0
#define VL_WRITINGINK  1
#define VL_DRAWINGINK  2
#define VL_LINKS  3
#define VL_SMARTLINKS  4

#define PS_LEFTMARGIN  0x0000
#define PS_TOPMARGIN  0x0001
#define PS_RULEDLINES  0x0002
#define PS_GRIDLINES  0x0004
#define PS_TOPLEFTMARGIN  0x0008
#define PS_NONE  0x0010
#define PS_DOTTEDLINES  0x0020
#define PS_YELLOWBACKGROUND  0x0040

#define MODE_PEN  0
#define MODE_SELECT  1
#define MODE_SPACE  2

#define ES_EX_CONTROLPARENT  0x00100000

typedef DWORD (CALLBACK *EDITSTREAMCALLBACK)(DWORD,LPBYTE,LONG,LONG*);

typedef struct _editstream {
    DWORD dwCookie;
    DWORD dwError;
    EDITSTREAMCALLBACK pfnCallback;
} EDITSTREAM;

typedef struct tagCOOKIE {
    HANDLE hFile;
    LPBYTE pbStart;
    LPBYTE pbCur;
    LONG bCount;
    DWORD dwError;
} COOKIE, *PCOOKIE;

typedef struct tagVIEWATTRIBUTES {
    UINT mask;
    UINT uView;
    UINT uZoom;
    UINT uPageStyle;
    UINT uInkLayer;
    BOOL fReserved;
} VIEWATTRIBUTES, *PVIEWATTRIBUTES;

void InitRichInkDLL(void);

DWORD CALLBACK EditStreamCallback(DWORD,LPBYTE,LONG,LONG*);

#ifdef __cplusplus
}
#endif

#endif /* _RICHINK_H */
