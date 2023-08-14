#ifndef _UXTHEME_H
#define _UXTHEME_H

/* Windows Theme API definitions (Windows XP) */

#include <commctrl.h>

#ifndef THEMEAPI
#define THEMEAPI         EXTERN_C DECLSPEC_IMPORT HRESULT STDAPICALLTYPE
#define THEMEAPI_(type)  EXTERN_C DECLSPEC_IMPORT type STDAPICALLTYPE
#endif

typedef HANDLE HTHEME;

#define DTT_GRAYED  0x1

#define HTTB_BACKGROUNDSEG  0x0000
#define HTTB_FIXEDBORDER  0x0002
#define HTTB_CAPTION  0x0004
#define HTTB_RESIZINGBORDER_LEFT  0x0010
#define HTTB_RESIZINGBORDER_TOP  0x0020
#define HTTB_RESIZINGBORDER_RIGHT  0x0040
#define HTTB_RESIZINGBORDER_BOTTOM  0x0080
#define HTTB_RESIZINGBORDER  (HTTB_RESIZINGBORDER_LEFT|HTTB_RESIZINGBORDER_TOP|HTTB_RESIZINGBORDER_RIGHT|HTTB_RESIZINGBORDER_BOTTOM)
#define HTTB_SIZINGTEMPLATE  0x0100
#define HTTB_SYSTEMSIZINGMARGINS  0x0200

#define MAX_INTLIST_COUNT  10

#define ETDT_DISABLE  0x00000001
#define ETDT_ENABLE  0x00000002
#define ETDT_USETABTEXTURE  0x00000004
#define ETDT_ENABLETAB  (ETDT_ENABLE|ETDT_USETABTEXTURE)

#define STAP_ALLOW_NONCLIENT  (1<<0)
#define STAP_ALLOW_CONTROLS  (1<<1)
#define STAP_ALLOW_WEBCONTENT  (1<<2)

#define SZ_THDOCPROP_DISPLAYNAME  L"DisplayName"
#define SZ_THDOCPROP_CANONICALNAME  L"ThemeName"
#define SZ_THDOCPROP_TOOLTIP  L"ToolTip"
#define SZ_THDOCPROP_AUTHOR  L"author"

typedef enum THEMESIZE {
    TS_MIN,
    TS_TRUE,
    TS_DRAW,
};

typedef struct _MARGINS {
    int cxLeftWidth;
    int cxRightWidth;
    int cyTopHeight;
    int cyBottomHeight;
} MARGINS, *PMARGINS;

typedef struct _INTLIST {
    int iValueCount;
    int iValues[MAX_INTLIST_COUNT];
} INTLIST, *PINTLIST;

typedef enum PROPERTYORIGIN {
    PO_STATE,
    PO_PART,
    PO_CLASS,
    PO_GLOBAL,
    PO_NOTFOUND
};

THEMEAPI_(HTHEME) OpenThemeData(HWND,LPCWSTR);
THEMEAPI CloseThemeData(HTHEME);
THEMEAPI DrawThemeBackground(HTHEME,HDC,int,int,const RECT*,const RECT*);
THEMEAPI DrawThemeText(HTHEME,HDC,int,int,LPCWSTR,int,DWORD,DWORD,const RECT*);
THEMEAPI GetThemeBackgroundContentRect(HTHEME,HDC,int,int,const RECT*,RECT*);
THEMEAPI GetThemeBackgroundExtent(HTHEME,HDC,int,int,const RECT*,RECT*);
THEMEAPI GetThemePartSize(HTHEME,HDC,int,int,RECT*,enum THEMESIZE,SIZE*);
THEMEAPI GetThemeTextExtent(HTHEME,HDC,int,int,LPCWSTR,int,DWORD,const RECT*,RECT*);
THEMEAPI GetThemeTextMetrics(HTHEME,HDC,int,int,TEXTMETRIC*);
THEMEAPI GetThemeBackgroundRegion(HTHEME,HDC,int,int,const RECT*,HRGN*);
THEMEAPI HitTestThemeBackground(HTHEME,HDC,int,int,DWORD,const RECT*,HRGN,POINT,WORD*);
THEMEAPI DrawThemeEdge(HTHEME,HDC,int,int,const RECT*,UINT,UINT,RECT*);
THEMEAPI DrawThemeIcon(HTHEME,HDC,int,int,const RECT*,HIMAGELIST,int);
THEMEAPI_(BOOL) IsThemePartDefined(HTHEME,int,int);
THEMEAPI_(BOOL) IsThemeBackgroundPartiallyTransparent(HTHEME,int,int);
THEMEAPI GetThemeColor(HTHEME,int,int,int,COLORREF*);
THEMEAPI GetThemeMetric(HTHEME,HDC,int,int,int,int*);
THEMEAPI GetThemeString(HTHEME,int,int,int,LPWSTR,int);
THEMEAPI GetThemeBool(HTHEME,int,int,int,BOOL*);
THEMEAPI GetThemeInt(HTHEME,int,int,int,int*);
THEMEAPI GetThemeEnumValue(HTHEME,int,int,int,int*);
THEMEAPI GetThemePosition(HTHEME,int,int,int,POINT*);
THEMEAPI GetThemeFont(HTHEME,HDC,int,int,int,LOGFONT*);
THEMEAPI GetThemeRect(HTHEME,int,int,int,RECT*);
THEMEAPI GetThemeMargins(HTHEME,HDC,int,int,int,RECT*,MARGINS*);
THEMEAPI GetThemeIntList(HTHEME,int,int,int,INTLIST*);
THEMEAPI GetThemePropertyOrigin(HTHEME,int,int,int,enum PROPERTYORIGIN*);
THEMEAPI SetWindowTheme(HWND,LPCWSTR,LPCWSTR);
THEMEAPI GetThemeFilename(HTHEME,int,int,int,LPWSTR,int);
THEMEAPI_(COLORREF) GetThemeSysColor(HTHEME,int);
THEMEAPI_(HBRUSH) GetThemeSysColorBrush(HTHEME,int);
THEMEAPI_(BOOL) GetThemeSysBool(HTHEME,int);
THEMEAPI_(int) GetThemeSysSize(HTHEME,int);
THEMEAPI GetThemeSysFont(HTHEME,int,LOGFONT*);
THEMEAPI GetThemeSysString(HTHEME,int,LPWSTR,int);
THEMEAPI GetThemeSysInt(HTHEME,int,int*);
THEMEAPI_(BOOL) IsThemeActive();
THEMEAPI_(BOOL) IsAppThemed();
THEMEAPI_(HTHEME) GetWindowTheme(HWND);
THEMEAPI EnableThemeDialogTexture(HWND,DWORD);
THEMEAPI_(BOOL) IsThemeDialogTextureEnabled(HWND);
THEMEAPI_(DWORD) GetThemeAppProperties();
THEMEAPI_(void) SetThemeAppProperties(DWORD);
THEMEAPI GetCurrentThemeName(LPWSTR,int,LPWSTR,int,LPWSTR,int);
THEMEAPI GetThemeDocumentationProperty(LPCWSTR,LPCWSTR,LPWSTR,int);
THEMEAPI DrawThemeParentBackground(HWND,HDC,RECT*);
THEMEAPI EnableTheming(BOOL);

#endif /* _UXTHEME_H */
