#ifndef _TODAYCMN_H
#define _TODAYCMN_H

/* Today screen definitions (Windows CE) */

#define MAX_ITEMNAME  32

#define k_cTodayItemsMax  12

#define IDI_ICON  128
#define IDD_TODAY_CUSTOM  500

#define ORDINAL_INITIALIZEITEM  240
#define ORDINAL_OPTIONSDIALOGPROC  241

#define WM_TODAYCUSTOM_CLEARCACHE  (WM_USER+242)
#define WM_TODAYCUSTOM_QUERYREFRESHCACHE  (WM_USER+243)

typedef enum _TODAYLISTITEMTYPE {
    tlitOwnerInfo = 0,
    tlitAppointments,
    tlitMail,
    tlitTasks,
    tlitCustom,
    tlitNil
} TODAYLISTITEMTYPE;

typedef struct _TODAYLISTITEM {
    TCHAR szName[MAX_ITEMNAME];
    TODAYLISTITEMTYPE tlit;
    DWORD dwOrder;
    DWORD cyp;
    BOOL fEnabled;
    BOOL fOptions;
    DWORD grfFlags;
    TCHAR szDLLPath[MAX_PATH];
    HINSTANCE hinstDLL;
    HWND hwndCustom;
    BOOL fSizeOnDraw;
    BYTE *prgbCachedData;
    DWORD cbCachedData;
} TODAYLISTITEM;

typedef HWND (*PFNCUSTOMINITIALIZEITEM)(TODAYLISTITEM*,HWND);
typedef BOOL (*PFNCUSTOMOPTIONSDLGPROC)(HWND,UINT,UINT,LONG);

#endif /* _TODAYCMN_H */
