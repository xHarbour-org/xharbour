#ifndef _COMMDLG_H
#define _COMMDLG_H

/* Windows Common Dialog API definitions */

#if (WINVER >= 0x0500)
#ifdef DEFINE_GUID
DEFINE_GUID(IID_IPrintDialogCallback,0x5852a2c3,0x6530,0x11d1,0xb6,0xa3,0x0,0x0,0xf8,0x75,0x7b,0xf9);
DEFINE_GUID(IID_IPrintDialogServices,0x509aaeda,0x5639,0x11d1,0xb6,0xa1,0x0,0x0,0xf8,0x75,0x7b,0xf9);
#endif

#ifdef STDMETHOD
#undef INTERFACE
#define INTERFACE IPrintDialogCallback
DECLARE_INTERFACE_(IPrintDialogCallback, IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG, AddRef)(THIS) PURE;
    STDMETHOD_(ULONG, Release)(THIS) PURE;
    STDMETHOD(InitDone)(THIS) PURE;
    STDMETHOD(SelectionChange)(THIS) PURE;
    STDMETHOD(HandleMessage)(THIS_ HWND,UINT,WPARAM,LPARAM,LRESULT*) PURE;
};

#undef INTERFACE
#define INTERFACE IPrintDialogServices
DECLARE_INTERFACE_(IPrintDialogServices, IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG, AddRef)(THIS) PURE;
    STDMETHOD_(ULONG, Release)(THIS) PURE;
    STDMETHOD(GetCurrentDevMode)(THIS_ LPDEVMODE,UINT*) PURE;
    STDMETHOD(GetCurrentPrinterName)(THIS_ LPTSTR,UINT*) PURE;
    STDMETHOD(GetCurrentPortName)(THIS_ LPTSTR,UINT*) PURE;
};
#endif /* STDMETHOD */
#endif /* WINVER >= 0x0500 */

#include <prsht.h>
#include <pshpack1.h>

#ifdef __cplusplus
extern "C" {
#endif

#include <cderr.h>

#define LBSELCHSTRINGA  "commdlg_LBSelChangedNotify"
#define SHAREVISTRINGA  "commdlg_ShareViolation"
#define FILEOKSTRINGA   "commdlg_FileNameOK"
#define COLOROKSTRINGA  "commdlg_ColorOK"
#define SETRGBSTRINGA   "commdlg_SetRGBColor"
#define HELPMSGSTRINGA  "commdlg_help"
#define FINDMSGSTRINGA  "commdlg_FindReplace"

#define LBSELCHSTRINGW  L"commdlg_LBSelChangedNotify"
#define SHAREVISTRINGW  L"commdlg_ShareViolation"
#define FILEOKSTRINGW   L"commdlg_FileNameOK"
#define COLOROKSTRINGW  L"commdlg_ColorOK"
#define SETRGBSTRINGW   L"commdlg_SetRGBColor"
#define HELPMSGSTRINGW  L"commdlg_help"
#define FINDMSGSTRINGW  L"commdlg_FindReplace"

#define CDN_FIRST  (0U-601U)
#define CDN_LAST  (0U-699U)

#define CDN_INITDONE  (CDN_FIRST-0)
#define CDN_SELCHANGE  (CDN_FIRST-1)
#define CDN_FOLDERCHANGE  (CDN_FIRST-2)
#define CDN_SHAREVIOLATION  (CDN_FIRST-3)
#define CDN_HELP  (CDN_FIRST-4)
#define CDN_FILEOK  (CDN_FIRST-5)
#define CDN_TYPECHANGE  (CDN_FIRST-6)
#define CDN_INCLUDEITEM  (CDN_FIRST-7)

#define CDM_FIRST  (WM_USER+100)
#define CDM_LAST  (WM_USER+200)

#define CDM_GETSPEC  (CDM_FIRST+0)
#define CDM_GETFILEPATH  (CDM_FIRST+1)
#define CDM_GETFOLDERPATH  (CDM_FIRST+2)
#define CDM_GETFOLDERIDLIST  (CDM_FIRST+3)
#define CDM_SETCONTROLTEXT  (CDM_FIRST+4)
#define CDM_HIDECONTROL  (CDM_FIRST+5)
#define CDM_SETDEFEXT  (CDM_FIRST+6)

#define WM_CHOOSEFONT_GETLOGFONT  (WM_USER+1)
#define WM_CHOOSEFONT_SETLOGFONT  (WM_USER+101)
#define WM_CHOOSEFONT_SETFLAGS  (WM_USER+102)

#define WM_PSD_PAGESETUPDLG  (WM_USER+0)
#define WM_PSD_FULLPAGERECT  (WM_USER+1)
#define WM_PSD_MINMARGINRECT  (WM_USER+2)
#define WM_PSD_MARGINRECT  (WM_USER+3)
#define WM_PSD_GREEKTEXTRECT  (WM_USER+4)
#define WM_PSD_ENVSTAMPRECT  (WM_USER+5)
#define WM_PSD_YAFULLPAGERECT  (WM_USER+6)

#define CC_RGBINIT  0x00000001
#define CC_FULLOPEN   0x00000002
#define CC_PREVENTFULLOPEN  0x00000004
#define CC_SHOWHELP  0x00000008
#define CC_ENABLEHOOK  0x00000010
#define CC_ENABLETEMPLATE  0x00000020
#define CC_ENABLETEMPLATEHANDLE  0x00000040
#define CC_SOLIDCOLOR  0x00000080
#define CC_ANYCOLOR  0x00000100

#define CF_SCREENFONTS  0x00000001
#define CF_PRINTERFONTS  0x00000002
#define CF_BOTH  (CF_SCREENFONTS|CF_PRINTERFONTS)
#define CF_SHOWHELP  0x00000004
#define CF_ENABLEHOOK  0x00000008
#define CF_ENABLETEMPLATE  0x00000010
#define CF_ENABLETEMPLATEHANDLE  0x00000020
#define CF_INITTOLOGFONTSTRUCT  0x00000040
#define CF_USESTYLE  0x00000080
#define CF_EFFECTS  0x00000100
#define CF_APPLY  0x00000200
#define CF_ANSIONLY  0x00000400
#define CF_SCRIPTSONLY  CF_ANSIONLY
#define CF_NOVECTORFONTS  0x00000800
#define CF_NOOEMFONTS  CF_NOVECTORFONTS
#define CF_NOSIMULATIONS  0x00001000
#define CF_LIMITSIZE  0x00002000
#define CF_FIXEDPITCHONLY  0x00004000
#define CF_WYSIWYG  0x00008000
#define CF_FORCEFONTEXIST  0x00010000
#define CF_SCALABLEONLY  0x00020000
#define CF_TTONLY  0x00040000
#define CF_NOFACESEL  0x00080000
#define CF_NOSTYLESEL  0x00100000
#define CF_NOSIZESEL  0x00200000
#define CF_SELECTSCRIPT  0x00400000
#define CF_NOSCRIPTSEL  0x00800000
#define CF_NOVERTFONTS  0x01000000

#define SIMULATED_FONTTYPE  0x8000
#define PRINTER_FONTTYPE  0x4000
#define SCREEN_FONTTYPE  0x2000
#define BOLD_FONTTYPE  0x0100
#define ITALIC_FONTTYPE  0x0200
#define REGULAR_FONTTYPE  0x0400

#define OFN_READONLY  0x00000001
#define OFN_OVERWRITEPROMPT  0x00000002
#define OFN_HIDEREADONLY  0x00000004
#define OFN_NOCHANGEDIR  0x00000008
#define OFN_SHOWHELP  0x00000010
#define OFN_ENABLEHOOK  0x00000020
#define OFN_ENABLETEMPLATE  0x00000040
#define OFN_ENABLETEMPLATEHANDLE  0x00000080
#define OFN_NOVALIDATE  0x00000100
#define OFN_ALLOWMULTISELECT  0x00000200
#define OFN_EXTENSIONDIFFERENT  0x00000400
#define OFN_PATHMUSTEXIST  0x00000800
#define OFN_FILEMUSTEXIST  0x00001000
#define OFN_CREATEPROMPT  0x00002000
#define OFN_SHAREAWARE  0x00004000
#define OFN_NOREADONLYRETURN  0x00008000
#define OFN_NOTESTFILECREATE  0x00010000
#define OFN_NONETWORKBUTTON  0x00020000
#define OFN_NOLONGNAMES  0x00040000
#define OFN_EXPLORER  0x00080000
#define OFN_NODEREFERENCELINKS  0x00100000
#define OFN_LONGNAMES  0x00200000
#define OFN_ENABLEINCLUDENOTIFY  0x00400000
#define OFN_ENABLESIZING  0x00800000
#if (_WIN32_WINNT >= 0x0500)
#define OFN_DONTADDTORECENT  0x02000000
#define OFN_FORCESHOWHIDDEN  0x10000000
#define OFN_EX_NOPLACESBAR  0x00000001
#endif /* _WIN32_WINNT >= 0x0500 */
#ifdef _WINCE
#define OFN_PROJECT  0x00400000
#define OFN_PROPERTY  0x00800000
#define OFN_SHOW_ALL  0x01000000
#define OFN_SHOWFAVORITESOPTS  0x02000000
#endif /* _WINCE */

#define OFN_SHAREFALLTHROUGH  2
#define OFN_SHARENOWARN  1
#define OFN_SHAREWARN  0

#define FR_DOWN  0x00000001
#define FR_WHOLEWORD  0x00000002
#define FR_MATCHCASE  0x00000004
#define FR_FINDNEXT  0x00000008
#define FR_REPLACE  0x00000010
#define FR_REPLACEALL  0x00000020
#define FR_DIALOGTERM  0x00000040
#define FR_SHOWHELP  0x00000080
#define FR_ENABLEHOOK  0x00000100
#define FR_ENABLETEMPLATE  0x00000200
#define FR_NOUPDOWN  0x00000400
#define FR_NOMATCHCASE  0x00000800
#define FR_NOWHOLEWORD  0x00001000
#define FR_ENABLETEMPLATEHANDLE  0x00002000
#define FR_HIDEUPDOWN  0x00004000
#define FR_HIDEMATCHCASE  0x00008000
#define FR_HIDEWHOLEWORD  0x00010000
#define FR_RAW  0x00020000
#define FR_MATCHDIAC  0x20000000
#define FR_MATCHKASHIDA  0x40000000
#define FR_MATCHALEFHAMZA  0x80000000

#define PD_ALLPAGES  0x00000000
#define PD_SELECTION  0x00000001
#define PD_PAGENUMS  0x00000002
#define PD_NOSELECTION  0x00000004
#define PD_NOPAGENUMS  0x00000008
#define PD_COLLATE  0x00000010
#define PD_PRINTTOFILE  0x00000020
#define PD_PRINTSETUP  0x00000040
#define PD_NOWARNING  0x00000080
#define PD_RETURNDC  0x00000100
#define PD_RETURNIC  0x00000200
#define PD_RETURNDEFAULT  0x00000400
#define PD_SHOWHELP  0x00000800
#define PD_ENABLEPRINTHOOK  0x00001000
#define PD_ENABLESETUPHOOK  0x00002000
#define PD_ENABLEPRINTTEMPLATE  0x00004000
#define PD_ENABLESETUPTEMPLATE  0x00008000
#define PD_ENABLEPRINTTEMPLATEHANDLE  0x00010000
#define PD_ENABLESETUPTEMPLATEHANDLE  0x00020000
#define PD_USEDEVMODECOPIES  0x00040000
#define PD_USEDEVMODECOPIESANDCOLLATE  0x00040000
#define PD_DISABLEPRINTTOFILE  0x00080000
#define PD_HIDEPRINTTOFILE  0x00100000
#define PD_NONETWORKBUTTON  0x00200000
#if (WINVER >= 0x0500)
#define PD_CURRENTPAGE  0x00400000
#define PD_NOCURRENTPAGE  0x00800000
#define PD_EXCLUSIONFLAGS  0x01000000
#define PD_USELARGETEMPLATE  0x10000000
#define PD_EXCL_COPIESANDCOLLATE  (DM_COPIES|DM_COLLATE)
#define PD_RESULT_CANCEL  0
#define PD_RESULT_PRINT  1
#define PD_RESULT_APPLY  2
#define START_PAGE_GENERAL  0xFFFFFFFF
#endif

#define PSD_DEFAULTMINMARGINS  0x00000000
#define PSD_INWININIINTLMEASURE  0x00000000
#define PSD_MINMARGINS  0x00000001
#define PSD_MARGINS  0x00000002
#define PSD_INTHOUSANDTHSOFINCHES  0x00000004
#define PSD_INHUNDREDTHSOFMILLIMETERS  0x00000008
#define PSD_DISABLEMARGINS  0x00000010
#define PSD_DISABLEPRINTER  0x00000020
#define PSD_NOWARNING  0x00000080
#define PSD_DISABLEORIENTATION  0x00000100
#define PSD_RETURNDEFAULT  0x00000400
#define PSD_DISABLEPAPER  0x00000200
#define PSD_SHOWHELP  0x00000800
#define PSD_ENABLEPAGESETUPHOOK  0x00002000
#define PSD_ENABLEPAGESETUPTEMPLATE  0x00008000
#define PSD_ENABLEPAGESETUPTEMPLATEHANDLE  0x00020000
#define PSD_ENABLEPAGEPAINTHOOK  0x00040000
#define PSD_DISABLEPAGEPAINTING  0x00080000
#define PSD_NONETWORKBUTTON  0x00200000

#define CD_LBSELNOITEMS  (-1)
#define CD_LBSELCHANGE  0
#define CD_LBSELSUB  1
#define CD_LBSELADD  2

#define DN_DEFAULTPRN  1

#define CommDlg_OpenSave_GetSpecA(d,s,m)  (int)SNDMSG(d,CDM_GETSPEC,(WPARAM)m,(LPARAM)(LPSTR)s)
#define CommDlg_OpenSave_GetSpecW(d,s,m)  (int)SNDMSG(d,CDM_GETSPEC,(WPARAM)m,(LPARAM)(LPWSTR)s)
#define CommDlg_OpenSave_GetFilePathA(d,s,m)  (int)SNDMSG(d,CDM_GETFILEPATH,(WPARAM)m,(LPARAM)(LPSTR)s)
#define CommDlg_OpenSave_GetFilePathW(d,s,m)  (int)SNDMSG(d,CDM_GETFILEPATH,(WPARAM)m,(LPARAM)(LPWSTR)s)
#define CommDlg_OpenSave_GetFolderPathA(d,s,m)  (int)SNDMSG(d,CDM_GETFOLDERPATH,(WPARAM)m,(LPARAM)(LPSTR)s)
#define CommDlg_OpenSave_GetFolderPathW(d,s,m)  (int)SNDMSG(d,CDM_GETFOLDERPATH,(WPARAM)m,(LPARAM)(LPWSTR)s)
#define CommDlg_OpenSave_GetFolderIDList(d,i,m)  (int)SNDMSG(d,CDM_GETFOLDERIDLIST,(WPARAM)m,(LPARAM)(PVOID)i)
#define CommDlg_OpenSave_SetControlText(d,i,t)  (void)SNDMSG(d,CDM_SETCONTROLTEXT,(WPARAM)i,(LPARAM)(LPSTR)t)
#define CommDlg_OpenSave_HideControl(d,i)  (void)SNDMSG(d,CDM_HIDECONTROL,(WPARAM)i,0)
#define CommDlg_OpenSave_SetDefExt(d,s)  (void)SNDMSG(d,CDM_SETDEFEXT,0,(LPARAM)(LPSTR)s)

typedef UINT_PTR (APIENTRY *__CDHOOKPROC)(HWND,UINT,WPARAM,LPARAM);

typedef __CDHOOKPROC LPCCHOOKPROC;
typedef __CDHOOKPROC LPCFHOOKPROC;
typedef __CDHOOKPROC LPFRHOOKPROC;
typedef __CDHOOKPROC LPOFNHOOKPROC;
typedef __CDHOOKPROC LPPAGEPAINTHOOK;
typedef __CDHOOKPROC LPPAGESETUPHOOK;
typedef __CDHOOKPROC LPSETUPHOOKPROC;
typedef __CDHOOKPROC LPPRINTHOOKPROC;

typedef struct tagCHOOSECOLORA {
    DWORD lStructSize;
    HWND hwndOwner;
    HWND hInstance;
    COLORREF rgbResult;
    COLORREF *lpCustColors;
    DWORD Flags;
    LPARAM lCustData;
    LPCCHOOKPROC lpfnHook;
    LPCSTR lpTemplateName;
} CHOOSECOLORA, *LPCHOOSECOLORA;

typedef struct tagCHOOSECOLORW {
    DWORD lStructSize;
    HWND hwndOwner;
    HWND hInstance;
    COLORREF rgbResult;
    COLORREF *lpCustColors;
    DWORD Flags;
    LPARAM lCustData;
    LPCCHOOKPROC lpfnHook;
    LPCWSTR lpTemplateName;
} CHOOSECOLORW, *LPCHOOSECOLORW;

typedef struct tagCHOOSEFONTA {
    DWORD lStructSize;
    HWND hwndOwner;
    HDC hDC;
    LPLOGFONTA lpLogFont;
    INT iPointSize;
    DWORD Flags;
    COLORREF rgbColors;
    LPARAM lCustData;
    LPCFHOOKPROC lpfnHook;
    LPCSTR lpTemplateName;
    HINSTANCE hInstance;
    LPSTR lpszStyle;
    WORD nFontType;
    WORD ___MISSING_ALIGNMENT__;
    INT nSizeMin;
    INT nSizeMax;
} CHOOSEFONTA, *LPCHOOSEFONTA;

typedef struct tagCHOOSEFONTW {
    DWORD lStructSize;
    HWND hwndOwner;
    HDC hDC;
    LPLOGFONTW lpLogFont;
    INT iPointSize;
    DWORD Flags;
    COLORREF rgbColors;
    LPARAM lCustData;
    LPCFHOOKPROC lpfnHook;
    LPCWSTR lpTemplateName;
    HINSTANCE hInstance;
    LPWSTR lpszStyle;
    WORD nFontType;
    WORD ___MISSING_ALIGNMENT__;
    INT nSizeMin;
    INT nSizeMax;
} CHOOSEFONTW, *LPCHOOSEFONTW;

typedef struct tagDEVNAMES {
    WORD wDriverOffset;
    WORD wDeviceOffset;
    WORD wOutputOffset;
    WORD wDefault;
} DEVNAMES, *LPDEVNAMES;

typedef struct tagFINDREPLACEA {
    DWORD lStructSize;
    HWND hwndOwner;
    HINSTANCE hInstance;
    DWORD Flags;
    LPSTR lpstrFindWhat;
    LPSTR lpstrReplaceWith;
    WORD wFindWhatLen;
    WORD wReplaceWithLen;
    LPARAM lCustData;
    LPFRHOOKPROC lpfnHook;
    LPCSTR lpTemplateName;
} FINDREPLACEA, *LPFINDREPLACEA;

typedef struct tagFINDREPLACEW {
    DWORD lStructSize;
    HWND hwndOwner;
    HINSTANCE hInstance;
    DWORD Flags;
    LPWSTR lpstrFindWhat;
    LPWSTR lpstrReplaceWith;
    WORD wFindWhatLen;
    WORD wReplaceWithLen;
    LPARAM lCustData;
    LPFRHOOKPROC lpfnHook;
    LPCWSTR lpTemplateName;
} FINDREPLACEW, *LPFINDREPLACEW;

typedef struct tagOFNA {
    DWORD lStructSize;
    HWND hwndOwner;
    HINSTANCE hInstance;
    LPCSTR lpstrFilter;
    LPSTR lpstrCustomFilter;
    DWORD nMaxCustFilter;
    DWORD nFilterIndex;
    LPSTR lpstrFile;
    DWORD nMaxFile;
    LPSTR lpstrFileTitle;
    DWORD nMaxFileTitle;
    LPCSTR lpstrInitialDir;
    LPCSTR lpstrTitle;
    DWORD Flags;
    WORD nFileOffset;
    WORD nFileExtension;
    LPCSTR lpstrDefExt;
    LPARAM lCustData;
    LPOFNHOOKPROC lpfnHook;
    LPCSTR lpTemplateName;
#if (_WIN32_WINNT >= 0x0500)
    void *pvReserved;
    DWORD dwReserved;
    DWORD FlagsEx;
#endif
} OPENFILENAMEA, *LPOPENFILENAMEA;

typedef struct tagOFNW {
    DWORD lStructSize;
    HWND hwndOwner;
    HINSTANCE hInstance;
    LPCWSTR lpstrFilter;
    LPWSTR lpstrCustomFilter;
    DWORD nMaxCustFilter;
    DWORD nFilterIndex;
    LPWSTR lpstrFile;
    DWORD nMaxFile;
    LPWSTR lpstrFileTitle;
    DWORD nMaxFileTitle;
    LPCWSTR lpstrInitialDir;
    LPCWSTR lpstrTitle;
    DWORD Flags;
    WORD nFileOffset;
    WORD nFileExtension;
    LPCWSTR lpstrDefExt;
    LPARAM lCustData;
    LPOFNHOOKPROC lpfnHook;
    LPCWSTR lpTemplateName;
#if (_WIN32_WINNT >= 0x0500)
    void *pvReserved;
    DWORD dwReserved;
    DWORD FlagsEx;
#endif
} OPENFILENAMEW, *LPOPENFILENAMEW;

typedef struct _OFNOTIFYA {
    NMHDR hdr;
    LPOPENFILENAMEA lpOFN;
    LPSTR pszFile;
} OFNOTIFYA, *LPOFNOTIFYA;

typedef struct _OFNOTIFYW {
    NMHDR hdr;
    LPOPENFILENAMEW lpOFN;
    LPWSTR pszFile;
} OFNOTIFYW, *LPOFNOTIFYW;

typedef struct _OFNOTIFYEXA {
    NMHDR hdr;
    LPOPENFILENAMEA lpOFN;
    LPVOID psf;
    LPVOID pidl;
} OFNOTIFYEXA, *LPOFNOTIFYEXA;

typedef struct _OFNOTIFYEXW {
    NMHDR hdr;
    LPOPENFILENAMEW lpOFN;
    LPVOID psf;
    LPVOID pidl;
} OFNOTIFYEXW, *LPOFNOTIFYEXW;

typedef struct tagPSDA {
    DWORD lStructSize;
    HWND hwndOwner;
    HGLOBAL hDevMode;
    HGLOBAL hDevNames;
    DWORD Flags;
    POINT ptPaperSize;
    RECT rtMinMargin;
    RECT rtMargin;
    HINSTANCE hInstance;
    LPARAM lCustData;
    LPPAGESETUPHOOK lpfnPageSetupHook;
    LPPAGEPAINTHOOK lpfnPagePaintHook;
    LPCSTR lpPageSetupTemplateName;
    HGLOBAL hPageSetupTemplate;
} PAGESETUPDLGA, *LPPAGESETUPDLGA;

typedef struct tagPSDW {
    DWORD lStructSize;
    HWND hwndOwner;
    HGLOBAL hDevMode;
    HGLOBAL hDevNames;
    DWORD Flags;
    POINT ptPaperSize;
    RECT rtMinMargin;
    RECT rtMargin;
    HINSTANCE hInstance;
    LPARAM lCustData;
    LPPAGESETUPHOOK lpfnPageSetupHook;
    LPPAGEPAINTHOOK lpfnPagePaintHook;
    LPCWSTR lpPageSetupTemplateName;
    HGLOBAL hPageSetupTemplate;
} PAGESETUPDLGW, *LPPAGESETUPDLGW;

typedef struct tagPDA {
    DWORD lStructSize;
    HWND hwndOwner;
    HGLOBAL hDevMode;
    HGLOBAL hDevNames;
    HDC hDC;
    DWORD Flags;
    WORD nFromPage;
    WORD nToPage;
    WORD nMinPage;
    WORD nMaxPage;
    WORD nCopies;
    HINSTANCE hInstance;
    LPARAM lCustData;
    LPPRINTHOOKPROC lpfnPrintHook;
    LPSETUPHOOKPROC lpfnSetupHook;
    LPCSTR lpPrintTemplateName;
    LPCSTR lpSetupTemplateName;
    HGLOBAL hPrintTemplate;
    HGLOBAL hSetupTemplate;
} PRINTDLGA, *LPPRINTDLGA;

typedef struct tagPDW {
    DWORD lStructSize;
    HWND hwndOwner;
    HGLOBAL hDevMode;
    HGLOBAL hDevNames;
    HDC hDC;
    DWORD Flags;
    WORD nFromPage;
    WORD nToPage;
    WORD nMinPage;
    WORD nMaxPage;
    WORD nCopies;
    HINSTANCE hInstance;
    LPARAM lCustData;
    LPPRINTHOOKPROC lpfnPrintHook;
    LPSETUPHOOKPROC lpfnSetupHook;
    LPCWSTR lpPrintTemplateName;
    LPCWSTR lpSetupTemplateName;
    HGLOBAL hPrintTemplate;
    HGLOBAL hSetupTemplate;
} PRINTDLGW, *LPPRINTDLGW;

#if (WINVER >= 0x0500)
typedef struct tagPRINTPAGERANGE {
    DWORD nFromPage;
    DWORD nToPage;
} PRINTPAGERANGE, *LPPRINTPAGERANGE;

typedef struct tagPDEXA {
    DWORD lStructSize;
    HWND hwndOwner;
    HGLOBAL hDevMode;
    HGLOBAL hDevNames;
    HDC hDC;
    DWORD Flags;
    DWORD Flags2;
    DWORD ExclusionFlags;
    DWORD nPageRanges;
    DWORD nMaxPageRanges;
    LPPRINTPAGERANGE lpPageRanges;
    DWORD nMinPage;
    DWORD nMaxPage;
    DWORD nCopies;
    HINSTANCE hInstance;
    LPCSTR lpPrintTemplateName;
    LPUNKNOWN lpCallback;
    DWORD nPropertyPages;
    HPROPSHEETPAGE *lphPropertyPages;
    DWORD nStartPage;
    DWORD dwResultAction;
} PRINTDLGEXA, *LPPRINTDLGEXA;

typedef struct tagPDEXW {
    DWORD lStructSize;
    HWND hwndOwner;
    HGLOBAL hDevMode;
    HGLOBAL hDevNames;
    HDC hDC;
    DWORD Flags;
    DWORD Flags2;
    DWORD ExclusionFlags;
    DWORD nPageRanges;
    DWORD nMaxPageRanges;
    LPPRINTPAGERANGE lpPageRanges;
    DWORD nMinPage;
    DWORD nMaxPage;
    DWORD nCopies;
    HINSTANCE hInstance;
    LPCWSTR lpPrintTemplateName;
    LPUNKNOWN lpCallback;
    DWORD nPropertyPages;
    HPROPSHEETPAGE *lphPropertyPages;
    DWORD nStartPage;
    DWORD dwResultAction;
} PRINTDLGEXW, *LPPRINTDLGEXW;
#endif /* WINVER >= 0x0500 */

BOOL WINAPI ChooseColorA(LPCHOOSECOLORA);
BOOL WINAPI ChooseColorW(LPCHOOSECOLORW);
BOOL WINAPI ChooseFontA(LPCHOOSEFONTA);
BOOL WINAPI ChooseFontW(LPCHOOSEFONTW);
DWORD WINAPI CommDlgExtendedError(void);
HWND WINAPI FindTextA(LPFINDREPLACEA);
HWND WINAPI FindTextW(LPFINDREPLACEW);
short WINAPI GetFileTitleA(LPCSTR,LPSTR,WORD);
short WINAPI GetFileTitleW(LPCWSTR,LPWSTR,WORD);
BOOL WINAPI GetOpenFileNameA(LPOPENFILENAMEA);
BOOL WINAPI GetOpenFileNameW(LPOPENFILENAMEW);
BOOL WINAPI GetSaveFileNameA(LPOPENFILENAMEA);
BOOL WINAPI GetSaveFileNameW(LPOPENFILENAMEW);
BOOL WINAPI PageSetupDlgA(LPPAGESETUPDLGA);
BOOL WINAPI PageSetupDlgW(LPPAGESETUPDLGW);
BOOL WINAPI PrintDlgA(LPPRINTDLGA);
BOOL WINAPI PrintDlgW(LPPRINTDLGW);
HWND WINAPI ReplaceTextA(LPFINDREPLACEA);
HWND WINAPI ReplaceTextW(LPFINDREPLACEW);
#if (WINVER >= 0x0500)
HRESULT WINAPI PrintDlgExA(LPPRINTDLGEXA);
HRESULT WINAPI PrintDlgExW(LPPRINTDLGEXW);
#endif

#ifdef UNICODE
typedef CHOOSECOLORW CHOOSECOLOR,*LPCHOOSECOLOR;
typedef CHOOSEFONTW CHOOSEFONT,*LPCHOOSEFONT;
typedef FINDREPLACEW FINDREPLACE,*LPFINDREPLACE;
typedef OPENFILENAMEW OPENFILENAME,*LPOPENFILENAME;
typedef OFNOTIFYW OFNOTIFY,*LPOFNOTIFY;
typedef OFNOTIFYEXW OFNOTIFYEX,*LPOFNOTIFYEX;
typedef PAGESETUPDLGW PAGESETUPDLG,*LPPAGESETUPDLG;
typedef PRINTDLGW PRINTDLG,*LPPRINTDLG;
#define LBSELCHSTRING LBSELCHSTRINGW
#define SHAREVISTRING SHAREVISTRINGW
#define FILEOKSTRING FILEOKSTRINGW
#define COLOROKSTRING COLOROKSTRINGW
#define SETRGBSTRING SETRGBSTRINGW
#define HELPMSGSTRING HELPMSGSTRINGW
#define FINDMSGSTRING FINDMSGSTRINGW
#define ChooseColor ChooseColorW
#define ChooseFont ChooseFontW
#define FindText FindTextW
#define GetFileTitle GetFileTitleW
#define GetOpenFileName GetOpenFileNameW
#define GetSaveFileName GetSaveFileNameW
#define PrintDlg PrintDlgW
#define PageSetupDlg PageSetupDlgW
#define ReplaceText ReplaceTextW
#define CommDlg_OpenSave_GetSpec CommDlg_OpenSave_GetSpecW
#define CommDlg_OpenSave_GetFilePath CommDlg_OpenSave_GetFilePathW
#define CommDlg_OpenSave_GetFolderPath CommDlg_OpenSave_GetFolderPathW
#if (WINVER >= 0x0500)
typedef PRINTDLGEXW PRINTDLGEX;
typedef LPPRINTDLGEXW LPPRINTDLGEX;
#define PrintDlgEx PrintDlgExW
#endif /* WINVER >= 0x0500 */
#else /* UNICODE */
typedef CHOOSECOLORA CHOOSECOLOR,*LPCHOOSECOLOR;
typedef CHOOSEFONTA CHOOSEFONT,*LPCHOOSEFONT;
typedef FINDREPLACEA FINDREPLACE,*LPFINDREPLACE;
typedef OPENFILENAMEA OPENFILENAME,*LPOPENFILENAME;
typedef OFNOTIFYA OFNOTIFY,*LPOFNOTIFY;
typedef OFNOTIFYEXA OFNOTIFYEX,*LPOFNOTIFYEX;
typedef PAGESETUPDLGA PAGESETUPDLG,*LPPAGESETUPDLG;
typedef PRINTDLGA PRINTDLG,*LPPRINTDLG;
#define LBSELCHSTRING LBSELCHSTRINGA
#define SHAREVISTRING SHAREVISTRINGA
#define FILEOKSTRING FILEOKSTRINGA
#define COLOROKSTRING COLOROKSTRINGA
#define SETRGBSTRING SETRGBSTRINGA
#define HELPMSGSTRING HELPMSGSTRINGA
#define FINDMSGSTRING FINDMSGSTRINGA
#define ChooseColor ChooseColorA
#define ChooseFont ChooseFontA
#define FindText FindTextA
#define GetFileTitle GetFileTitleA
#define GetOpenFileName GetOpenFileNameA
#define GetSaveFileName GetSaveFileNameA
#define PrintDlg PrintDlgA
#define PageSetupDlg PageSetupDlgA
#define ReplaceText ReplaceTextA
#define CommDlg_OpenSave_GetSpec CommDlg_OpenSave_GetSpecA
#define CommDlg_OpenSave_GetFilePath CommDlg_OpenSave_GetFilePathA
#define CommDlg_OpenSave_GetFolderPath CommDlg_OpenSave_GetFolderPathA
#if (WINVER >= 0x0500)
typedef PRINTDLGEXA PRINTDLGEX;
typedef LPPRINTDLGEXA LPPRINTDLGEX;
#define PrintDlgEx PrintDlgExA
#endif /* WINVER >= 0x0500 */
#endif /* UNICODE */

#ifdef __cplusplus
}
#endif

#include <poppack.h>

#endif /* _COMMDLG_H */
