pragma pack(4)
#define LPUNKNOWN        -4
#define LPPRINTPAGERANGE -4
#define HPROPSHEETPAGE   -4
#define SIZE_T ULONG
#define OFS_MAXPATHNAME  128

typedef struct {;
    UINT style;
    WNDPROC lpfnWndProc;
    int cbClsExtra;
    int cbWndExtra;
    HINSTANCE hInstance;
    HICON hIcon;
    HCURSOR hCursor;
    HBRUSH hbrBackground;
    LPCTSTR lpszMenuName;
    LPCTSTR lpszClassName;
} WNDCLASS, *PWNDCLASS

typedef struct {;
   HWND hwndFrom;
   UINT idFrom;
   UINT code;
} NMHDR

typedef struct tagPOINT { ;
    LONG x;
    LONG y;
} POINT, *PPOINT

typedef struct {;
    HWND hwnd;
    UINT message;
    WPARAM wParam;
    LPARAM lParam;
    DWORD time;
    POINT pt;
} MSG, *PMSG

typedef struct tagSIZE { ;
    LONG cx;
    LONG cy;
} SIZE, *PSIZE

typedef struct tagTEXTMETRIC { ;
    LONG tmHeight;
    LONG tmAscent;
    LONG tmDescent;
    LONG tmInternalLeading;
    LONG tmExternalLeading;
    LONG tmAveCharWidth;
    LONG tmMaxCharWidth;
    LONG tmWeight;
    LONG tmOverhang;
    LONG tmDigitizedAspectX;
    LONG tmDigitizedAspectY;
    TCHAR tmFirstChar;
    TCHAR tmLastChar;
    TCHAR tmDefaultChar;
    TCHAR tmBreakChar;
    BYTE tmItalic;
    BYTE tmUnderlined;
    BYTE tmStruckOut;
    BYTE tmPitchAndFamily;
    BYTE tmCharSet;
} TEXTMETRIC, *PTEXTMETRIC

typedef struct _RECT { ;
    LONG left;
    LONG top;
    LONG right;
    LONG bottom;
} RECT, *PRECT

typedef struct tagPAINTSTRUCT { ;
    HDC  hdc;
    BOOL fErase;
    RECT rcPaint;
    BOOL fRestore;
    BOOL fIncUpdate;
    BYTE rgbReserved[32];
} PAINTSTRUCT, *PPAINTSTRUCT

typedef struct tagWNDCLASSEX {;
    UINT        cbSize;
    UINT        style;
    WNDPROC     lpfnWndProc;
    int         cbClsExtra;
    int         cbWndExtra;
    HINSTANCE   hInstance;
    HICON       hIcon;
    HCURSOR     hCursor;
    HBRUSH      hbrBackground;
    LPCSTR      lpszMenuName;
    LPCSTR      lpszClassName;
    HICON       hIconSm;
} WNDCLASSEX

typedef struct _OSVERSIONINFOEX {;
   DWORD dwOSVersionInfoSize;
   DWORD dwMajorVersion;
   DWORD dwMinorVersion;
   DWORD dwBuildNumber;
   DWORD dwPlatformId;
   TCHAR szCSDVersion[128];
   WORD wServicePackMajor;
   WORD wServicePackMinor;
   WORD wSuiteMask;
   BYTE wProductType;
   BYTE wReserved;
} OSVERSIONINFOEX, *POSVERSIONINFOEX

typedef struct _OSVERSIONINFO { ;
   DWORD dwOSVersionInfoSize;
   DWORD dwMajorVersion;
   DWORD dwMinorVersion;
   DWORD dwBuildNumber;
   DWORD dwPlatformId;
   TCHAR szCSDVersion[128];
} OSVERSIONINFO

typedef struct tagLOGFONT { ;
   LONG  lfHeight;
   LONG  lfWidth;
   LONG  lfEscapement;
   LONG  lfOrientation;
   LONG  lfWeight;
   BYTE  lfItalic;
   BYTE  lfUnderline;
   BYTE  lfStrikeOut;
   BYTE  lfCharSet;
   BYTE  lfOutPrecision;
   BYTE  lfClipPrecision;
   BYTE  lfQuality;
   BYTE  lfPitchAndFamily;
   TCHAR lfFaceName[32];
} LOGFONT

typedef struct tagXLOGFONT { ;
   LONG  lfHeight;
   LONG  lfWidth;
   LONG  lfEscapement;
   LONG  lfOrientation;
   LONG  lfWeight;
   BYTE  lfItalic;
   BYTE  lfUnderline;
   BYTE  lfStrikeOut;
   BYTE  lfCharSet;
   BYTE  lfOutPrecision;
   BYTE  lfClipPrecision;
   BYTE  lfQuality;
   BYTE  lfPitchAndFamily;
   TCHAR lfFaceName[64];
} XLOGFONT

typedef struct tagENUMLOGFONT {;
  LOGFONT elfLogFont;
  TCHAR   elfFullName[LF_FULLFACESIZE];
  TCHAR   elfStyle[LF_FACESIZE];
} ENUMLOGFONT

typedef struct tagNONCLIENTMETRICS { ;
    UINT    cbSize;
    int     iBorderWidth;
    int     iScrollWidth;
    int     iScrollHeight;
    int     iCaptionWidth;
    int     iCaptionHeight;
    LOGFONT lfCaptionFont;
    int     iSmCaptionWidth;
    int     iSmCaptionHeight;
    LOGFONT lfSmCaptionFont;
    int     iMenuWidth;
    int     iMenuHeight;
    LOGFONT lfMenuFont;
    LOGFONT lfStatusFont;
    LOGFONT lfMessageFont;
} NONCLIENTMETRICS

typedef struct tagDRAWITEMSTRUCT {;
   UINT  CtlType;
   UINT  CtlID;
   UINT  itemID;
   UINT  itemAction;
   INT   itemState;
   HWND  hwndItem;
   HDC   hDC;
   RECT  rcItem;
   DWORD itemData;
} DRAWITEMSTRUCT

typedef struct MEASUREITEMSTRUCT {;
   UINT CtlType;
   UINT CtlID;
   UINT itemID;
   UINT itemWidth;
   UINT itemHeight;
   ULONG itemData;
} MEASUREITEMSTRUCT

typedef struct tagBUTTONINFO {;
    UINT cbSize;
    DWORD dwMask;
    int idCommand;
    int iImage;
    BYTE fsState;
    BYTE fsStyle;
    WORD cx;
    DWORD lParam;
    LPTSTR pszText;
    int cchText;
} TBBUTTONINFOA

typedef struct tagTBBUTTON {;
    int iBitmap;
    int idCommand;
    BYTE fsState;
    BYTE fsStyle;
    LONG dwData;
    LONG iString;
} TBBUTTON

typedef struct tagCOLORSCHEME {;
    DWORD dwSize;
    COLORREF clrBtnHighlight;
    COLORREF clrBtnShadow;
} COLORSCHEME, *LPCOLORSCHEME

typedef struct tagREBARINFO;
{;
    UINT        cbSize;
    UINT        fMask;
    HIMAGELIST  himl;
}   REBARINFO, FAR *LPREBARINFO

typedef struct tagREBARBANDINFOA;
{;
    UINT        cbSize;
    UINT        fMask;
    UINT        fStyle;
    COLORREF    clrFore;
    COLORREF    clrBack;
    LPSTR       lpText;
    UINT        cch;
    int         iImage;
    HWND        hwndChild;
    UINT        cxMinChild;
    UINT        cyMinChild;
    UINT        cx;
    HBITMAP     hbmBack;
    UINT        wID;
    UINT        cyChild;
    UINT        cyMaxChild;
    UINT        cyIntegral;
    UINT        cxIdeal;
    LPARAM      lParam;
    UINT        cxHeader;
} REBARBANDINFOA

typedef struct tagNMCUSTOMDRAWINFO {;
    NMHDR  hdr;
    DWORD  dwDrawStage;
    HDC    hdc;
    RECT   rc;
    DWORD  dwItemSpec;
    UINT   uItemState;
    LPARAM lItemlParam;
} NMCUSTOMDRAW

typedef struct _NMTBCUSTOMDRAW {;
    NMCUSTOMDRAW nmcd;
    HBRUSH       hbrMonoDither;
    HBRUSH       hbrLines;
    HANDLE       hpenLines;
    COLORREF     clrText;
    COLORREF     clrMark;
    COLORREF     clrTextHighlight;
    COLORREF     clrBtnFace;
    COLORREF     clrBtnHighlight;
    COLORREF     clrHighlightHotTrack;
    RECT         rcText;
    int          nStringBkMode;
    int          nHLStringBkMode;
    int          iListGap;
} NMTBCUSTOMDRAW

typedef struct tagNMTBHOTITEM {;
    NMHDR hdr;
    int idOld;
    int idNew;
    DWORD dwFlags;
} NMTBHOTITEM, *LPNMTBHOTITEM

typedef struct tagTOOLINFO{;
    UINT      cbSize;
    UINT      uFlags;
    HWND      hwnd;
    UINT      uId;
    RECT      rect;
    HINSTANCE hinst;
    LPTSTR    lpszText;
    LPARAM lParam;
} TOOLINFOA

typedef struct tagSCROLLBARINFO {;
    DWORD cbSize;
    RECT rcScrollBar;
    int dxyLineButton;
    int xyThumbTop;
    int xyThumbBottom;
    int reserved;
    DWORD rgstate[CCHILDREN_SCROLLBAR+1];
} SCROLLBARINFO, *PSCROLLBARINFO, *LPSCROLLBARINFO

typedef struct {;
    HWND hwnd;
    HWND hwndInsertAfter;
    int x;
    int y;
    int cx;
    int cy;
    UINT flags;
} WINDOWPOS

typedef struct tagMDINEXTMENU {;
    HANDLE hmenuIn;
    HANDLE hmenuNext;
    HWND  hwndNext;
} MDINEXTMENU

typedef struct tagCLIENTCREATESTRUCT {;
    HANDLE hWindowMenu;
    UINT   idFirstChild;
} CLIENTCREATESTRUCT

typedef struct tagCREATESTRUCT {;
    LPVOID lpCreateParams;
    HINSTANCE hInstance;
    HMENU hMenu;
    HWND hwndParent;
    int cy;
    int cx;
    int y;
    int x;
    LONG style;
    LPCTSTR lpszName;
    LPCTSTR lpszClass;
    DWORD dwExStyle;
} CREATESTRUCT, *LPCREATESTRUCT

typedef struct {;
    NMHDR     hdr;
    LPTSTR    lpszText;
    char      szText[80];
    HINSTANCE hinst;
    UINT      uflags;
} TOOLTIPTEXT

typedef struct tagNMREBAR {;
    NMHDR hdr;
    DWORD dwMask;
    UINT uBand;
    UINT fStyle;
    UINT wID;
    LPARAM lParam;
} NMREBAR

typedef struct tagNMREBARCHEVRON {;
    NMHDR hdr;
    UINT uBand;
    UINT wID;
    LPARAM lParam;
    RECT rc;
    LPARAM lParamNM;
} NMREBARCHEVRON, *LPNMREBARCHEVRON

typedef struct tagMENUINFO {;
   DWORD   cbSize;
   DWORD   fMask;
   DWORD   dwStyle;
   UINT    cyMax;
   HBRUSH  hbrBack;
   DWORD   dwContextHelpID;
   ULONG  dwMenuData;
}MENUINFO

typedef struct tagMENUITEMINFO {;
   UINT    cbSize;
   UINT    fMask;
   UINT    fType;
   UINT    fState;
   UINT    wID;
   HMENU   hSubMenu;
   HBITMAP hbmpChecked;
   HBITMAP hbmpUnchecked;
   ULONG   dwItemData;
   LPTSTR  dwTypeData;
   UINT    cch;
   HBITMAP hbmpItem;
} MENUITEMINFO

typedef struct tagNMTOOLBAR {;
    NMHDR   hdr;
    int     iItem;
    TBBUTTON tbButton;
    int     cchText;
    LPSTR   pszText;
    RECT    rcButton;
} NMTOOLBARA

typedef struct _ICONINFO {;
    BOOL    fIcon;
    DWORD   xHotspot;
    DWORD   yHotspot;
    HBITMAP hbmMask;
    HBITMAP hbmColor;
} ICONINFO

typedef struct tagSCROLLINFO{;
    UINT    cbSize;
    UINT    fMask;
    int     nMin;
    int     nMax;
    UINT    nPage;
    int     nPos;
    int     nTrackPos;
} SCROLLINFO

typedef struct {;
    RECT rgrc[3];
    WINDOWPOS lppos;
} NCCALCSIZE_PARAMS

typedef struct _HD_ITEM {;
    UINT    mask;
    int     cxy;
    LPSTR   pszText;
    HBITMAP hbm;
    int     cchTextMax;
    int     fmt;
    LPARAM  lParam;
    int     iImage;
    int     iOrder;
    UINT    type;
    LPVOID  pvFilter;
} HDITEM

typedef struct tagNMHEADERA {;
    NMHDR  hdr;
    int    iItem;
    int    iButton;
    HDITEM *pitem;
} NMHEADER

typedef struct {;
    DWORD style;
    DWORD dwExtendedStyle;
    SHORT x;
    SHORT y;
    SHORT cx;
    SHORT cy;
    WORD  id;
    WORD  extra;
    WORD  class;
    WORD  title;
    WORD  extradata;
} DLGITEMTEMPLATE

typedef struct {;
    DWORD style;
    DWORD dwExtendedStyle;
    WORD  cdit;
    SHORT x;
    SHORT y;
    SHORT cx;
    SHORT cy;
    WORD  menu;
    WORD  windowclass;
    WORD  title;
} DLGTEMPLATE

typedef struct {;
    WORD      dlgVer;
    WORD      signature;
    DWORD     helpID;
    DWORD     exStyle;
    DWORD     style;
    WORD      cDlgItems;
    SHORT     x;
    SHORT     y;
    SHORT     cx;
    SHORT     cy;
    char    menu;
    char    windowClass;
    WCHAR     title[256];
    WORD      pointsize;
    WORD      weight;
    BYTE      italic;
    BYTE      charset;
    WCHAR     typeface[32];
} DLGTEMPLATEEX

typedef struct tagTRACKMOUSEEVENT {;
    DWORD cbSize;
    DWORD dwFlags;
    HWND  hwndTrack;
    DWORD dwHoverTime;
} TRACKMOUSEEVENT, *LPTRACKMOUSEEVENT

typedef struct tagTBADDBITMAP {;
    HINSTANCE       hInst;
    UINT            nID[2];
} TBADDBITMAP, *LPTBADDBITMAP

typedef struct tagTCITEM {;
    UINT mask;
    DWORD dwState;
    DWORD dwStateMask;
    LPTSTR pszText;
    int cchTextMax;
    int iImage;
    LPARAM lParam;
} TCITEMA

typedef struct tagTVITEM {;
    UINT mask;
    HTREEITEM hItem;
    UINT state;
    UINT stateMask;
    LPTSTR pszText;
    int cchTextMax;
    int iImage;
    int iSelectedImage;
    int cChildren;
    LPARAM lParam;
} TVITEM, *LPTVITEM

typedef struct tagTVITEMEX {;
    UINT mask;
    HTREEITEM hItem;
    UINT state;
    UINT stateMask;
    LPTSTR pszText;
    int cchTextMax;
    int iImage;
    int iSelectedImage;
    int cChildren;
    LPARAM lParam;
    int iIntegral;
} TVITEMEX, *LPTVITEMEX

 typedef struct tagTVINSERTSTRUCT {;
    HTREEITEM hParent;
    HTREEITEM hInsertAfter;
    TVITEMEX item;
 } TVINSERTSTRUCT, *LPTVINSERTSTRUCT

typedef struct tagNMTREEVIEW {;
    NMHDR hdr;
    UINT action;
    TVITEM itemOld;
    TVITEM itemNew;
    POINT ptDrag;
} NMTREEVIEW

typedef struct tagNMTVCUSTOMDRAW {;
    NMCUSTOMDRAW nmcd;
    COLORREF     clrText;
    COLORREF     clrTextBk;
    int iLevel;
} NMTVCUSTOMDRAW, *LPNMTVCUSTOMDRAW

typedef struct tagTVHITTESTINFO {;
    POINT pt;
    UINT flags;
    HTREEITEM hItem;
} TVHITTESTINFO, *LPTVHITTESTINFO

typedef struct _LVITEM {;
    UINT   mask;
    int    iItem;
    int    iSubItem;
    UINT   state;
    UINT   stateMask;
    LPTSTR  pszText;
    int    cchTextMax;
    int    iImage;
    LPARAM lParam;
    int iIndent;
} LVITEMA, FAR *LPLVITEM

typedef struct { ;
    int mask;
    int fmt;
    int cx;
    LPTSTR pszText;
    int cchTextMax;
    LONG iSubItem;
    int iImage;
    int iOrder;
} LVCOLUMNA

typedef struct {;
    DWORD        lStructSize;
    HWND         hwndOwner;
    HDC          hDC;
    LOGFONT      *lpLogFont;
    INT          iPointSize;
    DWORD        Flags;
    COLORREF     rgbColors;
    LPARAM       lCustData;
    LPCFHOOKPROC lpfnHook;
    LPCTSTR      lpTemplateName;
    HINSTANCE    hInstance;
    LPTSTR       lpszStyle;
    WORD         nFontType;
    INT          nSizeMin;
    INT          nSizeMax;
} CHOOSEFONT, *LPCHOOSEFONT

typedef struct {;
    NMHDR hdr;
    DWORD dwFlag;
    int iWidth;
    int iHeight;
} NMPGCALCSIZE

typedef struct {;
    NMHDR hdr;
    WORD  fwKeys;
    RECT  rcParent;
    WORD  iDir;
    int   iXpos;
    WORD  iYpos;
    WORD  iScroll;
} NMPGSCROLL, *LPNMPGSCROLL

typedef struct tagOFN {;
    DWORD         lStructSize;
    HWND          hwndOwner;
    HINSTANCE     hInstance;
    LPCTSTR       lpstrFilter;
    LPTSTR        lpstrCustomFilter;
    DWORD         nMaxCustFilter;
    DWORD         nFilterIndex;
    LPCSTR        lpstrFile;
    DWORD         nMaxFile;
    LPTSTR        lpstrFileTitle;
    DWORD         nMaxFileTitle;
    LPCTSTR       lpstrInitialDir;
    LPCTSTR       lpstrTitle;
    DWORD         Flags;
    WORD          nFileOffset;
    WORD          nFileExtension;
    LPCTSTR       lpstrDefExt;
    DWORD         lCustData;
    LPOFNHOOKPROC lpfnHook;
    LPCTSTR       lpTemplateName;
    LPVOID        pvReserved ;
    DWORD         dwReserved ;
    DWORD         FlagsEx ;
} OPENFILENAME

typedef struct tagTVKEYDOWN {;
    NMHDR hdr;
    WORD wVKey;
    UINT flags;
} NMTVKEYDOWN, FAR *LPNMTVKEYDOWN

typedef struct _WINDOWPLACEMENT {;
    UINT length;
    UINT flags;
    UINT showCmd;
    POINT ptMinPosition;
    POINT ptMaxPosition;
    RECT rcNormalPosition;
} WINDOWPLACEMENT

typedef struct tagNMTVGETINFOTIP {;
    NMHDR hdr;
    LPTSTR pszText;
    int cchTextMax;
    HTREEITEM hItem;
    LPARAM lParam;
} NMTVGETINFOTIP

typedef struct tagLITEM {;
    UINT mask;
    int iLink;
    UINT state;
    UINT stateMask;
    TCHAR szID[48];
    TCHAR szUrl[2083];
} LITEM, *PLITEM

typedef struct _RB_HITTESTINFO {;
    POINT pt;
    UINT flags;
    int iBand;
} RBHITTESTINFO, *LPRBHITTESTINFO;

typedef struct tagPOINTS { ;
    SHORT x;
    SHORT y;
} POINTS, *PPOINTS

typedef struct _SECURITY_ATTRIBUTES {;
    DWORD nLength;
    LPVOID lpSecurityDescriptor;
    BOOL bInheritHandle;
} SECURITY_ATTRIBUTES

typedef struct _SYSTEMTIME {;
   WORD wYear;
   WORD wMonth;
   WORD wDayOfWeek;
   WORD wDay;
   WORD wHour;
   WORD wMinute;
   WORD wSecond;
   WORD wMilliseconds;
} SYSTEMTIME

typedef struct tagNMDATETIMECHANGE {;
   NMHDR nmhdr;
   DWORD dwFlags;
   SYSTEMTIME st;
} NMDATETIMECHANGE

typedef struct {;
    DWORD style;
    DWORD dwExtendedStyle;
    WORD  cdit;
    SHORT x;
    SHORT y;
    SHORT cx;
    SHORT cy;
    WORD  menu;
    WORD  windowclass;
    WORD  title;
} DLGTEMPLATEX

typedef struct {;
    DWORD cbSize;
    RECT  rcWindow;
    RECT  rcClient;
    DWORD dwStyle;
    DWORD dwExStyle;
    DWORD dwWindowStatus;
    UINT  cxWindowBorders;
    UINT  cyWindowBorders;
    WORD  atomWindowType;
    WORD  wCreatorVersion;
} WINDOWINFO

typedef struct {;
    POINT ptReserved;
    POINT ptMaxSize;
    POINT ptMaxPosition;
    POINT ptMinTrackSize;
    POINT ptMaxTrackSize;
} MINMAXINFO

typedef struct {;
    UINT mask;
    int iItem;
    LPTSTR pszText;
    int cchTextMax;
    int iImage;
    int iSelectedImage;
    int iOverlay;
    int iIndent;
    LPARAM lParam;
} COMBOBOXEXITEM

typedef struct tagDEVNAMES {;
    WORD wDriverOffset;
    WORD wDeviceOffset;
    WORD wOutputOffset;
    WORD wDefault;
} DEVNAMES

typedef struct tagPD {;
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
    LPCTSTR lpPrintTemplateName;
    LPCTSTR lpSetupTemplateName;
    HGLOBAL hPrintTemplate;
    HGLOBAL hSetupTemplate;
} PRINTDLG

typedef struct tagPDEX {;
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
    HANDLE lphPropertyPages;
    DWORD nStartPage;
    DWORD dwResultAction;
} PRINTDLGEX

typedef struct tagEDITBALLOONTIP {;
    DWORD   cbStruct;
    LPTSTR  pszTitle;
    LPTSTR  pszText;
    int     ttiIcon;
} EDITBALLOONTIP, *PEDITBALLOONTIP


typedef struct tagNMLVCUSTOMDRAW {;
    NMCUSTOMDRAW nmcd;
    COLORREF clrText;
    COLORREF clrTextBk;
    int iSubItem;
    DWORD dwItemType;
    COLORREF clrFace;
    int iIconEffect;
    int iIconPhase;
    int iPartId;
    int iStateId;
    RECT rcText;
    UINT uAlign;
} NMLVCUSTOMDRAW

typedef struct _HD_HITTESTINFO {;
    POINT pt;
    UINT flags;
    int iItem;
} HDHITTESTINFO, *LPHDHITTESTINFO;

typedef struct {;
    HBITMAP hbmImage;
    HBITMAP hbmMask;
    int Unused1;
    int Unused2;
    RECT rcImage;
} IMAGEINFO

typedef struct tagNMLINK {;
    NMHDR hdr;
    LITEM item;
} NMLINK

typedef struct tagLVGROUP {;
    UINT    cbSize;
    UINT    mask;
    LPTSTR  pszHeader;
    int     cchHeader;
    LPTSTR  pszFooter;
    int     cchFooter;
    int     iGroupId;
    UINT    stateMask;
    UINT    state;
    UINT    uAlign;
    LPTSTR  pszSubtitle;
    UINT    cchSubtitle;
    LPTSTR  pszTask;
    UINT    cchTask;
    LPTSTR  pszDescriptionTop;
    UINT    cchDescriptionTop;
    LPTSTR  pszDescriptionBottom;
    UINT    cchDescriptionBottom;
    int     iTitleImage;
    int     iExtendedImage;
    int     iFirstItem;         // Read only
    UINT    cItems;             // Read only
    LPTSTR  pszSubsetTitle;     // NULL if group is not subset
    UINT    cchSubsetTitle;
} LVGROUP, *PLVGROUP;

typedef struct _LVWITEM {;
    UINT mask;
    int iItem;
    int iSubItem;
    UINT state;
    UINT stateMask;
    LPTSTR pszText;
    int cchTextMax;
    int iImage;
    LPARAM lParam;
    int iIndent;
    int iGroupId;
    UINT cColumns; // tile view columns
    UINT puColumns;
} LVWITEM

typedef struct tagLVDISPINFO {;
    NMHDR hdr;
    LVWITEM item;
} NMLVDISPINFO

typedef struct tagNMLVCACHEHINT {;
    NMHDR   hdr;
    int     iFrom;
    int     iTo;
} NMLVCACHEHINT

typedef struct LVGROUPMETRICS {;
    UINT cbSize;
    UINT mask;
    UINT Left;
    UINT Top;
    UINT Right;
    UINT Bottom;
    COLORREF crLeft;
    COLORREF crTop;
    COLORREF crRight;
    COLORREF crBottom;
    COLORREF crHeader;
    COLORREF crFooter;
} LVGROUPMETRICS

typedef struct tagNMDAYSTATE {;
    NMHDR nmhdr;
    SYSTEMTIME stStart;
    int cDayState;
    DWORD prgDayState;
} NMDAYSTATE

typedef struct tagNMSELCHANGE {;
    NMHDR nmhdr;
    SYSTEMTIME stSelStart;
    SYSTEMTIME stSelEnd;
} NMSELCHANGE

typedef struct _NOTIFYICONDATA {;
    DWORD cbSize;
    HWND  hWnd;
    UINT  uID;
    UINT  uFlags;
    UINT  uCallbackMessage;
    HICON hIcon;
    char  szTip[128];
    DWORD dwState;
    DWORD dwStateMask;
    char  szInfo[256];
    UINT  uTimeout;
    char  szInfoTitle[64];
    DWORD dwInfoFlags;
} NOTIFYICONDATA

typedef struct _charrange {;
   LONG  cpMin;
   LONG  cpMax;
} CHARRANGE;

typedef struct _findtext {;
   CHARRANGE chrg;
   LPCSTR lpstrText;
} FINDTEXT;

typedef struct _gettextlengthex {;
   DWORD flags;
   UINT  codepage;
} GETTEXTLENGTHEX;

typedef struct _NM_UPDOWN {;
    NMHDR hdr;
    int iPos;
    int iDelta;
} NMUPDOWN, *LPNMUPDOWN;

typedef struct tagTCHITTESTINFO {;
    POINT pt;
    UINT flags;
} TCHITTESTINFO

typedef struct tagNMTCKEYDOWN {;
    NMHDR hdr;
    WORD wVKey;
    UINT flags;
} NMTCKEYDOWN

// task pane
typedef struct tagTASKPANE {;
   COLORREF            crBackground1;
   COLORREF            crBackground2;
   int                 iValue;
   RECT                rcPadding;
   RECT                rcBorderThickness;
   COLORREF            crBorder;
} TASKPANE

// pane task links (normal)
typedef struct tagTASKNORMAL {;
   RECT                rcTitlePadding;             // generic link padding (inner)
   RECT                rcMarginsActionTask;        // margins: action task (outer)
   RECT                rcMarginsDestinationTask;   // margins: destination task (outer)
   COLORREF            crLinkNormal;               // normal link color
   COLORREF            crLinkHot;                  // hot link color (mousefocused)
} TASKNORMAL

// pane task links (special)
typedef struct tagTASKSPECIAL {;
   RECT                rcTitlePadding;             // generic link padding (inner)
   RECT                rcMarginsActionTask;        // margins: action task (outer)
   RECT                rcMarginsDestinationTask;   // margins: destination task (outer)
   COLORREF            crLinkNormal;               // normal link color
   COLORREF            crLinkHot;                  // hot link color (mousefocused)
} TASKSPECIAL

typedef struct tagHEADERNORMAL {;
   char                szFontFace[MAX_PATH];       // font face for header and content
   int                 iFontSize;                  // font size for header and content
   int                 iMargin;                    // margin (pane to bar)
   HBITMAP             hAlphaBmpListHeader;        // header bitmap, if NULL: use ThemeRenderer
   int                 iHeaderBmpWidth;            // 0 or bitmap width
   int                 iHeaderBmpHeight;           // 0 or bitmap height
   COLORREF            crHeaderNormal;             // normal color (header only)
   COLORREF            crHeaderHot;                // hot color (header only)
   UINT                uAlignment;                 // 0 or text alignment (header only)
   int                 iFontWeightHeader;          // font weight (header only)
   RECT                rcPaddingHeader;            // outer
   RECT                rcBorderThicknessHeader;    // inner
   COLORREF            crBorderColorHeader;        // unused, PRE_XP: background color of header
   int                 iBmpArrowWidth;
   int                 iBmpArrowHeight;
   HBITMAP             hAlphaBmpArrowUp[3];        // normal, hot, pressed ([0]==NULL: use ThemeRenderer) [2]==NULL: no pressed bitmap, use hot y+1
   HBITMAP             hAlphaBmpArrowDn[3];        // normal, hot, pressed ([0]==NULL: use ThemeRenderer) [2]==NULL: no pressed bitmap, use hot y+1
   COLORREF            crTLbackground;             // background color (task list)
   RECT                rcTLBorderThickness;        // border thickness
   COLORREF            crTLBorder;                 // border color
   RECT                rcTLPadding;                // padding (task pane to items)
} HEADERNORMAL

  // special header

typedef struct tagHEADERSPECIAL {;
    char                szFontFace[MAX_PATH];       // font face for header and content
    int                 iFontSize;                  // font size for header and content
    int                 iMargin;                    // margin (pane to bar)
    HBITMAP             hAlphaBmpListHeader;        // header bitmap, if NULL: use ThemeRenderer
    int                 iHeaderBmpWidth;            // 0 or bitmap width
    int                 iHeaderBmpHeight;           // 0 or bitmap height
    COLORREF            crHeaderNormal;             // normal color (header only)
    COLORREF            crHeaderHot;                // hot color (header only)
    UINT                uAlignment;                 // 0 or text alignment (header only)
    int                 iFontWeightHeader;          // font weight (header only)
    RECT                rcPaddingHeader;            // outer
    RECT                rcBorderThicknessHeader;    // inner
    COLORREF            crBorderColorHeader;        // unused, PRE_XP: background color of header
    int                 iBmpArrowWidth;
    int                 iBmpArrowHeight;
    HBITMAP             hAlphaBmpArrowUp[3];        // normal, hot, pressed ([0]==NULL: use ThemeRenderer) [2]==NULL: no pressed bitmap, use hot y+1
    HBITMAP             hAlphaBmpArrowDn[3];        // normal, hot, pressed ([0]==NULL: use ThemeRenderer) [2]==NULL: no pressed bitmap, use hot y+1
    COLORREF            crTLbackground;             // background color (task list, watermark)
    UINT                uTLAlign;                   // alignment (watermark)
    RECT                rcTLBorderThickness;        // border thickness
    COLORREF            crTLBorder;                 // border color
    RECT                rcTLPadding;                // padding (task pane to items)
} HEADERSPECIAL

typedef struct tagEXPBARINFO {;
   int                   cbSize;
   BOOL                  bPreXP;             // true if pre XP style
   BOOL                  bUseThemeRenderer;  // true if theme renderer must be used
   BOOL                  bShellStyleDll;     // true if shellstyle Dll present and used
   TCHAR                 szMsStylesDllName[MAX_PATH];
   TCHAR                 szColorname[MAX_PATH];
   TCHAR                 szShellStyleDllName[MAX_PATH];
   int                   iBmpPreXPArrowWidth;
   int                   iBmpPreXPArrowHeight;
   HBITMAP               hAlphaPreXPBmpArrowUp[3];
   HBITMAP               hAlphaPreXPBmpArrowDn[3];
   TASKPANE              taskpane;
   TASKNORMAL            tasknormal;
   TASKSPECIAL           taskspecial;
   HEADERNORMAL          headernormal;
   HEADERSPECIAL         headerspecial;
} EXPBARINFO

typedef struct {;
    ULONG ulReserved;
    ULONG flFlags;
    ULONG nPosition;
    LPSTR lpszPathName;
    LPSTR lpszFileName;
    LPVOID lpFileType;
} MapiFileDesc, *lpMapiFileDesc;

typedef struct {;
    ULONG ulReserved;
    ULONG cbTag;
    BYTE lpTag;
    ULONG cbEncoding;
    BYTE lpEncoding;
} MapiFileTagExt, *lpMapiFileTagExt;

typedef struct {;
    ULONG ulReserved;
    ULONG ulRecipClass;
    LPSTR lpszName;
    LPSTR lpszAddress;
    ULONG ulEIDSize;
    LPVOID lpEntryID;
} MapiRecipDesc, *lpMapiRecipDesc;

typedef struct {;
    ULONG ulReserved;
    LPSTR lpszSubject;
    LPSTR lpszNoteText;
    LPSTR lpszMessageType;
    LPSTR lpszDateReceived;
    LPSTR lpszConversationID;
    ULONG flFlags;
    lpMapiRecipDesc lpOriginator;
    ULONG nRecipCount;
    lpMapiRecipDesc lpRecips;
    ULONG nFileCount;
    lpMapiFileDesc lpFiles;
} MapiMessage, *lpMapiMessage;

typedef struct _SERVICE_STATUS {;
    DWORD dwServiceType;
    DWORD dwCurrentState;
    DWORD dwControlsAccepted;
    DWORD dwWin32ExitCode;
    DWORD dwServiceSpecificExitCode;
    DWORD dwCheckPoint;
    DWORD dwWaitHint;
} SERVICE_STATUS

typedef struct _SHFILEINFO {;
   HICON hIcon;
   int   iIcon;
   DWORD dwAttributes;
   TCHAR szDisplayName[MAX_PATH];
   TCHAR szTypeName[80];
} SHFILEINFO

typedef struct _COMMTIMEOUTS {;
   DWORD ReadIntervalTimeout;
   DWORD ReadTotalTimeoutMultiplier;
   DWORD ReadTotalTimeoutConstant;
   DWORD WriteTotalTimeoutMultiplier;
   DWORD WriteTotalTimeoutConstant;
} COMMTIMEOUTS

typedef struct _OVERLAPPED {;
   DWORD Internal;
   DWORD InternalHigh;
   DWORD Offset;
   DWORD OffsetHigh;
   HANDLE hEvent;
} OVERLAPPED;

typedef struct tagMONITORINFOEX {;
   DWORD  cbSize; 
   RECT   rcMonitor; 
   RECT   rcWork; 
   DWORD  dwFlags; 
   TCHAR  szDevice[CCHDEVICENAME];
} MONITORINFOEX, *LPMONITORINFOEX

typedef struct {;
   LPARAM lParam;
   WPARAM wParam;
   UINT message;
   HWND hwnd;
} CWPSTRUCT, *PCWPSTRUCT

typedef struct _TRIVERTEX {;
   LONG        x;
   LONG        y;
   USHORT      Red;
   USHORT      Green;
   USHORT      Blue;
   USHORT      Alpha;
} TRIVERTEX

typedef struct _GRADIENT_RECT {;
   ULONG    UpperLeft;
   ULONG    LowerRight;
} GRADIENT_RECT

typedef struct _GRADIENT_TRIANGLE {;
   ULONG    Vertex1;
   ULONG    Vertex2;
   ULONG    Vertex3;
} GRADIENT_TRIANGLE

typedef struct _FILETIME {;
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
} FILETIME

typedef struct _WIN32_FIND_DATA {;
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    TCHAR cFileName[MAX_PATH];
    TCHAR cAlternateFileName[14];
} WIN32_FIND_DATA

typedef struct tagNMLISTVIEW {;
    NMHDR hdr;
    int iItem;
    int iSubItem;
    UINT uNewState;
    UINT uOldState;
    UINT uChanged;
    POINT ptAction;
    LPARAM lParam;
} NMLISTVIEW

typedef struct tagLVKEYDOWN {;
    NMHDR hdr;
    WORD wVKey;
    UINT flags;
} NMLVKEYDOWN

typedef struct tagLVFINDINFO {;
    UINT flags;
    LPCSTR psz;
    LPARAM lParam;
    POINT pt;
    UINT vkDirection;
} LVFINDINFO

typedef struct _NMLVFINDITEM {;
    NMHDR hdr;
    int iStart;
    LVFINDINFO lvfi;
} NMLVFINDITEM

typedef struct _charformat {;
    UINT cbSize;
    DWORD dwMask;
    DWORD dwEffects;
    LONG yHeight;
    LONG yOffset;
    COLORREF crTextColor;
    BYTE bCharSet;
    BYTE bPitchAndFamily;
    TCHAR szFaceName[LF_FACESIZE];
} CHARFORMAT

typedef struct _editstream {;
    DWORD dwCookie;
    DWORD dwError;
    DWORD pfnCallback;
} EDITSTREAM

typedef struct tagHyphenateInfo {;
    SHORT cbSize;
    SHORT dxHyphenateZone;
    DWORD pfnHyphenate;
} HYPHENATEINFO

typedef struct _MEMORYSTATUS {;
    DWORD dwLength;
    DWORD dwMemoryLoad;
    DWORD dwTotalPhys;
    DWORD dwAvailPhys;
    DWORD dwTotalPageFile;
    DWORD dwAvailPageFile;
    DWORD dwTotalVirtual;
    DWORD dwAvailVirtual;
} MEMORYSTATUS, *LPMEMORYSTATUS

typedef struct _MEMORYSTATUSEX {;
    DWORD dwLength;
    DWORD dwMemoryLoad;
    ULONG ullTotalPhys;
    ULONG ullAvailPhys;
    ULONG ullTotalPageFile;
    ULONG ullAvailPageFile;
    ULONG ullTotalVirtual;
    ULONG ullAvailVirtual;
    ULONG ullAvailExtendedVirtual;
} MEMORYSTATUSEX, *LPMEMORYSTATUSEX;

typedef struct _PERFORMANCE_INFORMATION {;
    DWORD cb;
    SIZE_T CommitTotal;
    SIZE_T CommitLimit;
    SIZE_T CommitPeak;
    SIZE_T PhysicalTotal;
    SIZE_T PhysicalAvailable;
    SIZE_T SystemCache;
    SIZE_T KernelTotal;
    SIZE_T KernelPaged;
    SIZE_T KernelNonpaged;
    SIZE_T PageSize;
    DWORD HandleCount;
    DWORD ProcessCount;
    DWORD ThreadCount;
} PERFORMANCE_INFORMATION

typedef struct _BY_HANDLE_FILE_INFORMATION {;
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;  
    FILETIME ftLastWriteTime;  
    DWORD dwVolumeSerialNumber;  
    DWORD nFileSizeHigh;  
    DWORD nFileSizeLow;  
    DWORD nNumberOfLinks;  
    DWORD nFileIndexHigh;  
    DWORD nFileIndexLow;
} BY_HANDLE_FILE_INFORMATION,  *PBY_HANDLE_FILE_INFORMATION;

typedef struct _OFSTRUCT {;
    BYTE cBytes;
    BYTE fFixedDisk;  
    WORD nErrCode;  
    WORD Reserved1;  
    WORD Reserved2;  
    TCHAR szPathName[OFS_MAXPATHNAME];
} OFSTRUCT

typedef struct _DCB {;
   DWORD DCBlength;
   DWORD BaudRate;
   DWORD data;
   WORD  wReserved;
   WORD  XonLim;
   WORD  XoffLim;
   BYTE  ByteSize;
   BYTE  Parity;
   BYTE  StopBits;
   BYTE  XonChar;
   BYTE  XoffChar;
   BYTE  ErrorChar;
   BYTE  EofChar;
   BYTE  EvtChar;
   WORD  wReserved1;
} DCB

typedef struct _COMSTAT {;
   DWORD data;
   DWORD cbInQue;
   DWORD cbOutQue;
} COMSTAT

typedef struct _COMMPROP {;
   WORD wPacketLength;
   WORD wPacketVersion;
   DWORD dwServiceMask;
   DWORD dwReserved1;
   DWORD dwMaxTxQueue;
   DWORD dwMaxRxQueue;
   DWORD dwMaxBaud;
   DWORD dwProvSubType;
   DWORD dwProvCapabilities;
   DWORD dwSettableParams;
   DWORD dwSettableBaud;
   WORD wSettableData;
   WORD wSettableStopParity;
   DWORD dwCurrentTxQueue;
   DWORD dwCurrentRxQueue;
   DWORD dwProvSpec1;
   DWORD dwProvSpec2;
   WCHAR wcProvChar[1];
} COMMPROP, *LPCOMMPROP;

typedef struct tagHELPINFO {;
    UINT cbSize;
    int iContextType;
    int iCtrlId;
    HANDLE hItemHandle;
    DWORD dwContextId;
    POINT MousePos;
} HELPINFO, *LPHELPINFO;

typedef struct tagBITMAP {;
    LONG bmType;
    LONG bmWidth;
    LONG bmHeight;
    LONG bmWidthBytes;
    WORD bmPlanes;
    WORD bmBitsPixel;
    LPVOID bmBits;
} BITMAP

typedef struct {;
    POINT pt;
    HWND hwnd;
    UINT wHitTestCode;
    ULONG dwExtraInfo;
} MOUSEHOOKSTRUCT

typedef struct tagNMTTDISPINFO {;
    NMHDR hdr;
    LPSTR lpszText;
    char szText[254];
    HINSTANCE hinst;
    UINT uFlags;
    LPARAM lParam;
} NMTTDISPINFO

typedef struct _DWM_BLURBEHIND {;
    DWORD dwFlags;
    BOOL fEnable;
    HANDLE hRgnBlur;
    BOOL fTransitionOnMaximized;
} DWM_BLURBEHIND

typedef struct _MARGINS {;
    int cxLeftWidth;
    int cxRightWidth;
    int cyTopHeight;
    int cyBottomHeight;
} MARGINS, *PMARGINS;

typedef struct tagNMITEMACTIVATE {;
    NMHDR hdr;
    int iItem;
    int iSubItem;
    UINT uNewState;
    UINT uOldState;
    UINT uChanged;
    POINT ptAction;
    LPARAM lParam;
    UINT uKeyFlags;
} NMITEMACTIVATE

typedef struct _SHFILEOPSTRUCTA {;
    HWND hwnd;
    UINT wFunc;
    LPCSTR pFrom;
    LPCSTR pTo;
    WORD fFlags;
    BOOL fAnyOperationsAborted;
    LPVOID hNameMappings;
    LPCSTR lpszProgressTitle;
} SHFILEOPSTRUCT

typedef struct _QUERY_SERVICE_CONFIG {;
    DWORD dwServiceType;
    DWORD dwStartType;
    DWORD dwErrorControl;
    LPSTR lpBinaryPathName;
    LPSTR lpLoadOrderGroup;
    DWORD dwTagId;
    LPSTR lpDependencies;
    LPSTR lpServiceStartName;
    LPSTR lpDisplayName;
} QUERY_SERVICE_CONFIG

typedef struct _SERVICE_DESCRIPTION {;
    LPTSTR lpDescription;
} SERVICE_DESCRIPTION

typedef struct _SHELLEXECUTEINFO {;
    DWORD cbSize;
    ULONG fMask;
    HWND hwnd;
    LPCSTR lpVerb;
    LPCSTR lpFile;
    LPCSTR lpParameters;
    LPCSTR lpDirectory;
    int nShow;
    HINSTANCE hInstApp;
    LPVOID lpIDList;
    LPCSTR lpClass;
    HKEY hkeyClass;
    DWORD dwHotKey;
    HANDLE hIcon;
    HANDLE hProcess;
} SHELLEXECUTEINFO

typedef struct tagRGBQUAD {;
    BYTE rgbRed;
    BYTE rgbGreen;
    BYTE rgbBlue;
    BYTE rgbReserved;
} RGBQUAD;

typedef struct tagBITMAPINFOHEADER{;
    DWORD biSize;
    LONG biWidth;
    LONG biHeight;
    WORD biPlanes;
    WORD biBitCount;
    DWORD biCompression;
    DWORD biSizeImage;
    LONG biXPelsPerMeter;
    LONG biYPelsPerMeter;
    DWORD biClrUsed;
    DWORD biClrImportant;
} BITMAPINFOHEADER

typedef struct tagBITMAPINFO {;
    BITMAPINFOHEADER bmiHeader;
    RGBQUAD bmiColors;
} BITMAPINFO

typedef struct _IMAGELISTDRAWPARAMS {;
    DWORD cbSize;
    HIMAGELIST himl;
    int i;
    HDC hdcDst;
    int x;
    int y;
    int cx;
    int cy;
    int xBitmap;
    int yBitmap;
    COLORREF rgbBk;
    COLORREF rgbFg;
    UINT fStyle;
    DWORD dwRop;
    DWORD fState;
    DWORD Frame;
    DWORD crEffect;
} IMAGELISTDRAWPARAMS;

typedef struct _DTTOPTS {;
    DWORD dwSize;
    DWORD dwFlags;
    COLORREF crText;
    COLORREF crBorder;
    COLORREF crShadow;
    int iTextShadowType;
    POINT ptShadowOffset;
    int iBorderSize;
    int iFontPropId;
    int iColorPropId;
    int iStateId;
    BOOL fApplyOverlay;
    int iGlowSize;
    DWORD pfnDrawTextCallback;
    LPARAM lParam;
} DTTOPTS, *PDTTOPTS;

typedef struct tagNMTVDISPINFO {;
    NMHDR hdr;
    TVITEM item;
} NMTVDISPINFO, *LPNMTVDISPINFO;

typedef struct tagPSD {;
   DWORD           lStructSize;
   HWND            hwndOwner;
   HGLOBAL         hDevMode;
   HGLOBAL         hDevNames;
   DWORD           Flags;
   POINT           ptPaperSize;
   RECT            rtMinMargin;
   RECT            rtMargin;
   HINSTANCE       hInstance;
   LPARAM          lCustData;
   LPPAGESETUPHOOK lpfnPageSetupHook;
   LPPAGEPAINTHOOK lpfnPagePaintHook;
   LPCTSTR         lpPageSetupTemplateName;
   HGLOBAL         hPageSetupTemplate;
} PAGESETUPDLG, *LPPAGESETUPDLG;

typedef struct tagDEVMODE {;
    WORD wDriverOffset;
    WORD wDeviceOffset;
    WORD wOutputOffset;
    WORD wDefault;
} DEVMODE

typedef struct tagCOMBOBOXINFO {;
    DWORD cbSize;
    RECT  rcItem;
    RECT  rcButton;
    DWORD stateButton;
    HWND  hwndCombo;
    HWND  hwndItem;
    HWND  hwndList;
} COMBOBOXINFO

typedef struct LVTILEINFO {;
    UINT  cbSize;
    int   iItem;
    UINT  cColumns;
    UINT  puColumns;
    int   piColFmt;
} LVTILEINFO, *PLVTILEINFO;

typedef struct LVTILEVIEWINFO {;
    UINT  cbSize;
    DWORD dwMask;
    DWORD dwFlags;
    SIZE  sizeTile;
    int   cLines;
    RECT  rcLabelMargin;
} LVTILEVIEWINFO, *PLVTILEVIEWINFO;

typedef struct {;
  UINT           cbSize;
  HWND           hwndOwner;
  HINSTANCE      hInstance;
  LPCTSTR        lpszText;
  LPCTSTR        lpszCaption;
  DWORD          dwStyle;
  LPCTSTR        lpszIcon;
  DWORD_PTR      dwContextHelpId;
  MSGBOXCALLBACK lpfnMsgBoxCallback;
  DWORD          dwLanguageId;
} MSGBOXPARAMS, *PMSGBOXPARAMS;
