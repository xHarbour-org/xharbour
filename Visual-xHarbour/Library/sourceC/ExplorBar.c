/*
 * $Id$
 */
#include <windows.h>
//#include <commdlg.h>
#include <commctrl.h>
//#include <shlobj.h>
#include <tchar.h>


//#include "item.api"
//#include "hbdefs.h"
//#include "hbvmpub.h"
//#include "hbinit.h"
#include "hbapi.h"
//#include "hbfast.h"
//#include "hbvm.h"
//#include "hbapierr.h"

#define TOK_UNKNOWN         0x00000000L
#define TOK_STRING          0x00000001L
#define TOK_LT              0x00000002L
#define TOK_GT              0x00000003L
#define TOK_EQ              0x00000004L
#define TOK_LP              0x00000005L
#define TOK_RP              0x00000006L
#define TOK_LB              0x00000007L
#define TOK_RB              0x00000008L
#define TOK_LC              0x00000009L
#define TOK_RC              0x0000000AL
#define TOK_COLON           0x0000000BL
#define TOK_SEMICOLON       0x0000000CL
#define TOK_COMMA           0x0000000DL
#define TOK_OR              0x0000000EL
#define TOK_MINUS           0x0000000FL
#define TOK_PLUS            0x00000010L
#define TOK_HTMLCOLOR       0x00000011L
#define TOK_INTEGER         0x00000012L
#define TOK_SLASH           0x00000013L

#define TD_ERROR            0xFFFFFFFFL
#define TD_UNKNOWN          0x00000000L
#define TD_INTEGER          0x00000001L
#define TD_INTEGER_PT       0x00000002L
#define TD_INTEGER_RP       0x00000003L
#define TD_RGB              0x00000004L
#define TD_ARGB             0x00000005L
#define TD_ALIGNMENT        0x00000006L
#define TD_CURSOR           0x00000007L
#define TD_FONTSTYLE        0x00000008L
#define TD_STRING           0x00000009L
#define TD_GRADIENT         0x0000000AL
#define TD_RECT             0x0000000BL
#define TD_BITMAP           0x0000000CL
#define TD_SYSMETRIC        0x0000000DL
#define TD_SYSMETRICSTR     0x0000000EL

#define TDA_LEFT            0x00000001L
#define TDA_CENTER          0x00000002L
#define TDA_RIGHT           0x00000004L
#define TDA_WRAP            0x00000008L
#define TDA_TOP             0x00000010L
#define TDA_MIDDLE          0x00000020L
#define TDA_BOTTOM          0x00000040L
#define TDA_FOCUSRECT       0x00000080L
#define TDA_LEFTTOP         (TDA_LEFT|TDA_TOP)
#define TDA_LEFTMIDDLE      (TDA_LEFT|TDA_MIDDLE)
#define TDA_LEFTBOTTOM      (TDA_LEFT|TDA_BOTTOM)
#define TDA_CENTERTOP       (TDA_CENTER|TDA_TOP)
#define TDA_CENTERMIDDLE    (TDA_CENTER|TDA_MIDDLE)
#define TDA_CENTERBOTTOM    (TDA_CENTER|TDA_BOTTOM)
#define TDA_RIGHTTOP        (TDA_RIGHT|TDA_TOP)
#define TDA_RIGHTMIDDLE     (TDA_RIGHT|TDA_MIDDLE)
#define TDA_RIGHTBOTTOM     (TDA_RIGHT|TDA_BOTTOM)
#define TDA_TOPLEFT         (TDA_TOP|TDA_LEFT)
#define TDA_TOPCENTER       (TDA_TOP|TDA_CENTER)
#define TDA_TOPRIGHT        (TDA_TOP|TDA_RIGHT)
#define TDA_MIDDLELEFT      (TDA_MIDDLE|TDA_LEFT)
#define TDA_MIDDLECENTER    (TDA_MIDDLE|TDA_CENTER)
#define TDA_MIDDLERIGHT     (TDA_MIDDLE|TDA_RIGHT)
#define TDA_BOTTOMLEFT      (TDA_BOTTOM|TDA_LEFT)
#define TDA_BOTTOMCENTER    (TDA_BOTTOM|TDA_CENTER)
#define TDA_BOTTOMRIGHT     (TDA_BOTTOM|TDA_RIGHT)
#define TDA_WRAPLEFT        (TDA_WRAP|TDA_LEFT)
#define TDA_WRAPCENTER      (TDA_WRAP|TDA_CENTER)
#define TDA_WRAPRIGHT       (TDA_WRAP|TDA_RIGHT)

#define TDFS_NORMAL         0x00000000L
#define TDFS_UNDERLINE      0x00000001L
#define TDFS_ITALIC         0x00000002L
#define TDFS_BOLD           0x00000004L

#define __strcpy            _tcscpy
#define __strcat            _tcscat
#define __strupr            _tcsupr
#define __strlwr            _strlwr
#define __strlen            _tcslen
#define __strstr            _tcsstr
#define __strchr            _tcschr
#define __strrchr           _tcsrchr
#define __sprintf           _stprintf
#define __strncmp           _tcsncmp
#define __strcmp            _tcscmp

#define stricmp           _stricmp

#define MANIFEST_RESOURCE_ID            1

#define WINDOWS_VERSION_MASK                            ((UINT)0x000000FFL)

#define UNKNOWN_WINDOWS                                 ((UINT)0x00000000L)
#define WINDOWS_WIN32s                                  ((UINT)0x00000001L)
#define WINDOWS_95                                      ((UINT)0x00000002L)
#define WINDOWS_95_OSR2                                 ((UINT)0x00000003L)
#define WINDOWS_98                                      ((UINT)0x00000004L)
#define WINDOWS_98_SE                                   ((UINT)0x00000005L)
#define WINDOWS_ME                                      ((UINT)0x00000006L)
#define WINDOWS_NT                                      ((UINT)0x00000007L)
#define WINDOWS_2000                                    ((UINT)0x00000008L)
#define WINDOWS_XP                                      ((UINT)0x00000009L)
#define WINDOWS_DOTNET_SERVER_2003                      ((UINT)0x0000000AL)
#define POST_WINDOWS_XP                                 ((UINT)0x0000000BL)

#define WINDOWS_WORKSTATION                             ((UINT)0x00000100L)
#define WINDOWS_NT_WORKSTATION_4                        ((UINT)0x00000200L)
#define WINDOWS_HOME_EDITION                            ((UINT)0x00000400L)
#define WINDOWS_PROFESSIONAL_EDITION                    ((UINT)0x00000800L)
#define WINSERVER_SERVER4                               ((UINT)0x00001000L)
#define WINSERVER_SERVER4_ENTERPRISE_EDITION            ((UINT)0x00002000L)
#define WINSERVER_SERVER                                ((UINT)0x00004000L)
#define WINSERVER_ADVANCED_SERVER                       ((UINT)0x00008000L)
#define WINSERVER_STANDARD_EDITION                      ((UINT)0x00010000L)
#define WINSERVER_WEB_EDITION                           ((UINT)0x00020000L)
#define WINSERVER_ENTERPRISE_EDITION                    ((UINT)0x00040000L)
#define WINSERVER_DATACENTER_EDITION                    ((UINT)0x00080000L)

static HMODULE   hUxTheme   = NULL;
static HINSTANCE g_hInstDLL = NULL;

#define IDB_ARRUP_NORMAL                107
#define IDB_ARRUP_HOT                   108
#define IDB_ARRUP_PRESSED               109
#define IDB_ARRDN_NORMAL                110
#define IDB_ARRDN_HOT                   111
#define IDB_ARRDN_PRESSED               112
#define IDB_ARRDN2_NORMAL               113
#define IDB_ARRDN2_HOT                  114
#define IDB_ARRDN2_PRESSED              115
#define IDB_ARRUP2_NORMAL               116
#define IDB_ARRUP2_HOT                  117
#define IDB_ARRUP2_PRESSED              118

#ifndef UNICODE
#define __stricmp           _stricmp
#else
#define __stricmp           _wcsicmp
#endif
#define ELEMSPEC_STRING     0
#define ELEMSPEC_ATOM       1

#define NUM_DEFCOLORS                 175

typedef struct tagCOLORDEF            COLDEF, *PCOLDEF;
typedef struct tagCURSORDEF           CURDEF, *PCURDEF;
typedef struct tagALIGNDEF            ALIGNDEF, *PALIGNDEF;

#define XCHGCOLORREF(_x)              ((((_x)&0xFF00FF00L)|(((_x)&0x00FF0000L)>>16)|(((_x)&0x000000FFL)<<16)))

#define ALPHA_SHIFT 24
#define RED_SHIFT   16
#define GREEN_SHIFT 8
#define BLUE_SHIFT  0
#define ALPHA_MASK  (0xff << ALPHA_SHIFT)

#define MAKEARGB(a, r, g, b) \
                ((((a) & 0xff) << ALPHA_SHIFT)| \
                 (((r) & 0xff) << RED_SHIFT)  | \
                 (((g) & 0xff) << GREEN_SHIFT)| \
                 (((b) & 0xff) << BLUE_SHIFT))

//----------------------------------------------------
//----------------------------------------------------



struct tagCOLORDEF
{
  char        szColor[60];
  ULONG       uColor;
};

struct tagCURSORDEF
{
  char        szCursor[60];
  LPCTSTR     lpCursorName;
};

struct tagALIGNDEF
{
  char        szAlign[60];
  UINT        uAlign;
};

static COLDEF sColors[NUM_DEFCOLORS] =
{
{ "`",0xF0F8FF },
{ "AntiqueWhite",0xFAEBD7 },
{ "Aqua",0x00FFFF },
{ "Aquamarine",0x7FFFD4 },
{ "Azure",0xF0FFFF },
{ "Beige",0xF5F5DC },
{ "Bisque",0xFFE4C4 },
{ "Black",0x000000 },
{ "BlanchedAlmond",0xFFEBCD },
{ "Blue",0x0000FF },
{ "BlueViolet",0x8A2BE2 },
{ "Brown",0xA52A2A },
{ "BurlyWood",0xDEB887 },
{ "CadetBlue",0x5F9EA0 },
{ "Chartreuse",0x7FFF00 },
{ "Chocolate",0xD2691E },
{ "Coral",0xFF7F50 },
{ "CornflowerBlue",0x6495ED },
{ "Cornsilk",0xFFF8DC },
{ "Crimson",0xDC143C },
{ "Cyan",0x00FFFF },
{ "DarkBlue",0x00008B },
{ "DarkCyan",0x008B8B },
{ "DarkGoldenrod",0xB8860B },
{ "DarkGray",0xA9A9A9 },
{ "DarkGreen",0x006400 },
{ "DarkGrey",0xA9A9A9 },
{ "DarkKhaki",0xBDB76B },
{ "DarkMagenta",0x8B008B },
{ "DarkOliveGreen",0x556B2F },
{ "DarkOrange",0xFF8C00 },
{ "DarkOrchid",0x9932CC },
{ "DarkRed",0x8B0000 },
{ "DarkSalmon",0xE9967A },
{ "DarkSeaGreen",0x8FBC8F },
{ "DarkSlateBlue",0x483D8B },
{ "DarkSlateGray",0x2F4F4F },
{ "DarkTurquoise",0x00CED1 },
{ "DarkViolet",0x9400D3 },
{ "DeepPink",0xFF1493 },
{ "DeepSkyBlue",0x00BFFF },
{ "DimGray",0x696969 },
{ "DkBlue",0x00008B },
{ "DkCyan",0x008B8B },
{ "DkGoldenrod",0xB8860B },
{ "DkGray",0xA9A9A9 },
{ "DkGreen",0x006400 },
{ "DkGrey",0xA9A9A9 },
{ "DkKhaki",0xBDB76B },
{ "DkMagenta",0x8B008B },
{ "DkOliveGreen",0x556B2F },
{ "DkOrange",0xFF8C00 },
{ "DkOrchid",0x9932CC },
{ "DkRed",0x8B0000 },
{ "DkSalmon",0xE9967A },
{ "DkSeaGreen",0x8FBC8F },
{ "DkSlateBlue",0x483D8B },
{ "DkSlateGray",0x2F4F4F },
{ "DkTurquoise",0x00CED1 },
{ "DkViolet",0x9400D3 },
{ "DodgerBlue",0x1E90FF },
{ "FireBrick",0xB22222 },
{ "FloralWhite",0xFFFAF0 },
{ "ForestGreen",0x228B22 },
{ "Fuchsia",0xFF00FF },
{ "Gainsboro",0xDCDCDC },
{ "GhostWhite",0xF8F8FF },
{ "Gold",0xFFD700 },
{ "Goldenrod",0xDAA520 },
{ "Gray",0x808080 },
{ "Green",0x008000 },
{ "GreenYellow",0xADFF2F },
{ "Grey",0x808080 },
{ "Honeydew",0xF0FFF0 },
{ "HotPink",0xFF69B4 },
{ "IndianRed",0xCD5C5C },
{ "Indigo",0x4B0082 },
{ "Ivory",0xFFFFF0 },
{ "Khaki",0xF0E68C },
{ "Lavender",0xE6E6FA },
{ "LavenderBlush",0xFFF0F5 },
{ "LawnGreen",0x7CFC00 },
{ "LemonChiffon",0xFFFACD },
{ "LightBlue",0xADD8E6 },
{ "LightCoral",0xF08080 },
{ "LightCyan",0xE0FFFF },
{ "LightGoldenrodYellow",0xFAFAD2 },
{ "LightGray",0xD3D3D3 },
{ "LightGreen",0x90EE90 },
{ "LightGrey",0xD3D3D3 },
{ "LightPink",0xFFB6C1 },
{ "LightSalmon",0xFFA07A },
{ "LightSeaGreen",0x20B2AA },
{ "LightSkyBlue",0x87CEFA },
{ "LightSlateGray",0x778899 },
{ "LightSteelBlue",0xB0C4DE },
{ "LightYellow",0xFFFFE0 },
{ "Lime",0x00FF00 },
{ "LimeGreen",0x32CD32 },
{ "Linen",0xFAF0E6 },
{ "LtBlue",0xADD8E6 },
{ "LtCoral",0xF08080 },
{ "LtCyan",0xE0FFFF },
{ "LtGoldenrodYellow",0xFAFAD2 },
{ "LtGray",0xD3D3D3 },
{ "LtGreen",0x90EE90 },
{ "LtGrey",0xD3D3D3 },
{ "LtPink",0xFFB6C1 },
{ "LtSalmon",0xFFA07A },
{ "LtSeaGreen",0x20B2AA },
{ "LtSkyBlue",0x87CEFA },
{ "LtSlateGray",0x778899 },
{ "LtSteelBlue",0xB0C4DE },
{ "LtYellow",0xFFFFE0 },
{ "Magenta",0xFF00FF },
{ "Maroon",0x800000 },
{ "MediumAquamarine",0x66CDAA },
{ "MediumBlue",0x0000CD },
{ "MediumOrchid",0xBA55D3 },
{ "MediumPurple",0x9370DB },
{ "MediumSeaGreen",0x3CB371 },
{ "MediumSlateBlue",0x7B68EE },
{ "MediumSpringGreen",0x00FA9A },
{ "MediumTurquoise",0x48D1CC },
{ "MediumVioletRed",0xC71585 },
{ "MidnightBlue",0x191970 },
{ "MintCream",0xF5FFFA },
{ "MistyRose",0xFFE4E1 },
{ "Moccasin",0xFFE4B5 },
{ "NavajoWhite",0xFFDEAD },
{ "Navy",0x000080 },
{ "OldLace",0xFDF5E6 },
{ "Olive",0x808000 },
{ "OliveDrab",0x6B8E23 },
{ "Orange",0xFFA500 },
{ "OrangeRed",0xFF4500 },
{ "Orchid",0xDA70D6 },
{ "PaleGoldenrod",0xEEE8AA },
{ "PaleGreen",0x98FB98 },
{ "PaleTurquoise",0xAFEEEE },
{ "PaleVioletRed",0xDB7093 },
{ "PapayaWhip",0xFFEFD5 },
{ "PeachPuff",0xFFDAB9 },
{ "Peru",0xCD853F },
{ "Pink",0xFFC0CB },
{ "Plum",0xDDA0DD },
{ "PowderBlue",0xB0E0E6 },
{ "Purple",0x800080 },
{ "Red",0xFF0000 },
{ "RosyBrown",0xBC8F8F },
{ "RoyalBlue",0x4169E1 },
{ "SaddleBrown",0x8B4513 },
{ "Salmon",0xFA8072 },
{ "SandyBrown",0xF4A460 },
{ "SeaGreen",0x2E8B57 },
{ "Seashell",0xFFF5EE },
{ "Sienna",0xA0522D },
{ "Silver",0xC0C0C0 },
{ "SkyBlue",0x87CEEB },
{ "SlateBlue",0x6A5ACD },
{ "SlateGray",0x708090 },
{ "Snow",0xFFFAFA },
{ "SpringGreen",0x00FF7F },
{ "SteelBlue",0x4682B4 },
{ "Tan",0xD2B48C },
{ "Teal",0x008080 },
{ "Thistle",0xD8BFD8 },
{ "Tomato",0xFF6347 },
{ "Turquoise",0x40E0D0 },
{ "Violet",0xEE82EE },
{ "Wheat",0xF5DEB3 },
{ "White",0xFFFFFF },
{ "WhiteSmoke",0xF5F5F5 },
{ "Yellow",0xFFFF00 },
{ "YellowGreen",0x9ACD32 }
};

#define NUM_DEFALIGNS               29

static ALIGNDEF     sAlignments[NUM_DEFALIGNS] =
{
  { "bottom", TDA_BOTTOM },
  { "bottomcenter", TDA_BOTTOMCENTER },
  { "bottomleft", TDA_BOTTOMLEFT },
  { "bottomright", TDA_BOTTOMRIGHT },
  { "center", TDA_CENTER },
  { "centerbottom", TDA_CENTERBOTTOM },
  { "centermiddle", TDA_CENTERMIDDLE },
  { "centertop", TDA_CENTERTOP },
  { "focusrect", TDA_FOCUSRECT },
  { "left", TDA_LEFT },
  { "leftbottom", TDA_LEFTBOTTOM },
  { "leftmiddle", TDA_LEFTMIDDLE },
  { "lefttop", TDA_LEFTTOP },
  { "middle", TDA_MIDDLE },
  { "middlecenter", TDA_MIDDLECENTER },
  { "middleleft", TDA_MIDDLELEFT },
  { "middleright", TDA_MIDDLERIGHT },
  { "right", TDA_RIGHT },
  { "rightbottom", TDA_RIGHTBOTTOM },
  { "rightmiddle", TDA_RIGHTMIDDLE },
  { "righttop", TDA_RIGHTTOP },
  { "top", TDA_TOP },
  { "topcenter", TDA_TOPCENTER },
  { "topleft", TDA_TOPLEFT },
  { "topright", TDA_TOPRIGHT },
  { "wrap", TDA_WRAP },
  { "wrapcenter", TDA_WRAPCENTER },
  { "wrapleft", TDA_WRAPLEFT },
  { "wrapright", TDA_WRAPRIGHT }
};

#define NUM_DEFCURSORS                14

static CURDEF         sCursors[NUM_DEFCURSORS] =
{
{ "arrow", IDC_ARROW },
{ "cross", IDC_CROSS },
{ "hand", IDC_HAND },
{ "help", IDC_HELP },
{ "ibeam", IDC_IBEAM },
{ "no", IDC_NO },
{ "size", IDC_SIZE },
{ "sizeall", IDC_SIZEALL },
{ "sizenesw", IDC_SIZENESW },
{ "sizens", IDC_SIZENS },
{ "sizenwse", IDC_SIZENWSE },
{ "sizewe", IDC_SIZEWE },
{ "uparrow", IDC_UPARROW },
{ "wait", IDC_WAIT }
};

static int CALLBACK EnumFontFamProc( ENUMLOGFONT *lpelf, NEWTEXTMETRIC *lpntm, int FontType, LPARAM lParam )
{
    return 0;
}

int Pt2FontHeight ( int iPt )
{
  HDC         hDC = GetDC(NULL);
   POINT       pt,ptOrg;

  pt.y  = MulDiv(iPt,GetDeviceCaps(hDC, LOGPIXELSY),72);
  DPtoLP(hDC, &pt, 1);
  ptOrg.x = 0;
  ptOrg.y = 0;
  DPtoLP(hDC, &ptOrg, 1);
  ReleaseDC(NULL,hDC);
  return -abs(pt.y - ptOrg.y);
}

typedef struct tagELEMSPECIFIER       ELEMSPEC, *PELEMSPEC;
struct tagELEMSPECIFIER
{
  UINT        uType;
  char        szString[256];
};

typedef struct tagTOKENDATA    TOKDATA, *PTOKDATA;
struct tagTOKENDATA
{
  UINT        uType;
  union
  {
    int               iValue;
    COLORREF          crRGB;
    COLORREF          crARGB;         // alpha: 0 = opaque, 255 = transparent (is that right?)
    UINT              uAlign;
    LPCTSTR           lpCursorName;
    UINT              uFontStyle;
    TCHAR             szString[MAX_PATH];
    struct
    {
      COLORREF        crARGB1;
      COLORREF        crARGB2;
      int             iValue;
    } gradient;
    struct
    {
      UINT            uIntTypes[4];
      RECT            rcRect;
    } rect;
    struct
    {
      HBITMAP         hBitmap;        // DIB section
      int             biCompression;
      BOOL            bColorIdx;
      union
      {
        COLORREF      crTransRGB;
        int           iTransIdx;
      };
    } bitmap;
  };
};

typedef struct tagTOKEN        TOKEN, *PTOKEN;
struct tagTOKEN
{
  PTOKEN                    pPrev;
  PTOKEN                    pNext;
  UINT                      uToken;
  UINT                      uDataSize;
  BYTE                      byData[4];
};


typedef struct tagEXPBARINFO              EXPBARINFO, *PEXPBARINFO;
struct tagEXPBARINFO
{
  int                   cbSize;

  BOOL                  bPreXP;             // true if pre XP style
  BOOL                  bUseThemeRenderer;  // true if theme renderer must be used
  BOOL                  bShellStyleDll;     // true if shellstyle Dll present and used

  TCHAR                 szMsStylesDllName[MAX_PATH];
  TCHAR                 szColorname[MAX_PATH];
  TCHAR                 szShellStyleDllName[MAX_PATH];

  // only for pre XP (arrows of normal collapsible header)

  int                   iBmpPreXPArrowWidth;
  int                   iBmpPreXPArrowHeight;
  HBITMAP               hAlphaPreXPBmpArrowUp[3];
  HBITMAP               hAlphaPreXPBmpArrowDn[3];

  // task pane

  struct
  {
    COLORREF            crBackground1;
    COLORREF            crBackground2;
    int                 iValue;
    RECT                rcPadding;
    RECT                rcBorderThickness;
    COLORREF            crBorder;
  } taskpane;

  // pane task links (normal)

  struct
  {
    RECT                rcTitlePadding;             // generic link padding (inner)
    RECT                rcMarginsActionTask;        // margins: action task (outer)
    RECT                rcMarginsDestinationTask;   // margins: destination task (outer)
    COLORREF            crLinkNormal;               // normal link color
    COLORREF            crLinkHot;                  // hot link color (mousefocused)
  } tasknormal;

  // pane task links (special)

  struct
  {
    RECT                rcTitlePadding;             // generic link padding (inner)
    RECT                rcMarginsActionTask;        // margins: action task (outer)
    RECT                rcMarginsDestinationTask;   // margins: destination task (outer)
    COLORREF            crLinkNormal;               // normal link color
    COLORREF            crLinkHot;                  // hot link color (mousefocused)
  } taskspecial;

  // normal header

  struct
  {
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
    HBITMAP             hAlphaBmpArrowUp[3];        // normal, hot, pressed ([0]==NULL: use ThemeRenderer)
                                                    // [2]==NULL: no pressed bitmap, use hot y+1
    HBITMAP             hAlphaBmpArrowDn[3];        // normal, hot, pressed ([0]==NULL: use ThemeRenderer)
                                                    // [2]==NULL: no pressed bitmap, use hot y+1

    COLORREF            crTLbackground;             // background color (task list)
    RECT                rcTLBorderThickness;        // border thickness
    COLORREF            crTLBorder;                 // border color
    RECT                rcTLPadding;                // padding (task pane to items)
  } headernormal;

  // special header

  struct
  {
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
    HBITMAP             hAlphaBmpArrowUp[3];        // normal, hot, pressed ([0]==NULL: use ThemeRenderer)
                                                    // [2]==NULL: no pressed bitmap, use hot y+1
    HBITMAP             hAlphaBmpArrowDn[3];        // normal, hot, pressed ([0]==NULL: use ThemeRenderer)
                                                    // [2]==NULL: no pressed bitmap, use hot y+1

    COLORREF            crTLbackground;             // background color (task list, watermark)
    UINT                uTLAlign;                   // alignment (watermark)
    RECT                rcTLBorderThickness;        // border thickness
    COLORREF            crTLBorder;                 // border color
    RECT                rcTLPadding;                // padding (task pane to items)
  } headerspecial;

};

extern BOOL               g_bXPTheme;

extern HIMAGELIST         g_hArrowIList;
extern UINT               g_uIconWidth;
extern UINT               g_uIconHeight;
extern UINT               g_uNumIcons;
extern BOOL               g_bEBI;
extern EXPBARINFO         g_sEBI;

BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );

static PTOKEN  ParsePropertyFile ( PBYTE pbyBuffer, UINT uSize );
static BOOL    GetStyleDllValues ( PTOKEN pTokens, PEXPBARINFO pEBI, HMODULE hinstDll );
static VOID    FreeTokens ( PTOKEN pTokens );
static PTOKEN  GetPropertyTokens ( PTOKEN     pTokens,
                           char      *pcszResId,        // e.g. "taskpane"
                           char      *pcszElement,      // e.g. "scrollbar"
                           int        iElemSpecifiers,  // no. of specifiers
                           PELEMSPEC  pSpecs,           // NULL or ptr to specs
                           char      *pcszProperty );
static BOOL    AnalyseToken ( PTOKEN pTokens, PTOKDATA pTD, HMODULE hinstDLL );
static PTOKEN  GetColor ( PTOKEN pTokens, DWORD *pdwColor, BOOL *pbIndex );
static HBITMAP Bitmap2AlphaBitmap ( HBITMAP hBitmap,
                                               DWORD   dwTransColor,
                                               BOOL    bColorIdx,
                                               int    *piWidth,
                                               int    *piHeight );

static HBITMAP LoadAlphaBitmap ( HMODULE   hInstDll,
                                            UINT      uBmpId,
                                            COLORREF  crReplaceCol,
                                            COLORREF  crReplaceBy,
                                            COLORREF  crTransparent,
                                            int      *piWidth,
                                            int      *piHeight );


BOOL APIENTRY GetLogFont ( HFONT hFont, PLOGFONT pLF );

VOID APIENTRY FreeExplorerBarInfo ( PEXPBARINFO pEBI )
{
  if (pEBI->hAlphaPreXPBmpArrowUp[0])
    DeleteObject(pEBI->hAlphaPreXPBmpArrowUp[0]);
  if (pEBI->hAlphaPreXPBmpArrowUp[1])
    DeleteObject(pEBI->hAlphaPreXPBmpArrowUp[1]);
  if (pEBI->hAlphaPreXPBmpArrowUp[2])
    DeleteObject(pEBI->hAlphaPreXPBmpArrowUp[2]);
  if (pEBI->hAlphaPreXPBmpArrowDn[0])
    DeleteObject(pEBI->hAlphaPreXPBmpArrowDn[0]);
  if (pEBI->hAlphaPreXPBmpArrowDn[1])
    DeleteObject(pEBI->hAlphaPreXPBmpArrowDn[1]);
  if (pEBI->hAlphaPreXPBmpArrowDn[2])
    DeleteObject(pEBI->hAlphaPreXPBmpArrowDn[2]);
  if (pEBI->headernormal.hAlphaBmpListHeader)
    DeleteObject(pEBI->headernormal.hAlphaBmpListHeader);
  if (pEBI->headernormal.hAlphaBmpArrowUp[0])
    DeleteObject(pEBI->headernormal.hAlphaBmpArrowUp[0]);
  if (pEBI->headernormal.hAlphaBmpArrowUp[1])
    DeleteObject(pEBI->headernormal.hAlphaBmpArrowUp[1]);
  if (pEBI->headernormal.hAlphaBmpArrowUp[2])
    DeleteObject(pEBI->headernormal.hAlphaBmpArrowUp[2]);
  if (pEBI->headernormal.hAlphaBmpArrowDn[0])
    DeleteObject(pEBI->headernormal.hAlphaBmpArrowDn[0]);
  if (pEBI->headernormal.hAlphaBmpArrowDn[1])
    DeleteObject(pEBI->headernormal.hAlphaBmpArrowDn[1]);
  if (pEBI->headernormal.hAlphaBmpArrowDn[2])
    DeleteObject(pEBI->headernormal.hAlphaBmpArrowDn[2]);
  if (pEBI->headerspecial.hAlphaBmpListHeader)
    DeleteObject(pEBI->headerspecial.hAlphaBmpListHeader);
  if (pEBI->headerspecial.hAlphaBmpArrowUp[0])
    DeleteObject(pEBI->headerspecial.hAlphaBmpArrowUp[0]);
  if (pEBI->headerspecial.hAlphaBmpArrowUp[1])
    DeleteObject(pEBI->headerspecial.hAlphaBmpArrowUp[1]);
  if (pEBI->headerspecial.hAlphaBmpArrowUp[2])
    DeleteObject(pEBI->headerspecial.hAlphaBmpArrowUp[2]);
  if (pEBI->headerspecial.hAlphaBmpArrowDn[0])
    DeleteObject(pEBI->headerspecial.hAlphaBmpArrowDn[0]);
  if (pEBI->headerspecial.hAlphaBmpArrowDn[1])
    DeleteObject(pEBI->headerspecial.hAlphaBmpArrowDn[1]);
  if (pEBI->headerspecial.hAlphaBmpArrowDn[2])
    DeleteObject(pEBI->headerspecial.hAlphaBmpArrowDn[2]);
}

BOOL APIENTRY GetExplorerBarInfo ( PEXPBARINFO pEBI, BOOL bIgnoreShellStyleDll )
{
  HKEY          hKey = NULL;
  TCHAR         szString[MAX_PATH];
  DWORD         dwSize;
  TCHAR        *p;
  HMODULE       hinstDll;
  HRSRC         hShellInfo;
  HGLOBAL       hShellInfoMem;
  PVOID         pvShellInfo;
  UINT          uShellInfoSize;
  PTOKEN        pTokens;
  LOGFONT       lf;

  if ((!pEBI) || (pEBI->cbSize!=sizeof(EXPBARINFO)))
    return FALSE;

  memset(pEBI,0,sizeof(EXPBARINFO));
  pEBI->cbSize = sizeof(EXPBARINFO);

  if (!hUxTheme)
  {
    DoThemePreXP:
    pEBI->bPreXP = TRUE;

    pEBI->taskpane.crBackground1    = GetSysColor(COLOR_WINDOW);
    pEBI->taskpane.crBackground2    = GetSysColor(COLOR_WINDOW);
    pEBI->taskpane.iValue           = 0;
    pEBI->taskpane.rcPadding.left   =
    pEBI->taskpane.rcPadding.top    =
    pEBI->taskpane.rcPadding.right  =
    pEBI->taskpane.rcPadding.bottom = 12;

    pEBI->tasknormal.rcTitlePadding.left   = 6;
    pEBI->tasknormal.rcTitlePadding.top    = 0;
    pEBI->tasknormal.rcTitlePadding.right  = 4;
    pEBI->tasknormal.rcTitlePadding.bottom = 0;
    pEBI->tasknormal.rcMarginsActionTask.left   = 0;
    pEBI->tasknormal.rcMarginsActionTask.top    = 4;
    pEBI->tasknormal.rcMarginsActionTask.right  = 0;
    pEBI->tasknormal.rcMarginsActionTask.bottom = 0;
    pEBI->tasknormal.rcMarginsDestinationTask.left   = 0;
    pEBI->tasknormal.rcMarginsDestinationTask.top    = 4;
    pEBI->tasknormal.rcMarginsDestinationTask.right  = 0;
    pEBI->tasknormal.rcMarginsDestinationTask.bottom = 0;
    pEBI->tasknormal.crLinkNormal = GetSysColor(COLOR_BTNTEXT);
    pEBI->tasknormal.crLinkHot    = GetSysColor(COLOR_HOTLIGHT);

    memcpy(&pEBI->taskspecial,&pEBI->tasknormal,sizeof(pEBI->tasknormal));

    if (GetLogFont((HFONT)GetStockObject(DEFAULT_GUI_FONT),&lf))
    {
#ifdef UNICODE
      WideCharToMultiByte(CP_ACP,0,lf.lfFaceName,-1,pEBI->headernormal.szFontFace,MAX_PATH,NULL,NULL);
#else
      strcpy(pEBI->headernormal.szFontFace,lf.lfFaceName);
#endif
      pEBI->headernormal.iFontSize = lf.lfHeight;
    }
    else
    {
      strcpy(pEBI->headernormal.szFontFace,"Arial");
      pEBI->headernormal.iFontSize = -13;
    }
    pEBI->headernormal.iMargin           = 15;
    pEBI->headernormal.crHeaderNormal    = GetSysColor(COLOR_BTNTEXT);
    pEBI->headernormal.crHeaderHot       = GetSysColor(COLOR_BTNTEXT);
    pEBI->headernormal.uAlignment        = TDA_MIDDLELEFT;
    pEBI->headernormal.iFontWeightHeader = FW_BOLD;

    pEBI->headernormal.rcPaddingHeader.left   = 10;
    pEBI->headernormal.rcPaddingHeader.top    = 0;
    pEBI->headernormal.rcPaddingHeader.right  = 1;
    pEBI->headernormal.rcPaddingHeader.bottom = 0;

    pEBI->headernormal.rcBorderThicknessHeader.left   = 2;
    pEBI->headernormal.rcBorderThicknessHeader.top    = 2;
    pEBI->headernormal.rcBorderThicknessHeader.right  = 2;
    pEBI->headernormal.rcBorderThicknessHeader.bottom = 0;

    pEBI->headernormal.crBorderColorHeader = GetSysColor(COLOR_3DFACE);

    pEBI->headernormal.iHeaderBmpWidth  = 186;
    pEBI->headernormal.iHeaderBmpHeight = 21;

    pEBI->headernormal.hAlphaBmpArrowUp[0] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRUP_NORMAL,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_BTNTEXT),
                                                             RGB(255,0,0),
                                                             &pEBI->headernormal.iBmpArrowWidth,
                                                             &pEBI->headernormal.iBmpArrowHeight);
    pEBI->headernormal.hAlphaBmpArrowUp[1] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRUP_HOT,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HOTLIGHT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);
    pEBI->headernormal.hAlphaBmpArrowUp[2] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRUP_PRESSED,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HOTLIGHT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);

    pEBI->headernormal.hAlphaBmpArrowDn[0] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRDN_NORMAL,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_BTNTEXT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);
    pEBI->headernormal.hAlphaBmpArrowDn[1] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRDN_HOT,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HOTLIGHT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);
    pEBI->headernormal.hAlphaBmpArrowDn[2] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRDN_PRESSED,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HOTLIGHT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);

    pEBI->headernormal.crTLbackground = GetSysColor(COLOR_WINDOW);
    pEBI->headernormal.crTLBorder     = GetSysColor(COLOR_3DFACE);

    pEBI->headernormal.rcTLBorderThickness.left   = 1;
    pEBI->headernormal.rcTLBorderThickness.right  = 1;
    pEBI->headernormal.rcTLBorderThickness.top    = 0;
    pEBI->headernormal.rcTLBorderThickness.bottom = 1;

    pEBI->headernormal.rcTLPadding.left = pEBI->headernormal.rcTLPadding.right  = 12;
    pEBI->headernormal.rcTLPadding.top  = pEBI->headernormal.rcTLPadding.bottom = 10;

    if (GetLogFont((HFONT)GetStockObject(DEFAULT_GUI_FONT),&lf))
    {
#ifdef UNICODE
      WideCharToMultiByte(CP_ACP,0,lf.lfFaceName,-1,pEBI->headerspecial.szFontFace,MAX_PATH,NULL,NULL);
#else
      strcpy(pEBI->headerspecial.szFontFace,lf.lfFaceName);
#endif
      pEBI->headerspecial.iFontSize = lf.lfHeight;
    }
    else
    {
      strcpy(pEBI->headerspecial.szFontFace,"Arial");
      pEBI->headerspecial.iFontSize = -13;
    }
    pEBI->headerspecial.iMargin           = 15;
    pEBI->headerspecial.crHeaderNormal    = GetSysColor(COLOR_HIGHLIGHTTEXT);
    pEBI->headerspecial.crHeaderHot       = GetSysColor(COLOR_HIGHLIGHTTEXT);
    pEBI->headerspecial.uAlignment        = TDA_MIDDLELEFT;
    pEBI->headerspecial.iFontWeightHeader = FW_BOLD;

    pEBI->headerspecial.rcPaddingHeader.left   = 10;
    pEBI->headerspecial.rcPaddingHeader.top    = 0;
    pEBI->headerspecial.rcPaddingHeader.right  = 1;
    pEBI->headerspecial.rcPaddingHeader.bottom = 0;

    pEBI->headerspecial.rcBorderThicknessHeader.left   = 2;
    pEBI->headerspecial.rcBorderThicknessHeader.top    = 2;
    pEBI->headerspecial.rcBorderThicknessHeader.right  = 2;
    pEBI->headerspecial.rcBorderThicknessHeader.bottom = 0;

    pEBI->headerspecial.crBorderColorHeader = GetSysColor(COLOR_HIGHLIGHT);

    pEBI->headerspecial.iHeaderBmpWidth  = 186;
    pEBI->headerspecial.iHeaderBmpHeight = 21;

    pEBI->headerspecial.hAlphaBmpArrowUp[0] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRUP_NORMAL,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HIGHLIGHTTEXT),
                                                             RGB(255,0,0),
                                                             &pEBI->headerspecial.iBmpArrowWidth,
                                                             &pEBI->headerspecial.iBmpArrowHeight);
    pEBI->headerspecial.hAlphaBmpArrowUp[1] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRUP_HOT,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HIGHLIGHTTEXT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);
    pEBI->headerspecial.hAlphaBmpArrowUp[2] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRUP_PRESSED,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HIGHLIGHTTEXT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);

    pEBI->headerspecial.hAlphaBmpArrowDn[0] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRDN_NORMAL,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HIGHLIGHTTEXT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);
    pEBI->headerspecial.hAlphaBmpArrowDn[1] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRDN_HOT,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HIGHLIGHTTEXT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);
    pEBI->headerspecial.hAlphaBmpArrowDn[2] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRDN_PRESSED,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HIGHLIGHTTEXT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);

    pEBI->headerspecial.crTLbackground = GetSysColor(COLOR_WINDOW);
    pEBI->headerspecial.crTLBorder     = GetSysColor(COLOR_HIGHLIGHT);
    pEBI->headerspecial.uTLAlign       = TDA_BOTTOMRIGHT;

    pEBI->headerspecial.rcTLBorderThickness.left   = 1;
    pEBI->headerspecial.rcTLBorderThickness.right  = 1;
    pEBI->headerspecial.rcTLBorderThickness.top    = 0;
    pEBI->headerspecial.rcTLBorderThickness.bottom = 1;

    pEBI->headerspecial.rcTLPadding.left = pEBI->headerspecial.rcTLPadding.right  = 12;
    pEBI->headerspecial.rcTLPadding.top  = pEBI->headerspecial.rcTLPadding.bottom = 10;

    pEBI->hAlphaPreXPBmpArrowUp[0] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRUP2_NORMAL,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_BTNTEXT),
                                                             RGB(255,0,0),
                                                             &pEBI->iBmpPreXPArrowWidth,
                                                             &pEBI->iBmpPreXPArrowHeight);
    pEBI->hAlphaPreXPBmpArrowUp[1] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRUP2_HOT,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HOTLIGHT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);
    pEBI->hAlphaPreXPBmpArrowUp[2] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRUP2_PRESSED,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HOTLIGHT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);

    pEBI->hAlphaPreXPBmpArrowDn[0] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRDN2_NORMAL,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_BTNTEXT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);
    pEBI->hAlphaPreXPBmpArrowDn[1] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRDN2_HOT,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HOTLIGHT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);
    pEBI->hAlphaPreXPBmpArrowDn[2] = LoadAlphaBitmap(g_hInstDLL,
                                                             IDB_ARRDN2_PRESSED,
                                                             RGB(13,14,15),
                                                             GetSysColor(COLOR_HOTLIGHT),
                                                             RGB(255,0,0),
                                                             NULL,NULL);

    return TRUE;
  }

  if ((ERROR_SUCCESS!=RegOpenKey(HKEY_CURRENT_USER,
                                 __T("Software\\Microsoft\\Windows\\CurrentVersion\\ThemeManager"),
                                 &hKey)) || (!hKey))
    goto DoThemePreXP;

  dwSize=sizeof(szString);
  if (ERROR_SUCCESS!=RegQueryValueEx(hKey,__T("ThemeActive"),NULL,NULL,(LPBYTE)szString,&dwSize))
  {
    RegCloseKey(hKey);
    goto DoThemePreXP;
  }

#ifdef UNICODE
  if (!_wtoi(szString))
#else
  if (!atoi(szString))
#endif
  {
    RegCloseKey(hKey);
    goto DoThemePreXP;
  }

  dwSize=sizeof(szString);
  if (ERROR_SUCCESS!=RegQueryValueEx(hKey,__T("DllName"),NULL,NULL,(LPBYTE)szString,&dwSize))
  {
    RegCloseKey(hKey);
    goto DoThemePreXP;
  }
  ExpandEnvironmentStrings(szString,pEBI->szMsStylesDllName,MAX_PATH-1);

  __strcpy(szString,pEBI->szMsStylesDllName);
  __strlwr(szString);
  if (!__strstr(szString,__T(".msstyles")))
  {
    RegCloseKey(hKey);
    memset(pEBI->szMsStylesDllName,0,sizeof(pEBI->szMsStylesDllName));
    goto DoThemePreXP;
  }

  dwSize=sizeof(pEBI->szColorname);
  if (ERROR_SUCCESS!=RegQueryValueEx(hKey,__T("ColorName"),NULL,NULL,(PBYTE)pEBI->szColorname,&dwSize))
  {
    RegCloseKey(hKey);
    ErrDoThemePreXP:
    memset(pEBI->szMsStylesDllName,0,sizeof(pEBI->szMsStylesDllName));
    memset(pEBI->szColorname,0,sizeof(pEBI->szColorname));
    goto DoThemePreXP;
  }
  RegCloseKey(hKey);

  __strcpy(szString,pEBI->szMsStylesDllName);
  p=__strrchr(szString,__T('\\'));
  if (!p)
    goto ErrDoThemePreXP;

  *p=0;
  __strcat(szString,__T("\\shell\\"));
  __strcat(szString,pEBI->szColorname);
  __strcat(szString,__T("\\shellstyle.dll"));

  __strcpy(pEBI->szShellStyleDllName,szString);

  // Try to open shell style dll and extract information

  if ((bIgnoreShellStyleDll) || (!(hinstDll = LoadLibraryEx(pEBI->szShellStyleDllName,NULL,LOAD_LIBRARY_AS_DATAFILE))))
  {
    NoShellStyle:
    //pEBI->bUseThemeRenderer = TRUE;
    goto DoThemePreXP;
  }
  else            // shell style dll present
  {
    pEBI->bShellStyleDll = TRUE;

    hShellInfo = FindResource(hinstDll,MAKEINTRESOURCE(1),TEXT("UIFILE"));

    if (!hShellInfo)
    {
      FreeLibrary(hinstDll);
      goto NoShellStyle;
    }

    hShellInfoMem = LoadResource(hinstDll,hShellInfo);
    if (!hShellInfoMem)
    {
      FreeLibrary(hinstDll);
      goto NoShellStyle;
    }

    uShellInfoSize = SizeofResource(hinstDll,hShellInfo);

    pvShellInfo = LockResource(hShellInfoMem);

    if ((!uShellInfoSize) || (!pvShellInfo))
    {
      if (pvShellInfo) UnlockResource(hShellInfoMem);
      FreeResource(hShellInfoMem);
      FreeLibrary(hinstDll);
      goto NoShellStyle;
    }

    pTokens = ParsePropertyFile((PBYTE)pvShellInfo,uShellInfoSize);

    if (!pTokens)
    {
      UnlockResource(hShellInfoMem);
      FreeResource(hShellInfoMem);
      FreeLibrary(hinstDll);
      goto NoShellStyle;
    }

    if (!GetStyleDllValues(pTokens,pEBI,hinstDll))
    {
      if (pEBI->headernormal.hAlphaBmpListHeader)
        DeleteObject(pEBI->headernormal.hAlphaBmpListHeader);
      if (pEBI->headernormal.hAlphaBmpArrowUp[0])
        DeleteObject(pEBI->headernormal.hAlphaBmpArrowUp[0]);
      if (pEBI->headernormal.hAlphaBmpArrowUp[1])
        DeleteObject(pEBI->headernormal.hAlphaBmpArrowUp[1]);
      if (pEBI->headernormal.hAlphaBmpArrowUp[2])
        DeleteObject(pEBI->headernormal.hAlphaBmpArrowUp[2]);
      if (pEBI->headernormal.hAlphaBmpArrowDn[0])
        DeleteObject(pEBI->headernormal.hAlphaBmpArrowDn[0]);
      if (pEBI->headernormal.hAlphaBmpArrowDn[1])
        DeleteObject(pEBI->headernormal.hAlphaBmpArrowDn[1]);
      if (pEBI->headernormal.hAlphaBmpArrowDn[2])
        DeleteObject(pEBI->headernormal.hAlphaBmpArrowDn[2]);
      if (pEBI->headerspecial.hAlphaBmpListHeader)
        DeleteObject(pEBI->headerspecial.hAlphaBmpListHeader);
      if (pEBI->headerspecial.hAlphaBmpArrowUp[0])
        DeleteObject(pEBI->headerspecial.hAlphaBmpArrowUp[0]);
      if (pEBI->headerspecial.hAlphaBmpArrowUp[1])
        DeleteObject(pEBI->headerspecial.hAlphaBmpArrowUp[1]);
      if (pEBI->headerspecial.hAlphaBmpArrowUp[2])
        DeleteObject(pEBI->headerspecial.hAlphaBmpArrowUp[2]);
      if (pEBI->headerspecial.hAlphaBmpArrowDn[0])
        DeleteObject(pEBI->headerspecial.hAlphaBmpArrowDn[0]);
      if (pEBI->headerspecial.hAlphaBmpArrowDn[1])
        DeleteObject(pEBI->headerspecial.hAlphaBmpArrowDn[1]);
      if (pEBI->headerspecial.hAlphaBmpArrowDn[2])
        DeleteObject(pEBI->headerspecial.hAlphaBmpArrowDn[2]);
      FreeTokens(pTokens);
      UnlockResource(hShellInfoMem);
      FreeResource(hShellInfoMem);
      FreeLibrary(hinstDll);
      goto NoShellStyle;
    }

    FreeTokens(pTokens);
    UnlockResource(hShellInfoMem);
    FreeResource(hShellInfoMem);
    FreeLibrary(hinstDll);
  }

  return TRUE;
}

#define CHARAVAIL           (uSize!=0)
#define IGNWS               { while ((CHARAVAIL) && ((*pbyBuffer)<=0x20)){pbyBuffer++;uSize--;} }
#define GETCHAR             ((!uSize) ? 0x00 : (*pbyBuffer))
#define EATCHAR             { if (uSize) { pbyBuffer++;uSize--; } }
#define CHARLA1             ((uSize<2) ? 0x00 : pbyBuffer[1])
#define HEXDIGIT(_c)        ( ( ((_c)>='0') && ((_c)<='9') ) || ( ((_c)>='A') && ((_c)<='F') ) || ( ((_c)>='a') && ((_c)<='f') ) )
#define DIGIT(_c)           (((_c)>='0') && ((_c)<='9'))

static PTOKEN ParsePropertyFile ( PBYTE pbyBuffer, UINT uSize )
{
  PTOKEN                    pRoot  = NULL;
  PTOKEN                    pAct   = NULL;
  PTOKEN                    pToken = NULL;
  BYTE                      byChar,byCharLA1;
  BYTE                      byString[256];
  UINT                      uStrLen,uToken;
  ULONG                     uHexVal;

  TokenLoop:
  if (!CHARAVAIL) return pRoot;
  IGNWS;
  byChar = GETCHAR;
  switch(byChar)
  {
    case '<':
      uToken = TOK_LT;
      DoToken:
      EATCHAR;
      pToken = (PTOKEN)malloc(sizeof(TOKEN));
      if (!pToken)
        goto ExitError;
      memset(pToken,0,sizeof(TOKEN));
      pToken->uToken = uToken;
      goto LinkToken;

    case '>':
      uToken = TOK_GT;
      goto DoToken;

    case '=':
      uToken = TOK_EQ;
      goto DoToken;

    case '(':
      uToken = TOK_LP;
      goto DoToken;

    case ')':
      uToken = TOK_RP;
      goto DoToken;

    case '[':
      uToken = TOK_LB;
      goto DoToken;

    case ']':
      uToken = TOK_RB;
      goto DoToken;

    case '{':
      uToken = TOK_LC;
      goto DoToken;

    case '}':
      uToken = TOK_RC;
      goto DoToken;

    case ':':
      uToken = TOK_COLON;
      goto DoToken;

    case ';':
      uToken = TOK_SEMICOLON;
      goto DoToken;

    case ',':
      uToken = TOK_COMMA;
      goto DoToken;

    case '|':
      uToken = TOK_OR;
      goto DoToken;

    case '/':
      uToken = TOK_SLASH;
      goto DoToken;

    case '#':
      byCharLA1 = CHARLA1;
      if (!(HEXDIGIT(byCharLA1)))
        goto UnknownToken;
      EATCHAR;
      uHexVal = 0L;
      byChar  = GETCHAR;
      while (HEXDIGIT(byChar))
      {
        byChar &= ~(0x20);
        byChar -= 0x10;
        if (byChar>0x09) byChar-=0x27;
        uHexVal = (uHexVal<<4)|((ULONG)byChar);
        EATCHAR;
        byChar = GETCHAR;
      }
      pToken = (PTOKEN)malloc(sizeof(TOKEN)+sizeof(ULONG));
      if (!pToken)
        goto ExitError;
      memset(pToken,0,sizeof(TOKEN)+sizeof(ULONG));

      pToken->uToken    = TOK_HTMLCOLOR;
      pToken->uDataSize = sizeof(ULONG);
      memcpy(pToken->byData,&uHexVal,sizeof(ULONG));
      goto LinkToken;

    case '-':
      uToken = TOK_MINUS;
      goto DoToken;

    case '+':
      uToken = TOK_PLUS;
      goto DoToken;

    case '0':
      if ((CHARLA1=='x') || (CHARLA1=='X')) // hex
      {
        EATCHAR;
        EATCHAR;
        uHexVal = 0;
        while ( (uSize) && HEXDIGIT(*pbyBuffer) )
        {
          byChar = *(pbyBuffer++);
          uSize--;
          uHexVal <<= 4;
          byChar &= ~(0x20);
          byChar -= 0x10;
          if (byChar>0x09) byChar-=0x27;
          uHexVal |= (ULONG)byChar;
        }
        goto PutValue;
      }
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      uHexVal = 0;
      while ( (uSize) && DIGIT(*pbyBuffer) )
      {
        byChar = *(pbyBuffer++);
        uSize--;
        uHexVal *= 10;
        uHexVal += (ULONG)(byChar-0x30);
      }
      PutValue:
      if ((pAct) && (pAct->uToken==TOK_MINUS))
      {
        pToken = pAct;
        pAct   = pAct->pPrev;
        if (!pAct) pRoot = NULL;
        free(pToken);
        uHexVal = (ULONG)(-((LONG)uHexVal));
      }
      else
      if ((pAct) && (pAct->uToken==TOK_PLUS))
      {
        pToken = pAct;
        pAct   = pAct->pPrev;
        if (!pAct) pRoot = NULL;
        free(pToken);
      }

      pToken = (PTOKEN)malloc(sizeof(TOKEN)+sizeof(ULONG));
      if (!pToken)
        goto ExitError;
      memset(pToken,0,sizeof(TOKEN)+sizeof(ULONG));

      pToken->uToken    = TOK_INTEGER;
      pToken->uDataSize = sizeof(ULONG);
      memcpy(pToken->byData,&uHexVal,sizeof(ULONG));
      goto LinkToken;

    default :
      if ( ((byChar>='A') && (byChar<='Z')) || ((byChar>='a') && (byChar<='z')) || (byChar=='_') )
      {
        uStrLen     = 1;
        byString[0] = byChar;
        EATCHAR;
        StringLoop:
        byChar = GETCHAR;
        if ( ((byChar>='A') && (byChar<='Z')) || ((byChar>='a') && (byChar<='z')) || ((byChar>='0') && (byChar<='9')) || (byChar=='_') )
        {
          byString[uStrLen++] = byChar;
          EATCHAR;
          goto StringLoop;
        }
        pToken = (PTOKEN)malloc(sizeof(TOKEN)+uStrLen);
        if (!pToken)
        {
          ExitError:
          while (pRoot)
          {
            pToken=pRoot;
            pRoot=pRoot->pNext;
            free(pToken);
          }
          return NULL;
        }
        memset(pToken,0,sizeof(TOKEN)+uStrLen);

        pToken->uToken    = TOK_STRING;
        pToken->uDataSize = uStrLen;
        memcpy(pToken->byData,byString,uStrLen);
        LinkToken:
        if (!pRoot)
          pRoot = pToken;
        else
        {
          pToken->pPrev = pAct;
          pAct->pNext = pToken;
        }
        pAct = pToken;
      }
      else
      {
        UnknownToken:
        pToken = (PTOKEN)malloc(sizeof(TOKEN));
        if (!pToken)
          goto ExitError;
        memset(pToken,0,sizeof(TOKEN));
        pToken->uToken    = TOK_UNKNOWN;
        pToken->uDataSize = 1;
        pToken->byData[0] = byChar;
        EATCHAR;
        goto LinkToken;
      }
      break;
  }
  goto TokenLoop;
}

static BOOL GetStyleDllValues ( PTOKEN pTokens, PEXPBARINFO pEBI, HMODULE hinstDll )
{
  ELEMSPEC      es[3];
  PTOKEN        pCurrToken;
  TOKDATA       TD;

  // 1.) task link information (normal) ////////////////////////////////////////////////////////////////

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectiontaskss",
                                 "button",0,NULL,
                                 "foreground");
  if (!pCurrToken) return FALSE;
  if (!AnalyseToken(pCurrToken,&TD,hinstDll)) return FALSE;
  if ((TD.uType!=TD_RGB) && (TD.uType!=TD_ARGB)) return FALSE;
  pEBI->tasknormal.crLinkNormal = TD.crRGB;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectiontaskss",
                                 "actiontask",0,NULL,
                                 "margin");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->tasknormal.rcMarginsActionTask,&TD.rect.rcRect,sizeof(RECT));

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectiontaskss",
                                 "destinationtask",0,NULL,
                                 "margin");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->tasknormal.rcMarginsDestinationTask,&TD.rect.rcRect,sizeof(RECT));

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"title");

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectiontaskss",
                                 "element",1,es,
                                 "padding");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->tasknormal.rcTitlePadding,&TD.rect.rcRect,sizeof(RECT));

  // may fail, if not: overrides color !!!
  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectiontaskss",
                                 "element",1,es,
                                 "foreground");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->tasknormal.crLinkNormal = TD.crRGB;

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"mousefocused");

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectiontaskss",
                                 "element",2,es,
                                 "foreground");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->tasknormal.crLinkHot = TD.crRGB;
  else
    return FALSE;

  // 2.) task link information (special) ///////////////////////////////////////////////////////////////

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectiontaskss",
                                 "button",0,NULL,
                                 "foreground");
  if (!pCurrToken) return FALSE;
  if (!AnalyseToken(pCurrToken,&TD,hinstDll)) return FALSE;
  if ((TD.uType!=TD_RGB) && (TD.uType!=TD_ARGB)) return FALSE;
  pEBI->taskspecial.crLinkNormal = TD.crRGB;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectiontaskss",
                                 "actiontask",0,NULL,
                                 "margin");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->taskspecial.rcMarginsActionTask,&TD.rect.rcRect,sizeof(RECT));

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectiontaskss",
                                 "destinationtask",0,NULL,
                                 "margin");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->taskspecial.rcMarginsDestinationTask,&TD.rect.rcRect,sizeof(RECT));

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"title");

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectiontaskss",
                                 "element",1,es,
                                 "padding");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->taskspecial.rcTitlePadding,&TD.rect.rcRect,sizeof(RECT));

  // may fail, if not: overrides color !!!
  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectiontaskss",
                                 "element",1,es,
                                 "foreground");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->taskspecial.crLinkNormal = TD.crRGB;

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"mousefocused");

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectiontaskss",
                                 "element",2,es,
                                 "foreground");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->taskspecial.crLinkHot = TD.crRGB;
  else
    return FALSE;

  // 3.) explorer bar information //////////////////////////////////////////////////////////////////////

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"sectionlist");

  pCurrToken = GetPropertyTokens(pTokens,
                                 "taskpane",
                                 "element",1,es,
                                 "background");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) )
  {
    if ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB))
    {
      pEBI->taskpane.crBackground1 = TD.crRGB;
      pEBI->taskpane.iValue        = 0;
    }
    else
    if (TD.uType==TD_GRADIENT)
    {
      pEBI->taskpane.crBackground1 = TD.gradient.crARGB1;
      pEBI->taskpane.crBackground2 = TD.gradient.crARGB2;
      pEBI->taskpane.iValue        = TD.gradient.iValue;
    }
    else
      return FALSE;
  }
  else
    return FALSE;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "taskpane",
                                 "element",1,es,
                                 "padding");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->taskpane.rcPadding,&TD.rect.rcRect,sizeof(RECT));
  else
  {
    pEBI->taskpane.rcPadding.left   = 12;
    pEBI->taskpane.rcPadding.top    = 12;
    pEBI->taskpane.rcPadding.right  = 12;
    pEBI->taskpane.rcPadding.bottom = 12;
  }

  pCurrToken = GetPropertyTokens(pTokens,
                                 "taskpane",
                                 "element",1,es,
                                 "borderthickness");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->taskpane.rcBorderThickness,&TD.rect.rcRect,sizeof(RECT));

  pCurrToken = GetPropertyTokens(pTokens,
                                 "taskpane",
                                 "element",1,es,
                                 "bordercolor");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->taskpane.crBorder = TD.crRGB;

  // 4.) normal header information /////////////////////////////////////////////////////////////////////

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "expando",0,NULL,
                                 "fontface");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_STRING) )

#ifdef UNICODE
    WideCharToMultiByte(CP_ACP,0,TD.szString,-1,pEBI->headernormal.szFontFace,MAX_PATH,NULL,NULL);
#else
    strcpy(pEBI->headernormal.szFontFace,TD.szString);
#endif
  else
  {
    HDC     hDC = GetDC(NULL);
    if ((EnumFontFamilies(hDC,TEXT("Tahoma"),(FONTENUMPROC)EnumFontFamProc,0) == 0))
      strcpy(pEBI->headernormal.szFontFace,"Tahoma");
    else
      strcpy(pEBI->headernormal.szFontFace,"Arial");
    ReleaseDC(NULL,hDC);
  }

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "expando",0,NULL,
                                 "fontsize");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) )
  {
    if ((TD.uType==TD_INTEGER) || (TD.uType==TD_INTEGER_RP))
      pEBI->headernormal.iFontSize = TD.iValue;
    else
    if (TD.uType==TD_INTEGER_PT)
      pEBI->headernormal.iFontSize = Pt2FontHeight(TD.iValue);
    else
      pEBI->headernormal.iFontSize = Pt2FontHeight(8);
  }
  else
    pEBI->headernormal.iFontSize = Pt2FontHeight(8);

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "expando",0,NULL,
                                 "margin");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
  {
    pEBI->headernormal.iMargin = TD.rect.rcRect.left;
    if (TD.rect.rcRect.top>pEBI->headernormal.iMargin) pEBI->headernormal.iMargin = TD.rect.rcRect.top;
    if (TD.rect.rcRect.right>pEBI->headernormal.iMargin) pEBI->headernormal.iMargin = TD.rect.rcRect.right;
    if (TD.rect.rcRect.bottom>pEBI->headernormal.iMargin) pEBI->headernormal.iMargin = TD.rect.rcRect.bottom;
  }
  else
    pEBI->headernormal.iMargin = 15;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "button",0,NULL,
                                 "bordercolor");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headernormal.crBorderColorHeader = TD.crRGB;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "button",0,NULL,
                                 "foreground");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headernormal.crHeaderNormal = TD.crRGB;
  else
    return FALSE;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "button",0,NULL,
                                 "fontweight");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_INTEGER) )
    pEBI->headernormal.iFontWeightHeader = TD.iValue;
  else
    pEBI->headernormal.iFontWeightHeader = FW_BOLD;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "button",0,NULL,
                                 "padding");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->headernormal.rcPaddingHeader,&TD.rect.rcRect,sizeof(RECT));

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "button",0,NULL,
                                 "borderthickness");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->headernormal.rcBorderThicknessHeader,&TD.rect.rcRect,sizeof(RECT));

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "button",0,NULL,
                                 "background");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headernormal.hAlphaBmpListHeader = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                &pEBI->headernormal.iHeaderBmpWidth,
                                                                &pEBI->headernormal.iHeaderBmpHeight);

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"title");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "element",1,es,
                                 "contentalign");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_ALIGNMENT) )
    pEBI->headernormal.uAlignment = TD.uAlign;

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"mousefocused");

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "element",2,es,
                                 "foreground");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headernormal.crHeaderHot = TD.crRGB;
  else
    pEBI->headernormal.crHeaderHot = pEBI->headernormal.crHeaderNormal;

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"arrow");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "element",1,es,
                                 "content");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headernormal.hAlphaBmpArrowDn[0] = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                &pEBI->headernormal.iBmpArrowWidth,
                                                                &pEBI->headernormal.iBmpArrowHeight);

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"selected");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "element",2,es,
                                 "content");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headernormal.hAlphaBmpArrowUp[0] = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                NULL,
                                                                NULL);

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"mousefocused");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "element",2,es,
                                 "content");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headernormal.hAlphaBmpArrowDn[1] = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                NULL,
                                                                NULL);

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"selected");
  es[2].uType = ELEMSPEC_STRING;
  strcpy(es[2].szString,"mousefocused");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "element",3,es,
                                 "content");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headernormal.hAlphaBmpArrowUp[1] = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                NULL,
                                                                NULL);

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"tasklist");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "tasklist",1,es,
                                 "background");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headernormal.crTLbackground = TD.crRGB;
  else
    return FALSE;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "tasklist",1,es,
                                 "borderthickness");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->headernormal.rcTLBorderThickness,&TD.rect.rcRect,sizeof(RECT));
  else
  {
    pEBI->headernormal.rcTLBorderThickness.left   = 1;
    pEBI->headernormal.rcTLBorderThickness.right  = 1;
    pEBI->headernormal.rcTLBorderThickness.top    = 0;
    pEBI->headernormal.rcTLBorderThickness.bottom = 1;
  }

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "tasklist",1,es,
                                 "padding");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->headernormal.rcTLPadding,&TD.rect.rcRect,sizeof(RECT));
  else
  {
    pEBI->headernormal.rcTLPadding.left = pEBI->headernormal.rcTLPadding.right  = 12;
    pEBI->headernormal.rcTLPadding.top  = pEBI->headernormal.rcTLPadding.bottom = 10;
  }

  pCurrToken = GetPropertyTokens(pTokens,
                                 "sectionss",
                                 "tasklist",1,es,
                                 "bordercolor");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headernormal.crTLBorder = TD.crRGB;
  else
    pEBI->headernormal.crTLBorder = RGB(0xFF,0xFF,0xFF);

  // 5.) special header information ////////////////////////////////////////////////////////////////////

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "expando",0,NULL,
                                 "fontface");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_STRING) )
#ifdef UNICODE
    WideCharToMultiByte(CP_ACP,0,TD.szString,-1,pEBI->headerspecial.szFontFace,MAX_PATH,NULL,NULL);
#else
    strcpy(pEBI->headerspecial.szFontFace,TD.szString);
#endif
  else
  {
    HDC     hDC = GetDC(NULL);
    if ((EnumFontFamilies(hDC,TEXT("Tahoma"),(FONTENUMPROC)EnumFontFamProc,0) == 0))
      strcpy(pEBI->headerspecial.szFontFace,"Tahoma");
    else
      strcpy(pEBI->headerspecial.szFontFace,"Arial");
    ReleaseDC(NULL,hDC);
  }

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "expando",0,NULL,
                                 "fontsize");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) )
  {
    if ((TD.uType==TD_INTEGER) || (TD.uType==TD_INTEGER_RP))
      pEBI->headerspecial.iFontSize = TD.iValue;
    else
    if (TD.uType==TD_INTEGER_PT)
      pEBI->headerspecial.iFontSize = Pt2FontHeight(TD.iValue);
    else
      pEBI->headerspecial.iFontSize = Pt2FontHeight(8);
  }
  else
    pEBI->headerspecial.iFontSize = Pt2FontHeight(8);

  pEBI->headerspecial.iMargin = pEBI->headernormal.iMargin;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "button",0,NULL,
                                 "bordercolor");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headerspecial.crBorderColorHeader = TD.crRGB;

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"header");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "button",1,es,
                                 "foreground");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headerspecial.crHeaderNormal = TD.crRGB;
  else
    return FALSE;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "button",1,es,
                                 "fontweight");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_INTEGER) )
    pEBI->headerspecial.iFontWeightHeader = TD.iValue;
  else
    pEBI->headerspecial.iFontWeightHeader = FW_BOLD;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "button",1,es,
                                 "padding");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->headerspecial.rcPaddingHeader,&TD.rect.rcRect,sizeof(RECT));

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "button",1,es,
                                 "borderthickness");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->headerspecial.rcBorderThicknessHeader,&TD.rect.rcRect,sizeof(RECT));

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "button",1,es,
                                 "background");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headerspecial.hAlphaBmpListHeader = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                &pEBI->headerspecial.iHeaderBmpWidth,
                                                                &pEBI->headerspecial.iHeaderBmpHeight);

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"title");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "element",1,es,
                                 "contentalign");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_ALIGNMENT) )
    pEBI->headerspecial.uAlignment = TD.uAlign;

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"mousefocused");

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "element",2,es,
                                 "foreground");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headerspecial.crHeaderHot = TD.crRGB;
  else
    pEBI->headerspecial.crHeaderHot = pEBI->headerspecial.crHeaderNormal;

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"arrow");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "element",1,es,
                                 "content");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headerspecial.hAlphaBmpArrowDn[0] = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                &pEBI->headerspecial.iBmpArrowWidth,
                                                                &pEBI->headerspecial.iBmpArrowHeight);

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"selected");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "element",2,es,
                                 "content");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headerspecial.hAlphaBmpArrowUp[0] = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                NULL,
                                                                NULL);

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"mousefocused");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "element",2,es,
                                 "content");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headerspecial.hAlphaBmpArrowDn[1] = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                NULL,
                                                                NULL);

  es[1].uType = ELEMSPEC_STRING;
  strcpy(es[1].szString,"selected");
  es[2].uType = ELEMSPEC_STRING;
  strcpy(es[2].szString,"mousefocused");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "element",3,es,
                                 "content");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_BITMAP) )
    pEBI->headerspecial.hAlphaBmpArrowUp[1] = Bitmap2AlphaBitmap(TD.bitmap.hBitmap,
                                                                TD.bitmap.crTransRGB,
                                                                TD.bitmap.bColorIdx,
                                                                NULL,
                                                                NULL);

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"watermark");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "element",1,es,
                                 "background");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headerspecial.crTLbackground = TD.crRGB;
  else
    return FALSE;

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "element",1,es,
                                 "contentalign");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_ALIGNMENT) )
    pEBI->headerspecial.uTLAlign = TD.uAlign;

  es[0].uType = ELEMSPEC_ATOM;
  strcpy(es[0].szString,"tasklist");
  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "tasklist",1,es,
                                 "borderthickness");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->headerspecial.rcTLBorderThickness,&TD.rect.rcRect,sizeof(RECT));
  else
  {
    pEBI->headerspecial.rcTLBorderThickness.left   = 1;
    pEBI->headerspecial.rcTLBorderThickness.right  = 1;
    pEBI->headerspecial.rcTLBorderThickness.top    = 0;
    pEBI->headerspecial.rcTLBorderThickness.bottom = 1;
  }

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "tasklist",1,es,
                                 "padding");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && (TD.uType==TD_RECT) )
    memcpy(&pEBI->headerspecial.rcTLPadding,&TD.rect.rcRect,sizeof(RECT));
  else
  {
    pEBI->headerspecial.rcTLPadding.left = pEBI->headerspecial.rcTLPadding.right  = 12;
    pEBI->headerspecial.rcTLPadding.top  = pEBI->headerspecial.rcTLPadding.bottom = 10;
  }

  pCurrToken = GetPropertyTokens(pTokens,
                                 "mainsectionss",
                                 "tasklist",1,es,
                                 "bordercolor");
  if ( (pCurrToken) && (AnalyseToken(pCurrToken,&TD,hinstDll)) && ((TD.uType==TD_RGB) || (TD.uType==TD_ARGB)) )
    pEBI->headerspecial.crTLBorder = TD.crRGB;
  else
    pEBI->headerspecial.crTLBorder = RGB(0xFF,0xFF,0xFF);

  return TRUE;
}


static VOID FreeTokens ( PTOKEN pTokens )
{
  PTOKEN        pTemp;
  while (pTokens)
  {
    pTemp=pTokens;
    pTokens=pTokens->pNext;
    free(pTemp);
  }
}

static PTOKEN GetPropertyTokens ( PTOKEN       pTokens,
                                             char        *pcszResId,        // e.g. "taskpane"
                                             char        *pcszElement,      // e.g. "scrollbar"
                                             int          iElemSpecifiers,  // no. of specifiers
                                             PELEMSPEC    pSpecs,           // NULL or ptr to specs
                                             char        *pcszProperty )
{
  int       i;

  while( pTokens && !( (pTokens) && (pTokens->uToken==TOK_LT) &&
            (pTokens->pNext) && (pTokens->pNext->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->pNext->byData,"style")) &&
            (pTokens->pNext->pNext) && (pTokens->pNext->pNext->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->pNext->pNext->byData,"resid")) &&
            (pTokens->pNext->pNext->pNext) && (pTokens->pNext->pNext->pNext->uToken==TOK_EQ) &&
            (pTokens->pNext->pNext->pNext->pNext) && (pTokens->pNext->pNext->pNext->pNext->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->pNext->pNext->pNext->pNext->byData,pcszResId)) &&
            (pTokens->pNext->pNext->pNext->pNext->pNext) && (pTokens->pNext->pNext->pNext->pNext->pNext->uToken==TOK_GT)
        ))
  {
    pTokens=pTokens->pNext;
  }

  if (!pTokens) return NULL;

  pTokens = pTokens->pNext->pNext->pNext->pNext->pNext->pNext;

  NextElement:

  if (!pTokens) return NULL;

  while (! ( (pTokens) && (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,pcszElement)) ) )
  {
    if ( (pTokens) && (pTokens->uToken==TOK_LT) &&
         (pTokens->pNext) && (pTokens->pNext->uToken==TOK_SLASH) &&
         (pTokens->pNext->pNext) && (pTokens->pNext->pNext->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->pNext->pNext->byData,"style")) &&
         (pTokens->pNext->pNext->pNext) && (pTokens->pNext->pNext->pNext->uToken==TOK_GT)
       )
    {
      return NULL;
    }
    pTokens=pTokens->pNext;
  }

  if (!pTokens) return NULL;

  pTokens = pTokens->pNext;

  if (!pTokens) return NULL;

  for (i=0;i<iElemSpecifiers;i++)
  {
    if (!((pTokens) && (pTokens->uToken==TOK_LB)))
    {
      if (pTokens) pTokens = pTokens->pNext;
      goto NextElement;
    }
    pTokens = pTokens->pNext;

    if (pSpecs[i].uType==ELEMSPEC_STRING)
    {
      if (!((pTokens) && (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,pSpecs[i].szString))))
      {
        if (pTokens) pTokens = pTokens->pNext;
        goto NextElement;
      }
      pTokens=pTokens->pNext;
      if (!((pTokens) && (pTokens->uToken==TOK_RB)))
      {
        if (pTokens) pTokens = pTokens->pNext;
        goto NextElement;
      }
      pTokens = pTokens->pNext;
    }
    else
    if (pSpecs[i].uType==ELEMSPEC_ATOM)
    {
      if (!( (pTokens) && (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,"id")) &&
             (pTokens->pNext) && (pTokens->pNext->uToken==TOK_EQ) &&
             (pTokens->pNext->pNext) && (pTokens->pNext->pNext->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->pNext->pNext->byData,"atom")) &&
             (pTokens->pNext->pNext->pNext) && (pTokens->pNext->pNext->pNext->uToken==TOK_LP) &&
             (pTokens->pNext->pNext->pNext->pNext) && (pTokens->pNext->pNext->pNext->pNext->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->pNext->pNext->pNext->pNext->byData,pSpecs[i].szString)) &&
             (pTokens->pNext->pNext->pNext->pNext->pNext) && (pTokens->pNext->pNext->pNext->pNext->pNext->uToken==TOK_RP)
         ))
      {
        if (pTokens) pTokens = pTokens->pNext;
        goto NextElement;
      }
      pTokens=pTokens->pNext->pNext->pNext->pNext->pNext->pNext;

      if (!((pTokens) && (pTokens->uToken==TOK_RB)))
      {
        if (pTokens) pTokens = pTokens->pNext;
        goto NextElement;
      }
      pTokens = pTokens->pNext;
    }
    else
      return NULL;
  }

  if (!pTokens) return NULL;

  if (pTokens->uToken != TOK_LC)
  {
    pTokens = pTokens->pNext;
    goto NextElement;
  }

  pTokens = pTokens->pNext;

  if (!pTokens) return NULL;

  // Lookup property ('}' is end of element char)

  while (!( (pTokens) && (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,pcszProperty)) &&
            (pTokens->pNext) && (pTokens->pNext->uToken==TOK_COLON)
        ))
  {
    if ((pTokens) && (pTokens->uToken==TOK_RC))
      return NULL;
    if (pTokens) pTokens=pTokens->pNext;
  }

  if (!pTokens) return NULL;

  pTokens = pTokens->pNext;

  if (!pTokens) return NULL;

  pTokens = pTokens->pNext;

  return pTokens;
}

static BOOL AnalyseToken ( PTOKEN pTokens, PTOKDATA pTD, HMODULE hinstDLL )
{
  int                 iCmp,iLeft,iRight,iMiddle;
  char                szStringA[MAX_PATH];
  DWORD               dwColor;
  PTOKEN              pNewTokens;

  memset(pTD,0,sizeof(TOKDATA));
  if (!pTokens) return FALSE;

  if (pTokens->uToken==TOK_INTEGER)
  {
    pTD->uType  = TD_INTEGER;
    pTD->iValue = *((int*)pTokens->byData);
    pTokens = pTokens->pNext;
    if ((pTokens) && (pTokens->uToken==TOK_STRING))
    {
      if (!stricmp( (char *) pTokens->byData,"rp"))
        pTD->uType = TD_INTEGER_RP;
      else
      if (!stricmp( (char *) pTokens->byData,"pt"))
        pTD->uType = TD_INTEGER_PT;
    }
    return TRUE;
  }

  if (pTokens->uToken==TOK_HTMLCOLOR)
  {
    pTD->uType = TD_RGB;
    pTD->crRGB = XCHGCOLORREF(*((PDWORD)pTokens->byData));
    return TRUE;
  }

  // Check colors

  if (pTokens->uToken==TOK_STRING)
  {
    iLeft  = 0;
    iRight = NUM_DEFCOLORS-1;

    while (iLeft<=iRight)
    {
      iMiddle = (iLeft+iRight)>>1;
      if (!(iCmp=stricmp( (char *) pTokens->byData,sColors[iMiddle].szColor)))
      {
        pTD->uType = TD_RGB;
        pTD->crRGB = XCHGCOLORREF((DWORD)sColors[iMiddle].uColor);
        return TRUE;
      }
      if (iCmp<0)
        iRight = iMiddle-1;
      else
        iLeft  = iMiddle+1;
    }
  }

  // Check alignments

  if (pTokens->uToken==TOK_STRING)
  {
    iLeft  = 0;
    iRight = NUM_DEFALIGNS-1;

    while (iLeft<=iRight)
    {
      iMiddle = (iLeft+iRight)>>1;
      if (!(iCmp=stricmp( (char *) pTokens->byData,sAlignments[iMiddle].szAlign)))
      {
        pTD->uType  = TD_ALIGNMENT;
        pTD->uAlign = (DWORD)sAlignments[iMiddle].uAlign;
        return TRUE;
      }
      if (iCmp<0)
        iRight = iMiddle-1;
      else
        iLeft  = iMiddle+1;
    }
  }

  // Check cursors

  if (pTokens->uToken==TOK_STRING)
  {
    iLeft  = 0;
    iRight = NUM_DEFCURSORS-1;

    while (iLeft<=iRight)
    {
      iMiddle = (iLeft+iRight)>>1;
      if (!(iCmp=stricmp( (char *) pTokens->byData,sCursors[iMiddle].szCursor)))
      {
        pTD->uType        = TD_CURSOR;
        pTD->lpCursorName = sCursors[iMiddle].lpCursorName;
        return TRUE;
      }
      if (iCmp<0)
        iRight = iMiddle-1;
      else
        iLeft  = iMiddle+1;
    }
  }

  // Check fontstyles

  if (pTokens->uToken==TOK_STRING)
  {
    if (!stricmp( (char *) pTokens->byData,"underline"))
    {
      pTD->uType      = TD_FONTSTYLE;
      pTD->uFontStyle = TDFS_UNDERLINE;
      NextFS:
      pTokens = pTokens->pNext;
      if ( (pTokens) && (pTokens->uToken==TOK_COMMA) &&
           (pTokens->pNext) && (pTokens->pNext->uToken==TOK_STRING)
         )
      {
        pTokens = pTokens->pNext;
        if (!stricmp( (char *) pTokens->byData,"underline"))
        {
          pTD->uFontStyle |= TDFS_UNDERLINE;
          goto NextFS;
        }
        else
        if (!stricmp( (char *) pTokens->byData,"bold"))
        {
          pTD->uFontStyle |= TDFS_BOLD;
          goto NextFS;
        }
        else
        if (!stricmp( (char *) pTokens->byData,"normal"))
        {
          pTD->uFontStyle = TDFS_NORMAL;
          goto NextFS;
        }
        else
        if ((!stricmp( (char *) pTokens->byData,"italic")) || (!stricmp( (char *) pTokens->byData,"oblique")))
        {
          pTD->uFontStyle |= TDFS_ITALIC;
          goto NextFS;
        }
      }
      return TRUE;
    }

    if (!stricmp( (char *) pTokens->byData,"bold"))
    {
      pTD->uType      = TD_FONTSTYLE;
      pTD->uFontStyle = TDFS_BOLD;
      goto NextFS;
    }

    if (!stricmp( (char *) pTokens->byData,"normal"))
    {
      pTD->uType      = TD_FONTSTYLE;
      pTD->uFontStyle = TDFS_NORMAL;
      goto NextFS;
    }

    if ((!stricmp( (char *) pTokens->byData,"italic")) || (!stricmp( (char *) pTokens->byData,"oblique")))
    {
      pTD->uType      = TD_FONTSTYLE;
      pTD->uFontStyle = TDFS_ITALIC;
      goto NextFS;
    }
  }

  if ((pTokens->uToken==TOK_STRING) &&
      (pTokens->pNext) && (pTokens->pNext->uToken==TOK_LP) &&
      (pTokens->pNext->pNext) && (pTokens->pNext->pNext->uToken==TOK_INTEGER) &&
      (pTokens->pNext->pNext->pNext) && (pTokens->pNext->pNext->pNext->uToken==TOK_RP)
     )
  {
    if (!stricmp( (char *) pTokens->byData,"sysmetric"))
    {
      pTD->uType  = TD_SYSMETRIC;
      pTD->iValue = *((int*)pTokens->pNext->pNext->byData);
      return TRUE;
    }
    else
    if (!stricmp( (char *) pTokens->byData,"sysmetricstr"))
    {
      pTD->uType  = TD_SYSMETRICSTR;
      pTD->iValue = *((int*)pTokens->pNext->pNext->byData);
      return TRUE;
    }
    else
    if (!stricmp( (char *) pTokens->byData,"rcstr"))
    {
      pTD->uType = TD_STRING;
      LoadString(hinstDLL,*((int*)pTokens->pNext->pNext->byData),pTD->szString,MAX_PATH);
      return TRUE;
    }
    else
    if (!stricmp( (char *) pTokens->byData,"rcint"))
    {
      pTD->uType   = TD_INTEGER;
      szStringA[0] = 0x00;
      LoadStringA(hinstDLL,*((int*)pTokens->pNext->pNext->byData),szStringA,MAX_PATH);
      pTD->iValue  = atoi(szStringA);

      pTokens = pTokens->pNext->pNext->pNext->pNext;

      if ((pTokens) && (pTokens->uToken==TOK_STRING))
      {
        if (!stricmp( (char *) pTokens->byData,"pt"))
          pTD->uType = TD_INTEGER_PT;
        else
        if (!stricmp( (char *) pTokens->byData,"rp"))
          pTD->uType = TD_INTEGER_RP;
      }

      return TRUE;
    }
  }

  if ( (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,"rect")) )
  {
    pTD->uType = TD_RECT;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_LP)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get left

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->rect.uIntTypes[0] = TD_INTEGER;
    pTD->rect.rcRect.left  = *((int*)pTokens->byData);
    pTokens = pTokens->pNext;
    if ((pTokens) && (pTokens->uToken==TOK_STRING))
    {
      if (!stricmp( (char *) pTokens->byData,"rp"))
      {
        pTD->rect.uIntTypes[0] = TD_INTEGER_RP;
        pTokens = pTokens->pNext;
      }
      else
      if (!stricmp( (char *) pTokens->byData,"pt"))
      {
        pTD->rect.uIntTypes[0] = TD_INTEGER_PT;
        pTokens = pTokens->pNext;
      }
      else
        return TRUE;
    }

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get top

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->rect.uIntTypes[1] = TD_INTEGER;
    pTD->rect.rcRect.top  = *((int*)pTokens->byData);
    pTokens = pTokens->pNext;
    if ((pTokens) && (pTokens->uToken==TOK_STRING))
    {
      if (!stricmp( (char *) pTokens->byData,"rp"))
      {
        pTD->rect.uIntTypes[1] = TD_INTEGER_RP;
        pTokens = pTokens->pNext;
      }
      else
      if (!stricmp( (char *) pTokens->byData,"pt"))
      {
        pTD->rect.uIntTypes[1] = TD_INTEGER_PT;
        pTokens = pTokens->pNext;
      }
      else
        return TRUE;
    }

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get right

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->rect.uIntTypes[2] = TD_INTEGER;
    pTD->rect.rcRect.right  = *((int*)pTokens->byData);
    pTokens = pTokens->pNext;
    if ((pTokens) && (pTokens->uToken==TOK_STRING))
    {
      if (!stricmp( (char *) pTokens->byData,"rp"))
      {
        pTD->rect.uIntTypes[2] = TD_INTEGER_RP;
        pTokens = pTokens->pNext;
      }
      else
      if (!stricmp( (char *) pTokens->byData,"pt"))
      {
        pTD->rect.uIntTypes[2] = TD_INTEGER_PT;
        pTokens = pTokens->pNext;
      }
      else
        return TRUE;
    }

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get bottom

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->rect.uIntTypes[3] = TD_INTEGER;
    pTD->rect.rcRect.bottom  = *((int*)pTokens->byData);
    pTokens = pTokens->pNext;
    if ((pTokens) && (pTokens->uToken==TOK_STRING))
    {
      if (!stricmp( (char *) pTokens->byData,"rp"))
      {
        pTD->rect.uIntTypes[3] = TD_INTEGER_RP;
        pTokens = pTokens->pNext;
      }
      else
      if (!stricmp( (char *) pTokens->byData,"pt"))
      {
        pTD->rect.uIntTypes[3] = TD_INTEGER_PT;
        pTokens = pTokens->pNext;
      }
      else
        return TRUE;
    }

    return TRUE;
  }

  if ( (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,"rgb")) )
  {
    pTD->uType = TD_RGB;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_LP)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get red

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->crRGB |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<16;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get green

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->crRGB |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<8;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get blue

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->crRGB |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL);

    pTD->crRGB = XCHGCOLORREF(pTD->crRGB);
    return TRUE;
  }

  if ( (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,"argb")) )
  {
    pTD->uType = TD_ARGB;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_LP)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get alpha

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->crARGB |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<24;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get red

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->crARGB |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<16;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get green

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->crARGB |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<8;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    // get blue

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;
    pTD->crARGB |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL);

    pTD->crARGB = XCHGCOLORREF(pTD->crARGB);
    return TRUE;
  }

  if ( (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,"rcbmp")) )
  {
    pTD->uType = TD_BITMAP;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_LP)) )
    {
      ErrorExit:
      memset(pTD,0,sizeof(TOKDATA));
      pTD->uType = TD_ERROR;
      return FALSE;
    }
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      goto ErrorExit;

    pTD->bitmap.hBitmap = (HBITMAP) LoadImage(hinstDLL,
                                    MAKEINTRESOURCE((*((int*)pTokens->byData))),
                                    IMAGE_BITMAP,
                                    0,0,
                                    LR_CREATEDIBSECTION);
    if (!pTD->bitmap.hBitmap)
      goto ErrorExit;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
    {
      ErrorExit2:
      DeleteObject(pTD->bitmap.hBitmap);
      goto ErrorExit;
    }
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      goto ErrorExit2;
    pTD->bitmap.biCompression = (*((int*)pTokens->byData))-1;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      goto ErrorExit2;

    pTokens = pTokens->pNext;

    pNewTokens = GetColor(pTokens,&dwColor,&pTD->bitmap.bColorIdx);
    if (!pNewTokens) goto ErrorExit2;
    pTD->bitmap.crTransRGB = dwColor;
    pTokens = pNewTokens;

    // ignore ",left,top,right,bottom)"

    return TRUE;
  }

  if ( (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,"gradient")) )
  {
    pTD->uType = TD_GRADIENT;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_LP)) )
      return TRUE;
    pTokens = pTokens->pNext;

    pNewTokens = GetColor(pTokens,&dwColor,NULL);
    if (!pNewTokens) return TRUE;
    pTD->gradient.crARGB1 = dwColor;
    pTokens = pNewTokens;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    pNewTokens = GetColor(pTokens,&dwColor,NULL);
    if (!pNewTokens) return TRUE;
    pTD->gradient.crARGB2 = dwColor;
    pTokens = pNewTokens;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return TRUE;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return TRUE;

    pTD->gradient.iValue = *((int*)pTokens->byData);
    if (pTD->gradient.iValue<0)
      pTD->gradient.iValue = 0;

    return TRUE;
  }

  memset(pTD,0,sizeof(TOKDATA));
  pTD->uType = TD_UNKNOWN;
  return FALSE;
}

static HBITMAP Bitmap2AlphaBitmap ( HBITMAP hBitmap,
                                               DWORD   dwTransColor,
                                               BOOL    bColorIdx,
                                               int    *piWidth,
                                               int    *piHeight )
{
  DIBSECTION          ds;
  HBITMAP             hAlphaBitmap = NULL;
  PDWORD              pdwImage;
  DWORD               dwColor;
  BYTE                byColor,byRed,byGreen,byBlue;
  int                 i,j;
  static DWORD        dwColorLUT[256];
  HDC                 hDC,hDCmem;
  HGDIOBJ             hOldObj;
  BITMAPINFO          bif;
  BITMAPINFOHEADER    bih;
  LPVOID              pvBits;

  if (!(sizeof(DIBSECTION)==GetObject(hBitmap,sizeof(DIBSECTION),(LPVOID)&ds)))
  {
    DeleteObject(hBitmap);
    return hAlphaBitmap;
  }

  if (piWidth) *piWidth   = ds.dsBm.bmWidth;
  if (piHeight) *piHeight = ds.dsBm.bmHeight;

  if (bColorIdx)
  {
    hAlphaBitmap = hBitmap;

    if (ds.dsBmih.biBitCount==32) // calulate premultiplied alpha channels
    {
      for (i=0;i<ds.dsBm.bmHeight;i++)
      {
        for (j=0;j<ds.dsBm.bmWidth;j++)
        {
          dwColor = *(((PDWORD)ds.dsBm.bmBits)+i*ds.dsBm.bmWidth+j);

          // AARRGGBB

          byColor = (BYTE)(dwColor>>24);
          byRed   = (BYTE)(dwColor>>16);
          byGreen = (BYTE)(dwColor>>8);
          byBlue  = (BYTE)(dwColor>>0);

          byRed   = (BYTE) (((DWORD)byRed)*((DWORD)byColor)/255);
          byGreen = (BYTE) (((DWORD)byGreen)*((DWORD)byColor)/255);
          byBlue  = (BYTE) (((DWORD)byBlue)*((DWORD)byColor)/255);

          dwColor = (((DWORD)byColor)<<24)|
                    (((DWORD)byRed)<<16)|
                    (((DWORD)byGreen)<<8)|
                    (((DWORD)byBlue)<<0);

          *(((PDWORD)ds.dsBm.bmBits)+i*ds.dsBm.bmWidth+j) = dwColor;

        }
      }

    }
  }
  else
  {
    if ((ds.dsBmih.biBitCount==8) && (ds.dsBmih.biCompression==BI_RGB))
    {
      pdwImage = (PDWORD)malloc((ds.dsBm.bmWidth<<2)*ds.dsBm.bmHeight);
      if (!pdwImage)
      {
        DeleteObject(hBitmap);
        return hAlphaBitmap;
      }

      memset(dwColorLUT,0,sizeof(dwColorLUT));
      hDC = GetDC(NULL);
      hDCmem = CreateCompatibleDC(hDC);
      hOldObj = SelectObject(hDCmem,hBitmap);
      GetDIBColorTable(hDCmem,0,256,(RGBQUAD*)dwColorLUT);
      SelectObject(hDCmem,hOldObj);
      DeleteDC(hDCmem);

      if (bColorIdx)
        dwTransColor = dwColorLUT[dwTransColor&255];
      else
        dwTransColor = XCHGCOLORREF(dwTransColor);

      dwTransColor &= 0x00FFFFFF;

      for (i=0;i<ds.dsBm.bmHeight;i++)
      {
        for (j=0;j<ds.dsBm.bmWidth;j++)
        {
          byColor = *(((PBYTE)ds.dsBm.bmBits)+i*ds.dsBm.bmWidthBytes+j);

          dwColor = dwColorLUT[byColor];

          if ((dwColor & 0x00FFFFFFL)==dwTransColor)
            pdwImage[i*ds.dsBm.bmWidth+j] = 0L;
          else
            pdwImage[i*ds.dsBm.bmWidth+j] = dwColor|0xFF000000L;
        }
      }

      memset(&bif,0,sizeof(BITMAPINFO));
      memset(&bih,0,sizeof(BITMAPINFOHEADER));
      bih.biSize        = sizeof(BITMAPINFOHEADER);
      bih.biWidth       = ds.dsBm.bmWidth;
      bih.biHeight      = ds.dsBm.bmHeight;
      bih.biPlanes      = 1;
      bih.biBitCount    = 32;
      bih.biCompression = BI_RGB;
      memcpy(&bif.bmiHeader,&bih,sizeof(BITMAPINFOHEADER));

      hAlphaBitmap = CreateDIBSection(hDC,&bif,DIB_RGB_COLORS,&pvBits,NULL,0);
      if (hAlphaBitmap)
      {
        memcpy(pvBits,pdwImage,(ds.dsBm.bmWidth*ds.dsBm.bmHeight)<<2);
      }

      ReleaseDC(NULL,hDC);
      DeleteObject(hBitmap);
      free(pdwImage);
    }
    else
    if ((ds.dsBmih.biBitCount==24) && (ds.dsBmih.biCompression==BI_RGB))
    {
      pdwImage = (PDWORD)malloc((ds.dsBm.bmWidth<<2)*ds.dsBm.bmHeight);
      if (!pdwImage)
      {
        DeleteObject(hBitmap);
        return hAlphaBitmap;
      }

      if (bColorIdx)
        dwTransColor = 0xFFFFFFFFL;
      else
        dwTransColor &= 0x00FFFFFF;

      dwTransColor = XCHGCOLORREF(dwTransColor);

      for (i=0;i<ds.dsBm.bmHeight;i++)
      {
        for (j=0;j<ds.dsBm.bmWidth;j++)
        {
          byBlue  = *(((PBYTE)ds.dsBm.bmBits)+i*ds.dsBm.bmWidthBytes+j*3+0);
          byGreen = *(((PBYTE)ds.dsBm.bmBits)+i*ds.dsBm.bmWidthBytes+j*3+1);
          byRed   = *(((PBYTE)ds.dsBm.bmBits)+i*ds.dsBm.bmWidthBytes+j*3+2);

          dwColor = (((DWORD)byRed)<<16)|(((DWORD)byGreen)<<8)|(((DWORD)byBlue)<<0);

          if (dwColor==dwTransColor)
            pdwImage[i*ds.dsBm.bmWidth+j] = 0L;
          else
            pdwImage[i*ds.dsBm.bmWidth+j] = dwColor|0xFF000000L;
        }
      }

      memset(&bif,0,sizeof(BITMAPINFO));
      memset(&bih,0,sizeof(BITMAPINFOHEADER));
      bih.biSize        = sizeof(BITMAPINFOHEADER);
      bih.biWidth       = ds.dsBm.bmWidth;
      bih.biHeight      = ds.dsBm.bmHeight;
      bih.biPlanes      = 1;
      bih.biBitCount    = 32;
      bih.biCompression = BI_RGB;
      memcpy(&bif.bmiHeader,&bih,sizeof(BITMAPINFOHEADER));
      hDC = GetDC(NULL);
      hAlphaBitmap = CreateDIBSection(hDC,&bif,DIB_RGB_COLORS,&pvBits,NULL,0);
      if (hAlphaBitmap)
      {
        memcpy(pvBits,pdwImage,(ds.dsBm.bmWidth*ds.dsBm.bmHeight)<<2);
      }
      ReleaseDC(NULL,hDC);
      DeleteObject(hBitmap);
      free(pdwImage);
    }
    else
      hAlphaBitmap = hBitmap;
  }
  return hAlphaBitmap;
}

static HBITMAP LoadAlphaBitmap ( HMODULE   hInstDll,
                                            UINT      uBmpId,
                                            COLORREF  crReplaceCol,
                                            COLORREF  crReplaceBy,
                                            COLORREF  crTransparent,
                                            int      *piWidth,
                                            int      *piHeight )
{
  HBITMAP             hBitmap;
  DIBSECTION          ds;
  HBITMAP             hAlphaBitmap = NULL;
  PDWORD              pdwImage;
  DWORD               dwColor;
  BYTE                byRed,byGreen,byBlue;
  int                 i,j;
  HDC                 hDC;
  BITMAPINFO          bif;
  BITMAPINFOHEADER    bih;
  LPVOID              pvBits;

  crReplaceCol  = XCHGCOLORREF(crReplaceCol);
  crReplaceBy   = XCHGCOLORREF(crReplaceBy);
  crTransparent = XCHGCOLORREF(crTransparent);

  hBitmap = (HBITMAP) LoadImage(hInstDll,
                      MAKEINTRESOURCE(uBmpId),
                      IMAGE_BITMAP,
                      0,0,
                      LR_CREATEDIBSECTION);
  if (!hBitmap) return NULL;

  if (!(sizeof(DIBSECTION)==GetObject(hBitmap,sizeof(DIBSECTION),(LPVOID)&ds)))
  {
    DeleteObject(hBitmap);
    return hAlphaBitmap;
  }

  if (piWidth) *piWidth   = ds.dsBm.bmWidth;
  if (piHeight) *piHeight = ds.dsBm.bmHeight;

  if ((ds.dsBmih.biBitCount==24) && (ds.dsBmih.biCompression==BI_RGB))
  {
    pdwImage = (PDWORD)malloc((ds.dsBm.bmWidth<<2)*ds.dsBm.bmHeight);
    if (!pdwImage)
    {
      DeleteObject(hBitmap);
      return hAlphaBitmap;
    }

    for (i=0;i<ds.dsBm.bmHeight;i++)
    {
      for (j=0;j<ds.dsBm.bmWidth;j++)
      {
        byBlue  = *(((PBYTE)ds.dsBm.bmBits)+i*ds.dsBm.bmWidthBytes+j*3+0);
        byGreen = *(((PBYTE)ds.dsBm.bmBits)+i*ds.dsBm.bmWidthBytes+j*3+1);
        byRed   = *(((PBYTE)ds.dsBm.bmBits)+i*ds.dsBm.bmWidthBytes+j*3+2);

        dwColor = (((DWORD)byRed)<<16)|(((DWORD)byGreen)<<8)|(((DWORD)byBlue)<<0);

        if (dwColor==crTransparent)
          pdwImage[i*ds.dsBm.bmWidth+j] = 0L;
        else
        {
          if (dwColor==crReplaceCol)
            pdwImage[i*ds.dsBm.bmWidth+j] = crReplaceBy|0xFF000000L;
          else
            pdwImage[i*ds.dsBm.bmWidth+j] = dwColor|0xFF000000L;
        }
      }
    }

    memset(&bif,0,sizeof(BITMAPINFO));
    memset(&bih,0,sizeof(BITMAPINFOHEADER));
    bih.biSize        = sizeof(BITMAPINFOHEADER);
    bih.biWidth       = ds.dsBm.bmWidth;
    bih.biHeight      = ds.dsBm.bmHeight;
    bih.biPlanes      = 1;
    bih.biBitCount    = 32;
    bih.biCompression = BI_RGB;
    memcpy(&bif.bmiHeader,&bih,sizeof(BITMAPINFOHEADER));
    hDC = GetDC(NULL);
    hAlphaBitmap = CreateDIBSection(hDC,&bif,DIB_RGB_COLORS,&pvBits,NULL,0);
    if (hAlphaBitmap)
    {
      memcpy(pvBits,pdwImage,(ds.dsBm.bmWidth*ds.dsBm.bmHeight)<<2);
    }
    ReleaseDC(NULL,hDC);
    DeleteObject(hBitmap);
    free(pdwImage);
  }
  else
    DeleteObject(hBitmap);

  return hAlphaBitmap;
}

static PTOKEN GetColor ( PTOKEN pTokens, DWORD *pdwColor, BOOL *pbIndex )
{
  if (pbIndex) *pbIndex = FALSE;

  if (!pTokens) return NULL;

  if (pTokens->uToken==TOK_INTEGER)
  {
    *pdwColor = (DWORD) (*((int*)pTokens->byData));
    if (pbIndex) *pbIndex = TRUE;
    return pTokens->pNext;
  }

  if (pTokens->uToken==TOK_HTMLCOLOR)
  {
    *pdwColor = XCHGCOLORREF((*((PDWORD)pTokens->byData)));
    return pTokens->pNext;
  }

  if ( (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,"rgb")) )
  {
    *pdwColor = 0L;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_LP)) )
      return NULL;
    pTokens = pTokens->pNext;

    // get red

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return NULL;
    (*pdwColor) |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<16;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return NULL;
    pTokens = pTokens->pNext;

    // get green

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return NULL;
    (*pdwColor) |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<8;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return NULL;
    pTokens = pTokens->pNext;

    // get blue

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return NULL;
    (*pdwColor) |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<0;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_RP)) )
      return NULL;

    *pdwColor = XCHGCOLORREF((*pdwColor));

    return pTokens->pNext;
  }

  if ( (pTokens->uToken==TOK_STRING) && (!stricmp( (char *) pTokens->byData,"argb")) )
  {
    *pdwColor = 0L;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_LP)) )
      return NULL;
    pTokens = pTokens->pNext;

    // get alpha

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return NULL;
    (*pdwColor) |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<24;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return NULL;
    pTokens = pTokens->pNext;

    // get red

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return NULL;
    (*pdwColor) |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<16;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return NULL;
    pTokens = pTokens->pNext;

    // get green

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return NULL;
    (*pdwColor) |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<8;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_COMMA)) )
      return NULL;
    pTokens = pTokens->pNext;

    // get blue

    if (! ((pTokens) && (pTokens->uToken==TOK_INTEGER)) )
      return NULL;
    (*pdwColor) |= (((DWORD)(*((int*)pTokens->byData)))&0xFFL)<<0;
    pTokens = pTokens->pNext;

    if (! ((pTokens) && (pTokens->uToken==TOK_RP)) )
      return NULL;

    *pdwColor = XCHGCOLORREF((*pdwColor));

    return pTokens->pNext;
  }

  return NULL;
}

BOOL APIENTRY GetLogFont ( HFONT hFont, PLOGFONT pLF )
{
   HDC             hDC = GetDC(NULL);
   HGDIOBJ         hOldObj;
   TEXTMETRIC      tm;

   memset(pLF,0,sizeof(LOGFONT));
   if (!hDC) return FALSE;

   if (!hFont)
   {
      ReleaseDC(NULL,hDC);
      return FALSE;
   }

   hOldObj = SelectObject(hDC,hFont);
   if (!GetTextMetrics(hDC,&tm))
   {
      SelectObject(hDC,hOldObj);
      ReleaseDC(NULL,hDC);
      return FALSE;
   }

   pLF->lfHeight         = -tm.tmAscent;
   pLF->lfWeight         = tm.tmWeight;
   pLF->lfItalic         = tm.tmItalic;
   pLF->lfCharSet        = tm.tmCharSet;
   pLF->lfQuality        = DEFAULT_QUALITY;
   pLF->lfPitchAndFamily = tm.tmPitchAndFamily;
   GetTextFace(hDC,sizeof(pLF->lfFaceName),pLF->lfFaceName);

   SelectObject(hDC,hOldObj);
   ReleaseDC(NULL,hDC);

   return TRUE;
}

static void APIPRIVATE AlphaBlendHeaderBitmap ( HDC hDC, HDC hDCMem, HBITMAP hBitmap, LPRECT lpRC, int iBmpWidth, int iBmpHeight )
{
   BLENDFUNCTION           bf;
   HBITMAP                 hOldBitmap;

   bf.BlendOp             = AC_SRC_OVER;
   bf.BlendFlags          = 0;
   bf.SourceConstantAlpha = 255;
   bf.AlphaFormat         = AC_SRC_ALPHA;

   hOldBitmap = (HBITMAP) SelectObject( hDCMem, hBitmap );

   AlphaBlend( hDC, lpRC->left,    lpRC->top,                        2, lpRC->bottom-lpRC->top, hDCMem,           0, 0,           2, iBmpHeight, bf );
   AlphaBlend( hDC, lpRC->right-2, lpRC->top,                        2, lpRC->bottom-lpRC->top, hDCMem, iBmpWidth-2, 0,           2, iBmpHeight, bf );
   AlphaBlend( hDC, lpRC->left+2,  lpRC->top, lpRC->right-lpRC->left-4, lpRC->bottom-lpRC->top, hDCMem,           2, 0, iBmpWidth-4, iBmpHeight, bf );

   SelectObject( hDCMem, hOldBitmap );
}

static EXPBARINFO p_EBI;

HB_FUNC( GETEXPLORERBARINFO )
{
   g_hInstDLL = GetModuleHandle(NULL);
   hUxTheme = LoadLibraryEx( "uxtheme.dll",NULL,NULL);
   p_EBI.cbSize = sizeof(EXPBARINFO);
   GetExplorerBarInfo( &p_EBI, FALSE );
}

HB_FUNC( EXPLORERBARINFO )
{
   hb_storclen( (char *) &p_EBI, sizeof(EXPBARINFO), 1 );
}

HB_FUNC( FREEEXPLORERBARINFO )
{
   FreeExplorerBarInfo( &p_EBI);
   if( !hUxTheme == NULL ){
      FreeLibrary( hUxTheme );
   }
}

HB_FUNC( ALPHABLENDHEADERBITMAP )
{
   RECT rc;
   Array2Rect( hb_param( 4, HB_IT_ARRAY ), &rc );
   AlphaBlendHeaderBitmap( (HDC) hb_parnl(1), (HDC) hb_parnl(2), (HBITMAP) hb_parnl(3), &rc, (int) hb_parni(5), (int) hb_parni(6) );
}

HB_FUNC( CHGCOLORREF )
{
   hb_retnl( (long) XCHGCOLORREF( hb_parnl(1) ) );
}

HB_FUNC( FROMARGB )
{
   hb_retnl( (long) MAKEARGB( hb_parni(1), hb_parni(4), hb_parni(3), hb_parni(2) ) );
}
