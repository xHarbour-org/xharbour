/*
 * $Id: hbgtwvw.h,v 1.5 2005/02/24 10:44:02 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 using GUI windows instead of Console
 * WITH MULTIPLE WINDOW SUPPORT
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 *
 * initially based on:
 *
 * Header File for Video subsystem for Win32 using GUI windows instead of Console
 * Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                Rees Software & Systems Ltd
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_WVW_H_
#define HB_WVW_H_

/*-------------------------------------------------------------------*/

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */

/*-------------------------------------------------------------------*/

#define HB_GT_NAME  WVW

/*-------------------------------------------------------------------*/

#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

#include <math.h>       /* fmod */
#include <windows.h>
#include <winuser.h>
#include <commctrl.h>
#include <commdlg.h>

#if defined(__MINGW32__) || defined(__WATCOMC__) || defined(_MSC_VER) || defined(__DMC__)
   #include <unknwn.h>
   #include <ole2.h>
   #include <ocidl.h>
   #include <olectl.h>

   #if defined(_MSC_VER) || defined( __DMC__ )
      #include <conio.h>

      #if !defined( __POCC__ )
         #if !defined( LONG_PTR )
            typedef __int64 LONG_PTR ;
            #ifndef SetWindowLongPtr
               #define SetWindowLongPtr SetWindowLong
            #endif
            #ifndef GetWindowLongPtr
               #define GetWindowLongPtr GetWindowLong
            #endif
         #endif
      #endif
   #endif
#else
   #include <olectl.h>
#endif

#include <time.h>
#include <ctype.h>

#include "hbset.h"
#include "hbapigt.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "inkey.ch"
#include "error.ch"
#include "hbvm.h"

/*-------------------------------------------------------------------*/

#define WVW_MAXWINDOWS        20

#define WVW_MAXWINNAMELENGTH  256

#define WVW_DEFAULT_MENUKEYEVENT 1024

#define WVW_MAX_STATUS_PARTS    40      /* max # of parts in Status Bar */
#define WVW_SPACE_BETWEEN_PARTS  2      /* pixel space between Status Bar's parts */

#define WVW_ID_SYSTEM_TIMER      1

/* 20040802 old settings:
#define WVW_ID_BASE_STATUSBAR  100
#define WVW_ID_BASE_TIMER      200

#define WVW_ID_BASE_TOOLTIP    300

#define WVW_ID_BASE_TOOLBAR    400

#define WVW_ID_BASE_SCROLLBAR  500

#define WVW_ID_BASE_PUSHBUTTON 600
*/

#define WVW_ID_BASE_STATUSBAR  100
#define WVW_ID_BASE_TIMER      100

#define WVW_ID_BASE_TOOLTIP    100

#define WVW_ID_BASE_TOOLBAR    100

#define WVW_ID_BASE_SCROLLBAR  100

#define WVW_ID_BASE_PROGRESSBAR 100

#define WVW_ID_BASE_PUSHBUTTON 64000

#define WVW_ID_MAX_PUSHBUTTON  WVW_ID_BASE_PUSHBUTTON+200-1
/* ie. effectively there are max 200 buttons on a window */

#define WVW_ID_BASE_COMBOBOX   WVW_ID_MAX_PUSHBUTTON+1
#define WVW_CB_KBD_STANDARD  0
#define WVW_CB_KBD_CLIPPER   1

#define WVW_COMBOBOX_MAXLEN  255  /* maximum length of combobox string */

#define WVW_CHAR_QUEUE_SIZE  128
#define WVW_CHAR_BUFFER     1024
#define WVW_MAX_ROWS         256
#define WVW_MAX_COLS         256
#define WVW_DEFAULT_ROWS      25
#define WVW_DEFAULT_COLS      80

#define WVW_PICTURES_MAX      20

#define WVW_FONTS_MAX         20
#define WVW_PENS_MAX          20
#define WVW_DLGML_MAX         20
#define WVW_DLGMD_MAX         20

#define WVW_TB_LABELMAXLENGTH 40

#define WVW_WHICH_WINDOW ( ISNIL( 1 ) ? ( s_bMainCoordMode ? s_usNumWindows-1 : s_usCurWindow ) : ((USHORT) hb_parni( 1 )) )

/*-------------------------------------------------------------------*/

#define BLACK          RGB( 0x0 ,0x0 ,0x0  )
#define BLUE           RGB( 0x0 ,0x0 ,0x85 )
#define GREEN          RGB( 0x0 ,0x85,0x0  )
#define CYAN           RGB( 0x0 ,0x85,0x85 )
#define RED            RGB( 0x85,0x0 ,0x0  )
#define MAGENTA        RGB( 0x85,0x0 ,0x85 )
#define BROWN          RGB( 0x85,0x85,0x0  )
#define WHITE          RGB( 0xC6,0xC6,0xC6 )
#define LIGHT_GRAY     RGB( 0x60,0x60,0x60 )
#define BRIGHT_BLUE    RGB( 0x00,0x00,0xFF )
#define BRIGHT_GREEN   RGB( 0x60,0xFF,0x60 )
#define BRIGHT_CYAN    RGB( 0x60,0xFF,0xFF )
#define BRIGHT_RED     RGB( 0xF8,0x00,0x26 )
#define BRIGHT_MAGENTA RGB( 0xFF,0x60,0xFF )
#define YELLOW         RGB( 0xFF,0xFF,0x00 )
#define BRIGHT_WHITE   RGB( 0xFF,0xFF,0xFF )

/*-------------------------------------------------------------------*/
#if defined(__DMC__)

   #define SBT_TOOLTIPS             0x0800
   #define SB_SETICON              (WM_USER+15)
   #define SB_SETTIPTEXT           (WM_USER+17)
   #define SB_GETTIPTEXT           (WM_USER+18)
   #define TBSTYLE_FLAT            0x0800
   #define TBSTYLE_LIST            0x1000
   #define TBSTYLE_CUSTOMERASE     0x2000
   #define IDB_HIST_SMALL_COLOR    8
   #define IDB_HIST_LARGE_COLOR    9
   #define TB_SETMAXTEXTROWS       (WM_USER + 60)
   #define PBS_VERTICAL            0x04
   #define PBS_SMOOTH              0x01
   #define CCM_FIRST               0x2000      // Common control shared messages
   #define CCM_SETBKCOLOR          (CCM_FIRST + 1) // lParam is bkColor
   #define PBM_SETBKCOLOR          CCM_SETBKCOLOR  // lParam = bkColor
   #define PBM_SETBARCOLOR         (WM_USER+9)		// lParam = bar color
   #define PBM_GETRANGE            (WM_USER+7)  // wParam = return (TRUE ? low : high). lParam = PPBRANGE or NULL
   #define PBM_GETPOS              (WM_USER+8)

   typedef DWORD UINT_PTR;

   typedef struct
   {
      int iLow;
      int iHigh;
   } PBRANGE, *PPBRANGE;

   #define ICC_BAR_CLASSES      0x00000004

   typedef USHORT COLOR16;

   typedef struct _TRIVERTEX {
         LONG    x;
         LONG    y;
         COLOR16 Red;
         COLOR16 Green;
         COLOR16 Blue;
         COLOR16 Alpha;
   } TRIVERTEX,*PTRIVERTEX,*LPTRIVERTEX;

   typedef struct tagINITCOMMONCONTROLSEX {
          DWORD dwSize;             // size of this structure
          DWORD dwICC;              // flags indicating which classes to be initialized
   } INITCOMMONCONTROLSEX, *LPINITCOMMONCONTROLSEX;

   WINCOMMCTRLAPI BOOL WINAPI InitCommonControlsEx(LPINITCOMMONCONTROLSEX);

   typedef struct _GRADIENT_RECT {
       ULONG UpperLeft;
       ULONG LowerRight;
   } GRADIENT_RECT,*PGRADIENT_RECT,*LPGRADIENT_RECT;
#endif
/*-------------------------------------------------------------------*/

#define WM_MY_UPDATE_CARET ( WM_USER + 0x0101 )

typedef BOOL ( WINAPI *wvwGradientFill )     (
                      HDC        hdc,
                      PTRIVERTEX pVertex,
                      ULONG      dwNumVertex,
                      PVOID      pMesh,
                      ULONG      dwNumMesh,
                      ULONG      dwMode      );

/*-------------------------------------------------------------------*/

#ifndef _MAX_PATH
   #define _MAX_PATH 256
#endif

typedef struct bitmap_handle
{
  char    szFilename[_MAX_PATH+1];
  HBITMAP hBitmap;
  int     iWidth, iHeight;
  struct bitmap_handle * pNext;
} BITMAP_HANDLE;

/*-------------------------------------------------------------------*/

#define WVW_CONTROL_SCROLLBAR  1
#define WVW_CONTROL_PUSHBUTTON 2
#define WVW_CONTROL_PROGRESSBAR  3
#define WVW_CONTROL_COMBOBOX   4

#define WVW_MAXCAPTIONLENGTH  80

typedef struct control_data
{
  BYTE    byCtrlClass;
  HWND    hWndCtrl;
  UINT    uiCtrlid;
  HB_ITEM hiCodeBlock;
  BOOL    bBusy;
  RECT    rCtrl, rOffCtrl;

  /* SCROLLBAR specifics: */
  /* also used by combobox to store kbd type */
  byte    bStyle;

  /* PUSHBUTTON specifics: */
  WNDPROC   OldProc;

  struct control_data * pNext;
} CONTROL_DATA;

/*-------------------------------------------------------------------*/

typedef struct app_data
{
  BOOL      CaretExist;                /* TRUE if a caret has been created                */
  BOOL      displayCaret;              /* flag to indicate if caret is on                 */

  BOOL      Win9X;                     /* Flag to say if running on Win9X not NT/2000/XP  */
  BOOL      AltF4Close;                /* Can use Alt+F4 to close application             */

  HPEN      penWhite;                  /* White pen to draw GDI elements */
  HPEN      penBlack;                  /* Black pen to draw GDI elements */
  HPEN      penWhiteDim;               /* White dim pen to draw GDI elements */
  HPEN      penDarkGray;               /* Dark gray pen to draw GDI elements */
  HPEN      penGray;                   /* Gray pen equivalent to Clipper White */
  HPEN      penNull;                   /* Null pen */
  HPEN      currentPen;                /* Handle to current pen settable at runtime */
  HBRUSH    currentBrush;              /* Handle to current brush settable by runtime */
  HBRUSH    diagonalBrush;             /* Handle to diaoganl brush to draw scrollbars */
  HBRUSH    solidBrush;                /* Handle to solid brush */
  HBRUSH    wvwWhiteBrush;             /* Wvw specific White colored brush */

  IPicture  *iPicture[ WVW_PICTURES_MAX ]; /* Array to hold the Picture Streams to avoid recurring loading and unloading */
  HFONT     hUserFonts[ WVW_FONTS_MAX ] ;  /* User defined font handles */
  HPEN      hUserPens[ WVW_PENS_MAX ];     /* User defined pens */

  HINSTANCE hMSImg32;                  /* Handle to the loaded library msimg32.dll */
  wvwGradientFill pfnGF;               /* Pointer to Address of the GradientFill function in MSImg32.dll */

  HWND      hDlgModeless[ WVW_DLGML_MAX ];        /* Handle to a modeless dialog                               */

  PHB_ITEM  pFunc[ WVW_DLGML_MAX ];               /* Function pointer for WndProc                              */
  HB_ITEM   cbFunc[ WVW_DLGML_MAX ];              /* codeblock for WndProc */
  int       iType[ WVW_DLGML_MAX ];               /* Type of Function Pointers - Function 1, Block 2, Method 3 */

  HWND      hDlgModal[ WVW_DLGMD_MAX ];        /* Handle to a modal dialog                               */
  PHB_ITEM  pFuncModal[ WVW_DLGMD_MAX ];               /* Function pointer for WndProc                              */
  HB_ITEM   cbFuncModal[ WVW_DLGMD_MAX ];              /* codeblock for WndProc */
  int       iTypeModal[ WVW_DLGMD_MAX ];               /* Type of Function Pointers - Function 1, Block 2, Method 3 */

  BITMAP_HANDLE * pbhBitmapList;

  PHB_DYNS  pSymWVW_PAINT;             /* Stores pointer to WVW_PAINT function     */
  PHB_DYNS  pSymWVW_SETFOCUS;          /* Stores pointer to WVW_SETFOCUS function  */
  PHB_DYNS  pSymWVW_KILLFOCUS;         /* Stores pointer to WVW_KILLFOCUS function */
  PHB_DYNS  pSymWVW_MOUSE;             /* Stores pointer to WVW_MOUSE function     */
  PHB_DYNS  pSymWVW_MENUSELECT;        /* Stores pointer to WVW_MENUSELECT function*/

  PHB_DYNS  pSymWVW_INPUTFOCUS;        /* Stores pointer to WVW_INPUTFOCUS function*/

  PHB_DYNS  pSymWVW_TIMER;             /* Stores pointer to WVW_TIMER function     */

} APP_DATA;

typedef struct win_data
{
  BYTE      byWinId;                   /*x Window's Id, a number 0..WVWMAXWINDOWS            */
  TCHAR     szWinName[ WVW_MAXWINNAMELENGTH ];  /*x name of Window ~ szAppName for Window 0  */

  BYTE      byLineSpacing;             /*x linespacing in pixels */
  int       iLSpaceColor;              /*x linespacing color index */

  USHORT    usRowOfs;                  /*x offset to Main Window's (0,0)                     */
  USHORT    usColOfs;                  /*x offset to Main Window's (0,0)                     */
  USHORT    uiDispCount;               /*x pending DispEnd() request                         */
  BOOL      bPaintPending;             /*x pending WVW_PAINT() execution                     */
  RECT      rPaintPending;             /*x rect of pending bPaintPending  */
  HWND      hStatusBar;                /* handle to status bar */
  USHORT    usSBHeight;                /* height of status bar */

  HWND      hToolBar;                  /* TB handle to toolbar        */
  USHORT    usTBHeight;                /* TB height of toolbar        */
  int       iStartStdBitmap,
            iStartViewBitmap,
            iStartHistBitmap;          /* start of bitmap index       */
  int       iTBImgWidth,
            iTBImgHeight;              /* image width and height      */
  WNDPROC   tbOldProc;

  CONTROL_DATA * pcdCtrlList;          /* lists of created controls, eg. scrollbars */

  HFONT     hPBfont;                   /* handle to font used by pushbuttons */

  HFONT     hCBfont;                   /* handle to font used by comboboxes */

  BOOL      bIgnoreWM_SYSCHAR;
  BOOL      bPaint;
  BOOL      bGetFocus;

  POINT     PTEXTSIZE;                 /* size of the fixed width font */
  BOOL      FixedFont;                 /* TRUE if current font is a fixed font */
  int       FixedSize[ WVW_MAX_COLS ]; /* buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected */
  USHORT    ROWS;                      /* number of displayable rows in window */
  USHORT    COLS;                      /* number of displayable columns in window */
  COLORREF  foreground;                /* foreground colour */

  COLORREF  background;                /* background colour */

  USHORT    BUFFERSIZE;                /* size of the screen text buffer */
  BYTE      byAttributes[ WVW_MAX_ROWS * WVW_MAX_COLS ]; /* buffer with the attributes */
  BYTE      byBuffer[ WVW_MAX_ROWS * WVW_MAX_COLS ];     /* buffer with the text to be displayed on the screen */
  BYTE      *pAttributes;              /* pointer to buffer */
  BYTE      *pBuffer;                  /*   "     "    "    */
  POINT     caretPos;                  /* the current caret position */

  int       CaretSize;                 /*x this may be specific to each windows, eg. different font size */
  POINT     mousePos;                  /* the last mousedown position */
  BOOL      MouseMove;                 /* Flag to say whether to return mouse movement events */
  HWND      hWnd;                      /* the window handle */
  int       Keys[ WVW_CHAR_QUEUE_SIZE ]; /* Array to hold the characters & events */
  int       keyPointerIn;              /* Offset into key array for character to be placed */
  int       keyPointerOut;             /* Offset into key array of next character to read */

  RECT      RectInvalid;               /* Invalid rectangle if DISPBEGIN() active */
  HFONT     hFont;
  int       fontHeight;                /* requested font height */
  int       fontWidth ;                /* requested font width */
  int       fontWeight;                /* Bold level */
  int       fontQuality;
  char      fontFace[ LF_FACESIZE ];   /* requested font face name LF_FACESIZE #defined in wingdi.h */

  int       LastMenuEvent;             /* Last menu item selected */
  int       MenuKeyEvent;              /* User definable event number for windows menu command */
  BOOL      CentreWindow;              /* True if window is to be Reset into centre of window */
  int       CodePage;                  /* Code page to use for display characters */

  BOOL      InvalidateWindow;          /* Flag for controlling whether to use ScrollWindowEx() */
  BOOL      EnableShortCuts;           /* Determines whether ALT key enables menu or system menu */

  HDC       hdc;                       /* Handle to Windows Device Context */

  HMENU     hPopup;                    /* Handle of context menu invokable with right click */

  HDC       hCompDC;                   /* Compatible DC to _s.hdc */
  HWND      hWndTT;                    /* Handle to hold tooltip information */
  BOOL      bToolTipActive;            /* Flag to set whether tooltip is active or not */

} WIN_DATA;

POINT  HB_EXPORT hb_wvw_gtGetXYFromColRow( WIN_DATA * pWindowData, USHORT col, USHORT row );
BOOL   HB_EXPORT hb_wvw_gtSetMenuKeyEvent( USHORT usWinNum, int iMenuKeyEvent );
BOOL   HB_EXPORT hb_wvw_gtSetCentreWindow( USHORT usWinNum, BOOL bCentre, BOOL bPaint );
void   HB_EXPORT hb_wvw_gtResetWindow( USHORT usWinNum );
BOOL   HB_EXPORT hb_wvw_gtSetCodePage( USHORT usWinNum, int iCodePage );
int    HB_EXPORT hb_wvw_gtGetLastMenuEvent( USHORT usWinNum );
void   HB_EXPORT hb_wvw_gtSetWindowTitle( USHORT usWinNum, char * title );
DWORD  HB_EXPORT hb_wvw_gtSetWindowIcon( USHORT usWinNum, int icon, char *lpicon );
DWORD  HB_EXPORT hb_wvw_gtSetWindowIconFromFile( USHORT usWinNum, char *icon );
int    HB_EXPORT hb_wvw_gtGetWindowTitle( USHORT usWinNum, char *title, int length );
BOOL   HB_EXPORT hb_wvw_gtSetFont( USHORT usWinNum, char *fontFace, int height, int width, int Bold, int Quality );

HWND   HB_EXPORT hb_wvw_gtGetWindowHandle( USHORT usWinNum );
void   HB_EXPORT hb_wvw_gtPostMessage( USHORT usWinNum, int message );
BOOL   HB_EXPORT hb_wvw_gtSetWindowPos( USHORT usWinNum, int left, int top );
BOOL   HB_EXPORT hb_wvw_gtSetAltF4Close( BOOL bCanClose );
void   HB_EXPORT hb_wvw_gtDoProcessMessages( USHORT usWinNum );
BOOL   HB_EXPORT hb_wvw_gtSetMouseMove( USHORT usWinNum, BOOL bHandleEvent );
BOOL   HB_EXPORT hb_wvw_gtEnableShortCuts( USHORT usWinNum, BOOL bEnable );
BOOL   HB_EXPORT hb_wvw_gtDrawImage( USHORT usWinNum, int x1, int y1, int wd, int ht, char * image );

void   HB_EXPORT hb_wvw_gtDrawBoxRaised( USHORT usWinNum, int iTop, int iLeft, int iBottom, int iRight,
                                         BOOL bTight);  /* <-- none in gtwvt */
void   HB_EXPORT hb_wvw_gtDrawBoxRecessed( USHORT usWinNum, int iTop, int iLeft, int iBottom, int iRight,
                                         BOOL bTight);  /* <-- none in gtwvt */
void   HB_EXPORT hb_wvw_gtDrawOutline( USHORT usWinNum, int iTop, int iLeft, int iBottom, int iRight );

static BOOL   hb_wvw_gtGetCharFromInputQueue( int * c );
void   HB_EXPORT hb_wvw_gtAddCharToInputQueue( int data );

IPicture * HB_EXPORT hb_wvw_gtLoadPicture( char * image );
BOOL HB_EXPORT hb_wvw_gtRenderPicture( USHORT usWinNum, int x1, int y1, int wd, int ht, IPicture * iPicture );
BOOL HB_EXPORT hb_wvw_gtDestroyPicture( IPicture * iPicture );

COLORREF HB_EXPORT hb_wvw_gtGetColorData( int iIndex );
BOOL   HB_EXPORT hb_wvw_gtSetColorData( int iIndex, COLORREF ulCr );

HB_EXPORT WIN_DATA * hb_wvw_gtGetGlobalData( USHORT usWinNum );

static LPWORD lpwAlign( LPWORD lpIn );
static int    nCopyAnsiToWideChar( LPWORD lpWCStr, LPSTR lpAnsiIn );

BOOL   HB_EXPORT CALLBACK hb_wvw_gtDlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );
BOOL   HB_EXPORT CALLBACK hb_wvw_gtDlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );

/*-------------------------------------------------------------------*/

#ifndef WM_MOUSEWHEEL
   #define WM_MOUSEWHEEL 0x020A
#endif

#ifndef INVALID_FILE_SIZE
   #define INVALID_FILE_SIZE (DWORD)0xFFFFFFFF
#endif

#ifndef CC_ANYCOLOR
   #define CC_ANYCOLOR 0x00000100
#endif

#ifndef IDC_HAND
   #define IDC_HAND MAKEINTRESOURCE(32649)
#endif

#ifndef GRADIENT_FILL_RECT_H
   #define GRADIENT_FILL_RECT_H 0x00
#endif

/*-------------------------------------------------------------------*/

#endif
