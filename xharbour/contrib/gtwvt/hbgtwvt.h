/*
 * $Id: hbgtwvt.h,v 1.33 2005/01/09 22:35:39 peterrees Exp $
 */

/*
 * Harbour Project source code:
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

//-------------------------------------------------------------------//
#ifndef HB_WVT_H_
//-------------------------------------------------------------------//

#define HB_WVT_H_

//-------------------------------------------------------------------//

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */

//-------------------------------------------------------------------//

#define HB_GT_NAME  WVT
//#define WVT_DEBUG

//-------------------------------------------------------------------//

#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

//-------------------------------------------------------------------//

#include <windows.h>
#include <winuser.h>
#include <commctrl.h>
#include <commdlg.h>

#if defined(__MINGW32__) || defined(__WATCOMC__) || defined(_MSC_VER)
   #include <unknwn.h>
   #include <ole2.h>
   #include <ocidl.h>
   #include <olectl.h>

   #if defined(_MSC_VER)
      #include <conio.h>
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
//#include "hbstack.h"

//-------------------------------------------------------------------//

#define WVT_CHAR_QUEUE_SIZE  128
#define WVT_CHAR_BUFFER     1024
#define WVT_MAX_ROWS         256
#define WVT_MAX_COLS         256
#define WVT_DEFAULT_ROWS      25
#define WVT_DEFAULT_COLS      80

#define WVT_PICTURES_MAX      20
#define WVT_FONTS_MAX         20
#define WVT_PENS_MAX          20
#define WVT_DLGML_MAX         20
#define WVT_DLGMD_MAX         20

//-------------------------------------------------------------------//

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

//-------------------------------------------------------------------//

#define WM_MY_UPDATE_CARET ( WM_USER + 0x0101 )

//-------------------------------------------------------------------//

typedef BOOL ( WINAPI *wvtGradientFill )     (
                      HDC        hdc,
                      PTRIVERTEX pVertex,
                      ULONG      dwNumVertex,
                      PVOID      pMesh,
                      ULONG      dwNumMesh,
                      ULONG      dwMode      );

//-------------------------------------------------------------------//

typedef struct global_data
{
  POINT     PTEXTSIZE;                 // size of the fixed width font
  BOOL      FixedFont;                 // TRUE if current font is a fixed font
  int       FixedSize[ WVT_MAX_COLS ]; // buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected
  USHORT    ROWS;                      // number of displayable rows in window
  USHORT    COLS;                      // number of displayable columns in window
  COLORREF  foreground;                // forground colour
  COLORREF  background;                // background colour

  USHORT    BUFFERSIZE;                // size of the screen text buffer
  BYTE      byAttributes[ WVT_MAX_ROWS * WVT_MAX_COLS ]; // buffer with the attributes
  BYTE      byBuffer[ WVT_MAX_ROWS * WVT_MAX_COLS ];     // buffer with the text to be displayed on the screen
  BYTE      *pAttributes;              // pointer to buffer
  BYTE      *pBuffer;                  //   "     "    "
  POINT     caretPos;                  // the current caret position
  BOOL      CaretExist;                // TRUE if a caret has been created
  int       CaretSize;
  POINT     mousePos;                  // the last mousedown position
  BOOL      MouseMove;                 // Flag to say whether to return mouse movement events
  HWND      hWnd;                      // the window handle
  int       Keys[ WVT_CHAR_QUEUE_SIZE ]; // Array to hold the characters & events
  int       keyPointerIn;              // Offset into key array for character to be placed
  int       keyPointerOut;             // Offset into key array of next character to read
  BOOL      displayCaret;              // flag to indicate if caret is on
  RECT      RectInvalid;               // Invalid rectangle if DISPBEGIN() active
  HFONT     hFont;
  int       fontHeight;                // requested font height
  int       fontWidth ;                // requested font width
  int       fontWeight;                // Bold level
  int       fontQuality;
  char      fontFace[ LF_FACESIZE ];   // requested font face name LF_FACESIZE #defined in wingdi.h
//  int       closeEvent;                // command to return ( in ReadKey ) on close
//  int       shutdownEvent;             // command to return ( in ReadKey ) on shutdown
  int       LastMenuEvent;             // Last menu item selected
  int       MenuKeyEvent;              // User definable event number for windows menu command
  BOOL      CentreWindow;              // True if window is to be Reset into centre of window
  int       CodePage;                  // Code page to use for display characters
  BOOL      Win9X;                     // Flag to say if running on Win9X not NT/2000/XP
  BOOL      AltF4Close;                // Can use Alt+F4 to close application
  BOOL      InvalidateWindow;          // Flag for controlling whether to use ScrollWindowEx()
  BOOL      EnableShortCuts;           // Determines whether ALT key enables menu or system menu
  HPEN      penWhite;                  // White pen to draw GDI elements
  HPEN      penBlack;                  // Black pen to draw GDI elements
  HPEN      penWhiteDim;               // White dim pen to draw GDI elements
  HPEN      penDarkGray;               // Dark gray pen to draw GDI elements
  HPEN      penGray;                   // Gray pen equivilant to Clipper White
  HPEN      penNull;                   // Null pen
  HPEN      currentPen;                // Handle to current pen settable at runtime
  HBRUSH    currentBrush;              // Handle to current brush settable by runtime
  HBRUSH    diagonalBrush;             // Handle to diaoganl brush to draw scrollbars
  HBRUSH    solidBrush;                // Handle to solid brush
  HBRUSH    wvtWhiteBrush;             // Wvt specific White colored brush
  HDC       hdc;                       // Handle to Windows Device Context
  PHB_DYNS  pSymWVT_PAINT;             // Stores pointer to WVT_PAINT function
  PHB_DYNS  pSymWVT_SETFOCUS;          // Stores pointer to WVT_SETFOCUS function
  PHB_DYNS  pSymWVT_KILLFOCUS;         // Stores pointer to WVT_KILLFOCUS function
  PHB_DYNS  pSymWVT_MOUSE;             // Stores pointer to WVT_MOUSE function
  PHB_DYNS  pSymWVT_TIMER;             // Stores pointer to WVT_TIMER function
  int       rowStart;                  // Holds nTop    of last WM_PAINT rectangle returned by Wvt_GetPaintRect()
  int       rowStop;                   // Holds nBottom of last WM_PAINT rectangle
  int       colStart;                  // Holds nLeft   of last WM_PAINT rectangle
  int       colStop;                   // Holds nRight  of last WM_PAINT rectangle
  HMENU     hPopup;                    // Handle of context menu invokable with right click
  IPicture  *iPicture[ WVT_PICTURES_MAX ]; // Array to hold the Picture Streams to avoid recurring loading and unloading
  HDC       hCompDC;                   // Compatible DC to _s.hdc
  HFONT     hUserFonts[ WVT_FONTS_MAX ] ;  // User defined font handles
  HPEN      hUserPens[ WVT_PENS_MAX ]; // User defined pens
  HWND      hWndTT;                    // Handle to hold tooltip information
  BOOL      bToolTipActive;            // Flag to set whether tooltip is active or not
  HINSTANCE hMSImg32;                  // Handle to the loaded library msimg32.dll
  wvtGradientFill pfnGF;               // Pointer to Address of the GradientFill function in MSImg32.dll
  HWND      hDlgModeless[ WVT_DLGML_MAX ]; // Handle to a modeless dialog
  PHB_ITEM  pFunc[ WVT_DLGML_MAX ];    // Function pointer for WndProc
  HB_ITEM   cbFunc[ WVT_DLGML_MAX ];   //codeblock for WndProc
  int       iType[ WVT_DLGML_MAX ];    // Type of Function Pointers - Function 1, Block 2, Method 3
  HWND      hDlgModal[ WVT_DLGMD_MAX ];// Handle to a modeless dialog
  PHB_ITEM  pFuncModal[ WVT_DLGMD_MAX ];  // Function pointer for WndProc
  HB_ITEM   cbFuncModal[ WVT_DLGMD_MAX ]; // codeblock for WndProc
  int       iTypeModal[ WVT_DLGMD_MAX ];  // Type of Function Pointers - Function 1, Block 2, Method 3
  BOOL      bGui;
  HDC       hGuiDC;
  HBITMAP   hGuiBmp;
  int       iGuiWidth;
  int       iGuiHeight;
} GLOBAL_DATA;

typedef GLOBAL_DATA * LPGLOBAL_DATA;

//-------------------------------------------------------------------//

POINT  HB_EXPORT hb_wvt_gtGetXYFromColRow( USHORT col, USHORT row );
BOOL   HB_EXPORT hb_wvt_gtSetMenuKeyEvent( int iMenuKeyEvent );
BOOL   HB_EXPORT hb_wvt_gtSetCentreWindow( BOOL bCentre, BOOL bPaint );
void   HB_EXPORT hb_wvt_gtResetWindow( void );
BOOL   HB_EXPORT hb_wvt_gtSetCodePage( int iCodePage );
int    HB_EXPORT hb_wvt_gtGetLastMenuEvent( void );
void   HB_EXPORT hb_wvt_gtSetWindowTitle( char * title );
DWORD  HB_EXPORT hb_wvt_gtSetWindowIcon( int icon, char *lpicon );
DWORD  HB_EXPORT hb_wvt_gtSetWindowIconFromFile( char *icon );
int    HB_EXPORT hb_wvt_gtGetWindowTitle( char *title, int length );
BOOL   HB_EXPORT hb_wvt_gtSetFont( char *fontFace, int height, int width, int Bold, int Quality );
//void   HB_EXPORT hb_wvt_gtSetCloseEvent( int iEvent );
//void   HB_EXPORT hb_wvt_gtSetShutdownEvent( int iEvent );
HWND   HB_EXPORT hb_wvt_gtGetWindowHandle( void );
void   HB_EXPORT hb_wvt_gtPostMessage( int message );
BOOL   HB_EXPORT hb_wvt_gtSetWindowPos( int left, int top );
BOOL   HB_EXPORT hb_wvt_gtSetAltF4Close( BOOL bCanClose );
void   HB_EXPORT hb_wvt_gtDoProcessMessages( void );
BOOL   HB_EXPORT hb_wvt_gtSetMouseMove( BOOL bHandleEvent );
BOOL   HB_EXPORT hb_wvt_gtEnableShortCuts( BOOL bEnable );
void   HB_EXPORT hb_wvt_gtAddCharToInputQueue( int data );
IPicture * HB_EXPORT hb_wvt_gtLoadPicture( char * image );
BOOL   HB_EXPORT hb_wvt_gtRenderPicture( int x1, int y1, int wd, int ht, IPicture * iPicture );
BOOL   HB_EXPORT hb_wvt_gtDestroyPicture( IPicture * iPicture );
COLORREF HB_EXPORT hb_wvt_gtGetColorData( int iIndex );
BOOL   HB_EXPORT hb_wvt_gtSetColorData( int iIndex, COLORREF ulCr );
BOOL   HB_EXPORT hb_wvt_DrawImage( HDC hdc, int x1, int y1, int wd, int ht, char * image );

LPWORD HB_EXPORT lpwAlign( LPWORD lpIn );
int    HB_EXPORT nCopyAnsiToWideChar( LPWORD lpWCStr, LPSTR lpAnsiIn );
BOOL   HB_EXPORT CALLBACK hb_wvt_gtDlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );
BOOL   HB_EXPORT CALLBACK hb_wvt_gtDlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam );

HB_EXPORT GLOBAL_DATA * hb_wvt_gtGetGlobalData( void );

void   HB_EXPORT hb_wvt_wvtCore( void );
void   HB_EXPORT hb_wvt_wvtUtils( void );

//-------------------------------------------------------------------//

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

//-------------------------------------------------------------------//

#endif

//-------------------------------------------------------------------//

