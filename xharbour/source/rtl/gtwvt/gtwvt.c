/*
 * Harbour Project source code:
 * Video subsystem for Win32 using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */

#define HB_GT_NAME	WVT
//#define WVT_DEBUG

#include <windows.h>
#include <winuser.h>
#include <commctrl.h>
#include <time.h>
#include <ctype.h>

#include "hbset.h"
#include "hbapigt.h"
#include "hbapierr.h"
#include "inkey.ch"
#include "error.ch"

#define WVT_CHAR_QUEUE_SIZE 128
#define WVT_CHAR_BUFFER 1024
#define WVT_MAX_ROWS 256
#define WVT_MAX_COLS 256
#define WVT_DEFAULT_ROWS 25
#define WVT_DEFAULT_COLS 80

#define BLACK RGB(0x0,0x0,0x0)
#define BLUE RGB(0x0,0x0,0xC0)
#define GREEN RGB(0x0,0xC0,0x0)
#define CYAN RGB(0x0,0xC0,0xC0)
#define RED RGB(0xC0,0x0,0x0)
#define MAGENTA RGB(0xC0,0x0,0xC0)
#define BROWN RGB(0xC0,0xC0,0x0)
#define WHITE RGB(0xF0,0xF0,0xF0)
#define LIGHT_GRAY RGB(0xA0,0xA0,0xA0)
#define BRIGHT_BLUE RGB(0x0,0x0,0xFF)
#define BRIGHT_GREEN RGB(0x0,0xFF,0x0)
#define BRIGHT_CYAN RGB(0x0,0xFF,0xFF)
#define BRIGHT_RED RGB(0xFF,0x0,0x0)
#define BRIGHT_MAGENTA RGB(0xFF,0x0,0xFF)
#define YELLOW RGB(0xFF,0xFF,0x0)
#define BRIGHT_WHITE RGB(0xFF,0xFF,0xFF)

#define WM_MY_UPDATE_CARET (WM_USER+0x0101)


static TCHAR szAppName[] = TEXT("xHarbour WVT");

typedef struct global_data
{

  POINT PTEXTSIZE;              // size of the fixed width font
  BOOL  FixedFont;              // TRUE if current font is a fixed font
  int   FixedSize[WVT_MAX_COLS];// buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected
  USHORT ROWS;                  // number of displayable rows in window
  USHORT COLS;                  // number of displayable columns in window
  COLORREF foreground;          // forground colour
  COLORREF background;          // background colour

  USHORT BUFFERSIZE;            // size of the screen text buffer
  BYTE byAttributes[WVT_MAX_ROWS * WVT_MAX_COLS]; // buffer with the attributes
  BYTE byBuffer[WVT_MAX_ROWS * WVT_MAX_COLS];     // buffer with the text to be displayed on the screen
  BYTE *pAttributes;            // pointer to buffer
  BYTE *pBuffer;                //   "     "    "
  POINT caretPos;               // the current caret position
  BOOL CaretExist;              // TRUE if a caret has been created
  int CaretSize;
  POINT mousePos;               // the last mousedown position
  HWND hWnd;                    // the window handle
  int Keys[WVT_CHAR_QUEUE_SIZE];    // Array to hold the characters & events
  int keyPointerIn;             // Offset into key array for character to be placed
  int keyPointerOut;            // Offset into key array of next character to read
  BOOL displayCaret;            // flag to indicate if caret is on
  RECT RectInvalid;             // Invalid rectangle if DISPBEGIN() active
  HFONT hFont;
  int fontHeight;               // requested font height
  int fontWidth ;               // requested font width
  int fontWeight;               // Bold level
  int fontQuality;
  char fontFace[LF_FACESIZE];   // requested font face name LF_FACESIZE #defined in wingdi.h
  int closeEvent;             // command to return (in ReadKey) on close
  int shutdownEvent;          // command to return (in ReadKey) on shutdown
  int LastMenuEvent;          // Last menu item selected
  int MenuKeyEvent;             // User definable event number for windows menu command
  BOOL CentreWindow;            // True if window is to be Reset into centre of window
  int CodePage;                 // Code page to use for display characters
  BOOL Win9X;                   // Flag to say if running on Win9X not NT/2000/XP
  BOOL AltF4Close;              // Can use Alt+F4 to close application
  BOOL InvalidateWindow;        // Flag for controlling whether to use ScrollWindowEx()
} GLOBAL_DATA;

static GLOBAL_DATA _s;

static COLORREF _COLORS[] = {
   BLACK,
   BLUE,
   GREEN,
   CYAN,
   RED,
   MAGENTA,
   BROWN,
   WHITE,
   LIGHT_GRAY,
   BRIGHT_BLUE,
   BRIGHT_GREEN,
   BRIGHT_CYAN,
   BRIGHT_RED,
   BRIGHT_MAGENTA,
   YELLOW,
   BRIGHT_WHITE
};

#ifdef WVT_DEBUG
static int nCountPuts=0,nCountScroll=0, nCountPaint=0, nSetFocus=0, nKillFocus=0;
#endif

static int _Alt_Alpha[] = {
  K_ALT_A, K_ALT_B, K_ALT_C, K_ALT_D, K_ALT_E, K_ALT_F, K_ALT_G, K_ALT_H,
  K_ALT_I, K_ALT_J, K_ALT_K, K_ALT_L, K_ALT_M, K_ALT_N, K_ALT_O, K_ALT_P,
  K_ALT_Q, K_ALT_R, K_ALT_S, K_ALT_T, K_ALT_U, K_ALT_V, K_ALT_W, K_ALT_X,
  K_ALT_Y, K_ALT_Z
  };

static int _Alt_Num[] = {
  K_ALT_0, K_ALT_1, K_ALT_2, K_ALT_3, K_ALT_4, K_ALT_5, K_ALT_6, K_ALT_7,
  K_ALT_8, K_ALT_9
  };

//private
static void gt_hbInitStatics(void);
static HWND hb_wvt_gtCreateWindow(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow);
static BOOL hb_wvt_gtInitWindow(HWND hWnd, USHORT col, USHORT row);
static void hb_wvt_gtResetWindowSize(HWND hWnd);
static LRESULT CALLBACK hb_wvt_gtWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
static BOOL hb_wvt_gtAllocSpBuffer(USHORT col, USHORT row);
static DWORD hb_wvt_gtProcessMessages(void);
static BOOL hb_wvt_gtValidWindowSize(int rows, int cols, HFONT hFont);


static void hb_wvt_gtSetCaretOn(BOOL bOn);
static BOOL hb_wvt_gtSetCaretPos(void);
static void hb_wvt_gtValidateCaret(void);

static USHORT hb_wvt_gtGetMouseX (void);
static USHORT hb_wvt_gtGetMouseY (void);
static void hb_wvt_gtSetMouseX (USHORT ix);
static void hb_wvt_gtSetMouseY (USHORT iy);

static BOOL hb_wvt_gtGetCharFromInputQueue (int * c);
static void hb_wvt_gtAddCharToInputQueue (int data);
static void hb_wvt_gtTranslateKey(int key, int shiftkey, int altkey, int controlkey);

static void hb_wvt_gtSetInvalidRect(USHORT left, USHORT top, USHORT right, USHORT bottom);
static void hv_wvt_gtDoInvalidateRect(void);

static void hb_wvt_gtHandleMenuSelection(int);
static int hb_wvt_gtGetCloseEvent(void);
static int hb_wvt_gtGetShutdownEvent(void);

static POINT hb_wvt_gtGetColRowFromXY( USHORT x, USHORT y);
static RECT hb_wvt_gtGetColRowFromXYRect( RECT xy);
static POINT hb_wvt_gtGetColRowForTextBuffer(USHORT index);
static POINT hb_wvt_gtGetXYFromColRow( USHORT col, USHORT row);
static void hb_wvt_gtValidateCol(void);
static void hb_wvt_gtValidateRow(void);

static USHORT hb_wvt_gtCalcPixelHeight(void);
static USHORT hb_wvt_gtCalcPixelWidth(void);
static BOOL hb_wvt_gtSetColors(HDC hdc, BYTE attr);
static HFONT hb_wvt_gtGetFont(char * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage);

static BOOL hb_wvt_gtTextOut(HDC hdc, USHORT col, USHORT row, LPCTSTR lpString,  USHORT cbString  );
static void hb_wvt_gtSetStringInTextBuffer(USHORT col, USHORT row, BYTE attr, BYTE *sBuffer, USHORT length);
static USHORT hb_wvt_gtGetIndexForTextBuffer(USHORT col, USHORT row);
static RECT hb_wvt_gtGetXYFromColRowRect( RECT colrow);
static RECT hb_wvt_gtGetColRowFromXYRect( RECT xy);

// mouse initialization was made in cmdarg.c
extern BOOL b_MouseEnable;

// set in mainwin.c
extern HINSTANCE hb_hInstance;
extern HINSTANCE hb_hPrevInstance;
extern int hb_iCmdShow;

static USHORT s_uiDispCount;
static USHORT s_usCursorStyle;
static USHORT s_usOldCurStyle;

static int s_iStdIn, s_iStdOut, s_iStdErr;



/* *********************************************************************** */

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

    /* stdin && stdout && stderr */
    s_iStdIn  = iFilenoStdin;
    s_iStdOut = iFilenoStdout;
    s_iStdErr = iFilenoStderr;

    s_usOldCurStyle = s_usCursorStyle = SC_NORMAL;
    gt_hbInitStatics();
    _s.hWnd = hb_wvt_gtCreateWindow( hb_hInstance,  hb_hPrevInstance,  "", hb_iCmdShow);
    if (!_s.hWnd)
    {
      //Runtime error
      hb_errRT_TERM( EG_CREATE, 10001, "WINAPI CreateWindow() failed", "hb_gt_Init()", 0, 0 );
    }
    if( b_MouseEnable )
    {
      HB_GT_FUNC(mouse_Init());
    }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Exit( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));
    if (_s.hWnd)
    {
      DestroyWindow(_s.hWnd);
    }
    UnregisterClass(szAppName,hb_hInstance);

    if( b_MouseEnable )
    {
       HB_GT_FUNC(mouse_Exit());
    }

}

/* *********************************************************************** */
/* returns the number of displayable columns
 */
USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));
  return(_s.COLS);
}

/* *********************************************************************** */
/* returns the number of displayable rows
 */
USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));
  return(_s.ROWS);
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Col( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));
  return(_s.caretPos.x);
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Row( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));
  return(_s.caretPos.y);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetPos( SHORT sRow, SHORT sCol, SHORT sMethod ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", sRow, sCol, sMethod));
  HB_SYMBOL_UNUSED( sMethod );
  if (sRow >= 0 && sRow< _s.ROWS && sCol>=0 && sCol <= _s.COLS )
  {
    _s.caretPos.x = sCol;
    _s.caretPos.y = sRow;
    hb_wvt_gtValidateCaret();
  }
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));
  HB_SYMBOL_UNUSED( pStr );
  HB_SYMBOL_UNUSED( ulLen );
  return(FALSE);
}


/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));
  return(TRUE);
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));
  return(s_usCursorStyle);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetCursorStyle( USHORT usStyle ))
{
  BOOL bCursorOn= TRUE;
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", usStyle));
  s_usCursorStyle = usStyle;
  switch( usStyle )
  {
    case SC_NONE:
      _s.CaretSize = 0 ;
      bCursorOn= FALSE;
      break ;
    case SC_INSERT:
      _s.CaretSize = (_s.PTEXTSIZE.y / 2) ;
      break;
    case SC_SPECIAL1:
      _s.CaretSize = _s.PTEXTSIZE.y ;
      break;
    case SC_SPECIAL2:
      _s.CaretSize = -(_s.PTEXTSIZE.y / 2) ;
      break;
    case SC_NORMAL:
    default:
      _s.CaretSize = 4 ;
      break;
  }
  if (bCursorOn )
  {
    _s.CaretExist = CreateCaret(_s.hWnd, (HBITMAP) NULL, _s.PTEXTSIZE.x, _s.CaretSize);
  }
  hb_wvt_gtSetCaretOn(bCursorOn);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_DispBegin( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));
  ++s_uiDispCount;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_DispEnd())
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));
  if (s_uiDispCount > 0)
  {
    --s_uiDispCount;
  }
  if (s_uiDispCount<= 0)
  {
    hv_wvt_gtDoInvalidateRect();
  }
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_DispCount())
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispCount()"));
  return(s_uiDispCount);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) byAttr, pbyStr, ulLen));
  hb_wvt_gtSetStringInTextBuffer( usCol, usRow, byAttr, pbyStr, ulLen);
#ifdef WVT_DEBUG
  nCountPuts++;
#endif
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen ))
{
  BYTE ucBuff[WVT_CHAR_BUFFER], *byChars;
  ULONG i;
  BOOL bMalloc = FALSE;
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", usRow, usCol, byAttr, byChar, ulLen));
  if (ulLen > WVT_CHAR_BUFFER)
  {  // Avoid allocating memory if possible
    byChars = (BYTE*) hb_xgrab(ulLen);
    bMalloc= TRUE;
  }
  else
  {
    byChars = ucBuff ;
  }

  for (i = 0; i < ulLen; i++)
  {
    *(byChars+i) = byChar;
  }

  hb_wvt_gtSetStringInTextBuffer(usCol, usRow, byAttr, byChars, ulLen);
  if (bMalloc)
  {
    hb_xfree(byChars);
  }
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_RectSize()"));
  return(rows * cols * 2);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_GetText( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer ))
{
  USHORT irow, icol, index, j;
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", top, left, bottom, right, sBuffer));

  j = 0;
  for (irow = top; irow <= bottom; irow++)
  {
    index = hb_wvt_gtGetIndexForTextBuffer(left, irow);
    for (icol = left; icol <= right; icol++)
    {
      if (index >= _s.BUFFERSIZE)
      {
        break;
      }
      else
      {
        sBuffer[j++] = _s.pBuffer[index];
        sBuffer[j++] = _s.pAttributes[index];
        index++;
      }
    }
  }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_PutText( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer ))
{
  USHORT irow, icol, index, j;
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", top, left, bottom, right, sBuffer));

  j = 0;
  for (irow = top; irow <= bottom; irow++)
  {
    index = hb_wvt_gtGetIndexForTextBuffer(left, irow);
    for (icol = left; icol <= right; icol++)
    {
      if (index >= _s.BUFFERSIZE)
      {
        break;
      }
      else
      {
        _s.pBuffer[index] = sBuffer[j++];
        _s.pAttributes[index] = sBuffer[j++];
        index++;
      }
    }
  }
  hb_wvt_gtSetInvalidRect(left, top, right, bottom);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetAttribute( USHORT rowStart, USHORT colStart, USHORT rowStop, USHORT colStop, BYTE attr ))
{
  USHORT irow, icol, index;
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", usTop, usLeft, usBottom, usRight, (int) attr));
  for ( irow = rowStart; irow <rowStop; irow++)
  {
    index = hb_wvt_gtGetIndexForTextBuffer(colStart, irow);
    for (icol = colStart; icol < colStop; icol++)
    {
      if (index >= _s.BUFFERSIZE)
      {
        break;
      }
      else
      {
        _s.pAttributes[index++] = attr;
      }
    }
  }
  hb_wvt_gtSetInvalidRect(colStart, rowStart, colStop, rowStop );
}

/* *********************************************************************** */
// copied from gtwin...

void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{
  SHORT usSaveRow, usSaveCol;
  UINT uiSize;
  unsigned char ucBlank[WVT_CHAR_BUFFER], ucBuff[WVT_CHAR_BUFFER*2] ;
  unsigned char * fpBlank ;
  unsigned char * fpBuff  ;
  int iLength = ( usRight - usLeft ) + 1;
  int iCount, iColOld, iColNew, iColSize;
  BOOL bMalloc = FALSE;

  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) byAttr, iRows, iCols));

  if (iLength > WVT_CHAR_BUFFER)
  { // Avoid allocating memory if possible
    fpBlank = ( unsigned char * ) hb_xgrab( iLength );
    fpBuff  = ( unsigned char * ) hb_xgrab( iLength * 2 );  //*2 room for attribs
    bMalloc = TRUE;
  }
  else
  {
    fpBlank = ucBlank ;
    fpBuff  = ucBuff  ;
  }

  memset( fpBlank, ' ', iLength );

  iColOld = iColNew = usLeft;
  iColSize = iLength -1;
  if( iCols >= 0 )
  {
    iColOld += iCols;
    iColSize -= iCols;
  }
  else
  {
    iColNew -= iCols;
    iColSize += iCols;
  }
  // use the ScrollWindowEx() where possible (Optimised for Terminal Server)
  // if both iCols & iRows are ZERO then the entire area is to be cleared and
  // there is no advantage in using ScrollWindowEx()
  _s.InvalidateWindow = HB_GT_FUNC(gt_DispCount()) > 0 || (!iRows && !iCols) ;
  // if _s.InvalidateWindow is FALSE it is used to stop
  //   HB_GT_FUNC(gt_Puts()) & HB_GT_FUNC(gt_PutText())
  //   from actually updating the screen. ScrollWindowEx() is used
  if (_s.InvalidateWindow )
  {
    HB_GT_FUNC(gt_DispBegin());
  }

  usSaveCol = HB_GT_FUNC(gt_Col() ) ;
  usSaveRow = HB_GT_FUNC(gt_Row() ) ;
  for( iCount = ( iRows >= 0 ? usTop : usBottom );
       ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
       ( iRows >= 0 ? iCount++ : iCount-- ) )
  {
      int iRowPos = iCount + iRows;


      /* Read the text to be scrolled into the current row */
      if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
      {
        HB_GT_FUNC(gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff ));
      }

      /* Blank the scroll region in the current row */
      HB_GT_FUNC(gt_Puts( iCount, usLeft, byAttr, fpBlank, iLength ));

      /* Write the scrolled text to the current row */
      if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
      {
        HB_GT_FUNC(gt_PutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff ));
      }
  }
  HB_GT_FUNC(gt_SetPos( usSaveRow, usSaveCol, HB_GT_SET_POS_AFTER ));

  if (_s.InvalidateWindow )
  {
    HB_GT_FUNC(gt_DispEnd());
  }
  else
  {
    RECT cr, crInvalid;
    cr.left = usLeft +  (iCols>0 ? 1 : 0) ;
    cr.top  = usTop  +  (iRows>0 ? 1 : 0) ;
    cr.right= usRight -  (iCols<0 ? 1 : 0) ;
    cr.bottom= usBottom - ( iRows<0 ? 1 : 0) ;
    cr = hb_wvt_gtGetXYFromColRowRect(cr);
    ScrollWindowEx(_s.hWnd, -iCols * _s.PTEXTSIZE.x, -iRows *_s.PTEXTSIZE.y, &cr, NULL, NULL, &crInvalid, 0) ;
    InvalidateRect(_s.hWnd, &crInvalid, FALSE);
    _s.InvalidateWindow= TRUE ;
  }
  if (bMalloc)
  {
    hb_xfree( fpBlank );
    hb_xfree( fpBuff );
  }
#ifdef WVT_DEBUG
  nCountScroll++;
#endif
}

/* *********************************************************************** */
/* resize the (existing) window
 */
BOOL HB_GT_FUNC(gt_SetMode( USHORT row, USHORT col ))
{
  BOOL bResult= FALSE;
  HFONT hFont
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", row, col));
  if (row<= WVT_MAX_ROWS && col<= WVT_MAX_COLS)
  {
    hFont = hb_wvt_gtGetFont(_s.fontFace, _s.fontHeight, _s.fontWidth, _s.fontWeight, _s.fontQuality, _s.CodePage);
    if ( hFont )
    {
      // make sure that the mode selected along with the current
      // font settings will fit in the window
      if ( hb_wvt_gtValidWindowSize(row,col, hFont))
      {
        bResult = hb_wvt_gtInitWindow(_s.hWnd, col, row);
      }
      DeleteObject(hFont);
    }
  }
  return(bResult);
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_GetBlink())
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));
  return(TRUE);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));
  HB_SYMBOL_UNUSED( bBlink );
}

/* *********************************************************************** */

char * HB_GT_FUNC(gt_Version( void ))
{
  return("xHarbour Terminal: Win32 buffered WVT");
}

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xPutch( USHORT iRow, USHORT iCol, BYTE bAttr, BYTE bChar ))
{
  USHORT index;
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", iRow, iCol, (int) bAttr, bChar));

  index = hb_wvt_gtGetIndexForTextBuffer(iCol, iRow);
  if (index < _s.BUFFERSIZE )
  {
    _s.pBuffer[index] = bChar;
    _s.pAttributes[index] = bAttr;

    //determine bounds of rect around character to refresh
    hb_wvt_gtSetInvalidRect(iCol, iRow, iCol, iRow);
  }
}

/* *********************************************************************** */

// copied from gtwin
USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * szBox, BYTE byAttr ))
{
    USHORT ret = 1;
    SHORT Row;
    SHORT Col;
    SHORT Height;
    SHORT Width;
    USHORT sWidth = HB_GT_FUNC(gt_GetScreenWidth());
    USHORT sHeight = HB_GT_FUNC(gt_GetScreenHeight());

    if( ( Left   >= 0 && Left   < sWidth  ) ||
        ( Right  >= 0 && Right  < sWidth  ) ||
        ( Top    >= 0 && Top    < sHeight ) ||
        ( Bottom >= 0 && Bottom < sHeight ) )
    {
        /* Ensure that box is drawn from top left to bottom right. */
        if( Top > Bottom )
        {
            Row = Top;
            Top = Bottom;
            Bottom = Row;
        }
        if( Left > Right )
        {
            Row = Left;
            Left = Right;
            Right = Row;
        }

        /* Draw the box or line as specified */
        Height = Bottom - Top + 1;
        Width  = Right - Left + 1;

        HB_GT_FUNC(gt_DispBegin());

        if( Height > 1 && Width > 1 &&
               Top >= 0 && Top < sHeight &&
              Left >= 0 && Left < sWidth )
        {
          HB_GT_FUNC(gt_xPutch( Top, Left, byAttr, szBox[ 0 ] )); /* Upper left corner */
        }

        Col = ( Height > 1 ? Left + 1 : Left );
        if( Col < 0 )
        {
            Width += Col;
            Col = 0;
        }
        if( Right >= sWidth )
        {
            Width -= Right - sWidth;
        }

        if( Col <= Right && Col < sWidth &&
                Top >= 0 && Top < sHeight )
        {
            HB_GT_FUNC(gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) )); /* Top line */
        }
        if( Height > 1 &&
               (Right - Left) > 1 && Right < sWidth &&
               Top >= 0 && Top < sHeight )
        {
            HB_GT_FUNC(gt_xPutch( Top, Right, byAttr, szBox[ 2 ] )); /* Upper right corner */
        }
        if( szBox[ 8 ] && Height > 2 && Width > 2 )
        {
            for( Row = Top + 1; Row < Bottom; Row++ )
            {
                if( Row >= 0 && Row < sHeight )
                {
                    Col = Left;
                    if( Col < 0 )
                    {
                      Col = 0; /* The width was corrected earlier. */
                    }
                    else
                    {
                      HB_GT_FUNC(gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] )); /* Left side */
                    }
                    HB_GT_FUNC(gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 )); /* Fill */
                    if( Right < sWidth )
                    {
                      HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                    }
                }
            }
        }
        else
        {
            for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
            {
                if( Row >= 0 && Row < sHeight )
                {
                    if( Left >= 0 && Left < sWidth )
                    {
                        HB_GT_FUNC(gt_xPutch( Row, Left, byAttr, szBox[ 7 ] )); /* Left side */
                    }
                    if( ( Width > 1 || Left < 0 ) && Right < sWidth )
                    {
                        HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                    }
                }
            }
        }

        if( Height > 1 && Width > 1 )
        {
            if( Left >= 0 && Bottom < sHeight )
            {
                HB_GT_FUNC(gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] )); /* Bottom left corner */
            }
            Col = Left + 1;
            if( Col < 0 )
            {
                Col = 0; /* The width was corrected earlier. */
            }
            if( Col <= Right && Bottom < sHeight )
            {
                HB_GT_FUNC(gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 )); /* Bottom line */
            }
            if( Right < sWidth && Bottom < sHeight )
            {
                HB_GT_FUNC(gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] )); /* Bottom right corner */
            }
        }
        HB_GT_FUNC(gt_DispEnd());
        ret = 0;
    }

    return(ret);
}



/* *********************************************************************** */

//copied from gtwin
USHORT HB_GT_FUNC(gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
    return( HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr )));
}

/* *********************************************************************** */
//copied from gtwin

USHORT HB_GT_FUNC(gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
    return( HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr )));
}

/* *********************************************************************** */
//copied from gtwin

USHORT HB_GT_FUNC(gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr ))
{
  USHORT ret = 1;
  USHORT sWidth = HB_GT_FUNC(gt_GetScreenWidth());
  if( Row >= 0 && Row < sWidth )
  {
      if( Left < 0 )
      {
          Left = 0;
      }
      else if( Left >= sWidth )
      {
          Left = sWidth - 1;
      }
      if( Right < 0 )
      {
          Right = 0;
      }
      else if( Right >= sWidth )
      {
          Right = sWidth - 1;
      }
      if( Left < Right )
      {
          HB_GT_FUNC(gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 ));
      }
      else
      {
          HB_GT_FUNC(gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 ));
      }
      ret = 0;
  }
  return( ret);
}

/* *********************************************************************** */
//copied from gtwin

USHORT HB_GT_FUNC(gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ))
{
    USHORT ret = 1;
    USHORT sWidth = HB_GT_FUNC(gt_GetScreenWidth());
    USHORT sHeight = HB_GT_FUNC(gt_GetScreenHeight());
    SHORT Row;

    if( Col >= 0 && Col < sWidth )
    {
        if( Top < 0 )
        {
            Top = 0;
        }
        else if( Top >= sHeight )
        {
            Top = sHeight - 1;
        }
        if( Bottom < 0 )
        {
            Bottom = 0;
        }
        else if( Bottom >= sHeight )
        {
            Bottom = sHeight - 1;
        }
        if( Top <= Bottom )
        {
            Row = Top;
        }
        else
        {
            Row = Bottom;
            Bottom = Top;
        }

        HB_GT_FUNC(gt_DispBegin());

        while( Row <= Bottom )
        {
            HB_GT_FUNC(gt_xPutch( Row++, Col, byAttr, byChar ));
        }
        HB_GT_FUNC(gt_DispEnd());

        ret = 0;
    }
    return( ret);
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_Suspend())
{
  return(TRUE);
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_Resume())
{
  return(TRUE);
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_PreExt())
{
  return(TRUE);
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_PostExt())
{
  return(TRUE);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_OutStd( BYTE * pbyStr, ULONG ulLen ))
{
  hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_OutErr( BYTE * pbyStr, ULONG ulLen ))
{
  hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_ExtendedKeySupport())
{
    return(FALSE);  // Only use standard Clipper hey handling
}


/* *********************************************************************** */
int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
  int c=0;
  BOOL bKey;
  HB_SYMBOL_UNUSED( eventmask ); // we ignore the eventmask!
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));
  hb_wvt_gtProcessMessages() ;
  bKey = hb_wvt_gtGetCharFromInputQueue(&c);
  return( bKey ? c : 0);
}

/* *********************************************************************** */
// Copied from gtwin
#if defined(__BORLANDC__) || defined(_MSC_VER)
static int hb_Inp9x( USHORT usPort )
{
  USHORT usVal;

    HB_TRACE(HB_TR_DEBUG, ("hb_Inp9x(%hu)", usPort));

    #if defined(__BORLANDC__)
       _DX = usPort;
       __emit__(0xEC);        /* ASM  IN AL, DX */
       __emit__(0x32,0xE4);   /* ASM XOR AH, AH */
       usVal = _AX;
    #else

       usVal = _inp( usPort );
    #endif

    return( usVal);
}

/* *********************************************************************** */
// Copied from gtwin
static int hb_Outp9x( USHORT usPort, USHORT usVal )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_Outp9x(%hu, %hu)", usPort, usVal));

    #if defined(__BORLANDC__)
      _DX = usPort;
      _AL = usVal;
      __emit__(0xEE);        /* ASM OUT DX, AL */
      __emit__(0x32,0xE4);   /* ASM XOR AH, AH */
      usVal = _AX;
    #else
       _outp( usPort, usVal );
    #endif

    return( usVal);
}

/* *********************************************************************** */
// Copied from gtwin
static void HB_GT_FUNC(gt_w9xTone( double dFreq, double dDurat, double dTick ))
{
  INT uLSB,uMSB;
  UINT uiValue;
  ULONG lAdjFreq;
  clock_t end_clock;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_w9xtone(%lf, %lf, %lf)", dFreq, dDurat, dTick));

    /* Clipper ignores Tone() requests if Frequency is less than
       < 20 hz (and so should we) to maintain compatibility .. */

    if ( dFreq > 20.0 )
    {

      /* Setup Sound Control Port Registers.. */

      /* select timer channel 2 */

      hb_Outp9x(67, 182) ;

      lAdjFreq = (ULONG)( 1193180 / dFreq ) ;

      if( (LONG) lAdjFreq < 0 )
         uLSB = lAdjFreq + 65536;
      else
         uLSB = lAdjFreq % 256;

      if( (LONG) lAdjFreq < 0 )
         uMSB = lAdjFreq + 65536;
      else
         uMSB = lAdjFreq / 256;


      /* set the frequency (LSB,MSB) */

      hb_Outp9x(66, uLSB);
      hb_Outp9x(66, uMSB);

      /* Get current Port setting */

      uiValue = hb_Inp9x( 97 );

      /* enable Speaker Data & Timer gate bits */

      uiValue = uiValue | 3;  /* 00000011B is bitmask to enable sound */

      /* Turn on Speaker - sound Tone for duration.. */

      hb_Outp9x(97, uiValue);

      end_clock = clock() + ( clock_t ) ( dDurat );
      while( clock() < end_clock )
      {
        hb_idleState();
      }
      hb_idleReset();

      /* Read back current Port value for Reset */

      uiValue = hb_Inp9x( 97 );

      /* disable Speaker Data & Timer gate bits */
      uiValue = uiValue & 0xFC ;

      /* Turn off the Speaker ! */

      hb_Outp9x(97, uiValue);

    }

    /* Delay (1) clock tick, just like Clipper .. */

    end_clock = clock() + ( clock_t ) ( dTick );
    while( clock() < end_clock )
    {
      hb_idleState();
    }
    hb_idleReset();

}
#endif

/* *********************************************************************** */
// Copied from gtwin

static void HB_GT_FUNC(gt_wNtTone( double dFreq, double dDurat, double dTick ))
{
  clock_t end_clock;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_wNtTone(%lf, %lf, %lf)", dFreq, dDurat, dTick));

    /* Clipper ignores Tone() requests if Frequency is less than
       < 20 hz (and so should we) to maintain compatibility .. */

    if ( dFreq > 20.0 )
    {
       Beep( (ULONG) dFreq, (ULONG) dDurat );
    }

    /* Delay (1) clock tick, just like Clipper .. */

    end_clock = clock() + ( clock_t ) ( dTick );
    while( clock() < end_clock )
    {
      hb_idleState();
    }
    hb_idleReset();

}

/* *********************************************************************** */
// Copied from gtwin

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
    double dMillisecs;
    OSVERSIONINFO osv;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

    /* The conversion from Clipper timer tick units to
       milliseconds is * 1000.0 / 18.2. */

    dDuration = HB_MIN( HB_MAX( 1, dDuration ), ULONG_MAX );

    if( dDuration > 0 )
    {
      double dTick = (double) ( CLOCKS_PER_SEC / 18.2 );

      dMillisecs = dDuration * dTick;   /* milliseconds */

      /* What version of Windows are you running? */
      osv.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
      GetVersionEx(&osv);

      /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
      if (osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
      {
         #if defined(__BORLANDC__) || defined( _MSC_VER )
            HB_GT_FUNC(gt_w9xTone( HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 ),
                                   dMillisecs, dTick ));
         #else
            HB_GT_FUNC(gt_wNtTone( HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 ),
                                   dMillisecs, dTick ));
         #endif
      }

      /* If Windows NT or NT2k, use wNtTone, which provides TONE()
         reset sequence support (new) */
      else if (osv.dwPlatformId == VER_PLATFORM_WIN32_NT)
      {
        /* We pass the Millisecond converted value here .. */
        HB_GT_FUNC(gt_wNtTone( HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 ),
                   dMillisecs, dTick ));
      }
   }
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Init( void ))
{
  hb_wvt_gtSetMouseX(0);
  hb_wvt_gtSetMouseY(0);
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Exit( void ))
{
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(mouse_IsPresent( void ))
{
   return( TRUE);
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Show( void ))
{
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Hide( void ))
{
}

/* *********************************************************************** */

int HB_GT_FUNC(mouse_Col( void ))
{
  return( hb_wvt_gtGetMouseX());
}

/* *********************************************************************** */

int HB_GT_FUNC(mouse_Row( void ))
{
  return( hb_wvt_gtGetMouseY());
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_SetPos( int iRow, int iCol ))
{
  hb_wvt_gtSetMouseX(iRow);
  hb_wvt_gtSetMouseY(iCol);
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(mouse_IsButtonPressed( int iButton ))
{
  BOOL bReturn = FALSE;
  if (iButton == 0)
  {
    bReturn =GetKeyState(VK_LBUTTON) & 0x8000;
  }
  else if (iButton== 1)
  {
    bReturn =GetKeyState(VK_RBUTTON) & 0x8000;
  }
  else if (iButton == 2)
  {
    bReturn =GetKeyState(VK_MBUTTON) & 0x8000;
  }
  return( bReturn);
}

/* *********************************************************************** */

int HB_GT_FUNC(mouse_CountButton( void ))
{
  return(GetSystemMetrics(SM_CMOUSEBUTTONS)) ;
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight ))
{
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight ))
{
   HB_SYMBOL_UNUSED( piTop );
   HB_SYMBOL_UNUSED( piLeft );
   HB_SYMBOL_UNUSED( piBottom );
   HB_SYMBOL_UNUSED( piRight );
}

/****** WVT specific functions *******/

static USHORT hb_wvt_gtCalcPixelHeight(void)
{
  return(_s.PTEXTSIZE.y*_s.ROWS);
}
static USHORT hb_wvt_gtCalcPixelWidth(void)
{
  return(_s.PTEXTSIZE.x*_s.COLS);
}

static BOOL hb_wvt_gtAllocSpBuffer(USHORT col, USHORT row)
{
  BOOL bRet = TRUE;
  _s.COLS = col;
  _s.ROWS = row;
  _s.BUFFERSIZE = col * row * sizeof(char);
  _s.pBuffer =  _s.byBuffer ;
  _s.pAttributes = _s.byAttributes;
  memset( _s.pBuffer, ' ', _s.BUFFERSIZE );
  memset( _s.pAttributes,_s.background, _s.BUFFERSIZE );
  return(bRet);
}

static BOOL hb_wvt_gtInitWindow(HWND hWnd, USHORT col, USHORT row)
{
  BOOL bRet = hb_wvt_gtAllocSpBuffer(col, row);
  hb_wvt_gtResetWindowSize(hWnd);
  return(bRet);
}

static BOOL hb_wvt_gtValidWindowSize(int rows, int cols, HFONT hFont)
{
  HDC hdc;
  HFONT hOldFont ;
  USHORT width, height, maxWidth, maxHeight;
  TEXTMETRIC tm;
  RECT rcWorkArea;

  SystemParametersInfo(SPI_GETWORKAREA,0, &rcWorkArea, 0);

  maxWidth = (rcWorkArea.right - rcWorkArea.left);
  maxHeight = (rcWorkArea.bottom - rcWorkArea.top);

  hdc = GetDC(_s.hWnd);
  hOldFont = (HFONT) SelectObject(hdc, hFont);
  GetTextMetrics(hdc, &tm);
  SelectObject(hdc, hOldFont); // Put old font back
  ReleaseDC(_s.hWnd, hdc);

  width= tm.tmAveCharWidth * cols ;  // Total pixel width this setting would take
  height=tm.tmHeight * rows;         // Total pixel height this setting would take
  return((width <= maxWidth) && (height <= maxHeight) );
}

static void hb_wvt_gtResetWindowSize(HWND hWnd)
{
  HDC hdc;
  HFONT hFont, hOldFont ;
  USHORT diffWidth, diffHeight;
  USHORT height, width;
  RECT wi, ci;
  TEXTMETRIC tm;
  HMENU hMenu;
  RECT rcWorkArea;
  int n;

  // set the font and get it's size to determine the size of the client area
  // for the required number of rows and columns

  hdc = GetDC(hWnd);
  hFont = hb_wvt_gtGetFont(_s.fontFace, _s.fontHeight, _s.fontWidth, _s.fontWeight, _s.fontQuality, _s.CodePage);
  _s.hFont = hFont ;
  hOldFont = (HFONT) SelectObject(hdc, hFont);
  if (hOldFont)
  {
    DeleteObject(hOldFont);
  }
  GetTextMetrics(hdc, &tm);
  SetTextCharacterExtra(hdc,0); // do not add extra char spacing even if bold
  ReleaseDC(hWnd, hdc);

  // we will need to use the font size to handle the transformations from
  // row column space in the future, so we keep it around in a static!
  //
  _s.PTEXTSIZE.x = tm.tmAveCharWidth; // For fixed FONT should == tm.tmMaxCharWidth
  _s.PTEXTSIZE.y = tm.tmHeight;       //     but seems to be a problem on Win9X so
                                      //     assume proportional fonts always for Win9X
  if ( _s.Win9X || (tm.tmPitchAndFamily & TMPF_FIXED_PITCH) || (_s.PTEXTSIZE.x != tm.tmMaxCharWidth))
  {
    _s.FixedFont = FALSE;
  }
  else
  {
    _s.FixedFont = TRUE ;
  }

  for(n=0 ; n< _s.COLS ; n++) // _s.FixedSize[] is used by ExtTextOut() to emulate
  {                           //          fixed font when a proportional font is used
    _s.FixedSize[n]= _s.PTEXTSIZE.x;
  }

  // resize the window to get the specified number of rows and columns
  height = hb_wvt_gtCalcPixelHeight();
  width = hb_wvt_gtCalcPixelWidth();


  GetWindowRect( hWnd, &wi);
  GetClientRect( hWnd, &ci);

  diffWidth = (wi.right - wi.left) - (ci.right);
  diffHeight = (wi.bottom - wi.top) - (ci.bottom);
  width  += diffWidth ;
  height += diffHeight;

  // Centre the window within the CLIENT area on the screen
  //                   but only if _s.CentreWindow == TRUE
  if (_s.CentreWindow && SystemParametersInfo(SPI_GETWORKAREA,0, &rcWorkArea, 0))
  {
    wi.left = rcWorkArea.left + (((rcWorkArea.right-rcWorkArea.left) - (width))/2) ;
    wi.top  = rcWorkArea.top + (((rcWorkArea.bottom-rcWorkArea.top) - (height))/2) ;
  }
  SetWindowPos(hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER);
}

static LRESULT CALLBACK hb_wvt_gtWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  static BOOL lMouseDown = FALSE, rMouseDown= FALSE;
  static BOOL bIgnoreWM_SYSCHAR = FALSE ;
  BOOL bRet;

  switch (message)
  {
    case WM_CREATE:
    {
      bRet = hb_wvt_gtInitWindow(hWnd, WVT_DEFAULT_COLS, WVT_DEFAULT_ROWS);
      return( bRet);
    }
    case WM_COMMAND: // handle menu items
    {
      hb_wvt_gtHandleMenuSelection((int) LOWORD(wParam));
      return(0);
    }
    case WM_PAINT:
    {
      PAINTSTRUCT ps;
      HDC hdc;
      USHORT irow;
      RECT updateRect, rcRect;


      GetUpdateRect(hWnd, &updateRect, FALSE);
      /* WARNING!!!
       * the GetUpdateRect call MUST be made BEFORE the BeginPaint call, since
       * BeginPaint resets the update rectangle - don't move it or nothing is drawn!
       */
      hdc = BeginPaint(hWnd, &ps);
      SelectObject(hdc, _s.hFont);

      /*
       * using the update rect, determine which rows and columns of text
       * to paint, and do so
       */
      if (_s.pBuffer != NULL && _s.pAttributes != NULL)
      {
        int colStart, colStop, rowStart, rowStop;

        // need to account for truncation in conversion
        // i.e. redraw any 'cell' partially covered...
        rcRect = hb_wvt_gtGetColRowFromXYRect(updateRect);
        rowStart = max(0, rcRect.top-1);
        rowStop = min(_s.ROWS, rcRect.bottom+1);
        colStart = max(0, rcRect.left -1);
        colStop = min(_s.COLS, rcRect.right+1);
        for ( irow = rowStart; irow <rowStop; irow++)
        {
          USHORT icol, index, startIndex, startCol, len;
          BYTE oldAttrib, attrib;
          icol = colStart;
          index = hb_wvt_gtGetIndexForTextBuffer(icol, irow);
          startIndex = index;
          startCol = icol;
          len = 0;
          oldAttrib = *(_s.pAttributes+index);
          /* attribute may change mid line...
          * so buffer up text with same attrib, and output it
          * then do next section with same attrib, etc
          */
          while (icol < colStop)
          {
            if (index >= _s.BUFFERSIZE )
            {
              break;
            }
            attrib = *(_s.pAttributes+index);
            if (attrib != oldAttrib)
            {
              hb_wvt_gtSetColors(hdc, oldAttrib);
              hb_wvt_gtTextOut(hdc, startCol, irow, (char const *) _s.pBuffer+startIndex, len);
              oldAttrib = attrib;
              startIndex = index;
              startCol = icol;
              len = 0;

            }
            icol++;
            len++;
            index++;
          }
          hb_wvt_gtSetColors(hdc, oldAttrib);
          hb_wvt_gtTextOut(hdc, startCol, irow, (char const *) _s.pBuffer+startIndex, len);
        }
      }
      EndPaint(hWnd, &ps);
#ifdef WVT_DEBUG
  printf("\nPuts(%d), Scroll(%d), Paint(%d), SetFocus(%d), KillFocus(%d) ",nCountPuts, nCountScroll, ++nCountPaint, nSetFocus, nKillFocus) ;
#endif
      return(0);
    }
    case WM_MY_UPDATE_CARET:
    {
      hb_wvt_gtSetCaretPos();
      return(0);
    }
    case WM_SETFOCUS:
    {
#ifdef WVT_DEBUG
  nSetFocus++;
#endif
      lMouseDown = FALSE;
      rMouseDown = FALSE;
      // create and show the caret
      // create an underline caret of height - _s.CaretSize
      _s.CaretExist = CreateCaret(hWnd, (HBITMAP) NULL, _s.PTEXTSIZE.x, _s.CaretSize);
      if (_s.CaretExist && _s.displayCaret)
      {
        hb_wvt_gtSetCaretPos();
        ShowCaret(hWnd);
      }
      return(0);
    }
    case WM_KILLFOCUS:
#ifdef WVT_DEBUG
  nKillFocus++;
#endif
      lMouseDown = FALSE;
      rMouseDown = FALSE;
      if (_s.CaretExist )
      {
        DestroyCaret();
        _s.CaretExist = FALSE ;
      }
      return(0);
    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
    {
      BOOL bAlt     = GetKeyState(VK_MENU) & 0x8000;
      bIgnoreWM_SYSCHAR= FALSE;
      switch (wParam)
      {
        case VK_LEFT:
          hb_wvt_gtTranslateKey(K_LEFT, K_LEFT, K_ALT_LEFT, K_CTRL_LEFT);
          break;
        case VK_RIGHT:
          hb_wvt_gtTranslateKey(K_RIGHT, K_RIGHT, K_ALT_RIGHT, K_CTRL_RIGHT);
          break;
        case VK_UP:
          hb_wvt_gtTranslateKey(K_UP, K_UP, K_ALT_UP, K_CTRL_UP);
          break;
        case VK_DOWN:
          hb_wvt_gtTranslateKey(K_DOWN, K_DOWN, K_ALT_DOWN, K_CTRL_DOWN);
          break;
        case VK_HOME:
          hb_wvt_gtTranslateKey(K_HOME, K_HOME, K_ALT_HOME, K_CTRL_HOME);
          break;
        case VK_END:
          hb_wvt_gtTranslateKey(K_END, K_END, K_ALT_END, K_CTRL_END);
          break;
        case VK_DELETE:
          hb_wvt_gtTranslateKey(K_DEL, K_DEL, K_ALT_DEL, K_CTRL_DEL);
          break;
        case VK_INSERT:
          hb_wvt_gtTranslateKey(K_INS, K_INS, K_ALT_INS, K_CTRL_INS);
          break;
        case VK_PRIOR:
          hb_wvt_gtTranslateKey(K_PGUP, K_PGUP, K_ALT_PGUP, K_CTRL_PGUP);
          break;
        case VK_NEXT:
          hb_wvt_gtTranslateKey(K_PGDN, K_PGDN, K_ALT_PGDN, K_CTRL_PGDN);
          break;
        case VK_F1:
          hb_wvt_gtTranslateKey(K_F1, K_SH_F1, K_ALT_F1, K_CTRL_F1);
          break;
        case VK_F2:
          hb_wvt_gtTranslateKey(K_F2, K_SH_F2, K_ALT_F2, K_CTRL_F2);
          break;
        case VK_F3:
          hb_wvt_gtTranslateKey(K_F3, K_SH_F3, K_ALT_F3, K_CTRL_F3);
          break;
        case VK_F4:
        {
          if (_s.AltF4Close && bAlt)
          {
            return(DefWindowProc(hWnd, message, wParam, lParam));
          }
          else
          {
            hb_wvt_gtTranslateKey(K_F4, K_SH_F4, K_ALT_F4, K_CTRL_F4);
          }
          break;
        }
        case VK_F5:
          hb_wvt_gtTranslateKey(K_F5, K_SH_F5, K_ALT_F5, K_CTRL_F5);
          break;
        case VK_F6:
          hb_wvt_gtTranslateKey(K_F6, K_SH_F6, K_ALT_F6, K_CTRL_F6);
          break;
        case VK_F7:
          hb_wvt_gtTranslateKey(K_F7, K_SH_F7, K_ALT_F7, K_CTRL_F7);
          break;
        case VK_F8:
          hb_wvt_gtTranslateKey(K_F8, K_SH_F8, K_ALT_F8, K_CTRL_F8);
          break;
        case VK_F9:
          hb_wvt_gtTranslateKey(K_F9, K_SH_F9, K_ALT_F9, K_CTRL_F9);
          break;
        case VK_F10:
          hb_wvt_gtTranslateKey(K_F10, K_SH_F10, K_ALT_F10, K_CTRL_F10);
          break;
        case VK_F11:
          hb_wvt_gtTranslateKey(K_F11, K_SH_F11, K_ALT_F11, K_CTRL_F11);
          break;
        case VK_F12:
          hb_wvt_gtTranslateKey(K_F12, K_SH_F12, K_ALT_F12, K_CTRL_F12);
          break;
        default:
        {
          BOOL bCtrl    = GetKeyState(VK_CONTROL) & 0x8000;
          BOOL bShift   = GetKeyState(VK_SHIFT) & 0x8000;
          int iScanCode = HIWORD(lParam) & 0xFF ;

          if (bCtrl && iScanCode == 76 ) // CTRL_VK_NUMPAD5)
          {
            hb_wvt_gtAddCharToInputQueue(KP_CTRL_5);
          }
          else if (iScanCode == 70 ) // Ctrl_Break key
          {
            hb_wvt_gtAddCharToInputQueue(HB_BREAK_FLAG); // Pretend Alt+C pressed
            bIgnoreWM_SYSCHAR= TRUE;
          }
          else if (bCtrl && iScanCode==53 && bShift)
          {
            hb_wvt_gtAddCharToInputQueue(K_CTRL_QUESTION);
          }
          else if ((bAlt || bCtrl) && (
              wParam==VK_MULTIPLY || wParam==VK_ADD || wParam== VK_SUBTRACT
              || wParam== VK_DIVIDE))
          {
            if (bAlt)
            {
              bIgnoreWM_SYSCHAR= TRUE;
            }
            switch (wParam)
            {
              case VK_MULTIPLY:
                hb_wvt_gtTranslateKey('*','*', KP_ALT_ASTERISK, KP_CTRL_ASTERISK);
                break;
              case VK_ADD:
                hb_wvt_gtTranslateKey('+','+', KP_ALT_PLUS, KP_CTRL_PLUS);
                break;
              case VK_SUBTRACT:
                hb_wvt_gtTranslateKey('-','-', KP_ALT_MINUS, KP_CTRL_MINUS);
                break;
              case VK_DIVIDE:
                hb_wvt_gtTranslateKey('/','/', KP_ALT_SLASH, KP_CTRL_SLASH);
                break;
            }
          }
          else
          {
            return(DefWindowProc(hWnd, message, wParam, lParam));
          }
        }
      }
      return(0);
    }
    case WM_CHAR:
    {
      int c = (int)wParam;
      if (!bIgnoreWM_SYSCHAR)
      {
        switch (c)
        {
          // handle special characters
          case VK_BACK:
            hb_wvt_gtTranslateKey(K_BS, K_BS, K_ALT_BS, K_CTRL_BS);
            break;
          case VK_TAB:
            hb_wvt_gtTranslateKey(K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB);
            break;
          case VK_RETURN:
            hb_wvt_gtTranslateKey(K_RETURN, K_RETURN, K_ALT_RETURN, K_CTRL_RETURN);
            break;
          case VK_ESCAPE:
            hb_wvt_gtAddCharToInputQueue(K_ESC);
            break;
          default:
            hb_wvt_gtAddCharToInputQueue(c);
            break;
        }
      }
      return(0);
    }
    case WM_SYSCHAR:
    {
      int c = toupper((int)wParam);
      if (!bIgnoreWM_SYSCHAR)
      {
        if (c >= 'A' && c <= 'Z')
        {
          hb_wvt_gtAddCharToInputQueue(_Alt_Alpha[c -'A']);
        }
        else if (c >= '0' && c <= '9')
        {
          hb_wvt_gtAddCharToInputQueue(_Alt_Num[wParam - '0']);
        }
        else if (wParam=='=' )
        {
          hb_wvt_gtAddCharToInputQueue(K_ALT_EQUALS);
        }
        else
        {
          hb_wvt_gtAddCharToInputQueue(wParam);
        }
      }
      return(0);
    }
    case WM_QUERYENDSESSION: // Closing down computer
    {
      /* if we have set a shutdown command return false,
       * so windows (and our app)doesn't shutdown
       * otherwise let the default handler take it
       */
      int iEvent = hb_wvt_gtGetShutdownEvent();
      if (iEvent)
      {
        hb_wvt_gtAddCharToInputQueue(iEvent);
        return(0);
      }
      break;
    }
    case WM_CLOSE:  // Clicked 'X' on system menu
    {
      /* if an event has been set then return it otherwise
         fake an Alt+C
      */
      int iEvent = hb_wvt_gtGetCloseEvent();
      if (!iEvent)
      {
        iEvent= HB_BREAK_FLAG ; // Pretend Alt+C pressed
      }
      hb_wvt_gtAddCharToInputQueue(iEvent);
      return(0);
    }
    case WM_QUIT:
    case WM_DESTROY:
      return(0);
    case WM_RBUTTONDOWN:
    case WM_LBUTTONDOWN:
    {
      if (!b_MouseEnable)
      {
        break;
      }
      else
      {
        POINT xy, colrow ;
        xy.x = LOWORD(lParam);
        xy.y = HIWORD(lParam);
        colrow = hb_wvt_gtGetColRowFromXY(xy.x, xy.y);
        hb_wvt_gtSetMouseX(colrow.x);
        hb_wvt_gtSetMouseY(colrow.y);
        if (message==WM_RBUTTONDOWN)
        {
          lMouseDown = TRUE;
        }
        else
        {
          rMouseDown = TRUE;
        }
        hb_wvt_gtAddCharToInputQueue(message==WM_LBUTTONDOWN ? K_LBUTTONDOWN : K_RBUTTONDOWN);
        return(0);
      }
    }
    case WM_RBUTTONUP:
    case WM_LBUTTONUP:
    {
      if ( !b_MouseEnable || (message==WM_LBUTTONUP && !lMouseDown) || (message==WM_RBUTTONUP && !rMouseDown))
      {
        break;
      }
      else
      {
        POINT xy, colrow ;
        xy.x = LOWORD(lParam);
        xy.y = HIWORD(lParam);
        colrow = hb_wvt_gtGetColRowFromXY(xy.x, xy.y);
        hb_wvt_gtSetMouseX(colrow.x);
        hb_wvt_gtSetMouseY(colrow.y);
        if (message==WM_RBUTTONUP)
        {
          lMouseDown = FALSE;
        }
        else
        {
          rMouseDown = FALSE;
        }
        hb_wvt_gtAddCharToInputQueue(message==WM_LBUTTONUP ? K_LBUTTONUP : K_RBUTTONUP);
        return(0);
      }
    }
  }
  return(DefWindowProc(hWnd, message, wParam, lParam));
}


static HWND hb_wvt_gtCreateWindow(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow)
{
  HWND hWnd;
  WNDCLASS wndclass;

  HB_SYMBOL_UNUSED( hPrevInstance );
  HB_SYMBOL_UNUSED( szCmdLine );
  InitCommonControls();

  wndclass.style         = CS_HREDRAW | CS_VREDRAW;
  wndclass.lpfnWndProc   = hb_wvt_gtWndProc;
  wndclass.cbClsExtra    = 0;
  wndclass.cbWndExtra    = 0;
  wndclass.hInstance     = hInstance;
  wndclass.hIcon         = NULL;
  wndclass.hCursor       = LoadCursor(NULL, IDC_ARROW);
  wndclass.hbrBackground = NULL;
  wndclass.lpszMenuName  = NULL;
  wndclass.lpszClassName = szAppName;

  if (! RegisterClass(&wndclass))
  {
    MessageBox(NULL, TEXT("Failed to register class."),
                szAppName, MB_ICONERROR);
    return(0);
  }

  hWnd = CreateWindow(szAppName,                          //classname
     TEXT("XHARBOUR_WVT"),                                     //window name
     WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX,  //style
     0,                                                   //x
     0,                                                   //y
     CW_USEDEFAULT,                                       //width
     CW_USEDEFAULT,                                       //height
     NULL,                                                //window parent
     NULL,                                                //menu
     hInstance,                                           //instance
     NULL);                                               //lpParam


  if (hWnd == NULL)
  {
    MessageBox(NULL, TEXT("Failed to create window."),
              TEXT("XHARBOUR_WVT"), MB_ICONERROR);
  }

  ShowWindow(hWnd, iCmdShow);

  UpdateWindow(hWnd);
  return( hWnd) ;
}

static DWORD hb_wvt_gtProcessMessages(void)
{
  MSG msg;
  while (PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ))
  {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
  return(msg.wParam);

}

static POINT hb_wvt_gtGetXYFromColRow( USHORT col, USHORT row)
{
  POINT xy;

  xy.x = (col)*_s.PTEXTSIZE.x;
  xy.y = (row)*_s.PTEXTSIZE.y;

  return(xy);
}

/*
 * get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 *
 */
static POINT hb_wvt_gtGetColRowFromXY( USHORT x, USHORT y)
{
  POINT colrow;

  colrow.x = (x/_s.PTEXTSIZE.x);
  colrow.y = (y/_s.PTEXTSIZE.y);

  return(colrow);
}

/*
 * return a rectangle with row and column data, corresponding to the XY pixel
 * coordinates
 * This works because we are using the FIXED system font
 *
 */
static RECT hb_wvt_gtGetColRowFromXYRect( RECT xy)
{
  RECT colrow;

  colrow.left = (xy.left/_s.PTEXTSIZE.x);
  colrow.top = (xy.top/_s.PTEXTSIZE.y);
  colrow.right = (xy.right/_s.PTEXTSIZE.x);
  colrow.bottom = (xy.bottom/_s.PTEXTSIZE.y);

  return(colrow);
}

/*
 * return a rectangle with the XY pixel coordinates corresponding to
 * the row and column data
 * This works because we are using the FIXED system font
 *
 */
static RECT hb_wvt_gtGetXYFromColRowRect( RECT colrow)
{
  RECT xy;

  xy.left = (colrow.left) * _s.PTEXTSIZE.x;
  xy.top = (colrow.top) * _s.PTEXTSIZE.y;
  xy.right = (colrow.right+1) * _s.PTEXTSIZE.x;
  xy.bottom = (colrow.bottom+1) * _s.PTEXTSIZE.y;


  return(xy);
}

/*
 * hb_wvt_gtSetCaretPos converts col and row to x and y (pixels) and calls
 * the Windows function SetCaretPos (with the expected coordinates)
 */
static BOOL hb_wvt_gtSetCaretPos(  )
{
  POINT xy;
  xy = hb_wvt_gtGetXYFromColRow(_s.caretPos.x, _s.caretPos.y);
  if ( _s.CaretSize > 0 )
  {
    xy.y += (_s.PTEXTSIZE.y - _s.CaretSize);
  }
  if (_s.CaretExist)
  {
    SetCaretPos( xy.x, xy.y);
  }
  return(TRUE);
}

/*
 * hb_wvt_gtValidateRow checks the row bounds for the caret, wrapping if indicated
 */
static void hb_wvt_gtValidateRow(void)
{
  if (_s.caretPos.y < 0 )
  {
    _s.caretPos.y = _s.ROWS-1;
    if (_s.caretPos.x > 0)
    {
      _s.caretPos.x--;
    }
    else
    {
      _s.caretPos.x = _s.COLS-1;
    }
  }
  else if (_s.caretPos.y >= _s.ROWS)
  {
    _s.caretPos.y = 0;
    if (_s.caretPos.x < _s.COLS-1)
    {
      _s.caretPos.x++;
    }
    else
    {
       _s.caretPos.x = 0;
    }
  }
}

/*
 * hb_wvt_gtValidateCol checks the column bounds for the caret, wrapping if indicated
 */
static void hb_wvt_gtValidateCol(void)
{
  if (_s.caretPos.x < 0)
  {
    _s.caretPos.x = _s.COLS-1;
    if (_s.caretPos.y > 0)
    {
      _s.caretPos.y--;
    }
    else
    {
      _s.caretPos.y = _s.ROWS-1;
    }
  }
  else if (_s.caretPos.x >= _s.COLS)
  {
    _s.caretPos.x = 0;
    if (_s.caretPos.y < _s.ROWS-1)
    {
      _s.caretPos.y++;
    }
    else
    {
      _s.caretPos.y = 0;
    }
  }
}

/*
 * hb_wvt_gtValidateCaret checks the bounds for the caret, wrapping if indicated
 * before setting the caret position on the screen
 */
static void hb_wvt_gtValidateCaret(void)
{
  RECT rect;
  POINT point;

  hb_wvt_gtValidateCol();
  hb_wvt_gtValidateRow();
  // send message to window to display updated caret
  SendMessage(_s.hWnd, WM_MY_UPDATE_CARET, 0, 0);
}

/*
 * hb_wvt_gtGetIndexForTextBuffer takes a row and column, and returns the appropriate
 * index into the screen Text buffer
 */
static USHORT hb_wvt_gtGetIndexForTextBuffer(USHORT col, USHORT row)
{
  return(row * _s.COLS + col);
}
 /*
  * hb_wvt_gtGetColRowForTextBuffer takes an index into the screen Text buffer
  * and returns the corresponding row and column
  */
static POINT hb_wvt_gtGetColRowForTextBuffer(USHORT index)
{
  POINT colrow;
  colrow.x = index % _s.COLS;
  colrow.y = index / _s.COLS;
  return(colrow);
}

/*
 * hb_wvt_gtTextOut converts col and row to x and y (pixels) and calls
 * the Windows function TextOut with the expected coordinates
 */
static BOOL hb_wvt_gtTextOut( HDC hdc,  USHORT col, USHORT row, LPCTSTR lpString, USHORT cbString  )
{
  BOOL Result ;
  POINT xy;
  if (cbString > _s.COLS) // make sure string is not too long
  {
    cbString = _s.COLS;
  }
  xy = hb_wvt_gtGetXYFromColRow(col, row);
  if (_s.FixedFont)
  {
    Result = TextOut(hdc, xy.x, xy.y, lpString, cbString);
  }
  else
  {
    Result = ExtTextOut(hdc, xy.x, xy.y, 0, NULL, lpString, cbString, _s.FixedSize) ;
  }
  return( Result ) ;
}

/* get for and background colours from attribute and set them for window
*/
static BOOL hb_wvt_gtSetColors(HDC hdc, BYTE attr)
{
  BOOL bResult= FALSE;
  if (attr != ' ')
  {
    int fore = attr & 0x000F;
    int back = (attr & 0x00F0)>>4;

    _s.foreground = _COLORS[fore];
    _s.background = _COLORS[back];

    SetTextColor(hdc, _s.foreground);
    SetBkColor(hdc, _s.background);
    bResult= TRUE;
  }
  return(bResult);
}

/* compute invalid rect in pixels, from row and col
*/
static void hb_wvt_gtSetInvalidRect(USHORT left, USHORT top, USHORT right, USHORT bottom)
{
  RECT rect;
  if (_s.InvalidateWindow)
  {
    rect.left = left;
    rect.top = top;
    rect.right = right;
    rect.bottom = bottom;
    rect = hb_wvt_gtGetXYFromColRowRect(rect);

    // check for wrapping
    rect.left = min(rect.left, rect.right);
    rect.top = min(rect.top, rect.bottom);

    rect.right = max(rect.left, rect.right);
    rect.bottom = max(rect.top, rect.bottom);
    if (_s.RectInvalid.left < 0 )
    {
      memcpy(&_s.RectInvalid, &rect, sizeof(RECT));
    }
    else
    {
      _s.RectInvalid.left = min(_s.RectInvalid.left, rect.left);
      _s.RectInvalid.top = min(_s.RectInvalid.top, rect.top);
      _s.RectInvalid.right = max(_s.RectInvalid.right, rect.right);
      _s.RectInvalid.bottom = max(_s.RectInvalid.bottom, rect.bottom);
    }
    hv_wvt_gtDoInvalidateRect() ;
  }
}

static void hv_wvt_gtDoInvalidateRect(void)
{
  if (HB_GT_FUNC(gt_DispCount()) <= 0 && (_s.RectInvalid.left != -1))
  {
    InvalidateRect(_s.hWnd, &_s.RectInvalid, TRUE);
    _s.RectInvalid.left = -1 ;
    hb_wvt_gtProcessMessages();
  }
}

static void hb_wvt_gtTranslateKey(int key, int shiftkey, int altkey, int controlkey)
{
  int nVirtKey = GetKeyState(VK_SHIFT);
  if (nVirtKey & 0x8000) // shift + key
  {
    hb_wvt_gtAddCharToInputQueue(shiftkey);
  }
  else
  {
    nVirtKey = GetKeyState(VK_MENU);
    if (nVirtKey & 0x8000) // alt + key
    {
      hb_wvt_gtAddCharToInputQueue(altkey);
    }
    else
    {
      nVirtKey = GetKeyState(VK_CONTROL);
      if (nVirtKey & 0x8000) // control + key
      {
        hb_wvt_gtAddCharToInputQueue(controlkey);
      }
      else //just key
      {
        hb_wvt_gtAddCharToInputQueue(key);
      }
    }
  }
}

// font stuff
/* use the standard fixed oem font, unless the caller has requested set size fonts
*/
static HFONT hb_wvt_gtGetFont(char * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage)
{
  HFONT hFont;
  if ( iHeight > 0)
  {
    LOGFONT logfont;
    logfont.lfEscapement = 0;
    logfont.lfOrientation = 0;
    logfont.lfWeight = iWeight ;
    logfont.lfItalic = 0;
    logfont.lfUnderline = 0;
    logfont.lfStrikeOut = 0;
    logfont.lfCharSet = iCodePage;// OEM_CHARSET;
    logfont.lfOutPrecision = 0;
    logfont.lfClipPrecision = 0;
    logfont.lfQuality = iQuality; // DEFAULT_QUALITY, DRAFT_QUALITY or PROOF_QUALITY
    logfont.lfPitchAndFamily = FIXED_PITCH+FF_MODERN; // all mapping depends on fixed width fonts!
    logfont.lfHeight = iHeight;
    logfont.lfWidth =  iWidth;
    strcpy(logfont.lfFaceName,pszFace);
    hFont = CreateFontIndirect(&logfont);
  }
  else
  {
//    hFont = GetStockObject(SYSTEM_FIXED_FONT);
    hFont = (HFONT) GetStockObject(OEM_FIXED_FONT);
  }
  return(hFont);

}

static void gt_hbInitStatics(void)
{
  OSVERSIONINFO osvi ;
  int i;
  _s.PTEXTSIZE.x = 0;
  _s.PTEXTSIZE.y = 0;
  _s.ROWS = WVT_DEFAULT_ROWS;
  _s.COLS = WVT_DEFAULT_COLS;
  _s.foreground = WHITE;
  _s.background = BLACK;
  _s.BUFFERSIZE = 0;
  _s.pAttributes = NULL;
  _s.pBuffer = NULL;
  _s.caretPos.x = 0;
  _s.caretPos.y = 0;
  _s.CaretExist = FALSE;
  _s.CaretSize  = 4;
  _s.mousePos.x = 0;
  _s.mousePos.y = 0;
  _s.hWnd = NULL;
  _s.keyPointerIn= 1;
  _s.keyPointerOut=0;
  _s.displayCaret = TRUE;
  _s.RectInvalid.left = -1 ;
  _s.fontHeight = 0;
  _s.fontWidth  = 0;
  _s.fontWeight = 0;
  _s.fontQuality = DEFAULT_QUALITY;
  strcpy(_s.fontFace,"Terminal");
  _s.closeEvent =  0;
  _s.shutdownEvent =  0;
  _s.LastMenuEvent = 0;
  _s.MenuKeyEvent    = 1024;
  _s.CentreWindow = TRUE; // Default is to always display window in centre of screen
  _s.CodePage     = GetACP() ;  // Set code page to default system

  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx (&osvi);
  _s.Win9X   = (osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS);
  _s.AltF4Close = FALSE;
  _s.InvalidateWindow= TRUE;
}

/*
 *  functions for handling the input queues for the mouse and keyboard
 */
static void hb_wvt_gtAddCharToInputQueue ( int data)
{
  int iNextPos;
  iNextPos = (_s.keyPointerIn >= WVT_CHAR_QUEUE_SIZE) ? 0 : _s.keyPointerIn+1 ;
  if (iNextPos != _s.keyPointerOut ) // Stop accepting characters once the buffer is full
  {
    _s.Keys[_s.keyPointerIn] = data ;
    _s.keyPointerIn = iNextPos ;
  }
}

static BOOL hb_wvt_gtGetCharFromInputQueue (int *c)
{
  int iNextPos;
  BOOL bRet = FALSE;
  *c = 0;
  iNextPos = (_s.keyPointerOut >= WVT_CHAR_QUEUE_SIZE) ? 0 : _s.keyPointerOut+1 ;
  if (iNextPos != _s.keyPointerIn )  // No more events in queue ??
  {
    *c = _s.Keys[iNextPos] ;
    _s.keyPointerOut = iNextPos ;
    bRet =  TRUE;
  }
  return(bRet);
}

static USHORT hb_wvt_gtGetMouseX (void)
{
  return(_s.mousePos.x);
}

static USHORT hb_wvt_gtGetMouseY (void)
{
  return(_s.mousePos.y);
}

static void hb_wvt_gtSetMouseX (USHORT ix)
{
  _s.mousePos.x = ix;
}

static void hb_wvt_gtSetMouseY (USHORT iy)
{
  _s.mousePos.y = iy;
}

/*
 * hb_wvt_gtSetStringInTextBuffer puts the string of the specified length into the TextBuffer at
 * the specified caret position
 * It then determines the invalid rectangle, so the string will be displayed
 */
static void hb_wvt_gtSetStringInTextBuffer(USHORT col, USHORT row, BYTE attr, BYTE *sBuffer, USHORT length)
{
  POINT end;
  USHORT index;


  // determine the index and put the string into the TextBuffer
  index = hb_wvt_gtGetIndexForTextBuffer(col, row);
  if (length + index <= _s.BUFFERSIZE)
  {
    memcpy((_s.pBuffer+index), sBuffer, length);
    if (attr != ' ') // if no attribute, don't overwrite
    {
      memset((_s.pAttributes+index), attr, length);
    }

    //determine bounds of rect around character to refresh
    end = hb_wvt_gtGetColRowForTextBuffer(index + (length -1) ); //location of last char
    hb_wvt_gtSetInvalidRect(col, row, end.x, end.y);
  }
}

static void hb_wvt_gtSetCaretOn(BOOL bOn)
{
  if (_s.CaretExist)
  {
    if (bOn)
    {
      hb_wvt_gtSetCaretPos();
      ShowCaret(_s.hWnd);
    }
    else
    {
      HideCaret(_s.hWnd);
    }
  }
  _s.displayCaret = bOn;
}



static void hb_wvt_gtHandleMenuSelection(int menuIndex)
{
  _s.LastMenuEvent = menuIndex ;
  hb_wvt_gtAddCharToInputQueue(_s.MenuKeyEvent);
}


static int hb_wvt_gtGetCloseEvent(void)
{
  return(_s.closeEvent);
}

static int hb_wvt_gtGetShutdownEvent(void)
{
  return(_s.shutdownEvent);
}

// Exported functions for API calls


BOOL HB_EXPORT hb_wvt_gtSetMenuKeyEvent(int iMenuKeyEvent)
{
  int iOldEvent;
  iOldEvent = _s.MenuKeyEvent ;
  if (iMenuKeyEvent)
  {
    _s.MenuKeyEvent = iMenuKeyEvent;
  }
  return(iOldEvent);
}

BOOL HB_EXPORT hb_wvt_gtSetCentreWindow(BOOL bCentre, BOOL bPaint)
{
  BOOL bWasCentre;
  bWasCentre = _s.CentreWindow ;
  _s.CentreWindow = bCentre;
  if ( bPaint)
  {
    hb_wvt_gtResetWindowSize(_s.hWnd) ;
  }
  return(bWasCentre);
}

void HB_EXPORT hb_wvt_gtResetWindow(void)
{
  hb_wvt_gtResetWindowSize(_s.hWnd) ;
}

BOOL HB_EXPORT hb_wvt_gtSetCodePage(int iCodePage)
{
  int iOldCodePage;
  iOldCodePage = _s.CodePage ;
  if (iCodePage)
  {
    _s.CodePage = iCodePage;
  }
  if (iOldCodePage != iCodePage)
  {
    hb_wvt_gtResetWindow();
  }
  return(iOldCodePage);
}

int HB_EXPORT hb_wvt_gtGetLastMenuEvent(void)
{
  return(_s.LastMenuEvent);
}

void HB_EXPORT hb_wvt_gtSetWindowTitle(char * title)
{
  SetWindowText(_s.hWnd, title);
}


DWORD HB_EXPORT hb_wvt_gtSetWindowIcon(int icon)
{
  HICON hIcon = LoadIcon(hb_hInstance, MAKEINTRESOURCE(icon));
  if (hIcon )
  {
    SendMessage(_s.hWnd, WM_SETICON, ICON_SMALL, (LPARAM)hIcon); // Set Title Bar ICON
    SendMessage(_s.hWnd, WM_SETICON, ICON_BIG, (LPARAM)hIcon); // Set Task List Icon
  }
  return((DWORD) hIcon) ;
}


int HB_EXPORT hb_wvt_gtGetWindowTitle(char *title, int length)
{
  return(GetWindowText(_s.hWnd, title, length));
}

BOOL HB_EXPORT hb_wvt_gtSetFont(char *fontFace, int height, int width, int Bold, int Quality )
{
  int size;
  BOOL bResult= TRUE ;
  HFONT hFont= hb_wvt_gtGetFont(fontFace, height, width, Bold, Quality, _s.CodePage);
  // make sure the font could actually be created
  if (hFont)
  {
    // make sure that the font  will fit inside the
    // window with the current _s.ROWS and _s.COLS setting
    if ( hb_wvt_gtValidWindowSize(_s.ROWS,_s.COLS, hFont))
    {
      _s.fontHeight = height;
      _s.fontWidth  = width;
      _s.fontWeight = Bold;
      _s.fontQuality = Quality;
      size = strlen(fontFace);
      if ((size> 0) && (size < LF_FACESIZE-1) )
      {
        strcpy( _s.fontFace, fontFace);
      }
      if (_s.hWnd)
      {
        // resize the window based on new fonts
        hb_wvt_gtResetWindowSize(_s.hWnd);
        // send message to force resize of caret
        SendMessage(_s.hWnd, WM_KILLFOCUS, 0, 0);
        SendMessage(_s.hWnd, WM_SETFOCUS, 0, 0);
      }
      bResult= TRUE;
    }
    DeleteObject(hFont);
  }
  return(bResult);
}


void HB_EXPORT hb_wvt_gtSetCloseEvent(int iEvent)
{
  _s.closeEvent = iEvent;
}


void HB_EXPORT hb_wvt_gtSetShutdownEvent(int iEvent)
{
  _s.shutdownEvent = iEvent;
}

HWND HB_EXPORT hb_wvt_gtGetWindowHandle(void)
{
  return(_s.hWnd);
}

void HB_EXPORT hb_wvt_gtPostMessage(int message)
{
  SendMessage(_s.hWnd, WM_CHAR,message, 0);
}

BOOL HB_EXPORT hb_wvt_gtSetWindowPos(int left, int top)
{
  RECT wi;
  GetWindowRect(_s.hWnd, &wi);
  return(SetWindowPos(_s.hWnd, NULL, left, top, (wi.right-wi.left)+1, (wi.bottom-wi.top)+1, SWP_NOZORDER));
}

BOOL HB_EXPORT hb_wvt_gtSetAltF4Close( BOOL bCanClose)
{
  BOOL bWas;
  bWas = _s.AltF4Close;
  _s.AltF4Close = bCanClose;
  return(bWas);
}

void HB_EXPORT hb_wvt_gtDoProcessMessages(void)
{
  hb_wvt_gtProcessMessages();
}

/* *********************************************************************** */

#ifdef HB_MULTI_GT

static void HB_GT_FUNC(gtFnInit( PHB_GT_FUNCS gt_funcs ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gtFnInit(%p)", gt_funcs));

    gt_funcs->Init                  = HB_GT_FUNC( gt_Init );
    gt_funcs->Exit                  = HB_GT_FUNC( gt_Exit );
    gt_funcs->GetScreenWidth        = HB_GT_FUNC( gt_GetScreenWidth );
    gt_funcs->GetScreenHeight       = HB_GT_FUNC( gt_GetScreenHeight );
    gt_funcs->Col                   = HB_GT_FUNC( gt_Col );
    gt_funcs->Row                   = HB_GT_FUNC( gt_Row );
    gt_funcs->SetPos                = HB_GT_FUNC( gt_SetPos );
    gt_funcs->AdjustPos             = HB_GT_FUNC( gt_AdjustPos );
    gt_funcs->IsColor               = HB_GT_FUNC( gt_IsColor );
    gt_funcs->GetCursorStyle        = HB_GT_FUNC( gt_GetCursorStyle );
    gt_funcs->SetCursorStyle        = HB_GT_FUNC( gt_SetCursorStyle );
    gt_funcs->DispBegin             = HB_GT_FUNC( gt_DispBegin );
    gt_funcs->DispEnd               = HB_GT_FUNC( gt_DispEnd );
    gt_funcs->DispCount             = HB_GT_FUNC( gt_DispCount );
    gt_funcs->Puts                  = HB_GT_FUNC( gt_Puts );
    gt_funcs->Replicate             = HB_GT_FUNC( gt_Replicate );
    gt_funcs->RectSize              = HB_GT_FUNC( gt_RectSize );
    gt_funcs->GetText               = HB_GT_FUNC( gt_GetText );
    gt_funcs->PutText               = HB_GT_FUNC( gt_PutText );
    gt_funcs->SetAttribute          = HB_GT_FUNC( gt_SetAttribute );
    gt_funcs->Scroll                = HB_GT_FUNC( gt_Scroll );
    gt_funcs->SetMode               = HB_GT_FUNC( gt_SetMode );
    gt_funcs->GetBlink              = HB_GT_FUNC( gt_GetBlink );
    gt_funcs->SetBlink              = HB_GT_FUNC( gt_SetBlink );
    gt_funcs->Version               = HB_GT_FUNC( gt_Version );
    gt_funcs->Box                   = HB_GT_FUNC( gt_Box );
    gt_funcs->BoxD                  = HB_GT_FUNC( gt_BoxD );
    gt_funcs->BoxS                  = HB_GT_FUNC( gt_BoxS );
    gt_funcs->HorizLine             = HB_GT_FUNC( gt_HorizLine );
    gt_funcs->VertLine              = HB_GT_FUNC( gt_VertLine );
    gt_funcs->Suspend               = HB_GT_FUNC( gt_Suspend );
    gt_funcs->Resume                = HB_GT_FUNC( gt_Resume );
    gt_funcs->PreExt                = HB_GT_FUNC( gt_PreExt );
    gt_funcs->PostExt               = HB_GT_FUNC( gt_PostExt );
    gt_funcs->OutStd                = HB_GT_FUNC( gt_OutStd );
    gt_funcs->OutErr                = HB_GT_FUNC( gt_OutErr );
    gt_funcs->Tone                  = HB_GT_FUNC( gt_Tone );
    gt_funcs->ExtendedKeySupport    = HB_GT_FUNC( gt_ExtendedKeySupport );
    gt_funcs->ReadKey               = HB_GT_FUNC( gt_ReadKey );
}

/* ********************************************************************** */

static void HB_GT_FUNC(mouseFnInit( PHB_GT_FUNCS gt_funcs ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_mouseFnInit(%p)", gt_funcs));

    gt_funcs->mouse_Init            = HB_GT_FUNC( mouse_Init );
    gt_funcs->mouse_Exit            = HB_GT_FUNC( mouse_Exit );
    gt_funcs->mouse_IsPresent       = HB_GT_FUNC( mouse_IsPresent );
    gt_funcs->mouse_Show            = HB_GT_FUNC( mouse_Show );
    gt_funcs->mouse_Hide            = HB_GT_FUNC( mouse_Hide );
    gt_funcs->mouse_Col             = HB_GT_FUNC( mouse_Col );
    gt_funcs->mouse_Row             = HB_GT_FUNC( mouse_Row );
    gt_funcs->mouse_SetPos          = HB_GT_FUNC( mouse_SetPos );
    gt_funcs->mouse_IsButtonPressed = HB_GT_FUNC( mouse_IsButtonPressed );
    gt_funcs->mouse_CountButton     = HB_GT_FUNC( mouse_CountButton );
    gt_funcs->mouse_SetBounds       = HB_GT_FUNC( mouse_SetBounds );
    gt_funcs->mouse_GetBounds       = HB_GT_FUNC( mouse_GetBounds );
}
/* ********************************************************************** */



/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             HB_GT_FUNC(gtFnInit), HB_GT_FUNC(mouseFnInit),
                             HB_GT_FUNC(wvtFnInit) };

HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( HB_GT_FUNC(_gt_Init_) )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( HB_GT_FUNC(_gt_Init_) )
#if defined(HB_STATIC_STARTUP) || ( (! defined(__GNUC__)) && (! defined(_MSC_VER)) )
   #pragma startup HB_GT_FUNC(_gt_Init_)
#endif

#endif  /* HB_MULTI_GT */


/* *********************************************************************** */
