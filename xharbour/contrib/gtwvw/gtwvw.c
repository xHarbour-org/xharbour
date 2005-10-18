/*
 * $Id: gtwvw.c,v 1.24 2005/10/11 12:55:07 marcosgambeta Exp $
 */

/*
 * GTWVW.C
 * Video subsystem for Win32 using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 *
 * GTWVW is initially created based on:
 *
 * =Id: gtwvt.c,v 1.60 2004/01/26 08:14:07 vouchcac Exp =
 *
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
 * the Free Software Foundation; either version 2, or (at your option )
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
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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

/*
* Individual authors:
* (C) 2003-2004 Giancarlo Niccolai <gc at niccolai dot ws>
*         Standard xplatform GT Info system,
*         Graphical object system and event system.
*         GTINFO() And GTO_* implementation.
*
* (C) 2004 Mauricio Abre <maurifull@datafull.com>
*         Cross-GT, multiplatform Graphics API
*
*/

#define HB_OS_WIN_32_USED

#define WINVER 0x0500
#define _WIN32_WINNT 0x0500

  #ifndef _WIN32_IE
    #define _WIN32_IE 0x0400
#endif

#include "hbgtwvw.h"

#include <windows.h>
#include <commctrl.h>

#if defined(__WATCOMC__)
  #include <conio.h>
#endif

/***

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

***/

/*-------------------------------------------------------------------*/

/* settable by user: ****************************************************/

static UINT s_uiPaintRefresh = 100;    /* milliseconds between timer check */

static BOOL s_bMainCoordMode = FALSE;  /* in this mode, all HB_GT_FUNC() uses Main Window's coordinate */

static BOOL s_bVertCaret     = FALSE;  /* if TRUE, caret is in Vertical style */

static BOOL s_bNOSTARTUPSUBWINDOW = FALSE;  /* if TRUE, subwindow will not be displayed during opening */
                                            /* use WVW_NOSTARTUPSUBWINDOW() to check/set it */

static BOOL s_bDefCentreWindow = FALSE;     /* default CentreWindow setting for subwindows */

static BOOL s_bDefHCentreWindow = FALSE;     /* default HCentreWindow setting for subwindows */
static BOOL s_bDefVCentreWindow = FALSE;     /* default VCentreWindow setting for subwindows */

static BYTE s_byDefLineSpacing = 0;    /* default line spacing */

static int  s_iDefLSpaceColor = -1;    /* if >= 0 this will be the color index
                                          for spacing between lines */

static LOGFONT s_lfPB = { 0 };       /* default font for pushbuttons */

static LOGFONT s_lfCB = { 0 };       /* default font for comboboxes */

/* read only by user ***/

/* for GTWVW private use: ***********************************************/
static BOOL s_bQuickSetMode = FALSE;   /* quick SetMode(), to reset maxrow() and maxcol() only */

static BOOL s_bFlashingWindow = FALSE; /* topmost window is flashing
                                          due to invalid input on other
                                          window */

static int  s_iScrolling = 0;           /* scrollbar is scrolling */
static int  s_iWrongButtonUp = 0;       /* number of consecutive scrollbar's WM_LBUTTONUP encountered by gtProcessMessages */
static int  s_iMaxWrongButtonUp = 500; /* max number of s_iWrongButtonUp. If it goes higher than this number,
                                           the scrollbar is forced to stop */

static TCHAR szAppName[] = TEXT( "xHarbour WVW" );
static TCHAR szSubWinName[] = TEXT( "xHarbour WVW subwindows" );
static BOOL  s_bSWRegistered = FALSE;

static USHORT s_usNumWindows;                    /*number of windows                         */
static USHORT s_usCurWindow = 0;                 /*current window handled by HB_GT_FUNC(...) */

static WIN_DATA *s_pWindows[ WVW_MAXWINDOWS ];   /*array of WIN_DATA                         */
static APP_DATA s_sApp;                          /*application wide vars                     */

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

#ifdef WVW_DEBUG
static int nCountPuts=0,nCountScroll=0, nCountPaint=0, nSetFocus=0, nKillFocus=0;
#endif

static int K_Ctrl[] = {
  K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G, K_CTRL_H,
  K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N, K_CTRL_O, K_CTRL_P,
  K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U, K_CTRL_V, K_CTRL_W, K_CTRL_X,
  K_CTRL_Y, K_CTRL_Z
  };

/*-------------------------------------------------------------------*/
/*                                                                   */
/*                  private functions declaration                    */
/*                                                                   */
HB_EXTERN_BEGIN
static void    gt_hbInitStatics( USHORT usWinNum, LPCTSTR lpszWinName, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 );
static HWND    hb_wvw_gtCreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow );
static BOOL    hb_wvw_gtInitWindow( WIN_DATA * pWindowData, HWND hWnd, USHORT col, USHORT row );
static void    hb_wvw_gtResetWindowSize( WIN_DATA * pWindowData, HWND hWnd );
static LRESULT CALLBACK hb_wvw_gtWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static BOOL    hb_wvw_gtAllocSpBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row );
static DWORD   hb_wvw_gtProcessMessages( WIN_DATA * pWindowData );

static BOOL    hb_wvw_gtValidWindowSize( WIN_DATA * pWindowData, int rows, int cols, HFONT hFont, int iWidth,
                                                              int *pmaxrows, int *pmaxcols );

static void    hb_wvw_gtSetCaretOn( WIN_DATA * pWindowData, BOOL bOn );
static BOOL    hb_wvw_gtSetCaretPos( WIN_DATA * pWindowData );
static void    hb_wvw_gtValidateCaret( WIN_DATA * pWindowData );

static USHORT  hb_wvw_gtGetMouseX( WIN_DATA * pWindowData );
static USHORT  hb_wvw_gtGetMouseY( WIN_DATA * pWindowData );
static void    hb_wvw_gtSetMouseX( WIN_DATA * pWindowData, USHORT ix );
static void    hb_wvw_gtSetMouseY( WIN_DATA * pWindowData, USHORT iy );

static void    hb_wvw_gtTranslateKey( int key, int shiftkey, int altkey, int controlkey );

static void    hb_wvw_gtSetInvalidRect( WIN_DATA * pWindowData, USHORT left, USHORT top, USHORT right, USHORT bottom );
static void    hb_wvw_gtDoInvalidateRect( WIN_DATA * pWindowData );

static void    hb_wvw_gtHandleMenuSelection( int );

static void    hb_wvw_gtUnreachedXY( WIN_DATA * pWindowData, int *cols, int *rows );
static POINT   hb_wvw_gtGetColRowFromXY( WIN_DATA * pWindowData, USHORT x, USHORT y );
static RECT    hb_wvw_gtGetColRowFromXYRect( WIN_DATA * pWIndowData, RECT xy );
static POINT   hb_wvw_gtGetColRowForTextBuffer( WIN_DATA * pWindowData, USHORT index );

static void    hb_wvw_gtValidateCol( WIN_DATA * pWindowData );
static void    hb_wvw_gtValidateRow( WIN_DATA * pWindowData );

static USHORT  hb_wvw_gtCalcPixelHeight( WIN_DATA * pWindowData );
static USHORT  hb_wvw_gtCalcPixelWidth( WIN_DATA * pWindowData );
static BOOL    hb_wvw_gtSetColors( WIN_DATA * pWindowData, HDC hdc, BYTE attr );

static HFONT   hb_wvw_gtGetFont( char * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage );

static BOOL    hb_wvw_gtTextOut( WIN_DATA * pWindowData, HDC hdc, USHORT col, USHORT row, LPCTSTR lpString,  USHORT cbString  );
static void    hb_wvw_gtSetStringInTextBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row, BYTE attr, BYTE *sBuffer, USHORT length );
static USHORT  hb_wvw_gtGetIndexForTextBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row );
static RECT    hb_wvw_gtGetXYFromColRowRect( WIN_DATA * pWindowData, RECT colrow );

static void    hb_wvw_gtCreateObjects( USHORT usWinNum );
static void    hb_wvw_gtKillCaret( WIN_DATA * pWindowData );
static void    hb_wvw_gtCreateCaret( WIN_DATA * pWindowData );
static void    hb_wvw_gtMouseEvent( WIN_DATA * pWindowData, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

static void    hb_wvw_gtCreateToolTipWindow( WIN_DATA * pWindowData );

/* multi-window related static functions: */
static void hb_wvw_gtWindowPrologue( void );
static void hb_wvw_gtWindowEpilogue( void );

static USHORT hb_wvw_gtOpenWindow( LPCTSTR lpszWinName, int usRow1, int usCol1, int usRow2, int usCol2, DWORD dwStyle, int iParentWin );
static void hb_wvw_gtCloseWindow( void );
static BOOL hb_wvw_gtAcceptingInput( void );
static BOOL hb_wvw_gtBufferedKey( LONG lKey );

static void hb_wvw_gtInputNotAllowed( USHORT usWinNum, UINT message, WPARAM wParam, LPARAM lParam );

static USHORT hb_wvw_gtRowOfs( USHORT usWinNum );
static USHORT hb_wvw_gtColOfs( USHORT usWinNum );
static BOOL hb_wvw_gtInWindow( USHORT usWinNum, USHORT usrow, USHORT uscol );
static USHORT hb_wvw_gtFindWindow(USHORT usRow, USHORT usCol);
static void hb_wvw_GTFUNCPrologue(BYTE byNumCoord, USHORT * pusRow1, USHORT * pusCol1,
                                                  USHORT * pusRow2, USHORT * pusCol2);
static void hb_wvw_GTFUNCEpilogue( void );
static void hb_wvw_HBFUNCPrologue(USHORT usWinNum,
                                  USHORT * pusRow1, USHORT * pusCol1,
                                  USHORT * pusRow2, USHORT * pusCol2);
static USHORT hb_wvw_gtSetCurWindow( USHORT usWinNum );

/* functions created in order to allow us operating MainCoord Mode: */
static void   hb_wvw_vmouse_Init( void );
static void   hb_wvw_vmouse_Exit( void );
static void   hb_wvw_vmouse_SetPos( WIN_DATA * pWindowData, USHORT usRow, USHORT usCol );
static USHORT hb_wvw_gt_usDispCount( WIN_DATA * pWindowData );
static void   hb_wvw_gt_vDispBegin( WIN_DATA * pWindowData );
static void   hb_wvw_gt_vDispEnd( WIN_DATA * pWindowData );
static SHORT hb_wvw_gt_sCol( WIN_DATA * pWindowData );
static SHORT hb_wvw_gt_sRow( WIN_DATA * pWindowData );
static void  hb_wvw_gt_vGetText( WIN_DATA * pWindowData, USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer );
static void  hb_wvw_gt_vPuts( WIN_DATA * pWindowData, USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen );
static void  hb_wvw_gt_vReplicate( WIN_DATA * pWindowData, USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen );
static void  hb_wvw_gt_vPutText( WIN_DATA * pWindowData, USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer );
static void  hb_wvw_gt_vSetAttribute( WIN_DATA * pWindowData, USHORT rowStart, USHORT colStart, USHORT rowStop, USHORT colStop, BYTE attr );
static BOOL  hb_wvw_gt_bSetMode( WIN_DATA * pWindowData, USHORT row, USHORT col );
static void  hb_wvw_gt_vxPutch( WIN_DATA * pWindowData, USHORT iRow, USHORT iCol, BYTE bAttr, BYTE bChar );
static USHORT hb_wvw_gt_usBox( WIN_DATA * pWindowData, SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * szBox, BYTE byAttr );
static void  hb_wvw_gt_vSetPos( WIN_DATA * pWindowData, SHORT sRow, SHORT sCol, SHORT sMethod );
static USHORT hb_wvw_gt_usGetScreenWidth( WIN_DATA * pWindowData );
static USHORT hb_wvw_gt_usGetScreenHeight( WIN_DATA * pWindowData );

static void   hb_wvw_InitPendingRect( WIN_DATA * pWindowData );
static void   hb_wvw_UpdatePendingRect( WIN_DATA * pWindowData, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 );

static BYTE   hb_wvw_LineHeight( WIN_DATA * pWindowData );
static void   hb_wvw_gtFillLineSpace( WIN_DATA * pWindowData, HDC hdc, USHORT startCol, USHORT irow, USHORT len, BYTE byAttrib );

static BITMAPINFO * PackedDibLoad (PTSTR szFileName);
static int PackedDibGetWidth (BITMAPINFO * pPackedDib);
static int PackedDibGetHeight (BITMAPINFO * pPackedDib);
static int PackedDibGetBitCount (BITMAPINFO * pPackedDib);

static int PackedDibGetInfoHeaderSize (BITMAPINFO * pPackedDib);
static int PackedDibGetColorsUsed (BITMAPINFO * pPackedDib);
static int PackedDibGetNumColors (BITMAPINFO * pPackedDib);
static int PackedDibGetColorTableSize (BITMAPINFO * pPackedDib);

#if 0
static RGBQUAD * PackedDibGetColorTablePtr (BITMAPINFO * pPackedDib);
static RGBQUAD * PackedDibGetColorTableEntry (BITMAPINFO * pPackedDib, int i);
#endif

static BYTE * PackedDibGetBitsPtr (BITMAPINFO * pPackedDib);

/* bitmap caching functions: */
static HBITMAP FindBitmapHandle(char * szFileName, int * piWidth, int * piHeight);
static void AddBitmapHandle(char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight);

/* control (eg. scrollbar) supporters: */
static HWND FindControlHandle(USHORT usWinNum, BYTE byCtrlClass, UINT uiCtrlid, byte * pbStyle);
static UINT FindControlId(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, byte * pbStyle);
static UINT LastControlId(USHORT usWinNum, BYTE byCtrlClass);
static void AddControlHandle(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid, PHB_ITEM phiCodeBlock, RECT rCtrl, RECT rOffCtrl, byte bStyle);

static CONTROL_DATA * GetControlData(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid);

static BOOL StoreControlProc(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, WNDPROC OldProc);
static WNDPROC GetControlProc(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl);

static int GetControlClass(USHORT usWinNum, HWND hWndCtrl);

static void RunControlBlock(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT message, WPARAM wParam, LPARAM lParam, BYTE bEventType );
static void ReposControls(USHORT usWinNum, BYTE byCtrlClass);

/*-------------------------------------------------------------------*/
/*                                                                   */
/* mouse initialization was made in cmdarg.c                         */
/*                                                                   */

/* set in mainwin.c                                                  */
/*                                                                   */
extern HANDLE  hb_hInstance;
extern HANDLE  hb_hPrevInstance;
extern int     hb_iCmdShow;

static USHORT  s_usCursorStyle;
static USHORT  s_usOldCurStyle;

static int s_iStdIn, s_iStdOut, s_iStdErr;

/* last updated GT object */
HB_GT_GOBJECT *last_gobject;

HB_EXTERN_END

extern BOOL    b_MouseEnable;

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                     GT Specific Functions                         */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ) )
{
    /* FSG: filename var for application name */
    PHB_FNAME pFileName;
    USHORT i;

    HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Init()" ) );

    /* stdin && stdout && stderr */
    s_iStdIn  = iFilenoStdin;
    s_iStdOut = iFilenoStdout;
    s_iStdErr = iFilenoStderr;

    last_gobject = NULL;

    s_usOldCurStyle = s_usCursorStyle = SC_NORMAL;

    s_usNumWindows = 0;
    for(i = 0; i < WVW_MAXWINDOWS; i++)
    {
      s_pWindows[i] = NULL;
    }

    hb_wvw_gtWindowPrologue( );
    gt_hbInitStatics(0, (LPCTSTR) szAppName, 0, 0, WVW_DEFAULT_ROWS-1, WVW_DEFAULT_COLS-1 );

    s_pWindows[0]->hWnd = hb_wvw_gtCreateWindow( ( HINSTANCE ) hb_hInstance, ( HINSTANCE ) hb_hPrevInstance,  "", hb_iCmdShow );

    if ( !s_pWindows[0]->hWnd )
    {
      /*  Runtime error
       */
      hb_errRT_TERM( EG_CREATE, 10001, "WINAPI CreateWindow() failed", "hb_gt_Init()", 0, 0 );
    }

    pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );

    hb_wvw_gtSetWindowTitle( 0, pFileName->szName );

    hb_xfree( pFileName );

    hb_wvw_gtCreateObjects(0);
    s_pWindows[0]->hdc = GetDC( s_pWindows[0]->hWnd );
    s_pWindows[0]->hCompDC = CreateCompatibleDC( s_pWindows[0]->hdc );

    if( b_MouseEnable )
    {
      hb_wvw_vmouse_Init();

      hb_wvw_gtCreateToolTipWindow(s_pWindows[0]);
    }

}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_Exit( void ) )
{
    int i;
    int j;
    WIN_DATA * pWindowData;
    BITMAP_HANDLE * pbh;
    CONTROL_DATA * pcd;

    HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Exit()" ) );

    for ( i = 0; i < WVW_DLGML_MAX; i++ )
    {
       if ( s_sApp.hDlgModeless[ i ] )
       {
          SendMessage( s_sApp.hDlgModeless[ i ], WM_CLOSE, 0, 0 );
       }
    }

    /* destroy all objects from all windows */

    for (j= (int) (s_usNumWindows-1); j >= 0; j--)
    {
       pWindowData = (WIN_DATA*) s_pWindows[ j ];

       if ( pWindowData->hWnd )
       {

         KillTimer( pWindowData->hWnd, WVW_ID_SYSTEM_TIMER );
         if ( s_sApp.pSymWVW_TIMER )
         {
           KillTimer( pWindowData->hWnd, WVW_ID_BASE_TIMER+j );
         }

         /* 20040921 IMPORTANT:
            All these PENs and BRUSHes deletions return OK,
            but GDI objects are reported as still in use by Task Manager.
            We now temporarily disable PENs and BRUSHes creation during
            window ppening.
            See also hb_wvw_gtCloseWindow().
            TODO: pls choose:
            (1) store PENs and BRUSHes as application-wide
            or
            (2) do the creation and deletion only when required
          */
         /* 20040923 choose #1 of above option
         DeleteObject( ( HPEN   ) pWindowData->penWhite );
         DeleteObject( ( HPEN   ) pWindowData->penWhiteDim );
         DeleteObject( ( HPEN   ) pWindowData->penBlack );
         DeleteObject( ( HPEN   ) pWindowData->penDarkGray );
         DeleteObject( ( HPEN   ) pWindowData->penGray );
         DeleteObject( ( HPEN   ) pWindowData->penNull );
         DeleteObject( ( HPEN   ) pWindowData->currentPen );
         DeleteObject( ( HBRUSH ) pWindowData->currentBrush );
         DeleteObject( ( HBRUSH ) pWindowData->diagonalBrush );
         DeleteObject( ( HBRUSH ) pWindowData->solidBrush );
         DeleteObject( ( HBRUSH ) pWindowData->wvwWhiteBrush );
         */

         DeleteObject( ( HFONT ) pWindowData->hFont );

         if ( pWindowData->hdc )
         {
            ReleaseDC( pWindowData->hWnd, pWindowData->hdc );
         }

         if ( pWindowData->hCompDC )
         {
            DeleteDC( pWindowData->hCompDC );
         }

         while (pWindowData->pcdCtrlList)
         {
           pcd     = pWindowData->pcdCtrlList->pNext;
           DestroyWindow (pWindowData->pcdCtrlList->hWndCtrl) ;

           if (pWindowData->pcdCtrlList->phiCodeBlock)
           {
              hb_itemRelease( pWindowData->pcdCtrlList->phiCodeBlock );

           }

           hb_xfree( pWindowData->pcdCtrlList );
           pWindowData->pcdCtrlList = pcd;
         }

         DestroyWindow( pWindowData->hWnd );

         if (pWindowData->hPBfont)
         {
           DeleteObject( ( HFONT ) pWindowData->hPBfont );
         }

         if (pWindowData->hCBfont)
         {
           DeleteObject( ( HFONT ) pWindowData->hCBfont );
         }

       }

       /* 20040923 delete all PENs and BRUSHes */
       DeleteObject( ( HPEN   ) s_sApp.penWhite );
       DeleteObject( ( HPEN   ) s_sApp.penWhiteDim );
       DeleteObject( ( HPEN   ) s_sApp.penBlack );
       DeleteObject( ( HPEN   ) s_sApp.penDarkGray );
       DeleteObject( ( HPEN   ) s_sApp.penGray );
       DeleteObject( ( HPEN   ) s_sApp.penNull );
       DeleteObject( ( HPEN   ) s_sApp.currentPen );
       DeleteObject( ( HBRUSH ) s_sApp.currentBrush );
       DeleteObject( ( HBRUSH ) s_sApp.diagonalBrush );
       DeleteObject( ( HBRUSH ) s_sApp.solidBrush );
       DeleteObject( ( HBRUSH ) s_sApp.wvwWhiteBrush );

       hb_wvw_gtWindowEpilogue(  );
    }

    if ( s_bSWRegistered )
    {
      UnregisterClass( szSubWinName,( HINSTANCE ) hb_hInstance );
    }

    UnregisterClass( szAppName,( HINSTANCE ) hb_hInstance );

    for ( i = 0; i < WVW_PICTURES_MAX; i++ )
    {
       if ( s_sApp.iPicture[ i ] )
       {
          hb_wvw_gtDestroyPicture( s_sApp.iPicture[ i ] );
       }
    }

    for ( i = 0; i < WVW_FONTS_MAX; i++ )
    {
       if ( s_sApp.hUserFonts[ i ] )
       {
          DeleteObject( s_sApp.hUserFonts[ i ] );
       }
    }

    for ( i = 0; i < WVW_PENS_MAX; i++ )
    {
       if ( s_sApp.hUserPens[ i ] )
       {
          DeleteObject( s_sApp.hUserPens[ i ] );
       }
    }

    if ( s_sApp.hMSImg32 )
    {
       FreeLibrary( s_sApp.hMSImg32 );
    }

    while (s_sApp.pbhBitmapList)
    {
      pbh     = s_sApp.pbhBitmapList->pNext;
      DeleteObject (s_sApp.pbhBitmapList->hBitmap) ;

      hb_xfree( s_sApp.pbhBitmapList );
      s_sApp.pbhBitmapList = pbh;
    }

    if( b_MouseEnable )
    {

       hb_wvw_vmouse_Exit();
    }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   returns the number of displayable columns                       */
/*                                                                   */
USHORT HB_GT_FUNC( gt_GetScreenWidth( void ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetScreenWidth()" ) );

  if (s_bMainCoordMode)
  {

    return( hb_wvw_gt_usGetScreenWidth( s_pWindows[ 0 ] ) );
  }
  else
  {
    return( hb_wvw_gt_usGetScreenWidth( s_pWindows[ s_usCurWindow ] ) );
  }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   returns the number of displayable rows                          */
/*                                                                   */
USHORT HB_GT_FUNC( gt_GetScreenHeight( void ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetScreenHeight()" ) );

  if (s_bMainCoordMode)
  {

    return( hb_wvw_gt_usGetScreenHeight( s_pWindows[ 0 ] ) );
  }
  else
  {
    return( hb_wvw_gt_usGetScreenHeight( s_pWindows[ s_usCurWindow ] ) );
  }
}

/*-------------------------------------------------------------------*/

SHORT HB_GT_FUNC( gt_Col( void ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Col()" ) );

  if (s_bMainCoordMode)
  {

    return( (SHORT) hb_wvw_gt_sCol( s_pWindows[ 0 ] ) );
  }
  else
  {
    return( (SHORT) hb_wvw_gt_sCol( s_pWindows[ s_usCurWindow ] ) );
  }
}

/*-------------------------------------------------------------------*/

SHORT HB_GT_FUNC( gt_Row( void ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Row()" ) );

  if (s_bMainCoordMode)
  {

    return( (SHORT) hb_wvw_gt_sRow( s_pWindows[ 0 ] ) );
  }
  else
  {
    return( (SHORT) hb_wvw_gt_sRow( s_pWindows[ s_usCurWindow ] ) );
  }
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_SetPos( SHORT sRow, SHORT sCol, SHORT sMethod ) )
{
  USHORT usRow = (USHORT) sRow, usCol = (USHORT) sCol;

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_SetPos( %hd, %hd, %hd )", sRow, sCol, sMethod ) );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(2, &usRow, &usCol, NULL, NULL);
  }

  hb_wvw_gt_vSetPos( s_pWindows[ s_usCurWindow ], (SHORT) usRow, (SHORT) usCol, sMethod );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }
}

/*-------------------------------------------------------------------*/

BOOL HB_GT_FUNC( gt_AdjustPos( BYTE * pStr, ULONG ulLen ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_AdjustPos( %s, %lu )", pStr, ulLen ) );

  HB_SYMBOL_UNUSED( pStr );
  HB_SYMBOL_UNUSED( ulLen );

  return( FALSE );
}

/*-------------------------------------------------------------------*/

BOOL HB_GT_FUNC( gt_IsColor( void ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_IsColor()" ) );

  return( TRUE );
}

/*-------------------------------------------------------------------*/

USHORT HB_GT_FUNC( gt_GetCursorStyle( void ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetCursorStyle()" ) );

  return( s_usCursorStyle );
}

/*-------------------------------------------------------------------*/

/*NOTE: works on TOPMOST window, NOT Current Window!
 *      (caret exists only in TOPMOST window)
 */

void HB_GT_FUNC( gt_SetCursorStyle( USHORT usStyle ) )
{
  BOOL bCursorOn= TRUE;
  WIN_DATA * pWindowData;
  USHORT  usFullSize;

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_SetCursorStyle( %hu )", usStyle ) );

  pWindowData = (WIN_DATA*) s_pWindows[ s_usNumWindows-1 ];
  usFullSize = s_bVertCaret ? pWindowData->PTEXTSIZE.x : pWindowData->PTEXTSIZE.y;

  s_usCursorStyle = usStyle;
  switch( usStyle )
  {
    case SC_NONE:
      pWindowData->CaretSize = 0 ;
      bCursorOn= FALSE;
      break ;
    case SC_INSERT:
      pWindowData->CaretSize = ( usFullSize / 2 ) ;
      break;
    case SC_SPECIAL1:
      pWindowData->CaretSize = usFullSize ;
      break;
    case SC_SPECIAL2:
      pWindowData->CaretSize = -( usFullSize / 2 ) ;
      break;
    case SC_NORMAL:
    default:
      pWindowData->CaretSize = 2 ;
      break;
  }

  if ( bCursorOn )
  {
    if (!s_bVertCaret)
    {
      s_sApp.CaretExist = CreateCaret( pWindowData->hWnd, ( HBITMAP ) NULL, pWindowData->PTEXTSIZE.x, pWindowData->CaretSize );
    }
    else
    {
      s_sApp.CaretExist = CreateCaret( pWindowData->hWnd, ( HBITMAP ) NULL, pWindowData->CaretSize, pWindowData->PTEXTSIZE.y );
    }
  }
  hb_wvw_gtSetCaretOn( pWindowData, bCursorOn );
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_DispBegin( void ) )
{

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_DispBegin()" ) );

  hb_wvw_gt_vDispBegin( s_pWindows[ s_usNumWindows-1 ] );
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_DispEnd( void ) )
{

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_DispEnd()" ) );

  hb_wvw_gt_vDispEnd( s_pWindows[ s_usNumWindows-1 ] );
}

/*-------------------------------------------------------------------*/

USHORT HB_GT_FUNC( gt_DispCount() )
{

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_DispCount()" ) );

  return( hb_wvw_gt_usDispCount( s_pWindows[ s_usNumWindows-1 ] ) );
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Puts( %hu, %hu, %d, %p, %lu )", usRow, usCol, ( int ) byAttr, pbyStr, ulLen ) );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(2, &usRow, &usCol, NULL, NULL);

    /* GTAPI didn't check string length properly in case of MainCoord Mode,
       so we must cut it here now */
    if (usCol+ulLen-1 > (ULONG) s_pWindows[ s_usCurWindow ]->COLS-1)
    {
      ulLen -= (usCol+ulLen-1) - (s_pWindows[ s_usCurWindow ]->COLS-1);
    }
  }

  hb_wvw_gt_vPuts( s_pWindows[ s_usCurWindow ], usRow, usCol, byAttr, pbyStr, ulLen );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Replicate( %hu, %hu, %i, %i, %lu )", usRow, usCol, byAttr, byChar, ulLen ) );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(2, &usRow, &usCol, NULL, NULL);
  }

  hb_wvw_gt_vReplicate( s_pWindows[ s_usCurWindow ], usRow, usCol, byAttr, byChar, ulLen );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }
}

/*-------------------------------------------------------------------*/

int HB_GT_FUNC( gt_RectSize( USHORT rows, USHORT cols ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_RectSize()" ) );

  return( rows * cols * 2 );
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_GetText( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetText( %hu, %hu, %hu, %hu, %p )", top, left, bottom, right, sBuffer ) );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(4, &top, &left, &bottom, &right);
  }

  hb_wvw_gt_vGetText( s_pWindows[ s_usCurWindow ], top, left, bottom, right, sBuffer );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_PutText( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_PutText( %hu, %hu, %hu, %hu, %p )", top, left, bottom, right, sBuffer ) );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(4, &top, &left, &bottom, &right);
  }

  hb_wvw_gt_vPutText( s_pWindows[ s_usCurWindow ], top, left, bottom, right, sBuffer );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_SetAttribute( USHORT rowStart, USHORT colStart, USHORT rowStop, USHORT colStop, BYTE attr ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_SetAttribute( %hu, %hu, %hu, %hu, %d", rowStart, colStart, rowStop, colStop, ( int ) attr ) );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(4, &rowStart, &colStart, &rowStop, &colStop);
  }

  hb_wvw_gt_vSetAttribute( s_pWindows[ s_usCurWindow ], rowStart, colStart, rowStop, colStop, attr );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    copied from gtwin...                                           */
/*                                                                   */

void HB_GT_FUNC( gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols ) )
{
  SHORT         usSaveRow, usSaveCol;

  BYTE ucBlank[ WVW_CHAR_BUFFER ], ucBuff[ WVW_CHAR_BUFFER * 2 ] ;
  BYTE * fpBlank ;
  BYTE * fpBuff  ;
  int           iLength = ( usRight - usLeft ) + 1;
  int           iCount, iColOld, iColNew, iColSize;
  BOOL          bMalloc = FALSE;

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Scroll( %hu, %hu, %hu, %hu, %d, %hd, %hd )", usTop, usLeft, usBottom, usRight, ( int ) byAttr, iRows, iCols ) );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(4, &usTop, &usLeft, &usBottom, &usRight);
  }

  if ( iLength > WVW_CHAR_BUFFER )
  { /* Avoid allocating memory if possible */
    fpBlank = ( BYTE * ) hb_xgrab( iLength );
    fpBuff  = ( BYTE * ) hb_xgrab( iLength * 2 );  /* *2 room for attribs */
    bMalloc = TRUE;
  }
  else
  {
    fpBlank = ucBlank ;
    fpBuff  = ucBuff  ;
  }

  memset( fpBlank, hb_ctGetClearB(), iLength );

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
  /* use the ScrollWindowEx() where possible ( Optimised for Terminal Server )
   * if both iCols & iRows are ZERO then the entire area is to be cleared and
   * there is no advantage in using ScrollWindowEx()
   */

  s_pWindows[ s_usCurWindow ]->InvalidateWindow = hb_wvw_gt_usDispCount( s_pWindows[ s_usCurWindow ] ) > 0 || ( !iRows && !iCols ) ;

  /* if s_pWindows[ s_usCurWindow ]->InvalidateWindow is FALSE it is used to stop
   *   HB_GT_FUNC( gt_Puts() ) & HB_GT_FUNC( gt_PutText() )
   *   from actually updating the screen. ScrollWindowEx() is used
   */
  if ( s_pWindows[ s_usCurWindow ]->InvalidateWindow )
  {
    hb_wvw_gt_vDispBegin( s_pWindows[ s_usCurWindow ] );
  }

  usSaveCol = hb_wvw_gt_sCol( s_pWindows[ s_usCurWindow ] );
  usSaveRow = hb_wvw_gt_sRow( s_pWindows[ s_usCurWindow ] );
  for( iCount = ( iRows >= 0 ? usTop : usBottom );
       ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
       ( iRows >= 0 ? iCount++ : iCount-- ) )
  {
      int iRowPos = iCount + iRows;

      /* Read the text to be scrolled into the current row */
      if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
      {
        hb_wvw_gt_vGetText( s_pWindows[ s_usCurWindow ], iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff );
      }

      /* Blank the scroll region in the current row */
      hb_wvw_gt_vPuts( s_pWindows[ s_usCurWindow ], iCount, usLeft, byAttr, fpBlank, iLength );

      /* Write the scrolled text to the current row */
      if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
      {
        hb_wvw_gt_vPutText( s_pWindows[ s_usCurWindow ], iCount, iColNew, iCount, iColNew + iColSize, fpBuff );
      }
  }

  hb_wvw_gt_vSetPos( s_pWindows[ s_usCurWindow ], usSaveRow, usSaveCol, HB_GT_SET_POS_AFTER );

  if ( s_pWindows[ s_usCurWindow ]->InvalidateWindow )
  {
    hb_wvw_gt_vDispEnd( s_pWindows[ s_usCurWindow ] );

  }
  else
  {
    RECT cr  = { 0 }, crInvalid = { 0 };

    cr.left   = usLeft   + ( iCols>0 ? 1 : 0 ) ;
    cr.top    = usTop    + ( iRows>0 ? 1 : 0 ) ;
    cr.right  = usRight  - ( iCols<0 ? 1 : 0 ) ;
    cr.bottom = usBottom - ( iRows<0 ? 1 : 0 ) ;

    cr = hb_wvw_gtGetXYFromColRowRect( s_pWindows[ s_usCurWindow ], cr );

    cr.top    -= ( s_pWindows[ s_usCurWindow ]->byLineSpacing / 2 );

    cr.bottom += ( s_pWindows[ s_usCurWindow ]->byLineSpacing / 2 );

    ScrollWindowEx( s_pWindows[ s_usCurWindow ]->hWnd, -iCols * s_pWindows[ s_usCurWindow ]->PTEXTSIZE.x, -iRows * hb_wvw_LineHeight( s_pWindows[ s_usCurWindow ] ), &cr, NULL, NULL, &crInvalid, 0 ) ;
    InvalidateRect( s_pWindows[ s_usCurWindow ]->hWnd, &crInvalid, FALSE );
    s_pWindows[ s_usCurWindow ]->InvalidateWindow = TRUE ;

  }
  if ( bMalloc )
  {
    hb_xfree( fpBlank );
    hb_xfree( fpBuff );
  }

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }

#ifdef WVW_DEBUG
  nCountScroll++;
#endif
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    resize the ( existing ) window                                 */
/*                                                                   */

BOOL HB_GT_FUNC( gt_SetMode( USHORT row, USHORT col ) )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_SetMode( %hu, %hu )", row, col ) );

   if (s_bQuickSetMode)
   { /*this is eg. done when we are closing window
      *we do nothing here, what we need is performed by GTAPI level
      *ie. setting its s_height and s_width (= maxrow() and maxcol() )
      */
     return( TRUE );
   }

   return( hb_wvw_gt_bSetMode( s_pWindows[ s_usCurWindow ], row, col) );
}

/*-------------------------------------------------------------------*/

BOOL HB_GT_FUNC( gt_GetBlink() )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_GetBlink()" ) );
  return( TRUE );
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_SetBlink( BOOL bBlink ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_SetBlink( %d )", ( int ) bBlink ) );
  HB_SYMBOL_UNUSED( bBlink );
}

/*-------------------------------------------------------------------*/

char * HB_GT_FUNC(gt_Version( int iType ))
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Version()" ) );

   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

  return( "xHarbour Terminal: Win32 buffered WVW" );
}

/*-------------------------------------------------------------------*/

#if 0
static void HB_GT_FUNC( gt_xPutch( USHORT iRow, USHORT iCol, BYTE bAttr, BYTE bChar ) )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_xPutch( %hu, %hu, %d, %i )", iRow, iCol, ( int ) bAttr, bChar ) );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(2, &iRow, &iCol, NULL, NULL);
  }

  hb_wvw_gt_vxPutch( s_pWindows[ s_usCurWindow ], iRow, iCol, bAttr, bChar );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }
}
#endif

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    copied from gtwin                                              */
/*                                                                   */
USHORT HB_GT_FUNC( gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * szBox, BYTE byAttr ) )
{
  USHORT usTop   = (Top   < 0 ? 0 : (USHORT) Top);
  USHORT usLeft  = (Left  < 0 ? 0 : (USHORT) Left);
  USHORT usBottom   = (Bottom   < 0 ? 0 : (USHORT) Bottom);
  USHORT usRight = (Right < 0 ? 0 : (USHORT) Right);
  USHORT usResult;

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(4, &usTop, &usLeft, &usBottom, &usRight);
  }

    usResult = hb_wvw_gt_usBox( s_pWindows[ s_usCurWindow ], usTop, usLeft, usBottom, usRight,
                              szBox, byAttr ) ;

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }

  return(usResult);
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   copied from gtwin                                               */
/*                                                                   */
USHORT HB_GT_FUNC( gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ) )
{
  USHORT usTop   = (Top   < 0 ? 0 : (USHORT) Top);
  USHORT usLeft  = (Left  < 0 ? 0 : (USHORT) Left);
  USHORT usBottom   = (Bottom   < 0 ? 0 : (USHORT) Bottom);
  USHORT usRight = (Right < 0 ? 0 : (USHORT) Right);
  USHORT usResult;

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(4, &usTop, &usLeft, &usBottom, &usRight);
  }

    usResult = hb_wvw_gt_usBox( s_pWindows[ s_usCurWindow ], usTop, usLeft, usBottom, usRight, pbyFrame, byAttr );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }

  return(usResult);
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   copied from gtwin                                               */
/*                                                                   */
USHORT HB_GT_FUNC( gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ) )
{
  USHORT usTop   = (Top   < 0 ? 0 : (USHORT) Top);
  USHORT usLeft  = (Left  < 0 ? 0 : (USHORT) Left);
  USHORT usBottom   = (Bottom   < 0 ? 0 : (USHORT) Bottom);
  USHORT usRight = (Right < 0 ? 0 : (USHORT) Right);
  USHORT usResult;

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(4, &usTop, &usLeft, &usBottom, &usRight);
  }

    usResult = hb_wvw_gt_usBox( s_pWindows[ s_usCurWindow ], usTop, usLeft, usBottom, usRight, pbyFrame, byAttr );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }

  return(usResult);
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   copied from gtwin                                               */
/*                                                                   */
USHORT HB_GT_FUNC( gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr ) )
{

  USHORT ret    = 1;

  USHORT sWidth;
  USHORT usRow   = (Row   < 0 ? 0 : (USHORT) Row);
  USHORT usLeft  = (Left  < 0 ? 0 : (USHORT) Left);
  USHORT usRight = (Right < 0 ? 0 : (USHORT) Right);

  if ( s_bMainCoordMode )
  {

    if (usLeft > usRight)
    {
      USHORT temp;
      temp = usLeft;
      usLeft = usRight;
      usRight = temp;
    }
    hb_wvw_GTFUNCPrologue(4, &usRow, &usLeft, NULL, &usRight);
  }

  sWidth = hb_wvw_gt_usGetScreenWidth( s_pWindows[ s_usCurWindow ] );

  if( usRow < sWidth )
  {

      if( usLeft >= sWidth )
      {
          usLeft = sWidth - 1;
      }

      if( usRight >= sWidth )
      {
          usRight = sWidth - 1;
      }
      if( usLeft < usRight )
      {

          hb_wvw_gt_vReplicate( s_pWindows[ s_usCurWindow ], usRow, usLeft, byAttr, byChar, usRight - usLeft + 1 );
      }
      else
      {

          hb_wvw_gt_vReplicate( s_pWindows[ s_usCurWindow ], usRow, usRight, byAttr, byChar, usLeft - usRight + 1 );
      }
      ret = 0;
  }

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }

  return( ret );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   copied from gtwin                                               */
/*                                                                   */
USHORT HB_GT_FUNC( gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ) )
{

    USHORT ret     = 1;
    USHORT sWidth ;
    USHORT sHeight;
    SHORT  Row;
    USHORT usCol   = (Col   < 0 ? 0 : (USHORT) Col);
    USHORT usTop  = (Top  < 0 ? 0 : (USHORT) Top);
    USHORT usBottom = (Bottom < 0 ? 0 : (USHORT) Bottom);

    if ( s_bMainCoordMode )
    {

      if (usTop > usBottom)
      {
        USHORT temp;
        temp = usTop;
        usTop = usBottom;
        usBottom = temp;
      }
      hb_wvw_GTFUNCPrologue(3, &usTop, &usCol, &usBottom, NULL);
    }

    sWidth  = hb_wvw_gt_usGetScreenWidth( s_pWindows[ s_usCurWindow ] );
    sHeight = hb_wvw_gt_usGetScreenHeight( s_pWindows[ s_usCurWindow ] );

    if( usCol < sWidth )
    {

        if( usTop >= sHeight )
        {
            usTop = sHeight - 1;
        }

        if( usBottom >= sHeight )
        {
            usBottom = sHeight - 1;
        }
        if( usTop <= usBottom )
        {
            Row = usTop;
        }
        else
        {
            Row    = usBottom;
            usBottom = usTop;
        }

        hb_wvw_gt_vDispBegin( s_pWindows[ s_usCurWindow ] );

        while( Row <= usBottom )
        {

            hb_wvw_gt_vxPutch( s_pWindows[ s_usCurWindow ], Row++, usCol, byAttr, byChar );
        }

        hb_wvw_gt_vDispEnd( s_pWindows[ s_usCurWindow ] );

        ret = 0;
    }

    if ( s_bMainCoordMode )
    {

      hb_wvw_GTFUNCEpilogue( );
    }

    return( ret );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    like gtwin                                                     */
/*                                                                   */
BOOL HB_GT_FUNC( gt_Suspend() )
{
  return( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   like gtwin                                                      */
/*                                                                   */
BOOL HB_GT_FUNC( gt_Resume() )
{
  return( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   like gtwin                                                      */
/*                                                                   */
BOOL HB_GT_FUNC( gt_PreExt() )
{
  return( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   like gtwin                                                      */
/*                                                                   */
BOOL HB_GT_FUNC( gt_PostExt() )
{
  return( TRUE );
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_OutStd( BYTE * pbyStr, ULONG ulLen ) )
{
  hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( gt_OutErr( BYTE * pbyStr, ULONG ulLen ) )
{
  hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
}

/*-------------------------------------------------------------------*/

int HB_GT_FUNC( gt_ExtendedKeySupport() )
{
    return( FALSE );  /* Only use standard Clipper hey handling */
}

/*-------------------------------------------------------------------*/

int HB_GT_FUNC( gt_ReadKey( HB_inkey_enum eventmask ) )
{
  int  c = 0;
  BOOL bKey;

  HB_TRACE( HB_TR_DEBUG, ( "hb_gt_ReadKey( %d )", ( int ) eventmask ) );

  HB_SYMBOL_UNUSED( eventmask );

  hb_wvw_gtProcessMessages( s_pWindows[ s_usCurWindow ]) ;

  bKey = hb_wvw_gtGetCharFromInputQueue( &c );

  return( bKey ? c : 0 );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   Copied from gtwin                                               */
/*                                                                   */

#if defined( __BORLANDC__ ) || defined( _MSC_VER ) || defined(__WATCOMC__) || defined(__MINGW32__)
static int hb_Inp9x( USHORT usPort )
{
  USHORT usVal;

  HB_TRACE( HB_TR_DEBUG, ( "hb_Inp9x( %hu )", usPort ) );

  #if defined( __BORLANDC__ ) || defined(__DMC__)
     _DX = usPort;
     __emit__( 0xEC );        /* ASM  IN AL, DX */
     __emit__( 0x32,0xE4 );   /* ASM XOR AH, AH */
     usVal = _AX;

  #elif defined( __XCC__ )

     __asm {
              mov   dx, usPort
              xor   ax, ax
              in    al, dx
              mov   usVal, ax
           }

  #elif defined( __MINGW32__ )
     __asm__ __volatile__ ("inb %w1,%b0":"=a" (usVal):"Nd" (usPort));

  #elif defined( __WATCOMC__ )

     usVal = inp( usPort );

  #else

     usVal = _inp( usPort );
  #endif

  return( usVal );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Copied from gtwin                                              */
/*                                                                   */
static int hb_Outp9x( USHORT usPort, USHORT usVal )
{
  HB_TRACE( HB_TR_DEBUG, ( "hb_Outp9x( %hu, %hu )", usPort, usVal ) );

  #if defined( __BORLANDC__ ) || defined(__DMC__)
    _DX = usPort;
    _AL = usVal;
    __emit__( 0xEE );        /* ASM OUT DX, AL */

   #elif defined( __XCC__ )

      __asm {
               mov   dx, usPort
               mov   ax, usVal
               out   dx, al
            }

   #elif defined( __MINGW32__ )

      __asm__ __volatile__ ("outb %b0,%w1": :"a" (usVal), "Nd" (usPort));

   #elif defined( __WATCOMC__ )

       outp( usPort, usVal );

  #else
     _outp( usPort, usVal );
  #endif

  return( usVal );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Copied from gtwin                                              */
/*                                                                   */
/* dDurat is in seconds */
static void HB_GT_FUNC(gt_w9xTone( double dFreq, double dDurat ))
{
    INT   uLSB,uMSB;
    ULONG lAdjFreq;

    HB_TRACE( HB_TR_DEBUG, ("hb_gt_w9xtone(%lf, %lf)", dFreq, dDurat ) );

    /* sync with internal clock with very small time period */
    hb_idleSleep( 0.01 );

    /* Clipper ignores Tone() requests (but delays anyway) if Frequency is
       less than < 20 hz (and so should we) to maintain compatibility .. */

    if ( dFreq >= 20.0 )
    {
      /* Setup Sound Control Port Registers and timer channel 2 */
      hb_Outp9x( 67, 182 ) ;

      lAdjFreq = ( ULONG ) ( 1193180 / dFreq ) ;

      if( ( LONG ) lAdjFreq < 0 )
         uLSB = lAdjFreq + 65536;
      else
         uLSB = lAdjFreq % 256;

      if( ( LONG ) lAdjFreq < 0 )
         uMSB = lAdjFreq + 65536;
      else
         uMSB = lAdjFreq / 256;

      /* set the frequency (LSB,MSB) */

      hb_Outp9x( 66, uLSB );
      hb_Outp9x( 66, uMSB );

      /* Get current Port setting */
      /* enable Speaker Data & Timer gate bits */
      /* (00000011B is bitmask to enable sound) */
      /* Turn on Speaker - sound Tone for duration.. */

      hb_Outp9x( 97, hb_Inp9x( 97 ) | 3 );

      hb_idleSleep( dDurat );

      /* Read back current Port value for Reset */
      /* disable Speaker Data & Timer gate bits */
      /* (11111100B is bitmask to disable sound) */
      /* Turn off the Speaker ! */

      hb_Outp9x( 97, hb_Inp9x( 97 ) & 0xFC );

    }
    else
    {
       hb_idleSleep( dDurat );
    }
}
#endif

/*-------------------------------------------------------------------*/
/*                                                                   */
/* dDurat is in seconds */
/*                                                                   */
static void HB_GT_FUNC( gt_wNtTone( double dFreq, double dDurat ) )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_wNtTone(%lf, %lf)", dFreq, dDurat ) );

    /* Clipper ignores Tone() requests (but delays anyway) if Frequency is
       less than < 20 hz.  Windows NT minimum is 37... */

    /* sync with internal clock with very small time period */
    hb_idleSleep( 0.01 );

    if ( dFreq >= 37.0 )
    {
       Beep( (ULONG) dFreq, (ULONG) ( dDurat * 1000 ) ); /* Beep wants Milliseconds */
    }
    else
    {
       hb_idleSleep( dDurat );
    }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/* dDuration is in 'Ticks' (18.2 per second) */
/*                                                                   */
void HB_GT_FUNC( gt_Tone( double dFrequency, double dDuration ) )
{
    OSVERSIONINFO osv;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

    /*
      According to the Clipper NG, the duration in 'ticks' is truncated to the
      interger portion  ... Depending on the platform, xHarbour allows a finer
      resolution, but the minimum is 1 tick (for compatibility)
     */
    /* Convert from ticks to seconds */
    dDuration  = ( HB_MIN( HB_MAX( 1.0, dDuration ), ULONG_MAX ) ) / 18.2;

    /* keep the frequency in an acceptable range */
    dFrequency =   HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );

    /* What version of Windows are you running? */
    osv.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
    GetVersionEx( &osv );

    /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
    if ( osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
    {

       #if defined( __BORLANDC__ ) || defined( _MSC_VER ) || defined( __WATCOMC__ )  || defined(__MINGW32__)
          HB_GT_FUNC( gt_w9xTone( dFrequency, dDuration ) );
       #else
          HB_GT_FUNC( gt_wNtTone( dFrequency, dDuration ) );
       #endif
    }

    /* If Windows NT or NT2k, use wNtTone, which provides TONE()
       reset sequence support (new) */
    else if ( osv.dwPlatformId == VER_PLATFORM_WIN32_NT )
    {
      HB_GT_FUNC( gt_wNtTone( dFrequency, dDuration ) );
    }
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( mouse_Init( void ) )
{
  hb_wvw_vmouse_Init();
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( mouse_Exit( void ) )
{
  hb_wvw_vmouse_Exit();
}

/*-------------------------------------------------------------------*/

BOOL HB_GT_FUNC( mouse_IsPresent( void ) )
{
   return( TRUE );
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( mouse_Show( void ) )
{
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( mouse_Hide( void ) )
{
}

/*-------------------------------------------------------------------*/

int HB_GT_FUNC( mouse_Col( void ) )
{

  if ( s_bMainCoordMode )
  {
    return( hb_wvw_gtGetMouseX( s_pWindows[ s_usNumWindows-1 ] ) + hb_wvw_gtColOfs( s_usNumWindows-1 ) );
  }
  else
  {
    return( hb_wvw_gtGetMouseX( s_pWindows[ s_usCurWindow ] ) );
  }
}

/*-------------------------------------------------------------------*/

int HB_GT_FUNC( mouse_Row( void ) )
{

  if ( s_bMainCoordMode )
  {
    return( hb_wvw_gtGetMouseY( s_pWindows[ s_usNumWindows-1 ] ) + hb_wvw_gtRowOfs( s_usNumWindows-1 ));
  }
  else
  {
    return( hb_wvw_gtGetMouseY( s_pWindows[ s_usCurWindow ] ) );
  }
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( mouse_SetPos( int iRow, int iCol ) )
{
  USHORT usRow = (iRow   < 0 ? 0 : (USHORT) iRow);
  USHORT usCol = (iCol  < 0 ? 0 : (USHORT) iCol);

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCPrologue(2, &usRow, &usCol, NULL, NULL);
  }

  hb_wvw_vmouse_SetPos( s_pWindows[ s_usCurWindow ], usRow, usCol );

  if ( s_bMainCoordMode )
  {

    hb_wvw_GTFUNCEpilogue( );
  }

}

/*-------------------------------------------------------------------*/

BOOL HB_GT_FUNC( mouse_IsButtonPressed( int iButton ) )
{
  BOOL bReturn = FALSE;

  if ( iButton == 0 )
  {
    bReturn = GetKeyState( VK_LBUTTON ) & 0x8000;
  }
  else if ( iButton== 1 )
  {
    bReturn = GetKeyState( VK_RBUTTON ) & 0x8000;
  }
  else if ( iButton == 2 )
  {
    bReturn = GetKeyState( VK_MBUTTON ) & 0x8000;
  }

  return( bReturn );
}

/*-------------------------------------------------------------------*/

int HB_GT_FUNC( mouse_CountButton( void ) )
{
  return( GetSystemMetrics( SM_CMOUSEBUTTONS ) ) ;
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight ) )
{
   HB_SYMBOL_UNUSED( iTop    );
   HB_SYMBOL_UNUSED( iLeft   );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight  );
}

/*-------------------------------------------------------------------*/

void HB_GT_FUNC( mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight ) )
{
   HB_SYMBOL_UNUSED( piTop    );
   HB_SYMBOL_UNUSED( piLeft   );
   HB_SYMBOL_UNUSED( piBottom );
   HB_SYMBOL_UNUSED( piRight  );
}

/* ************************** Clipboard support ********************************** */

void HB_GT_FUNC( gt_GetClipboard( char *szData, ULONG *pulMaxSize ) )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;

   UINT      uFormat = ( s_pWindows[s_usCurWindow]->CodePage == OEM_CHARSET ) ? CF_OEMTEXT : CF_TEXT;

   if ( !IsClipboardFormatAvailable(uFormat) )
   {
     *pulMaxSize = 0;
     return;
   }

   if (!OpenClipboard( NULL ))
   {
     *pulMaxSize = 0;
     return;
   }

   hglb = GetClipboardData(uFormat);
   if (hglb != NULL)
   {
      lptstr = (LPSTR) GlobalLock(hglb);
      if (lptstr != NULL)
      {
         ULONG ulLen = (ULONG) strlen( lptstr );
         if ( *pulMaxSize == 0 || *pulMaxSize > ulLen )
         {
            *pulMaxSize = ulLen;
         }

         if ( *pulMaxSize == 0 )
         {
            return;
         }

         memcpy( szData, lptstr, *pulMaxSize );

         szData[*pulMaxSize] = '\0';
         GlobalUnlock(hglb);
      }
   }
   CloseClipboard();
}

void HB_GT_FUNC( gt_SetClipboard( char *szData, ULONG ulSize ) )
{
   LPTSTR  lptstrCopy;

   HGLOBAL hglbCopy;
   UINT    uFormat = ( s_pWindows[s_usCurWindow]->CodePage == OEM_CHARSET ) ? CF_OEMTEXT : CF_TEXT;

/*  This poses problems when some other application copies a bitmap on the
    clipboard. The only way to set text to clipboard is made possible
    only if another application copies some text on the clipboard.

   if ( !IsClipboardFormatAvailable( CF_TEXT ) )
   {
     return;
   }
*/

   if ( ! OpenClipboard( NULL ) )
   {

     return;
   }
   EmptyClipboard();

   /* Allocate a global memory object for the text.
    */
   hglbCopy = GlobalAlloc( GMEM_MOVEABLE, ( ulSize+1 ) * sizeof( TCHAR ) );
   if ( hglbCopy == NULL )
   {
       CloseClipboard();
       return;
   }

   /* Lock the handle and copy the text to the buffer.
    */
   lptstrCopy = ( LPSTR ) GlobalLock( hglbCopy );

   memcpy( lptstrCopy, szData, ( ulSize+1 ) * sizeof( TCHAR ) );

   lptstrCopy[ ulSize+1 ] = ( TCHAR ) 0;
   GlobalUnlock( hglbCopy );

   /* Place the handle on the clipboard.
    */
   SetClipboardData( uFormat, hglbCopy );

   CloseClipboard();
}

ULONG HB_GT_FUNC( gt_GetClipboardSize( void ) )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;
   UINT      uFormat = ( s_pWindows[s_usCurWindow]->CodePage == OEM_CHARSET ) ? CF_OEMTEXT : CF_TEXT;
   int ret;

   if ( !IsClipboardFormatAvailable(uFormat) )
   {
     return 0;
   }

   if (!OpenClipboard( NULL ))
   {
     return 0;
   }

   hglb = GetClipboardData(uFormat);
   ret = 0;
   if (hglb != NULL)
   {
      lptstr = (LPSTR) GlobalLock(hglb);
      if (lptstr != NULL)
      {
         ret = strlen( lptstr );
         GlobalUnlock(hglb);
      }
   }
   CloseClipboard();
   return ret;

}

/* *********************************************************************** */

void HB_GT_FUNC( gt_ProcessMessages( void ) )
{
   hb_wvw_gtProcessMessages( s_pWindows[ s_usCurWindow ]) ;

}

/* *********************************************************************** */

/*WARNING: assume working on current window
 *NOTES: in MainCoord Mode current window is always the Main Window
 */
int HB_GT_FUNC( gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam ) )
{
   int iOldValue;
   WIN_DATA * pWindowData = s_pWindows[ s_usCurWindow ];

   switch ( iMsgType )
   {
      case GTI_ISGRAPHIC:
         return (int) TRUE;

      case GTI_MOUSESTATUS:
         iOldValue = ( int ) b_MouseEnable;
         if ( bUpdate )
         {
            b_MouseEnable = iParam;
         }
         return iOldValue;

      case GTI_FONTNAME:
         if ( bUpdate )
         {
            /* of current window */
            strcpy( pWindowData->fontFace, (char *) vpParam );
         }
         return ( int ) TRUE;

      case GTI_FONTSIZE:
         iOldValue = (int) pWindowData->PTEXTSIZE.y;
         if ( bUpdate )
         {
            HFONT hFont = hb_wvw_gtGetFont( pWindowData->fontFace, iParam, pWindowData->fontWidth, pWindowData->fontWeight, pWindowData->fontQuality, pWindowData->CodePage );
            /* make sure the font could actually be created */
            if ( hFont )
            {
               pWindowData->fontHeight = iParam;
               /* is the window already opened? */
               if ( pWindowData->hWnd )
               {
                 /* resize the window based on new fonts
                  */
                 hb_wvw_gtResetWindowSize( pWindowData, pWindowData->hWnd );

                 /* force resize of caret
                  */
                 hb_wvw_gtKillCaret( pWindowData );
                 hb_wvw_gtCreateCaret( pWindowData );
               }

               DeleteObject( hFont );
            }
         }
      return iOldValue;

      case GTI_FONTWIDTH:
         iOldValue = (int) pWindowData->PTEXTSIZE.x;
         if ( bUpdate )
         {
            /* store font status for next operation on fontsize */
            pWindowData->fontWidth = iParam;
         }
      return iOldValue;

      case GTI_FONTWEIGHT:
         switch( pWindowData->fontWeight )
         {
            case FW_THIN:
            case FW_EXTRALIGHT:
            case FW_LIGHT:
               iOldValue = GTI_FONTW_THIN;
            break;

            case FW_DONTCARE:
            case FW_NORMAL:
            case FW_MEDIUM:
               iOldValue = GTI_FONTW_NORMAL;
            break;

            case FW_SEMIBOLD:
            case FW_BOLD:
            case FW_EXTRABOLD:
            case FW_HEAVY:
               iOldValue = GTI_FONTW_BOLD;
            break;

            default:
               iOldValue = 0;
            break;
         }

         if ( bUpdate )
         {

            switch( iParam )
            {
               case GTI_FONTW_THIN:
                  pWindowData->fontWeight = FW_LIGHT;

               case GTI_FONTW_NORMAL:
                  pWindowData->fontWeight = FW_NORMAL;
               break;

               case GTI_FONTW_BOLD:
                  pWindowData->fontWeight = FW_BOLD;
            }
         }
      return iOldValue;

      case GTI_FONTQUALITY:
         switch( pWindowData->fontQuality )
         {
            case ANTIALIASED_QUALITY:
               iOldValue = GTI_FONTQ_HIGH;
            break;

            case DEFAULT_QUALITY:
            case DRAFT_QUALITY:
               iOldValue = GTI_FONTQ_NORMAL;
            break;

            case NONANTIALIASED_QUALITY:
            case PROOF_QUALITY:
               iOldValue = GTI_FONTQ_DRAFT;
            break;

            default:
               iOldValue = 0;
            break;
         }

         if ( bUpdate )
         {
            switch( iParam )
            {
               case GTI_FONTQ_HIGH:
                  pWindowData->fontQuality = ANTIALIASED_QUALITY;
               break;

               case GTI_FONTQ_NORMAL:
                  pWindowData->fontQuality = DEFAULT_QUALITY;
               break;

               case GTI_FONTQ_DRAFT:
                  pWindowData->fontQuality = DRAFT_QUALITY;
            }
         }
      return iOldValue;

      case GTI_SCREENHEIGHT:
         /*NOTE 20040618 currently not includes StatusBar and ToolBar, if any.
          *TODO          I Think it should return ALL window height, incl
          *              StatusBar and ToolBar
          *              ie. hb_wvw_gtCalcPixelHeight()
          *SEEALSO       hb_wvw_gtCalcPixelHeight()
          */

         /*NOTE 20040719 screenheight includes linespacing, if any */

         iOldValue = hb_wvw_LineHeight( pWindowData ) * pWindowData->ROWS;

         if ( bUpdate )
         {

            hb_wvw_gt_bSetMode( pWindowData, (USHORT) (iParam/hb_wvw_LineHeight( pWindowData )), pWindowData->COLS );
         }
      return iOldValue;

      case GTI_SCREENWIDTH:
         iOldValue = pWindowData->PTEXTSIZE.x * pWindowData->COLS;
         if ( bUpdate )
         {

            hb_wvw_gt_bSetMode( pWindowData, pWindowData->ROWS, (USHORT) (iParam/pWindowData->PTEXTSIZE.x) );
         }
      return iOldValue;

      case GTI_DESKTOPWIDTH:
      {
         RECT rDesk = { 0 };
         HWND hDesk;

         hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         return rDesk.right - rDesk.left;
      }

      case GTI_DESKTOPHEIGHT:
      {
         /*NOTE 20040618 currently includes StatusBar and ToolBar, if any.
          *TODO          Think: should it return chars area only?
          *SEEALSO       hb_wvw_gtCalcPixelHeight() - usSBHeight - usTBHeight
          */

         RECT rDesk = { 0 };
         HWND hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         return rDesk.bottom - rDesk.top;
      }

      case GTI_DESKTOPCOLS:
      {
         RECT rDesk = { 0 };
         HWND hDesk;

         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );

         return (rDesk.right - rDesk.left) / pWindowData->PTEXTSIZE.x;
      }

      case GTI_DESKTOPROWS:
      {
         /*NOTE 20040618 currently includes StatusBar and ToolBar, if any.
          *TODO          I Think it should it return chars area only?
          *SEEALSO       hb_wvw_gtCalcPixelHeight() - usSBHeight - usTBHeight
          */

         /*NOTE 20040719 screenheight includes linespacing, if any */

         RECT rDesk = { 0 };
         HWND hDesk;

         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );

         return (rDesk.bottom - rDesk.top) / hb_wvw_LineHeight( pWindowData );
      }

      case GTI_INPUTFD:
         return (int) GetStdHandle( STD_INPUT_HANDLE );

      case GTI_OUTPUTFD:
         return (int) GetStdHandle( STD_INPUT_HANDLE );

      case GTI_ERRORFD:
         return (int) GetStdHandle( STD_ERROR_HANDLE );

      case GTI_WINTITLE:
         {
            hb_wvw_gtSetWindowTitle( s_usCurWindow, (char *) vpParam );
            return 1;
         }

      case GTI_CODEPAGE:
         return (int) hb_wvw_gtSetCodePage( s_usCurWindow, iParam );

      case GTI_ICONFILE:
         return (long) hb_wvw_gtSetWindowIconFromFile( s_usCurWindow, (char *) vpParam  );

      case GTI_ICONRES:
         return (long) hb_wvw_gtSetWindowIcon( s_usCurWindow, iParam, (char *) vpParam );

      /* TODO: these two doesn't seem right. see gtwin about what they're supposed to do */
      case GTI_VIEWMAXWIDTH:
         return pWindowData->COLS;

      case GTI_VIEWMAXHEIGHT:
         return pWindowData->ROWS;

   }

   /* DEFAULT: there's something wrong if we are here. */
   return -1;
}

/* ********** Graphics API ********** */

/*
 * NOTE:
 *      gfxPrimitive() parameters may have different meanings
 *      ie: - Desired color is 'iBottom' for PUTPIXEL and 'iRight' for CIRCLE
 *          - Red is iTop, Green iLeft and Blue is iBottom for MAKECOLOR
 *
 */

#define SetGFXContext() hPen=CreatePen(PS_SOLID,1,color); hOldPen=(HPEN) SelectObject(hdc,hPen); hBrush=(HBRUSH) CreateSolidBrush(color); hOldBrush=(HBRUSH) SelectObject(hdc,hBrush); bOut=TRUE

/*WARNING: assume working on current window
 *NOTES: in MainCoord Mode current window is always the Main Window
 */

int HB_GT_FUNC( gt_gfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor ) )
{
COLORREF      color;
HPEN          hPen, hOldPen;
HBRUSH        hBrush, hOldBrush;
HDC           hdc;
BOOL          bOut = FALSE;
int           iRet = 0;
WIN_DATA *    pWindowData = s_pWindows[ s_usCurWindow ];

   hdc = GetDC( pWindowData->hWnd );

   switch ( iType )
   {
      case GFX_ACQUIRESCREEN:
      case GFX_RELEASESCREEN:
         ReleaseDC(pWindowData->hWnd, hdc);
         return 1;
      case GFX_MAKECOLOR:
         ReleaseDC(pWindowData->hWnd, hdc);
         return (int) ( iTop << 16 | iLeft << 8 | iBottom );
      case GFX_PUTPIXEL:
         color = RGB( iBottom >> 16, ( iBottom & 0xFF00 ) >> 8, iBottom & 0xFF );
         SetGFXContext();

         MoveToEx( hdc, iLeft, iTop, NULL );
         LineTo( hdc, iLeft, iTop );

         iRet = 1;
         break;
      case GFX_LINE:
         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         MoveToEx( hdc, iLeft, iTop, NULL );
         LineTo( hdc, iRight, iBottom );

         iRet = 1;
         break;
      case GFX_RECT:
      {
         RECT r = { 0 };
         r.left = iLeft;
         r.top = iTop;
         r.right = iRight;
         r.bottom = iBottom;

         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         FrameRect( hdc, &r, hBrush );

         iRet = 1;
      }
      break;
      case GFX_FILLEDRECT:
         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         Rectangle( hdc, iLeft, iTop, iRight, iBottom );

         iRet = 1;
         break;
      case GFX_CIRCLE:
         color = RGB( iRight >> 16, ( iRight & 0xFF00 ) >> 8, iRight & 0xFF );
         SetGFXContext();

         Arc( hdc, iLeft - iBottom / 2, iTop - iBottom / 2, iLeft + iBottom / 2, iTop + iBottom / 2, 0, 0, 0, 0 );

         iRet = 1;
         break;
      case GFX_FILLEDCIRCLE:
         color = RGB( iRight >> 16, ( iRight & 0xFF00 ) >> 8, iRight & 0xFF );
         SetGFXContext();

         Ellipse( hdc, iLeft - iBottom / 2, iTop - iBottom / 2, iLeft + iBottom / 2, iTop + iBottom / 2 );

         iRet = 1;
         break;
      case GFX_ELLIPSE:
         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         Arc( hdc, iLeft - iRight / 2, iTop - iBottom / 2, iLeft + iRight / 2, iTop + iBottom / 2, 0, 0, 0, 0 );

         iRet = 1;
         break;
      case GFX_FILLEDELLIPSE:
         color = RGB( iColor >> 16, ( iColor & 0xFF00 ) >> 8, iColor & 0xFF );
         SetGFXContext();

         Ellipse( hdc, iLeft - iRight / 2, iTop - iBottom / 2, iLeft + iRight / 2, iTop + iBottom / 2 );

         iRet = 1;
         break;
      case GFX_FLOODFILL:
         color = RGB( iBottom >> 16, ( iBottom & 0xFF00 ) >> 8, iBottom & 0xFF );
         SetGFXContext();

         FloodFill( hdc, iLeft, iTop, iColor );

         iRet = 1;
         break;
  }

  if ( bOut )
  {
     SelectObject( hdc, hOldPen );
     SelectObject( hdc, hOldBrush );
     DeleteObject( hBrush );
     DeleteObject( hPen );
  }

  ReleaseDC(pWindowData->hWnd, hdc);
  return iRet;
}

void HB_GT_FUNC( gt_gfxText( int iTop, int iLeft, char *cBuf, int iColor, int iSize, int iWidth ) )
{
  HB_SYMBOL_UNUSED( iTop );
  HB_SYMBOL_UNUSED( iLeft );
  HB_SYMBOL_UNUSED( cBuf );
  HB_SYMBOL_UNUSED( iColor );
  HB_SYMBOL_UNUSED( iSize );
  HB_SYMBOL_UNUSED( iWidth );
}

/* ******** Graphics API end ******** */

#ifdef HB_MULTI_GT

/*NOTE: are these workable in MULTI_GT ? */
static void HB_GT_FUNC( gtFnInit( PHB_GT_FUNCS gt_funcs ) )
{
    HB_TRACE( HB_TR_DEBUG, ( "hb_gtFnInit( %p )", gt_funcs ) );

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
    gt_funcs->info                  = HB_GT_FUNC( gt_info );
    gt_funcs->SetClipboard          = HB_GT_FUNC( gt_SetClipboard );
    gt_funcs->GetClipboard          = HB_GT_FUNC( gt_GetClipboard );
    gt_funcs->GetClipboardSize      = HB_GT_FUNC( gt_GetClipboardSize );
    gt_funcs->ProcessMessages       = HB_GT_FUNC( gt_ProcessMessages );

    /* Graphics API */
    gt_funcs->gfxPrimitive          = HB_GT_FUNC( gt_gfxPrimitive );
}

/*-------------------------------------------------------------------*/

/*NOTE: are these workable in MULTI_GT ?*/
static void HB_GT_FUNC( mouseFnInit( PHB_GT_FUNCS gt_funcs ) )
{
    HB_TRACE( HB_TR_DEBUG, ( "hb_mouseFnInit( %p )", gt_funcs ) );

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

/*-------------------------------------------------------------------*/

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             HB_GT_FUNC( gtFnInit ), HB_GT_FUNC( mouseFnInit ) };

HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_

#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif

#endif  /* HB_MULTI_GT */

/*-------------------------------------------------------------------*
 *
 *                 Modeless Dialogs Implementation
 *                 copied and modified from Pritpal Bedi's work in GTWVT
 *
 *-------------------------------------------------------------------*/

HB_EXPORT BOOL CALLBACK hb_wvw_gtDlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   long int bReturn = FALSE ;
   PHB_ITEM pFunc = NULL;
   PHB_DYNS pDynSym;

   iType = (int) NULL;

   for ( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
   {
      if ( ( s_sApp.hDlgModeless[ iIndex ] != NULL ) && ( s_sApp.hDlgModeless[ iIndex ] == hDlg ) )
      {
         if ( s_sApp.pFunc[ iIndex ] != NULL )
         {
            pFunc = s_sApp.pFunc[ iIndex ];
            iType = s_sApp.iType[ iIndex ];
         }
         break;
      }
   }

   if ( pFunc )
   {
      switch ( iType )
      {
         case 1:
         {
            pDynSym = ( PHB_DYNS ) pFunc;
            hb_vmPushState();
            hb_vmPushSymbol( pDynSym->pSymbol );
            hb_vmPushNil();
            hb_vmPushLong( ( ULONG ) hDlg    );
            hb_vmPushLong( ( UINT  ) message );
            hb_vmPushLong( ( ULONG ) wParam  );
            hb_vmPushLong( ( ULONG ) lParam  );
            hb_vmDo( 4 );
            bReturn = hb_itemGetNL( &HB_VM_STACK.Return );
            hb_vmPopState();
            break;
         }

         case 2:
         {

            /* eval the codeblock */
            if (s_sApp.pFunc[ iIndex ]->type == HB_IT_BLOCK)
            {
              HB_ITEM hihDlg, himessage, hiwParam, hilParam;
              PHB_ITEM pReturn;

              hihDlg.type = HB_IT_NIL;
              hb_itemPutNL( &hihDlg, (ULONG) hDlg );

              himessage.type = HB_IT_NIL;
              hb_itemPutNL( &himessage, (ULONG) message );

              hiwParam.type = HB_IT_NIL;
              hb_itemPutNL( &hiwParam, (ULONG) wParam );

              hilParam.type = HB_IT_NIL;
              hb_itemPutNL( &hilParam, (ULONG) lParam );

              pReturn = hb_itemDo( (PHB_ITEM) s_sApp.pFunc[ iIndex ], 4, &hihDlg, &himessage, &hiwParam, &hilParam );

              bReturn = hb_itemGetNL( pReturn );
              hb_itemRelease( pReturn );
            }
            else
            {

            }

            break;
         }
      }
   }

   switch( message )
   {
      case WM_COMMAND:
      {
         switch( LOWORD( wParam ) )
         {
            case IDOK:
            {
               DestroyWindow( hDlg );
               bReturn = TRUE;
            }
            break;

            case IDCANCEL:
            {
               DestroyWindow( hDlg );
               bReturn = FALSE;
            }
            break;
         }
      }
      break;

      case WM_CLOSE:
      {
         DestroyWindow( hDlg );
         bReturn = FALSE;
      }
      break;

      case WM_NCDESTROY:
      {
         if ( s_sApp.pFunc[ iIndex ] != NULL && s_sApp.iType[ iIndex ] == 2 )
         {
            hb_itemRelease( s_sApp.pFunc[ iIndex ] );

         }
         s_sApp.hDlgModeless[ iIndex ] = NULL;

         s_sApp.pFunc[ iIndex ] = NULL;
         s_sApp.iType[ iIndex ] = (int) NULL;
         bReturn = FALSE;
      }
      break;
   }

   return bReturn;
}

/*-------------------------------------------------------------------*/

HB_EXPORT BOOL CALLBACK hb_wvw_gtDlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   long int bReturn = FALSE ;
   PHB_ITEM pFunc   = NULL;
   PHB_DYNS pDynSym;
   int      iFirst  = ( int ) lParam;

   if ( iFirst > 0 && iFirst <= WVW_DLGMD_MAX )
   {
      s_sApp.hDlgModal[ iFirst-1 ] = hDlg ;
      SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
      return ( bReturn );
   }

   iType = ( int ) NULL;

   for ( iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++ )
   {
      if ( ( s_sApp.hDlgModal[ iIndex ] != NULL ) && ( s_sApp.hDlgModal[ iIndex ] == hDlg ) )
      {
         if ( s_sApp.pFuncModal[ iIndex ] != NULL )
         {
            pFunc = s_sApp.pFuncModal[ iIndex ];
            iType = s_sApp.iTypeModal[ iIndex ];
         }
         break;
      }
   }

   if ( pFunc )
   {
      switch ( iType )
      {
         case 1:
         {
            pDynSym = ( PHB_DYNS ) pFunc;
            hb_vmPushState();
            hb_vmPushSymbol( pDynSym->pSymbol );
            hb_vmPushNil();
            hb_vmPushLong( ( ULONG ) hDlg    );
            hb_vmPushLong( ( UINT  ) message );
            hb_vmPushLong( ( ULONG ) wParam  );
            hb_vmPushLong( ( ULONG ) lParam  );
            hb_vmDo( 4 );
            bReturn = hb_itemGetNL( &HB_VM_STACK.Return );
            hb_vmPopState();
            break;
         }

         case 2:
         {
            /* eval the codeblock */
            if (s_sApp.pFuncModal[ iIndex ]->type == HB_IT_BLOCK )
            {
              HB_ITEM hihDlg, himessage, hiwParam, hilParam;
              PHB_ITEM pReturn;

              hihDlg.type = HB_IT_NIL;
              hb_itemPutNL( &hihDlg, (ULONG) hDlg );

              himessage.type = HB_IT_NIL;
              hb_itemPutNL( &himessage, (ULONG) message );

              hiwParam.type = HB_IT_NIL;
              hb_itemPutNL( &hiwParam, (ULONG) wParam );

              hilParam.type = HB_IT_NIL;
              hb_itemPutNL( &hilParam, (ULONG) lParam );

              pReturn = hb_itemDo( (PHB_ITEM) s_sApp.pFuncModal[ iIndex ], 4, &hihDlg, &himessage, &hiwParam, &hilParam );
              bReturn = hb_itemGetNL( pReturn );
              hb_itemRelease( pReturn );
            }
            else
            {

            }

            break;
         }
      }
   }

   switch( message )
   {
      case WM_COMMAND:
      {
         switch( LOWORD( wParam ) )
         {
            case IDOK:
            {
               EndDialog( hDlg, IDOK );
               bReturn = TRUE;
            }
            break;

            case IDCANCEL:
            {
               EndDialog( hDlg, IDCANCEL );
               bReturn = FALSE;
            }
            break;
         }
      }
      break;

      case WM_CLOSE:
      {
         EndDialog( hDlg, IDCANCEL );
         bReturn = FALSE;
      }
      break;

      case WM_NCDESTROY:
      {
         if ( s_sApp.pFuncModal[ iIndex ] != NULL && s_sApp.iTypeModal[ iIndex ] == 2 )
         {
            hb_itemRelease( s_sApp.pFuncModal[ iIndex ] );

         }
         s_sApp.hDlgModal[ iIndex ]   = NULL;
         s_sApp.pFuncModal[ iIndex ]  = NULL;
         s_sApp.iTypeModal[ iIndex ]  = ( int ) NULL;
         bReturn = FALSE;
      }
      break;
   }

   return bReturn;
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                    WVW specific functions                         */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

static void hb_wvw_gtCreateObjects( USHORT usWinNum )
{
   LOGBRUSH lb = { 0 };

   /* 20040921 IMPORTANT:
      All these PENs and BRUSHes creations are temporarily disabled
      because WINDOW #1's CAN'T BE DELETED LATER!
      See also hb_wvw_gtCloseWindow() and gt_Exit()
      TODO: pls choose:
      (1) store PENs and BRUSHes as application-wide
      or
      (2) do the creation and deletion only when required
    */
   /* 20040923 choose #1 of above option */
   if (usWinNum>0)
   {
      return;
   }

   s_sApp.penWhite     = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 255,255,255 ) );
   s_sApp.penBlack     = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );
   s_sApp.penWhiteDim  = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 205,205,205 ) );
   s_sApp.penDarkGray  = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB( 150,150,150 ) );
   s_sApp.penGray      = CreatePen( PS_SOLID, 0, ( COLORREF ) _COLORS[ 7 ] );
   s_sApp.penNull      = CreatePen( PS_NULL , 0, ( COLORREF ) _COLORS[ 7 ] );

   s_sApp.currentPen   = CreatePen( PS_SOLID, 0, ( COLORREF ) RGB(   0,  0,  0 ) );

   lb.lbStyle      = BS_NULL;
   lb.lbColor      = RGB( 198,198,198 );
   lb.lbHatch      = 0;
   s_sApp.currentBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_HATCHED;
   lb.lbColor      = RGB( 210,210,210 );
   lb.lbHatch      = HS_DIAGCROSS; /* HS_BDIAGONAL; */
   s_sApp.diagonalBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_SOLID;
   lb.lbColor      = 0;   /* RGB( 0,0,0 ); */
   lb.lbHatch      = 0;
   s_sApp.solidBrush = CreateBrushIndirect( &lb );

   lb.lbStyle      = BS_SOLID;
   lb.lbColor      = _COLORS[ 7 ];
   lb.lbHatch      = 0;
   s_sApp.wvwWhiteBrush= CreateBrushIndirect( &lb );

}

/*-------------------------------------------------------------------*/

/*NOTE/TODO: this doesn't take MenuBar into account */
static USHORT hb_wvw_gtCalcPixelHeight( WIN_DATA * pWindowData )
{

  return( hb_wvw_LineHeight( pWindowData )*pWindowData->ROWS +
          pWindowData->usSBHeight +
          pWindowData->usTBHeight);
}

/*-------------------------------------------------------------------*/

static USHORT hb_wvw_gtCalcPixelWidth( WIN_DATA * pWindowData )
{
  return( pWindowData->PTEXTSIZE.x*pWindowData->COLS );
}

/*-------------------------------------------------------------------*/

static BOOL hb_wvw_gtAllocSpBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row )
{
  BOOL bRet = TRUE;

  pWindowData->COLS        = col;
  pWindowData->ROWS        = row;
  pWindowData->BUFFERSIZE  = col * row * sizeof( char );
  pWindowData->pBuffer     = pWindowData->byBuffer ;
  pWindowData->pAttributes = pWindowData->byAttributes;
  memset( pWindowData->pBuffer, ' ', pWindowData->BUFFERSIZE );

  if (pWindowData->byWinId==0)
  {

    memset( pWindowData->pAttributes, 0x07, pWindowData->BUFFERSIZE );

  }
  else
  {
    memset( pWindowData->pAttributes, hb_gtCurrentColor(), pWindowData->BUFFERSIZE );
  }

  return( bRet );
}

/*-------------------------------------------------------------------*/

static BOOL hb_wvw_gtInitWindow( WIN_DATA * pWindowData, HWND hWnd, USHORT col, USHORT row )
{

  BOOL bRet = hb_wvw_gtAllocSpBuffer( pWindowData, col, row );

  hb_wvw_gtResetWindowSize( pWindowData, hWnd );

  return( bRet );
}

/*-------------------------------------------------------------------*/

/* WVT commented out this function. WVW is still using it. */

static BOOL hb_wvw_gtValidWindowSize( WIN_DATA * pWindowData, int rows, int cols, HFONT hFont, int iWidth,
                                                              int *pmaxrows, int *pmaxcols )
{
  HDC        hdc;
  HFONT      hOldFont ;
  USHORT     width, height, maxWidth, maxHeight;
  USHORT     diffHeight, diffWidth;
  TEXTMETRIC tm = { 0 };
  RECT       rcWorkArea = { 0 };

  RECT       wi = { 0 }, ci = { 0 };

  SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );

  maxWidth  = (SHORT) ( rcWorkArea.right - rcWorkArea.left + 1);
  maxHeight = (SHORT) ( rcWorkArea.bottom - rcWorkArea.top + 1);

  hdc       = GetDC( pWindowData->hWnd );
  hOldFont  = ( HFONT ) SelectObject( hdc, hFont );
  GetTextMetrics( hdc, &tm );
  SelectObject( hdc, hOldFont );

  ReleaseDC( pWindowData->hWnd, hdc );

  width     = (iWidth < 0 ? -iWidth : tm.tmAveCharWidth) * cols ;  /* Total pixel width this setting would take */
  height    = tm.tmHeight * rows;         /* Total pixel height this setting would take */

  GetWindowRect( pWindowData->hWnd, &wi );
  GetClientRect( pWindowData->hWnd, &ci );

  diffWidth  = (SHORT) (( wi.right  - wi.left ) - ( ci.right  ));
  diffHeight = (SHORT) (( wi.bottom - wi.top  ) - ( ci.bottom ));
  width     += diffWidth;
  height    += diffHeight;

  height   += (pWindowData->byLineSpacing * rows);

  height   += pWindowData->usTBHeight;

  height   += pWindowData->usSBHeight;

  /* TODO: should also calc menu */

  /* before returning, put the max possible rows/cols to pmaxrows/pmaxcols */
  if (pmaxrows)
  {
    (*pmaxrows) = (maxHeight - diffHeight - pWindowData->usTBHeight - pWindowData->usSBHeight) /
                  hb_wvw_LineHeight( pWindowData );
  }
  if (pmaxcols)
  {
    (*pmaxcols) = (maxWidth - diffWidth) /
                  (iWidth < 0 ? -iWidth : tm.tmAveCharWidth);
  }

  return( ( width <= maxWidth ) && ( height <= maxHeight ) );
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtResetWindowSize( WIN_DATA * pWindowData, HWND hWnd )
{
  HDC        hdc;
  HFONT      hFont, hOldFont ;
  USHORT     diffWidth, diffHeight;
  USHORT     height, width;
  RECT       wi = { 0 }, ci = { 0 };
  TEXTMETRIC tm = { 0 };

  RECT       rcWorkArea = { 0 };
  RECT       rcMainClientArea = { 0 };
  int        n;
  WIN_DATA * pMainWindow;

  pMainWindow = s_pWindows[ 0 ];

  /* set the font and get it's size to determine the size of the client area
   * for the required number of rows and columns
   */
  hdc      = GetDC( hWnd );
  hFont    = hb_wvw_gtGetFont( pWindowData->fontFace, pWindowData->fontHeight, pWindowData->fontWidth, pWindowData->fontWeight, pWindowData->fontQuality, pWindowData->CodePage );

  if ( pWindowData->hFont )
  {
    DeleteObject( pWindowData->hFont );
  }

  pWindowData->hFont = hFont ;
  hOldFont = ( HFONT ) SelectObject( hdc, hFont );

  GetTextMetrics( hdc, &tm );
  SetTextCharacterExtra( hdc,0 ); /* do not add extra char spacing even if bold */

  SelectObject( hdc, hOldFont );
  ReleaseDC( hWnd, hdc );

  /* we will need to use the font size to handle the transformations from
   * row column space in the future, so we keep it around in a static!
   */

  pWindowData->PTEXTSIZE.x = pWindowData->fontWidth<0 ? -pWindowData->fontWidth : tm.tmAveCharWidth; /* For fixed FONT should == tm.tmMaxCharWidth */
  pWindowData->PTEXTSIZE.y = tm.tmHeight;       /*     but seems to be a problem on Win9X so */
                                      /*     assume proportional fonts always for Win9X */

  if (pWindowData->fontWidth < 0 || s_sApp.Win9X || ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) || ( pWindowData->PTEXTSIZE.x != tm.tmMaxCharWidth ) )
  {

    pWindowData->FixedFont = FALSE;
  }
  else
  {
    pWindowData->FixedFont = TRUE ;
  }

  for( n=0 ; n< pWindowData->COLS ; n++ ) /* pWindowData->FixedSize[] is used by ExtTextOut() to emulate */
  {                             /*          fixed font when a proportional font is used */
    pWindowData->FixedSize[ n ] = pWindowData->PTEXTSIZE.x;
  }

  if ( IsZoomed( pWindowData->hWnd ) )
  {

    if ( SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
    {
      wi.top = rcWorkArea.top;
      wi.left = rcWorkArea.left;
      wi.bottom = rcWorkArea.bottom;
      wi.right = rcWorkArea.right;
    }
    else
    {
      GetWindowRect( hWnd, &wi );
    }

    height = wi.bottom - wi.top + 1;
    width  = wi.right - wi.left + 1;

  }

  else if (pWindowData->byWinId==0)
  {
    /* resize the window to get the specified number of rows and columns
     */
    height = hb_wvw_gtCalcPixelHeight( pWindowData );
    width  = hb_wvw_gtCalcPixelWidth( pWindowData );

    GetWindowRect( hWnd, &wi );
    GetClientRect( hWnd, &ci );

    diffWidth  = (SHORT) (( wi.right  - wi.left ) - ( ci.right  ));
    diffHeight = (SHORT) (( wi.bottom - wi.top  ) - ( ci.bottom ));
    width      += diffWidth ;
    height     += diffHeight;

    /* Centre the window within the CLIENT area on the screen
     *                   but only if pWindowData->CentreWindow == TRUE
     */

    /*
    if ( pWindowData->CentreWindow && SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
    {
      wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
      wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
    }
    */

    if ( SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 ) )
    {
      if ( pWindowData->CentreWindow )
      {
        wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
        wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
      }
      else
      {
        if ( pWindowData->HCentreWindow )
        {
          wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
        }
        if ( pWindowData->VCentreWindow )
        {
          wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
        }
      }
    }

  }
  else
  {

    /* resize the window to get the specified number of rows and columns
     */
    height = hb_wvw_gtCalcPixelHeight( pWindowData );
    width  = hb_wvw_gtCalcPixelWidth( pWindowData );

    GetWindowRect( hWnd, &wi );
    GetClientRect( hWnd, &ci );

    diffWidth  = (SHORT) (( wi.right  - wi.left ) - ( ci.right  ));
    diffHeight = (SHORT) (( wi.bottom - wi.top  ) - ( ci.bottom ));
    width      += diffWidth ;
    height     += diffHeight;

    /* Centre the window within the area of the MAIN WINDOW
     *                   but only if pWindowData->CentreWindow == TRUE
     */
    GetWindowRect( (*pMainWindow).hWnd, &rcWorkArea);
    GetClientRect( (*pMainWindow).hWnd, &rcMainClientArea);

    if ( pWindowData->CentreWindow )
    {
      wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
      wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
    }
    else
    {

      if ( pWindowData->HCentreWindow )
      {
        wi.left = rcWorkArea.left + ( ( ( rcWorkArea.right-rcWorkArea.left ) - ( width  ) ) / 2 ) ;
      }
      else
      {

        wi.left = rcWorkArea.left + ( pWindowData->usColOfs * (*pMainWindow).PTEXTSIZE.x );
      }

      if ( pWindowData->VCentreWindow )
      {
        wi.top  = rcWorkArea.top  + ( ( ( rcWorkArea.bottom-rcWorkArea.top ) - ( height ) ) / 2 ) ;
      }
      else
      {

        wi.top = rcWorkArea.top + ( pWindowData->usRowOfs * hb_wvw_LineHeight( pMainWindow ) );

        wi.top -= diffHeight;

        wi.top += (( rcWorkArea.bottom - rcWorkArea.top  ) - ( rcMainClientArea.bottom ));

        wi.top += (*pMainWindow).usTBHeight;

        wi.top -= (*pWindowData).usTBHeight;
      }
    }

  }

  if ( !IsZoomed( hWnd ) )
  {

     SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
  }
  else
  {

     SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

     InvalidateRect( hWnd, NULL, FALSE );
  }

  if (pWindowData->hStatusBar != NULL)
  {
     SetWindowPos( pWindowData->hStatusBar, NULL, wi.left, wi.bottom - pWindowData->usSBHeight, width, pWindowData->usSBHeight, SWP_NOZORDER );

  }

  if (pWindowData->hToolBar != NULL)
  {

     SetWindowPos( pWindowData->hToolBar, NULL, wi.left, wi.top - pWindowData->usTBHeight, width, pWindowData->usTBHeight, SWP_NOZORDER );

  }

  if (pWindowData->pcdCtrlList != NULL)
  {

    ReposControls(pWindowData->byWinId, 0);
  }

  if (pWindowData->byWinId == s_usNumWindows-1)
  {
     hb_wvw_gtSetCaretPos(pWindowData);
  }

}

/*-------------------------------------------------------------------*/

static int hb_wvw_key_ansi_to_oem( int c )
{
   char pszAnsi[4];
   char pszOem[4];

   sprintf( pszAnsi, "%c", c );
   CharToOemBuff( ( LPCSTR ) pszAnsi, ( LPTSTR ) pszOem, 1 );
   c = (BYTE) * pszOem;

   return c;
}

/*-------------------------------------------------------------------*/
/* JC1: rendering of graphical objects */

static void s_wvw_paintGraphicObjects( HDC hdc, RECT *updateRect )
{
   HB_GT_GOBJECT *pObj;
   COLORREF color;
   HPEN hPen, hOldPen;
   HBRUSH hBrush, hOldBrush;

   pObj = hb_gt_gobjects;

   while ( pObj )
   {
      /* Check if pObj boundaries are inside the area to be updated */
      if ( hb_gtGobjectInside( pObj, updateRect->left, updateRect->top,
         updateRect->right, updateRect->bottom ) )
      {
         color = RGB( pObj->color.usRed >> 8, pObj->color.usGreen >> 8,
               pObj->color.usBlue >> 8);
         hPen = CreatePen( PS_SOLID, 1, color );
         hOldPen = (HPEN) SelectObject( hdc, hPen );
         hBrush = (HBRUSH) CreateSolidBrush( color );
         hOldBrush = (HBRUSH) SelectObject( hdc, hBrush );

         switch( pObj->type )
         {
            case GTO_POINT:
               MoveToEx( hdc, pObj->x, pObj->y, NULL );
               LineTo( hdc, pObj->x, pObj->y );
            break;

            case GTO_LINE:
               /* For lines, width and height represent X2, Y2 */
               MoveToEx( hdc, pObj->x, pObj->y, NULL );
               LineTo( hdc, pObj->width, pObj->height );
            break;

            case GTO_SQUARE:
            {
               RECT r = { 0 };
               r.left  = pObj->x;
               r.top   = pObj->y;
               r.right = pObj->x + pObj->width;
               r.bottom= pObj->y + pObj->height;

               FrameRect( hdc, &r, hBrush );
            }
            break;

            case GTO_RECTANGLE:
               /* For lines, width and height represent X2, Y2 */
               Rectangle( hdc,
                  pObj->x, pObj->y,
                  pObj->x + pObj->width, pObj->y + pObj->height );
            break;

            case GTO_CIRCLE:
               Arc( hdc,
                  pObj->x, pObj->y,
                  pObj->x + pObj->width, pObj->y + pObj->height,
                  0,0,0,0 );
            break;

            case GTO_DISK:
               Ellipse( hdc,
                  pObj->x, pObj->y,
                  pObj->x + pObj->width, pObj->y + pObj->height );
            break;

            case GTO_TEXT:
               TextOut( hdc,
                  pObj->x, pObj->y,
                  pObj->data, pObj->data_len );
            break;
         }
         SelectObject( hdc, hOldPen );
         SelectObject( hdc, hOldBrush );
         DeleteObject( hBrush );
         DeleteObject( hPen );
      }

      pObj = pObj->next;
   }

}

/*-------------------------------------------------------------------*/

static void xUserPaintNow( USHORT usWinNum )
{
  static BOOL bRunning = FALSE;

  /*make sure we don't execute it > 1 time
   *eg. if s_uiPaintRefresh is too small
   */
  if (bRunning) return;

  bRunning = TRUE;

  s_pWindows[usWinNum]->bPaintPending = FALSE;

  hb_vmPushState();
  hb_vmPushSymbol( s_sApp.pSymWVW_PAINT->pSymbol );
  hb_vmPushNil();
  hb_vmPushInteger( ( int ) (usWinNum)  );

  /* follow WVT convention to not passing coordinates anymore
  hb_vmPushInteger( ( int ) (rpaint.top)  );
  hb_vmPushInteger( ( int ) (rpaint.left)  );
  hb_vmPushInteger( ( int ) (rpaint.bottom)  );
  hb_vmPushInteger( ( int ) (rpaint.right)  );
  hb_vmDo( 5 );
  */

  hb_vmDo( 1 );

  hb_vmPopState();

  if ( !s_pWindows[usWinNum]->bPaintPending )
  {
    hb_wvw_InitPendingRect( s_pWindows[ usWinNum ] );
  }

  bRunning = FALSE;
}

/*-------------------------------------------------------------------*/

static void xUserTimerNow( USHORT usWinNum, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  static BOOL bRunning = FALSE;

  /*make sure we don't execute it > 1 time
   *eg. if timer interval is too small
   *the call will be lost in this case
   */
  if (bRunning) return;

  bRunning = TRUE;

  hb_vmPushState();
  hb_vmPushSymbol( s_sApp.pSymWVW_TIMER->pSymbol );
  hb_vmPushNil();
  hb_vmPushInteger( ( int ) (usWinNum)  );
  hb_vmPushLong( ( LONG ) hWnd    );
  hb_vmPushLong( ( LONG ) message );
  hb_vmPushLong( ( LONG ) wParam  );
  hb_vmPushLong( ( LONG ) lParam  );
  hb_vmDo( 5 );

  hb_vmPopState();

  bRunning = FALSE;
}

/*-------------------------------------------------------------------*/

static LRESULT CALLBACK hb_wvw_gtWndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{

  BOOL       bRet;

  USHORT     usWinNum;
  WIN_DATA * pWindowData;

  for(usWinNum=0; usWinNum<s_usNumWindows; usWinNum++)
  {
    if (s_pWindows[usWinNum]->hWnd == hWnd)
    {
     break;
    }
  }

  if(usWinNum>=s_usNumWindows)
  {

    usWinNum = s_usNumWindows-1;
  }

  pWindowData = s_pWindows[usWinNum];

  switch ( message )
  {
    case WM_CREATE:
    {

      bRet = hb_wvw_gtInitWindow( pWindowData, hWnd, pWindowData->COLS, pWindowData->ROWS );

      return( bRet );
    }

    case WM_COMMAND: /* handle menu items */
    {

      if (s_usNumWindows!=usWinNum+1)
      {

        hb_wvw_gtInputNotAllowed( usWinNum, message, wParam, lParam );
      }
      else
      {

        if (LOWORD(wParam) < WVW_ID_BASE_PUSHBUTTON )
        {

           hb_wvw_gtHandleMenuSelection( ( int ) LOWORD( wParam ) );
        }
        else
        if (LOWORD(wParam) <= WVW_ID_MAX_PUSHBUTTON)
        {

           HWND hWndCtrl = (HWND) lParam;
           UINT uiPBid;
           byte bStyle;

           uiPBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_PUSHBUTTON, hWndCtrl, &bStyle) ;
           if (uiPBid==0)
           {

             hb_wvw_gtHandleMenuSelection( ( int ) LOWORD( wParam ) );

             return(0);
           }

           RunControlBlock(usWinNum, WVW_CONTROL_PUSHBUTTON, hWndCtrl, message, wParam, lParam, 0 );

           return 0 ;
        } /* button click */

        else
        {
           /*
           int lowordwParam = (int) LOWORD(wParam);
           int hiwordwParam = (int) HIWORD(wParam);
           int lowordlParam = (int) LOWORD(lParam);
           int hiwordlParam = (int) HIWORD(lParam);
           HWND hWndCtrl = (HWND) lParam;

           TraceLog( NULL, "debugging: WM_COMMAND is processed?\n" );
           TraceLog( NULL, "  lowordwParam (control id)=%i\n", lowordwParam );
           TraceLog( NULL, "  hiwordwParam (notification)=%i\n", hiwordwParam );
           TraceLog( NULL, "  lowordlParam=%i\n", lowordlParam );
           TraceLog( NULL, "  hiwordlParam=%i\n", hiwordlParam );
           */

           switch( HIWORD(wParam) )
           {

             case CBN_SELCHANGE:
             case CBN_SETFOCUS:
             case CBN_KILLFOCUS:
             {

                HWND hWndCtrl = (HWND) lParam;
                UINT uiCBid;
                byte bStyle;

                uiCBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_COMBOBOX, hWndCtrl, &bStyle) ;
                if (uiCBid==0)
                {

                  hb_wvw_gtHandleMenuSelection( ( int ) LOWORD( wParam ) );

                  return(0);
                }

                RunControlBlock(usWinNum, WVW_CONTROL_COMBOBOX, hWndCtrl, message, wParam, lParam, (BYTE) HIWORD(wParam));

                return 0 ;
             }
           }

           return 1;
        } /* combobox */

      }
      return( 0 );
    }

    case WM_MENUSELECT:
    {
      if ( s_sApp.pSymWVW_MENUSELECT )
      {
        hb_vmPushState();
        hb_vmPushSymbol( s_sApp.pSymWVW_MENUSELECT->pSymbol );
        hb_vmPushNil();
        hb_vmPushInteger( ( int ) (usWinNum)  );
        hb_vmPushLong( ( LONG ) hWnd    );
        hb_vmPushLong( ( LONG ) message );
        hb_vmPushLong( ( LONG ) wParam  );
        hb_vmPushLong( ( LONG ) lParam  );
        hb_vmDo( 5 );

        hb_vmPopState();
      }
      return( 0 );
    }

    case WM_PAINT:
    {
      PAINTSTRUCT ps = { 0 };
      HDC         hdc;
      USHORT      irow;
      RECT        updateRect = { 0 }, rcRect = { 0 };

      RECT        ci = { 0 };
      int         ixbeyond;
      int         iybeyond;
      BOOL        bR = FALSE, bB = FALSE;
      int colStart = 0 , colStop = 0, rowStart = 0, rowStop = 0;
      HFONT       hOldFont;

      GetUpdateRect( hWnd, &updateRect, FALSE );
      /* WARNING!!!
       * the GetUpdateRect call MUST be made BEFORE the BeginPaint call, since
       * BeginPaint resets the update rectangle - don't move it or nothing is drawn!
       */

      /* 20050625 TODO: MSDN says app should NOT call BeginPaint if GetUpdateRect returns zero */

      hdc = BeginPaint( hWnd, &ps );

      hOldFont = (HFONT) SelectObject( hdc, pWindowData->hFont );

      ixbeyond = pWindowData->COLS  * pWindowData->PTEXTSIZE.x;

      iybeyond = hb_wvw_LineHeight( pWindowData )*pWindowData->ROWS +
                 pWindowData->usTBHeight;

      if ( updateRect.left > ixbeyond || updateRect.top > iybeyond )
      {
        /* do nothing now, will be handled later */

      }
      else
      {

        /*
         * using the update rect, determine which rows and columns of text
         * to paint, and do so
         */

        if ( pWindowData->pBuffer != NULL && pWindowData->pAttributes != NULL )
        {

          /* need to account for truncation in conversion
           * i.e. redraw any 'cell' partially covered...
           */
          rcRect   = hb_wvw_gtGetColRowFromXYRect( pWindowData, updateRect );

          /*
          WVT uses global vars as follows:

          _s.rowStart = max( 0, rcRect.top-1 );
          _s.rowStop  = min( _s.ROWS, rcRect.bottom+1 );
          _s.colStart = max( 0, rcRect.left -1 );
          _s.colStop  = min( _s.COLS, rcRect.right+1 );

          WVW can't do that way, because we use TIMER method to repaint
          WVW's pending repaint rect is stored in rPaintPending
          */

          rowStart = max( 0, rcRect.top );

          rowStop  = min( pWindowData->ROWS-1, rcRect.bottom );

          colStart = rcRect.left;
          colStop  = rcRect.right;

          for ( irow = rowStart; irow <= rowStop; irow++ )
          {
            USHORT icol, index, startIndex, startCol, len;
            BYTE oldAttrib, attrib;

            icol       = colStart;
            index      = hb_wvw_gtGetIndexForTextBuffer( pWindowData, icol, irow );
            startIndex = index;
            startCol   = icol;
            len        = 0;
            oldAttrib  = *( pWindowData->pAttributes+index );

            /* attribute may change mid line...
            * so buffer up text with same attrib, and output it
            * then do next section with same attrib, etc
            */

            while ( icol <= colStop )
            {
              if ( index >= pWindowData->BUFFERSIZE )
              {
                break;
              }
              attrib = *( pWindowData->pAttributes+index );
              if ( attrib != oldAttrib )
              {
                hb_wvw_gtSetColors( pWindowData, hdc, oldAttrib );
                hb_wvw_gtTextOut( pWindowData, hdc, startCol, irow, ( char const * ) pWindowData->pBuffer+startIndex, len );

                if (pWindowData->byLineSpacing > 0)
                {
                  hb_wvw_gtFillLineSpace( pWindowData, hdc, startCol, irow, len, oldAttrib );
                }

                oldAttrib  = attrib;
                startIndex = index;
                startCol   = icol;
                len        = 0;

              }
              icol++;
              len++;
              index++;
            }

            hb_wvw_gtSetColors( pWindowData, hdc, oldAttrib );
            hb_wvw_gtTextOut( pWindowData, hdc, startCol, irow, ( char const * ) pWindowData->pBuffer+startIndex, len );

            if (pWindowData->byLineSpacing > 0)
            {
              hb_wvw_gtFillLineSpace( pWindowData, hdc, startCol, irow, len, oldAttrib );
            }
          }
        }
      }

      /* Here we must also paint the unreachable region on the right, if any.
         This beyond reach area is due to Min/Max/Close button on
         a small window
         OR
         unreached area due to MAXIMIZED mode
       */

      if ( updateRect.right == ixbeyond )
      {

        GetClientRect( hWnd, &ci );

        if (ci.right > ixbeyond)
        {
          rcRect.left   = ixbeyond;
          rcRect.top    = updateRect.top;
          rcRect.right  = ci.right;
          rcRect.bottom = updateRect.bottom;

          InvalidateRect( hWnd, &rcRect, FALSE );

          bR = TRUE;

        }

      }

      else if ( updateRect.right > ixbeyond )
      {

        LOGBRUSH lb = { 0 };
        HBRUSH   hBrush;

        COLORREF bkColor = _COLORS[ pWindowData->byAttributes[0] >> 4 ];

        rcRect.left   = max( ixbeyond, updateRect.left );
        rcRect.top    = updateRect.top;
        rcRect.right  = updateRect.right;
        rcRect.bottom = updateRect.bottom;

        lb.lbStyle = BS_SOLID;
        lb.lbColor = bkColor;
        lb.lbHatch = 0;

        hBrush     = CreateBrushIndirect( &lb );

        FillRect( hdc, &rcRect, hBrush );
        DeleteObject( hBrush );
      }

      if (IsZoomed(hWnd))
      {

        if ( updateRect.bottom == iybeyond )
        {

          GetClientRect( hWnd, &ci );

          if (ci.bottom > iybeyond)
          {
            rcRect.left   = updateRect.left;
            rcRect.top    = iybeyond;
            rcRect.right  = updateRect.right;
            rcRect.bottom = ci.bottom;

            InvalidateRect( hWnd, &rcRect, FALSE );
            bB = TRUE;
          }

        }

        /* Here we must also paint the unreachable region on the bottom, if any.
           This beyond reach area is due to MAXIMIZED state of
           a small window */
        else if ( updateRect.bottom > iybeyond )
        {

          LOGBRUSH lb = { 0 };
          HBRUSH   hBrush;

          COLORREF bkColor = _COLORS[ pWindowData->byAttributes[0] >> 4 ];

          rcRect.left = updateRect.left;
          rcRect.top   = max( iybeyond, updateRect.top );
          rcRect.right  = updateRect.right;
          rcRect.bottom = updateRect.bottom;

          lb.lbStyle = BS_SOLID;
          lb.lbColor = bkColor;
          lb.lbHatch = 0;

          hBrush     = CreateBrushIndirect( &lb );

          FillRect( hdc, &rcRect, hBrush );
          DeleteObject( hBrush );
        }

        if ( bR && bB )
        {

          rcRect.left   = ixbeyond;
          rcRect.top    = iybeyond;
          rcRect.right  = ci.right;
          rcRect.bottom = ci.bottom;

          InvalidateRect( hWnd, &rcRect, FALSE );

        }
      }

      if ( hb_gt_gobjects != NULL )
      {
         s_wvw_paintGraphicObjects( hdc, &updateRect );
      }

      SelectObject( hdc, hOldFont );

      EndPaint( hWnd, &ps );

      if ( pWindowData->bPaint )
      {
        if ( s_sApp.pSymWVW_PAINT )
        {

          pWindowData->bPaintPending = TRUE;

          hb_wvw_UpdatePendingRect( pWindowData, (USHORT) rowStart, (USHORT) colStart,
                                                 (USHORT) rowStop, (USHORT) colStop);

          if (s_uiPaintRefresh==0)
          {
             xUserPaintNow(usWinNum);
          }
        }
      }
      else
      {

        pWindowData->bPaint = TRUE;
      }
#ifdef WVW_DEBUG
  printf( "\nPuts( %d ), Scroll( %d ), Paint( %d ), SetFocus( %d ), KillFocus( %d ) ",nCountPuts, nCountScroll, ++nCountPaint, nSetFocus, nKillFocus ) ;
#endif
      return( 0 );
    }

    case WM_MY_UPDATE_CARET:
    {
      hb_wvw_gtSetCaretPos(pWindowData);
      return( 0 );
    }

    case WM_SETFOCUS:
    {
#ifdef WVW_DEBUG
  nSetFocus++;
#endif

      if (usWinNum == s_usNumWindows-1)
      {

        if (!s_bMainCoordMode)
        {

          hb_gtSetPos(pWindowData->caretPos.y, pWindowData->caretPos.x);
        }
        else
        {

          hb_gtSetPos(pWindowData->caretPos.y + hb_wvw_gtRowOfs( usWinNum ),
                      pWindowData->caretPos.x + hb_wvw_gtColOfs( usWinNum ));
        }

        hb_wvw_gtCreateCaret(pWindowData) ;

      }

      if ( pWindowData->bGetFocus )
      {

        if ( s_sApp.pSymWVW_SETFOCUS )
        {
          hb_vmPushState();
          hb_vmPushSymbol( s_sApp.pSymWVW_SETFOCUS->pSymbol );
          hb_vmPushNil();
          hb_vmPushInteger( ( int ) (usWinNum)  );
          hb_vmPushLong( ( LONG ) hWnd    );
          hb_vmDo( 2 );

          hb_vmPopState();
        }

      }
      else
      {

        pWindowData->bGetFocus = TRUE;
      }

      return( 0 );
    }

    case WM_KILLFOCUS:
    {
#ifdef WVW_DEBUG
  nKillFocus++;
#endif

        hb_wvw_gtKillCaret( pWindowData );

      if ( s_sApp.pSymWVW_KILLFOCUS )
      {
        hb_vmPushState();
        hb_vmPushSymbol( s_sApp.pSymWVW_KILLFOCUS->pSymbol );
        hb_vmPushNil();
        hb_vmPushInteger( ( int ) (usWinNum)  );
        hb_vmPushLong( ( LONG ) hWnd );
        hb_vmDo( 2 );

        hb_vmPopState();
      }
      return( 0 );
    }

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
    {
      BOOL bAlt         = GetKeyState( VK_MENU ) & 0x8000;

      if ( !hb_wvw_gtAcceptingInput() )
      {
        if ( hb_wvw_gtBufferedKey( (LONG) wParam ) )
        {

          hb_wvw_gtInputNotAllowed( usWinNum, message, wParam, lParam );
        }
        return(0);
      }

      pWindowData->bIgnoreWM_SYSCHAR = FALSE;
      switch ( wParam )
      {

        case VK_LEFT:
          hb_wvw_gtTranslateKey( K_LEFT, K_SH_LEFT, K_ALT_LEFT, K_CTRL_LEFT );
          break;
        case VK_RIGHT:
          hb_wvw_gtTranslateKey( K_RIGHT, K_SH_RIGHT, K_ALT_RIGHT, K_CTRL_RIGHT );
          break;
        case VK_UP:
          hb_wvw_gtTranslateKey( K_UP, K_SH_UP, K_ALT_UP, K_CTRL_UP );
          break;
        case VK_DOWN:
          hb_wvw_gtTranslateKey( K_DOWN, K_SH_DOWN, K_ALT_DOWN, K_CTRL_DOWN );
          break;
        case VK_HOME:
          hb_wvw_gtTranslateKey( K_HOME, K_SH_HOME, K_ALT_HOME, K_CTRL_HOME );
          break;
        case VK_END:
          hb_wvw_gtTranslateKey( K_END, K_SH_END, K_ALT_END, K_CTRL_END );
          break;
        case VK_DELETE:
          hb_wvw_gtTranslateKey( K_DEL, K_SH_DEL, K_ALT_DEL, K_CTRL_DEL );
          break;
        case VK_INSERT:
          hb_wvw_gtTranslateKey( K_INS, K_SH_INS, K_ALT_INS, K_CTRL_INS );
          break;
        case VK_PRIOR:
          hb_wvw_gtTranslateKey( K_PGUP, K_SH_PGUP, K_ALT_PGUP, K_CTRL_PGUP );
          break;
        case VK_NEXT:
          hb_wvw_gtTranslateKey( K_PGDN, K_SH_PGDN, K_ALT_PGDN, K_CTRL_PGDN );
          break;
        case VK_F1:
          hb_wvw_gtTranslateKey( K_F1, K_SH_F1, K_ALT_F1, K_CTRL_F1 );
          break;
        case VK_F2:
          hb_wvw_gtTranslateKey( K_F2, K_SH_F2, K_ALT_F2, K_CTRL_F2 );
          break;
        case VK_F3:
          hb_wvw_gtTranslateKey( K_F3, K_SH_F3, K_ALT_F3, K_CTRL_F3 );
          break;
        case VK_F4:
        {
          if ( s_sApp.AltF4Close && bAlt )
          {
            return( DefWindowProc( hWnd, message, wParam, lParam ) );
          }
          else
          {
            hb_wvw_gtTranslateKey( K_F4, K_SH_F4, K_ALT_F4, K_CTRL_F4 );
          }
          break;
        }
        case VK_F5:
          hb_wvw_gtTranslateKey( K_F5, K_SH_F5, K_ALT_F5, K_CTRL_F5 );
          break;
        case VK_F6:
          hb_wvw_gtTranslateKey( K_F6, K_SH_F6, K_ALT_F6, K_CTRL_F6 );
          break;
        case VK_F7:
          hb_wvw_gtTranslateKey( K_F7, K_SH_F7, K_ALT_F7, K_CTRL_F7 );
          break;
        case VK_F8:
          hb_wvw_gtTranslateKey( K_F8, K_SH_F8, K_ALT_F8, K_CTRL_F8 );
          break;
        case VK_F9:
          hb_wvw_gtTranslateKey( K_F9, K_SH_F9, K_ALT_F9, K_CTRL_F9 );
          break;
        case VK_F10:
          hb_wvw_gtTranslateKey( K_F10, K_SH_F10, K_ALT_F10, K_CTRL_F10 );
          break;
        case VK_F11:
          hb_wvw_gtTranslateKey( K_F11, K_SH_F11, K_ALT_F11, K_CTRL_F11 );
          break;
        case VK_F12:
          hb_wvw_gtTranslateKey( K_F12, K_SH_F12, K_ALT_F12, K_CTRL_F12 );
          break;
        default:
        {
          BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
          BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
          int  iScanCode = HIWORD( lParam ) & 0xFF ;

          if ( bCtrl && iScanCode == 76 ) /* CTRL_VK_NUMPAD5 ) */
          {
            hb_wvw_gtAddCharToInputQueue( KP_CTRL_5 );
          }
          else if ( bCtrl && wParam == VK_TAB ) /* K_CTRL_TAB */
          {

            if ( bShift )
            {
               hb_wvw_gtAddCharToInputQueue( K_CTRL_SH_TAB );
            }
            else
            {
               hb_wvw_gtAddCharToInputQueue( K_CTRL_TAB );
            }
          }
          else if ( iScanCode == 70 ) /* Ctrl_Break key OR Scroll LOCK key */
          {
            if ( bCtrl )  /* Not scroll lock */
            {
              hb_wvw_gtAddCharToInputQueue( HB_BREAK_FLAG ); /* Pretend Alt+C pressed */

              pWindowData->bIgnoreWM_SYSCHAR = TRUE;
            }
            else
            {
              DefWindowProc( hWnd, message, wParam, lParam ) ;  /* Let windows handle ScrollLock */
            }
          }
          else if ( bCtrl && iScanCode==53 && bShift )
          {
            hb_wvw_gtAddCharToInputQueue( K_CTRL_QUESTION );
          }
          else if ( ( bAlt || bCtrl ) && (
              wParam==VK_MULTIPLY || wParam==VK_ADD || wParam== VK_SUBTRACT
              || wParam== VK_DIVIDE ) )
          {
            if ( bAlt )
            {

              pWindowData->bIgnoreWM_SYSCHAR = TRUE;
            }
            switch ( wParam )
            {
              case VK_MULTIPLY:
                hb_wvw_gtTranslateKey( '*','*', KP_ALT_ASTERISK, KP_CTRL_ASTERISK );
                break;
              case VK_ADD:
                hb_wvw_gtTranslateKey( '+','+', KP_ALT_PLUS, KP_CTRL_PLUS );
                break;
              case VK_SUBTRACT:
                hb_wvw_gtTranslateKey( '-','-', KP_ALT_MINUS, KP_CTRL_MINUS );
                break;
              case VK_DIVIDE:
                hb_wvw_gtTranslateKey( '/','/', KP_ALT_SLASH, KP_CTRL_SLASH );
                break;
            }
          }
          else if ( pWindowData->EnableShortCuts )
          {
            return( DefWindowProc( hWnd, message, wParam, lParam ) );
          }
        }
      }
      return( 0 );
    }

    case WM_CHAR:
    {
      BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
      int  iScanCode = HIWORD( lParam ) & 0xFF ;
      int c = ( int )wParam;
      HWND hMouseCapturer;

      hMouseCapturer = GetCapture();
      if (hMouseCapturer)
      {

        SendMessage( hMouseCapturer, WM_LBUTTONUP, 0, 0 );
      }

      if ( !hb_wvw_gtAcceptingInput() )
      {

        hb_wvw_gtInputNotAllowed( usWinNum, message, wParam, lParam );
        return(0);
      }

      if ( !pWindowData->bIgnoreWM_SYSCHAR )
      {

        if ( bCtrl && iScanCode == 28 )  /* K_CTRL_RETURN */
        {
          hb_wvw_gtAddCharToInputQueue( K_CTRL_RETURN );
        }
        else if ( bCtrl && ( c >= 1 && c<= 26 ) )  /* K_CTRL_A - Z */
        {
          hb_wvw_gtAddCharToInputQueue( K_Ctrl[c-1]  );
        }
        else
        {
          switch ( c )
          {
            /* handle special characters */
            case VK_BACK:
              hb_wvw_gtTranslateKey( K_BS, K_SH_BS, K_ALT_BS, K_CTRL_BS );
              break;
            case VK_TAB:
              hb_wvw_gtTranslateKey( K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB );
              break;
            case VK_RETURN:
              hb_wvw_gtTranslateKey( K_RETURN, K_SH_RETURN, K_ALT_RETURN, K_CTRL_RETURN );
              break;
            case VK_ESCAPE:
              hb_wvw_gtAddCharToInputQueue( K_ESC );
              break;
            default:

              if( pWindowData->CodePage == OEM_CHARSET )
              {
                 c = hb_wvw_key_ansi_to_oem( c );
              }
              hb_wvw_gtAddCharToInputQueue( c );
              break;
          }
        }
      }

      pWindowData->bIgnoreWM_SYSCHAR = FALSE;
      return( 0 );
    }

    case WM_SYSCHAR:
    {

      if ( !hb_wvw_gtAcceptingInput() )
      {

        hb_wvw_gtInputNotAllowed( usWinNum, message, wParam, lParam );

        pWindowData->bIgnoreWM_SYSCHAR = FALSE;
        return(0);
      }

      if ( !pWindowData->bIgnoreWM_SYSCHAR )
      {
        int c, iScanCode = HIWORD( lParam ) & 0xFF ;
        switch ( iScanCode )
        {
          case  2:
            c = K_ALT_1 ;
            break;
          case  3:
            c = K_ALT_2 ;
            break;
          case  4:
            c = K_ALT_3 ;
            break;
          case  5:
            c = K_ALT_4 ;
            break;
          case  6:
            c = K_ALT_5 ;
            break;
          case  7:
            c = K_ALT_6 ;
            break;
          case  8:
            c = K_ALT_7 ;
            break;
          case  9:
            c = K_ALT_8 ;
            break;
          case 10:
            c = K_ALT_9 ;
            break;
          case 11:
            c = K_ALT_0 ;
            break;
          case 13:
            c = K_ALT_EQUALS ;
            break;
          case 14:
            c = K_ALT_BS ;
            break;
          case 16:
            c = K_ALT_Q ;
            break;
          case 17:
            c = K_ALT_W ;
            break;
          case 18:
            c = K_ALT_E ;
            break;
          case 19:
            c = K_ALT_R ;
            break;
          case 20:
            c = K_ALT_T ;
            break;
          case 21:
            c = K_ALT_Y ;
            break;
          case 22:
            c = K_ALT_U ;
            break;
          case 23:
            c = K_ALT_I ;
            break;
          case 24:
            c = K_ALT_O ;
            break;
          case 25:
            c = K_ALT_P ;
            break;
          case 30:
            c = K_ALT_A ;
            break;
          case 31:
            c = K_ALT_S ;
            break;
          case 32:
            c = K_ALT_D ;
            break;
          case 33:
            c = K_ALT_F ;
            break;
          case 34:
            c = K_ALT_G ;
            break;
          case 35:
            c = K_ALT_H ;
            break;
          case 36:
            c = K_ALT_J ;
            break;
          case 37:
            c = K_ALT_K ;
            break;
          case 38:
            c = K_ALT_L ;
            break;
          case 44:
            c = K_ALT_Z ;
            break;
          case 45:
            c = K_ALT_X ;
            break;
          case 46:
            c = K_ALT_C ;
            break;
          case 47:
            c = K_ALT_V ;
            break;
          case 48:
            c = K_ALT_B ;
            break;
          case 49:
            c = K_ALT_N ;
            break;
          case 50:
            c = K_ALT_M ;
            break;
          default:
            c = ( int ) wParam ;
            break;
        }
        hb_wvw_gtAddCharToInputQueue( c );

      }

      pWindowData->bIgnoreWM_SYSCHAR = FALSE;
      return( 0 );
    }

    case WM_QUERYENDSESSION: /* Closing down computer */
    {
      /* if we have set a shutdown command return false,
       * so windows ( and our app )doesn't shutdown
       * otherwise let the default handler take it
       */
      if ( hb_gtHandleShutdown() )
      {
         return 0;
      }
      break;
    }

    case WM_CLOSE:  /* Clicked 'X' on system menu */
    {
      /* if an event has been set then return it otherwise
         fake an Alt+C
      */

      /* 20040610
         reject if not accepting input (topmost window not on focus) */
      if ( !hb_wvw_gtAcceptingInput() )
      {

        hb_wvw_gtInputNotAllowed( usWinNum, message, wParam, lParam );

        return(0);
      }

      if (usWinNum == 0)
      {
         if (s_usNumWindows==1)
         {

           hb_wvw_gtAddCharToInputQueue( HB_BREAK_FLAG );
           hb_wvw_gtAddCharToInputQueue( K_ESC );
         }
      }
      else
      {
         hb_wvw_gtAddCharToInputQueue( K_ESC );
      }

      return( 0 );
    }

    case WM_QUIT:
    case WM_DESTROY:
      return( 0 );

    case WM_RBUTTONDOWN:
    case WM_LBUTTONDOWN:
    case WM_RBUTTONUP:
    case WM_LBUTTONUP:
    case WM_RBUTTONDBLCLK:
    case WM_LBUTTONDBLCLK:
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    case WM_MBUTTONDBLCLK:
    case WM_MOUSEMOVE:
    case WM_MOUSEWHEEL:
    case WM_NCMOUSEMOVE:
    {

      if ( !hb_wvw_gtAcceptingInput() || ( usWinNum != s_usNumWindows-1 ) )
      {

        return(0);
      }

      hb_wvw_gtMouseEvent( pWindowData, hWnd, message, wParam, lParam );
      return( 0 );
    }

    case WM_TIMER:
    {

      if ( wParam < WVW_ID_BASE_TIMER && pWindowData->bPaintPending )
      {
         xUserPaintNow(usWinNum);
      }

      if ( wParam >= WVW_ID_BASE_TIMER && s_sApp.pSymWVW_TIMER )
      {
        xUserTimerNow(usWinNum, hWnd, message, wParam, lParam);
      }

      return( 0 );
    }

    case WM_HSCROLL :
    case WM_VSCROLL :
    {
      HWND hWndCtrl = (HWND) lParam;
      UINT uiXBid;
      byte bStyle;

      /* reject if not accepting input (topmost window not on focus) */

      if ( !hb_wvw_gtAcceptingInput() )
      {

        hb_wvw_gtInputNotAllowed( usWinNum, message, wParam, lParam );
        return(0);
      }

      /*************/

      uiXBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_SCROLLBAR, hWndCtrl, &bStyle) ;
      if (uiXBid==0)
      {

        return(0);
      }

      RunControlBlock(usWinNum, WVW_CONTROL_SCROLLBAR, hWndCtrl, message, wParam, lParam, 0 );

      return 0 ;
    } /* WM_VSCROLL  WM_HSCROLL */

    case WM_SIZE:
    {

      hb_wvw_gtResetWindowSize( pWindowData, hWnd );

      if ( s_sApp.pSymWVW_SIZE )
      {
        hb_vmPushState();
        hb_vmPushSymbol( s_sApp.pSymWVW_SIZE->pSymbol );
        hb_vmPushNil();
        hb_vmPushInteger( ( int ) (usWinNum)  );
        hb_vmPushLong( ( LONG ) hWnd    );
        hb_vmPushLong( ( LONG ) message );
        hb_vmPushLong( ( LONG ) wParam  );
        hb_vmPushLong( ( LONG ) lParam  );
        hb_vmDo( 5 );

        hb_vmPopState();
      }

      return( 0 );
    }

  }
  return( DefWindowProc( hWnd, message, wParam, lParam ) );
}

/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/

static HWND hb_wvw_gtCreateWindow( HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow )
{
  HWND     hWnd;
  WNDCLASS wndclass = { 0 };

  HB_SYMBOL_UNUSED( hPrevInstance );
  HB_SYMBOL_UNUSED( szCmdLine );

  InitCommonControls();

  wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS | CS_OWNDC;
  wndclass.lpfnWndProc   = hb_wvw_gtWndProc;
  wndclass.cbClsExtra    = 0;
  wndclass.cbWndExtra    = 0;
  wndclass.hInstance     = hInstance;
  wndclass.hIcon         = NULL;
  wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
  wndclass.hbrBackground = NULL;
  wndclass.lpszMenuName  = NULL;
  wndclass.lpszClassName = szAppName;

  if ( ! RegisterClass( &wndclass ) )
  {
    MessageBox( NULL, TEXT( "Failed to register class." ),
                szAppName, MB_ICONERROR );
    return( 0 );
  }

  hWnd = CreateWindow( szAppName,                         /*classname      */
     TEXT( "XHARBOUR_WVW" ),                              /*window name    */

     WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX | /*style          */
     WS_CLIPCHILDREN,

     0,                                                   /*x              */
     0,                                                   /*y              */
     CW_USEDEFAULT,                                       /*width          */
     CW_USEDEFAULT,                                       /*height         */
     NULL,                                                /*window parent  */
     NULL,                                                /*menu           */
     hInstance,                                           /*instance       */
     NULL );                                              /*lpParam        */

  if ( hWnd == NULL )
  {
    MessageBox( NULL, TEXT( "Failed to create window." ),
                  TEXT( "XHARBOUR_WVW" ), MB_ICONERROR );
    return NULL;
  }

  s_pWindows[ s_usNumWindows-1 ]->hWnd = hWnd;

  if ( s_sApp.pSymWVW_PAINT && s_uiPaintRefresh > 0)
  {
    SetTimer( hWnd, WVW_ID_SYSTEM_TIMER, (UINT) s_uiPaintRefresh, NULL );
  }

  /* If you wish to show window the way you want, put somewhere in your application
   * ANNOUNCE HB_NOSTARTUPWINDOW
   * If so compiled, then you need to issue Wvw_ShowWindow( nWinNum, SW_RESTORE )
   * at the point you desire in your code.
   */
  if ( hb_dynsymFind( "HB_NOSTARTUPWINDOW" ) != NULL )
  {
     iCmdShow = SW_HIDE;
  }

  ShowWindow( hWnd, iCmdShow );

  UpdateWindow( hWnd );

  return( hWnd ) ;
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtCreateToolTipWindow( WIN_DATA * pWindowData )
{
   INITCOMMONCONTROLSEX icex = { 0 };
   HWND                 hwndTT;
   TOOLINFO             ti = { 0 };

   /* Load the tooltip class from the DLL.
    */
   icex.dwSize = sizeof( icex );
   icex.dwICC  = ICC_BAR_CLASSES;

   if( !InitCommonControlsEx( &icex ) )
   {
      return;
   }

   /* Create the tooltip control.
    *
    *TODO: shouldn't we set hWndOwner to pWindowData->hWnd instead of NULL?
    */
   hwndTT = CreateWindow( TOOLTIPS_CLASS, TEXT( "" ),
                          WS_POPUP | TTS_ALWAYSTIP ,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL,
                          ( HMENU ) NULL,
                          ( HINSTANCE ) hb_hInstance,
                          NULL );

   SetWindowPos( hwndTT,
                 HWND_TOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE );

   /* Prepare TOOLINFO structure for use as tracking tooltip.
    */
   ti.cbSize    = sizeof( TOOLINFO );
   ti.uFlags    = TTF_SUBCLASS;
   ti.hwnd      = pWindowData->hWnd;
   ti.uId       = WVW_ID_BASE_TOOLTIP+pWindowData->byWinId;
   ti.hinst     = ( HINSTANCE ) hb_hInstance;
   ti.lpszText  = "";
   ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

   /* Add the tool to the control, displaying an error if needed.
    */
   if( ! SendMessage( hwndTT, TTM_ADDTOOL, 0, ( LPARAM ) &ti ) )
   {
      return ;
   }

   pWindowData->hWndTT = hwndTT;
}

/*-------------------------------------------------------------------*/

static DWORD hb_wvw_gtProcessMessages( WIN_DATA * pWindowData )
{
  MSG msg;
  int  iIndex;
  BOOL bProcessed;

  /* See if we have some graphic object to draw */

  if ( hb_gt_gobjects == NULL )
  {
    last_gobject = NULL;
  }
  else if( hb_gt_gobjects_end != last_gobject )
  {
    last_gobject = hb_gt_gobjects_end;
    InvalidateRect( pWindowData->hWnd, NULL, FALSE );
  }

  while ( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
  {

    if ( s_iScrolling && msg.message == WM_LBUTTONUP )
    {

      s_iWrongButtonUp++;
      if (s_iWrongButtonUp >= s_iMaxWrongButtonUp)
      {
        HWND hMouseCapturer;

        hMouseCapturer = GetCapture();
        if (hMouseCapturer)
        {

          SendMessage( hMouseCapturer, WM_LBUTTONUP, 0, 0 );
          ReleaseCapture();
        }

        s_iScrolling = 0;

      }

      return 0;
    }
    else
    {
      s_iWrongButtonUp = 0;
      PeekMessage( &msg, NULL, 0, 0, PM_REMOVE );
    }

    bProcessed = FALSE;
    for ( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
    {
       if ( s_sApp.hDlgModeless[ iIndex ] != 0 )
       {
          if ( IsDialogMessage( s_sApp.hDlgModeless[ iIndex ], &msg ) )
          {
             bProcessed = TRUE;
             break;
          }
       }
    }

    if ( bProcessed == FALSE )
    {
       TranslateMessage( &msg );
       DispatchMessage( &msg );
    }
  }
  return( msg.wParam );

}

/*-------------------------------------------------------------------*/

POINT HB_EXPORT hb_wvw_gtGetXYFromColRow( WIN_DATA * pWindowData, USHORT col, USHORT row )
{
  POINT xy = { 0 };

  xy.x = ( col ) * pWindowData->PTEXTSIZE.x;

  xy.y = ( row ) * hb_wvw_LineHeight( pWindowData ) + (pWindowData->byLineSpacing / 2);

  xy.y += pWindowData->usTBHeight;

  return( xy );
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtUnreachedXY( WIN_DATA * pWindowData, int *cols, int *rows )
{
  RECT       ci = { 0 };
  POINT      xy = { 0 };

  if ( !IsZoomed( pWindowData->hWnd ) )
  {
    if (rows) (*rows) = 0;
    if (cols) (*cols) = 0;
    return;
  }

  xy = hb_wvw_gtGetXYFromColRow( pWindowData, pWindowData->COLS, pWindowData->ROWS );

  GetClientRect( pWindowData->hWnd, &ci );

  if (rows) (*rows) = ci.bottom - xy.y - pWindowData->usSBHeight;
  if (cols) (*cols) = ci.right - xy.x;
}

/*-------------------------------------------------------------------*/

/*
 * get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 *
 */
static POINT hb_wvw_gtGetColRowFromXY( WIN_DATA * pWindowData, USHORT x, USHORT y )
{
  POINT colrow = { 0 };

  colrow.x = ( x/pWindowData->PTEXTSIZE.x );

  y -= pWindowData->usTBHeight;

  colrow.y = ( y/ ( pWindowData->PTEXTSIZE.y  + pWindowData->byLineSpacing ) );

  return( colrow );
}

/*-------------------------------------------------------------------*/
/*
 * return a rectangle with row and column data, corresponding to the XY pixel
 * coordinates
 * This works because we are using the FIXED system font
 *
 */

static RECT hb_wvw_gtGetColRowFromXYRect( WIN_DATA * pWindowData, RECT xy )
{
  RECT colrow = { 0 };
  USHORT usLineSpaces;

  xy.top    -= pWindowData->usTBHeight;
  xy.bottom -= pWindowData->usTBHeight;

  /* TODO: pls improve efficiency */
  usLineSpaces = pWindowData->byLineSpacing;

  colrow.left   = ( xy.left   / pWindowData->PTEXTSIZE.x );
  colrow.top    = ( xy.top    / (pWindowData->PTEXTSIZE.y + usLineSpaces) );

  /* Adjust for when rectangle EXACTLY overlaps characters */
  colrow.right  = ( xy.right  / pWindowData->PTEXTSIZE.x - ( xy.right % pWindowData->PTEXTSIZE.x ? 0 : 1 ) );
  colrow.bottom = ( xy.bottom / (pWindowData->PTEXTSIZE.y + usLineSpaces) - ( xy.bottom % (pWindowData->PTEXTSIZE.y + usLineSpaces) ? 0 : 1 ) );

  return( colrow );
}

/*-------------------------------------------------------------------*/
/*
 * return a rectangle with the XY pixel coordinates corresponding to
 * the row and column data
 * This works because we are using the FIXED system font
 *
 */
static RECT hb_wvw_gtGetXYFromColRowRect( WIN_DATA * pWindowData, RECT colrow )
{
  RECT xy = { 0 };

  xy.left   = ( colrow.left     ) * pWindowData->PTEXTSIZE.x;

  xy.top    = ( colrow.top      ) * hb_wvw_LineHeight( pWindowData ) + (pWindowData->byLineSpacing / 2);

  xy.right  = ( colrow.right+1  ) * pWindowData->PTEXTSIZE.x;

  xy.bottom = ( colrow.bottom+1 ) * hb_wvw_LineHeight( pWindowData )
              - (pWindowData->byLineSpacing / 2);

  xy.top    += pWindowData->usTBHeight;
  xy.bottom += pWindowData->usTBHeight;

  return( xy );
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtCreateCaret(WIN_DATA * pWindowData)
{
   /* create and show the caret
    * create an underline caret of height - _s.CaretSize
    */

   if (pWindowData->byWinId == s_usNumWindows-1)
   {
     if (!s_bVertCaret)
     {
       s_sApp.CaretExist = CreateCaret( pWindowData->hWnd, ( HBITMAP ) NULL, pWindowData->PTEXTSIZE.x, pWindowData->CaretSize );
     }
     else
     {
       s_sApp.CaretExist = CreateCaret( pWindowData->hWnd, ( HBITMAP ) NULL, pWindowData->CaretSize, pWindowData->PTEXTSIZE.y );
     }
   }
   else
   {
     s_sApp.CaretExist = FALSE;
   }

   if ( s_sApp.CaretExist && s_sApp.displayCaret )
   {
      hb_wvw_gtSetCaretPos(pWindowData);
      ShowCaret( pWindowData->hWnd );
   }
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtKillCaret( WIN_DATA * pWindowData )
{
   HB_SYMBOL_UNUSED( pWindowData );

   if ( s_sApp.CaretExist )
   {
      DestroyCaret();
      s_sApp.CaretExist = FALSE ;
   }
}

/*-------------------------------------------------------------------*/
/*
 * hb_wvw_gtSetCaretPos converts col and row to x and y ( pixels ) and calls
 * the Windows function SetCaretPos ( with the expected coordinates )
 */

static BOOL hb_wvw_gtSetCaretPos(WIN_DATA * pWindowData)
{
  POINT xy = { 0 };

  xy = hb_wvw_gtGetXYFromColRow( pWindowData, (SHORT) pWindowData->caretPos.x, (SHORT) pWindowData->caretPos.y );
  if ( pWindowData->CaretSize > 0 )
  {
    if (!s_bVertCaret)
    {

      xy.y += ( pWindowData->PTEXTSIZE.y - pWindowData->CaretSize );
    }

  }
  else
  {
    if (!s_bVertCaret)
    {
      xy.y -= pWindowData->CaretSize;
    }
    else
    {
      xy.x += pWindowData->PTEXTSIZE.x;
    }
  }
  if ( s_sApp.CaretExist )
  {
    SetCaretPos( xy.x, xy.y );
  }
  return( TRUE );
}

/*-------------------------------------------------------------------*/
/*
 * hb_wvw_gtValidateRow checks the row bounds for the caret, wrapping if indicated
 */

static void hb_wvw_gtValidateRow( WIN_DATA * pWindowData )
{
  if ( pWindowData->caretPos.y < 0 )
  {
    pWindowData->caretPos.y = pWindowData->ROWS-1;
    if ( pWindowData->caretPos.x > 0 )
    {
      pWindowData->caretPos.x--;
    }
    else
    {
      pWindowData->caretPos.x = pWindowData->COLS-1;
    }
  }
  else if ( pWindowData->caretPos.y >= pWindowData->ROWS )
  {
    pWindowData->caretPos.y = 0;
    if ( pWindowData->caretPos.x < pWindowData->COLS-1 )
    {
      pWindowData->caretPos.x++;
    }
    else
    {
       pWindowData->caretPos.x = 0;
    }
  }
}

/*-------------------------------------------------------------------*/
/*
 * hb_wvw_gtValidateCol checks the column bounds for the caret, wrapping if indicated
 */

static void hb_wvw_gtValidateCol( WIN_DATA * pWindowData )
{
  if ( pWindowData->caretPos.x < 0 )
  {
    pWindowData->caretPos.x = pWindowData->COLS-1;
    if ( pWindowData->caretPos.y > 0 )
    {
      pWindowData->caretPos.y--;
    }
    else
    {
      pWindowData->caretPos.y = pWindowData->ROWS-1;
    }
  }
  else if ( pWindowData->caretPos.x >= pWindowData->COLS )
  {
    pWindowData->caretPos.x = 0;
    if ( pWindowData->caretPos.y < pWindowData->ROWS-1 )
    {
      pWindowData->caretPos.y++;
    }
    else
    {
      pWindowData->caretPos.y = 0;
    }
  }
}

/*-------------------------------------------------------------------*/
/*
 * hb_wvw_gtValidateCaret checks the bounds for the caret, wrapping if indicated
 * before setting the caret position on the screen
 */

static void hb_wvw_gtValidateCaret( WIN_DATA * pWindowData )
{
  hb_wvw_gtValidateCol( pWindowData );
  hb_wvw_gtValidateRow( pWindowData );

  /* send message to window to display updated caret
   */
  SendMessage( pWindowData->hWnd, WM_MY_UPDATE_CARET, 0, 0 );
}

/*-------------------------------------------------------------------*/
/*
 * hb_wvw_gtGetIndexForTextBuffer takes a row and column, and returns the appropriate
 * index into the screen Text buffer
 */

static USHORT hb_wvw_gtGetIndexForTextBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row )
{
  return( row * pWindowData->COLS + col );
}

/*-------------------------------------------------------------------*/
 /*
  * hb_wvw_gtGetColRowForTextBuffer takes an index into the screen Text buffer
  * and returns the corresponding row and column
  */
static POINT hb_wvw_gtGetColRowForTextBuffer( WIN_DATA * pWindowData, USHORT index )
{
  POINT colrow = { 0 };

  colrow.x = index % pWindowData->COLS;
  colrow.y = index / pWindowData->COLS;

  return( colrow );
}

/*-------------------------------------------------------------------*/
/*
 * hb_wvw_gtTextOut converts col and row to x and y ( pixels ) and calls
 * the Windows function TextOut with the expected coordinates
 */

static BOOL hb_wvw_gtTextOut( WIN_DATA * pWindowData, HDC hdc,  USHORT col, USHORT row, LPCTSTR lpString, USHORT cbString  )
{
  BOOL Result ;
  POINT xy = { 0 };
  RECT mClip = { 0 };

  if ( cbString > pWindowData->COLS )
  {
    cbString = pWindowData->COLS;
  }
  xy = hb_wvw_gtGetXYFromColRow( pWindowData, col, row );

  /* safer solution by Oscar Hernandez Suarez: */

  SetRect(&mClip, xy.x, xy.y, xy.x+cbString*pWindowData->PTEXTSIZE.x, xy.y+pWindowData->PTEXTSIZE.y);
  if ( pWindowData->FixedFont )
  {
    Result = ExtTextOut(hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &mClip, lpString,
    cbString, NULL);
  }
  else
  {
    Result = ExtTextOut(hdc, xy.x, xy.y, ETO_CLIPPED|ETO_OPAQUE, &mClip, lpString,
    cbString, pWindowData->FixedSize);
  }

  return( Result ) ;
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/* get for and background colours from attribute and set them for window
*/

static BOOL hb_wvw_gtSetColors( WIN_DATA * pWindowData, HDC hdc, BYTE attr )
{
  int fore = attr & 0x000F;
  int back = ( attr & 0x00F0 )>>4;

  pWindowData->foreground = _COLORS[ fore ];
  pWindowData->background = _COLORS[ back ];

  SetTextColor( hdc, pWindowData->foreground );
  SetBkColor( hdc, pWindowData->background );

  return( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/* compute invalid rect in pixels, from row and col
*/
static void hb_wvw_gtSetInvalidRect( WIN_DATA * pWindowData, USHORT left, USHORT top, USHORT right, USHORT bottom )
{
  RECT rect = { 0 };

  if ( pWindowData->InvalidateWindow )
  {
    rect.left   = left;
    rect.top    = top;
    rect.right  = right;
    rect.bottom = bottom;

    rect = hb_wvw_gtGetXYFromColRowRect( pWindowData, rect );

    /* check for wrapping */
    /*                    */
    rect.left = min( rect.left, rect.right );
    rect.top  = min( rect.top, rect.bottom );

    rect.right  = max( rect.left, rect.right );
    rect.bottom = max( rect.top, rect.bottom );

    rect.top    -= (pWindowData->byLineSpacing / 2);
    rect.bottom += (pWindowData->byLineSpacing / 2);

    if ( pWindowData->RectInvalid.left < 0 )
    {
      memcpy( &(pWindowData->RectInvalid), &rect, sizeof( RECT ) );
    }
    else
    {
      pWindowData->RectInvalid.left   = min( pWindowData->RectInvalid.left  , rect.left   );
      pWindowData->RectInvalid.top    = min( pWindowData->RectInvalid.top   , rect.top    );
      pWindowData->RectInvalid.right  = max( pWindowData->RectInvalid.right , rect.right  );
      pWindowData->RectInvalid.bottom = max( pWindowData->RectInvalid.bottom, rect.bottom );
    }
    hb_wvw_gtDoInvalidateRect( pWindowData ) ;
  }
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtDoInvalidateRect( WIN_DATA * pWindowData )
{

  if ( hb_wvw_gt_usDispCount( pWindowData ) <= 0 && ( pWindowData->RectInvalid.left != -1 ) )
  {

    InvalidateRect( pWindowData->hWnd, &pWindowData->RectInvalid, FALSE );

    pWindowData->RectInvalid.left = -1 ;
    hb_wvw_gtProcessMessages( pWindowData );
  }
}

/*-------------------------------------------------------------------*/

/*NOTE: this function is called when after a key event occurs.
 *      since we are accepting input only from focused topmost window, no need to handle input on other window
 *      (in current design, only topmost window accepting input)
 */

static void hb_wvw_gtTranslateKey( int key, int shiftkey, int altkey, int controlkey )
{
  int nVirtKey = GetKeyState( VK_MENU );
  if ( nVirtKey & 0x8000 )
  {
    hb_wvw_gtAddCharToInputQueue( altkey );
  }
  else
  {
    nVirtKey = GetKeyState( VK_CONTROL );
    if ( nVirtKey & 0x8000 )
    {
      hb_wvw_gtAddCharToInputQueue( controlkey );
    }
    else
    {
      nVirtKey = GetKeyState( VK_SHIFT );
      if ( nVirtKey & 0x8000 )
      {
        hb_wvw_gtAddCharToInputQueue( shiftkey );
      }
      else
      {
        hb_wvw_gtAddCharToInputQueue( key );
      }
    }
  }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/* font stuff                                                        */
/* use the standard fixed oem font, unless the caller has requested set size fonts
*/

static HFONT hb_wvw_gtGetFont( char * pszFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage )
{
  HFONT hFont;
  if ( iHeight > 0 )
  {
    LOGFONT logfont = {0};

    logfont.lfEscapement     = 0;
    logfont.lfOrientation    = 0;
    logfont.lfWeight         = iWeight ;
    logfont.lfItalic         = 0;
    logfont.lfUnderline      = 0;
    logfont.lfStrikeOut      = 0;
    logfont.lfCharSet        = iCodePage;             /* OEM_CHARSET;                                    */
    logfont.lfOutPrecision   = 0;
    logfont.lfClipPrecision  = 0;
    logfont.lfQuality        = iQuality;              /* DEFAULT_QUALITY, DRAFT_QUALITY or PROOF_QUALITY */
    logfont.lfPitchAndFamily = FIXED_PITCH+FF_MODERN; /* all mapping depends on fixed width fonts!       */
    logfont.lfHeight         = iHeight;
    logfont.lfWidth          = iWidth < 0 ? -iWidth : iWidth ;

    strcpy( logfont.lfFaceName,pszFace );

    hFont = CreateFontIndirect( &logfont );
  }
  else
  {

    hFont = ( HFONT ) GetStockObject( OEM_FIXED_FONT );
  }
  return( hFont );

}

/*-------------------------------------------------------------------*/

static void gt_hbInitStatics( USHORT usWinNum, LPCTSTR lpszWinName, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 )
{
  OSVERSIONINFO osvi ;
  HINSTANCE h;
  WIN_DATA * pWindowData;
  WIN_DATA * pPrevWindow;
  int           iIndex;

  pWindowData = s_pWindows[ usWinNum ];

  if (usWinNum == 0)
  {
    pWindowData->byWinId          = (BYTE) usWinNum;
    strcpy(pWindowData->szWinName, lpszWinName);

    pWindowData->usRowOfs         = usRow1;
    pWindowData->usColOfs         = usCol1;
    pWindowData->uiDispCount      = 0;

    pWindowData->ROWS             = usRow2-usRow1+1;
    pWindowData->COLS             = usCol2-usCol1+1;
    pWindowData->foreground       = WHITE;
    pWindowData->background       = BLACK;
    pWindowData->BUFFERSIZE       = 0;
    pWindowData->pAttributes      = NULL;
    pWindowData->pBuffer          = NULL;
    pWindowData->caretPos.x       = 0;
    pWindowData->caretPos.y       = 0;

    s_sApp.CaretExist       = FALSE;

    pWindowData->CaretSize        = 2;
    pWindowData->mousePos.x       = 0;
    pWindowData->mousePos.y       = 0;
    pWindowData->MouseMove        = FALSE ;
    pWindowData->hWnd             = NULL;
    pWindowData->keyPointerIn     = 1;
    pWindowData->keyPointerOut    = 0;

    s_sApp.displayCaret     = TRUE;

    pWindowData->RectInvalid.left = -1 ;

    pWindowData->PTEXTSIZE.x      = 8;
    pWindowData->PTEXTSIZE.y      = 12;

    pWindowData->fontHeight       = 20;
    pWindowData->fontWidth        = 10;
    pWindowData->fontWeight       = FW_NORMAL;
    pWindowData->fontQuality      = DEFAULT_QUALITY;
    strcpy( pWindowData->fontFace,"Courier New" );

    pWindowData->LastMenuEvent    = 0;
    pWindowData->MenuKeyEvent     = WVW_DEFAULT_MENUKEYEVENT;
    pWindowData->CentreWindow     = TRUE;       /* Default is to always display window in centre of screen */

    /* two following parameters are meaningful only if CentreWindow is FALSE */
    pWindowData->HCentreWindow    = FALSE;      /* horizontally */
    pWindowData->VCentreWindow    = FALSE;      /* vertically */

    pWindowData->CodePage         = GetACP() ;  /* Set code page to default system                         */

    osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
    GetVersionEx ( &osvi );
    s_sApp.Win9X            = ( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
    s_sApp.AltF4Close       = FALSE;

    pWindowData->InvalidateWindow = TRUE;
    pWindowData->EnableShortCuts  = TRUE;

    pWindowData->bToolTipActive   = FALSE;
    pWindowData->hWndTT           = NULL;
    pWindowData->hPopup           = NULL;

    s_lfPB.lfHeight         = pWindowData->fontHeight - 2;
    s_lfPB.lfWidth          = 0;
      s_lfPB.lfEscapement     = 0;
      s_lfPB.lfOrientation    = 0;
    s_lfPB.lfWeight         = 0;
    s_lfPB.lfItalic         = 0;
    s_lfPB.lfUnderline      = 0;
    s_lfPB.lfStrikeOut      = 0;
    s_lfPB.lfCharSet        = DEFAULT_CHARSET;

    s_lfPB.lfQuality        = DEFAULT_QUALITY;
    s_lfPB.lfPitchAndFamily = FF_DONTCARE;
    strcpy( s_lfPB.lfFaceName, "Arial" );

    s_lfCB.lfHeight         = pWindowData->fontHeight - 2;
    s_lfCB.lfWidth          = 0;
      s_lfCB.lfEscapement     = 0;
      s_lfCB.lfOrientation    = 0;
    s_lfCB.lfWeight         = 0;
    s_lfCB.lfItalic         = 0;
    s_lfCB.lfUnderline      = 0;
    s_lfCB.lfStrikeOut      = 0;
    s_lfCB.lfCharSet        = DEFAULT_CHARSET;

    s_lfCB.lfQuality        = DEFAULT_QUALITY;
    s_lfCB.lfPitchAndFamily = FF_DONTCARE;
    strcpy( s_lfCB.lfFaceName, "Arial" );

    s_sApp.pSymWVW_PAINT    = hb_dynsymFind( "WVW_PAINT" ) ;
    s_sApp.pSymWVW_SETFOCUS = hb_dynsymFind( "WVW_SETFOCUS" ) ;
    s_sApp.pSymWVW_KILLFOCUS= hb_dynsymFind( "WVW_KILLFOCUS" ) ;
    s_sApp.pSymWVW_MOUSE    = hb_dynsymFind( "WVW_MOUSE" ) ;
    s_sApp.pSymWVW_MENUSELECT = hb_dynsymFind( "WVW_MENUSELECT" ) ;

    s_sApp.pSymWVW_SIZE     = hb_dynsymFind( "WVW_SIZE" ) ;

    s_sApp.pSymWVW_INPUTFOCUS = hb_dynsymFind( "WVW_INPUTFOCUS" ) ;
    s_sApp.pSymWVW_TIMER = hb_dynsymFind( "WVW_TIMER" ) ;

    h = LoadLibraryEx( "msimg32.dll", NULL, 0 );
    if ( h )
    {
      s_sApp.pfnGF = ( wvwGradientFill ) GetProcAddress( h, "GradientFill" );
      if ( s_sApp.pfnGF )
      {
        s_sApp.hMSImg32 = h;
      }
    }

    for ( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
    {
       s_sApp.hDlgModeless[ iIndex ]        = NULL;

       s_sApp.pFunc[ iIndex ]               = NULL;
       s_sApp.iType[ iIndex ]               = (int) NULL;
    }

    for ( iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++ )
    {
       s_sApp.hDlgModal[ iIndex ]           = NULL;
       s_sApp.pFuncModal[ iIndex ]          = NULL;
       s_sApp.iTypeModal[ iIndex ]          = ( int ) NULL;
    }

    s_sApp.pbhBitmapList = NULL;

  }
  else
  {

    if (!s_bMainCoordMode)
    {
      pPrevWindow = s_pWindows[ s_usCurWindow ];
    }
    else
    {
      pPrevWindow = s_pWindows[ usWinNum-1 ];
    }

    pWindowData->byWinId          = (BYTE) usWinNum;
    strcpy(pWindowData->szWinName, lpszWinName);
    pWindowData->usRowOfs         = usRow1;
    pWindowData->usColOfs         = usCol1;
    pWindowData->uiDispCount      = 0;

    pWindowData->ROWS             = usRow2-usRow1+1;
    pWindowData->COLS             = usCol2-usCol1+1;

    pWindowData->foreground       = pPrevWindow->foreground;
    pWindowData->background       = pPrevWindow->background;
    pWindowData->BUFFERSIZE       = 0;
    pWindowData->pAttributes      = NULL;
    pWindowData->pBuffer          = NULL;
    pWindowData->caretPos.x       = 0;
    pWindowData->caretPos.y       = 0;

    pWindowData->CaretSize        = pPrevWindow->CaretSize;
    pWindowData->mousePos.x       = 0;
    pWindowData->mousePos.y       = 0;
    pWindowData->MouseMove        = pPrevWindow->MouseMove;
    pWindowData->hWnd             = NULL;
    pWindowData->keyPointerIn     = 1;
    pWindowData->keyPointerOut    = 0;

    pWindowData->RectInvalid.left = -1 ;
    pWindowData->PTEXTSIZE.x      = pPrevWindow->PTEXTSIZE.x;
    pWindowData->PTEXTSIZE.y      = pPrevWindow->PTEXTSIZE.y;
    pWindowData->fontHeight       = pPrevWindow->fontHeight;
    pWindowData->fontWidth        = pPrevWindow->fontWidth;
    pWindowData->fontWeight       = pPrevWindow->fontWeight;
    pWindowData->fontQuality      = pPrevWindow->fontQuality;
    strcpy( pWindowData->fontFace, pPrevWindow->fontFace);
    pWindowData->LastMenuEvent    = 0;
    pWindowData->MenuKeyEvent     = WVW_DEFAULT_MENUKEYEVENT;

    pWindowData->CentreWindow     = s_bDefCentreWindow;

    /* two following parameters are meaningful only if CentreWindow is FALSE */
    pWindowData->HCentreWindow    = s_bDefHCentreWindow;      /* horizontally */
    pWindowData->VCentreWindow    = s_bDefVCentreWindow;      /* vertically */

    pWindowData->CodePage         = pPrevWindow->CodePage;

    pWindowData->InvalidateWindow = TRUE;
    pWindowData->EnableShortCuts  = pPrevWindow->EnableShortCuts;

    pWindowData->bToolTipActive   = FALSE;
    pWindowData->hWndTT           = NULL;
    pWindowData->hPopup           = NULL;

  }

  pWindowData->bIgnoreWM_SYSCHAR = FALSE;
  pWindowData->bPaint            = FALSE;
  pWindowData->bGetFocus         = FALSE;

  pWindowData->byLineSpacing = s_byDefLineSpacing;

  pWindowData->iLSpaceColor = s_iDefLSpaceColor;

  pWindowData->bPaintPending = FALSE;
  hb_wvw_InitPendingRect( pWindowData );

  pWindowData->hStatusBar = NULL;
  pWindowData->usSBHeight= 0;

  pWindowData->hToolBar = NULL;
  pWindowData->usTBHeight= 0;

  pWindowData->pcdCtrlList = NULL;

  pWindowData->hPBfont = NULL;  /* will be created on first creation of pushbutton, if ever */

  pWindowData->hCBfont = NULL;  /* will be created on first creation of combobox, if ever */

  s_usCurWindow = usWinNum;
}

/*-------------------------------------------------------------------*/
/*
 *  functions for handling the input queues for the mouse and keyboard
 */

/*NOTE: current design allows topmost window only who accepts input */

void HB_EXPORT hb_wvw_gtAddCharToInputQueue ( int data )
{
  int iNextPos;

  iNextPos = ( s_pWindows[ s_usNumWindows-1 ]->keyPointerIn >= WVW_CHAR_QUEUE_SIZE - 1 ) ? 0 : s_pWindows[ s_usNumWindows-1 ]->keyPointerIn+1 ;
  if ( iNextPos != s_pWindows[ s_usNumWindows-1 ]->keyPointerOut )
  {
    s_pWindows[ s_usNumWindows-1 ]->Keys[ s_pWindows[ s_usNumWindows-1 ]->keyPointerIn ] = data ;
    s_pWindows[ s_usNumWindows-1 ]->keyPointerIn = iNextPos ;
  }
}

/*-------------------------------------------------------------------*/

static BOOL hb_wvw_gtGetCharFromInputQueue ( int *c )
{
  int iNextPos;
  BOOL bRet = FALSE;

  *c = 0;

  iNextPos = ( s_pWindows[ s_usNumWindows-1 ]->keyPointerOut >= WVW_CHAR_QUEUE_SIZE - 1 ) ? 0 : s_pWindows[ s_usNumWindows-1 ]->keyPointerOut+1 ;
  if ( iNextPos != s_pWindows[ s_usNumWindows-1 ]->keyPointerIn )
  {
    *c = s_pWindows[ s_usNumWindows-1 ]->Keys[ iNextPos ] ;
    s_pWindows[ s_usNumWindows-1 ]->keyPointerOut = iNextPos ;
    bRet =  TRUE;
  }
  return( bRet );
}

/*-------------------------------------------------------------------*/

static USHORT hb_wvw_gtGetMouseX ( WIN_DATA * pWindowData )
{
  return( (SHORT) pWindowData->mousePos.x );
}

/*-------------------------------------------------------------------*/

static USHORT hb_wvw_gtGetMouseY ( WIN_DATA * pWindowData )
{
  return( (SHORT) pWindowData->mousePos.y );
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtSetMouseX ( WIN_DATA * pWindowData, USHORT ix )
{
  pWindowData->mousePos.x = ix;
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtSetMouseY ( WIN_DATA * pWindowData, USHORT iy )
{
  pWindowData->mousePos.y = iy;
}

/*-------------------------------------------------------------------*/
/*
 * hb_wvw_gtSetStringInTextBuffer puts the string of the specified length into the TextBuffer at
 * the specified caret position
 * It then determines the invalid rectangle, so the string will be displayed
 */
static void hb_wvw_gtSetStringInTextBuffer( WIN_DATA * pWindowData, USHORT col, USHORT row, BYTE attr, BYTE *sBuffer, USHORT length )
{
  POINT end = { 0 };
  USHORT index;

  /* determine the index and put the string into the TextBuffer
   */
  index = hb_wvw_gtGetIndexForTextBuffer( pWindowData, col, row );
  if ( length + index <= pWindowData->BUFFERSIZE )
  {
    memcpy( ( pWindowData->pBuffer+index ), sBuffer, length );

    memset( ( pWindowData->pAttributes+index ), attr, length );

    /*  determine bounds of rect around character to refresh
     */
    end = hb_wvw_gtGetColRowForTextBuffer( pWindowData, index + ( length -1 ) );
    hb_wvw_gtSetInvalidRect( pWindowData, (SHORT) col, (SHORT) row, (SHORT) end.x, (SHORT) end.y );
  }
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtSetCaretOn( WIN_DATA * pWindowData, BOOL bOn )
{

  if ( s_sApp.CaretExist )
  {
    if ( bOn )
    {
      hb_wvw_gtSetCaretPos(pWindowData);
      ShowCaret( pWindowData->hWnd );
    }
    else
    {
      HideCaret( pWindowData->hWnd );
    }
  }

  s_sApp.displayCaret = bOn;
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtHandleMenuSelection( int menuIndex )
{
  s_pWindows[ s_usNumWindows-1 ]->LastMenuEvent = menuIndex ;
  hb_wvw_gtAddCharToInputQueue( s_pWindows[ s_usNumWindows-1 ]->MenuKeyEvent );
}

/*-------------------------------------------------------------------*/

static void hb_wvw_gtMouseEvent( WIN_DATA * pWindowData, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  POINT xy = { 0 }, colrow = { 0 } ;
  SHORT keyCode = 0;
  SHORT keyState = 0;
  ULONG lPopupRet ;

  HB_SYMBOL_UNUSED( hWnd );
  HB_SYMBOL_UNUSED( wParam );

  if ( !b_MouseEnable )
  {
    return;
  }
  else
  {

    if ( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE )
    {
      if ( ! pWindowData->MouseMove )
      {
        return;
      }
    }

    xy.x   = LOWORD( lParam );
    xy.y   = HIWORD( lParam );

    colrow = hb_wvw_gtGetColRowFromXY( pWindowData, (SHORT) xy.x, (SHORT) xy.y );

    hb_wvw_gtSetMouseX( pWindowData, (SHORT) colrow.x );
    hb_wvw_gtSetMouseY( pWindowData, (SHORT) colrow.y );

    switch( message )
    {
      case WM_LBUTTONDBLCLK:
        keyCode = K_LDBLCLK;
        break;

      case WM_RBUTTONDBLCLK:
        keyCode = K_RDBLCLK;
        break;

      case WM_LBUTTONDOWN:
        {

          HWND hWndFocus = (HWND) GetFocus();

          if ( GetControlClass(pWindowData->byWinId, hWndFocus) > 0 )
          {

            SetFocus(hWnd);
          }

        }
        keyCode = K_LBUTTONDOWN;
        break;

      case WM_RBUTTONDOWN:
        keyCode = K_RBUTTONDOWN;
        break;

      case WM_LBUTTONUP:
        keyCode = K_LBUTTONUP;
        break;

      case WM_RBUTTONUP:

        if ( pWindowData->hPopup )
        {
           GetCursorPos( &xy );
           lPopupRet = TrackPopupMenu( pWindowData->hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, NULL );
           if ( lPopupRet )
           {
              hb_wvw_gtAddCharToInputQueue( lPopupRet );
           }
          return;
        }
        else
        {
          keyCode = K_RBUTTONUP;
          break;
        }

      case WM_MBUTTONDOWN:
        keyCode = K_MBUTTONDOWN;
        break;

      case WM_MBUTTONUP:
        keyCode = K_MBUTTONUP;
        break;

      case WM_MBUTTONDBLCLK:
        keyCode = K_MDBLCLK;
        break;

      case WM_MOUSEMOVE:
        keyState = wParam;

        if      ( keyState == MK_LBUTTON )
        {
           keyCode = K_MMLEFTDOWN;
        }
        else if ( keyState == MK_RBUTTON )
        {
           keyCode = K_MMRIGHTDOWN;
        }
        else if ( keyState == MK_MBUTTON )
        {
           keyCode = K_MMMIDDLEDOWN;
        }
        else
        {
           keyCode = K_MOUSEMOVE;
        }
        break;

      case WM_MOUSEWHEEL:
        keyState = HIWORD( wParam );

        if ( keyState > 0 )
        {
           keyCode = K_MWFORWARD;
        }
        else
        {
           keyCode = K_MWBACKWARD;
        }

        break;

      case WM_NCMOUSEMOVE:
         {
            keyCode = K_NCMOUSEMOVE;
         }
         break;
    }

    if ( s_sApp.pSymWVW_MOUSE && keyCode != 0 )
    {
      hb_vmPushState();
      hb_vmPushSymbol( s_sApp.pSymWVW_MOUSE->pSymbol );
      hb_vmPushNil();
      hb_vmPushInteger( ( int ) (pWindowData->byWinId)  );
      hb_vmPushLong( ( SHORT ) keyCode );
      hb_vmPushLong( ( SHORT ) colrow.y );
      hb_vmPushLong( ( SHORT ) colrow.x );
      hb_vmPushLong( ( SHORT ) keyState );
      hb_vmDo( 5 );

      hb_vmPopState();
    }

      hb_wvw_gtAddCharToInputQueue( keyCode );

  }
}

static void hb_wvw_gtWindowPrologue( void )
{

  if (s_usNumWindows >= WVW_MAXWINDOWS)
  {
    /*  Runtime error
     */
    hb_errRT_TERM( EG_BOUND, 10001, "Maximum Number of Windows Exceeded", "hb_wvw_gtWindowPrologue()", 0, 0 );
  }

  s_usNumWindows++;
  s_pWindows[s_usNumWindows-1] = (WIN_DATA *) hb_xgrab(sizeof(WIN_DATA));

}

static void hb_wvw_gtWindowEpilogue( void )
{

  if (s_usNumWindows == 0)
  {

    hb_errRT_TERM( EG_BOUND, 10001, "No more window to destroy", "hb_wvw_gtWindowEpilogue()", 0, 0 );
  }
  hb_xfree(s_pWindows[s_usNumWindows-1]);
  s_usNumWindows--;

  if (s_usNumWindows>0)
  {
    s_usCurWindow = s_usNumWindows-1;
  }
}

static USHORT hb_wvw_gtOpenWindow( LPCTSTR lpszWinName, int iRow1, int iCol1, int iRow2, int iCol2,
                                   DWORD dwStyle, int iParentWin )
{ /*assume s_usNumWindows >= 1 (ie. this will be the second or third window)
   *this is similar to gt_init(), only gt_init() is for Main Window
   *usRowx and usColx determine the initial position and initial size of window
   *(relative to MAIN window, NOT to parent window)
   */

    HWND hWnd;

    WNDCLASS wndclass;
    WIN_DATA * pParentWindow;
    int    iCmdShow;

    HB_TRACE( HB_TR_DEBUG, ( "hb_wvw_gtOpenWindow()" ) );

    /* in MainCoord Mode make sure that usRowx and usColx are within Main Window's bound! */
    if ( s_bMainCoordMode && (!hb_wvw_gtInWindow(0, iRow1, iCol1) || !hb_wvw_gtInWindow(0, iRow2, iCol2)) )
    {
      MessageBox( NULL, TEXT( "Invalid (Row,Col)" ),
                  lpszWinName, MB_ICONERROR );
      return( 0 );
    }

    if (iParentWin < 0)
    {
      pParentWindow = NULL;

    }
    else
    {

      pParentWindow = s_pWindows[ (USHORT) iParentWin ];
    }

    InitCommonControls();

    if ( !s_bSWRegistered && (s_usNumWindows == 1) )
    {
      wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS | CS_OWNDC;
      wndclass.lpfnWndProc   = hb_wvw_gtWndProc;
      wndclass.cbClsExtra    = 0;
      wndclass.cbWndExtra    = 0;
      wndclass.hInstance     = hb_hInstance;
      wndclass.hIcon         = NULL;
      wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
      wndclass.hbrBackground = NULL;
      wndclass.lpszMenuName  = NULL;
      wndclass.lpszClassName = szSubWinName;

      if ( ! RegisterClass( &wndclass ) )
      {

        MessageBox( NULL, TEXT( "Failed to register class." ),
                    szSubWinName, MB_ICONERROR );
        return( 0 );
      }

      s_bSWRegistered = TRUE;
    }

    hb_wvw_gtWindowPrologue( );
    gt_hbInitStatics(s_usNumWindows-1, lpszWinName, iRow1, iCol1, iRow2, iCol2);

    hWnd = CreateWindow( szSubWinName,
       lpszWinName,                              /* window name */

       dwStyle,

       /* notes: do NOT use WS_CHILD style for subwindows
                 child windows can NOT get input focus
          TODO: handle WM_MOVE to simulate behaviour similar to WS_CHILD's
                at least to keep subwindow "nearby" the MAIN window */

       0,                                                   /*x               */
       0,                                                   /*y               */
       CW_USEDEFAULT,                                       /*width           */
       CW_USEDEFAULT,                                       /*height          */

       (pParentWindow==NULL ? NULL : pParentWindow->hWnd),  /*x parent window */

       NULL,                                                /*menu            */
       (HINSTANCE) hb_hInstance,                            /*x instance      */
       NULL );                                              /*lpParam         */

    s_pWindows[ s_usNumWindows-1 ]->hWnd = hWnd;

    if ( hWnd == NULL )
    {

      LPVOID lpMsgBuf;

      FormatMessage(
          FORMAT_MESSAGE_ALLOCATE_BUFFER |
          FORMAT_MESSAGE_FROM_SYSTEM |
          FORMAT_MESSAGE_IGNORE_INSERTS,
          NULL,
          GetLastError(),
          MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
          (LPTSTR) &lpMsgBuf,
          0,
          NULL
      );

      MessageBox( NULL, (LPCTSTR)lpMsgBuf, "WINAPI failed to CreateWindow", MB_ICONERROR );
      LocalFree( lpMsgBuf );

      hb_wvw_gtWindowEpilogue( );

      return( 0 );
    }

    if ( s_sApp.pSymWVW_PAINT && s_uiPaintRefresh > 0)
    {
      SetTimer( hWnd, WVW_ID_SYSTEM_TIMER, (UINT) s_uiPaintRefresh, NULL );
    }

    /* If you wish to show window the way you want, put somewhere in your application
     * ANNOUNCE HB_NOSTARTUPWINDOW
     * If so compiled, then you need to issue Wvw_ShowWindow( nWinNum, SW_RESTORE )
     * at the point you desire in your code.
     */

    if ( s_bNOSTARTUPSUBWINDOW )
    {
       iCmdShow = SW_HIDE;
    }
    else
    {
       iCmdShow = SW_SHOWNORMAL;
    }

    ShowWindow( hWnd, iCmdShow );
    UpdateWindow( hWnd );

    hb_wvw_gtSetWindowTitle( s_usNumWindows-1, (char*) lpszWinName );

    hb_wvw_gtCreateObjects(s_usNumWindows-1);

    s_pWindows[ s_usNumWindows-1 ]->hdc = GetDC( s_pWindows[ s_usNumWindows-1 ]->hWnd );
    s_pWindows[ s_usNumWindows-1 ]->hCompDC = CreateCompatibleDC( s_pWindows[ s_usNumWindows-1 ]->hdc );

    /**
    if you want every window be created tooltip window
    but i think we'd better leave it to WVW_SetToolTipActive(.t.)

    if( b_MouseEnable )
    {
      hb_wvw_gtCreateToolTipWindow(s_pWindows[s_usNumWindows-1]);
    }

    **/

    return( s_usNumWindows-1 );
}

static void hb_wvw_gtCloseWindow( void )
{ /*assume s_usNumWindows >= 2 (ie. we are not closing main window)
   *similar to gt_exit(), only gt_exit() closes main window
   */

    WIN_DATA * pWindowData;
    CONTROL_DATA * pcd;

    HB_TRACE( HB_TR_DEBUG, ( "hb_wvw_gtCloseWindow()" ) );

    /* destroy objects from current (last/topmost) window */

    pWindowData = (WIN_DATA*) s_pWindows[ s_usNumWindows-1 ];

    if ( pWindowData->hWnd )
    {

      KillTimer( pWindowData->hWnd, WVW_ID_SYSTEM_TIMER );

      if ( s_sApp.pSymWVW_TIMER )
      {
        KillTimer( pWindowData->hWnd, WVW_ID_BASE_TIMER+s_usNumWindows-1 );
      }

      /* 20040921 IMPORTANT:
         All these PENs and BRUSHes deletions return OK,
         but GDI objects are reported as still in use by Task Manager.
         We now temporarily disable PENs and BRUSHes creation during
         window ppening.
         See also gt_exit.
         TODO: pls choose:
         (1) store PENs and BRUSHes as application-wide
         or
         (2) do the creation and deletion only when required
       */
      /* 20040923 choose #1 of above option
      DeleteObject( ( HPEN   ) pWindowData->penWhite );
      DeleteObject( ( HPEN   ) pWindowData->penWhiteDim );
      DeleteObject( ( HPEN   ) pWindowData->penBlack );
      DeleteObject( ( HPEN   ) pWindowData->penDarkGray );
      DeleteObject( ( HPEN   ) pWindowData->penGray );
      DeleteObject( ( HPEN   ) pWindowData->penNull );
      DeleteObject( ( HPEN   ) pWindowData->currentPen );
      DeleteObject( ( HBRUSH ) pWindowData->currentBrush );
      DeleteObject( ( HBRUSH ) pWindowData->diagonalBrush );
      DeleteObject( ( HBRUSH ) pWindowData->solidBrush );
      DeleteObject( ( HBRUSH ) pWindowData->wvwWhiteBrush );
      */

      DeleteObject( ( HFONT ) pWindowData->hFont );

      if ( pWindowData->hdc )
      {
        ReleaseDC( pWindowData->hWnd, pWindowData->hdc );
      }

      if ( pWindowData->hCompDC )
      {
        DeleteDC( pWindowData->hCompDC );
      }

      while (pWindowData->pcdCtrlList)
      {
        pcd     = pWindowData->pcdCtrlList->pNext;
        DestroyWindow (pWindowData->pcdCtrlList->hWndCtrl) ;

        if (pWindowData->pcdCtrlList->phiCodeBlock)
        {
           hb_itemRelease( pWindowData->pcdCtrlList->phiCodeBlock );

        }

        hb_xfree( pWindowData->pcdCtrlList );
        pWindowData->pcdCtrlList = pcd;
      }

      DestroyWindow( pWindowData->hWnd );

      if (pWindowData->hPBfont)
      {
        DeleteObject( ( HFONT ) pWindowData->hPBfont );
      }

      if (pWindowData->hCBfont)
      {
        DeleteObject( ( HFONT ) pWindowData->hCBfont );
      }
    }

    hb_wvw_gtWindowEpilogue(  );

    /*

    if (s_usNumWindows == 1)
    {
      if (!UnregisterClass( szSubWinName,( HINSTANCE ) hb_hInstance ))
      {
        MessageBox( NULL, TEXT( "Failed UnregisterClass" ),
                    szAppName, MB_ICONERROR );
      }
    }
    */

    SetFocus( s_pWindows[ s_usNumWindows-1 ]->hWnd );

}

static BOOL hb_wvw_gtBufferedKey( LONG lKey )
{

  return( lKey != VK_SHIFT &&
          lKey != VK_MENU &&
          lKey != VK_CONTROL &&
          lKey != VK_PAUSE &&
          lKey != VK_CAPITAL &&
          lKey != VK_NUMLOCK &&
          lKey != VK_SCROLL);
}

static BOOL hb_wvw_gtAcceptingInput( void )
{ /* returns TRUE if we are accepting input,
   * ie. Current focused window is the topmost window
   */

  HWND hWndFocus = (HWND) GetFocus();

  return ( hWndFocus == s_pWindows[ s_usNumWindows-1 ]->hWnd ||
           GetControlClass(s_usNumWindows-1, hWndFocus) > 0 );
}

/* this TIMERPROC is to flash the topmost window using FlashWindow.
   need to do it this way since FlashWindowEx is not available in Win95 */
static VOID CALLBACK hb_wvw_gtFlashWindow(HWND hwnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime)
{
  static BYTE byCount = 0;

  HB_SYMBOL_UNUSED( uMsg );
  HB_SYMBOL_UNUSED( dwTime );

  FlashWindow( s_pWindows[ s_usNumWindows-1 ]->hWnd, TRUE );

  if (++byCount >= 15)
  {

    KillTimer( hwnd, idEvent );
    byCount = 0;
    s_bFlashingWindow = FALSE;
  }
}

static void hb_wvw_gtInputNotAllowed( USHORT usWinNum, UINT message, WPARAM wParam, LPARAM lParam )
{

  /* user may handle this event and returns .t. from .PRG level
     using function WVW_INPUTFOCUS()
   */
  if ( s_sApp.pSymWVW_INPUTFOCUS )
  {
    BOOL bHandled;

    hb_vmPushState();
    hb_vmPushSymbol( s_sApp.pSymWVW_INPUTFOCUS->pSymbol );
    hb_vmPushNil();
    hb_vmPushInteger( ( int ) (usWinNum)  );
    hb_vmPushLong( ( LONG ) s_pWindows[usWinNum]->hWnd    );
    hb_vmPushLong( ( LONG ) message );
    hb_vmPushLong( ( LONG ) wParam  );
    hb_vmPushLong( ( LONG ) lParam  );
    hb_vmDo( 5 );
    bHandled = hb_itemGetL( &HB_VM_STACK.Return );
    hb_vmPopState();

    if (bHandled)
    {
      return;
    }
  }

  MessageBeep(MB_OK);

  /* this simpler method is not available in Win95
  fwi.cbSize = sizeof(fwi);
  fwi.hwnd = s_pWindows[ s_usNumWindows-1 ]->hWnd;
  fwi.dwFlags = FLASHW_CAPTION | FLASHW_TRAY;
  fwi.uCount = 5;
  fwi.dwTimeout = 100;
  FlashWindowEx(&fwi);
  */

  if (!s_bFlashingWindow)
  {
    s_bFlashingWindow = TRUE;
    SetTimer( NULL, 0, 50, (TIMERPROC) hb_wvw_gtFlashWindow );
  }

}

/* ********************************************************************
   MainCoord Mode
   ********************************************************************
   In this mode an xHarbour program uses (row,col) coordinate relative to
   Main Window's coordinate. It is similar to old Clipper program which
   uses coordinate relative to the physical screen area.

   This mode can be set and reset during runtime,eg.
   oldCoordMode := WVW_SetMainCoord( .t. )

   Illustration:
   +------
   |Main Window (Window 0)
   |maxrow()=24 maxcol()=79
   |   +---------------
   |   |Window1 RowOfs=3 ColOfs=4
   |   |maxrow()=9 maxcol()=29
   |   |          +--------------------------------------+
   |   |          |Window2 RowOfs=6 ColOfs=15            |
   |   |          |maxrow()=3 maxcol()=49                |
   |   |          |                                      |

   @ 6,15 say "text1" will be written to Window2 starting at 0,0
   @ 3,15 say "text2" will be written to Window1 starting at 0,11
   @ 3, 2 say "text3" will be written to Main Window starting at 3,2

   Notice that the entire "text3" will be written in Main Window, disregarding
   the fact that "xt3" might be expected to be written to Window1. This
   potential unfortunate situation is considered a "punishment" for the
   "bad" practice of the programmer.

   If more than one pair of coordinate is dealt with, the second one is ignored.
   Example:
   scroll(2,2,10,10) will operate on Main Window on the above illustration.

   WARNING:
   Remember current restriction that topmost window is the only one allowed
   to accept user input, still.

   GTWVW parts that needs to be modified:
   HB_GT_FUNC( ... ) should NOT call another HB_GT_FUNC() to avoid
   double translation of coordinates.

   Then all output oriented HB_GT_FUNC() needs to translate coord:
   - HB_GT_FUNC( gt_PutText() )         c
   - HB_GT_FUNC( gt_Puts() )            c
   - HB_GT_FUNC( gt_GetText() )         c
   - HB_GT_FUNC( gt_SetPos() )          c
   - HB_GT_FUNC( gt_SetAttribute() )    c
   - HB_GT_FUNC( gt_xPutch() )          c
   - HB_GT_FUNC( gt_GetScreenWidth() )  c
   - HB_GT_FUNC( gt_GetScreenHeight() ) c
   - HB_GT_FUNC( gt_Row() ) : c
   - HB_GT_FUNC( gt_Col() ) : c
   - etc.

   Higher level functions uses coord as passed by user, eg.:
   - HB_FUNC( WVW_NOPENWINDOW )
   - etc.

   Lower level functions (both static and exported ones) use coord relative
   to the referred window, eg.:
   - hb_wvw_gtSetStringInTextBuffer()
   - hb_wvw_gtTextOut()
   - etc

*/

/* returns row offset of window usWinNum */
static USHORT hb_wvw_gtRowOfs( USHORT usWinNum )
{
  return( s_pWindows[usWinNum]->usRowOfs );
}

/* returns col offset of window usWinNum */
static USHORT hb_wvw_gtColOfs( USHORT usWinNum )
{
  return( s_pWindows[usWinNum]->usColOfs );
}

/*(usrow,uscol) is coordinate relative to Main Window (MainCoord Mode)
 *returns true if usrow and uscol is within maxrow() and maxcol() of Window usWinNum
 */
static BOOL hb_wvw_gtInWindow( USHORT usWinNum, USHORT usrow, USHORT uscol )
{
  return( usrow >= hb_wvw_gtRowOfs(usWinNum) &&
          usrow <= (s_pWindows[usWinNum]->ROWS-1 + hb_wvw_gtRowOfs(usWinNum)) &&
          uscol >= hb_wvw_gtColOfs(usWinNum) &&
          uscol <= (s_pWindows[usWinNum]->COLS-1 + hb_wvw_gtColOfs(usWinNum)) );
}

/*returns winnum containing (usRow,usCol) coordinate
 *only meaningful in s_bMainCoordMode
 */
static USHORT hb_wvw_gtFindWindow(USHORT usRow, USHORT usCol)
{
  USHORT i;

  if (!s_bMainCoordMode)
  {
    return (s_usNumWindows-1);
  }

  for (i=s_usNumWindows-1; i>0; i--)
  {
    if ( hb_wvw_gtInWindow(i, usRow, usCol)  )
    {
      break;
    }
  }

  return (i);
}

/* this is the prologue for any HB_GT_FUNC() that is output/coordinate oriented
 * called only if s_bMainCoordMode
 * row2 and col2 is not taken into account during window finding, but they are translated too
 */
static void hb_wvw_GTFUNCPrologue(BYTE byNumCoord, USHORT * pusRow1, USHORT * pusCol1,
                                                  USHORT * pusRow2, USHORT * pusCol2)
{
  USHORT usWinNum;

  if (byNumCoord<2) (*pusCol1) = s_pWindows[0]->caretPos.x;
  if (byNumCoord<1) (*pusRow1) = s_pWindows[0]->caretPos.y;

  usWinNum = hb_wvw_gtFindWindow(*pusRow1, *pusCol1);

  if (pusRow1) (*pusRow1) -= hb_wvw_gtRowOfs( usWinNum );
  if (pusCol1) (*pusCol1) -= hb_wvw_gtColOfs( usWinNum );
  if (pusRow2) (*pusRow2) -= hb_wvw_gtRowOfs( usWinNum );
  if (pusCol2) (*pusCol2) -= hb_wvw_gtColOfs( usWinNum );

  hb_wvw_gtSetCurWindow( usWinNum );

}

/* this is the epilogue for any HB_GT_FUNC() that is output/coordinate oriented
 * called only if s_bMainCoordMode
 */
static void hb_wvw_GTFUNCEpilogue( void )
{

  s_pWindows[0]->caretPos.y = s_pWindows[ s_usCurWindow ]->caretPos.y + hb_wvw_gtRowOfs( s_usCurWindow );
  s_pWindows[0]->caretPos.x = s_pWindows[ s_usCurWindow ]->caretPos.x + hb_wvw_gtColOfs( s_usCurWindow );

  hb_wvw_gtSetCurWindow( 0 );

  if ( s_sApp.CaretExist && s_sApp.displayCaret )
  {
     hb_wvw_gtSetCaretPos(s_pWindows[s_usNumWindows-1]);

  }

}

static void hb_wvw_HBFUNCPrologue(USHORT usWinNum,
                                  USHORT * pusRow1, USHORT * pusCol1,
                                  USHORT * pusRow2, USHORT * pusCol2)
{

  if (pusRow1) (*pusRow1) -= hb_wvw_gtRowOfs( usWinNum );
  if (pusCol1) (*pusCol1) -= hb_wvw_gtColOfs( usWinNum );
  if (pusRow2) (*pusRow2) -= hb_wvw_gtRowOfs( usWinNum );
  if (pusCol2) (*pusCol2) -= hb_wvw_gtColOfs( usWinNum );

}

/*assigns a new value to s_usCurWindow
 *returns old value of s_usCurWindow
 *WARNING!! we must make sure that it is done in !s_bMainCoordMode, otherwise
 *          some GT_FUNC will be trapped into circular reference!
 */
static USHORT hb_wvw_gtSetCurWindow( USHORT usWinNum )
{
  USHORT usOldWin = s_usCurWindow;
  BOOL   bMainCoordMode;

  if (usWinNum==usOldWin)
  {
    return(usOldWin);
  }

  s_usCurWindow = usWinNum;

  bMainCoordMode = s_bMainCoordMode;
  s_bMainCoordMode = FALSE;

  /*updating GTAPI's statics:
   *tell GTAPI about the new maxrow(), maxcol()
   */
  s_bQuickSetMode = TRUE;

  hb_gtSetMode( s_pWindows[ s_usCurWindow ]->ROWS, s_pWindows[ s_usCurWindow ]->COLS );
  s_bQuickSetMode = FALSE;

  /* tell GTAPI about the new row(), col() */

  hb_gtSetPos( s_pWindows[ s_usCurWindow ]->caretPos.y,
               s_pWindows[ s_usCurWindow ]->caretPos.x );
  /* done updating GTAPI's statics......... */

  s_bMainCoordMode = bMainCoordMode;

  return(usOldWin);
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Supporters of HB_GT_FUNC(...)                       */
/*               DONE: These all are to be made window selective!    */
/*                     all row and col are relative to its own window! */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/* NOTE:works for topmost window only */
static void hb_wvw_vmouse_Init( void )
{

  hb_wvw_vmouse_SetPos( s_pWindows[ s_usNumWindows-1 ], 0, 0);
}

static void   hb_wvw_vmouse_Exit( void )
{
}

static void   hb_wvw_vmouse_SetPos( WIN_DATA * pWindowData, USHORT usRow, USHORT usCol )
{
  POINT xy = { 0 };

  hb_wvw_gtSetMouseY( pWindowData, usRow );
  hb_wvw_gtSetMouseX( pWindowData, usCol );

  xy = hb_wvw_gtGetXYFromColRow( pWindowData, usCol, usRow );

  if ( ClientToScreen( pWindowData->hWnd, &xy ) )
  {
     SetCursorPos( xy.x, xy.y + ( pWindowData->PTEXTSIZE.y / 2 ) );
  }
}

static USHORT hb_wvw_gt_usDispCount( WIN_DATA * pWindowData )
{
  return( pWindowData->uiDispCount );
}

static void hb_wvw_gt_vDispBegin( WIN_DATA * pWindowData )
{
  ++(pWindowData->uiDispCount);
}

static void   hb_wvw_gt_vDispEnd( WIN_DATA * pWindowData )
{

  if ( pWindowData->uiDispCount > 0 )
  {
    --(pWindowData->uiDispCount);
  }
  if ( pWindowData->uiDispCount<= 0 )
  {
    hb_wvw_gtDoInvalidateRect( pWindowData );
  }
}

static SHORT hb_wvw_gt_sCol( WIN_DATA * pWindowData )
{
  return( (USHORT) pWindowData->caretPos.x );
}

static SHORT hb_wvw_gt_sRow( WIN_DATA * pWindowData )
{
  return( (USHORT) pWindowData->caretPos.y );
}

static void hb_wvw_gt_vGetText( WIN_DATA * pWindowData, USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer )
{
  USHORT irow, icol, index, j;

  j = 0;
  for ( irow = top; irow <= bottom; irow++ )
  {
    index = hb_wvw_gtGetIndexForTextBuffer( pWindowData, left, irow );
    for ( icol = left; icol <= right; icol++ )
    {
      if ( index >= pWindowData->BUFFERSIZE )
      {
        break;
      }
      else
      {
        sBuffer[ j++ ] = pWindowData->pBuffer[ index ];
        sBuffer[ j++ ] = pWindowData->pAttributes[ index ];
        index++;
      }
    }
  }
}

static void  hb_wvw_gt_vPuts( WIN_DATA * pWindowData, USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen )
{
  hb_wvw_gtSetStringInTextBuffer( pWindowData, (SHORT) usCol, (SHORT) usRow, byAttr, pbyStr, (SHORT) ulLen );
#ifdef WVW_DEBUG
  nCountPuts++;
#endif
}

static void  hb_wvw_gt_vReplicate( WIN_DATA * pWindowData, USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen )
{
  BYTE  ucBuff[ WVW_CHAR_BUFFER ], *byChars;
  ULONG i;
  BOOL  bMalloc = FALSE;

  if ( ulLen > WVW_CHAR_BUFFER )
  {
    byChars = ( BYTE* ) hb_xgrab( ulLen );
    bMalloc= TRUE;
  }
  else
  {
    byChars = ucBuff ;
  }

  for ( i = 0; i < ulLen; i++ )
  {
    *( byChars+i ) = byChar;
  }

  hb_wvw_gtSetStringInTextBuffer( pWindowData, (SHORT) usCol, (SHORT) usRow, byAttr, byChars, (SHORT) ulLen );
  if ( bMalloc )
  {
    hb_xfree( byChars );
  }
}

static void  hb_wvw_gt_vPutText( WIN_DATA * pWindowData, USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer )
{
  USHORT irow, icol, index, j;

  j = 0;
  for ( irow = top; irow <= bottom; irow++ )
  {
    index = hb_wvw_gtGetIndexForTextBuffer( pWindowData, left, irow );
    for ( icol = left; icol <= right; icol++ )
    {
      if ( index >= pWindowData->BUFFERSIZE )
      {
        break;
      }
      else
      {
        pWindowData->pBuffer[ index ] = sBuffer[ j++ ];
        pWindowData->pAttributes[ index ] = sBuffer[ j++ ];
        index++;
      }
    }
  }
  hb_wvw_gtSetInvalidRect( pWindowData, left, top, right, bottom );
}

static void  hb_wvw_gt_vSetAttribute( WIN_DATA * pWindowData, USHORT rowStart, USHORT colStart, USHORT rowStop, USHORT colStop, BYTE attr )
{
  USHORT irow, icol, index;

  for ( irow = rowStart; irow <=rowStop; irow++ )
  {
    index = hb_wvw_gtGetIndexForTextBuffer( pWindowData, colStart, irow );
    for ( icol = colStart; icol <= colStop; icol++ )
    {
      if ( index >= pWindowData->BUFFERSIZE )
      {
        break;
      }
      else
      {
        pWindowData->pAttributes[ index++ ] = attr;
      }
    }
  }
  hb_wvw_gtSetInvalidRect( pWindowData, colStart, rowStart, colStop, rowStop );
}

static BOOL  hb_wvw_gt_bSetMode( WIN_DATA * pWindowData, USHORT row, USHORT col )
{
   BOOL bResult= FALSE;
   HFONT hFont;

   if ( row<= WVW_MAX_ROWS && col<= WVW_MAX_COLS )
   {

      if ( pWindowData->hWnd )
      {
         hFont = hb_wvw_gtGetFont( pWindowData->fontFace, pWindowData->fontHeight, pWindowData->fontWidth, pWindowData->fontWeight, pWindowData->fontQuality, pWindowData->CodePage );
         if ( hFont )
         {
            /* make sure that the mode selected along with the current
             * font settings will fit in the window
             *
             * JC1: See my note
             *x gtwvt comments out the following condition! (see also SetFont)
             *x TODO: I THINK I am right to keep it, am I?
             */
            if ( hb_wvw_gtValidWindowSize( pWindowData, row,col, hFont, pWindowData->fontWidth, NULL, NULL ) )
            {
              bResult = hb_wvw_gtInitWindow( pWindowData, pWindowData->hWnd, col, row );
            }

            DeleteObject( hFont );
         }
      }
      else
      {

         bResult = hb_wvw_gtAllocSpBuffer( pWindowData, row, col );
      }
   }
   return( bResult );
}

static void  hb_wvw_gt_vxPutch( WIN_DATA * pWindowData, USHORT iRow, USHORT iCol, BYTE bAttr, BYTE bChar )
{
  USHORT index;

  index = hb_wvw_gtGetIndexForTextBuffer( pWindowData, iCol, iRow );
  if ( index < pWindowData->BUFFERSIZE )
  {
    pWindowData->pBuffer[ index ]     = bChar;
    pWindowData->pAttributes[ index ] = bAttr;

    /*  determine bounds of rect around character to refresh
     */
    hb_wvw_gtSetInvalidRect( pWindowData, iCol, iRow, iCol, iRow );
  }
}

static USHORT hb_wvw_gt_usBox( WIN_DATA * pWindowData, SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * szBox, BYTE byAttr )
{
    USHORT ret = 1;
    SHORT  Row;
    SHORT  Col;
    SHORT  Height;
    SHORT  Width;
    USHORT sWidth  = hb_wvw_gt_usGetScreenWidth( pWindowData );
    USHORT sHeight = hb_wvw_gt_usGetScreenHeight( pWindowData );

    if( ( Left   >= 0 && Left   < sWidth  ) ||
        ( Right  >= 0 && Right  < sWidth  ) ||
        ( Top    >= 0 && Top    < sHeight ) ||
        ( Bottom >= 0 && Bottom < sHeight ) )
    {
        /* Ensure that box is drawn from top left to bottom right. */
        if( Top > Bottom )
        {
            Row    = Top;
            Top    = Bottom;
            Bottom = Row;
        }
        if( Left > Right )
        {
            Row   = Left;
            Left  = Right;
            Right = Row;
        }

        /* Draw the box or line as specified */
        Height = Bottom - Top + 1;
        Width  = Right - Left + 1;

        hb_wvw_gt_vDispBegin( pWindowData );

        if( Height > 1 && Width > 1 &&
               Top >= 0 && Top  < sHeight &&
              Left >= 0 && Left < sWidth )
        {

          hb_wvw_gt_vxPutch( pWindowData, Top, Left, byAttr, szBox[ 0 ] ); /* Upper left corner */
        }

        Col = ( Width > 1 ? Left + 1 : Left );
        if( Col < 0 )
        {
            Width += Col;
            Col = 0;
        }
        if( Right >= sWidth )
        {
            Width -= Right - sWidth;
        }

        if( Col < Right && Col < sWidth &&
                Top >= 0 && Top < sHeight )
        {

            hb_wvw_gt_vReplicate( pWindowData, Top, Col, byAttr, szBox[ 1 ], Width + ( ( Right - Left ) > 1 ? -2 : 0 ) ); /* Top line */
        }
        if( Height > 1 &&
               ( Right - Left ) > 0 && Right < sWidth &&
               Top >= 0 && Top < sHeight )
        {

            hb_wvw_gt_vxPutch( pWindowData, Top, Right, byAttr, szBox[ 2 ] ); /* Upper right corner */
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

                      hb_wvw_gt_vxPutch( pWindowData, Row, Col++, byAttr, szBox[ 7 ] );           /* Left side */
                    }

                    hb_wvw_gt_vReplicate( pWindowData, Row, Col, byAttr, szBox[ 8 ], Width - 2 ); /* Fill */
                    if( Right < sWidth )
                    {

                      hb_wvw_gt_vxPutch( pWindowData, Row, Right, byAttr, szBox[ 3 ] );           /* Right side */
                    }
                }
            }
        }
        else
        {
            for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( ( Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
            {
                if( Row >= 0 && Row < sHeight )
                {
                    if( Left >= 0 && Left < sWidth )
                    {

                        hb_wvw_gt_vxPutch( pWindowData, Row, Left, byAttr, szBox[ 7 ] );            /* Left side */
                    }
                    if( ( Width > 1 || Left < 0 ) && Right < sWidth )
                    {

                        hb_wvw_gt_vxPutch( pWindowData, Row, Right, byAttr, szBox[ 3 ] );           /* Right side */
                    }
                }
            }
        }

        if( Height > 1 && Width > 1 )
        {
            if( Left >= 0 && Bottom < sHeight )
            {

                hb_wvw_gt_vxPutch( pWindowData, Bottom, Left, byAttr, szBox[ 6 ] );                /* Bottom left corner */
            }
            Col = Left + 1;
            if( Col < 0 )
            {
                Col = 0; /* The width was corrected earlier. */
            }
            if( Col <= Right && Bottom < sHeight )
            {

                hb_wvw_gt_vReplicate( pWindowData, Bottom, Col, byAttr, szBox[ 5 ], Width - 2 );  /* Bottom line */
            }
            if( Right < sWidth && Bottom < sHeight )
            {

                hb_wvw_gt_vxPutch( pWindowData, Bottom, Right, byAttr, szBox[ 4 ] );              /* Bottom right corner */
            }
        }

        hb_wvw_gt_vDispEnd( pWindowData );
        ret = 0;
    }

    return( ret );
}

static void  hb_wvw_gt_vSetPos( WIN_DATA * pWindowData, SHORT sRow, SHORT sCol, SHORT sMethod )
{
  HB_SYMBOL_UNUSED( sMethod );

  if ( sRow >= 0 && sRow< pWindowData->ROWS && sCol>=0 && sCol <= pWindowData->COLS )
  {
    pWindowData->caretPos.x = sCol;
    pWindowData->caretPos.y = sRow;
    hb_wvw_gtValidateCaret( pWindowData );
  }
}

static USHORT hb_wvw_gt_usGetScreenWidth( WIN_DATA * pWindowData )
{
  return( pWindowData->COLS );
}

static USHORT hb_wvw_gt_usGetScreenHeight( WIN_DATA * pWindowData )
{
  return( pWindowData->ROWS );
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Other static functions                              */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/*hb_wvw_InitPendingRect( ) is called during init static, or after userpaint */
/*This function must be called only when bPaintPending == FALSE              */
/*-------------------------------------------------------------------*/
static void   hb_wvw_InitPendingRect( WIN_DATA * pWindowData )
{

  pWindowData->rPaintPending.left   = WVW_MAX_COLS-1;
  pWindowData->rPaintPending.top    = WVW_MAX_ROWS-1;
  pWindowData->rPaintPending.right  = 0;
  pWindowData->rPaintPending.bottom = 0;
}

/*-------------------------------------------------------------------*/
/*hb_wvw_UpdatePendingRect( ) is called by hb_wvw_gtWndProc()        */
/*This function's job is to update paint pending rect                */
/*-------------------------------------------------------------------*/
static void   hb_wvw_UpdatePendingRect( WIN_DATA * pWindowData, USHORT usRow1, USHORT usCol1, USHORT usRow2, USHORT usCol2 )
{
  pWindowData->rPaintPending.left = HB_MIN( pWindowData->rPaintPending.left, usCol1 );
  pWindowData->rPaintPending.top  = HB_MIN( pWindowData->rPaintPending.top, usRow1 );
  pWindowData->rPaintPending.right = HB_MAX( pWindowData->rPaintPending.right, usCol2 );
  pWindowData->rPaintPending.bottom  = HB_MAX( pWindowData->rPaintPending.bottom, usRow2 );
}

/* returns lineheight, ie. including linespacing if any */
static BYTE   hb_wvw_LineHeight( WIN_DATA * pWindowData )
{
  return pWindowData->PTEXTSIZE.y + pWindowData->byLineSpacing;
}

/* fills linespace above and below the text area.
   caller should check that linespacing is > 0.
   has no effect if linespacing == 0 */
static void hb_wvw_gtFillLineSpace( WIN_DATA * pWindowData, HDC hdc, USHORT startCol, USHORT irow, USHORT len, BYTE byAttrib )
{
  RECT     rc = { 0 };
  LOGBRUSH lb = { 0 };
  HBRUSH   hBrush;

  BYTE     byColorIndex = pWindowData->iLSpaceColor < 0 ?
                          ( byAttrib & 0x00F0 )>>4 :
                          pWindowData->iLSpaceColor;
  COLORREF bkColor = _COLORS[ byColorIndex ];

  rc.top    = irow;
  rc.left   = startCol;
  rc.bottom = irow;
  rc.right  = startCol + len - 1;
  rc = hb_wvw_gtGetXYFromColRowRect( pWindowData, rc );

  lb.lbStyle = BS_SOLID;
  lb.lbColor = bkColor;
  lb.lbHatch = 0;

  hBrush     = CreateBrushIndirect( &lb );

  rc.bottom  = rc.top;
  rc.top    -= (pWindowData->byLineSpacing / 2);
  FillRect( hdc, &rc, hBrush );

  rc.top     = rc.bottom + pWindowData->PTEXTSIZE.y;
  rc.bottom  = rc.top + (pWindowData->byLineSpacing / 2);
  FillRect( hdc, &rc, hBrush );

  DeleteObject( hBrush );

}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Window Related xHarbour callable functions          */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*20040713 this function was named WVW_lOpenWindow()
 *now it is wvw_nOpenWindow()
 *it now returns numeric

 *WVW_nOpenWindow(cWinName, row1, col1, row2, col2, ;
 *                nStyle, nParentWin)
 *rowx and colx are relative to MAIN WINDOW (not current window!)
 *rowx and colx are used for:
 *(1) positioning window to its initial position,
 *(2) determining the size of the window (new maxrow() and maxcol())
 *(3) saved into RowOfs and ColOfs for MainCoord mode
 *
 *nStyle is window style (eg. WS_OVERLAPPEDWINDOW, etc.)
 *       default is: WS_CAPTION|WS_SYSMENU |WS_CLIPCHILDREN
 *       WARNING: you must know what you're doing if you supplied this param
 *       NOTES: if you will use controls such as PUSHBUTTON,
 *              you MUST include WS_CLIPCHILDREN.
 *
 *nParentWin is parent window of the new on we're about to open.
 *       default is: current window (in Standard Mode)
 *                   last window (in MainCoord Mode)
 *       If you want the new window to not have parent,
 *       pass -1 as nParentWin.
 *
 *
 *returns window number if successful
 *returns 0 if failed
 */
HB_FUNC( WVW_NOPENWINDOW )
{
  LPCTSTR lpszWinName;

  int  iLen;
  WIN_DATA * pParentWindow;

  WIN_DATA * pWindowData;
  int irow1, icol1, irow2, icol2;
  RECT wi = { 0 }, rcWorkArea = { 0 };
  USHORT usWinNum;

  DWORD  dwStyle    = ( ISNIL( 6 ) ? (WS_POPUP|WS_CAPTION|WS_SYSMENU |WS_CLIPCHILDREN) : ((DWORD) hb_parni( 6 )) );
  int    iParentWin = ( ISNIL( 7 ) ? ( s_bMainCoordMode ? s_usNumWindows-1 : s_usCurWindow ) : ((int) hb_parni( 7 )) );
  PHB_FNAME pFileName = NULL;

  if (s_usNumWindows == 0)
  {

    hb_retni( 0 );
    return;
  }

  if (s_usNumWindows == WVW_MAXWINDOWS)
  {
    MessageBox( NULL, TEXT("Too many Windows to open"),
                "Error", MB_ICONERROR );
    hb_retni( 0 );
    return;
  }

  if (iParentWin > s_usNumWindows-1)
  {
    MessageBox( NULL, TEXT("Invalid Parent Window"),
                "Error", MB_ICONERROR );
    hb_retni( 0 );
    return;
  }

  if (iParentWin<0)
  {
    if (!s_bMainCoordMode)
    {
      pParentWindow = s_pWindows[ s_usCurWindow ];
    }
    else
    {
      pParentWindow = s_pWindows[ s_usNumWindows-1 ];
    }
  }
  else
  {

    pParentWindow = s_pWindows[ (USHORT) iParentWin ];
  }

  if ( ISCHAR(1) )
  {
    iLen = hb_parclen(1);
    if ( iLen > WVW_MAXWINNAMELENGTH-1)
    {
      MessageBox( NULL, TEXT( "Windows name too long" ),
                  TEXT("Error"), MB_ICONERROR );
      hb_retni( 0 );
      return;
    }
    lpszWinName = hb_parcx(1);
  }
  else if (ISNIL(1))
  {

    pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );
    lpszWinName = pFileName->szName;

  }
  else
  {

    hb_errRT_TERM( EG_DATATYPE, 10001, "Window Title must be character", "WVW_nOpenWindow()", 0, 0 );
    hb_retni( 0 );
    return;
  }

  irow1 = ISNIL(2) ? 0 : hb_parni(2);
  icol1 = ISNIL(3) ? 0 : hb_parni(3);
  irow2 = ISNIL(4) ? pParentWindow->ROWS-1 :  hb_parni(4);
  icol2 = ISNIL(5) ? pParentWindow->COLS-1 :  hb_parni(5);

  usWinNum = hb_wvw_gtOpenWindow( lpszWinName, irow1, icol1, irow2, icol2,
                                  dwStyle, iParentWin);

  if ( usWinNum == 0 )
  {
    hb_retni( 0 );

    if (pFileName)
    {
      hb_xfree( pFileName );
    }

    return;
  }

  pWindowData = s_pWindows[usWinNum];

  GetWindowRect( pWindowData->hWnd, &wi );
  SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );
  if (wi.right < rcWorkArea.left || wi.left > rcWorkArea.right ||
      wi.top   > rcWorkArea.bottom || wi.bottom < rcWorkArea.top)
  {

     hb_wvw_gtSetCentreWindow( 0, TRUE, TRUE );

     hb_wvw_gtSetCentreWindow( usWinNum, s_bDefCentreWindow, TRUE );

  }

  if (s_bMainCoordMode)
  {

    s_usCurWindow = usWinNum;
  }

  hb_gtSetMode( pWindowData->ROWS, pWindowData->COLS );

  if (s_bMainCoordMode)
  {

    hb_wvw_gtSetCurWindow( 0 );
  }

  SendMessage( pWindowData->hWnd, WM_SETFOCUS, 0, 0 );

  if (pFileName)
  {
    hb_xfree( pFileName );
  }

  hb_retni( usWinNum );

}

/*WVW_lCloseWindow()
 *closes the last/topmost window
 *returns .t. if successful
 */
HB_FUNC( WVW_LCLOSEWINDOW )
{
  WIN_DATA * pWindowData;
  if (s_usNumWindows <= 1)
  {

    MessageBox( NULL, TEXT("No more window to close"),
                "Error", MB_ICONERROR );
    hb_retl( FALSE );
    return;
  }

  hb_wvw_gtCloseWindow( );

  if (!s_bMainCoordMode)
  {

    s_bQuickSetMode = TRUE;

    hb_gtSetMode( s_pWindows[ s_usNumWindows-1 ]->ROWS, s_pWindows[ s_usNumWindows-1 ]->COLS );
    s_bQuickSetMode = FALSE;
  }
  else
  {

    hb_wvw_gtSetCurWindow( 0 );
  }

  pWindowData = s_pWindows[s_usNumWindows-1];

  SendMessage( pWindowData->hWnd, WM_SETFOCUS, 0, 0 );

  hb_retl( TRUE );
}

/*WVW_nNumWindows()
 *returns number of windows opened (including main window)
 */
HB_FUNC( WVW_NNUMWINDOWS )
{
  hb_retni( (int) s_usNumWindows );
}

/*WVW_xReposWindow(lAnchored)
 *reposition all windows to their initial position
 *
 * if lAnchored == .t. (default)
 *    all subwindows are positioned according to their respective (row1,col1) coordinate
 * else
 *    all subwindows are positioned according to whatever their "CenterWindow" setting
 *    (see also WVW_CENTERWINDOW())
 */
HB_FUNC( WVW_XREPOSWINDOW )
{
  USHORT i;
  BOOL   bAnchored = (ISLOG(1) ? hb_parl(1) : TRUE);

  /* centerize Main Window, only if not maximized */

  {
    hb_wvw_gtSetCentreWindow( 0, TRUE, TRUE );
  }

  /* reposition all subwindows */
  for (i=1; i<s_usNumWindows; i++)
  {

    if (bAnchored)
    {
       hb_wvw_gtSetCentreWindow( i, FALSE, TRUE );
    }
    else
    {
       hb_wvw_gtSetCentreWindow( i, s_pWindows[ i ]->CentreWindow, TRUE );
    }
  }
}

/*WVW_nSetCurWindow( nWinNum )   (0==MAIN)
 *assigns nWinNum as the new current window (s_usCurWindow)
 *returns old current window
 *example: saved := WVW_nSetCurWindow(0)
 *         ? "This will be displayed in Main Window"
 *         WVW_nSetCurWindow(saved)
 *notes: makes sense only if !s_bMainCoordMode
 */
HB_FUNC( WVW_NSETCURWINDOW )
{
  SHORT sWinNum;
  if ( ISNIL(1) )
  {
    hb_retni( (int) (s_usCurWindow) );
  }
  else
  {
    sWinNum = (SHORT) hb_parni(1);
    if (sWinNum >= 0 && sWinNum < s_usNumWindows)
    {
      hb_retni( (int) ( hb_wvw_gtSetCurWindow( sWinNum ) ) );
    }
    else
    {
      hb_errRT_TERM( EG_BOUND, 10001, "Window Number out of range", "WVW_nSetCurWindow()", 0, 0 );

    }
  }
}

/*WVW_nRowOfs( [nWinNum] )
 *returns row offset of window #nWinNum (0==MAIN), relative to Main Window
 *nWinNum defaults to current window
 */
HB_FUNC( WVW_NROWOFS )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;

  hb_retni( (int) hb_wvw_gtRowOfs( usWinNum ) );
}

/*WVW_nColOfs( [nWinNum] )
 *returns col offset of window #nWinNum (0==MAIN), relative to Main Window
 *nWinNum defaults to topmost window
 */
HB_FUNC( WVW_NCOLOFS )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;

  hb_retni( (int) hb_wvw_gtColOfs( usWinNum ) );
}

/*
 *WVW_MAXMAXROW( [nWinNum] )
 *returns maximum possible MAXROW() in current screen setting for font used by window nWinNum
 *
 */
HB_FUNC( WVW_MAXMAXROW )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   int maxrows;

   /* rows and cols passed are dummy ones */
   hb_wvw_gtValidWindowSize( s_pWindows[usWinNum], 10, 10, s_pWindows[usWinNum]->hFont, s_pWindows[usWinNum]->fontWidth,
                                                              &maxrows, NULL )  ;
   hb_retni( maxrows-1 );
}

/*
 *WVW_MAXMAXCOL( [nWinNum] )
 *returns maximum possible MAXCOL() in current screen setting for font used by window nWinNum
 *
 */
HB_FUNC( WVW_MAXMAXCOL )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   int maxcols;

   /* rows and cols passed are dummy ones */
   hb_wvw_gtValidWindowSize( s_pWindows[usWinNum], 10, 10, s_pWindows[usWinNum]->hFont, s_pWindows[usWinNum]->fontWidth,
                                                              NULL, &maxcols )  ;
   hb_retni( maxcols-1 );
}

/*
 *WVW_UNREACHEDBR( [nWinNum], [nBottomPixels], [nRightPixels] )
 * get unreached pixels
 * below maxrow() to nBottomPixels
 * and on the right of maxcols() to nRightPixels
 *
 */
HB_FUNC( WVW_UNREACHEDBR )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   int cols, rows;

   hb_wvw_gtUnreachedXY( s_pWindows[usWinNum], &cols, &rows );
   if (ISBYREF(2)) hb_storni( rows, 2 );
   if (ISBYREF(3)) hb_storni( cols, 3 );
}

/*WVW_SetMainCoord( [lMainCoord] )
 *returns old setting of s_bMainCoordMode,
 *then assigns s_bMainCoordMode := lMainCoord (if supplied)
 */
HB_FUNC( WVW_SETMAINCOORD )
{
   BOOL bOldMainCoordMode = s_bMainCoordMode;

   if ( ! ISNIL( 1 ) )
   {
     s_bMainCoordMode = hb_parl( 1 );

     if ( !s_bMainCoordMode )
     {

       hb_wvw_gtSetCurWindow( s_usNumWindows-1 );
     }
     else
     {

       hb_wvw_gtSetCurWindow( 0 );
     }
   }

   hb_retl( bOldMainCoordMode );
}

/* WVW_ADDROWS( [nWinNum], nRows)
 * add nRows rows to window nWinNum (nRows may be < 0)
 * returns .t. if successful
 *
 * NOTES: newly added rows (for nRows>0) will be colored with
 *        column 0 of original last row
 * WARNING: no checking if window size will become larger than desktop area
 *          (except if in MainCoord Mode, because it is implied from
 *           restriction of resulted maxrow())
 */

/* WARNING! this function relies on the fact that char/attr buffers are static!
 */
HB_FUNC( WVW_ADDROWS )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  int        iRows = ISNIL(2) ? 0 : hb_parni(2);
  USHORT     height, width;
  USHORT     diffheight, diffwidth;
  USHORT     usNumChars;

  RECT       wi = { 0 }, ci = { 0 };

  if (iRows == 0)
  {
    hb_retl( TRUE );
    return;
  }

  if (/* (iRows < 0) || */
      (s_bMainCoordMode && (pWindowData->usRowOfs+pWindowData->ROWS+iRows > s_pWindows[0]->ROWS)) ||
      (pWindowData->ROWS+iRows > WVW_MAX_ROWS) ||
      (pWindowData->ROWS+iRows < 1)
     )
  {
    hb_retl( FALSE );
    return;
  }

  usNumChars = iRows * pWindowData->COLS;

  if (iRows>0)
  {
    /* initialize chars and attributes */
    USHORT usBufLastRow = hb_wvw_gtGetIndexForTextBuffer( pWindowData, 0, pWindowData->ROWS-1 );
    USHORT usBufStart   = hb_wvw_gtGetIndexForTextBuffer( pWindowData, 0, pWindowData->ROWS );

    memset( &(pWindowData->byBuffer[usBufStart]), ' ', usNumChars );

    memset( &(pWindowData->byAttributes[usBufStart]), (char) pWindowData->byAttributes[usBufLastRow], usNumChars );
  }

  /* update vars */
  pWindowData->ROWS += iRows;
  pWindowData->BUFFERSIZE += usNumChars * sizeof(char);

  if (!s_bMainCoordMode)
  {
    USHORT usCurWindow = s_usCurWindow;
    s_usCurWindow = usWinNum;

    s_bQuickSetMode = TRUE;

    hb_gtSetMode( pWindowData->ROWS, pWindowData->COLS );
    s_bQuickSetMode = FALSE;

    s_usCurWindow = usCurWindow;
  }

  /* resize the window to get the specified number of rows and columns
   */
  height = hb_wvw_gtCalcPixelHeight( pWindowData );
  width  = hb_wvw_gtCalcPixelWidth( pWindowData );

  GetWindowRect( pWindowData->hWnd, &wi );
  GetClientRect( pWindowData->hWnd, &ci );
  diffheight = (wi.bottom-wi.top) - (ci.bottom-ci.top);
  diffwidth  = (wi.right-wi.left) - (ci.right-ci.left);

  height += diffheight;
  width += diffwidth;

  SetWindowPos( pWindowData->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

  if (pWindowData->hStatusBar != NULL)
  {
     SetWindowPos( pWindowData->hStatusBar, NULL, wi.left, wi.bottom - pWindowData->usSBHeight, width, pWindowData->usSBHeight, SWP_NOZORDER );

  }

  /**** THESE are not required, because we simply enlarged/shrinked the window downward
        NOTICE however that some control may not be fully visible

  if (pWindowData->hToolBar != NULL)
  {

     SetWindowPos( pWindowData->hToolBar, NULL, wi.left, wi.top - pWindowData->usTBHeight, width, pWindowData->usTBHeight, SWP_NOZORDER );

  }

  if (pWindowData->pcdCtrlList != NULL)
  {

    ReposControls(pWindowData->byWinId, 0);
  }

  if (pWindowData->byWinId == s_usNumWindows-1)
  {
     hb_wvw_gtSetCaretPos(pWindowData);
  }

  ******/

  if (iRows>0)
  {
    /* invalidate rect of the newly added rows */
    pWindowData->InvalidateWindow = TRUE;
    hb_wvw_gtSetInvalidRect( pWindowData,
                             (SHORT) 0, (SHORT) pWindowData->ROWS-iRows,
                             (SHORT) pWindowData->COLS-1, (SHORT) pWindowData->ROWS-1 );
  }

  hb_retl( TRUE );

}

/*
 *WVW_NOCLOSE( [nWinNum] )
 *disable CLOSE 'X' button of a window
 *
 *no return value
 */
HB_FUNC( WVW_NOCLOSE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   HMENU  hMenu = GetSystemMenu(s_pWindows[usWinNum]->hWnd, FALSE);

   if (hMenu)
   {
      DeleteMenu( hMenu, SC_CLOSE, MF_BYCOMMAND );
      DrawMenuBar( s_pWindows[usWinNum]->hWnd );
   }
}

/*
 *WVW_SETWINSTYLE( [nWinNum], [nStyle] )
 *Get/Set window style
 *NOTES: if window has controls (eg. pushbutton, scrollbar)
 *       you should include WS_CLIPCHILDREN in nStyle
 *
 *SIDE EFFECT:
 *       if window is hidden, applying nStyle here will cause it to show
 *
 *return Window Style prior to applying the new style
 */

HB_FUNC( WVW_SETWINSTYLE )
{
   USHORT   usWinNum = WVW_WHICH_WINDOW;
   LONG_PTR lpStyle;

   if (ISNUM(2))
   {
      lpStyle = SetWindowLongPtr( s_pWindows[usWinNum]->hWnd, GWL_STYLE, (LONG_PTR) hb_parnl(2) );
      SetWindowPos( s_pWindows[usWinNum]->hWnd,
                    NULL,
                    0,0,0,0,
                    SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_FRAMECHANGED );
      ShowWindow( s_pWindows[usWinNum]->hWnd, SW_SHOWNORMAL );
   }
   else
   {
      lpStyle = GetWindowLongPtr( s_pWindows[usWinNum]->hWnd, GWL_STYLE );
   }
   hb_retnl( lpStyle );
}

/*
 *WVW_ENABLEMAXIMIZE( [nWinNum], [lEnable] )
 *Get/Set maximize button
 *
 *returns maximize box state prior to applying the new style
 *
 *NOTE: in order to enable MAXIMIZE button, app should have WVW_SIZE() callback function
 */

HB_FUNC( WVW_ENABLEMAXIMIZE )
{
   USHORT   usWinNum = WVW_WHICH_WINDOW;
   LONG_PTR lpStyle;
   BOOL     bState;

   lpStyle = GetWindowLongPtr( s_pWindows[usWinNum]->hWnd, GWL_STYLE );
   bState  = (BOOL) (lpStyle & (LONG_PTR) WS_MAXIMIZEBOX);
   hb_retl(bState);

   if (ISLOG(2))
   {
      if (hb_parl(2))
      {
         if (bState) return; /* no need */
         lpStyle |= (LONG_PTR) WS_MAXIMIZEBOX;
      }
      else
      {
         if (!bState) return; /* no need */
         lpStyle &= ~ (LONG_PTR) WS_MAXIMIZEBOX;
      }

      SetWindowLongPtr( s_pWindows[usWinNum]->hWnd, GWL_STYLE, lpStyle );
      SetWindowPos( s_pWindows[usWinNum]->hWnd,
                    NULL,
                    0,0,0,0,
                    SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_FRAMECHANGED );
      ShowWindow( s_pWindows[usWinNum]->hWnd, SW_SHOW );
   }
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               GTWVW parameter setting from .prg                   */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*WVW_SetPaintRefresh( [nPaintRefresh] )
 *returns old setting of s_uiPaintRefresh (millisec between calls to WVW_PAINT)
 *then assigns s_uiPaintRefresh:= nPaintRefresh (if supplied)
 *NOTES: nPaintRefresh must be >= 50
 *       or nPaintRefresh == 0, causing Repaint to execute immediately, as GTWVT
 */
HB_FUNC( WVW_SETPAINTREFRESH )
{
   UINT uiOldPaintRefresh = s_uiPaintRefresh;

   if ( ISNUM( 1 ) && (hb_parni(1) >= 50 || hb_parni(1)==0) )
   {
     s_uiPaintRefresh = hb_parni( 1 );

     if ( s_sApp.pSymWVW_PAINT )
     {
       USHORT i;
       for (i=0; i<s_usNumWindows; i++)
       {

         if (s_uiPaintRefresh > 0)
         {
           SetTimer( s_pWindows[i]->hWnd, WVW_ID_SYSTEM_TIMER, (UINT) s_uiPaintRefresh, NULL );
         }
         else
         {
           KillTimer( s_pWindows[i]->hWnd, WVW_ID_SYSTEM_TIMER );
         }
       }
     }
   }

   hb_retni( uiOldPaintRefresh );
}

/*WVW_SetVertCaret( [lOn] )
 *if lOn is supplied:
 *lOn == .t.: turn caret into vertical caret
 *lOn == .f.: turn caret into horizontal caret
 *return old setting of s_bVertCaret
 */
/*TODO: do you want to make it window selective?*/
HB_FUNC( WVW_SETVERTCARET )
{
   BOOL bOldVertCaret = s_bVertCaret;
   WIN_DATA * pWindowData = s_pWindows[ s_usNumWindows-1 ];

   if ( ! ISNIL( 1 ) )
   {
     s_bVertCaret = hb_parl( 1 );

     /*TODO: we should recalculate width and height of caret! */
     hb_wvw_gtKillCaret(pWindowData);
     hb_wvw_gtCreateCaret(pWindowData);

   }

   hb_retl( bOldVertCaret );
}

/*WVW_SetDefCentreWindow( [lCentre] )
 *returns old setting of s_bDefCentreWindow (default CentreWindow setting for newly opened subwindow)
 *then assigns s_bDefCentreWindow := lCentre (if supplied)
 *NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFCENTREWINDOW )
{
   BOOL bOldDef = s_bDefCentreWindow;

   if ( !ISNIL(1) && ISLOG( 1 ) )
   {
     s_bDefCentreWindow = hb_parl( 1 );
   }

   hb_retl( bOldDef );
}

/*WVW_SetDefHCentreWindow( [lCentre] )
 *returns old setting of s_bDefHCentreWindow (default horizontal CentreWindow setting for newly opened subwindow)
 *then assigns s_bDefHCentreWindow := lCentre (if supplied)
 *NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFHCENTREWINDOW )
{
   BOOL bOldDef = s_bDefHCentreWindow;

   if ( !ISNIL(1) && ISLOG( 1 ) )
   {
     s_bDefHCentreWindow = hb_parl( 1 );
   }

   hb_retl( bOldDef );
}

/*WVW_SetDefVCentreWindow( [lCentre] )
 *returns old setting of s_bDefVCentreWindow (default horizontal CentreWindow setting for newly opened subwindow)
 *then assigns s_bDefVCentreWindow := lCentre (if supplied)
 *NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFVCENTREWINDOW )
{
   BOOL bOldDef = s_bDefVCentreWindow;

   if ( !ISNIL(1) && ISLOG( 1 ) )
   {
     s_bDefVCentreWindow = hb_parl( 1 );
   }

   hb_retl( bOldDef );
}

/*WVW_SetDefLineSpacing( [nLineSpacing] )
 *returns old setting of s_byDefLineSpacing (default linespacing between lines)
 *then assigns s_byDefLineSpacing:= nLineSpacing (if supplied)
 *NOTES:
 * - nLineSpacing will be the default line spacing for all window opens
 * - nLineSpacing must be even, positive number <= 40
 *   otherwise it will be ignored
 * - to check line spacing being used by a window, use WVW_SetLineSpacing()
 */
HB_FUNC( WVW_SETDEFLINESPACING )
{
   BYTE byOldLineSpacing = s_byDefLineSpacing;

   if ( !ISNIL(1) && ISNUM( 1 ) && hb_parni(1) >= 0 && hb_parni(1) <= 40 &&  /*nobody is crazy enough to use > 40 */
        fmod( hb_parni(1), 2 ) == 0 )
   {
     s_byDefLineSpacing = hb_parni( 1 );
   }

   hb_retni( byOldLineSpacing );
}

/*WVW_SetLineSpacing( [nWinNum], [nLineSpacing] )
 *returns old setting of linespacing between lines in window nWinNum
 *then set the line spacing to nLineSpacing (if supplied)
 *NOTES:
 * - nLineSpacing must be even, positive number <= 40
 *   otherwise it will be ignored
 * - if window size will become too high, line spacing is restored
 * - to change default line spacing for next window open, use WVW_SetDefLineSpacing()
 */
HB_FUNC( WVW_SETLINESPACING )
{
   USHORT   usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   BYTE     byOldLineSpacing = pWindowData->byLineSpacing;

   if ( !ISNIL(2) && ISNUM( 2 ) && hb_parni(2) >= 0 && hb_parni(2) <= 40 &&  /*nobody is crazy enough to use > 40 */
        fmod( hb_parni(2), 2 ) == 0 )
   {
      USHORT     height, maxHeight;
      RECT       rcWorkArea = { 0 };

      SystemParametersInfo( SPI_GETWORKAREA,0, &rcWorkArea, 0 );
      maxHeight = (SHORT) ( rcWorkArea.bottom - rcWorkArea.top );

      pWindowData->byLineSpacing = hb_parni( 2 );
      height    = hb_wvw_gtCalcPixelHeight( pWindowData );

      /* TODO/WARNING: this height doesn't take Menu Bar into account */
      if (height >= maxHeight)
      {
         pWindowData->byLineSpacing = byOldLineSpacing;
      }
      else
      {

         hb_wvw_gtResetWindow( usWinNum );
      }
   }

   hb_retni( byOldLineSpacing );
}

/*WVW_SetDefLSpaceColor( [nColorIndex] )
 *returns old setting of s_iDefLSpaceColor (color index of spacing between lines)
 *then assigns s_iDefLSpaceColor:= nColorIndex (if supplied)
 *NOTES:
 * - nColorIndex will be the default line spacing color for all window opens
 * - nColorIndex must >= 0 and <= 15, or == -1
 *   nCOlorIndex == 0:black, 1:blue, ..., 7:white, ..., 15:bright white
 *   nColorIndex == -1 means line spacing has no color
 * - to check line spacing color being used by a window, use WVW_SetLSpaceColor()
 */
HB_FUNC( WVW_SETDEFLSPACECOLOR )
{
   BYTE iOldDefLSpaceColor = s_iDefLSpaceColor;

   if ( !ISNIL(1) && ISNUM( 1 ) && hb_parni(1) >= -1 && hb_parni(1) <= 15 )
   {
     s_iDefLSpaceColor = hb_parni( 1 );
   }

   hb_retni( iOldDefLSpaceColor );
}

/*WVW_SetLSpaceColor( [nWinNum], [nColorIndex] )
 *returns old setting of line space color in window nWinNum
 *then set the line spacing color to nColorIndex (if supplied)
 *NOTES:
 * - nColorIndex must be >= 0 and <= 15, or -1
 *   otherwise it will be ignored
 *   nCOlorIndex == 0:black, 1:blue, ..., 7:white, ..., 15:bright white
 * - nColorIndex == -1 means line spacing is not colored
 * - to change default line space color for next window open, use WVW_SetDefLineSpacing()
 */
HB_FUNC( WVW_SETLSPACECOLOR )
{
   USHORT   usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int      iOldLSpaceColor = pWindowData->iLSpaceColor;

   if ( !ISNIL(2) && ISNUM( 2 ) && hb_parni(2) >= -1 && hb_parni(2) <= 15 )
   {
      pWindowData->iLSpaceColor = hb_parni( 2 );

      if ( iOldLSpaceColor != pWindowData->iLSpaceColor )
      {
         hb_wvw_gtSetInvalidRect( pWindowData, 0, 0, pWindowData->COLS-1, pWindowData->ROWS-1 );
      }
   }

   hb_retni( iOldLSpaceColor );
}

/*WVW_NoStartupSubWindow( [lOn] )
 *if lOn is supplied:
 *lOn == .t.: when opening window, window will not be displayed
 *lOn == .f.: when opening window, window will be displayed (default)
 *return old setting of s_bNOSTARTUPWINDOW
 */
HB_FUNC( WVW_NOSTARTUPSUBWINDOW )
{
   BOOL bOldNOSTARTUPSUBWINDOW = s_bNOSTARTUPSUBWINDOW;

   if ( ! ISNIL( 1 ) )
   {
     s_bNOSTARTUPSUBWINDOW = hb_parl( 1 );
   }

   hb_retl( bOldNOSTARTUPSUBWINDOW );
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Exported functions for API calls                    */

/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtSetMenuKeyEvent( USHORT usWinNum, int iMenuKeyEvent )
{
  int iOldEvent;

  iOldEvent = s_pWindows[usWinNum]->MenuKeyEvent ;
  if ( iMenuKeyEvent )
  {
    s_pWindows[usWinNum]->MenuKeyEvent = iMenuKeyEvent;
  }
  return( iOldEvent );
}

/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtSetCentreWindow( USHORT usWinNum, BOOL bCentre, BOOL bPaint )
{
  BOOL bWasCentre;

  bWasCentre = s_pWindows[usWinNum]->CentreWindow ;
  s_pWindows[usWinNum]->CentreWindow = bCentre;
  if ( bPaint )
  {

    if (!IsZoomed( s_pWindows[usWinNum]->hWnd ))
    {
      ShowWindow( s_pWindows[usWinNum]->hWnd, SW_RESTORE );

    }
    else
    {
      ShowWindow( s_pWindows[usWinNum]->hWnd, SW_MAXIMIZE );
    }

    hb_wvw_gtResetWindowSize( s_pWindows[ usWinNum ], s_pWindows[usWinNum]->hWnd ) ;
  }
  return( bWasCentre );
}

/*-------------------------------------------------------------------*/

void HB_EXPORT hb_wvw_gtResetWindow( USHORT usWinNum )
{
  hb_wvw_gtResetWindowSize( s_pWindows[ usWinNum ], s_pWindows[usWinNum]->hWnd ) ;
}

/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtSetCodePage( USHORT usWinNum, int iCodePage )
{
  int iOldCodePage;

  iOldCodePage = s_pWindows[usWinNum]->CodePage ;
  if ( iCodePage )
  {
    s_pWindows[usWinNum]->CodePage = iCodePage;
  }
  if ( iOldCodePage != iCodePage )
  {
    hb_wvw_gtResetWindow( usWinNum );
  }
  return( iOldCodePage );
}

/*-------------------------------------------------------------------*/

int HB_EXPORT hb_wvw_gtGetLastMenuEvent( USHORT usWinNum )
{
  return( s_pWindows[usWinNum]->LastMenuEvent );
}

/*-------------------------------------------------------------------*/

int HB_EXPORT hb_wvw_gtSetLastMenuEvent( USHORT usWinNum, int iLastMenuEvent )
{
  int iRetval = s_pWindows[usWinNum]->LastMenuEvent;
  s_pWindows[usWinNum]->LastMenuEvent = iLastMenuEvent;
  return( iRetval );
}

/*-------------------------------------------------------------------*/

void HB_EXPORT hb_wvw_gtSetWindowTitle( USHORT usWinNum, char * title )
{
  SetWindowText( s_pWindows[usWinNum]->hWnd, title );
}

/*-------------------------------------------------------------------*/

DWORD HB_EXPORT hb_wvw_gtSetWindowIcon( USHORT usWinNum, int icon, char *lpIconName )
{

  HICON hIcon;

  if( lpIconName == NULL )
  {
    hIcon = LoadIcon( ( HINSTANCE ) hb_hInstance, MAKEINTRESOURCE( icon ) );
  }
  else
  {
    hIcon = LoadIcon( ( HINSTANCE ) hb_hInstance, lpIconName );
  }

  if ( hIcon )
  {
    SendMessage( s_pWindows[usWinNum]->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM )hIcon ); /* Set Title Bar ICON */
    SendMessage( s_pWindows[usWinNum]->hWnd, WM_SETICON, ICON_BIG, ( LPARAM )hIcon ); /* Set Task List Icon */
  }
  return( ( DWORD ) hIcon ) ;
}

/*-------------------------------------------------------------------*/

DWORD HB_EXPORT hb_wvw_gtSetWindowIconFromFile( USHORT usWinNum, char *icon )
{

  HICON hIcon = (HICON) LoadImage( ( HINSTANCE ) NULL, icon, IMAGE_ICON, 0, 0, LR_LOADFROMFILE );

  if ( hIcon )
  {
    SendMessage( s_pWindows[usWinNum]->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar ICON */
    SendMessage( s_pWindows[usWinNum]->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); /* Set Task List Icon */

  }
  return( ( DWORD ) hIcon ) ;
}

/*-------------------------------------------------------------------*/

int HB_EXPORT hb_wvw_gtGetWindowTitle( USHORT usWinNum, char *title, int length )
{
  return( GetWindowText( s_pWindows[usWinNum]->hWnd, title, length ) );
}

/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtSetFont( USHORT usWinNum, char *fontFace, int height, int width, int Bold, int Quality )
{
  int   size;
  BOOL  bResult = FALSE ;
  HFONT hFont   = hb_wvw_gtGetFont( fontFace, height, width, Bold, Quality, s_pWindows[usWinNum]->CodePage );
  WIN_DATA * pWindowData = s_pWindows[ usWinNum ];

  /* make sure the font could actually be created
   */
  if ( hFont )
  {
    /* make sure that the font  will fit inside the
     * window with the current pWindowData->ROWS and pWindowData->COLS setting
     *
     *JC1: There's definitely something WRONG with this way of thinking.
     * This makes effectively impossible to enlarge the window from it's
     * initial size.
     *
     *x with the above remark, gtwvt comments out the following condition:
     *x TODO: I THINK I am I to keep it, am I?
     */

    if ( hb_wvw_gtValidWindowSize( pWindowData, pWindowData->ROWS,pWindowData->COLS, hFont, width, NULL, NULL ) )
    {
      pWindowData->fontHeight  = height;
      pWindowData->fontWidth   = width;
      pWindowData->fontWeight  = Bold;
      pWindowData->fontQuality = Quality;

      size = strlen( fontFace );
      if ( ( size > 0 ) && ( size < LF_FACESIZE-1 ) )
      {
        strcpy( pWindowData->fontFace, fontFace );
      }
      if ( pWindowData->hWnd )
      {
        /* resize the window based on new fonts
         */
        hb_wvw_gtResetWindowSize( pWindowData, pWindowData->hWnd );

        /* force resize of caret
         */
        hb_wvw_gtKillCaret(pWindowData);
        hb_wvw_gtCreateCaret(pWindowData);
      }
      bResult= TRUE;
    }
    DeleteObject( hFont );
  }
  return( bResult );
}

/*-------------------------------------------------------------------*/

HWND HB_EXPORT hb_wvw_gtGetWindowHandle( USHORT usWinNum )
{
  return( s_pWindows[usWinNum]->hWnd );
}

/*-------------------------------------------------------------------*/

void HB_EXPORT hb_wvw_gtPostMessage( USHORT usWinNum, int message )
{
  SendMessage( s_pWindows[usWinNum]->hWnd, WM_CHAR,message, 0 );
}

/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtSetWindowPos( USHORT usWinNum, int left, int top )
{
  RECT wi = { 0 };

  GetWindowRect( s_pWindows[usWinNum]->hWnd, &wi );
  return( SetWindowPos( s_pWindows[usWinNum]->hWnd, NULL, left, top, ( wi.right-wi.left )+1, ( wi.bottom-wi.top )+1, SWP_NOZORDER ) );
}

/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtSetAltF4Close( BOOL bCanClose )
{
  BOOL bWas;

  bWas = s_sApp.AltF4Close;
  s_sApp.AltF4Close = bCanClose;
  return( bWas );
}

/*-------------------------------------------------------------------*/

void HB_EXPORT hb_wvw_gtDoProcessMessages( USHORT usWinNum )
{
  /*NOTE: despite the parameter, the following will actually process messages for all windows*/
  hb_wvw_gtProcessMessages( s_pWindows[ usWinNum ] );
}

/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtSetMouseMove( USHORT usWinNum, BOOL bHandleEvent )
{
  BOOL bWas = s_pWindows[usWinNum]->MouseMove;
  s_pWindows[usWinNum]->MouseMove = bHandleEvent;
  return( bWas );
}

/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtEnableShortCuts( USHORT usWinNum, BOOL bEnable )
{
  BOOL bWas = s_pWindows[usWinNum]->EnableShortCuts;
  s_pWindows[usWinNum]->EnableShortCuts = bEnable;
  return( bWas );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*            Function borrowed from HBPrint.lib                     */
/*                                                                   */

BOOL HB_EXPORT hb_wvw_gtDrawImage( USHORT usWinNum, int x1, int y1, int wd, int ht, char * image )
{
  IStream  *iStream;
  IPicture *iPicture;
  HGLOBAL  hGlobal;
  HANDLE   hFile;
  DWORD    nFileSize;
  DWORD    nReadByte;
  LONG     lWidth,lHeight;
  int      x,y,xe,ye;
  int      c   = x1 ;
  int      r   = y1 ;
  int      dc  = wd ;
  int      dr  = ht ;
  int      tor =  0 ;
  int      toc =  0 ;
  HRGN     hrgn1;
  POINT    lpp = { 0 };
  BOOL     bResult = FALSE;

  WIN_DATA * pWindowData = s_pWindows[ usWinNum ];

  hFile = CreateFile( image, GENERIC_READ, 0, NULL, OPEN_EXISTING,
                                      FILE_ATTRIBUTE_NORMAL, NULL );
  if ( hFile != INVALID_HANDLE_VALUE )
  {
    nFileSize = GetFileSize( hFile, NULL );

    if ( nFileSize != INVALID_FILE_SIZE )
    {
      hGlobal = GlobalAlloc( GPTR, nFileSize );

      if ( hGlobal )
      {
        if ( ReadFile( hFile, hGlobal, nFileSize, &nReadByte, NULL ) )
        {
          CreateStreamOnHGlobal( hGlobal, TRUE, &iStream );
          OleLoadPicture( iStream, nFileSize, TRUE, (REFIID) &IID_IPicture, ( LPVOID* )&iPicture );
          if ( iPicture )
          {
            iPicture->lpVtbl->get_Width( iPicture,&lWidth );
            iPicture->lpVtbl->get_Height( iPicture,&lHeight );

            if ( dc  == 0 )
            {
              dc = ( int ) ( ( float ) dr * lWidth  / lHeight );
            }
            if ( dr  == 0 )
            {
              dr = ( int ) ( ( float ) dc * lHeight / lWidth  );
            }
            if ( tor == 0 )
            {
              tor = dr;
            }
            if ( toc == 0 )
            {
              toc = dc;
            }
            x  = c;
            y  = r;
            xe = c + toc - 1;
            ye = r + tor - 1;

            GetViewportOrgEx( pWindowData->hdc, &lpp );

            hrgn1 = CreateRectRgn( c+lpp.x, r+lpp.y, xe+lpp.x, ye+lpp.y );
            SelectClipRgn( pWindowData->hdc, hrgn1 );

            while ( x < xe )
            {
              while ( y < ye )
              {
                iPicture->lpVtbl->  Render( iPicture, pWindowData->hdc, x, y, dc, dr, 0,
                                            lHeight, lWidth, -lHeight, NULL );
                y += dr;
              }
              y =  r;
              x += dc;
            }

            SelectClipRgn( pWindowData->hdc, NULL );
            DeleteObject( hrgn1 );

            iPicture->lpVtbl->Release( iPicture );
            bResult = TRUE ;
          }
        }
        GlobalFree( hGlobal );
      }
    }
    CloseHandle( hFile );
  }
  return( bResult );
}

/*-------------------------------------------------------------------*/

IPicture * HB_EXPORT hb_wvw_gtLoadPicture( char * image )
{
  IStream  *iStream;

  LPVOID   iPicture = NULL;
  HGLOBAL  hGlobal;
  HANDLE   hFile;
  DWORD    nFileSize;
  DWORD    nReadByte;

  hFile = CreateFile( image, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );
  if ( hFile != INVALID_HANDLE_VALUE )
  {
    nFileSize = GetFileSize( hFile, NULL );

    if ( nFileSize != INVALID_FILE_SIZE )
    {
      hGlobal = GlobalAlloc( GPTR, nFileSize );

      if ( hGlobal )
      {
        if ( ReadFile( hFile, hGlobal, nFileSize, &nReadByte, NULL ) )
        {
          CreateStreamOnHGlobal( hGlobal, TRUE, &iStream );

          OleLoadPicture( iStream, nFileSize, TRUE, (REFIID) &IID_IPicture, &iPicture );

        }
        GlobalFree( hGlobal );
      }
    }
    CloseHandle( hFile );
  }

  return ( IPicture * ) iPicture;
}

/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtRenderPicture( USHORT usWinNum, int x1, int y1, int wd, int ht, IPicture * iPicture )
{
  LONG     lWidth,lHeight;
  int      x,y,xe,ye;
  int      c   = x1 ;
  int      r   = y1 ;
  int      dc  = wd ;
  int      dr  = ht ;
  int      tor =  0 ;
  int      toc =  0 ;
  HRGN     hrgn1;
  POINT    lpp = { 0 };
  BOOL     bResult = FALSE;
  WIN_DATA * pWindowData = s_pWindows[usWinNum];

  if ( iPicture )
  {
    iPicture->lpVtbl->get_Width( iPicture,&lWidth );
    iPicture->lpVtbl->get_Height( iPicture,&lHeight );

    if ( dc  == 0 )
    {
      dc = ( int ) ( ( float ) dr * lWidth  / lHeight );
    }
    if ( dr  == 0 )
    {
      dr = ( int ) ( ( float ) dc * lHeight / lWidth  );
    }
    if ( tor == 0 )
    {
      tor = dr;
    }
    if ( toc == 0 )
    {
      toc = dc;
    }
    x  = c;
    y  = r;
    xe = c + toc - 1;
    ye = r + tor - 1;

    GetViewportOrgEx( pWindowData->hdc, &lpp );

    hrgn1 = CreateRectRgn( c+lpp.x, r+lpp.y, xe+lpp.x, ye+lpp.y );
    SelectClipRgn( pWindowData->hdc, hrgn1 );

    while ( x < xe )
    {
      while ( y < ye )
      {
        iPicture->lpVtbl->  Render( iPicture, pWindowData->hdc, x, y, dc, dr, 0,
                                            lHeight, lWidth, -lHeight, NULL );
        y += dr;
      }
      y =  r;
      x += dc;
    }

    SelectClipRgn( pWindowData->hdc, NULL );
    DeleteObject( hrgn1 );

    bResult = TRUE ;
  }

  return( bResult );
}

/*-------------------------------------------------------------------*/

BOOL HB_EXPORT hb_wvw_gtDestroyPicture( IPicture * iPicture )
{
   BOOL bResult = FALSE;

   if ( iPicture )
   {
      iPicture->lpVtbl->Release( iPicture );
      bResult = TRUE;
   }
   return bResult;
}

/*-------------------------------------------------------------------*/

HB_EXPORT WIN_DATA * hb_wvw_gtGetGlobalData( USHORT usWinNum )
{
   return s_pWindows[ usWinNum ];
}

/*-------------------------------------------------------------------*/

HB_EXPORT COLORREF hb_wvw_gtGetColorData( int iIndex )
{
   return _COLORS[ iIndex ];
}

/*-------------------------------------------------------------------*/

HB_EXPORT BOOL hb_wvw_gtSetColorData( int iIndex, COLORREF ulCr )
{
   BOOL bResult = FALSE;

   if ( iIndex >= 0 && iIndex < 16 )
   {
      _COLORS[ iIndex ] = ulCr;
      bResult = TRUE;
   }
   return bResult;
}

/*-------------------------------------------------------------------*/

/*
   difference with gtwvt's:
   here we have an option bTight.
   if it is true, only one pixel lines used (outer lines are not drawn)

   TODO: combine it with aOffset like DrawImage ?
*/

HB_EXPORT void hb_wvw_gtDrawBoxRaised( USHORT usWinNum, int iTop, int iLeft, int iBottom, int iRight,
                                       BOOL bTight )  /* <-- none in gtwvt */
{
   WIN_DATA * pWindowData = s_pWindows[ usWinNum ];

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_sApp.penWhiteDim );
   }
   else
   {
     SelectObject( pWindowData->hdc, s_sApp.penWhite );
   }

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );        /*  Top Inner  */
   LineTo( pWindowData->hdc, iRight, iTop );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );        /*  Left Inner */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_sApp.penWhite );

     MoveToEx( pWindowData->hdc, iLeft-1, iTop-1, NULL );    /*  Top Outer  */
     LineTo( pWindowData->hdc, iRight+1, iTop-1 );

     MoveToEx( pWindowData->hdc, iLeft-1, iTop-1, NULL );    /*  Left Outer */
     LineTo( pWindowData->hdc, iLeft-1, iBottom+1 );
   }

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_sApp.penDarkGray );
   }
   else
   {
     SelectObject( pWindowData->hdc, s_sApp.penBlack );
   }

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );     /*  Bottom Inner  */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iRight, iBottom, NULL );    /*  Right Inner   */
   LineTo( pWindowData->hdc, iRight, iTop );

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_sApp.penBlack );

     MoveToEx( pWindowData->hdc, iLeft-1, iBottom+1, NULL ); /*  Bottom Outer */
     LineTo( pWindowData->hdc, iRight+1+1, iBottom+1 );

     MoveToEx( pWindowData->hdc, iRight+1, iTop-1, NULL );   /*  Right Outer  */
     LineTo( pWindowData->hdc, iRight+1, iBottom+1 );
   }

}

/*-------------------------------------------------------------------*/

/*
   difference with gtwvt's:
   here we have an option bTight.
   if it is true, only one pixel lines used (outer lines are not drawn)

   TODO: combine it with aOffset like DrawImage ?
*/

HB_EXPORT void hb_wvw_gtDrawBoxRecessed( USHORT usWinNum, int iTop, int iLeft, int iBottom, int iRight,
                                       BOOL bTight )
{
   WIN_DATA * pWindowData = s_pWindows[ usWinNum ];

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_sApp.penWhiteDim );
   }
   else
   {
     SelectObject( pWindowData->hdc, s_sApp.penWhite );
   }

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );            /* Right Inner  */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );          /* Bottom Inner */
   LineTo( pWindowData->hdc, iRight, iBottom );

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_sApp.penWhite );

     MoveToEx( pWindowData->hdc, iRight+1, iTop-1, NULL );        /* Right Outer  */
     LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 );

     MoveToEx( pWindowData->hdc, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
     LineTo( pWindowData->hdc, iRight + 2, iBottom + 1 );

   }

   SelectObject( pWindowData->hdc, s_sApp.penBlack );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );             /* Left Inner */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );             /* Top Inner  */
   LineTo( pWindowData->hdc, iRight, iTop );

   if (!bTight)
   {
     SelectObject( pWindowData->hdc, s_sApp.penDarkGray );

     MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );     /* Left Outer */
     LineTo( pWindowData->hdc, iLeft - 1 , iBottom + 1 );

     MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );     /* Top Outer  */
     LineTo( pWindowData->hdc, iRight + 1, iTop - 1 );
   }

}

/*-------------------------------------------------------------------*/

HB_EXPORT void hb_wvw_gtDrawOutline( USHORT usWinNum, int iTop, int iLeft, int iBottom, int iRight )
{
   WIN_DATA * pWindowData = s_pWindows[ usWinNum ];

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );        /*  Top     */
   LineTo( pWindowData->hdc, iRight, iTop );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );        /*  Left    */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );     /*  Bottom   */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );       /*  Right    */
   LineTo( pWindowData->hdc, iRight, iBottom + 1);

}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                 Peter Rees <peter@rees.co.nz>                     */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETMENU )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  SetMenu( s_pWindows[usWinNum]->hWnd, ( HMENU ) hb_parni( 2 ) ) ;

  hb_wvw_gtResetWindow( usWinNum );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETPOPUPMENU )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  HMENU hPopup = s_pWindows[usWinNum]->hPopup ;

   s_pWindows[usWinNum]->hPopup = ( HMENU ) hb_parnl( 2 );
   /* if ( hPopup ) */
   {
      hb_retnl( ( LONG ) hPopup );
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_CREATEMENU )
{
  hb_retnl( ( LONG ) CreateMenu() ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_CREATEPOPUPMENU )
{
  hb_retnl( ( LONG ) CreatePopupMenu() ) ;
}

/*-------------------------------------------------------------------*/

/* WVW_APPENDMENU( hMenu, nFlags, nMenuItemId, cCaption ) */

HB_FUNC( WVW_APPENDMENU )
{
  char    ucBuf[ 256 ];
  int     i,iLen ;
  LPCTSTR lpszCaption;

  if ( !(hb_parni(2) & (MF_SEPARATOR|MF_POPUP)) &&
        (hb_parni(3) >= WVW_ID_BASE_PUSHBUTTON ) )
  {
    MessageBox( NULL, TEXT( "Menu Command Id too high. Potential conflict with pushbutton" ),
                szAppName, MB_ICONERROR );
    hb_retl( FALSE );
    return;
  }

  if ( ISCHAR( 4 ) )
  {
    iLen = hb_parclen( 4 );
    if ( iLen > 0 && iLen < 256 )
    {
      lpszCaption = hb_parcx( 4 ) ;
      for ( i=0; i< iLen ; i++ )
      {
        ucBuf[ i ] = ( *lpszCaption == '~' ) ? '&' : *lpszCaption ;
        lpszCaption++;
      }
      ucBuf[ iLen ]= '\0';
      lpszCaption = ucBuf ;
    }
    else
    {
      lpszCaption = hb_parcx( 4 ) ;
    }
  }
  else
  {
    lpszCaption = ( LPCTSTR ) hb_parni( 4 ) ;
  }

  hb_retl( AppendMenu( ( HMENU ) hb_parnl( 1 ), ( UINT ) hb_parni( 2 ), ( UINT_PTR ) hb_parni( 3 ),( LPCTSTR ) lpszCaption ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_DELETEMENU )
{
  hb_retl( DeleteMenu( ( HMENU ) hb_parnl( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_DESTROYMENU )
{
  hb_retl( DestroyMenu( ( HMENU ) hb_parnl( 1 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_ENABLEMENUITEM )
{
  hb_retni( EnableMenuItem( ( HMENU ) hb_parnl( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETLASTMENUEVENT )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  hb_retni( hb_wvw_gtGetLastMenuEvent( usWinNum ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETLASTMENUEVENT )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  hb_retni( hb_wvw_gtSetLastMenuEvent( usWinNum, hb_parni(2) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETMENUKEYEVENT )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  int iEvent = 0;

  if ( ISNUM( 2 ) )
  {
    iEvent = hb_parnl( 2 ) ;
  }

  hb_retni( hb_wvw_gtSetMenuKeyEvent( usWinNum, iEvent ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_DRAWMENUBAR )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  DrawMenuBar( pWindowData->hWnd ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETSCREENWIDTH )
{
  hb_retni( GetSystemMetrics( SM_CXSCREEN ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETSCREENHEIGHT )
{
  hb_retni( GetSystemMetrics( SM_CYSCREEN ) );
}

/*-------------------------------------------------------------------*/

/*WVW_SetWindowCentre( nWinNum,   (0==MAIN)
 *                     lCentre,
 *                     lPaintIt)  (if .f. it will just assign lCentre to WIN_DATA)
 */
HB_FUNC( WVW_SETWINDOWCENTRE )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  hb_wvw_gtSetCentreWindow( usWinNum, hb_parl( 2 ), hb_parl( 3 ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETALTF4CLOSE )
{
  hb_retl( hb_wvw_gtSetAltF4Close( hb_parl( 1 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_PROCESSMESSAGES )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  hb_wvw_gtDoProcessMessages( usWinNum );

  hb_retl( 1 );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETTITLE )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  BYTE ucText[ 1024 ];

  hb_wvw_gtGetWindowTitle( usWinNum, ( char* ) ucText, 1023 );

  hb_retc( ( char* ) ucText ) ;
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*   Author.....: Francesco Saverio Giudice <info@fsgiudice.com>     */
/*   Syntax.....: Wvw_GetRGBColor( nColor ) --> nRGBColor            */
/*   Description: Return the RGB values passing the color positional value */
/*                0=Black, 1=Blue, etc                               */
/*                as returned from hb_ColorToN()                     */
/*   Creat. Date: 2004/01/15                                         */
/*   Last Modif.: 2004/01/15                                         */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETRGBCOLOR )
{
   int iColor;
   if ( !ISNIL( 1 ) )
   {
      iColor = hb_parni( 1 );
      if ( iColor >= 0 && iColor < 16 )  /* Test bound error */

      {
         hb_retnl( _COLORS[ iColor ] );

      }
   }
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                       Giancarlo Niccolai                          */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/*                    Clipboard functions                            */
/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETCLIPBOARD )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;

   if ( !IsClipboardFormatAvailable(CF_TEXT) )
   {
     hb_ret();
   }

   if (!OpenClipboard( NULL ))
   {
     hb_ret();
   }

   hglb = GetClipboardData(CF_TEXT);
   if (hglb != NULL)
   {
      lptstr = (LPSTR) GlobalLock(hglb);
      if (lptstr != NULL)
      {
         hb_retc( lptstr );
         GlobalUnlock(hglb);
      }
   }
   CloseClipboard();
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETCLIPBOARD )
{
   LPTSTR  lptstrCopy;
   HGLOBAL hglbCopy;
   char *  cText;
   int     nLen;

   if ( !IsClipboardFormatAvailable( CF_TEXT ) )
   {
     hb_retl( FALSE );
     return;
   }

   /* Check params
    */
   if ( ! ISCHAR( 1 ) )
   {
     hb_retl( FALSE );
     return;
   }

   if ( ! OpenClipboard( NULL ) )
   {
     hb_retl( FALSE );
     return;
   }
   EmptyClipboard();

   /* Get text from PRG
    */
   cText = hb_parcx( 1 );
   nLen  = hb_parclen( 1 );

   /* Allocate a global memory object for the text.
    */
   hglbCopy = GlobalAlloc( GMEM_MOVEABLE, ( nLen+1 ) * sizeof( TCHAR ) );
   if ( hglbCopy == NULL )
   {
       CloseClipboard();
       hb_retl( FALSE );
       return;
   }

   /* Lock the handle and copy the text to the buffer.
    */
   lptstrCopy = ( LPSTR ) GlobalLock( hglbCopy );
   memcpy( lptstrCopy, cText, ( nLen+1 ) * sizeof( TCHAR ) );
   lptstrCopy[ nLen+1 ] = ( TCHAR ) 0;
   GlobalUnlock( hglbCopy );

   /* Place the handle on the clipboard.
    */
   SetClipboardData( CF_TEXT, hglbCopy );

   CloseClipboard();
   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_PASTEFROMCLIPBOARD )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;
   ULONG     ul;

   if ( !IsClipboardFormatAvailable( CF_TEXT ) )
   {
     hb_ret();
   }

   if ( !OpenClipboard( NULL ) )
   {
     hb_ret();
   }

   hglb = GetClipboardData( CF_TEXT );
   if ( hglb != NULL )
   {
      lptstr = ( LPSTR ) GlobalLock( hglb );
      if ( lptstr != NULL )
      {
         /*TraceLog( NULL, "Clipboard %s\n", (LPSTR) lptstr );        */
         /*TraceLog( NULL, "Clipboard size %u\n", GlobalSize(hglb) ); */

         for ( ul=0; ul < GlobalSize( hglb ); ul++ )
         {
            hb_wvw_gtAddCharToInputQueue( ( int ) lptstr[ ul ] );
            /*TraceLog( NULL, "Value %i\n", ( int ) lptstr[ ul ] );   */
         }
         GlobalUnlock( hglb ) ;
      }
   }
   CloseClipboard();
}

HB_FUNC( WVW_KEYBOARD )
{
   hb_wvw_gtAddCharToInputQueue( hb_parnl( 1 ) );
}

/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/*                    End of Clipboard Functions                     */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_INVALIDATERECT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   RECT  rc = { 0 };
   POINT xy = { 0 };

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy           = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   rc.top       = xy.y;
   rc.left      = xy.x;
   xy           = hb_wvw_gtGetXYFromColRow( pWindowData, usRight+1, usBottom+1 );
   rc.bottom    = xy.y - 1;
   rc.right     = xy.x - 1;

   InvalidateRect( pWindowData->hWnd, &rc, TRUE );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_ISLBUTTONPRESSED )
{
   hb_retl( GetKeyState( VK_LBUTTON ) & 0x8000 );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_CLIENTTOSCREEN )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HB_ITEM  aXY;
   HB_ITEM  temp;
   POINT    xy = { 0 };
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, NULL, NULL);
   }

   xy = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );

   ClientToScreen( pWindowData->hWnd, &xy );

   aXY.type  = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &aXY, 2 );

   hb_arraySetForward( &aXY, 1, hb_itemPutNL( &temp, xy.x ) );
   hb_arraySetForward( &aXY, 2, hb_itemPutNL( &temp, xy.y ) );

   hb_itemReturn( &aXY );
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Pritpal Bedi <pritpal@vouchcac.com>                 */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETFONT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   hb_retl( hb_wvw_gtSetFont( usWinNum,
            ISNIL( 2 ) ? s_pWindows[usWinNum]->fontFace   : hb_parcx( 2 ),
            ISNIL( 3 ) ? s_pWindows[usWinNum]->fontHeight : hb_parni( 3 ),
            ISNIL( 4 ) ? s_pWindows[usWinNum]->fontWidth  : hb_parni( 4 ),
            ISNIL( 5 ) ? s_pWindows[usWinNum]->fontWeight : hb_parni( 5 ),
            ISNIL( 6 ) ? s_pWindows[usWinNum]->fontQuality: hb_parni( 6 )
           ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETICON )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   if ( ISNUM( 2 ) || ISCHAR( 3 ) )
   {

      hb_retnl( hb_wvw_gtSetWindowIcon( usWinNum, hb_parni( 2 ), hb_parc( 3 ) ) ) ;
   }
   else
   {
      hb_retnl( hb_wvw_gtSetWindowIconFromFile( usWinNum, hb_parcx( 2 ) ) ) ;
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETTITLE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   hb_wvw_gtSetWindowTitle( usWinNum, hb_parcx( 2 ) ) ;
   return ;
}

/*-------------------------------------------------------------------*/

/* WVW_SetWindowPos( nWinNum, nXposition, nYposition)  (position in pixel) */
HB_FUNC( WVW_SETWINDOWPOS )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   hb_wvw_gtSetWindowPos( usWinNum, hb_parni( 2 ), hb_parni( 3 ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETWINDOWHANDLE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   hb_retnl( ( LONG ) hb_wvw_gtGetWindowHandle( usWinNum ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETCODEPAGE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   hb_retni( hb_wvw_gtSetCodePage( usWinNum, hb_parni( 2 ) ) );
}

/*-------------------------------------------------------------------*/

/* WVW_CenterWindow( nWinNum, lCenter, lPaint )   (nWinNum==0==MAIN) */
HB_FUNC( WVW_CENTERWINDOW )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   hb_retl( hb_wvw_gtSetCentreWindow( usWinNum,
               ISNIL( 2 ) ? TRUE  : hb_parl( 2 ),
               ISNIL( 3 ) ? FALSE : hb_parl( 3 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETMOUSEMOVE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   if ( ISNIL( 2 ) )
   {
      hb_retl( s_pWindows[usWinNum]->MouseMove );
   }
   else
   {
      hb_retl( hb_wvw_gtSetMouseMove( usWinNum, hb_parl( 2 ) ) );
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETXYFROMROWCOL )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   HB_ITEM  aXY;
   HB_ITEM  temp;
   POINT     xy = { 0 };

   xy   = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], hb_parni( 3 ), hb_parni( 2 ) );

   aXY.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &aXY, 2 );

   hb_arraySetForward( &aXY, 1, hb_itemPutNL( &temp, xy.x ));
   hb_arraySetForward( &aXY, 2, hb_itemPutNL( &temp, xy.y ) );

   hb_itemReturn( &aXY );

   /** 20040330 old method:
   PHB_ITEM  aXY;
   PHB_ITEM  temp;
   POINT     xy = { 0 };

   xy   = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], hb_parni( 3 ), hb_parni( 2 ) );

   aXY  = hb_itemArrayNew( 2 );

   temp = hb_itemPutNL( NULL, xy.x );
   hb_arraySet( aXY, 1, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, xy.y );
   hb_arraySet( aXY, 2, temp );
   hb_itemRelease( temp );

   hb_itemReturn( aXY );
   hb_itemRelease( aXY );
   ***/
}

/*-------------------------------------------------------------------*/

/* WVW_GetRowColFromXY( [nWinNum], nX, nY )
 * return an array {nRow, nCol}
 */
HB_FUNC( WVW_GETROWCOLFROMXY )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   HB_ITEM  aRowCol;
   HB_ITEM  temp;
   POINT    RowCol;

   RowCol   = hb_wvw_gtGetColRowFromXY( s_pWindows[ usWinNum ], hb_parni( 2 ), hb_parni( 3 ) );

   aRowCol.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &aRowCol, 2 );

   hb_arraySetForward( &aRowCol, 1, hb_itemPutNL( &temp, RowCol.y ));
   hb_arraySetForward( &aRowCol, 2, hb_itemPutNL( &temp, RowCol.x ) );

   hb_itemReturn( &aRowCol );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETFONTINFO )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   HB_ITEM  info;
   HB_ITEM  temp;

   info.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &info, 7 );

   hb_arraySetForward( &info, 1, hb_itemPutC( &temp, s_pWindows[usWinNum]->fontFace ) );
   hb_arraySetForward( &info, 2, hb_itemPutNL( &temp, s_pWindows[usWinNum]->fontHeight ) );
   hb_arraySetForward( &info, 3, hb_itemPutNL( &temp, s_pWindows[usWinNum]->fontWidth ) );
   hb_arraySetForward( &info, 4, hb_itemPutNL( &temp, s_pWindows[usWinNum]->fontWeight ) );
   hb_arraySetForward( &info, 5, hb_itemPutNL( &temp, s_pWindows[usWinNum]->fontQuality ) );
   hb_arraySetForward( &info, 6, hb_itemPutNL( &temp, s_pWindows[usWinNum]->PTEXTSIZE.y ) );
   hb_arraySetForward( &info, 7, hb_itemPutNL( &temp, s_pWindows[usWinNum]->PTEXTSIZE.x ) );

   hb_itemReturn( &info );

}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETPALETTE )
{
   HB_ITEM  info;
   HB_ITEM  temp;
   int       i;

   info.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &info, 16 );

   for ( i = 0; i < 16; i++ )
   {
      hb_arraySetForward( &info, i+1, hb_itemPutNL( &temp, _COLORS[ i ] ) );

   }

   hb_itemReturn( &info );

}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Wvw_SetPalette( aRGBValues ) -> An array of 16 elements with RGB values */
/*                                                                   */
HB_FUNC( WVW_SETPALETTE )
{
   int       i;

   for ( i = 0; i < 16; i++ )
   {
      _COLORS[ i ] = hb_parnl( 1, i+1 );

   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_MINIMIZE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   ShowWindow( s_pWindows[usWinNum]->hWnd, SW_MINIMIZE );
}

/*-------------------------------------------------------------------*/

/* wvw_maximize( [nWinNum] )
   maximizes the window, if callback function WVW_SIZE exists

   note: in gtwvt wvt_maximize() restores the window, not maximizes it
   see also: WVW_RESTORE(), WVW_MAXMAXROW(), WVW_MAXMAXCOL()
 */
HB_FUNC( WVW_MAXIMIZE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   if ( !(s_sApp.pSymWVW_SIZE) )
   {
     /* the old, default behaviour as in gtwvt */
     ShowWindow( s_pWindows[usWinNum]->hWnd, SW_RESTORE );
   }
   else
   {
     /* app seems to be ready to handle the maximized window */
     ShowWindow( s_pWindows[usWinNum]->hWnd, SW_MAXIMIZE );

   }
}

/* wvw_restore( [nWinNum] )
   restores the window (similar with gtwvt's wvt_maximize())

   WARNING: restoring window from its maximized state might need handling
            in callback function WVW_SIZE,
            because this function assumes no change in maxrow()/maxcol()
   see also: WVW_MAXIMIZE(), WVW_MAXMAXROW(), WVW_MAXMAXCOL()
 */
HB_FUNC( WVW_RESTORE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   ShowWindow( s_pWindows[usWinNum]->hWnd, SW_RESTORE );
}

/* about WVW_SIZE callback function:

   parameters:
   function WVW_SIZE(nWinNum, hWnd, message, wParam, lParam)

   notes:
   * this function is called by gtwvw AFTER the size is changed
   * WARNING: screen repainting is not performed completely by gtwvw at this point of call
   * WARNING: this function may be called BEFORE gtwvw initialization (use wvw_gtinit() to check)
   * WARNING: this function may be called AFTER xharbour vm cleans up static variables,
              so do NOT use static variables in this function (unless you guard the usage properly)!
              you may however uses MEMVAR such as PUBLIC variables

 */

/*-------------------------------------------------------------------*/

/* NOTE: this is not supported in GTWVW
HB_FUNC( WVW_SETGUI )
{
   BOOL bGui = s_sApp.bGui;

   if ( ! ISNIL( 1 ) )
   {
      s_sApp.bGui = hb_parl( 1 );
   }

   hb_retl( bGui );
}
*/

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETONTOP )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   RECT rect = { 0 };

   GetWindowRect( s_pWindows[usWinNum]->hWnd, &rect );

   hb_retl( SetWindowPos( s_pWindows[usWinNum]->hWnd, HWND_TOPMOST,
                          rect.left,
                          rect.top,
                          0,
                          0,
                          SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETASNORMAL )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   RECT rect = { 0 };

   GetWindowRect( s_pWindows[usWinNum]->hWnd, &rect );

   hb_retl( SetWindowPos( s_pWindows[usWinNum]->hWnd, HWND_NOTOPMOST,
                          rect.left,
                          rect.top,
                          0,
                          0,
                          SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE ) );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   aScr := Wvw_SaveScreen( nWinNum, nTop, nLeft, nBottom, nRight ) */
/*                                                                   */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
 *      besides, with Windowing feature, it seems not needed anymore
 */

HB_FUNC( WVW_SAVESCREEN )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HBITMAP  hBmp, oldBmp;
   POINT    xy = { 0 };
   int      iTop, iLeft, iBottom, iRight, iWidth, iHeight;
   HB_ITEM  info;
   HB_ITEM  temp;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   info.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usLeft, usTop );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usRight + 1, usBottom + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   iWidth  = iRight - iLeft + 1;
   iHeight = iBottom - iTop + 1;

   hBmp    = CreateCompatibleBitmap( s_pWindows[usWinNum]->hdc, iWidth, iHeight ) ;

   oldBmp = (HBITMAP) SelectObject( pWindowData->hCompDC, hBmp );
   BitBlt( pWindowData->hCompDC, 0, 0, iWidth, iHeight, pWindowData->hdc, iLeft, iTop, SRCCOPY );
   SelectObject( pWindowData->hCompDC, oldBmp );

   hb_arrayNew( &info, 3 );

   hb_arraySetForward( &info, 1, hb_itemPutNI( &temp, iWidth ) );
   hb_arraySetForward( &info, 2, hb_itemPutNI( &temp, iHeight ) );
   hb_arraySetForward( &info, 3, hb_itemPutNL( &temp, ( ULONG ) hBmp ) );

   hb_itemReturn( &info );

}

/*-------------------------------------------------------------------*/
/*                                                                     */
/*   Wvw_RestScreen( nWinNum, nTop, nLeft, nBottom, nRight, aScr, lDoNotDestroyBMP )*/
/*                                                                     */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
 *      besides, with Windowing feature, it seems not needed anymore
 */

HB_FUNC( WVW_RESTSCREEN )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT   xy = { 0 };
   int     iTop, iLeft, iBottom, iRight, iWidth, iHeight;

   HBITMAP hBmp;

   BOOL    bResult = FALSE;
   BOOL    bDoNotDestroyBMP = ISNIL( 7 ) ? FALSE : hb_parl( 7 );
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usLeft, usTop );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usRight + 1, usBottom + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   iWidth  = iRight - iLeft + 1 ;
   iHeight = iBottom - iTop + 1 ;

   hBmp    = (HBITMAP) SelectObject( pWindowData->hCompDC, ( HBITMAP ) hb_parnl( 6,3 ) );
   if ( hBmp )
   {
      if ( ( iWidth == hb_parni( 6,1 ) )  && ( iHeight == hb_parni( 6,2 ) ) )
      {
         if ( BitBlt( s_pWindows[usWinNum]->hdc,
                      iLeft,
                      iTop,
                      iWidth,
                      iHeight,
                      s_pWindows[usWinNum]->hCompDC,
                      0,
                      0,
                      SRCCOPY ) )
         {
            bResult = TRUE;
         }
      }
      else
      {
         if ( StretchBlt( s_pWindows[usWinNum]->hdc,
                          iLeft,
                          iTop,
                          iWidth,
                          iHeight,
                          s_pWindows[usWinNum]->hCompDC,
                          0,
                          0,
                          hb_parni( 6,1 ),
                          hb_parni( 6,2 ),
                          SRCCOPY ) )
         {
            bResult = TRUE;
         }
      }
   }

   SelectObject( pWindowData->hCompDC, hBmp );

   if ( ! bDoNotDestroyBMP )
   {
      DeleteObject( ( HBITMAP ) hb_parnl( 6,3 ) );
   }

   hb_retl( bResult );
}

/*-------------------------------------------------------------------*/
/*                                                                     */
/* Wvw_CreateFont( cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline,*/
/*                 lStrikeout, nCharSet, nQuality, nEscapement )            */
/*                                                                          */
HB_FUNC( WVW_CREATEFONT )
{
   USHORT usWinNum = s_usNumWindows-1;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   LOGFONT  logfont;
   HFONT    hFont;

   logfont.lfEscapement     = ( ISNIL( 10 ) ? 0 : ( hb_parni( 10 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( ISNIL(  4 ) ? 0 : hb_parni( 4 ) );
   logfont.lfItalic         = ( ISNIL(  5 ) ? 0 : hb_parl(  5 ) );
   logfont.lfUnderline      = ( ISNIL(  6 ) ? 0 : hb_parl(  6 ) );
   logfont.lfStrikeOut      = ( ISNIL(  7 ) ? 0 : hb_parl(  7 ) );
   logfont.lfCharSet        = ( ISNIL(  8 ) ? pWindowData->CodePage : hb_parni( 8 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( ISNIL( 9 ) ? DEFAULT_QUALITY : hb_parni( 9 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( ISNIL(  2 ) ? pWindowData->fontHeight : hb_parni( 2 ) );
   logfont.lfWidth          = ( ISNIL(  3 ) ? ( pWindowData->fontWidth < 0 ? -pWindowData->fontWidth : pWindowData->fontWidth ) : hb_parni( 3 ) );

   strcpy( logfont.lfFaceName, ( ISNIL( 1 ) ? pWindowData->fontFace : hb_parcx( 1 ) ) );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      hb_retnl( ( ULONG ) hFont );
   }
   else
   {
      hb_retnl( 0 );
   }
}

/*-------------------------------------------------------------------
 *
 *    WVW_DRAWLABELOBJ( [nWinNum], nTop, nLeft, nBottom, nRight, cLabel, nAlignHorz, nAlignVert, nTextColor, nBkColor, hFont,
 *                      aOffset )
 */
HB_FUNC( WVW_DRAWLABELOBJ )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT    xy = { 0 };
   int      iTop, iLeft, iBottom, iRight, x, y;
   int      iOffTop, iOffLeft, iOffBottom, iOffRight;
   RECT     rect = { 0 };
   HFONT    oldFont;
   int      oldTextAlign, iAlignHorz, iAlignVert, iAlignH = 0, iAlignV;
   COLORREF oldBkColor, oldTextColor;
   UINT     uiOptions;
   SIZE     sz = { 0 };

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   iOffTop    = !ISNIL( 12 ) ? hb_parni( 12,1 ) :  0 ;
   iOffLeft   = !ISNIL( 12 ) ? hb_parni( 12,2 ) :  0 ;
   iOffBottom = !ISNIL( 12 ) ? hb_parni( 12,3 ) :  0 ;
   iOffRight  = !ISNIL( 12 ) ? hb_parni( 12,4 ) :  0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy           = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop         = xy.y + iOffTop;
   iLeft        = xy.x + iOffLeft;

   xy           = hb_wvw_gtGetXYFromColRow( pWindowData, usRight+1, usBottom+1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom      = xy.y - 1 + 1 + iOffBottom;
   iRight       = xy.x - 1 + 1 + iOffRight;

   iAlignHorz   = ( ISNIL( 7 ) ? 0 : hb_parni( 7 ) );
   iAlignVert   = ( ISNIL( 8 ) ? 0 : hb_parni( 8 ) );

   oldTextColor = SetTextColor( pWindowData->hdc, ISNIL( 9 ) ? pWindowData->foreground : ( COLORREF ) hb_parnl( 9 ) );
   oldBkColor   = SetBkColor( pWindowData->hdc, ISNIL( 10 ) ? pWindowData->background : ( COLORREF ) hb_parnl( 10 ) );
   oldFont      = ( HFONT ) SelectObject( pWindowData->hdc, ( HFONT ) hb_parnl( 11 ) );

   GetTextExtentPoint32( pWindowData->hdc, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), &sz );

   x = iLeft;
   y = iTop;

   switch ( iAlignHorz )
   {
      case 0:
      {
         iAlignH = TA_LEFT;
      }
      break;
      case 1:
      {
         iAlignH = TA_RIGHT;
         x = iRight;
      }
      break;
      case 2:
      {
         iAlignH = TA_CENTER;
         x = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
      }
      break;
   }

   iAlignV = TA_TOP;

   switch ( iAlignVert )
   {
      case 1:
      {
         y = iBottom - sz.cy;
      }
      break;
      case 2:
      {
         y = iTop + ( ( iBottom - iTop + 1 - sz.cy ) / 2 ) ;
      }
      break;
   }

   oldTextAlign = SetTextAlign( pWindowData->hdc, iAlignH | iAlignV );

   rect.top     = iTop;
   rect.left    = iLeft;
   rect.bottom  = iBottom;
   rect.right   = iRight;

   uiOptions    = ETO_CLIPPED | ETO_OPAQUE ;

   ExtTextOut( pWindowData->hdc, x, y, uiOptions, &rect, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), NULL );

   SelectObject( pWindowData->hdc, oldFont );
   SetTextAlign( pWindowData->hdc, oldTextAlign );
   SetBkColor( pWindowData->hdc, oldBkColor );
   SetTextColor( pWindowData->hdc, oldTextColor );

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/

/* removed from GTWVT, so we remove it from here also. I really don;t like doing it...
HB_FUNC( WVW_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HANDLE ) hb_parnl( 1 ) ) );
}
*/

/*-------------------------------------------------------------------*/
/*                                                                                       */
/*    Wvw_DrawToolButtonState( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOff, nState )*/
/*                                                                                       */
HB_FUNC( WVW_DRAWTOOLBUTTONSTATE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iState = hb_parni( 7 );

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + hb_parni( 6,1 );
   iLeft   = xy.x + hb_parni( 6,2 );

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parni( 6,3 );
   iRight  = xy.x - 1 + hb_parni( 6,4 );

   switch ( iState )
   {
      case 0:
      {
         SelectObject( pWindowData->hdc, s_sApp.penGray );

         MoveToEx( pWindowData->hdc, iRight, iTop, NULL );           /* Right       */
         LineTo( pWindowData->hdc, iRight, iBottom + 1);

         MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );         /* Bottom      */
         LineTo( pWindowData->hdc, iRight, iBottom );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left        */
         LineTo( pWindowData->hdc, iLeft, iBottom );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top         */
         LineTo( pWindowData->hdc, iRight, iTop );
      }
      break;

      case 1:
      {
         SelectObject( pWindowData->hdc, s_sApp.penBlack );

         MoveToEx( pWindowData->hdc, iRight, iTop, NULL );           /* Right       */
         LineTo( pWindowData->hdc, iRight, iBottom+1 );

         MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );         /* Bottom      */
         LineTo( pWindowData->hdc, iRight, iBottom );

         SelectObject( pWindowData->hdc, s_sApp.penWhite );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left        */
         LineTo( pWindowData->hdc, iLeft, iBottom );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top         */
         LineTo( pWindowData->hdc, iRight, iTop );
      }
      break;

      case 2:
      {
         SelectObject( pWindowData->hdc, s_sApp.penWhite );

         MoveToEx( pWindowData->hdc, iRight, iTop, NULL );           /* Right       */
         LineTo( pWindowData->hdc, iRight, iBottom+1 );

         MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );         /* Bottom      */
         LineTo( pWindowData->hdc, iRight, iBottom );

         SelectObject( pWindowData->hdc, s_sApp.penBlack );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left        */
         LineTo( pWindowData->hdc, iLeft, iBottom );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top         */
         LineTo( pWindowData->hdc, iRight, iTop );
      }
      break;

   }
   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                                                 */
/*   Wvw_DrawScrollButton( [nWinNum], nTop, nLeft, nBottom, nRight, aOffPixels, nTLBR, lDepressed )*/
/*                                                                                                 */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be usefull */
HB_FUNC( WVW_DRAWSCROLLBUTTON )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   int   iTop, iLeft, iBottom, iRight;
   POINT    * Point;
   POINT    xy = { 0 };
   int      iHeight, iOff;
   BOOL     bDepressed = ISNIL( 8 ) ? FALSE : hb_parl( 8 ) ;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + hb_parni( 6,1 );
   iLeft   = xy.x + hb_parni( 6,2 );

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parni( 6,3 );
   iRight  = xy.x - 1 + hb_parni( 6,4 );

   Point      = ( POINT * ) hb_xgrab( 3 * sizeof( POINT ) );
   iOff       = 6;

   iHeight    = iBottom - iTop + 1;

   if ( bDepressed )
   {
      hb_wvw_gtDrawBoxRecessed( usWinNum, iTop+1, iLeft+1, iBottom-2, iRight-2, FALSE );
   }
   else
   {
      hb_wvw_gtDrawBoxRaised( usWinNum, iTop+1, iLeft+1, iBottom-2, iRight-2, FALSE );
   }
   SelectObject( pWindowData->hdc, s_sApp.solidBrush );

   switch ( hb_parni( 7 ) )
   {
      case 1:
      {
         xy.y       = iTop + iOff - 1;
         xy.x       = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
         Point[ 0 ] = xy ;
         xy.y       = iBottom - iOff - 1;
         xy.x       = iLeft + iOff - 1;
         Point[ 1 ] = xy;
         xy.x       = iRight - iOff + 1;
         Point[ 2 ] = xy;
      }
      break;

      case 2:
      {
         xy.y       = iTop + ( ( iBottom - iTop + 1 ) / 2 );
         xy.x       = iLeft + iOff;
         Point[ 0 ] = xy ;
         xy.x       = iRight - iOff - 1;
         xy.y       = iTop + iOff - 1;
         Point[ 1 ] = xy;
         xy.y       = iBottom - iOff + 1;
         Point[ 2 ] = xy;
      }
      break;

      case 3:
      {
         xy.x       = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
         xy.y       = iBottom - iOff;
         Point[ 0 ] = xy ;
         xy.x       = iLeft + iOff - 1;
         xy.y       = iBottom - iHeight + iOff + 1;
         Point[ 1 ] = xy;
         xy.x       = iRight - iOff + 1;
         Point[ 2 ] = xy;
      }
      break;

      case 4:
      {
         xy.x       = iRight - iOff - 1;
         xy.y       = iTop + ( ( iBottom - iTop + 1 ) / 2 );
         Point[ 0 ] = xy ;
         xy.x       = iLeft + iOff + 1;
         xy.y       = iTop + iOff - 1;
         Point[ 1 ] = xy;
         xy.y       = iBottom - iOff + 1;
         Point[ 2 ] = xy;
      }
      break;
   }

   Polygon( pWindowData->hdc, Point, 3 );

   hb_xfree( Point );
}

/*-------------------------------------------------------------------*/
/*                                                                                              */
/*  Wvw_DrawScrollbarThumbVert( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlScroll, nThumbPos )*/
/*                                                                                              */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be usefull */
HB_FUNC( WVW_DRAWSCROLLTHUMBVERT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int iTabTop, iTabLft, iTabBtm, iTabRgt;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   USHORT   usTabTop = hb_parni( 7 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
     hb_wvw_HBFUNCPrologue(usWinNum, &usTabTop, NULL, NULL, NULL);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + hb_parni( 6,1 );
   iLeft   = xy.x + hb_parni( 6,2 );

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parni( 6,3 );
   iRight  = xy.x - 1 + hb_parni( 6,4 );

   SetBkColor( pWindowData->hdc, RGB( 230,230,230 ) );
   SelectObject( pWindowData->hdc, s_sApp.diagonalBrush );

   SelectObject( pWindowData->hdc, s_sApp.penNull );
   Rectangle( pWindowData->hdc, iLeft, iTop, iRight+1, iBottom+1 );

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft /* dummy */ , usTabTop );
   iTabTop = xy.y;

   iTabLft  = iLeft;
   iTabBtm  = iTabTop + pWindowData->PTEXTSIZE.y - 1;
   iTabRgt  = iRight;

   SelectObject( pWindowData->hdc, s_sApp.wvwWhiteBrush );
   SelectObject( pWindowData->hdc, s_sApp.penGray );
   Rectangle( pWindowData->hdc, iTabLft, iTabTop, iTabRgt+1, iTabBtm );

   hb_wvw_gtDrawBoxRaised( usWinNum, iTabTop+1, iTabLft+1, iTabBtm-2, iTabRgt-2, FALSE );
}

/*-------------------------------------------------------------------*/
/*                                                                                              */
/*  Wvw_DrawScrollbarThumbHorz( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffset, nThumbPos )*/
/*                                                                                              */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be usefull */
HB_FUNC( WVW_DRAWSCROLLTHUMBHORZ )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT xy = { 0 };
   int iThumbLeft, iThumbRight;
   int   iTop, iLeft, iBottom, iRight;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   USHORT   usThumbLeft = hb_parni( 7 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
     hb_wvw_HBFUNCPrologue(usWinNum, NULL, &usThumbLeft, NULL, NULL);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + hb_parni( 6,1 );
   iLeft   = xy.x + hb_parni( 6,2 );

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parni( 6,3 );
   iRight  = xy.x - 1 + hb_parni( 6,4 );

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usThumbLeft, usTop /* dummy */ );
   iThumbLeft  = xy.x;

   iThumbRight = iThumbLeft + ( pWindowData->PTEXTSIZE.x * 2 ) - 1;

   SetBkColor( pWindowData->hdc, RGB( 230,230,230 ) );
   SelectObject( pWindowData->hdc, s_sApp.diagonalBrush );
   SelectObject( pWindowData->hdc, s_sApp.penNull );
   Rectangle( pWindowData->hdc, iLeft, iTop, iRight+1, iBottom+1 );

   SelectObject( pWindowData->hdc, s_sApp.wvwWhiteBrush );
   SelectObject( pWindowData->hdc, s_sApp.penGray );
   Rectangle( pWindowData->hdc, iThumbLeft, iTop, iThumbRight, iBottom );
   hb_wvw_gtDrawBoxRaised( usWinNum, iTop+1, iThumbLeft+1, iBottom-2, iThumbRight-2, FALSE );
}

/*-------------------------------------------------------------------*/
/*                                                                                                      */
/*    Wvw_DrawShadedRect( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffSet, nHorVert, aRGBb, aRGBe  )*/
/*                                                                                                      */
HB_FUNC( WVW_DRAWSHADEDRECT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   BOOL bGF = FALSE;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   if ( s_sApp.hMSImg32 )
   {
      TRIVERTEX     vert[ 2 ] ;
      GRADIENT_RECT gRect  = { 0 };

      int iMode       = ISNIL( 7 ) ? GRADIENT_FILL_RECT_H : hb_parni( 7 ) ;
      POINT xy = { 0 };
      int   iTop, iLeft, iBottom, iRight;

      xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
      iTop    = xy.y + hb_parni( 6,1 );
      iLeft   = xy.x + hb_parni( 6,2 );

      xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

      xy.y   -= pWindowData->byLineSpacing;

      iBottom = xy.y - 1 + hb_parni( 6,3 );
      iRight  = xy.x - 1 + hb_parni( 6,4 );

      vert[ 0 ].x     = iLeft;
      vert[ 0 ].y     = iTop;
      vert[ 0 ].Red   = hb_parni( 8,1 );
      vert[ 0 ].Green = hb_parni( 8,2 );
      vert[ 0 ].Blue  = hb_parni( 8,3 );
      vert[ 0 ].Alpha = hb_parni( 8,4 );

      vert[ 1 ].x     = iRight;
      vert[ 1 ].y     = iBottom;
      vert[ 1 ].Red   = hb_parni( 9,1 );
      vert[ 1 ].Green = hb_parni( 9,2 );
      vert[ 1 ].Blue  = hb_parni( 9,3 );
      vert[ 1 ].Alpha = hb_parni( 9,4 );

      gRect.UpperLeft = 0;
      gRect.LowerRight= 1;

      bGF = ( BOOL ) s_sApp.pfnGF( pWindowData->hdc, vert, 2, &gRect, 1, iMode );
   }
   hb_retl( bGF );
}

/*-------------------------------------------------------------------*/
/*                                                                                 */
/*   Wvw_DrawTextBox( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffSet, cText, ;*/
/*                    nAlignHorz, nAlignVert, nTextColor, nBackColor, ;            */
/*                    nBackMode, hFont )                                           */
/*                                                                                 */
HB_FUNC( WVW_DRAWTEXTBOX )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;

   int iAlignHorz   = ( ISNIL( 8 ) ? 0 : hb_parni( 8 ) );

   int iAlignH = 0;

   COLORREF oldTextColor, oldBkColor;
   HFONT    oldFont;
   int      oldTextAlign, oldBkMode;
   RECT     rc = { 0 };

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + hb_parni( 6,1 );
   iLeft   = xy.x + hb_parni( 6,2 );

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parni( 6,3 );
   iRight  = xy.x - 1 + hb_parni( 6,4 );

   switch ( iAlignHorz )
   {
      case 0:
      {
         iAlignH = DT_LEFT;
      }
      break;
      case 1:
      {
         iAlignH = DT_RIGHT;
      }
      break;
      case 2:
      {
         iAlignH = DT_CENTER;
      }
      break;
   }

   rc.top       = iTop;
   rc.left      = iLeft;
   rc.bottom    = iBottom;
   rc.right     = iRight;

   oldTextAlign = SetTextAlign( pWindowData->hdc, TA_TOP | TA_LEFT | TA_NOUPDATECP );
   oldTextColor = SetTextColor( pWindowData->hdc, ISNIL( 10 ) ? pWindowData->foreground : ( COLORREF ) hb_parnl( 10 ) );
   oldBkColor   = SetBkColor( pWindowData->hdc, ISNIL( 11 ) ? pWindowData->background : ( COLORREF ) hb_parnl( 11) );
   oldBkMode    = SetBkMode( pWindowData->hdc, ISNIL( 12 ) ? OPAQUE : hb_parni( 12 ) );
   oldFont      = ( HFONT ) SelectObject( pWindowData->hdc, ( HFONT ) hb_parnl( 13 ) );

   DrawText( pWindowData->hdc, hb_parcx( 7 ), strlen( hb_parcx( 7 ) ), &rc, iAlignH | DT_WORDBREAK | DT_TOP );

   SetTextColor( pWindowData->hdc, oldTextColor );
   SetBkColor( pWindowData->hdc, oldBkColor );
   SetBkMode( pWindowData->hdc, oldBkMode );
   SetTextAlign( pWindowData->hdc, oldTextAlign );
   SelectObject( pWindowData->hdc, oldFont );
}

/*-------------------------------------------------------------------
 *
 * Wvw_DrawProgressBar( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlTLBR, nPercent,;
 *                      nBackColor, nBarColor, cImage, lVertical, nDirection )
 */
HB_FUNC( WVW_DRAWPROGRESSBAR )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   int      iTop     ;
   int      iLeft    ;
   int      iBottom  ;
   int      iRight   ;
   int      iPercent,  iBarUpto, iDirection;
   BOOL     bVertical, bImage;

   COLORREF crBarColor;
   HBRUSH   hBrush;
   LOGBRUSH lb = { 0 };
   RECT     rc = { 0 };
   POINT    xy = { 0 };

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + hb_parni( 6,1 );
   iLeft   = xy.x + hb_parni( 6,2 );

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parni( 6,3 );
   iRight  = xy.x - 1 + hb_parni( 6,4 );

   iPercent   = hb_parni( 7 );
   bImage     = ISNIL( 10 ) ? FALSE : TRUE ;
   bVertical  = ISNIL( 11 ) ? FALSE : hb_parl( 11 ) ;
   iDirection = ISNIL( 12 ) ? 0 : hb_parni( 12 );

   if ( bVertical )
   {
      if ( iDirection == 0 )
      {
         iBarUpto  = iTop + ( ( iBottom - iTop ) * iPercent / 100 );
         rc.top    = iTop;
         rc.left   = iLeft;
         rc.bottom = iBarUpto;
         rc.right  = iRight;
      }
      else
      {
         iBarUpto  = iBottom - ( ( iBottom - iTop ) * iPercent / 100 );
         rc.top    = iBarUpto;
         rc.left   = iLeft;
         rc.bottom = iBottom;
         rc.right  = iRight;
      }
   }
   else
   {
      if ( iDirection == 0 )
      {
         iBarUpto  = iLeft + ( ( iRight - iLeft ) * iPercent / 100 );
         rc.top    = iTop;
         rc.left   = iLeft;
         rc.bottom = iBottom;
         rc.right  = iBarUpto;
      }
      else
      {
         iBarUpto  = iRight - ( ( iRight - iLeft ) * iPercent / 100 );
         rc.top    = iTop;
         rc.left   = iBarUpto;
         rc.bottom = iBottom;
         rc.right  = iRight;
      }
   }

   if ( bImage )
   {
      hb_wvw_gtDrawImage( usWinNum, rc.left, rc.top, rc.right-rc.left+1, rc.bottom-rc.top+1, hb_parc( 10 ) );
   }
   else
   {

      crBarColor  = ISNIL( 9 ) ? hb_wvw_gtGetColorData(  0 ) : (COLORREF) hb_parnl( 9 );

      lb.lbStyle  = BS_SOLID;
      lb.lbColor  = crBarColor;
      lb.lbHatch  = 0;

      hBrush      = CreateBrushIndirect( &lb );

      rc.bottom++;
      rc.right++;
      FillRect( pWindowData->hdc, &rc, hBrush );

      DeleteObject( hBrush );
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETCURSORPOS )
 {
    POINT    xy = { 0 };
    HB_ITEM  info;
    HB_ITEM  temp;

    GetCursorPos( &xy );

    info.type = HB_IT_NIL;
    temp.type = HB_IT_NIL;

    hb_arrayNew( &info, 2 );

    hb_arraySetForward( &info, 1, hb_itemPutNI( &temp, xy.x ) );
    hb_arraySetForward( &info, 2, hb_itemPutNI( &temp, xy.y ) );

    hb_itemReturn( &info );
 }

/*-------------------------------------------------------------------*/

/* WVW_TrackPopupMenu([nWinNum], n) */
HB_FUNC( WVW_TRACKPOPUPMENU )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT xy = { 0 };

   GetCursorPos( &xy );

   hb_retnl( TrackPopupMenu( ( HMENU ) hb_parnl( 2 ) ,
                     TPM_CENTERALIGN | TPM_RETURNCMD ,
                                                xy.x ,
                                                xy.y ,
                                                   0 ,
                                            pWindowData->hWnd ,
                                                NULL ) );
}

/*-------------------------------------------------------------------*/

/* WVW_GetMenu([nWinNum]) */
HB_FUNC( WVW_GETMENU )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   hb_retnl( ( ULONG ) GetMenu( pWindowData->hWnd ) );
}

/*-------------------------------------------------------------------*/

/* WVW_ShowWindow( [nWinNum], nCmdShow ) */
HB_FUNC( WVW_SHOWWINDOW )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int    iCmdShow = ISNUM(2) ? hb_parni(2) : SW_SHOWNORMAL;

   ShowWindow( pWindowData->hWnd, iCmdShow );
}

/*-------------------------------------------------------------------*/

/* WVW_UpdateWindow( [nWinNum] ) */
HB_FUNC( WVW_UPDATEWINDOW )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   UpdateWindow( pWindowData->hWnd );
}

/*-------------------------------------------------------------------*
 *-------------------------------------------------------------------*
 *-------------------------------------------------------------------*
 *
 *                             Dialogs
 *          original work by Pritpal Bedi in WVTUTILS.C
 *-------------------------------------------------------------------*
 *-------------------------------------------------------------------*
 *-------------------------------------------------------------------*/

HB_FUNC( WVW_CREATEDIALOGDYNAMIC )
{
   PHB_ITEM pFirst = hb_param( 3,HB_IT_ANY );
   PHB_ITEM pFunc  = NULL ;
   PHB_DYNS pExecSym;
   HWND     hDlg = NULL;
   int      iIndex;
   int      iType = 0;
   int      iResource = hb_parni( 4 );

   /* check if we still have room for a new dialog */

   for ( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
   {
      if ( s_sApp.hDlgModeless[ iIndex ] == NULL )
      {
         break;
      }
   }

   if ( iIndex >= WVW_DLGML_MAX )
   {
      /* no more room */
      hb_retnl( (ULONG) NULL );
      return;
   }

   if ( HB_IS_BLOCK( pFirst ) )
   {

      /* pFunc is pointing to stored code block (later) */
      pFunc = hb_itemNew( pFirst );
      iType = 2;
   }
   else if( pFirst->type == HB_IT_STRING )
   {
      hb_dynsymLock();
      pExecSym = hb_dynsymFindName( pFirst->item.asString.value );
      hb_dynsymUnlock();
      if ( pExecSym )
      {
         pFunc = ( PHB_ITEM ) pExecSym;
      }
      iType = 1;
   }

   {

      if ( ISNUM( 3 ) )
      {
         hDlg = CreateDialogIndirect( ( HINSTANCE     ) hb_hInstance,
                                      ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                                        hb_parl( 2 ) ? s_pWindows[0]->hWnd : NULL,
                                      ( DLGPROC       ) hb_parnl( 3 ) );
      }
      else
      {

         switch ( iResource )
         {
            case 0:
            {
               hDlg = CreateDialog( ( HINSTANCE     ) hb_hInstance,
                                                      hb_parc( 1 ),
                                                      hb_parl( 2 ) ? s_pWindows[0]->hWnd : NULL,
                                                      (DLGPROC) hb_wvw_gtDlgProcMLess );
            }
            break;

            case 1:
            {
               hDlg = CreateDialog( ( HINSTANCE     ) hb_hInstance,
                                    MAKEINTRESOURCE( ( WORD ) hb_parni( 1 ) ),
                                                      hb_parl( 2 ) ? s_pWindows[0]->hWnd : NULL,
                                                      (DLGPROC) hb_wvw_gtDlgProcMLess );
            }
            break;

            case 2:
            {
               hDlg = CreateDialogIndirect( ( HINSTANCE     ) hb_hInstance,
                                            ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                                              hb_parl( 2 ) ? s_pWindows[0]->hWnd : NULL,
                                                             (DLGPROC) hb_wvw_gtDlgProcMLess );
            }
            break;
         }
      }

      if ( hDlg )
      {
         s_sApp.hDlgModeless[ iIndex ] = hDlg;
         if ( pFunc )
         {

            /* if codeblock, store the codeblock and lock it there */
            if (HB_IS_BLOCK( pFirst ))
            {
               s_sApp.pcbFunc[ iIndex ] = pFunc;

            }

            s_sApp.pFunc[ iIndex ] = pFunc;
            s_sApp.iType[ iIndex ] = iType;
         }
         else
         {
            s_sApp.pFunc[ iIndex ] = NULL;
            s_sApp.iType[ iIndex ] = 0;
         }
         SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
      }
      else
      {

         if (iType==2 && pFunc)
         {
            hb_itemRelease( pFunc );
         }

         s_sApp.hDlgModeless[ iIndex ] = NULL;
      }
   }

   hb_retnl( ( ULONG ) hDlg );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_CREATEDIALOGMODAL )
{
   PHB_ITEM pFirst    = hb_param( 3,HB_IT_ANY );
   PHB_ITEM pFunc     = NULL ;
   PHB_DYNS pExecSym;
   int      iIndex;
   int      iResource = hb_parni( 4 );
   int      iResult   = 0;
   HWND     hParent   = ISNIL( 5 ) ? s_pWindows[0]->hWnd : ( HWND ) hb_parnl( 5 );

   /* check if we still have room for a new dialog */
   for ( iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++ )
   {
      if ( s_sApp.hDlgModal[ iIndex ] == NULL )
      {
         break;
      }
   }

   if ( iIndex >= WVW_DLGMD_MAX )
   {
      /* no more room */
      hb_retni( ( int ) NULL );
      return;
   }

   if ( HB_IS_BLOCK( pFirst ) )
   {
      /* pFunc is pointing to stored code block (later) */

      s_sApp.pcbFuncModal[ iIndex ] = hb_itemNew( pFirst );

      pFunc = s_sApp.pcbFuncModal[ iIndex ];
      s_sApp.pFuncModal[ iIndex ] = pFunc;
      s_sApp.iTypeModal[ iIndex ] = 2;
   }
   else if( pFirst->type == HB_IT_STRING )
   {
      hb_dynsymLock();
      pExecSym = hb_dynsymFindName( pFirst->item.asString.value );
      hb_dynsymUnlock();
      if ( pExecSym )
      {
         pFunc = ( PHB_ITEM ) pExecSym;
      }
      s_sApp.pFuncModal[ iIndex ] = pFunc;
      s_sApp.iTypeModal[ iIndex ] = 1;
   }

   switch ( iResource )
   {
      case 0:
      {
         iResult = DialogBoxParam( ( HINSTANCE     ) hb_hInstance,
                                                     hb_parc( 1 ),
                                                     hParent,
                                                     (DLGPROC) hb_wvw_gtDlgProcModal,
                                ( LPARAM ) ( DWORD ) iIndex+1 );
      }
      break;

      case 1:
      {
         iResult = DialogBoxParam( ( HINSTANCE     ) hb_hInstance,
                           MAKEINTRESOURCE( ( WORD ) hb_parni( 1 ) ),
                                                     hParent,
                                                     (DLGPROC) hb_wvw_gtDlgProcModal,
                                ( LPARAM ) ( DWORD ) iIndex+1 );
      }
      break;

      case 2:
      {
         iResult = DialogBoxIndirectParam( ( HINSTANCE     ) hb_hInstance,
                                           ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                                             hParent,
                                                            (DLGPROC) hb_wvw_gtDlgProcModal,
                                        ( LPARAM ) ( DWORD ) iIndex+1 );
      }
      break;
   }

   hb_retni( iResult );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW__MAKEDLGTEMPLATE )
{
   WORD  *p, *pdlgtemplate ;
   WORD  nItems = hb_parni( 1, 4 ) ;
   int   i, nchar ;
   DWORD lStyle ;

   pdlgtemplate = p = ( PWORD ) LocalAlloc( LPTR, 65534 )  ;

    lStyle = hb_parnl(1,3) ;

    *p++ = 1                        ;
    *p++ = 0xFFFF                   ;
    *p++ = LOWORD ( hb_parnl(1,1) ) ;
    *p++ = HIWORD ( hb_parnl(1,1) ) ;

    *p++ = LOWORD ( hb_parnl(1,2) ) ;
    *p++ = HIWORD ( hb_parnl(1,2) ) ;

    *p++ = LOWORD (lStyle)          ;
    *p++ = HIWORD (lStyle)          ;

    *p++ = (WORD)   nItems          ;
    *p++ = (short)  hb_parni(1,5)   ;
    *p++ = (short)  hb_parni(1,6)   ;
    *p++ = (short)  hb_parni(1,7)   ;
    *p++ = (short)  hb_parni(1,8)   ;
    *p++ = (short)  0               ;
    *p++ = (short)  0x00            ;

    if ( hb_parinfa( 1,11 ) == HB_IT_STRING )
    {
        nchar = nCopyAnsiToWideChar( p, TEXT( hb_parcx( 1,11 ) ) ) ;
        p += nchar   ;
    }
    else
    {
      *p++ =0 ;
    }

    if ( ( lStyle & DS_SETFONT ) )
    {
      *p++ = (short) hb_parni(1,12) ;
      *p++ = (short) hb_parni(1,13) ;
      *p++ = (short) hb_parni(1,14) ;

      nchar = nCopyAnsiToWideChar( p, TEXT( hb_parcx(1,15) ) ) ;
      p += nchar ;
    } ;

   for ( i = 1 ; i <= nItems ; i++ ) {

      p = lpwAlign (p) ;

      *p++ = LOWORD ( hb_parnl(2,i) ) ;
      *p++ = HIWORD ( hb_parnl(2,i) ) ;

      *p++ = LOWORD ( hb_parnl(3,i) ) ;
      *p++ = HIWORD ( hb_parnl(3,i) ) ;

      *p++ = LOWORD ( hb_parnl(4,i) ) ;
      *p++ = HIWORD ( hb_parnl(4,i) ) ;

      *p++ = (short)  hb_parni(5,i)   ;
      *p++ = (short)  hb_parni(6,i)   ;
      *p++ = (short)  hb_parni(7,i)   ;
      *p++ = (short)  hb_parni(8,i)   ;

      *p++ = LOWORD ( hb_parnl(9,i) ) ;
      *p++ = HIWORD ( hb_parnl(9,i) ) ;

      if ( hb_parinfa( 10,i ) == HB_IT_STRING )
         {
         nchar = nCopyAnsiToWideChar( p, TEXT ( hb_parcx( 10,i ) ) ) ;
         p += nchar ;
         }
      else
         {
         *p++ = 0xFFFF ;
         *p++ = (WORD) hb_parni(10,i) ;
         }

      if ( hb_parinfa( 11,i ) == HB_IT_STRING )
         {
         nchar = nCopyAnsiToWideChar( p, ( LPSTR ) hb_parcx( 11,i ) ) ;
         p += nchar ;
         }
      else
         {
         *p++ = 0xFFFF ;
         *p++ = (WORD) hb_parni(11,i) ;
         }

      *p++ = 0x00 ;
    } ;

    p = lpwAlign( p )  ;

    hb_retclen( ( LPSTR ) pdlgtemplate, ( ( ULONG ) p - ( ULONG ) pdlgtemplate ) ) ;

    LocalFree( LocalHandle( pdlgtemplate ) ) ;
}

/*-------------------------------------------------------------------
 *
 *  Helper routine.  Take an input pointer, return closest
 *  pointer that is aligned on a DWORD (4 byte) boundary.
 */

static LPWORD lpwAlign( LPWORD lpIn )
{
   ULONG ul;
   ul = ( ULONG ) lpIn;
   ul += 3;
   ul >>=2;
   ul <<=2;
  return ( LPWORD ) ul;
}

static int nCopyAnsiToWideChar( LPWORD lpWCStr, LPSTR lpAnsiIn )
{
   int nChar = 0;

   do
   {
      *lpWCStr++ = ( WORD ) *lpAnsiIn;
      nChar++;
   }
   while ( *lpAnsiIn++ );

   return nChar;
}

HB_FUNC( WVW_LBADDSTRING )
{
   SendMessage( GetDlgItem( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ) ), LB_ADDSTRING, 0, ( LPARAM )( LPSTR ) hb_parcx( 3 ) );
}

HB_FUNC( WVW_LBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ) ), LB_SETCURSEL, hb_parni( 3 ), 0 );
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC( WVW_CBADDSTRING )
{
   SendMessage( GetDlgItem( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ) ), CB_ADDSTRING, 0, ( LPARAM )( LPSTR ) hb_parcx( 3 ) );
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC( WVW_CBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ) ), CB_SETCURSEL, hb_parni( 3 ), 0 );
}

HB_FUNC( WVW_DLGSETICON )
{
   HICON hIcon;

   if ( ISNUM( 2 ) )
   {
      hIcon = LoadIcon( ( HINSTANCE ) hb_hInstance, MAKEINTRESOURCE( hb_parni( 2 ) ) );
   }
   else
   {
      hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, hb_parc( 2 ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
   }

   if ( hIcon )
   {
      SendMessage( ( HWND ) hb_parnl( 1 ), WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar ICON */
      SendMessage( ( HWND ) hb_parnl( 1 ), WM_SETICON, ICON_BIG,   ( LPARAM ) hIcon ); /* Set Task List Icon */
   }

   if ( hIcon )
   {
      hb_retnl( ( ULONG ) hIcon );
   }
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                      GUI Drawing Functions                        */
/*               Pritpal Bedi <pritpal@vouchcac.com>                 */
/*                                                                   */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*   Wvw_SetPen( nPenStyle, nWidth, nColor )                         */
/*                                                                   */

/* IMPORTANT: in prev release this functions has nWinNum parameter
              PENs are now application-wide.
 */

HB_FUNC( WVW_SETPEN )
{

   int      iPenWidth, iPenStyle;
   COLORREF crColor;
   HPEN     hPen;

   if ( ISNIL( 1 ) )
   {
      hb_retl( FALSE );
   }

   iPenStyle = hb_parni( 1 ) ;
   iPenWidth = ISNIL( 2 ) ? 0 : hb_parni( 2 );
   crColor   = ISNIL( 3 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 3 );

   hPen      = CreatePen( iPenStyle, iPenWidth, crColor );

   if ( hPen )
   {
      /* 20040923, was:
      if ( s_pWindows[usWinNum]->currentPen )
      {
         DeleteObject( s_pWindows[usWinNum]->currentPen );
      }
      s_pWindows[usWinNum]->currentPen = hPen;
      */

      if ( s_sApp.currentPen )
      {
         DeleteObject( s_sApp.currentPen );
      }
      s_sApp.currentPen = hPen;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   Wvw_SetBrush( nStyle, nColor, [ nHatch ] )                      */
/*                                                                   */

/* IMPORTANT: in prev release this functions has nWinNum parameter
              BRUSHes are now application-wide.
 */

HB_FUNC( WVW_SETBRUSH )
{

   HBRUSH   hBrush;
   LOGBRUSH lb = { 0 };

   if ( ISNIL( 1 ) )
   {
      hb_retl( FALSE );
   }

   lb.lbStyle = hb_parnl( 1 );
   lb.lbColor = ISNIL( 2 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 2 ) ;
   lb.lbHatch = ISNIL( 3 ) ? 0 : hb_parnl( 3 );

   hBrush     = CreateBrushIndirect( &lb );

   if ( hBrush )
   {
      /* 20040923,was:
      if ( s_pWindows[usWinNum]->currentBrush )
      {
         DeleteObject( s_pWindows[usWinNum]->currentBrush );
      }
      s_pWindows[usWinNum]->currentBrush = hBrush;
      */

      if ( s_sApp.currentBrush )
      {
         DeleteObject( s_sApp.currentBrush );
      }
      s_sApp.currentBrush = hBrush;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

/*-------------------------------------------------------------------
 *
 *   Wvw_DrawBoxGet( [nWinNum], nRow, nCol, nWidth,;
 *                   aOffset )   <-- additional parm, not exist in GTWVT
 *
 *NOTES: unlike GTWVT, GTWVW draw white lines on outer right and outer bottom
 *       Besides, scope is the same as DRAWBOXRECESSED, ie.
 *       two pixel out of char boundary
 */

HB_FUNC( WVW_DRAWBOXGET )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   POINT yz = { 0 };
   WIN_DATA * pWindowData;
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   USHORT usRow, usCol, usLen;

   pWindowData = s_pWindows[ usWinNum ];

   usRow = hb_parni( 2 );
   usCol = hb_parni( 3 );
   usLen = hb_parni( 4 );

   iOffTop    = !ISNIL( 5 ) ? hb_parni( 5,1 ) :  0 ;
   iOffLeft   = !ISNIL( 5 ) ? hb_parni( 5,2 ) :  0 ;
   iOffBottom = !ISNIL( 5 ) ? hb_parni( 5,3 ) :  0 ;
   iOffRight  = !ISNIL( 5 ) ? hb_parni( 5,4 ) :  0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usRow, &usCol, NULL, NULL);
   }

   xy = hb_wvw_gtGetXYFromColRow( pWindowData, usCol, usRow );
   iTop  = xy.y - 1 + iOffTop;
   iLeft = xy.x - 1 + iOffLeft;

   yz = hb_wvw_gtGetXYFromColRow( pWindowData, usCol + usLen, usRow + 1 );

   yz.y -= pWindowData->byLineSpacing;

   iBottom = yz.y + iOffBottom;
   iRight  = yz.x + iOffRight;

   SelectObject( pWindowData->hdc, s_sApp.penBlack );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );

   LineTo( pWindowData->hdc, iRight, iTop );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );

   LineTo( pWindowData->hdc, iLeft, iBottom );

   SelectObject( pWindowData->hdc, s_sApp.penDarkGray );

   MoveToEx( pWindowData->hdc, iLeft-1, iTop-1, NULL );

   LineTo( pWindowData->hdc, iRight+1, iTop-1 );

   MoveToEx( pWindowData->hdc, iLeft-1, iTop-1, NULL );

   LineTo( pWindowData->hdc, iLeft-1, iBottom+1 );

   /* GTWVW also draws right and bottom outer with single white line */
   SelectObject( pWindowData->hdc, s_sApp.penWhite );

   MoveToEx( pWindowData->hdc, iRight+1, iTop-1, NULL );
   LineTo( pWindowData->hdc, iRight+1, iBottom+1+1 );

   MoveToEx( pWindowData->hdc, iLeft-1, iBottom+1, NULL );
   LineTo( pWindowData->hdc, iRight+1, iBottom+1 );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );
   LineTo( pWindowData->hdc, iRight, iBottom+1 );

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   Wvw_DrawBoxRaised( nWinNum,                                     */
/*                   nTop, nLeft, nBottom, nRight,                   */
/*                   lTight) <--none in gtwvt                        */
/*                                                                   */
/*   if lTight, box is drawn inside the character region                               */
/*   AND top and left lines are lower two pixel down to make room for above/left object*/
/*   WARNING: gui object of this type subject to be overwritten by chars               */
/*   NOTE that these lines are to be overwritten by displayed char,                    */
/*        we are depending on the fact that gui object will be painted last            */
/*                                                                                     */

HB_FUNC( WVW_DRAWBOXRAISED )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   WIN_DATA * pWindowData;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   BOOL  bTight = ( ISNIL( 6 ) ? FALSE : hb_parl( 6 ) );

   pWindowData = s_pWindows[ usWinNum ];

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = bTight ? xy.y+2 : xy.y - 1;
   iLeft   = bTight ? xy.x+2 : xy.x - 1;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight+1, usBottom+1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = bTight ? xy.y - 1 : xy.y;
   iRight  = bTight ? xy.x - 1 : xy.x;

   hb_wvw_gtDrawBoxRaised( pWindowData->byWinId, iTop, iLeft, iBottom, iRight,
                                    bTight );  /* <-- none in gtwvt */

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                                         */
/*    Wvw_DrawBoxRecessed( nWinNum, ;                                                      */
/*                   nTop, nLeft, nBottom, nRight,                                         */
/*                   lTight/aOffset) <--none in gtwvt                                              */
/*                                                                                         */
/*   if lTight, box is drawn inside the character region                                   */
/*   AND top and left lines are lower two pixel down to make room for above/left object    */
/*   WARNING: gui object of this type subject to be overwritten by chars                   */
/*   NOTE that these lines are to be overwritten by displayed char,                        */
/*        we are depending on the fact that gui object will be painted last                */
/*                                                                                         */
/*   lTight may be replaced with aOffset parm {top,left,bottom,right}                      */
/*     ie. offset in pixel unit                                                            */
/*                                                                                         */

HB_FUNC( WVW_DRAWBOXRECESSED )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   WIN_DATA * pWindowData;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   BOOL  bUseArray = ISARRAY(6);

   BOOL  bTight = ( bUseArray || ISNIL( 6 ) ? FALSE : hb_parl( 6 ) );
   int   iOLeft, iOTop, iORight, iOBottom;

   pWindowData = s_pWindows[ usWinNum ];
   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   if (bTight)
   {

     iOTop   = 2;
     iOLeft  = 2;
     iOBottom= -1;
     iORight = -1;
   }
   else if (bUseArray)
   {

     iOTop   = hb_parni( 6,1 )-1;
     iOLeft  = hb_parni( 6,2 )-1;
     iOBottom= hb_parni( 6,3 );
     iORight = hb_parni( 6,4 );
   }
   else
   {

     iOTop = -1;
     iOLeft= -1;
     iOBottom = 0;
     iORight  = 0;
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + iOTop;
   iLeft   = xy.x + iOLeft;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y + iOBottom;
   iRight  = xy.x + iORight;

   hb_wvw_gtDrawBoxRecessed( pWindowData->byWinId, iTop, iLeft, iBottom, iRight,
                                    bTight );

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------
 *
 *    Wvw_DrawBoxGroup( nWinNum, ;
 *                   nTop, nLeft, nBottom, nRight,;
 *                   [aOffset] )
 *
 * NOTE: aOffset is TLBR offset in pixel. none in GTWVT
 */

HB_FUNC( WVW_DRAWBOXGROUP )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   WIN_DATA * pWindowData;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   pWindowData = s_pWindows[ usWinNum ];

   iOffTop    = !ISNIL( 6 ) ? hb_parni( 6,1 ) : 0 ;
   iOffLeft   = !ISNIL( 6 ) ? hb_parni( 6,2 ) : 0 ;
   iOffBottom = !ISNIL( 6 ) ? hb_parni( 6,3 ) : 0 ;
   iOffRight  = !ISNIL( 6 ) ? hb_parni( 6,4 ) : 0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y - 1 + iOffTop;
   iLeft   = xy.x - 1 + iOffLeft;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y + iOffBottom;
   iRight  = xy.x + iOffRight;

   SelectObject( pWindowData->hdc, s_sApp.penDarkGray );

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );          /* Right Inner   */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );        /* Bottom Inner  */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );   /* Left Outer    */
   LineTo( pWindowData->hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );   /* Top Outer     */
   LineTo( pWindowData->hdc, iRight + 1, iTop - 1 );

   SelectObject( pWindowData->hdc, s_sApp.penWhite );

   MoveToEx( pWindowData->hdc, iRight + 1, iTop, NULL );       /* Right Outer   */
   LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft -1, iBottom + 1, NULL );  /* Bottom Outer  */
   LineTo( pWindowData->hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left Inner    */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top Inner     */
   LineTo( pWindowData->hdc, iRight, iTop );

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Wvw_DrawBoxRaised( nWinNum, ;                                  */
/*                   nTop, nLeft, nBottom, nRight )                  */
/*                                                                   */

HB_FUNC( WVW_DRAWBOXGROUPRAISED )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   WIN_DATA * pWindowData;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   pWindowData = s_pWindows[ usWinNum ];

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y;
   iRight  = xy.x;

   SelectObject( pWindowData->hdc, s_sApp.penWhite );

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );           /* Right Inner   */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );         /* Bottom Inner  */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );    /* Left Outer    */
   LineTo( pWindowData->hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );    /* Top Outer     */
   LineTo( pWindowData->hdc, iRight + 1, iTop - 1 );

   SelectObject( pWindowData->hdc, s_sApp.penDarkGray );

   MoveToEx( pWindowData->hdc, iRight + 1, iTop, NULL );       /* Right Outer   */
   LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft -1, iBottom + 1, NULL );  /* Bottom Outer  */
   LineTo( pWindowData->hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left Inner    */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top Inner     */
   LineTo( pWindowData->hdc, iRight, iTop );

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                        */
/*    Wvw_DrawImage( nWinNum, ;                                           */
/*                   nTop, nLeft, nBottom, nRight, cImage/nPictureSlot, ; */
/*                   lTight/aOffset) <--none in gtwvt                     */
/*                                                                        */

HB_FUNC( WVW_DRAWIMAGE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   int   iLeft, iTop, iRight, iBottom;
   WIN_DATA * pWindowData;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   BOOL  bTight = ( ISARRAY(7) || ISNIL( 7 ) ? FALSE : hb_parl( 7 ) );
   BOOL  bUseArray = ISARRAY(7);
   int   iOLeft, iOTop, iORight, iOBottom;
   BOOL  bResult;

   pWindowData = s_pWindows[ usWinNum ];

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   if (bTight)
   {
     iOTop   = 2+1;
     iOLeft  = 2+1;
     iOBottom= -1;
     iORight = -1;
   }
   else if (bUseArray)
   {
     iOTop   = hb_parni( 7,1 );
     iOLeft  = hb_parni( 7,2 );
     iOBottom= hb_parni( 7,3 );
     iORight = hb_parni( 7,4 );
   }
   else
   {
     iOTop = iOLeft = iOBottom = iORight = 0;
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + iOTop;
   iLeft   = xy.x + iOLeft;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y-1 + iOBottom;
   iRight  = xy.x-1 + iORight;

   if ( ISNUM( 6 ) )
   {

      bResult = hb_wvw_gtRenderPicture( usWinNum, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, s_sApp.iPicture[ hb_parni( 6 )-1 ] ) ;
   }
   else
   {

      bResult = hb_wvw_gtDrawImage( usWinNum, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, hb_parcx( 6 ) ) ;
   }

   hb_retl( bResult );
}

/*-------------------------------------------------------------------*/
/*                                                                           */
/*    WVW_DRAWLABEL( nWinNum, ;                                              */
/*                   nRow, nCol, cLabel, nAlign, nEscapement, nTextColor, ;  */
/*                   nBkColor, cFontFace,nHeight, nWidth, nWeight, ;         */
/*                   nQuality, nCharSet, lItalic, lUnderline, lStrikeOut )   */
/*                                                                           */

HB_FUNC( WVW_DRAWLABEL )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT    xy = { 0 };
   HFONT    hFont, oldFont;
   LOGFONT  logfont = { 0 };
   int      oldTextAlign;
   COLORREF oldBkColor, oldTextColor;
   WIN_DATA * pWindowData;
   USHORT   usRow    = hb_parni( 2 ),
            usCol    = hb_parni( 3 );

   pWindowData = s_pWindows[ usWinNum ];

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usRow, &usCol, NULL, NULL);
   }

   logfont.lfEscapement     = ( ISNIL(  6 ) ? 0 : ( hb_parni( 6 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( ISNIL( 12 ) ? 0 : hb_parni( 12 ) );
   logfont.lfItalic         = ( ISNIL( 15 ) ? 0 : hb_parl( 15 ) );
   logfont.lfUnderline      = ( ISNIL( 16 ) ? 0 : hb_parl( 16 ) );
   logfont.lfStrikeOut      = ( ISNIL( 17 ) ? 0 : hb_parl( 17 ) );
   logfont.lfCharSet        = ( ISNIL( 14 ) ? pWindowData->CodePage : hb_parni( 14 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( ISNIL( 13 ) ? DEFAULT_QUALITY : hb_parni( 13 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( ISNIL( 10 ) ? pWindowData->fontHeight : hb_parni( 10 ) );
   logfont.lfWidth          = ( ISNIL( 11 ) ? (pWindowData->fontWidth <0 ? -pWindowData->fontWidth : pWindowData->fontWidth)  : hb_parni( 11 ) );

   strcpy( logfont.lfFaceName, ( ISNIL( 9 ) ? pWindowData->fontFace : hb_parcx( 9 ) ) );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      xy           = hb_wvw_gtGetXYFromColRow( pWindowData, usCol, usRow );

      oldBkColor   = SetBkColor( pWindowData->hdc, ISNIL( 8 ) ? pWindowData->background : ( COLORREF ) hb_parnl( 8 ) );
      oldTextColor = SetTextColor( pWindowData->hdc, ISNIL( 7 ) ? pWindowData->foreground : ( COLORREF ) hb_parnl( 7 ) );
      oldTextAlign = SetTextAlign( pWindowData->hdc, ( ISNIL( 5 ) ? TA_LEFT : hb_parni( 5 ) ) );
      oldFont      = (HFONT) SelectObject( pWindowData->hdc, hFont );

      ExtTextOut( pWindowData->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 4 ), strlen( hb_parcx( 4 ) ), NULL );

      SelectObject( pWindowData->hdc, oldFont );
      DeleteObject( hFont );
      SetTextAlign( pWindowData->hdc, oldTextAlign );
      SetBkColor( pWindowData->hdc, oldBkColor );
      SetTextColor( pWindowData->hdc, oldTextColor );

      hb_retl( TRUE );
   }

   hb_retl( FALSE );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Wvw_DrawOutline( nWinNum, ;                                    */
/*                   nTop, nLeft, nBottom, nRight,                   */
/*                   nThick, nShape, nRGBColor )                     */
/*                                                                   */

HB_FUNC( WVW_DRAWOUTLINE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   HPEN  hPen;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   WIN_DATA * pWindowData;

   pWindowData = s_pWindows[ usWinNum ];

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight+1, usBottom+1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y;
   iRight  = xy.x;

   if ( ISNUM( 6 ) )
   {

      hPen = CreatePen( hb_parni( 6 ), 0, ( ISNIL( 8 ) ? 0 : ( COLORREF ) hb_parnl( 8 ) ) );
      if ( hPen )
      {
         SelectObject( pWindowData->hdc, hPen );
      }
   }
   else
   {
      hPen = 0;
      SelectObject( pWindowData->hdc, s_sApp.penBlack );
   }

   hb_wvw_gtDrawOutline( usWinNum, iTop, iLeft, iBottom, iRight );

   if ( hPen )
   {
      DeleteObject( hPen );
   }

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/
/*                  1                                                                               */
/*                  2      3       4       5        6        7       8       9      10      11      */
/*                 12                                                                               */
/*   Wvw_DrawLine( nWinNum, ;                                                                       */
/*                 nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nStyle, nThick, nColor,; */
/*                 aOffset)                                                                         */
/*                                                                                                  */

HB_FUNC( WVW_DRAWLINE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT    xy = { 0 };
   int      iTop, iLeft, iBottom, iRight, iOffset ;
   int      iOffTop, iOffLeft, iOffBottom, iOffRight;
   int      iOrient, iFormat, iAlign, iStyle, iThick;
   int      x, y;
   COLORREF cr;
   HPEN     hPen;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   WIN_DATA * pWindowData;

   pWindowData = s_pWindows[ usWinNum ];

   iOffTop    = !ISNIL( 12 ) ? hb_parni( 12,1 ) : 0 ;
   iOffLeft   = !ISNIL( 12 ) ? hb_parni( 12,2 ) : 0 ;
   iOffBottom = !ISNIL( 12 ) ? hb_parni( 12,3 ) : 0 ;
   iOffRight  = !ISNIL( 12 ) ? hb_parni( 12,4 ) : 0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + iOffTop;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y-1 + iOffBottom;
   iRight  = xy.x-1 + iOffRight;

   iOrient = ISNIL( 6 ) ? 0 : hb_parni( 6 );
   iFormat = ISNIL( 7 ) ? 0 : hb_parni( 7 );
   iAlign  = ISNIL( 8 ) ? 0 : hb_parni( 8 );
   iStyle  = ISNIL( 9 ) ? 0 : hb_parni( 9 );
   iThick  = ISNIL(10 ) ? 0 : hb_parni(10 );
   cr      = ISNIL(11 ) ? 0 : ( COLORREF ) hb_parnl( 11 );

   x       = iLeft ;
   y       = iTop ;

   switch ( iAlign )
   {
      case 0:                  /* Center      */
      {
         if ( iOrient == 0 )   /* Horizontal  */
         {
            iOffset = ( ( iBottom - iTop ) / 2 ) ;
            y       = iTop + iOffset ;
         }
         else
         {
            iOffset = ( ( iRight - iLeft ) / 2 ) ;
            x       = iLeft + iOffset ;
         }
      }
      break;

      case 1:                  /* Top      */
      break;

      case 2:                  /* bottom   */
      {
         if ( iFormat == 0 || iFormat == 1 )
         {
            y = iBottom - 1;
         }
         else
         {
            y = iBottom;
         }
      }
      break;

      case 3:                  /* Left     */
      break;

      case 4:                  /* Right    */
      {
         if ( iFormat == 0 || iFormat == 1 )
         {
            x = iRight - 1;
         }
         else
         {
            x = iRight;
         }
      }
      break;
   }

   hPen = CreatePen( iStyle, iThick, cr );

   switch ( iFormat )
   {
      case 0:                                       /* Raised        */
      {
         if ( iOrient == 0 )                        /*  Horizontal   */
         {
            SelectObject( pWindowData->hdc, s_sApp.penWhite );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y+1, NULL );
            LineTo( pWindowData->hdc, iRight, y+1 );
         }
         else                                       /*  Vertical     */
         {
            SelectObject( pWindowData->hdc, s_sApp.penWhite );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x+1, y, NULL );
            LineTo( pWindowData->hdc, x+1, iBottom );
         }
      }
      break;

      case 1:                                      /* Recessed       */
      {
         if ( iOrient == 0 )                       /* Horizontal     */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
            SelectObject( pWindowData->hdc, s_sApp.penWhite );
            MoveToEx( pWindowData->hdc, x, y+1, NULL );
            LineTo( pWindowData->hdc, iRight, y+1 );
         }
         else                                      /*  Vertical      */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
            SelectObject( pWindowData->hdc, s_sApp.penWhite );
            MoveToEx( pWindowData->hdc, x+1, y, NULL );
            LineTo( pWindowData->hdc, x+1, iBottom );
         }
      }
      break;

      case 2:                                      /* Plain          */
      {
         if ( iOrient == 0 )                       /* Horizontal     */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
         }
         else                                      /*  Vertical      */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
         }
       }
      break;
   }

   DeleteObject( hPen );
   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Inside the area requested!                                     */
/*    Wvw_DrawEllipse( nWinNum, nTop, nLeft, nBottom, nRight ,;      */
/*                     aOffset)                                      */
/*                                                                   */

HB_FUNC( WVW_DRAWELLIPSE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int      iOffTop, iOffLeft, iOffBottom, iOffRight;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   iOffTop    = !ISNIL( 6 ) ? hb_parni( 6,1 ) : 0 ;
   iOffLeft   = !ISNIL( 6 ) ? hb_parni( 6,2 ) : 0 ;
   iOffBottom = !ISNIL( 6 ) ? hb_parni( 6,3 ) : 0 ;
   iOffRight  = !ISNIL( 6 ) ? hb_parni( 6,4 ) : 0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usLeft, usTop );
   iTop    = xy.y + iOffTop;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usRight + 1, usBottom + 1 );

   xy.y   -= s_pWindows[ usWinNum ]->byLineSpacing;

   iBottom = xy.y-1 + iOffBottom;
   iRight  = xy.x-1 + iOffRight;

   SelectObject( s_pWindows[usWinNum]->hdc, s_sApp.currentBrush );
   SelectObject( s_pWindows[usWinNum]->hdc, s_sApp.currentPen );

   hb_retl( Ellipse( s_pWindows[usWinNum]->hdc, iLeft, iTop, iRight, iBottom ) );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Wvw_DrawRectangle( nWinNum, nTop, nLeft, nBottom, nRight )     */
/*                     aOffset)                                      */
/*                                                                   */

HB_FUNC( WVW_DRAWRECTANGLE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int      iOffTop, iOffLeft, iOffBottom, iOffRight;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   iOffTop    = !ISNIL( 6 ) ? hb_parni( 6,1 ) : 0 ;
   iOffLeft   = !ISNIL( 6 ) ? hb_parni( 6,2 ) : 0 ;
   iOffBottom = !ISNIL( 6 ) ? hb_parni( 6,3 ) : 0 ;
   iOffRight  = !ISNIL( 6 ) ? hb_parni( 6,4 ) : 0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usLeft, usTop );
   iTop    = xy.y + iOffTop;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usRight + 1, usBottom + 1 );

   xy.y   -= s_pWindows[ usWinNum ]->byLineSpacing;

   iBottom = xy.y-1 + iOffBottom;
   iRight  = xy.x-1 + iOffRight;

   SelectObject( s_pWindows[usWinNum]->hdc, s_sApp.currentBrush );
   SelectObject( s_pWindows[usWinNum]->hdc, s_sApp.currentPen );

   hb_retl( Rectangle( s_pWindows[usWinNum]->hdc, iLeft, iTop, iRight, iBottom ) );
}

/*-------------------------------------------------------------------*/
/*by bdj                                                                                */
/*none in gtwvt                                                                         */
/*    Wvw_FillRectangle( nWinNum, nTop, nLeft, nBottom, nRight, nRGBcolor/hBrush,       */
/*                       lTight, lUseBrush )                                            */
/*                                                                                      */
/*   if lTight, rect is drawn inside the character region                               */
/*   AND top and left lines are lower two pixel down to make room for above/left object */
/*   WARNING: gui object of this type subject to be overwritten by chars                */
/*   NOTE that these lines are to be overwritten by displayed char,                     */
/*        we are depending on the fact that gui object will be painted last             */
/*                                                                                      */
/*   if lUseBrush, nRGBcolor is treated as a BRUSH handle                               */
/*                                                                                      */

HB_FUNC( WVW_FILLRECTANGLE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   COLORREF crRGBcolor = ( ISNIL( 6 ) ? 0 : hb_parnl( 6 ) );
   BOOL     bTight = ( ISNIL( 7 ) ? FALSE : hb_parl( 7 ) );
   BOOL     bUseBrush = ( ISNIL( 8 ) ? FALSE : hb_parl( 8 ) );
   LOGBRUSH lb = { 0 };
   HBRUSH   hBrush;
   RECT     xyRect = { 0 };

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usLeft, usTop );
   iTop    = bTight ? xy.y+2 : xy.y;
   iLeft   = bTight ? xy.x+2 : xy.x;

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usRight + 1, usBottom + 1 );

   xy.y   -= s_pWindows[ usWinNum ]->byLineSpacing;

   iBottom = xy.y-1;
   iRight  = xy.x-1;

   xyRect.left = iLeft;
   xyRect.top  = iTop;
   xyRect.right= iRight+1;
   xyRect.bottom = iBottom+1;

   lb.lbStyle = BS_SOLID;
   lb.lbColor = crRGBcolor;
   lb.lbHatch = 0;

   hBrush     = !bUseBrush ? CreateBrushIndirect( &lb ) : (HBRUSH) hb_parnl(6);

   FillRect( s_pWindows[usWinNum]->hdc, &xyRect, hBrush );

   if (!bUseBrush)
   {
     DeleteObject( hBrush );
   }

   hb_retl( TRUE );
}

/* PENDING decision:
20040908 TODO: GTWVT deliberately adds new parm aOffset before nRoundHeight
               I hate it when doing such thing
 */

/*-------------------------------------------------------------------*/
/*                                                                                         */
/*    Wvw_DrawRoundRect( nWinNum, nTop, nLeft, nBottom, nRight, ;
 *                       aOffset, ; <-- new parm
 *                       nRoundHeight, nRoundWidth */
/*                                                                                         */

/* WARNING!!!
 * unlike previous release of GTWVW, 6th parameter is now aOffset
 * This placement of new parameter is made in line with gtwvt's way of doing it
 */

HB_FUNC( WVW_DRAWROUNDRECT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight, iWd, iHt;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   int      iOffTop, iOffLeft, iOffBottom, iOffRight;

   iOffTop    = !ISNIL( 6 ) ? hb_parni( 6,1 ) : 0 ;
   iOffLeft   = !ISNIL( 6 ) ? hb_parni( 6,2 ) : 0 ;
   iOffBottom = !ISNIL( 6 ) ? hb_parni( 6,3 ) : 0 ;
   iOffRight  = !ISNIL( 6 ) ? hb_parni( 6,4 ) : 0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usLeft, usTop );
   iTop    = xy.y + iOffTop;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usRight + 1, usBottom + 1 );

   xy.y   -= s_pWindows[ usWinNum ]->byLineSpacing;

   iBottom = xy.y-1 + iOffBottom;
   iRight  = xy.x-1 + iOffRight;

   iWd     = ISNIL( 8 ) ? 0 : hb_parni( 8 );
   iHt     = ISNIL( 7 ) ? 0 : hb_parni( 7 );

   SelectObject( s_pWindows[usWinNum]->hdc, s_sApp.currentBrush );
   SelectObject( s_pWindows[usWinNum]->hdc, s_sApp.currentPen );

   hb_retl( RoundRect( s_pWindows[usWinNum]->hdc, iLeft, iTop, iRight, iBottom, iWd, iHt ) );
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Wvw_DrawFocusRect( nWinNum, nTop, nLeft, nBottom, nRight,;     */
/*                 aOffset)                                          */
/*                                                                   */

HB_FUNC( WVW_DRAWFOCUSRECT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   RECT  rc = { 0 };
   POINT xy = { 0 };
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   int      iOffTop, iOffLeft, iOffBottom, iOffRight;

   iOffTop    = !ISNIL( 6 ) ? hb_parni( 6,1 ) : 0 ;
   iOffLeft   = !ISNIL( 6 ) ? hb_parni( 6,2 ) : 0 ;
   iOffBottom = !ISNIL( 6 ) ? hb_parni( 6,3 ) : 0 ;
   iOffRight  = !ISNIL( 6 ) ? hb_parni( 6,4 ) : 0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy        = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usLeft, usTop );
   rc.top    = xy.y + iOffTop;
   rc.left   = xy.x + iOffLeft;

   xy        = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usRight + 1, usBottom + 1 );

   xy.y   -= s_pWindows[ usWinNum ]->byLineSpacing;

   rc.bottom = xy.y-1 + iOffBottom;
   rc.right  = xy.x-1 + iOffRight;

   hb_retl( DrawFocusRect( s_pWindows[usWinNum]->hdc, &rc ) );
}

/*NOTE: this is compatibility function with GTWVT
 *      similar with WVW_FillRectangle()
 */
/*-------------------------------------------------------------------
 *
 *   Wvw_DrawColorRect( nWinNum, nTop, nLeft, nBottom, nRight, aPxlOff, nRGB )
 */
HB_FUNC( WVW_DRAWCOLORRECT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   int      iOffTop, iOffLeft, iOffBottom, iOffRight;
   RECT rc = { 0 };
   POINT xy = { 0 };
   HBRUSH hBrush;

   iOffTop    = !ISNIL( 6 ) ? hb_parni( 6,1 ) : 0 ;
   iOffLeft   = !ISNIL( 6 ) ? hb_parni( 6,2 ) : 0 ;
   iOffBottom = !ISNIL( 6 ) ? hb_parni( 6,3 ) : 0 ;
   iOffRight  = !ISNIL( 6 ) ? hb_parni( 6,4 ) : 0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy        = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usLeft, usTop );
   rc.top    = xy.y + iOffTop;
   rc.left   = xy.x + iOffLeft;

   xy        = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usRight + 1, usBottom + 1 );

   xy.y   -= s_pWindows[ usWinNum ]->byLineSpacing;

   rc.bottom = xy.y-1 + iOffBottom;
   rc.right  = xy.x-1 + iOffRight;

   hBrush = CreateSolidBrush( ( COLORREF ) hb_parnl( 7 ) );

   if ( hBrush )
   {

      hb_retl( FillRect( pWindowData->hdc, &rc, hBrush ) );

      DeleteObject( hBrush );
   }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   Wvw_DrawGridHorz( nWinNum, ;                                    */
/*                   nTop, nLeft, nRight, nRows )                    */
/*                                                                   */

HB_FUNC( WVW_DRAWGRIDHORZ )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   USHORT usAtRow = hb_parni( 2 );
   int   iRows  = hb_parni( 5 );
   int   i, y;
   int   iLeft, iRight;
   WIN_DATA * pWindowData;
   USHORT usLeft, usRight;

   pWindowData = s_pWindows[ usWinNum ];

   usLeft = hb_parni( 3 );
   usRight = hb_parni( 4 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usAtRow, &usLeft, NULL, &usRight);
   }

   iLeft  = ( usLeft * pWindowData->PTEXTSIZE.x );
   iRight = ( ( ( usRight + 1 ) * pWindowData->PTEXTSIZE.x ) - 1 );

   SelectObject( pWindowData->hdc, s_sApp.currentPen );

   for ( i = 0; i < iRows; i++ )
   {

      y = ( ( usAtRow ) * hb_wvw_LineHeight( pWindowData ) );

      y += pWindowData->usTBHeight;

      MoveToEx( pWindowData->hdc, iLeft, y, NULL );
      LineTo( pWindowData->hdc, iRight, y );

      usAtRow++;
   }

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------
 *
 *     Wvw_DrawGridVert( nWinNum, ;
 *                   nTop, nBottom, aCols, nCols,;
 *                   [aOffset] )
 *
 * NOTE: aOffset is TLBR offset in pixel. none in GTWVT
 *       actually aOffset[4] (Right Offset) is not used here
 */

HB_FUNC( WVW_DRAWGRIDVERT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   int iTop, iBottom, x;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   int i;
   int iCharHeight, iCharWidth;
   int iTabs = hb_parni( 5 );
   WIN_DATA * pWindowData;
   USHORT usTop, usBottom, usCol;

   pWindowData = s_pWindows[ usWinNum ];

   if ( ! iTabs )
   {
      hb_retl( FALSE );
   }

   iOffTop    = !ISNIL( 6 ) ? hb_parni( 6,1 ) : 0 ;
   iOffLeft   = !ISNIL( 6 ) ? hb_parni( 6,2 ) : 0 ;
   iOffBottom = !ISNIL( 6 ) ? hb_parni( 6,3 ) : 0 ;
   iOffRight  = !ISNIL( 6 ) ? hb_parni( 6,4 ) : 0;  /* is not actually used */

   HB_SYMBOL_UNUSED( iOffRight );

   usTop = hb_parni( 2 );
   usBottom = hb_parni( 3 );
   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, NULL, &usBottom, NULL);
   }

   iCharWidth  = pWindowData->PTEXTSIZE.x;

   iCharHeight = hb_wvw_LineHeight( pWindowData );

   iTop    = ( usTop * iCharHeight ) + pWindowData->usTBHeight + iOffTop;
   iBottom = ( ( usBottom + 1 ) * iCharHeight ) - 1 + pWindowData->usTBHeight + iOffBottom;

   SelectObject( pWindowData->hdc, s_sApp.currentPen );

   for ( i = 1; i <= iTabs; i++ )
   {
      usCol = hb_parni( 4,i );
      if (s_bMainCoordMode)
      {
        usCol -= pWindowData->usColOfs;
      }

      x = ( usCol * iCharWidth ) + iOffLeft;

      MoveToEx( pWindowData->hdc, x, iTop, NULL );
      LineTo( pWindowData->hdc, x, iBottom );
   }

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                                  */
/*    Wvw_DrawButton( nWinNum, ;                                                    */
/*                   nTop, nLeft, nBottom, nRight, cText, cImage/nImage, nFormat, ; */
/*                    nTextColor, nBkColor, nImageAt )                              */
/*                                                                                  */

HB_FUNC( WVW_DRAWBUTTON )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   SIZE     sz = { 0 };
   POINT    xy = { 0 };
   RECT     rc = { 0 };
   int      iTop, iLeft, iBottom, iRight;
   int      iAlign, oldTextAlign, oldBkMode;
   int      iTextHeight /*, iTextWidth */ ;
   int      iImageWidth, iImageHeight;
   COLORREF /* oldBkColor, */ oldTextColor;
   LOGBRUSH lb = { 0 };
   HBRUSH   hBrush;
   IPicture *iPicture;

   BOOL     bText     = ISCHAR( 6 );
   BOOL     bImage    = !( ISNIL( 7 ) );
   int      iFormat   = ISNIL(  8 ) ? 0 : hb_parni( 8 );

   COLORREF textColor = ISNIL(  9 ) ? _COLORS[ 0 ] : ( COLORREF ) hb_parnl( 9 ) ;
   COLORREF bkColor   = ISNIL( 10 ) ? _COLORS[ 7 ] : ( COLORREF ) hb_parnl( 10) ;

   WIN_DATA * pWindowData;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   pWindowData = s_pWindows[ usWinNum ];

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy         = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop       = xy.y;
   iLeft      = xy.x;

   xy         = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom    = xy.y-1;
   iRight     = xy.x-1;

   lb.lbStyle = BS_SOLID;
   lb.lbColor = bkColor;
   lb.lbHatch = 0;

   hBrush     = CreateBrushIndirect( &lb );

   rc.left    = iLeft ;
   rc.top     = iTop ;
   rc.right   = iRight + 1;
   rc.bottom  = iBottom + 1;

   FillRect( pWindowData->hdc, &rc, hBrush );

   DeleteObject( hBrush );

   switch ( iFormat )
   {
      case 1:
         hb_wvw_gtDrawBoxRecessed( pWindowData->byWinId, iTop+1, iLeft+1, iBottom-1, iRight-1,
                                   FALSE );
         break;
      case 2:
         break;
      case 3:
         hb_wvw_gtDrawOutline( pWindowData->byWinId, iTop, iLeft, iBottom, iRight );
         break;

      case 4:
         break;

      default:
         hb_wvw_gtDrawBoxRaised( pWindowData->byWinId, iTop+1, iLeft+1, iBottom-1, iRight-1,
                                 FALSE );
         break;
   }

   if ( bText )
   {
      ( HFONT ) SelectObject( pWindowData->hdc, GetStockObject( DEFAULT_GUI_FONT ) );

      GetTextExtentPoint32( pWindowData->hdc, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), &sz );

      iTextHeight  = sz.cy;

      xy.x = iLeft + ( ( iRight - iLeft + 1 ) / 2 ) ;

      if ( bImage )
      {
         xy.y = ( iBottom - 2 - iTextHeight );
      }
      else
      {

         xy.y = iTop + ( ( iBottom - iTop + 1 - iTextHeight ) / 2 ) ;
      }

      if ( iFormat == 1 )
      {
         xy.x = xy.x + 2;
         xy.y = xy.y + 2;
      }

      iAlign = TA_CENTER + TA_TOP ;

      oldTextAlign = SetTextAlign( pWindowData->hdc, iAlign );
      oldBkMode    = SetBkMode( pWindowData->hdc, TRANSPARENT );
      oldTextColor = SetTextColor( pWindowData->hdc, textColor );

      ExtTextOut( pWindowData->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), NULL );

      SetTextColor( pWindowData->hdc, oldTextColor );
      SetBkMode( pWindowData->hdc, oldBkMode );
      SetTextAlign( pWindowData->hdc, oldTextAlign );
   }
   else
   {
      iTextHeight = -1;
   }

   if ( bImage )
   {
      iImageWidth = ( iRight - iLeft + 1 - 8 );

      iImageHeight = ( iBottom - iTop + 1 - 8 - iTextHeight );

      if ( ISNUM( 7 ) )
      {
         iPicture = s_sApp.iPicture[ hb_parni( 7 ) - 1 ];

         hb_wvw_gtRenderPicture( usWinNum, iLeft+4, iTop+4, iImageWidth, iImageHeight, iPicture );
      }
      else
      {

         hb_wvw_gtDrawImage( usWinNum, iLeft+4, iTop+4, iImageWidth, iImageHeight, hb_parcx( 7 ) );
      }
   }

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/

/* WVW_DrawStatusbar() is meant for WVT compatibility only.
   WVW_SBxxxx() functions are recommended instead.
 */

HB_FUNC( WVW_DRAWSTATUSBAR )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int   iPanels   = hb_parni( 2 );
   int   i, iNext;
   int   iTop, iLeft, iBottom, iRight;
   POINT xy = { 0 };
   USHORT   usTop    ,
            usLeft   ,
            usBottom ,
            usRight  ;

   iNext = 0;

   for ( i = 0; i < iPanels; i++ )
   {
      usTop    = hb_parni( 3, iNext+1 );
      usLeft   = hb_parni( 3, iNext+2 );
      usBottom = hb_parni( 3, iNext+3 );
      usRight  = hb_parni( 3, iNext+4 );

      if (s_bMainCoordMode)
      {
        hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
      }

      xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
      iTop    = xy.y;
      iLeft   = xy.x + 1;

      xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight , usBottom + 1 );

      xy.y   -= pWindowData->byLineSpacing;

      iBottom = xy.y - 1;
      iRight  = xy.x - 2;

      SelectObject( pWindowData->hdc, s_sApp.penWhite );

      MoveToEx( pWindowData->hdc, iRight, iTop, NULL );            /* Right  */
      LineTo( pWindowData->hdc, iRight, iBottom );

      MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );          /* Bottom */
      LineTo( pWindowData->hdc, iRight, iBottom );

      SelectObject( pWindowData->hdc, s_sApp.penDarkGray );

      MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );             /* Left   */
      LineTo( pWindowData->hdc, iLeft, iBottom );

      MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );             /* Top    */
      LineTo( pWindowData->hdc, iRight, iTop );

      iNext = iNext + 4;
   }

   usTop    = hb_parni( 3, (4 * iPanels)-1 );
   usLeft   = hb_parni( 3, 4 * iPanels );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, NULL, NULL);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft , usTop + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iTop    = xy.y - 2;
   iLeft   = xy.x - 2;
   iBottom = iTop;
   iRight  = iLeft;

   SelectObject( pWindowData->hdc, s_sApp.penBlack );

   MoveToEx( pWindowData->hdc, iLeft-4, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop-4 );
   MoveToEx( pWindowData->hdc, iLeft-7, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop-7 );
   MoveToEx( pWindowData->hdc, iLeft-10, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop-10 );

   SelectObject( pWindowData->hdc, s_sApp.penWhite );

   MoveToEx( pWindowData->hdc, iLeft-5, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop-5 );
   MoveToEx( pWindowData->hdc, iLeft-8, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop-8 );
   MoveToEx( pWindowData->hdc, iLeft-11, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop-11 );
}

/*-------------------------------------------------------------------*/
/*                    End of Drawing Primitives                      */
/*-------------------------------------------------------------------*/
/*                                                                   */
/*              Utility Functions . A Natural Extension              */
/*                copied and modified from gtwvt                     */

/*-------------------------------------------------------------------*/
/*                                                                      */
/*     Wvw_ChooseFont( cFontName, nHeight, nWidth, nWeight, nQuality, ; */
/*                                    lItalic, lUnderline, lStrikeout ) */
/*                                                                      */

HB_FUNC( WVW_CHOOSEFONT )
{

   CHOOSEFONT  cf = { 0 };
   LOGFONT     lf = { 0 };
   LONG        PointSize = 0;

   if ( ! ISNIL( 2 ) )
   {
      PointSize = -MulDiv( ( LONG ) hb_parnl( 2 ), GetDeviceCaps( s_pWindows[ s_usNumWindows-1 ]->hdc, LOGPIXELSY ), 72 ) ;
   }

   lf.lfHeight         = PointSize;
   lf.lfWidth          = ISNIL( 3 ) ? 0 : hb_parni( 3 );
   lf.lfWeight         = ISNIL( 4 ) ? 0 : hb_parni( 4 );
   lf.lfItalic         = ISNIL( 6 ) ? 0 : hb_parl( 6 );
   lf.lfUnderline      = ISNIL( 7 ) ? 0 : hb_parl( 7 );
   lf.lfStrikeOut      = ISNIL( 8 ) ? 0 : hb_parl( 8 );
   lf.lfCharSet        = DEFAULT_CHARSET;
   lf.lfQuality        = ISNIL( 5 ) ? DEFAULT_QUALITY : hb_parni( 5 );
   lf.lfPitchAndFamily = FF_DONTCARE;
   if ( ISCHAR( 1 ) )
   {
      strcpy( lf.lfFaceName, hb_parcx( 1 ) );
   }

   cf.lStructSize      = sizeof( CHOOSEFONT );
   cf.hwndOwner        = s_pWindows[ s_usNumWindows-1 ]->hWnd;
   cf.hDC              = ( HDC ) NULL;
   cf.lpLogFont        = &lf;
   cf.iPointSize       = 0;
   cf.Flags            = CF_SCREENFONTS | CF_EFFECTS | CF_SHOWHELP | CF_INITTOLOGFONTSTRUCT ;
   cf.rgbColors        = RGB( 0,0,0 );
   cf.lCustData        = 0L;
   cf.lpfnHook         = ( LPCFHOOKPROC ) NULL;
   cf.lpTemplateName   = ( LPSTR ) NULL;
   cf.hInstance        = ( HINSTANCE ) NULL;
   cf.lpszStyle        = ( LPSTR ) NULL;
   cf.nFontType        = SCREEN_FONTTYPE;
   cf.nSizeMin         = 0;
   cf.nSizeMax         = 0;

   if ( ChooseFont( &cf ) )
   {
      PointSize = -MulDiv( lf.lfHeight, 72, GetDeviceCaps( s_pWindows[ s_usNumWindows-1 ]->hdc, LOGPIXELSY ) ) ;

      hb_reta( 8 );

      hb_storc( lf.lfFaceName      , -1, 1 );
      hb_stornl( ( LONG ) PointSize, -1, 2 );
      hb_storni( lf.lfWidth        , -1, 3 );
      hb_storni( lf.lfWeight       , -1, 4 );
      hb_storni( lf.lfQuality      , -1, 5 );
      hb_storl( lf.lfItalic        , -1, 6 );
      hb_storl( lf.lfUnderline     , -1, 7 );
      hb_storl( lf.lfStrikeOut     , -1, 8 );
   }
   else
   {
      hb_reta( 8 );

      hb_storc( ""         , -1, 1 );
      hb_stornl( ( LONG ) 0, -1, 2 );
      hb_storni( 0         , -1, 3 );
      hb_storni( 0         , -1, 4 );
      hb_storni( 0         , -1, 5 );
      hb_storl( 0          , -1, 6 );
      hb_storl( 0          , -1, 7 );
      hb_storl( 0          , -1, 8 );
   }

   return ;
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*    Wvw_ChooseColor( nRGBInit, aRGB16, nFlags ) => nRGBSelected    */
/*                                                                   */

HB_FUNC( WVW_CHOOSECOLOR )
{

   CHOOSECOLOR cc ;
   COLORREF    crCustClr[ 16 ] ;
   int         i ;

   for( i = 0 ; i < 16 ; i++ )
   {

     crCustClr[ i ] = ( ISARRAY( 2 ) ? (COLORREF) hb_parnl( 2, i+1 ) : GetSysColor( COLOR_BTNFACE ) ) ;
   }

   cc.lStructSize    = sizeof( CHOOSECOLOR ) ;
   cc.hwndOwner      = s_pWindows[ s_usNumWindows-1 ]->hWnd ;
   cc.rgbResult      = ISNIL( 1 ) ?  0 : ( COLORREF ) hb_parnl( 1 ) ;
   cc.lpCustColors   = crCustClr ;

   cc.Flags         = ( WORD ) ( ISNIL( 3 ) ? CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN | CC_SHOWHELP : hb_parnl( 3 ) );

   if ( ChooseColor( &cc ) )
   {
      hb_retnl( cc.rgbResult ) ;
   }
   else
   {
      hb_retnl( 0 );
   }
}

/*-------------------------------------------------------------------*/

/*WVW_SETMOUSEPOS( nWinNum, nRow, nCol ) nWinNum is 0 based        */
/*WHAT'S the difference with GT_FUNC( mouse_SetPos ) ???           */
/*this func is able to position cursor on any window               */

/*NOTE: consider using 'standard' SETMOUSE() instead:     */
/*      SETMOUSE(.t., nRow, nCol)                                  */
/*      This will treat (nRow,nCol) according to current s_bMainCoordMode setting */

HB_FUNC( WVW_SETMOUSEPOS )
{
   POINT xy = { 0 };
   USHORT usWinNum = WVW_WHICH_WINDOW;
   USHORT   usRow    = hb_parni( 2 ),
            usCol   = hb_parni( 3 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usRow, &usCol, NULL, NULL);
   }

   xy = hb_wvw_gtGetXYFromColRow( s_pWindows[ usWinNum ], usCol, usRow );

   if ( ClientToScreen( s_pWindows[usWinNum]->hWnd, &xy ) )
   {
      hb_retl( SetCursorPos( xy.x, xy.y + ( s_pWindows[usWinNum]->PTEXTSIZE.y / 2 ) ) );
   }
   else
   {
      hb_retl( FALSE );
   }
}

/*-------------------------------------------------------------------*/

/*WVW_GetPaintRect( nWinNum )   nWinNum is 0 based               */
/*returns array of paint pending rect {top, left, bottom, right} */
/*WARNING:                                                       */
/*unlike WVT, top maybe > bottom                                 */
/*            left maybe > right                                 */
/*in these cases, no paint request is pending                    */
/*(in WVT these is reflected in {0,0,0,0})                       */
HB_FUNC( WVW_GETPAINTRECT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   RECT   rPaintRect = s_pWindows[usWinNum]->rPaintPending;
   HB_ITEM  info;
   HB_ITEM  temp;

   info.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &info, 4 );

   hb_arraySetForward( &info, 1, hb_itemPutNI( &temp, rPaintRect.top ) );
   hb_arraySetForward( &info, 2, hb_itemPutNI( &temp, rPaintRect.left ) );
   hb_arraySetForward( &info, 3, hb_itemPutNI( &temp, rPaintRect.bottom  ) );
   hb_arraySetForward( &info, 4, hb_itemPutNI( &temp, rPaintRect.right  ) );

   hb_itemReturn( &info );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETPOINTER )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   int     iCursor = hb_parni( 2 );
   HCURSOR hCursor;

   switch ( iCursor )
   {
   case 1:
      hCursor = LoadCursor( NULL, IDC_ARROW    );
      break;

   case 2:
      hCursor = LoadCursor( NULL, IDC_IBEAM    );
      break;

   case 3:
      hCursor = LoadCursor( NULL, IDC_WAIT     );
      break;

   case 4:
      hCursor = LoadCursor( NULL, IDC_CROSS    );
      break;

   case 5:
      hCursor = LoadCursor( NULL, IDC_UPARROW  );
      break;

   case 6:
      hCursor = LoadCursor( NULL, IDC_SIZE     );
      break;

   case 7:
      hCursor = LoadCursor( NULL, IDC_ICON     );
      break;

   case 8:
      hCursor = LoadCursor( NULL, IDC_SIZENWSE );
      break;

   case 9:
      hCursor = LoadCursor( NULL, IDC_SIZENESW );
      break;

   case 10:
      hCursor = LoadCursor( NULL, IDC_SIZEWE   );
      break;

   case 11:
      hCursor = LoadCursor( NULL, IDC_SIZENS   );
      break;

   case 12:
      hCursor = LoadCursor( NULL, IDC_SIZEALL  );
      break;

   case 13:
      hCursor = LoadCursor( NULL, IDC_NO       );
      break;

   case 14:
      hCursor = LoadCursor( NULL, IDC_HAND     );
      break;

   case 15:
      hCursor = LoadCursor( NULL, IDC_APPSTARTING );
      break;

   case 16:
      hCursor = LoadCursor( NULL, IDC_HELP     );
      break;

   default:
      hCursor = LoadCursor( NULL, IDC_ARROW    );
      break;
   }

   SetClassLong( s_pWindows[usWinNum]->hWnd, GCL_HCURSOR, ( DWORD ) hCursor );
}

/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/*                                                                   */
/*   Wvw_LoadPicture( nSlot, cFilePic )                              */
/*                                                                   */
HB_FUNC( WVW_LOADPICTURE )
{

   IPicture * iPicture = hb_wvw_gtLoadPicture( hb_parcx( 2 ) );
   BOOL       bResult  = FALSE;
   int        iSlot    = hb_parni( 1 ) - 1 ;

   if ( iPicture )
   {
      if ( s_sApp.iPicture[ iSlot ] )
      {
         hb_wvw_gtDestroyPicture( s_sApp.iPicture[ iSlot ] );
      }

      s_sApp.iPicture[ iSlot ] = iPicture;
      bResult = TRUE;
   }
   hb_retl( bResult );
}

/*-------------------------------------------------------------------*/
/*                                                                                                */
/* Wvw_LoadFont( nSlotFont, cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout, */
/*               nCharSet, nQuality, nEscapement )                                                */
/*                                                                                                */
HB_FUNC( WVW_LOADFONT )
{
   USHORT usWinNum = s_usNumWindows-1;

   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   LOGFONT  logfont;
   int      iSlot = hb_parni( 1 ) - 1;
   HFONT    hFont;

   logfont.lfEscapement     = ( ISNIL( 11 ) ? 0 : ( hb_parni( 11 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( ISNIL(  5 ) ? 0 : hb_parni( 5 ) );
   logfont.lfItalic         = ( ISNIL(  6 ) ? 0 : hb_parl(  6 ) );
   logfont.lfUnderline      = ( ISNIL(  7 ) ? 0 : hb_parl(  7 ) );
   logfont.lfStrikeOut      = ( ISNIL(  8 ) ? 0 : hb_parl(  8 ) );
   logfont.lfCharSet        = ( ISNIL(  9 ) ? pWindowData->CodePage : hb_parni(  9 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( ISNIL( 10 ) ? DEFAULT_QUALITY : hb_parni( 10 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( ISNIL(  3 ) ? pWindowData->fontHeight : hb_parni( 3 ) );
   logfont.lfWidth          = ( ISNIL(  4 ) ? ( pWindowData->fontWidth < 0 ? -pWindowData->fontWidth : pWindowData->fontWidth ) : hb_parni( 4 ) );

   strcpy( logfont.lfFaceName, ( ISNIL( 2 ) ? pWindowData->fontFace : hb_parcx( 2 ) ) );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      if ( s_sApp.hUserFonts[ iSlot ] )
      {
         DeleteObject( s_sApp.hUserFonts[ iSlot ] );
      }
      s_sApp.hUserFonts[ iSlot ] = hFont;
   }
}

/*-------------------------------------------------------------------*/
/*                                                                   */
/*  Wvw_LoadPen( nSlot, nStyle, nWidth, nRGBColor )                  */
/*                                                                   */
HB_FUNC( WVW_LOADPEN )
{

   int      iPenWidth, iPenStyle;
   COLORREF crColor;
   HPEN     hPen;
   int      iSlot = hb_parni( 1 ) - 1;

   iPenStyle = ISNIL( 2 ) ? 0 : hb_parni( 2 );
   iPenWidth = ISNIL( 3 ) ? 0 : hb_parni( 3 );
   crColor   = ISNIL( 4 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 4 );

   hPen      = CreatePen( iPenStyle, iPenWidth, crColor );

   if ( hPen )
   {
      if ( s_sApp.hUserPens[ iSlot ] )
      {
         DeleteObject( s_sApp.hUserPens[ iSlot ] );
      }
      s_sApp.hUserPens[ iSlot ] = hPen;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

/*-------------------------------------------------------------------*/
/*                                                                                        */
/*  Wvw_DrawPicture( [nWinNum], nTop, nLeft, nBottom, nRight, nSlot, lTight/aAdj ) -> lOk */
/*  nSlot <= 20  aAdj == { 0,0,-2,-2 } To Adjust the pixels for { Top,Left,Bottom,Right } */
/*                                                                                        */

HB_FUNC( WVW_DRAWPICTURE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   POINT    xy = { 0 };
   int      iTop, iLeft, iBottom, iRight;

   int      iSlot   = hb_parni( 6 ) - 1;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   BOOL  bTight = ( ISARRAY(7) || ISNIL( 7 ) ? FALSE : hb_parl( 7 ) );  /* <-- none in gtwvt */
   BOOL  bUseArray = ISARRAY(7);
   int   iOLeft, iOTop, iORight, iOBottom;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   if (bTight)
   {
     iOTop   = 2+1;
     iOLeft  = 2+1;
     iOBottom= -1;
     iORight = -1;
   }
   else if (bUseArray)
   {
     iOTop   = hb_parni( 7,1 );
     iOLeft  = hb_parni( 7,2 );
     iOBottom= hb_parni( 7,3 );
     iORight = hb_parni( 7,4 );
   }
   else
   {
     iOTop = iOLeft = iOBottom = iORight = 0;
   }

   if ( iSlot < WVW_PICTURES_MAX )
   {
      if ( s_sApp.iPicture[ iSlot ] )
      {
         xy       = hb_wvw_gtGetXYFromColRow( s_pWindows[usWinNum], usLeft, usTop );
         iTop     = xy.y + iOTop;
         iLeft    = xy.x + iOLeft;

         xy       = hb_wvw_gtGetXYFromColRow( s_pWindows[usWinNum], usRight + 1, usBottom + 1 );
         iBottom  = xy.y-1 + iOBottom;
         iRight   = xy.x-1 + iORight;

         hb_retl( hb_wvw_gtRenderPicture( usWinNum, iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, s_sApp.iPicture[ iSlot ] ) );

      }
   }

}

/*-------------------------------------------------------------------*/
/*                                                                                              */
/*    WVW_DRAWLABELEX( [nWinNum], nRow, nCol, cLabel, nAlign, nTextColor, nBkColor, nSlotFont ) */
/*                                                                                              */

HB_FUNC( WVW_DRAWLABELEX )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT    xy = { 0 };
   HFONT    oldFont;
   int      oldTextAlign;
   COLORREF oldBkColor, oldTextColor;
   int      iSlot = hb_parni( 8 ) - 1;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, NULL, NULL);
   }

   if ( s_sApp.hUserFonts[ iSlot ] )
   {
      xy           = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );

      oldBkColor   = SetBkColor( pWindowData->hdc, ISNIL( 7 ) ? pWindowData->background : ( COLORREF ) hb_parnl( 7 ) );
      oldTextColor = SetTextColor( pWindowData->hdc, ISNIL( 6 ) ? pWindowData->foreground : ( COLORREF ) hb_parnl( 6 ) );
      oldTextAlign = SetTextAlign( pWindowData->hdc, ( ISNIL( 5 ) ? TA_LEFT : hb_parni( 5 ) ) );
      oldFont      = ( HFONT ) SelectObject( pWindowData->hdc, s_sApp.hUserFonts[ iSlot ] );

      ExtTextOut( pWindowData->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 4 ), strlen( hb_parcx( 4 ) ), NULL );

      SelectObject( pWindowData->hdc, oldFont );
      SetTextAlign( pWindowData->hdc, oldTextAlign );
      SetBkColor( pWindowData->hdc, oldBkColor );
      SetTextColor( pWindowData->hdc, oldTextColor );

      hb_retl( TRUE );
   }

   hb_retl( FALSE );
}

/*-------------------------------------------------------------------*/
/*                        1      2       3       4        5        6       7       8      9        */
/*   Wvw_DrawLineEx( [nWinNum], nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nSlotPen ) */
/*                                                                                                 */

HB_FUNC( WVW_DRAWLINEEX )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT    xy = { 0 };
   int      iTop, iLeft, iBottom, iRight, iOffset ;
   int      iOrient, iFormat, iAlign ;
   int      x, y;
   HPEN     hPen;
   int      iSlot = hb_parni( 9 ) - 1;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   iOrient = ISNIL( 6 ) ? 0 : hb_parni( 6 );
   iFormat = ISNIL( 7 ) ? 0 : hb_parni( 7 );
   iAlign  = ISNIL( 8 ) ? 0 : hb_parni( 8 );

   x       = iLeft ;
   y       = iTop ;

   switch ( iAlign )
   {
      case 0:                  /* Center       */
      {
         if ( iOrient == 0 )   /* Horizontal   */
         {
            iOffset = ( ( iBottom - iTop ) / 2 ) ;
            y       = iTop + iOffset ;
         }
         else
         {
            iOffset = ( ( iRight - iLeft ) / 2 ) ;
            x       = iLeft + iOffset ;
         }
      }
      break;

      case 1:                  /* Top     */
      break;

      case 2:                  /* bottom  */
      {
         if ( iFormat == 0 || iFormat == 1 )  /* Raised/Recessd */
         {
            y = iBottom - 1;
         }
         else
         {
            y = iBottom;
         }
      }
      break;

      case 3:                  /* Left    */
      break;

      case 4:                  /* Right   */
      {
         if ( iFormat == 0 || iFormat == 1 )  /* Raised/Recessd */
         {
            x = iRight - 1;
         }
         else
         {
            x = iRight;
         }
      }
      break;
   }

   hPen = s_sApp.hUserPens[ iSlot ];

   switch ( iFormat )
   {
      case 0:                                       /* Raised       */
      {
         if ( iOrient == 0 )                        /*  Horizontal  */
         {
            SelectObject( pWindowData->hdc, s_sApp.penWhite );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y+1, NULL );
            LineTo( pWindowData->hdc, iRight, y+1 );
         }
         else                                       /*  Vertical    */
         {
            SelectObject( pWindowData->hdc, s_sApp.penWhite );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x+1, y, NULL );
            LineTo( pWindowData->hdc, x+1, iBottom );
         }
      }
      break;

      case 1:                                      /* Recessed    */
      {
         if ( iOrient == 0 )                       /* Horizontal  */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
            SelectObject( pWindowData->hdc, s_sApp.penWhite );
            MoveToEx( pWindowData->hdc, x, y+1, NULL );
            LineTo( pWindowData->hdc, iRight, y+1 );
         }
         else                                      /*  Vertical   */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
            SelectObject( pWindowData->hdc, s_sApp.penWhite );
            MoveToEx( pWindowData->hdc, x+1, y, NULL );
            LineTo( pWindowData->hdc, x+1, iBottom );
         }
      }
      break;

      case 2:                                      /* Plain      */
      {
         if ( iOrient == 0 )                       /* Horizontal */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
         }
         else                                      /*  Vertical  */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
         }
       }
      break;
   }

   hb_retl( TRUE );
}

/*-------------------------------------------------------------------*/
/*                                                                           */
/*    Wvw_DrawOutlineEx( [nWinNum], nTop, nLeft, nBottom, nRight, nSlotPen ) */
/*                                                                           */

HB_FUNC( WVW_DRAWOUTLINEEX )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iSlot = hb_parni( 6 ) - 1;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight+1, usBottom+1 );
   iBottom = xy.y;
   iRight  = xy.x;

   if ( s_sApp.hUserPens[ iSlot ] )
   {
      SelectObject( pWindowData->hdc, s_sApp.hUserPens[ iSlot ] );
   }
   else
   {
      SelectObject( pWindowData->hdc, s_sApp.penBlack );
   }

   hb_wvw_gtDrawOutline( usWinNum, iTop, iLeft, iBottom, iRight );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_MESSAGEBOX )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   hb_retni( MessageBox( s_pWindows[usWinNum]->hWnd, hb_parcx( 2 ), hb_parcx( 3 ), ISNIL( 4 ) ? MB_OK : hb_parni( 4 ) ) );
}

#if _WIN32_IE > 0x400

/*-------------------------------------------------------------------*/
/*                                                                   */
/*                              Tooltips                             */
/*                                                                   */
/*-------------------------------------------------------------------*/

/*WVW_SetToolTopActive([nWinNum], [lToggle]) */
HB_FUNC( WVW_SETTOOLTIPACTIVE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   BOOL        bActive = pWindowData->bToolTipActive;

   if ( ! ISNIL( 2 ) )
   {

      if ( hb_parl(2) && (pWindowData->hWndTT==NULL) )
      {
        hb_wvw_gtCreateToolTipWindow(pWindowData);
      }

      pWindowData->bToolTipActive = hb_parl( 2 );
   }

   hb_retl( bActive );
}

/*-------------------------------------------------------------------*/
/*                                                                        */
/*   Wvw_SetToolTip( [nWinNum], nTop, nLeft, nBottom, nRight, cToolText ) */
/*                                                                        */
HB_FUNC( WVW_SETTOOLTIP )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   TOOLINFO ti = { 0 };
   POINT    xy = { 0 };
   int      iTop, iLeft, iBottom, iRight;

   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   if ( ! pWindowData->bToolTipActive )
   {
      return;
   }

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   ti.cbSize    = sizeof( TOOLINFO );
   ti.hwnd      = pWindowData->hWnd;
   ti.uId       = WVW_ID_BASE_TOOLTIP+usWinNum;

   if ( SendMessage( pWindowData->hWndTT, TTM_GETTOOLINFO, 0, ( LPARAM ) &ti ) )
   {
      xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
      iTop    = xy.y;
      iLeft   = xy.x;

      xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight+1, usBottom+1 );
      iBottom = xy.y - 1;
      iRight  = xy.x - 1;

      ti.lpszText    = hb_parc( 6 );
      ti.rect.left   = iLeft;
      ti.rect.top    = iTop;
      ti.rect.right  = iRight;
      ti.rect.bottom = iBottom;

      SendMessage( pWindowData->hWndTT, TTM_SETTOOLINFO, 0, ( LPARAM ) &ti );
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETTOOLTIPTEXT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   TOOLINFO ti;

   ti.cbSize = sizeof( TOOLINFO );
   ti.hwnd   = pWindowData->hWnd;
   ti.uId    = 100000;

   if ( SendMessage( pWindowData->hWndTT, TTM_GETTOOLINFO, 0, ( LPARAM ) &ti ) )
   {
      ti.lpszText = hb_parcx( 2 );
      SendMessage( pWindowData->hWndTT, TTM_UPDATETIPTEXT, 0, ( LPARAM ) &ti );
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETTOOLTIPMARGIN )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   RECT rc = { 0 };

   rc.left   = hb_parni( 3 );
   rc.top    = hb_parni( 2 );
   rc.right  = hb_parni( 5 );
   rc.bottom = hb_parni( 4 );

   SendMessage( pWindowData->hWndTT, TTM_SETMARGIN, 0, ( LPARAM ) &rc );
}

HB_FUNC( WVW_SETTOOLTIPWIDTH )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   int iTipWidth = SendMessage( pWindowData->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 );

   if ( ISNUM( 2 ) )
   {
      SendMessage( pWindowData->hWndTT, TTM_SETMAXTIPWIDTH, 0, ( LPARAM ) ( int ) hb_parni( 2 ) );
   }

   hb_retni( iTipWidth );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETTOOLTIPBKCOLOR )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   COLORREF cr = SendMessage( pWindowData->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 );

   if ( ISNUM( 2 ) )
   {
      SendMessage( pWindowData->hWndTT, TTM_SETTIPBKCOLOR, ( WPARAM ) ( COLORREF ) hb_parnl( 2 ), 0 );
   }
   hb_retnl( ( COLORREF ) cr );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETTOOLTIPTEXTCOLOR )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   COLORREF cr = SendMessage( pWindowData->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 );

   if ( ISNUM( 2 ) )
   {
      SendMessage( pWindowData->hWndTT, TTM_SETTIPTEXTCOLOR, ( WPARAM ) ( COLORREF ) hb_parnl( 2 ), 0 );
   }
   hb_retnl( ( COLORREF ) cr );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_SETTOOLTIPTITLE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int iIcon;

   if ( ! ISNIL( 3 ) )
   {
      iIcon = ISNIL( 2 ) ? 0 : hb_parni( 2 );
      if ( iIcon > 3 )
      {
         iIcon = 0 ;
      }
      SendMessage( pWindowData->hWndTT, TTM_SETTITLE, ( WPARAM ) iIcon, ( LPARAM ) hb_parcx( 3 ) );
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETTOOLTIPWIDTH )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   hb_retni( SendMessage( pWindowData->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETTOOLTIPBKCOLOR )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   hb_retnl( ( COLORREF ) SendMessage( pWindowData->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_GETTOOLTIPTEXTCOLOR )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   hb_retnl( ( COLORREF ) SendMessage( pWindowData->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 ) );
}

#endif

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*                                                                   */
/*               Miscellaneous xHarbour callable functions           */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* STATUS BAR                                                        */
/*-------------------------------------------------------------------*/

/*WVW_SBcreate( [nWinNum] )
 *create status bar for window nWinNum, with one part.
 *returns handle to status bar of windows nWinNum
 *returns 0 if failed, eg. if there is already a status bar for this window
 */
HB_FUNC( WVW_SBCREATE)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND hWndParent;
   HWND hWndSB;
   int  ptArray[WVW_MAX_STATUS_PARTS];

   if (!(pWindowData->hStatusBar==NULL))
   {
     hb_retnl( 0 );
     return;
   }

   hWndParent = pWindowData->hWnd;
   hWndSB = CreateStatusWindow(WS_CHILD | WS_VISIBLE | WS_BORDER | SBT_TOOLTIPS ,
                                      NULL,
                                      hWndParent,
                                      WVW_ID_BASE_STATUSBAR+usWinNum);
   if(hWndSB)
   {

     RECT rSB = { 0 };
     if (GetClientRect(hWndSB, &rSB))
     {
       pWindowData->usSBHeight = rSB.bottom;
     }
     pWindowData->hStatusBar = hWndSB;

     hb_wvw_gtResetWindow( usWinNum );

     ptArray[0] = rSB.right;

     SendMessage(hWndSB, SB_SETPARTS, 1, (LPARAM)(LPINT)ptArray);

   }

   hb_retnl ( (LONG) hWndSB );
}

/*WVW_SBdestroy( [nWinNum] )
 *destroy status bar for window nWinNum
 */
HB_FUNC( WVW_SBDESTROY)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   if (!(pWindowData->hStatusBar==NULL))
   {
     DestroyWindow( pWindowData->hStatusBar );
     pWindowData->hStatusBar = NULL;
     pWindowData->usSBHeight = 0;

     hb_wvw_gtResetWindow( usWinNum );
   }
}

/*WVW_SBaddPart(nWinNum, cMaxText, nWidth, nStyle, lResetParts, [cIcon , cToolTip])
 *ps.
 *lResetParts==.t. :: remove all previously created parts
 *nStyle: 0 (default), 0x0200 (SBT_POPOUT), 0x0100 (SBT_NOBORDERS)
 *nWidth: expected width in pixels
 *NOTE: if cMaxText is passed, nWidth is ignored. width of cMaxText will be used instead
 *NOTE: the leftmost part will eventually have width of remaining spaces
 *NOTE: cIcon and cToolTip does not work currently
 *
 *returns number of parts
 *returns 0 if failed
 */
HB_FUNC (WVW_SBADDPART)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND  hWndSB;
   int   ptArray[WVW_MAX_STATUS_PARTS];
   int   numOfParts;
   int   n ;
   RECT  rSB = { 0 };
   WORD  displayFlags;
   HICON hIcon;
   BOOL  lResetParts;
   USHORT usWidth;

   hWndSB = pWindowData->hStatusBar;
   if (hWndSB==NULL)
   {
     hb_retnl( 0 );
     return;
   }

   displayFlags = ISNIL(4) ? 0 : (WORD) hb_parnl(4);
   lResetParts  = !ISNIL(5) && hb_parl(5);
   usWidth      = ISNIL(3) || hb_parni(3) <= 0 ? 5*WVW_SPACE_BETWEEN_PARTS : (USHORT) hb_parni(3);

   if (ISCHAR(2))
   {
     HDC   hDCSB = GetDC(hWndSB);
     SIZE  size = { 0 };

     HFONT hFont = (HFONT) SendMessage( hWndSB, WM_GETFONT, (WPARAM) 0, (LPARAM) 0);
     HFONT hOldFont  = ( HFONT ) SelectObject( hDCSB, hFont );

     if (GetTextExtentPoint32(hDCSB, hb_parcx(2), hb_parclen(2)+1, &size))
     {
       usWidth = size.cx;
     }

     SelectObject( hDCSB, hOldFont );

     ReleaseDC(hWndSB, hDCSB);
   }

   if ( !lResetParts )
   {

    numOfParts = SendMessage(hWndSB,SB_GETPARTS, WVW_MAX_STATUS_PARTS , (LPARAM)(LPINT)ptArray);
   }
   else
   {
     numOfParts = 0;
   }
   numOfParts ++ ;

   GetClientRect(hWndSB, &rSB);

   ptArray[numOfParts-1] = rSB.right;
   if (!lResetParts)
   {
     for ( n=0; n < numOfParts-1; n++)
     {
       ptArray[n] -=  (usWidth + WVW_SPACE_BETWEEN_PARTS);
    }
   }

   SendMessage(hWndSB,  SB_SETPARTS, numOfParts,(LPARAM)(LPINT)ptArray);

   if (!ISNIL(6))
   {
     int cy = rSB.bottom - rSB.top - 4;
     int cx = cy;

     hIcon = (HICON) LoadImage(0, hb_parcx(6), IMAGE_ICON, cx, cy, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT | LR_DEFAULTSIZE);

     if (hIcon==NULL)
     {
       hIcon = (HICON) LoadImage(GetModuleHandle(NULL), hb_parcx(6), IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR | LR_DEFAULTSIZE);
     }

     if (!(hIcon==NULL))
     {
       SendMessage(hWndSB, SB_SETICON, (WPARAM) numOfParts-1, (LPARAM) hIcon);
     }
   }

   SendMessage(hWndSB, SB_SETTEXT, (numOfParts-1) | displayFlags, (LPARAM) NULL);
   if (!ISNIL(7))
   {

     SendMessage(hWndSB, SB_SETTIPTEXT, (WPARAM) (numOfParts-1), (LPARAM) hb_parcx(7));
   }

   hb_retni( numOfParts );
}

/*WVW_SBrefresh(nWinNum)
 *reinitialize StatusBar's parts, eg. after window resize
 *TODO: do it automatically, after hb_wvw_gtResetWindowSize()
 *returns number of parts
 *returns 0 if failed
 */
HB_FUNC (WVW_SBREFRESH)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND  hWndSB;
   int   ptArray[WVW_MAX_STATUS_PARTS];
   int   numOfParts;
   int   n ;
   int   iDiff;
   RECT  rSB = { 0 };

   hWndSB = pWindowData->hStatusBar;
   if (hWndSB==NULL)
   {
     hb_retnl( 0 );
     return;
   }

   numOfParts = SendMessage(hWndSB,SB_GETPARTS, WVW_MAX_STATUS_PARTS , (LPARAM)(LPINT)ptArray);
   if (numOfParts==0)
   {
     hb_retnl( 0 );
     return;
   }

   GetClientRect(hWndSB, &rSB);
   iDiff = rSB.right - ptArray[numOfParts-1];

   for ( n=0; n <= numOfParts-1; n++)
   {
       ptArray[n] += iDiff;
   }

   SendMessage(hWndSB,  SB_SETPARTS, numOfParts,(LPARAM)(LPINT)ptArray);

   hb_retni( numOfParts );
}

/*WVW_SBsetText([nWinNum], [nPart], cText)
 *Set Text of status bar's part #npart
 */
HB_FUNC (WVW_SBSETTEXT)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int iPart = ISNIL( 2 ) ? 1 : hb_parni( 2 );
   char cString [1024] = "";
   int displayFlags = (int) HIWORD(SendMessage( pWindowData->hStatusBar, SB_GETTEXT, (WPARAM) iPart, (LPARAM) cString));
   SendMessage( pWindowData->hStatusBar, SB_SETTEXT, iPart | displayFlags, (LPARAM) hb_parcx(3));
}

/*WVW_SBgetText([nWinNum], [nPart])
 *Get Text of status bar's part #npart
 */
HB_FUNC (WVW_SBGETTEXT)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int iPart = ISNIL( 2 ) ? 1 : hb_parni( 2 );
   char cString [1024] = "";
   SendMessage( pWindowData->hStatusBar, SB_GETTEXT, (WPARAM) iPart, (LPARAM) cString);
   hb_retc(cString) ;
}

/*WVW_SBgetparts([nWinNum])
 *Get number of parts in statusbar of window nWinNum
 */
HB_FUNC (WVW_SBGETPARTS)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int numOfParts = (int) SendMessage( pWindowData->hStatusBar, SB_GETPARTS, WVW_MAX_STATUS_PARTS, 0 );

   hb_retni( numOfParts );
}

/*-------------------------------------------------------------------*/
/* TIMER                                                             */
/*-------------------------------------------------------------------*/

/*WVW_SetTimer([nWinNum], nInterval)
 *set timer event for every nInterval millisec
 *(effective only if WVW_TIMER() function exists)
 *eg. it can be usefull to update clock on status bar
 *returns .t. if successfull
 */
/*20040602: WARNING: WVT is slightly different*/
HB_FUNC (WVW_SETTIMER)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   if ( s_sApp.pSymWVW_TIMER )
   {
     SetTimer( pWindowData->hWnd, WVW_ID_BASE_TIMER+usWinNum, (UINT) hb_parni(2), NULL );

     hb_retl( TRUE );
   }
   else
   {
     hb_retl( FALSE );
   }
}

/*WVW_KillTimer([nWinNum])
 *kill the timer event handler for window nWinNum
 *returns .t. if successfull
 */
/*20040602: WARNING: WVT is slightly different */
HB_FUNC (WVW_KILLTIMER)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   if ( s_sApp.pSymWVW_TIMER )
   {
     KillTimer( pWindowData->hWnd, WVW_ID_BASE_TIMER+usWinNum );
     hb_retl( TRUE );
   }
   else
   {
     hb_retl( FALSE );
   }
}

/*-------------------------------------------------------------------*/
/* TOOLBAR begins                                                    */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* Supporting functions                                              */
/*-------------------------------------------------------------------*/

static BITMAPINFO * PackedDibLoad (PTSTR szFileName)
{
     BITMAPFILEHEADER bmfh ;
     BITMAPINFO     * pbmi ;
     BOOL             bSuccess ;
     DWORD            dwPackedDibSize, dwBytesRead ;
     HANDLE           hFile ;

     hFile = CreateFile (szFileName, GENERIC_READ, FILE_SHARE_READ, NULL,
                         OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL) ;

     if (hFile == INVALID_HANDLE_VALUE)
          return NULL ;

     bSuccess = ReadFile (hFile, &bmfh, sizeof (BITMAPFILEHEADER),
                          &dwBytesRead, NULL) ;

     if (!bSuccess || (dwBytesRead != sizeof (BITMAPFILEHEADER))
                   || (bmfh.bfType != * (WORD *) "BM"))
     {
          CloseHandle (hFile) ;
          return NULL ;
     }

     dwPackedDibSize = bmfh.bfSize - sizeof (BITMAPFILEHEADER) ;

     pbmi = (BITMAPINFO *) hb_xgrab(dwPackedDibSize) ;

     bSuccess = ReadFile (hFile, pbmi, dwPackedDibSize, &dwBytesRead, NULL) ;
     CloseHandle (hFile) ;

     if (!bSuccess || (dwBytesRead != dwPackedDibSize))
     {

          hb_xfree (pbmi) ;
          return NULL ;
     }

     return pbmi ;
}

static int PackedDibGetWidth (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcWidth ;
     else
          return pPackedDib->bmiHeader.biWidth ;
}

static int PackedDibGetHeight (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcHeight ;
     else
          return abs (pPackedDib->bmiHeader.biHeight) ;
}

static int PackedDibGetBitCount (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcBitCount ;
     else
          return pPackedDib->bmiHeader.biBitCount ;
}

static int PackedDibGetInfoHeaderSize (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcSize ;

     else if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPINFOHEADER))
          return pPackedDib->bmiHeader.biSize +
                    (pPackedDib->bmiHeader.biCompression ==
                                        BI_BITFIELDS ? 12 : 0) ;

     else return pPackedDib->bmiHeader.biSize ;
}

static int PackedDibGetColorsUsed (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return 0 ;
     else
          return pPackedDib->bmiHeader.biClrUsed ;
}

static int PackedDibGetNumColors (BITMAPINFO * pPackedDib)
{
     int iNumColors ;

     iNumColors = PackedDibGetColorsUsed (pPackedDib) ;

     if (iNumColors == 0 && PackedDibGetBitCount (pPackedDib) < 16)
          iNumColors = 1 << PackedDibGetBitCount (pPackedDib) ;

     return iNumColors ;
}

static int PackedDibGetColorTableSize (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return PackedDibGetNumColors (pPackedDib) * sizeof (RGBTRIPLE) ;
     else
          return PackedDibGetNumColors (pPackedDib) * sizeof (RGBQUAD) ;
}

#if 0
static RGBQUAD * PackedDibGetColorTablePtr (BITMAPINFO * pPackedDib)
{
     if (PackedDibGetNumColors (pPackedDib) == 0)
          return 0 ;
     return (RGBQUAD *) (((BYTE *) pPackedDib) +
                                   PackedDibGetInfoHeaderSize (pPackedDib)) ;
}

static RGBQUAD * PackedDibGetColorTableEntry (BITMAPINFO * pPackedDib, int i)
{
     if (PackedDibGetNumColors (pPackedDib) == 0)
          return 0 ;

     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return (RGBQUAD *)
               (((RGBTRIPLE *) PackedDibGetColorTablePtr (pPackedDib)) + i) ;
     else
          return PackedDibGetColorTablePtr (pPackedDib) + i ;
}
#endif

static BYTE * PackedDibGetBitsPtr (BITMAPINFO * pPackedDib)
{
     return ((BYTE *) pPackedDib) + PackedDibGetInfoHeaderSize (pPackedDib) +
                                    PackedDibGetColorTableSize (pPackedDib) ;
}

static HBITMAP FindBitmapHandle(char * szFileName, int * piWidth, int * piHeight)
{
  BITMAP_HANDLE * pbh = s_sApp.pbhBitmapList;
  BOOL bStrictDimension = !(*piWidth==-1 && *piHeight==-1);
  while (pbh)
  {

    if (strcmp(szFileName, pbh->szFilename)==0 &&
        (!bStrictDimension ||
         (*piWidth == pbh->iWidth &&
          *piHeight== pbh->iHeight
         )
        )
       )
    {
      if (!bStrictDimension)
      {
        *piWidth = pbh->iWidth;
        *piHeight= pbh->iHeight;
      }
      return pbh->hBitmap;
    }
    pbh = pbh->pNext;
  }
  return NULL;
}

static void AddBitmapHandle(char * szFileName, HBITMAP hBitmap, int iWidth, int iHeight)
{
  BITMAP_HANDLE * pbhNew = (BITMAP_HANDLE *) hb_xgrab( sizeof( BITMAP_HANDLE ) );

  strcpy(pbhNew->szFilename, szFileName);
  pbhNew->hBitmap = hBitmap;
  pbhNew->iWidth = iWidth;
  pbhNew->iHeight = iHeight;
  pbhNew->pNext = s_sApp.pbhBitmapList;

  s_sApp.pbhBitmapList = pbhNew;
}

/* add one button to existing Toolbar */
/*
  uiBitmap is resource id
*/

static BOOL AddTBButton(HWND hWndToolbar, char * szBitmap, UINT uiBitmap, char * pszLabel, int iCommand, int iBitmapType, BOOL bMap3Dcolors, WIN_DATA * pWindowData)
{
   TBBUTTON tbb;
   TBADDBITMAP tbab;
   char szBuffer[WVW_TB_LABELMAXLENGTH+2];
   int iNewBitmap, iNewString;
   int  iOffset;
   BOOL bSuccess;
   HBITMAP hBitmap;

   if (iCommand == 0)
   {
      tbb.iBitmap = 0;
      tbb.idCommand = 0;
      tbb.fsState = TBSTATE_ENABLED;
      tbb.fsStyle = TBSTYLE_SEP;
      tbb.dwData = 0;
      tbb.iString = 0;

      bSuccess = SendMessage(hWndToolbar, TB_ADDBUTTONS, (WPARAM) 1, (LPARAM) (LPTBBUTTON) &tbb);
      return( bSuccess );
   }

   switch (iBitmapType)
   {
      case 0:
        iOffset = 0;
        break;
      case 1:
        iOffset = pWindowData->iStartStdBitmap;
        break;
      case 2:
        iOffset = pWindowData->iStartViewBitmap;
        break;
      case 3:
        iOffset = pWindowData->iStartHistBitmap;
        break;
      default:
        iOffset = 0;
        break;
   }

   if (iBitmapType==0)
   {
      int iExpWidth, iExpHeight;

      iExpWidth = pWindowData->iTBImgWidth;
      iExpHeight = pWindowData->iTBImgHeight;

      if (szBitmap)
      {
         /* loading from file */

         int iWidth, iHeight;

         hBitmap = FindBitmapHandle(szBitmap, &iExpWidth, &iExpHeight);

         if (!hBitmap)
         {

            BITMAPINFO        * pPackedDib = NULL;
            HDC                 hdc;

            if (!bMap3Dcolors)
            {

               pPackedDib = PackedDibLoad (szBitmap) ;

            }

            if (pPackedDib || bMap3Dcolors)
            {

                 hdc = GetDC (hWndToolbar) ;

                 if (!bMap3Dcolors)
                 {

                    hBitmap = CreateDIBitmap (hdc,
                                              (PBITMAPINFOHEADER) pPackedDib,
                                              CBM_INIT,
                                              PackedDibGetBitsPtr (pPackedDib),
                                              pPackedDib,
                                              DIB_RGB_COLORS) ;

                    iWidth = PackedDibGetWidth(pPackedDib);
                    iHeight = PackedDibGetHeight(pPackedDib);

                 }
                 else
                 {

                    hBitmap = ( HBITMAP ) LoadImage( ( HINSTANCE ) NULL,
                                          szBitmap,
                                          IMAGE_BITMAP,
                                          iExpWidth,
                                          iExpHeight,

                                          LR_LOADFROMFILE | LR_LOADMAP3DCOLORS);

                    if (hBitmap==NULL)
                    {
                        return FALSE;
                    }

                    iWidth = iExpWidth;
                    iHeight= iExpHeight;

                 }

                 if (iExpWidth!=iWidth || iExpHeight!=iHeight)
                 {

                   HDC hdcSource, hdcTarget;
                   HBITMAP hBitmap2;
                   BOOL bResult;

                   hdcSource = CreateCompatibleDC(hdc);
                   SelectObject(hdcSource, hBitmap);

                   hdcTarget = CreateCompatibleDC(hdc);
                   hBitmap2 = CreateCompatibleBitmap(hdcSource, iExpWidth, iExpHeight);
                   SelectObject(hdcTarget, hBitmap2);

                   bResult = StretchBlt(
                                         hdcTarget,      /* handle to destination DC                 */
                                         0,              /* x-coord of destination upper-left corner */
                                         0,              /* y-coord of destination upper-left corner */
                                         iExpWidth,      /* width of destination rectangle           */
                                         iExpHeight,     /* height of destination rectangle          */
                                         hdcSource,      /* handle to source DC                      */
                                         0,              /* x-coord of source upper-left corner      */
                                         0,              /* y-coord of source upper-left corner      */
                                         iWidth,         /* width of source rectangle                */
                                         iHeight,        /* height of source rectangle               */
                                         SRCCOPY         /* raster operation code                    */
                                       );

                   if (!bResult)
                   {

                     MessageBox( NULL, TEXT( "Cannot shrink/stretch bitmap for Toolbar" ),
                                 szAppName, MB_ICONERROR );

                     DeleteObject(hBitmap2);
                   }
                   else
                   {

                     DeleteObject(hBitmap);
                     hBitmap = hBitmap2;
                     iWidth = iExpWidth;
                     iHeight = iExpHeight;
                   }

                   DeleteDC(hdcSource);
                   DeleteDC(hdcTarget);

                 }

                 ReleaseDC (hWndToolbar, hdc) ;

                 AddBitmapHandle(szBitmap, hBitmap, iWidth, iHeight);

                 if (pPackedDib)
                 {

                    hb_xfree (pPackedDib) ;

                 }
            }
            else
            {
               return FALSE;
            }
         }

         tbab.hInst = NULL;
         tbab.nID   = (UINT) hBitmap;
         iNewBitmap = SendMessage(hWndToolbar, TB_ADDBITMAP, (WPARAM) 1, (WPARAM) &tbab);

      }

      else  /* loading from resources */
      {

         UINT uiOptions = bMap3Dcolors ? LR_LOADMAP3DCOLORS : LR_DEFAULTCOLOR;
         char szResname[_MAX_PATH+1];
         sprintf( szResname, "?%u", uiBitmap );

         hBitmap = FindBitmapHandle(szResname, &iExpWidth, &iExpHeight);

         if (!hBitmap)
         {

            hBitmap = ( HBITMAP ) LoadImage( ( HINSTANCE ) hb_hInstance,
                                  (LPCTSTR) MAKEINTRESOURCE( (WORD) uiBitmap ),
                                  IMAGE_BITMAP,
                                  iExpWidth,
                                  iExpHeight,

                                  uiOptions);

            if (hBitmap==NULL)
            {

               return FALSE;
            }

            AddBitmapHandle(szResname, hBitmap, iExpWidth, iExpHeight);
         }

         tbab.hInst = NULL;
         tbab.nID   = (UINT) hBitmap;
         iNewBitmap = SendMessage(hWndToolbar, TB_ADDBITMAP, (WPARAM) 1, (WPARAM) &tbab);

      }
   }
   else /* system bitmap */
   {
      iNewBitmap = (int) uiBitmap + iOffset;
   }

   szBuffer[0] = (char) 0;
   strcat(szBuffer, pszLabel);
   szBuffer[strlen(szBuffer)+1] = (char) 0;
   iNewString = SendMessage(hWndToolbar, TB_ADDSTRING,(WPARAM) 0, (LPARAM) szBuffer);

   tbb.iBitmap = iNewBitmap;
   tbb.idCommand = iCommand;
   tbb.fsState = TBSTATE_ENABLED;
   tbb.fsStyle = TBSTYLE_BUTTON;
   tbb.dwData = 0;
   tbb.iString = iNewString;

   bSuccess = SendMessage(hWndToolbar, TB_ADDBUTTONS, (WPARAM) 1, (LPARAM) (LPTBBUTTON) &tbb);

   return (bSuccess);
}

static int IndexToCommand(HWND hWndTB, int iIndex)
{
   TBBUTTON tbb;

   if (SendMessage(hWndTB, TB_GETBUTTON,(WPARAM) iIndex, (LPARAM) (LPTBBUTTON) &tbb) )
   {
     return(tbb.idCommand);
   }
   else
   {
     return(0);
   }
}

static int CommandToIndex(HWND hWndTB, int iCommand)
{
   return ( SendMessage(hWndTB, TB_COMMANDTOINDEX,(WPARAM) iCommand, (LPARAM) 0) );
}

static void hb_wvw_gtTBinitSize( WIN_DATA * pWindowData, HWND hWndTB )
{
   RECT rTB = { 0 };

   SendMessage(hWndTB, TB_AUTOSIZE,(WPARAM) 0, (LPARAM) 0);

   if (GetClientRect(hWndTB, &rTB))
   {

     USHORT usTBHeight = rTB.bottom;

     pWindowData->usTBHeight = usTBHeight + 2;

   }
}

static LRESULT CALLBACK hb_wvw_gtTBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  HWND hWndParent = GetParent(hWnd);
  USHORT usWinNum;
  WIN_DATA * pWindowData;

  if (hWndParent==NULL)
  {
    /* TODO: runtime/internal error is better */
    MessageBox( NULL, TEXT( "hb_wvw_gtTBProc(): parent of toolbar is missing" ),
                szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  for(usWinNum=0; usWinNum<s_usNumWindows; usWinNum++)
  {
    if (s_pWindows[usWinNum]->hWnd == hWndParent)
    {
     break;
    }
  }
  if(usWinNum>=s_usNumWindows)
  {
    /* TODO: runtime/internal error is better */
    MessageBox( NULL, TEXT( "hb_wvw_gtTBProc(): invalid handle of tollbar's parent" ),
                szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  pWindowData = s_pWindows[usWinNum];

  switch ( message )
  {
    case WM_PAINT:
    {

       HGDIOBJ hOldObj;
       HDC  hdc;
       RECT rTB = { 0 };
       int iTop, iRight;

       CallWindowProc( (WNDPROC) pWindowData->tbOldProc, hWnd, message, wParam, lParam );

       GetClientRect(hWnd, &rTB);
       iTop = rTB.bottom - 3;
       iRight = rTB.right;

       hdc = GetDC(hWnd);

       hOldObj = SelectObject( hdc, s_sApp.penWhite );

       MoveToEx( hdc, 0, iTop, NULL );            /* Top */
       LineTo( hdc, iRight, iTop );

       SelectObject( hdc, s_sApp.penBlack );

       MoveToEx( hdc, 0, iTop+2, NULL );            /* Bottom */
       LineTo( hdc, iRight, iTop+2 );

       SelectObject( hdc, s_sApp.penDarkGray );
       MoveToEx( hdc, 0, iTop+1, NULL );            /* Middle */
       LineTo( hdc, iRight, iTop+1 );

       SelectObject( pWindowData->hdc, hOldObj );
       ReleaseDC(hWnd, hdc);

       return 0;
    }

  }

  return( CallWindowProc( (WNDPROC) pWindowData->tbOldProc, hWnd, message, wParam, lParam ) );
}

/*-------------------------------------------------------------------*/
/* .prg callable functions                                           */
/*-------------------------------------------------------------------*/

/*WVW_TBCreate([nWinNum], lDisplayText, nStyle, nSystemBitmap, nImageWidth, nImageHeight)
 *creates a toolbar at the top (no button initially)
 *lDisplayText==.f. button's string is used as tooltips (default)
 *nStyle: toolbar style, defaults to TBSTYLE_FLAT | TBSTYLE_TOOLTIPS
 *nSystemBitmap: 0:none, 1:small, 2:large (defaults: 1)
 *               small=16x16 large=24x24
 *nImageWidth/Height are in effect only if nSystemBitmap==0
 */
HB_FUNC( WVW_TBCREATE)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND hWndParent = pWindowData->hWnd;
   HWND hWndTB;
   int   iMaxTextRows = (int) ( ISNIL( 2 ) ? 0 : (hb_parl( 2 ) ? 1 : 0) );
   DWORD dwStyle = (DWORD) ( ISNIL( 3 ) ? TBSTYLE_FLAT | TBSTYLE_TOOLTIPS : hb_parni( 3 ) );
   int   iSystemBitmap = (int) ( ISNIL( 4 ) ? 1 : hb_parni( 4 ) );
   int   iImageWidth  = (int) ( iSystemBitmap==0 && ISNUM( 5 ) ? hb_parni( 5 ) : -1 );
   int   iImageHeight = (int) ( iSystemBitmap==0 && ISNUM( 6 ) ? hb_parni( 6 ) : -1 );
   TBADDBITMAP tbab = { 0 };

   if (pWindowData->hToolBar!=NULL)
   {
     hb_retnl( 0 );
     return;
   }

   if (iImageWidth < 0)
   {
      switch (iSystemBitmap)
      {
         case 1:
            iImageWidth = 16;
            break;
         case 2:
            iImageWidth = 24;
            break;
         default:
            iImageWidth = 16;
            break;
      }
   }
   if (iImageHeight < 0)
   {
      switch (iSystemBitmap)
      {
         case 1:
            iImageHeight = 16;
            break;
         case 2:
            iImageHeight = 24;
            break;
         default:
            iImageHeight = iImageWidth;
            break;
      }
   }

   hWndTB = CreateToolbarEx(hWndParent,
                                   WS_CHILD | WS_VISIBLE | dwStyle,
                                   WVW_ID_BASE_TOOLBAR+usWinNum,
                                   0,
                                   (HINSTANCE) hb_hInstance,
                                   0,
                                   NULL,
                                   0,
                                   0,
                                   0,
                                   iImageWidth,
                                   iImageHeight,
                                   sizeof(TBBUTTON));

   if (hWndTB == NULL)
   {
      MessageBox( NULL, TEXT( "Failed CreateToolbarEx..." ),
                  szAppName, MB_ICONERROR );
      hb_retnl(0);
   }

   pWindowData->tbOldProc = (WNDPROC) SetWindowLong (hWndTB,
                                      GWL_WNDPROC, (LONG) hb_wvw_gtTBProc) ;

   if (iSystemBitmap > 0)
   {
      tbab.hInst = HINST_COMMCTRL;

      tbab.nID   = iSystemBitmap == 1 ? IDB_STD_SMALL_COLOR : IDB_STD_LARGE_COLOR;
      pWindowData->iStartStdBitmap= SendMessage(hWndTB, TB_ADDBITMAP, (WPARAM) 0, (WPARAM) &tbab);

      tbab.nID   = iSystemBitmap == 1 ? IDB_VIEW_SMALL_COLOR : IDB_VIEW_LARGE_COLOR;
      pWindowData->iStartViewBitmap = SendMessage(hWndTB, TB_ADDBITMAP, (WPARAM) 0, (WPARAM) &tbab);

      tbab.nID   = iSystemBitmap == 1 ? IDB_HIST_SMALL_COLOR : IDB_HIST_LARGE_COLOR;
      pWindowData->iStartHistBitmap = SendMessage(hWndTB, TB_ADDBITMAP, (WPARAM) 0, (WPARAM) &tbab);
   }
   else
   {
      pWindowData->iStartStdBitmap = 0;
      pWindowData->iStartViewBitmap = 0;
      pWindowData->iStartHistBitmap = 0;
   }

   pWindowData->iTBImgWidth = iImageWidth;
   pWindowData->iTBImgHeight = iImageHeight;

   SendMessage(hWndTB, TB_SETMAXTEXTROWS,(WPARAM) iMaxTextRows, (LPARAM) 0);

   if (hWndTB)
   {

     hb_wvw_gtTBinitSize( pWindowData, hWndTB );

     pWindowData->hToolBar = hWndTB;

     hb_wvw_gtResetWindow( usWinNum );
   }

   hb_retnl ( (LONG) hWndTB );
}

/*WVW_TBAddButton([nWinNum], nCommand, xBitmap, cLabel, nBitmapType,;
 *                           lMap3Dcolors)
 *adds one button on the right of existing buttons
 *xBitmap:
 *nBitmap is resource id. or use cBitmap as bitmap file name.
 *(bitmap from resources cannot have > 256 colors)
 *
 *cLabel: if lDisplayText, it will be displayed below the bitmap
 *        otherwise it will be used as tooltip
 *nBitmapType: 0:custom, 1:system std bitmap, 2:system view bitmap, 3:system hist bitmap
 *lMap3Dcolors: defaults to .f.
 *           (meaningfull for custom bitmap only)
 *           if .t. the following color mapping will be performed:
 *              RGB(192,192,192) --> COLOR_3DFACE   ("transparent")
 *              RGB(128,128,128) --> COLOR_3DSHADOW
 *              RGB(223,223,223) --> COLOR_3DLIGHT
 *           This might be desirable to have transparent effect.
 *           LIMITATION: this will work on 256 colored bitmaps only
 */

HB_FUNC( WVW_TBADDBUTTON )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int  iCommand = ISNIL(2) ? 0 : hb_parni(2);

   char * szBitmap = ISCHAR(3) ? (char*) hb_parcx(3) : NULL;
   UINT uiBitmap = ISNUM(3) ? (UINT) hb_parni(3) : 0;

   char * szLabel= ISNIL(4) ? "" : (char*) hb_parcx(4);
   int  iBitmapType = ISNIL(5) ? 0 : (int) hb_parni(5);
   BOOL bMap3Dcolors   = ISLOG(6) ? hb_parl(6) : FALSE;
   HWND hWndTB;
   USHORT usOldHeight;

   hWndTB = pWindowData->hToolBar;
   if (hWndTB==NULL)
   {
     hb_retl( FALSE );
     return;
   }

   if ( iCommand >= WVW_ID_BASE_PUSHBUTTON )
   {
     MessageBox( NULL, TEXT( "Toolbar button Command Id too high. Potential conflict with pushbutton" ),
                 szAppName, MB_ICONERROR );
     hb_retl( FALSE );
     return;
   }

   if (strlen(szLabel) > WVW_TB_LABELMAXLENGTH)
   {
      MessageBox( NULL, TEXT( "Cannot addbutton, Label too long..." ),
                  szAppName, MB_ICONERROR );
      hb_retl( FALSE );
      return;
   }

   usOldHeight = pWindowData->usTBHeight;

   if (! AddTBButton(hWndTB, szBitmap, uiBitmap, szLabel, iCommand, iBitmapType, bMap3Dcolors, pWindowData) )
   {
      MessageBox( NULL, TEXT( "Failed addbutton..." ),
                  szAppName, MB_ICONERROR );
      hb_retl( FALSE );
      return;
   }

   hb_wvw_gtTBinitSize( pWindowData, hWndTB );

   if (pWindowData->usTBHeight != usOldHeight)
   {
     hb_wvw_gtResetWindow( usWinNum );
   }

   hb_retl ( TRUE );
}

/*WVW_TBButtonCount([nWinNum])
 *returns number of buttons in toolbar on window nWinNum
 */
HB_FUNC( WVW_TBBUTTONCOUNT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND hWndTB;

   hWndTB = pWindowData->hToolBar;
   if (hWndTB==NULL)
   {
     hb_retni( 0 );
     return;
   }

   hb_retni ( SendMessage(hWndTB, TB_BUTTONCOUNT,(WPARAM) 0, (LPARAM) 0) );
}

/*WVW_TBDelButton([nWinNum], nButton)
 *nButton is zero based index of button to delete
 *index=0 is the leftmost button
 *NOTE: button separator is indexed and deleteable too
 */
HB_FUNC( WVW_TBDELBUTTON )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int  iButton = ISNUM(2) ? hb_parni(2) : -1;
   HWND hWndTB;
   USHORT usOldHeight;

   hWndTB = pWindowData->hToolBar;
   if (hWndTB==NULL || iButton < 0)
   {
     hb_retl( FALSE );
     return;
   }

   usOldHeight = pWindowData->usTBHeight;

   if (! SendMessage(hWndTB, TB_DELETEBUTTON,(WPARAM) iButton, (LPARAM) 0) )
   {
     hb_retl( FALSE );
     return;
   }

   hb_wvw_gtTBinitSize( pWindowData, hWndTB );

   if (pWindowData->usTBHeight != usOldHeight)
   {
     hb_wvw_gtResetWindow( usWinNum );
   }

   hb_retl ( TRUE );
}

/*WVW_TBEnableButton([nWinNum], nButton, [lToggle])
 *nButton is zero based index of button to enable/disable
 *index=0 is the leftmost button
 *NOTE: button separator is indexed too
 *returns .t. if successful
 */
HB_FUNC( WVW_TBENABLEBUTTON )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   int  iButton = ISNUM(2) ? hb_parni(2) : -1;
   BOOL bEnable = ISLOG(3)? hb_parl(3) : TRUE;
   int  iCommand;
   HWND hWndTB;
   USHORT usOldHeight;

   hWndTB = pWindowData->hToolBar;
   if (hWndTB==NULL || iButton < 0)
   {
     hb_retl( FALSE );
     return;
   }

   iCommand = IndexToCommand(hWndTB, iButton);
   if (iCommand < 0)
   {
     hb_retl( FALSE );
     return;
   }

   usOldHeight = pWindowData->usTBHeight;

   if (! SendMessage(hWndTB, TB_ENABLEBUTTON,(WPARAM) iCommand, (LPARAM) MAKELONG(bEnable,0) ) )
   {
     hb_retl( FALSE );
     return;
   }

   hb_wvw_gtTBinitSize( pWindowData, hWndTB );

   if (pWindowData->usTBHeight != usOldHeight)
   {
     hb_wvw_gtResetWindow( usWinNum );
   }

   hb_retl ( TRUE );
}

/*WVW_TBdestroy( [nWinNum] )
 *destroy toolbar for window nWinNum
 */
HB_FUNC( WVW_TBDESTROY)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   if (!(pWindowData->hToolBar==NULL))
   {
     DestroyWindow( pWindowData->hToolBar );
     pWindowData->hToolBar = NULL;
     pWindowData->usTBHeight = 0;

     hb_wvw_gtResetWindow( usWinNum );
   }
}

/*WVW_TBINDEX2CMD([nWinNum], nIndex)
 *returns Command Id of button nIndex (0 based)
 *returns -1 if the button does not exist
 */
HB_FUNC( WVW_TBINDEX2CMD )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND   hWndTB = pWindowData->hToolBar;
   int    iIndex = hb_parni(2);
   int    iCmd   = IndexToCommand(hWndTB, iIndex);

   hb_retni( (int) ( iCmd > 0 ? iCmd : -1 ) );
}

/*WVW_TBCmd2Index([nWinNum], nCmd)
 *returns Index (0 based) of button whose command id is nCmd
 *returns -1 if the button does not exist
 */
HB_FUNC( WVW_TBCMD2INDEX )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND   hWndTB = pWindowData->hToolBar;
   int    iCmd = hb_parni(2);

   hb_retni( CommandToIndex(hWndTB, iCmd) );
}

/*-------------------------------------------------------------------*/
/* TOOLBAR ends                                                      */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* SCROLLBAR begins                                                  */
/*-------------------------------------------------------------------*/

/*********************** start control (eg. scrollbar) list handler ***************************/

static HWND FindControlHandle(USHORT usWinNum, BYTE byCtrlClass, UINT uiCtrlid, byte * pbStyle)
{
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass && uiCtrlid == pcd->uiCtrlid)
    {
      *pbStyle = pcd->bStyle;
      return pcd->hWndCtrl;
    }
    pcd = pcd->pNext;
  }
  return NULL;
}

static UINT FindControlId(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, byte * pbStyle)
{
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass && hWndCtrl == pcd->hWndCtrl)
    {
      *pbStyle = pcd->bStyle;
      return pcd->uiCtrlid;
    }
    pcd = pcd->pNext;
  }
  return (UINT) NULL;
}

static UINT LastControlId(USHORT usWinNum, BYTE byCtrlClass)
{
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

  while (pcd && byCtrlClass != pcd->byCtrlClass)
  {
    pcd = pcd->pNext;
  }

  if (pcd)
  {
    return pcd->uiCtrlid;
  }
  else
  {
    return 0;
  }
}

static void AddControlHandle(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid, PHB_ITEM phiCodeBlock, RECT rCtrl, RECT rOffCtrl, byte bStyle)
{
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  CONTROL_DATA * pcdNew = (CONTROL_DATA *) hb_xgrab( sizeof( CONTROL_DATA ) );

  pcdNew->byCtrlClass = byCtrlClass;
  pcdNew->hWndCtrl = hWndCtrl;
  pcdNew->uiCtrlid = uiCtrlid;

  pcdNew->phiCodeBlock = NULL;

  if (phiCodeBlock != NULL)
  {
     pcdNew->phiCodeBlock = hb_itemNew( phiCodeBlock );

  }

  pcdNew->bBusy = FALSE;

  pcdNew->rCtrl.top = rCtrl.top;
  pcdNew->rCtrl.left = rCtrl.left;
  pcdNew->rCtrl.bottom = rCtrl.bottom;
  pcdNew->rCtrl.right = rCtrl.right;
  pcdNew->rOffCtrl.top = rOffCtrl.top;
  pcdNew->rOffCtrl.left = rOffCtrl.left;
  pcdNew->rOffCtrl.bottom = rOffCtrl.bottom;
  pcdNew->rOffCtrl.right = rOffCtrl.right;

  pcdNew->bStyle = bStyle;

  pcdNew->OldProc = (WNDPROC) NULL;

  pcdNew->pNext = pWindowData->pcdCtrlList;

  pWindowData->pcdCtrlList = pcdNew;
}

static CONTROL_DATA * GetControlData(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT uiCtrlid)
{
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass &&
        ((hWndCtrl && hWndCtrl == pcd->hWndCtrl) ||
         (uiCtrlid && uiCtrlid == pcd->uiCtrlid))  )
    {
      return pcd;
    }
    pcd = pcd->pNext;
  }
  return NULL;
}

static BOOL StoreControlProc(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, WNDPROC OldProc)
{
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass && hWndCtrl == pcd->hWndCtrl)
    {
      pcd->OldProc = OldProc;
      return TRUE;
    }
    pcd = pcd->pNext;
  }
  return FALSE;
}

static WNDPROC GetControlProc(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl)
{
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (byCtrlClass == pcd->byCtrlClass && hWndCtrl == pcd->hWndCtrl)
    {
      return pcd->OldProc;
    }
    pcd = pcd->pNext;
  }
  return (WNDPROC) NULL;
}

static int GetControlClass(USHORT usWinNum, HWND hWndCtrl)
{

  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd)
  {
    if (hWndCtrl == pcd->hWndCtrl)
    {
      return pcd->byCtrlClass;
    }
    pcd = pcd->pNext;
  }
  return 0;
}

static void RunControlBlock(USHORT usWinNum, BYTE byCtrlClass, HWND hWndCtrl, UINT message, WPARAM wParam, LPARAM lParam, BYTE bEventType )
{
  static int iHitBusy = 0;
  WIN_DATA * pWindowData = s_pWindows[usWinNum];
  CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
  while (pcd && (byCtrlClass != pcd->byCtrlClass || hWndCtrl != pcd->hWndCtrl))
  {
    pcd = pcd->pNext;
  }

  if (pcd==NULL)
  {
    return;
  }

  if ( (pcd->byCtrlClass==WVW_CONTROL_SCROLLBAR ||
        pcd->byCtrlClass==WVW_CONTROL_PUSHBUTTON ||
        pcd->byCtrlClass==WVW_CONTROL_COMBOBOX
        )
       && pcd->phiCodeBlock )

  {
     PHB_ITEM phiWinNum, phiXBid, phiXBmsg, phiXBpos;
     PHB_ITEM pReturn;

     if (pcd->bBusy)
     {
       iHitBusy++;

       return;
     }
     else
     {
       iHitBusy = 0;
     }

     pcd->bBusy = TRUE;

     phiWinNum = hb_itemNew(NULL);
     hb_itemPutNI( phiWinNum, (int) usWinNum );

     phiXBid = hb_itemNew(NULL);
     hb_itemPutNI( phiXBid, (int) pcd->uiCtrlid );

     if (pcd->byCtrlClass==WVW_CONTROL_SCROLLBAR)
     {
        phiXBmsg = hb_itemNew(NULL);
        hb_itemPutNI( phiXBmsg, (int) LOWORD(wParam) );

        phiXBpos = hb_itemNew(NULL);
        hb_itemPutNI( phiXBpos, (int) HIWORD(wParam) );

        pReturn = hb_itemDo( pcd->phiCodeBlock, 4, phiWinNum, phiXBid, phiXBmsg, phiXBpos );
        hb_itemRelease( pReturn );
        hb_itemRelease( phiXBmsg );
        hb_itemRelease( phiXBpos );
     }
     else if (pcd->byCtrlClass==WVW_CONTROL_PUSHBUTTON)
     {

        pReturn = hb_itemDo( pcd->phiCodeBlock, 2, phiWinNum, phiXBid );
        hb_itemRelease( pReturn );
     }

     else if (pcd->byCtrlClass==WVW_CONTROL_COMBOBOX)
     {
        int     iCurSel;

        PHB_ITEM phiEvent, phiIndex;

        switch (bEventType)
        {
           case CBN_SELCHANGE:
           case CBN_SETFOCUS:
           case CBN_KILLFOCUS:
           {
              iCurSel = SendMessage((HWND) pcd->hWndCtrl,
                                    CB_GETCURSEL,
                                    (WPARAM) 0,
                                    (LPARAM) 0
                                    );
              if (iCurSel == CB_ERR)
              {
                 break;
              }

              /***********************
              let user find by his own, what is the string of iCurSel
              we don;t have to do this:

              iTextLen = SendMessage((HWND) pcd->hWndCtrl,
                                     CB_GETLBTEXTLEN,
                                     (WPARAM) iCurSel;
                                     (LPARAM) 0
                                     );
              lptstrSelected = ( char* ) hb_xgrab( iTextLen+1 );

              SendMessage((HWND) pcd->hWndCtrl,
                          CB_GETLBTEXT,
                          (WPARAM) iCurSel,
                          (LPARAM) lptstrSelected
                          );

              ...

              hb_xfree( lptstrSelected );

              **************************/

              /* now execute the codeblock */
              phiEvent = hb_itemNew(NULL);
              hb_itemPutNI( phiEvent, (int) bEventType );

              phiIndex = hb_itemNew(NULL);
              hb_itemPutNI( phiIndex, (int) iCurSel );

              pReturn = hb_itemDo( pcd->phiCodeBlock, 4, phiWinNum, phiXBid, phiEvent, phiIndex );
              hb_itemRelease( pReturn );
              hb_itemRelease( phiEvent );
              hb_itemRelease( phiIndex );

           }
        }
     }

     hb_itemRelease( phiWinNum );
     hb_itemRelease( phiXBid );

     pcd->bBusy = FALSE;

  }

  HB_SYMBOL_UNUSED( message );
  HB_SYMBOL_UNUSED( lParam );

  return;
}

static void ReposControls(USHORT usWinNum, BYTE byCtrlClass)
{
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

   while (pcd)
   {
      if (byCtrlClass==0 || byCtrlClass==pcd->byCtrlClass)
      {
         POINT xy = { 0 };
         int  iTop, iLeft, iBottom, iRight;

         xy      = hb_wvw_gtGetXYFromColRow( pWindowData, pcd->rCtrl.left, pcd->rCtrl.top );
         iTop    = xy.y + pcd->rOffCtrl.top ;
         iLeft   = xy.x + pcd->rOffCtrl.left;

         xy      = hb_wvw_gtGetXYFromColRow( pWindowData, pcd->rCtrl.right + 1, pcd->rCtrl.bottom + 1 );

         xy.y   -= pWindowData->byLineSpacing;

         if (pcd->byCtrlClass==WVW_CONTROL_SCROLLBAR)
         {

            if (pcd->bStyle==SBS_VERT)
            {
              iBottom = xy.y - 1 + pcd->rOffCtrl.bottom;
              iRight  = iLeft + pWindowData->PTEXTSIZE.y -1 + pcd->rOffCtrl.right;
            }
            else
            {
              iRight  = xy.x - 1 + pcd->rOffCtrl.right;
              iBottom  = iTop + pWindowData->PTEXTSIZE.y -1 + pcd->rOffCtrl.bottom;
            }
         }
         else if (pcd->byCtrlClass==WVW_CONTROL_PUSHBUTTON)
         {
            iBottom = xy.y - 1 + pcd->rOffCtrl.bottom;
            iRight  = xy.x - 1 + pcd->rOffCtrl.right;
         }

         else if (pcd->byCtrlClass==WVW_CONTROL_PROGRESSBAR)
         {
            iBottom = xy.y - 1 + pcd->rOffCtrl.bottom;
            iRight  = xy.x - 1 + pcd->rOffCtrl.right;
         }

         else if (pcd->byCtrlClass==WVW_CONTROL_COMBOBOX)
         {

            iBottom = xy.y - 1 + (pcd->rOffCtrl.bottom * hb_wvw_LineHeight(pWindowData));
            iRight  = xy.x - 1 + pcd->rOffCtrl.right;
         }

         else
         {

           hb_errRT_TERM( EG_NOFUNC, 10001, "Undefined Control Class", "ReposControls()", 0, 0 );

           /* dummy assignment, to avoid warning in mingw32: */
           iBottom = 0;
           iRight  = 0;
         }

         SetWindowPos( pcd->hWndCtrl, NULL, iLeft, iTop, iRight-iLeft+1, iBottom-iTop+1, SWP_NOZORDER );
      }

      pcd = pcd->pNext;
   }
}

/*********************** end control (eg. scrollbar) list handler ***************************/

/*-------------------------------------------------------------------*/
/* SCROLLBAR begins                                                  */
/*-------------------------------------------------------------------*/

static LRESULT CALLBACK hb_wvw_gtXBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  HWND hWndParent = GetParent(hWnd);
  USHORT usWinNum;

  UINT uiXBid;
  byte bStyle;
  WNDPROC OldProc;

  if (message == WM_MOUSEACTIVATE)
  {

    s_iScrolling = 1;

  }

  if (hWndParent==NULL)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  for(usWinNum=0; usWinNum<s_usNumWindows; usWinNum++)
  {
    if (s_pWindows[usWinNum]->hWnd == hWndParent)
    {
     break;
    }
  }
  if(usWinNum>=s_usNumWindows)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  uiXBid = (UINT) GetWindowLong(hWnd, GWL_ID);
  if (uiXBid==0)
  {
    MessageBox( NULL, TEXT( "Failed FindControlId of Scrollbar" ),
                szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  OldProc = GetControlProc(usWinNum, WVW_CONTROL_SCROLLBAR, hWnd);
  if (OldProc == NULL)
  {
    MessageBox( NULL, TEXT( "Failed GetControlProc of Scrollbar" ),
                szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  switch ( message )
  {

    case WM_LBUTTONUP:
    {

      CallWindowProc( OldProc, hWnd, message, wParam, lParam );
      if (GetCapture() == hWnd)
      {
        ReleaseCapture();

        InvalidateRect( hWnd, NULL, FALSE );

      }
      return 0;

    }

    case WM_RBUTTONDOWN:
    {

      s_iScrolling = 0;

      return 0;
    }
    case WM_RBUTTONUP:
    {

      return 0;
    }

  }

  if (message == WM_CAPTURECHANGED)
  {
    s_iScrolling = 0;

  }

  return( CallWindowProc( OldProc, hWnd, message, wParam, lParam ) );
}

/*WVW_XBcreate( [nWinNum], nStyle, nTop, nLeft, nLength, bBlock, aOffset)
 *create scroll bar for window nWinNum
 *nStyle: SBS_HORZ (0)=horizontal, SBS_VERT (1)=vertical
 *nTop: row of top/left corner (in character unit)
 *nLeft: col of top/left corner (in character unit)
 *nLength: length of scrollbar (in character unit)
 *NOTES: width of scrollbar (in character unit)
 *            horiz: defaults to one character height
 *            verti: defaults to one character _height_ too (!)
 *       use aOffset to adjust the dimension
 *aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of scroll bar.
 *         defaults for vertical scroll bar: {0,+3,0,0}
 *         defaults for horiz scroll bar: {+3-linespacing,0,0,0}
 *         NOTES: these defaults are meant to make room for other common
 *                GUI elements like raised/recessed lines.
 *
 *bBlock:  codeblock to execute on every WM_VSCROLL/WM_HSCROLL event.
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nXBid  : scrollbar id
 *         nXBmsg : scrollbar message, ie. one of these:
 *         nXBpos : scrollthumb position (only if message==SB_THUMB...)
 *         the "must be handled" messages:
 *             SB_LINEUP/SB_LINELEFT     0: up/left button clicked
 *             SB_LINEDOWN/SB_LINERIGHT  1: down/right button clicked
 *             SB_PAGEUP/SB_PAGELEFT     2: upper/left shaft clicked
 *             SB_PAGEDOWN/SB_PAGERIGHT  3: lower/right shaft clicked
 *         the "may not be handled" messages:
 *             SB_THUMBPOSITION          4: scroll thumb is released at position nXBpos
 *             SB_THUMBTRACK             5: scroll thumb is being dragged at position nXBpos
 *             SB_ENDSCROLL              8
 *
 *returns control id of newly created scroll bar of windows nWinNum
 *returns 0 if failed
 *
 *example:
 *WVW_XBcreate( , 1, 10, 70, 12)
 *  :: creates Vertical scrollbar on current window at (10,70) with length 12
 *     dimensions using default ones.
 *     buttons/parts behaviour using default ones.
 *
 *WVW_XBcreate( , 1, 10, 70, 12, {0, +5, 0, +5} )
 *  :: creates Vertical scrollbar on current window at (10,70) with length 12
 *     left and right coordinate is shifted 5 pixels to the right.
 *     buttons/parts behaviour using default ones.
 *
 *NOTES:
 *ScrollRange is always 0 - 100.
 *Initial ScrollPos is 0
 */

HB_FUNC( WVW_XBCREATE)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND hWndParent = pWindowData->hWnd;
   HWND hWndXB;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   int  iStyle = (int) ( !ISNUM( 2 ) ? -1 : hb_parni( 2 ) );
   UINT uiXBid;
   USHORT   usTop    = hb_parni( 3 ),
            usLeft   = hb_parni( 4 ),
            usBottom,
            usRight;

   if (iStyle < SBS_HORZ || iStyle > SBS_VERT || !ISBLOCK(6))
   {
     hb_retnl(0);
     return;
   }

   if (iStyle==SBS_VERT)
   {
     usBottom = usTop + hb_parni( 5 ) -1;
     usRight = usLeft;

     iOffTop    = !ISNIL( 7 ) ? hb_parni( 7,1 ) : 0 ;
     iOffLeft   = !ISNIL( 7 ) ? hb_parni( 7,2 ) : +3 ;
     iOffBottom = !ISNIL( 7 ) ? hb_parni( 7,3 ) : 0 ;
     iOffRight  = !ISNIL( 7 ) ? hb_parni( 7,4 ) : 0;
   }
   else
   {
     usRight = usLeft + hb_parni( 5 ) -1;
     usBottom = usTop;

     iOffTop    = !ISNIL( 7 ) ? hb_parni( 7,1 ) : +3-pWindowData->byLineSpacing ;
     iOffLeft   = !ISNIL( 7 ) ? hb_parni( 7,2 ) : 0 ;
     iOffBottom = !ISNIL( 7 ) ? hb_parni( 7,3 ) : 0;
     iOffRight  = !ISNIL( 7 ) ? hb_parni( 7,4 ) : 0 ;
   }

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + iOffTop ;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   if (iStyle==SBS_VERT)
   {
     iBottom = xy.y - 1 + iOffBottom;
     iRight  = iLeft + pWindowData->PTEXTSIZE.y -1 + iOffRight;
   }
   else
   {
     iRight  = xy.x - 1 + iOffRight;
     iBottom  = iTop + pWindowData->PTEXTSIZE.y -1 + iOffBottom;
   }

   uiXBid = LastControlId(usWinNum, WVW_CONTROL_SCROLLBAR);
   if (uiXBid==0)
   {
     uiXBid = WVW_ID_BASE_SCROLLBAR;
   }
   else
   {
     uiXBid++;
   }

   hWndXB = CreateWindowEx(
       0L,
       "SCROLLBAR",
       (LPSTR) NULL,
       WS_CHILD | WS_VISIBLE | (DWORD) iStyle,
       iLeft,
       iTop,
       iRight-iLeft+1,
       iBottom-iTop+1,
       hWndParent,
       (HMENU) uiXBid,
       (HINSTANCE) hb_hInstance,
       (LPVOID) NULL
   );

   if(hWndXB)
   {

     RECT rXB = { 0 }, rOffXB = { 0 };

     WNDPROC OldProc;

     rXB.top = usTop;     rXB.left= usLeft;
     rXB.bottom=usBottom; rXB.right =usRight;
     rOffXB.top = iOffTop;     rOffXB.left= iOffLeft;
     rOffXB.bottom=iOffBottom; rOffXB.right =iOffRight;

     SetScrollRange (hWndXB, SB_CTL, 0, 99, FALSE) ;
     SetScrollPos   (hWndXB, SB_CTL, 0, TRUE) ;

     AddControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, hWndXB, uiXBid, (HB_ITEM *) hb_param( 6, HB_IT_BLOCK ), rXB, rOffXB, (byte) iStyle);

     OldProc = (WNDPROC) SetWindowLong (hWndXB,
                                        GWL_WNDPROC, (LONG) hb_wvw_gtXBProc) ;

     StoreControlProc(usWinNum, WVW_CONTROL_SCROLLBAR, hWndXB, OldProc);

     hb_retnl( (LONG) uiXBid );
   }
   else
   {

     hb_retnl( (LONG) 0 );
   }
}

/*WVW_XBdestroy( [nWinNum], nXBid )
 *destroy scrollbar nXBid for window nWinNum
 */
HB_FUNC( WVW_XBDESTROY)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   UINT uiXBid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev = NULL;

   while (pcd)
   {
     if (pcd->byCtrlClass == WVW_CONTROL_SCROLLBAR && pcd->uiCtrlid == uiXBid)
     {
       break;
     }

     pcdPrev = pcd;
     pcd = pcd->pNext;
   }
   if (pcd==NULL) { return; }

   DestroyWindow (pcd->hWndCtrl) ;

   if (pcdPrev==NULL)
   {
     pWindowData->pcdCtrlList = pcd->pNext;
   }
   else
   {
     pcdPrev->pNext = pcd->pNext;
   }

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   hb_xfree( pcd );
}

/*WVW_XBupdate(nWinNum, XBid, [nPos], [nPageSize], [nMin], [nMax])
 *update scrollbar data and its display
 *nPos, nPageSize, nMin, nMax are optional.
 *however, both nMin & nMax must be supplied, or not at all.
 *returns current position of scroll thumb.
 *returns -1 if update failed.
*/
HB_FUNC( WVW_XBUPDATE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiXBid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   byte bStyle;
   HWND hWndXB = FindControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, uiXBid, &bStyle);
   int  iPos = (int) ( ISNIL(3) ? 0 : hb_parni( 3 ) );
   int  iPage = (int) ( ISNIL( 4 ) ? 0  : hb_parni( 4 ) );
   int  iMin = (int) ( ISNIL(5) ? 0 : hb_parni( 5 ) );
   int  iMax = (int) ( ISNIL(6) ? 0 : hb_parni( 6 ) );
   SCROLLINFO si;
   int  iRetval;
   UINT fMask = SIF_DISABLENOSCROLL;

   if (uiXBid==0 || hWndXB==NULL || iPage<0)
   {
     hb_retni(-1);
     return;
   }

   if (!ISNIL(3)) { fMask = fMask | SIF_POS; }
   if (!ISNIL(4)) { fMask = fMask | SIF_PAGE; }
   if (!ISNIL(5) && !ISNIL(6)) { fMask = fMask | SIF_RANGE; }

   si.cbSize = sizeof(si);
   si.fMask  = fMask;
   si.nMin   = iMin;
   si.nMax   = iMax;
   si.nPage  = (UINT) iPage;
   si.nPos   = iPos;
   iRetval = SetScrollInfo(hWndXB,
                           SB_CTL,
                           (LPCSCROLLINFO) &si,
                           TRUE);

   hb_retni(iRetval);
}

/* WVW_XBinfo( [nWinNum], XBid )
 * return an array {nMin, nMax, nPageSize, nPos, nTrackPos }
 * return an empty array {} if invalid parameter passed.
 */
HB_FUNC( WVW_XBINFO )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   HB_ITEM  aInfo;
   HB_ITEM  temp;
   SCROLLINFO si;

   UINT uiXBid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   byte bStyle;
   HWND hWndXB = FindControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, uiXBid, &bStyle);

   if (uiXBid==0 || hWndXB==NULL )
   {
     aInfo.type = HB_IT_NIL;

     hb_arrayNew( &aInfo, 0 );
     hb_itemReturn( &aInfo );
     return;
   }

   si.cbSize = sizeof( si );
   si.fMask = SIF_ALL;

   if ( !GetScrollInfo( hWndXB, SB_CTL, &si ) )
   {
     aInfo.type = HB_IT_NIL;

     hb_arrayNew( &aInfo, 0 );
     hb_itemReturn( &aInfo );
     return;
   }

   aInfo.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &aInfo, 5 );

   hb_arraySetForward( &aInfo, 1, hb_itemPutNL( &temp, si.nMin ));
   hb_arraySetForward( &aInfo, 2, hb_itemPutNL( &temp, si.nMax ));
   hb_arraySetForward( &aInfo, 3, hb_itemPutNL( &temp, si.nPage ));
   hb_arraySetForward( &aInfo, 4, hb_itemPutNL( &temp, si.nPos ));
   hb_arraySetForward( &aInfo, 5, hb_itemPutNL( &temp, si.nTrackPos ));

   hb_itemReturn( &aInfo );
}

/*WVW_XBenable( [nWinNum], nXBid, nFlags )
 *enable/disable scrollbar nXBid in window nWinNum (default to topmost window)
 *nFlags: ESB_ENABLE_BOTH                    0: enable both arrows
 *        ESB_DISABLE_LEFT/ESB_DISABLE_UP    1: disable left/up arrow
 *        ESB_DISABLE_RIGHT/ESB_DISABLE_DOWN 2: disable right/down arrow
 *        ESB_DISABLE_BOTH                   3: disable both arrow
 *returns .t. if successful
 */
HB_FUNC( WVW_XBENABLE)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiXBid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   UINT uiFlags = (UINT) ( ISNIL( 3 ) ? 0  : hb_parni( 3 ) );
   byte bStyle;
   HWND hWndXB = uiXBid==0 ? NULL : FindControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, uiXBid, &bStyle);

   if (uiXBid==0 || hWndXB==NULL || uiFlags > ESB_DISABLE_BOTH )
   {
     hb_retl( FALSE );
     return;
   }

   hb_retl( EnableScrollBar( hWndXB, SB_CTL, uiFlags ) );
}

/*WVW_XBshow( [nWinNum], nXBid, lShow )
 *show/hide scrollbar nXBid in window nWinNum (default to topmost window)
 *nXBid is the handle of the scrolbar
 *lShow: .T. shows the scrolbar (default)
 *       .F. hides the scrolbar
 *returns .t. if successful
 */
HB_FUNC( WVW_XBSHOW )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   UINT uiXBid = (UINT) ( ISNIL( 2 ) ? 0 : hb_parni( 2 ) );
   BOOL bShow = (BOOL) ( ISLOG( 3 ) ? hb_parl( 3 ) : TRUE );
   byte bStyle;
   HWND hWndXB = uiXBid == 0 ? NULL : FindControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, uiXBid, &bStyle );

   if (uiXBid==0 || hWndXB==NULL)
   {
     hb_retl( FALSE );
     return;
   }

   hb_retl( ShowScrollBar( hWndXB, SB_CTL, bShow ) );
}

/*-------------------------------------------------------------------*/
/* SCROLLBAR ends                                                    */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* PUSHBUTTON begins                                                 */
/*-------------------------------------------------------------------*/

static LRESULT CALLBACK hb_wvw_gtBtnProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  HWND hWndParent = GetParent(hWnd);
  USHORT usWinNum;

  UINT uiPBid;

  WNDPROC OldProc;

  if (hWndParent==NULL)
  {
    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  for(usWinNum=0; usWinNum<s_usNumWindows; usWinNum++)
  {
    if (s_pWindows[usWinNum]->hWnd == hWndParent)
    {
     break;
    }
  }
  if(usWinNum>=s_usNumWindows)
  {
    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  uiPBid = (UINT) GetWindowLong(hWnd, GWL_ID);
  if (uiPBid==0)
  {

    MessageBox( NULL, TEXT( "Failed FindControlId" ),
                szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  OldProc = GetControlProc(usWinNum, WVW_CONTROL_PUSHBUTTON, hWnd);
  if (OldProc == NULL)
  {

    MessageBox( NULL, TEXT( "Failed GetControlProc" ),
                szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  switch ( message )
  {

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
    {
      BOOL bAlt      = GetKeyState( VK_MENU ) & 0x8000;
      BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
      BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
      int  c = (int) wParam;

      if (!bAlt && !bCtrl && !bShift && wParam == VK_SPACE)
      {
        break;
      }

      if ( !hb_wvw_gtBufferedKey( (LONG) wParam ) )
      {
        break;
      }

      switch ( c )
      {

        /*
        case VK_RETURN:
        {

          SendMessage(hWnd, BM_CLICK, 0, 0);

          break;

        }
        */

        default:
        {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);

          break;
        }

      }

      return 0;

    }

    /*
    case WM_RBUTTONDBLCLK:
    case WM_LBUTTONDBLCLK:
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    case WM_MBUTTONDBLCLK:
    case WM_MOUSEMOVE:
    case WM_MOUSEWHEEL:
    */
    {

    }

  }

  return( CallWindowProc( (WNDPROC) OldProc, hWnd, message, wParam, lParam ) );
}

/*WVW_PBcreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, cImage/nImage, bBlock, aOffset)
 *create pushbutton for window nWinNum
 *nTop: row of top/left corner (in character unit)
 *nLeft: col of top/left corner (in character unit)
 *nBottom: row of bottom/right corner (in character unit) defaults==nTop
 *nRight: col of bottom/right corner (in character unit) defaults==??
 *cText: caption, default == ""
 *
 *reserved for future: (TODO):
 *cImage: bitmap file name, can be supplied as nImage: bitmap resource id
 *this parm is now ignored
 *
 *aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of pushbutton.
 *         defaults for vertical scroll bar: {0,+3,0,0}
 *         defaults for horiz scroll bar: {+3-linespacing,0,0,0}
 *         NOTES: these defaults are meant to make room for other common
 *                GUI elements like raised/recessed lines.
 *
 *bBlock:  codeblock to execute on every BN_CLICK event.
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nPBid  : pushbutton id
 *
 *returns control id of newly created pushbutton of windows nWinNum
 *returns 0 if failed
 *
 *example:
 */

HB_FUNC( WVW_PBCREATE)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND hWndParent = pWindowData->hWnd;
   HWND hWndPB;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   int   iStyle;
   UINT uiPBid;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   LPCTSTR  lpszCaption = ISCHAR(6) ? hb_parcx(6) : NULL;
   char   * szBitmap = ISCHAR(7) ? (char*) hb_parcx(7) : NULL;
   UINT     uiBitmap = ISNUM(7) ? (UINT) hb_parni(7) : 0;

   HB_SYMBOL_UNUSED( szBitmap );
   HB_SYMBOL_UNUSED( uiBitmap );

   if (!ISBLOCK(8))
   {
     hb_retnl(0);
     return;
   }

   if (pWindowData->hPBfont==NULL)
   {
      pWindowData->hPBfont = CreateFontIndirect( &s_lfPB );
      if (pWindowData->hPBfont==NULL)
      {
        hb_retnl(0);
        return;
      }
   }

   iOffTop    = !ISNIL( 9 ) ? hb_parni( 9,1 ) : -2 ;
   iOffLeft   = !ISNIL( 9 ) ? hb_parni( 9,2 ) : -2 ;
   iOffBottom = !ISNIL( 9 ) ? hb_parni( 9,3 ) : +2 ;
   iOffRight  = !ISNIL( 9 ) ? hb_parni( 9,4 ) : +2;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + iOffTop ;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   uiPBid = LastControlId(usWinNum, WVW_CONTROL_PUSHBUTTON);
   if (uiPBid==0)
   {
     uiPBid = WVW_ID_BASE_PUSHBUTTON;
   }
   else
   {
     uiPBid++;
   }

   iStyle = BS_PUSHBUTTON;

   hWndPB = CreateWindowEx(
       0L,
       "BUTTON",
       (LPSTR) lpszCaption,
       WS_CHILD | WS_VISIBLE | (DWORD) iStyle,
       iLeft,
       iTop,
       iRight-iLeft+1,
       iBottom-iTop+1,
       hWndParent,
       (HMENU) uiPBid,
       (HINSTANCE) hb_hInstance,
       (LPVOID) NULL
   );

   if(hWndPB)
   {
     RECT rXB = { 0 }, rOffXB = { 0 };
     WNDPROC OldProc;

     rXB.top = usTop;     rXB.left= usLeft;
     rXB.bottom=usBottom; rXB.right =usRight;
     rOffXB.top = iOffTop;     rOffXB.left= iOffLeft;
     rOffXB.bottom=iOffBottom; rOffXB.right =iOffRight;

     AddControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, hWndPB, uiPBid, (HB_ITEM *) hb_param( 8, HB_IT_BLOCK ), rXB, rOffXB, (byte) iStyle);

     OldProc = (WNDPROC) SetWindowLong (hWndPB,
                                        GWL_WNDPROC, (LONG) hb_wvw_gtBtnProc) ;

     StoreControlProc(usWinNum, WVW_CONTROL_PUSHBUTTON, hWndPB, OldProc);

     SendMessage( hWndPB, WM_SETFONT, (WPARAM) pWindowData->hPBfont, (LPARAM) TRUE);

     hb_retnl( (LONG) uiPBid );
   }
   else
   {

     hb_retnl( (LONG) 0 );
   }
}

/*WVW_PBdestroy( [nWinNum], nPBid )
 *destroy button nPBid for window nWinNum
 */
HB_FUNC( WVW_PBDESTROY)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   UINT uiPBid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev = NULL;

   while (pcd)
   {

     if (pcd->byCtrlClass == WVW_CONTROL_PUSHBUTTON && pcd->uiCtrlid == uiPBid)
     {
       break;
     }

     pcdPrev = pcd;
     pcd = pcd->pNext;

   }

   if (pcd==NULL) { return; }

   DestroyWindow (pcd->hWndCtrl) ;

   if (pcdPrev==NULL)
   {
     pWindowData->pcdCtrlList = pcd->pNext;
   }
   else
   {
     pcdPrev->pNext = pcd->pNext;
   }

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   hb_xfree( pcd );

}

/*WVW_PBsetFocus( [nWinNum], nButtonId )
 *set the focus to button nButtonId in window nWinNum
 */
HB_FUNC( WVW_PBSETFOCUS )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = ISNIL(2) ? 0 : hb_parni(2);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);

  if (hWndPB)
  {
    hb_retl(hWndPB == SetFocus(hWndPB));
  }
  else
  {
    hb_retl(FALSE);
  }
}

/*WVW_PBenable( [nWinNum], nButtonId, [lToggle] )
 *enable/disable button nButtonId on window nWinNum
 *(lToggle defaults to .t., ie. enabling the button)
 *return previous state of the button (TRUE:enabled FALSE:disabled)
 *(if nButtonId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_PBENABLE )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = ISNIL(3) ? TRUE : hb_parl(3);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);

  if (hWndPB)
  {
    hb_retl( EnableWindow(hWndPB, bEnable)==0 );

    if (!bEnable)
    {
       SetFocus( s_pWindows[ usWinNum ]->hWnd );
    }
  }
  else
  {
    hb_retl(FALSE);
  }
}

/*WVW_PBsetcodeblock( [nWinNum], nPBid, bBlock )
 *assign (new) codeblock bBlock to button nPBid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_PBSETCODEBLOCK )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiPBid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_PUSHBUTTON, NULL, uiPBid);
   PHB_ITEM phiCodeBlock = hb_param( 3, HB_IT_BLOCK );
   if (!phiCodeBlock || pcd==NULL || pcd->bBusy)
   {

     /*
     if (!ISBLOCK(3))
     {
        MessageBox( NULL, TEXT( "Codeblock Expected" ),
                    szAppName, MB_ICONERROR );
     }

     if (pcd==NULL)
     {
        MessageBox( NULL, TEXT( "Control Data not Found" ),
                    szAppName, MB_ICONERROR );
     }

     if (pcd->bBusy)
     {
        MessageBox( NULL, TEXT( "Codeblock is busy" ),
                    szAppName, MB_ICONERROR );
     }
     */

     hb_retl( FALSE );
     return;
   }

   pcd->bBusy = TRUE;

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   pcd->phiCodeBlock = hb_itemNew( phiCodeBlock );

   pcd->bBusy = FALSE;

   hb_retl( TRUE );
}

/*WVW_PBsetstyle( [nWinNum], nPBid, nStyle )
 *assign new style nStyle to button nPBid for window nWinNum
 *typical usage: nStyle==BS_DEFPUSHBUTTON (==01) to turn the button
 *                                               into default push button
 *                                               (thick border)
 *                       BS_PUSHBUTTON    (==00) to turn the button
 *                                               into regular push button
 *
 *using other styles like BS_MULTILINE may also be usefull,
 *but I haven't tried that
 *
 * this function always return .t.
 */
HB_FUNC( WVW_PBSETSTYLE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT  uiPBid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   ULONG ulStyle = (ULONG) ( ISNIL( 3 ) ? 0  : hb_parni( 3 ) );
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_PUSHBUTTON, NULL, uiPBid);

   if (pcd->hWndCtrl)
   {
      SendMessage(pcd->hWndCtrl, BM_SETSTYLE, (WPARAM) ulStyle, (LPARAM) TRUE);
   }

   hb_retl( TRUE );
}

/*WVW_PBSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 *this will initialize font for ALL pushbuttons in window nWinNum
 *(including ones created later on)
 */
HB_FUNC( WVW_PBSETFONT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   BOOL  retval = TRUE;

   s_lfPB.lfHeight         = ISNIL( 3 ) ? pWindowData->fontHeight - 2 : hb_parnl( 3 );
   s_lfPB.lfWidth          = ISNIL( 4 ) ? s_lfPB.lfWidth : hb_parni( 4 );
     s_lfPB.lfEscapement     = 0;
     s_lfPB.lfOrientation    = 0;
   s_lfPB.lfWeight         = ISNIL( 5 ) ? s_lfPB.lfWeight : hb_parni( 5 );
   s_lfPB.lfItalic         = ISNIL( 7 ) ? s_lfPB.lfItalic : hb_parl( 7 );
   s_lfPB.lfUnderline      = ISNIL( 8 ) ? s_lfPB.lfUnderline : hb_parl( 8 );
   s_lfPB.lfStrikeOut      = ISNIL( 9 ) ? s_lfPB.lfStrikeOut : hb_parl( 9 );
   s_lfPB.lfCharSet        = DEFAULT_CHARSET;

   s_lfPB.lfQuality        = ISNIL( 6 ) ? s_lfPB.lfQuality : hb_parni( 6 );
   s_lfPB.lfPitchAndFamily = FF_DONTCARE;
   if ( ISCHAR( 2 ) )
   {
      strcpy( s_lfPB.lfFaceName, hb_parcx( 2 ) );
   }

   if (pWindowData->hPBfont)
   {
      HFONT hOldFont = pWindowData->hPBfont;
      HFONT hFont = CreateFontIndirect( &s_lfPB );
      if (hFont)
      {
         CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

         while (pcd)
         {
           if ((pcd->byCtrlClass == WVW_CONTROL_PUSHBUTTON) &&
               ((HFONT) SendMessage( pcd->hWndCtrl, WM_GETFONT, (WPARAM) 0, (LPARAM) 0) == hOldFont)
              )
           {
              SendMessage( pcd->hWndCtrl, WM_SETFONT, (WPARAM) hFont, (LPARAM) TRUE);
           }

           pcd = pcd->pNext;
         }

         pWindowData->hPBfont = hFont;
         DeleteObject( (HFONT) hOldFont );

      }
      else
      {
         retval = FALSE;
      }
   }

   hb_retl( retval );

}

/*-------------------------------------------------------------------*/
/* PUSHBUTTON ends                                                   */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* PROGRESSBAR begins                                                 */
/*-------------------------------------------------------------------*/

/*WVW_PGcreate( [nWinNum], nTop, nLeft, nBottom, nRight, [aOffset],
 *                         [nBackColor], [nBarColor], [lSmooth], [lVertical])
 *create progress bar for window nWinNum
 *nTop: row of top/left corner (in character unit)
 *nLeft: col of top/left corner (in character unit)
 *nBottom: row of bottom/right corner (in character unit)
 *nRight: col of bottom/right corner (in character unit)
 *aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of progress bar. defaults: {0, 0, 0, 0}
 *nBackColor: color of background (as RGB value)
 *nBarColor: color of bar (as RGB value)
 *lSmooth: if .t., draw as smooth bar (default is .f.)
 *lVertical: if .t., draw as vertical progress bar (default is .f.)
 *
 *returns control id of newly created progress bar of windows nWinNum
 *returns 0 if failed
 *
 *example:
 *WVW_PGcreate( , 5, 10, 5, 30)
 *  :: creates horiz progressbar on current window at (5,10) to (5,30)
 *     colors using default ones.
 *
 *WVW_PGcreate( , 5, 10, 5, 30, {-1, 0, +1, 0} )
 *  :: same as above, but the bar is enlarged 1 pixel to the top
 *     and 1 pixel to the bottom
 *
 *NOTES:
 *ProgressRange is initially set as 0 - 100.
 *Initial ProgressPos is 0
 */

HB_FUNC( WVW_PGCREATE)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND hWndParent = pWindowData->hWnd;
   HWND hWndPG;
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   int   iStyle = 0;
   BOOL  bBackColor= !ISNIL(7);
   BOOL  bBarColor = !ISNIL(8);
   BOOL  bSmooth   = ( !ISLOG( 9 ) ? FALSE : hb_parl( 9 ) );
   BOOL  bVertical = ( !ISLOG(10 ) ? FALSE : hb_parl(10 ) );
   UINT  uiPGid;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );

   InitCommonControls();

   iOffTop    = !ISNIL( 6 ) ? hb_parni( 6,1 ) : 0;
   iOffLeft   = !ISNIL( 6 ) ? hb_parni( 6,2 ) : 0;
   iOffBottom = !ISNIL( 6 ) ? hb_parni( 6,3 ) : 0;
   iOffRight  = !ISNIL( 6 ) ? hb_parni( 6,4 ) : 0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + iOffTop ;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
   xy.y   -= pWindowData->byLineSpacing;
   iBottom = xy.y-1 + iOffBottom ;
   iRight  = xy.x-1 + iOffRight;

   uiPGid = LastControlId(usWinNum, WVW_CONTROL_PROGRESSBAR);
   if (uiPGid==0)
   {
     uiPGid = WVW_ID_BASE_PROGRESSBAR;
   }
   else
   {
     uiPGid++;
   }

   if (bVertical)
   {

      iStyle = iStyle | PBS_VERTICAL;
   }
   if (bSmooth)
   {

      iStyle = iStyle | PBS_SMOOTH;
   }

   hWndPG = CreateWindowEx(
       0L,
       PROGRESS_CLASS,
       (LPSTR) NULL,
       WS_CHILD | WS_VISIBLE | (DWORD) iStyle,
       iLeft,
       iTop,
       iRight-iLeft+1,
       iBottom-iTop+1,
       hWndParent,
       (HMENU) uiPGid,
       (HINSTANCE) hb_hInstance,
       (LPVOID) NULL
   );

   if(hWndPG)
   {
      RECT rXB = { 0 } , rOffXB = { 0 };

      if (bBackColor)
      {
         SendMessage( hWndPG, PBM_SETBKCOLOR, 0, (LPARAM) (COLORREF) hb_parnl(7) );
      }
      if (bBarColor)
      {
         SendMessage( hWndPG, PBM_SETBARCOLOR, 0, (LPARAM) (COLORREF) hb_parnl(8) );
      }

      SendMessage( hWndPG, PBM_SETRANGE, 0, MAKELPARAM(0, 100) );
      SendMessage( hWndPG, PBM_SETPOS, (WPARAM) 0, 0 );

      rXB.top = usTop;     rXB.left= usLeft;
      rXB.bottom=usBottom; rXB.right =usRight;
      rOffXB.top = iOffTop;     rOffXB.left= iOffLeft;
      rOffXB.bottom=iOffBottom; rOffXB.right =iOffRight;

      AddControlHandle(usWinNum, WVW_CONTROL_PROGRESSBAR, hWndPG, uiPGid, (HB_ITEM *) NULL, rXB, rOffXB, (byte) iStyle);

      hb_retnl( (LONG) uiPGid );
   }
   else
   {

      hb_retnl( (LONG) 0 );
   }
}

/*WVW_PGdestroy( [nWinNum], nPGid )
 *destroy progressbar nPGid for window nWinNum
 *This function has no return value.
 */
HB_FUNC( WVW_PGDESTROY)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   UINT uiPGid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev = NULL;

   while (pcd)
   {
     if (pcd->byCtrlClass == WVW_CONTROL_PROGRESSBAR && pcd->uiCtrlid == uiPGid)
     {
       break;
     }

     pcdPrev = pcd;
     pcd = pcd->pNext;
   }
   if (pcd==NULL) { return; }

   DestroyWindow (pcd->hWndCtrl) ;

   if (pcdPrev==NULL)
   {
     pWindowData->pcdCtrlList = pcd->pNext;
   }
   else
   {
     pcdPrev->pNext = pcd->pNext;
   }

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   hb_xfree( pcd );
}

/*WVW_PGsetrange(nWinNum, PGid, [nMin], [nMax])
 *update progressbar data range (default is 0-100)
 *nMin: a number in range of -32767 to +32767
 *nMax: a number in range of -32767 to +32767
 *
 *Remark: progress position is reset to nMin
 *
 *returns .t. if operation considered successfull
*/
HB_FUNC( WVW_PGSETRANGE )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiPGid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   byte bStyle;
   HWND hWndPG = FindControlHandle(usWinNum, WVW_CONTROL_PROGRESSBAR, uiPGid, &bStyle);
   int  iMin = (int) ( ISNIL(3) ? 0 : hb_parni( 3 ) );
   int  iMax = (int) ( ISNIL(4) ? 0 : hb_parni( 4 ) );

   if (uiPGid==0 || hWndPG==NULL || (iMin > iMax) )
   {
     hb_retl(FALSE);
     return;
   }

   SendMessage( hWndPG, PBM_SETRANGE, 0, MAKELPARAM(iMin, iMax) );
   SendMessage( hWndPG, PBM_SETPOS, (WPARAM) iMin, 0 );

   hb_retl(TRUE);
}

/*WVW_PGsetpos(nWinNum, PGid, [nPos])
 *update progressbar position within current range
 *nPos: a number in range of current range
 *returns .t. if operation considered successfull
*/
HB_FUNC( WVW_PGSETPOS )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiPGid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   byte bStyle;
   HWND hWndPG = FindControlHandle(usWinNum, WVW_CONTROL_PROGRESSBAR, uiPGid, &bStyle);
   int  iPos = (int) ( ISNIL(3) ? 0 : hb_parni( 3 ) );
   PBRANGE pbrange;

   if (uiPGid==0 || hWndPG==NULL)
   {
     hb_retl(FALSE);
     return;
   }

   SendMessage( hWndPG, PBM_GETRANGE, (WPARAM) TRUE, (LPARAM) &pbrange );

   if (iPos < pbrange.iLow || iPos > pbrange.iHigh)
   {
     hb_retl(FALSE);
     return;
   }

   SendMessage( hWndPG, PBM_SETPOS, (WPARAM) iPos, 0 );

   hb_retl(TRUE);
}

/*WVW_PGgetpos(nWinNum, PGid)
 *get progressbar current position
 *returns 0 if operation failed
*/
HB_FUNC( WVW_PGGETPOS )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiPGid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   byte bStyle;
   HWND hWndPG = FindControlHandle(usWinNum, WVW_CONTROL_PROGRESSBAR, uiPGid, &bStyle);

   if (uiPGid==0 || hWndPG==NULL)
   {
     hb_retni(0);
     return;
   }

   hb_retni( (int) SendMessage( hWndPG, PBM_GETPOS, (WPARAM) 0, (LPARAM) 0 ) );

}

/*-------------------------------------------------------------------*/
/* PROGRESSBAR ends                                                   */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*/
/* COMBOBOX begins (experimental)                                    */
/*-------------------------------------------------------------------*/

static LRESULT CALLBACK hb_wvw_gtCBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
  HWND hWndParent = GetParent(hWnd);
  USHORT usWinNum;

  UINT uiCBid;
  WNDPROC OldProc;
  BYTE bKbdType;

  if (hWndParent==NULL)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  for(usWinNum=0; usWinNum<s_usNumWindows; usWinNum++)
  {
    if (s_pWindows[usWinNum]->hWnd == hWndParent)
    {
     break;
    }
  }
  if(usWinNum>=s_usNumWindows)
  {

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  uiCBid = (UINT) FindControlId (usWinNum, WVW_CONTROL_COMBOBOX, hWnd, &bKbdType) ;

  if (uiCBid==0)
  {
    MessageBox( NULL, TEXT( "Failed FindControlId" ),
                szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  OldProc = GetControlProc(usWinNum, WVW_CONTROL_COMBOBOX, hWnd);
  if (OldProc == NULL)
  {
    MessageBox( NULL, TEXT( "Failed GetControlProc" ),
                szAppName, MB_ICONERROR );

    return( DefWindowProc( hWnd, message, wParam, lParam ) );
  }

  switch ( message )
  {

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
    {
      BOOL bAlt      = GetKeyState( VK_MENU ) & 0x8000;
      BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
      BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
      int  c = (int) wParam;
      BOOL bDropped;

      if ( !hb_wvw_gtBufferedKey( (LONG) wParam ) )
      {
        break;
      }

      bDropped = SendMessage( (HWND) hWnd,
                              CB_GETDROPPEDSTATE,
                              (WPARAM) 0,
                              (LPARAM) 0
                             );

      if (bKbdType==WVW_CB_KBD_STANDARD)
      {

        switch ( c )
        {

          case VK_F4:
          {
            if (bAlt)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          case VK_ESCAPE:
          {
            if (!bCtrl && !bAlt && !bShift && !bDropped)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          case VK_TAB:
          {
            if (!bCtrl && !bAlt)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          case VK_RETURN:
          {
            if (!bCtrl && !bAlt && !bShift && !bDropped)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          default:
          {
            break;
          }
        }

        break;

      } /* WVW_CB_KBD_STANDARD */
      else /* assume WVW_CB_KBD_CLIPPER */
      {
        switch(c)
        {

          case VK_F4:
          {
            if (bAlt)
            {
               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }

          case VK_RETURN:
          {

            if (bDropped || bAlt || bShift || bCtrl)
            {

              break;
            }
            else
            {

              if (!bDropped)
              {
                SendMessage(
                  (HWND) hWnd,
                  CB_SHOWDROPDOWN,
                  (WPARAM) TRUE,
                  (LPARAM) 0
                );
                return 0;
              }
              else
              {

                SetFocus(hWndParent);
                PostMessage(hWndParent, message, wParam, lParam);
                return 0;
              }
            }
          }

          case VK_ESCAPE:
          {
            if (bDropped || bAlt || bShift || bCtrl)
            {

              break;
            }
            else
            {
              SetFocus(hWndParent);
              PostMessage(hWndParent, message, wParam, lParam);
              return 0;
            }
          }

          case VK_UP:
          case VK_DOWN:
          case VK_RIGHT:
          case VK_LEFT:
          case VK_HOME:
          case VK_END:
          case VK_PRIOR:
          case VK_NEXT:
          {
            if (bDropped)
            {

              break;
            }
            else
            {
              SetFocus(hWndParent);
              PostMessage(hWndParent, message, wParam, lParam);
              return 0;
            }
          }

          case VK_TAB:
          {
            if (!bCtrl && !bAlt )
            {

               SetFocus(hWndParent);
               PostMessage(hWndParent, message, wParam, lParam);
               return 0;
            }
            break;
          }
        }
        break;

      }
    }

  }

  return( CallWindowProc( (WNDPROC) OldProc, hWnd, message, wParam, lParam ) );
}

/*WVW_CBcreate( [nWinNum], nTop, nLeft, nWidth, aText, bBlock, nListLines, ;
 *                         nReserved, nKbdType, aOffset)
 *create combobox (drop-down list, no editbox) for window nWinNum
 *nTop: row of top/left corner (in character unit)
 *nLeft: col of top/left corner (in character unit)
 *nWidth: width of combobox (in character unit)
 *aText: array of drop-down list members, default = {"empty"}
 *       eg. {"yes","no"}
 *bBlock: codeblock to execute on these events:
 *         event=CBN_SELCHANGE(1): user changes selection
                        (not executed if selection
                        is changed programmatically)
 *         event=CBN_SETFOCUS
 *         event=CBN_KILLFOCUS
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nCBid  : combobox id
 *         nType  : event type (CBN_SELCHANGE/CBN_SETFOCUS/CBN_KILLFOCUS supported)
 *         nIndex : index of the selected list item (0 based)
 *nListLines: number of lines for list items, default = 3
 *            (will be automatically truncated if it's > len(aText))
 *nReserved: reserved for future (this parm is now ignored)
 *
 *nKbdType: WVW_CB_KBD_STANDARD (0): similar to standard windows convention
 *            ENTER/ESC: will kill focus from combobox
 *          WVW_CB_KBD_CLIPPER (1):
 *            ENTER: drop (show) the list box
 *            UP/DOWN/TAB/SHIFTTAB/ESC: kill focus
 *default is WVW_CB_KBD_STANDARD (0)
 *
 *aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of combobox.
 *         defaults: {-2,-2,+2,+2}
 *         NOTES: the third element (y2) is actually ignored.
 *                height of combobox is always 1 char height
 *                (see also wvw_cbSetFont())
 *
 *returns control id of newly created combobox of windows nWinNum
 *returns 0 if failed
 *
 *example:
 */

HB_FUNC( WVW_CBCREATE)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   HWND hWndParent = pWindowData->hWnd;
   HWND hWndCB;

   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   UINT uiCBid;
   USHORT   usWidth  = hb_parni( 4 );
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = usTop,
            usRight  = usLeft + usWidth - 1;
   USHORT   usNumElement = ISARRAY(5) ? hb_arrayLen( hb_param(5, HB_IT_ARRAY) ) : 0;
   USHORT   usListLines  = ISNUM(7) ? hb_parni(7) : 3;
   BYTE     byCharHeight = hb_wvw_LineHeight(pWindowData);

   /* in the future combobox type might be selectable by 8th parameter */
   int      iStyle = CBS_DROPDOWNLIST | WS_VSCROLL;
   BYTE     bKbdType = ISNUM(9) ? hb_parni(9) : WVW_CB_KBD_STANDARD;

   if (pWindowData->hCBfont==NULL)
   {
      pWindowData->hCBfont = CreateFontIndirect( &s_lfCB );
      if (pWindowData->hCBfont==NULL)
      {
        hb_retnl(0);
        return;
      }
   }

   iOffTop    = !ISNIL( 10 ) ? hb_parni( 10,1 ) : 0;
   iOffLeft   = !ISNIL( 10 ) ? hb_parni( 10,2 ) : 0;

   iOffBottom = usListLines;
   iOffRight  = !ISNIL( 10 ) ? hb_parni( 10,4 ) : 0;

   if (s_bMainCoordMode)
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y + iOffTop ;
   iLeft   = xy.x + iOffLeft;

   xy      = hb_wvw_gtGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + (iOffBottom * byCharHeight);
   iRight  = xy.x - 1 + iOffRight;

   uiCBid = LastControlId(usWinNum, WVW_CONTROL_COMBOBOX);
   if (uiCBid==0)
   {
     uiCBid = WVW_ID_BASE_COMBOBOX;
   }
   else
   {
     uiCBid++;
   }

   InitCommonControls();

   hWndCB = CreateWindowEx(
       0L,
       "COMBOBOX",
       (LPSTR) NULL,
       WS_CHILD | WS_VISIBLE | (DWORD) iStyle,
       iLeft,
       iTop,
       iRight-iLeft+1,
       iBottom-iTop+1,
       hWndParent,
       (HMENU) uiCBid,
       (HINSTANCE) hb_hInstance,
       (LPVOID) NULL
   );

   if(hWndCB)
   {
     RECT rXB = { 0 }, rOffXB = { 0 };
     WNDPROC OldProc;
     USHORT i;
     TCHAR szDefault[] = TEXT( "empty" );

     SendMessage(
       (HWND) hWndCB,
       WM_SETREDRAW,
       (WPARAM) TRUE,
       (LPARAM) 0
     );

     if (usNumElement==0)
     {
       if (SendMessage((HWND) hWndCB,
                    CB_ADDSTRING,
                    (WPARAM) 0,
                    (LPARAM) (LPCTSTR) szDefault
                  ) < 0)
       {
         /* ignore failure */

       }
     }
     else
     {
       for(i=1; i<=usNumElement; i++)
       {
         if (SendMessage((HWND) hWndCB,
                      CB_ADDSTRING,
                      (WPARAM) 0,
                      (LPARAM) (LPCTSTR) hb_parcx( 5, i)
                    ) < 0)
         {
           /* ignore failure */

         }
       }
     }

     SendMessage(
       (HWND) hWndCB,
       CB_SETCURSEL,
       (WPARAM) 0,
       (LPARAM) 0
     );

     SendMessage(
       (HWND) hWndCB,
       CB_SETEXTENDEDUI,
       (WPARAM) TRUE,
       (LPARAM) 0
     );

     rXB.top = usTop;     rXB.left= usLeft;
     rXB.bottom=usBottom; rXB.right =usRight;
     rOffXB.top = iOffTop;     rOffXB.left= iOffLeft;

     rOffXB.bottom=iOffBottom; rOffXB.right =iOffRight;

     AddControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, hWndCB, uiCBid, (HB_ITEM *) hb_param( 6, HB_IT_BLOCK ), rXB, rOffXB, (byte) bKbdType);

     OldProc = (WNDPROC) SetWindowLong (hWndCB,
                                        GWL_WNDPROC, (LONG) hb_wvw_gtCBProc) ;

     StoreControlProc(usWinNum, WVW_CONTROL_COMBOBOX, hWndCB, OldProc);

     SendMessage( hWndCB, WM_SETFONT, (WPARAM) pWindowData->hCBfont, (LPARAM) TRUE);

     hb_retnl( (LONG) uiCBid );
   }
   else
   {

     hb_retnl( (LONG) 0 );
   }
}

/*WVW_CBdestroy( [nWinNum], nCBid )
 *destroy combobox nCBid for window nWinNum
 */
HB_FUNC( WVW_CBDESTROY)
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];
   UINT uiCBid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev = (CONTROL_DATA *) NULL;

   while (pcd)
   {
     if (pcd->byCtrlClass == WVW_CONTROL_COMBOBOX && pcd->uiCtrlid == uiCBid)
     {
       break;
     }

     pcdPrev = pcd;
     pcd = pcd->pNext;
   }
   if (pcd==NULL) { return; }

   DestroyWindow (pcd->hWndCtrl) ;

   if (pcdPrev==NULL)
   {
     pWindowData->pcdCtrlList = pcd->pNext;
   }
   else
   {
     pcdPrev->pNext = pcd->pNext;
   }

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   hb_xfree( pcd );
}

/*WVW_CBsetFocus( [nWinNum], nComboId )
 *set the focus to combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBSETFOCUS )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = ISNIL(2) ? 0 : hb_parni(2);
  byte   bStyle;
  HWND   hWndCB = FindControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, uiCtrlId, &bStyle);

  if (hWndCB)
  {
    hb_retl(hWndCB == SetFocus(hWndCB));
  }
  else
  {
    hb_retl(FALSE);
  }
}

/*WVW_CBisFocused( [nWinNum], nComboId )
 *returns .t. if the focus is on combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBISFOCUSED )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = ISNIL(2) ? 0 : hb_parni(2);
  byte   bStyle;
  HWND   hWndCB = FindControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, uiCtrlId, &bStyle);

  hb_retl((HWND) GetFocus() == hWndCB);
}

/*WVW_CBenable( [nWinNum], nComboId, [lEnable] )
 *enable/disable button nComboId on window nWinNum
 *(lEnable defaults to .t., ie. enabling the combobox)
 *return previous state of the combobox (TRUE:enabled FALSE:disabled)
 *(if nComboId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_CBENABLE )
{
  USHORT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = ISNIL(3) ? TRUE : hb_parl(3);
  byte   bStyle;
  HWND   hWndCB = FindControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, uiCtrlId, &bStyle);

  if (hWndCB)
  {
    hb_retl( EnableWindow(hWndCB, bEnable)==0 );

    if (!bEnable)
    {
       SetFocus( s_pWindows[ usWinNum ]->hWnd );
    }
  }
  else
  {
    hb_retl(FALSE);
  }
}

/*WVW_CBsetcodeblock( [nWinNum], nCBid, bBlock )
 *assign (new) codeblock bBlock to combobox nCBid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_CBSETCODEBLOCK )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = (UINT) ( ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   PHB_ITEM phiCodeBlock = hb_param( 3, HB_IT_BLOCK );
   if (!phiCodeBlock || pcd==NULL || pcd->bBusy)
   {
     hb_retl( FALSE );
     return;
   }

   pcd->bBusy = TRUE;

   if (pcd->phiCodeBlock)
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   pcd->phiCodeBlock = hb_itemNew( phiCodeBlock );

   pcd->bBusy = FALSE;

   hb_retl( TRUE );
}

/*WVW_CBSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 *this will initialize font for ALL comboboxes in window nWinNum
 *(including ones created later on)
 *
 *TODO: ? should nHeight be ignored, and always forced to use standard char height?
 */
HB_FUNC( WVW_CBSETFONT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = s_pWindows[usWinNum];

   BOOL  retval = TRUE;

   s_lfCB.lfHeight         = ISNIL( 3 ) ? pWindowData->fontHeight - 2 : hb_parnl( 3 );
   s_lfCB.lfWidth          = ISNIL( 4 ) ? s_lfCB.lfWidth : hb_parni( 4 );
     s_lfCB.lfEscapement     = 0;
     s_lfCB.lfOrientation    = 0;
   s_lfCB.lfWeight         = ISNIL( 5 ) ? s_lfCB.lfWeight : hb_parni( 5 );
   s_lfCB.lfItalic         = ISNIL( 7 ) ? s_lfCB.lfItalic : hb_parl( 7 );
   s_lfCB.lfUnderline      = ISNIL( 8 ) ? s_lfCB.lfUnderline : hb_parl( 8 );
   s_lfCB.lfStrikeOut      = ISNIL( 9 ) ? s_lfCB.lfStrikeOut : hb_parl( 9 );
   s_lfCB.lfCharSet        = DEFAULT_CHARSET;

   s_lfCB.lfQuality        = ISNIL( 6 ) ? s_lfCB.lfQuality : hb_parni( 6 );
   s_lfCB.lfPitchAndFamily = FF_DONTCARE;
   if ( ISCHAR( 2 ) )
   {
      strcpy( s_lfCB.lfFaceName, hb_parcx( 2 ) );
   }

   if (pWindowData->hCBfont)
   {
      HFONT hOldFont = pWindowData->hCBfont;
      HFONT hFont = CreateFontIndirect( &s_lfCB );
      if (hFont)
      {
         CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

         while (pcd)
         {
           if ((pcd->byCtrlClass == WVW_CONTROL_COMBOBOX) &&
               ((HFONT) SendMessage( pcd->hWndCtrl, WM_GETFONT, (WPARAM) 0, (LPARAM) 0) == hOldFont)
              )
           {
              SendMessage( pcd->hWndCtrl, WM_SETFONT, (WPARAM) hFont, (LPARAM) TRUE);

           }

           pcd = pcd->pNext;
         }

         pWindowData->hCBfont = hFont;
         DeleteObject( (HFONT) hOldFont );

      }
      else
      {
         retval = FALSE;
      }
   }

   hb_retl( retval );

}

/*WVW_CBsetIndex( [nWinNum], nCBid, nIndex )
 *set current selection of combobox nCBid in window nWinNum to nIndex
 *(nIndex is 0 based)
 *returns .t. if successful.
 *
 *NOTE: the better name to this function should be WVW_CBsetCurSel()
 *      but that name is already used.
 *      (WVW_CBsetcursel() and WVW_CBaddstring() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBSETINDEX )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   int iIndex = hb_parni(3);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   BOOL  retval;

   if (pcd==NULL || iIndex < 0)
   {
     hb_retl(FALSE);
     return;
   }

   retval = (SendMessage((HWND) pcd->hWndCtrl,
                         CB_SETCURSEL,
                         (WPARAM) iIndex,
                         (LPARAM) 0
                         )==iIndex);
   hb_retl(retval);
}

/*WVW_CBgetIndex( [nWinNum], nCBid )
 *get current selection of combobox nCBid in window nWinNum
 *return nIndex (0 based)
 *returns CB_ERR (-1) if none selected
 *
 *NOTE: the better name to this function should be WVW_CBgetCurSel()
 *      but that name is potentially misleading to WVW_CBsetCursel
 *      which is not our family of WVW_CB* functions
 *      (WVW_CBsetcursel() and WVW_CBaddstring() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBGETINDEX )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   int retval;

   if (pcd==NULL)
   {
     hb_retni(CB_ERR);
     return;
   }

   retval = SendMessage((HWND) pcd->hWndCtrl,
                        CB_GETCURSEL,
                        (WPARAM) 0,
                        (LPARAM) 0
                        );
   hb_retni(retval);
}

/*WVW_CBfindString( [nWinNum], nCBid, cString )
 *find index of cString in combobox nCBid in window nWinNum
 *returns index of cString (0 based)
 *returns CB_ERR (-1) if string not found
 *
 *NOTE:case insensitive
 */
HB_FUNC( WVW_CBFINDSTRING )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   int retval;

   if (pcd==NULL)
   {
     hb_retni(CB_ERR);
     return;
   }

   retval = SendMessage((HWND) pcd->hWndCtrl,
                         CB_FINDSTRING,
                         (WPARAM) -1,
                         (LPARAM) (LPCSTR) hb_parcx(3)
                         );
   hb_retni(retval);
}

/*WVW_cbGetCurText( [nWinNum], nCBid )
 *get current selected cString in combobox nCBid in window nWinNum
 *returns "" if none selected
 *
 */
HB_FUNC( WVW_CBGETCURTEXT )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   int iCurSel,iTextLen;
   LPTSTR lptstr = NULL;

   if (pcd==NULL)
   {
     hb_retclen(lptstr,0);
     return;
   }

   iCurSel = SendMessage((HWND) pcd->hWndCtrl,
                        CB_GETCURSEL,
                        (WPARAM) 0,
                        (LPARAM) 0
                        );
   iTextLen = SendMessage((HWND) pcd->hWndCtrl,
                          CB_GETLBTEXTLEN,
                          (WPARAM) iCurSel,
                          (LPARAM) 0
                          );
   if (iTextLen==CB_ERR)
   {
     hb_retclen(lptstr, 0);
     return;
   }

   lptstr = ( char* ) hb_xgrab( iTextLen+1 );
   if (SendMessage((HWND) pcd->hWndCtrl,
                   CB_GETLBTEXT,
                   (WPARAM) iCurSel,
                   (LPARAM) lptstr
                   )==CB_ERR)
   {
     hb_retclen(lptstr, 0);
   }
   else
   {
     hb_retc(lptstr);
   }
   hb_xfree( lptstr );
}

/*WVW_cbIsDropped( [nWinNum], nCBid )
 *get current dropped state of combobox nCBid in window nWinNum
 *returns .t. if listbox is being shown, otherwise .f.
 *Also returns .f. if nCBid not valid
 */
HB_FUNC( WVW_CBISDROPPED )
{
   USHORT usWinNum = WVW_WHICH_WINDOW;

   UINT uiCBid = hb_parni(2);
   CONTROL_DATA * pcd = GetControlData(usWinNum, WVW_CONTROL_COMBOBOX, NULL, uiCBid);
   BOOL bDropped;

   if (pcd==NULL)
   {
      hb_retl(FALSE);
      return;
   }

   bDropped = SendMessage( (HWND) pcd->hWndCtrl,
                           CB_GETDROPPEDSTATE,
                           (WPARAM) 0,
                           (LPARAM) 0
                          );
   hb_retl(bDropped);
}

/*-------------------------------------------------------------------*/
/* COMBOBOX ends (experimental)                                    */
/*-------------------------------------------------------------------*/

/*-------------------------------------------------------------------*
 *-------------------------------------------------------------------*
 *-------------------------------------------------------------------*
 *
 *             Direct WinApi Functions - Prefixed WIN_*()
 *            Original work of Pritpal Bedi on WVTUTILS.C
 *
 *TODO: should be moved to separate modul. totally independent of GT.
 *
 *-------------------------------------------------------------------*
 *-------------------------------------------------------------------*
 *-------------------------------------------------------------------*/

HB_FUNC( WIN_SENDMESSAGE )
{
   char *cText = NULL;

   if( ISBYREF( 4 ) )
   {
      cText = ( char* ) hb_xgrab( hb_parcsiz( 4 ) );
      hb_xmemcpy( cText, hb_parcx( 4 ), hb_parcsiz( 4 ) );
   }

   hb_retnl( ( ULONG ) SendMessage( ( HWND ) hb_parnl( 1 ),
                                    ( UINT ) hb_parni( 2 ),
                                    ( ISNIL( 3 ) ? 0 : ( WPARAM ) hb_parnl( 3 ) ),
                                    ( ISNIL( 4 ) ? 0 : ( ISBYREF( 4 ) ? ( LPARAM ) ( LPSTR ) cText :
                                       ( ISCHAR( 4 ) ? ( LPARAM )( LPSTR ) hb_parcx( 4 ) :
                                           ( LPARAM ) hb_parnl( 4 ) ) ) ) )
           );

   if ( ISBYREF( 4 ) )
   {
      hb_storclen( cText, hb_parcsiz( 4 ), 4 );
      hb_xfree( cText );
   }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SENDDLGITEMMESSAGE )
{
   char     *cText;
   PHB_ITEM pText = hb_param( 5, HB_IT_STRING );

   if( pText )
   {
      cText = ( char* ) hb_xgrab( pText->item.asString.length + 1 );
      hb_xmemcpy( cText, pText->item.asString.value, pText->item.asString.length + 1 );
   }
   else
   {
      cText = NULL;
   }

   hb_retnl( ( LONG ) SendDlgItemMessage( ( HWND ) hb_parnl( 1 ) ,
                                          ( int  ) hb_parni( 2 ) ,
                                          ( UINT ) hb_parni( 3 ) ,
                                          ( ISNIL( 4 ) ? 0 : ( WPARAM ) hb_parnl( 4 ) ),
                                          ( cText ? ( LPARAM ) cText : ( LPARAM ) hb_parnl( 5 ) )
                                        )
           );

  if( pText )
  {
     hb_storclen( cText, pText->item.asString.length, 5 ) ;
  }

  if( cText )
  {
     hb_xfree( cText );
  }
}

/*-------------------------------------------------------------------
 *
 *  WIN_SetTimer( hWnd, nIdentifier, nTimeOut )
 */

HB_FUNC( WIN_SETTIMER )
{
   hb_retl( SetTimer( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ), NULL ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETFOCUS )
{
   SetFocus( ( HWND ) hb_parnl( 1 ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETTEXTCOLOR )
{
   hb_retnl( ( ULONG ) SetTextColor( ( HDC ) hb_parnl( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETBKCOLOR )
{
   hb_retnl( ( ULONG ) SetBkColor( ( HDC ) hb_parnl( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETBKMODE )
{
   hb_retni( ( int ) SetBkMode( ( HDC ) hb_parnl( 1 ), hb_parni( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_GETSTOCKOBJECT )
{
   hb_retnl( ( ULONG ) GetStockObject( hb_parnl( 1 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HGDIOBJ ) hb_parnl( 1 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SELECTOBJECT )
{
   hb_retnl( ( ULONG ) SelectObject( ( HDC ) hb_parnl( 1 ), ( HGDIOBJ ) hb_parnl( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_LOWORD )
{
   hb_retnl( LOWORD( hb_parnl( 1 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_HIWORD )
{
   hb_retnl( HIWORD( hb_parnl( 1 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_MULDIV )
{
   hb_retni( MulDiv( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_GETDIALOGBASEUNITS )
{
   hb_retnl( ( LONG ) GetDialogBaseUnits() ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETMENU )
{
   SetMenu( ( HWND ) hb_parnl( 1 ), ( HMENU ) hb_parni( 2 ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETDLGITEMTEXT )
{
   SetDlgItemText( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ), hb_parc( 3 ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_GETDLGITEMTEXT )
{
   USHORT iLen = SendMessage( GetDlgItem( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ) ), WM_GETTEXTLENGTH, 0, 0 ) + 1 ;
   char *cText = ( char* ) hb_xgrab( iLen );

   GetDlgItemText( ( HWND ) hb_parnl( 1 ),
                   hb_parni( 2 ),
                   ( LPTSTR ) cText,
                   iLen
                 );

   hb_retc( cText );
   hb_xfree( cText );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_CHECKDLGBUTTON )
{
   hb_retl( CheckDlgButton( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ),
                            ISNUM( 3 ) ? hb_parni( 3 ) : ( UINT ) hb_parl( 3 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_ISDLGBUTTONCHECKED )
{
   hb_retni( IsDlgButtonChecked( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ) ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_CHECKRADIOBUTTON )
{
    hb_retl( CheckRadioButton( ( HWND ) hb_parnl( 1 ),
                                        hb_parni( 2 ),
                                        hb_parni( 3 ),
                                        hb_parni( 4 )
                              ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_GETDLGITEM )
{
   hb_retnl( ( ULONG ) GetDlgItem( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_MESSAGEBOX )
{
   hb_retni( MessageBox( ( HWND ) hb_parnl( 1 ), hb_parcx( 2 ), hb_parcx( 3 ), ISNIL( 4 ) ? MB_OK : hb_parni( 4 ) ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_INVALIDATERECT )
{
   InvalidateRect( ( HWND ) hb_parnl( 1 ), NULL, TRUE );
}

/*-------------------------------------------------------------------
 *
 *  Win_LoadIcon( ncIcon )
 */

HB_FUNC( WIN_LOADICON )
{
   HICON hIcon;

   if ( ISNUM( 1 ) )
   {
      hIcon = LoadIcon( ( HINSTANCE ) hb_hInstance, MAKEINTRESOURCE( hb_parni( 1 ) ) );
   }
   else
   {
      hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, hb_parc( 1 ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
   }

   hb_retnl( ( ULONG ) hIcon ) ;
}

/*-------------------------------------------------------------------
 *
 *  Win_LoadImage( ncImage, nSource ) -> hImage
 *    nSource == 0 ResourceIdByNumber
 *    nSource == 0 ResourceIdByName
 *    nSource == 0 ImageFromDiskFile
 */

HB_FUNC( WIN_LOADIMAGE )
{
   HBITMAP hImage = NULL;
   int     iSource = hb_parni( 2 );

   switch ( iSource )
   {
      case 0:
      {
         hImage = LoadBitmap( ( HINSTANCE ) hb_hInstance, MAKEINTRESOURCE( hb_parni( 1 ) ) );
      }
      break;

      case 1:
      {
         hImage = LoadBitmap( ( HINSTANCE ) hb_hInstance, hb_parc( 1 ) );
      }
      break;

      case 2:
      {
         hImage = ( HBITMAP ) LoadImage( ( HINSTANCE ) NULL, hb_parc( 1 ), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE );
      }
      break;
   }

   hb_retnl( ( ULONG ) hImage ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_GETCLIENTRECT )
{
   RECT     rc = { 0 };
   HB_ITEM  info;
   HB_ITEM  temp;

   GetClientRect( ( HWND ) hb_parnl( 1 ), &rc );

   info.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &info, 4 );

   hb_arraySetForward( &info, 1, hb_itemPutNI( &temp, rc.left   ) );
   hb_arraySetForward( &info, 2, hb_itemPutNI( &temp, rc.top    ) );
   hb_arraySetForward( &info, 3, hb_itemPutNI( &temp, rc.right  ) );
   hb_arraySetForward( &info, 4, hb_itemPutNI( &temp, rc.bottom ) );

   hb_itemReturn( &info );
}

/*-------------------------------------------------------------------
 *
 *    Win_DrawImage( hdc, nLeft, nTop, nWidth, nHeight, cImage ) in Pixels
 */

/* sorry, not supported in GTWVW
HB_FUNC( WIN_DRAWIMAGE )
{
   hb_retl( hb_wvt_DrawImage( ( HDC ) hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                                   hb_parni( 4 ), hb_parni( 5 ), hb_parc( 6 ) ) );
}
*/

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_GETDC )
{
   hb_retnl( ( ULONG ) GetDC( ( HWND ) hb_parnl( 1 )  ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_RELEASEDC )
{
   hb_retl( ReleaseDC( ( HWND ) hb_parnl( 1 ), ( HDC ) hb_parnl( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_RECTANGLE )
{
   Rectangle( ( HDC ) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_CREATEBRUSH )
{
   LOGBRUSH lb = { 0 };

   lb.lbStyle = hb_parni( 1 );
   lb.lbColor = ISNIL( 2 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 2 ) ;
   lb.lbHatch = ISNIL( 3 ) ? 0 : hb_parni( 3 );

   hb_retnl( ( ULONG ) CreateBrushIndirect( &lb ) );
}

/*-------------------------------------------------------------------
 *
 *   Win_DrawText( hDC, cText, aRect, nFormat )
 */

HB_FUNC( WIN_DRAWTEXT )
{
   RECT rc = { 0 };

   rc.left   = hb_parni( 3,1 );
   rc.top    = hb_parni( 3,2 );
   rc.right  = hb_parni( 3,3 );
   rc.bottom = hb_parni( 3,4 );

   hb_retl( DrawText( ( HDC ) hb_parnl( 1 ), hb_parc( 2 ), strlen( hb_parc( 2 ) ), &rc, hb_parni( 4 ) ) );
}

