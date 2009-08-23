/*
* $Id: gtwvw.c,v 1.48 2008/11/19 05:24:51 andijahja Exp $
 */
/*
 * WVWDRAW.C
 * Video subsystem for Win32 using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw draw functions
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
 *    hb_gt_wvw_Tone()
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

#include "hbole.h"
#include <tchar.h>
#include <stdlib.h>

#define TTS_BALLOON             0x40 // added by MAG
#define HB_OS_WIN_32_USED

#define WINVER 0x0500
#define _WIN32_WINNT 0x0500

  #ifndef _WIN32_IE
    #define _WIN32_IE 0x0400
#endif

#include "hbgtwvw.h"

#include <windows.h>
#include <commctrl.h>


HB_FUNC( WVW_YESCLOSE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );   
   HMENU  hMenu = GetSystemMenu( pWindowData->hWnd, FALSE);

   if (hMenu)
   {
      AppendMenu( hMenu, SC_CLOSE, MF_BYCOMMAND, "");
      DrawMenuBar( pWindowData->hWnd );
   }
}
HB_FUNC( WIN_SENDMESSAGE )
{
   char *cText = NULL;

   if( ISBYREF( 4 ) )
   {
      cText = ( char* ) hb_xgrab( hb_parcsiz( 4 ) );
      hb_xmemcpy( cText, hb_parcx( 4 ), hb_parcsiz( 4 ) );
   }

   hb_retnl( ( ULONG ) SendMessage( ( HWND ) HB_PARHANDLE( 1 ),
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

   hb_retnl( ( LONG ) SendDlgItemMessage( ( HWND ) HB_PARHANDLE( 1 ) ,
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
   hb_retl( SetTimer( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parni( 3 ), NULL ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETFOCUS )
{
   SetFocus( ( HWND ) HB_PARHANDLE( 1 ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETTEXTCOLOR )
{
   hb_retnl( ( ULONG ) SetTextColor( ( HDC ) HB_PARHANDLE( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETBKCOLOR )
{
   hb_retnl( ( ULONG ) SetBkColor( ( HDC ) HB_PARHANDLE( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETBKMODE )
{
   hb_retni( ( int ) SetBkMode( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_GETSTOCKOBJECT )
{
   hb_retnl( ( ULONG ) GetStockObject( hb_parnl( 1 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HGDIOBJ ) HB_PARHANDLE( 1 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SELECTOBJECT )
{
   hb_retnl( ( ULONG ) SelectObject( ( HDC ) HB_PARHANDLE( 1 ), ( HGDIOBJ ) HB_PARHANDLE( 2 ) ) );
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


/*-------------------------------------------------------------------*/

HB_FUNC( WIN_SETDLGITEMTEXT )
{
   SetDlgItemText( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parc( 3 ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_GETDLGITEMTEXT )
{
   USHORT iLen = (USHORT) SendMessage( (HWND)GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) , (UINT)WM_GETTEXTLENGTH, (WPARAM)0, (LPARAM)0 ) + 1 ;
   char *cText = ( char* ) hb_xgrab( iLen +1  );

   GetDlgItemText( ( HWND ) HB_PARHANDLE( 1 ),
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
   hb_retl( CheckDlgButton( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ),
                            ISNUM( 3 ) ? hb_parni( 3 ) : ( UINT ) hb_parl( 3 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_ISDLGBUTTONCHECKED )
{
   hb_retni( IsDlgButtonChecked( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_CHECKRADIOBUTTON )
{
    hb_retl( CheckRadioButton( ( HWND ) HB_PARHANDLE( 1 ),
                                        hb_parni( 2 ),
                                        hb_parni( 3 ),
                                        hb_parni( 4 )
                              ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_GETDLGITEM )
{
   hb_retnl( ( ULONG ) GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_MESSAGEBOX )
{
   hb_retni( MessageBox( ( HWND ) HB_PARHANDLE( 1 ), hb_parcx( 2 ), hb_parcx( 3 ), ISNIL( 4 ) ? MB_OK : hb_parni( 4 ) ) ) ;
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_INVALIDATERECT )
{
   InvalidateRect( ( HWND ) HB_PARHANDLE( 1 ), NULL, TRUE );
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
   PHB_ITEM  info = hb_itemArrayNew(4);
   PHB_ITEM  temp = hb_itemNew(NULL);

   GetClientRect( ( HWND ) HB_PARHANDLE( 1 ), &rc );

   hb_arraySet( info, 1, hb_itemPutNI( temp, rc.left   ) );
   hb_arraySet( info, 2, hb_itemPutNI( temp, rc.top    ) );
   hb_arraySet( info, 3, hb_itemPutNI( temp, rc.right  ) );
   hb_arraySet( info, 4, hb_itemPutNI( temp, rc.bottom ) );

   hb_itemRelease( temp );
   hb_itemReturn( info );
   hb_itemRelease( info );
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
   HB_RETHANDLE( GetDC( ( HWND ) HB_PARHANDLE( 1 )  ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_RELEASEDC )
{
   hb_retl( ReleaseDC( ( HWND ) HB_PARHANDLE( 1 ), ( HDC ) HB_PARHANDLE( 2 ) ) );
}

/*-------------------------------------------------------------------*/

HB_FUNC( WIN_RECTANGLE )
{
   Rectangle( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
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

   hb_retl( DrawText( ( HDC ) HB_PARHANDLE( 1 ), hb_parc( 2 ), strlen( hb_parc( 2 ) ), &rc, hb_parni( 4 ) ) );
}


///////////////////////////////////////////////////////////////////
//                                                               //
// Adiciones a GtWVW desarrolladas por SOLUCIONES PERCEPTIVAS... //
//                                                               //
///////////////////////////////////////////////////////////////////

HB_FUNC( WVW_GBCREATE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   // int   iStyle;
   UINT uiPBid;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   LPCTSTR  lpszCaption = ISCHAR(6) ? hb_parcx(6) : NULL;
   char   * szBitmap = ISCHAR(7) ? (char*) hb_parcx(7) : NULL;
   UINT     uiBitmap = ISNUM(7) ? (UINT) hb_parni(7) : 0;
   double   dStretch = !ISNIL(10) ? hb_parnd(10) : 1;
   BOOL     bMap3Dcolors = ISLOG(11) ? (BOOL) hb_parl(11) : FALSE;

   iOffTop    = !ISNIL( 9 ) ? hb_parni( 9,1 ) : -1 ;
   iOffLeft   = !ISNIL( 9 ) ? hb_parni( 9,2 ) : -1 ;
   iOffBottom = !ISNIL( 9 ) ? hb_parni( 9,3 ) : +1 ;
   iOffRight  = !ISNIL( 9 ) ? hb_parni( 9,4 ) : +1;

   uiPBid = ButtonCreate( usWinNum, usTop, usLeft, usBottom, usRight, lpszCaption,
                          szBitmap, uiBitmap, hb_param( 8, HB_IT_BLOCK ),
                          iOffTop, iOffLeft, iOffBottom, iOffRight,
                          dStretch, bMap3Dcolors,
                          BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE );
   hb_retnl( (LONG) uiPBid );
}

// BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE
// BS_GROUPBOX | WS_GROUP | BS_TEXT | WS_OVERLAPPED

HB_FUNC( WVW_RBCREATE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   // int   iStyle;
   UINT uiPBid;
   USHORT   usTop    = hb_parni( 2 ),
            usLeft   = hb_parni( 3 ),
            usBottom = hb_parni( 4 ),
            usRight  = hb_parni( 5 );
   LPCTSTR  lpszCaption = ISCHAR(6) ? hb_parcx(6) : NULL;
   char   * szBitmap = ISCHAR(7) ? (char*) hb_parcx(7) : NULL;
   UINT     uiBitmap = ISNUM(7) ? (UINT) hb_parni(7) : 0;
   double   dStretch = !ISNIL(10) ? hb_parnd(10) : 1;
   BOOL     bMap3Dcolors = ISLOG(11) ? (BOOL) hb_parl(11) : FALSE;

   if (!ISBLOCK(8))
   {
     hb_retnl(0);
     return;
   }

   iOffTop    = !ISNIL( 9 ) ? hb_parni( 9,1 ) : -2 ;
   iOffLeft   = !ISNIL( 9 ) ? hb_parni( 9,2 ) : -2 ;
   iOffBottom = !ISNIL( 9 ) ? hb_parni( 9,3 ) : +2 ;
   iOffRight  = !ISNIL( 9 ) ? hb_parni( 9,4 ) : +2;

   uiPBid = ButtonCreate( usWinNum, usTop, usLeft, usBottom, usRight, lpszCaption,
                          szBitmap, uiBitmap, hb_param( 8, HB_IT_BLOCK ),
                          iOffTop, iOffLeft, iOffBottom, iOffRight,
                          dStretch, bMap3Dcolors,
                          BS_AUTORADIOBUTTON /*| WS_GROUP*/ );
   hb_retnl( (LONG) uiPBid );
}

HB_FUNC( WVW_SETCONTROLTEXT )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = ISNIL(2) ? 0 : hb_parni(2);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);

  if (uiCtrlId==0 || hWndPB==NULL)
  {
    return;
  }
  SetWindowText( hWndPB, hb_parcx(3) );
  hb_retl( TRUE );
}

HB_FUNC( WVW_PBVISIBLE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = ISNIL(3) ? TRUE : hb_parl(3);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);
  int    iCmdShow;

  if (uiCtrlId==0 || hWndPB==NULL)
  {
    hb_retl( FALSE );
    return;
  }

  if ( bEnable )
  {
    iCmdShow = SW_SHOW;
  }
  else
  {
    iCmdShow = SW_HIDE;
  }
  hb_retl( ShowWindow(hWndPB, iCmdShow)==0 );
}

HB_FUNC( WVW_CBVISIBLE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = ISNIL(3) ? TRUE : hb_parl(3);
  byte   bStyle;
  HWND   hWndCB = FindControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, uiCtrlId, &bStyle);
  int    iCmdShow;

  if (hWndCB)
  {
     if ( bEnable )
     {
       iCmdShow = SW_SHOW;
     }
     else
     {
       iCmdShow = SW_HIDE;
     }
     hb_retl( ShowWindow(hWndCB, iCmdShow)==0 );
  }
  else
  {
    hb_retl(FALSE);
  }
}

HB_FUNC( WVW_CXVISIBLE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = ISNIL(3) ? TRUE : hb_parl(3);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);
  int    iCmdShow;

  if (uiCtrlId==0 || hWndPB==NULL)
  {
    hb_retl( FALSE );
    return;
  }

  if ( bEnable )
  {
    iCmdShow = SW_SHOW;
  }
  else
  {
    iCmdShow = SW_HIDE;
  }
  hb_retl( ShowWindow(hWndPB, iCmdShow)==0 );
}

/*WVW_XBVisible( [nWinNum], nXBid, lShow )
 *show/hide scrollbar nXBid in window nWinNum (default to topmost window)
 *nWinNum better be NIL
 *nXBid is the handle of the scrolbar
 *lShow: .T. shows the scrolbar (default)
 *       .F. hides the scrolbar
 *returns .t. if successful
 */
HB_FUNC( WVW_XBVISIBLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
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




HB_FUNC( WVW_MOUSE_COL )
{
  WVW_DATA * pData =  hb_getWvwData( ) ;   
  if ( hb_gt_wvw_GetMainCoordMode() )
  {
    hb_retni( hb_gt_wvwGetMouseX( pData->s_pWindows[ pData->s_usNumWindows-1 ] ) + hb_gt_wvwColOfs( pData->s_usNumWindows-1 ) );
  }
  else
  {
    hb_retni( hb_gt_wvwGetMouseX( pData->s_pWindows[ pData->s_usCurWindow ] ) );
  }
}

/*-------------------------------------------------------------------*/

HB_FUNC( WVW_MOUSE_ROW )
{
  WVW_DATA * pData =  hb_getWvwData( ) ;   
  if ( hb_gt_wvw_GetMainCoordMode() )
  {
    hb_retni( hb_gt_wvwGetMouseY( pData->s_pWindows[ pData->s_usNumWindows-1 ] ) + hb_gt_wvwRowOfs( pData->s_usNumWindows-1 ));
  }
  else
  {
    hb_retni( hb_gt_wvwGetMouseY( pData->s_pWindows[ pData->s_usCurWindow ] ) );
  }
}

HB_FUNC( SENDMESSAGE )
{

    hb_retnl( (LONG) SendMessage(
                       (HWND) HB_PARHANDLE(1),     // handle of destination window
                       (UINT) hb_parni( 2 ),    // message to send
                       (WPARAM) hb_parnl( 3 ),    // first message parameter
                       (ISCHAR(4))? (LPARAM) hb_parc( 4 ) :
                          (LPARAM) hb_parnl( 4 ) // second message parameter
                     ) );
}

HB_FUNC( SETPARENT )
{
   UINT usWinNum = WVW_WHICH_WINDOW; // filho
   UINT usWinNum1 = ( ISNIL( 2 ) ? ( hb_gt_wvw_GetMainCoordMode() ? ((hb_gt_wvw_GetNumWindows())-1) : hb_gt_wvw_GetCurWindow() ) : ((USHORT) hb_parni( 2 )) ); //pai
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   WIN_DATA * pWindowData1 = hb_gt_wvw_GetWindowsData( usWinNum1 );
   HWND hWndParent = pWindowData->hWnd;
   HWND hWndParent1 = pWindowData1->hWnd;

   if ( usWinNum1 != 0)
      SetParent(hWndParent,hWndParent1);
}


HB_FUNC( BRINGTOTOP1 )
{
   HWND hWnd  = (HWND) HB_PARHANDLE( 1 ) ;
//   DWORD  ForegroundThreadID;
//DWORD  ThisThreadID;
//DWORD  timeout;

   if (IsIconic(hWnd) )
   {
      ShowWindow(hWnd,SW_RESTORE);
      hb_retl(TRUE);
      return;
   }
   BringWindowToTop(hWnd); // IE 5.5 related hack
   SetForegroundWindow(hWnd);
}

HB_FUNC( ISWINDOW )
{
   hb_retl(IsWindow((HWND) HB_PARHANDLE( 1 ) ) ) ;
}


HB_FUNC( ADDTOOLTIPEX ) // changed by MAG
{
//   HWND hWnd = (HWND) hb_parnl( 1 );
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   WVW_DATA * pData =  hb_getWvwData( ) ;   

   int iStyle = TTS_ALWAYSTIP;
   INITCOMMONCONTROLSEX icex = { 0 };
   TOOLINFO             ti = { 0 };

   /* Load the tooltip class from the DLL.
    */
   icex.dwSize = sizeof( icex );
   icex.dwICC  = ICC_BAR_CLASSES;

   if( !InitCommonControlsEx( &icex ) )
   {
   }

//   if ( lToolTipBalloon )
//   {
      iStyle = iStyle | TTS_BALLOON;
//   }

   if( !pData->hWndTT )
      pData->hWndTT = CreateWindow( TOOLTIPS_CLASS, (LPSTR) NULL, iStyle,
                CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                NULL, (HMENU) NULL, GetModuleHandle( NULL ), NULL );
   if( !pData->hWndTT )
   {
      hb_retnl( 0 );
      return;
   }
   ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
   ti.hwnd = pWindowData->hWnd;
   ti.uId = (UINT) hb_parnl( 2 );
   // ti.uId = (UINT) GetDlgItem( hWnd, hb_parni( 2 ) );
   ti.hinst = GetModuleHandle( NULL );
   ti.lpszText = (LPSTR) hb_parc( 3 );

   hb_retl( SendMessage( pData->hWndTT, TTM_ADDTOOL, 0, (LPARAM) (LPTOOLINFO) &ti) );
}





/*
 * CreateImagelist( array, cx, cy, nGrow, flags )
*/
HB_FUNC( CREATEIMAGELIST )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   UINT flags = ( ISNIL(5) ) ? ILC_COLOR : hb_parni(5);
   HIMAGELIST himl;
   ULONG ul, ulLen = hb_arrayLen( pArray );
   HBITMAP hbmp;

   himl = ImageList_Create( hb_parni(2), hb_parni(3), flags,
                   ulLen, hb_parni(4) );

   for( ul=1; ul<=ulLen; ul++ )
   {
      hbmp = (HBITMAP)hb_arrayGetNL( pArray, ul );
      ImageList_Add( himl, hbmp, (HBITMAP) NULL );
      DeleteObject(hbmp);
   }

   HB_RETHANDLE( himl );
}

HB_FUNC( IMAGELIST_ADD )
{
   hb_retnl( ImageList_Add( (HIMAGELIST)HB_PARHANDLE(1), (HBITMAP)HB_PARHANDLE(2), (HBITMAP) NULL ) );
}

HB_FUNC( IMAGELIST_ADDMASKED )
{
   hb_retnl( ImageList_AddMasked( (HIMAGELIST)HB_PARHANDLE(1), (HBITMAP)HB_PARHANDLE(2), (COLORREF) hb_parnl(3) ) );
}


HB_FUNC( GETBITMAPSIZE )
{
   BITMAP  bitmap;
   PHB_ITEM aMetr = hb_itemArrayNew( 3 );
   PHB_ITEM temp;

   GetObject( (HBITMAP) HB_PARHANDLE( 1 ), sizeof( BITMAP ), ( LPVOID ) &bitmap );

   temp = hb_itemPutNL( NULL, bitmap.bmWidth );
   hb_itemArrayPut( aMetr, 1, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, bitmap.bmHeight );
   hb_itemArrayPut( aMetr, 2, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, bitmap.bmBitsPixel );
   hb_itemArrayPut( aMetr, 3, temp );
   hb_itemRelease( temp );

   hb_itemReturn( aMetr );
   hb_itemRelease( aMetr );
}

HB_FUNC( GETICONSIZE )
{
   ICONINFO iinfo;
   PHB_ITEM aMetr = hb_itemArrayNew( 2 );
   PHB_ITEM temp;

   GetIconInfo( (HICON) HB_PARHANDLE( 1 ), &iinfo );

   temp = hb_itemPutNL( NULL, iinfo.xHotspot * 2 );
   hb_itemArrayPut( aMetr, 1, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, iinfo.yHotspot * 2 );
   hb_itemArrayPut( aMetr, 2, temp );
   hb_itemRelease( temp );

   hb_itemReturn( aMetr );
   hb_itemRelease( aMetr );
}


HB_FUNC( LOADIMAGE )
{
   if ( ISNUM( 2 ) )
      hb_retnl( (LONG)
          LoadImage( ( HINSTANCE ) hb_hInstance,  //ISNIL( 1 ) ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl( 1 ),    // handle of the instance that contains the image
                  (LPCTSTR)MAKEINTRESOURCE(hb_parnl(2)),          // name or identifier of image
                  (UINT) hb_parni(3),           // type of image
                  hb_parni(4),                  // desired width
                  hb_parni(5),                  // desired height
                  (UINT)hb_parni(6)             // load flags
     ) );

   else
      HB_RETHANDLE(
          LoadImage( (HINSTANCE)hb_parnl(1),    // handle of the instance that contains the image
                  (LPCTSTR)hb_parc(2),          // name or identifier of image
                  (UINT) hb_parni(3),           // type of image
                  hb_parni(4),                  // desired width
                  hb_parni(5),                  // desired height
                  (UINT)hb_parni(6)             // load flags
      ) );

}

HB_FUNC( LOADBITMAP )
{
   if( ISNUM(1) )
   {
      if( !ISNIL(2) && hb_parl(2) )
//               hb_retnl( (LONG) LoadBitmap( GetModuleHandle( NULL ),  MAKEINTRESOURCE(hb_parnl( 1 ) )) );
         HB_RETHANDLE( LoadBitmap( NULL, (LPCTSTR) hb_parnl( 1 ) ) );
      else
         HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), (LPCTSTR) hb_parnl( 1 ) ) );
   }
   else
     HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), (LPCTSTR) hb_parc( 1 ) ) );
}

HB_FUNC( LOADBITMAPEX )
{
    HINSTANCE h = ISNUM(1) ? (HINSTANCE) hb_parnl( 1 ) :GetModuleHandle( NULL ) ;
   if( ISNUM(1) && ISNUM(2) )
   {
      if( !ISNIL(3) && hb_parl(3) )
//               hb_retnl( (LONG) LoadBitmap( h,  MAKEINTRESOURCE(hb_parnl( 2 ) )) );
         HB_RETHANDLE( LoadBitmap( h, (LPCTSTR) hb_parnl( 3 ) ) );
      else
         HB_RETHANDLE( LoadBitmap( (HINSTANCE) h, (LPCTSTR) hb_parnl( 2 ) ) );
   }
   else
      HB_RETHANDLE( LoadBitmap( h, (LPCTSTR) hb_parc( 2 ) ) );
}


HB_FUNC( OPENIMAGE )
{
   char* cFileName = hb_parc(1);
   BOOL  lString = (ISNIL(2))? 0 : hb_parl(2);
   int iFileSize;
   FILE* fp;
   // IPicture * pPic;
   LPPICTURE pPic;
   IStream * pStream;
   HGLOBAL hG;
   HBITMAP hBitmap = 0;

   if( lString )
   {
      iFileSize = hb_parclen( 1 );
      hG = GlobalAlloc( GPTR,iFileSize );
      if( !hG )
      {
         hb_retnl(0);
         return;
      }
      memcpy( (void*)hG, (void*)cFileName,iFileSize );
   }
   else
   {
      fp = fopen( cFileName,"rb" );
      if( !fp )
      {
         hb_retnl(0);
         return;
      }

      fseek( fp,0,SEEK_END );
      iFileSize = ftell( fp );
      hG = GlobalAlloc( GPTR,iFileSize );
      if( !hG )
      {
         fclose( fp );
         hb_retnl(0);
         return;
      }
      fseek( fp,0,SEEK_SET );
      fread( (void*)hG, 1, iFileSize, fp );
      fclose( fp );
   }

   CreateStreamOnHGlobal( hG,0,&pStream );

   if( !pStream )
   {
      GlobalFree( hG );
      hb_retnl(0);
      return;
   }

//#if defined(__cplusplus)
   //OleLoadPicture( pStream,0,0,&IID_IPicture,(void**)&pPic );
   //pStream->Release();
//#else
   OleLoadPicture( pStream,0,0,&IID_IPicture,(void**)&pPic );
   pStream->lpVtbl->Release( pStream );
//#endif

   GlobalFree( hG );

   if( !pPic )
   {
      hb_retnl(0);
      return;
   }

//#if defined(__cplusplus)
   //pPic->get_Handle( (OLE_HANDLE*)&hBitmap );
//#else
   pPic->lpVtbl->get_Handle( pPic, (OLE_HANDLE*)&hBitmap );
//#endif

   hb_retnl( (LONG) CopyImage( hBitmap,IMAGE_BITMAP,0,0,LR_COPYRETURNORG ) );

//#if defined(__cplusplus)
   //pPic->Release();
//#else
   pPic->lpVtbl->Release( pPic );
//#endif
}

HB_FUNC( OPENBITMAP )
{
   BITMAPFILEHEADER bmfh;
   BITMAPINFOHEADER bmih;
   LPBITMAPINFO lpbmi;
   DWORD dwRead;
   LPVOID lpvBits;
   HGLOBAL hmem1, hmem2;
   HBITMAP hbm;
   HDC hDC = (hb_pcount()>1 && !ISNIL(2))? (HDC)HB_PARHANDLE(2):NULL;
   HANDLE hfbm = CreateFile( hb_parc( 1 ), GENERIC_READ, FILE_SHARE_READ,
                   (LPSECURITY_ATTRIBUTES) NULL, OPEN_EXISTING,
                   FILE_ATTRIBUTE_READONLY, (HANDLE) NULL );

   if( ( (long int)hfbm ) <= 0 )
   {
      HB_RETHANDLE(NULL);
      return;
   }
   /* Retrieve the BITMAPFILEHEADER structure. */
   ReadFile( hfbm, &bmfh, sizeof(BITMAPFILEHEADER), &dwRead, NULL );

   /* Retrieve the BITMAPFILEHEADER structure. */
   ReadFile( hfbm, &bmih, sizeof(BITMAPINFOHEADER), &dwRead, NULL );

   /* Allocate memory for the BITMAPINFO structure. */

   hmem1 = GlobalAlloc( GHND, sizeof(BITMAPINFOHEADER) +
             ((1<<bmih.biBitCount) * sizeof(RGBQUAD)));
   lpbmi = (LPBITMAPINFO)GlobalLock( hmem1 );

   /*  Load BITMAPINFOHEADER into the BITMAPINFO  structure. */
   lpbmi->bmiHeader.biSize = bmih.biSize;
   lpbmi->bmiHeader.biWidth = bmih.biWidth;
   lpbmi->bmiHeader.biHeight = bmih.biHeight;
   lpbmi->bmiHeader.biPlanes = bmih.biPlanes;

   lpbmi->bmiHeader.biBitCount = bmih.biBitCount;
   lpbmi->bmiHeader.biCompression = bmih.biCompression;
   lpbmi->bmiHeader.biSizeImage = bmih.biSizeImage;
   lpbmi->bmiHeader.biXPelsPerMeter = bmih.biXPelsPerMeter;
   lpbmi->bmiHeader.biYPelsPerMeter = bmih.biYPelsPerMeter;
   lpbmi->bmiHeader.biClrUsed = bmih.biClrUsed;
   lpbmi->bmiHeader.biClrImportant = bmih.biClrImportant;

   /*  Retrieve the color table.
    * 1 << bmih.biBitCount == 2 ^ bmih.biBitCount
   */
   switch(bmih.biBitCount)
   {
      case 1  :
      case 4  :
      case 8  :
         ReadFile(hfbm, lpbmi->bmiColors,
           ((1<<bmih.biBitCount) * sizeof(RGBQUAD)),
           &dwRead, (LPOVERLAPPED) NULL);
           break;

      case 16 :
      case 32 :
         if( bmih.biCompression == BI_BITFIELDS )
           ReadFile(hfbm, lpbmi->bmiColors,
             ( 3 * sizeof(RGBQUAD)),
             &dwRead, (LPOVERLAPPED) NULL);
         break;

      case 24 :
         break;
   }

   /* Allocate memory for the required number of  bytes. */
   hmem2 = GlobalAlloc( GHND, (bmfh.bfSize - bmfh.bfOffBits) );
   lpvBits = GlobalLock(hmem2);

   /* Retrieve the bitmap data. */

   ReadFile(hfbm, lpvBits, (bmfh.bfSize - bmfh.bfOffBits), &dwRead, NULL );

   if( !hDC )
      hDC = GetDC( 0 );

   /* Create a bitmap from the data stored in the .BMP file.  */
   hbm = CreateDIBitmap( hDC, &bmih, CBM_INIT, lpvBits, lpbmi, DIB_RGB_COLORS );

   if( hb_pcount() < 2 || ISNIL(2) )
      ReleaseDC( 0, hDC );

   /* Unlock the global memory objects and close the .BMP file. */
   GlobalUnlock(hmem1);
   GlobalUnlock(hmem2);
   GlobalFree(hmem1);
   GlobalFree(hmem2);
   CloseHandle(hfbm);

   HB_RETHANDLE( hbm );
}


HB_FUNC( SETTEXTCOLOR )
{
   COLORREF crColor = SetTextColor(
              (HDC) HB_PARHANDLE( 1 ),      // handle of device context
              (COLORREF) hb_parnl( 2 )  // text color
            );
   hb_retnl( (LONG) crColor );
}

HB_FUNC( SETBKCOLOR )
{
   COLORREF crColor = SetBkColor(
              (HDC) HB_PARHANDLE( 1 ),      // handle of device context
              (COLORREF) hb_parnl( 2 )  // text color
            );
   hb_retnl( (LONG) crColor );
}

HB_FUNC( CREATESOLIDBRUSH )
{
   HB_RETHANDLE( CreateSolidBrush(
               (COLORREF) hb_parnl( 1 )   // brush color
             ) );
}

HB_FUNC( CREATEHATCHBRUSH )
{
   HB_RETHANDLE( CreateHatchBrush(
               hb_parni(1), (COLORREF) hb_parnl(2) ) );
}
HB_FUNC( RGB )
{
   hb_retnl( RGB( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) ) ;
}

HB_FUNC( GETSYSCOLOR )
{
   hb_retnl( (LONG) GetSysColor( hb_parni( 1 ) ) );
}

HB_FUNC( REDRAWWINDOW )
{
   RedrawWindow(
    (HWND) HB_PARHANDLE( 1 ),  // handle of window
    NULL,                  // address of structure with update rectangle
    NULL,                  // handle of update region
    (UINT)hb_parni( 2 )    // array of redraw flags
   );
}

/* CreateFont( fontName, nWidth, hHeight [,fnWeight] [,fdwCharSet],
               [,fdwItalic] [,fdwUnderline] [,fdwStrikeOut]  )
*/
HB_FUNC( CREATEFONT )
{
   HFONT hFont;
   int fnWeight = ( ISNIL(4) )? 0:hb_parni(4);
   DWORD fdwCharSet = ( ISNIL(5) )? 0:hb_parnl(5);
   DWORD fdwItalic = ( ISNIL(6) )? 0:hb_parnl(6);
   DWORD fdwUnderline = ( ISNIL(7) )? 0:hb_parnl(7);
   DWORD fdwStrikeOut = ( ISNIL(8) )? 0:hb_parnl(8);

   hFont = CreateFont(
    hb_parni( 3 ),      // logical height of font
    hb_parni( 2 ),      // logical average character width
    0,  // angle of escapement
    0,  // base-line orientation angle
    fnWeight,   // font weight
    fdwItalic,  // italic attribute flag
    fdwUnderline,       // underline attribute flag
    fdwStrikeOut,       // strikeout attribute flag
    fdwCharSet, // character set identifier
    0,  // output precision
    0,  // clipping precision
    0,  // output quality
    0,  // pitch and family
    (LPCTSTR) hb_parc( 1 )      // pointer to typeface name string
   );
   HB_RETHANDLE( hFont );
}


HB_FUNC( SELECTFONT )
{

   CHOOSEFONT cf;
   LOGFONT lf;
   HFONT hfont;
   PHB_ITEM pObj = ( ISNIL(1) )? NULL:hb_param( 1, HB_IT_OBJECT );
   //PHB_ITEM temp1;
   PHB_ITEM aMetr = hb_itemArrayNew( 9 ),temp;

   cf.lStructSize = sizeof(CHOOSEFONT);
   cf.hwndOwner = (HWND)NULL;
   cf.hDC = (HDC)NULL;
   cf.lpLogFont = &lf;
   cf.iPointSize = 0;
   cf.Flags = CF_SCREENFONTS | ( (pObj)? CF_INITTOLOGFONTSTRUCT:0 );
   cf.rgbColors = RGB(0,0,0);
   cf.lCustData = 0L;
   cf.lpfnHook = (LPCFHOOKPROC)NULL;
   cf.lpTemplateName = (LPSTR)NULL;

   cf.hInstance = (HINSTANCE) NULL;
   cf.lpszStyle = (LPSTR)NULL;
   cf.nFontType = SCREEN_FONTTYPE;
   cf.nSizeMin = 0;
   cf.nSizeMax = 0;

   /* Display the CHOOSEFONT common-dialog box. */

   if( !ChooseFont(&cf) )
   {
      hb_itemRelease( aMetr );
      hb_ret();
      return;
   }

   /* Create a logical font based on the user's   */
   /* selection and return a handle identifying   */
   /* that font.                                  */

   hfont = CreateFontIndirect(cf.lpLogFont);
   temp = hb_itemPutNL( NULL, (LONG) hfont );
   hb_itemArrayPut( aMetr, 1, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutC( NULL, lf.lfFaceName );
   hb_itemArrayPut( aMetr, 2, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lf.lfWidth );
   hb_itemArrayPut( aMetr, 3, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lf.lfHeight );
   hb_itemArrayPut( aMetr, 4, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lf.lfWeight );
   hb_itemArrayPut( aMetr, 5, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNI( NULL, lf.lfCharSet );
   hb_itemArrayPut( aMetr, 6, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNI( NULL, lf.lfItalic );
   hb_itemArrayPut( aMetr, 7, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNI( NULL, lf.lfUnderline );
   hb_itemArrayPut( aMetr, 8, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNI( NULL, lf.lfStrikeOut );
   hb_itemArrayPut( aMetr, 9, temp );
   hb_itemRelease( temp );

   hb_itemReturn( aMetr );
   hb_itemRelease( aMetr );

}

HB_FUNC( INVALIDATERECT )
{
   RECT rc;

   if( hb_pcount() > 2 )
   {
      rc.left = hb_parni( 3 );
      rc.top = hb_parni( 4 );
      rc.right = hb_parni( 5 );
      rc.bottom = hb_parni( 6 );
   }

   InvalidateRect(
    (HWND) HB_PARHANDLE( 1 ), // handle of window with changed update region
    ( hb_pcount() > 2 )? &rc:NULL,  // address of rectangle coordinates
    hb_parni( 2 ) // erase-background flag
   );
}

HB_FUNC(TOOLBARADDBUTTONS)
{

   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   HWND hWndCtrl = ( HWND ) HB_PARHANDLE( 2 ) ;
   /* HWND hToolTip = ( HWND ) hb_parnl( 5 ) ; */
   PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );
   int iButtons= hb_parni( 4 );
   TBBUTTON  *tb = ( struct _TBBUTTON * ) hb_xgrab( iButtons * sizeof( TBBUTTON ) );
   PHB_ITEM pTemp;
   //BOOL bSystem;

   ULONG ulCount;
   ULONG ulID;
   DWORD style = GetWindowLong( hWndCtrl, GWL_STYLE );
   USHORT  usOldHeight;

   SetWindowLong(hWndCtrl,GWL_STYLE,style|TBSTYLE_TOOLTIPS |TBSTYLE_FLAT);

   SendMessage( hWndCtrl, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0L);
   usOldHeight = pWindowData->usTBHeight;
   for ( ulCount =0 ;  ( ulCount < hb_arrayLen( pArray ) ); ulCount++ )
   {

      pTemp = hb_arrayGetItemPtr( pArray , ulCount + 1 );
      ulID=hb_arrayGetNI( pTemp, 1 );
      //bSystem = hb_arrayGetL( pTemp, 9 );

//      if (bSystem)
//      if (ulID > 0 && ulID <  31 )
  //    {
         tb[ ulCount ].iBitmap   = ulID > 0 ? ( int ) ulID : -1;
//      }
//      else
//      {
//         tb[ ulCount ].iBitmap   = ulID > 0 ? ( int ) ulCount : -1;
//      }
      tb[ ulCount ].idCommand = hb_arrayGetNI( pTemp, 2 );
      tb[ ulCount ].fsState   = hb_arrayGetNI( pTemp, 3 );
      tb[ ulCount ].fsStyle   = hb_arrayGetNI( pTemp, 4 );
      tb[ ulCount ].dwData    = hb_arrayGetNI( pTemp, 5 );
      tb[ ulCount ].iString   = hb_arrayGetCLen( pTemp, 6 )  >0 ? ( int ) hb_arrayGetCPtr( pTemp, 6 ) : 0 ;

   }

   SendMessage( hWndCtrl, TB_ADDBUTTONS, (WPARAM) iButtons, (LPARAM) (LPTBBUTTON) tb);
   SendMessage( hWndCtrl, TB_AUTOSIZE, 0, 0 );
   hb_gt_wvwTBinitSize( pWindowData, hWndCtrl );

   if (pWindowData->usTBHeight != usOldHeight)
   {
     hb_gt_wvwResetWindow( usWinNum );
   }

   hb_xfree( tb );
}

HB_FUNC(SETBITMAPRESOURCEID)
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   TBADDBITMAP tbab;
   HBITMAP hBitmap  = (HBITMAP) HB_PARHANDLE( 3 ) ;
   UINT uiBitmap = (UINT) hb_parni( 4 );
   HWND hWndToolbar = pWindowData->hToolBar;
   int iNewBitmap;
   int iBitmapType = hb_parni( 2 );
   int iOffset;


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


      tbab.hInst = NULL;
      tbab.nID   = (UINT) hBitmap;
      iNewBitmap = SendMessage(hWndToolbar, TB_ADDBITMAP, (WPARAM) 1, (WPARAM) &tbab);

   }
   else /* system bitmap */
   {
      iNewBitmap = (int) uiBitmap + iOffset;
   }
   hb_retni( iNewBitmap ) ;

}


HB_FUNC( DRAWICON )
{
   DrawIcon( (HDC)HB_PARHANDLE( 1 ), hb_parni( 3 ), hb_parni( 4 ), (HICON)HB_PARHANDLE( 2 ) );
}

HB_FUNC( LOADICON )
{
   if( ISNUM(1) )
      HB_RETHANDLE( LoadIcon( NULL, (LPCTSTR) hb_parnl( 1 ) ) );
   else
      HB_RETHANDLE( LoadIcon( GetModuleHandle( NULL ), (LPCTSTR) hb_parc( 1 ) ) );
}
HB_FUNC( DRAWBITMAP )
{
   HDC hDC = (HDC) HB_PARHANDLE( 1 );
   HDC hDCmem = CreateCompatibleDC( hDC );
   DWORD dwraster = (ISNIL(3))? SRCCOPY:hb_parnl(3);
   HBITMAP hBitmap = (HBITMAP) HB_PARHANDLE( 2 );
   BITMAP  bitmap;
   int nWidthDest = ( hb_pcount()>=5 && !ISNIL(6) )? hb_parni(6):0;
   int nHeightDest = ( hb_pcount()>=6 && !ISNIL(7) )? hb_parni(7):0;

   SelectObject( hDCmem, hBitmap );
   GetObject( hBitmap, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   if( nWidthDest && ( nWidthDest != bitmap.bmWidth || nHeightDest != bitmap.bmHeight ))
   {
      StretchBlt( hDC, hb_parni(4), hb_parni(5), nWidthDest, nHeightDest, hDCmem,
                  0, 0, bitmap.bmWidth, bitmap.bmHeight, dwraster );
   }
   else
   {
      BitBlt( hDC, hb_parni(4), hb_parni(5), bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwraster );
   }

   DeleteDC( hDCmem );
}
HB_FUNC( WINDOW2BITMAP )
{
   HWND hWnd = (HWND) HB_PARHANDLE( 1 );
   BOOL lFull = ( ISNIL(2) )? 0 : (BOOL)hb_parl(2);
   HDC hDC = ( lFull )? GetWindowDC( hWnd ) : GetDC( hWnd );
   HDC hDCmem = CreateCompatibleDC( hDC );
   HBITMAP hBitmap;
   RECT rc;

   if( lFull )
      GetWindowRect( hWnd, &rc );
   else
      GetClientRect( hWnd, &rc );

   hBitmap = CreateCompatibleBitmap( hDC, rc.right-rc.left, rc.bottom-rc.top );
   SelectObject( hDCmem, hBitmap );

   BitBlt( hDCmem, 0, 0, rc.right-rc.left, rc.bottom-rc.top, hDC, 0, 0, SRCCOPY );

   DeleteDC( hDCmem );
   DeleteDC( hDC );
   HB_RETHANDLE( hBitmap );
}
