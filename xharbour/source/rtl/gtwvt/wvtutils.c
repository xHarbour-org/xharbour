/*
 * $Id: wvtcore.c,v 1.0 2004/05/13 17:30:12 vouchcac Exp $
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
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

//-------------------------------------------------------------------//

#define HB_OS_WIN_32_USED

//-------------------------------------------------------------------//

#include "hbgtwvt.h"

//-------------------------------------------------------------------//
//
//              Pritpal Bedi <pritpal@vouchcac.com>
//
//-------------------------------------------------------------------//
//
//     Wvt_ChooseFont( cFontName, nHeight, nWidth, nWeight, nQuality, ;
//                                    lItalic, lUnderline, lStrikeout )
//
HB_FUNC( WVT_CHOOSEFONT )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   CHOOSEFONT  cf;
   LOGFONT     lf;
   LONG        PointSize = 0;

   if ( ! ISNIL( 2 ) )
   {
      PointSize = -MulDiv( ( LONG ) hb_parnl( 2 ), GetDeviceCaps( _s->hdc, LOGPIXELSY ), 72 ) ;
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
   cf.hwndOwner        = _s->hWnd;
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
      PointSize = -MulDiv( lf.lfHeight, 72, GetDeviceCaps( _s->hdc, LOGPIXELSY ) ) ;

      hb_reta( 8 );

      hb_storc(  lf.lfFaceName     , -1, 1 );
      hb_stornl( ( LONG ) PointSize, -1, 2 );
      hb_storni( lf.lfWidth        , -1, 3 );
      hb_storni( lf.lfWeight       , -1, 4 );
      hb_storni( lf.lfQuality      , -1, 5 );
      hb_storl(  lf.lfItalic       , -1, 6 );
      hb_storl(  lf.lfUnderline    , -1, 7 );
      hb_storl(  lf.lfStrikeOut    , -1, 8 );
   }
   else
   {
      hb_reta( 8 );

      hb_storc(  ""        , -1, 1 );
      hb_stornl( ( LONG ) 0, -1, 2 );
      hb_storni( 0         , -1, 3 );
      hb_storni( 0         , -1, 4 );
      hb_storni( 0         , -1, 5 );
      hb_storl(  0         , -1, 6 );
      hb_storl(  0         , -1, 7 );
      hb_storl(  0         , -1, 8 );
   }

   return ;
}

//-------------------------------------------------------------------//
//
//    Wvt_ChooseColor( nRGBInit, aRGB16, nFlags ) => nRGBSelected
//
HB_FUNC( WVT_CHOOSECOLOR )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   CHOOSECOLOR cc ;
   COLORREF    crCustClr[ 16 ] ;
   int         i ;

   for( i = 0 ; i < 16 ; i++ )
   {
     crCustClr[ i ] = ( ISARRAY( 3 ) ? hb_parnl( 3, i+1 ) : GetSysColor( COLOR_BTNFACE ) ) ;
   }

   cc.lStructSize   = sizeof( CHOOSECOLOR ) ;
   cc.hwndOwner     = _s->hWnd ;
   cc.rgbResult     = ISNIL( 1 ) ?  0 : ( COLORREF ) hb_parnl( 1 ) ;
   cc.lpCustColors  = crCustClr ;
   cc.Flags         = ( WORD ) ( ISNIL( 3 ) ? CC_ANYCOLOR | CC_RGBINIT : hb_parnl( 3 ) );

   if ( ChooseColor( &cc ) )
   {
      hb_retnl( cc.rgbResult ) ;
   }
   else
   {
      hb_retnl( 0 );
   }
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_MESSAGEBOX )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   MessageBox( _s->hWnd, hb_parcx( 1 ), hb_parcx( 2 ), ISNIL( 3 ) ? MB_OK : hb_parni( 3 ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETTOOLTIPACTIVE )
{
   GLOBAL_DATA *_s     = hb_wvt_gtGetGlobalData();
   BOOL        bActive = _s->bToolTipActive;

   if ( ! ISNIL( 1 ) )
   {
      _s->bToolTipActive = hb_parl( 1 );
   }

   hb_retl( bActive );
}

//-------------------------------------------------------------------//
//
//   Wvt_SetToolTip( nTop, nLeft, nBottom, nRight, cToolText )
//
HB_FUNC( WVT_SETTOOLTIP )
{
   GLOBAL_DATA *_s  = hb_wvt_gtGetGlobalData();
   TOOLINFO ti;
   POINT    xy;
   int      iTop, iLeft, iBottom, iRight;

   if ( ! _s->bToolTipActive )
   {
      return;
   }

   ti.cbSize    = sizeof( TOOLINFO );
   ti.hwnd      = _s->hWnd;
   ti.uId       = 100000;

   if ( SendMessage( _s->hWndTT, TTM_GETTOOLINFO, 0, ( LPARAM ) &ti ) )
   {
      xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
      iTop    = xy.y;
      iLeft   = xy.x;

      xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 )+1, hb_parni( 3 )+1 );
      iBottom = xy.y - 1;
      iRight  = xy.x - 1;

      ti.lpszText    = hb_parc( 5 );
      ti.rect.left   = iLeft;
      ti.rect.top    = iTop;
      ti.rect.right  = iRight;
      ti.rect.bottom = iBottom;

      SendMessage( _s->hWndTT, TTM_SETTOOLINFO, 0, ( LPARAM ) &ti );
   }
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETTIMER )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   hb_retl( SetTimer( _s->hWnd, 101, hb_parni( 1 ), NULL ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_KILLTIMER )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   hb_retl( KillTimer( _s->hWnd, 101 ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETONTOP )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   RECT rect;

   GetWindowRect( _s->hWnd, &rect );

   hb_retl( SetWindowPos( _s->hWnd, HWND_TOPMOST,
                          rect.left,
                          rect.top,
                          0,
                          0,
                          SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETASNORMAL )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   RECT rect;

   GetWindowRect( _s->hWnd, &rect );

   hb_retl( SetWindowPos( _s->hWnd, HWND_NOTOPMOST,
                          rect.left,
                          rect.top,
                          0,
                          0,
                          SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_MINIMIZE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   ShowWindow( _s->hWnd, SW_MINIMIZE );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_MAXIMIZE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   ShowWindow( _s->hWnd, SW_RESTORE );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETMOUSEPOS )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT xy;

   xy = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

   if ( ClientToScreen( _s->hWnd, &xy ) )
   {
      hb_retl( SetCursorPos( xy.x, xy.y + ( _s->PTEXTSIZE.y / 2 ) ) );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_GETPAINTRECT )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   HB_ITEM  info;
   HB_ITEM  temp;

   info.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &info, 4 );

   hb_arraySetForward( &info, 1, hb_itemPutNI( &temp, _s->rowStart ) );
   hb_arraySetForward( &info, 2, hb_itemPutNI( &temp, _s->colStart ) );
   hb_arraySetForward( &info, 3, hb_itemPutNI( &temp, _s->rowStop  ) );
   hb_arraySetForward( &info, 4, hb_itemPutNI( &temp, _s->colStop  ) );

   hb_itemReturn( &info );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETPOINTER )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   int     iCursor = hb_parni( 1 );
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

   SetClassLong( _s->hWnd, GCL_HCURSOR, ( DWORD ) hCursor );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETFONT )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   hb_retl( hb_wvt_gtSetFont(
            ISNIL( 1 ) ? _s->fontFace   : hb_parcx( 1 ),
            ISNIL( 2 ) ? _s->fontHeight : hb_parni( 2 ),
            ISNIL( 3 ) ? _s->fontWidth  : hb_parni( 3 ),
            ISNIL( 4 ) ? _s->fontWeight : hb_parni( 4 ),
            ISNIL( 5 ) ? _s->fontQuality: hb_parni( 5 )
           ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETICON )
{
   if ( ISNUM( 1 ) )
   {
      hb_retnl( hb_wvt_gtSetWindowIcon( hb_parni( 1 ) ) ) ;
   }
   else
   {
      hb_retnl( hb_wvt_gtSetWindowIconFromFile( hb_parcx( 1 ) ) ) ;
   }
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETTITLE )
{
   hb_wvt_gtSetWindowTitle( hb_parcx( 1 ) ) ;
   return ;
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETWINDOWPOS )
{
   hb_wvt_gtSetWindowPos( hb_parni( 1 ), hb_parni( 2 ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_GETWINDOWHANDLE )
{
   hb_retnl( ( LONG ) hb_wvt_gtGetWindowHandle() ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETCODEPAGE )
{
   hb_retni( hb_wvt_gtSetCodePage( hb_parni( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_CENTERWINDOW )
{
   hb_retl( hb_wvt_gtSetCentreWindow(
               ISNIL( 1 ) ? TRUE  : hb_parl( 1 ),
               ISNIL( 2 ) ? FALSE : hb_parl( 2 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETMOUSEMOVE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   if ( ISNIL( 1 ) )
   {
      hb_retl( _s->MouseMove );
   }
   else
   {
      hb_retl( hb_wvt_gtSetMouseMove( hb_parl( 1 ) ) );
   }
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_GETXYFROMROWCOL )
{
   HB_ITEM  aXY;
   HB_ITEM  temp;
   POINT    xy;

   xy = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

   aXY.type  = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &aXY, 2 );

   hb_arraySetForward( &aXY, 1, hb_itemPutNL( &temp, xy.x ) );
   hb_arraySetForward( &aXY, 2, hb_itemPutNL( &temp, xy.y ) );

   hb_itemReturn( &aXY );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_GETFONTINFO )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   HB_ITEM  info;
   HB_ITEM  temp;

   info.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &info, 7 );

   hb_arraySetForward( &info, 1, hb_itemPutC(  &temp, _s->fontFace    ) );
   hb_arraySetForward( &info, 2, hb_itemPutNL( &temp, _s->fontHeight  ) );
   hb_arraySetForward( &info, 3, hb_itemPutNL( &temp, _s->fontWidth   ) );
   hb_arraySetForward( &info, 4, hb_itemPutNL( &temp, _s->fontWeight  ) );
   hb_arraySetForward( &info, 5, hb_itemPutNL( &temp, _s->fontQuality ) );
   hb_arraySetForward( &info, 6, hb_itemPutNL( &temp, _s->PTEXTSIZE.y ) );
   hb_arraySetForward( &info, 7, hb_itemPutNL( &temp, _s->PTEXTSIZE.x ) );

   hb_itemReturn( &info );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_GETPALETTE )
{
   HB_ITEM  info;
   HB_ITEM  temp;
   int       i;

   info.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   hb_arrayNew( &info, 16 );

   for ( i = 0; i < 16; i++ )
   {
      hb_arraySetForward( &info, i+1, hb_itemPutNL( &temp, hb_wvt_gtGetColorData( i ) ) );
   }

   hb_itemReturn( &info );
}

//-------------------------------------------------------------------//
//
//    Wvt_SetPalette( aRGBValues ) -> An array of 16 elements with RGB values
//
HB_FUNC( WVT_SETPALETTE )
{
   int       i;

   for ( i = 0; i < 16; i++ )
   {
      hb_wvt_gtSetColorData( i, hb_parnl( 1, i+1 ) );
   }
}

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                 Peter Rees <peter@rees.co.nz>
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

HB_FUNC( WVT_SETMENU )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   SetMenu( _s->hWnd, ( HMENU ) hb_parni( 1 ) ) ;

   hb_wvt_gtResetWindow();
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETPOPUPMENU )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();

   HMENU hPopup = _s->hPopup ;

   _s->hPopup = ( HMENU ) hb_parnl( 1 );
   if ( hPopup )
   {
      hb_retnl( ( LONG ) hPopup );
   }
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_CREATEMENU )
{
  hb_retnl( ( LONG ) CreateMenu() ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_CREATEPOPUPMENU )
{
  hb_retnl( ( LONG ) CreatePopupMenu() ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_APPENDMENU )
{
  char    ucBuf[ 256 ];
  int     i,iLen ;
  LPCTSTR lpszCaption;

  if ( ISCHAR( 4 ) )
  {
    iLen = hb_parclen( 4 );
    if ( iLen > 0 && iLen < 256 )   // Translate '~' to '&'
    {
      lpszCaption = hb_parcx( 4 ) ;
      for ( i = 0; i < iLen; i++ )
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
    lpszCaption = ( LPCTSTR ) hb_parni( 4 ) ; // It is a SEPARATOR or Submenu
  }

  hb_retl( AppendMenu( ( HMENU ) hb_parnl( 1 ), ( UINT ) hb_parni( 2 ), ( UINT_PTR ) hb_parni( 3 ), ( LPCTSTR ) lpszCaption ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_DELETEMENU )
{
  hb_retl( DeleteMenu( ( HMENU ) hb_parnl( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_DESTROYMENU )
{
  hb_retl( DestroyMenu( ( HMENU ) hb_parnl( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_ENABLEMENUITEM )
{
  hb_retni( EnableMenuItem( ( HMENU ) hb_parnl( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_GETLASTMENUEVENT )
{
  hb_retni( hb_wvt_gtGetLastMenuEvent() ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETMENUKEYEVENT )
{
  int iEvent = 0;

  if ( ISNUM( 1 ) )
  {
    iEvent = hb_parnl( 1 ) ;
  }

  hb_retni( hb_wvt_gtSetMenuKeyEvent( iEvent ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_GETSCREENWIDTH )
{
  hb_retni( GetSystemMetrics( SM_CXSCREEN ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_GETSCREENHEIGHT )
{
  hb_retni( GetSystemMetrics( SM_CYSCREEN ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETWINDOWCENTRE )
{
  hb_wvt_gtSetCentreWindow( hb_parl( 1 ), hb_parl( 2 ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETALTF4CLOSE )
{
  hb_retl( hb_wvt_gtSetAltF4Close( hb_parl( 1 ) ) );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_PROCESSMESSAGES )
{
  hb_wvt_gtDoProcessMessages();

  hb_retl( 1 );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_GETTITLE )
{
  BYTE ucText[ 1024 ];

  hb_wvt_gtGetWindowTitle( ( char* ) ucText, 1023 );

  hb_retc( ( char* ) ucText ) ;
}

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//   Author.....: Francesco Saverio Giudice <info@fsgiudice.com>
//   Syntax.....: Wvt_GetRGBColor( nColor ) --> nRGBColor
//   Description: Return the RGB values passing the color positional value
//                0=Black, 1=Blue, etc
//                as returned from hb_ColorToN()
//   Creat. Date: 2004/01/15
//   Last Modif.: 2004/01/15
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

HB_FUNC( WVT_GETRGBCOLOR )
{
   int iColor;
   if ( !ISNIL( 1 ) )
   {
      iColor = hb_parni( 1 );
      if ( iColor >= 0 && iColor < 16 )  /* Test bound error */
      {
         hb_retnl( hb_wvt_gtGetColorData( iColor ) );
      }
   }
}

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                       Giancarlo Niccolai
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

HB_FUNC( WVT_GETCLIPBOARD )
{
    HGLOBAL   hglb;
    LPTSTR    lptstr;

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
          hb_retc( lptstr );
          GlobalUnlock( hglb );
       }
    }
    CloseClipboard();
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_SETCLIPBOARD )
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

   // Check params
   //
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

   // Get text from PRG
   //
   cText = hb_parcx( 1 );
   nLen  = hb_parclen( 1 );

   // Allocate a global memory object for the text.
   //
   hglbCopy = GlobalAlloc( GMEM_MOVEABLE, ( nLen+1 ) * sizeof( TCHAR ) );
   if ( hglbCopy == NULL )
   {
       CloseClipboard();
       hb_retl( FALSE );
       return;
   }

   // Lock the handle and copy the text to the buffer.
   //
   lptstrCopy = ( LPSTR ) GlobalLock( hglbCopy );
   memcpy( lptstrCopy, cText, ( nLen+1 ) * sizeof( TCHAR ) );
   lptstrCopy[ nLen+1 ] = ( TCHAR ) 0;    // null character
   GlobalUnlock( hglbCopy );

   // Place the handle on the clipboard.
   //
   SetClipboardData( CF_TEXT, hglbCopy );

   CloseClipboard();
   hb_retl( TRUE );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_PASTEFROMCLIPBOARD )
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
         for ( ul=0; ul < GlobalSize( hglb ); ul++ )
         {
            hb_wvt_gtAddCharToInputQueue( ( int ) lptstr[ ul ] );
         }
         GlobalUnlock( hglb ) ;
      }
   }
   CloseClipboard();
}

//-------------------------------------------------------------------//

