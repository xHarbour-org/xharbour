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
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                      GUI Drawing Functions
//               Pritpal Bedi <pritpal@vouchcac.com>
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//   Wvt_SetPen( nPenStyle, nWidth, nColor )
//
HB_FUNC( WVT_SETPEN )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
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
      if ( _s->currentPen )
      {
         DeleteObject( _s->currentPen );
      }
      _s->currentPen = hPen;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//-------------------------------------------------------------------//
//
//   Wvt_SetBrush( nStyle, nColor, [ nHatch ] )
//
HB_FUNC( WVT_SETBRUSH )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   HBRUSH   hBrush;
   LOGBRUSH lb;

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
      if ( _s->currentBrush )
      {
         DeleteObject( _s->currentBrush );
      }
      _s->currentBrush = hBrush;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//-------------------------------------------------------------------//
//
//   Wvt_DrawBoxGet( nRow, nCol, nWidth )
//
HB_FUNC( WVT_DRAWBOXGET )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT xy;
   POINT yz;

   xy = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   yz = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ) + hb_parni( 3 ), hb_parni( 1 ) + 1 );


   SelectObject( _s->hdc, _s->penBlack );

   MoveToEx( _s->hdc, xy.x-1, xy.y-1, NULL );        // Top Inner
   LineTo( _s->hdc, yz.x-1, xy.y-1 );

   MoveToEx( _s->hdc, xy.x-1, xy.y-1, NULL );        // Left Inner
   LineTo( _s->hdc, xy.x-1, yz.y-1 );

   SelectObject( _s->hdc, _s->penDarkGray );

   MoveToEx( _s->hdc, xy.x-2, xy.y-2, NULL );        // Top Outer
   LineTo( _s->hdc, yz.x, xy.y-2 );

   MoveToEx( _s->hdc, xy.x-2, xy.y-2, NULL );        // Top Inner
   LineTo( _s->hdc, xy.x-2, yz.y );

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//   Wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight )
//
HB_FUNC( WVT_DRAWBOXRAISED )
{
   POINT xy;
   int   iTop, iLeft, iBottom, iRight;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 )+1, hb_parni( 3 )+1 );
   iBottom = xy.y;
   iRight  = xy.x;

   hb_retl( hb_wvt_gtDrawBoxRaised( iTop, iLeft, iBottom, iRight ) );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawBoxRecessed( nTop, nLeft, nBottom, nRight )
//
HB_FUNC( WVT_DRAWBOXRECESSED )
{
   POINT xy;
   int   iTop, iLeft, iBottom, iRight;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y;
   iRight  = xy.x;

   hb_retl( hb_wvt_gtDrawBoxRecessed( iTop, iLeft, iBottom, iRight ) );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawBoxGroup( nTop, nLeft, nBottom, nRight )
//
HB_FUNC( WVT_DRAWBOXGROUP )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT xy;
   int   iTop, iLeft, iBottom, iRight;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y;
   iRight  = xy.x;

   SelectObject( _s->hdc, _s->penDarkGray );

   MoveToEx( _s->hdc, iRight, iTop, NULL );           // Right Inner
   LineTo( _s->hdc, iRight, iBottom );

   MoveToEx( _s->hdc, iLeft, iBottom, NULL );         // Bottom Inner
   LineTo( _s->hdc, iRight, iBottom );

   MoveToEx( _s->hdc, iLeft - 1, iTop - 1, NULL );    // Left Outer
   LineTo( _s->hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( _s->hdc, iLeft - 1, iTop - 1, NULL );    // Top Outer
   LineTo( _s->hdc, iRight + 1, iTop - 1 );


   SelectObject( _s->hdc, _s->penWhite );

   MoveToEx( _s->hdc, iRight + 1, iTop, NULL );       // Right Outer
   LineTo( _s->hdc, iRight + 1, iBottom + 1 );

   MoveToEx( _s->hdc, iLeft -1, iBottom + 1, NULL );  // Bottom Outer
   LineTo( _s->hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx( _s->hdc, iLeft, iTop, NULL );            // Left Inner
   LineTo( _s->hdc, iLeft, iBottom );

   MoveToEx( _s->hdc, iLeft, iTop, NULL );            // Top Inner
   LineTo( _s->hdc, iRight, iTop );

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight )
//
HB_FUNC( WVT_DRAWBOXGROUPRAISED )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT xy;
   int   iTop, iLeft, iBottom, iRight;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y;
   iRight  = xy.x;


   SelectObject( _s->hdc, _s->penWhite );

   MoveToEx( _s->hdc, iRight, iTop, NULL );           // Right Inner
   LineTo( _s->hdc, iRight, iBottom );

   MoveToEx( _s->hdc, iLeft, iBottom, NULL );         // Bottom Inner
   LineTo( _s->hdc, iRight, iBottom );

   MoveToEx( _s->hdc, iLeft - 1, iTop - 1, NULL );    // Left Outer
   LineTo( _s->hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( _s->hdc, iLeft - 1, iTop - 1, NULL );    // Top Outer
   LineTo( _s->hdc, iRight + 1, iTop - 1 );

   SelectObject( _s->hdc, _s->penDarkGray );

   MoveToEx( _s->hdc, iRight + 1, iTop, NULL );       // Right Outer
   LineTo( _s->hdc, iRight + 1, iBottom + 1 );

   MoveToEx( _s->hdc, iLeft -1, iBottom + 1, NULL );  // Bottom Outer
   LineTo( _s->hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx( _s->hdc, iLeft, iTop, NULL );            // Left Inner
   LineTo( _s->hdc, iLeft, iBottom );

   MoveToEx( _s->hdc, iLeft, iTop, NULL );            // Top Inner
   LineTo( _s->hdc, iRight, iTop );

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawImage( nTop, nLeft, nBottom, nRight, cImage/nPictureSlot )
//
HB_FUNC( WVT_DRAWIMAGE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT xy;
   int   iLeft, iTop, iRight, iBottom;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y ;
   iLeft   = xy.x ;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y - 1;
   iRight  = xy.x - 1;

   if ( ISNUM( 5 ) )
   {
      hb_wvt_gtRenderPicture( iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, _s->iPicture[ hb_parni( 5 )-1 ] ) ;
   }
   else
   {
      hb_wvt_gtDrawImage( iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, hb_parcx( 5 ) ) ;
   }

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    WVT_DRAWLABEL( nRow, nCol, cLabel, nAlign, nEscapement, nTextColor, nBkColor,
//                     cFontFace, , , , ,  )
//
HB_FUNC( WVT_DRAWLABEL )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT    xy;
   HFONT    hFont, oldFont;
   LOGFONT  logfont;
   int      oldTextAlign;
   COLORREF oldBkColor, oldTextColor;

   logfont.lfEscapement     = ( ISNIL(  5 ) ? 0 : ( hb_parni( 5 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( ISNIL( 11 ) ? 0 : hb_parni( 11 ) );
   logfont.lfItalic         = ( ISNIL( 14 ) ? 0 : hb_parl( 14 ) );
   logfont.lfUnderline      = ( ISNIL( 15 ) ? 0 : hb_parl( 15 ) );
   logfont.lfStrikeOut      = ( ISNIL( 16 ) ? 0 : hb_parl( 16 ) );
   logfont.lfCharSet        = ( ISNIL( 13 ) ? _s->CodePage : hb_parni( 13 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( ISNIL( 12 ) ? DEFAULT_QUALITY : hb_parni( 12 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( ISNIL(  9 ) ? _s->fontHeight : hb_parni(  9 ) );
   logfont.lfWidth          = ( ISNIL( 10 ) ? (_s->fontWidth < 0 ? -_s->fontWidth : _s->fontWidth ) : hb_parni( 10 ) );

   strcpy( logfont.lfFaceName, ( ISNIL( 8 ) ? _s->fontFace : hb_parcx( 8 ) ) );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      xy           = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
      oldBkColor   = SetBkColor( _s->hdc, ISNIL( 7 ) ? _s->background : ( COLORREF ) hb_parnl( 7 ) );
      oldTextColor = SetTextColor( _s->hdc, ISNIL( 6 ) ? _s->foreground : ( COLORREF ) hb_parnl( 6 ) );
      oldTextAlign = SetTextAlign( _s->hdc, ( ISNIL( 4 ) ? TA_LEFT : hb_parni( 4 ) ) );
      oldFont      = ( HFONT ) SelectObject( _s->hdc, hFont );

      //  Ground is Ready, Drat the Text
      //
      ExtTextOut( _s->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 3 ), strlen( hb_parcx( 3 ) ), NULL );

      //  Restore Old Settings
      //
      SelectObject( _s->hdc, oldFont );
      DeleteObject( hFont );
      SetTextAlign( _s->hdc, oldTextAlign );
      SetBkColor( _s->hdc, oldBkColor );
      SetTextColor( _s->hdc, oldTextColor );

      hb_retl( TRUE );
   }

   hb_retl( FALSE );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawOutline( nTop, nLeft, nBottom, nRight, nThick, nShape, nRGBColor )
//
HB_FUNC( WVT_DRAWOUTLINE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   HPEN  hPen;
   POINT xy;
   int   iTop, iLeft, iBottom, iRight;
   BOOL  bResult;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 )+1, hb_parni( 3 )+1 );
   iBottom = xy.y;
   iRight  = xy.x;

   if ( ISNUM( 5 ) )
   {
      hPen = CreatePen( hb_parni( 5 ), 0, ( ISNIL( 6 ) ? 0 : ( COLORREF ) hb_parnl( 6 ) ) );
      if ( hPen )
      {
         SelectObject( _s->hdc, hPen );
      }
   }
   else
   {
      hPen = 0;
      SelectObject( _s->hdc, _s->penBlack );
   }

   bResult = hb_wvt_gtDrawOutline( iTop, iLeft, iBottom, iRight );

   if ( hPen )
   {
      DeleteObject( hPen );
   }

   hb_retl( bResult );
}

//-------------------------------------------------------------------//
//                  1      2       3       4        5        6       7       8       9      10
//   Wvt_DrawLine( nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nStyle, nThick, nColor )
//
HB_FUNC( WVT_DRAWLINE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT    xy;
   int      iTop, iLeft, iBottom, iRight, iOffset ;
   int      iOrient, iFormat, iAlign, iStyle, iThick;
   int      x, y;
   COLORREF cr;
   HPEN     hPen;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   //   Resolve Parameters
   iOrient = ISNIL( 5 ) ? 0 : hb_parni( 5 );
   iFormat = ISNIL( 6 ) ? 0 : hb_parni( 6 );
   iAlign  = ISNIL( 7 ) ? 0 : hb_parni( 7 );
   iStyle  = ISNIL( 8 ) ? 0 : hb_parni( 8 );
   iThick  = ISNIL( 9 ) ? 0 : hb_parni( 9 );
   cr      = ISNIL(10 ) ? 0 : ( COLORREF ) hb_parnl( 10 );

   x       = iLeft ;
   y       = iTop ;

   switch ( iAlign )
   {
      case 0:                  // Center
      {
         if ( iOrient == 0 )   // Horizontal
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

      case 1:                  // Top
      break;

      case 2:                  // bottom
      {
         if ( iFormat == 0 || iFormat == 1 )  // Raised/Recessd
         {
            y = iBottom - 1;
         }
      }
      break;

      case 3:                  // Left
      break;

      case 4:                  // Right
      {
         if ( iFormat == 0 || iFormat == 1 )  // Raised/Recessd
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
      case 0:                                       // Raised
      {
         if ( iOrient == 0 )                        //  Horizontal
         {
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y+1, NULL );
            LineTo( _s->hdc, iRight, y+1 );
         }
         else                                       //  Vertical
         {
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x+1, y, NULL );
            LineTo( _s->hdc, x+1, iBottom );
         }
      }
      break;

      case 1:                                      // Recessed
      {
         if ( iOrient == 0 )                       // Horizontal
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y+1, NULL );
            LineTo( _s->hdc, iRight, y+1 );
         }
         else                                      //  Vertical
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x+1, y, NULL );
            LineTo( _s->hdc, x+1, iBottom );
         }
      }
      break;

      case 2:                                      // Plain
      {
         if ( iOrient == 0 )                       // Horizontal
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
         }
         else                                      //  Vertical
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
         }
       }
      break;
   }

   DeleteObject( hPen );
   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    Inside the area requested!
//    Wvt_DrawEllipse( nTop, nLeft, nBottom, nRight )
//
HB_FUNC( WVT_DRAWELLIPSE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT xy;
   int   iTop, iLeft, iBottom, iRight;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   SelectObject( _s->hdc, _s->currentBrush );
   SelectObject( _s->hdc, _s->currentPen );

   hb_retl( Ellipse( _s->hdc, iLeft, iTop, iRight, iBottom ) );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawRectangle( nTop, nLeft, nBottom, nRight )
//
HB_FUNC( WVT_DRAWRECTANGLE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT xy;
   int   iTop, iLeft, iBottom, iRight;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   SelectObject( _s->hdc, _s->currentBrush );
   SelectObject( _s->hdc, _s->currentPen );

   hb_retl( Rectangle( _s->hdc, iLeft, iTop, iRight, iBottom ) );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawRoundRect( nTop, nLeft, nBottom, nRight, nRoundHeight, nRoundWidth )
//
HB_FUNC( WVT_DRAWROUNDRECT )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT xy;
   int   iTop, iLeft, iBottom, iRight, iWd, iHt;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   iWd     = ISNIL( 6 ) ? 0 : hb_parni( 6 );
   iHt     = ISNIL( 5 ) ? 0 : hb_parni( 5 );

   SelectObject( _s->hdc, _s->currentBrush );
   SelectObject( _s->hdc, _s->currentPen   );

   hb_retl( RoundRect( _s->hdc, iLeft, iTop, iRight, iBottom, iWd, iHt ) );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawFocusRect( nTop, nLeft, nBottom, nRight )
//
HB_FUNC( WVT_DRAWFOCUSRECT )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   RECT  rc;
   POINT xy;

   xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   rc.top    = xy.y;
   rc.left   = xy.x;

   xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   rc.bottom = xy.y-1;
   rc.right  = xy.x-1;

   hb_retl( DrawFocusRect( _s->hdc, &rc ) );
}

//-------------------------------------------------------------------//
//
//   Wvt_DrawGridHorz( nTop, nLeft, nRight, nRows )
//
HB_FUNC( WVT_DRAWGRIDHORZ )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   int   iAtRow = hb_parni( 1 );
   int   iRows  = hb_parni( 4 );
   int   i, y;
   int   iLeft, iRight;

   iLeft  = ( hb_parni( 2 ) * _s->PTEXTSIZE.x );
   iRight = ( ( ( hb_parni( 3 ) + 1 ) * _s->PTEXTSIZE.x ) - 1 );

   SelectObject( _s->hdc, _s->currentPen );

   for ( i = 0; i < iRows; i++ )
   {
      y = ( ( iAtRow ) * _s->PTEXTSIZE.y );

      MoveToEx( _s->hdc, iLeft, y, NULL );
      LineTo( _s->hdc, iRight, y );

      iAtRow++;
   }

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//     Wvt_DrawGridVert( nTop, nBottom, aCols, nCols )
//
HB_FUNC( WVT_DRAWGRIDVERT )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   int iTop, iBottom, x, i, iCharHeight, iCharWidth;
   int iTabs = hb_parni( 4 );

   if ( ! iTabs )
   {
      hb_retl( FALSE );
   }

   iCharWidth  = _s->PTEXTSIZE.x;
   iCharHeight = _s->PTEXTSIZE.y;

   iTop    = ( hb_parni( 1 ) * iCharHeight );
   iBottom = ( ( hb_parni( 2 ) + 1 ) * iCharHeight ) - 1;

   SelectObject( _s->hdc, _s->currentPen );

   for ( i = 1; i <= iTabs; i++ )
   {
      x = ( hb_parni( 3,i ) * iCharWidth );

      MoveToEx( _s->hdc, x, iTop, NULL );
      LineTo( _s->hdc, x, iBottom );
   }

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawButton( nTop, nLeft, nBottom, nRight, cText, cnImage, ;
//                    nFormat, nTextColor, nBkColor, nImageAt ) ;
//
HB_FUNC( WVT_DRAWBUTTON )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   SIZE     sz;
   POINT    xy;
   RECT     rc;
   int      iTop, iLeft, iBottom, iRight;
   int      iAlign, oldTextAlign, oldBkMode;
   int      iTextHeight /*, iTextWidth */ ;
   int      iImageWidth, iImageHeight;
   COLORREF /* oldBkColor, */ oldTextColor;
   LOGBRUSH lb;
   HBRUSH   hBrush;
   IPicture *iPicture;

   BOOL     bText     = ISCHAR( 5 );
   BOOL     bImage    = !( ISNIL( 6 ) );
   int      iFormat   = ISNIL(  7 ) ? 0 : hb_parni( 7 );
   COLORREF textColor = ISNIL(  8 ) ? hb_wvt_gtGetColorData( 0 ) : ( COLORREF ) hb_parnl( 8 ) ;
   COLORREF bkColor   = ISNIL(  9 ) ? hb_wvt_gtGetColorData( 7 ) : ( COLORREF ) hb_parnl( 9 ) ;
   // int      iImageAt  = ISNIL( 10 ) ? 0 : hb_parni( 10 );

   xy         = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop       = xy.y;
   iLeft      = xy.x;

   xy         = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom    = xy.y-1;
   iRight     = xy.x-1;

   lb.lbStyle = BS_SOLID;
   lb.lbColor = bkColor;
   lb.lbHatch = 0;

   hBrush     = CreateBrushIndirect( &lb );

   rc.left    = iLeft ;
   rc.top     = iTop ;
   rc.right   = iRight  + 1;
   rc.bottom  = iBottom + 1;

   FillRect( _s->hdc, &rc, hBrush );

   DeleteObject( hBrush );

   switch ( iFormat )
   {
      case 1:
         hb_wvt_gtDrawBoxRecessed( iTop+1, iLeft+1, iBottom-1, iRight-1 );
         break;
      case 2:
         break;
      case 3:
         hb_wvt_gtDrawOutline( iTop, iLeft, iBottom, iRight );
         break;
      default:
         hb_wvt_gtDrawBoxRaised( iTop+1, iLeft+1, iBottom-1, iRight-1 );
         break;
   }

   if ( bText )
   {
      ( HFONT ) SelectObject( _s->hdc, GetStockObject( DEFAULT_GUI_FONT ) );

      GetTextExtentPoint32( _s->hdc, hb_parcx( 5 ), strlen( hb_parcx( 5 ) ), &sz );
      // iTextWidth   = sz.cx;
      iTextHeight  = sz.cy;

      xy.x = iLeft + ( ( iRight - iLeft + 1 ) / 2 ) ;

      if ( bImage )
      {
         xy.y = ( iBottom - 2 - iTextHeight );
      }
      else
      {
         xy.y = iTop + ( ( iBottom - iTop + 1 ) / 2 ) - ( iTextHeight / 2 );
      }

      if ( iFormat == 1 )
      {
         xy.x = xy.x + 2;
         xy.y = xy.y + 2;
      }

      iAlign = TA_CENTER + TA_TOP ;

      oldTextAlign = SetTextAlign( _s->hdc, iAlign );
      oldBkMode    = SetBkMode( _s->hdc, TRANSPARENT );
      oldTextColor = SetTextColor( _s->hdc, textColor );

      ExtTextOut( _s->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 5 ), strlen( hb_parcx( 5 ) ), NULL );

      SetTextColor( _s->hdc, oldTextColor );
      SetBkMode( _s->hdc, oldBkMode );
      SetTextAlign( _s->hdc, oldTextAlign );
   }
   else
   {
      iTextHeight = -1;
   }

   if ( bImage )
   {
      iImageWidth = ( iRight - iLeft + 1 - 8 );

      iImageHeight = ( iBottom - iTop + 1 - 8 - iTextHeight );

      if ( ISNUM( 6 ) )
      {
         iPicture = _s->iPicture[ hb_parni( 6 ) - 1 ];
         hb_wvt_gtRenderPicture( iLeft+4, iTop+4, iImageWidth, iImageHeight, iPicture );
      }
      else
      {
         hb_wvt_gtDrawImage( iLeft+4, iTop+4, iImageWidth, iImageHeight, hb_parcx( 6 ) );
      }
   }

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_DRAWSTATUSBAR )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   int   iPanels   = hb_parni( 1 );
   int   i, iNext;
   int   iTop, iLeft, iBottom, iRight;
   POINT xy;

   iNext = 0;

   for ( i = 0; i < iPanels; i++ )
   {
      xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2, iNext+2 ), hb_parni( 2, iNext+1 ) );
      iTop    = xy.y;
      iLeft   = xy.x + 1;

      xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2, iNext+4 ), hb_parni( 2, iNext+3 )+1 );
      iBottom = xy.y - 1;
      iRight  = xy.x - 2;

      SelectObject( _s->hdc, _s->penWhite );

      MoveToEx( _s->hdc, iRight, iTop, NULL );            // Right
      LineTo( _s->hdc, iRight, iBottom );

      MoveToEx( _s->hdc, iLeft, iBottom, NULL );          // Bottom
      LineTo( _s->hdc, iRight, iBottom );

      SelectObject( _s->hdc, _s->penDarkGray );

      MoveToEx( _s->hdc, iLeft, iTop, NULL );             // Left
      LineTo( _s->hdc, iLeft, iBottom );

      MoveToEx( _s->hdc, iLeft, iTop, NULL );             // Top
      LineTo( _s->hdc, iRight, iTop );

      iNext = iNext + 4;
   }

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2, 4 * iPanels ), hb_parni( 2, ( 4 * iPanels ) - 1 )+1 );
   iTop    = xy.y - 2;
   iLeft   = xy.x - 2;
   iBottom = iTop;
   iRight  = iLeft;

   SelectObject( _s->hdc, _s->penBlack );

   MoveToEx( _s->hdc, iLeft-4, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-4 );
   MoveToEx( _s->hdc, iLeft-7, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-7 );
   MoveToEx( _s->hdc, iLeft-10, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-10 );

   SelectObject( _s->hdc, _s->penWhite );

   MoveToEx( _s->hdc, iLeft-5, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-5 );
   MoveToEx( _s->hdc, iLeft-8, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-8 );
   MoveToEx( _s->hdc, iLeft-11, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-11 );
}

//-------------------------------------------------------------------//
//
//  Wvt_DrawPicture( nTop, nLeft, nBottom, nRight, nSlot, aAdj ) -> lOk
//  nSlot <= 20  aAdj == { 0,0,-2,-2 } To Adjust the pixels for { Top,Left,Bottom,Right }
//
HB_FUNC( WVT_DRAWPICTURE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT    xy;
   int      iTop, iLeft, iBottom, iRight;
   int      iSlot   = hb_parni( 5 ) - 1;

   if ( iSlot < WVT_PICTURES_MAX )
   {
      if ( _s->iPicture[ iSlot ] )
      {
         xy       = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
         iTop     = xy.y + hb_parni( 6,1 );
         iLeft    = xy.x + hb_parni( 6,2 );

         xy       = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
         iBottom  = xy.y-1 + hb_parni( 6,3 );
         iRight   = xy.x-1 + hb_parni( 6,4 );

         hb_retl( hb_wvt_gtRenderPicture( iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, _s->iPicture[ iSlot ] ) );
      }
   }
}

//-------------------------------------------------------------------//
//
//    WVT_DRAWLABELEX( nRow, nCol, cLabel, nAlign, nTextColor, nBkColor, nSlotFont )
//
HB_FUNC( WVT_DRAWLABELEX )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT    xy;
   HFONT    oldFont;
   int      oldTextAlign;
   COLORREF oldBkColor, oldTextColor;
   int      iSlot = hb_parni( 7 ) - 1;

   if ( _s->hUserFonts[ iSlot ] )
   {
      xy           = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
      oldBkColor   = SetBkColor( _s->hdc, ISNIL( 6 ) ? _s->background : ( COLORREF ) hb_parnl( 6 ) );
      oldTextColor = SetTextColor( _s->hdc, ISNIL( 5 ) ? _s->foreground : ( COLORREF ) hb_parnl( 5 ) );
      oldTextAlign = SetTextAlign( _s->hdc, ( ISNIL( 4 ) ? TA_LEFT : hb_parni( 4 ) ) );
      oldFont      = ( HFONT ) SelectObject( _s->hdc, _s->hUserFonts[ iSlot ] );

      //  Ground is Ready, Drat the Text
      //
      ExtTextOut( _s->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 3 ), strlen( hb_parcx( 3 ) ), NULL );

      //  Restore Old Settings
      //
      SelectObject( _s->hdc, oldFont );
      SetTextAlign( _s->hdc, oldTextAlign );
      SetBkColor( _s->hdc, oldBkColor );
      SetTextColor( _s->hdc, oldTextColor );

      hb_retl( TRUE );
   }

   hb_retl( FALSE );
}

//-------------------------------------------------------------------//
//                  1      2       3       4        5        6       7       8
//   Wvt_DrawLine( nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nSlotPen )
//
HB_FUNC( WVT_DRAWLINEEX )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT    xy;
   int      iTop, iLeft, iBottom, iRight, iOffset ;
   int      iOrient, iFormat, iAlign ;
   int      x, y;
   HPEN     hPen;
   int      iSlot = hb_parni( 8 ) - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   //   Resolve Parameters
   iOrient = ISNIL( 5 ) ? 0 : hb_parni( 5 );
   iFormat = ISNIL( 6 ) ? 0 : hb_parni( 6 );
   iAlign  = ISNIL( 7 ) ? 0 : hb_parni( 7 );

   x       = iLeft ;
   y       = iTop ;

   switch ( iAlign )
   {
      case 0:                  // Center
      {
         if ( iOrient == 0 )   // Horizontal
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

      case 1:                  // Top
      break;

      case 2:                  // bottom
      {
         if ( iFormat == 0 || iFormat == 1 )  // Raised/Recessd
         {
            y = iBottom - 1;
         }
      }
      break;

      case 3:                  // Left
      break;

      case 4:                  // Right
      {
         if ( iFormat == 0 || iFormat == 1 )  // Raised/Recessd
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

   hPen = _s->hUserPens[ iSlot ];

   switch ( iFormat )
   {
      case 0:                                       // Raised
      {
         if ( iOrient == 0 )                        //  Horizontal
         {
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y+1, NULL );
            LineTo( _s->hdc, iRight, y+1 );
         }
         else                                       //  Vertical
         {
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x+1, y, NULL );
            LineTo( _s->hdc, x+1, iBottom );
         }
      }
      break;

      case 1:                                      // Recessed
      {
         if ( iOrient == 0 )                       // Horizontal
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y+1, NULL );
            LineTo( _s->hdc, iRight, y+1 );
         }
         else                                      //  Vertical
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x+1, y, NULL );
            LineTo( _s->hdc, x+1, iBottom );
         }
      }
      break;

      case 2:                                      // Plain
      {
         if ( iOrient == 0 )                       // Horizontal
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
         }
         else                                      //  Vertical
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
         }
       }
      break;
   }

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawOutlineEx( nTop, nLeft, nBottom, nRight, nSlotPen )
//
HB_FUNC( WVT_DRAWOUTLINEEX )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT xy;
   int   iTop, iLeft, iBottom, iRight;
   int   iSlot = hb_parni( 5 ) - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 )+1, hb_parni( 3 )+1 );
   iBottom = xy.y;
   iRight  = xy.x;

   if ( _s->hUserPens[ iSlot ] )
   {
      SelectObject( _s->hdc, _s->hUserPens[ iSlot ] );
   }
   else
   {
      SelectObject( _s->hdc, _s->penBlack );
   }

   hb_wvt_gtDrawOutline( iTop, iLeft, iBottom, iRight );
}

//-------------------------------------------------------------------//
//
//   Wvt_LoadPicture( nSlot, cFilePic )
//
HB_FUNC( WVT_LOADPICTURE )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   IPicture * iPicture = hb_wvt_gtLoadPicture( hb_parcx( 2 ) );
   BOOL       bResult  = FALSE;
   int        iSlot    = hb_parni( 1 ) - 1 ;

   if ( iPicture )
   {
      if ( _s->iPicture[ iSlot ] )
      {
         hb_wvt_gtDestroyPicture( _s->iPicture[ iSlot ] );
      }

      _s->iPicture[ iSlot ] = iPicture;
      bResult = TRUE;
   }
   hb_retl( bResult );
}

//-------------------------------------------------------------------//
//
// Wvt_LoadFont( nSlotFont, cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout,
//               nCharSet, nQuality, nEscapement )
//
HB_FUNC( WVT_LOADFONT )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   LOGFONT  logfont;
   int      iSlot = hb_parni( 1 ) - 1;
   HFONT    hFont;

   logfont.lfEscapement     = ( ISNIL( 11 ) ? 0 : ( hb_parni( 11 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( ISNIL(  5 ) ? 0 : hb_parni( 5 ) );
   logfont.lfItalic         = ( ISNIL(  6 ) ? 0 : hb_parl(  6 ) );
   logfont.lfUnderline      = ( ISNIL(  7 ) ? 0 : hb_parl(  7 ) );
   logfont.lfStrikeOut      = ( ISNIL(  8 ) ? 0 : hb_parl(  8 ) );
   logfont.lfCharSet        = ( ISNIL(  9 ) ? _s->CodePage : hb_parni( 9 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( ISNIL( 10 ) ? DEFAULT_QUALITY : hb_parni( 10 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( ISNIL(  3 ) ? _s->fontHeight : hb_parni( 3 ) );
   logfont.lfWidth          = ( ISNIL(  4 ) ? ( _s->fontWidth < 0 ? -_s->fontWidth : _s->fontWidth ) : hb_parni( 4 ) );

   strcpy( logfont.lfFaceName, ( ISNIL( 2 ) ? _s->fontFace : hb_parcx( 2 ) ) );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      if ( _s->hUserFonts[ iSlot ] )
      {
         DeleteObject( _s->hUserFonts[ iSlot ] );
      }
      _s->hUserFonts[ iSlot ] = hFont;
   }
}

//-------------------------------------------------------------------//
//
//  Wvt_LoadPen( nSlot, nStyle, nWidth, nRGBColor )
//
HB_FUNC( WVT_LOADPEN )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   int      iPenWidth, iPenStyle;
   COLORREF crColor;
   HPEN     hPen;
   int      iSlot = hb_parni( 1 ) - 1;

   iPenStyle = ISNIL( 2 ) ? 0 : hb_parni( 2 ) ;
   iPenWidth = ISNIL( 3 ) ? 0 : hb_parni( 3 );
   crColor   = ISNIL( 4 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 4 );

   hPen      = CreatePen( iPenStyle, iPenWidth, crColor );

   if ( hPen )
   {
      if ( _s->hUserPens[ iSlot ] )
      {
         DeleteObject( _s->hUserPens[ iSlot ] );
      }
      _s->hUserPens[ iSlot ] = hPen;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//-------------------------------------------------------------------//
//
//   aScr := Wvt_SaveScreen( nTop, nLeft, nBottom, nRight )
//
HB_FUNC( WVT_SAVESCREEN )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   HBITMAP  hBmp, oldBmp;
   POINT    xy;
   int      iTop, iLeft, iBottom, iRight, iWidth, iHeight;
   HB_ITEM  info;
   HB_ITEM  temp;

   info.type = HB_IT_NIL;
   temp.type = HB_IT_NIL;

   xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop      = xy.y;
   iLeft     = xy.x;

   xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom   = xy.y-1;
   iRight    = xy.x-1;

   iWidth    = iRight - iLeft + 1;
   iHeight   = iBottom - iTop + 1;

   hBmp      = CreateCompatibleBitmap( _s->hdc, iWidth, iHeight ) ;

   oldBmp = (HBITMAP) SelectObject( _s->hCompDC, hBmp );
   BitBlt( _s->hCompDC, 0, 0, iWidth, iHeight, _s->hdc, iLeft, iTop, SRCCOPY );
   SelectObject( _s->hCompDC, oldBmp );

   hb_arrayNew( &info, 3 );

   hb_arraySetForward( &info, 1, hb_itemPutNI( &temp, iWidth ) );
   hb_arraySetForward( &info, 2, hb_itemPutNI( &temp, iHeight ) );
   hb_arraySetForward( &info, 3, hb_itemPutNL( &temp, ( ULONG ) hBmp ) );

   hb_itemReturn( &info );
}

//-------------------------------------------------------------------//
//
//   Wvt_RestScreen( nTop, nLeft, nBottom, nRight, aScr, lDoNotDestroyBMP )
//
HB_FUNC( WVT_RESTSCREEN )
{
   GLOBAL_DATA *_s = hb_wvt_gtGetGlobalData();
   POINT   xy;
   int     iTop, iLeft, iBottom, iRight, iWidth, iHeight;
   HBITMAP hBmp;

   BOOL    bResult = FALSE;
   BOOL    bDoNotDestroyBMP = ISNIL( 6 ) ? FALSE : hb_parl( 6 );

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   iWidth  = iRight - iLeft + 1 ;
   iHeight = iBottom - iTop + 1 ;

   hBmp    = (HBITMAP) SelectObject( _s->hCompDC, ( HBITMAP ) hb_parnl( 5,3 ) );
   if ( hBmp )
   {
      if ( ( iWidth == hb_parni( 5,1 ) )  && ( iHeight == hb_parni( 5,2 ) ) )
      {
         if ( BitBlt( _s->hdc,
                      iLeft,
                      iTop,
                      iWidth,
                      iHeight,
                      _s->hCompDC,
                      0,
                      0,
                      SRCCOPY ) )
         {
            bResult = TRUE;
         }
      }
      else
      {
         if ( StretchBlt( _s->hdc,
                          iLeft,
                          iTop,
                          iWidth,
                          iHeight,
                          _s->hCompDC,
                          0,
                          0,
                          hb_parni( 5,1 ),
                          hb_parni( 5,2 ),
                          SRCCOPY ) )
         {
            bResult = TRUE;
         }
      }
   }

   SelectObject( _s->hCompDC, hBmp );

   if ( ! bDoNotDestroyBMP )
   {
      DeleteObject( ( HBITMAP ) hb_parnl( 5,3 ) );
   }

   hb_retl( bResult );
}

//-------------------------------------------------------------------//

