/*
 * $Id: setclear.c,v 1.0 2004/10/23 23:00:00 oh1 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Functions:
 * SETCLEARA(), SETCLEARB(), GETCLEARA(), GETCLEARB()
 * CLEARWIN()
 *
 * Copyright 2004 Henryk Olkowski <oh1@op.pl>
 * www - http://www.xharbour.org
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

#include "hbapi.h"
#include "hbapigt.h"
/****************************************************************************/
HB_FUNC( GETCLEARA )
{
   hb_retni( hb_ctGetClearA() );
}
/****************************************************************************/
HB_FUNC( GETCLEARB )
{
   hb_retni( hb_ctGetClearB() );
}
/****************************************************************************/
/* Changes the default attribute for screen clear */
HB_FUNC( SETCLEARA )
{
   if( hb_pcount() >= 1 && ISCHAR( 1 ) )
   {
      hb_ctSetClearA( hb_gtColorToN( hb_parcx( 1 ) ) );
   }
   else if( hb_pcount() >= 1 && ISNUM( 1 ) )
   {
      hb_ctSetClearA( hb_parni( 1 ) );
   }
   else
   {
      hb_ctSetClearA( 7 );
   }

   hb_retclen( "", 0);
}
/****************************************************************************/
/* Changes the default character for screen clear */
HB_FUNC( SETCLEARB )
{
   if( hb_pcount() >= 1 && ISCHAR( 1 ) )
   {
      hb_ctSetClearB( hb_parcx( 1 )[ 0 ] );
   }
   else if( hb_pcount() >= 1 && ISNUM( 1 ) )
   {
      hb_ctSetClearB( hb_parni( 1 ) );
   }
   else
   {
      hb_ctSetClearB( 255 );
   }

   hb_retclen( "", 0);
}
/****************************************************************************/
/* Clears a screen area */
HB_FUNC( CLEARWIN )
{
   SHORT     FRow, FCol, LRow, LCol, nAttr, pAttr, nChar, np;
   HB_CT_WND * wnd;

   wnd = hb_ctWCurrent();
   np  = hb_pcount();

   FRow = np >= 1 && ISNUM( 1 ) ? hb_parni( 1 ) : wnd->iRow;
   FCol = np >= 2 && ISNUM( 2 ) ? hb_parni( 2 ) : wnd->iCol;
   LRow = np >= 3 && ISNUM( 3 ) ? hb_parni( 3 ) : wnd->ULRow - wnd->UFRow;
   LCol = np >= 4 && ISNUM( 4 ) ? hb_parni( 4 ) : wnd->ULCol - wnd->UFCol;

   if( np >= 5 && ISCHAR( 5 ) )     nAttr = hb_gtColorToN( hb_parcx( 5 ) );
   else if( np >= 5 && ISNUM( 5 ) ) nAttr = hb_parni( 5 );
   else                             nAttr = hb_ctGetClearA();

   if( np >= 6 && ISCHAR( 6 ) )     nChar = hb_parcx( 6 )[ 0 ];
   else if( np >= 6 && ISNUM( 6 ) ) nChar = hb_parni( 6 );
   else                             nChar = hb_ctGetClearB();

   LRow = HB_MAX( 0, HB_MIN( LRow, wnd->ULRow - wnd->UFRow ) );
   LCol = HB_MAX( 0, HB_MIN( LCol, wnd->ULCol - wnd->UFCol ) );

   if( FRow >= 0 && FRow <= LRow &&
       FCol >= 0 && FCol <= LCol )
   {
      pAttr = hb_ctSetCurColor( nAttr );

      for( np = FRow; np <= LRow; np++ )
         hb_gtRepChar( np, FCol, nChar, LCol - FCol + 1 );

      hb_ctSetCurColor( pAttr );
   }

   hb_retclen( "", 0);
}
/****************************************************************************/
