/*
 * $Id: ctwin.c,v 1.2 2004/10/24 17:59:27 druzus Exp $
 */

/*
 * xHarbour Project source code:
 *   Windows functions for CT3
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

#include <ctype.h>

#include "hbapigt.h"
#include "hbset.h"
#include "hb_io.h"
#include "hbvm.h"
#include "inkey.ch"

/****************************************************************************/
/* CT3 Windows functions                                                    */
/****************************************************************************/
/*
  WACLOSE()    - Close all windows
  WBOARD()     - Allocates screen area for windows
  WBOX()       - Places a frame around the active window
  WCENTER()    - Returns a window to the visible area, or centers it
  WCLOSE()     - Close the active window
  WCOL()       - Return position of the leftmost column
  WFCOL()      - Return position of the leftmost column of formatted area
  WFLASTCOL()  - Return position of the rightmost column of formatted area
  WFLASTROW()  - Return position of the bottom row of formatted area
  WFORMAT()    - Set the usable area within a window
  WFROW()      - Return position of the top row of formatted area
  WLASTCOL()   - Return position of the rightmost column
  WLASTROW()   - Return position of the bottom row
  WMODE()      - Set the screen border overstep mode
  WMOVE()      - Moves a window
  WNUM()       - Get the highest windows handle
  WOPEN()      - Opens a new window
  WROW()       - Return position of the top row
  WSELECT()    - Activate window
  WSETMOVE()   - Set the interactive movement mode
  WSETSHADOW() - Set the window shadow color
  WSTACK()     - Get the array of windows handle                          (New)
  WSTEP()      - Set the step width of interactive window movement
*/

/****************************************************************************/
HB_FUNC( WACLOSE ) /* Close all windows */
{
   hb_retni( hb_ctWAClose() );
}
/****************************************************************************/
HB_FUNC( WBOARD ) /* Allocates screen area for windows */
{
   SHORT FRow, FCol, LRow, LCol;

   FRow = ISNUM( 1 ) ? hb_parni( 1 ) : 0;
   FCol = ISNUM( 2 ) ? hb_parni( 2 ) : 0;
   LRow = ISNUM( 3 ) ? hb_parni( 3 ) : hb_ctMaxRow( TRUE );
   LCol = ISNUM( 4 ) ? hb_parni( 4 ) : hb_ctMaxCol( TRUE );

   hb_retni( hb_ctWBoard( FRow, FCol, LRow, LCol ) );
}
/****************************************************************************/
HB_FUNC( WBOX )     /* Places a frame around the active window */
{
   char        * cBox, cBox2[ 10 ], c;
   HB_CT_WND   * wnd;
   int         i, j;
   static char * cWBOX[] = {
            _B_DOUBLE,        // 0  WB_DOUBLE_CLEAR
            _B_SINGLE,        // 1  WB_SINGLE_CLEAR
            _B_DOUBLE_SINGLE, // 2  WB_DOUBLE_SINGLE_CLEAR
            _B_SINGLE_DOUBLE, // 3  WB_SINGLE_DOUBLE_CLEAR

            _B_DOUBLE,        // 4  WB_DOUBLE
            _B_SINGLE,        // 5  WB_SINGLE
            _B_DOUBLE_SINGLE, // 6  WB_DOUBLE_SINGLE
            _B_SINGLE_DOUBLE, // 7  WB_SINGLE_DOUBLE

            "лплллмлл",       // 8  WB_HALF_FULL_CLEAR
            "опнннмоо",       // 9  WB_HALF_CLEAR
            "олнннлоо",       // 10 WB_FULL_HALF_CLEAR
            "лллллллл",       // 11 WB_FULL_CLEAR

            "лплллмлл",       // 12 WB_HALF_FULL
            "опнннмоо",       // 13 WB_HALF
            "олнннлоо",       // 14 WB_FULL_HALF
            "лллллллл" };     // 15 WB_FULL

   wnd = hb_ctWCurrent();

   if( ISCHAR( 1 ) )
   {
      i = 4;
      cBox = hb_parcx( 1 );
   }
   else if( ISNUM( 1 ) )
   {
      i = hb_parni( 1 );
      if( i < 0 || i > 15 ) i = 0;
      cBox = cWBOX[ i ];
   }
   else
   {
      i = 0;
      cBox = cWBOX[ i ];
   }

   c = ' ';
   for( j = 0; j < 9; j++ )
   {
      c = cBox[ j ];
      if( !c ) break;
      cBox2[ j ] = c;
   }
   for( ; j < 8; j++ ) cBox2[ j ] = c;

   if( ( i % 8 ) < 4 && j < 9 ) cBox2[ j++ ] = hb_ctGetClearB();
   cBox2[ j ] = '\0';

   if( wnd->ULRow - wnd->UFRow <= 1 || wnd->ULCol - wnd->UFCol <= 1 )
   {
      hb_retni( wnd->NCur );
      return;
   }

   hb_gtBox( 0, 0, wnd->ULRow - wnd->UFRow, wnd->ULCol - wnd->UFCol,
             ( BYTE * ) cBox2 );

   if( wnd->NCur == 0 )
   {
      if( wnd->iRow < 1 ) wnd->iRow = 1;
      if( wnd->iCol < 1 ) wnd->iCol = 1;
      hb_gtSetPos( wnd->iRow, wnd->iCol );
   }
   else
   {
      hb_ctWFormat( 1, 1, 1, 1 );
      hb_gtSetPos( 0, 0 );
   }

   hb_retni( wnd->NCur );
}
/****************************************************************************/
HB_FUNC( WCENTER )  /* Returns a window to the visible area, or centers it */
{
   hb_retni( hb_ctWCenter( hb_parl( 1 ) ) );
}
/****************************************************************************/
HB_FUNC( WCLOSE ) /* Close the active window */
{
   hb_retni( hb_ctWClose() );
}
/****************************************************************************/
HB_FUNC( WCOL ) /* Return position of the leftmost column */
{
   HB_CT_WND * wnd;
   SHORT       FCol;

   wnd = hb_ctWCurrent();

   FCol = wnd->WFCol;

   if( hb_parl( 1 ) )
   {
      FCol += ( wnd->BFCol + wnd->BLCol - wnd->WFCol - wnd->WLCol ) / 2;

      if( FCol < wnd->BFCol )
         FCol = wnd->BFCol;
      else if( FCol + wnd->WNCol - 1 > wnd->BLCol )
         FCol = wnd->BLCol - wnd->WNCol + 1;
   }

   hb_retni( FCol );
}
/****************************************************************************/
HB_FUNC( WFCOL ) /* Return position of the leftmost column of formatted area */
{
   HB_CT_WND * wnd;
   SHORT       UFCol;

   wnd = hb_ctWCurrent();

   UFCol = ( wnd->NCur ==0 ? wnd->BFCol : wnd->UFCol );
   if( hb_parl( 1 ) ) UFCol -= wnd->WFCol;

   hb_retni( UFCol );
}
/****************************************************************************/
HB_FUNC( WFLASTCOL ) /* Return position of the rightmost column of formatted area */
{
   HB_CT_WND * wnd;
   SHORT       ULCol;

   wnd = hb_ctWCurrent();

   ULCol = ( wnd->NCur ==0 ? wnd->BLCol : wnd->ULCol );

   if( hb_parl( 1 ) ) ULCol = wnd->WLCol - ULCol;

   hb_retni( ULCol );
}
/****************************************************************************/
HB_FUNC( WFLASTROW ) /* Return position of the bottom row of formatted area */
{
   HB_CT_WND * wnd;
   SHORT       ULRow;

   wnd = hb_ctWCurrent();

   ULRow = ( wnd->NCur ==0 ? wnd->BLRow : wnd->ULRow );

   if( hb_parl( 1 ) ) ULRow = wnd->WLRow - ULRow;

   hb_retni( ULRow );
}
/****************************************************************************/
HB_FUNC( WFORMAT ) /* Set the usable area within a window */
{
   SHORT NCur;

   NCur = hb_ctWSelect( -2 );

   if( NCur >= 1 )
      NCur = hb_ctWFormat( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                           hb_parni( 4 ) );

   hb_retni( NCur );
}
/****************************************************************************/
HB_FUNC( WFROW ) /* Return position of the top row of formatted area */
{
   HB_CT_WND * wnd;
   SHORT       UFRow;

   wnd = hb_ctWCurrent();

   UFRow = ( wnd->NCur ==0 ? wnd->BFRow : wnd->UFRow );
   if( hb_parl( 1 ) ) UFRow -= wnd->WFRow;

   hb_retni( UFRow );
}
/****************************************************************************/
HB_FUNC( WLASTCOL ) /* Return position of the rightmost column */
{
   HB_CT_WND * wnd;
   SHORT       LCol;

   wnd = hb_ctWCurrent();

   LCol = wnd->WLCol;

   if( hb_parl( 1 ) )
   {
      LCol += ( wnd->BFCol + wnd->BLCol - wnd->WFCol - wnd->WLCol ) / 2;

      if( LCol - wnd->WNCol + 1 < wnd->BFCol )
         LCol = wnd->BFCol + wnd->WNCol - 1;
      else if( LCol > wnd->BLCol )
         LCol = wnd->BLCol;
   }

   hb_retni( LCol );
}
/****************************************************************************/
HB_FUNC( WLASTROW ) /* Return position of the bottom row */
{
   HB_CT_WND * wnd;
   SHORT       LRow;

   wnd = hb_ctWCurrent();

   LRow = wnd->WLRow;

   if( hb_parl( 1 ) )
   {
      LRow += ( wnd->BFRow + wnd->BLRow - wnd->WFRow - wnd->WLRow ) / 2;

      if( LRow - wnd->WNRow + 1 < wnd->BFRow )
         LRow = wnd->BFRow + wnd->WNRow - 1;
      else if( LRow > wnd->BLRow )
         LRow = wnd->BLRow;
   }

   hb_retni( LRow );
}
/****************************************************************************/
HB_FUNC( WMODE ) /* Set the screen border overstep mode */
{
   BOOL MFRow = -2, MFCol = -2, MLRow = -2, MLCol = -2;

   if( ISLOG( 1 ) ) MFRow = hb_parl( 1 );
   if( ISLOG( 2 ) ) MFCol = hb_parl( 2 );
   if( ISLOG( 3 ) ) MLRow = hb_parl( 3 );
   if( ISLOG( 4 ) ) MLCol = hb_parl( 4 );

   hb_ctWMode( MFRow, MFCol, MLRow, MLCol );

   hb_retni( 0 );
}
/****************************************************************************/
HB_FUNC( WMOVE ) /* Moves a window */
{
   SHORT iWnd;

   iWnd = hb_ctWSelect( -1 );

   if( iWnd > 0 && hb_pcount() >= 2 && ISNUM( 1 ) && ISNUM( 2 ) )
   {
      iWnd = hb_ctWMove( hb_parni( 1 ), hb_parni( 2 ) );
   }

   hb_retni( iWnd );
}
/****************************************************************************/
HB_FUNC( WNUM ) /* Get the highest windows handle */
{
   hb_retni( hb_ctWNum() );
}
/****************************************************************************/
HB_FUNC( WOPEN ) /* Opens a new window */
{
   SHORT iwnd = -1;

   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) && ISNUM( 4 ) )
      iwnd = hb_ctWOpen( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                         hb_parni( 4 ), hb_parl( 5 ) );

   hb_retni( iwnd );
}
/****************************************************************************/
HB_FUNC( WROW ) /* Return position of the top row */
{
   HB_CT_WND * wnd;
   SHORT       FRow;

   wnd = hb_ctWCurrent();

   FRow = wnd->WFRow;

   if( hb_parl( 1 ) )
   {
      FRow += ( wnd->BFRow + wnd->BLRow - wnd->WFRow - wnd->WLRow ) / 2;

      if( FRow < wnd->BFRow )
         FRow = wnd->BFRow;
      else if( FRow + wnd->WNRow - 1 > wnd->BLRow )
         FRow = wnd->BLRow - wnd->WNRow + 1;
   }

   hb_retni( FRow );
}
/****************************************************************************/
HB_FUNC( WSELECT ) /* Activate window */
{
   hb_retni( hb_ctWSelect( ISNUM( 1 ) ? hb_parni( 1 ) : -1 ) );
}
/****************************************************************************/
HB_FUNC( WSETMOVE ) /* Set the interactive movement mode */
{
   hb_retl( hb_ctWSetMove( ISLOG( 1 ) ? hb_parl( 1 ) : -2 ) );
}
/****************************************************************************/
HB_FUNC( WSETSHADOW ) /* Set the window shadow color */
{
   SHORT nAttr = -2;

   if( ISCHAR( 1 ) )     nAttr = hb_gtColorToN( hb_parcx( 1 ) );
   else if( ISNUM( 1 ) ) nAttr = hb_parni( 1 );

   hb_retni( hb_ctWSetShadow( nAttr ) );
}
/****************************************************************************/
/* Get the array of windows handle ( New ) */
HB_FUNC( WSTACK )
{
   SHORT    i, SMax, *Stac;
   PHB_ITEM pStack, pN;

   hb_ctWStack( &Stac, &SMax );

   pStack = hb_itemArrayNew( SMax );

   for( i = 0; i < SMax; i++ )
   {
     pN = hb_itemPutNL( NULL, Stac[ i ] );
     hb_arraySet( pStack, i + 1, pN );
     hb_itemRelease( pN );
   }

   hb_itemRelease( hb_itemReturn( pStack ) );
}
/****************************************************************************/
HB_FUNC( WSTEP ) /* Set the step width of interactive window movement */
{
   SHORT iRet = -1;

   if( ISNUM( 1 ) && ISNUM( 2 ) )
      iRet = hb_ctWStep( hb_parni( 1 ), hb_parni( 2 ) );

   hb_retni( iRet );
}
/****************************************************************************/
