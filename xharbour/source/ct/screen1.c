/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 video functions:
 *
 * SCREENATTR(), SCREENMIX(), SAYSCREEN(),
 * CLEARWIN(), INVERTWIN(), UNTEXTWIN(), CHARWIN(), COLORWIN(), COLORREPL()
 *
 *   and Harbour extension:
 *
 * SCREENTEXT()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbdefs.h"
#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( SCREENATTR )
{
   SHORT    sRow, sCol;
   int      iRow, iCol;
   BYTE     bColor, bAttr;
   USHORT   usChar;

   hb_gtGetPos( &sRow, &sCol );
   iRow  = ISNUM( 1 ) ? hb_parni( 1 ) : sRow;
   iCol  = ISNUM( 2 ) ? hb_parni( 2 ) : sCol;

   if( hb_gtGetChar( ( USHORT ) iRow, ( USHORT ) iCol, &bColor, &bAttr, &usChar ) != SUCCESS )
      bColor = 0;

   hb_retni( ( int ) bColor );
}

HB_FUNC( SCREENMIX )
{
   HB_SIZE ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      const char * szText   = hb_parc( 1 ), * szAttr;
      HB_SIZE      ulAttr   = hb_parclen( 2 ), ul = 0;
      SHORT        sRow, sCol;
      int          iRow, iCol, i;

      if( ulAttr == 0 )
      {
         szAttr   = " ";
         ulAttr   = 1;
      }
      else
         szAttr = hb_parc( 2 );

      hb_gtGetPos( &sRow, &sCol );
      iRow  = ISNUM( 3 ) ? hb_parni( 3 ) : sRow;
      iCol  = ISNUM( 4 ) ? hb_parni( 4 ) : sCol;

      if( iRow >= 0 && iCol >= 0 &&
          iRow <= hb_gtMaxRow() && iCol <= hb_gtMaxCol() )
      {
         hb_gtBeginWrite();
         i = iCol;
         do
         {
            if( hb_gtPutChar( ( USHORT ) iRow, ( USHORT ) i++, szAttr[ ul ], 0, *szText++ ) != SUCCESS )
            {
               if( ++iRow > hb_gtMaxRow() )
                  break;
               --szText;
               ++ulLen;
               i = iCol;
            }
            else if( ++ul == ulAttr )
               ul = 0;
         }
         while( --ulLen );
         hb_gtEndWrite();
      }
   }

   hb_retc( NULL );
}

HB_FUNC( SAYSCREEN )
{
   HB_SIZE ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      const char *   szText = hb_parc( 1 );
      SHORT          sRow, sCol;
      int            iRow, iCol, i;

      hb_gtGetPos( &sRow, &sCol );
      iRow  = ISNUM( 2 ) ? hb_parni( 2 ) : sRow;
      iCol  = ISNUM( 3 ) ? hb_parni( 3 ) : sCol;

      if( iRow >= 0 && iCol >= 0 &&
          iRow <= hb_gtMaxRow() && iCol <= hb_gtMaxCol() )
      {
         hb_gtBeginWrite();
         i = iCol;
         do
         {
            BYTE     bColor, bAttr;
            USHORT   usChar;

            if( hb_gtGetChar( ( USHORT ) iRow, ( USHORT ) i, &bColor, &bAttr, &usChar ) != SUCCESS )
            {
               if( ++iRow > hb_gtMaxRow() )
                  break;
               ++ulLen;
               i = iCol;
            }
            else
               hb_gtPutChar( ( USHORT ) iRow, ( USHORT ) i++, bColor, bAttr, *szText++ );
         }
         while( --ulLen );
         hb_gtEndWrite();
      }
   }

   hb_retc( NULL );
}

static BOOL hb_ctGetWinCord( int * piTop, int * piLeft,
                             int * piBottom, int * piRight )
{
   int   iMaxRow  = hb_gtMaxRow();
   int   iMaxCol  = hb_gtMaxCol();

   hb_gtGetPosEx( piTop, piLeft );

   if( ISNUM( 1 ) )
      *piTop = hb_parni( 1 );

   if( ISNUM( 2 ) )
      *piLeft = hb_parni( 2 );

   if( ISNUM( 3 ) )
   {
      *piBottom = hb_parni( 3 );
      if( *piBottom > iMaxRow )
         *piBottom = iMaxRow;
   }
   else
      *piBottom = iMaxRow;

   if( ISNUM( 4 ) )
   {
      *piRight = hb_parni( 4 );
      if( *piRight > iMaxCol )
         *piRight = iMaxCol;
   }
   else
      *piRight = iMaxCol;

   return *piTop >= 0 && *piLeft >= 0 &&
          *piTop <= *piBottom && *piLeft <= *piRight;
}

static int hb_ctGetClearChar( int iParam )
{
   int iChar;

   if( ISNUM( iParam ) )
      iChar = hb_parni( iParam );
   else if( ISCHAR( iParam ) )
      iChar = ( UCHAR ) hb_parc( iParam )[ 0 ];
   else
      iChar = hb_gtGetClearChar();

   return iChar;
}

static int hb_ctGetClearColor( int iParam )
{
   int iColor;

   if( ISNUM( iParam ) )
      iColor = hb_parni( iParam );
   else if( ISCHAR( iParam ) )
      iColor = hb_gtColorToN( hb_parc( iParam ) );
   else
      iColor = hb_gtGetClearColor();

   return iColor;
}

HB_FUNC( CLEARWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      BYTE bColor, bChar;

      bColor   = ( BYTE ) hb_ctGetClearColor( 5 );
      bChar    = ( BYTE ) hb_ctGetClearChar( 6 );

      hb_gtScrollEx( iTop, iLeft, iBottom, iRight, bColor, bChar, 0, 0 );
   }

   hb_retc( NULL );
}

HB_FUNC( INVERTWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE     bColor, bAttr;
            USHORT   usChar;

            hb_gtGetChar( ( USHORT ) iTop, ( USHORT ) iCol, &bColor, &bAttr, &usChar );
            bColor = ( bColor & 0x88 ) |
                     ( ( bColor & 0x07 ) << 4 ) |
                     ( ( bColor >> 4 ) & 0x07 );
            hb_gtPutChar( ( USHORT ) iTop, ( USHORT ) iCol, bColor, bAttr, usChar );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc( NULL );
}

HB_FUNC( UNTEXTWIN )
{
   int   iTop, iLeft, iBottom, iRight;
   UCHAR ucRepl, ucInit, ucEnd;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      ucRepl = ( UCHAR ) hb_ctGetClearChar( 5 );

      if( ISNUM( 6 ) )
         ucInit = ( UCHAR ) hb_parni( 6 );
      else if( hb_parclen( 6 ) > 0 )
         ucInit = ( UCHAR ) hb_parc( 6 )[ 0 ];
      else
         ucInit = 176;

      if( ISNUM( 7 ) )
         ucEnd = ( UCHAR ) hb_parni( 7 );
      else if( hb_parclen( 7 ) > 0 )
         ucEnd = ( UCHAR ) hb_parc( 7 )[ 0 ];
      else
         ucEnd = 223;

      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE     bColor, bAttr;
            USHORT   usChar;

            hb_gtGetChar( ( USHORT ) iTop, ( USHORT ) iCol, &bColor, &bAttr, &usChar );
            if( ucInit <= ucEnd ? ( usChar < ucInit || usChar > ucEnd ) :
                ( usChar > ucEnd && usChar < ucInit ) )
               hb_gtPutChar( ( USHORT ) iTop, ( USHORT ) iCol, bColor, bAttr, ucRepl );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc( NULL );
}

HB_FUNC( CHARWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      UCHAR ucNewChar, ucOldChar = 0;
      BOOL  fAll = FALSE;

      ucNewChar = ( UCHAR ) hb_ctGetClearChar( 5 );

      if( ISNUM( 6 ) )
         ucOldChar = ( UCHAR ) hb_parni( 6 );
      else if( hb_parclen( 6 ) > 0 )
         ucOldChar = ( UCHAR ) hb_parc( 6 )[ 0 ];
      else
         fAll = TRUE;

      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE     bColor, bAttr;
            USHORT   usChar;

            hb_gtGetChar( ( USHORT ) iTop, ( USHORT ) iCol, &bColor, &bAttr, &usChar );
            if( fAll || usChar == ucOldChar )
               hb_gtPutChar( ( USHORT ) iTop, ( USHORT ) iCol, bColor, bAttr, ucNewChar );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc( NULL );
}

HB_FUNC( COLORWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      UCHAR ucNewColor, ucOldColor = 0;
      BOOL  fAll = FALSE;

      ucNewColor = ( UCHAR ) hb_ctGetClearColor( 5 );

      if( ISNUM( 6 ) || ISCHAR( 6 ) )
         ucOldColor = ( UCHAR ) hb_ctGetClearColor( 6 );
      else
         fAll = TRUE;

      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE     bColor, bAttr;
            USHORT   usChar;

            hb_gtGetChar( ( USHORT ) iTop, ( USHORT ) iCol, &bColor, &bAttr, &usChar );
            if( fAll || bColor == ucOldColor )
               hb_gtPutChar( ( USHORT ) iTop, ( USHORT ) iCol, ucNewColor, bAttr, usChar );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc( NULL );
}

HB_FUNC( SCREENTEXT )
{
   int      iTop, iLeft, iBottom, iRight;
   char *   pBuffer, * szText;
   ULONG    ulSize;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      ulSize   = ( ULONG ) ( iBottom - iTop + 1 ) * ( iRight - iLeft + 1 );
      szText   = pBuffer = ( char * ) hb_xgrab( ulSize + 1 );
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE     bColor, bAttr;
            USHORT   usChar;
            hb_gtGetChar( ( USHORT ) iTop, ( USHORT ) iCol, &bColor, &bAttr, &usChar );
            *szText++ = ( char ) usChar;
            ++iCol;
         }
         ++iTop;
      }
      hb_retclen_buffer( pBuffer, ulSize );
   }
   else
      hb_retc( NULL );
}

HB_FUNC( COLORREPL )
{
   int   iMaxRow  = hb_gtMaxRow();
   int   iMaxCol  = hb_gtMaxCol();
   int   iRow     = 0, iCol;
   UCHAR ucNewColor, ucOldColor = 0;
   BOOL  fAll     = FALSE;

   ucNewColor = ( UCHAR ) hb_ctGetClearColor( 1 );

   if( ISNUM( 2 ) || ISCHAR( 2 ) )
      ucOldColor = ( UCHAR ) hb_ctGetClearColor( 2 );
   else
      fAll = TRUE;

   hb_gtBeginWrite();
   while( iRow <= iMaxRow )
   {
      iCol = 0;
      while( iCol <= iMaxCol )
      {
         BYTE     bColor, bAttr;
         USHORT   usChar;

         hb_gtGetChar( ( USHORT ) iRow, ( USHORT ) iCol, &bColor, &bAttr, &usChar );
         if( fAll || bColor == ucOldColor )
            hb_gtPutChar( ( USHORT ) iRow, ( USHORT ) iCol, ucNewColor, bAttr, usChar );
         ++iCol;
      }
      ++iRow;
   }
   hb_gtEndWrite();

   hb_retc( NULL );
}
